"Act on the text selected in any app, like embark-act: search the web for
it, open it as a URL or path, calculate it, show it in large type, edit it in
Emacs or ask ChatGPT about it"

(local minibuffer (require :lib.minibuffer))
(local large-type (require :lib.large-type))
(local clipboard (require :clipboard))
(local launcher (require :launcher))

(local copy-timeout 0.6)
(local poll-interval 0.05)
(local prompt-chars 40)

;; Kept here so that pending timers aren't garbage collected.
(var timer nil)

(fn poll [ready? on-ready on-timeout seconds]
  "Call ON-READY once READY? holds, checking every poll-interval, or
  ON-TIMEOUT after SECONDS."
  (var waited 0)
  (when timer (timer:stop))
  (set timer (hs.timer.doEvery poll-interval
                               (fn []
                                 (set waited (+ waited poll-interval))
                                 (if (ready?) (do (timer:stop) (on-ready))
                                     (>= waited seconds)
                                     (do (timer:stop) (on-timeout)))))))

;; Getting the selection ============================
;; ==================================================

(fn ax-selection []
  "The selected text of the focused element, for apps that expose it."
  (let [system (hs.axuielement.systemWideElement)
        (_ element) (pcall #(system:attributeValue :AXFocusedUIElement))
        (_ text) (when element
                   (pcall #(element:attributeValue :AXSelectedText)))]
    (when (and (= (type text) :string) (not= text "")) text)))

(fn borrow-clipboard [f then]
  "Call F with the clipboard to use as it likes, then put back what it held
  and call THEN; the clipboard history doesn't see any of it."
  (let [saved (hs.pasteboard.readAllData)]
    (clipboard.ignore-for (+ copy-timeout 1))
    (f (fn [...]
         (if (next saved)
             (hs.pasteboard.writeAllData saved)
             (hs.pasteboard.clearContents))
         (then ...)))))

(fn copy-selection [callback]
  "Call CALLBACK with the text that Cmd-C copies from the frontmost app, or nil."
  (borrow-clipboard
   (fn [restore]
     (let [count (hs.pasteboard.changeCount)]
       (hs.eventtap.keyStroke [:cmd] :c 0)
       (poll #(not= (hs.pasteboard.changeCount) count)
             #(restore (hs.pasteboard.getContents))
             #(restore nil)
             copy-timeout)))
   callback))

(fn selection [callback]
  "Call CALLBACK with the selected text and where it came from."
  (let [text (ax-selection)]
    (if text
        (callback text :selection)
        (copy-selection
         (fn [copied]
           (if (and copied (not= copied ""))
               (callback copied :selection)
               (let [clip (hs.pasteboard.getContents)]
                 (callback (when (not= clip "") clip) :clipboard))))))))

;; Actions ==========================================
;; ==================================================

(fn open [...]
  (: (hs.task.new "/usr/bin/open" nil [...]) :start))

(fn path-of [text]
  (let [path (: (text:match "^%s*(.-)%s*$") :gsub "^~" (os.getenv :HOME))]
    (when (and (path:match "^/") (hs.fs.attributes path)) path)))

(fn count-words [text]
  (let [text (text:gsub "\r\n?" "\n")
        (_ lines) (text:gsub "\n" "")
        (_ words) (text:gsub "%S+" "")]
    (minibuffer.message
     (string.format "%d line%s, %d word%s, %d character%s"
                    (+ lines 1) (if (= lines 0) "" "s")
                    words (if (= words 1) "" "s")
                    (utf8.len text) (if (= (utf8.len text) 1) "" "s")))))

(fn elisp-string [text]
  (.. "\"" (: (text:gsub "\\" "\\\\") :gsub "\"" "\\\"") "\""))

(fn edit-in-emacs [text]
  "Open TEXT in a new Emacs buffer."
  ((. (require :emacs) :emacsclient)
   ["-n" "-e"
    (.. "(progn (select-frame-set-input-focus (selected-frame))"
        " (switch-to-buffer (generate-new-buffer \"*selection*\"))"
        " (insert " (elisp-string text) ") (goto-char (point-min)))")])
  (hs.timer.doAfter 0.2 #(hs.application.launchOrFocus :Emacs)))

(fn paste [text]
  (borrow-clipboard
   (fn [restore]
     (hs.pasteboard.setContents text)
     (hs.eventtap.keyStroke [:cmd] :v 0)
     ;; Give the app time to read the clipboard before it is restored.
     (hs.timer.doAfter 0.3 restore))
   #nil))

(fn ask-chatgpt [text]
  "Put TEXT in the ChatGPT composer as a draft, without sending it."
  (let [cold? (not (hs.application.get :ChatGPT))]
    (hs.application.launchOrFocus :ChatGPT)
    (poll #(let [app (hs.application.frontmostApplication)]
             (and app (= (app:name) :ChatGPT) (app:focusedWindow)))
          ;; A freshly launched app takes a while to load the conversation.
          #((. (require :chatgpt) :focus-composer) #(paste text)
                                                   (when cold? 15))
          #(minibuffer.message "ChatGPT didn't come up")
          10)))

(fn actions-for [text]
  (let [items []
        url (launcher.url-of text)
        path (path-of text)
        value (launcher.calculate text)]
    (fn add [title annotation f]
      (table.insert items {:text title :annotation (or annotation "") :run f}))
    (when path
      (add "Open" path #(open path))
      (add "Reveal in Finder" path #(open "-R" path))
      (add "Open in Emacs" path
           #(do ((. (require :emacs) :emacsclient) ["-n" path])
                (hs.timer.doAfter 0.2 #(hs.application.launchOrFocus :Emacs)))))
    (when (and url (not path)) (add "Open URL" url #(open url)))
    (when value
      (add (.. "Copy Result " value) "calculate"
           #(do (hs.pasteboard.setContents value)
                (minibuffer.message (.. "Copied " value)))))
    (let [encoded (: (hs.http.encodeForQuery text) :gsub "%%" "%%%%")]
      (each [_ search (ipairs launcher.web-searches)]
        (let [search-url (search.url:gsub "%%s" encoded)]
          (add (.. "Search " search.name) search.keyword #(open search-url)))))
    (add "Copy" nil #(do (hs.pasteboard.setContents text)
                         (minibuffer.message "Copied")))
    (add "Large Type" nil #(large-type.show text))
    (add "Count Words" nil #(count-words text))
    (add "Edit in Emacs" nil #(edit-in-emacs text))
    (add "Ask ChatGPT" "draft" #(ask-chatgpt text))
    items))

(fn label [text]
  (let [line (: (text:gsub "%s+" " ") :match "^%s*(.-)%s*$")]
    (if (> (utf8.len line) prompt-chars)
        (.. (line:sub 1 (- (or (utf8.offset line (+ prompt-chars 1)) 0) 1)) "…")
        line)))

(fn act []
  "Choose an action for the selected text, or the clipboard without one."
  (selection
   (fn [text source]
     (if (not text)
         (minibuffer.message "No selection")
         (minibuffer.completing-read
          (.. "Act on " (if (= source :clipboard) "clipboard " "")
              "“" (label text) "”: ")
          (actions-for text)
          {:on-accept (fn [c] (when c (c.run)))})))))

{: act
 : selection}
