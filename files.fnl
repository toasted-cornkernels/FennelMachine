"
The file behind the focused window, like Emacs' visited file: the AXDocument
of a document window, or Finder's selection (else its folder). Reveal it in
Finder (dired-jump), copy its path, rename, duplicate or trash it; find a file,
reopen a recent one, save the document; visit this config in Emacs.
"
(local minibuffer (require :lib.minibuffer))

(local home (os.getenv :HOME))
(local recent-days 90)
(local recent-limit 100)

(fn url-decode [s]
  (pick-values 1 (s:gsub "%%(%x%x)" #(string.char (tonumber $1 16)))))

(fn file-url->path [url]
  (when (and (= (type url) :string) (url:match "^file://"))
    (let [path (url-decode (-> url (: :gsub "^file://localhost" "") (: :gsub "^file://" "")))]
      (if (and (> (length path) 1) (= (path:sub -1) "/")) (path:sub 1 -2) path))))

(fn window-document [win]
  "The AXDocument of WIN: a file:// URL, or a web page's URL in Safari."
  (let [element (and win (hs.axuielement.windowElement win))
        document (and element (element:attributeValue :AXDocument))]
    (when (and (= (type document) :string) (not= document "")) document)))

(fn finder-path []
  "Finder's first selected item, else the folder of its front window."
  (let [(ok path) (hs.osascript.applescript
                   "tell application \"Finder\"
                      set sel to selection
                      if sel is not {} then return POSIX path of (item 1 of sel as alias)
                      if (count of Finder windows) > 0 then return POSIX path of (target of front Finder window as alias)
                    end tell
                    return \"\"")]
    (when (and ok (= (type path) :string) (not= path ""))
      (if (and (> (length path) 1) (= (path:sub -1) "/")) (path:sub 1 -2) path))))

(fn current-path []
  "The file or folder of the focused window, or nil."
  (let [app (hs.application.frontmostApplication)]
    (if (and app (= (app:bundleID) "com.apple.finder"))
        (finder-path)
        (file-url->path (window-document (hs.window.focusedWindow))))))

(fn abbreviate [path]
  (if (= (path:sub 1 (length home)) home) (.. "~" (path:sub (+ (length home) 1))) path))

(fn dirname [path] (or (path:match "^(.*)/[^/]+/?$") "/"))
(fn basename [path] (or (path:match "([^/]+)/?$") path))

(fn with-path [f]
  (match (current-path)
    path (f path)
    _ (minibuffer.message "No file behind this window")))

(fn reveal []
  "Reveal the current file in Finder, like dired-jump."
  (with-path #((. (require :launcher) :open) "-R" $1)))

(fn copy-path []
  "Copy the current file's path."
  (with-path #(do (hs.pasteboard.setContents $1)
                  (minibuffer.message (.. "Copied " $1)))))

(fn read-new-path [prompt path on-path]
  "Read a new name for PATH, prefilled with its name; a name without a slash
  stays in the same folder, ~ is expanded. ON-PATH gets the new path."
  (minibuffer.completing-read
   prompt []
   {:on-accept (fn [_ input]
                 (let [input (input:match "^%s*(.-)%s*$")
                       input (input:gsub "^~" home)
                       new (if (input:find "/" 1 true) input (.. (dirname path) "/" input))]
                   (if (or (= input "") (= new path)) (minibuffer.message "Unchanged")
                       (hs.fs.attributes new) (minibuffer.message (.. (abbreviate new) " exists"))
                       (on-path new))))})
  (minibuffer.set-input (basename path)))

(fn rename []
  "Rename (or move) the current file, like rename-visited-file. Document
  apps follow the file to its new name."
  (with-path
    (fn [path]
      (read-new-path (.. "Rename " (basename path) " to: ") path
                     (fn [new]
                       (let [(ok err) (os.rename path new)]
                         (minibuffer.message (if ok (.. "Renamed to " (abbreviate new))
                                                 (.. "Rename failed: " (tostring err))))))))))

(fn duplicate []
  "Copy the current file to a new path, like copy-this-file."
  (with-path
    (fn [path]
      (read-new-path (.. "Copy " (basename path) " to: ") path
                     (fn [new]
                       (let [(_ ok) (hs.execute (.. "/bin/cp -Rp " (string.format "%q" path)
                                                    " " (string.format "%q" new)))]
                         (minibuffer.message (if ok (.. "Copied to " (abbreviate new))
                                                 "Copy failed"))))))))

(fn trash []
  "Move the current file to the Trash after asking, like
  delete-current-buffer-file."
  (with-path
    (fn [path]
      (minibuffer.y-or-n-p
       (.. "Move " (basename path) " to the Trash?")
       #(let [(ok _ err) (hs.osascript.applescript
                          (.. "tell application \"Finder\" to delete POSIX file "
                              (hs.inspect path)))]
          (minibuffer.message (if ok (.. "Trashed " (abbreviate path))
                                  (.. "Trash failed: " (hs.inspect err)))))))))

(fn find-file []
  "Find a file with Spotlight in the launcher, like find-file."
  ((. (require :launcher) :show) "'"))

;; Kept so the task isn't garbage collected while it runs.
(var recent-task nil)

(fn parse-recent [stdout]
  "Paths from mdfind -attr kMDItemLastUsedDate output, most recent first."
  (let [items []]
    (each [line (stdout:gmatch "[^\n]+")]
      (let [(path date) (line:match "^(.-)%s+kMDItemLastUsedDate = (.+)$")]
        (when (and path (not (path:match "/%.")) (not (path:match "%.app$"))
                   (let [attrs (hs.fs.attributes path)]
                     (and attrs (= attrs.mode :file))))
          (table.insert items {: path : date}))))
    (table.sort items #(> $1.date $2.date))
    (icollect [i item (ipairs items)] (when (<= i recent-limit) item.path))))

(fn recent-files []
  "Reopen a file opened in the last few months, most recent first, like
  consult-recent-file."
  (when recent-task (recent-task:terminate))
  (set recent-task
       (hs.task.new "/usr/bin/mdfind"
                    (fn [_ stdout]
                      (set recent-task nil)
                      (let [paths (parse-recent (or stdout ""))]
                        (if (= (length paths) 0)
                            (minibuffer.message "No recent files")
                            ((. (require :launcher) :read-file) "Recent file: " paths))))
                    ["-onlyin" home "-attr" "kMDItemLastUsedDate"
                     (.. "kMDItemLastUsedDate >= $time.today(-" recent-days ")")]))
  (recent-task:start))

(fn save [] "Save the document (Cmd-S), like save-buffer." (hs.eventtap.keyStroke [:cmd] :s))

(fn save-as []
  "Save the document under another name (Cmd-Shift-S), like write-file."
  (hs.eventtap.keyStroke [:cmd :shift] :s))

(fn visit-config []
  "Open this config's config.fnl in Emacs, like visit-init-dot-el."
  (let [path (.. hs.configdir "/config.fnl")]
    ((. (require :emacs) :emacsclient) ["-n" (or (hs.fs.pathToAbsolute path) path)]))
  (hs.timer.doAfter 0.2 #(hs.application.launchOrFocus :Emacs)))

{: current-path
 : window-document
 : file-url->path
 : parse-recent
 : reveal
 : copy-path
 : rename
 : duplicate
 : trash
 : find-file
 : recent-files
 : save
 : save-as
 : visit-config}
