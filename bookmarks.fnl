"
Bookmarks, like Emacs' bookmark.el, for whatever the focused window shows:
the browser page (its URL), the document (its file), or else the app and the
window's title. Jumping opens the URL or file in the same app, or brings the
app forward and focuses a window with that title. Kept in hs.settings.
"
(local minibuffer (require :lib.minibuffer))
(local files (require :files))

(local settings-key "bookmarks")

;; Chromium browsers, whose windows carry no AXDocument: their front tab's URL
;; comes from AppleScript.
(local chromium-ids {"com.google.Chrome" true
                     "com.google.Chrome.canary" true
                     "com.brave.Browser" true
                     "com.microsoft.edgemac" true
                     "com.vivaldi.Vivaldi" true
                     "company.thebrowser.Browser" true})

(fn load [] (or (hs.settings.get settings-key) []))
(fn save [marks] (hs.settings.set settings-key marks))

(fn chromium-url [bundle]
  (let [(ok url) (hs.osascript.applescript
                  (.. "tell application id " (hs.inspect bundle)
                      " to return URL of active tab of front window"))]
    (when (and ok (= (type url) :string) (not= url "")) url)))

(fn location []
  "What the focused window shows, as a bookmark without a name."
  (let [win (hs.window.focusedWindow)
        app (hs.application.frontmostApplication)
        bundle (and app (app:bundleID))
        document (files.window-document win)
        path (if (= bundle "com.apple.finder") (files.current-path)
                 (files.file-url->path document))
        url (when (not path)
              (if (and document (document:match "^%a[%w+.-]*://")) document
                  (and bundle (. chromium-ids bundle)) (chromium-url bundle)))]
    (when app
      {:app (app:name)
       : bundle
       :title (and win (win:title))
       : path
       : url})))

(fn describe [mark]
  (or mark.url (and mark.path ((. (require :launcher) :abbreviate) mark.path))
      (and mark.title (not= mark.title "") mark.title)
      ""))

(fn bookmark-set []
  "Bookmark the focused window under a name (default: its title), replacing a
  bookmark of the same name, like bookmark-set."
  (match (location)
    loc (do
          (minibuffer.completing-read
           (.. "Bookmark " (describe loc) " as: ")
           []
           {:on-accept (fn [_ input]
                         (let [name (input:match "^%s*(.-)%s*$")]
                           (if (= name "")
                               (minibuffer.message "No name given")
                               (let [marks (icollect [_ m (ipairs (load))]
                                             (when (not= m.name name) m))]
                                 (set loc.name name)
                                 (table.insert marks 1 loc)
                                 (save marks)
                                 (minibuffer.message (.. "Bookmarked " name))))))})
          (minibuffer.set-input (or loc.title loc.app)))
    _ (minibuffer.message "Nothing to bookmark")))

(fn open [...]
  (: (hs.task.new "/usr/bin/open" nil [...]) :start))

(fn jump-to [mark]
  (if mark.url (if mark.bundle (open "-b" mark.bundle mark.url) (open mark.url))
      (and mark.path (= mark.bundle "com.apple.finder"))
      (let [attrs (hs.fs.attributes mark.path)]
        (if (and attrs (= attrs.mode :directory)) (open mark.path) (open "-R" mark.path)))
      mark.path (if mark.bundle (open "-b" mark.bundle mark.path) (open mark.path))
      (let [app (or (and mark.bundle (hs.application.get mark.bundle))
                    (hs.application.get mark.app))]
        (if (not app)
            (if mark.bundle (hs.application.launchOrFocusByBundleID mark.bundle)
                (hs.application.launchOrFocus mark.app))
            (let [win (and mark.title
                           (hs.fnutils.find (app:allWindows) #(= ($:title) mark.title)))]
              ((. (require :buffers) :show-app) app)
              (when win
                (when (win:isMinimized) (win:unminimize))
                (win:focus)))))))

(fn read-bookmark [prompt on-accept actions]
  (let [marks (load)]
    (if (= (length marks) 0)
        (minibuffer.message "No bookmarks")
        (minibuffer.completing-read
         prompt
         (icollect [_ m (ipairs marks)]
           {:text m.name
            :annotation (.. (or m.app "") "  " (describe m))
            :keywords (.. (or m.app "") " " (or m.url m.path ""))
            :mark m})
         {:on-accept (fn [c] (when c (on-accept c.mark)))
          : actions}))))

(fn bookmark-delete [?mark]
  "Delete a bookmark, like bookmark-delete."
  (fn delete [mark]
    (save (icollect [_ m (ipairs (load))] (when (not= m.name mark.name) m)))
    (minibuffer.message (.. "Deleted bookmark " mark.name)))
  (if (and ?mark ?mark.name) (delete ?mark)
      (read-bookmark "Delete bookmark: " delete)))

(fn bookmark-jump []
  "Jump to a bookmark, like bookmark-jump. s-d deletes the selected one."
  (read-bookmark "Jump to bookmark: " jump-to
                 {"s-d" (fn [c]
                          (when c
                            (bookmark-delete c.mark)
                            (hs.timer.doAfter 0.3 bookmark-jump)))}))

{: location
 : bookmark-set
 : bookmark-jump
 : bookmark-delete}
