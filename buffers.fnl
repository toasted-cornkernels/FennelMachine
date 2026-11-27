"Apps as Emacs buffers, their windows as the buffer's windows: switch to an
app or any window by name, cycle the windows of the frontmost app, return to
the last app or window, hide (bury) an app, list the apps ibuffer-style; kill
the buffer (quit the app), force quit it or relaunch it (revert)"

(local minibuffer (require :lib.minibuffer))

(local relaunch-timeout 10)

;; Kept here so that the timer isn't garbage collected while it waits.
(var relaunch-timer nil)

;; A window filter fills in asynchronously, so one is started by init.
;; Titled standard windows: no panels or untitled helper windows, but
;; minimized windows (whose subrole turns into AXDialog) and those on other
;; Spaces are included.
;; Kept active, the filter follows window events as they happen; a passive
;; one re-checks every window over Accessibility on each getWindows, which
;; held up switch-to-window by a few milliseconds per open window.
(var filter nil)

(fn init []
  (when (not filter)
    (set filter (-> (hs.window.filter.new false)
                    (: :setDefaultFilter
                       {:allowRoles [:AXStandardWindow :AXDialog]
                        :allowTitles 1})
                    (: :keepActive))))
  filter)

(fn windows []
  "Standard windows, most recently focused first."
  (: (init) :getWindows hs.window.filter.sortByFocusedLast))

(fn same? [a b]
  (and a b (= (a:id) (b:id))))

(fn show [win]
  (let [app (win:application)]
    (when (and app (app:isHidden)) (app:unhide))
    (when (win:isMinimized) (win:unminimize))
    (win:focus)))

(fn candidate [win]
  (let [app (win:application)
        app-name (or (and app (app:name)) "")
        title (win:title)
        flags (.. (if (win:isMinimized) " [minimized]" "")
                  (if (and app (app:isHidden)) " [hidden]" ""))]
    {:text (if (= title "") app-name title)
     :annotation (.. app-name flags)
     :keywords app-name
     :window win}))

(fn switch-to-window []
  "Read a window in the minibuffer, like switch-to-buffer: most recent first,
  the current window last. s-w closes the selected window."
  (let [current (hs.window.focusedWindow)
        wins (windows)
        others (icollect [_ w (ipairs wins)] (when (not (same? w current)) w))]
    (when current (table.insert others current))
    (if (= (length others) 0)
        (minibuffer.message "No windows")
        (minibuffer.completing-read
         "Switch to window: "
         (icollect [_ w (ipairs others)] (candidate w))
         {:on-accept (fn [c] (when c (show c.window)))
          :actions {"s-w" (fn [c]
                            (when c
                              (if (c.window:close)
                                  ;; Give the filter a moment to drop it.
                                  (hs.timer.doAfter 0.2 switch-to-window)
                                  (minibuffer.message
                                   (.. "Can't close " c.text)))))}}))))

(fn app-windows [app]
  "The standard, unminimized windows of APP in a stable order."
  (let [wins (icollect [_ w (ipairs (app:allWindows))]
               (when (and (w:isStandard) (not (w:isMinimized))) w))]
    (table.sort wins #(< ($1:id) ($2:id)))
    wins))

(fn cycle [step]
  "Focus the next (STEP 1) or previous (STEP -1) window of the frontmost app,
  like next-buffer / previous-buffer."
  (let [app (hs.application.frontmostApplication)
        wins (if app (app-windows app) [])
        current (hs.window.focusedWindow)
        n (length wins)
        index (accumulate [found nil i w (ipairs wins)]
                (or found (when (same? w current) i)))]
    (if (< n 2)
        (minibuffer.message (.. "No other " (if app (app:name) "") " window"))
        (let [target (. wins (+ 1 (% (+ (- (or index 1) 1) step) n)))]
          (target:focus)))))

(fn next-window [] (cycle 1))
(fn previous-window [] (cycle -1))

(fn last-window []
  "Switch to the previously focused window, like evil-switch-to-windows-last-buffer."
  (let [current (hs.window.focusedWindow)
        target (accumulate [found nil _ w (ipairs (windows))]
                 (or found (when (not (same? w current)) w)))]
    (if target (show target) (minibuffer.message "No other window"))))

(fn frontmost []
  (let [app (hs.application.frontmostApplication)]
    (if app app (do (minibuffer.message "No frontmost app") nil))))

(fn regular? [app]
  (and app (= (app:kind) 1)))

(fn apps []
  "Running apps with a Dock icon, most recently focused first (by their
  windows), then the ones without windows."
  (let [seen {}
        acc []]
    (fn add [app]
      (when (and (regular? app) (not (. seen (app:pid))))
        (tset seen (app:pid) true)
        (table.insert acc app)))
    (each [_ w (ipairs (windows))] (add (w:application)))
    (each [_ app (ipairs (hs.application.runningApplications))] (add app))
    acc))

(fn same-app? [a b]
  (and a b (= (a:pid) (b:pid))))

(fn show-app [app]
  "Bring APP forward like a buffer: unhide it, and unminimize a window when
  none is visible."
  (when (app:isHidden) (app:unhide))
  (when (= (length (app:visibleWindows)) 0)
    (let [w (hs.fnutils.find (app:allWindows) #($:isMinimized))]
      (when w (w:unminimize))))
  (app:activate))

(fn app-candidate [app mark?]
  (let [n (length (app-windows app))
        main (app:mainWindow)
        title (or (and main (main:title)) "")]
    {:text (.. (if mark? "* " "") (app:name))
     :annotation (.. (if (= n 1) "1 window" (.. n " windows"))
                     (if (app:isHidden) " [hidden]" "")
                     (if (= title "") "" (.. "  " title)))
     :keywords (or (app:bundleID) "")
     : app}))

(fn others-first [all current same?]
  "ALL with CURRENT moved to the end, like switch-to-buffer's list."
  (let [others (icollect [_ x (ipairs all)] (when (not (same? x current)) x))]
    (when (hs.fnutils.find all #(same? $1 current)) (table.insert others current))
    others))

(fn switch-to-app []
  "Read an app in the minibuffer, like switch-to-buffer: most recent first,
  the frontmost last. s-q quits the selected app and reopens the list."
  (let [listed (others-first (apps) (hs.application.frontmostApplication) same-app?)]
    (minibuffer.completing-read
     "Switch to app: "
     (icollect [_ app (ipairs listed)] (app-candidate app))
     {:on-accept (fn [c] (when c (show-app c.app)))
      :actions {"s-q" (fn [c]
                        (when c
                          (c.app:kill)
                          (hs.timer.doAfter 0.5 switch-to-app)))}})))

(fn last-app []
  "Switch to the previously used app, like evil-switch-to-windows-last-buffer."
  (let [current (hs.application.frontmostApplication)
        target (hs.fnutils.find (apps) #(not (same-app? $1 current)))]
    (if target (show-app target) (minibuffer.message "No other app"))))

(fn bury-app []
  "Hide the frontmost app (Cmd-H), like bury-buffer."
  (match (frontmost)
    app (app:hide)))

(fn new-window []
  "A new window of the frontmost app (Cmd-N), like clone-indirect-buffer."
  (hs.eventtap.keyStroke [:cmd] :n))

;; pid -> true for the apps marked in `list-apps`.
(var marks {})

(fn list-apps []
  "An ibuffer-like list of the running apps. s-m marks or unmarks the
  selected app, s-u unmarks all; s-d quits, s-D force quits and s-h hides the
  marked apps (or the selected one), then the list comes back. RET switches."
  (let [listed (apps)
        candidates (icollect [_ app (ipairs listed)]
                     (app-candidate app (. marks (app:pid))))
        selected #(let [s (. (minibuffer.current) :session)]
                    (and s (. s.matches s.selected)))
        targets (fn [c]
                  (let [marked (icollect [_ app (ipairs listed)]
                                 (when (. marks (app:pid)) app))]
                    (if (> (length marked) 0) marked
                        c [c.app]
                        [])))
        run-on (fn [verb f]
                 (fn [c]
                   (let [ts (targets c)]
                     (each [_ app (ipairs ts)] (f app))
                     (set marks {})
                     (when (> (length ts) 0)
                       (minibuffer.message (.. verb " " (length ts)
                                               (if (= (length ts) 1) " app" " apps"))))
                     (hs.timer.doAfter 0.6 list-apps))))]
    (minibuffer.completing-read
     "Apps: "
     candidates
     {:on-accept (fn [c] (set marks {}) (when c (show-app c.app)))
      :on-cancel #(set marks {})
      :keys {"s-m" #(let [c (selected)]
                      (when c
                        (let [pid (c.app:pid)
                              mark? (not (. marks pid))]
                          (tset marks pid (or mark? nil))
                          (set c.text (.. (if mark? "* " "") (c.app:name)))))
                      true)
             "s-u" #(do (set marks {})
                        (each [_ c (ipairs candidates)] (set c.text (c.app:name)))
                        true)}
      :actions {"s-d" (run-on "Quit" #($:kill))
                "s-D" (run-on "Force quit" #($:kill9))
                "s-h" (run-on "Hid" #($:hide))}})))

(fn quit-app []
  "Quit the frontmost app the way Cmd-Q does, so it may still ask to save,
  like kill-buffer."
  (match (frontmost)
    app (minibuffer.y-or-n-p (.. "Quit " (app:name) "?")
                             #(app:kill))))

(fn force-quit-app []
  "Terminate the frontmost app without letting it save, like Force Quit."
  (match (frontmost)
    app (minibuffer.y-or-n-p (.. "Force quit " (app:name) "? Unsaved changes are lost.")
                             #(app:kill9))))

(fn relaunch [app]
  "Quit APP and open it again once it has exited, like revert-buffer.
  Hammerspoon itself relaunches."
  (let [name (app:name)
        bundle (app:bundleID)]
    (if (= bundle hs.processInfo.bundleID) (hs.relaunch)
        (not bundle) (minibuffer.message (.. "Can't relaunch " name))
        (do
          (app:kill)
          (var waited 0)
          (when relaunch-timer (relaunch-timer:stop))
          (set relaunch-timer
               (hs.timer.doEvery
                0.25
                (fn []
                  (set waited (+ waited 0.25))
                  (if (app:isRunning)
                      (when (>= waited relaunch-timeout)
                        (relaunch-timer:stop)
                        (minibuffer.message (.. name " didn't quit")))
                      (do
                        (relaunch-timer:stop)
                        (hs.application.launchOrFocusByBundleID bundle))))))))))

(fn relaunch-app []
  "Quit the frontmost app and open it again once it has exited, after
  confirmation."
  (match (frontmost)
    app (minibuffer.y-or-n-p (.. "Relaunch " (app:name) "?")
                             #(relaunch app))))

{: init
 : windows
 : candidate
 : switch-to-window
 : next-window
 : previous-window
 : last-window
 : apps
 : show-app
 : switch-to-app
 : last-app
 : bury-app
 : new-window
 : list-apps
 : quit-app
 : force-quit-app
 : relaunch
 : relaunch-app}
