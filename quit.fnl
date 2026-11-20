"Quit, force quit or relaunch the frontmost app, after confirmation"

(local minibuffer (require :lib.minibuffer))

(local relaunch-timeout 10)

;; Kept here so that the timer isn't garbage collected while it waits.
(var relaunch-timer nil)

(fn frontmost []
  (let [app (hs.application.frontmostApplication)]
    (if app app (do (minibuffer.message "No frontmost app") nil))))

(fn quit-app []
  "Quit the frontmost app the way Cmd-Q does, so it may still ask to save."
  (match (frontmost)
    app (minibuffer.y-or-n-p (.. "Quit " (app:name) "?")
                             #(app:kill))))

(fn force-quit-app []
  "Terminate the frontmost app without letting it save, like Force Quit."
  (match (frontmost)
    app (minibuffer.y-or-n-p (.. "Force quit " (app:name) "? Unsaved changes are lost.")
                             #(app:kill9))))

(fn relaunch-app []
  "Quit the frontmost app and open it again once it has exited."
  (match (frontmost)
    app
    (let [name (app:name)
          bundle (app:bundleID)]
      (if (= bundle hs.processInfo.bundleID) (hs.relaunch)
          (not bundle) (minibuffer.message (.. "Can't relaunch " name))
          (minibuffer.y-or-n-p
           (.. "Relaunch " name "?")
           (fn []
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
                           (hs.application.launchOrFocusByBundleID bundle))))))))))))

{: quit-app
 : force-quit-app
 : relaunch-app}
