"Windows as Emacs buffers: switch to any window by name, cycle the windows
of the frontmost app, return to the last window"

(local minibuffer (require :lib.minibuffer))

;; A window filter fills in asynchronously, so one is started by init.
;; Titled standard windows: no panels or untitled helper windows, but
;; minimized windows (whose subrole turns into AXDialog) and those on other
;; Spaces are included.
(var filter nil)

(fn init []
  (when (not filter)
    (set filter (: (hs.window.filter.new false) :setDefaultFilter
                   {:allowRoles [:AXStandardWindow :AXDialog]
                    :allowTitles 1})))
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

{: init
 : windows
 : switch-to-window
 : next-window
 : previous-window
 : last-window}
