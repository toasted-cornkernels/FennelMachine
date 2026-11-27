;; `hs.window.orderedWindows` that doesn't stall on a hung app.
;;
;; It asks every running app for its windows, several times over, and an app
;; whose main thread is stuck makes each request wait out the accessibility
;; timeout (1.5 s here): one hung Emacs made it take 4.5 s.  Here only apps
;; with windows on screen are asked, after a probe with a short timeout, and
;; an app failing the probe is left out for a while, twice as long each time
;; it fails again, so a hung app costs a probe only now and then.

(local probe-timeout 0.1)
(local first-retry 30)
(local max-retry 900)

;; pid -> {:until when to probe it again :wait the current backoff}.
(local unresponsive {})

(fn responsive? [app]
  "Whether APP answers accessibility requests promptly."
  (let [pid (app:pid)
        now (hs.timer.secondsSinceEpoch)
        skipped (. unresponsive pid)]
    (if (and skipped (< now skipped.until))
        false
        (let [element (hs.axuielement.applicationElement app)
              started (hs.timer.absoluteTime)]
          (when element
            (pcall #(element:setTimeout probe-timeout))
            (pcall #(element:attributeValue :AXWindows)))
          (let [slow? (or (not element)
                          (>= (- (hs.timer.absoluteTime) started)
                              (* 0.9 probe-timeout 1e9)))]
            (tset unresponsive pid
                  (when slow?
                    (let [wait (if skipped
                                   (math.min max-retry (* 2 skipped.wait))
                                   first-retry)]
                      {:until (+ now wait) : wait})))
            (not slow?))))))

(fn ordered-windows [?own?]
  "Visible windows, front to back, of the apps that respond. With ?OWN?,
  Hammerspoon's own standard windows too, such as the console, but not its
  canvases; it has no Dock icon, so it is otherwise left out."
  (let [pids {}
        by-id {}
        own hs.processInfo.processID]
    (each [_ info (ipairs (or (hs.window.list false) []))]
      (tset pids info.kCGWindowOwnerPID true))
    (each [pid (pairs pids)]
      (let [app (hs.application.applicationForPID pid)]
        (if (= pid own)
            (when (and ?own? app)
              (each [_ win (ipairs (app:visibleWindows))]
                (when (= (win:subrole) :AXStandardWindow)
                  (tset by-id (or (win:id) -1) win))))
            (when (and app (> (app:kind) 0) (responsive? app))
              (each [_ win (ipairs (app:visibleWindows))]
                (tset by-id (or (win:id) -1) win))))))
    (icollect [_ id (ipairs (hs.window._orderedwinids))]
      (. by-id id))))

{: ordered-windows : responsive?}
