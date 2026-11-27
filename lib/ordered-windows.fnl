;; `hs.window.orderedWindows` that doesn't stall on a hung app.
;;
;; It asks every running app for its windows, several times over, and an app
;; whose main thread is stuck makes each request wait out the accessibility
;; timeout (1.5 s here): one hung Emacs made it take 4.5 s.  Here only apps
;; with windows on screen are asked, after a probe with a short timeout, and
;; an app failing the probe is left out for a while.

(local probe-timeout 0.2)
(local retry-after 30)

;; pid -> when to probe it again.
(local unresponsive {})

(fn responsive? [app]
  "Whether APP answers accessibility requests promptly."
  (let [pid (app:pid)
        now (hs.timer.secondsSinceEpoch)
        until (. unresponsive pid)]
    (if (and until (< now until))
        false
        (let [element (hs.axuielement.applicationElement app)
              started (hs.timer.absoluteTime)]
          (when element
            (pcall #(element:setTimeout probe-timeout))
            (pcall #(element:attributeValue :AXWindows)))
          (let [slow? (or (not element)
                          (>= (- (hs.timer.absoluteTime) started)
                              (* 0.9 probe-timeout 1e9)))]
            (tset unresponsive pid (when slow? (+ now retry-after)))
            (not slow?))))))

(fn ordered-windows []
  "Visible windows, front to back, of the apps that respond."
  (let [pids {}
        by-id {}]
    (each [_ info (ipairs (or (hs.window.list false) []))]
      (tset pids info.kCGWindowOwnerPID true))
    (each [pid (pairs pids)]
      (let [app (hs.application.applicationForPID pid)]
        (when (and app (> (app:kind) 0) (responsive? app))
          (each [_ win (ipairs (app:visibleWindows))]
            (tset by-id (or (win:id) -1) win)))))
    (icollect [_ id (ipairs (hs.window._orderedwinids))]
      (. by-id id))))

{: ordered-windows : responsive?}
