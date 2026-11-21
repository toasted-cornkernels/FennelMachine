;; Notification Center from the keyboard, through its accessibility tree.
;; A banner that peeks in (top right) can be buried (swiped away into
;; Notification Center) or cleared (removed for good).  Burying drags the
;; banner off to the right with synthetic mouse events, as a trackpad swipe
;; can't be synthesised; the pointer is put back right after.

(local minibuffer (require :lib.minibuffer))

(fn ax [element attribute]
  (: element :attributeValue attribute))

(fn find [element pred depth]
  "Depth-first search below ELEMENT for the first element satisfying PRED."
  (let [depth (or depth 0)]
    (if (pred element) element
        (< depth 14)
        (accumulate [found nil _ child (ipairs (or (ax element :AXChildren) []))
                     &until found]
          (find child pred (+ depth 1))))))

(fn find-all [element pred]
  (let [found []]
    (find element #(do (when (pred $1) (table.insert found $1)) false))
    found))

(fn window []
  "Notification Center's (screen-wide, transparent) window, if it is running."
  (let [app (hs.application.get :com.apple.notificationcenterui)]
    (when app
      (?. (ax (hs.axuielement.applicationElement app) :AXWindows) 1))))

(fn open? []
  (let [w (window)]
    (and w (find w #(= (ax $1 :AXIdentifier) :widget-editor-button)) true)))

(fn clock []
  "The menu bar clock, which toggles Notification Center."
  (let [app (hs.application.get :com.apple.controlcenter)]
    (when app
      (accumulate [item nil _ bar (ipairs (or (ax (hs.axuielement.applicationElement app)
                                                 :AXChildren)
                                             []))
                   &until item]
        (hs.fnutils.find (or (ax bar :AXChildren) [])
                         #(= (ax $1 :AXIdentifier) :com.apple.menuextra.clock))))))

(fn wait-for [get on-found on-timeout]
  "Call ON-FOUND with (GET)'s value once it is truthy, polling for up to 2 s."
  (var tries 0)
  (var timer nil)
  (set timer (hs.timer.doEvery 0.05
                               (fn []
                                 (set tries (+ tries 1))
                                 (let [value (get)]
                                   (when (or value (> tries 40))
                                     (timer:stop)
                                     (if value (on-found value)
                                         on-timeout (on-timeout))))))))

(fn toggle []
  "Show or hide Notification Center (as clicking the menu bar clock)."
  (let [c (clock)]
    (if c
        (: c :performAction :AXPress)
        (minibuffer.message "Can't find the clock in the menu bar"))))

(fn open []
  "Show Notification Center, or hide it when it is shown."
  (toggle))

(fn press-menu-item [title on-done]
  (wait-for #(let [w (window)]
               (and w (find w #(and (= (ax $1 :AXRole) :AXMenuItem)
                                    (or (= (ax $1 :AXTitle) title)
                                        (= (ax $1 :AXDescription) title))))))
            (fn [item]
              (: item :performAction :AXPress)
              (on-done))
            #(minibuffer.message (.. "Can't find \"" title "\""))))

(fn clear-all-now []
  (let [was-open? (open?)
        close #(when (and (not was-open?) (open?)) (toggle))]
    (when (not was-open?) (toggle))
    (wait-for #(let [w (and (open?) (window))]
                 (and w (find w #(= (ax $1 :AXIdentifier) :xmark))))
              (fn [button]
                (: button :performAction :AXShowMenu)
                (press-menu-item "Clear All Notifications"
                                 #(hs.timer.doAfter 0.5 close)))
              #(do (minibuffer.message "Nothing to clear")
                   (close)))))

(fn clear-all []
  "Clear every notification in Notification Center, after confirmation."
  (minibuffer.y-or-n-p "Clear all notifications?" clear-all-now))

(fn on-screen? [element]
  "Whether ELEMENT's top left corner is on a screen (not sliding in from
  beyond the edge)."
  (let [frame (ax element :AXFrame)]
    (and frame
         (hs.fnutils.some (hs.screen.allScreens)
                          #(: (hs.geometry.point frame.x frame.y) :inside
                              (: $1 :fullFrame))))))

(fn current []
  "The notification peeking in at the top right: the topmost banner, or the
  topmost alert (alerts stay until dismissed) when no banner is showing."
  (let [w (and (not (open?)) (window))
        top-of (fn [elements]
                 (table.sort elements #(< (. (ax $1 :AXFrame) :y)
                                          (. (ax $2 :AXFrame) :y)))
                 (. elements 1))
        of-kind (fn [subrole]
                  (find-all w #(and (= (ax $1 :AXSubrole) subrole)
                                    (on-screen? $1))))]
    (when w
      (or (top-of (of-kind :AXNotificationCenterBanner))
          (top-of (of-kind :AXNotificationCenterAlert))))))

(fn with-current [f]
  (if (open?)
      (minibuffer.message "Notification Center is open")
      (let [n (current)]
        (if n (f n) (minibuffer.message "No notification on screen")))))

(fn clear-current []
  "Remove the notification peeking in at the top right for good."
  (with-current
    (fn [n]
      (let [close (hs.fnutils.find (or (: n :actionNames) [])
                                   #(or (= $1 :AXClose)
                                        (= (string.match $1 "^Name:([^\n]*)") :Close)))]
        (if close
            (: n :performAction close)
            (minibuffer.message "This notification can't be closed"))))))

(fn drag-away [n]
  "Drag notification N off to the right, then put the pointer back."
  (let [frame (ax n :AXFrame)
        ;; Left of centre, clear of the close button and any action buttons.
        start {:x (+ frame.x (* frame.w 0.3)) :y (+ frame.y (/ frame.h 2))}
        saved (hs.mouse.absolutePosition)
        types hs.eventtap.event.types
        post #(: (hs.eventtap.event.newMouseEvent $1 $2) :post)
        steps 18
        distance 300]
    (post types.leftMouseDown start)
    (var step 0)
    (var timer nil)
    (set timer
         (hs.timer.doEvery 0.012
                           (fn []
                             (set step (+ step 1))
                             (let [at {:x (+ start.x (* distance (/ step steps)))
                                       :y start.y}]
                               (if (< step steps)
                                   (post types.leftMouseDragged at)
                                   (do (timer:stop)
                                       (post types.leftMouseUp at)
                                       (hs.timer.doAfter 0.05
                                                         #(hs.mouse.absolutePosition saved))))))))))

(fn settled [n on-settled]
  "Call ON-SETTLED once N has finished sliding in."
  (var last nil)
  (wait-for #(let [x (?. (ax n :AXFrame) :x)
                   still? (and x (= x last))]
               (set last x)
               still?)
            #(on-settled)
            #(on-settled)))

(fn bury-current []
  "Swipe the notification peeking in at the top right away into
  Notification Center, keeping it there."
  (with-current (fn [n] (settled n #(drag-away n)))))

{: open
 : clear-all
 : clear-current
 : bury-current}
