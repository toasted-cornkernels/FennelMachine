"
Split View: two windows side by side in a full-screen Space of their own, and
the separator between them moved by keyboard.

`pair` reads the left window and then the right one in the minibuffer, like
switch-to-buffer. macOS has no API to pair windows, so it goes the way a
user would: it tiles the left one with Window › Full Screen Tile › Left of
Screen, which shows the other windows of its desktop as thumbnails on the
right, and clicks the right one's. The thumbnails are found by asking
Accessibility which window is at each point of that half, as each stands for
its window there.

Only windows of the left one's desktop are offered, so the right one is
brought there first: out of full screen, unminimized, and, from another
desktop, held by its title bar while switching desktops, the way one drags a
window across them (hs.spaces.moveWindowToSpace no longer works).

`move-separator-left` / `-right` resize the left window of the pair, the
right one following; outside Split View, they move the edge the focused
window shares with a window beside it.
"

(local minibuffer (require :lib.minibuffer))
(local {: ordered-windows} (require :lib.ordered-windows))
(local buffers (require :buffers))
(local spaces (require :spaces))

(local menu-path ["Window" "Full Screen Tile" "Left of Screen"])
;; The same item in the green button's menu, for apps without the former.
(local button-item "Left of Screen")
;; The separator moves by this share of the screen's width.
(local separator-step (/ 1 32))
;; How far apart two windows' edges may be to count as shared; Split View
;; keeps a gap of a dozen points for its separator.
(local edge-slack 24)
(local scan-step 32)
(local poll-interval 0.1)

;; Kept here so that pending timers aren't garbage collected.
(var poller nil)

(fn stop []
  (when poller
    (poller:stop)
    (set poller nil)))

(fn wait-until [ready? timeout on-ready on-timeout]
  "Call ON-READY once READY? is true, checking every poll-interval, or
  ON-TIMEOUT after TIMEOUT seconds."
  (stop)
  (let [started (hs.timer.secondsSinceEpoch)]
    (set poller
         (hs.timer.doEvery
          poll-interval
          (fn []
            (if (ready?)
                (do (stop) (on-ready))
                (> (- (hs.timer.secondsSinceEpoch) started) timeout)
                (do (stop) (when on-timeout (on-timeout)))))))))

(fn after [seconds f]
  (stop)
  (set poller (hs.timer.doAfter seconds #(do (set poller nil) (f)))))

(fn fail [text]
  (stop)
  (minibuffer.message text))

(fn same? [a b]
  (and a b (= (a:id) (b:id))))

(fn return-pointer [point]
  "Move the pointer back to POINT with a real mouse-moved event: warping it
  there leaves the system believing it's where it was, e.g. still at the top
  edge, keeping the menu bar and a full-screen title bar revealed."
  (: (hs.eventtap.event.newMouseEvent hs.eventtap.event.types.mouseMoved point)
     :post))

(fn name-of [win]
  (let [title (win:title)]
    (if (= title "") (or (-?> (win:application) (: :name)) "window") title)))

(fn space-of [win]
  (. (or (hs.spaces.windowSpaces win) []) 1))

(fn user-space? [space]
  (and space (= (hs.spaces.spaceType space) :user)))

;; Getting the windows ready ========================
;; ==================================================

(fn leave-full-screen [win k]
  "Take WIN out of full screen, from its own Space, then call K."
  (win:focus)
  (wait-until #(= (hs.spaces.focusedSpace) (space-of win)) 3
              (fn []
                (win:setFullScreen false)
                (wait-until #(and (not (win:isFullScreen))
                                  (user-space? (space-of win)))
                            5
                            ;; Let the animation end.
                            #(after 0.5 k)
                            #(fail (.. "Couldn't take " (name-of win)
                                       " out of full screen"))))
              #(fail (.. "Couldn't get to " (name-of win)))))

(fn green-button [win]
  (let [ax (hs.axuielement.windowElement win)]
    (and ax (or ax.AXFullScreenButton ax.AXZoomButton))))

(fn emacsclient []
  (accumulate [found nil _ path (ipairs ["/usr/local/bin/emacsclient"
                                         "/opt/homebrew/bin/emacsclient"
                                         (.. (os.getenv :HOME)
                                             "/.nix-profile/bin/emacsclient")
                                         "/run/current-system/sw/bin/emacsclient"])
               &until found]
    (when (hs.fs.attributes path) path)))

(fn elisp-string [text]
  (.. "\"" (-> text (: :gsub "\\" "\\\\") (: :gsub "\"" "\\\"")) "\""))

(fn leave-emacs-full-screen [win k]
  "Turn off the Emacs frame WIN's own full screen (`toggle-frame-fullscreen'
  on the Mac port), which has no title bar to tile it by. Emacs makes a new
  window for the frame, so call K with that one."
  (let [title (win:title)
        app (win:application)
        client (emacsclient)
        form (.. "(let ((f (seq-find (lambda (f) (member " (elisp-string title)
                 " (list (frame-parameter f 'title) (frame-parameter f 'name))))"
                 " (frame-list))))"
                 " (when (and f (frame-parameter f 'fullscreen))"
                 " (set-frame-parameter f 'fullscreen nil) t))")
        pid (app:pid)
        ;; Every Space's windows, unlike app:allWindows.
        new #(accumulate [found nil _ w (ipairs (buffers.windows)) &until found]
               (when (and (= (: (w:application) :pid) pid) (= (w:title) title)
                          (green-button w))
                 w))]
    (var found nil)
    (if (not client)
        (fail "Couldn't find emacsclient")
        ;; The new window opens in the active Space, so go to the frame's.
        (do
          (win:focus)
          (wait-until
           #(= (hs.spaces.focusedSpace) (space-of win)) 3
           (fn []
             (: (hs.task.new client
                             (fn [code]
                               (if (= code 0)
                                   (wait-until #(do (set found (new)) found) 3
                                               #(after 0.3 #(k found))
                                               #(fail (.. "Couldn't take " title
                                                          " out of full screen")))
                                   (fail "emacsclient failed; is the server running?")))
                             ["-e" form])
                :start))
           #(fail (.. "Couldn't get to " title)))))))

(fn show [win k]
  "Unhide, unminimize and take WIN out of full screen, then call K with it
  (a new window, when an app had to remake it)."
  (let [app (win:application)]
    (when (and app (app:isHidden)) (app:unhide))
    (when (win:isMinimized) (win:unminimize))
    (if (win:isFullScreen)
        (leave-full-screen win #(k win))
        (wait-until #(and (not (win:isMinimized)) (user-space? (space-of win)))
                    3
                    #(if (green-button win)
                         (k win)
                         (= (-?> app (: :bundleID)) :org.gnu.Emacs)
                         (leave-emacs-full-screen win k)
                         (fail (.. (name-of win) " has no title bar to tile it by")))
                    #(fail (.. "Couldn't show " (name-of win)))))))

(fn grip [win]
  "A point of WIN's title bar to drag it by: one where Accessibility finds
  the window itself, its toolbar or its title, rather than a control."
  (let [f (win:frame)]
    (var found nil)
    (each [_ dy (ipairs [6 12 18]) &until found]
      (for [x (+ f.x 90) (- (+ f.x f.w) 20) 16 &until found]
        (let [e (hs.axuielement.systemElementAtPosition x (+ f.y dy))
              role (and e e.AXRole)]
          (when (and (or (= role :AXWindow) (= role :AXToolbar)
                         (= role :AXStaticText))
                     (same? (-?> (if (= role :AXWindow) e e.AXWindow)
                                 (: :asHSWindow))
                            win))
            (set found {: x :y (+ f.y dy)})))))
    (or found {:x (+ f.x (/ f.w 2)) :y (+ f.y 6)})))

(fn bring-to-space [win space k]
  "Move WIN to the desktop SPACE: hold it by its title bar, switch to SPACE,
  let go. Then call K."
  (win:focus)
  (wait-until
   #(= (hs.spaces.focusedSpace) (space-of win)) 3
   (fn []
     (let [types hs.eventtap.event.types
           mouse (hs.mouse.absolutePosition)
           at (grip win)
           dragged {:x (+ at.x 1) :y at.y}]
       (: (hs.eventtap.event.newMouseEvent types.leftMouseDown at) :post)
       (hs.timer.usleep 100000)
       (: (hs.eventtap.event.newMouseEvent types.leftMouseDragged dragged) :post)
       (hs.timer.usleep 100000)
       (spaces.goto-space space)
       (wait-until #(= (hs.spaces.focusedSpace) space) 3
                   (fn []
                     (after 0.4
                            (fn []
                              (: (hs.eventtap.event.newMouseEvent types.leftMouseUp
                                                                  dragged)
                                 :post)
                              (return-pointer mouse)
                              (wait-until #(= (space-of win) space) 1.5 k
                                          #(fail (.. "Couldn't bring " (name-of win)
                                                     " to this desktop"))))))
                   (fn []
                     (: (hs.eventtap.event.newMouseEvent types.leftMouseUp dragged)
                        :post)
                     (return-pointer mouse)
                     (fail "Couldn't switch desktops")))))
   #(fail (.. "Couldn't get to " (name-of win)))))

;; Pairing ==========================================
;; ==================================================

(fn thumbnail [win screen]
  "Where WIN's thumbnail is in the right half of SCREEN while macOS offers
  windows to tile beside another: the hit nearest the middle of its hits."
  (let [f (screen:fullFrame)
        hits []]
    (for [y (+ f.y (/ scan-step 2)) (+ f.y f.h) scan-step]
      (for [x (+ f.x (/ f.w 2) (/ scan-step 2)) (+ f.x f.w) scan-step]
        (let [e (hs.axuielement.systemElementAtPosition x y)
              w (and e (if (= e.AXRole :AXWindow) e e.AXWindow))]
          (when (same? (and w (w:asHSWindow)) win)
            (table.insert hits {: x : y})))))
    (when (< 0 (length hits))
      (let [cx (/ (accumulate [s 0 _ p (ipairs hits)] (+ s p.x)) (length hits))
            cy (/ (accumulate [s 0 _ p (ipairs hits)] (+ s p.y)) (length hits))
            distance #(+ (math.abs (- $.x cx)) (math.abs (- $.y cy)))]
        (table.sort hits #(< (distance $1) (distance $2)))
        (. hits 1)))))

(fn paired? [left right]
  (and (left:isFullScreen) (right:isFullScreen)
       (= (space-of left) (space-of right))
       (< (. (left:frame) :x) (. (right:frame) :x))))

(fn click-thumbnail [left right attempts]
  (var spot nil)
  (wait-until
   #(do (set spot (thumbnail right (left:screen))) spot) 4
   (fn []
     (let [mouse (hs.mouse.absolutePosition)]
       (hs.eventtap.leftClick spot)
       (return-pointer mouse))
     (wait-until #(paired? left right) 3
                 (fn []
                   (left:focus)
                   (minibuffer.message (.. "Paired " (name-of left) " | "
                                           (name-of right))))
                 #(if (< 1 attempts)
                      (click-thumbnail left right (- attempts 1))
                      (do (hs.eventtap.keyStroke [] :escape 0)
                          (fail "Couldn't pair them")))))
   (fn []
     (hs.eventtap.keyStroke [] :escape 0)
     (fail (.. "macOS didn't offer " (name-of right))))))

(fn find-item [e title depth]
  (if (and (= e.AXRole :AXMenuItem) (= e.AXTitle title))
      e
      (< 0 depth)
      (accumulate [found nil _ c (ipairs (or e.AXChildren [])) &until found]
        (find-item c title (- depth 1)))))

(fn menu-below [button]
  "The menu a button just showed below itself, by hit-testing there: it isn't
  always among the button's children."
  (let [p button.AXPosition
        hit (accumulate [found nil _ dy (ipairs [24 40 56 72]) &until found]
              (let [e (and p (hs.axuielement.systemElementAtPosition
                              (+ p.x 4) (+ p.y dy)))]
                (when (and e (= e.AXRole :AXMenuItem)) e)))]
    (var menu nil)
    (var e hit)
    (while e
      (when (= e.AXRole :AXMenu) (set menu e))
      (set e e.AXParent))
    menu))

(fn tile-left [win k]
  "Tile WIN to the left of a new full-screen Space, through the Window menu
  or, failing that, its green button's menu. Then call K."
  (if (: (win:application) :selectMenuItem menu-path)
      (k)
      (let [button (green-button win)
            cannot #(fail (.. (name-of win) " can't be tiled in full screen"))]
        (var item nil)
        (if (not button)
            (cannot)
            (do
              ;; It reports failure even as the menu opens.
              (button:performAction :AXShowMenu)
              (wait-until #(do (set item (or (find-item button button-item 6)
                                             (-?> (menu-below button)
                                                  (find-item button-item 3))))
                               item)
                          1.5
                          #(do (item:performAction :AXPress) (k))
                          #(do (hs.eventtap.keyStroke [] :escape 0) (cannot))))))))

(fn tile [left right]
  "Tile LEFT to the left in full screen and pick RIGHT for the other side."
  ;; Focused last but one, it comes first among the thumbnails.
  (right:focus)
  (left:focus)
  (wait-until
   #(same? (hs.window.focusedWindow) left) 2
   #(tile-left left #(click-thumbnail left right 2))
   #(fail (.. "Couldn't focus " (name-of left)))))

(fn pair-windows [left right]
  "Put LEFT and RIGHT side by side in Split View, a full-screen Space of their
  own."
  (show
   left
   (fn [left]
     (show
      right
      (fn [right]
        (let [space (space-of left)]
          (if (= (space-of right) space)
              (tile left right)
              (not= (: (left:screen) :id) (: (right:screen) :id))
              (fail (.. (name-of right) " is on another screen"))
              (bring-to-space right space #(tile left right)))))))))

(fn candidates [wins]
  (icollect [_ w (ipairs wins)] (buffers.candidate w)))

(fn pair []
  "Read two windows, left and right, and pair them in Split View."
  (let [current (hs.window.focusedWindow)
        wins (buffers.windows)
        ;; The focused window first: it's the likeliest to pair.
        ordered (icollect [_ w (ipairs wins)] (when (not (same? w current)) w))]
    (when current (table.insert ordered 1 current))
    (if (< (length ordered) 2)
        (minibuffer.message "Need two windows to pair")
        (minibuffer.completing-read
         "Left window: " (candidates ordered)
         {:on-accept
          (fn [a]
            (when a
              (minibuffer.completing-read
               (.. "Right of " a.text ": ")
               (candidates (icollect [_ w (ipairs ordered)]
                             (when (not (same? w a.window)) w)))
               {:on-accept (fn [b]
                             (when b (pair-windows a.window b.window)))})))}))))

;; The separator ====================================
;; ==================================================

(fn vertical-overlap? [a b]
  (and (< a.y (+ b.y b.h)) (< b.y (+ a.y a.h))))

(fn neighbour-pair [win]
  "WIN and the window sharing its left or right edge, left one first: its
  Split View partner, or a window tiled beside it."
  (let [f (win:frame)
        others (icollect [_ w (ipairs (ordered-windows))]
                 (when (and (not (same? w win)) (w:isStandard) (w:isVisible)
                            (= (: (w:screen) :id) (: (win:screen) :id))
                            (vertical-overlap? f (w:frame)))
                   w))
        gap #(let [g ($:frame)]
               (if (< g.x f.x)
                   (math.abs (- f.x (+ g.x g.w)))
                   (math.abs (- g.x (+ f.x f.w)))))
        beside (icollect [_ w (ipairs others)]
                 (when (<= (gap w) edge-slack) w))]
    (table.sort beside #(< (gap $1) (gap $2)))
    (case (. beside 1)
      other (if (< (. (other:frame) :x) f.x) [other win] [win other]))))

(fn move-separator [direction]
  (let [win (hs.window.focusedWindow)
        pair (and win (neighbour-pair win))]
    (if (not pair)
        (minibuffer.message "No window beside this one")
        (let [[left right] pair
              lf (left:frame)
              rf (right:frame)
              dx (* direction (math.floor (* separator-step
                                             (. (: (left:screen) :frame) :w))))]
          (if (and (left:isFullScreen) (right:isFullScreen))
              ;; Split View moves the right one along.
              (left:setSize {:w (+ lf.w dx) :h lf.h})
              (do
                ((. (require :windows) :remember-frames) [left right])
                (left:setFrame {:x lf.x :y lf.y :w (+ lf.w dx) :h lf.h})
                (right:setFrame {:x (+ rf.x dx) :y rf.y :w (- rf.w dx) :h rf.h})))
          (when (= (. (left:frame) :w) lf.w)
            (minibuffer.message "Can't move the separator further"))))))

(fn move-separator-left [] (move-separator -1))
(fn move-separator-right [] (move-separator 1))

;; Swapping ==========================================
;; ==================================================

(local title-bar-roles {:AXGroup true :AXToolbar true :AXStaticText true
                        :AXWindow true})

(fn title-bar-point [win x]
  "Where WIN's title bar is at X, or nil until it's revealed: in full screen
  it slides down under the menu bar, so look below that."
  (let [top (. (win:frame) :y)
        first (hs.axuielement.systemElementAtPosition x (+ top 2))]
    (var found nil)
    (when (and first (first.AXRole:match "^AXMenuBar"))
      ;; Scan from the bar's bottom: until it has slid in, the window
      ;; itself answers up there.
      (for [y (+ top (math.max 24 (or (-?> first.AXFrame (. :h)) 0))) (+ top 120) 4
            &until found]
        (let [e (hs.axuielement.systemElementAtPosition x y)
              role (and e e.AXRole)]
          (when (and role (not (role:match "^AXMenu")) (. title-bar-roles role)
                     (same? (-?> (if (= role :AXWindow) e e.AXWindow)
                                 (: :asHSWindow))
                            win))
            ;; A little inside it: its top edge borders the menu bar.
            (set found {: x :y (+ y 6)})))))
    found))

(fn drag [from to k]
  "Drag the mouse with the left button from FROM to TO, then call K."
  (let [types hs.eventtap.event.types
        post #(: (hs.eventtap.event.newMouseEvent $1 $2) :post)
        steps 30]
    ;; Glide down from the top edge, as jumping off it can hide the
    ;; menu bar and the title bar with it.
    (for [y (math.max 0 (- from.y 30)) from.y 2]
      (post types.mouseMoved {:x from.x : y})
      (hs.timer.usleep 10000))
    (post types.mouseMoved from)
    (hs.timer.usleep 200000)
    (post types.leftMouseDown from)
    (hs.timer.usleep 300000)
    (for [i 1 steps]
      (post types.leftMouseDragged
            {:x (+ from.x (* (- to.x from.x) (/ i steps)))
             :y (+ from.y (* (- to.y from.y) (/ i steps)))})
      (hs.timer.usleep 30000))
    ;; Let Split View make room before dropping.
    (after 1 #(do (post types.leftMouseUp to) (k)))))

(fn swap []
  "Swap the two windows of the focused Split View, the way a user would:
  drag the left one's title bar onto the right one."
  (let [win (hs.window.focusedWindow)
        pair (and win (win:isFullScreen) (neighbour-pair win))]
    (if (not (and pair (: (. pair 1) :isFullScreen) (: (. pair 2) :isFullScreen)))
        (minibuffer.message "Not in Split View")
        (let [[left right] pair
              mouse (hs.mouse.absolutePosition)
              lf (left:frame)
              rf (right:frame)
              x (+ lf.x (/ lf.w 2))
              done #(do (return-pointer mouse) (win:focus))]
          (var at nil)
          ;; Reveal the menu bar and, under it, the title bar.
          (: (hs.eventtap.event.newMouseEvent hs.eventtap.event.types.mouseMoved
                                              {: x :y lf.y})
             :post)
          (wait-until
           #(do (set at (title-bar-point left x)) at) 3
           ;; Accessibility has it before it has slid into place.
           #(after 1
                   #(drag at {:x (+ rf.x (/ rf.w 2)) :y (+ at.y 200)}
                          #(wait-until #(> (. (left:frame) :x) (. (right:frame) :x)) 3
                                       (fn []
                                         (done)
                                         (minibuffer.message
                                          (.. "Swapped " (name-of right) " | "
                                              (name-of left))))
                                       #(do (done) (fail "Couldn't swap them")))))
           #(do (done) (fail "Couldn't find the title bar")))))))

{: pair
 : pair-windows
 : swap
 : move-separator-left
 : move-separator-right}
