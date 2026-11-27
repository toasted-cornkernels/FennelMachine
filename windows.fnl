(local {: filter
        : get-in
        : count
        : concat
        : contains?
        : map
        : for-each
        : split} (require :lib.functional))

(local {: global-filter} (require :lib.utils))
(local minibuffer (require :lib.minibuffer))
(local {: ordered-windows} (require :lib.ordered-windows))
(local {: atom : deref : swap! : reset!} (require :lib.atom))

(require-macros :lib.advice.macros)

;; History ==========================================
;; ==================================================

;; Like winner-mode: each step holds the frames some windows had before a
;; change, so commands that move several windows undo in one step.
(local history {:undo [] :redo []})
(local history-limit 50)

(fn snapshot [wins]
  (icollect [_ w (ipairs wins)]
    (when (w:id) {:id (w:id) :frame (w:frame)})))

(fn same-step? [a b]
  (and a b (= (length a) (length b))
       (accumulate [same true i e (ipairs a)]
         (and same (= e.id (. b i :id)) (= e.frame (. b i :frame))))))

(fn history.push [self wins]
  "Record the frames of WINS (the focused window by default) before a change."
  (let [step (snapshot (or wins [(hs.window.focusedWindow)]))]
    (when (and (< 0 (length step))
               (not (same-step? step (. self.undo (length self.undo)))))
      (table.insert self.undo step)
      (when (< history-limit (length self.undo))
        (table.remove self.undo 1)))
    (set self.redo [])))

(fn restore-step [from to]
  "Move the last step of FROM back into place, recording the frames it
  replaces in TO. Returns false when FROM is empty."
  (let [step (table.remove from)]
    (if (not step)
        false
        (let [back []]
          (each [_ e (ipairs step)]
            (let [w (hs.window.get e.id)]
              (when w
                (table.insert back {:id e.id :frame (w:frame)})
                (w:setFrame e.frame))))
          (table.insert to back)
          true))))

(fn history.pop [self]
  "Undo the last window change."
  (if (restore-step self.undo self.redo)
      (minibuffer.message (.. "Undo" (if (< 0 (length self.undo))
                                         (.. " (" (length self.undo) " more)")
                                         "")))
      (minibuffer.message "Nothing to undo")))

(fn history.unpop [self]
  "Redo the last undone window change."
  (if (restore-step self.redo self.undo)
      (minibuffer.message "Redo")
      (minibuffer.message "Nothing to redo")))

(fn remember-frames [wins]
  "Record the frames of WINS so that the next change to them can be undone."
  (: history :push wins))

(fn undo []
  (: history :pop))

(fn redo []
  (: history :unpop))

;; My Addition!! ====================================
;; ==================================================

;; Each adjustment takes the focused window's frame and its screen's
;; usable frame (excluding menu bar and Dock) and returns the target frame.

(fn bar-settings []
  "Whether the menu bar and the Dock hide, and the Dock's side. Asked of
  cfprefsd through defaults: the plist files lag behind."
  (let [(out) (hs.execute
               (.. "/usr/bin/defaults read -g _HIHideMenuBar 2>/dev/null; echo ';'; "
                   "/usr/bin/defaults read com.apple.dock autohide 2>/dev/null; echo ';'; "
                   "/usr/bin/defaults read com.apple.dock orientation 2>/dev/null"))
        [menu dock side] (icollect [field (string.gmatch (.. out ";") "([^;]*);")]
                           (field:match "^%s*(.-)%s*$"))]
    {:menu-hidden? (= menu "1")
     :dock-hidden? (= dock "1")
     :dock-side (if (or (= side nil) (= side "")) :bottom side)}))

(fn menu-bar-height []
  (or (-?> (hs.application.frontmostApplication)
           (hs.axuielement.applicationElement)
           (. :AXMenuBar)
           (. :AXFrame)
           (. :h))
      30))

(fn usable-frame [screen]
  "SCREEN's frame minus the menu bar and the Dock where they aren't hidden.
  Hammerspoon's screen:frame() can keep the insets of a menu bar or Dock that
  has since been set to hide (or shown again), so go by the settings."
  (let [full (screen:fullFrame)
        cached (screen:frame)
        {: menu-hidden? : dock-hidden? :dock-side side} (bar-settings)
        top (if menu-hidden? full.y
                (> cached.y full.y) cached.y
                (+ full.y (menu-bar-height)))
        left (if (and dock-hidden? (= side :left)) full.x cached.x)
        right (if (and dock-hidden? (= side :right))
                  (+ full.x full.w)
                  (+ cached.x cached.w))
        bottom (if (and dock-hidden? (= side :bottom))
                   (+ full.y full.h)
                   (+ cached.y cached.h))]
    (hs.geometry.rect left top (- right left) (- bottom top))))

(fn nudge [dx dy dw dh]
  (fn [window-frame]
    (set window-frame.x (+ window-frame.x dx))
    (set window-frame.y (+ window-frame.y dy))
    (set window-frame.w (+ window-frame.w dw))
    (set window-frame.h (+ window-frame.h dh))
    window-frame))

(fn center-enlarge [rate]
  (fn [window-frame screen-frame]
    (set window-frame.x (+ screen-frame.x (* screen-frame.w (- 1 rate) 0.5)))
    (set window-frame.y (+ screen-frame.y (* screen-frame.h (- 1 rate) 0.5)))
    (set window-frame.w (* screen-frame.w rate))
    (set window-frame.h (* screen-frame.h rate))
    window-frame))

(fn unit-rect [[ux uy uw uh]]
  (fn [window-frame screen-frame]
    (set window-frame.x (+ screen-frame.x (* screen-frame.w ux)))
    (set window-frame.y (+ screen-frame.y (* screen-frame.h uy)))
    (set window-frame.w (* screen-frame.w uw))
    (set window-frame.h (* screen-frame.h uh))
    window-frame))

(local window-adjustments
       {:top-border-up (nudge 0 -10 0 10)
        :top-border-down (nudge 0 10 0 -10)
        :left-border-left (nudge -10 0 10 0)
        :left-border-right (nudge 10 0 -10 0)
        :bottom-border-down (nudge 0 0 0 10)
        :bottom-border-up (nudge 0 0 0 -10)
        :right-border-left (nudge 0 0 -10 0)
        :right-border-right (nudge 0 0 10 0)
        :move-up (nudge 0 -10 0 0)
        :move-down (nudge 0 10 0 0)
        :move-left (nudge -10 0 0 0)
        :move-right (nudge 10 0 0 0)
        :center-enlarge-with-rate (center-enlarge 0.85)
        :resize-half-left (unit-rect [0 0 0.5 1])
        :resize-half-right (unit-rect [0.5 0 0.5 1])
        :resize-half-top (unit-rect [0 0 1 0.5])
        :resize-half-bottom (unit-rect [0 0.5 1 0.5])})

(fn target-frame [window window-adjustment]
  (window-adjustment (window:frame) (usable-frame (window:screen))))

(fn apply-once [window-adjustment]
  "Apply `window-adjustment` to the focused window a single time."
  (let [window (hs.window.focusedWindow)]
    (when window
      (: history :push)
      (window:setFrame (target-frame window window-adjustment)))))

;; Do it repeatedly =================================
;; ==================================================

(fn same-frame? [a b]
  (and (<= (math.abs (- a.x b.x)) 1)
       (<= (math.abs (- a.y b.y)) 1)
       (<= (math.abs (- a.w b.w)) 1)
       (<= (math.abs (- a.h b.h)) 1)))

(fn near? [a b]
  (<= (math.abs (- a b)) 1))

(fn keep-anchors [window before target]
  "Where TARGET moves one edge of the window and leaves the opposite one
  where it was BEFORE, keep that edge in place even if the app refused the
  new size: dragging a border of a window at its minimum size must not slide
  the window."
  (let [now (window:frame)
        x (if (and (not (near? target.x before.x))
                   (near? (+ target.x target.w) (+ before.x before.w)))
              (- (+ before.x before.w) now.w)
              now.x)
        y (if (and (not (near? target.y before.y))
                   (near? (+ target.y target.h) (+ before.y before.h)))
              (- (+ before.y before.h) now.h)
              now.y)]
    (when (not (and (near? x now.x) (near? y now.y)))
      (window:setTopLeft {: x : y}))))

(fn repeat-apply [window-adjustment]
  "Repeatedly do `window-adjustment` until its position stabilizes.
  The target is computed once, then re-applied (without animation) while the
  window is neither at the target nor stuck where the previous attempt left
  it. Some apps clamp position against their old size, so one setFrame can
  land short; apps that snap to a grid stop changing and end the loop."
  (let [window (hs.window.focusedWindow)]
    (when window
      (: history :push)
      (let [before (window:frame)
            target (target-frame window window-adjustment)]
        (var previous nil)
        (var tries 0)
        (while (and (< tries 5)
                    (not (same-frame? (window:frame) target))
                    (not (and previous (same-frame? (window:frame) previous))))
          (set previous (window:frame))
          (window:setFrame target 0)
          (set tries (+ tries 1)))
        (keep-anchors window before target)))))

(local window-adjusters
       (collect [name window-adjustment (pairs window-adjustments)
                 &into (collect [name window-adjustment (pairs window-adjustments)]
                         (.. name :-repeatedly)
                         #(repeat-apply window-adjustment))]
         name #(apply-once window-adjustment)))

;; Shared Functions =================================
;; ==================================================

(fn maximize-window-frame []
  (: history :push)
  (: (hs.window.focusedWindow) :maximize 0))

(fn minimize-window-frame []
  (: (hs.window.focusedWindow) :minimize))

(fn position-window-center [ratio-str window screen]
  "Takes the center-ratio key from config, or default value if not
   provided, and the window center-window-frame was called with,
   and the current screen.  Should calculate the centered
   dimensions of the target window using the ratio values This
   function is advisable."
  (let [frame (: screen :fullFrame)
        [w-percent h-percent] (split ":" ratio-str)
        w-percent (/ (tonumber w-percent) 100)
        h-percent (/ (tonumber h-percent) 100)
        update {:w (* w-percent frame.w) :h (* h-percent frame.h) :x 0 :y 0}]
    (doto window
      (: :setFrameInScreenBounds update)
      (: :centerOnScreen))))

(fn center-window-frame []
  (: history :push)
  (let [win (hs.window.focusedWindow)
        prev-duration hs.window.animationDuration
        config (get-config)
        ratio (or config.modules.windows.center-ratio "80:50")
        screen (hs.screen.primaryScreen)]
    (tset hs.window :animationDuration 0)
    (position-window-center ratio win screen)
    (tset hs.window :animationDuration prev-duration)))

(fn activate-app [app-name]
  (hs.application.launchOrFocus app-name)
  (let [app (hs.application.find app-name)]
    (when app
      (: app :activate)
      (: app :unhide))))

(fn set-mouse-cursor-at [app-title]
  (let [sf (: (: (hs.application.find app-title) :focusedWindow) :frame)
        desired-point (hs.geometry.point (- (+ sf._x sf._w) (/ sf._w 2))
                                         (- (+ sf._y sf._h) (/ sf._h 2)))]
    (hs.mouse.setAbsolutePosition desired-point)))

(fn show-grid []
  (: history :push)
  (hs.grid.show))

(fn jump-to-last-window []
  (-> (global-filter)
      (: :getWindows hs.window.filter.sortByFocusedLast)
      (. 2)
      (: :focus)))

;; Jumping Windows ==================================
;; ==================================================

(fn jump-window [arrow]
  "Focus the nearest window in the direction of ARROW (:h :j :k :l), like
  Emacs' windmove. Prefers the frontmost of the candidates."
  (let [dir {:h :West :j :South :k :North :l :East}
        names {:h "left" :j "below" :k "above" :l "right"}
        win (hs.window.focusedWindow)]
    (when (not (and win (: win (.. :focusWindow (. dir arrow)) nil true true)))
      (minibuffer.message (.. "No window " (. names arrow))))))

(fn jump-window-left []
  (jump-window :h))

(fn jump-window-below []
  (jump-window :j))

(fn jump-window-above []
  (jump-window :k))

(fn jump-window-right []
  (jump-window :l))

;; Emacs-style window commands =====================
;; ==================================================

(fn screen-windows [screen]
  "The visible standard windows on SCREEN in the current Space, ordered left
  to right, then top to bottom."
  (let [wins (icollect [_ w (ipairs (ordered-windows))]
               (when (and (w:isStandard) (w:isVisible)
                          (= (: (w:screen) :id) (screen:id)))
                 w))]
    (table.sort wins #(let [a ($1:frame) b ($2:frame)]
                        (if (= a.x b.x) (< a.y b.y) (< a.x b.x))))
    wins))

(fn current-screen []
  (let [win (hs.window.focusedWindow)]
    (or (and win (win:screen)) (hs.screen.mainScreen))))

(fn close-window []
  "Close the focused window, like delete-window."
  (let [win (hs.window.focusedWindow)]
    (if (not win) (minibuffer.message "No window to close")
        (not (win:close)) (minibuffer.message (.. "Can't close " (win:title))))))

(fn toggle-fullscreen []
  (let [win (hs.window.focusedWindow)]
    (if win (win:toggleFullScreen) (minibuffer.message "No window"))))

(fn tile-windows []
  "Give the visible windows of the focused screen equal space, like
  balance-windows: side by side up to three, else in a grid."
  (let [screen (current-screen)
        wins (screen-windows screen)
        n (length wins)]
    (if (= n 0)
        (minibuffer.message "No windows to tile")
        (let [sf (usable-frame screen)
              cols (if (<= n 3) n (math.ceil (math.sqrt n)))
              rows (math.ceil (/ n cols))
              h (/ sf.h rows)]
          (: history :push wins)
          (each [i w (ipairs wins)]
            (let [row (// (- i 1) cols)
                  col (% (- i 1) cols)
                  ;; The last row spreads its windows over the whole width.
                  in-row (if (= row (- rows 1)) (- n (* row cols)) cols)
                  width (/ sf.w in-row)]
              (w:setFrame {:x (+ sf.x (* col width)) :y (+ sf.y (* row h))
                           :w width :h h})))))))

(fn split-with-last [side-by-side?]
  "Put the focused window and the one focused before it on the focused
  window's screen, side by side or one above the other."
  (let [wins (: hs.window.filter.defaultCurrentSpace :getWindows
                hs.window.filter.sortByFocusedLast)
        a (hs.window.focusedWindow)
        b (accumulate [found nil _ w (ipairs wins)]
            (or found (when (and a (not= (w:id) (a:id)) (w:isStandard)) w)))]
    (if (not (and a b))
        (minibuffer.message "Need two windows to split")
        (let [sf (usable-frame (a:screen))]
          (: history :push [a b])
          (if side-by-side?
              (do (a:setFrame {:x sf.x :y sf.y :w (/ sf.w 2) :h sf.h})
                  (b:setFrame {:x (+ sf.x (/ sf.w 2)) :y sf.y :w (/ sf.w 2) :h sf.h}))
              (do (a:setFrame {:x sf.x :y sf.y :w sf.w :h (/ sf.h 2)})
                  (b:setFrame {:x sf.x :y (+ sf.y (/ sf.h 2)) :w sf.w :h (/ sf.h 2)})))
          (a:focus)))))

(fn split-side-by-side [] (split-with-last true))
(fn split-stacked [] (split-with-last false))

(fn rotate-windows [step]
  "Move each visible window of the focused screen into the place of the next
  one (STEP 1) or the previous one (STEP -1), like evil-window-rotate."
  (let [wins (screen-windows (current-screen))
        n (length wins)
        frames (icollect [_ w (ipairs wins)] (w:frame))]
    (if (< n 2)
        (minibuffer.message "Need two windows to rotate")
        (do
          (: history :push wins)
          (each [i w (ipairs wins)]
            (w:setFrame (. frames (+ 1 (% (+ (- i 1) step n) n)))))))))

(fn rotate-windows-down [] (rotate-windows 1))
(fn rotate-windows-up [] (rotate-windows -1))

(fn allowed-app? [window]
  ((: window :isStandard) true false))

(fn jump []
  "Displays hammerspoon's window jump UI"
  (let [wns (->> (hs.window.allWindows)
                 (filter allowed-app?))]
    (hs.hints.windowHints wns nil true)))

;; Movement/Resizing Constants ======================
;; ==================================================

(local arrow-map {:k {:half [0 0 1 0.5]
                      :movement [0 -20]
                      :complement :h
                      :resize :Shorter}
                  :j {:half [0 0.5 1 0.5]
                      :movement [0 20]
                      :complement :l
                      :resize :Taller}
                  :h {:half [0 0 0.5 1]
                      :movement [-20 0]
                      :complement :j
                      :resize :Thinner}
                  :l {:half [0.5 0 0.5 1]
                      :movement [20 0]
                      :complement :k
                      :resize :Wider}})

(fn grid [method direction]
  "Moves, expands, or shrinks the active window by the next grid
  dimension. Grid settings are specified in config.fnl."
  (let [fn-name (.. method direction)
        f (. hs.grid fn-name)]
    (f (hs.window.focusedWindow))))

;; Resize window by half ============================
;; ==================================================

(fn rect [rct]
  "Change a window's rect geometry which includes x, y, width, and
  height Takes a rectangle table Performs side-effects to move or
  resize the active window and update history.  Returns nil"
  (: history :push)
  (let [win (hs.window.focusedWindow)]
    (when win (: win :move rct))))

;; Resize window by increments ======================
;; ==================================================

(fn resize-by-increment [arrow]
  "Resize the active window by the next window increment Let's say we
  make the grid dimensions 4x4 and we place a window in the 1x1
  meaning first column in the first row.  We then resize an increment
  right. The dimensions would now be 2x1

  Takes an arrow like :h :j :k :l
  Performs a side-effect to resize the current window to the next grid increment
  Returns nil"
  (let [directions {:h :Left :j :Down :k :Up :l :Right}]
    (: history :push)
    (when (or (= arrow :h) (= arrow :l))
      (hs.grid.resizeWindowThinner (hs.window.focusedWindow)))
    (when (or (= arrow :j) (= arrow :k))
      (hs.grid.resizeWindowShorter (hs.window.focusedWindow)))
    (grid :pushWindow (. directions arrow))))

(fn resize-inc-left []
  (resize-by-increment :h))

(fn resize-inc-bottom []
  (resize-by-increment :j))

(fn resize-inc-top []
  (resize-by-increment :k))

(fn resize-inc-right []
  (resize-by-increment :l))

;; Resize windows ===================================
;; ==================================================

(fn resize-window [arrow]
  "Resizes a window against the grid specifed in config.fnl Takes an
  arrow string like :h :k :j :l Performs a side effect to resize the
  current window.  Returns nil"
  (: history :push)
  ;; hs.grid.resizeWindowShorter/Taller/Thinner/Wider
  (grid :resizeWindow (. arrow-map arrow :resize)))

(fn resize-left []
  (resize-window :h))

(fn resize-up []
  (resize-window :j))

(fn resize-down []
  (resize-window :k))

(fn resize-right []
  (resize-window :l))

;; Resize to grid preset ============================
;; ==================================================

(fn resize-to-grid [grid]
  (: history :push)
  (hs.grid.set (hs.window.focusedWindow) grid))

;; Move to screen directions ========================
;; ==================================================

(fn move-to-screen [screen]
  "Moves current window onto given hs.screen instance"
  (let [w (hs.window.focusedWindow)
        no-resize true]
    (: w :moveToScreen screen no-resize)))

(fn move-screen [method]
  " Moves a window to the display in the specified direction
  :north ^ :south v :east -> :west <- Takes a method name of the
  hammer spoon window instance.  You probably will not be using this
  function directly.  Performs a side effect that will move a window
  the next screen in specified direction.  Returns nil"
  (let [window (hs.window.focusedWindow)]
    (: window method nil true)))

(fn move-north []
  (move-screen :moveOneScreenNorth))

(fn move-south []
  (move-screen :moveOneScreenSouth))

(fn move-east []
  (move-screen :moveOneScreenEast))

(fn move-west []
  (move-screen :moveOneScreenWest))

(local canvas (require :hs.canvas))
(local screen-number-canvases (atom []))

;; Move to screen by number =========================
;; ==================================================

(fn show-display-number [idx screen]
  "Shows a big number at the corner of hs.screen.
   To be used as for multi-monitor setups, to easily identify index of
   each screen."
  (let [cs (canvas.new {})
        font-size (/ (. (: screen :frame) :w) 10)]
    (swap! screen-number-canvases (fn [t] (concat t [cs])))
    (doto cs
      (: :frame (: screen :frame))
      (: :appendElements [{:action :fill
                           :type :text
                           :frame {:x :0.93 :y 0 :h :1 :w :1}
                           :text (hs.styledtext.new idx
                                                    {:font {:size font-size}
                                                     :color {:red 1
                                                             :green 0.5
                                                             :blue 0
                                                             :alpha 1}})
                           :withShadow true}])
      (: :show))))

(fn show-display-numbers [screens]
  "Shows big number at the corner of each screen.
   To be used as for multi-monitor setups, to easily identify index of
   each screen."
  (let [ss (hs.screen.allScreens)]
    (when (< 1 (count ss))
      (each [idx display (ipairs (hs.screen.allScreens))]
        (show-display-number idx display)))))

(fn hide-display-numbers []
  "Hides big numbers at the corner of each screen that are used for
   guidance in multi-monitor setups."
  (for-each (fn [c] (: c :delete 0.4)) (deref screen-number-canvases))
  (reset! screen-number-canvases []))

(fn monitor-item [screen i]
  "Creates a menu item to move the frontMost window to the specified
  screen index Takes a hs.screen instance and an index integer Returns
  a table-map to add to a config.fnl modal menu"
  {:title (.. "Monitor " i)
   :key (tostring i)
   :group :monitor
   :action (fn [] (when screen (move-to-screen screen)))})

(fn remove-monitor-items [menu]
  "Removes the monitor items from a menu Takes a menu table-map Mutates
  the menu object to remove items with :group :monitor flags Returns
  mutated table-map"
  (->> menu.items
       (filter #(not (= (. $ :group) :monitor)))
       (tset menu :items))
  menu)

(fn add-monitor-items [menu screens]
  "Update a menu by adding an item for each connected monitor Takes a
  menu table-map and a table-list of hs.screens Mutates the menu.items
  by adding items for each monitor Returns mutated modal menu
  table-map"
  (->> screens
       (map monitor-item)
       (concat menu.items)
       (tset menu :items))
  menu)

(fn enter-window-menu [menu]
  "Handler that can be used when entering the windows menu
  Takes modal menu table-map
  - Hides any previous display numbers
  - Shows display numbers at top right of each screen
  - Removes previous monitor items if any were added
  - Adds monitor items based on currently connected monitors
  Returns mutated modal menu table-map for threading or chaining"
  (let [screens (hs.screen.allScreens)]
    (hide-display-numbers)
    (show-display-numbers screens)
    (remove-monitor-items menu)
    (add-monitor-items menu screens))
  menu)

(fn exit-window-menu [menu]
  "Handler that can be used when exiting the windows menu
  - Removes previous monitor items if any were added
  Returns mutated modal menu table-map for threading or chaining"
  (hide-display-numbers)
  menu)

;; Initialization ===================================
;; ==================================================

(fn init [config]
  "Initializes the windows module
   Performs side effects:
   - Set grid margins from config.fnl like {:grid {:margins [10 10]}}
   - Set the grid dimensions from config.fnl like {:grid {:size \"3x2\"}}"
  (hs.grid.setMargins (or (get-in [:grid :margins] config) [0 0]))
  (hs.grid.setGrid (or (get-in [:grid :size] config) :3x2)))

;; Exports ==========================================
;; ==================================================

(collect [name f (pairs window-adjusters)
         &into {: activate-app
                : center-window-frame
                : enter-window-menu
                : exit-window-menu
                : hide-display-numbers
                : init
                : jump
                : jump-to-last-window
                : close-window
                : toggle-fullscreen
                : tile-windows
                : split-side-by-side
                : split-stacked
                : rotate-windows-down
                : rotate-windows-up
                : redo
                : remember-frames
                : jump-window-above
                : jump-window-below
                : jump-window-left
                : jump-window-right
                : maximize-window-frame
                : minimize-window-frame
                : move-east
                : move-north
                : move-south
                : move-to-screen
                : move-west
                : position-window-center
                : rect
                : resize-down
                : resize-inc-bottom
                : resize-inc-left
                : resize-inc-right
                : resize-inc-top
                : resize-left
                : resize-right
                : resize-up
                : resize-to-grid
                : set-mouse-cursor-at
                : show-display-numbers
                : show-grid
                : usable-frame
                : undo}]
  name f)
