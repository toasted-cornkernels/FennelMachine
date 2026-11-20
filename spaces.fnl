"Spaces as tabs, like tab-bar-mode: switch by position, step left and right,
return to the last Space, pick one from a list, add and close desktops"

(local minibuffer (require :lib.minibuffer))
(local system-shortcuts (require :system-shortcuts))

;; Symbolic hotkeys of Keyboard Shortcuts › Mission Control.
(local move-left-hotkey 79)
(local move-right-hotkey 81)
(local desktop-1-hotkey 118)
(local max-desktop-hotkeys 16)

;; Space IDs, most recently shown first. Kept in hs.settings so that a reload
;; doesn't forget them; IDs last until the Dock restarts, and gone ones are
;; skipped.
(local recent-key :spaces.recent)
(local recent-limit 50)
(var recent [])
(var watcher nil)
;; Kept here so that pending timers aren't garbage collected.
(var timer nil)

(fn screen-spaces [screen]
  (or (hs.spaces.spacesForScreen (or screen (hs.screen.mainScreen))) []))

(fn desktops [screen]
  "The user desktops of SCREEN, numbered as Mission Control numbers them."
  (icollect [_ s (ipairs (screen-spaces screen))]
    (when (= (hs.spaces.spaceType s) :user) s)))

(fn index-of [list x]
  (accumulate [found nil i y (ipairs list)]
    (or found (when (= x y) i))))

(fn screen-of [space]
  (accumulate [found nil _ screen (ipairs (hs.screen.allScreens))]
    (or found (when (index-of (screen-spaces screen) space) screen))))

(fn own-window [space]
  "The most recently used window shown only on SPACE, e.g. a full-screen app's."
  (hs.fnutils.find ((. (require :buffers) :windows))
                   #(let [spaces (or (hs.spaces.windowSpaces $1) [])]
                      (and (= (length spaces) 1) (= (. spaces 1) space)))))

(fn goto-space [space]
  "Show SPACE with the usual slide: a desktop through its Switch to Desktop
  shortcut, a full-screen Space by focusing its window. Otherwise, or if that
  didn't get there, through Mission Control."
  (let [n (index-of (desktops (screen-of space)) space)
        win (when (not n) (own-window space))]
    (if (and n (<= n max-desktop-hotkeys))
        (system-shortcuts.run (+ desktop-1-hotkey n -1))
        win
        (do
          (win:focus)
          (when timer (timer:stop))
          (set timer (hs.timer.doAfter 1 #(when (not= (hs.spaces.focusedSpace) space)
                                            (hs.spaces.gotoSpace space)))))
        (hs.spaces.gotoSpace space))))

(fn goto-space-number [n]
  "Show the Nth Space of the screen in Mission Control's order, counting
  full-screen apps, like tab-bar-select-tab."
  (let [space (. (screen-spaces) n)]
    (if space (goto-space space) (minibuffer.message (.. "No Space " n)))))

(fn space-left [] (system-shortcuts.run move-left-hotkey))
(fn space-right [] (system-shortcuts.run move-right-hotkey))

(fn visit [space]
  (when space
    (let [others (icollect [_ s (ipairs recent)] (when (not= s space) s))]
      (set recent [space])
      (each [_ s (ipairs others)]
        (when (< (length recent) recent-limit)
          (table.insert recent s)))
      (hs.settings.set recent-key recent))))

(fn last-space []
  "Go back to the previously shown Space, like tab-recent."
  (let [focused (hs.spaces.focusedSpace)
        previous (hs.fnutils.find recent #(and (not= $1 focused) (screen-of $1)))]
    (if previous
        (goto-space previous)
        (minibuffer.message "No previous Space"))))

(fn windows-by-id []
  "Every titled window, on any Space; hs.window.get only sees the current one."
  (collect [_ w (ipairs (: ((. (require :buffers) :init)) :getWindows))]
    (values (w:id) w)))

(fn space-windows [space by-id]
  (icollect [_ id (ipairs (or (hs.spaces.windowsForSpace space) []))]
    (. by-id id)))

(fn space-name [space by-id]
  (let [n (index-of (desktops (screen-of space)) space)
        app (-?> (space-windows space by-id) (. 1) (: :application))]
    (if n (.. "Desktop " n)
        (.. (if app (.. (app:name) " ") "") "(full screen)"))))

(fn window-names [space by-id]
  "The apps with windows on SPACE, for annotations."
  (let [names []
        seen {}]
    (each [_ win (ipairs (space-windows space by-id))]
      (let [name (-?> (win:application) (: :name))]
        (when (and name (not (. seen name)))
          (tset seen name true)
          (table.insert names name))))
    (table.concat names ", ")))

(fn list-spaces []
  "Pick a Space from every Space on every screen, most recently shown first
  and the current one last; Spaces not shown since loading follow in Mission
  Control's order."
  (let [focused (hs.spaces.focusedSpace)
        by-id (windows-by-id)
        rank (collect [i space (ipairs recent)] (values space i))
        candidates []]
    (each [_ screen (ipairs (hs.screen.allScreens))]
      (each [_ space (ipairs (screen-spaces screen))]
        (table.insert candidates
                      {:text (.. (space-name space by-id)
                                 (if (= space focused) " (current)" ""))
                       :annotation (window-names space by-id)
                       : space
                       :order (if (= space focused) math.huge
                                  (or (. rank space)
                                      (+ recent-limit (length candidates) 1)))})))
    (table.sort candidates #(< $1.order $2.order))
    (minibuffer.completing-read
     "Switch to Space: " candidates
     {:on-accept (fn [c] (when c (goto-space c.space)))})))

(fn new-space []
  "Add a desktop to the focused screen and show it."
  (let [screen (hs.screen.mainScreen)
        (ok err) (hs.spaces.addSpaceToScreen screen false)]
    (if (not ok)
        (minibuffer.message (.. "Can't add a desktop: " (tostring err)))
        (let [all (desktops screen)]
          (minibuffer.message (.. "Added Desktop " (length all)))
          ;; The Dock needs a moment before the new desktop can be switched to.
          (when timer (timer:stop))
          (set timer (hs.timer.doAfter 0.5 #(goto-space (. all (length all)))))))))

(fn close-space []
  "Close the current desktop after asking; macOS moves its windows to another
  desktop. Shows the desktop to its left (or right) first, as a Space can't be
  removed while shown."
  (let [space (hs.spaces.focusedSpace)
        all (desktops)
        n (index-of all space)]
    (if (not n) (minibuffer.message "Only desktops can be closed")
        (= (length all) 1) (minibuffer.message "Can't close the only desktop")
        (minibuffer.y-or-n-p
         (.. "Close Desktop " n "?")
         (fn []
           (let [target (. all (if (> n 1) (- n 1) (+ n 1)))]
             (goto-space target)
             (var waited 0)
             (when timer (timer:stop))
             (set timer
                  (hs.timer.doEvery
                   0.2
                   (fn []
                     (set waited (+ waited 0.2))
                     (if (= (hs.spaces.focusedSpace) target)
                         (do
                           (timer:stop)
                           ;; Wait for the slide to finish.
                           (hs.timer.doAfter
                            0.5
                            #(let [(ok err) (hs.spaces.removeSpace space false)]
                               (minibuffer.message
                                (if ok (.. "Closed Desktop " n)
                                    (.. "Can't close Desktop " n ": "
                                        (tostring err)))))))
                         (>= waited 5)
                         (do
                           (timer:stop)
                           (minibuffer.message "Couldn't leave the desktop"))))))))))))

(fn init []
  (set recent (or (hs.settings.get recent-key) []))
  (visit (hs.spaces.focusedSpace))
  (set watcher (hs.spaces.watcher.new
                (fn []
                  (let [space (hs.spaces.focusedSpace)]
                    (when (not= space (. recent 1))
                      (visit space))))))
  (watcher:start)
  watcher)

(local exports
       {: init
        : goto-space-number
        : space-left
        : space-right
        : last-space
        : list-spaces
        : new-space
        : close-space})

(for [n 1 9]
  (tset exports (.. "goto-space-" n) #(goto-space-number n)))

exports
