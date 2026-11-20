"
Named window layouts, like Emacs' desktop-save-mode: save where the windows
are, put them back later. Layouts live in hs.settings.

Windows are matched by app (bundle ID) and title; windows of the same app
whose titles changed take the remaining saved frames in order. Apps that
aren't running are skipped, not launched. Restoring can be undone with the
window menu's undo.
"
(local minibuffer (require :lib.minibuffer))
(local buffers (require :buffers))
(local windows (require :windows))

(local settings-key :layouts)

;; The layout last saved or restored, for revert.
(var current nil)

(fn all-layouts []
  (or (hs.settings.get settings-key) {}))

(fn store [layouts]
  (hs.settings.set settings-key layouts))

(fn rect-table [r]
  {:x r.x :y r.y :w r.w :h r.h})

(fn capture []
  "The visible standard windows, frontmost first."
  (icollect [_ w (ipairs (buffers.windows))]
    (let [app (w:application)
          screen (w:screen)]
      (when (and app (app:bundleID) (not (w:isMinimized))
                 (not (app:isHidden)) (w:isStandard))
        {:bundle (app:bundleID)
         :app (app:name)
         :title (w:title)
         :frame (rect-table (w:frame))
         :screen (and screen (screen:getUUID))
         :unit (and screen (rect-table (screen:toUnitRect (w:frame))))}))))

(fn target-frame [entry]
  "ENTRY's frame on its screen, or scaled onto the main screen when that
  screen is gone."
  (if (or (not entry.screen) (hs.screen.find entry.screen))
      (hs.geometry.rect entry.frame)
      (: (hs.screen.mainScreen) :fromUnitRect (hs.geometry.rect entry.unit))))

(fn assign [entries]
  "Pair saved ENTRIES with current windows: exact titles first, then the
  remaining windows of each app in order."
  (let [by-app {}
        taken {}
        pairs* []]
    (each [_ w (ipairs (buffers.windows))]
      (let [app (w:application)
            bundle (and app (app:bundleID))]
        (when bundle
          (when (not (. by-app bundle)) (tset by-app bundle []))
          (table.insert (. by-app bundle) w))))
    (fn take [entry pred]
      (let [w (hs.fnutils.find (or (. by-app entry.bundle) [])
                               #(and (not (. taken ($1:id))) (pred $1)))]
        (when w
          (tset taken (w:id) true)
          (table.insert pairs* [entry w]))
        w))
    (let [unmatched (icollect [_ e (ipairs entries)]
                      (when (not (take e #(= ($1:title) e.title))) e))]
      (each [_ e (ipairs unmatched)]
        (take e #true)))
    pairs*))

(fn apply-layout [name]
  (let [layout (. (all-layouts) name)]
    (if (not layout)
        (minibuffer.message (.. "No layout " name))
        (let [matched (assign layout.windows)
              wins (icollect [_ [_ w] (ipairs matched)] w)]
          (windows.remember-frames wins)
          (each [_ [entry w] (ipairs matched)]
            (when (w:isMinimized) (w:unminimize))
            (w:setFrame (target-frame entry) 0))
          ;; Raise back to front so the saved stacking order returns.
          (for [i (length matched) 1 -1]
            (let [[_ w] (. matched i)] (w:raise)))
          (let [[_ front] (or (. matched 1) [])]
            (when front (front:focus)))
          (set current name)
          (let [missing (- (length layout.windows) (length matched))]
            (minibuffer.message
             (.. "Restored " name
                 (if (> missing 0)
                     (.. " (" missing " window" (if (= missing 1) "" "s")
                         " not open)")
                     ""))))))))

(fn candidates []
  (let [layouts (all-layouts)
        names (icollect [name (pairs layouts)] name)]
    (table.sort names #(> (or (. layouts $1 :saved) 0) (or (. layouts $2 :saved) 0)))
    (icollect [_ name (ipairs names)]
      (let [l (. layouts name)]
        {:text name
         :annotation (.. (length l.windows) " windows, "
                         (os.date "%Y-%m-%d %H:%M" l.saved))}))))

(fn write-layout [name]
  (let [layouts (all-layouts)
        wins (capture)]
    (tset layouts name {:saved (os.time) :windows wins})
    (store layouts)
    (set current name)
    (minibuffer.message (.. "Saved " name " (" (length wins) " windows)"))))

(fn save []
  "Save the current window layout under a name; existing names are offered,
  and overwriting one asks first."
  (minibuffer.completing-read
   "Save layout: " (candidates)
   {:on-accept
    (fn [candidate input]
      (let [name (if (input:match "%S")
                     (input:match "^%s*(.-)%s*$")
                     (and candidate candidate.text))]
        (if (not name) (minibuffer.message "No name given")
            (. (all-layouts) name)
            (minibuffer.y-or-n-p (.. "Overwrite layout " name "?")
                                 #(write-layout name))
            (write-layout name))))}))

(fn read-layout [prompt on-name]
  (let [cs (candidates)]
    (if (= 0 (length cs))
        (minibuffer.message "No saved layouts")
        (minibuffer.completing-read
         prompt cs
         {:on-accept #(when $1 (on-name $1.text))}))))

(fn restore []
  "Move the open windows to a saved layout."
  (read-layout "Restore layout: " apply-layout))

(fn delete []
  "Delete a saved layout, after confirmation."
  (read-layout "Delete layout: "
               (fn [name]
                 (minibuffer.y-or-n-p
                  (.. "Delete layout " name "?")
                  (fn []
                    (let [layouts (all-layouts)]
                      (tset layouts name nil)
                      (store layouts)
                      (when (= current name) (set current nil))
                      (minibuffer.message (.. "Deleted " name))))))))

(fn revert []
  "Restore the layout last saved or restored again."
  (if current
      (apply-layout current)
      (minibuffer.message "No layout saved or restored yet")))

{: save : restore : delete : revert}
