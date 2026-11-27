"
Choose a window by typing the label drawn on it, like ace-window: focus it,
close it, or swap places with the focused window. Also select the Nth window
on screen, numbered left to right like winum.
"
(local minibuffer (require :lib.minibuffer))
(local {: palette : font-name} (require :lib.which-key))
(local {: ordered-windows} (require :lib.ordered-windows))

(local label-chars "asdfghjkl")
(local label-size 64)

;; The labels being shown: {:items [{:win :label :canvas}] :typed :tap :act}.
(var state nil)

(fn hide []
  (when state
    (when state.tap (state.tap:stop))
    (each [_ item (ipairs state.items)] (item.canvas:delete))
    (set state nil)))

(fn label-canvas [win label]
  (let [frame (win:frame)
        size (hs.drawing.getTextDrawingSize label {:font font-name :size label-size})
        w (+ size.w 48)
        h (+ size.h 16)
        canvas (hs.canvas.new {:x (+ frame.x (/ (- frame.w w) 2))
                               :y (+ frame.y (/ (- frame.h h) 2))
                               : w : h})]
    (canvas:level hs.canvas.windowLevels.overlay)
    (canvas:behavior [:canJoinAllSpaces :transient])
    (canvas:appendElements
     [{:type :rectangle :fillColor {:hex palette.bg :alpha 0.92}
       :strokeColor {:hex palette.key} :strokeWidth 3 :action :strokeAndFill
       :roundedRectRadii {:xRadius 12 :yRadius 12}}
      {:type :text :text (hs.styledtext.new label {:font {:name font-name :size label-size}
                                                   :color {:hex palette.key}
                                                   :paragraphStyle {:alignment :center}})
       :frame {:x 0 :y 8 : w : h}}])
    (canvas:show)
    canvas))

(fn on-key [event]
  (let [key (. hs.keycodes.map (event:getKeyCode))
        flags (event:getFlags)
        chars (event:getCharacters)
        s state]
    (if (or (= key :escape) (and flags.ctrl (= key :g)))
        (do (hide) (minibuffer.message "Quit"))
        flags.cmd (do (hide) false)
        (let [typed (.. s.typed (or chars ""))
              exact (hs.fnutils.find s.items #(= $.label typed))
              prefixed (icollect [_ item (ipairs s.items)]
                         (when (= (item.label:sub 1 (length typed)) typed) item))]
          (if exact (do (hide) (s.act exact.win))
              (= (length prefixed) 0) (do (hide) (minibuffer.message (.. "No window labelled " typed)))
              (do (set s.typed typed)
                  (each [_ item (ipairs s.items)]
                    (item.canvas:alpha (if (hs.fnutils.contains prefixed item) 1 0.3)))))))
    true))

(fn standard-windows []
  (icollect [_ w (ipairs (ordered-windows))]
    (when (w:isStandard) w)))

(fn choose [prompt act ?exclude-focused]
  "Label the visible windows, read a label and call ACT with its window."
  (hide)
  (let [focused (hs.window.focusedWindow)
        wins (icollect [_ w (ipairs (standard-windows))]
               (when (not (and ?exclude-focused focused (= (w:id) (focused:id)))) w))]
    (if (= (length wins) 0)
        (minibuffer.message "No other window")
        (let [labels ((. (require :hints) :make-labels) (length wins) label-chars)]
          (set state {:typed ""
                      : act
                      :items (icollect [i w (ipairs wins)]
                               {:win w :label (. labels i)
                                :canvas (label-canvas w (. labels i))})})
          (minibuffer.message prompt 10)
          (set state.tap (hs.eventtap.new [hs.eventtap.event.types.keyDown] on-key))
          (state.tap:start)))))

(fn focus [win]
  (let [app (win:application)]
    (when (and app (app:isHidden)) (app:unhide))
    (win:focus)))

(fn select-window []
  "Focus a window chosen by its label, like ace-window."
  (choose "Select window:" focus))

(fn delete-window []
  "Close a window chosen by its label, like ace-delete-window."
  (choose "Delete window:" #(when (not ($:close))
                              (minibuffer.message (.. "Can't close " ($:title))))))

(fn swap-window []
  "Swap the focused window's place with one chosen by its label, like
  ace-swap-window; the focused window keeps focus."
  (let [focused (hs.window.focusedWindow)]
    (if (not focused)
        (minibuffer.message "No focused window")
        (choose "Swap with window:"
                (fn [other]
                  (let [a (focused:frame)
                        b (other:frame)]
                    (focused:setFrame b 0)
                    (other:setFrame a 0)
                    (focused:focus)))
                true))))

(fn numbered-windows []
  "Visible standard windows, numbered by screen (left to right), then by
  their left edge, then their top."
  (let [wins (standard-windows)
        key (fn [w]
              (let [f (w:frame)
                    sf (: (w:screen) :frame)]
                [sf.x f.x f.y]))]
    (table.sort wins (fn [a b]
                       (let [[as ax ay] (key a)
                             [bs bx by] (key b)]
                         (if (not= as bs) (< as bs)
                             (not= ax bx) (< ax bx)
                             (< ay by)))))
    wins))

(fn select-window-n [n]
  (let [win (. (numbered-windows) n)]
    (if win (focus win) (minibuffer.message (.. "No window " n)))))

(local exports
       {: choose
        : hide
        : select-window
        : delete-window
        : swap-window
        : numbered-windows
        : select-window-n})

(for [i 1 9]
  (tset exports (.. "select-window-" i) #(select-window-n i)))

exports
