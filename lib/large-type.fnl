"
Large Type: show text across the screen in the largest font that fits, for
reading from a distance. Any key or click dismisses it.
"
(local which-key (require :lib.which-key))
(local {: palette} which-key)

(local font "Helvetica Neue")
(local max-size 180)
(local min-size 18)

(var canvas nil)
(var tap nil)

(fn hide []
  (when tap
    (: tap :stop)
    (set tap nil))
  (when canvas
    (: canvas :delete)
    (set canvas nil)))

(fn fitting-size [text max-w max-h]
  (var size max-size)
  (while (and (> size min-size)
              (let [{: w : h} (hs.drawing.getTextDrawingSize
                               text {:font font : size})]
                (or (> w max-w) (> h max-h))))
    (set size (math.floor (* size 0.9))))
  size)

(fn show [text]
  "Show TEXT in Large Type on the main screen."
  (hide)
  (let [text (tostring text)
        frame (: (or (hs.screen.mainScreen) (hs.screen.primaryScreen)) :frame)
        max-w (* frame.w 0.85)
        size (fitting-size text max-w (* frame.h 0.7))
        measured (hs.drawing.getTextDrawingSize text {:font font : size})
        w (+ (math.min max-w measured.w) (* size 0.8))
        h (+ measured.h (* size 0.6))
        rect {:x (+ frame.x (/ (- frame.w w) 2)) :y (+ frame.y (/ (- frame.h h) 2))
              : w : h}]
    (set canvas
         (doto (hs.canvas.new rect)
           (: :level hs.canvas.windowLevels.overlay)
           (: :behavior [:canJoinAllSpaces :stationary])
           (: :appendElements
              [{:type :rectangle :action :fill :fillColor {:hex palette.bg :alpha 0.92}
                :roundedRectRadii {:xRadius 18 :yRadius 18}
                :trackMouseDown true}
               {:type :text :text (hs.styledtext.new
                                   text {:font {:name font : size}
                                         :color {:hex palette.fg}
                                         :paragraphStyle {:alignment :center}})
                :frame {:x (* size 0.4) :y (* size 0.3)
                        :w (- w (* size 0.8)) :h measured.h}}])
           (: :mouseCallback hide)
           (: :show)))
    (set tap (doto (hs.eventtap.new [hs.eventtap.event.types.keyDown]
                                    (fn [] (hide) true))
               (: :start)))))

{: show
 : hide
 :showing (fn [] (not= canvas nil))}
