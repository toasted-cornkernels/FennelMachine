;; Recording HUD for voice.fnl: what's happening (starting, listening, ended
;; at the length limit, transcribing), the language, the time so far, a live
;; microphone level meter and how to stop. Bottom centre, so it stays clear of
;; the Now Playing HUD.

(local {: palette : font-name} (require :lib.which-key))

(local width 420)
(local height 76)
(local bottom-margin 120)
(local pad 14)
(local bar-count 28)
(local bar-w 4)
(local bar-gap 2)
(local meter-w (- (* bar-count (+ bar-w bar-gap)) bar-gap))
(local floor-db -60)
;; Only digital silence: a real microphone's noise floor sits well above this.
(local silence-db -90)
(local red "#f7768e")
(local amber "#e0af68")

(local hud {})

(fn text [str size color alignment]
  (hs.styledtext.new (or str "")
                     {:font {:name font-name : size}
                      :color {:hex color}
                      :paragraphStyle {:lineBreak :truncateTail
                                       :alignment (or alignment :left)}}))

(fn clock [seconds]
  (let [s (math.floor (or seconds 0))]
    (string.format "%d:%02d" (math.floor (/ s 60)) (% s 60))))

(fn silent? []
  "Whether the microphone has delivered nothing but zeros for a second and a
half of listening: macOS withholding it, or a muted input."
  (and (= hud.phase :listening)
       (> (- (hs.timer.secondsSinceEpoch) hud.listening-since) 1.5)
       (not hud.heard?)))

(fn meter []
  (let [x0 (- width pad meter-w)
        mid (/ height 2)
        n (length hud.levels)
        bars []]
    (for [i 1 bar-count]
      (let [db (or (. hud.levels (+ (- n bar-count) i)) floor-db)
            fraction (math.max 0.06 (math.min 1 (/ (- db floor-db) (- floor-db))))
            h (* fraction (- height (* 2 pad) 8))]
        (table.insert
         bars
         {:type :rectangle :action :fill
          :fillColor {:hex (if (= hud.phase :listening) palette.key palette.border)}
          :roundedRectRadii {:xRadius 2 :yRadius 2}
          :frame {:x (+ x0 (* (- i 1) (+ bar-w bar-gap))) :y (- mid (/ h 2))
                  :w bar-w :h h}})))
    bars))

(var hide nil)

(fn render []
  ;; Whoever transcribes hides it when done; this is only in case they can't.
  (when (and hud.canvas hud.stopped-at
             (> (- (hs.timer.secondsSinceEpoch) hud.stopped-at) 310))
    (hide))
  (when hud.canvas
    (let [now (hs.timer.secondsSinceEpoch)
          ;; The dot breathes while listening.
          pulse (if (= hud.phase :listening)
                    (+ 0.55 (* 0.45 (math.abs (math.sin (* now 3)))))
                    1)
          [title color] (if (silent?) ["Mic is silent" amber]
                            (= hud.phase :starting) ["Starting…" palette.dim]
                            (= hud.phase :ended) ["Time limit reached" amber]
                            (= hud.phase :transcribing) ["Transcribing…" palette.prefix]
                            [(.. "Listening · " (or hud.language "")) red])
          elapsed (clock (- (or hud.stopped-at now) hud.started-at))
          text-w (- width pad 18 12 meter-w pad)
          elements [{:type :rectangle :action :fill
                     :fillColor {:hex palette.bg :alpha 0.97}
                     :strokeColor {:hex palette.border} :strokeWidth 1
                     :roundedRectRadii {:xRadius 10 :yRadius 10}}
                    {:type :circle :action :fill
                     :fillColor {:hex color :alpha pulse}
                     :center {:x (+ pad 7) :y (+ pad 12)} :radius 6}
                    {:type :text :text (text title 17 color)
                     :frame {:x (+ pad 20) :y pad :w (- text-w 50) :h 24}}
                    {:type :text :text (text elapsed 15 palette.dim :right)
                     :frame {:x (+ pad 20 (- text-w 50)) :y (+ pad 2) :w 50 :h 22}}
                    {:type :text
                     :text (text (if (= hud.phase :transcribing)
                                     (or hud.cancel-hint "")
                                     hud.hint)
                                 13 palette.dim)
                     :frame {:x (+ pad 20) :y (+ pad 28) :w text-w :h 20}}]]
      (each [_ bar (ipairs (meter))]
        (table.insert elements bar))
      (hud.canvas:replaceElements elements))))

(set hide (fn []
  "Hide the recording HUD."
  (when hud.timer (hud.timer:stop))
  (when hud.canvas (hud.canvas:delete))
  (set hud.timer nil)
  (set hud.canvas nil)))

(fn show [{: language : hint : cancel-hint}]
  "Show the HUD for a recording that's starting, in LANGUAGE. HINT says how to
stop; CANCEL-HINT, if any, how to cancel while transcribing. Clicks go through
to the windows below."
  (hide)
  (let [frame (: (hs.screen.mainScreen) :frame)
        now (hs.timer.secondsSinceEpoch)]
    (set hud.phase :starting)
    (set hud.language language)
    (set hud.hint (or hint ""))
    (set hud.cancel-hint cancel-hint)
    (set hud.levels [])
    (set hud.heard? false)
    (set hud.started-at now)
    (set hud.listening-since now)
    (set hud.stopped-at nil)
    (set hud.canvas
         (doto (hs.canvas.new {:x (+ frame.x (/ (- frame.w width) 2))
                               :y (- (+ frame.y frame.h) height bottom-margin)
                               :w width :h height})
           (: :level hs.canvas.windowLevels.overlay)
           (: :behavior [:canJoinAllSpaces :transient])))
    (render)
    (hud.canvas:show)
    (set hud.timer (hs.timer.doEvery 0.1 render))))

(fn set-phase [phase]
  "Move the HUD to PHASE: :listening, :ended (the recording stopped by itself)
or :transcribing."
  (when hud.canvas
    (when (and (= phase :listening) (not= hud.phase :listening))
      (set hud.listening-since (hs.timer.secondsSinceEpoch)))
    (when (or (= phase :ended)
              (and (= phase :transcribing) (not= hud.phase :ended)))
      (set hud.stopped-at (hs.timer.secondsSinceEpoch)))
    (set hud.phase phase)
    (render)))

(fn done []
  "Hide the HUD if it's showing a transcription, not a newer recording."
  (when (= hud.phase :transcribing)
    (hide)))

(fn level [db]
  "Add a microphone level reading, in dBFS (-math.huge for silence)."
  (when hud.canvas
    (when (> db silence-db) (set hud.heard? true))
    (table.insert hud.levels db)
    (when (> (length hud.levels) bar-count)
      (table.remove hud.levels 1))))

{: show : hide : done : set-phase : level}
