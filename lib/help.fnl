"
A less-like viewer for long help text: a panel above the echo area showing one
screen at a time. Lines are plain strings (which may contain newlines) or lists
of [text face] segments, with faces :fg :heading :key and :dim. Long lines wrap,
segmented ones with a hanging indent under their second segment.

  j k C-n C-p <down> <up> RET     scroll a line
  SPC f C-v <pagedown>            next page
  b DEL M-v <pageup>              previous page
  d u                             half a page down / up
  g < M-< <home>  G > M-> <end>   top / bottom
  / n N                           search, next / previous match
  M-w                             copy the text
  o                               open it in an Emacs *Hammerspoon Help* buffer
  s                               visit the source in Emacs, when there is one
  ?                               list these keys in the echo area
  q ESC C-g                       quit; Cmd chords quit and pass through
"
(local which-key (require :lib.which-key))
(local hotkeys (require :lib.hotkeys))
(local minibuffer (require :lib.minibuffer))
(local {: palette : font-name : font-size} which-key)

(local faces {:fg palette.fg :heading palette.prefix :key palette.key
              :dim palette.dim})
(local colors {:match "#3a3d5c" :mode-line "#2c2e40"})
(local pad-x 18)
(local pad-y 12)
(local max-cols 110)
(local buffer-name "*Hammerspoon Help*")

(local line-height
       (. (hs.drawing.getTextDrawingSize "M" {:font font-name :size font-size}) :h))

(var hud nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn split-lines [s]
  (icollect [l (string.gmatch (.. s "\n") "(.-)\n")] l))

(fn segments [line]
  "LINE as a list of [text face] segments."
  (if (= (type line) :string) [[line :fg]]
      (icollect [_ seg (ipairs line)]
        (if (= (type seg) :string) [seg :fg] seg))))

(fn expand [lines]
  "Split string lines on newlines and expand tabs."
  (let [acc []]
    (each [_ line (ipairs lines)]
      (if (= (type line) :string)
          (each [_ l (ipairs (split-lines line))]
            (table.insert acc [[(l:gsub "\t" "    ") :fg]]))
          (table.insert acc (icollect [_ [text face] (ipairs (segments line))]
                              [(: (tostring text) :gsub "\t" "    ") face]))))
    acc))

(fn plain [segs]
  (table.concat (icollect [_ [text] (ipairs segs)] text)))

(fn width [s] (or (utf8.len s) (length s)))

(fn wrap [segs cols]
  "Break SEGS into rows of at most COLS characters, preferring spaces."
  (let [chars []]
    (each [_ [text face] (ipairs segs)]
      (each [c (text:gmatch utf8.charpattern)]
        (table.insert chars [c face])))
    (let [n (length chars)
          hang (if (> (length segs) 1)
                   (math.min (width (. segs 1 1)) (// cols 2))
                   0)
          rows []]
      (var start 1)
      (while (<= start n)
        (let [first? (= (length rows) 0)
              avail (if first? cols (- cols hang))
              stop (if (<= (- n start) (- avail 1))
                       n
                       (do
                         (var i (+ start avail -1))
                         (while (and (> i start) (not= (. chars i 1) " "))
                           (set i (- i 1)))
                         (if (> i start) i (+ start avail -1))))
              row (if first? [] [[(string.rep " " hang) :fg]])]
          (for [i start stop]
            (let [[c face] (. chars i)
                  last (. row (length row))]
              (if (and last (= (. last 2) face))
                  (tset last 1 (.. (. last 1) c))
                  (table.insert row [c face]))))
          (table.insert rows row)
          (set start (+ stop 1))
          (while (and (<= start n) (= (. chars start 1) " "))
            (set start (+ start 1)))))
      (if (= n 0) [[]] rows))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn styled [text face]
  (hs.styledtext.new text {:font {:name font-name :size font-size}
                           :color {:hex (or (. faces face) face)}
                           :paragraphStyle {:lineBreak :clip}}))

(fn styled-row [row]
  (accumulate [st (styled "" :fg) _ [text face] (ipairs row)]
    (.. st (styled text face))))

(fn max-top []
  (math.max 1 (+ (- (length hud.rows) hud.page) 1)))

(fn position []
  (let [top hud.top
        bottom (+ top hud.page -1)
        n (length hud.rows)]
    (if (and (= top 1) (>= bottom n)) "All"
        (= top 1) "Top"
        (>= bottom n) "Bot"
        (.. (math.floor (* 100 (/ (- top 1) (- n hud.page)))) "%"))))

(fn mode-line []
  (let [left (.. " " (position) "  L" hud.top "/" (length hud.rows)
                 (if hud.query (.. "  /" hud.query) ""))
        right (.. "q quit  SPC b page  / search  M-w copy  o Emacs"
                  (if hud.source "  s source" "") "  ? keys ")
        gap (math.max 2 (- hud.cols (width left) (width right)))]
    (.. (styled left :fg) (styled (string.rep " " gap) :fg) (styled right :dim))))

(fn render []
  (let [{: w : h} hud
        body-y (+ pad-y line-height 8)
        elements [{:type :rectangle :action :fill
                   :fillColor {:hex palette.bg :alpha 0.97}
                   :strokeColor {:hex palette.border} :strokeWidth 1
                   :roundedRectRadii {:xRadius 10 :yRadius 10}}
                  {:type :text :text (styled hud.title :heading)
                   :frame {:x pad-x :y pad-y :w (- w (* 2 pad-x))
                           :h (+ line-height 2)}}
                  {:type :rectangle :action :fill :fillColor {:hex palette.border}
                   :frame {:x pad-x :y (+ pad-y line-height 3)
                           :w (- w (* 2 pad-x)) :h 1}}]]
    (for [i 0 (- hud.page 1)]
      (let [index (+ hud.top i)
            row (. hud.rows index)
            y (+ body-y (* i line-height))]
        (when row
          (when (= index hud.match)
            (table.insert elements {:type :rectangle :action :fill
                                    :fillColor {:hex colors.match}
                                    :frame {:x (- pad-x 4) : y
                                            :w (- w (* 2 pad-x) -8)
                                            :h line-height}}))
          (table.insert elements {:type :text :text (styled-row row)
                                  :frame {:x pad-x : y :w (- w (* 2 pad-x))
                                          :h (+ line-height 2)}}))))
    (let [y (- h line-height 10)]
      (table.insert elements {:type :rectangle :action :fill
                              :fillColor {:hex colors.mode-line}
                              :roundedRectRadii {:xRadius 10 :yRadius 10}
                              :frame {:x 1 :y (- y 4) :w (- w 2)
                                      :h (+ line-height 13)}})
      (table.insert elements {:type :text :text (mode-line)
                              :frame {:x pad-x : y :w (- w (* 2 pad-x))
                                      :h (+ line-height 2)}}))
    (: hud.canvas :replaceElements elements)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn close []
  (when hud
    (: hud.tap :stop)
    (: hud.canvas :delete)
    (set hud nil)))

(fn scroll-to [top]
  (set hud.top (math.max 1 (math.min (max-top) top)))
  (render))

(fn scroll [n] (scroll-to (+ hud.top n)))

(fn page [n] (scroll (* n (math.max 1 (- hud.page 1)))))

(fn half-page [n] (scroll (* n (math.max 1 (// hud.page 2)))))

(fn text []
  (table.concat (icollect [_ line (ipairs hud.lines)] (plain line)) "\n"))

(fn copy []
  (hs.pasteboard.setContents (text))
  (minibuffer.message (.. "Copied " (length hud.lines) " lines")))

(fn elisp-string [s]
  (.. "\"" (: (s:gsub "\\" "\\\\") :gsub "\"" "\\\"") "\""))

(fn focus-emacs []
  (let [app (hs.application.get "org.gnu.Emacs")]
    (when app (: app :activate))))

(fn emacsclient [args]
  (let [emacs (require :emacs)]
    (emacs.emacsclient args)
    (focus-emacs)))

(fn open-in-emacs []
  "Show the text in an Emacs special-mode buffer and focus it."
  (let [path (os.tmpname)
        file (io.open path :w)]
    (: file :write (text) "\n")
    (: file :close)
    (emacsclient
     ["-r" "-n" "-e"
      (string.format
       "(let ((buf (get-buffer-create %s)))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert-file-contents %s)
              (special-mode)
              (setq header-line-format %s)
              (goto-char (point-min))))
          (delete-file %s)
          (pop-to-buffer buf)
          (select-frame-set-input-focus (selected-frame)))"
       (elisp-string buffer-name) (elisp-string path)
       (elisp-string hud.title) (elisp-string path))])
    (close)))

(fn visit-source []
  (if hud.source
      (let [{: file : line} hud.source]
        (emacsclient ["-r" "-n" (.. "+" (or line 1)) file])
        (close))
      (minibuffer.message "No source for this help")))

(fn search-from [start dir]
  (let [query (hud.query:lower)]
    (var i start)
    (var found nil)
    (while (and (not found) (>= i 1) (<= i (length hud.rows)))
      (when (: (: (plain (. hud.rows i)) :lower) :find query 1 true)
        (set found i))
      (set i (+ i dir)))
    (if found
        (do (set hud.match found)
            (when (or (< found hud.top) (>= found (+ hud.top hud.page)))
              (set hud.top (math.max 1 (math.min (max-top) (- found 2)))))
            (render))
        (minibuffer.message (.. "Search failed: " hud.query)))))

(fn next-match [dir]
  (if (not hud.query)
      (minibuffer.message "No previous search")
      (search-from (+ (or hud.match (- hud.top dir)) dir) dir)))

(fn read-search []
  (let [current hud
        resume #(hs.timer.doAfter 0 #(when (= hud current) (: current.tap :start)))]
    (: hud.tap :stop)
    (minibuffer.completing-read
     "Search: " []
     {:on-accept (fn [_ input]
                   (resume)
                   (when (and (= hud current) input (not= input ""))
                     (set hud.query input)
                     (set hud.match nil)
                     (search-from hud.top 1)))
      :on-cancel resume})))

(fn key-help []
  (minibuffer.message
   (.. "j/k line  SPC/b page  d/u half  g/G top/bottom  / n N search  "
       "M-w copy  o Emacs  s source  q quit")
   6))

(local commands
       (let [t {}
             bind (fn [keys f] (each [_ k (ipairs keys)] (tset t k f)))]
         (bind [:j "C-n" "<down>" "RET"] #(scroll 1))
         (bind [:k "C-p" "<up>"] #(scroll -1))
         (bind ["SPC" :f "C-v" "<pagedown>"] #(page 1))
         (bind [:b "DEL" "M-v" "<pageup>"] #(page -1))
         (bind [:d] #(half-page 1))
         (bind [:u] #(half-page -1))
         (bind [:g :< "M-S-," "<home>"] #(scroll-to 1))
         (bind [:G :> "M-S-." "<end>"] #(scroll-to (max-top)))
         (bind [:q "ESC" "C-g"] close)
         (bind ["M-w"] copy)
         (bind [:o] open-in-emacs)
         (bind [:s] visit-source)
         (bind ["/"] read-search)
         (bind [:n] #(next-match 1))
         (bind [:N] #(next-match -1))
         (bind ["?"] key-help)
         t))

(fn key-of [event]
  (let [flags (: event :getFlags)
        chars (: event :getCharacters)]
    (if (and (not flags.ctrl) (not flags.alt) (not flags.cmd) chars
             (= (width chars) 1) (chars:match "^%g$"))
        chars
        (hotkeys.event-chord event))))

(fn on-key [event]
  (if (. (: event :getFlags) :cmd)
      (do (close) false)
      (let [f (. commands (key-of event))]
        (when f (f))
        true)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn show [opts]
  "
  Show help OPTS {:title str :lines [line ...] :source {:file :line}?
  :at-end bool?}, or a plain string, in the viewer. :source enables `s` to
  visit it in Emacs; :at-end starts at the bottom, like *Messages*.
  "
  (close)
  (let [opts (if (= (type opts) :string) {:lines [opts]} opts)
        frame (: (or (hs.screen.mainScreen) (hs.screen.primaryScreen)) :frame)
        cw (which-key.char-width)
        cols (math.min max-cols (math.floor (/ (- (* frame.w 0.9) (* 2 pad-x)) cw)))
        w (+ (* cols cw) (* 2 pad-x))
        ;; Leave room at the bottom for the echo area.
        max-h (- (* frame.h 0.92) (* 3 line-height) (* 2 pad-y))
        chrome (+ pad-y line-height 8 line-height 24)
        lines (expand (or opts.lines []))
        rows (let [acc []]
               (each [_ line (ipairs lines)]
                 (each [_ row (ipairs (wrap line cols))]
                   (table.insert acc row)))
               acc)
        page (math.max 1 (math.min (length rows)
                                   (math.floor (/ (- max-h chrome) line-height))))
        h (+ chrome (* page line-height))
        rect {:x (+ frame.x (/ (- frame.w w) 2))
              :y (+ frame.y (math.max 0 (/ (- max-h h) 2)) (* frame.h 0.03))
              : w : h}
        canvas (doto (hs.canvas.new rect)
                 (: :level hs.canvas.windowLevels.overlay)
                 (: :behavior [:canJoinAllSpaces :stationary]))]
    (set hud {: canvas : lines : rows : page : cols : w : h
              :top (if opts.at-end (math.max 1 (+ (- (length rows) page) 1)) 1)
              :title (or opts.title "Help")
              :source opts.source
              :tap (hs.eventtap.new [hs.eventtap.event.types.keyDown] on-key)})
    (render)
    (: canvas :show)
    (: hud.tap :start)
    nil))

{: show
 : close
 :showing (fn [] (not= hud nil))
 :current (fn [] hud)}
