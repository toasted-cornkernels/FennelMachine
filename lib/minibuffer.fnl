"
Emacs style echo area and minibuffer along the bottom of the screen, drawn like
the which-key panel.

`message` shows text in the echo area for a few seconds; while the minibuffer
is reading, it is shown after the input as ` [text]` instead.

`completing-read` reads a line with Emacs editing keys and narrows a collection
of candidates as you type, vertico/orderless style: space-separated terms match
in any order, anywhere in a candidate.

  C-n C-p TAB S-TAB <down> <up>   select the next/previous candidate
  C-v M-v                         scroll the candidates by a page
  RET                             accept (modifiers are passed on)
  C-g ESC                         quit
  C-a C-e C-b C-f M-b M-f         move
  DEL C-d M-DEL C-w M-d C-k C-u   delete and kill
  C-y s-v                         yank the last kill / paste the clipboard
"
(local which-key (require :lib.which-key))
(local {: palette : font-name : font-size : emacs-key} which-key)

(local max-rows 10)
(local pad-x 14)
(local pad-y 8)
(local echo-seconds 3)
(local flash-seconds 1.5)
(local colors {:selection "#2c2e40" :match "#bf90f5" :cursor "#bf90f5"})

(var canvas nil)
(var echo-timer nil)
(var session nil)
;; The last completing-read, for `repeat`: its prompt, collection, opts, and
;; the input and selected text it ended with.
(var last-read nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn styled [s color bg]
  (hs.styledtext.new s {:font {:name font-name :size font-size}
                        :color {:hex color}
                        :backgroundColor (when bg {:hex bg})}))

(fn right-aligned [s color]
  (hs.styledtext.new s {:font {:name font-name :size font-size}
                        :color {:hex color}
                        :paragraphStyle {:alignment :right}}))

(local line-height
       (. (hs.drawing.getTextDrawingSize "M" {:font font-name :size font-size}) :h))

(fn screen-frame []
  (which-key.bottom-frame (or (hs.screen.mainScreen) (hs.screen.primaryScreen))))

(fn max-chars []
  (math.floor (/ (- (. (screen-frame) :w) (* 2 pad-x)) (which-key.char-width))))

(fn hide []
  (when echo-timer
    (: echo-timer :stop)
    (set echo-timer nil))
  (when canvas
    (: canvas :delete)
    (set canvas nil)))

(fn draw [lines selected-line]
  "Show LINES, each {:left styled-text :right styled-text? :bg colour?}, in a full-width
  strip at the bottom of the screen, highlighting line SELECTED-LINE."
  (let [frame (screen-frame)
        h (+ (* (length lines) line-height) (* 2 pad-y))
        rect {:x frame.x :y (- (+ frame.y frame.h) h) :w frame.w : h}
        elements [{:type :rectangle :action :fill :fillColor {:hex palette.bg}}
                  {:type :rectangle :action :fill :fillColor {:hex palette.border}
                   :frame {:x 0 :y 0 :w frame.w :h 1}}]]
    (when selected-line
      (table.insert elements
                    {:type :rectangle :action :fill
                     :fillColor {:hex colors.selection}
                     :frame {:x 0 :y (+ pad-y (* (- selected-line 1) line-height))
                             :w frame.w :h line-height}}))
    (each [i line (ipairs lines)]
      (let [text-frame {:x pad-x :y (+ pad-y (* (- i 1) line-height))
                        :w (- frame.w (* 2 pad-x)) :h (+ line-height 2)}]
        (when line.bg
          (table.insert elements
                        {:type :rectangle :action :fill :fillColor {:hex line.bg}
                         :frame {:x 0 :y text-frame.y :w frame.w :h line-height}}))
        (table.insert elements {:type :text :text line.left :frame text-frame})
        (when line.right
          (table.insert elements {:type :text :text line.right :frame text-frame}))))
    (when (not canvas)
      (set canvas (doto (hs.canvas.new rect)
                    (: :level hs.canvas.windowLevels.overlay)
                    (: :behavior [:canJoinAllSpaces :stationary]))))
    (doto canvas
      (: :frame rect)
      (: :replaceElements elements)
      (: :show))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn width [s] (or (utf8.len s) (length s)))

(fn truncate [s n]
  (if (<= (width s) n) s
      (<= n 1) ""
      (.. (s:sub 1 (- (utf8.offset s n) 1)) "…")))

(fn highlighted [text terms color]
  "TEXT with the first match of each of TERMS in the match colour."
  (let [lower (text:lower)
        mask {}]
    (each [_ term (ipairs terms)]
      (let [(s e) (lower:find term 1 true)]
        (when s
          (for [k s e] (tset mask k true)))))
    (var out (styled "" color))
    (var i 1)
    (while (<= i (length text))
      (let [m (not (not (. mask i)))]
        (var j i)
        (while (and (< j (length text)) (= m (not (not (. mask (+ j 1))))))
          (set j (+ j 1)))
        (set out (.. out (styled (text:sub i j) (if m colors.match color))))
        (set i (+ j 1))))
    out))

(fn prev-char [s i]
  "Byte position of the character before position I (0 = start)."
  (if (<= i 0) 0
      (let [n (utf8.len s 1 i)]
        (if (and n (> n 1)) (- (utf8.offset s n) 1) 0))))

(fn next-char [s i]
  (if (>= i (length s)) (length s)
      (let [o (utf8.offset s 2 (+ i 1))]
        (if o (- o 1) (length s)))))

(fn word-char? [s i]
  (let [c (s:sub i i)]
    (or (c:match "[%w_]") (>= (c:byte) 128))))

(fn backward-word-pos [s i]
  (var p i)
  (while (and (> p 0) (not (word-char? s p))) (set p (- p 1)))
  (while (and (> p 0) (word-char? s p)) (set p (- p 1)))
  p)

(fn forward-word-pos [s i]
  (var p i)
  (while (and (< p (length s)) (not (word-char? s (+ p 1)))) (set p (+ p 1)))
  (while (and (< p (length s)) (word-char? s (+ p 1))) (set p (+ p 1)))
  p)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn terms-of [input]
  (icollect [term (string.gmatch (input:lower) "%S+")] term))

(fn score [candidate terms]
  "Lower is better: 0 per term matching at the start, 1 at a word start, 2
  elsewhere. nil when a term does not match at all."
  (var total 0)
  (each [_ term (ipairs terms) &until (not total)]
    (let [pos (candidate.haystack:find term 1 true)]
      (set total
           (if (not pos) nil
               (= pos 1) total
               (: (candidate.haystack:sub (- pos 1) (- pos 1)) :match "[%w_]")
               (+ total 2)
               (+ total 1)))))
  total)

(fn normalize [candidate]
  (set candidate.text (candidate.text:gsub "%s+" " "))
  (set candidate.haystack
       (: (if candidate.keywords (.. candidate.text " " candidate.keywords)
              candidate.text) :lower))
  candidate)

(fn dynamic-candidates []
  "The candidates computed from the input by the session's :dynamic function."
  (if session.dynamic
      (let [(ok result) (pcall session.dynamic session.input)]
        (if (and ok result)
            (do
              (each [_ key (ipairs [:before :after :only])]
                (each [_ c (ipairs (or (. result key) []))]
                  (normalize c)))
              result)
            (do
              (when (not ok) (print (.. "minibuffer dynamic: " (tostring result))))
              {})))
      {}))

(fn refilter []
  (let [terms (terms-of session.input)
        exact (: (session.input:lower) :match "^%s*(.-)%s*$")
        dynamic (dynamic-candidates)
        scored (if dynamic.only []
                   (icollect [i c (ipairs (or session.collection []))]
                     (let [s (score c terms)
                           ;; A candidate that is exactly the input goes first.
                           s (if (and s (= (c.text:lower) exact)) -1 s)]
                       (when s {: c : s : i}))))
        matches (or dynamic.only (icollect [_ c (ipairs (or dynamic.before []))] c))]
    (table.sort scored #(if (= $1.s $2.s) (< $1.i $2.i) (< $1.s $2.s)))
    (each [_ x (ipairs scored)] (table.insert matches x.c))
    (when (not dynamic.only)
      (each [_ c (ipairs (or dynamic.after []))] (table.insert matches c)))
    (set session.terms terms)
    (set session.matches matches)
    (set session.selected 1)
    (set session.offset 0)))

(fn select [index]
  (let [n (length session.matches)]
    (when (> n 0)
      (set session.selected (+ 1 (% (- index 1) n)))
      (if (< session.selected (+ session.offset 1))
          (set session.offset (- session.selected 1))
          (> session.selected (+ session.offset max-rows))
          (set session.offset (- session.selected max-rows))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering the minibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn prompt-line []
  (let [{: input : point : matches : selected} session
        count (if session.view ""
                  session.collection
                  (string.format "%d/%d " (if (> (length matches) 0) selected 0)
                                 (length matches))
                  "…/… ")
        suffix (when session.suffix
                 (let [(ok text) (pcall session.suffix input)]
                   (if ok text (do (print (.. "minibuffer suffix: " (tostring text))) nil))))
        after (input:sub (+ point 1))
        cursor-end (next-char input point)
        under (if (= point (length input)) " " (input:sub (+ point 1) cursor-end))]
    (.. (styled count palette.dim)
        (styled session.prompt palette.prefix)
        (styled (input:sub 1 point) palette.fg)
        (styled under palette.bg colors.cursor)
        (styled (if (= point (length input)) "" (after:sub (+ (length under) 1)))
                palette.fg)
        (if suffix (.. (styled "  " palette.fg) (styled suffix palette.fg colors.selection))
            (styled "" palette.fg))
        (if session.flash (styled (.. " [" session.flash "]") palette.dim)
            (styled "" palette.fg)))))

(fn render-view []
  (let [(ok lines) (pcall session.view (prompt-line))]
    (if ok
        (draw lines nil)
        (do (print (.. "minibuffer view: " (tostring lines)))
            (draw [{:left (prompt-line)}] nil)))))

(fn render-candidates []
  (let [chars (max-chars)
        lines [{:left (prompt-line)}]]
    (for [i (+ session.offset 1)
          (math.min (length session.matches) (+ session.offset max-rows))]
      (let [c (. session.matches i)
            annotation (or c.annotation "")
            room (- chars (width annotation) 2)]
        (table.insert lines
                      {:left (highlighted (truncate c.text room) session.terms
                                          palette.fg)
                       :right (right-aligned annotation palette.dim)})))
    (draw lines (when (> (length session.matches) 0)
                  (+ 1 (- session.selected session.offset))))))

(fn render []
  (if session.view (render-view) (render-candidates))
  (when session.on-change
    (let [(ok err) (pcall session.on-change session.matches
                          (. session.matches session.selected) session.input)]
      (when (not ok) (print (.. "minibuffer on-change: " (tostring err)))))))

(fn flash [text]
  (set session.flash text)
  (when session.flash-timer (: session.flash-timer :stop))
  (let [s session]
    (set session.flash-timer
         (hs.timer.doAfter flash-seconds
                           #(when (= session s)
                              (set s.flash nil)
                              (render))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn set-input [input point]
  (let [changed (not= input session.input)]
    (set session.input input)
    (set session.point point)
    (when changed (refilter))))

(fn insert [text]
  (let [text (text:gsub "[%c]" " ")
        {: input : point} session]
    (set-input (.. (input:sub 1 point) text (input:sub (+ point 1)))
               (+ point (length text)))))

(fn delete-region [from to kill?]
  "Delete input bytes FROM+1..TO, saving them as the last kill if KILL?."
  (let [{: input} session
        from (math.max 0 from)
        to (math.min (length input) to)]
    (when (< from to)
      (when kill? (set session.kill (input:sub (+ from 1) to)))
      (set-input (.. (input:sub 1 from) (input:sub (+ to 1))) from))))

(fn move [point] (set session.point point))

(local commands
       {"C-a" #(move 0)
        :home #(move 0)
        "C-e" #(move (length session.input))
        :end #(move (length session.input))
        "C-b" #(move (prev-char session.input session.point))
        "<left>" #(move (prev-char session.input session.point))
        "C-f" #(move (next-char session.input session.point))
        "<right>" #(move (next-char session.input session.point))
        "M-b" #(move (backward-word-pos session.input session.point))
        "M-f" #(move (forward-word-pos session.input session.point))
        "DEL" #(delete-region (prev-char session.input session.point) session.point)
        "C-d" #(delete-region session.point (next-char session.input session.point))
        :forwarddelete #(delete-region session.point
                                       (next-char session.input session.point))
        "M-DEL" #(delete-region (backward-word-pos session.input session.point)
                                session.point true)
        "C-w" #(delete-region (backward-word-pos session.input session.point)
                              session.point true)
        "M-d" #(delete-region session.point
                              (forward-word-pos session.input session.point) true)
        "C-k" #(delete-region session.point (length session.input) true)
        "C-u" #(delete-region 0 session.point true)
        "C-y" #(insert (or session.kill (hs.pasteboard.getContents) ""))
        "C-n" #(select (+ session.selected 1))
        "<down>" #(select (+ session.selected 1))
        "TAB" #(select (+ session.selected 1))
        "C-p" #(select (- session.selected 1))
        "<up>" #(select (- session.selected 1))
        "S-TAB" #(select (- session.selected 1))
        "C-v" #(select (math.min (length session.matches)
                                 (+ session.selected max-rows)))
        :pagedown #(select (math.min (length session.matches)
                                     (+ session.selected max-rows)))
        "M-v" #(select (math.max 1 (- session.selected max-rows)))
        :pageup #(select (math.max 1 (- session.selected max-rows)))})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sessions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn close []
  "End the current minibuffer session, returning it."
  (let [s session]
    (when (and s s.repeat)
      (set s.repeat.collection s.collection)
      (set s.repeat.input s.input)
      (let [c (. s.matches s.selected)]
        (set s.repeat.selected (and c c.text))))
    (when s
      (: s.tap :stop)
      (when s.flash-timer (: s.flash-timer :stop))
      (set session nil)
      (hide))
    s))

(fn cancel []
  (let [s (close)]
    (when (and s s.on-cancel) (s.on-cancel))))

(fn accept [flags]
  (let [s (close)]
    (when s
      (s.on-accept (. s.matches s.selected) s.input flags))))

(fn mods-of [flags]
  (icollect [_ m (ipairs [:ctrl :alt :shift :cmd])]
    (when (. flags m) m)))

(fn run-action [action]
  "Close the session and call ACTION with the selected candidate and the input."
  (let [s (close)]
    (when s
      (action (. s.matches s.selected) s.input))))

(fn on-key [event]
  (let [key (. hs.keycodes.map (: event :getKeyCode))
        flags (: event :getFlags)
        chars (: event :getCharacters)
        autorepeat? (= 1 (: event :getProperty
                            hs.eventtap.event.properties.keyboardEventAutorepeat))
        desc (and key (emacs-key (mods-of flags) key))
        command (. commands desc)
        action (and desc session.actions (. session.actions desc))
        key-fn (and desc session.keys
                    (if (= (type session.keys) :function)
                        #(session.keys desc chars)
                        (. session.keys desc)))]
    (if action (do (run-action action) true)
        ;; A session key that declines (returns false) gets the usual meaning.
        (and key-fn (key-fn)) (do (render) true)
        (and flags.cmd (= key :v))
        (do (insert (or (hs.pasteboard.getContents) "")) (render) true)
        (or (= key :return) (= key :padenter)) (do (accept flags) true)
        ;; Other Cmd shortcuts belong to the system, e.g. Cmd-Tab.
        flags.cmd (do (cancel) false)
        (or (= key :escape) (= desc "C-g")) (do (cancel) true)
        command (do (command) (render) true)
        (or flags.ctrl flags.alt)
        (do
          ;; Ignore the auto-repeat of a still-held hotkey.
          (when (not autorepeat?)
            (flash (.. desc " is undefined"))
            (render))
          true)
        (and chars (not= chars "") (not (chars:match "%c")))
        (do (insert chars) (render) true)
        true)))

(fn completing-read [prompt collection opts]
  "Read a line in the minibuffer, narrowing COLLECTION as you type.

  COLLECTION is a list of {:text :annotation :keywords} tables, where the
  optional :keywords also match but are not shown (other keys are kept), or
  nil while it is still being gathered; see `set-collection`. OPTS:
    :on-accept (fn [candidate input flags])  RET, with the selected candidate
                                              (nil without matches)
    :on-cancel (fn [])                        C-g / ESC
    :on-change (fn [matches selected input])  after every redisplay
    :dynamic (fn [input])                     candidates computed from the input:
                                              {:before [..] :after [..]} around the
                                              matches of COLLECTION, or {:only [..]}
                                              instead of them; not filtered
    :actions {key (fn [candidate input])}     extra keys such as s-l that end the read
                                              and act on the selection
    :keys {key (fn [])}                       extra keys that keep reading, e.g. to
                                              move a calendar; return false to give
                                              the key its usual meaning. Or one
                                              (fn [key chars]) for every key
    :view (fn [prompt-line])                  the lines ({:left :right} styled text,
                                              :bg a line's background colour)
                                              to show instead of the candidates;
                                              include PROMPT-LINE, the input line
    :suffix (fn [input])                      text shown highlighted after the input,
                                              e.g. how it is read
    :no-repeat true                           leave it out of `repeat`"
  (close)
  (hide)
  (set session {: prompt
                :input ""
                :point 0
                :matches []
                :selected 1
                :offset 0
                :terms []
                :on-accept (or opts.on-accept #nil)
                :on-cancel opts.on-cancel
                :on-change opts.on-change
                :dynamic opts.dynamic
                :actions opts.actions
                :keys opts.keys
                :view opts.view
                :suffix opts.suffix
                :tap (hs.eventtap.new [hs.eventtap.event.types.keyDown] on-key)})
  (when (not opts.no-repeat)
    (set last-read {: prompt : opts})
    (set session.repeat last-read))
  (when collection
    (set session.collection (icollect [_ c (ipairs collection)] (normalize c))))
  (refilter)
  (render)
  (: session.tap :start))

(fn set-collection [collection]
  "Supply the collection of the active `completing-read`."
  (when session
    (each [_ c (ipairs collection)] (normalize c))
    (set session.collection collection)
    (refilter)
    (render)))

(fn refresh []
  "Recompute the candidates of the active `completing-read`, e.g. when results
  of its :dynamic function arrive asynchronously. The selection stays on the
  same candidate when it is still there."
  (when session
    (let [selected (. session.matches session.selected)]
      (refilter)
      (each [i c (ipairs session.matches)]
        (when (= c selected) (select i))))
    (render)))

(fn message [text seconds]
  "Show TEXT in the echo area, like Emacs' `message`. It is also printed to
  the Hammerspoon console, which plays the part of *Messages*."
  (let [text (tostring text)]
    (print (.. "message: " text))
    (if session
        (do (flash text) (render))
        (do
          (draw (icollect [line (string.gmatch (.. text "\n") "(.-)\n")]
                  {:left (styled (truncate line (max-chars)) palette.fg)}))
          (when echo-timer (: echo-timer :stop))
          (set echo-timer (hs.timer.doAfter (or seconds echo-seconds) hide))))))

(fn y-or-n-p [prompt on-yes on-no]
  "Ask PROMPT in the minibuffer and call ON-YES after y, or ON-NO (if any)
  after n, C-g or ESC."
  (let [no #(if on-no (on-no) (message "Quit"))]
    (completing-read (.. prompt " (y or n) ") []
                     {:actions {:y #(on-yes) :n no}
                      :no-repeat true
                      :on-accept no
                      :on-cancel no})))

(fn repeat []
  "Reopen the last minibuffer session with its input and selection, like
  vertico-repeat."
  (if (not last-read)
      (message "No earlier minibuffer session")
      (let [{: prompt : collection : opts : input : selected} last-read]
        (completing-read prompt collection opts)
        (when (and input (not= input ""))
          (set-input input (length input)))
        (let [i (and selected
                     (accumulate [found nil i c (ipairs session.matches)]
                       (or found (when (= c.text selected) i))))]
          (when i (select i)))
        (render))))

{: message
 :clear (fn [] "Clear the echo area unless the minibuffer is reading."
          (when (not session) (hide)))
 : completing-read
 : repeat
 : max-chars
 :set-input (fn [input] "Replace the input of the active `completing-read`."
              (when session
                (set-input input (length input))
                (render)))
 : set-collection
 : refresh
 : y-or-n-p
 :active? (fn [] (not= session nil))
 :abort cancel
 :current (fn [] {: canvas : session})}
