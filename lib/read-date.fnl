"
An Emacs calendar and org-read-date style date picker in the minibuffer, with
evil states: three months of the calendar above a `Date+time: <2026-11-20 Fri
16:05 EST>` prompt.

Normal state (a count such as 3 repeats a key):
  h l / j k          a day back / forward, a week forward / back (as drawn)
  s-h s-k / s-j s-l  a month back / forward
  H J / K L          a year back / forward
  M-h M-j / M-k M-l  an hour back / forward
  M-H M-J / M-K M-L  30 minutes back / forward
  C-h C-j / C-k C-l  a minute back / forward
  .                  today
  i a                insert state, to type the date, time or zone
  RET  ESC           accept, quit

Insert state reads the input like org-read-date, e.g. `fri 4pm`, `12/25 16:00
EST`, `+2d Seoul`, showing how as `=> <2026-12-25 Fri 16:00 EST>`; ESC applies
it and returns to normal state, RET accepts it.
"
(local minibuffer (require :lib.minibuffer))
(local tz (require :lib.tz))
(local which-key (require :lib.which-key))
(local {: palette : font-name : font-size} which-key)

(local colors {:cursor "#bf90f5" :read "#2c2e40" :tag "#77a2f9"})
(local month-names ["January" "February" "March" "April" "May" "June" "July"
                    "August" "September" "October" "November" "December"])
(local day-names ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
                  "Saturday"])
(local short-days ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn add-months [days n]
  "DAYS moved N months, keeping the day of the month where it exists (Jan 31
  + 1 month is Feb 28 or 29), as calendar-forward-month does."
  (let [(y m d) (tz.civil-from-days days)
        index (+ (* y 12) (- m 1) n)
        y (// index 12)
        m (+ (% index 12) 1)]
    (tz.days-from-civil y m (math.min d (tz.days-in-month y m)))))

(fn month-of [word]
  "1-12 for a month name or its abbreviation (3 letters or more), else nil."
  (when (>= (length word) 3)
    (accumulate [found nil i name (ipairs month-names)]
      (or found (when (= (: (name:lower) :sub 1 (length word)) word) i)))))

(fn weekday-of [word]
  "0-6 (Sunday-Saturday) for a weekday name or its abbreviation, else nil."
  (when (>= (length word) 2)
    (accumulate [found nil i name (ipairs day-names)]
      (or found (when (= (: (name:lower) :sub 1 (length word)) word) (- i 1))))))

(fn stamp [c ?zone?]
  "2026-12-25 Fri 16:00, with the zone's abbreviation if ?ZONE?."
  (.. (string.format "%04d-%02d-%02d %s %02d:%02d" c.year c.month c.day
                     (. short-days (+ c.wday 1)) c.hour c.min)
      (if ?zone? (.. " " c.abbr) "")))

(fn long-date [days]
  "Friday, November 20, 2026"
  (let [(y m d) (tz.civil-from-days days)]
    (string.format "%s, %s %d, %d" (. day-names (+ (tz.weekday days) 1))
                   (. month-names m) d y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading the input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local fillers {:at true :on true :in true :the true :of true :from true})

(fn take-zone [s]
  "The zone named by up to three consecutive words of S, and S without them."
  (let [words (icollect [w (s:gmatch "%S+")] w)]
    (var found nil)
    (for [n (math.min 3 (length words)) 1 -1 &until found]
      (for [i 1 (+ (- (length words) n) 1) &until found]
        (let [phrase (table.concat words " " i (+ i n -1))]
          (when (and (phrase:match "^[%a][%a%s/_%-]*$")
                     (not (month-of phrase)) (not (weekday-of phrase))
                     (not (. fillers phrase)))
            (let [zone (tz.lookup phrase)]
              (when zone
                (set found zone)
                (for [_ 1 n] (table.remove words i))))))))
    (values found (table.concat words " "))))

(fn parse [input ctx]
  "Read INPUT like org-read-date. CTX: {:cursor :hour :min :zone :now
  :touched?}, the picker's date (days), time and zone, the current instant and
  whether the picker was changed (without :hour and :min, the current time).
  Returns {:days :hour :min :zone :rest}, REST being what could not be read."
  (let [(typed-zone text) (take-zone (: (input:lower) :gsub "," " "))
        zone (or typed-zone ctx.zone)
        now (tz.components zone ctx.now)
        today now.days
        ;; A zone alone on an untouched picker is the current time there.
        here-now? (and typed-zone (not ctx.touched?))]
    (var s (.. " " text " "))
    (var days nil)
    (var base ctx.cursor)
    (var hour nil)
    (var min nil)
    (fn take [pattern]
      "Remove the first match of PATTERN from s and return its captures."
      (let [found [(s:find pattern)]]
        (when (. found 1)
          (set s (.. (s:sub 1 (- (. found 1) 1)) " " (s:sub (+ (. found 2) 1))))
          (if (> (length found) 2) [(table.unpack found 3)] [true]))))
    (fn year-of [y]
      (let [y (tonumber y)]
        (if (not y) nil (< y 100) (+ 2000 y) y)))
    (fn ymd [y m d]
      "The date Y-M-D; without Y, the next one from today (org's prefer-future)."
      (let [m (tonumber m) d (tonumber d)]
        (when (and m d (<= 1 m 12) (>= d 1))
          (let [(this-year) (tz.civil-from-days today)
                year (or y this-year)
                date (tz.days-from-civil year m (math.min d (tz.days-in-month year m)))]
            (if (and (not y) (< date today))
                (tz.days-from-civil (+ year 1) m (math.min d (tz.days-in-month (+ year 1) m)))
                date)))))
    (fn set-time [h m ampm]
      (let [h (tonumber h) m (tonumber (or m 0))]
        (when (and h (<= 0 h 23) (<= 0 m 59))
          (set hour (if (= ampm "p") (+ (% h 12) 12) (= ampm "a") (% h 12) h))
          (set min m))))
    ;; Time
    (match (take "%f[%d](%d%d?):(%d%d)%s*([ap])%.?m?%.?%f[%s]")
      [h m ap] (set-time h m ap)
      _ (match (take "%f[%d](%d%d?):(%d%d)%f[%D]")
          [h m] (set-time h m)
          _ (match (take "%f[%d](%d%d?)%s*([ap])%.?m?%.?%f[%s]")
              [h ap] (set-time h 0 ap)
              _ (if (take "%f[%a]noon%f[%A]") (set-time 12 0)
                    (take "%f[%a]midnight%f[%A]") (set-time 0 0)))))
    (when (take "%f[%a]now%f[%A]")
      (set days today)
      (when (not hour) (set-time now.hour now.min)))
    ;; Dates
    (match (take "%f[%d](%d%d%d%d)%-(%d%d?)%-(%d%d?)%f[%D]")
      [y m d] (set days (ymd (tonumber y) m d))
      _ (match (take "%f[%d](%d%d?)/(%d%d?)/(%d%d%d?%d?)%f[%D]")
          [m d y] (set days (ymd (year-of y) m d))
          _ (match (take "%f[%d](%d%d?)/(%d%d?)%f[%D]")
              [m d] (set days (ymd nil m d))
              _ (match (take "%f[%d](%d%d?)%.(%d%d?)%.(%d*)")
                  [d m y] (set days (ymd (year-of y) m d))))))
    (when (not days)
      (let [year (match (take "%f[%d](%d%d%d%d)%f[%D]") [y] (tonumber y))
            named (fn [m d]
                    (let [mo (month-of m)]
                      (when mo (ymd year mo d))))]
        (each [word day (s:gmatch "(%a+)%s+(%d%d?)%f[%D]") &until days]
          (let [date (named word day)]
            (when date (set days date) (take (.. word "%s+" day "%f[%D]")))))
        (each [day word (s:gmatch "%f[%d](%d%d?)%s+(%a+)") &until days]
          (let [date (named word day)]
            (when date (set days date) (take (.. "%f[%d]" day "%s+" word "%f[%A]")))))
        (each [word (s:gmatch "%a+") &until days]
          (let [mo (month-of word)]
            (when mo
              (let [(_ _ d) (tz.civil-from-days base)]
                (set days (ymd year mo d))
                (take (.. "%f[%a]" word "%f[%A]"))))))
        (when (and year (not days))
          (let [(_ m d) (tz.civil-from-days base)]
            (set days (tz.days-from-civil year m (math.min d (tz.days-in-month year m))))))))
    (each [_ word (ipairs [:today :tomorrow :yesterday])]
      (when (take (.. "%f[%a]" word "%f[%A]"))
        (set days (+ today (match word :today 0 :tomorrow 1 :yesterday -1)))))
    ;; Relative dates: +3d +2w -1m +1y +2tue from today, ++3d from the calendar.
    (match (take "%f[%S]([%+%-][%+%-]?)(%d*)(%a*)%f[%s]")
      [sign n unit]
      (let [back? (= (sign:sub 1 1) "-")
            k (or (tonumber n) (if (= unit "") 0 1))
            n (if back? (- k) k)
            from (if (= (length sign) 2) base (or days today))
            wd (weekday-of unit)]
        (set days
             (if (or (= unit "") (= unit "d")) (+ from n)
                 (= unit "w") (+ from (* 7 n))
                 (= unit "m") (add-months from n)
                 (= unit "y") (add-months from (* 12 n))
                 ;; As org: +tue is the next Tuesday after today, -tue the last.
                 wd (let [ahead (% (- wd (tz.weekday from)) 7)
                          delta (if (= ahead 0) 7 ahead)
                          delta (if back? (let [d (- delta 7)] (if (= d 0) -7 d)) delta)]
                      (+ from delta (* (math.max 0 (- k 1)) (if back? -7 7))))
                 from))))
    (when (take "%f[%S]%.%f[%s]")
      (set days today))
    (each [word (s:gmatch "%a+") &until days]
      (let [wd (weekday-of word)]
        (when wd
          (set days (+ today (% (- wd (tz.weekday today)) 7)))
          (take (.. "%f[%a]" word "%f[%A]")))))
    (when (not days)
      (match (take "%f[%d](%d%d?)%f[%D]")
        [d] (let [d (tonumber d)
                  (y m) (tz.civil-from-days base)
                  date (when (<= 1 d (tz.days-in-month y m))
                         (tz.days-from-civil y m d))]
              ;; From an untouched calendar, a day that has passed is next month's.
              (set days (if (and date (= base ctx.today) (< date today))
                            (let [next (add-months (tz.days-from-civil y m 1) 1)
                                  (ny nm) (tz.civil-from-days next)]
                              (when (<= d (tz.days-in-month ny nm))
                                (tz.days-from-civil ny nm d)))
                            date)))))
    (each [word (pairs fillers)]
      (take (.. "%f[%a]" word "%f[%A]")))
    (let [rest (-> s (: :gsub "[,%s]+" " ") (: :match "^%s*(.-)%s*$"))]
      {:days (or days (if here-now? today ctx.cursor))
       :hour (or hour (if (or here-now? (not ctx.hour)) now.hour ctx.hour))
       :min (or min (if (or here-now? (not ctx.min)) now.min ctx.min))
       : zone
       :time? (not= hour nil)
       :rest (when (not= rest "") rest)})))

(fn resolve [read]
  "The instant and wall clock components of a `parse` result."
  (let [wall (+ (* read.days 86400) (* read.hour 3600) (* read.min 60))
        instant (tz.wall->instant read.zone wall)]
    {: instant :zone read.zone :wall (tz.components read.zone instant)}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calendar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn style [s color ?bg ?underline]
  (hs.styledtext.new s {:font {:name font-name :size font-size}
                        :color {:hex color}
                        :backgroundColor (when ?bg {:hex ?bg})
                        :underlineStyle (when ?underline 1)}))

(fn month-lines [y m state]
  "The 8 lines (title, weekdays, 6 weeks) of month Y-M, each 20 columns."
  (let [first (tz.days-from-civil y m 1)
        lead (tz.weekday first)
        n (tz.days-in-month y m)
        title (.. (. month-names m) " " y)
        left (// (- 20 (length title)) 2)
        lines [(style (.. (string.rep " " left) title
                          (string.rep " " (- 20 left (length title))))
                      palette.key)
               (.. (style "Su" palette.dim) (style " Mo Tu We Th Fr " palette.prefix)
                   (style "Sa" palette.dim))]]
    (for [week 0 5]
      (var line (style "" palette.fg))
      (for [col 0 6]
        (let [d (+ (* week 7) col (- lead) 1)
              days (+ first d -1)
              sep (if (= col 0) "" " ")]
          (set line
               (.. line (style sep palette.fg)
                   (if (or (< d 1) (> d n))
                       (style "  " palette.fg)
                       (= days state.cursor)
                       (style (string.format "%2d" d) palette.bg colors.cursor
                              (= days state.today))
                       (= days state.read)
                       (style (string.format "%2d" d) palette.fg colors.read
                              (= days state.today))
                       (style (string.format "%2d" d) palette.fg nil
                              (= days state.today)))))))
      (table.insert lines line))
    lines))

(fn hints [state]
  (if state.insert?
      (.. "type a date, time or zone: 16:00  4pm  fri  12/25  dec 25  +2d  ++1w  "
          "EST  Seoul  ·  ESC calendar  RET accept")
      (.. "h l day  j k week  s-h s-l month  H L year  M-h M-l hour  "
          "M-H M-L 30 min  C-h C-l minute  . today  "
          "i type  RET accept  ESC quit")))

(fn normal-prompt [state prompt]
  "The prompt line of normal state: the picked time as a block, the count."
  (let [{: session} (minibuffer.current)
        flash (and session session.flash)]
    (.. (style prompt palette.prefix)
        (style (.. "<" state.text ">") palette.bg colors.cursor)
        (style (if state.count (.. "  " state.count "-") "") palette.dim)
        (style (if flash (.. " [" flash "]") "") palette.dim))))

(fn calendar-lines [state prompt prompt-line]
  (let [chars (minibuffer.max-chars)
        width 66
        pad (string.rep " " (math.max 0 (// (- chars width) 2)))
        months (icollect [_ k (ipairs [-1 0 1])]
                 (let [(y m) (tz.civil-from-days (add-months state.view k))]
                   (month-lines y m state)))
        lines []]
    (for [i 1 8]
      (table.insert lines
                    {:left (.. (style pad palette.fg)
                               (. months 1 i) (style "   " palette.fg)
                               (. months 2 i) (style "   " palette.fg)
                               (. months 3 i))}))
    (table.insert lines
                  {:left (.. (style (if state.insert? " <I> " " <N> ") colors.tag)
                             (style (.. "  " (hints state)) palette.fg))
                   :right (hs.styledtext.new (.. (long-date state.cursor) "  > ")
                                             {:font {:name font-name :size font-size}
                                              :color {:hex palette.fg}
                                              :paragraphStyle {:alignment :right}})
                   :bg palette.border})
    (table.insert lines {:left (if state.insert? prompt-line
                                   (normal-prompt state prompt))})
    lines))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading a date
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn read-date [opts]
  "Read a date and time in the minibuffer with a calendar, like org-read-date
  with evil keys. OPTS: :prompt (default \"Date+time\"), :zone (default the
  local zone), :on-accept (fn [result]) with {:instant :zone :wall :text},
  :on-cancel."
  (let [now (os.time)
        prompt (.. (or opts.prompt "Date+time") ": ")
        zone (or opts.zone (tz.local-zone))
        start (tz.components zone now)
        state {:cursor start.days :view start.days :today start.days
               :minute (+ (* start.hour 60) start.min) : zone
               :touched? false :insert? false :count nil :read nil :text ""}]
    (fn ctx []
      {:cursor state.cursor :hour (// state.minute 60) :min (% state.minute 60)
       :zone state.zone : now :touched? state.touched?})
    (fn current [input]
      (let [read (parse input (ctx))
            result (resolve read)]
        (set result.text (stamp result.wall true))
        (set result.read read)
        result))
    (fn move [days]
      (set state.cursor days)
      (set state.touched? true)
      ;; Recenter when the cursor leaves the three months shown.
      (let [(cy cm) (tz.civil-from-days days)
            (vy vm) (tz.civil-from-days state.view)]
        (when (> (math.abs (- (+ (* cy 12) cm) (+ (* vy 12) vm))) 1)
          (set state.view days))))
    (fn shift-minutes [n]
      (let [total (+ (* state.cursor 1440) state.minute n)]
        (set state.minute (% total 1440))
        (move (// total 1440))))
    (fn commit []
      "Apply the input to the picker and clear it."
      (let [{: session} (minibuffer.current)
            input (if session session.input "")]
        (when (not= (input:match "^%s*(.-)%s*$") "")
          (let [{: read} (current input)]
            (move read.days)
            (set state.minute (+ (* read.hour 60) read.min))
            (set state.zone read.zone)
            (set state.today (. (tz.components read.zone now) :days))
            (minibuffer.set-input "")
            (when read.rest (minibuffer.message (.. "can't read: " read.rest)))))))
    (let [day #(move (+ state.cursor $1))
          week #(move (+ state.cursor (* 7 $1)))
          month #(move (add-months state.cursor $1))
          year #(move (add-months state.cursor (* 12 $1)))
          hour #(shift-minutes (* 60 $1))
          half-hour #(shift-minutes (* 30 $1))
          minute #(shift-minutes $1)
          insert #(set state.insert? true)
          normal {:h #(day (- $1)) :l #(day $1) :j #(week $1) :k #(week (- $1))
                  "<left>" #(day (- $1)) "<right>" #(day $1)
                  "<down>" #(week $1) "<up>" #(week (- $1))
                  :s-h #(month (- $1)) :s-k #(month (- $1))
                  :s-j #(month $1) :s-l #(month $1)
                  :H #(year (- $1)) :J #(year (- $1)) :K #(year $1) :L #(year $1)
                  :M-h #(hour (- $1)) :M-j #(hour (- $1)) :M-k #(hour $1) :M-l #(hour $1)
                  :M-H #(half-hour (- $1)) :M-J #(half-hour (- $1))
                  :M-K #(half-hour $1) :M-L #(half-hour $1)
                  :C-h #(minute (- $1)) :C-j #(minute (- $1))
                  :C-k #(minute $1) :C-l #(minute $1)
                  "." #(move state.today)
                  :i insert :a insert :I insert :A insert}]
      (fn normal-key [desc chars]
        (let [f (. normal desc)
              digit (and chars (chars:match "^%d$"))]
          (if f (do (f (or state.count 1)) (set state.count nil) true)
              (and digit (or state.count (not= digit "0")))
              (do (set state.count (math.min 9999 (+ (* 10 (or state.count 0))
                                                     (tonumber digit))))
                  true)
              (and (= desc "ESC") state.count) (do (set state.count nil) true)
              ;; Pasting types the date.
              (= desc "s-v") (do (set state.insert? true) false)
              ;; RET, ESC, C-g and the other modified keys keep their meaning.
              (or (= desc "RET") (= desc "ESC") (desc:match "^[CMs]%-")) false
              (do (minibuffer.message (.. desc " is undefined, i types")) true))))
      (minibuffer.completing-read
       prompt nil
       {:keys (fn [desc chars]
                (if (not state.insert?) (normal-key desc chars)
                    (= desc "ESC") (do (commit) (set state.insert? false) true)
                    false))
        :suffix (fn [input]
                  (let [result (current input)]
                    (set state.read (when (and state.insert? (not= input ""))
                                      result.read.days))
                    (set state.text (. (current "") :text))
                    (.. "=> <" result.text ">"
                        (if result.read.rest (.. "  can't read: " result.read.rest)
                            ""))))
        :view #(calendar-lines state prompt $1)
        :on-accept (fn [_ input]
                     (when opts.on-accept (opts.on-accept (current input))))
        :on-cancel opts.on-cancel}))))

{: read-date
 : parse
 : resolve
 : stamp
 : add-months}
