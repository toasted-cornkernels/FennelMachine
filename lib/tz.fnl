"
Time zones from the system's tz database, read from its TZif files instead of
shelling out to date(1): the UTC offset and abbreviation of a zone at any
instant (DST included), wall clock time in a zone to UTC, and zone names.

Instants are seconds since the epoch (UTC). Days are days since 1970-01-01.
"

(local dir
       (let [dirs ["/var/db/timezone/zoneinfo" "/usr/share/zoneinfo"]]
         (or (accumulate [found nil _ d (ipairs dirs)]
               (or found (let [f (io.open (.. d "/zone.tab"))]
                           (when f (f:close) d))))
             (. dirs 2))))

;; 28 Gregorian years (in 1901-2099): the weekdays repeat, so DST rules do too.
(local cycle (* 10227 86400))
(local nul (string.char 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Civil dates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn days-from-civil [y m d]
  "Days since 1970-01-01 of the date Y-M-D (proleptic Gregorian)."
  (let [y (if (<= m 2) (- y 1) y)
        era (// y 400)
        yoe (- y (* era 400))
        doy (+ (// (+ (* 153 (% (+ m 9) 12)) 2) 5) (- d 1))
        doe (+ (* yoe 365) (// yoe 4) (- (// yoe 100)) doy)]
    (+ (* era 146097) doe -719468)))

(fn civil-from-days [days]
  "The year, month and day of DAYS since 1970-01-01."
  (let [z (+ days 719468)
        era (// z 146097)
        doe (- z (* era 146097))
        yoe (// (- (+ (- doe (// doe 1460)) (// doe 36524)) (// doe 146096)) 365)
        doy (- doe (- (+ (* 365 yoe) (// yoe 4)) (// yoe 100)))
        mp (// (+ (* 5 doy) 2) 153)
        d (+ (- doy (// (+ (* 153 mp) 2) 5)) 1)
        m (if (< mp 10) (+ mp 3) (- mp 9))
        y (+ yoe (* era 400))]
    (values (if (<= m 2) (+ y 1) y) m d)))

(fn weekday [days]
  "0 for Sunday to 6 for Saturday."
  (% (+ days 4) 7))

(fn days-in-month [y m]
  (- (days-from-civil (if (= m 12) (+ y 1) y) (+ (% m 12) 1) 1)
     (days-from-civil y m 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TZif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn parse [data]
  "The transitions and local time types of TZif DATA (version 2+ data block
  when present), and whether its footer has a DST rule."
  (when (= (data:sub 1 4) "TZif")
    (let [counts ">I4I4I4I4I4I4"
          (isutc isstd leap times types chars) (string.unpack counts data 21)
          v2? (not= (data:sub 5 5) nul)
          header (+ 45 (* times 5) (* types 6) chars (* leap 8) isstd isutc)
          (isutc isstd leap times types chars)
          (if v2?
              (string.unpack counts data (+ header 20))
              (values isutc isstd leap times types chars))
          size (if v2? 8 4)
          fmt (if v2? ">i8" ">i4")
          transitions []
          indices []
          infos []]
      (var pos (if v2? (+ header 44) 45))
      (for [i 1 times]
        (tset transitions i (string.unpack fmt data pos))
        (set pos (+ pos size)))
      (for [i 1 times]
        (tset indices i (+ 1 (string.unpack "B" data pos)))
        (set pos (+ pos 1)))
      (let [abbrs (data:sub (+ pos (* types 6)) (+ pos (* types 6) chars -1))]
        (for [i 1 types]
          (let [(offset dst idx) (string.unpack ">i4BB" data pos)
                stop (or (abbrs:find nul (+ idx 1) true) (+ (length abbrs) 1))]
            (tset infos i {: offset :dst? (= dst 1) :abbr (abbrs:sub (+ idx 1) (- stop 1))})
            (set pos (+ pos 6)))))
      (set pos (+ pos chars (* leap (+ size 4)) isstd isutc))
      (let [footer (and v2? (data:match "^\n([^\n]*)\n" pos))]
        {: transitions
         : indices
         : infos
         :rule? (not (not (and footer (footer:find "," 1 true))))}))))

(local cache {})

(fn load [zone]
  "The parsed TZif file of ZONE (e.g. Europe/Berlin), or nil."
  (when (and zone (not (zone:find ".." 1 true)) (not (zone:find "^/")))
    (when (= (. cache zone) nil)
      (let [file (io.open (.. dir "/" zone) :rb)
            data (when file (let [d (file:read :a)] (file:close) d))
            (ok parsed) (pcall parse (or data ""))]
        (tset cache zone (or (and ok parsed) false))))
    (or (. cache zone) nil)))

(fn info [zone t]
  "{:offset :abbr :dst?} of ZONE at instant T, or nil for an unknown zone."
  (let [z (load zone)]
    (when z
      (let [{: transitions : indices : infos} z
            n (length transitions)
            last (. transitions n)
            ;; The files list transitions until 2037; later, the rule repeats.
            t (if (and z.rule? last (> t last))
                  (- t (* cycle (math.ceil (/ (- t last) cycle))))
                  t)]
        (if (or (= n 0) (< t (. transitions 1)))
            (or (accumulate [found nil _ i (ipairs infos)]
                  (or found (when (not i.dst?) i)))
                (. infos 1))
            (do
              (var lo 1)
              (var hi n)
              (while (< lo hi)
                (let [mid (// (+ lo hi 1) 2)]
                  (if (<= (. transitions mid) t)
                      (set lo mid)
                      (set hi (- mid 1)))))
              (. infos (. indices lo))))))))

(fn wall->instant [zone wall]
  "The instant at which ZONE's clocks show WALL (seconds since the epoch as if
  the wall clock were UTC): the earlier one when clocks go back, and in a gap
  when they go forward, the time read with the offset before it (2:30 is 3:30)."
  (let [offset #(let [i (info zone $1)] (if i i.offset 0))
        before (offset (- wall 86400))
        after (offset (+ wall 86400))
        valid? #(= (offset (- wall $1)) $1)]
    (if (and (valid? before) (valid? after)) (math.min (- wall before) (- wall after))
        (valid? before) (- wall before)
        (valid? after) (- wall after)
        (- wall before))))

(fn components [zone t]
  "The wall clock in ZONE at instant T: {:year :month :day :hour :min :wday
  :days :abbr :offset}."
  (let [i (or (info zone t) {:offset 0 :abbr "UTC"})
        wall (+ t i.offset)
        days (// wall 86400)
        secs (- wall (* days 86400))
        (year month day) (civil-from-days days)]
    {: year : month : day
     :hour (// secs 3600)
     :min (// (% secs 3600) 60)
     :wday (weekday days)
     : days
     :abbr i.abbr
     :offset i.offset}))

(fn offset-string [offset]
  "+09:00 for 9 hours east of UTC."
  (let [a (math.abs offset)]
    (string.format "%s%02d:%02d" (if (< offset 0) "-" "+") (// a 3600) (// (% a 3600) 60))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zone names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Abbreviations name a region, not a fixed offset: EST in July reads as New
;; York time (EDT), and conversions show the abbreviation in effect.
(local aliases
       {:utc "UTC" :gmt "UTC" :z "UTC" :zulu "UTC"
        :et "America/New_York" :est "America/New_York" :edt "America/New_York"
        :eastern "America/New_York"
        :ct "America/Chicago" :cst "America/Chicago" :cdt "America/Chicago"
        :central "America/Chicago"
        :mt "America/Denver" :mst "America/Denver" :mdt "America/Denver"
        :mountain "America/Denver"
        :pt "America/Los_Angeles" :pst "America/Los_Angeles" :pdt "America/Los_Angeles"
        :pacific "America/Los_Angeles"
        :akst "America/Anchorage" :akdt "America/Anchorage" :hst "Pacific/Honolulu"
        :bst "Europe/London" :wet "Europe/Lisbon" :west "Europe/Lisbon"
        :cet "Europe/Berlin" :cest "Europe/Berlin"
        :eet "Europe/Athens" :eest "Europe/Athens" :msk "Europe/Moscow"
        :ist "Asia/Kolkata" :pkt "Asia/Karachi" :ict "Asia/Bangkok" :wib "Asia/Jakarta"
        :sgt "Asia/Singapore" :hkt "Asia/Hong_Kong" :kst "Asia/Seoul" :jst "Asia/Tokyo"
        :awst "Australia/Perth" :acst "Australia/Adelaide" :acdt "Australia/Adelaide"
        :aest "Australia/Sydney" :aedt "Australia/Sydney"
        :nzst "Pacific/Auckland" :nzdt "Pacific/Auckland"
        :brt "America/Sao_Paulo" :art "America/Argentina/Buenos_Aires"
        "new york city" "America/New_York" :nyc "America/New_York"
        :boston "America/New_York" :washington "America/New_York" :dc "America/New_York"
        :atlanta "America/New_York" :miami "America/New_York"
        :dallas "America/Chicago" :houston "America/Chicago" :austin "America/Chicago"
        "san francisco" "America/Los_Angeles" :sf "America/Los_Angeles"
        :seattle "America/Los_Angeles" :la "America/Los_Angeles"
        :munich "Europe/Berlin" :frankfurt "Europe/Berlin" :hamburg "Europe/Berlin"
        :barcelona "Europe/Madrid" :milan "Europe/Rome" :geneva "Europe/Zurich"
        :beijing "Asia/Shanghai" :shenzhen "Asia/Shanghai"
        :mumbai "Asia/Kolkata" :delhi "Asia/Kolkata" "new delhi" "Asia/Kolkata"
        :bangalore "Asia/Kolkata" :bengaluru "Asia/Kolkata"
        :osaka "Asia/Tokyo" :busan "Asia/Seoul"
        :korea "Asia/Seoul" "south korea" "Asia/Seoul"
        :uk "Europe/London" :england "Europe/London" :britain "Europe/London"
        :us "America/New_York" :usa "America/New_York"})

;; Offered first by `zones`, after the local zone.
(local popular
       ["America/New_York" "America/Chicago" "America/Denver" "America/Los_Angeles"
        "Pacific/Honolulu" "UTC" "Europe/London" "Europe/Berlin" "Europe/Paris"
        "Europe/Athens" "Europe/Moscow" "Asia/Dubai" "Asia/Kolkata" "Asia/Shanghai"
        "Asia/Seoul" "Asia/Tokyo" "Asia/Singapore" "Australia/Sydney"
        "Pacific/Auckland" "America/Sao_Paulo"])

(var zone-list nil)
(local primary {})

(fn read-lines [path f]
  (let [file (io.open path)]
    (when file
      (each [line (file:lines)]
        (when (not (line:match "^#"))
          (f line)))
      (file:close))))

(fn zones []
  "Every zone of zone.tab and UTC, as {:id :city :country :code}, by id."
  (when (not zone-list)
    (let [countries {}
          found [{:id "UTC" :city "UTC" :country "" :code ""}]]
      (read-lines (.. dir "/iso3166.tab")
                  #(let [(code name) ($1:match "^(%u%u)\t(.+)$")]
                     (when code (tset countries code name))))
      (read-lines (.. dir "/zone.tab")
                  #(let [(code id) ($1:match "^(%u%u)\t%S+\t(%S+)")]
                     (when id
                       ;; zone.tab lists a country's main zone first (US: New York).
                       (when (not (. primary code)) (tset primary code id))
                       (table.insert found
                                     {: id
                                      : code
                                      :city (: (id:match "([^/]+)$") :gsub "_" " ")
                                      :country (or (. countries code) code)}))))
      (table.sort found #(< $1.id $2.id))
      (set zone-list found)))
  zone-list)

(fn local-zone []
  "The zone of the system clock, e.g. America/New_York."
  (let [env (os.getenv :TZ)
        link (when (not env)
               (let [p (io.popen "/usr/bin/readlink /etc/localtime")
                     out (p:read :a)]
                 (p:close)
                 out))]
    (or (and env (env:gsub "^:" ""))
        (and link (link:match "zoneinfo/(%S+)"))
        "UTC")))

(fn normalize [name]
  (-> (name:lower) (: :gsub "_" " ") (: :gsub "%s+" " ") (: :match "^%s*(.-)%s*$")))

(fn lookup [name]
  "The zone id NAME stands for: an abbreviation (EST, CET, KST), a zone id
  (Europe/Berlin), a city (Seoul, New York) or a country (Germany), or nil."
  (let [key (normalize name)
        all (zones)]
    (when (not= key "")
      (or (. aliases key)
          (when (= key "local") (local-zone))
          (accumulate [found nil _ z (ipairs all)]
            (or found (when (or (= (normalize z.id) key) (= (normalize z.city) key))
                        z.id)))
          (accumulate [found nil _ z (ipairs all)]
            (or found
                (let [country (normalize z.country)]
                  (when (and (not= country "")
                             (or (= country key)
                                 (= (normalize (country:gsub "%s*%b()" "")) key)))
                    (. primary z.code)))))))))

{: dir
 : days-from-civil
 : civil-from-days
 : weekday
 : days-in-month
 : info
 : wall->instant
 : components
 : offset-string
 : zones
 : aliases
 : popular
 : local-zone
 : lookup}
