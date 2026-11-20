(local is (require :lib.testing.assert))
(local tz (require :lib.tz))
(local {: parse : resolve : stamp : add-months} (require :lib.read-date))

(local zone "America/New_York")
(local today (tz.days-from-civil 2026 11 20))
(local now (tz.wall->instant zone (+ (* today 86400) (* 15 3600) (* 46 60))))

(fn read [input ?cursor]
  "INPUT read on Friday 2026-11-20 15:46 in New York, as a stamp with zone id."
  (let [r (parse input {:cursor (or ?cursor today) : today : zone : now})
        result (resolve r)]
    (.. (stamp result.wall) " " result.zone (if r.rest (.. " ?" r.rest) ""))))

(describe
 "Time zones"
 (fn []
   (it "converts between zones across DST"
       (fn []
         (let [t (tz.wall->instant zone (+ (* (tz.days-from-civil 2026 12 25) 86400)
                                           (* 16 3600)))
               c (tz.components "Europe/Berlin" t)]
           (is.eq? (stamp c true) "2026-12-25 Fri 22:00 CET" "EST to CET")
           (is.eq? (stamp (tz.components "Asia/Seoul" t) true) "2026-12-26 Sat 06:00 KST"
                   "EST to KST"))
         (let [t (tz.wall->instant zone (+ (* (tz.days-from-civil 2026 7 4) 86400)
                                           (* 12 3600)))]
           (is.eq? (stamp (tz.components zone t) true) "2026-07-04 Sat 12:00 EDT" "EDT")
           (is.eq? (stamp (tz.components "Europe/London" t) true)
                   "2026-07-04 Sat 17:00 BST" "BST"))))

   (it "reads wall clocks around DST changes"
       (fn []
         (let [at #(let [t (tz.wall->instant $1 (+ (* (tz.days-from-civil $2 $3 $4) 86400)
                                                   (* $5 3600) (* 30 60)))]
                     (stamp (tz.components $1 t) true))]
           (is.eq? (at zone 2026 3 8 2) "2026-03-08 Sun 03:30 EDT" "gap, New York")
           (is.eq? (at "Europe/Berlin" 2026 3 29 2) "2026-03-29 Sun 03:30 CEST" "gap, Berlin")
           (is.eq? (at zone 2026 11 1 1) "2026-11-01 Sun 01:30 EDT" "overlap, New York")
           (is.eq? (at "Europe/Berlin" 2026 10 25 2) "2026-10-25 Sun 02:30 CEST"
                   "overlap, Berlin")
           (is.eq? (at "Asia/Seoul" 2026 6 1 12) "2026-06-01 Mon 12:30 KST" "no DST"))))

   (it "follows the zone's rule after its last transition"
       (fn []
         (let [t (tz.wall->instant "Europe/Berlin"
                                   (+ (* (tz.days-from-civil 2065 7 1) 86400) (* 12 3600)))]
           (is.eq? (. (tz.components "Europe/Berlin" t) :abbr) "CEST" "2065 summer")
           (is.eq? (. (tz.components "UTC" t) :hour) 10 "2065 offset"))))

   (it "looks up abbreviations, cities and countries"
       (fn []
         (is.eq? (tz.lookup "EST") "America/New_York" "EST")
         (is.eq? (tz.lookup "cet") "Europe/Berlin" "CET")
         (is.eq? (tz.lookup "Seoul") "Asia/Seoul" "city")
         (is.eq? (tz.lookup "sao paulo") "America/Sao_Paulo" "city with a space")
         (is.eq? (tz.lookup "Germany") "Europe/Berlin" "country")
         (is.eq? (tz.lookup "nowhere") nil "unknown")))

   (it "adds months like the calendar"
       (fn []
         (let [jan31 (tz.days-from-civil 2028 1 31)]
           (is.eq? (add-months jan31 1) (tz.days-from-civil 2028 2 29) "leap February")
           (is.eq? (add-months jan31 -2) (tz.days-from-civil 2027 11 30) "backwards"))))))

(describe
 "Reading dates"
 (fn []
   (it "defaults to now and the calendar's date"
       (fn []
         (is.eq? (read "") "2026-11-20 Fri 15:46 America/New_York" "empty")
         (is.eq? (read "" (tz.days-from-civil 2026 12 1))
                 "2026-12-01 Tue 15:46 America/New_York" "calendar")))

   (it "reads times"
       (fn []
         (is.eq? (read "16:00") "2026-11-20 Fri 16:00 America/New_York" "24h")
         (is.eq? (read "4pm") "2026-11-20 Fri 16:00 America/New_York" "pm")
         (is.eq? (read "4:30 p.m.") "2026-11-20 Fri 16:30 America/New_York" "p.m.")
         (is.eq? (read "4 p.m.") "2026-11-20 Fri 16:00 America/New_York" "4 p.m.")
         (is.eq? (read "12am") "2026-11-20 Fri 00:00 America/New_York" "midnight am")
         (is.eq? (read "noon") "2026-11-20 Fri 12:00 America/New_York" "noon")))

   (it "reads dates"
       (fn []
         (is.eq? (read "2026-12-25") "2026-12-25 Fri 15:46 America/New_York" "ISO")
         (is.eq? (read "12/25 16:00") "2026-12-25 Fri 16:00 America/New_York" "US")
         (is.eq? (read "12/25/27") "2027-12-25 Sat 15:46 America/New_York" "US year")
         (is.eq? (read "25.12.") "2026-12-25 Fri 15:46 America/New_York" "European")
         (is.eq? (read "dec 25") "2026-12-25 Fri 15:46 America/New_York" "month day")
         (is.eq? (read "Dec 25, 2027") "2027-12-25 Sat 15:46 America/New_York" "comma")
         (is.eq? (read "25 December 2027") "2027-12-25 Sat 15:46 America/New_York"
                 "day month year")
         (is.eq? (read "1/5") "2027-01-05 Tue 15:46 America/New_York" "prefers the future")
         (is.eq? (read "march") "2027-03-20 Sat 15:46 America/New_York" "month")
         (is.eq? (read "15") "2026-12-15 Tue 15:46 America/New_York" "day passed")
         (is.eq? (read "25") "2026-11-25 Wed 15:46 America/New_York" "day")
         (is.eq? (read "2027") "2027-11-20 Sat 15:46 America/New_York" "year")))

   (it "reads relative dates"
       (fn []
         (is.eq? (read "fri") "2026-11-20 Fri 15:46 America/New_York" "this Friday")
         (is.eq? (read "Monday 9am") "2026-11-23 Mon 09:00 America/New_York" "weekday")
         (is.eq? (read "+2d") "2026-11-22 Sun 15:46 America/New_York" "days")
         (is.eq? (read "+1w") "2026-11-27 Fri 15:46 America/New_York" "week")
         (is.eq? (read "+1m") "2026-12-20 Sun 15:46 America/New_York" "month")
         (is.eq? (read "-1y") "2025-11-20 Thu 15:46 America/New_York" "year")
         (is.eq? (read "+tue") "2026-11-24 Tue 15:46 America/New_York" "next Tuesday")
         (is.eq? (read "+2tue") "2026-12-01 Tue 15:46 America/New_York" "2nd Tuesday")
         (is.eq? (read "+fri") "2026-11-27 Fri 15:46 America/New_York" "next Friday")
         (is.eq? (read "-fri") "2026-11-13 Fri 15:46 America/New_York" "last Friday")
         (is.eq? (read "++3" (tz.days-from-civil 2026 12 1))
                 "2026-12-04 Fri 15:46 America/New_York" "from the calendar")
         (is.eq? (read "tomorrow 9am") "2026-11-21 Sat 09:00 America/New_York" "tomorrow")
         (is.eq? (read "now") "2026-11-20 Fri 15:46 America/New_York" "now")))

   (it "reads time zones"
       (fn []
         (is.eq? (read "12/25 16:00 EST") "2026-12-25 Fri 16:00 America/New_York" "EST")
         (is.eq? (read "16:00 cet") "2026-11-20 Fri 16:00 Europe/Berlin" "CET")
         (is.eq? (read "dec 25 4pm sao paulo") "2026-12-25 Fri 16:00 America/Sao_Paulo"
                 "two words")
         (is.eq? (read "Seoul") "2026-11-21 Sat 05:46 Asia/Seoul" "now there")
         (is.eq? (read "at 4pm on dec 25 in Tokyo") "2026-12-25 Fri 16:00 Asia/Tokyo"
                 "fillers")))

   (it "keeps the picker's date and time once it was moved"
       (fn []
         (let [ctx {:cursor (tz.days-from-civil 2026 12 25) :hour 17 :min 5 : zone
                    : now : today :touched? true}
               at #(let [r (parse $1 ctx)]
                     (.. (stamp (. (resolve r) :wall)) " " r.zone))]
           (is.eq? (at "") "2026-12-25 Fri 17:05 America/New_York" "picker")
           (is.eq? (at "Seoul") "2026-12-25 Fri 17:05 Asia/Seoul" "zone")
           (is.eq? (at "9am") "2026-12-25 Fri 09:00 America/New_York" "time")
           (is.eq? (at "++1w") "2027-01-01 Fri 17:05 America/New_York" "from the picker"))))

   (it "reports what it cannot read"
       (fn []
         (is.eq? (read "foo 16:00") "2026-11-20 Fri 16:00 America/New_York ?foo" "rest")))))
