"
Convert a date and time between time zones: pick it in the org-style
calendar (lib/read-date), e.g. `12/25 16:00 EST`, then the zone to convert to,
e.g. `CET`. The zones show the converted time as you type; RET shows it in the
echo area and copies it.
"
(local minibuffer (require :lib.minibuffer))
(local read-date (require :lib.read-date))
(local tz (require :lib.tz))

(local recent-key "timeconvert.recent")
(local max-recent 8)

(fn remember [zone]
  (let [recent (icollect [_ z (ipairs (or (hs.settings.get recent-key) []))]
                 (when (not= z zone) z))]
    (table.insert recent 1 zone)
    (while (> (length recent) max-recent) (table.remove recent))
    (hs.settings.set recent-key recent)))

(fn ordered-zones []
  "Zone ids: recently used, the local zone, the world clock's, popular, the rest."
  (let [seen {}
        ids []
        add #(when (and $1 (not (. seen $1)))
               (tset seen $1 true)
               (table.insert ids $1))]
    (each [_ z (ipairs (or (hs.settings.get recent-key) []))] (add z))
    (add (tz.local-zone))
    (each [_ [z] (ipairs (. (require :sysinfo) :cities))] (add z))
    (each [_ z (ipairs tz.popular)] (add z))
    (each [_ z (ipairs (tz.zones))] (add z.id))
    ids))

(fn zone-candidates [from]
  (let [info (collect [_ z (ipairs (tz.zones))] z.id z)
        names (collect [_ [z name] (ipairs (. (require :sysinfo) :cities))] z name)
        abbreviations {}]
    (each [alias z (pairs tz.aliases)]
      (tset abbreviations z (.. (or (. abbreviations z) "") " " alias)))
    (icollect [_ id (ipairs (ordered-zones))]
      (let [z (. info id)
            c (tz.components id from.instant)]
        {:text id
         : id
         :keywords (.. (if z (.. z.city " " z.country " " z.code) "")
                       " " (or (. names id) "") (or (. abbreviations id) "")
                       " " c.abbr)
         :annotation (.. (if z (.. z.country "   ") "")
                         (read-date.stamp c true) " " (tz.offset-string c.offset))}))))

(fn show-result [from zone]
  (let [text (read-date.stamp (tz.components zone from.instant) true)]
    (remember zone)
    (hs.pasteboard.setContents text)
    (minibuffer.message (.. from.text " = " text "  (copied)"))))

(fn convert []
  "Read a date and time, then the zone to convert it to."
  (read-date.read-date
   {:prompt "Convert"
    :on-accept
    (fn [from]
      (minibuffer.completing-read
       (.. "Convert " from.text " to: ")
       (zone-candidates from)
       {:on-accept (fn [candidate input]
                     (let [zone (if candidate candidate.id (tz.lookup input))]
                       (if zone
                           (show-result from zone)
                           (minibuffer.message (.. "Unknown time zone: " input)))))}))}))

(fn history-count [] (length (or (hs.settings.get recent-key) [])))

(fn clear-history []
  "Forget the recently used zones."
  (hs.settings.clear recent-key))

{: convert : history-count : clear-history}
