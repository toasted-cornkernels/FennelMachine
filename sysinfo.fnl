"
System information, a world clock and a process list, like Emacs'
`emacs-uptime', `world-clock' and `proced'.
"
(local minibuffer (require :lib.minibuffer))
(local help (require :lib.help))

;; Mirrors elispm/world-clock-cities in init.el.
(local cities [["America/New_York" "New York"]
               ["America/Phoenix" "Phoenix"]
               ["America/Los_Angeles" "Los Angeles"]
               ["Europe/London" "Oxford"]
               ["Europe/Zurich" "Zurich"]
               ["Asia/Seoul" "Seoul"]
               ["Asia/Tokyo" "Tokyo"]])

(local clock-format "%a %d %b %I:%M %p %Z")

(fn sh-quote [s]
  (.. "'" (pick-values 1 (s:gsub "'" "'\\''")) "'"))

(fn disk-size [bytes]
  "Decimal units, like Finder."
  (if (>= bytes 1e12)
      (string.format "%.2f TB" (/ bytes 1e12))
      (string.format "%.1f GB" (/ bytes 1e9))))

(fn memory-size [bytes]
  "Binary units, like Activity Monitor."
  (string.format "%.1f GB" (/ bytes (^ 1024 3))))

(fn row [label value]
  (string.format "%-9s %s" label value))

(fn duration [seconds]
  (let [days (math.floor (/ seconds 86400))
        hours (math.floor (/ (% seconds 86400) 3600))
        minutes (math.floor (/ (% seconds 3600) 60))]
    (.. (if (> days 0) (.. days (if (= days 1) " day, " " days, ")) "")
        (string.format "%d:%02d" hours minutes))))

(fn sysctl []
  (let [out (hs.execute "/usr/sbin/sysctl -n kern.boottime vm.loadavg hw.model machdep.cpu.brand_string hw.ncpu")
        lines (icollect [line (out:gmatch "[^\n]+")] line)]
    {:boot (tonumber (: (or (. lines 1) "") :match "sec = (%d+)"))
     :load (: (or (. lines 2) "") :match "{%s*(.-)%s*}")
     :model (. lines 3)
     :cpu (. lines 4)
     :cores (. lines 5)}))

(fn memory-row []
  (let [vm (hs.host.vmStat)
        ;; Activity Monitor's "Memory Used": app memory, wired and compressed.
        used (* vm.pageSize (+ (- vm.anonymousPages vm.pagesPurgeable)
                               vm.pagesWiredDown vm.pagesUsedByVMCompressor))]
    (row "Memory" (.. (memory-size used) " used of " (memory-size vm.memSize)))))

(fn power-row []
  (let [percentage (hs.battery.percentage)
        source (or (hs.battery.powerSource) "unknown")]
    (row "Power"
         (if percentage
             (string.format "%s, battery %d%%%s" source (math.floor percentage)
                            (if (hs.battery.isCharging) " (charging)" ""))
             source))))

(fn disk-rows []
  (let [volumes (icollect [path v (pairs (or (hs.host.volumeInformation) {}))]
                  (when (and v.NSURLVolumeIsBrowsableKey v.NSURLVolumeTotalCapacityKey)
                    {: path
                     :name (or v.NSURLVolumeLocalizedNameKey path)
                     :total v.NSURLVolumeTotalCapacityKey
                     :free (or v.NSURLVolumeAvailableCapacityForImportantUsageKey
                               v.NSURLVolumeAvailableCapacityKey 0)}))]
    (table.sort volumes #(< $1.path $2.path))
    (icollect [i v (ipairs volumes)]
      (row (if (= i 1) "Disk" "")
           (.. v.name ": " (disk-size v.free) " free of " (disk-size v.total))))))

(fn network-row []
  (let [interface (hs.network.primaryInterfaces)
        details (and interface (hs.network.interfaceDetails interface))
        addresses (or (?. details :IPv4 :Addresses) [])]
    (row "Network"
         (if interface
             (.. interface " " (table.concat addresses ", "))
             "offline"))))

(fn info []
  "Show the macOS version, host, hardware, uptime, power, disks and network."
  (let [s (sysctl)
        lines [(row "macOS" (hs.host.operatingSystemVersionString))
               (row "Host" (.. (hs.host.localizedName)
                               (if s.model (.. " (" s.model ")") "")))
               (row "CPU" (.. (or s.cpu "?") (if s.cores (.. " × " s.cores) "")))
               (memory-row)
               (row "Uptime" (if s.boot (duration (- (os.time) s.boot)) "?"))
               (row "Load" (or s.load "?"))
               (power-row)]]
    (each [_ line (ipairs (disk-rows))]
      (table.insert lines line))
    (table.insert lines (network-row))
    (help.show {:title "System Information" : lines})))

(fn world-clock []
  "Show the time in the cities of `cities'."
  (let [width (accumulate [w 0 _ [_ name] (ipairs cities)]
                (math.max w (utf8.len name)))
        script (table.concat
                (icollect [_ [zone] (ipairs cities)]
                  (.. "TZ=" (sh-quote zone) " /bin/date " (sh-quote (.. "+" clock-format))))
                "; ")
        times (icollect [line (: (hs.execute script) :gmatch "[^\n]+")] line)]
    (help.show {:title "World Clock"
                :lines (icollect [i [_ name] (ipairs cities)]
                         (.. name (string.rep " " (- (+ width 2) (utf8.len name)))
                             (or (. times i) "?")))})))

(fn processes []
  (let [out (hs.execute "/bin/ps -xco pid=,pcpu=,rss=,comm= -r")]
    (icollect [line (out:gmatch "[^\n]+")]
      (let [(pid cpu rss name) (line:match "^%s*(%d+)%s+([%d.]+)%s+(%d+)%s+(.-)%s*$")]
        (when pid
          {:text name
           : pid
           :annotation (string.format "%6s  %5s%%  %8s" pid cpu
                                      (string.format "%.0f MB" (/ (tonumber rss) 1024)))
           :keywords pid})))))

(fn signal [candidate sig verb]
  (when candidate
    (minibuffer.y-or-n-p
     (.. verb " " candidate.text " (" candidate.pid ")?")
     (fn []
       (let [(_ ok) (hs.execute (.. "/bin/kill -" sig " " candidate.pid))]
         (minibuffer.message
          (if ok
              (.. "Sent " sig " to " candidate.text " (" candidate.pid ")")
              (.. "Couldn't signal " candidate.text " (" candidate.pid ")"))))))))

(fn list-processes []
  "Pick one of your processes, sorted by CPU use. RET terminates it and s-k
  force quits it, both after confirmation."
  (minibuffer.completing-read
   "Process: " (processes)
   {:on-accept #(signal $1 :TERM "Terminate")
    :actions {:s-k #(signal $1 :KILL "Force quit")}}))

{: info : world-clock :processes list-processes : cities}
