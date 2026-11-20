"Internet radio, like eradio: pick a station from Emacs' eradio-channels and
play it in a background mpv, which the other Media keys then control"

(local minibuffer (require :lib.minibuffer))
(local emacs (require :emacs))
(local mpv (require :mpv))

(local socket (.. (: (or (os.getenv :TMPDIR) "/tmp") :gsub "/$" "") "/mpv-radio.sock"))
(local mpv-paths ["/opt/homebrew/bin/mpv" "/usr/local/bin/mpv"
                  (.. "/etc/profiles/per-user/" (os.getenv :USER) "/bin/mpv")
                  "/run/current-system/sw/bin/mpv"])

;; Used when Emacs or eradio isn't running.
(local default-stations
       [{:name "WFUV 90.7" :url "https://onair.wfuv.org/onair-aacplus"}
        {:name "WNYC 93.9 FM" :url "https://fm939.wnyc.org/wnycfm.aac"}
        {:name "WBBR 1130 AM" :url "http://14123.live.streamtheworld.com/WBBRAMAAC_SC"}])

(var stations nil)

(local stations-file (.. (: (or (os.getenv :TMPDIR) "/tmp") :gsub "/$" "")
                         "/hammerspoon-radio.json"))

(fn read-stations [callback]
  "Call CALLBACK with the stations of eradio-channels, else the last ones read,
  else the defaults. Emacs writes them to a file: emacsclient garbles long
  non-ASCII output."
  (os.remove stations-file)
  (emacs.emacsclient
   ["-e" (string.format
          "(when (boundp 'eradio-channels)
             (let ((coding-system-for-write 'utf-8))
               (with-temp-file %q
                 (insert (json-encode
                          (mapcar (lambda (c) (list (cons 'name (car c))
                                                    (cons 'url (cdr c))))
                                  eradio-channels)))))
             nil)"
          stations-file)]
   (fn [code]
     (let [file (and (= code 0) (io.open stations-file))
           json (when file (let [text (file:read :a)] (file:close) text))
           (ok decoded) (pcall hs.json.decode (or json ""))]
       (os.remove stations-file)
       (when (and ok (= (type decoded) :table) (> (length decoded) 0))
         (set stations decoded))
       (callback (or stations default-stations))))))

(fn mpv-path []
  (accumulate [found nil _ path (ipairs mpv-paths)]
    (or found (when (hs.fs.attributes path) path))))

(fn sh-quote [s]
  (.. "'" (s:gsub "'" "'\\''") "'"))

(fn spawn [station]
  "Start the radio mpv, detached so it outlives a Hammerspoon reload."
  (let [path (mpv-path)]
    (if (not path)
        (minibuffer.message "mpv is not installed")
        (do
          (os.remove socket)
          (: (hs.task.new "/bin/sh" nil
                          ["-c" (.. "nohup " (sh-quote path)
                                    " --no-video --no-terminal --idle=no"
                                    " --input-ipc-server=" (sh-quote socket)
                                    " --force-media-title=" (sh-quote station.name)
                                    " " (sh-quote station.url)
                                    " >/dev/null 2>&1 &")])
             :start)))))

(fn play [station]
  "Play STATION in the running radio mpv, or start one."
  (if (not (hs.fs.attributes socket))
      (do (spawn station)
          (minibuffer.message (.. "Playing " station.name)))
      (mpv.request socket
                   [["set_property" "force-media-title" station.name]
                    ["loadfile" station.url "replace"]
                    ["set_property" "pause" false]]
                   (fn [[_ loaded]]
                     ;; A socket left behind by an mpv that has gone.
                     (when (not loaded) (spawn station))
                     (minibuffer.message (.. "Playing " station.name))))))

(fn host-of [url]
  (or (url:match "^%a+://([^/:?]+)") url))

(fn playing-title [callback]
  "Call CALLBACK with the media title of the radio mpv, or nil."
  (if (hs.fs.attributes socket)
      (mpv.request socket [["get_property" "force-media-title"]]
                   (fn [[title]]
                     (callback (and title (= title.error :success) title.data))))
      (callback nil)))

(fn choose-station []
  "Pick a station in the minibuffer and play it."
  (playing-title
   (fn [playing]
     (read-stations
      (fn [list]
        (minibuffer.completing-read
         "Radio: "
         (icollect [_ s (ipairs list)]
           {:text s.name
            :annotation (.. (host-of s.url) (if (= s.name playing) "  playing" ""))
            :station s})
         {:on-accept (fn [c] (when c (play c.station)))}))))))

(fn stop []
  "Quit the radio mpv."
  (mpv.request socket [["quit"]]
               (fn [[answer]]
                 (os.remove socket)
                 (minibuffer.message (if answer "Radio stopped" "The radio isn't on")))))

{: choose-station
 : stop}
