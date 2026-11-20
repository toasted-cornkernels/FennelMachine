"Internet radio, like eradio: pick a station from Emacs' eradio-channels and
play it in a background mpv, which the Media keys also control"

(local minibuffer (require :lib.minibuffer))
(local emacs (require :emacs))
(local mpv (require :mpv))

(local socket (.. (: (or (os.getenv :TMPDIR) "/tmp") :gsub "/$" "") "/mpv-radio.sock"))
(local mpv-paths ["/opt/homebrew/bin/mpv" "/usr/local/bin/mpv"
                  (.. "/etc/profiles/per-user/" (os.getenv :USER) "/bin/mpv")
                  "/run/current-system/sw/bin/mpv"])

;; Read when Emacs can't be asked, e.g. its server isn't running.
(local init-files [(.. (os.getenv :HOME) "/.emacs.d/init.el")
                   (.. (os.getenv :HOME) "/.config/emacs/init.el")])

(var stations nil)

(local stations-file (.. (: (or (os.getenv :TMPDIR) "/tmp") :gsub "/$" "")
                         "/hammerspoon-radio.json"))

(fn init-file-stations []
  "The (\"name\" . \"url\") pairs of the eradio-channels setting in the Emacs
  init file."
  (accumulate [found nil _ path (ipairs init-files)]
    (or found
        (let [file (io.open path)
              text (when file (let [t (file:read :a)] (file:close) t))
              start (and text (text:find "%(eradio%-channels%s+'%("))
              form (and start (text:match "%b()" start))
              found (when form
                      (icollect [name url (form:gmatch "%(%s*\"(.-)\"%s*%.%s*\"(.-)\"%s*%)")]
                        {: name : url}))]
          (when (and found (> (length found) 0)) found)))))

(fn read-stations [callback]
  "Call CALLBACK with the stations of eradio-channels: from Emacs (loading
  eradio if needed), else the last ones read, else the init file's. Emacs
  writes them to a file: emacsclient garbles long non-ASCII output."
  (os.remove stations-file)
  (emacs.emacsclient
   ["-e" (string.format
          "(when (and (require 'json) (require 'eradio nil t))
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
       (callback (or stations (init-file-stations) []))))))

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

(local last-station-key :radio.last-station)

(fn play [station]
  "Play STATION in the running radio mpv, or start one."
  (hs.settings.set last-station-key {:name station.name :url station.url})
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
        (if (= 0 (length list))
            (minibuffer.message "No stations: eradio-channels isn't set in Emacs")
            (minibuffer.completing-read
             "Radio: "
             (icollect [_ s (ipairs list)]
               {:text s.name
                :annotation (.. (host-of s.url) (if (= s.name playing) "  playing" ""))
                :station s})
             {:on-accept (fn [c] (when c (play c.station)))})))))))

(fn stop []
  "Quit the radio mpv."
  (mpv.request socket [["quit"]]
               (fn [[answer]]
                 (os.remove socket)
                 (minibuffer.message (if answer "Radio stopped" "The radio isn't on")))))

(fn toggle []
  "Stop the radio if it is on, else play the last station again (or pick
  one), like eradio-toggle."
  (playing-title
   (fn [playing]
     (let [last (hs.settings.get last-station-key)]
       (if playing (stop)
           last (play last)
           (choose-station))))))

{: choose-station
 : stop
 : toggle}
