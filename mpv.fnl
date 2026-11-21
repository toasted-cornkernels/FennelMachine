;; EMMS's controls (Emacs' mode keymap, Cmd-Shift-, a m e), through the JSON IPC
;; socket of the mpv EMMS started (--input-ipc-server). Play/pause and tracks go
;; through EMMS, so it keeps its playlist and paused state; volume, mute and
;; seeking go to mpv directly. Also `open`, for Cmd-SPC m a.

(local minibuffer (require :lib.minibuffer))
(local emacs (require :emacs))
(local now-playing (require :now-playing))

(local timeout 1)
(local volume-step 5)
(local pending {})

(local socket-dir (: (or (os.getenv :TMPDIR) "/tmp") :gsub "/$" ""))

(fn ipc-server [pid args]
  "The socket from --input-ipc-server, else the one mpv-scripts/ipc-socket.lua
  made for PID, if any."
  (or (args:match "%-%-input%-ipc%-server=(%S+)")
      (args:match "%-%-input%-ipc%-server%s+(%S+)")
      (let [socket (.. socket-dir "/mpv-" pid ".sock")]
        (when (hs.fs.attributes socket) socket))))

(fn seconds [etime]
  "Convert ps's elapsed time, [[dd-]hh:]mm:ss, to seconds."
  (let [(days rest) (etime:match "^(%d+)%-(.*)$")]
    (+ (* (or (tonumber days) 0) 86400)
       (accumulate [acc 0 n (string.gmatch (or rest etime) "%d+")]
         (+ (* acc 60) (tonumber n))))))

(fn parse-processes [stdout]
  (let [procs {}]
    (each [line (stdout:gmatch "[^\n]+")]
      (let [(pid ppid etime args)
            (line:match "^%s*(%d+)%s+(%d+)%s+(%S+)%s+(.*)$")]
        (when pid
          (tset procs (tonumber pid) {:pid (tonumber pid)
                                      :ppid (tonumber ppid)
                                      :age (seconds etime)
                                      : args}))))
    procs))

(fn instances [callback]
  "Call CALLBACK with the running mpv processes that have an IPC socket, as
  {:pid :socket :emms?}, the most recently started first."
  (var task nil)
  (set task
       (hs.task.new "/bin/sh"
                  (fn [code stdout]
                    (tset pending task nil)
                    (let [procs (if (= code 0) (parse-processes stdout) {})
                          found []]
                      (each [_ proc (pairs procs)]
                        (let [exe (proc.args:match "^(%S+)")
                              socket (ipc-server proc.pid proc.args)
                              parent (. procs proc.ppid)]
                          (when (and socket (= (exe:match "[^/]+$") :mpv))
                            (table.insert found
                                          {:pid proc.pid
                                           :age proc.age
                                           : socket
                                           :emms? (and parent
                                                       (: (parent.args:lower) :find "emacs")
                                                       true)}))))
                      (table.sort found #(< $1.age $2.age))
                      (callback found)))
                  ;; Filtered in the shell: hs.task stalls on ps's full output.
                  ["-c" "/bin/ps -axo pid=,ppid=,etime=,args= | /usr/bin/grep -i -e mpv -e emacs"]))
  (tset pending task true)
  (: task :start))

(fn request [socket commands callback]
  "Send COMMANDS (each a list such as [\"cycle\" \"pause\"]) to the mpv at
  SOCKET and call CALLBACK with their responses, in order. A response is
  {:data :error}, or nil if mpv didn't answer within the timeout."
  (let [sock (hs.socket.new)
        responses {}
        state {:remaining (length commands) :done false}]
    (fn finish []
      (when (not state.done)
        (set state.done true)
        (when state.timer (: state.timer :stop))
        (: sock :disconnect)
        (tset pending sock nil)
        (when callback
          (callback (fcollect [i 1 (length commands)] (. responses i))))))
    (tset pending sock true)
    (: sock :setCallback
       (fn [data]
         (let [(ok msg) (pcall hs.json.decode data)
               id (and ok (= (type msg) :table) msg.request_id)]
           (when (and id (= nil (. responses id)) (<= 1 id (length commands)))
             (tset responses id {:data msg.data :error msg.error})
             (set state.remaining (- state.remaining 1))))
         (if (<= state.remaining 0)
             (finish)
             (: sock :read "\n"))))
    (set state.timer (hs.timer.doAfter timeout finish))
    (: sock :connect socket
       (fn []
         (each [i command (ipairs commands)]
           (: sock :write (.. (hs.json.encode {: command :request_id i}) "\n")))
         (: sock :read "\n")))))

(fn with-emms [f]
  "Call F with the mpv that EMMS started."
  (instances
   (fn [found]
     (let [instance (hs.fnutils.find found #$1.emms?)]
       (if instance
           (f instance)
           (minibuffer.message "EMMS has no mpv running"))))))

(fn show-status [instance]
  (request instance.socket [["get_property" "pause"]
                            ["get_property" "idle-active"]
                            ["get_property" "media-title"]]
           (fn [[pause idle title]]
             (minibuffer.message
              (if (or (not pause) (not idle)) "mpv isn't responding"
                  idle.data "mpv is idle"
                  (.. (if pause.data "Paused: " "Playing: ")
                      (or (and title title.data) "")))))))

(fn show-hud [instance]
  (now-playing.show instance request))

(fn emms [form instance property]
  "Evaluate FORM in Emacs, then show the status once mpv's PROPERTY changes.
  EMMS loads tracks asynchronously, so this can take a second or two."
  (fn get [callback]
    (request instance.socket [["get_property" property]]
             (fn [[response]] (callback (and response response.data)))))
  (get (fn [before]
         (var tries 15)
         (fn poll []
           (set tries (- tries 1))
           (get (fn [now]
                  (if (or (not= now before) (= tries 0))
                      (if (= property "path")
                          (show-hud instance)
                          (show-status instance))
                      (hs.timer.doAfter 0.2 poll)))))
         (emacs.emacsclient ["-e" form]
                            (fn [code _ stderr]
                              (if (= code 0)
                                  (poll)
                                  (minibuffer.message
                                   (emacs.emacsclient-failure code stderr))))))))

(fn play-or-pause []
  "Play or pause EMMS."
  (with-emms
   #(emms "(if emms-player-playing-p (emms-pause) (emms-start))" $1 "pause")))

(fn next-track []
  "Play EMMS's next track, then show Now Playing."
  (with-emms #(emms "(emms-next)" $1 "path")))

(fn prev-track []
  "Play EMMS's previous track, then show Now Playing."
  (with-emms #(emms "(emms-previous)" $1 "path")))

(fn change-volume [delta]
  (with-emms
   (fn [instance]
     (request instance.socket [["add" "volume" delta]
                               ["get_property" "volume"]]
              (fn [[_ volume]]
                (minibuffer.message
                 (if (and volume (= volume.error :success))
                     (string.format "EMMS volume: %d%%" volume.data)
                     "mpv isn't responding")))))))

(fn clock [seconds]
  (let [s (math.floor (or seconds 0))]
    (if (>= s 3600)
        (string.format "%d:%02d:%02d" (// s 3600) (// (% s 3600) 60) (% s 60))
        (string.format "%d:%02d" (// s 60) (% s 60)))))

(fn seek [seconds]
  "Seek EMMS's track SECONDS forward (negative: backward)."
  (with-emms
   (fn [instance]
     (fn report []
       (request instance.socket [["get_property" "time-pos"]
                                 ["get_property" "duration"]]
                (fn [[pos duration]]
                  (minibuffer.message
                   (.. "EMMS: " (clock (and pos pos.data))
                       (if (and duration (= (type duration.data) :number))
                           (.. " / " (clock duration.data))
                           ""))))))
     (request instance.socket [["seek" seconds "relative"]]
              (fn [[seeked]]
                (if (not seeked) (minibuffer.message "mpv isn't responding")
                    (not= seeked.error :success) (minibuffer.message "Nothing to seek in")
                    (now-playing.visible?) (show-hud instance)
                    ;; mpv seeks asynchronously; time-pos is stale right after.
                    (hs.timer.doAfter 0.15 report)))))))

(fn seek-forward []
  "Seek EMMS 10 seconds forward."
  (seek 10))

(fn seek-backward []
  "Seek EMMS 10 seconds backward."
  (seek -10))

(fn seek-forward-more []
  "Seek EMMS a minute forward."
  (seek 60))

(fn seek-backward-more []
  "Seek EMMS a minute backward."
  (seek -60))

(fn volume-up []
  "Raise the volume of EMMS's mpv."
  (change-volume volume-step))

(fn volume-down []
  "Lower the volume of EMMS's mpv."
  (change-volume (- volume-step)))

(fn mute []
  "Mute or unmute EMMS's mpv."
  (with-emms
   (fn [instance]
     (request instance.socket [["cycle" "mute"] ["get_property" "mute"]]
              (fn [[_ muted]]
                (minibuffer.message
                 (if (not (and muted (= muted.error :success)))
                     "mpv isn't responding"
                     muted.data "EMMS muted"
                     "EMMS unmuted")))))))

(fn show-now-playing []
  "Show the Now Playing HUD (album art, title, artist, album, progress), or
  hide it if it is showing."
  (if (now-playing.visible?)
      (now-playing.hide)
      (with-emms show-hud)))

(local executables ["/opt/homebrew/bin/mpv" "/usr/local/bin/mpv"
                    (.. "/etc/profiles/per-user/" (os.getenv :USER) "/bin/mpv")
                    "/run/current-system/sw/bin/mpv"])

(fn executable []
  "The mpv binary, if installed."
  (hs.fnutils.find executables #(hs.fs.attributes $1)))

(fn players []
  "Running mpvs that can show video: all but audio-only ones, like EMMS's and
  the radio's (--no-video)."
  (icollect [_ app (ipairs (hs.application.runningApplications))]
    (when (and (or (= (: app :bundleID) :io.mpv) (= (: app :name) :mpv))
               (not (: (hs.execute (.. "/bin/ps -o args= -p " (: app :pid)))
                       :find "--no-video" 1 true)))
      app)))

(fn show [app]
  "Focus APP's window, or activate APP: macOS then switches to the Space of its
  window, which it can't open on a full screen Space."
  (let [window (hs.fnutils.find (: app :allWindows) #(: $1 :isStandard))]
    (if window
        (: window :focus)
        (: app :activate true))
    window))

(fn await-window [pid]
  "Show the window of the mpv with PID once it has opened it."
  (var tries 30)
  (fn check []
    (set tries (- tries 1))
    ;; Not applicationForPID: it logs an error until mpv has registered.
    (let [app (hs.fnutils.find (hs.application.runningApplications)
                               #(= (: $1 :pid) pid))]
      (when (and (not (and app (show app))) (> tries 0))
        (hs.timer.doAfter 0.15 check))))
  (check))

(fn open []
  "Show mpv's window, else open mpv's idle window, to drop files or URLs on."
  (let [player (. (players) 1)
        path (executable)]
    (if player
        (show player)
        (not path)
        (minibuffer.message "mpv is not installed")
        ;; Detached, so it outlives a Hammerspoon reload.
        (: (hs.task.new "/bin/sh"
                        (fn [_ out]
                          (let [pid (tonumber (out:match "%d+"))]
                            (when pid (await-window pid))))
                        ["-c" (.. "nohup '" path "' --player-operation-mode=pseudo-gui"
                                  " >/dev/null 2>&1 & echo $!")])
           :start))))

;; Commands for Emacs' mode keymap (config.fnl), which runs them with the app.
(local commands
       {:emms.play-or-pause {:title "Play or pause EMMS" :fn #(play-or-pause)}
        :emms.next {:title "EMMS's next track" :fn #(next-track)}
        :emms.previous {:title "EMMS's previous track" :fn #(prev-track)}
        :emms.seek-forward {:title "Seek 10 seconds forward" :fn #(seek-forward)}
        :emms.seek-backward {:title "Seek 10 seconds backward" :fn #(seek-backward)}
        :emms.seek-forward-more {:title "Seek a minute forward" :fn #(seek-forward-more)}
        :emms.seek-backward-more {:title "Seek a minute backward" :fn #(seek-backward-more)}
        :emms.volume-up {:title "Raise mpv's volume" :fn #(volume-up)}
        :emms.volume-down {:title "Lower mpv's volume" :fn #(volume-down)}
        :emms.mute {:title "Mute or unmute mpv" :fn #(mute)}
        :emms.now-playing {:title "Now Playing HUD" :fn #(show-now-playing)}})

{: play-or-pause
 : seek
 : seek-forward
 : seek-backward
 : seek-forward-more
 : seek-backward-more
 : show-now-playing
 : prev-track
 : next-track
 : volume-up
 : volume-down
 : mute
 : open
 : executable
 : commands
 : instances
 : request}
