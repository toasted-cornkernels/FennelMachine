;; Media controls for mpv, through its JSON IPC socket (--input-ipc-server, or
;; the one mpv-scripts/ipc-socket.lua gives every other mpv).
;; mpv instances started by EMMS are driven through EMMS for play/pause and
;; tracks, so EMMS keeps its playlist and paused state in sync.

(local minibuffer (require :lib.minibuffer))
(local emacs (require :emacs))
(local now-playing (require :now-playing))

(local timeout 1)
(local volume-step 5)
(local pending {})
(var last-socket nil)

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

(fn playing? [instance callback]
  (request instance.socket [["get_property" "pause"]
                            ["get_property" "idle-active"]]
           (fn [[pause idle]]
             (callback (and pause idle
                            (= pause.data false)
                            (= idle.data false))))))

(fn choose [found answers]
  "The playing instance, else the one controlled last, else the newest."
  (var choice nil)
  (each [i instance (ipairs found) &until choice]
    (when (. answers i) (set choice instance)))
  (each [_ instance (ipairs found) &until choice]
    (when (= instance.socket last-socket) (set choice instance)))
  (or choice (. found 1)))

(fn with-instance [f]
  "Call F with the mpv to control (see `choose`)."
  (fn control [instance]
    (set last-socket instance.socket)
    (f instance))
  (instances
   (fn [found]
     (if (= 0 (length found))
         (minibuffer.message "No mpv with an IPC socket is running")
         (= 1 (length found))
         (control (. found 1))
         (let [answers {}
               state {:remaining (length found)}]
           (each [i instance (ipairs found)]
             (playing? instance
                       (fn [yes?]
                         (tset answers i yes?)
                         (set state.remaining (- state.remaining 1))
                         (when (= 0 state.remaining)
                           (control (choose found answers)))))))))))

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
         (emacs.emacsclient ["-e" form])
         (var tries 15)
         (fn poll []
           (set tries (- tries 1))
           (get (fn [now]
                  (if (or (not= now before) (= tries 0))
                      (if (= property "path")
                          (show-hud instance)
                          (show-status instance))
                      (hs.timer.doAfter 0.2 poll)))))
         (hs.timer.doAfter 0.2 poll))))

(fn command [instance cmd on-error after]
  (request instance.socket [cmd]
           (fn [[response]]
             (if (and response (= response.error :success))
                 ((or after show-status) instance)
                 (minibuffer.message
                  (if response (or on-error response.error)
                      "mpv isn't responding"))))))

(fn play-or-pause []
  "Toggle pause in mpv (EMMS's mpv through EMMS)."
  (with-instance
   (fn [instance]
     (if instance.emms?
         (emms "(if emms-player-playing-p (emms-pause) (emms-start))" instance
               "pause")
         (command instance ["cycle" "pause"])))))

(fn next-track []
  "Play the next track in mpv's playlist (or EMMS's)."
  (with-instance
   (fn [instance]
     (if instance.emms?
         (emms "(emms-next)" instance "path")
         (command instance ["playlist-next"] "No next track" show-hud)))))

(fn prev-track []
  "Play the previous track in mpv's playlist (or EMMS's)."
  (with-instance
   (fn [instance]
     (if instance.emms?
         (emms "(emms-previous)" instance "path")
         (command instance ["playlist-prev"] "No previous track" show-hud)))))

(fn change-volume [delta]
  (with-instance
   (fn [instance]
     (request instance.socket [["add" "volume" delta]
                               ["get_property" "volume"]]
              (fn [[_ volume]]
                (minibuffer.message
                 (if (and volume (= volume.error :success))
                     (string.format "mpv volume: %d%%" volume.data)
                     "mpv isn't responding")))))))

(fn clock [seconds]
  (let [s (math.floor (or seconds 0))]
    (if (>= s 3600)
        (string.format "%d:%02d:%02d" (// s 3600) (// (% s 3600) 60) (% s 60))
        (string.format "%d:%02d" (// s 60) (% s 60)))))

(fn seek [seconds]
  "Seek mpv SECONDS forward (negative: backward) in the current track."
  (with-instance
   (fn [instance]
     (fn report []
       (request instance.socket [["get_property" "time-pos"]
                                 ["get_property" "duration"]]
                (fn [[pos duration]]
                  (minibuffer.message
                   (.. "mpv: " (clock (and pos pos.data))
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
  "Seek mpv 10 seconds forward."
  (seek 10))

(fn seek-backward []
  "Seek mpv 10 seconds backward."
  (seek -10))

(fn seek-forward-more []
  "Seek mpv a minute forward."
  (seek 60))

(fn seek-backward-more []
  "Seek mpv a minute backward."
  (seek -60))

(fn volume-up []
  "Raise mpv's volume."
  (change-volume volume-step))

(fn volume-down []
  "Lower mpv's volume."
  (change-volume (- volume-step)))

(fn mute []
  "Toggle mute in mpv."
  (with-instance
   (fn [instance]
     (request instance.socket [["cycle" "mute"] ["get_property" "mute"]]
              (fn [[_ muted]]
                (minibuffer.message
                 (if (not (and muted (= muted.error :success)))
                     "mpv isn't responding"
                     muted.data "mpv muted"
                     "mpv unmuted")))))))

(fn show-now-playing []
  "Show the Now Playing HUD (album art, title, artist, album, progress), or
  hide it if it is showing."
  (if (now-playing.visible?)
      (now-playing.hide)
      (with-instance show-hud)))

(fn open-emms []
  "Show EMMS's playlist (or the music directory) in an Emacs frame named EMMS."
  (emacs.emacsclient
   ["-e"
    "(let ((frame (or (seq-find (lambda (f) (equal (frame-parameter f 'name) \"EMMS\"))
                               (frame-list))
                     (make-frame '((name . \"EMMS\"))))))
       (select-frame-set-input-focus frame)
       (with-selected-frame frame
         (if (and (boundp 'emms-playlist-buffer) (buffer-live-p emms-playlist-buffer))
             (emms-playlist-mode-go)
           (dired (if (boundp 'emms-source-file-default-directory)
                      emms-source-file-default-directory
                    \"~/Music\")))))"])
  (hs.timer.doAfter 0.2 #(hs.application.launchOrFocus :Emacs)))

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
 : open-emms
 : instances
 : request}
