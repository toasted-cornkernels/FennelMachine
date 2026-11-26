"
Microphone recording for the local voice interface.

Emacs can't get microphone access on macOS (the Homebrew Emacs.app is unsigned,
so TCC denies it without asking), while Hammerspoon.app is signed with the
audio-input entitlement. So Hammerspoon records and Emacs drives it over hs.ipc:

  hs -c 'return require(\"voice\").start()'
  hs -c 'return require(\"voice\").stop()'   -- path of the finished WAV

Transcription is the client's job (whisper-server on 127.0.0.1). Everything here
is inert unless ~/.config/voice/config.json exists, which Nix writes only on the
personal Macs.
"
(local log (hs.logger.new "voice.fnl" "warning"))

(local config-file (.. (os.getenv "HOME") "/.config/voice/config.json"))
(local max-seconds 300)

(fn enabled? []
  (not= nil (hs.fs.attributes config-file)))

(fn find-ffmpeg []
  (hs.fnutils.find [(.. "/etc/profiles/per-user/" (os.getenv "USER") "/bin/ffmpeg")
                    "/run/current-system/sw/bin/ffmpeg"
                    "/opt/homebrew/bin/ffmpeg"
                    "/usr/local/bin/ffmpeg"]
                   #(hs.fs.attributes $1)))

;; The running recording: {:task hs.task :path string :ready? boolean}
(var current nil)

(fn recording? []
  (and current (current.task:isRunning)))

(fn start []
  "Start recording 16 kHz mono from the default input. Returns \"ok\" or an
error message."
  (let [ffmpeg (find-ffmpeg)]
    (if (not (enabled?)) "error: voice isn't set up on this machine"
        (recording?) "error: already recording"
        (not ffmpeg) "error: ffmpeg not found"
        (let [path (.. (hs.fs.temporaryDirectory) "voice-"
                       (os.date "%Y%m%d-%H%M%S") ".wav")
              rec {: path :ready? false}
              task (hs.task.new ffmpeg
                                (fn [code _ stderr]
                                  (when (and (not= code 0) (not= code 255))
                                    (log.w "ffmpeg exited" code stderr)))
                                ;; Opening the device takes a moment; ffmpeg's
                                ;; progress reports tell when audio arrives.
                                (fn [_ stdout _]
                                  (when (stdout:match "out_time_us=[1-9]")
                                    (set rec.ready? true))
                                  (not rec.ready?))
                                ["-hide_banner" "-loglevel" "error" "-nostdin"
                                 "-progress" "pipe:1" "-stats_period" "0.1"
                                 "-f" "avfoundation" "-i" ":default"
                                 "-t" (tostring max-seconds)
                                 "-ac" "1" "-ar" "16000" "-c:a" "pcm_s16le"
                                 "-y" path])]
          (set rec.task task)
          (set current rec)
          (if (task:start) "ok" "error: couldn't start ffmpeg")))))

(fn ready? []
  "Whether audio is being captured yet."
  (if (and (recording?) current.ready?) "yes" "no"))

(fn stop []
  "Stop recording; returns the WAV's path, or an error message."
  (if (not current) "error: not recording"
      (let [{: task : path} current]
        (set current nil)
        (when (task:isRunning)
          ;; SIGINT lets ffmpeg write the WAV header before exiting.
          (task:interrupt)
          (task:waitUntilExit))
        (if (hs.fs.attributes path) path "error: no audio was written"))))

(fn cancel []
  "Stop recording and discard the audio."
  (let [result (stop)]
    (when (not (result:match "^error:"))
      (os.remove result))
    "ok"))

{: start : stop : cancel :ready ready? :recording recording? :enabled enabled?}
