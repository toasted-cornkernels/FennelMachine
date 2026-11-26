"
Microphone recording for the local voice interface.

Emacs can't get microphone access on macOS (the Homebrew Emacs.app is unsigned,
so TCC denies it without asking), while Hammerspoon.app is signed with the
audio-input entitlement. So Hammerspoon records and Emacs drives it over hs.ipc:

  hs -c 'return require(\"voice\").start()'
  hs -c 'return require(\"voice\").stop()'   -- path of the finished WAV

Emacs transcribes its own recordings; the global client here (`dictate', on
Ctrl-Cmd-L) transcribes and pastes into whatever app is in front, handing
over to Emacs's voice-dictate when Emacs is. Both go through the same learned
corrections (`correct', `remember'), kept in ~/.local/share/voice/.

Text comes in as hs arguments, never spliced into Lua code:

  hs -c 'return require(\"voice\").correct(_cli.args[2], _cli.args[3])' -- TEXT LANG

Everything runs against whisper-server on 127.0.0.1, and nothing happens unless
~/.config/voice/config.json exists, which Nix writes only on the personal Macs.
"
(local log (hs.logger.new "voice.fnl" "warning"))
(local vt (require :lib.voice-text))
(local hud (require :voice-hud))

(local home (os.getenv "HOME"))
(local config-file (.. home "/.config/voice/config.json"))
(local data-dir (.. home "/.local/share/voice"))
(local corrections-file (.. data-dir "/corrections.json"))
(local curl "/usr/bin/curl")
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

(fn levels [rec chunk]
  "Feed the microphone levels in ffmpeg's output CHUNK to the HUD. Chunks
can end mid-line, so the rest waits for the next one."
  (let [buffered (.. (or rec.pending "") chunk)
        cut (or (buffered:match ".*()\n") 1)]
    (set rec.pending (buffered:sub cut))
    (each [db (: (buffered:sub 1 (- cut 1)) :gmatch "RMS_level=(%S+)")]
      (hud.level (or (tonumber db) (- math.huge))))))

(fn start [hint cancel-hint]
  "Start recording 16 kHz mono from the default input, showing the HUD with
HINT on how to stop (and CANCEL-HINT on how to cancel while transcribing).
Returns \"ok\" or an error message."
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
                                  (when (and (not rec.ready?)
                                             (stdout:match "out_time_us=[1-9]"))
                                    (set rec.ready? true)
                                    (when (= current rec)
                                      (hud.set-phase :listening)))
                                  (when (= current rec)
                                    (levels rec stdout))
                                  true)
                                ["-hide_banner" "-loglevel" "error" "-nostdin"
                                 "-progress" "pipe:1" "-stats_period" "0.1"
                                 "-f" "avfoundation" "-i" ":default"
                                 "-t" (tostring max-seconds)
                                 ;; Levels for the HUD's meter, every 0.1 s
                                 ;; at 16 kHz; the audio itself is untouched.
                                 "-af" (.. "aresample=16000,asetnsamples=n=1600,"
                                           "astats=metadata=1:reset=1:"
                                           "measure_overall=RMS_level:"
                                           "measure_perchannel=none,"
                                           "ametadata=mode=print:"
                                           "key=lavfi.astats.Overall.RMS_level:"
                                           "file=/dev/stdout:direct=1")
                                 "-ac" "1" "-ar" "16000" "-c:a" "pcm_s16le"
                                 "-y" path])]
          (set rec.task task)
          (set current rec)
          (if (task:start)
              (do (hud.show {:language (or (hs.settings.get :voice.language) :en)
                             :hint (or hint "") :cancel-hint cancel-hint})
                  "ok")
              (do (set current nil)
                  "error: couldn't start ffmpeg"))))))

(fn ready? []
  "Whether audio is being captured yet."
  (if (and (recording?) current.ready?) "yes" "no"))

(fn stop [keep-hud]
  "Stop recording; returns the WAV's path, or an error message. The HUD goes,
unless KEEP-HUD: then it shows that transcription is under way."
  (if keep-hud (hud.set-phase :transcribing) (hud.hide))
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

;; Corrections ======================================

(fn read-corrections []
  (or (and (hs.fs.attributes corrections-file) (hs.json.read corrections-file))
      {}))

(fn correct [text language]
  "TEXT with the learned corrections for LANGUAGE (or all, for auto) applied."
  (vt.correct (or text "") (read-corrections) (or language :auto)))

(fn replace [text heard meant]
  "TEXT with whole-word HEARD replaced by MEANT."
  (vt.replace (or text "") (or heard "") (or meant "")))

(fn remember [language heard meant]
  "From now on, correct HEARD to MEANT in LANGUAGE (guessed from HEARD if auto).
Returns the language it went under."
  (let [language (if (or (= language nil) (= language :auto))
                     (vt.language-of heard)
                     language)
        corrections (read-corrections)
        entries (or (. corrections language) {})]
    ;; The same word in another case replaces the old entry.
    (each [old _ (pairs entries)]
      (when (= (old:lower) (heard:lower))
        (tset entries old nil)))
    (tset entries heard meant)
    (tset corrections language entries)
    (each [_ dir (ipairs [(.. home "/.local") (.. home "/.local/share") data-dir])]
      (when (not (hs.fs.attributes dir))
        (hs.fs.mkdir dir)))
    (if (hs.json.write corrections corrections-file true true)
        language
        (.. "error: couldn't write " corrections-file))))

;; Language ==========================================

(fn config []
  (and (enabled?) (hs.json.read config-file)))

(fn language []
  "The language to transcribe as, shared with Emacs."
  (or (hs.settings.get :voice.language) :en))

(fn set-language [lang]
  (hs.settings.set :voice.language lang)
  lang)

(fn languages []
  (let [cfg (or (config) {})]
    (doto (icollect [_ l (ipairs (or cfg.languages []))] l)
      (table.insert :auto))))

;; Global client ======================================

(local minibuffer (require :lib.minibuffer))

;; The dictation in flight: {:phase :starting|:listening|:transcribing
;;                           :app :window :language :task :timer}
(var dictation nil)
(var escape nil)
(var paused-emms false)

(fn say [msg]
  (minibuffer.message msg))

(fn emacs-running? []
  (not= nil (hs.application.find "org.gnu.Emacs" true)))

(fn emacs-media [form]
  "Pause or resume EMMS via voice.el, if Emacs runs; its music would end up in
the transcript otherwise."
  (when (emacs-running?)
    ((. (require :emacs) :emacsclient)
     ["-e" (.. "(when (require 'voice nil t) " form ")")]
     (fn [] nil))))

(fn finish []
  (when (and dictation dictation.timer)
    (dictation.timer:stop))
  (when escape
    (escape:disable))
  (when paused-emms
    (set paused-emms false)
    (emacs-media "(voice--resume-media)"))
  (set dictation nil)
  (hud.hide))

(fn same-place? [d]
  (let [app (hs.application.frontmostApplication)
        window (hs.window.focusedWindow)]
    (and app d.app (= (app:pid) (d.app:pid))
         (= (and window (window:id)) (and d.window (d.window:id))))))

(fn paste [text]
  "Paste TEXT where the cursor is, then put the clipboard back. Both writes
are marked transient so clipboard histories skip them."
  (let [saved (hs.pasteboard.readAllData)]
    (hs.pasteboard.writeAllData {"public.utf8-plain-text" text
                                 "org.nspasteboard.TransientType" ""})
    (hs.eventtap.keyStroke [:cmd] :v)
    (hs.timer.doAfter
     0.5
     (fn []
       (if (next (or saved {}))
           (do (tset saved "org.nspasteboard.TransientType" "")
               (hs.pasteboard.writeAllData saved))
           (hs.pasteboard.clearContents))))))

(fn deliver [d text]
  (if (same-place? d)
      (do (paste text)
          (say (.. "Voice: " text)))
      ;; Never paste into somewhere the user didn't start dictating.
      (do (hs.pasteboard.setContents text)
          (say (.. "Voice: focus moved, so copied instead: " text)))))

(fn transcribe-file [path lang callback]
  "Transcribe the WAV at PATH as LANG with the local server, then delete it and
call CALLBACK with the corrected text, or nil and what went wrong. Returns the
curl task, or nil if it couldn't start."
  (let [cfg (config)
        words (?. cfg :vocabulary lang)
        task (hs.task.new
              curl
              (fn [code stdout stderr]
                (os.remove path)
                (let [(ok json) (pcall hs.json.decode (or stdout ""))
                      text (and ok (= code 0) (= (type json) :table)
                                (= (type json.text) :string)
                                (vt.clean json.text))]
                  (if text
                      (callback (correct text lang))
                      (callback nil (or (stderr:match "[^\n]+")
                                        (stdout:match "[^\n]+")
                                        (.. "curl exited " code))))))
              ["--silent" "--show-error" "--fail-with-body"
               ;; Local only: never route through a proxy.
               "--noproxy" "*" "--max-time" "300"
               "-F" (.. "file=@" path)
               "--form-string" (.. "language=" lang)
               "--form-string" (.. "prompt=" (table.concat (or words []) ", "))
               "-F" "response_format=json"
               cfg.endpoint])]
    (if (task:start)
        task
        (do (os.remove path) nil))))

(fn transcribe [d path]
  (set d.phase :transcribing)
  (set d.task
       (transcribe-file path d.language
                        (fn [text err]
                          (when (= dictation d)
                            (finish)
                            (if (not text) (say (.. "Voice: transcription failed: " err))
                                (= text "") (say "Voice: heard nothing")
                                (deliver d text))))))
  (when (not d.task)
    (finish)
    (say "Voice: couldn't run curl")))

(fn cancel-dictation []
  "Stop the global dictation and throw it away."
  (when dictation
    (if (= dictation.phase :transcribing)
        (when dictation.task (dictation.task:terminate))
        (cancel))
    (finish)
    (say "Voice: cancelled")))

(fn start-dictation []
  (let [d {:phase :starting
           :app (hs.application.frontmostApplication)
           :window (hs.window.focusedWindow)
           :language (language)}
        result (start "⌃⌘L to stop  ·  esc to cancel" "esc to cancel")]
    (if (result:match "^error:")
        (say (.. "Voice: " (result:gsub "^error: " "")))
        (let [deadline (+ (hs.timer.secondsSinceEpoch) 5)]
          (set dictation d)
          (when (and (emacs-running?) (not paused-emms))
            (set paused-emms true)
            (emacs-media "(voice--pause-media)"))
          (when (not escape)
            (set escape (hs.hotkey.new [] :escape cancel-dictation)))
          (escape:enable)
          (set d.timer
               (hs.timer.doEvery
                0.1
                (fn []
                  (if (not= dictation d) nil
                      (= (ready?) "yes")
                      (do (d.timer:stop)
                          (set d.phase :listening))
                      (> (hs.timer.secondsSinceEpoch) deadline)
                      (do (cancel)
                          (finish)
                          (say "Voice: no audio from the microphone"))))))))))

(fn stop-dictation []
  (let [d dictation
        path (stop true)]
    (when d.timer (d.timer:stop))
    (when paused-emms
      (set paused-emms false)
      (emacs-media "(voice--resume-media)"))
    (if (path:match "^error:")
        (do (finish) (say (.. "Voice: " (path:gsub "^error: " ""))))
        (transcribe d path))))

(fn dictate []
  "Start dictating into the app in front, or stop and paste the transcript.
In Emacs, voice-dictate does it instead, inserting at point."
  (let [front (hs.application.frontmostApplication)]
    (if (not (enabled?)) (say "Voice isn't set up on this machine")
        (and (not dictation) front (= (front:bundleID) "org.gnu.Emacs"))
        ((. (require :emacs) :run-command) :voice-dictate)
        (= (?. dictation :phase) :transcribing)
        (say "Voice: still transcribing the last one")
        dictation (stop-dictation)
        (start-dictation))))

(fn choose-language []
  "Pick the language to transcribe as, for Emacs too."
  (when (enabled?)
    (let [chooser (hs.chooser.new
                   (fn [choice]
                     (when choice
                       (set-language choice.text)
                       (say (.. "Voice language: " choice.text)))))]
      (chooser:choices (icollect [_ l (ipairs (languages))]
                         {:text l :subText (if (= l (language)) "current" "")}))
      (chooser:show))))

(fn edit-corrections []
  ((. (require :emacs) :run-command) :voice-edit-corrections))

{: start : stop : cancel :ready ready? :recording recording? :enabled enabled?
 : correct : replace : remember : language :set_language set-language : languages
 :transcribe_file transcribe-file
 ;; For Emacs, once the transcript of a recording stopped with keep-hud is in.
 :transcribed #(do (hud.done) "ok")
 : dictate :cancel-dictation cancel-dictation :choose-language choose-language
 :edit-corrections edit-corrections}
