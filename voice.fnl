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
corrections (`correct', `remember'), kept in ~/.local/share/voice/, and
dictation carries out spoken markers (\"new line\", \"question mark\",
\"scratch that\", \"backspace\"; `markers'). While dictating, \"stop/pause/resume
dictation\" are listened for (see Spoken controls), and the text shows as it's
said, in the HUD and for Emacs in the buffer (see Live dictation).
`correct-dictation' fixes a word of the last global dictation in place (via
Accessibility; else it copies the fixed text) and remembers the fix.

Text comes in as hs arguments, never spliced into Lua code:

  hs -c 'return require(\"voice\").correct(_cli.args[2], _cli.args[3])' -- TEXT LANG

Everything runs against whisper-server on 127.0.0.1, and nothing happens unless
~/.config/voice/config.json exists, which Nix writes only on the personal Macs.
"
(local log (hs.logger.new "voice.fnl" "warning"))
(local vt (require :lib.voice-text))
(local va (require :lib.voice-audio))
(local vs (require :lib.voice-stream))
(local hud (require :voice-hud))

(local home (os.getenv "HOME"))
(local config-file (.. home "/.config/voice/config.json"))
(local data-dir (.. home "/.local/share/voice"))
(local corrections-file (.. data-dir "/corrections.json"))
(local curl "/usr/bin/curl")
(local max-seconds 300)
;; Listening for spoken controls: how much to transcribe, and what counts as sound
(local listen-seconds 3)
(local loud-db -50)
(local control-prompts
       {:en "Stop dictation. Pause dictation. Resume dictation."
        :ko "받아쓰기 중지, 받아쓰기 일시 중지, 받아쓰기 계속"
        :ja "音声入力終了、音声入力一時停止、音声入力再開"})

(fn enabled? []
  (not= nil (hs.fs.attributes config-file)))

(fn find-ffmpeg []
  (hs.fnutils.find [(.. "/etc/profiles/per-user/" (os.getenv "USER") "/bin/ffmpeg")
                    "/run/current-system/sw/bin/ffmpeg"
                    "/opt/homebrew/bin/ffmpeg"
                    "/usr/local/bin/ffmpeg"]
                   #(hs.fs.attributes $1)))

;; The running recording: {:task hs.task :path string :ready? boolean}, and
;; for dictation, what listening for spoken controls needs: see `start'.
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
      (let [db (or (tonumber db) (- math.huge))]
        (when (> db loud-db)
          (set rec.loud-at (hs.timer.secondsSinceEpoch)))
        (hud.level db)))))

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

;; Spoken controls ======================================
;; While dictating, a small whisper model (the config's listener) transcribes
;; the last few seconds whenever there has been sound, for "stop dictation",
;; "pause dictation" and "resume dictation" (and in Korean and Japanese).
;; Paused stretches are still recorded, to hear "resume", but left out of the
;; audio that `stop' returns.

(fn listener-endpoint []
  (?. (and (enabled?) (hs.json.read config-file)) :listener))

(fn pause-recording [rec]
  (when (not rec.paused)
    (let [at (va.data-size rec.path)]
      (when at
        (table.insert rec.ranges [rec.active-from at])
        (set rec.paused true)
        (hud.set-phase :paused)))))

(fn resume-recording [rec]
  (when rec.paused
    (set rec.active-from (or (va.data-size rec.path) rec.active-from))
    (set rec.paused false)
    (hud.set-phase :listening)))

(fn heard-control [rec control]
  (when (= current rec)
    (match control
      :pause (pause-recording rec)
      :resume (resume-recording rec)
      :stop (when (not rec.stopping)
              (set rec.stopping true)
              (rec.on-stop)))))

(fn check-tail [rec]
  "Transcribe the last few seconds of REC with the listener, and act on a
spoken control at their end."
  (let [lang (or (hs.settings.get :voice.language) :en)
        out (.. rec.path ".tail.wav")]
    (when (va.tail rec.path listen-seconds out)
      (let [task (hs.task.new
                  curl
                  (fn [code stdout _]
                    (os.remove out)
                    (set rec.checking nil)
                    (let [(ok json) (pcall hs.json.decode (or stdout ""))]
                      (when (and (= code 0) ok (= (type json) :table)
                                 (= (type json.text) :string))
                        (let [control (vt.voice-control (vt.clean json.text))]
                          (when control (heard-control rec control))))))
                  ["--silent" "--noproxy" "*" "--max-time" "10"
                   "-F" (.. "file=@" out)
                   "--form-string" (.. "language=" lang)
                   "--form-string" (.. "prompt=" (or (. control-prompts lang) ""))
                   "-F" "response_format=json"
                   rec.listener])]
        (if (task:start)
            (set rec.checking task)
            (os.remove out))))))

(fn listen-tick [rec]
  "Check again if there has been sound since the last check began, in the
last few seconds."
  (let [now (hs.timer.secondsSinceEpoch)]
    (when (and (= current rec) rec.ready? (not rec.checking) (not rec.stopping)
               rec.loud-at (> rec.loud-at (or rec.checked-at 0))
               (< (- now rec.loud-at) listen-seconds))
      (set rec.checked-at now)
      (check-tail rec))))

(fn stop-listening [rec]
  (when rec.listen-timer (rec.listen-timer:stop))
  (when rec.checking (rec.checking:terminate))
  (when rec.live (set rec.live.done true)))

(fn emacs-spoken-stop []
  ((. (require :emacs) :emacsclient)
   ["-e" "(when (fboundp 'voice-spoken-stop) (voice-spoken-stop))"]
   (fn [] nil)))

;; Live dictation ======================================
;; With live on, the listener instead transcribes the recording's growing tail
;; over and over (lib/voice-stream), so the text comes in as it's said: in the
;; HUD, and for Emacs into the buffer (voice.el's voice--live). Spoken controls
;; are heard in the same passes. The text is the small model's; the full one
;; still transcribes the whole recording at the end.

(local live-step 0.5) ; seconds of new audio before the next pass
(local pause-keep 4) ; seconds kept while paused, to hear "resume dictation"
(local bytes-per-second 32000)

(fn elisp-string [s]
  (.. "\"" (: (: s :gsub "\\" "\\\\") :gsub "\"" "\\\"") "\""))

(fn push-emacs [live]
  "Send LIVE's latest text to Emacs, one emacsclient at a time."
  (if live.pushing
      (set live.dirty true)
      (do (set live.pushing true)
          (set live.dirty false)
          ((. (require :emacs) :emacsclient)
           ["-e" (.. "(when (fboundp 'voice--live) (voice--live "
                     (elisp-string live.text) " " (elisp-string live.preview) "))")]
           (fn []
             (set live.pushing false)
             (when (and live.dirty (not live.done))
               (push-emacs live)))))))

(fn live-render [rec]
  "Show what's been heard so far: the committed text with its markers carried
out, except an unfinished marker at its end (\"question\" might become \"?\"),
which goes with the words still pending to the preview."
  (let [live rec.live
        st live.stream
        raw (vs.committed-text st)
        cut (vt.pending-marker raw)
        done-part (if cut (raw:sub 1 (- cut 1)) raw)
        held (if cut (raw:sub cut) "")
        text (vt.markers (correct (vt.clean done-part) live.language))
        pending (.. held (vs.tentative-text st))
        ;; Keep the space before the pending words, if any.
        preview (.. (if (and (pending:match "^%s") (not= text "")) " " "")
                    (vt.clean pending))]
    (when (or (not= text live.text) (not= preview live.preview))
      (set live.text text)
      (set live.preview preview)
      (hud.live text preview)
      (when live.emacs (push-emacs live)))))

(fn live-control [rec words]
  "Act on a spoken control at the end of what's been said since the last one."
  (let [live rec.live
        st live.stream
        said (if rec.paused
                 (table.concat (icollect [_ w (ipairs words)] w.word))
                 (.. (table.concat (fcollect [i (+ live.control-mark 1) (length st.committed)]
                                     (. st.committed i :text)))
                     (vs.tentative-text st)))
        control (vt.voice-control (vt.clean said))]
    (when control
      (set live.control-mark (length st.committed))
      (when (= control :resume)
        (vs.restart st (/ (or (va.data-size rec.path) 0) bytes-per-second)))
      (when (= control :pause)
        (vs.restart st st.buffer-start))
      (heard-control rec control))))

(fn live-pass [rec upto]
  "Transcribe REC's buffer up to UPTO bytes, then take in the words."
  (let [live rec.live
        st live.stream
        from (* 2 (math.floor (* st.buffer-start 16000)))
        now (/ upto bytes-per-second)
        out (.. rec.path ".live.wav")
        vocabulary (vt.prompt (?. (config) :vocabulary live.language) live.language)
        context (vt.clean (vs.prompt st))]
    (when (va.splice rec.path [[from upto]] out)
      (let [started (hs.timer.secondsSinceEpoch)
            task (hs.task.new
                  curl
                  (fn [code stdout _]
                    (os.remove out)
                    (set rec.checking nil)
                    (let [(ok json) (pcall hs.json.decode (or stdout ""))]
                      (when (and (= current rec) (not rec.stopping) (= code 0) ok
                                 (= (type json) :table))
                        (let [words []]
                          (each [_ s (ipairs (or json.segments []))]
                            (each [_ w (ipairs (vs.segment-words s))]
                              (table.insert words w)))
                          (if rec.paused
                              (live-control rec words)
                              (do (vs.update st words now
                                             (or (not rec.loud-at)
                                                 (> (- started rec.loud-at) 1)))
                                  (live-control rec words)
                                  (when (and (= current rec) (not rec.paused)
                                             (not rec.stopping))
                                    (live-render rec))))))))
                  ["--silent" "--noproxy" "*" "--max-time" "10"
                   "-F" (.. "file=@" out)
                   "--form-string" (.. "language=" live.language)
                   "--form-string" (.. "prompt=" (vt.clean (.. vocabulary " " context)))
                   "-F" "response_format=verbose_json"
                   "-F" "temperature_inc=0"
                   "-F" (.. "audio_ctx=" (+ 64 (math.ceil (* 50 (- now st.buffer-start)))))
                   rec.listener])]
        (if (task:start)
            (set rec.checking task)
            (os.remove out))))))

(fn live-tick [rec]
  "Start the next pass once there's enough new audio, and the last is done."
  (let [live rec.live
        st live.stream
        upto (and (= current rec) rec.ready? (not rec.checking) (not rec.stopping)
                  (va.data-size rec.path))
        now (and upto (/ upto bytes-per-second))
        wall (hs.timer.secondsSinceEpoch)]
    (when (and now (>= (- now live.passed-at) live-step))
      (when (and rec.paused (> (- now st.buffer-start) pause-keep))
        (vs.restart st (- now pause-keep)))
      ;; Nothing heard lately and nothing pending: no need to ask whisper.
      (if (and (or (not rec.loud-at) (> (- wall rec.loud-at) 1.5))
               (= 0 (length st.previous)))
          (do (vs.idle st now)
              (set live.passed-at now))
          (do (set live.passed-at now)
              (live-pass rec upto))))))

(fn sweep []
  "Delete recordings left behind by a crash or reload: anything of ours in the
temporary directory untouched for 15 minutes, past any recording (5 at most)
and its transcription (5 more)."
  (let [dir (hs.fs.temporaryDirectory)
        cutoff (- (os.time) (* 15 60))]
    (each [name (hs.fs.dir dir)]
      (when (name:match "^voice%-%d+%-%d+.*%.wav$")
        (let [file (.. dir name)
              modified (hs.fs.attributes file :modification)]
          (when (and modified (< modified cutoff))
            (os.remove file)))))))

;; For testing: a WAV to play in real time instead of the microphone, until reload.
(var test-input nil)

(fn input-args [args]
  "ARGS for ffmpeg, recording from `test-input' if that's set."
  (if (not test-input) args
      (let [out []
            at (hs.fnutils.indexOf args "avfoundation")]
        (each [i a (ipairs args)]
          (if (= i (- at 1)) (table.insert out "-re")
              (= i at) nil
              (= i (+ at 2)) (table.insert out test-input)
              (table.insert out a)))
        out)))

(fn start [hint cancel-hint listen live]
  "Start recording 16 kHz mono from the default input, showing the HUD with
HINT on how to stop (and CANCEL-HINT on how to cancel while transcribing).
For dictation, LISTEN says how a spoken \"stop dictation\" stops it: a
function, or \"emacs\" for voice.el's; pause and resume are handled here.
LIVE (\"live\", or \"emacs\" to also send it to voice.el) shows the text
as it's said. Returns \"ok\" or an error message."
  (let [ffmpeg (find-ffmpeg)]
    (if (not (enabled?)) "error: voice isn't set up on this machine"
        (recording?) "error: already recording"
        (not ffmpeg) "error: ffmpeg not found"
        (let [_ (sweep)
              path (.. (hs.fs.temporaryDirectory) "voice-"
                       (os.date "%Y%m%d-%H%M%S") ".wav")
              on-stop (if (= listen "emacs") emacs-spoken-stop
                          (= (type listen) :function) listen)
              rec {: path :ready? false : on-stop :ranges [] :active-from 0
                   :listener (and on-stop (listener-endpoint))}
              _ (when (and rec.listener (or (= live "live") (= live "emacs")))
                  (set rec.live {:stream (vs.new) :language (language)
                                 :emacs (= live "emacs") :text "" :preview ""
                                 :passed-at 0 :control-mark 0}))
              task (hs.task.new ffmpeg
                                (fn [code _ stderr]
                                  (when (and (not= code 0) (not= code 255))
                                    (log.w "ffmpeg exited" code stderr))
                                  ;; Ended by itself, at the length limit:
                                  ;; the audio waits for stop as usual.
                                  (stop-listening rec)
                                  (when (= current rec)
                                    (hud.set-phase :ended)))
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
                                (input-args
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
                                 ;; Write as it comes, not in 256 KB blocks:
                                 ;; listening reads the file while it grows.
                                 "-flush_packets" "1"
                                 "-y" path]))]
          (set rec.task task)
          (set current rec)
          (when rec.listener
            (set rec.listen-timer
                 (if rec.live
                     (hs.timer.doEvery 0.1 #(live-tick rec))
                     (hs.timer.doEvery 0.5 #(listen-tick rec)))))
          (if (task:start)
              (do (hud.show {:language (or (hs.settings.get :voice.language) :en)
                             :hint (or hint "") :cancel-hint cancel-hint
                             :live (not= nil rec.live)})
                  "ok")
              (do (set current nil)
                  (stop-listening rec)
                  "error: couldn't start ffmpeg"))))))

(fn ready? []
  "Whether audio is being captured yet."
  (if (and (recording?) current.ready?) "yes" "no"))

(fn stop [keep-hud]
  "Stop recording; returns the WAV's path, or an error message. The HUD goes,
unless KEEP-HUD: then it shows that transcription is under way."
  (if keep-hud (hud.set-phase :transcribing) (hud.hide))
  (if (not current) "error: not recording"
      (let [rec current
            {: task : path} rec]
        (set current nil)
        (stop-listening rec)
        (when (task:isRunning)
          ;; SIGINT lets ffmpeg write the WAV header before exiting.
          (task:interrupt)
          (task:waitUntilExit))
        (if (not (hs.fs.attributes path)) "error: no audio was written"
            (= 0 (length rec.ranges)) path
            ;; Paused at some point: only what was said while not paused.
            (let [ranges (icollect [_ r (ipairs rec.ranges)] r)
                  _ (when (not rec.paused) (table.insert ranges [rec.active-from nil]))
                  out (va.splice path ranges (pick-values 1 (path:gsub "%.wav$" "-spoken.wav")))]
              (os.remove path)
              (or out "error: couldn't splice the audio"))))))

(fn cancel []
  "Stop recording and discard the audio."
  (let [result (stop)]
    (when (not (result:match "^error:"))
      (os.remove result))
    "ok"))

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

(var last nil)

(fn ax [f]
  "F's result, or nil if the Accessibility call fails, as many apps' do."
  (let [(ok result) (pcall f)]
    (when ok result)))

(fn focused-element []
  (ax #(: (hs.axuielement.systemWideElement) :attributeValue :AXFocusedUIElement)))

(fn text-at [element location len]
  (when (and element location (>= location 0))
    (let [s (ax #(element:parameterizedAttributeValue
                  :AXStringForRange {: location :length len}))]
      (when (= (type s) :string) s))))

(fn deliver [d text]
  (if (same-place? d)
      (let [element (focused-element)
            selection (and element (ax #(element:attributeValue :AXSelectedTextRange)))
            at (and (= (type selection) :table) selection.location)
            before (and at (> at 0) (text-at element (- at 1) 1))
            inserted (if (vt.needs-space? before text) (.. " " text) text)]
        (paste inserted)
        ;; Where it went, so `correct-dictation' can fix it in place.
        (set last {: text :language d.language :app d.app : element
                   :location (and at (+ at (- (length inserted) (length text))))})
        (say (.. "Voice: " text "   (⌘SPC v c to fix a word)")))
      ;; Never paste into somewhere the user didn't start dictating.
      (do (hs.pasteboard.setContents text)
          (say (.. "Voice: focus moved, so copied instead: " text)))))

(fn prompt [lang]
  "Whisper's prompt for LANG, from the configured vocabulary."
  (vt.prompt (?. (config) :vocabulary lang) lang))

(fn words [text]
  "TEXT's words from macOS's on-device tokenizer, which knows Japanese, or nil."
  (let [(ok result)
        (hs.osascript.javascript
         (.. "ObjC.import('NaturalLanguage');
const s = $(" (hs.json.encode [text]) "[0]);
const t = $.NLTokenizer.alloc.initWithUnit($.NLTokenUnitWord);
t.string = s;
const ranges = t.tokensForRange($.NSMakeRange(0, s.length));
const out = [];
for (let i = 0; i < ranges.count; i++) {
  out.push(s.substringWithRange(ranges.objectAtIndex(i).rangeValue).js);
}
out;"))]
    (when (and ok (= (type result) :table)) result)))

(fn candidates [text]
  "Words of TEXT that might have been misheard, one per line."
  (table.concat (vt.word-candidates text (words text)) "\n"))

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
               "--form-string" (.. "prompt=" (vt.prompt words lang))
               "-F" "response_format=json"
               cfg.endpoint])]
    (if (task:start)
        task
        (do (os.remove path) nil))))

(fn markers [text]
  "TEXT's spoken dictation markers carried out (\"new line\", \"question
mark\", \"stop dictation\", \"scratch that\"), as JSON {text, stopped,
scratch} so that leading and trailing newlines survive the trip to Emacs;
scratch: take back the previous dictation."
  (let [(text stopped scratch) (vt.markers (or text ""))]
    (hs.json.encode {: text : stopped : scratch})))

(fn take-back []
  "Delete the last dictation where it was pasted, for a \"scratch that\" said
on its own, if it's still there and that app is in front. Whether it was."
  (let [{: element : location : text : app} (or last {})
        front (hs.application.frontmostApplication)
        len (and text (vt.utf16-length text))]
    (when (and len front app (= (front:pid) (app:pid))
               (= (text-at element location len) text)
               (ax #(element:setAttributeValue :AXSelectedTextRange
                                               {: location :length len})))
      (hs.eventtap.keyStroke [] :delete 0)
      (set last nil)
      true)))

(fn transcribe [d path]
  (set d.phase :transcribing)
  (set d.task
       (transcribe-file path d.language
                        (fn [text err]
                          (when (= dictation d)
                            (finish)
                            (if (not text) (say (.. "Voice: transcription failed: " err))
                                (= text "") (say "Voice: heard nothing")
                                (let [(text _ scratch-previous) (vt.markers text)
                                      took-back (and scratch-previous (take-back))]
                                  (if (not= text "") (deliver d text)
                                      took-back (say "Voice: took back the last dictation")
                                      scratch-previous
                                      (say "Voice: couldn't take back the last dictation here")
                                      (say "Voice: nothing left to type"))))))))
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

(var stop-dictation nil)

(fn start-dictation []
  (let [d {:phase :starting
           :app (hs.application.frontmostApplication)
           :window (hs.window.focusedWindow)
           :language (language)}
        result (start "⌃⌘L to stop  ·  esc to cancel" "esc to cancel"
                      #(when (and (= dictation d) (= d.phase :listening))
                         (stop-dictation))
                      "live")]
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

(set stop-dictation (fn []
  (let [d dictation
        path (stop true)]
    (when d.timer (d.timer:stop))
    (when paused-emms
      (set paused-emms false)
      (emacs-media "(voice--resume-media)"))
    (if (path:match "^error:")
        (do (finish) (say (.. "Voice: " (path:gsub "^error: " ""))))
        (transcribe d path)))))

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

(fn fix-in-place [fixed]
  "Replace the last dictation with FIXED where it was pasted, if it's still
there and that app is in front; else copy FIXED. Whether it was replaced."
  (let [{: element : location : text : app} last
        front (hs.application.frontmostApplication)
        len (vt.utf16-length text)]
    (if (and front app (= (front:pid) (app:pid))
             (= (text-at element location len) text)
             (ax #(element:setAttributeValue :AXSelectedTextRange
                                             {: location :length len})))
        (do (paste fixed)
            (set last.text fixed)
            true)
        (do (hs.pasteboard.setContents fixed)
            false))))

(fn correct-as [heard meant]
  (let [fixed (vt.replace last.text heard meant)
        language (remember last.language heard meant)]
    (if (language:match "^error:")
        (say (.. "Voice: " language))
        (= fixed last.text)
        (say (.. "Voice: " heard " → " meant ", for " language
                 " from now on (not in the last dictation)"))
        (fix-in-place fixed)
        (say (.. "Voice: " heard " → " meant ", for " language " from now on"))
        (say (.. "Voice: " heard " → " meant ", for " language
                 " from now on; couldn't edit it there, so copied the fixed text")))))

(fn correct-dictation []
  "Fix a misheard word of the last dictation, there and from now on.
In Emacs, voice-correct does it instead."
  (let [front (hs.application.frontmostApplication)]
    (if (not (enabled?)) (say "Voice isn't set up on this machine")
        (and front (= (front:bundleID) "org.gnu.Emacs"))
        ((. (require :emacs) :run-command) :voice-correct)
        (not last) (say "Voice: no dictation to correct")
        (minibuffer.completing-read
         "Heard (wrong): "
         (icollect [_ w (ipairs (vt.word-candidates last.text (words last.text)))]
           {:text w})
         {:on-accept
          (fn [candidate input]
            (let [heard (if candidate candidate.text input)]
              (if (= (heard:match "^%s*(.-)%s*$") "")
                  (say "Voice: nothing to correct")
                  (do (minibuffer.completing-read
                       (.. "Meant instead of \"" heard "\": ") []
                       {:on-accept (fn [_ meant]
                                     (if (= meant "")
                                         (say "Voice: give what was meant")
                                         (correct-as heard meant)))})
                      (minibuffer.set-input heard)))))}))))

(fn edit-corrections []
  ((. (require :emacs) :run-command) :voice-edit-corrections))

{: start : stop : cancel :ready ready? :recording recording? :enabled enabled?
 : correct : replace : remember : language :set_language set-language : languages
 :transcribe_file transcribe-file : prompt : candidates : markers
 ;; For Emacs, once the transcript of a recording stopped with keep-hud is in.
 :transcribed #(do (hud.done) "ok")
 :test_input #(do (set test-input (if (= $1 "") nil $1)) "ok")
 : dictate :cancel-dictation cancel-dictation :choose-language choose-language
 :edit-corrections edit-corrections :correct-dictation correct-dictation}
