"
Keyboard macros, like Emacs' kmacro: record the keys you type, replay them,
name and keep them across reloads.

Recording stops from the leader menu, so the keys from the last leader chord
on (the stop command itself) are left out. A macro may still use the leader
in the middle: replayed keys run hotkeys like typed ones.
"
(local minibuffer (require :lib.minibuffer))

(local last-key "kmacro.last")
(local named-key "kmacro.named")
(local step-delay 0.004)

(local types hs.eventtap.event.types)
(local mod-names [:cmd :alt :ctrl :shift :fn])

;; {:tap :events :menubar} while recording.
(var recording nil)
;; The replay timer, kept so it isn't garbage collected, and set while
;; playing so that a recording leaves the replayed keys out.
(var playing nil)

(fn mods-of [flags]
  (icollect [_ m (ipairs mod-names)] (when (. flags m) m)))

(fn same-mods? [a b]
  (and (= (length a) (length b))
       (hs.fnutils.every a #(hs.fnutils.contains b $1))))

(fn leader-chords []
  "The keys that open a leader menu, as {:code :mods}."
  (let [(ok config) (pcall require :config)]
    (icollect [_ k (ipairs (or (and ok config.keys) []))]
      (when (and (= (type k.action) :string) (k.action:match "^lib%.modal:"))
        {:code (. hs.keycodes.map k.key) :mods (or k.mods [])}))))

(fn leader? [chords event]
  (hs.fnutils.some chords #(and (= $1.code event.c) (same-mods? $1.mods event.f))))

(fn trim-leader [events]
  "EVENTS up to the last leader chord, which started the command that stopped
  the recording. The hotkey may show up twice in a row; both go."
  (let [chords (leader-chords)
        last (accumulate [found nil i e (ipairs events)]
               (if (and e.d (leader? chords e)) i found))]
    (if (not last)
        events
        (do
          (var start last)
          (while (and (> start 1) (leader? chords (. events (- start 1))))
            (set start (- start 1)))
          (icollect [i e (ipairs events)] (when (< i start) e))))))

(fn recording? [] (not= recording nil))

(fn start []
  "Start recording keys, like kmacro-start-macro."
  (if recording
      (minibuffer.message "Already defining a keyboard macro")
      (let [events []
            tap (hs.eventtap.new [types.keyDown types.keyUp]
                                 (fn [event]
                                   (when (not playing)
                                     (table.insert events
                                                   {:d (= (event:getType) types.keyDown)
                                                    :c (event:getKeyCode)
                                                    :f (mods-of (event:getFlags))}))
                                   false))
            menubar (hs.menubar.new)]
        (when menubar
          (menubar:setTitle "● Macro")
          (menubar:setTooltip "Defining a keyboard macro (s-SPC K ) stops)"))
        (set recording {: tap : events : menubar})
        (tap:start)
        (minibuffer.message "Defining keyboard macro..."))))

(fn key-downs [events]
  (accumulate [n 0 _ e (ipairs events)] (if e.d (+ n 1) n)))

(fn stop []
  "Stop recording and keep the macro as the last one, like kmacro-end-macro."
  (if (not recording)
      (minibuffer.message "Not defining a keyboard macro")
      (let [{: tap : events : menubar} recording
            events (trim-leader events)]
        (tap:stop)
        (when menubar (menubar:delete))
        (set recording nil)
        (if (= (key-downs events) 0)
            (minibuffer.message "Empty keyboard macro; the last one is kept")
            (do (hs.settings.set last-key events)
                (minibuffer.message (.. "Keyboard macro defined: " (key-downs events)
                                        " keys")))))))

(fn toggle []
  "Start recording, or stop when recording."
  (if recording (stop) (start)))

(fn play [events]
  "Post EVENTS one by one, a timer tick apart so apps keep up."
  (if playing
      (minibuffer.message "A keyboard macro is already running")
      (do
        (var i 0)
        (fn step []
          (set i (+ i 1))
          (let [e (. events i)]
            (if e
                (do (: (hs.eventtap.event.newKeyEvent e.f e.c e.d) :post)
                    (set playing (hs.timer.doAfter step-delay step)))
                (set playing nil))))
        (set playing true)
        (step))))

(fn call-last []
  "Replay the last macro, like kmacro-call-macro."
  (if recording
      (minibuffer.message "Can't replay while defining a keyboard macro")
      (let [events (hs.settings.get last-key)]
        (if (and events (> (length events) 0))
            (play events)
            (minibuffer.message "No keyboard macro defined")))))

(fn named [] (or (hs.settings.get named-key) {}))

(fn name-last []
  "Keep the last macro under a name, like kmacro-name-last-macro."
  (let [events (hs.settings.get last-key)]
    (if (not events)
        (minibuffer.message "No keyboard macro defined")
        (minibuffer.completing-read
         "Name for the last macro: " []
         {:on-accept (fn [_ input]
                       (let [name (input:match "^%s*(.-)%s*$")]
                         (if (= name "")
                             (minibuffer.message "No name given")
                             (let [saved (named)]
                               (tset saved name events)
                               (hs.settings.set named-key saved)
                               (minibuffer.message (.. "Saved macro " name))))))}))))

(fn read-named [prompt f]
  (let [saved (named)
        names (icollect [name (pairs saved)] name)]
    (table.sort names)
    (if (= (length names) 0)
        (minibuffer.message "No named keyboard macros")
        (minibuffer.completing-read
         prompt
         (icollect [_ name (ipairs names)]
           {:text name :annotation (.. (key-downs (. saved name)) " keys")})
         {:on-accept (fn [c] (when c (f c.text (. saved c.text))))}))))

(fn run-named []
  "Replay a named macro; it becomes the last one."
  (read-named "Run macro: "
              (fn [_ events]
                (hs.settings.set last-key events)
                ;; Let the minibuffer's key-up go by first.
                (hs.timer.doAfter 0.1 #(play events)))))

(fn delete-named []
  "Forget a named macro."
  (read-named "Delete macro: "
              (fn [name]
                (let [saved (named)]
                  (tset saved name nil)
                  (hs.settings.set named-key saved)
                  (minibuffer.message (.. "Deleted macro " name))))))

{: start
 : stop
 : toggle
 : recording?
 : call-last
 : name-last
 : run-named
 : delete-named
 : trim-leader
 : play}
