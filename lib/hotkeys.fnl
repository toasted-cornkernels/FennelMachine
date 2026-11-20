"
A registry of hotkeys for the describe commands. `install` wraps hs.hotkey.new
(which hs.hotkey.bind and hs.hotkey.modal:bind both go through) to remember
each hotkey's chord, callback and the code that bound it, and the helpers here
render chords in Emacs notation and find where a function was defined.
"
(local which-key (require :lib.which-key))

(local records (setmetatable {} {:__mode :k}))

(local compiler (let [(ok c) (pcall require :fennel.compiler)] (when ok c)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local mod-names [[:ctrl ["ctrl" "control" "⌃"]]
                  [:alt ["alt" "option" "⌥"]]
                  [:shift ["shift" "⇧"]]
                  [:cmd ["cmd" "command" "⌘"]]])

(local plain-keys {:space true :tab true :return true :escape true :delete true
                   :left true :right true :up true :down true})

(fn normalize-mods [mods]
  "MODS in any form hs.hotkey accepts, as an ordered subset of
  [:ctrl :alt :shift :cmd]."
  (let [s (string.lower (if (= (type mods) :table)
                            (table.concat mods " ")
                            (tostring (or mods ""))))]
    (icollect [_ [mod names] (ipairs mod-names)]
      (when (hs.fnutils.some names #(s:find $1 1 true)) mod))))

(fn key-name [key]
  (if (= (type key) :number)
      (or (. hs.keycodes.map key) (tostring key))
      (let [k (tostring key)]
        (if (. hs.keycodes.map (k:lower)) (k:lower) k))))

(fn chord [mods key]
  "MODS + KEY in Emacs notation: s-SPC, C-<f2>, M-S-[, s-J."
  (let [key (key-name key)
        key (if (and (> (or (utf8.len key) (length key)) 1)
                     (not (. plain-keys key)))
                (.. "<" key ">")
                key)]
    (which-key.emacs-key (normalize-mods mods) key)))

(fn event-chord [event]
  "The chord of a keyDown EVENT in Emacs notation."
  (let [flags (: event :getFlags)]
    (chord (icollect [_ m (ipairs [:ctrl :alt :shift :cmd])]
             (when (. flags m) m))
           (: event :getKeyCode))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source locations and docstrings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn lua-location [source line]
  "Map a Lua chunk SOURCE and LINE back to the Fennel file and line, if any."
  (let [file (and source (source:match "^@(.+)$"))]
    (when file
      (let [entry (?. compiler :sourcemap source line)]
        (if entry
            {:file (. entry 1) :line (. entry 2)}
            {: file : line})))))

(fn fn-location [f]
  "Where function F was defined, as {:file :line}; nil for C functions."
  (when (= (type f) :function)
    (let [info (debug.getinfo f "S")]
      (when (not= info.what "C")
        (lua-location info.source info.linedefined)))))

(fn dedent [s]
  (let [lines (icollect [l (string.gmatch (.. s "\n") "(.-)\n")] l)
        indent (accumulate [n nil i l (ipairs lines)]
                 (if (and (> i 1) (l:match "%S"))
                     (math.min (or n math.huge) (length (l:match "^%s*")))
                     n))
        out (icollect [i l (ipairs lines)]
              (if (and (> i 1) indent) (l:sub (+ indent 1)) l))]
    (: (table.concat out "\n") :match "^%s*(.-)%s*$")))

(fn docstring [f]
  "The docstring of Fennel function F, dedented, or nil."
  (let [fennel (require :fennel)
        doc (and (= (type f) :function)
                 (fennel.metadata:get f "fnl/docstring"))]
    (when (and doc (doc:match "%S"))
      (dedent doc))))

(fn caller-location []
  "The first stack frame outside hs.hotkey and this module."
  (var level 3)
  (var found nil)
  (while (and (not found) (< level 30))
    (let [info (debug.getinfo level "Sl")]
      (if (not info)
          (set level 30)
          (and (info.source:match "^@")
               (not (info.source:match "/hotkey%.lua$"))
               (not (info.source:match "/lib/hotkeys%.fnl$")))
          (set found (lua-location info.source info.currentline))
          (set level (+ level 1)))))
  found)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Registry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn callable? [x]
  (or (= (type x) :function)
      (and (= (type x) :table) (not= nil (?. (getmetatable x) :__call)))))

(fn install []
  "Wrap hs.hotkey.new so every hotkey created from now on is recorded."
  (when (not (rawget hs.hotkey :_recorded))
    (let [original hs.hotkey.new]
      (set hs.hotkey._recorded true)
      (set hs.hotkey.new
           (fn [mods key message pressed released repeat]
             (let [hk (original mods key message pressed released repeat)
                   ;; hs.hotkey.new shifts the callbacks left when the message
                   ;; is omitted, nil included.
                   [p r rp] (if (or (= message nil) (callable? message))
                                [message pressed released]
                                [pressed released repeat])
                   f (or p r rp)]
               (tset records hk
                     {:chord (chord mods key)
                      :mods (normalize-mods mods)
                      :key (key-name key)
                      :message (when (= (type message) :string) message)
                      :fn f
                      :location (fn-location f)
                      :caller (caller-location)})
               hk))))))

(fn active []
  "The hotkeys currently enabled (the topmost one per chord) as records."
  (icollect [_ hk (ipairs (hs.hotkey.getHotkeys))]
    (or (. records hk) {:chord hk.idx :message hk.msg})))

(fn lookup [chord-str]
  "The enabled hotkey record for CHORD-STR, e.g. \"s-SPC\", or nil."
  (hs.fnutils.find (active) #(= $1.chord chord-str)))

{: install
 : chord
 : event-chord
 : normalize-mods
 : active
 : lookup
 : fn-location
 : docstring}
