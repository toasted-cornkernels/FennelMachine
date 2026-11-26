"
Text handling for the voice interface, shared by Emacs (voice.el, over hs IPC)
and the global client in voice.fnl so that both correct transcripts alike.
Pure functions only.
"

(fn char-class [c]
  "What kind of character codepoint C is, for telling where words start and end."
  (if (= c nil) nil
      (<= 0x3040 c 0x309f) :hiragana
      (<= 0x30a0 c 0x30ff) :katakana
      (or (<= 0x4e00 c 0x9fff) (<= 0x3400 c 0x4dbf) (<= 0xf900 c 0xfaff)
          (<= 0x20000 c 0x2fa1f))
      :han
      (or (<= 0xac00 c 0xd7af) (<= 0x1100 c 0x11ff) (<= 0x3130 c 0x318f))
      :hangul
      (or (<= 48 c 57) (<= 65 c 90) (<= 97 c 122)
          (and (<= 0xc0 c 0x24f) (not= c 0xd7) (not= c 0xf7)))
      :word
      nil))

(fn fold [c]
  "Codepoint C in lower case (ASCII and Latin-1)."
  (if (or (<= 65 c 90) (and (<= 0xc0 c 0xde) (not= c 0xd7))) (+ c 32) c))

(fn codepoints [s]
  "S as a list of codepoints, or nil if it isn't valid UTF-8."
  (let [(ok cs) (pcall #(icollect [_ c (utf8.codes s)] c))]
    (when ok cs)))

(fn match-at? [t i h]
  (var k 1)
  (while (and (<= k (length h))
              (= (fold (or (. t (+ i k -1)) -1)) (fold (. h k))))
    (set k (+ k 1)))
  (> k (length h)))

(fn replace [text heard meant]
  "TEXT with whole-word HEARD replaced by MEANT; case doesn't matter for HEARD.

A match must not continue a word of the same kind on either side: ビジ isn't
in ビジネス, nor cat in concatenate. Korean words take particles, so after
Hangul only the start counts: 장치 is replaced in 장치에서는."
  (let [t (codepoints text)
        h (codepoints heard)]
    (if (or (not t) (not h) (= 0 (length h)))
        text
        (let [first (char-class (. h 1))
              last (char-class (. h (length h)))
              m (length h)
              out []]
          (var i 1)
          (while (<= i (length t))
            (if (and (match-at? t i h)
                     (or (not first) (not= first (char-class (. t (- i 1)))))
                     (or (not last) (= last :hangul)
                         (not= last (char-class (. t (+ i m))))))
                (do (table.insert out meant)
                    (set i (+ i m)))
                (do (table.insert out (utf8.char (. t i)))
                    (set i (+ i 1)))))
          (table.concat out)))))

(fn corrections-for [corrections language]
  "[heard meant] pairs from CORRECTIONS ({language {heard meant}}) that apply
to LANGUAGE, all of them for auto; longest first so they don't cut into each
other."
  (let [result []]
    (each [lang entries (pairs corrections)]
      (when (and (= (type entries) :table) (or (= language :auto) (= lang language)))
        (each [heard meant (pairs entries)]
          (table.insert result [heard meant]))))
    (table.sort result (fn [a b]
                        (let [la (or (utf8.len (. a 1)) 0)
                              lb (or (utf8.len (. b 1)) 0)]
                          (if (= la lb) (< (. a 1) (. b 1)) (> la lb)))))
    result))

(fn correct [text corrections language]
  "TEXT with the CORRECTIONS for LANGUAGE applied."
  (accumulate [text text _ [heard meant] (ipairs (corrections-for corrections language))]
    (replace text heard meant)))

(fn clean [text]
  "whisper-server's TEXT as one line: segments carry their own leading space
when a new word starts, so the newlines between them just go."
  (-> text
      (string.gsub "\n" "")
      (string.gsub "[ \t][ \t]+" " ")
      (string.match "^%s*(.-)%s*$")))

(fn language-of [text]
  "A guess at the language TEXT is in, by its script."
  (var found nil)
  (each [_ c (ipairs (or (codepoints text) [])) &until found]
    (let [class (and (>= c 128) (char-class c))]
      (set found (match class
                   :hangul :ko
                   (where (or :hiragana :katakana :han)) :ja))))
  (or found :en))

{: char-class : replace : corrections-for : correct : clean : language-of}
