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

(fn prompt [words lang]
  "Whisper's prompt priming it with WORDS. A plain sentence works better than
a bare list in English."
  (if (or (not words) (= (length words) 0)) ""
      (= lang :en) (.. "I use " (table.concat words ", ") ".")
      (table.concat words ", ")))

(fn utf16-length [s]
  "S's length in UTF-16 code units, as macOS's text ranges count it."
  (accumulate [n 0 _ c (utf8.codes s)] (+ n (if (> c 0xffff) 2 1))))

(fn cjk? [c]
  (or (= (char-class c) :han) (= (char-class c) :hiragana)
      (= (char-class c) :katakana)
      ;; CJK punctuation (。、「) and full-width forms
      (<= 0x3000 c 0x303f) (<= 0xff00 c 0xffef)))

(local no-space-after
       {9 true 10 true 13 true 32 true 0xa0 true 0x2028 true
        40 true 91 true 123 true 0x201c true 0x2018 true})

(fn needs-space? [before text]
  "Whether TEXT should get a space before it when inserted after the character
BEFORE (a string, or nil at the start of the text), so as not to run into a
word. Not after spaces or opening brackets, and Japanese and Chinese don't
separate words with spaces."
  (let [b (and before (. (or (codepoints before) []) 1))
        t (. (or (codepoints (or text "")) []) 1)]
    (not (not (and b t
                   (not (. no-space-after b))
                   (not (cjk? b)) (not (cjk? t))
                   (or (not= nil (char-class t)) (= t 34) (= t 39) (= t 40)))))))

(fn kana-tail? [word]
  "Whether WORD is only hiragana: a particle, an ending, or okurigana."
  (let [cs (codepoints word)]
    (and cs (> (length cs) 0)
         (accumulate [ok true _ c (ipairs cs)]
           (and ok (or (<= 0x3041 c 0x309f) (= c 0x30fc)))))))

(local korean-particles
       ["에서는" "에서도" "으로는" "에게서" "한테서" "까지" "부터" "에서" "에게"
        "한테" "으로" "처럼" "보다" "에는" "에도" "은" "는" "이" "가" "을" "를"
        "의" "에" "로" "와" "과" "도" "만"])

(fn fallback-words [text]
  "TEXT's words split at spaces and punctuation, for when there's no tokenizer."
  (icollect [w (string.gmatch text "[^%s%p]+")] w))

(fn locate [text words]
  "WORDS (in order) as {:from :to} byte ranges in TEXT; missing ones are skipped."
  (var start 1)
  (icollect [_ w (ipairs words)]
    (let [(from to) (string.find text w start true)]
      (when from
        (set start (+ to 1))
        {: from : to}))))

(fn word-candidates [text words]
  "Pieces of TEXT that might have been misheard, in order. WORDS are TEXT's
words from a tokenizer, or nil to split at spaces and punctuation.

Words, and Korean words without their particle. Japanese has no spaces and the
tokenizer splits finely, so a word also takes up to two more pieces: kanji or
katakana continuing it (図書+館), then its hiragana tail (行き+まし+た). A tail
is never offered alone or cut short."
  (let [tokens (locate text (or words (fallback-words text)))
        count (length tokens)
        word-at #(string.sub text (. tokens $ :from) (. tokens $ :to))
        glued #(and (> $ 1) (<= $ count)
                    (= (+ (. tokens (- $ 1) :to) 1) (. tokens $ :from)))
        seen {}
        candidates []
        add #(when (not (. seen $))
               (tset seen $ true)
               (table.insert candidates $))]
    (for [i 1 count]
      (when (not (and (glued i) (kana-tail? (word-at i))))
        (var j i)
        (var in-tail false)
        (while (and (<= j (math.min count (+ i 2)))
                    (or (= j i) (glued j)))
          (let [kana (kana-tail? (word-at j))
                word (string.sub text (. tokens i :from) (. tokens j :to))
                cs (codepoints word)
                particle (and cs (= (char-class (. cs (length cs))) :hangul)
                              (accumulate [found nil _ p (ipairs korean-particles)
                                           &until found]
                                (when (and (> (length word) (length p))
                                           (= (string.sub word (- (length p))) p))
                                  p)))]
            (if (and (> j i) in-tail (not kana))
                (set j (+ count 1))
                (do (set in-tail (and (> j i) kana))
                    (when (not (and in-tail (glued (+ j 1))
                                    (kana-tail? (word-at (+ j 1)))))
                      (add word)
                      (when particle
                        (add (string.sub word 1 (- (length word) (length particle))))))
                    (set j (+ j 1))))))))
    candidates))

{: char-class : replace : corrections-for : correct : clean : language-of : prompt
 : utf16-length : needs-space? : kana-tail? : word-candidates}
