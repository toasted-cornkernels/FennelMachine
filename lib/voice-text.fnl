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

;;; Dictation markers: spoken punctuation, line breaks, quotes, "scratch that"
;;; and the controls "stop/pause/resume dictation"

;; After a Korean marker these make it a word in the sentence: 마침표 찍어,
;; 세 줄 요약
(local korean-not-markers
       ["요약" "정도" "짜리" "이상" "이하" "더" "씩" "만" "추가" "넣" "빼" "삭제"
        "찍" "대신" "없" "있" "하나" "두" "세 개"])

(fn korean-dictation [verbs]
  "받아쓰기 VERB, also as whisper tends to hear 받아쓰기."
  (let [forms []]
    (each [_ d (ipairs ["받아쓰기" "받아 쓰기" "바다 쓰기" "바다쓰기"])]
      (each [_ v (ipairs verbs)]
        (table.insert forms (.. d " " v))))
    forms))

(fn japanese-dictation [verbs]
  (icollect [_ v (ipairs verbs)] (.. "音声入力" v)))

(local dictation-markers
       ;; :say forms are matched as whole words, spaces standing for any run of
       ;; spaces; Japanese ones (:glued) may sit right against other text. A word
       ;; in :unless-before right before makes it words, unless it starts or
       ;; ends a sentence.
       [{:say ["new line" "newline" "line break"] :newline 1}
        {:say ["new paragraph"] :newline 2}
        {:say ["question mark"] :mark "?"}
        {:say ["exclamation mark" "exclamation point"] :mark "!"}
        {:say ["period" "full stop"] :mark "." :unless-next ["of"]}
        {:say ["comma"] :mark "," :unless-next ["separated" "delimited" "separator"]}
        {:say ["colon"] :mark ":"}
        {:say ["semicolon" "semi-colon" "semi colon"] :mark ";"}
        {:say ["stop dictation" "end dictation" "top dictation" "stock dictation"]
         :control :stop}
        {:say ["pause dictation" "post dictation" "post-dictation" "paws dictation"]
         :control :pause}
        {:say ["resume dictation" "continue dictation"] :control :resume}
        {:say ["scratch that" "strike that"] :scratch true
         ;; "don't scratch that", "we should scratch that" ("don't" ends in "t")
         :unless-before ["don't" "dont" "do" "to" "can" "can't" "cannot" "will"
                         "won't" "would" "should" "could" "must" "might" "may"
                         "please" "t" "i" "we" "you" "they" "he" "she" "let's" "lets"
                         "not" "didn't" "never" "just" "and" "or"]
         :unless-next ["idea" "off" "itch" "out" "from" "part" "plan" "one" "line" "word"]}
        {:say ["backspace" "back space"] :backspace true
         :unless-next ["key" "keys" "button" "character"] :not-after ["press" "hit" "pressing" "hitting" "presses"]}
        {:say ["open quote" "begin quote"] :open "\""}
        {:say ["close quote" "end quote" "unquote"] :close "\""}
        {:say ["open paren" "open parenthesis" "left paren"] :open "("}
        {:say ["close paren" "close parenthesis" "right paren"] :close ")"}
        {:say ["open bracket" "left bracket"] :open "["}
        {:say ["close bracket" "right bracket"] :close "]"}
        {:say ["mark" "point"] :residue true}
        ;; Korean, with what whisper makes of 새 줄 (three lines: 세 줄, 3줄);
        ;; 세 줄 요약 really is three lines
        {:say ["새 줄" "새줄" "줄 바꿈" "줄바꿈" "세 줄" "세줄" "3줄" "새출" "새술" "세술"] :newline 1
         :unless-next korean-not-markers}
        {:say ["새 문단" "새문단" "새 단락" "새단락" "세 문단" "문단 바꿈"] :newline 2
         :unless-next korean-not-markers}
        {:say ["마침표" "온점"] :mark "." :unless-next korean-not-markers}
        {:say ["쉼표" "심표" "반점"] :mark "," :unless-next korean-not-markers}
        {:say ["물음표"] :mark "?" :unless-next korean-not-markers}
        {:say ["느낌표"] :mark "!" :unless-next korean-not-markers}
        {:say (korean-dictation ["중지" "끝" "종료"]) :control :stop}
        {:say (korean-dictation ["일시 중지" "일시정지" "일식 중지" "잠깐 멈춤"]) :control :pause}
        {:say (korean-dictation ["계속" "재개"]) :control :resume}
        {:say ["방금 거 지워" "방금 것 지워" "방금 거 취소"] :scratch true
         :unless-next ["줘" "주" "줄" "요" "달" "해"]}
        {:say ["백스페이스" "백 스페이스" "빽스페이스"] :backspace true
         :unless-next (doto (icollect [_ w (ipairs korean-not-markers)] w) (table.insert "키")) :not-after ["눌러" "누르"]}
        {:say ["따옴표 열고" "따옴표 열기" "따옴표 시작"] :open "\""}
        {:say ["따옴표 닫고" "따옴표 닫기" "따옴표 끝"] :close "\""}
        {:say ["괄호 열고" "괄호 열기"] :open "("}
        {:say ["괄호 닫고" "괄호 닫기"] :close ")"}
        ;; Japanese, with whisper's homophones (開業, 簡単符, 区点, 等点)
        {:say ["改行" "開業" "開行"] :newline 1 :glued true
         :unless-next ["コード" "文字" "医" "届" "資金" "準備"]}
        {:say ["新しい段落" "改段落"] :newline 2 :glued true}
        {:say ["句点" "区点" "苦点" "くてん"] :mark "。" :glued true}
        {:say ["9点"] :mark "。" :glued true :alone true}
        {:say ["く点"] :mark "。" :glued true :ends true}
        {:say ["読点" "糖点" "等点" "当点" "とうてん"] :mark "、" :glued true}
        {:say ["疑問符" "疑問不" "疑問婦" "はてなマーク" "クエスチョンマーク"] :mark "？" :glued true}
        {:say ["感嘆符" "簡単符" "簡単譜" "簡単不" "びっくりマーク" "エクスクラメーションマーク"] :mark "！"
         :glued true}
        {:say (japanese-dictation ["終了" "停止"]) :control :stop :glued true}
        {:say (japanese-dictation ["一時停止" "1時停止" "一時中止"]) :control :pause :glued true}
        {:say (japanese-dictation ["再開"]) :control :resume :glued true}
        {:say ["バックスペース" "バック・スペース" "バックフェース" "バック・フェース"] :backspace true :glued true :unless-next ["キー" "ボタン"]}
        {:say ["今のなし" "今のは無し" "今の取り消し" "いまのなし"] :scratch true :glued true}
        {:say ["かぎかっこ" "かぎ括弧" "鍵括弧" "かぎかっこ開き"] :open "「" :glued true}
        {:say ["かぎかっこ閉じ" "かぎ括弧閉じ" "鍵括弧閉じ" "かぎかっことじ"] :close "」" :glued true
         :particles-ok true}
        {:say ["かっこ" "括弧" "カッコ"] :open "（" :glued true
         :unless-next ["いい" "いー" "良" "よ" "悪" "わる" "つけ" "内" "付" "書"]}
        {:say ["かっこ閉じ" "括弧閉じ" "かっことじ" "カッコ閉じ"] :close "）" :glued true
         :particles-ok true}])

;; Before a marker these write it as words: "literal comma"
(local literal-forms
       (icollect [_ s (ipairs ["literal" "문자 그대로" "글자 그대로" "文字通り" "もじどおり"])]
         (codepoints s)))

(local determiners
       (collect [_ w (ipairs ["a" "an" "the" "this" "that" "these" "those" "each"
                              "every" "no" "one" "my" "your" "our" "their" "his"
                              "her" "its" "per" "any" "some" "which" "what"
                              ;; instructions about the mark rather than the mark
                              "add" "insert" "put" "use" "remove" "delete" "replace"
                              "with" "without" "type" "missing" "extra" "trailing"])]
         w true))

;; After a Japanese marker these make it a word in the sentence: 改行を入れる
(local japanese-particles
       (collect [_ c (utf8.codes "をがはのにもでとしすさせやへ")] c true))

(local auto-punctuation (collect [_ c (utf8.codes ".,?!;:、。？！") ] c true))
(local commas (collect [_ c (utf8.codes ",;、") ] c true))

(fn space? [c] (or (= c 32) (= c 9) (= c 10) (= c 0x3000)))

(fn match-form [t i f]
  "The index where form F (codepoints) ends if it is at I in T; a space in F
stands for any run of whitespace."
  (var j i)
  (var ok true)
  (each [_ c (ipairs f) &until (not ok)]
    (if (= c 32)
        (if (space? (. t j))
            (while (space? (. t j)) (set j (+ j 1)))
            (set ok false))
        (if (= (fold (or (. t j) -1)) (fold c))
            (set j (+ j 1))
            (set ok false))))
  (when ok (- j 1)))

(fn word-before [t i]
  "The word ending just before the spaces before I in T, lower case, and where
it starts; nil if punctuation comes first."
  (var j (- i 1))
  (while (space? (. t j)) (set j (- j 1)))
  (let [e j]
    (while (and (. t j) (char-class (. t j))) (set j (- j 1)))
    (when (< j e)
      (values (table.concat (fcollect [k (+ j 1) e] (utf8.char (fold (. t k)))))
              (+ j 1)))))

(fn starts-with? [t i word]
  (let [w (codepoints word)]
    (and (match-at? t i w)
         (let [class (char-class (. w (length w)))]
           (or (= class :han) (= class :katakana) (= class :hangul) (= class :hiragana)
               (not= class (char-class (. t (+ i (length w))))))))))

(fn next-word-in? [t e words glued]
  (var j (+ e 1))
  (when (not glued) (while (space? (. t j)) (set j (+ j 1))))
  (accumulate [found false _ w (ipairs (or words [])) &until found]
    (starts-with? t j w)))

(local marker-forms
       (let [forms []]
         (each [_ m (ipairs dictation-markers)]
           (each [_ s (ipairs m.say)]
             (table.insert forms {:f (codepoints s) : m})))
         (table.sort forms #(> (length $1.f) (length $2.f)))
         forms))

(fn marker-at? [t i]
  (accumulate [found false _ {: f} (ipairs marker-forms) &until found]
    (not= nil (match-form t i f))))

(fn ends? [t e]
  "Whether nothing but punctuation, the end or another marker follows E in T,
spaces aside."
  (var j (+ e 1))
  (while (space? (. t j)) (set j (+ j 1)))
  (let [after (. t j)]
    (or (= after nil) (. auto-punctuation after) (marker-at? t j))))

(fn alone? [t i e]
  "Whether I..E in T stands apart: punctuation or the start before it, and
nothing but punctuation, spaces, the end or another marker after it."
  (let [before (. t (- i 1))]
    (and (or (= before nil) (. auto-punctuation before)) (ends? t e))))

(fn apart? [t i e]
  "Whether I..E in T starts a sentence or ends one: \"Scratch that. Next\",
but not \"don't scratch that surface\"."
  (var j (- i 1))
  (while (space? (. t j)) (set j (- j 1)))
  (let [before (. t j)]
    (set j (+ e 1))
    (while (space? (. t j)) (set j (+ j 1)))
    (or (= nil before) (and (. auto-punctuation before) (not (. commas before)))
        (= nil (. t j)) (and (. auto-punctuation (. t j)) (not (. commas (. t j)))))))

(fn literal-before [t i]
  "Where a literal escape (\"literal\", 文字通り) ends right before I in T
starts, if one does."
  (var j (- i 1))
  (while (space? (. t j)) (set j (- j 1)))
  (accumulate [found nil _ f (ipairs literal-forms) &until found]
    (let [s (- j (length f) -1)]
      (when (and (>= s 1) (match-form t s f)
                 (= j (match-form t s f))
                 (or (= (char-class (. f 1)) :han) (= (char-class (. f 1)) :hiragana)
                     (not= (char-class (. f 1)) (char-class (. t (- s 1))))))
        s))))

(fn find-markers [t]
  "The dictation markers in T as {: start : end : marker : literal} in order,
the longest one winning where they overlap."
  (let [found []]
    (var i 1)
    (while (<= i (length t))
      (var hit nil)
      (each [_ {: f : m} (ipairs marker-forms) &until hit]
        (let [e (match-form t i f)
              first (char-class (. f 1))
              last (char-class (. f (length f)))]
          (when (and e
                     (if m.glued
                         (or m.particles-ok
                             (not (. japanese-particles (or (. t (+ e 1)) -1))))
                         (and (not= first (char-class (. t (- i 1))))
                              (not= last (char-class (. t (+ e 1))))
                              ;; comma-separated, semicolon's
                              (not= 45 (. t (+ e 1))) (not= 39 (. t (+ e 1)))))
                     (or (not m.alone) (alone? t i e))
                     (or (not m.ends) (ends? t e))
                     (or (not m.not-after) (not (accumulate [in false _ x (ipairs m.not-after)]
                                                  (or in (= x (word-before t i))))))
                     (or (not m.unless-before) (apart? t i e)
                         (let [w (word-before t i)]
                           (not (accumulate [in false _ x (ipairs m.unless-before)]
                                  (or in (= x w))))))
                     (not (next-word-in? t e m.unless-next m.glued)))
            (let [before (word-before t i)
                  literal (literal-before t i)]
              (if m.residue
                  ;; whisper wrote "question" as "?" and left "mark"
                  (let [p (do (var j (- i 1))
                              (while (space? (. t j)) (set j (- j 1)))
                              (. t j))
                        a (do (var j (+ e 1))
                              (while (space? (. t j)) (set j (+ j 1)))
                              (. t j))]
                    (when (and (or (= p 63) (= p 33))
                               (or (= a nil) (. auto-punctuation a) (marker-at? t (+ e 1))))
                      (set hit {:start i :end e :marker {:mark (utf8.char p)}})))
                  literal
                  (set hit {:start literal :end e :literal i})
                  (not (. determiners (or before "")))
                  (set hit {:start i :end e :marker m}))))))
      (if hit
          (do (table.insert found hit)
              (set i (+ hit.end 1)))
          (set i (+ i 1))))
    found))

(local sentence-ends (collect [_ c (utf8.codes ".?!。？！\n")] c true))

(fn markers [text]
  "TEXT with its spoken dictation markers carried out: \"new line\", \"question
mark\", \"open quote\", \"scratch that\" and the like, in English, Korean and
Japanese. A spoken mark replaces the punctuation whisper put around it;
\"literal comma\" writes the word, and so does a marker used as a noun (\"add a
new line\", 改行を入れる). \"pause/resume dictation\" just go: the recorder
acts on them. Returns the text, whether \"stop dictation\" ended it there, and
whether a \"scratch that\" came with nothing before it, to take back the
previous dictation."
  (let [t (or (codepoints (-> text (string.gsub "%?%?+" "?") (string.gsub "!!+" "!")))
              [])
        out []]
    (var keep 0) ; what was spoken stays: no trimming below this
    (var lead false) ; after a marker skip :blanks, or :all whisper's punctuation too
    (var space false)
    (var cap false)
    (var stopped false)
    (var scratch-previous false)
    (fn trim [chars]
      (while (and (> (length out) keep) (. chars (. out (length out))))
        (table.remove out)))
    (local blanks {32 true 9 true 10 false 0x3000 true})
    (fn last-out [] (. out (length out)))
    (var swallowed-end false) ; a marker ate whisper's . ? ! after it
    (fn emit [from to]
      (for [k from to]
        (let [c (. t k)]
          (if (and lead (or (. blanks c) (and (= lead :all) (. auto-punctuation c))))
              (when (and (. sentence-ends c) (not= c 10)) (set swallowed-end true))
              (do (when lead
                    (when (and space (char-class c) (> (length out) 0)
                               (not= 10 (last-out)))
                      (table.insert out 32))
                    (set lead false))
                  (table.insert out
                                (if (and cap (<= 97 c 122)) (- c 32)
                                    ;; "typo backspace. Bug" -> "typo bug"
                                    (and swallowed-end (not cap) (<= 65 c 90)
                                         (<= 97 (or (. t (+ k 1)) 0) 122)
                                         (> (length out) 0)
                                         (not (. sentence-ends (last-out)))
                                         (not= 10 (last-out)))
                                    (+ c 32)
                                    c))
                  (set cap false)
                  (set swallowed-end false))))))
    (fn take-sentence []
      "Take the last sentence off OUT, with whisper's punctuation and any line
breaks after it. Returns how many characters it had and whether it ended in a
comma."
      (while (and (> (length out) 0) (or (. blanks (last-out)) (= 10 (last-out))))
        (table.remove out))
      (let [comma? (not= nil (. commas (or (last-out) -1)))]
        (trim auto-punctuation)
        (var n 0)
        (while (and (> (length out) 0) (not (. sentence-ends (last-out))))
          (when (not (. blanks (last-out))) (set n (+ n 1)))
          (table.remove out))
        (values n comma?)))
    (fn scratch []
      "Take back the sentence before; a word or so ending in a comma (\"No,\")
takes the one before it too. With nothing before, the previous dictation goes."
      (set keep 0)
      (trim blanks)
      (if (= (length out) 0)
          (set scratch-previous true)
          (let [(n comma?) (take-sentence)]
            (when (and comma? (<= n 4) (> (length out) 0))
              (take-sentence))))
      (trim blanks)
      (set [keep lead space]
           [(length out) :all (not (cjk? (or (last-out) 32)))])
      (set cap (or (= (length out) 0) (not= nil (. sentence-ends (last-out))))))
    (fn backspace []
      "Take back the word before, or the mark: what whisper wrote next to it
goes too."
      (trim blanks)
      (trim auto-punctuation)
      (trim blanks)
      (set keep (math.min keep (length out)))
      (let [c (last-out)
            class (char-class (or c -1))]
        (if (= c nil) nil
            (= c 10) (table.remove out)
            (not class) (table.remove out)
            (do (while (and (> (length out) 0) (= class (char-class (last-out))))
                  (table.remove out))
                ;; Japanese: okurigana and particles go with their kanji (共有して)
                (when (= class :hiragana)
                  (let [stem (char-class (or (last-out) -1))]
                    (when (or (= stem :han) (= stem :katakana))
                      (while (and (> (length out) 0) (= stem (char-class (last-out))))
                        (table.remove out))))))))
      (trim blanks)
      (set keep (math.min keep (length out)))
      (set [lead space] [:all (and (> (length out) 0) (not (cjk? (last-out)))
                                   (not= 10 (last-out)))])
      (set cap (or (= (length out) 0) (not= nil (. sentence-ends (last-out))))))
    (var i 1)
    (each [_ hit (ipairs (find-markers t)) &until stopped]
      (emit i (- hit.start 1))
      (let [m hit.marker]
        (if hit.literal
            (emit hit.literal hit.end)
            (= m.control :stop)
            (do (trim blanks) (trim commas) (trim blanks) (set stopped true))
            m.control
            ;; Whisper's punctuation after the phrase stays, instead of any before
            (let [after (do (var j (+ hit.end 1))
                            (while (. blanks (. t j)) (set j (+ j 1)))
                            (. t j))]
              (trim blanks)
              (when (. auto-punctuation (or after -1)) (trim auto-punctuation))
              (set [lead space] [:blanks true]))
            m.scratch
            (scratch)
            m.backspace
            (backspace)
            m.newline
            (do (trim blanks) (trim commas) (trim blanks)
                (for [_ 1 m.newline] (table.insert out 10))
                (set [keep lead space cap] [(length out) :all false true]))
            m.open
            (let [c (. (codepoints m.open) 1)]
              (trim blanks)
              (when (and (< c 128) (> (length out) 0) (char-class (last-out)))
                (table.insert out 32))
              (when (and (< c 128) (> (length out) 0) (. auto-punctuation (last-out))
                         (not (cjk? (last-out))))
                (table.insert out 32))
              (table.insert out c)
              (set [keep lead space] [(length out) :all false]))
            m.close
            (let [c (. (codepoints m.close) 1)]
              (trim blanks) (trim commas) (trim blanks)
              (table.insert out c)
              (set [keep lead space] [(length out) :blanks (< c 128)]))
            (do (trim blanks) (trim auto-punctuation) (trim blanks)
                (each [_ c (utf8.codes m.mark)] (table.insert out c))
                (set [keep lead space] [(length out) :all (< (. (codepoints m.mark) 1) 128)])
                (set cap (not= nil (string.find ".?!" m.mark 1 true))))))
      (set i (+ hit.end 1)))
    (when (not stopped) (emit i (length t)))
    (set keep 0)
    (trim blanks)
    (values (table.concat (icollect [_ c (ipairs out)] (utf8.char c))) stopped
            scratch-previous)))

(fn pending-marker [text]
  "Where (a byte index) the end of TEXT, still being dictated, could be the
start of a marker, \"question\" of \"question mark\": live dictation holds that
back until what follows decides. nil if it can't."
  (let [t (or (codepoints (or text "")) [])
        n (length t)
        ;; whisper's punctuation at the end of a pass says nothing
        e (do (var e n)
              (while (and (> e 0) (or (space? (. t e)) (. auto-punctuation (. t e))))
                (set e (- e 1)))
              e)]
    (var found nil)
    ;; from about three words back to the last character, after any marker
    (var words 0)
    (var i e)
    (local hits (find-markers t))
    (local done (let [h (. hits (length hits))] (if h h.end 0)))
    (while (and (> i done) (< words 4))
      (when (or (space? (. t (- i 1) )) (= nil (. t (- i 1))) (cjk? (. t i))
                (= (char-class (. t i)) :hangul))
        (let [tail (fcollect [k i e] (. t k))]
          (when (accumulate [hit false _ {: f} (ipairs marker-forms) &until hit]
                  (and (> (length f) (length tail))
                       (do (var j 1) (var k 1) (var ok true)
                           (while (and ok (<= k (length tail)))
                             (if (and (= 32 (. f j)) (space? (. tail k)))
                                 (do (while (space? (. tail k)) (set k (+ k 1)))
                                     (set j (+ j 1)))
                                 (= (fold (. f j)) (fold (. tail k)))
                                 (do (set j (+ j 1)) (set k (+ k 1)))
                                 (set ok false)))
                           (and ok (<= j (length f))))))
            (set found i)))
        (when (space? (. t (- i 1))) (set words (+ words 1))))
      (set i (- i 1)))
    (when found
      (+ 1 (length (table.concat (fcollect [k 1 (- found 1)] (utf8.char (. t k)))))))))

(fn voice-control [text]
  "The dictation control TEXT ends with, said while recording: :stop, :pause
or :resume; nil if none."
  (let [t (or (codepoints (or text "")) [])
        hits (find-markers t)
        hit (. hits (length hits))]
    (when (and hit hit.marker hit.marker.control)
      (var j (+ hit.end 1))
      (while (and (. t j) (or (space? (. t j)) (. auto-punctuation (. t j))))
        (set j (+ j 1)))
      (when (= nil (. t j)) hit.marker.control))))

{: char-class : replace : corrections-for : correct : clean : language-of : prompt
 : utf16-length : needs-space? : kana-tail? : word-candidates : markers
 : voice-control : pending-marker}
