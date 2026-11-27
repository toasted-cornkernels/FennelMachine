(local is (require :lib.testing.assert))
(local vt (require :lib.voice-text))

(describe
 "Voice text"
 (fn []
   (it "replaces whole words only"
       (fn []
         (is.eq? (vt.replace "最終更新ビジで並べ替え" "ビジ" "日時") "最終更新日時で並べ替え" "ja in a sentence")
         (is.eq? (vt.replace "ビジネスの話" "ビジ" "日時") "ビジネスの話" "not inside a katakana word")
         (is.eq? (vt.replace "concatenate the cat" "cat" "dog") "concatenate the dog" "not inside an English word")
         (is.eq? (vt.replace "보장치" "장치" "기기") "보장치" "not inside a Korean word")
         (is.eq? (vt.replace "the e max editor" "e max" "Emacs") "the Emacs editor" "several words")))

   (it "primes whisper with a sentence in English"
       (fn []
         (is.eq? (vt.prompt ["Emacs" "Nix"] :en) "I use Emacs, Nix." "en")
         (is.eq? (vt.prompt ["도서관"] :ko) "도서관" "other languages")
         (is.eq? (vt.prompt nil :en) "" "no vocabulary")
         (is.eq? (vt.prompt [] :ja) "" "empty vocabulary")))

   (it "offers whole words to correct, Japanese pieces joined"
       (fn []
         (is.eq? (table.concat (vt.word-candidates "昨日は、図書館に行きました。"
                                                   ["昨日" "は" "図書" "館" "に" "行き" "まし" "た"])
                               "|")
                 "昨日|昨日は|図書|図書館|図書館に|館|館に|行き|行きました"
                 "never a particle or a cut-short tail alone")
         (is.eq? (table.concat (vt.word-candidates "알 수 없는 장치에서는" nil) "|")
                 "알|수|없는|없|장치에서는|장치"
                 "Korean words also without their particle")))

   (it "spaces dictation off the word before it"
       (fn []
         (is.eq? (vt.needs-space? "d" "Hello") true "after a word")
         (is.eq? (vt.needs-space? "." "Next") true "after a full stop")
         (is.eq? (vt.needs-space? " " "Hello") false "after a space")
         (is.eq? (vt.needs-space? "(" "Hello") false "after a bracket")
         (is.eq? (vt.needs-space? nil "Hello") false "at the start")
         (is.eq? (vt.needs-space? "d" ", then") false "before punctuation")
         (is.eq? (vt.needs-space? "。" "Next") false "after Japanese")
         (is.eq? (vt.needs-space? "d" "図書館") false "before Japanese")
         (is.eq? (vt.needs-space? "다" "그리고") true "Korean is spaced")
         (is.eq? (vt.needs-space? "“" "Hi") false "after an opening quote")))

   (it "counts UTF-16 units like macOS"
       (fn []
         (is.eq? (vt.utf16-length "a図😀") 4 "an emoji is two")))

   (it "lets Korean particles follow"
       (fn []
         (is.eq? (vt.replace "알 수 없는 장치에서는" "장치" "기기") "알 수 없는 기기에서는" "particle")))

   (it "ignores case in what was heard"
       (fn []
         (is.eq? (vt.replace "Cat sat." "cat" "dog") "dog sat." "capitalised")
         (is.eq? (vt.replace "use magit, then Magit" "magit" "Magit") "use Magit, then Magit" "both")))

   (it "leaves invalid UTF-8 alone"
       (fn []
         (is.eq? (vt.replace "ab\xffcd" "b" "x") "ab\xffcd" "invalid")))

   (it "applies the corrections for the language, longest first"
       (fn []
         (let [corrections {:en {"e max" "Emacs" "max" "MAX"} :ja {"ビジ" "日時"}}]
           (is.eq? (vt.correct "open e max now" corrections :en) "open Emacs now" "longest first")
           (is.eq? (vt.correct "最終更新ビジで" corrections :en) "最終更新ビジで" "not another language's")
           (is.eq? (vt.correct "e max ビジで" corrections :auto) "Emacs 日時で" "auto uses all")
           (is.eq? (vt.correct "anything" {:en []} :en) "anything" "empty list"))))

   (it "joins whisper's segments"
       (fn []
         (is.eq? (vt.clean " Open Magit and the Hamm\nerspoon files.\n") "Open Magit and the Hammerspoon files." "split word")
         (is.eq? (vt.clean " reload the\n Hammerspoon config\n") "reload the Hammerspoon config" "new segment")))

   (it "guesses the language by script"
       (fn []
         (is.eq? (vt.language-of "ビジ") :ja "katakana")
         (is.eq? (vt.language-of "日時") :ja "kanji")
         (is.eq? (vt.language-of "장치") :ko "hangul")
         (is.eq? (vt.language-of "emacs") :en "latin")))

   (it "carries out spoken dictation markers"
       (fn []
         (is.eq? (vt.markers "Hello world new line second line.") "Hello world\nSecond line." "new line")
         (is.eq? (vt.markers "First point. New paragraph. Second point.") "First point.\n\nSecond point." "new paragraph")
         (is.eq? (vt.markers "Dear team, comma, thanks for the help period.") "Dear team, thanks for the help." "marks replace whisper's punctuation")
         (is.eq? (vt.markers "Is it done?? Question mark") "Is it done?" "whisper's own doubled mark")
         (is.eq? (vt.markers "Really question mark exclamation mark") "Really?!" "spoken marks stay")
         (is.eq? (vt.markers "Hi Bob, how are you? Mark.") "Hi Bob, how are you?" "what whisper left of question mark")
         (is.eq? (vt.markers "New line.") "\n" "only a marker")
         (is.eq? (vt.markers "이거 봐 question mark") "이거 봐?" "English marker in Korean")
         (is.eq? (vt.markers "다 됐어. 물음표 3줄 다음 항목 쉼표 그리고 나머지 느낌표.") "다 됐어?\n다음 항목, 그리고 나머지!" "Korean, with 3줄 for 새 줄")
         (is.eq? (vt.markers "こんにちは、開業よろしくお願いします。") "こんにちは\nよろしくお願いします。" "Japanese, with 開業 for 改行")
         (is.eq? (vt.markers "本当ですか?疑問不?本当です。簡単不。") "本当ですか？本当です！" "Japanese homophones")))

   (it "leaves markers used as words alone"
       (fn []
         (each [_ s (ipairs ["Add a new line after the header." "Over a period of time."
                             "Remove the trailing comma" "Make it comma-separated"
                             "Make it comma separated" "Use semicolon here"
                             "Why? Point taken." "세 줄 요약해 줘" "새 줄을 추가해 줘"
                             "마침표 찍어 줘" "改行を入れてください" "改行コードを確認して"
                             "今日のテスト。9点だった" "よく点検して"])]
           (is.eq? (vt.markers s) s s))
         (is.eq? (vt.markers "Type the word literal comma in the box.") "Type the word comma in the box." "literal")))

   (it "stops at stop dictation"
       (fn []
         (let [(text stopped) (vt.markers "The meeting is at noon period. Stop dictation and this is ignored.")]
           (is.eq? text "The meeting is at noon." "en text")
           (is.eq? stopped true "en stopped"))
         (is.eq? (vt.markers "회의는 내일이야 마침표 받아쓰기 중지 이건 무시") "회의는 내일이야." "ko")
         (is.eq? (vt.markers "会議は明日です。9点音声入力終了。これは無視。") "会議は明日です。" "ja")
         (is.eq? (select 2 (vt.markers "No markers here.")) false "not stopped")))))
