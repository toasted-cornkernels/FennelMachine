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
         (is.eq? (vt.language-of "emacs") :en "latin")))))
