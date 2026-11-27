(local is (require :lib.testing.assert))
(local vs (require :lib.voice-stream))

(fn words [spec]
  "\"Hello@0 world@0.5\" as server words, each ending where the next starts."
  (let [out []]
    (each [text at (spec:gmatch "(%S+)@([%d.]+)")]
      (table.insert out {:word (.. " " text) :start (tonumber at) :end (+ (tonumber at) 0.4)}))
    out))

(fn text [st] (vs.committed-text st))

(describe
 "Voice stream"
 (fn []
   (it "commits what two passes agree on"
       (fn []
         (let [st (vs.new)]
           (vs.update st (words "Hello@0 there@0.5 how@1") 1.2)
           (is.eq? (text st) "" "one pass: nothing yet")
           (vs.update st (words "Hello@0 there@0.5 how@1 are@1.4 you@1.8") 2)
           (is.eq? (text st) " Hello there how" "the agreed prefix")
           (is.eq? (vs.tentative-text st) " are you" "the rest is tentative")
           (vs.update st (words "Hello@0 there@0.5 how@1 are@1.4 you@1.8 doing@2.2") 2.6)
           (is.eq? (text st) " Hello there how are you" "the committed part is skipped by its text"))))

   (it "holds back the period whisper ends a pass with"
       (fn []
         (let [st (vs.new)
               pass [{:word " Hi" :start 0 :end 0.4} {:word "." :start 0.4 :end 0.5}]]
           (vs.update st pass 1)
           (vs.update st pass 1.5)
           (is.eq? (text st) " Hi" "period held")
           (vs.finish st pass)
           (is.eq? (text st) " Hi." "until the end"))))

   (it "finds where to go on when whisper words the committed part differently"
       (fn []
         (let [st (vs.new)]
           (vs.update st (words "Eventually@0 we@0.6 trim@0.9") 1.2)
           (vs.update st (words "Eventually@0 we@0.6 trim@0.9 the@1.2") 1.6)
           (is.eq? (text st) " Eventually we trim" "committed")
           (vs.update st (words "Usually@0 we@0.5 trim@0.8 the@1.1 buffer@1.3") 2)
           (vs.update st (words "Usually@0 we@0.5 trim@0.8 the@1.1 buffer@1.3 at@1.8") 2.4)
           (is.eq? (text st) " Eventually we trim the buffer" "after its last word"))))

   (it "drops what whisper makes up in a loop"
       (fn []
         (let [st (vs.new)
               pass (words "I@0 said@0.3 it@0.6 again@0.9 and@1.2 again@1.5 and@1.8 again@2.1 and@2.4 again@2.7 and@3")]
           (vs.update st pass 3)
           (vs.update st pass 3.5)
           (is.eq? (text st) " I said it again and again and" "a repeated pair, twice")
           (let [st (vs.new)
                 pass (words "Fine@0 x@1 y@1 z@1 w@1")]
             (vs.update st pass 2)
             (vs.update st pass 2.5)
             (is.eq? (text st) " Fine" "tokens stacked at one instant")))))

   (it "keeps the buffer short and prompts with what's before it"
       (fn []
         (let [st (vs.new)
               pass (words "One@0 two@1 three@2 four@3 five@4")]
           (set (. pass 3 :word) " three.")
           (vs.update st pass 4.5)
           (vs.update st pass 5)
           (is.eq? st.buffer-start 2.4 "trimmed at the sentence end")
           (is.eq? (vs.prompt st) " One two three." "the prompt")
           (let [st (vs.new)]
             (vs.update st [] 4 true)
             (is.eq? st.buffer-start 3 "silence goes")
             (let [st (vs.new)]
               (vs.update st [] 4 false)
               (is.eq? st.buffer-start 0 "unless it's quiet"))))))

   (it "puts Korean and Japanese characters back together"
       (fn []
         (let [ws (vs.segment-words
                   {:text "오늘 회의는" :words [{:word "오" :start 0 :end 0.2}
                                                {:word "\239\191\189" :start 0.2 :end 0.3}
                                                {:word "\239\191\189" :start 0.3 :end 0.4}
                                                {:word " 회의" :start 0.4 :end 0.8}
                                                {:word "는" :start 0.8 :end 1}]})]
           (is.eq? (table.concat (icollect [_ w (ipairs ws)] w.word) "|") "오|늘| 회의|는" "tokens")
           (is.eq? (. ws 2 :end) 0.4 "merged times"))))

   (it "moves on through silence and pauses"
       (fn []
         (let [st (vs.new)]
           (vs.idle st 5)
           (is.eq? st.buffer-start 3 "idle: keep the shortest buffer")
           (set st.previous [{:text " hi" :start 3.5 :end 3.8}])
           (vs.idle st 9)
           (is.eq? st.buffer-start 3 "idle: not with words pending")
           (vs.restart st 7)
           (is.eq? st.buffer-start 7 "restart")
           (is.eq? (length st.previous) 0 "restart drops what's pending"))))

   (it "tells how to edit the text in place"
       (fn []
         (let [e (vs.edit "Hello wor" "Hello world")]
           (is.eq? e.delete 0 "append: delete")
           (is.eq? e.insert "ld" "append: insert"))
         (let [e (vs.edit "Thanks question" "Thanks?")]
           (is.eq? e.delete 9 "retract")
           (is.eq? e.insert "?" "retract: insert"))
         (let [e (vs.edit "자료는 미리" "자료는 미리 보내")]
           (is.eq? e.delete 0 "ko")
           (is.eq? e.insert " 보내" "ko insert"))))))
