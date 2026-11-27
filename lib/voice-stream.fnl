"
Live dictation: whisper isn't a streaming model, so the recording's growing
tail (the buffer) is transcribed again and again by the small listener model,
and the words two passes in a row agree on are committed (LocalAgreement, as in
whisper_streaming). The buffer then starts after the committed words, keeping
each pass short. Pure: the caller does the audio and the HTTP.
"

(local min-window 4) ; trim at a committed sentence end past this many seconds
(local max-window 8) ; trim at any committed word past this many seconds
(local quiet-window 3)
(local min-buffer 2) ; whisper's VAD drops speech in shorter buffers ; with nothing heard, keep only the last second or so

(fn new []
  "A fresh stream: nothing committed, the buffer starting at 0 s."
  {:committed [] :committed-end 0 :buffer-start 0 :previous [] :tentative []})

(fn norm [s]
  (: (: s :gsub "^%s+" "") :lower))

(fn punctuation? [s]
  (not= nil (: (norm s) :match "^[%p、。？！，]+$")))

(fn committed-text [st]
  (table.concat (icollect [_ w (ipairs st.committed)] w.text)))

(fn looping [hyp]
  "HYP up to where whisper starts going round in circles: a run of 4+ tokens
at the same instant (made up, not heard), or a run of tokens said again right
away (1-2 tokens three more times, or 3+ tokens twice more); the first two
go through (\"again and again\")."
  (var cut nil)
  (for [i 1 (length hyp) &until cut]
    (var same-time 1)
    (while (and (. hyp (+ i same-time))
                (= (. hyp (+ i same-time) :start) (. hyp i :start)))
      (set same-time (+ same-time 1)))
    (when (>= same-time 4) (set cut i))
    (for [k 1 8 &until cut]
      (let [times (if (< k 3) 4 3)]
        (when (<= (+ i (* k times) -1) (length hyp))
          (var same true)
          (for [j 0 (- (* k (- times 1)) 1)]
            (when (not= (norm (. hyp (+ i j) :text)) (norm (. hyp (+ i j k) :text)))
              (set same false)))
          (when (and same (not (punctuation? (. hyp i :text))))
            (set cut (+ i k k)))))))
  (if cut (fcollect [i 1 (- cut 1)] (. hyp i)) hyp))

(fn after-anchor [st tokens]
  "TOKENS after the committed text's end, where a pass words what's committed
differently: after the last word of it said near that time, else after that
time (whisper's times are rough, so the anchor comes first)."
  (let [last (. st.committed (length st.committed))
        word (and last (norm last.text))]
    (var at nil)
    (when (and word (not (punctuation? word)))
      (for [i (length tokens) 1 -1 &until at]
        (let [tok (. tokens i)]
          (when (and (= (norm tok.text) word)
                     (< (math.abs (- tok.end st.committed-end)) 1.5))
            (set at i)))))
    (if at
        (icollect [i tok (ipairs tokens)] (when (> i at) tok))
        (icollect [_ tok (ipairs tokens)]
          (when (> tok.start (- st.committed-end 0.1)) tok)))))

(fn hypothesis [st words]
  "The tokens in WORDS (server times, relative to the buffer) past what is
committed already, with absolute times. A pass starts by saying again what's
committed inside the buffer, so that's skipped by its text; where whisper words
it differently, by its time (rough, so tokens repeating the committed tail
go too)."
  (let [tokens (icollect [_ w (ipairs words)]
                 (when (not= (norm w.word) "")
                   {:text w.word :start (+ st.buffer-start w.start)
                    :end (+ st.buffer-start w.end)}))
        inside (icollect [_ w (ipairs st.committed)]
                 (when (> w.end (+ st.buffer-start 0.05)) w))]
    (var same 0)
    (while (and (< same (length inside)) (. tokens (+ same 1))
                (= (norm (. tokens (+ same 1) :text)) (norm (. inside (+ same 1) :text))))
      (set same (+ same 1)))
    (let [rest (if (= same (length inside))
                   (icollect [i tok (ipairs tokens)] (when (> i same) tok))
                   (after-anchor st tokens))
          c st.committed
          n (length c)]
      ;; the longest run (up to 5) of leading tokens that repeats the committed tail
      (var drop 0)
      (when (< same (length inside))
        (for [k (math.min 5 n (length rest)) 1 -1 &until (> drop 0)]
          (var ok true)
          (for [j 1 k]
            (when (not= (norm (. rest j :text)) (norm (. c (+ (- n k) j) :text)))
              (set ok false)))
          (when ok (set drop k))))
      (looping (icollect [i tok (ipairs rest)] (when (> i drop) tok))))))

(fn segment-words [segment]
  "SEGMENT's tokens (verbose_json) as {: word : start : end}, whole characters:
a token can hold part of a Korean or Japanese character, which the server sends
as U+FFFD, so tokens with one are merged with their neighbours and given the
segment text's bytes instead."
  (let [text (or segment.text "")
        out []
        bad "\239\191\189"]
    (var p 1)
    (var run nil)
    (fn close-run [upto]
      (when run
        (table.insert out {:word (text:sub p (- upto 1)) :start run.start :end run.end})
        (set p upto)
        (set run nil)))
    (each [_ w (ipairs (or segment.words []))]
      (let [word (or w.word "")]
        (if (word:find bad 1 true)
            (if run (set run.end w.end) (set run {:start w.start :end w.end}))
            (let [at (text:find word p true)]
              (if (and run at) (close-run at))
              (if (= at p)
                  (do (table.insert out {: word :start w.start :end w.end})
                      (set p (+ p (length word))))
                  ;; out of step with the text: keep it as sent
                  (table.insert out {: word :start w.start :end w.end}))))))
    (close-run (+ (length text) 1))
    out))

(fn sentence-end [st]
  "The end time of the last committed token that ends a sentence, or nil."
  (var found nil)
  (for [i (length st.committed) 1 -1 &until found]
    (let [w (. st.committed i)]
      (when (: w.text :match "[.?!。？！]%s*$")
        (set found w.end))))
  found)

(fn update [st words now silent?]
  "Take a pass's WORDS ({: word : start : end}, seconds into the buffer) over the
buffer as recorded up to NOW; SILENT? if the recording has been quiet for the
last second (only then is unheard audio let go). Commits what this pass and the last agree on,
holding back punctuation at the very end (whisper ends every pass with a
period). Returns the newly committed tokens."
  (let [hyp (hypothesis st words)
        prev st.previous
        new []]
    (var agree true)
    (each [i tok (ipairs hyp) &until (not agree)]
      (let [p (. prev i)]
        (if (and p (= (norm p.text) (norm tok.text))
                 (not (and (= i (length hyp)) (punctuation? tok.text))))
            (table.insert new tok)
            (set agree false))))
    (each [_ tok (ipairs new)]
      (table.insert st.committed tok)
      (set st.committed-end (math.max st.committed-end tok.end)))
    ;; a pass that heard nothing (VAD on a short buffer) doesn't wipe out the last
    (when (or (> (length hyp) 0) silent?)
      (set st.previous (icollect [i tok (ipairs hyp)] (when (> i (length new)) tok))))
    (set st.tentative st.previous)
    ;; Keep the buffer short
    (let [window (- now st.buffer-start)
          at-sentence (sentence-end st)]
      (if (and silent? (= 0 (length st.previous)) (> window quiet-window))
          (do (set st.buffer-start (math.max st.committed-end (- now 1)))
              (set st.previous []))
          (and at-sentence (> window min-window) (> at-sentence st.buffer-start))
          (set st.buffer-start (math.min at-sentence (- now min-buffer)))
          (and (> window max-window) (> st.committed-end st.buffer-start))
          (set st.buffer-start (math.min st.committed-end (- now min-buffer))))
      (set st.buffer-start (math.max 0 st.buffer-start)))
    new))

(fn finish [st words]
  "At the end of the recording: commit all of the last pass's WORDS (over the
whole buffer), or the previous pass's if this one heard nothing."
  (let [hyp (hypothesis st words)]
    (each [_ tok (ipairs (if (> (length hyp) 0) hyp st.previous))]
      (table.insert st.committed tok)
      (set st.committed-end (math.max st.committed-end tok.end))))
  (set st.previous [])
  (set st.tentative []))

(fn idle [st now]
  "Nothing was heard since the last pass, and nothing is pending: the buffer
can start later, up to NOW less the shortest buffer worth transcribing."
  (when (= 0 (length st.previous))
    (set st.buffer-start (math.max st.buffer-start st.committed-end (- now min-buffer)))))

(fn restart [st at]
  "Drop what's pending and go on from AT (seconds): after a pause, whatever was
said up to it is left out."
  (set st.previous [])
  (set st.tentative [])
  (set st.buffer-start (math.max st.buffer-start at)))

(fn tentative-text [st]
  (table.concat (icollect [_ w (ipairs st.tentative)] w.text)))

(fn prompt [st]
  "The tail of the committed text before the buffer, to keep whisper in
context across trims. (What's committed inside the buffer stays out: whisper
would take it as already said and skip it.)"
  (let [t (table.concat (icollect [_ w (ipairs st.committed)]
                          (when (<= w.end (+ st.buffer-start 0.05)) w.text)))]
    (if (> (length t) 200)
        (: (t:sub -200) :gsub "^[\128-\191]+" "")
        t)))

(fn edit [old new]
  "How to turn the text OLD into NEW at its end: {: delete : insert}, DELETE
characters (code points) off OLD's end, then INSERT."
  (let [a (icollect [_ c (utf8.codes old)] c)
        b (icollect [_ c (utf8.codes new)] c)]
    (var i 1)
    (while (and (. a i) (= (. a i) (. b i))) (set i (+ i 1)))
    {:delete (- (length a) (- i 1))
     :insert (table.concat (fcollect [k i (length b)] (utf8.char (. b k))))}))

{: new : segment-words : update : finish : idle : restart
 : committed-text : tentative-text : prompt : edit}
