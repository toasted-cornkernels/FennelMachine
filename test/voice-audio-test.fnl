(local is (require :lib.testing.assert))
(local va (require :lib.voice-audio))

(fn wav-like-ffmpeg [samples]
  "A WAV as ffmpeg writes it while recording: a LIST chunk before the data, and
sizes not filled in yet."
  (let [list "INFOISFT\014\000\000\000Lavf61.7.100\000\000"]
    (.. (string.pack "<c4I4c4c4I4I2I2I4I4I2I2" "RIFF" 0 "WAVE" "fmt " 16 1 1 16000 32000 2 16)
        (string.pack "<c4I4" "LIST" (length list)) list
        (string.pack "<c4I4" "data" 0) samples)))

(fn spit [path s]
  (with-open [f (io.open path :wb)] (f:write s)))

(fn slurp [path]
  (with-open [f (io.open path :rb)] (f:read "*a")))

(describe
 "Voice audio"
 (fn []
   (let [path (os.tmpname)
         out (os.tmpname)
         ;; 2 s: the first second all "a", the second all "b"
         samples (.. (string.rep "a" 32000) (string.rep "b" 32000))]
     (spit path (wav-like-ffmpeg samples))

     (it "measures the samples behind the LIST chunk"
         (fn []
           (is.eq? (va.data-size path) 64000 "size")
           (is.eq? (va.data-size (.. path ".missing")) nil "no file")))

     (it "takes the last seconds"
         (fn []
           (is.eq? (va.tail path 0.5 out) out "written")
           (let [s (slurp out)]
             (is.eq? (s:sub 1 44) (va.header 16000) "fresh header")
             (is.eq? (s:sub 45) (string.rep "b" 16000) "samples"))
           (va.tail path 10 out)
           (is.eq? (length (slurp out)) (+ 44 64000) "all there is")))

     (it "splices out paused stretches"
         (fn []
           (is.eq? (va.splice path [[0 16000] [48000 nil]] out) out "written")
           (let [s (slurp out)]
             (is.eq? (s:sub 1 44) (va.header 32000) "header")
             (is.eq? (s:sub 45) (.. (string.rep "a" 16000) (string.rep "b" 16000)) "samples"))
           (va.splice path [[70000 80000]] out)
           (is.eq? (length (slurp out)) 44 "past the end")
           (os.remove path)
           (os.remove out))))))
