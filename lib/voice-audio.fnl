"
WAV handling for the voice recorder (16 kHz mono 16-bit PCM, as ffmpeg writes
it): the last seconds of a recording still being written, for listening for
spoken controls, and the recorded parts of one that was paused. Plain Lua io.
"

(local bytes-per-second 32000)

(fn data-offset [f]
  "Where the samples start in the open WAV F (ffmpeg puts a LIST chunk before
them), or nil."
  (f:seek :set 0)
  (let [head (or (f:read 4096) "")]
    (when (and (= (head:sub 1 4) "RIFF") (= (head:sub 9 12) "WAVE"))
      (var pos 13)
      (var found nil)
      (while (and (not found) (<= (+ pos 7) (length head)))
        (let [id (head:sub pos (+ pos 3))
              size (string.unpack "<I4" head (+ pos 4))]
          (if (= id "data")
              (set found (+ pos 7))
              (set pos (+ pos 8 size (% size 2))))))
      found)))

(fn header [n]
  "A WAV header for N bytes of 16 kHz mono 16-bit PCM."
  (string.pack "<c4I4c4c4I4I2I2I4I4I2I2c4I4"
               "RIFF" (+ 36 n) "WAVE" "fmt " 16 1 1 16000 bytes-per-second 2 16
               "data" n))

(fn data-size [path]
  "How many bytes of samples the WAV at PATH has so far, or nil."
  (let [f (io.open path :rb)]
    (when f
      (let [offset (data-offset f)
            size (f:seek :end)]
        (f:close)
        (when offset (- size offset (% (- size offset) 2)))))))

(fn write [out parts]
  (let [o (io.open out :wb)]
    (when o
      (var n 0)
      (each [_ p (ipairs parts)] (set n (+ n (length p))))
      (o:write (header n))
      (each [_ p (ipairs parts)] (o:write p))
      (o:close)
      out)))

(fn tail [path seconds out]
  "The last SECONDS of the WAV at PATH, which may still be being written, as a
WAV at OUT; OUT, or nil."
  (let [f (io.open path :rb)]
    (when f
      (let [offset (data-offset f)
            size (f:seek :end)]
        (if (not offset)
            (do (f:close) nil)
            (let [have (- size offset (% (- size offset) 2))
                  n (math.min have (* 2 (math.floor (/ (* seconds bytes-per-second) 2))))]
              (f:seek :set (+ offset (- have n)))
              (let [samples (or (f:read n) "")]
                (f:close)
                (write out [samples]))))))))

(fn splice [path ranges out]
  "The parts of the WAV at PATH given by RANGES, {from to} byte offsets into
its samples (to nil: the end), joined as a WAV at OUT; OUT, or nil."
  (let [f (io.open path :rb)]
    (when f
      (let [offset (data-offset f)
            size (f:seek :end)]
        (if (not offset)
            (do (f:close) nil)
            (let [have (- size offset)
                  parts (icollect [_ [from to] (ipairs ranges)]
                          (let [a (math.max 0 (math.min from have))
                                b (math.min have (or to have))]
                            (when (> b a)
                              (f:seek :set (+ offset a))
                              (f:read (- b a)))))]
              (f:close)
              (write out parts)))))))

{: data-size : tail : splice : header}
