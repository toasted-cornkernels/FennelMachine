;; Now Playing HUD for mpv: album art, title, artist, album and progress.
;; mpv.fnl passes the instance to show and its IPC `request` function.

(local {: palette : font-name} (require :lib.which-key))

(local width 520)
(local height 168)
(local margin 20)
(local pad 16)
(local art-size (- height (* 2 pad)))
(local display-seconds 8)
(local properties [:path :working-directory :metadata :media-title :pause
                   :idle-active :time-pos :duration :seekable])

(local hud {})

(fn find-executable [name]
  (hs.fnutils.find [(.. "/etc/profiles/per-user/" (os.getenv "USER") "/bin/" name)
                    (.. "/run/current-system/sw/bin/" name)
                    (.. "/opt/homebrew/bin/" name)
                    (.. "/usr/local/bin/" name)]
                   #(hs.fs.attributes $ :mode)))

(local ffmpeg (find-executable :ffmpeg))

(local cache-dir (.. (hs.fs.temporaryDirectory) "hammerspoon-now-playing/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Album art
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn local-file [path cwd]
  "PATH as an absolute file name, or nil if it is a URL."
  (when (and path (not (path:match "^%a[%w+.-]*://")))
    (if (= (path:sub 1 1) "/") path (.. (or cwd "") "/" path))))

;; Everything that reads the music files runs in this script, outside
;; Hammerspoon's main thread: a slow (or asleep) drive or a privacy prompt
;; must not freeze Hammerspoon. It writes the art to $3 as a PNG.
(local art-script "
ffmpeg=$1 file=$2 out=$3
if [ -n \"$ffmpeg\" ] && \"$ffmpeg\" -v error -nostdin -y -i \"$file\" -map 0:v:0 \\
     -frames:v 1 -vf scale=256:256:force_original_aspect_ratio=decrease \"$out\" \\
     2>/dev/null && [ -s \"$out\" ]; then exit 0; fi
dir=$(dirname \"$file\")
images=$(ls \"$dir\" | grep -iE '\\.(jpe?g|png)$')
image=$(printf '%s\\n' \"$images\" | grep -iE 'cover|folder|front' | head -n 1)
# Otherwise only the sole image of an album folder, not any image that happens
# to sit next to a loose file in, say, Downloads.
if [ -z \"$image\" ] && [ \"$(printf '%s\\n' \"$images\" | grep -c .)\" = 1 ] &&
   [ \"$(ls \"$dir\" | grep -ciE '\\.(flac|mp3|m4a|aac|ogg|opus|wav|aiff?|ape|wma|wv)$')\" -ge 2 ]; then
  image=$images
fi
[ -n \"$image\" ] && /usr/bin/sips -s format png -Z 256 \"$dir/$image\" --out \"$out\" >/dev/null 2>&1
")

(local art-timeout 10)

(fn find-art [file callback]
  "Call CALLBACK with a PNG of FILE's album art, or nil: its embedded cover
  (extracted with ffmpeg), else an image in its directory. Results are cached
  in the temporary directory."
  (let [cached (.. cache-dir (hs.hash.MD5 file) ".png")]
    (if (hs.fs.attributes cached :mode)
        (callback cached)
        (do
          (hs.fs.mkdir cache-dir)
          (when (and hud.task (: hud.task :isRunning)) (: hud.task :terminate))
          (var task nil)
          (set task (hs.task.new "/bin/sh"
                                 (fn [code]
                                   (when (= task hud.task) (set hud.task nil))
                                   (callback (when (and (= code 0)
                                                        (hs.fs.attributes cached :mode))
                                               cached)))
                                 ["-c" art-script "now-playing" (or ffmpeg "")
                                  file cached]))
          (set hud.task task)
          (: task :start)
          (hs.timer.doAfter art-timeout
                            #(when (: task :isRunning) (: task :terminate)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn tag [metadata name]
  "The metadata tag NAME, matched case-insensitively (FLAC tags are upper case)."
  (var value nil)
  (each [k v (pairs (or metadata {})) &until value]
    (when (= (k:lower) name) (set value v)))
  value)

(fn clock [seconds]
  (let [s (math.floor (or seconds 0))
        h (math.floor (/ s 3600))
        m (math.floor (/ (% s 3600) 60))]
    (if (> h 0)
        (string.format "%d:%02d:%02d" h m (% s 60))
        (string.format "%d:%02d" m (% s 60)))))

(fn text [str size color alignment]
  (hs.styledtext.new (or str "")
                     {:font {:name font-name : size}
                      :color {:hex color}
                      :paragraphStyle {:lineBreak :truncateTail
                                       :alignment (or alignment :left)}}))

(fn render []
  (let [s hud.state
        x (+ pad art-size pad)
        w (- width x pad)
        title (or (tag s.metadata :title) s.media-title "Nothing playing")
        ;; Live streams report their buffered stretch as the duration.
        duration (when (not= s.seekable false) s.duration)
        state (if (not s.responding) "mpv isn't responding"
                  s.idle-active "Idle"
                  (.. (if s.pause "⏸ " "▶ ") (clock s.time-pos)
                      (if duration (.. " / " (clock duration)) "")))
        progress (if (and duration (> duration 0) s.time-pos)
                     (math.min 1 (/ s.time-pos duration))
                     0)
        art (when hud.art-file (hs.image.imageFromPath hud.art-file))
        art-frame {:x pad :y pad :w art-size :h art-size}
        elements [{:type :rectangle :action :fill
                   :fillColor {:hex palette.bg :alpha 0.97}
                   :strokeColor {:hex palette.border} :strokeWidth 1
                   :roundedRectRadii {:xRadius 10 :yRadius 10}}
                  {:type :rectangle :action :fill :frame art-frame
                   :fillColor {:hex palette.border}
                   :roundedRectRadii {:xRadius 6 :yRadius 6}}
                  (if art
                      {:type :image :image art :frame art-frame
                       :imageScaling :scaleProportionally}
                      {:type :text :text (text "♪" 64 palette.dim :center)
                       :frame {:x pad :y (+ pad (/ art-size 2) -44)
                               :w art-size :h 90}})
                  {:type :text :text (text title 20 palette.fg)
                   :frame {: x :y (+ pad 2) : w :h 28}}
                  {:type :text :text (text (or (tag s.metadata :artist)
                                               (tag s.metadata :album_artist))
                                           16 palette.prefix)
                   :frame {: x :y (+ pad 36) : w :h 24}}
                  {:type :text :text (text (tag s.metadata :album) 16 palette.dim)
                   :frame {: x :y (+ pad 62) : w :h 24}}
                  {:type :text :text (text state 14 palette.dim)
                   :frame {: x :y (- height pad 34) : w :h 22}}
                  {:type :rectangle :action :fill
                   :fillColor {:hex palette.border}
                   :roundedRectRadii {:xRadius 2 :yRadius 2}
                   :frame {: x :y (- height pad 6) : w :h 4}}
                  {:type :rectangle :action :fill
                   :fillColor {:hex palette.key}
                   :roundedRectRadii {:xRadius 2 :yRadius 2}
                   :frame {: x :y (- height pad 6) :w (* w progress) :h 4}}]]
    (: hud.canvas :replaceElements elements)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Showing and hiding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn hide []
  "Hide the Now Playing HUD."
  (when hud.refresh-timer (: hud.refresh-timer :stop))
  (when hud.hide-timer (: hud.hide-timer :stop))
  (when hud.canvas (: hud.canvas :delete))
  (set hud.canvas nil)
  (set hud.refresh-timer nil)
  (set hud.hide-timer nil))

(fn visible? [] (not= nil hud.canvas))

(fn refresh []
  (hud.request
   hud.instance.socket
   (icollect [_ p (ipairs properties)] ["get_property" p])
   (fn [responses]
     (when hud.canvas
       (let [state {:responding (not= nil (. responses 1))}]
         (each [i p (ipairs properties)]
           (let [r (. responses i)]
             (tset state p (and r (= r.error :success) r.data))))
         (set hud.state state)
         (let [file (local-file state.path state.working-directory)]
           (when (not= file hud.file)
             (set hud.file file)
             (set hud.art-file nil)
             (when file
               (find-art file (fn [art]
                                (when (and hud.canvas (= file hud.file))
                                  (set hud.art-file art)
                                  (render)))))))
         (render))))))

(fn show [instance request]
  "Show the Now Playing HUD for the mpv INSTANCE, using REQUEST (mpv.fnl's
  IPC function) to read its properties. It updates every second and hides
  after a few seconds. Clicks go through to the windows below."
  (when (and hud.instance (not= hud.instance.socket instance.socket))
    (set hud.file nil)
    (set hud.art-file nil))
  (set hud.instance instance)
  (set hud.request request)
  (when (not hud.canvas)
    (let [frame (: (hs.screen.mainScreen) :frame)]
      (set hud.state {:responding true})
      (set hud.canvas
           (doto (hs.canvas.new {:x (- (+ frame.x frame.w) width margin)
                                 :y (+ frame.y margin) :w width :h height})
             (: :level hs.canvas.windowLevels.overlay)
             (: :behavior [:canJoinAllSpaces :transient])))
      (render)
      (: hud.canvas :show)
      (set hud.refresh-timer (hs.timer.doEvery 1 refresh))))
  (when hud.hide-timer (: hud.hide-timer :stop))
  (set hud.hide-timer (hs.timer.doAfter display-seconds hide))
  (refresh))

{: show : hide : visible? : find-art}
