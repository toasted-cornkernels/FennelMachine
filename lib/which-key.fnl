"
Emacs which-key style display for modal menus: a full-width panel along the
bottom of the screen listing `key : command` entries column by column, with an
echo line such as `SPC w-` underneath.
"
(local {: concat
        : filter
        : join
        : map} (require :lib.functional))

;; Hammerspoon reads hex colours as Generic RGB, which renders lighter than the
;; sRGB colours Emacs uses. These values were calibrated so the panel shows as
;; bg #232431, border #464858, fg #cdd5f1, key #c4a7f1, prefix #90b2f4 and
;; dim #9398ae, matching the which-key screenshots.
(local palette {:bg "#161623"
                :border "#343647"
                :fg "#c1cbf1"
                :key "#bf90f5"
                :prefix "#77a2f9"
                :dim "#8187a1"})

(local font-size 18)
(local min-rows 6)
(local column-gap "  ")
(local separator " : ")
(local pad-x 14)
(local pad-y 8)

(local font-name
       (let [preferred :D2CodingLigatureNF
             names (hs.styledtext.fontNames)]
         (if (hs.fnutils.contains names preferred) preferred :Menlo)))

(var canvas nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local named-keys {:space "SPC" :tab "TAB" :return "RET" :escape "ESC"
                   :delete "DEL" :left "<left>" :right "<right>"
                   :up "<up>" :down "<down>"})

(local mod-prefixes [[:ctrl "C-"] [:alt "M-"] [:shift "S-"] [:cmd "s-"]])

(fn has? [list x]
  (hs.fnutils.contains (or list []) x))

(local shifted-symbols {";" ":" "/" "?" "'" "\"" "," "<" "." ">" "[" "{" "]" "}"
                        "=" "+" "-" "_" "`" "~" "\\" "|" :1 "!" :2 "@" :3 "#"
                        :4 "$" :5 "%" :6 "^" :7 "&" :8 "*" :9 "(" :0 ")"})

(fn emacs-key
  [mods key]
  "Render a binding the way Emacs does: s-h for Cmd-h, H for Shift-h, : for
  Shift-; (US layout), SPC."
  (let [key (tostring key)
        base (or (. named-keys (string.lower key)) key)
        shifted (when (has? mods :shift)
                  (if (base:match "^%a$") (base:upper) (. shifted-symbols base)))
        base (or shifted base)]
    (.. (table.concat
         (icollect [_ [mod prefix] (ipairs mod-prefixes)]
           (when (and (has? mods mod)
                      (not (and shifted (= mod :shift))))
             prefix)))
        base)))

(fn mod-rank [mods]
  (accumulate [rank 0 i [mod] (ipairs mod-prefixes)]
    (if (and (has? mods mod) (not= mod :shift)) (+ rank i) rank)))

(fn item->entry
  [item]
  (let [prefix? (or item.items item.menu-fn)
        title (or item.title "")
        desc (if prefix? (if (= (title:sub 1 1) "+") title (.. "+" title))
                 item.command item.command
                 (not= title "") title
                 (= (type item.action) :string) (or (item.action:match ":(.+)$")
                                                    item.action)
                 "lambda")]
    {:key (emacs-key item.mods item.key)
     :base (tostring item.key)
     :rank (mod-rank item.mods)
     :prefix? (not (not prefix?))
     : desc}))

(fn bound? [item]
  (and item.key (not= item.key "")
       (or item.action item.items item.menu-fn)))

(fn entry< [a b]
  "Commands before prefixes, then plain keys before modified ones, then by key
  with lowercase letters ahead of uppercase as which-key orders them."
  (let [order (fn [e]
                (let [b e.key]
                  (if (> (utf8.len b) 1) (.. "0" b)
                      (b:match "%u") (.. "3" (b:lower))
                      (.. "2" b))))]
    (if (not= a.prefix? b.prefix?) (not a.prefix?)
        (not= a.rank b.rank) (< a.rank b.rank)
        (< (order a) (order b)))))

(fn menu-entries
  [menu]
  (let [entries (map item->entry (filter bound? (or menu.items [])))]
    (table.sort entries entry<)
    entries))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn width [s] (or (utf8.len s) (length s)))

(fn split-columns
  [entries rows]
  (let [columns []]
    (each [i entry (ipairs entries)]
      (let [c (+ 1 (math.floor (/ (- i 1) rows)))]
        (when (not (. columns c)) (tset columns c []))
        (table.insert (. columns c) entry)))
    (map (fn [column]
           {:entries column
            :key-w (accumulate [w 0 _ e (ipairs column)] (math.max w (width e.key)))
            :desc-w (accumulate [w 0 _ e (ipairs column)] (math.max w (width e.desc)))})
         columns)))

(fn total-width
  [columns]
  (accumulate [w 0 i c (ipairs columns)]
    (+ w c.key-w (width separator) c.desc-w (if (> i 1) (width column-gap) 0))))

(fn layout
  [entries max-chars]
  "Fill columns top to bottom, adding rows until everything fits the width."
  (let [n (length entries)]
    (var rows (math.max 1 (math.min n min-rows)))
    (var columns (split-columns entries rows))
    (while (and (> (total-width columns) max-chars) (< rows n))
      (set rows (+ rows 1))
      (set columns (split-columns entries rows)))
    {: rows : columns}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn styled [s color]
  (hs.styledtext.new s {:font {:name font-name :size font-size}
                        :color {:hex color}}))

(fn pad-left [s w] (.. (string.rep " " (- w (width s))) s))
(fn pad-right [s w] (.. s (string.rep " " (- w (width s)))))

(fn char-width []
  (/ (. (hs.drawing.getTextDrawingSize (string.rep "M" 100)
                                       {:font font-name :size font-size})
        :w)
     100))

(fn cell
  [entry column last?]
  "Styled text for one entry; trailing padding is omitted in the last column."
  (if entry
      (.. (styled (pad-left entry.key column.key-w) palette.key)
          (styled separator palette.dim)
          (styled (if last? entry.desc (pad-right entry.desc column.desc-w))
                  (if entry.prefix? palette.prefix palette.fg)))
      (styled (if last? ""
                  (string.rep " " (+ column.key-w (width separator) column.desc-w)))
              palette.fg)))

(fn render
  [{: rows : columns} echo]
  (var text (styled "" palette.fg))
  (for [r 1 rows]
    (each [i column (ipairs columns)]
      (when (> i 1)
        (set text (.. text (styled column-gap palette.fg))))
      (set text (.. text (cell (. column.entries r) column
                               (= i (length columns))))))
    (set text (.. text (styled "\n" palette.fg))))
  (.. text
      (styled "\n" palette.fg)
      ;; Zero-width non-joiners keep the font's programming ligatures out of
      ;; key paths, or e.g. `s-<-` (Cmd-Shift-,) reads as an arrow.
      (styled (echo.path:gsub utf8.charpattern "%0\u{200C}") palette.fg)
      (styled (.. " " echo.label "  [C-g back, ESC quit]") palette.dim)))

(fn echo-for
  [menu history]
  "The echo line, e.g. `SPC w-` followed by the current keymap's name."
  (let [keys (icollect [i m (ipairs (or history []))]
               (let [key (or m.leader-key m.key)]
                 (when (and (> i 1) key)
                   (emacs-key (or m.leader-mods m.mods) key))))
        header (hs.fnutils.find (or menu.items []) #(= $1.key ""))
        label (if (<= (length (or history [])) 1) (or menu.key "Global")
                  header header.title
                  (let [t (or menu.title "")]
                    (if (= (t:sub 1 1) "+") (t:sub 2) t)))]
    {:path (.. (join " " (concat (if (?. history 2 :local-leader?) [] ["SPC"])
                                 keys))
               "-")
     : label}))

(fn hide
  []
  (when canvas
    (canvas:delete)
    (set canvas nil)))

(fn show
  [menu history]
  "Show MENU (a lib.modal menu) at the bottom of the screen with the focused
  window. HISTORY is the modal menu history, used for the `SPC w-` echo line."
  (hide)
  (let [screen (or (hs.screen.mainScreen) (hs.screen.primaryScreen))
        frame (screen:frame)
        max-chars (math.floor (/ (- frame.w (* 2 pad-x)) (char-width)))
        text (render (layout (menu-entries menu) max-chars)
                     (echo-for menu history))
        size (hs.drawing.getTextDrawingSize text)
        h (+ (math.ceil size.h) (* 2 pad-y))
        c (hs.canvas.new {:x frame.x :y (- (+ frame.y frame.h) h)
                          :w frame.w :h h})]
    (doto c
      (: :level hs.canvas.windowLevels.overlay)
      (: :behavior [:canJoinAllSpaces :stationary])
      (: :appendElements
         [{:type :rectangle :action :fill :fillColor {:hex palette.bg}}
          {:type :rectangle :action :fill :fillColor {:hex palette.border}
           :frame {:x 0 :y 0 :w frame.w :h 1}}
          {:type :text :text text
           :frame {:x pad-x :y pad-y :w (- frame.w (* 2 pad-x)) :h (+ size.h 4)}}])
      (: :show))
    (set canvas c)))

{: show
 : hide
 :current (fn [] canvas)
 : menu-entries
 : emacs-key
 : palette
 : font-name
 : font-size
 : char-width}
