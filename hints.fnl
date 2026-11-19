"
Homerow / Vimium style keyboard hints for macOS.

`show-hints` labels the clickable elements on screen (the windows, the
frontmost app's menu bar, status menus and the Dock) with short home-row
labels found through the Accessibility API. Typing a label acts on the element:

  label            click (AXPress when the element supports it)
  Shift + last     right click
  Cmd + last       Cmd-click, e.g. open a link in a new tab
  Ctrl + last      double click
  Alt + last       move the pointer there without clicking
  DEL              delete the last typed character
  /                search: type part of an element's text to narrow the
                   hints, RET clicks the only match or relabels the matches
  ESC / C-g        cancel

Synthetic clicks put the pointer back where it was.
"

(local log (hs.logger.new "hints.fnl" "info"))
(local types hs.eventtap.event.types)

;; g is left out so that C-g can always cancel.
(local hint-chars "sadfjklewcmpuio")
(local max-visited 8000)
(local time-budget-ns 1.5e9)
(local max-windows 10)

;; Hammerspoon reads hex colours as Generic RGB; see lib/which-key.fnl.
(local palette {:bg "#bf90f5" :fg "#161623" :typed "#6f4e99" :border "#161623"})
(local font-size 16)
(local pad-x 5)
(local pad-y 2)

(local font-name
       (let [preferred :D2CodingLigatureNF]
         (if (hs.fnutils.contains (hs.styledtext.fontNames) preferred)
             preferred
             :Menlo)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Geometry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn intersection [a b]
  (let [x1 (math.max a.x b.x)
        y1 (math.max a.y b.y)
        x2 (math.min (+ a.x a.w) (+ b.x b.w))
        y2 (math.min (+ a.y a.h) (+ b.y b.h))]
    (when (and (> x2 x1) (> y2 y1))
      {:x x1 :y y1 :w (- x2 x1) :h (- y2 y1)})))

(fn inside? [rect point]
  (and (>= point.x rect.x) (< point.x (+ rect.x rect.w))
       (>= point.y rect.y) (< point.y (+ rect.y rect.h))))

(fn contains? [outer inner]
  (and (<= outer.x inner.x) (<= outer.y inner.y)
       (>= (+ outer.x outer.w) (+ inner.x inner.w))
       (>= (+ outer.y outer.h) (+ inner.y inner.h))))

(fn center [rect]
  {:x (+ rect.x (/ rect.w 2)) :y (+ rect.y (/ rect.h 2))})

(fn any? [pred list]
  (var found false)
  (each [_ x (ipairs list) &until found]
    (set found (pred x)))
  found)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessibility tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn attr [element name]
  "Read an AX attribute, returning nil if the element has gone stale."
  (let [(ok value) (pcall #(: element :attributeValue name))]
    (when ok value)))

(fn has-action? [element names]
  (let [(ok actions) (pcall #(: element :actionNames))]
    (and ok actions (any? #(hs.fnutils.contains names $) actions))))

(local clickable-roles
       {:AXButton true :AXLink true :AXCheckBox true :AXRadioButton true
        :AXPopUpButton true :AXMenuButton true :AXComboBox true
        :AXTextField true :AXTextArea true :AXDisclosureTriangle true
        :AXMenuBarItem true :AXMenuItem true :AXRow true :AXSlider true
        :AXIncrementor true :AXColorWell true :AXDockItem true})

;; Clickable elements whose children are never worth a hint of their own, plus
;; content that never contains anything clickable.
(local leaf-roles
       {:AXButton true :AXLink true :AXCheckBox true :AXRadioButton true
        :AXPopUpButton true :AXMenuButton true :AXComboBox true
        :AXTextField true :AXTextArea true :AXDisclosureTriangle true
        :AXMenuBarItem true :AXSlider true :AXIncrementor true
        :AXDockItem true :AXStaticText true :AXImage true :AXScrollBar true
        :AXValueIndicator true :AXSplitter true})

;; Scroll bar parts are AXButtons too.
(local skipped-subroles
       {:AXIncrementArrow true :AXDecrementArrow true
        :AXIncrementPage true :AXDecrementPage true})

(fn clickable? [element role subrole in-row?]
  (if (. skipped-subroles subrole) false
      ;; Table cells are reachable through their row; free-standing cells
      ;; (e.g. Chromium grids) are not.
      (= role :AXCell) (not in-row?)
      ;; Finder icon view items are images with an Open action.
      (= role :AXImage) (has-action? element [:AXOpen :AXPress])
      (. clickable-roles role) true
      false))

(fn now [] (hs.timer.absoluteTime))

(fn collect-tree [root {: clip : occluders : window} acc budget pick?]
  "Walk ROOT's accessibility tree and append a candidate to ACC for every
  element satisfying PICK? (default `clickable?`) that is visible: inside CLIP (narrowed by each enclosing
  scroll area) and not under one of the OCCLUDERS, the frames of windows in
  front. Subtrees that are scrolled out, collapsed to Chromium's 1px frames or
  fully covered by another window are skipped."
  (fn visit [element clip in-row?]
    (set budget.visited (+ budget.visited 1))
    (when (and (< budget.visited max-visited) (< (now) budget.deadline))
      (let [role (attr element :AXRole)
            frame (attr element :AXFrame)
            visible (and frame (intersection frame clip))]
        (when (or (= frame nil)
                  (and visible (> frame.w 1) (> frame.h 1)
                       (not (any? #(contains? $ visible) occluders))))
          (when (and visible
                     ((or pick? clickable?) element role
                                            (attr element :AXSubrole) in-row?)
                     (not (any? #(inside? $ (center visible)) occluders)))
            (table.insert acc {: element : role :frame visible : window}))
          ;; A menu bar item's menu is only worth walking while it is open.
          (when (or (not (. leaf-roles role))
                    (and (= role :AXMenuBarItem) (attr element :AXSelected)))
            (let [clip (if (and visible (= role :AXScrollArea)) visible clip)
                  in-row? (or in-row? (= role :AXRow))]
              (each [_ child (ipairs (or (attr element :AXChildren) []))]
                (visit child clip in-row?))))))))
  (visit root clip false))

(fn on-screen-frame [rect]
  "Clip RECT to the screen it is mostly on."
  (let [screen (or (hs.screen.find (center rect)) (hs.screen.mainScreen))]
    (intersection rect (: screen :fullFrame))))

(fn window-roots []
  "Visible standard windows, front to back, each clipped to its screen and
  occluded by the windows in front of it."
  (let [roots []
        occluders []]
    (each [_ win (ipairs (hs.window.orderedWindows))]
      (when (< (length roots) max-windows)
        (let [frame (: win :frame)
              clip (on-screen-frame frame)]
          (when (and clip (not (any? #(contains? $ clip) occluders)))
            (table.insert roots {:element (hs.axuielement.windowElement win)
                                 : clip
                                 :occluders (icollect [_ o (ipairs occluders)] o)
                                 :window win}))
          (table.insert occluders frame))))
    roots))

(fn screen-clip []
  (let [frames (icollect [_ s (ipairs (hs.screen.allScreens))] (: s :fullFrame))
        x1 (math.min (table.unpack (icollect [_ f (ipairs frames)] f.x)))
        y1 (math.min (table.unpack (icollect [_ f (ipairs frames)] f.y)))
        x2 (math.max (table.unpack (icollect [_ f (ipairs frames)] (+ f.x f.w))))
        y2 (math.max (table.unpack (icollect [_ f (ipairs frames)] (+ f.y f.h))))]
    {:x x1 :y y1 :w (- x2 x1) :h (- y2 y1)}))

(local unresponsive-pids {})

(fn chrome-roots []
  "The frontmost app's menu bar, every app's status menu items and the Dock."
  (let [clip (screen-clip)
        roots []
        front (hs.application.frontmostApplication)]
    (when front
      (let [menubar (attr (hs.axuielement.applicationElement front) :AXMenuBar)]
        (when menubar
          (table.insert roots {:element menubar : clip :occluders []}))))
    ;; Background-only processes never own menu extras. Some processes never
    ;; answer accessibility requests (e.g. WebKit content processes); they are
    ;; asked once with a short timeout and skipped from then on.
    (each [_ app (ipairs (hs.application.runningApplications))]
      (when (and (>= (: app :kind) 0) (not (. unresponsive-pids (: app :pid))))
        (let [element (hs.axuielement.applicationElement app)
              _ (pcall #(: element :setTimeout 0.1))
              started (now)
              extras (attr element :AXExtrasMenuBar)]
          (when (> (- (now) started) 5e7)
            (tset unresponsive-pids (: app :pid) true))
          (when extras
            ;; The container reports a bogus off-screen frame; its items don't.
            (each [_ item (ipairs (or (attr extras :AXChildren) []))]
              (table.insert roots {:element item : clip :occluders []}))))))
    (let [dock (hs.application.get :com.apple.dock)]
      (when dock
        (table.insert roots {:element (hs.axuielement.applicationElement dock)
                             : clip :occluders []})))
    roots))

(fn dedupe [candidates]
  "Drop candidates that would put a second label on the same spot, such as a
  row and the free-standing cell inside it."
  (let [seen {}]
    (icollect [_ c (ipairs candidates)]
      (let [p (center c.frame)
            key (.. (math.floor (/ p.x 4)) ":" (math.floor (/ p.y 4)))]
        (when (not (. seen key))
          (tset seen key true)
          c)))))

(local woken-pids {})

(fn wake-web-content []
  "Electron and Chromium only build their web accessibility tree once an
  assistive app asks for it, which AXManualAccessibility does without the
  side effects of AXEnhancedUserInterface. The tree is built asynchronously,
  so the very first invocation in such an app may miss web content."
  (let [app (hs.application.frontmostApplication)
        pid (and app (: app :pid))]
    (when (and pid (not (. woken-pids pid)))
      (tset woken-pids pid true)
      (pcall #(: (hs.axuielement.applicationElement app)
                 :setAttributeValue :AXManualAccessibility true)))))

(fn collect-candidates []
  (wake-web-content)
  (let [acc []
        budget {:visited 0 :deadline (+ (now) time-budget-ns)}
        started (now)]
    (each [_ root (ipairs (window-roots))]
      (collect-tree root.element root acc budget))
    (each [_ root (ipairs (chrome-roots))]
      (collect-tree root.element root acc budget))
    (let [result (dedupe acc)]
      (log.df "%d hints from %d elements in %.0f ms" (length result)
              budget.visited (/ (- (now) started) 1e6))
      result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Labels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn make-labels [n]
  "N prefix-free labels as short as possible, using Vimium's scheme: keep
  expanding the oldest label by every hint character until there are enough."
  (let [chars (icollect [c (hint-chars:gmatch ".")] c)
        labels [""]]
    (var offset 1)
    (while (or (< (- (length labels) offset -1) n) (= (length labels) 1))
      (let [label (. labels offset)]
        (set offset (+ offset 1))
        (each [_ c (ipairs chars)]
          (table.insert labels (.. c label)))))
    (let [result (fcollect [i offset (+ offset n -1)] (. labels i))]
      (table.sort result)
      (icollect [_ label (ipairs result)] (label:reverse)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Elements that do the right thing on AXPress without moving the pointer.
;; Rows, cells, images and text inputs get a real click instead: AXPress is not
;; supported there, and a click places the caret where the user expects.
(local press-roles
       {:AXButton true :AXLink true :AXCheckBox true :AXRadioButton true
        :AXPopUpButton true :AXMenuButton true :AXDisclosureTriangle true
        :AXMenuBarItem true :AXMenuItem true :AXDockItem true})

(fn click [point {: button : mods : count}]
  "Post a synthetic click at POINT with exactly MODS held, regardless of the
  keys physically down, then put the pointer back."
  (let [saved (hs.mouse.absolutePosition)
        right? (= button :right)
        down-type (if right? types.rightMouseDown types.leftMouseDown)
        up-type (if right? types.rightMouseUp types.leftMouseUp)
        click-state hs.eventtap.event.properties.mouseEventClickState]
    (for [n 1 (or count 1)]
      (let [down (hs.eventtap.event.newMouseEvent down-type point (or mods []))
            up (hs.eventtap.event.newMouseEvent up-type point (or mods []))]
        (: down :setProperty click-state n)
        (: up :setProperty click-state n)
        (: down :post)
        (hs.timer.usleep 15000)
        (: up :post)))
    (hs.timer.doAfter 0.05 #(hs.mouse.absolutePosition saved))))

(fn press [element]
  (let [(ok result) (pcall #(: element :performAction :AXPress))]
    (and ok result)))

(fn perform [hint mods]
  (let [point (center hint.frame)]
    (if mods.alt (hs.mouse.absolutePosition point)
        mods.shift (click point {:button :right})
        mods.cmd (click point {:mods [:cmd]})
        mods.ctrl (click point {:count 2})
        (and (. press-roles hint.role) (press hint.element)) nil
        (click point {}))))

(fn activate-hint [hint mods]
  "Bring the hint's window forward first: a click on an inactive window often
  only activates it, and a pressed button should end up in front."
  (let [win hint.window
        focused (hs.window.focusedWindow)]
    (if (and win (not mods.alt)
             (not (and focused (= (: focused :id) (: win :id)))))
        (do
          (: win :focus)
          (hs.timer.doAfter 0.15 #(perform hint mods)))
        (perform hint mods))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overlay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(var state nil)

(local char-size
       (hs.drawing.getTextDrawingSize "M" {:font font-name :size font-size}))

(fn label-rect [hint]
  "Small elements get a centred label, wide ones a label at their left edge."
  (let [w (+ (* (length hint.label) char-size.w) (* 2 pad-x))
        h (+ char-size.h (* 2 pad-y))
        f hint.frame
        x (if (< f.w (* 3 w)) (- (+ f.x (/ f.w 2)) (/ w 2)) (+ f.x 2))
        y (- (+ f.y (/ f.h 2)) (/ h 2))]
    {: x : y : w : h}))

(fn label-text [label typed]
  (let [text (hs.styledtext.new label
                                {:font {:name font-name :size font-size}
                                 :color {:hex palette.fg}
                                 :paragraphStyle {:alignment :center}})]
    (if (> (length typed) 0)
        (: text :setStyle {:color {:hex palette.typed}} 1 (length typed))
        text)))

(fn element-text [element]
  "Lower-cased text describing ELEMENT and its first two levels of children,
  e.g. a Finder row's file name or a link's text."
  (let [parts []]
    (fn add [e depth]
      (each [_ name (ipairs [:AXTitle :AXDescription :AXValue :AXHelp])]
        (let [value (attr e name)]
          (when (= (type value) :string)
            (table.insert parts (value:sub 1 200)))))
      ;; A menu bar item's menu would match everything inside it.
      (when (and (< depth 2) (not= (attr e :AXRole) :AXMenuBarItem))
        (each [_ child (ipairs (or (attr e :AXChildren) []))]
          (add child (+ depth 1)))))
    (add element 0)
    (string.lower (table.concat parts " "))))

(fn visible-hints []
  "Hints matching the typed label prefix and, while searching, the query."
  (let [typed state.typed
        query (and state.query (string.lower state.query))]
    (icollect [_ hint (ipairs state.hints)]
      (when (and (= (hint.label:sub 1 (length typed)) typed)
                 (or (not query) (hint.text:find query 1 true)))
        hint))))

(fn echo-element [canvas-frame]
  (let [text (.. "/" state.query)
        w (+ (* (math.max 20 (+ (utf8.len text) 2)) char-size.w) (* 4 pad-x))
        h (+ char-size.h (* 4 pad-y))]
    [{:type :rectangle
      :frame {:x (/ (- canvas-frame.w w) 2) :y (- canvas-frame.h h 60) : w : h}
      :roundedRectRadii {:xRadius 4 :yRadius 4}
      :fillColor {:hex "#161623"}
      :strokeColor {:hex palette.bg}
      :strokeWidth 1}
     {:type :text
      :frame {:x (+ (/ (- canvas-frame.w w) 2) (* 2 pad-x))
              :y (+ (- canvas-frame.h h 60) (* 2 pad-y))
              :w (- w (* 4 pad-x)) :h char-size.h}
      :text (hs.styledtext.new text {:font {:name font-name :size font-size}
                                     :color {:hex "#c1cbf1"}})}]))

(fn render []
  (let [visible (visible-hints)
        main-screen (: (hs.screen.mainScreen) :fullFrame)]
    (each [_ {: canvas : frame} (ipairs state.canvases)]
      (let [elements (if (and state.query (= frame.x main-screen.x)
                              (= frame.y main-screen.y))
                         (echo-element frame)
                         [])]
        (each [_ hint (ipairs visible)]
          (let [r (label-rect hint)]
            (when (inside? frame (center r))
              (let [local-frame {:x (- r.x frame.x) :y (- r.y frame.y)
                                 :w r.w :h r.h}]
                (table.insert elements
                              {:type :rectangle
                               :frame local-frame
                               :roundedRectRadii {:xRadius 3 :yRadius 3}
                               :fillColor {:hex palette.bg}
                               :strokeColor {:hex palette.border :alpha 0.6}
                               :strokeWidth 1})
                (table.insert elements
                              {:type :text
                               :frame {:x local-frame.x
                                       :y (+ local-frame.y pad-y)
                                       :w local-frame.w
                                       :h (- local-frame.h pad-y)}
                               :text (label-text hint.label state.typed)})))))
        (: canvas :replaceElements elements)))))

(fn make-canvases []
  (icollect [_ screen (ipairs (hs.screen.allScreens))]
    (let [frame (: screen :fullFrame)
          canvas (hs.canvas.new frame)]
      (: canvas :level hs.canvas.windowLevels.overlay)
      (: canvas :behavior [:canJoinAllSpaces :transient])
      (: canvas :show)
      {: canvas : frame})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn hide-hints []
  "Leave hint or scroll mode."
  (when state
    (: state.tap :stop)
    (when state.scroller
      (: state.scroller :stop))
    (each [_ {: canvas} (ipairs state.canvases)]
      (: canvas :delete))
    (set state nil)))

(fn type-char [char flags]
  (let [previous state.typed]
    (set state.typed (.. previous char))
    (let [hints (visible-hints)]
      (if (= (length hints) 0) (set state.typed previous)
          (and (= (length hints) 1) (= (. hints 1 :label) state.typed))
          (let [hint (. hints 1)]
            (hide-hints)
            (activate-hint hint flags))
          (render)))))

(fn start-search []
  (each [_ hint (ipairs state.hints)]
    (when (not hint.text)
      (set hint.text (element-text hint.element))))
  (set state.query "")
  (set state.typed "")
  (render))

(fn finish-search [flags]
  "Click the only match, or give the matches fresh, shorter labels."
  (let [matches (visible-hints)
        n (length matches)]
    (if (= n 0) nil
        (= n 1) (let [hint (. matches 1)]
                  (hide-hints)
                  (activate-hint hint flags))
        (let [labels (make-labels n)]
          (each [i hint (ipairs matches)]
            (set hint.label (. labels i)))
          (set state.hints matches)
          (set state.query nil)
          (render)))))

(fn on-search-key [event key flags]
  (if (= key :delete)
      (do
        (set state.query (if (= state.query "") nil
                             (let [n (utf8.len state.query)]
                               (state.query:sub 1 (- (utf8.offset state.query n) 1)))))
        (render))
      (or (= key :return) (= key :padenter))
      (finish-search flags)
      (not (or flags.cmd flags.ctrl))
      (let [chars (: event :getCharacters)]
        (when (and chars (not= chars "") (not (chars:match "%c")))
          (set state.query (.. state.query chars))
          (render)))))

(fn on-key [event]
  "Swallow the keys used for hints; any other key closes the hints and goes
  through, so e.g. Cmd-Tab or Cmd-Shift-J (scroll mode) still work."
  (let [key (. hs.keycodes.map (: event :getKeyCode))
        flags (: event :getFlags)
        autorepeat? (= 1 (: event :getProperty
                            hs.eventtap.event.properties.keyboardEventAutorepeat))]
    (if (or (= key :escape) (and flags.ctrl (= key :g)))
        (do (hide-hints) true)
        ;; The hotkey again closes the hints; while it is still held from
        ;; opening them, its space auto-repeats and is ignored.
        (and flags.cmd (= key :space))
        (do (when (not autorepeat?) (hide-hints)) true)
        ;; Cmd + a label character is a Cmd-click, but other Cmd shortcuts
        ;; (with Shift, or while searching) belong to the system.
        (and flags.cmd (or flags.shift state.query))
        (do (hide-hints) false)
        state.query (do (on-search-key event key flags) true)
        (= key "/") (do (start-search) true)
        (= key :delete)
        (do
          (set state.typed (state.typed:sub 1 -2))
          (render)
          true)
        (and key (= (length key) 1) (hint-chars:find key 1 true))
        (do (type-char key flags) true)
        (= key :space) true
        (do (hide-hints) false))))

(fn show-hints []
  "Label every clickable element on screen and wait for a label to be typed."
  (hide-hints)
  (let [candidates (collect-candidates)]
    (if (= (length candidates) 0)
        (hs.alert.show "No clickable elements found")
        (let [labels (make-labels (length candidates))]
          (each [i c (ipairs candidates)]
            (set c.label (. labels i)))
          (set state {:hints candidates
                      :typed ""
                      :canvases (make-canvases)
                      :tap (hs.eventtap.new [types.keyDown] on-key)})
          (render)
          (: state.tap :start)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scroll mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local scroll-step 14)
(local scroll-tick (/ 1 60))
(local fast-factor 4)
(local scroll-directions {:j [0 -1] :k [0 1] :h [1 0] :l [-1 0]})

(fn scroll-at [point dx dy]
  "Scroll events go to whatever is under their location, so the pointer
  never has to move. Flags are cleared, as Shift turns wheel scrolling
  horizontal in many apps."
  (let [event (hs.eventtap.event.newScrollEvent [dx dy] [] :pixel)]
    (: event :setFlags {})
    (: event :location point)
    (: event :post)))

(fn scroll-areas []
  "Visible scroll areas of the focused window, largest first. Falls back to
  the window itself when it exposes none, e.g. some Electron apps."
  (let [win (hs.window.focusedWindow)
        clip (and win (on-screen-frame (: win :frame)))]
    (if (not clip) []
        (let [acc []
              budget {:visited 0 :deadline (+ (now) time-budget-ns)}]
          (collect-tree (hs.axuielement.windowElement win)
                        {: clip :occluders [] :window win} acc budget
                        #(= $2 :AXScrollArea))
          (let [areas (icollect [_ a (ipairs acc)]
                        (when (and (>= a.frame.w 80) (>= a.frame.h 60)) a))]
            (table.sort areas #(> (* $1.frame.w $1.frame.h)
                                  (* $2.frame.w $2.frame.h)))
            (if (= (length areas) 0) [{:frame clip :window win}] areas))))))

(fn render-scroll []
  (each [_ {: canvas : frame} (ipairs state.canvases)]
    (let [elements []]
      (each [i area (ipairs state.areas)]
        (let [f area.frame
              local-frame {:x (- f.x frame.x) :y (- f.y frame.y) :w f.w :h f.h}]
          (when (inside? frame (center f))
            (when (= i state.index)
              (table.insert elements
                            {:type :rectangle
                             :action :stroke
                             :frame {:x (+ local-frame.x 2) :y (+ local-frame.y 2)
                                     :w (- local-frame.w 4) :h (- local-frame.h 4)}
                             :roundedRectRadii {:xRadius 6 :yRadius 6}
                             :strokeColor {:hex palette.bg}
                             :strokeWidth 3}))
            (when (> (length state.areas) 1)
              (let [w (+ char-size.w (* 2 pad-x))
                    h (+ char-size.h (* 2 pad-y))
                    x (+ local-frame.x 8)
                    y (+ local-frame.y 8)]
                (table.insert elements
                              {:type :rectangle
                               :frame {: x : y : w : h}
                               :roundedRectRadii {:xRadius 3 :yRadius 3}
                               :fillColor {:hex palette.bg
                                           :alpha (if (= i state.index) 1 0.6)}
                               :strokeColor {:hex palette.border :alpha 0.6}
                               :strokeWidth 1})
                (table.insert elements
                              {:type :text
                               :frame {: x :y (+ y pad-y) : w :h (- h pad-y)}
                               :text (label-text (tostring i) "")}))))))
      (: canvas :replaceElements elements))))

(fn stop-scrolling []
  (when state.scroller
    (: state.scroller :stop)
    (set state.scroller nil)
    (set state.scroll-key nil)))

(fn start-scrolling [key [dx dy] shift?]
  "Scroll smoothly until KEY is released. Shift, held at the start or pressed
  while scrolling, speeds it up."
  (stop-scrolling)
  (let [step (fn []
               (let [area (. state.areas state.index)
                     fast? (or shift?
                               (. (hs.eventtap.checkKeyboardModifiers) :shift))
                     speed (* scroll-step (if fast? fast-factor 1))]
                 (scroll-at (center area.frame) (* dx speed) (* dy speed))))]
    (step)
    (set state.scroll-key key)
    (set state.scroller (hs.timer.doEvery scroll-tick step))))

(fn scroll-by [dy]
  (scroll-at (center (. state.areas state.index :frame)) 0 dy))

(fn select-area [index]
  (when (. state.areas index)
    (stop-scrolling)
    (set state.index index)
    (render-scroll)))

(fn on-scroll-key-down [key flags pending-g?]
  (let [direction (. scroll-directions key)
        half-page (math.floor (/ (. state.areas state.index :frame :h) 2))]
    (if (or (= key :escape) (= key :q) (and flags.ctrl (= key :g)))
        (do (hide-hints) true)
        ;; Cmd-Shift-J again leaves; other Cmd keys leave and go through.
        flags.cmd (do (hide-hints) (= key :j))
        direction (do (start-scrolling key direction flags.shift) true)
        (= key :d) (do (scroll-by (- half-page)) true)
        (= key :u) (do (scroll-by half-page) true)
        (and (= key :g) flags.shift) (do (scroll-by -1e6) true)
        (= key :g) (do
                     (if pending-g?
                         (scroll-by 1e6)
                         (set state.pending-g true))
                     true)
        (= key :tab) (let [n (length state.areas)
                           step (if flags.shift -1 1)]
                       (select-area (+ (% (+ state.index -1 step n) n) 1))
                       true)
        (and key (or (key:match "^%d$") (key:match "^pad%d$")))
        (do (select-area (tonumber (key:match "%d"))) true)
        (do (hide-hints) false))))

(fn on-scroll-key [event]
  "h/j/k/l scroll while held, d/u by half a page, gg/G to the top/bottom,
  TAB or a number picks another area, ESC, C-g or q leaves. Other keys leave
  scroll mode and go through."
  (let [key (. hs.keycodes.map (: event :getKeyCode))
        direction (. scroll-directions key)
        autorepeat? (= 1 (: event :getProperty
                            hs.eventtap.event.properties.keyboardEventAutorepeat))]
    (if (= (: event :getType) types.keyUp)
        (do
          (when (= key state.scroll-key) (stop-scrolling))
          (not= direction nil))
        ;; The scroll timer already runs while h/j/k/l are held.
        (and direction autorepeat?)
        true
        (let [pending-g? state.pending-g]
          (set state.pending-g false)
          (on-scroll-key-down key (: event :getFlags) pending-g?)))))

(fn scroll-mode []
  "Scroll the focused window's scroll areas from the keyboard."
  (hide-hints)
  (let [areas (scroll-areas)]
    (if (= (length areas) 0)
        (hs.alert.show "Nothing to scroll")
        (do
          (set state {:areas areas
                      :index 1
                      :canvases (make-canvases)
                      :tap (hs.eventtap.new [types.keyDown types.keyUp]
                                            on-scroll-key)})
          (render-scroll)
          (: state.tap :start)))))

{: show-hints
 : hide-hints
 : scroll-mode
 : make-labels
 :current (fn [] state)}
