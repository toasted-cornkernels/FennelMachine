"
Homerow / Vimium style keyboard hints for macOS.

`show-hints` labels the clickable elements on screen (the windows, panels of
menu-bar apps such as the emoji picker, the frontmost app's menu bar, status
menus and the Dock) with short home-row labels found through the Accessibility
API. Typing a label acts on the element:

  label            click (AXPress when the element supports it)
  Shift + last     right click
  Cmd + last       Cmd-click, e.g. open a link in a new tab
  Ctrl + last      double click
  Alt + last       move the pointer there without clicking
  DEL              delete the last typed character
  /                switch to `search`
  ESC / C-g        cancel

Synthetic clicks put the pointer back where it was.

`search` is a Shortcat style alternative in the minibuffer (lib/minibuffer.fnl):
type part of the text of anything clickable on screen, a window title or a menu
item of the frontmost app. Matches on screen are outlined; RET acts on the
selected one, with the same modifiers as above.
"

(local minibuffer (require :lib.minibuffer))
(local which-key (require :lib.which-key))

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

(fn bounds [rects]
  "The smallest rectangle containing every one of RECTS."
  (let [x1 (math.min (table.unpack (icollect [_ r (ipairs rects)] r.x)))
        y1 (math.min (table.unpack (icollect [_ r (ipairs rects)] r.y)))
        x2 (math.max (table.unpack (icollect [_ r (ipairs rects)] (+ r.x r.w))))
        y2 (math.max (table.unpack (icollect [_ r (ipairs rects)] (+ r.y r.h))))]
    {:x x1 :y y1 :w (- x2 x1) :h (- y2 y1)}))

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

(fn window-roots [floating]
  "Visible standard windows, front to back, each clipped to its screen and
  occluded by the windows in front of it and by the FLOATING panel frames."
  (let [roots []
        occluders (icollect [_ o (ipairs floating)] o)]
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
  (bounds (icollect [_ s (ipairs (hs.screen.allScreens))] (: s :fullFrame))))

(local unresponsive-pids {})

(fn app-attribute [app name]
  "Read attribute NAME of APP's accessibility element. Some processes never
  answer accessibility requests (e.g. WebKit content processes); they are asked
  once with a short timeout and skipped from then on."
  (when (not (. unresponsive-pids (: app :pid)))
    (let [element (hs.axuielement.applicationElement app)
          _ (pcall #(: element :setTimeout 0.1))
          started (now)
          value (attr element name)]
      (when (> (- (now) started) 5e7)
        (tset unresponsive-pids (: app :pid) true))
      value)))

;; Windows of agent apps that are drawn as overlays rather than panels.
(local skipped-panel-subroles {:AXUnknown true})

(fn panel-roots [clip]
  "Windows of agent (menu-bar-only) apps, which `hs.window.orderedWindows`
  leaves out, e.g. the Emoji & Symbols picker and Character Viewer. A popover
  can extend far past its anchor window, so each panel is clipped to the
  bounds of its window and popovers. Returns the roots and the frames they
  cover."
  (let [roots []
        frames []]
    (each [_ app (ipairs (hs.application.runningApplications))]
      ;; Hammerspoon is an agent app too: skip our own overlays.
      (when (and (= (: app :kind) 0) (not= (: app :pid) hs.processInfo.processID))
        (each [_ win (ipairs (or (app-attribute app :AXWindows) []))]
          (when (not (. skipped-panel-subroles (attr win :AXSubrole)))
            (let [covered (icollect [_ el (ipairs [win (table.unpack (or (attr win :AXChildren) []))])]
                            (when (or (= el win) (= (attr el :AXRole) :AXPopover))
                              (attr el :AXFrame)))
                  panel-clip (and (> (length covered) 0)
                                  (intersection clip (bounds covered)))]
              (when panel-clip
                (table.insert roots {:element win :clip panel-clip :occluders []})
                (each [_ frame (ipairs covered)]
                  (table.insert frames frame))))))))
    (values roots frames)))

(fn chrome-roots [clip]
  "The frontmost app's menu bar, every app's status menu items and the Dock."
  (let [roots []
        front (hs.application.frontmostApplication)]
    (when front
      (let [menubar (attr (hs.axuielement.applicationElement front) :AXMenuBar)]
        (when menubar
          (table.insert roots {:element menubar : clip :occluders []}))))
    ;; Background-only processes never own menu extras.
    (each [_ app (ipairs (hs.application.runningApplications))]
      (when (>= (: app :kind) 0)
        (let [extras (app-attribute app :AXExtrasMenuBar)]
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
        started (now)
        clip (screen-clip)
        (panels floating) (panel-roots clip)]
    (each [_ root (ipairs panels)]
      (collect-tree root.element root acc budget))
    (each [_ root (ipairs (window-roots floating))]
      (collect-tree root.element root acc budget))
    (each [_ root (ipairs (chrome-roots clip))]
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
  "Text describing ELEMENT and its first two levels of children, e.g. a Finder
  row's file name or a link's text, without repeats."
  (let [parts []
        seen {}]
    (fn add [e depth]
      (each [_ name (ipairs [:AXTitle :AXDescription :AXValue :AXHelp])]
        (let [value (attr e name)]
          (when (and (= (type value) :string) (value:find "%S")
                     (not (. seen value)))
            (tset seen value true)
            (table.insert parts (value:sub 1 200)))))
      ;; A menu bar item's menu would match everything inside it.
      (when (and (< depth 2) (not= (attr e :AXRole) :AXMenuBarItem))
        (each [_ child (ipairs (or (attr e :AXChildren) []))]
          (add child (+ depth 1)))))
    (add element 0)
    (table.concat parts " ")))

(fn visible-hints []
  "Hints matching the typed label prefix."
  (let [typed state.typed]
    (icollect [_ hint (ipairs state.hints)]
      (when (= (hint.label:sub 1 (length typed)) typed)
        hint))))

(fn render []
  (let [visible (visible-hints)]
    (each [_ {: canvas : frame} (ipairs state.canvases)]
      (let [elements []]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search (Shortcat style, in the minibuffer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn annotation [...]
  (let [parts (table.pack ...)]
    (table.concat (fcollect [i 1 parts.n]
                    (let [part (. parts i)]
                      (when (and part (not= part "")) part)))
                  "  ")))

(fn app-name-for [element names]
  "Name of the app owning ELEMENT, cached in NAMES by pid."
  (let [(ok pid) (pcall #(: element :pid))]
    (when ok
      (when (= (. names pid) nil)
        (let [app (hs.application.applicationForPID pid)]
          (tset names pid (or (and app (: app :name)) false))))
      (or (. names pid) nil))))

(fn element-candidates [names]
  (icollect [_ hint (ipairs (collect-candidates))]
    (let [text (element-text hint.element)
          role (or (attr hint.element :AXRoleDescription) hint.role)]
      {:text (if (= text "") role text)
       :annotation (annotation role (app-name-for hint.element names))
       :kind :element
       : hint})))

(fn shortcut [item]
  "A menu item's key equivalent in Emacs notation, e.g. s-S for Cmd-Shift-S."
  (let [char (attr item :AXMenuItemCmdChar)
        flags (or (attr item :AXMenuItemCmdModifiers) 0)
        mods (icollect [_ [bit mod] (ipairs [[4 :ctrl] [2 :alt] [1 :shift]])]
               (when (not= 0 (band flags bit)) mod))]
    (when (and (= (type char) :string) (char:match "^%S$"))
      (when (= 0 (band flags 8))
        (table.insert mods :cmd))
      (which-key.emacs-key mods (if (hs.fnutils.contains mods :shift)
                                    char
                                    (char:lower))))))

(fn menu-candidates [app]
  "Enabled items of APP's menus, closed ones included, as File › Save As…"
  (let [acc []
        app-name (: app :name)]
    (fn walk [menu path depth]
      (each [_ item (ipairs (or (attr menu :AXChildren) []))]
        (let [title (attr item :AXTitle)]
          (when (and (= (type title) :string) (not= title "")
                     (not= title "Services"))
            (let [label (.. path " › " title)
                  submenu (. (or (attr item :AXChildren) []) 1)]
              (if submenu
                  (when (< depth 4) (walk submenu label (+ depth 1)))
                  (attr item :AXEnabled)
                  (table.insert acc {:text label
                                     :annotation (annotation (shortcut item)
                                                             "menu item" app-name)
                                     :kind :menu
                                     :element item})))))))
    (let [bar (attr (hs.axuielement.applicationElement app) :AXMenuBar)]
      (each [_ top (ipairs (or (and bar (attr bar :AXChildren)) []))]
        (let [submenu (. (or (attr top :AXChildren) []) 1)]
          (when submenu
            (walk submenu (or (attr top :AXTitle) "") 1)))))
    acc))

(fn window-candidates []
  (icollect [_ win (ipairs (hs.window.orderedWindows))]
    (let [app (: win :application)
          app-name (or (and app (: app :name)) "")
          title (or (: win :title) "")]
      {:text (if (= title "") app-name title)
       :annotation (annotation "window" app-name)
       :kind :window
       : win})))

(fn search-candidates []
  (let [names {}
        front (hs.application.frontmostApplication)
        acc (element-candidates names)]
    (each [_ c (ipairs (window-candidates))]
      (table.insert acc c))
    (when front
      (each [_ c (ipairs (menu-candidates front))]
        (table.insert acc c)))
    acc))

(var search-canvases nil)

(fn hide-search-overlay []
  (when search-canvases
    (each [_ {: canvas} (ipairs search-canvases)]
      (: canvas :delete))
    (set search-canvases nil)))

(local max-outlines 200)

(fn outline [rect frame selected?]
  {:type :rectangle
   :frame {:x (- rect.x frame.x) :y (- rect.y frame.y) :w rect.w :h rect.h}
   :roundedRectRadii {:xRadius 3 :yRadius 3}
   :fillColor {:hex palette.bg :alpha (if selected? 0.3 0)}
   :strokeColor {:hex palette.bg :alpha (if selected? 1 0.8)}
   :strokeWidth (if selected? 4 2)})

(fn render-search [matches selected input]
  "Outline the on-screen matches once something is typed, and the selection."
  (when search-canvases
    (let [rects []]
      (when (not= input "")
        (each [i c (ipairs matches) &until (> i max-outlines)]
          (when (and (= c.kind :element) (not= c selected))
            (table.insert rects [c.hint.frame false]))))
      (when selected
        (if (= selected.kind :element)
            (table.insert rects [selected.hint.frame true])
            (= selected.kind :window)
            (table.insert rects [(: selected.win :frame) true])))
      (each [_ {: canvas : frame} (ipairs search-canvases)]
        (let [elements (icollect [_ [rect selected?] (ipairs rects)]
                         (when (intersection rect frame)
                           (outline rect frame selected?)))]
          ;; replaceElements rejects an empty list.
          (while (> (length canvas) 0) (: canvas :removeElement))
          (when (> (length elements) 0)
            (: canvas :replaceElements elements)))))))

(fn accept-search [candidate input flags]
  (hide-search-overlay)
  (if (not candidate)
      (when (not= input "") (minibuffer.message (.. "No match for " input)))
      (= candidate.kind :element) (activate-hint candidate.hint flags)
      (= candidate.kind :menu)
      (when (not (press candidate.element))
        (minibuffer.message (.. "Could not choose " candidate.text)))
      (= candidate.kind :window) (: candidate.win :focus)))

(fn search []
  "Shortcat style search in the minibuffer: type part of the text of anything
  clickable on screen, a window title or a menu item of the frontmost app; RET
  acts on the selection with the same modifiers as hint labels."
  (hide-hints)
  (hide-search-overlay)
  (set search-canvases (make-canvases))
  (minibuffer.completing-read "Click: " nil
                              {:on-accept accept-search
                               :on-cancel hide-search-overlay
                               :on-change render-search})
  ;; Let the minibuffer appear before the slower accessibility walk.
  (hs.timer.doAfter 0.01
                    #(when (minibuffer.active?)
                       (minibuffer.set-collection (search-candidates)))))

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
        ;; (with Shift) belong to the system.
        (and flags.cmd flags.shift)
        (do (hide-hints) false)
        (= key "/") (do (hide-hints) (search) true)
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
        (minibuffer.message "No clickable elements found")
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
        (minibuffer.message "Nothing to scroll")
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
 : search
 : make-labels
 :current (fn [] state)}
