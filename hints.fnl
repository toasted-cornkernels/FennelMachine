"
Homerow / Vimium style keyboard hints for macOS.

`show-hints` labels the clickable elements on screen (the windows, panels of
menu-bar apps such as the emoji picker, the frontmost app's menu bar, status
menus and the Dock) with short home-row labels found through the Accessibility
API. Typing a label clicks the element (AXPress when the element supports it
and no modifier is held). Modifiers held on its last key are held for the
click, e.g. Cmd to open a link in a new tab. Before the last key:

  r                right click
  t                double click
  h                hover: move the pointer there without clicking
  Shift-H          press and hold, e.g. for a button's menu
                   (each of these keys again goes back to a click)
  g                drag: the label picks what to drag, a second one where
                   to; modifiers on its last key are held, e.g. Alt to copy
  y                copy text: labels go over the text on screen instead,
                   a line at a time in web pages; the label picks where it
                   starts, a second one where it ends. Web text is selected
                   too, so Shift with the arrow keys can still extend it
  v                caret mode: labels go over web text a line at a time,
                   and the label puts a cursor at its start, which h/l move
                   a character, j/k a line on screen and 0/$ to the line's
                   ends, and Vim's w/b/e/ge and W/B/E/gE by words, )/( by
                   sentences, }/{ to a paragraph's end and start, and
                   ]]/[[ and ][/[] to headings and the ends of the
                   sections they start; as in Vim, v selects from the
                   character under it
                   and V whole lines, then y copies, o goes to the other
                   end and ESC stops selecting; ESC, C-g or q leaves.
                   Chromium's selection is drawn over the page, as it
                   can't be set; y or Cmd-C copies it
  DEL              delete the last typed character, or the first pick
  x                clear the text selected in the web pages on screen (but
                   Chromium's) and in the focused text field
  Shift-P          park: stay in hint mode after each action, e.g. to
                   Cmd-click several files; Shift-P again leaves
  /                switch to `search`
  TAB              switch to scroll mode (`scroll-mode`); TAB there
                   switches back
  ?                show or hide the bindings HUD, in scroll mode too
  ESC / C-g        cancel

Status items of a menu bar hidden by a full screen app are labelled where
they show once it comes back; picking one brings it back and opens it.

A HUD in the bottom right corner of the main screen lists these bindings;
once hidden with ?, it stays hidden until ? shows it again.
Synthetic clicks put the pointer back where it was.

`search` is a Shortcat style alternative in the minibuffer (lib/minibuffer.fnl):
type part of the text of anything clickable on screen, a window title or a menu
item of the frontmost app. Matches on screen are outlined; RET clicks the
selected one, S-RET right clicks, s-RET Cmd-clicks, C-RET double clicks and
M-RET hovers.
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

;; Hammerspoon reads hex colours as Generic RGB; see lib/which-key.fnl. Labels
;; take the colour of the pending action, or peach while parked; calibrated to
;; show as batppuccin-mocha's colours do in Emacs: right sky #89dceb, double
;; green #a6e3a1, hold flamingo #f2cdcd, hover yellow #f9e2af, drag blue
;; #89b4fa, copy red #f38ba8, caret teal #94e2d5, parked peach #fab387; the
;; caret mode cursor is rosewater #f5e0dc, as Emacs's is.
(local palette {:bg "#bf90f5" :fg "#161623" :typed "#6f4e99" :border "#161623"
                :parked "#f5a677" :right "#79d4e6" :double "#97e091"
                :hold "#edc1c3" :hover "#f7dd9f" :drag "#78a6f8"
                :copy "#f07897" :caret "#82dbcb" :cursor "#f2d9d4"})
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

(fn subtract [rect hole]
  "The parts of RECT outside HOLE, as up to four rectangles."
  (let [cut (intersection rect hole)]
    (if (not cut) [rect]
        (let [right (+ rect.x rect.w)
              bottom (+ rect.y rect.h)
              pieces [{:x rect.x :y rect.y :w rect.w :h (- cut.y rect.y)}
                      {:x rect.x :y (+ cut.y cut.h) :w rect.w
                       :h (- bottom cut.y cut.h)}
                      {:x rect.x :y cut.y :w (- cut.x rect.x) :h cut.h}
                      {:x (+ cut.x cut.w) :y cut.y :w (- right cut.x cut.w)
                       :h cut.h}]]
          (icollect [_ p (ipairs pieces)]
            (when (and (> p.w 0) (> p.h 0)) p))))))

(fn merge-pieces [pieces]
  "Join pieces sharing a whole edge, which cutting by several holes leaves
  apart, e.g. a row's strips above and beside two overlapping windows."
  (var merged? true)
  (while merged?
    (set merged? false)
    (for [i 1 (length pieces) &until merged?]
      (for [j (+ i 1) (length pieces) &until merged?]
        (let [a (. pieces i)
              b (. pieces j)
              joined (if (and (= a.x b.x) (= a.w b.w)
                              (or (= (+ a.y a.h) b.y) (= (+ b.y b.h) a.y)))
                         {:x a.x :y (math.min a.y b.y) :w a.w :h (+ a.h b.h)}
                         (and (= a.y b.y) (= a.h b.h)
                              (or (= (+ a.x a.w) b.x) (= (+ b.x b.w) a.x)))
                         {:x (math.min a.x b.x) :y a.y :w (+ a.w b.w) :h a.h})]
          (when joined
            (tset pieces i joined)
            (table.remove pieces j)
            (set merged? true))))))
  pieces)

(fn uncovered [rect holes]
  "The largest part of RECT outside every one of HOLES, or nil if they cover
  it entirely, e.g. the uncovered half of a list row under another window."
  (var pieces [rect])
  (each [_ hole (ipairs holes)]
    (set pieces (merge-pieces
                 (accumulate [acc [] _ p (ipairs pieces)]
                   (do (each [_ q (ipairs (subtract p hole))]
                         (table.insert acc q))
                       acc)))))
  (var best nil)
  (each [_ p (ipairs pieces)]
    (when (or (not best) (> (* p.w p.h) (* best.w best.h)))
      (set best p)))
  best)

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

;; Notification Center's cards: pressing one opens it (or expands a stack).
(local notification-subroles
       {:AXNotificationCenterAlert true :AXNotificationCenterBanner true
        :AXNotificationCenterAlertStack true
        :AXNotificationCenterBannerStack true})

(fn focusable? [element]
  (let [(ok settable) (pcall #(: element :isAttributeSettable :AXFocused))]
    (and ok settable)))

(fn degenerate? [frame]
  (or (<= frame.w 1) (<= frame.h 1)))

(fn within-parent? [element]
  (let [frame (attr element :AXFrame)
        parent (attr element :AXParent)
        outer (and parent (attr parent :AXFrame))]
    (or (not frame) (not outer) (degenerate? outer)
        (contains? {:x (- outer.x 1) :y (- outer.y 1)
                    :w (+ outer.w 2) :h (+ outer.h 2)}
                   frame))))

(fn clickable? [element role subrole in-row?]
  (if (. skipped-subroles subrole) false
      (. notification-subroles subrole) true
      ;; Table cells are reachable through their row; free-standing cells
      ;; (e.g. Chromium grids) are not.
      (= role :AXCell) (not in-row?)
      ;; A row's text that can't take focus is just its label, such as a file
      ;; name in Finder's list view, and the row already has a hint.
      (and in-row? (or (= role :AXTextField) (= role :AXTextArea)))
      (focusable? element)
      ;; Text that can be pressed and focused is an item, e.g. Calendar's
      ;; events; web page text is neither. Calendar keeps the events hidden
      ;; behind "+N more" with stale frames sticking out of their day, so the
      ;; text must lie within its container.
      (= role :AXStaticText)
      (and (has-action? element [:AXPress]) (focusable? element)
           (within-parent? element))
      ;; Finder icon view items are images with an Open action.
      (= role :AXImage) (has-action? element [:AXOpen :AXPress])
      (. clickable-roles role) true
      false))

(fn now [] (hs.timer.absoluteTime))

(local min-shown 8)

(fn shown-enough? [shown frame]
  "Whether SHOWN, the part of FRAME that is scrolled into view and uncovered,
  is big enough to label: at least `min-shown` each way, or half of a smaller
  element, so that a sliver of a row at the edge of a list is not."
  (and shown
       (>= shown.w (math.min min-shown (/ frame.w 2)))
       (>= shown.h (math.min min-shown (/ frame.h 2)))))

(local table-roles {:AXTable true :AXOutline true})

(fn header-extent [table-element]
  "The area TABLE-ELEMENT's column header covers, if it has one. Finder's
  header is a zero-height group whose buttons float over the rows."
  (let [header (attr table-element :AXHeader)
        frame (and header (attr header :AXFrame))]
    (when frame
      (let [rects (icollect [_ child (ipairs (or (attr header :AXChildren) []))]
                    (attr child :AXFrame))]
        (when (not (degenerate? frame))
          (table.insert rects frame))
        (when (> (length rects) 0)
          (let [b (bounds rects)
                x1 (math.min b.x frame.x)
                x2 (math.max (+ b.x b.w) (+ frame.x frame.w))]
            (values header {:x x1 :y b.y :w (- x2 x1) :h b.h})))))))

(fn collect-tree [root {: clip : occluders : window} acc budget pick?]
  "Walk ROOT's accessibility tree and append a candidate to ACC for every
  element satisfying PICK? (default `clickable?`) that is visible: inside CLIP
  (narrowed by each enclosing scroll area) and not entirely under the
  OCCLUDERS, the frames of windows in front, or under the column header of
  its table, which rows scroll beneath. A partly covered element is labelled
  on its largest uncovered part. Subtrees that are scrolled out or
  fully covered by another window are skipped, and so are those of Chromium's
  collapsed 1px elements; a single zero-size layer is walked through, though,
  such as the header of Finder's list view, which holds its column buttons.
  The pids of apps showing web content are recorded in BUDGET.web-pids, and
  candidates inside it are marked `web?`, the web area they are in."
  (let [pid (and window (: window :pid))]
    (fn visit [element clip occluders in-row? in-web? thin?]
      (set budget.visited (+ budget.visited 1))
      (when (and (< budget.visited max-visited) (< (now) budget.deadline))
        (let [role (attr element :AXRole)
              subrole (attr element :AXSubrole)
              frame (attr element :AXFrame)
              visible (and frame (intersection frame clip))
              pass-through? (and frame (degenerate? frame) (not thin?)
                                 (inside? clip frame))]
          (when (or (= frame nil) pass-through?
                    (and visible (not (degenerate? frame))
                         (not (any? #(contains? $ visible) occluders))))
            (let [shown (and visible (not (degenerate? frame))
                             (uncovered visible occluders))]
              (when (and (shown-enough? shown frame)
                         ((or pick? clickable?) element role subrole in-row?
                                                in-web?))
                (table.insert acc {: element : role : subrole :frame shown
                                   : window :web? in-web?})))
            (when (and pid budget.web-pids (= role :AXWebArea))
              (tset budget.web-pids pid true))
            ;; A menu bar item's menu is only worth walking while it is open.
            (when (or (not (. leaf-roles role))
                      (and (= role :AXMenuBarItem) (attr element :AXSelected)))
              (let [clip (if (and visible (= role :AXScrollArea)) visible clip)
                    in-row? (or in-row? (= role :AXRow))
                    in-web? (or in-web? (and (= role :AXWebArea) element))
                    (header covered) (when (. table-roles role)
                                       (header-extent element))
                    under-header (if covered
                                     (doto (icollect [_ o (ipairs occluders)] o)
                                       (table.insert covered))
                                     occluders)]
                (each [_ child (ipairs (or (attr element :AXChildren) []))]
                  (visit child clip
                         (if (= child header) occluders under-header)
                         in-row? in-web? (or thin? pass-through?)))))))))
    (visit root clip occluders false false false)))

(fn screen-frame [rect]
  "The full frame of the screen RECT is mostly on."
  (: (or (hs.screen.find (center rect)) (hs.screen.mainScreen)) :fullFrame))

(fn on-screen-frame [rect]
  "Clip RECT to the screen it is mostly on."
  (intersection rect (screen-frame rect)))

;; Clicks this close outside a window's edge go to the window, to resize it.
(local resize-margin 4)

(fn window-roots [floating]
  "Visible standard windows, front to back, each clipped to its screen and
  occluded by the windows in front of it, including their resize margins,
  and by the FLOATING panel frames."
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
          (table.insert occluders
                        {:x (- frame.x resize-margin) :y (- frame.y resize-margin)
                         :w (+ frame.w (* 2 resize-margin))
                         :h (+ frame.h (* 2 resize-margin))}))))
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

(local max-overlay-depth 6)

(fn painted-frames [win]
  "The frames WIN actually draws over the windows below it. Notification
  Center draws its alerts and widgets on a transparent window as large as the
  screen, so such a window is looked through, along with the layers inside it
  spanning (nearly) the screen's width or height, down to the cards on it."
  (let [frame (attr win :AXFrame)
        screen (and frame (screen-frame frame))]
    (if (not frame) []
        (not (contains? frame screen)) [frame]
        (let [frames []]
          (fn layer? [f depth]
            (and (< depth max-overlay-depth)
                 (or (>= f.w (* 0.9 screen.w)) (>= f.h (* 0.9 screen.h)))))
          (fn visit [element depth]
            (let [f (attr element :AXFrame)]
              (if (not f) nil
                  (layer? f depth)
                  (each [_ child (ipairs (or (attr element :AXChildren) []))]
                    (visit child (+ depth 1)))
                  (and (> f.w 1) (> f.h 1))
                  (table.insert frames f))))
          (visit win 0)
          frames))))

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
            (let [covered (painted-frames win)
                  _ (each [_ el (ipairs (or (attr win :AXChildren) []))]
                      (let [frame (and (= (attr el :AXRole) :AXPopover)
                                       (attr el :AXFrame))]
                        (when frame (table.insert covered frame))))
                  panel-clip (and (> (length covered) 0)
                                  (intersection clip (bounds covered)))]
              (when panel-clip
                (table.insert roots {:element win :clip panel-clip :occluders []})
                (each [_ frame (ipairs covered)]
                  (table.insert frames frame))))))))
    (values roots frames)))

(fn hidden-bar-frame [frame]
  "Where FRAME, a status item's, shows once its menu bar does, if the menu bar
  is hidden now, e.g. by a full screen app: hidden, the bar slides up past the
  screen's top edge, so the item's bottom lies as far above the edge as its top
  will lie below it."
  (when (and frame (not (degenerate? frame)))
    (let [x (+ frame.x (/ frame.w 2))
          bottom (+ frame.y frame.h)]
      (accumulate [shown nil _ screen (ipairs (hs.screen.allScreens)) &until shown]
        (let [full (: screen :fullFrame)]
          (when (and (>= x full.x) (< x (+ full.x full.w)) (<= bottom full.y))
            {:x frame.x :y (- (* 2 full.y) bottom) :w frame.w :h frame.h}))))))

(fn chrome-roots [clip]
  "The frontmost app's menu bar, every app's status menu items and the Dock.
  Status items of a hidden menu bar are returned as candidates of their own,
  labelled where they will show: pressing one brings the menu bar back."
  (let [roots []
        hidden []
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
              (let [shown (hidden-bar-frame (attr item :AXFrame))]
                (if shown
                    (table.insert hidden {:element item :role :AXMenuBarItem
                                          :subrole (attr item :AXSubrole)
                                          :frame shown :hidden-bar? true})
                    (table.insert roots {:element item : clip :occluders []}))))))))
    (let [dock (hs.application.get :com.apple.dock)]
      (when dock
        (table.insert roots {:element (hs.axuielement.applicationElement dock)
                             : clip :occluders []})))
    (values roots hidden)))

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

(fn chromium? [app]
  "Chromium and Electron apps ship a crashpad handler with their framework."
  (let [frameworks (.. (or (: app :path) "") "/Contents/Frameworks")]
    (var found false)
    (when (hs.fs.attributes frameworks)
      (each [name (hs.fs.dir frameworks) &until found]
        (when (name:match "%.framework$")
          (let [helpers (.. frameworks "/" name "/Helpers/")]
            (set found (or (hs.fs.attributes (.. helpers "chrome_crashpad_handler"))
                           (hs.fs.attributes (.. helpers "browser_crashpad_handler"))
                           false))))))
    found))

(fn wake [app]
  "Electron and Chromium only build their web accessibility tree once an
  assistive app asks for it, which AXManualAccessibility does without the
  side effects of AXEnhancedUserInterface. Some Chromium builds (e.g. the
  ChatGPT app) don't support it, and get AXEnhancedUserInterface instead.
  Safari (WebKit) builds the tree the first time a client lists the web
  view's children. Either way it arrives asynchronously, up to a few seconds
  later. Returns true if APP was asked to build a tree."
  (let [element (hs.axuielement.applicationElement app)
        set! (fn [name]
               (let [(ok result) (pcall #(: element :setAttributeValue name true))]
                 (and ok result true)))]
    (pcall #(: element :setTimeout 0.5))
    (or (set! :AXManualAccessibility)
        ;; Reported as not implemented, yet it builds the tree.
        (and (chromium? app) (do (set! :AXEnhancedUserInterface) true))
        false)))

(fn wake-web-content [roots]
  "Wake every app with a window among ROOTS, and the frontmost one, the
  first time it is seen. Returns the newly seen pids, each mapped to whether
  it was asked to build a web tree."
  (let [fresh {}
        apps (icollect [_ root (ipairs roots)]
               (and root.window (: root.window :application)))
        front (hs.application.frontmostApplication)]
    (when front (table.insert apps front))
    (each [_ app (ipairs apps)]
      (let [pid (: app :pid)]
        (when (not (. woken-pids pid))
          (tset woken-pids pid true)
          (tset fresh pid (wake app)))))
    fresh))

(fn collect-candidates [?pick ?chrome?]
  "The candidates on screen, and what the rescan needs: the pids woken for
  the first time, the pids showing web content and their web hint counts.
  ?PICK replaces `clickable?`, and leaves out the menu bar and the Dock unless
  ?CHROME? is set."
  (let [acc []
        budget {:visited 0 :deadline (+ (now) time-budget-ns) :web-pids {}}
        started (now)
        clip (screen-clip)
        (panels floating) (panel-roots clip)
        windows (window-roots floating)
        fresh (wake-web-content windows)]
    (each [_ root (ipairs panels)]
      (collect-tree root.element root acc budget ?pick))
    (each [_ root (ipairs windows)]
      (collect-tree root.element root acc budget ?pick))
    (when (or (not ?pick) ?chrome?)
      (let [(chrome hidden-bar) (chrome-roots clip)]
        (each [_ root (ipairs chrome)]
          (collect-tree root.element root acc budget ?pick))
        (each [_ c (ipairs hidden-bar)]
          (table.insert acc c))))
    (let [result (dedupe acc)
          web-hints {}]
      (when (or (>= budget.visited max-visited) (>= (now) budget.deadline))
        (log.wf "Out of budget after %d elements in %.0f ms: some hints are missing"
                budget.visited (/ (- (now) started) 1e6)))
      (each [_ c (ipairs result)]
        (when (and c.web? c.window)
          (let [pid (: c.window :pid)]
            (tset web-hints pid (+ 1 (or (. web-hints pid) 0))))))
      (log.df "%d hints from %d elements in %.0f ms" (length result)
              budget.visited (/ (- (now) started) 1e6))
      (values result {: fresh :web-pids budget.web-pids : web-hints}))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text, for copying a range of it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn param [element name value]
  "Read a parameterized AX attribute, nil if the element has gone stale."
  (let [(ok result) (pcall #(: element :parameterizedAttributeValue name value))]
    (when ok result)))

(fn string-value [element ?name]
  (let [value (attr element (or ?name :AXValue))]
    (when (and (= (type value) :string) (value:find "%S"))
      value)))

(fn text-target? [element role subrole _ in-web?]
  "Text on a web page, links included, and native labels and text fields."
  (if in-web?
      (or (= role :AXLink) (and (= role :AXStaticText) (string-value element) true))
      (and (or (= role :AXStaticText) (= role :AXTextField) (= role :AXTextArea))
           (not= subrole :AXSecureTextField)
           (string-value element)
           true)))

;; Web text this tall may wrap; it is split into lines, each labelled.
(local single-line-height 30)
(local max-lines 200)

(fn chromium-web? [web]
  (not= nil (attr web :ChromeAXNodeId)))

(fn marker-order [web]
  "A function telling whether text marker A comes before B in WEB, or at it.
  WebKit numbers its markers through the page; Chromium's numbers count from
  the start of the element a marker is in, so there it asks which of the two
  starts the range they make."
  (if (chromium-web? web)
      (fn [a b]
        (or (= a b)
            (let [range (param web :AXTextMarkerRangeForUnorderedTextMarkers [a b])]
              (and range (= (: range :startMarker) a)))))
      (fn [a b]
        (let [i (param web :AXIndexForTextMarker a)
              j (param web :AXIndexForTextMarker b)]
          (and i j (<= i j))))))

(fn char-bounds [web from to]
  "The bounds of the character between markers FROM and TO. Chromium gives
  the whole page's when the two are in different elements, as at the end of
  one, so then the element's own are asked for."
  (let [bounds (param web :AXBoundsForTextMarkerRange
                      (hs.axuielement.axtextmarker.newRange from to))]
    (if (and bounds (< bounds.w 200) (< bounds.h 200))
        bounds
        (let [element (param web :AXUIElementForTextMarker from)
              offset (param web :AXIndexForTextMarker from)]
          (or (and element offset
                   (param element :AXBoundsForRange {:location offset :length 1}))
              bounds)))))

(fn chromium-string [web from to]
  "The text from marker FROM to TO in WEB, a Chromium page. Chromium leaves
  out the line breaks between blocks, where a step from a character to the
  next takes in no text, so one goes wherever such a step goes down a line."
  (let [before? (marker-order web)
        parts []]
    (var marker from)
    (var last nil)
    (var gap? false)
    (for [_ 1 20000 &until (or (not marker) (before? to marker))]
      (let [next (param web :AXNextTextMarkerForTextMarker marker)
            text (and next (not= next marker)
                      (param web :AXStringForTextMarkerRange
                             (hs.axuielement.axtextmarker.newRange marker next)))]
        (if (= text "") (set gap? true)
            text (do
                   (when (and gap? last (not (: (. last 3) :find "\n$")))
                     (let [a (char-bounds web (. last 1) (. last 2))
                           b (char-bounds web marker next)]
                       (when (and a b (>= b.y (+ a.y (/ a.h 2))))
                         (table.insert parts "\n"))))
                   (set gap? false)
                   (set last [marker next text])
                   (table.insert parts text)))
        (set marker (when text next))))
    (table.concat parts)))

(fn text-lines [c ?limit]
  "Web text candidate C as one candidate per line on screen, each with the
  text markers of its start and end, looking at no more than ?LIMIT lines. A
  text's first line can start halfway along the line, after a link, say, so it
  is labelled where it starts."
  (let [web c.web?
        range (param web :AXTextMarkerRangeForUIElement c.element)
        start (and range (: range :startMarker))
        end (and range (: range :endMarker))
        before? (marker-order web)
        full (attr c.element :AXFrame)
        line-candidate (fn [frame from to]
                         {:element c.element :role c.role :frame frame
                          :window c.window :web? web :start from :end to})]
    (if (not end) []
        (or (not full) (<= full.h single-line-height))
        [(line-candidate c.frame start end)]
        (let [lines []]
          (var marker start)
          (var previous nil)
          (for [i 1 (or ?limit max-lines) &until (not marker)]
            (let [line (param web :AXLineTextMarkerRangeForTextMarker marker)
                  line-end (and line (: line :endMarker))]
              (if (or (not line-end) (and previous (before? line-end previous)))
                  (set marker nil)
                  (let [last? (before? end line-end)
                        from (if (= i 1) start (: line :startMarker))
                        to (if last? end line-end)
                        bounds (param web :AXBoundsForTextMarkerRange
                                      (hs.axuielement.axtextmarker.newRange from to))
                        shown (and bounds (intersection bounds c.frame))]
                    (when (shown-enough? shown bounds)
                      (table.insert lines (line-candidate shown from to)))
                    (set previous line-end)
                    (set marker (when (not last?)
                                  (param web :AXNextTextMarkerForTextMarker
                                         line-end)))))))
          lines))))

(fn text-candidates []
  "The text on screen: web text a line at a time, other text an element at a
  time."
  (let [acc []]
    (each [_ c (ipairs (collect-candidates text-target?))]
      (if c.web?
          (each [_ line (ipairs (text-lines c))]
            (table.insert acc line))
          (table.insert acc c)))
    (dedupe acc)))

(fn drag-target? [element role subrole in-row? in-web?]
  "What can be clicked, text, which may be a draggable item's or a drop zone's,
  and images, unless they are in a row, which is labelled already. Named lists
  are drop zones too, such as the days of Calendar's month view."
  (or (clickable? element role subrole in-row?)
      (and (not in-row?)
           (or (text-target? element role subrole in-row? in-web?)
               (= role :AXImage)
               (and (= role :AXList) (string-value element :AXTitle)
                    true)))))

(local drop-strip 24)

(fn drag-candidates []
  "What can be dragged or dropped on. Web text is labelled where it starts,
  not at the left of the paragraph it wraps in, so is dragged from there. A
  list, such as a day of Calendar's month view, is labelled and dropped on
  along its bottom, below what it holds, where there is usually room."
  (dedupe
   (icollect [_ c (ipairs (collect-candidates drag-target? true))]
     (let [line (and c.web? (= c.role :AXStaticText) (. (text-lines c 1) 1))
           f c.frame]
       (if line (set c.frame line.frame)
           (and (= c.role :AXList) (> f.h (* 2 drop-strip)))
           (set c.frame {:x f.x :y (- (+ f.y f.h) drop-strip) :w f.w :h drop-strip}))
       c))))

(fn copy-text [text]
  (if (and text (text:find "%S"))
      (do
        (hs.pasteboard.setContents text)
        (minibuffer.message
         (let [line (: (text:gsub "%s+" " ") :match "^%s*(.-)%s*$")]
           (.. "Copied " (if (> (utf8.len line) 60)
                             (.. (line:sub 1 (- (utf8.offset line 60) 1)) "…")
                             line)))))
      (minibuffer.message "No text to copy")))

(fn select-text [a b hints]
  "Select the text from the start of A to the end of B, whichever comes
  first, and copy it. Web text is selected in the page, so Shift with the
  arrow keys can still adjust the end; other text, which can't be selected
  this way, is copied from A through B in HINTS order. Chromium ignores
  selections set this way, so there the text is only copied."
  (let [web a.web?]
    (if (and web a.start b.start (= web b.web?))
        (let [chromium? (chromium-web? web)
              (first last) (if ((marker-order web) a.start b.start)
                               (values a b)
                               (values b a))
              ;; Markers from AXTextMarkerRangeForUIElement read fine, but
              ;; selecting them can land a few characters off, past a link,
              ;; say; markers made from character indices select exactly.
              remake #(let [i (and (not chromium?)
                                   (param web :AXIndexForTextMarker $))]
                        (or (and i (param web :AXTextMarkerForIndex i)) $))
              range (hs.axuielement.axtextmarker.newRange (remake first.start)
                                                          (remake last.end))]
          (when (not chromium?)
            ;; Focused, the page shows the selection as active, and takes keys.
            (pcall #(: web :setAttributeValue :AXFocused true))
            (pcall #(: web :setAttributeValue :AXSelectedTextMarkerRange range)))
          (copy-text (if chromium?
                         (chromium-string web first.start last.end)
                         (param web :AXStringForTextMarkerRange range))))
        (let [position (fn [hint]
                         (accumulate [found nil i h (ipairs hints) &until found]
                           (when (= h hint) i)))
              i (or (position a) 1)
              j (or (position b) i)]
          (copy-text
           (table.concat
            (fcollect [k (math.min i j) (math.max i j)]
              (let [h (. hints k)]
                (or (string-value h.element) (attr h.element :AXTitle))))
            "\n"))))))

(fn deselect-web [web]
  "Collapse WEB's selection to where it ends. True if text was selected.
  Chromium ignores selections set this way, so it is left alone."
  (let [range (and (not (chromium-web? web)) (attr web :AXSelectedTextMarkerRange))
        from (and range (param web :AXIndexForTextMarker (: range :startMarker)))
        to (and range (param web :AXIndexForTextMarker (: range :endMarker)))
        marker (and from to (< from to) (param web :AXTextMarkerForIndex to))]
    (when marker
      (let [(ok) (pcall #(: web :setAttributeValue :AXSelectedTextMarkerRange
                            (hs.axuielement.axtextmarker.newRange marker marker)))]
        ok))))

(fn deselect-focused []
  "Collapse the selection of the focused text element, such as a text field,
  outside web pages. True if text was selected."
  (let [element (attr (hs.axuielement.systemWideElement) :AXFocusedUIElement)
        range (and element (not= (attr element :AXRole) :AXWebArea)
                   (attr element :AXSelectedTextRange))]
    (when (and range (> range.length 0))
      (let [(ok) (pcall #(: element :setAttributeValue :AXSelectedTextRange
                            {:location (+ range.location range.length)
                             :length 0}))]
        ok))))

(fn clear-selections [candidates]
  "Clear the text selected in the web pages among CANDIDATES and in the
  focused text element."
  (let [webs []]
    (each [_ c (ipairs candidates)]
      (when (and c.web? (not (any? #(= $ c.web?) webs)))
        (table.insert webs c.web?)))
    (let [cleared (accumulate [n 0 _ web (ipairs webs)]
                    (if (deselect-web web) (+ n 1) n))
          cleared (if (deselect-focused) (+ cleared 1) cleared)]
      (minibuffer.message (if (> cleared 0) "Selection cleared" "Nothing selected")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Labels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn make-labels [n ?chars]
  "N prefix-free labels as short as possible, using Vimium's scheme: keep
  expanding the oldest label by every character of ?CHARS (default
  `hint-chars`) until there are enough."
  (let [chars (icollect [c (: (or ?chars hint-chars) :gmatch ".")] c)
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

;; A press this long opens e.g. the menu of Safari's speaker button; released
;; much later (about a second), the menu closes again.
(local hold-time 0.5)
(var release-timer nil)

(fn click [point {: button : mods : count : hold}]
  "Post a synthetic click at POINT with exactly MODS held, regardless of the
  keys physically down, then put the pointer back. HOLD keeps the button down
  that many seconds, a long press."
  (let [saved (hs.mouse.absolutePosition)
        right? (= button :right)
        down-type (if right? types.rightMouseDown types.leftMouseDown)
        up-type (if right? types.rightMouseUp types.leftMouseUp)
        click-state hs.eventtap.event.properties.mouseEventClickState
        event (fn [type n]
                (doto (hs.eventtap.event.newMouseEvent type point (or mods []))
                  (: :setProperty click-state n)))
        restore #(hs.timer.doAfter 0.05 #(hs.mouse.absolutePosition saved))]
    (if hold
        (do
          (: (event down-type 1) :post)
          (set release-timer
               (hs.timer.doAfter hold
                                 #(do (: (event up-type 1) :post)
                                      (set release-timer nil)
                                      (restore)))))
        (do
          (for [n 1 (or count 1)]
            (: (event down-type n) :post)
            (hs.timer.usleep 15000)
            (: (event up-type n) :post))
          (restore)))))

(fn press [element]
  (let [(ok result) (pcall #(: element :performAction :AXPress))]
    (and ok result)))

(local click-modifiers [:cmd :shift :alt :ctrl])

(fn held-modifiers [flags]
  (icollect [_ m (ipairs click-modifiers)] (when (. flags m) m)))

;; Keys typed before a label's last key to do something other than click. The
;; last two pick two labels: what to drag and where to, or where the text to
;; copy starts and ends.
(local hint-actions {:r :right :t :double :h :hover :v :caret :g :drag :y :copy})
(local action-names {:right "right click" :double "double click"
                     :hold "press and hold" :hover "hover"
                     :drag "drag" :copy "copy text" :caret "caret"})
(local pair-actions {:drag true :copy true})

;; Defined with caret mode below; picking a text after v starts it.
(var caret-mode nil)

(var drag-timers [])

(fn drag [from to mods]
  "Drag from the middle of FROM to the middle of TO with MODS held, e.g. alt
  to copy a file, in small steps and with pauses: apps only start a drag once
  the pointer moves with the button down (selected text only after a pause,
  or the press starts a new selection), and targets only accept a drop once
  the pointer has lingered over them. Web text that can't be dragged gets
  selected instead, and stays selected (x clears it)."
  (let [saved (hs.mouse.absolutePosition)
        a (center from.frame)
        b (center to.frame)
        steps 12
        step-time 0.02
        pressed 0.3
        click-state hs.eventtap.event.properties.mouseEventClickState
        post (fn [type point]
               (: (doto (hs.eventtap.event.newMouseEvent type point mods)
                    (: :setProperty click-state 1))
                  :post))
        at (fn [delay f] (table.insert drag-timers (hs.timer.doAfter delay f)))
        moved (+ pressed (* steps step-time))]
    (set drag-timers [])
    (hs.mouse.absolutePosition a)
    (at 0.05 #(post types.leftMouseDown a))
    (for [i 1 steps]
      (let [t (/ i steps)
            p {:x (+ a.x (* t (- b.x a.x))) :y (+ a.y (* t (- b.y a.y)))}]
        (at (+ pressed (* i step-time)) #(post types.leftMouseDragged p))))
    (at (+ moved 0.1) #(post types.leftMouseDragged b))
    (at (+ moved 0.25) #(post types.leftMouseUp b))
    (at (+ moved 0.4) #(hs.mouse.absolutePosition saved))))

(fn perform [hint action flags]
  "Do ACTION (nil to click) on HINT with the modifiers in FLAGS held. A plain
  click on a button presses it through Accessibility instead."
  (let [point (center hint.frame)
        mods (held-modifiers flags)]
    (case action
      :caret (caret-mode hint)
      :hover (hs.mouse.absolutePosition point)
      ;; Nothing is there to click until the hidden menu bar shows again.
      (where _ hint.hidden-bar?) (press hint.element)
      :right (click point {:button :right : mods})
      :double (click point {:count 2 : mods})
      :hold (click point {:hold hold-time : mods})
      _ (when (not (and (= (length mods) 0)
                        (or (. press-roles hint.role)
                            (. notification-subroles hint.subrole))
                        (press hint.element)))
          (click point {: mods})))))

(fn activate-hint [hint action flags]
  "Bring the hint's window forward first: a click on an inactive window often
  only activates it, and a pressed button should end up in front."
  (let [win hint.window
        focused (hs.window.focusedWindow)]
    (if (and win (not= action :hover)
             (not (and focused (= (: focused :id) (: win :id)))))
        (do
          (: win :focus)
          (hs.timer.doAfter 0.15 #(perform hint action flags)))
        (perform hint action flags))))

(fn activate-pair [from to action flags hints]
  "Drag FROM to TO, or copy the text from FROM to TO, bringing its window
  forward first so the selection can be adjusted from the keyboard. A drag
  leaves the windows as they are: the source's coming forward could cover the
  target."
  (if (= action :drag)
      (if (= from to)
          (minibuffer.message "Pick somewhere else to drop it")
          (drag from to (held-modifiers flags)))
      (let [win from.window
            focused (hs.window.focusedWindow)]
        (if (and win (not (and focused (= (: focused :id) (: win :id)))))
            (do
              (: win :focus)
              (hs.timer.doAfter 0.15 #(select-text from to hints)))
            (select-text from to hints)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overlay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(var state nil)
;; Defined with scroll mode below; TAB in hint mode switches to it.
(var scroll-mode nil)

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

;; The bindings HUD in the bottom right corner of the main screen. It is drawn
;; beneath the labels, so those of elements behind it stay readable.
(local hud-palette {:bg "#161623" :border "#343647" :fg "#c1cbf1"
                    :key "#bf90f5" :dim "#8187a1"})
(local hud-font-size 13)
(local hud-margin 12)
(local hud-pad-x 12)
(local hud-pad-y 8)

;; Whether the HUD is shown; `?` toggles it, and it stays so until toggled again.
(var hud-shown? true)

(fn toggle-hud []
  (set hud-shown? (not hud-shown?)))

(fn hud-bindings [parked?]
  [[nil "On a label's last key:"]
   ["key" "click"]
   ["mods-key" "click with those modifiers"]
   [nil "Before it, to act otherwise:"]
   ["r" "right click" palette.right]
   ["t" "double click" palette.double]
   ["h" "hover, no click" palette.hover]
   ["H" "press and hold" palette.hold]
   ["g" "drag: pick what, then where to" palette.drag]
   ["y" "copy text: pick its start, then end" palette.copy]
   ["v" "caret: put a cursor on the text" palette.caret]
   ["P" (if parked? "leave park" "park: stay after each action") palette.parked]
   [nil "Any time:"]
   ["DEL" "delete a typed key, or the first pick"]
   ["x" "clear the text selection"]
   ["/" "search by text"]
   ["TAB" "scroll mode"]
   ["?" "hide this HUD"]
   ["ESC C-g" "cancel"]])

(fn caret-bindings [visual]
  (let [moves [["h l" "previous / next character"]
               ["j k" "next / previous line"]
               ["0 $" "start / end of the line"]
               ["w b" "next / previous word"]
               ["e ge" "end of the word / previous word"]
               ["W B E gE" "the same, by words between blanks"]
               [") (" "next / previous sentence"]
               ["} {" "end / start of the paragraph"]
               ["]] [[" "next / previous heading"]
               ["][ []" "end of the section / previous one"]]
        modes (if visual
                  [[nil "On the selection:"]
                   ["y" "copy it, and stop selecting"]
                   ["o" "go to its other end"]
                   ["v V" (if (= visual :char)
                              "stop selecting / by lines"
                              "by characters / stop selecting")]
                   ["ESC" "stop selecting"]]
                  [[nil "Select:"]
                   ["v" "from this character"]
                   ["V" "by lines, from this one"]])
        rest [[nil "Any time:"]
              ["?" "hide this HUD"]
              [(if visual "C-g q" "ESC C-g q") "leave"]]]
    (let [all []]
      (each [_ rows (ipairs [moves modes rest])]
        (each [_ row (ipairs rows)]
          (table.insert all row)))
      all)))

(fn scroll-bindings []
  [[nil "Type an area's label to pick it, then:"]
   ["h j k l" "scroll while held"]
   ["S-hjkl" "scroll faster"]
   ["d u" "half a page down / up"]
   ["gg G" "top / bottom"]
   ["TAB" "back to hints"]
   ["?" "hide this HUD"]
   ["ESC C-g q" "leave"]])

(fn hud-text [title-text title-color bindings]
  "The HUD's text: TITLE-TEXT, then BINDINGS, [key description key-color]
  rows, where a row without a key is a subsection heading, with a blank line
  before it."
  (let [width (accumulate [w 0 _ [key] (ipairs bindings)]
                (math.max w (length (or key ""))))
        font {:name font-name :size hud-font-size}
        styled (fn [text color]
                 (hs.styledtext.new text {: font :color {:hex color}}))]
    (accumulate [text (styled title-text title-color)
                 _ [key desc key-color] (ipairs bindings)]
      (if key
          (.. text
              (styled (.. "\n" key (string.rep " " (- width (length key))) "  ")
                      (or key-color hud-palette.key))
              (styled desc hud-palette.fg))
          (.. text (styled (.. "\n\n" desc) hud-palette.dim))))))

(fn hud-elements [canvas-frame screen-frame title-text title-color bindings]
  "Canvas elements for a HUD listing BINDINGS in the bottom right corner of
  SCREEN-FRAME, the part of the screen CANVAS-FRAME covers below the menu bar."
  (let [text (hud-text title-text title-color bindings)
        size (hs.drawing.getTextDrawingSize text)
        w (+ (math.ceil size.w) (* 2 hud-pad-x))
        h (+ (math.ceil size.h) (* 2 hud-pad-y))
        x (- (+ screen-frame.x screen-frame.w) canvas-frame.x w hud-margin)
        y (- (+ screen-frame.y screen-frame.h) canvas-frame.y h hud-margin)]
    [{:type :rectangle
      :frame {: x : y : w : h}
      :roundedRectRadii {:xRadius 8 :yRadius 8}
      :fillColor {:hex hud-palette.bg :alpha 0.92}
      :strokeColor {:hex hud-palette.border}
      :strokeWidth 1}
     {:type :text
      :frame {:x (+ x hud-pad-x) :y (+ y hud-pad-y)
              :w (- w (* 2 hud-pad-x)) :h (- h (* 2 hud-pad-y))}
      : text}]))

(fn hud-title []
  (.. (if state.parked? "Hints (parked)" "Hints")
      (if state.action
          (.. ": " (. action-names state.action)
              (if (not state.from) ""
                  (= state.action :drag) ", now where to"
                  ", now where it ends"))
          "")))

(fn render []
  (let [visible (visible-hints)
        label-color (or (. palette state.action)
                        (if state.parked? palette.parked palette.bg))]
    (each [_ {: canvas : frame : screen} (ipairs state.canvases)]
      (let [elements (if (and hud-shown? (= (: screen :id) state.hud-screen))
                         (hud-elements frame (which-key.bottom-frame screen)
                                       (hud-title)
                                       (or (. palette state.action)
                                           (if state.parked?
                                               palette.parked
                                               hud-palette.key))
                                       (hud-bindings state.parked?))
                         [])]
        ;; The first of a pair of picks stays outlined until the second.
        (when (and state.from (intersection state.from.frame frame))
          (let [f state.from.frame]
            (table.insert elements
                          {:type :rectangle :action :strokeAndFill
                           :frame {:x (- f.x frame.x) :y (- f.y frame.y)
                                   :w f.w :h f.h}
                           :roundedRectRadii {:xRadius 3 :yRadius 3}
                           :fillColor {:hex label-color :alpha 0.2}
                           :strokeColor {:hex label-color}
                           :strokeWidth 3})))
        (each [_ hint (ipairs visible)]
          (let [r (label-rect hint)]
            (when (inside? frame (center r))
              (let [local-frame {:x (- r.x frame.x) :y (- r.y frame.y)
                                 :w r.w :h r.h}]
                (table.insert elements
                              {:type :rectangle
                               :frame local-frame
                               :roundedRectRadii {:xRadius 3 :yRadius 3}
                               :fillColor {:hex label-color}
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
      {: canvas : frame : screen})))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn hide-hints []
  "Leave hint, scroll or caret mode."
  (when state
    (: state.tap :stop)
    (when state.scroller
      (: state.scroller :stop))
    (when state.rescanner
      (: state.rescanner :stop))
    (when state.follower
      (: state.follower :stop))
    (each [_ {: canvas} (ipairs state.canvases)]
      (: canvas :delete))
    (set state nil)))

(fn label! [candidates]
  (let [labels (make-labels (length candidates))]
    (each [i c (ipairs candidates)]
      (set c.label (. labels i)))))

(local refresh-delay 0.3)

(fn hint-set [action]
  (case action :copy :text :caret :text :drag :drag _ :click))

(fn candidates-for [action]
  "What ACTION labels: text to copy, things to drag, or what can be clicked."
  (case (hint-set action)
    :text (text-candidates)
    :drag (drag-candidates)
    _ (collect-candidates)))

(fn refresh [shown]
  "Relabel what is on screen now that an action may have changed it, unless
  the next label is already being typed, or its first pick made."
  (when (and (= state shown) (= state.typed "") (not state.from))
    (let [candidates (candidates-for state.action)]
      (if (= (length candidates) 0)
          (hide-hints)
          (do
            (label! candidates)
            (set state.hints candidates)
            (render))))))

(fn type-char [char flags]
  (let [previous state.typed]
    (set state.typed (.. previous char))
    (let [hints (visible-hints)]
      (if (= (length hints) 0) (set state.typed previous)
          (and (= (length hints) 1) (= (. hints 1 :label) state.typed))
          (let [hint (. hints 1)
                action state.action
                from state.from
                all state.hints
                act (if (not (. pair-actions action))
                        #(activate-hint hint action flags)
                        #(activate-pair from hint action flags all))]
            (if (and (. pair-actions action) (not from))
                ;; The first of two picks: keep the labels up for the second.
                (do
                  (set state.from hint)
                  (set state.typed "")
                  (render))
                state.parked?
                (let [shown state]
                  (set state.typed "")
                  (set state.action nil)
                  (set state.from nil)
                  (render)
                  (act)
                  ;; A drag takes a while; relabel once it is over.
                  (hs.timer.doAfter (if (= action :drag) 1 refresh-delay)
                                    #(refresh shown)))
                (do
                  (hide-hints)
                  (act))))
          (render)))))

(fn set-action [action]
  "Make ACTION pending, or none if it already is. Copying text labels the
  text on screen instead of what can be clicked, line by line; dragging labels
  text and images too."
  (let [previous state.action
        next (when (not= previous action) action)
        hints (when (not= (hint-set previous) (hint-set next))
                (candidates-for next))]
    (if (and hints (= (length hints) 0))
        (minibuffer.message (if (= next :copy) "No text found" "Nothing to click"))
        (do
          (when hints
            (label! hints)
            (set state.hints hints))
          (set state.action next)
          (set state.from nil)
          (set state.typed "")))
    (render)))

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

(fn search-action [flags]
  "The action and modifiers RET with FLAGS stands for in `search`, which has
  no keys to spare for actions: S-RET right click, s-RET Cmd-click, C-RET
  double click, M-RET hover."
  (if flags.alt (values :hover {})
      flags.shift (values :right {})
      flags.cmd (values nil {:cmd true})
      flags.ctrl (values :double {})
      (values nil {})))

(fn accept-search [candidate input flags]
  (hide-search-overlay)
  (if (not candidate)
      (when (not= input "") (minibuffer.message (.. "No match for " input)))
      (= candidate.kind :element)
      (activate-hint candidate.hint (search-action flags))
      (= candidate.kind :menu)
      (when (not (press candidate.element))
        (minibuffer.message (.. "Could not choose " candidate.text)))
      (= candidate.kind :window) (: candidate.win :focus)))

(fn search []
  "Shortcat style search in the minibuffer: type part of the text of anything
  clickable on screen, a window title or a menu item of the frontmost app; RET
  acts on the selection, as `search-action` describes."
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

(local modifier-symbols [[:cmd "⌘"] [:ctrl "⌃"] [:alt "⌥"] [:shift "⇧"]])

(fn hotkey? [key flags]
  "Whether KEY with the modifiers in FLAGS is a Hammerspoon hotkey."
  (and key (or flags.cmd flags.ctrl flags.alt)
       (let [idx (.. (table.concat (icollect [_ [m symbol] (ipairs modifier-symbols)]
                                     (when (. flags m) symbol)))
                     (key:upper))]
         (any? #(= $.idx idx) (hs.hotkey.getHotkeys)))))

(fn on-key [event]
  "Type a label or an action key; any other key closes the hints and goes
  through, so e.g. Cmd-Tab still works. At the start of a label, a key bound as
  a Hammerspoon hotkey, e.g. Ctrl-Alt-SPC (search), goes through as well;
  past it, a modified label key is a modified click."
  (let [key (. hs.keycodes.map (: event :getKeyCode))
        flags (: event :getFlags)
        plain? (not (or flags.cmd flags.ctrl flags.alt flags.shift))
        autorepeat? (= 1 (: event :getProperty
                            hs.eventtap.event.properties.keyboardEventAutorepeat))]
    (if (or (= key :escape) (and flags.ctrl (= key :g)))
        (do (hide-hints) true)
        ;; The hotkey again closes the hints; while it is still held from
        ;; opening them, its space auto-repeats and is ignored.
        (and flags.cmd (= key :space))
        (do (when (not autorepeat?) (hide-hints)) true)
        (and (= state.typed "") (hotkey? key flags))
        (do (hide-hints) false)
        (and (= key "/") flags.shift (not (or flags.cmd flags.ctrl flags.alt)))
        (do (toggle-hud) (render) true)
        (= key "/") (do (hide-hints) (search) true)
        (and plain? (= key :x))
        (do (clear-selections state.hints) true)
        (and (= key :tab) (not (or flags.cmd flags.ctrl flags.alt)))
        (do (scroll-mode) true)
        (and plain? (. hint-actions key))
        (do (set-action (. hint-actions key)) true)
        (and (= key :h) flags.shift (not (or flags.cmd flags.ctrl flags.alt)))
        (do (set-action :hold) true)
        ;; p is a hint character, so Shift-P only parks between labels;
        ;; mid-label it is still a Shift-click.
        (and (= key :p) flags.shift (not flags.alt) (not flags.ctrl)
             (not flags.cmd) (= state.typed ""))
        (if state.parked?
            (do (hide-hints) true)
            (do
              (set state.parked? true)
              (set state.typed "")
              (render)
              true))
        (= key :delete)
        (do
          ;; With nothing typed, DEL takes back the first of two picks.
          (if (and (= state.typed "") state.from)
              (set state.from nil)
              (set state.typed (state.typed:sub 1 -2)))
          (render)
          true)
        (and key (= (length key) 1) (hint-chars:find key 1 true))
        (do (type-char key flags) true)
        (= key :space) true
        (do (hide-hints) false))))

(local rescan-interval 0.25)
(local rescan-limit 20)

(fn settled? [fresh info previous]
  "Whether every newly woken app that may still be building web content has
  shown some and its web hints stayed the same since the PREVIOUS scan."
  (accumulate [ok true pid asked? (pairs fresh) &until (not ok)]
    (if (or asked? (. info.web-pids pid))
        (let [n (or (. info.web-hints pid) 0)]
          (and (> n 0) (= n (or (. previous pid) 0))))
        true)))

(fn rescan []
  "Pick up web content that appears after the first walk woke it. Each app
  that was just woken and shows web content is rescanned until its web hints
  reach a fixpoint (some appeared and the next scan found the same); rescans
  also stop once a label is being typed, or when the time is up."
  (set state.rescans (+ state.rescans 1))
  (let [(candidates info) (collect-candidates)
        previous state.web-hints]
    (each [pid asked? (pairs info.fresh)]
      (tset state.fresh pid asked?))
    (set state.web-hints info.web-hints)
    (when (> (length candidates) (length state.hints))
      (label! candidates)
      (set state.hints candidates)
      (render))
    (when (or (settled? state.fresh info previous)
              (>= state.rescans rescan-limit))
      (: state.rescanner :stop))))

(fn show-hints []
  "Label every clickable element on screen and wait for a label to be typed."
  (hide-hints)
  (let [(candidates info) (collect-candidates)]
    (if (= (length candidates) 0)
        (minibuffer.message "No clickable elements found")
        (do
          (label! candidates)
          (set state {:hints candidates
                      :typed ""
                      :canvases (make-canvases)
                      :hud-screen (: (hs.screen.mainScreen) :id)
                      :tap (hs.eventtap.new [types.keyDown] on-key)})
          (render)
          (: state.tap :start)
          (when (next info.fresh)
            (set state.rescans 0)
            (set state.fresh info.fresh)
            (set state.web-hints info.web-hints)
            (set state.rescanner
                 (hs.timer.doEvery rescan-interval
                                   #(when (and state (= state.typed "")
                                               (= (hint-set state.action)
                                                  :click))
                                      (rescan)))))))))


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

;; Labels for scroll areas leave out the keys scroll mode uses.
(local scroll-label-chars "asfewrcmnpioty")
(local min-area {:w 80 :h 60})

(fn big-enough? [frame]
  (and frame (>= frame.w min-area.w) (>= frame.h min-area.h)))

(local web-panel-subroles {:AXLandmarkMain true
                           :AXLandmarkNavigation true
                           :AXLandmarkComplementary true})

(fn scroller-class? [class]
  (let [class (class:lower)]
    (or (class:match "^overflow%-auto$") (class:match "^overflow%-scroll$")
        (class:match "^overflow%-[xy]%-auto$")
        (class:match "^overflow%-[xy]%-scroll$")
        (class:match "scrollable") (class:match "scroller")
        (class:match "scroll%-container") (class:match "scroll%-area"))))

(fn scroll-area? [element role subrole _ in-web?]
  "Scroll areas, and in web content the panels that may scroll on their own:
  browsers expose an element with CSS overflow scrolling as a plain group, so
  landmarks and elements whose class names say they scroll stand in for them."
  (or (= role :AXScrollArea)
      (and in-web? (= role :AXGroup)
           (or (. web-panel-subroles subrole)
               (any? scroller-class? (or (attr element :AXDOMClassList) []))))))

(fn distinct-areas [areas]
  "Drop web panels that scroll with their page, being taller than or not
  fully inside it, and landmarks too short to be a panel. Of nested areas,
  drop the outer if its center, where it is scrolled, lies in the inner."
  (let [pages (icollect [_ a (ipairs areas)]
                (when (= a.role :AXScrollArea) (attr a.element :AXFrame)))
        fits? (fn [a]
                (or (= a.role :AXScrollArea)
                    (let [frame (attr a.element :AXFrame)
                          slack {:x (+ frame.x 1) :y (+ frame.y 1)
                                 :w (- frame.w 2) :h (- frame.h 2)}]
                      (any? #(and (contains? $ slack)
                                  (or (not (. web-panel-subroles a.subrole))
                                      (>= frame.h (* 0.4 $.h))))
                            pages))))
        kept (icollect [_ a (ipairs areas)] (when (fits? a) a))]
    ;; Collected in tree order, an inner area comes after its outer one.
    (icollect [i a (ipairs kept)]
      (when (not (any? (fn [[j b]]
                         (and (not= i j) (contains? a.frame b.frame)
                              (or (< i j) (not (contains? b.frame a.frame)))
                              (inside? b.frame (center a.frame))))
                       (icollect [j b (ipairs kept)] [j b])))
        a))))

(fn scroll-areas []
  "The visible parts of every scroll area on screen, in panels and in all
  windows, including web panels that scroll on their own: the focused
  window's first, then front to back, each window's largest first. A window
  exposing none, e.g. some Electron apps, is scrolled as a whole."
  (let [budget {:visited 0 :deadline (+ (now) time-budget-ns)}
        clip (screen-clip)
        (panels floating) (panel-roots clip)
        focused (hs.window.focusedWindow)
        focused-id (and focused (: focused :id))
        roots (icollect [_ p (ipairs panels)] p)
        groups []]
    (each [_ root (ipairs (window-roots floating))]
      (table.insert roots root))
    (each [_ root (ipairs roots)]
      (let [acc []]
        (collect-tree root.element root acc budget scroll-area?)
        (let [areas (icollect [_ a (ipairs (distinct-areas acc))]
                      (when (big-enough? a.frame)
                        (doto a (tset :window root.window))))
              whole (when (and root.window (= (length areas) 0))
                      (uncovered root.clip root.occluders))]
          (table.sort areas #(> (* $1.frame.w $1.frame.h)
                                (* $2.frame.w $2.frame.h)))
          (when (big-enough? whole)
            (table.insert areas {:frame whole :window root.window}))
          (if (and focused-id root.window (= (: root.window :id) focused-id))
              (table.insert groups 1 areas)
              (table.insert groups areas)))))
    (let [result []]
      (each [_ areas (ipairs groups)]
        (each [_ a (ipairs areas)]
          (table.insert result a)))
      (each [i label (ipairs (make-labels (length result) scroll-label-chars))]
        (tset result i :label label))
      result)))

(fn render-scroll []
  (each [_ {: canvas : frame : screen} (ipairs state.canvases)]
    (let [elements (if (and hud-shown? (= (: screen :id) state.hud-screen))
                       (hud-elements frame (which-key.bottom-frame screen) "Scroll"
                                     hud-palette.key (scroll-bindings))
                       [])]
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
            (when (and (> (length state.areas) 1)
                       (= (area.label:sub 1 (length state.typed)) state.typed))
              (let [w (+ (* (length area.label) char-size.w) (* 2 pad-x))
                    h (+ char-size.h (* 2 pad-y))
                    x (+ local-frame.x 8)
                    y (+ local-frame.y 8)]
                (table.insert elements
                              {:type :rectangle
                               :frame {: x : y : w : h}
                               :roundedRectRadii {:xRadius 3 :yRadius 3}
                               :fillColor {:hex palette.bg}
                               :strokeColor {:hex palette.border :alpha 0.6}
                               :strokeWidth 1})
                (table.insert elements
                              {:type :text
                               :frame {: x :y (+ y pad-y) : w :h (- h pad-y)}
                               :text (label-text area.label state.typed)}))))))
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
    (set state.typed "")
    (render-scroll)))

(fn type-area-char [char]
  "Narrow the area labels by CHAR; a complete label selects its area."
  (let [typed (.. state.typed char)
        matches (icollect [i area (ipairs state.areas)]
                  (when (= (area.label:sub 1 (length typed)) typed) i))]
    (if (= (length matches) 0) (set state.typed "")
        (and (= (length matches) 1)
             (= (. state.areas (. matches 1) :label) typed))
        (select-area (. matches 1))
        (set state.typed typed))
    (render-scroll)))

(fn on-scroll-key-down [key flags pending-g?]
  (let [direction (. scroll-directions key)
        half-page (math.floor (/ (. state.areas state.index :frame :h) 2))]
    (if (or (= key :escape) (= key :q) (and flags.ctrl (= key :g)))
        (do (hide-hints) true)
        ;; Cmd keys leave and go through, e.g. Cmd-Shift-SPC back to hints.
        flags.cmd (do (hide-hints) false)
        direction (do (start-scrolling key direction flags.shift) true)
        (= key :d) (do (scroll-by (- half-page)) true)
        (= key :u) (do (scroll-by half-page) true)
        (and (= key :g) flags.shift) (do (scroll-by -1e6) true)
        (= key :g) (do
                     (if pending-g?
                         (scroll-by 1e6)
                         (set state.pending-g true))
                     true)
        (and (= key :tab) (not (or flags.ctrl flags.alt)))
        (do (show-hints) true)
        (and (= key "/") flags.shift (not (or flags.ctrl flags.alt)))
        (do (toggle-hud) (render-scroll) true)
        (and key (= (length key) 1) (not flags.shift)
             (scroll-label-chars:find key 1 true))
        (do (type-area-char key) true)
        (= key :delete)
        (do (set state.typed (state.typed:sub 1 -2)) (render-scroll) true)
        (do (hide-hints) false))))

(fn on-scroll-key [event]
  "h/j/k/l scroll while held, d/u by half a page, gg/G to the top/bottom,
  an area's label picks another area, TAB goes back to hints, ESC, C-g or q
  leaves. Other keys leave scroll mode and go through."
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

(set scroll-mode
     (fn []
       "Scroll any scroll area on screen from the keyboard, starting with the
       focused window's largest."
       (hide-hints)
       (let [areas (scroll-areas)]
         (if (= (length areas) 0)
             (minibuffer.message "Nothing to scroll")
             (do
               (set state {:areas areas
                           :index 1
                           :typed ""
                           :hud-screen (: (hs.screen.mainScreen) :id)
                           :canvases (make-canvases)
                           :tap (hs.eventtap.new [types.keyDown types.keyUp]
                                                 on-scroll-key)})
               (render-scroll)
               (: state.tap :start))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Caret mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A cursor on a web page's text, Vimium style, kept as a character index into
;; the page's text: the web area's text markers are made from it whenever
;; needed, which is exact where markers from elements may not be. Chromium
;; numbers characters from the start of the element they are in instead, so
;; there the markers are stepped to one at a time from where the cursor
;; starts, numbered from it, and kept: a tape that grows either way as the
;; cursor goes.

(fn define-caret-mode []
  "Caret mode's definitions, in a function of their own: Lua allows a
  function at most 200 locals, and the module's main chunk has most of
  them."
  (local caret-scan-limit 400)
  (local caret-follow-interval 0.2)
  (local tape-limit 5000)

  (fn tape-extend [delta]
    "Step the tape a character further, forward for DELTA 1, back for -1;
    false at the page's end or start, where it is marked."
    (if (if (> delta 0) (>= state.hi state.text-end) (<= state.lo state.text-start))
        false
        (let [web state.web
              edge (if (> delta 0) state.hi state.lo)
              marker (. state.markers edge)
              next (param web (if (> delta 0) :AXNextTextMarkerForTextMarker
                                  :AXPreviousTextMarkerForTextMarker)
                          marker)
              ;; Past either end, Chromium gives a marker with no text.
              text (and next (not= next marker)
                        (param web :AXStringForTextMarkerRange
                               (if (> delta 0)
                                   (hs.axuielement.axtextmarker.newRange marker next)
                                   (hs.axuielement.axtextmarker.newRange next marker))))
              i (+ edge delta)]
          (if text
              (do
                (tset state.markers i next)
                (tset state.texts (math.min edge i) text)
                (if (> delta 0) (set state.hi i) (set state.lo i))
                true)
              (do
                (if (> delta 0) (set state.text-end edge) (set state.text-start edge))
                false)))))

  (fn tape-marker [i]
    "The marker before character I of the tape, stepping to it if need be."
    (when (<= (- state.lo tape-limit) i (+ state.hi tape-limit))
      (while (and (> i state.hi) (tape-extend 1)))
      (while (and (< i state.lo) (tape-extend -1)))
      (. state.markers i)))

  (fn tape-index [marker]
    "Where MARKER is on the tape, stepping to it if need be: the first of the
    tape's markers at or after it."
    (let [before? state.before?]
      (var steps 0)
      (while (and (not (before? marker (. state.markers state.hi)))
                  (< steps tape-limit) (tape-extend 1))
        (set steps (+ steps 1)))
      (while (and (not (before? (. state.markers state.lo) marker))
                  (< steps tape-limit) (tape-extend -1))
        (set steps (+ steps 1)))
      (when (and (before? marker (. state.markers state.hi))
                 (before? (. state.markers state.lo) marker))
        (var lo state.lo)
        (var hi state.hi)
        (while (< lo hi)
          (let [mid (math.floor (/ (+ lo hi) 2))]
            (if (before? marker (. state.markers mid))
                (set hi mid)
                (set lo (+ mid 1)))))
        ;; A line's boundary orders after the stepped marker at the same
        ;; place, which has the other affinity: no text between them.
        (let [previous (. state.markers (- lo 1))]
          (if (and previous (not (before? (. state.markers lo) marker))
                   (= "" (param state.web :AXStringForTextMarkerRange
                                (hs.axuielement.axtextmarker.newRange previous
                                                                      marker))))
              (- lo 1)
              lo)))))

  (fn reset-tape [marker]
    "Start the tape afresh at MARKER, numbered 0, with the cursor on it."
    (set state.markers {0 marker})
    (set state.texts {})
    (set state.lo 0)
    (set state.hi 0)
    (set state.text-start (- math.huge))
    (set state.text-end math.huge)
    (set state.index 0))

  (fn live? [marker]
    (let [next (and marker (param state.web :AXNextTextMarkerForTextMarker marker))]
      (and next
           (param state.web :AXStringForTextMarkerRange
                  (hs.axuielement.axtextmarker.newRange marker next))
           true)))

  (fn text-marker [marker]
    "MARKER as one in the text: an element's, such as a heading's, gives its
    bounds, not its characters', as do those stepped to from it; a step away
    and back gives the text's."
    (let [web state.web
          previous (param web :AXPreviousTextMarkerForTextMarker marker)]
      (or (and previous (param web :AXNextTextMarkerForTextMarker previous))
          marker)))

  (fn relocate [point]
    "The marker before the character at POINT on screen, looked for in the
    element there."
    (let [web state.web
          app (hs.axuielement.applicationElementForPID (: web :pid))
          (ok element) (pcall #(: app :elementAtPosition point))
          range (and ok element (param web :AXTextMarkerRangeForUIElement element))]
      (when range
        (var marker (text-marker (: range :startMarker)))
        (var found nil)
        (for [_ 1 tape-limit &until (or found (not marker))]
          (let [next (param web :AXNextTextMarkerForTextMarker marker)
                bounds (and next (char-bounds web marker next))]
            (if (and bounds (> bounds.w 0) (inside? bounds point))
                (set found marker)
                (set marker (when (and next (state.before? next (: range :endMarker)))
                              next)))))
        found)))

  (fn sync-tape []
    "Number Chromium's text afresh from the cursor, as markers can go stale
    when the page redraws what they are in, a code block coming into view,
    say. If the cursor's has, it is found again where it last showed; the
    other end of the selection is found on the new tape, or where it showed."
    (when (not (live? state.cursor))
      (set state.cursor (or (and state.cursor-point (relocate state.cursor-point))
                            state.cursor)))
    (reset-tape state.cursor)
    (when state.visual
      (when (not (live? state.anchor-marker))
        (set state.anchor-marker (and state.anchor-point
                                      (relocate state.anchor-point))))
      (set state.anchor (or (and state.anchor-marker (tape-index state.anchor-marker))
                            0))))

  (fn caret-marker [i]
    (if state.tape?
        (tape-marker i)
        (param state.web :AXTextMarkerForIndex i)))

  (fn caret-index [marker]
    (if state.tape?
        (tape-index marker)
        (param state.web :AXIndexForTextMarker marker)))

  (fn index-range [web from to]
    "The text marker range from index FROM to TO (exclusive) of WEB."
    (let [a (caret-marker from)
          b (caret-marker to)]
      (when (and a b)
        (hs.axuielement.axtextmarker.newRange a b))))

  (fn caret-text [web i]
    "The text at index I of WEB: a character, mostly, but empty where a line
    wraps or a block ends, and led by the bullet at a list item's start."
    (if state.tape?
        (when (and (tape-marker i) (tape-marker (+ i 1)))
          (. state.texts i))
        (let [range (index-range web i (+ i 1))]
          (when range
            (param web :AXStringForTextMarkerRange range)))))

  (fn caret-char [web i]
    "The text of the character at index I of WEB and its bounds on screen."
    (let [range (index-range web i (+ i 1))]
      (when range
        (values (caret-text web i)
                (if state.tape?
                    (char-bounds web (: range :startMarker) (: range :endMarker))
                    (param web :AXBoundsForTextMarkerRange range))))))

  (fn caret-stop [web i]
    "The bounds of the character at I if the cursor can rest there: one that
    shows, not a line break, the empty boundary between blocks or an object,
    such as an image, which Chromium shows as U+FFFC."
    (let [(text bounds) (caret-char web i)]
      (when (and text (not= text "") (not (text:find "^[\r\n]"))
                 (not (text:find "\239\191\188"))
                 bounds (> bounds.w 0) (> bounds.h 0))
        bounds)))

  (fn caret-step [web i delta]
    "The nearest index past I, in the direction of DELTA, the cursor can rest
    on."
    (let [limit state.text-end]
      (var found nil)
      (var j (+ i delta))
      (for [_ 1 caret-scan-limit &until (or found (< j state.text-start) (>= j limit))]
        (if (caret-stop web j)
            (set found j)
            (set j (+ j delta))))
      found))

  (fn page-unit [web i unit]
    (let [marker (caret-marker i)
          range (and marker (param web (.. :AX unit :TextMarkerRangeForTextMarker)
                                   marker))
          from (and range (caret-index (: range :startMarker)))
          to (and range (caret-index (: range :endMarker)))]
      (when (and from to)
        (values from to))))

  (fn caret-line [web i]
    (page-unit web i :Line))

  (fn wrapped? [web i unit]
    "Whether the page splits a UNIT at I only where Chromium does, though
    the text goes on: where a line wraps or an inline element, such as a
    code span, starts or ends; and for a sentence, not after one's end."
    (let [a (caret-step web i -1)
          b (if (caret-stop web i) i (caret-step web i 1))
          a-bounds (and a (caret-stop web a))
          b-bounds (and b (caret-stop web b))]
      (when (and a-bounds b-bounds (< (- b a) 8))
        (let [gap (fcollect [j (+ a 1) (- b 1)] (caret-text web j))
              gap-text (table.concat gap)
              (_ line-to) (caret-line web a)]
          (and (= (length gap) (- b a 1))
               (not (gap-text:find "[\r\n]"))
               (not (gap-text:find "\239\191\188"))
               (if (< (math.abs (- (. (center a-bounds) :y)
                                   (. (center b-bounds) :y)))
                      (/ (math.min a-bounds.h b-bounds.h) 2))
                   true
                   (and (not (accumulate [empty false _ text (ipairs gap)]
                               (or empty (= text ""))))
                        line-to (< a line-to) (<= line-to b)))
               (or (= unit :Paragraph)
                   (not (: (table.concat (fcollect [j (- b 8) (- b 1)]
                                           (or (caret-text web j) "")))
                           :find "[.!?]%W*$"))))))))

  (fn caret-unit [web i unit]
    "The start and end (exclusive) indices of the UNIT holding I, as the page
    splits the text: :Line (on screen), :Sentence or :Paragraph (a block).
    Chromium splits the last two where lines wrap and inline elements start
    or end too; they are joined."
    (var (from to) (page-unit web i unit))
    (when (and state.tape? from (not= unit :Line))
      ;; At a boundary, the unit given can be either side's.
      (let [later #(let [(_ to) (page-unit web $1 unit)] (and to (> to $2) to))
            earlier #(let [from (page-unit web $1 unit)] (and from (< from $2) from))]
        (var done false)
        (for [_ 1 max-lines &until (or done (not (wrapped? web to unit)))]
          (let [next-to (or (later to to) (later (+ to 1) to))]
            (if next-to (set to next-to) (set done true))))
        (set done false)
        (for [_ 1 max-lines &until (or done (not (wrapped? web from unit)))]
          (let [previous-from (or (earlier (- from 1) from)
                                  (earlier (- from 2) from))]
            (if previous-from (set from previous-from) (set done true))))))
    (when (and from to)
      (values from to)))

  (fn caret-first [web from to]
    (let [i (caret-step web (- from 1) 1)]
      (when (and i (< i to)) i)))

  (fn caret-last [web from to]
    (let [i (caret-step web to -1)]
      (when (and i (>= i from)) i)))

  (fn caret-at-x [web from to x]
    "The character of the line from FROM to TO (exclusive) under X, or the
    nearest one, found by bisecting the line's characters' left edges; only
    those the cursor can rest on, as the others' bounds can be anywhere."
    (let [first (caret-first web from to)
          last (caret-last web from to)
          left #(let [b (caret-stop web $)] (if b b.x math.huge))]
      (when (and first last)
        (var lo first)
        (var hi last)
        (while (< lo hi)
          (let [mid (math.ceil (/ (+ lo hi) 2))
                stop (caret-step web (- mid 1) 1)]
            (if (and stop (<= stop hi) (< (left stop) x))
                (set lo stop)
                (set hi (- mid 1)))))
        lo)))

  (fn caret-from [web i]
    "I if the cursor can rest there, or the nearest index after it that can."
    (if (caret-stop web i) i (caret-step web i 1)))

  (fn blank-codepoint? [c]
    (or (<= c 32) (= c 0xA0) (= c 0x1680) (<= 0x2000 c 0x200B) (= c 0x2028)
        (= c 0x2029) (= c 0x202F) (= c 0x205F) (= c 0x3000) (= c 0xFEFF)
        (= c 0xFFFC)))

  (fn punctuation-codepoint? [c]
    (or (< c 0xC0) (<= 0x2010 c 0x2BFF) (<= 0x3001 c 0x303F)
        (<= 0xFE30 c 0xFE4F) (<= 0xFF00 c 0xFF0F) (<= 0xFF1A c 0xFF20)
        (<= 0xFF3B c 0xFF40) (<= 0xFF5B c 0xFF65)))

  (fn char-class [text big?]
    "The class, as Vim's word motions see it, of the character TEXT ends with:
    :blank, or with BIG? :word, or else keyword characters, apart by script
    as in Vim, or :punct."
    (let [c (when (and text (not= text ""))
              (utf8.codepoint text (utf8.offset text -1)))]
      (if (or (not c) (blank-codepoint? c)) :blank
          big? :word
          (< c 0x80) (if (: (string.char c) :find "[%w_]") :word :punct)
          (punctuation-codepoint? c) :punct
          (<= 0x3040 c 0x309F) :hiragana
          (<= 0x30A0 c 0x30FF) :katakana
          (or (<= 0x3400 c 0x4DBF) (<= 0x4E00 c 0x9FFF) (<= 0xF900 c 0xFAFF)) :han
          (<= 0xAC00 c 0xD7AF) :hangul
          (<= 0x1F000 c 0x1FAFF) :emoji
          :word)))

  (fn word-motion [web i kind big?]
    "Where Vim's word motion KIND, :w, :b, :e or :ge, takes the cursor from I;
    with BIG?, W, B, E or gE, by words between blanks. Empty text, where a
    line wraps or a block ends, is blank."
    (let [memo {}
          class (fn [j]
                  (if (or (< j state.text-start) (>= j state.text-end)) :edge
                      (do
                        (when (= nil (. memo j))
                          (tset memo j (char-class (caret-text web j) big?)))
                        (. memo j))))
          delta (if (or (= kind :w) (= kind :e)) 1 -1)]
      (var j i)
      (var steps 0)
      (fn skip [c]
        (while (and (= (class j) c) (< steps caret-scan-limit))
          (set j (+ j delta))
          (set steps (+ steps 1))))
      (fn extend [c]
        "Move to the far end, in the direction of the motion, of the run of C."
        (while (and (= (class (+ j delta)) c) (< steps caret-scan-limit))
          (set j (+ j delta))
          (set steps (+ steps 1))))
      (case kind
        :w (let [c (class j)]
             (when (not= c :blank) (skip c))
             (skip :blank))
        :ge (let [c (class j)]
              (when (not= c :blank) (skip c))
              (skip :blank))
        _ (do
            (set j (+ j delta))
            (skip :blank)
            (extend (class j))))
      (when (and (not= (class j) :edge) (not= (class j) :blank)
                 (< steps caret-scan-limit))
        (if (caret-stop web j) j (caret-step web j delta)))))

  (fn unit-motion [web i unit forward?]
    "Vim's ) and ( for UNIT :Sentence, to the next sentence's start or this
    one's, or the one before if at it; } and { for :Paragraph, to this block's
    end or start, or the next one's or the one before's if at it."
    (let [(from to) (caret-unit web i unit)]
      (when from
        (if (and forward? (= unit :Sentence))
            (let [target (caret-from web to)]
              ;; At a sentence's start, WebKit gives the one before it.
              (if (and target (> target i)) target
                  (let [next (caret-step web i 1)
                        (_ next-to) (when next (caret-unit web next unit))]
                    (when next-to (caret-from web next-to)))))
            forward?
            (let [last (caret-step web to -1)]
              (if (and last (> last i)) last
                  (let [next (caret-step web i 1)
                        (_ next-to) (when next (caret-unit web next unit))]
                    (when next-to (caret-step web next-to -1)))))
            (let [first (caret-from web from)]
              (if (and first (< first i)) first
                  (let [prev (caret-step web i -1)
                        (prev-from) (when prev (caret-unit web prev unit))]
                    (when prev-from (caret-from web prev-from)))))))))

  (fn heading-markers [web]
    "The text markers where the page's headings start, in order; but not
    those hidden for screen readers only, a pixel or two wide, such as
    ChatGPT's \"You said:\". (Those off screen can be a pixel high.)"
    (icollect [_ heading (ipairs (or (param web :AXUIElementsForSearchPredicate
                                            {:AXSearchKey :AXHeadingSearchKey
                                             :AXDirection :AXDirectionNext
                                             :AXResultsLimit -1
                                             :AXVisibleOnly false})
                                     []))]
      (let [size (attr heading :AXSize)
            range (when (or (not size) (> size.w 2))
                    (param web :AXTextMarkerRangeForUIElement heading))]
        (when range
          (: range :startMarker)))))

  (fn tape-section-motion [web forward? land]
    "Section motions on Chromium, where headings can be far: one is found by
    comparing its marker with the cursor's, and the tape numbered afresh
    from it to LAND, as section-motion does, on the text around it."
    (let [cursor state.cursor
          before? state.before?
          markers (heading-markers web)
          order (if forward? markers (fcollect [k (length markers) 1 -1] (. markers k)))
          marker-at #(and $ (caret-marker $))
          landed (fn [start]
                   (reset-tape (text-marker start))
                   (marker-at (land 0)))
          found (accumulate [found nil _ start (ipairs order) &until found]
                  (when (if forward? (not (before? start cursor)) (before? start cursor))
                    (let [m (landed start)]
                      (when (and m (if forward?
                                       (not (before? m cursor))
                                       (not (before? cursor m))))
                        m))))
          ;; Text shown to screen readers only is a pixel or two in size.
          hidden? #(let [leaf (param web :AXUIElementForTextMarker (caret-marker $))
                         size (and leaf (attr leaf :AXSize))]
                     (and size (<= size.w 2)))
          ;; A chat's composer comes after the conversation: its end is
          ;; before the next text field's start.
          field (when forward?
                  (. (or (param web :AXUIElementsForSearchPredicate
                                {:AXSearchKey :AXTextFieldSearchKey
                                 :AXStartElement (param web :AXUIElementForTextMarker
                                                        cursor)
                                 :AXDirection :AXDirectionNext
                                 :AXResultsLimit 1
                                 :AXVisibleOnly false})
                         [])
                     1))
          field-range (and field (param web :AXTextMarkerRangeForUIElement field))
          target (or found
                     (if field-range
                         (do (reset-tape (text-marker (: field-range :startMarker)))
                             (var j (caret-step web 0 -1))
                             (for [_ 1 caret-scan-limit &until (not (and j (hidden? j)))]
                               (set j (caret-step web j -1)))
                             (marker-at j))
                         ;; Otherwise the page's end or start, as far as the
                         ;; tape goes.
                         (let [delta (if forward? 1 -1)]
                           (reset-tape cursor)
                           (for [_ 1 tape-limit &until (not (tape-extend delta))]
                             nil)
                           (marker-at (if forward?
                                          (caret-step web state.hi -1)
                                          (caret-from web state.lo))))))]
      (set state.cursor (or target cursor))
      (sync-tape)
      (when target 0)))

  (fn section-motion [web i keys]
    "Vim's ]] and [[, to the next or previous heading, and ][ and [], to the
    end of this section or the one before, a section running from a heading
    to the next; the page's end or start when there is none."
    (let [forward? (= (keys:sub 1 1) "]")
          heading? (= (keys:sub 1 1) (keys:sub 2 2))
          land #(if heading? (caret-from web $) (caret-step web $ -1))]
      (if state.tape?
          (tape-section-motion web forward? land)
          (let [starts (icollect [_ marker (ipairs (heading-markers web))]
                         (caret-index marker))
                _ (table.sort starts)
                order (if forward? starts
                          (fcollect [k (length starts) 1 -1] (. starts k)))
                found (accumulate [found nil _ start (ipairs order) &until found]
                        (let [j (land start)]
                          (when (and j (if forward? (> j i) (< j i)))
                            j)))]
            (or found
                (if forward?
                    (caret-step web state.text-end -1)
                    (caret-from web 0)))))))

  (fn caret-bounds []
    (caret-stop state.web state.index))

  (fn selection-pieces [from to]
    "The text from index FROM to TO (exclusive) a line at a time, as the
    first and last characters the cursor can rest on of each, for drawing
    the selection where the page can't show it."
    (let [web state.web
          pieces []]
      (var i (caret-from web from))
      (for [_ 1 max-lines &until (or (not i) (>= i to))]
        (let [(_ line-to) (caret-line web i)
              stop (math.min (if (and line-to (> line-to i)) line-to (+ i 1)) to)
              last (caret-last web i stop)]
          (when last
            (table.insert pieces [i last]))
          (set i (caret-from web stop))))
      pieces))

  (fn selection-rectangles [frame]
    "The selection's lines drawn in FRAME, Chromium's being drawn rather than
    shown by the page. Characters' bounds are asked for one at a time: a
    range's across elements are the whole page's."
    (icollect [_ [first last] (ipairs (or state.pieces []))]
      (let [a (caret-stop state.web first)
            b (caret-stop state.web last)]
        (when (and a b (inside? frame (center a)))
          (let [top (math.min a.y b.y)]
            {:type :rectangle :action :fill
             :frame {:x (- a.x frame.x) :y (- top frame.y)
                     :w (- (+ b.x b.w) a.x)
                     :h (- (math.max (+ a.y a.h) (+ b.y b.h)) top)}
             :fillColor {:hex palette.caret :alpha 0.4}})))))

  (fn caret-viewport []
    "The part of the page on screen: the scroll area the web area is in."
    (let [parent (attr state.web :AXParent)
          frame (if (and parent (= (attr parent :AXRole) :AXScrollArea))
                    (attr parent :AXFrame)
                    (attr state.web :AXFrame))]
      (and frame (intersection frame (screen-clip)))))

  (fn caret-title []
    (.. (case state.visual
          :char "Caret: visual"
          :line "Caret: visual line"
          _ "Caret")
        (if state.pending (.. "  " state.pending) "")))

  (fn render-caret []
    (let [bounds (caret-bounds)
          anchor (and state.visual state.tape? (caret-stop state.web state.anchor))]
      ;; Where to find them again should Chromium redraw their text.
      (when bounds
        (set state.cursor-point (center bounds)))
      (when anchor
        (set state.anchor-point (center anchor)))
      (each [_ {: canvas : frame : screen} (ipairs state.canvases)]
        (let [elements (selection-rectangles frame)]
          (when (and hud-shown? (= (: screen :id) state.hud-screen))
            (each [_ element (ipairs (hud-elements frame (which-key.bottom-frame screen)
                                                   (caret-title) palette.caret
                                                   (caret-bindings state.visual)))]
              (table.insert elements element)))
          (when (and bounds (inside? frame (center bounds)))
            (table.insert elements
                          {:type :rectangle :action :strokeAndFill
                           :frame {:x (- bounds.x frame.x) :y (- bounds.y frame.y)
                                   :w bounds.w :h bounds.h}
                           ;; Outlined in dark to show on light pages too.
                           :fillColor {:hex palette.cursor :alpha 0.75}
                           :strokeColor {:hex palette.border}
                           :strokeWidth 1.5}))
          (: canvas :replaceElements elements)))))

  (fn reveal-caret []
    "Scroll the page so that a few lines around the cursor show."
    (let [bounds (caret-bounds)
          view (caret-viewport)]
      (when (and bounds view)
        (let [margin (* 3 bounds.h)
              above (- (+ view.y margin) bounds.y)
              below (- (+ bounds.y bounds.h margin) (+ view.y view.h))
              dy (if (> above 0) above (> below 0) (- below) 0)]
          (when (not= dy 0)
            (scroll-at (center view) 0 dy))))))

  (fn caret-selection []
    "The start and end (exclusive) of the text selected from the anchor to the
    cursor, both included, or the whole lines they are on."
    (let [web state.web
          lo (math.min state.anchor state.index)
          hi (math.max state.anchor state.index)]
      (if (= state.visual :line)
          (let [(from) (caret-line web lo)
                (_ to) (caret-line web hi)]
            (values (or from lo) (or to (+ hi 1))))
          (values lo (+ hi 1)))))

  (fn select-caret-range []
    "Select the page's text from the anchor to the cursor; on Chromium, which
    ignores this, note the lines to draw instead."
    (let [(from to) (caret-selection)]
      (if state.tape?
          (set state.pieces (selection-pieces from to))
          (let [range (index-range state.web from to)]
            (when range
              (pcall #(: state.web :setAttributeValue :AXSelectedTextMarkerRange
                         range)))))))

  (fn move-caret [i ?goal-x]
    "Put the cursor on I; ?GOAL-X is the column vertical moves keep to."
    (when i
      (set state.index i)
      (set state.goal-x ?goal-x)
      (when state.tape?
        (set state.cursor (caret-marker i)))
      (when state.visual
        (select-caret-range))
      (reveal-caret)
      (render-caret)))

  (fn set-visual [kind]
    "Select by KIND, :char or :line, from the cursor on, or switch to it while
    selecting; KIND again, or nil, stops selecting."
    (if (or (not kind) (= kind state.visual))
        (do
          (set state.visual nil)
          (set state.pieces nil)
          (deselect-web state.web))
        (do
          (when (not state.visual)
            (set state.anchor state.index)
            (set state.anchor-marker state.cursor)
            ;; Focused, the page shows the selection as active.
            (pcall #(: state.web :setAttributeValue :AXFocused true)))
          (set state.visual kind)
          (select-caret-range)))
    (render-caret))

  (fn yank-selection []
    "Copy the selected text and stop selecting, the cursor at its start, as
    Vim does."
    (let [(from to) (caret-selection)
          range (index-range state.web from to)
          start (math.min state.anchor state.index)]
      (copy-text (let [text (and range
                                 (if state.tape?
                                     (chromium-string state.web (: range :startMarker)
                                                      (: range :endMarker))
                                     (param state.web :AXStringForTextMarkerRange
                                            range)))]
                   ;; Lines end with one line break, however far apart blocks are.
                   (if (and text (= state.visual :line)) (pick-values 1 (text:gsub "\n+$" "\n"))
                       text)))
      (set state.index start)
      (when state.tape?
        (set state.cursor (caret-marker start)))
      (set state.goal-x nil)
      (set-visual nil)))

  (fn swap-caret-ends []
    (let [anchor state.anchor]
      (set state.anchor state.index)
      (set state.anchor-marker state.cursor)
      (move-caret anchor)))

  (fn resync []
    (sync-tape)
    (when state.visual
      (select-caret-range)))

  (fn follow-caret []
    "Redraw the cursor, as the page can scroll by other means; on Chromium,
    first find it again if the page redrew its text."
    (when (and state.tape? (not state.lost) (not (caret-bounds)))
      (resync)
      (set state.lost (not (caret-bounds))))
    (render-caret))

  (fn caret-vertical [direction]
    "Move the cursor a line down (DIRECTION 1) or up (-1), keeping to its
    column."
    (let [web state.web
          bounds (caret-bounds)
          goal (or state.goal-x (and bounds (+ bounds.x (/ bounds.w 2))))
          (from to) (caret-line web state.index)
          next (when from
                 (if (> direction 0)
                     (caret-step web (- to 1) 1)
                     (caret-step web from -1)))
          (next-from next-to) (when next (caret-line web next))]
      (when (and goal next-from)
        (move-caret (or (caret-at-x web next-from next-to goal) next) goal))))

  (fn caret-line-end [end?]
    (let [(from to) (caret-line state.web state.index)]
      (when from
        (if end?
            (move-caret (caret-last state.web from to) math.huge)
            (move-caret (caret-first state.web from to))))))

  (local shifted-keys {"9" "(" "0" ")" "[" "{" "]" "}" "4" "$" "/" "?"})

  (fn typed-key [event]
    "What a key event types on a US layout, as Vim names keys, or nil when
    Cmd, Ctrl or Alt is held."
    (let [key (. hs.keycodes.map (: event :getKeyCode))
          flags (: event :getFlags)]
      (when (and key (not (or flags.cmd flags.ctrl flags.alt)))
        (if (not flags.shift) key
            (. shifted-keys key) (. shifted-keys key)
            (= (length key) 1) (key:upper)))))

  (local caret-motions
         {:h #(caret-step $1 $2 -1)
          :l #(caret-step $1 $2 1)
          :w #(word-motion $1 $2 :w)
          :b #(word-motion $1 $2 :b)
          :e #(word-motion $1 $2 :e)
          :ge #(word-motion $1 $2 :ge)
          :W #(word-motion $1 $2 :w true)
          :B #(word-motion $1 $2 :b true)
          :E #(word-motion $1 $2 :e true)
          :gE #(word-motion $1 $2 :ge true)
          ")" #(unit-motion $1 $2 :Sentence true)
          "(" #(unit-motion $1 $2 :Sentence false)
          "}" #(unit-motion $1 $2 :Paragraph true)
          "{" #(unit-motion $1 $2 :Paragraph false)
          "]]" #(section-motion $1 $2 "]]")
          "[[" #(section-motion $1 $2 "[[")
          "][" #(section-motion $1 $2 "][")
          "[]" #(section-motion $1 $2 "[]")})

  (local caret-prefixes {:g true "[" true "]" true})

  (fn on-caret-key [event]
    "Vim's motions move the cursor: h/l a character, j/k a line, 0/$ to the
    line's ends, w/b/e/ge and W/B/E/gE by words, )/( by sentences, }/{ by
    paragraphs and ]]/[[/][/[] by sections, from heading to heading. v
    selects from the cursor by characters and V by lines, y copies the
    selection, o goes to its other end and ESC stops selecting. ESC when not
    selecting, C-g or q leaves. Other keys, Cmd-c say, leave caret mode,
    keeping the selection, and go through; but on Chromium, where the
    selection is only drawn, Cmd-c copies it as y does."
    (let [key (. hs.keycodes.map (: event :getKeyCode))
          flags (: event :getFlags)
          typed (typed-key event)
          pending state.pending
          keys (if pending (.. pending (or typed "")) typed)
          motion (and keys (. caret-motions keys))]
      (set state.pending nil)
      (when state.tape?
        (resync)
        (set state.lost false))
      (if (and flags.ctrl (= key :g)) (do (hide-hints) true)
          motion (do (move-caret (motion state.web state.index)) true)
          ;; Any other key after a prefix only cancels it, as in Vim.
          pending (do (render-caret) true)
          (and typed (. caret-prefixes typed))
          (do (set state.pending typed) (render-caret) true)
          (= key :escape) (do (if state.visual (set-visual nil) (hide-hints)) true)
          (= typed :q) (do (hide-hints) true)
          (= typed :v) (do (set-visual :char) true)
          (= typed :V) (do (set-visual :line) true)
          (and (= typed :y) state.visual) (do (yank-selection) true)
          (and (= typed :o) state.visual) (do (swap-caret-ends) true)
          (= typed :j) (do (caret-vertical 1) true)
          (= typed :k) (do (caret-vertical -1) true)
          (= typed :0) (do (caret-line-end false) true)
          (= typed "$") (do (caret-line-end true) true)
          (= typed "?") (do (toggle-hud) (render-caret) true)
          ;; Chromium's selection is only drawn, so Cmd-c can't copy it.
          (and flags.cmd (= key :c) state.visual state.tape?)
          (do (yank-selection) (hide-hints) true)
          (do (hide-hints) false))))

  (fn [hint]
    "Put a cursor at the start of HINT, a web text line, to move it with
    the keyboard."
    (hide-hints)
    (let [web hint.web?
          tape? (and web (chromium-web? web))
          start (and web hint.start
                     (if tape? 0 (param web :AXIndexForTextMarker hint.start)))
          text-end (and web (if tape? math.huge
                                (param web :AXIndexForTextMarker
                                       (attr web :AXEndTextMarker))))]
      (if (not (and start text-end))
          (minibuffer.message "The caret only works on web pages for now")
          (do
            (set state {:web web
                        :index start
                        :text-start (if tape? (- math.huge) 0)
                        :text-end text-end
                        : tape?
                        :cursor hint.start
                        :before? (marker-order web)
                        :hud-screen (: (hs.screen.mainScreen) :id)
                        :canvases (make-canvases)
                        :tap (hs.eventtap.new [types.keyDown] on-caret-key)})
            (when tape?
              (reset-tape hint.start))
            (when (not (caret-stop web start))
              (set state.index (or (caret-step web start 1) start))
              (when tape?
                (set state.cursor (caret-marker state.index))))
            ;; The cursor follows the page as it scrolls by other means.
            (set state.follower (hs.timer.doEvery caret-follow-interval
                                                  follow-caret))
            (render-caret)
            (: state.tap :start))))))

(set caret-mode (define-caret-mode))

{: show-hints
 : hide-hints
 : scroll-mode
 : search
 : make-labels
 :current (fn [] state)}
