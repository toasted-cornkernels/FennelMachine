"ChatGPT desktop app helpers for keyboard-only use"

(local log (hs.logger.new "chatgpt.fnl" "info"))

(local composer-description "Ask ChatGPT")
(local max-visited 5000)
(local max-attempts 5)
(local retry-interval 0.05)

;; Utils ============================================
;; ==================================================

(fn attr [element name]
  "Read an AX attribute, returning nil if the element has gone stale."
  (let [(ok value) (pcall #(: element :attributeValue name))]
    (when ok value)))

(fn visible-in? [element window-frame]
  "Chromium keeps scrolled-out or hidden nodes in the tree with 1px frames."
  (let [frame (attr element :AXFrame)]
    (and frame (> frame.w 1) (> frame.h 1)
         (< frame.x (+ window-frame.x window-frame.w))
         (> (+ frame.x frame.w) window-frame.x)
         (< frame.y (+ window-frame.y window-frame.h))
         (> (+ frame.y frame.h) window-frame.y))))

(fn composer? [element window-frame]
  (and (= (attr element :AXRole) :AXTextArea)
       (= (attr element :AXDescription) composer-description)
       (visible-in? element window-frame)))

(fn find-composer [window]
  "Search WINDOW's accessibility tree for the visible message composer.
  Children are visited last-to-first: the composer sits at the bottom of the
  DOM, so this usually touches a few dozen nodes instead of the whole tree."
  (let [window-frame (: window :frame)]
    (var visited 0)
    (fn search [element]
      (set visited (+ visited 1))
      (if (> visited max-visited) nil
          (composer? element window-frame) element
          (let [children (or (attr element :AXChildren) [])]
            (var found nil)
            (for [i (length children) 1 -1 &until found]
              (set found (search (. children i))))
            found)))
    (search (hs.axuielement.windowElement window))))

(fn focused? [app element]
  (= (attr (hs.axuielement.applicationElement app) :AXFocusedUIElement)
     element))

;; Focus composer ===================================
;; ==================================================

(fn focus-composer []
  "Move keyboard focus to the message composer of the open conversation.
  Setting AXFocused makes Chromium call focus() on the composer's DOM node, so
  the editor keeps its draft and restores its previous caret position. Focus
  is applied asynchronously by the renderer, hence the verify-and-retry loop,
  which also covers the composer briefly missing while a view is loading."
  (let [app (hs.application.frontmostApplication)]
    (fn attempt [n]
      (when (: app :isFrontmost)
        (let [window (: app :focusedWindow)
              composer (and window (find-composer window))]
          (if (and composer (focused? app composer)) nil
              (> n max-attempts) (do
                                   (log.w "Could not focus the composer")
                                   (alert "ChatGPT composer not found"))
              (do
                (when composer
                  (pcall #(: composer :setAttributeValue :AXFocused true)))
                (hs.timer.doAfter retry-interval #(attempt (+ n 1))))))))
    (when (and app (= (: app :name) :ChatGPT))
      (attempt 1))))

{: focus-composer}
