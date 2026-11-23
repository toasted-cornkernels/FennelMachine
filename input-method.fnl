;; Input Methods ====================================
;; ==================================================

(hs.hotkey.bind [:cmd :shift] :j (fn []
                                   (hs.keycodes.setMethod "ひらがな"))
                nil nil)

(hs.hotkey.bind [:cmd :shift] :k (fn []
                                   (hs.keycodes.setMethod "2-Set Korean"))
                nil nil)

(hs.hotkey.bind [:cmd :shift] :e (fn []
                                   (hs.keycodes.setLayout "U.S."))
                nil nil)
