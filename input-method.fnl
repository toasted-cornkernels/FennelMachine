(require-macros :lib.macros)

;; SOUND_UP
;; SOUND_DOWN
;; MUTE
;; BRIGHTNESS_UP
;; BRIGHTNESS_DOWN
;; CONTRAST_UP
;; CONTRAST_DOWN
;; POWER
;; LAUNCH_PANEL
;; VIDMIRROR
;; PLAY
;; EJECT
;; NEXT
;; PREVIOUS
;; FAST
;; REWIND
;; ILLUMINATION_UP
;; ILLUMINATION_DOWN
;; ILLUMINATION_TOGGLE
;; CAPS_LOCK
;; HELP
;; NUM_LOCK

;; Mission Control ==================================
;; ==================================================

(hs.hotkey.bind [:ctrl :cmd] :h (fn []
                                  (: (hs.eventtap.event.newKeyEvent [:fn :ctrl]
                                                                    :left true)
                                     :post)
                                  (: (hs.eventtap.event.newKeyEvent [:fn :ctrl]
                                                                    :left false)
                                     :post)))

(hs.hotkey.bind [:ctrl :cmd] :j (fn []
                                  (: (hs.eventtap.event.newKeyEvent [:fn :ctrl]
                                                                    :down true)
                                     :post)
                                  (: (hs.eventtap.event.newKeyEvent [:fn :ctrl]
                                                                    :down false)
                                     :post)))

(hs.hotkey.bind [:ctrl :cmd] :k (fn []
                                  (: (hs.eventtap.event.newKeyEvent [:fn :ctrl]
                                                                    :up true)
                                     :post)
                                  (: (hs.eventtap.event.newKeyEvent [:fn :ctrl]
                                                                    :up false)
                                     :post)))

(hs.hotkey.bind [:ctrl :cmd] :l (fn []
                                  (: (hs.eventtap.event.newKeyEvent [:fn :ctrl]
                                                                    :right true)
                                     :post)
                                  (: (hs.eventtap.event.newKeyEvent [:fn :ctrl]
                                                                    :right false)
                                     :post)))

;; toggle hs.console ================================
;; ==================================================

(hs.hotkey.bind [:ctrl :cmd] "`"
                (fn []
                  (if-let [console (hs.console.hswindow)]
                          (when (= console (hs.console.hswindow))
                            (hs.closeConsole))
                          (hs.openConsole))))

;; Terminal-style backspace =========================
;; ==================================================

;; TODO: Make Emacs an exception

(hs.hotkey.bind [:ctrl] :h (fn []
                             (: (hs.eventtap.event.newKeyEvent [] :delete true)
                                :post)
                             (: (hs.eventtap.event.newKeyEvent [] :delete false)
                                :post)) nil
                (fn []
                  (: (hs.eventtap.event.newKeyEvent [] :delete true) :post)
                  (: (hs.eventtap.event.newKeyEvent [] :delete false) :post)))


(hs.hotkey.bind [:ctrl] :m (fn []
                             (: (hs.eventtap.event.newKeyEvent [] :return true)
                                :post)
                             (: (hs.eventtap.event.newKeyEvent [] :return false)
                                :post)) nil
                (fn []
                  (: (hs.eventtap.event.newKeyEvent [] :return true) :post)
                  (: (hs.eventtap.event.newKeyEvent [] :return false) :post)))

(hs.hotkey.bind [:ctrl] "[" (fn []
                              (: (hs.eventtap.event.newKeyEvent [] :escape true)
                                 :post)
                              (: (hs.eventtap.event.newKeyEvent [] :escape
                                                                false)
                                 :post)) nil
                (fn []
                  (: (hs.eventtap.event.newKeyEvent [] :escape true) :post)
                  (: (hs.eventtap.event.newKeyEvent [] :escape false) :post)))

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

;; Volume keys ======================================
;; ==================================================

(hs.hotkey.bind [:alt :shift] "["
                (fn []
                  (: (hs.eventtap.event.newSystemKeyEvent :SOUND_DOWN true)
                     :post)
                  (: (hs.eventtap.event.newSystemKeyEvent :SOUND_DOWN false)
                     :post)))

(hs.hotkey.bind [:alt :shift] "]"
                (fn []
                  (: (hs.eventtap.event.newSystemKeyEvent :SOUND_UP true) :post)
                  (: (hs.eventtap.event.newSystemKeyEvent :SOUND_UP false)
                     :post)))

(hs.hotkey.bind [:alt :shift] "\\"
                (fn []
                  (: (hs.eventtap.event.newSystemKeyEvent :MUTE true) :post)
                  (: (hs.eventtap.event.newSystemKeyEvent :MUTE false) :post)))

(hs.hotkey.bind [:alt :shift] :s (fn []
                                   (: (hs.eventtap.event.newSystemKeyEvent :PAUSE
                                                                           true)
                                      :post)
                                   (: (hs.eventtap.event.newSystemKeyEvent :PAUSE
                                                                           false)
                                      :post)))

;; Brightness =======================================
;; ==================================================

(hs.hotkey.bind [:alt :shift] "-"
                (fn []
                  (: (hs.eventtap.event.newSystemKeyEvent :BRIGHTNESS_DOWN true)
                     :post)
                  (: (hs.eventtap.event.newSystemKeyEvent :BRIGHTNESS_DOWN
                                                          false)
                     :post)))

(hs.hotkey.bind [:alt :shift] "="
                (fn []
                  (: (hs.eventtap.event.newSystemKeyEvent :BRIGHTNESS_UP true)
                     :post)
                  (: (hs.eventtap.event.newSystemKeyEvent :BRIGHTNESS_UP false)
                     :post)))

;; Keyboard Illumination ============================
;; ==================================================

;; (hs.hotkey.bind [:alt :shift] :8 nil
;;                 (fn []
;;                   (: (hs.eventtap.event.newSystemKeyEvent :ILLUMINATION_TOGGLE
;;                                                           true)
;;                      :post)
;;                   (: (hs.eventtap.event.newSystemKeyEvent :ILLUMINATION_TOGGLE
;;                                                           false)
;;                      :post)))

;; (hs.hotkey.bind [:alt :shift] :9 nil
;;                 (fn []
;;                   (: (hs.eventtap.event.newSystemKeyEvent :ILLUMINATION_DOWN
;;                                                           true)
;;                      :post)
;;                   (: (hs.eventtap.event.newSystemKeyEvent :ILLUMINATION_DOWN
;;                                                           false)
;;                      :post)))

;; (hs.hotkey.bind [:alt :shift] :0 nil
;;                 (fn []
;;                   (: (hs.eventtap.event.newSystemKeyEvent :ILLUMINATION_UP true)
;;                      :post)
;;                   (: (hs.eventtap.event.newSystemKeyEvent :ILLUMINATION_UP
;;                                                           false)
;;                      :post)))

;; Rshift+esc to tilde ==============================
;; ==================================================

;; (hs.hotkey.bind [:rightshift] :escape nil
;;                 (fn []
;;                   (: (hs.eventtap.event.newKeyEvent [:rightshift] "`" true)
;;                      :post)
;;                   (: (hs.eventtap.event.newKeyEvent [:rightshift] "`" false)
;;                      :post)))

;; Cmd+shift+m to Shift+F10 ==========================
;; ==================================================

(hs.hotkey.bind [:cmd :shift] :m nil
                (fn []
                  (: (hs.eventtap.event.newKeyEvent [:shift] :f10 true) :post)
                  (: (hs.eventtap.event.newKeyEvent [:shift] :f10 false) :post)))

