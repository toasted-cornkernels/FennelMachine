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

;; TODO Take apart this monolith
(fn right-cmd-input-switch-emacs-sensitive [e]
  (let [flags (e:getFlags)
        current-app-name (: (hs.application.frontmostApplication) :name)]
    (when (and flags.cmd (not (or flags.alt flags.shift flags.ctrl flags.fn)))
      (let [key-code (e:getKeyCode)]
        (if (= key-code 54)
            (match (hs.keycodes.currentLayout)
              :U.S. (if (= current-app-name :Emacs)
                        (hs.eventtap.keyStroke {} :f6)
                        (hs.keycodes.setMethod "2-Set Korean"))
              "2-Set Korean" (if (= current-app-name :Emacs)
                                 (hs.eventtap.keyStroke {} :f6)
                                 (hs.keycodes.setLayout :U.S.))))))))

(global layout-watcher (: (hs.eventtap.new [hs.eventtap.event.types.flagsChanged]
                                           right-cmd-input-switch-emacs-sensitive)
                          :start))

;; Mission Control ==================================
;; ==================================================

(hs.hotkey.bind [:ctrl :cmd] :h nil
                (fn []
                  (: (hs.eventtap.event.newKeyEvent [:fn :ctrl] :left true)
                     :post)
                  (: (hs.eventtap.event.newKeyEvent [:fn :ctrl] :left false)
                     :post)))

(hs.hotkey.bind [:ctrl :cmd] :j nil
                (fn []
                  (: (hs.eventtap.event.newKeyEvent [:fn :ctrl] :down true)
                     :post)
                  (: (hs.eventtap.event.newKeyEvent [:fn :ctrl] :down false)
                     :post)))

(hs.hotkey.bind [:ctrl :cmd] :k nil
                (fn []
                  (: (hs.eventtap.event.newKeyEvent [:fn :ctrl] :up true) :post)
                  (: (hs.eventtap.event.newKeyEvent [:fn :ctrl] :up false)
                     :post)))

(hs.hotkey.bind [:ctrl :cmd] :l nil
                (fn []
                  (: (hs.eventtap.event.newKeyEvent [:fn :ctrl] :right true)
                     :post)
                  (: (hs.eventtap.event.newKeyEvent [:fn :ctrl] :right false)
                     :post)))

;; toggle hs.console ================================
;; ==================================================

(hs.hotkey.bind [:ctrl :cmd] "`" nil
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

(hs.hotkey.bind [:ctrl] :j (fn []
                             (: (hs.eventtap.event.newKeyEvent [] :return true)
                                :post)
                             (: (hs.eventtap.event.newKeyEvent [] :return false)
                                :post)) nil
                (fn []
                  (: (hs.eventtap.event.newKeyEvent [] :return true) :post)
                  (: (hs.eventtap.event.newKeyEvent [] :return false) :post)))

(hs.hotkey.bind [:ctrl] :m (fn []
                             (: (hs.eventtap.event.newKeyEvent [] :return true)
                                :post)
                             (: (hs.eventtap.event.newKeyEvent [] :return false)
                                :post)) nil
                (fn []
                  (: (hs.eventtap.event.newKeyEvent [] :return true) :post)
                  (: (hs.eventtap.event.newKeyEvent [] :return false) :post)))

;; (hs.hotkey.bind [:ctrl] :u (fn []
;;                              (: (hs.eventtap.event.newKeyEvent [:cmd] :delete true)
;;                                 :post)
;;                              (: (hs.eventtap.event.newKeyEvent [:cmd] :delete false)
;;                                 :post)) nil
;;                 (fn []
;;                   (: (hs.eventtap.event.newKeyEvent [:cmd] :delete true) :post)
;;                   (: (hs.eventtap.event.newKeyEvent [:cmd] :delete false) :post)))

;; Arrow keys =======================================
;; ==================================================

(hs.hotkey.bind [:cmd :shift] :h (fn []
                                   (: (hs.eventtap.event.newKeyEvent [:fn]
                                                                     :left true)
                                      :post)
                                   (: (hs.eventtap.event.newKeyEvent [:fn]
                                                                     :left false)
                                      :post))
                nil
                (fn []
                  (: (hs.eventtap.event.newKeyEvent [:fn] :left true) :post)
                  (: (hs.eventtap.event.newKeyEvent [:fn] :left false) :post)))

(hs.hotkey.bind [:cmd :shift] :j (fn []
                                   (: (hs.eventtap.event.newKeyEvent [:fn]
                                                                     :down true)
                                      :post)
                                   (: (hs.eventtap.event.newKeyEvent [:fn]
                                                                     :down false)
                                      :post))
                nil
                (fn []
                  (: (hs.eventtap.event.newKeyEvent [:fn] :down true) :post)
                  (: (hs.eventtap.event.newKeyEvent [:fn] :down false) :post)))

(hs.hotkey.bind [:cmd :shift] :k (fn []
                                   (: (hs.eventtap.event.newKeyEvent [:fn] :up
                                                                     true)
                                      :post)
                                   (: (hs.eventtap.event.newKeyEvent [:fn] :up
                                                                     false)
                                      :post))
                nil
                (fn []
                  (: (hs.eventtap.event.newKeyEvent [:fn] :up true) :post)
                  (: (hs.eventtap.event.newKeyEvent [:fn] :up false) :post)))

(hs.hotkey.bind [:cmd :shift] :l (fn []
                                   (: (hs.eventtap.event.newKeyEvent [:fn]
                                                                     :right true)
                                      :post)
                                   (: (hs.eventtap.event.newKeyEvent [:fn]
                                                                     :right
                                                                     false)
                                      :post))
                nil (fn []
                     (: (hs.eventtap.event.newKeyEvent [:fn] :right true) :post)
                     (: (hs.eventtap.event.newKeyEvent [:fn] :right false)
                        :post)))

;; Mouse keys: normal speed =========================
;; ==================================================

:TODO

;; Mouse keys: fast speed ===========================
;; ==================================================

:TODO

;; Volume keys ======================================
;; ==================================================

(hs.hotkey.bind [:alt :shift] "[" nil
                (fn []
                  (: (hs.eventtap.event.newSystemKeyEvent :SOUND_DOWN true)
                     :post)
                  (: (hs.eventtap.event.newSystemKeyEvent :SOUND_DOWN false)
                     :post)))

(hs.hotkey.bind [:alt :shift] "]" nil
                (fn []
                  (: (hs.eventtap.event.newSystemKeyEvent :SOUND_UP true) :post)
                  (: (hs.eventtap.event.newSystemKeyEvent :SOUND_UP false)
                     :post)))

(hs.hotkey.bind [:alt :shift] "\\" nil
                (fn []
                  (: (hs.eventtap.event.newSystemKeyEvent :MUTE true) :post)
                  (: (hs.eventtap.event.newSystemKeyEvent :MUTE false) :post)))

(hs.hotkey.bind [:alt :shift] :s nil
                (fn []
                  (: (hs.eventtap.event.newSystemKeyEvent :PAUSE true) :post)
                  (: (hs.eventtap.event.newSystemKeyEvent :PAUSE false) :post)))

(hs.hotkey.bind [:alt :shift] :p nil
                (fn []
                  (: (hs.eventtap.event.newSystemKeyEvent :PREVIOUS true) :post)
                  (: (hs.eventtap.event.newSystemKeyEvent :PREVIOUS false)
                     :post)))

(hs.hotkey.bind [:alt :shift] :n nil
                (fn []
                  (: (hs.eventtap.event.newSystemKeyEvent :NEXT true) :post)
                  (: (hs.eventtap.event.newSystemKeyEvent :NEXT false) :post)))

;; Brightness =======================================
;; ==================================================

(hs.hotkey.bind [:alt :shift] "-" nil
                (fn []
                  (: (hs.eventtap.event.newSystemKeyEvent :BRIGHTNESS_DOWN true)
                     :post)
                  (: (hs.eventtap.event.newSystemKeyEvent :BRIGHTNESS_DOWN
                                                          false)
                     :post)))

(hs.hotkey.bind [:alt :shift] "=" nil
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

(hs.hotkey.bind [:rightshift] :escape nil
                (fn []
                  (: (hs.eventtap.event.newKeyEvent [:rightshift] "`" true)
                     :post)
                  (: (hs.eventtap.event.newKeyEvent [:rightshift] "`" false)
                     :post)))

;; Cmd+shift+m to Shift+F10 ==========================
;; ==================================================

(hs.hotkey.bind [:cmd :shift] :m nil
                (fn []
                  (: (hs.eventtap.event.newKeyEvent [:shift] :f10 true) :post)
                  (: (hs.eventtap.event.newKeyEvent [:shift] :f10 false) :post)))

