(require-macros :lib.macros)
(require-macros :lib.advice.macros)
(local windows (require :windows))
(local emacs (require :emacs))
(local slack (require :slack))
(local vim (require :vim))

(local {: concat : logf} (require :lib.functional))

;; Actions ==========================================
;; ==================================================

(fn activate [app-name]
  (fn activate []
    (windows.activate-app app-name)))

;; General ==========================================
;; ==================================================

(local music-app "Apple Music")
(local back-key {:key :space :title :Back :action :previous})

;; Windows ==========================================
;; ==================================================

(local window-jumps [{:mods [:cmd] :key :hjkl :title :Jump}
                     {:mods [:cmd]
                      :key :h
                      :action "windows:jump-window-left"
                      :repeatable true}
                     {:mods [:cmd]
                      :key :j
                      :action "windows:jump-window-above"
                      :repeatable true}
                     {:mods [:cmd]
                      :key :k
                      :action "windows:jump-window-below"
                      :repeatable true}
                     {:mods [:cmd]
                      :key :l
                      :action "windows:jump-window-right"
                      :repeatable true}])

(local window-halves [{:key :hjkl :title :Halves}
                      {:key :h
                       :action "windows:resize-half-left"
                       :repeatable true}
                      {:key :j
                       :action "windows:resize-half-bottom"
                       :repeatable true}
                      {:key :k
                       :action "windows:resize-half-top"
                       :repeatable true}
                      {:key :l
                       :action "windows:resize-half-right"
                       :repeatable true}])

(local window-increments [{:mods [:alt] :key :hjkl :title :Increments}
                          {:mods [:alt]
                           :key :h
                           :action "windows:resize-inc-left"
                           :repeatable true}
                          {:mods [:alt]
                           :key :j
                           :action "windows:resize-inc-bottom"
                           :repeatable true}
                          {:mods [:alt]
                           :key :k
                           :action "windows:resize-inc-top"
                           :repeatable true}
                          {:mods [:alt]
                           :key :l
                           :action "windows:resize-inc-right"
                           :repeatable true}])

(local window-resize [{:mods [:shift] :key :hjkl :title :Resize}
                      {:mods [:shift]
                       :key :h
                       :action "windows:resize-left"
                       :repeatable true}
                      {:mods [:shift]
                       :key :j
                       :action "windows:resize-down"
                       :repeatable true}
                      {:mods [:shift]
                       :key :k
                       :action "windows:resize-up"
                       :repeatable true}
                      {:mods [:shift]
                       :key :l
                       :action "windows:resize-right"
                       :repeatable true}])

(local window-move-screens [{:key "n, p" :title "Move next\\previous screen"}
                            {:mods [:shift]
                             :key "n, p"
                             :title "Move up\\down screens"}
                            {:key :n
                             :action "windows:move-south"
                             :repeatable true}
                            {:key :p
                             :action "windows:move-north"
                             :repeatable true}
                            {:mods [:shift]
                             :key :n
                             :action "windows:move-west"
                             :repeatable true}
                            {:mods [:shift]
                             :key :p
                             :action "windows:move-east"
                             :repeatable true}])

(local window-bindings
       (concat [back-key
                window-jumps
                window-halves
                window-increments
                window-resize
                window-move-screens]
               [{:key :w
                 :title "Last Window"
                 :action "windows:jump-to-last-window"}
                {:key :m
                 :title :Maximize
                 :action "windows:maximize-window-frame"}
                {:key :c :title :Center :action "windows:center-window-frame"}
                {:key :g :title :Grid :action "windows:show-grid"}
                {:key :u :title :Undo :action "windows:undo"}]))

;; Apps Menu ========================================
;; ==================================================

(fn make-app-binding [app-name key]
  {: key :title app-name :action (activate app-name)})

(local app-bindings [back-key
                     (make-app-binding :Emacs :e)
                     (make-app-binding :Safari :f)
                     (make-app-binding :Terminal :i)
                     (make-app-binding :KakaoTalk :k)
                     (make-app-binding :Slack :s)
                     (make-app-binding :Calendar :a)
                     (if (hs.application.find :Chrome)
                         (make-app-binding :Chrome :c)
                         (make-app-binding "Brave Browser" :b))
                     (make-app-binding :Zoom :z)
                     (make-app-binding :Mail :m)
                     (make-app-binding music-app :p)])

(local media-bindings [back-key
                       {:key :h
                        :title "Prev Track"
                        :action "multimedia:prev-track"
                        :repeatable true}
                       {:key :j
                        :title "Volume Down"
                        :action "multimedia:volume-down"
                        :repeatable true}
                       {:key :k
                        :title "Volume Up"
                        :action "multimedia:volume-up"
                        :repeatable true}
                       {:key :l
                        :title "Next Track"
                        :action "multimedia:next-track"
                        :repeatable true}
                       {:key :s
                        :title "Play or Pause"
                        :action "multimedia:play-or-pause"}
                       {:key :a
                        :title (.. "Launch " music-app)
                        :action (activate music-app)}])

(local brightness-bindings [back-key
                            {:key :h
                             :title "Brightness Down"
                             :action (fn []
                                       (hs.brightness.set (- (hs.brightness.get)
                                                             1)))
                             :repeatable true}
                            {:key :l
                             :title "Brightness Up"
                             :action (fn []
                                       (hs.brightness.set (+ (hs.brightness.get)
                                                             2)))
                             ; it's for a very weird reason
                             :repeatable true}])

(local emacs-bindings [back-key
                       {:key :c
                        :title :Capture
                        :action (fn []
                                  (emacs.capture))}
                       {:key :z
                        :title :Note
                        :action (fn []
                                  (emacs.note))}
                       {:key :v
                        :title :Split
                        :action "emacs:vertical-split-with-emacs"}
                       {:key :f
                        :title "Full Screen"
                        :action "emacs:full-screen"}])

;; Main Menu & Config ===============================
;; ==================================================

(local menu-items
       [{:key "'" :title :Hammerspoon :action hs.toggleConsole}
        {:key :space :title :Alfred :action (activate "Alfred 5")}
        {:key :w
         :title :Window
         :enter "windows:enter-window-menu"
         :exit "windows:exit-window-menu"
         :items window-bindings}
        {:key :a :title :Apps :items app-bindings}
        {:key :b :title :Brightness :items brightness-bindings}
        {:key :m :title :Media :items media-bindings}
        {:key :x :title :Emacs :items emacs-bindings}])

(local agnostic-keys [{:mods [:cmd]
                       :key :space
                       :action "lib.modal:activate-modal"}
                      {:mods [:cmd :ctrl] :key "`" :action hs.toggleConsole}
                      {:mods [:cmd :ctrl]
                       :key :return
                       :action (activate :Safari)}
                      {:mods [:cmd :ctrl]
                       :key :delete
                       :action (activate :Brave)}
                      {:mods [:cmd :ctrl]
                       :key :o
                       :action "emacs:edit-with-emacs"}])

;; App Specific Config ==============================
;; ==================================================

(local browser-keys [{:mods [:cmd :shift]
                      :key :l
                      :action "chrome:open-location"}
                     {:mods [:alt]
                      :key :k
                      :action "chrome:next-tab"
                      :repeat true}
                     {:mods [:alt]
                      :key :j
                      :action "chrome:prev-tab"
                      :repeat true}])

(local browser-items
       (concat menu-items
               [{:key "'"
                 :title "Edit with Emacs"
                 :action "emacs:edit-with-emacs"}]))

(local brave-config {:key "Brave Browser"
                     :keys browser-keys
                     :items browser-items})

(local chrome-config {:key "Google Chrome"
                      :keys browser-keys
                      :items browser-items})

(local firefox-config {:key :Firefox :keys browser-keys :items browser-items})

(local emacs-config {:key :Emacs
                     :activate (fn []
                                 (vim.disable))
                     :deactivate (fn []
                                   (vim.enable))
                     :launch "emacs:maximize"
                     :items []
                     :keys []})

(local hammerspoon-config {:key :Hammerspoon
                           :items (concat menu-items
                                          [{:key :r
                                            :title "Reload Console"
                                            :action hs.reload}
                                           {:key :c
                                            :title "Clear Console"
                                            :action hs.console.clearConsole}])
                           :keys []})

(local slack-config {:key :Slack
                     :keys [{:mods [:cmd]
                             :key :g
                             :action "slack:scroll-to-bottom"}
                            {:mods [:ctrl]
                             :key :r
                             :action "slack:add-reaction"}
                            {:mods [:ctrl]
                             :key :h
                             :action "slack:prev-element"}
                            {:mods [:ctrl]
                             :key :l
                             :action "slack:next-element"}
                            {:mods [:ctrl] :key :t :action "slack:thread"}
                            {:mods [:ctrl] :key :p :action "slack:prev-day"}
                            {:mods [:ctrl] :key :n :action "slack:next-day"}
                            {:mods [:ctrl]
                             :key :e
                             :action "slack:scroll-up"
                             :repeat true}
                            {:mods [:ctrl]
                             :key :y
                             :action "slack:scroll-down"
                             :repeat true}
                            {:mods [:ctrl]
                             :key :i
                             :action "slack:next-history"
                             :repeat true}
                            {:mods [:ctrl]
                             :key :o
                             :action "slack:prev-history"
                             :repeat true}
                            {:mods [:ctrl]
                             :key :j
                             :action "slack:down"
                             :repeat true}
                            {:mods [:ctrl]
                             :key :k
                             :action "slack:up"
                             :repeat true}]})

(local apps [brave-config
             chrome-config
             firefox-config
             emacs-config
             hammerspoon-config
             slack-config])

(local config {:title "Main Menu"
               :items menu-items
               :keys agnostic-keys
               :enter (fn []
                        (windows.hide-display-numbers))
               :exit (fn []
                       (windows.hide-display-numbers))
               : apps
               :hyper {:key :F18}
               :modules {:windows {:center-ratio "80:50"}}})

;; nREPL server =====================================
;; ==================================================

(local repl (require :repl))
(repl.run (repl.start))

;; TEMP

(global e (: (hs.eventtap.new [hs.eventtap.event.types.flagsChanged
                               hs.eventtap.event.types.keyUp
                               hs.eventtap.event.types.keyDown]
                              (fn [ev]
                                (let [raw-flags (band (. (. (ev:getRawEventData)
                                                            :CGEventData)
                                                         :flags)
                                                      3758096127)]
                                  (when (= raw-flags 1048592)
                                    (match (hs.keycodes.currentLayout)
                                      :U.S. (hs.keycodes.setMethod "2-Set Korean")
                                      "2-Set Korean" (hs.keycodes.setLayout :U.S.))
                                    (values true {}))))) :start))

;; Exports ==========================================
;; ==================================================

config

