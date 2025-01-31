(require-macros :lib.macros)
(require-macros :lib.advice.macros)
(local windows (require :windows))
(local emacs (require :emacs))
(local slack (require :slack))
(local vim (require :vim))
(local input-method (require :input-method))
(local display (require :display))
(local {: concat : logf : filter : identity} (require :lib.functional))
(local {: desparsify} (require :lib.utils))

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

(local window-bindings
       [back-key
        {:key :hjkl :title :Move}
        {:key :h :action "windows:move-left-repeatedly" :repeatable true}
        {:key :j :action "windows:move-down-repeatedly" :repeatable true}
        {:key :k :action "windows:move-up-repeatedly" :repeatable true}
        {:key :l :action "windows:move-right-repeatedly" :repeatable true}
        {:mods [:cmd] :key :hjkl :title :Resize}
        {:mods [:cmd]
         :key :h
         :action "windows:resize-border-left-repeatedly"
         :repeatable true}
        {:mods [:cmd]
         :key :j
         :action "windows:resize-border-down-repeatedly"
         :repeatable true}
        {:mods [:cmd]
         :key :k
         :action "windows:resize-border-up-repeatedly"
         :repeatable true}
        {:mods [:cmd]
         :key :l
         :action "windows:resize-border-right-repeatedly"
         :repeatable true}
        {:mods [:shift] :key :hjkl :title "Resize Half"}
        {:mods [:shift]
         :key :h
         :action "windows:resize-half-left-repeatedly"
         :repeatable false}
        {:mods [:shift]
         :key :j
         :action "windows:resize-half-bottom-repeatedly"
         :repeatable false}
        {:mods [:shift]
         :key :k
         :action "windows:resize-half-top-repeatedly"
         :repeatable false}
        {:mods [:shift]
         :key :l
         :action "windows:resize-half-right-repeatedly"
         :repeatable false}
        {:key :w :title "Last Window" :action "windows:jump-to-last-window"}
        {:key :m :title :Maximize :action "windows:maximize-window-frame"}
        {:mods [:shift]
         :key :m
         :title :Minimize
         :action "windows:minimize-window-frame"}
        {:key :c
         :title :Center
         :action "windows:center-enlarge-with-rate-repeatedly"}
        {:key :g :title :Grid :action "windows:show-grid"}
        {:key :u :title :Undo :action "windows:undo"}])

;; Apps Menu ========================================
;; ==================================================

(fn app-binding [app-name key]
  {: key :title app-name :action (activate app-name)})

(fn app-binding-if-exists [app-name key]
  (when (hs.fs.displayName (.. :/Applications/ app-name :.app))
    {: key :title app-name :action (activate app-name)}))

(local app-bindings
       (desparsify
        [back-key
         (app-binding :Emacs :e)
         (app-binding :Safari :f)
         (app-binding :Ghostty :i)
         (app-binding :Slack :s)
         (app-binding :Calendar :a)
         (app-binding :Zoom :z)
         (app-binding :Mail :m)
         (app-binding music-app :p)
         (app-binding-if-exists "Visual Studio Code" :v)
         (app-binding-if-exists "Google Chrome" :c)
         (app-binding-if-exists "Brave Browser" :b)
         (app-binding-if-exists :KakaoTalk :k)]))

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
                       {:key :m :title :Mute :action "multimedia:mute"}
                       {:key :a
                        :title (.. "Launch " music-app)
                        :action (activate music-app)}])

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

(local display-bindings
       [back-key
        {:key :h
         :title "Brightness Down"
         :action (fn []
                   (hs.brightness.set (- (hs.brightness.get) 1)))
         :repeatable true}
        {:key :l
         :title "Brightness Up"
         :action (fn []
                   (hs.brightness.set (+ (hs.brightness.get) 2)))
         ; it's for a very weird reason
         :repeatable true}
        {:key :t :title "Toggle Dark Mode" :action "display:toggle-dark-mode"}])

;; Main Menu & Config ===============================
;; ==================================================

(local menu-items
       [{:key "'" :title :Hammerspoon :action hs.toggleConsole}
        {:key :r :title :Relaunch :action hs.relaunch}
        {:key :space :title :Alfred :action (activate "Alfred 5")}
        {:key :w
         :title :Window
         :enter "windows:enter-window-menu"
         :exit "windows:exit-window-menu"
         :items window-bindings}
        {:key :a :title :Apps :items app-bindings}
        {:key :m :title :Media :items media-bindings}
        {:key :x :title :Emacs :items emacs-bindings}
        {:key :d :title :Display :items display-bindings}])

(local agnostic-keys [{:mods [:cmd]
                       :key :space
                       :action "lib.modal:activate-modal"}
                      {:mods [:cmd :ctrl] :key "`" :action hs.toggleConsole}
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

;; (local repl (require :repl))
;; (repl.run (repl.start))

;; Exports ==========================================
;; ==================================================

config
