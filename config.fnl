(require-macros :lib.macros)
(require-macros :lib.advice.macros)
(local windows (require :windows))
(local emacs (require :emacs))
(local slack (require :slack))
(local vim (require :vim))
(local input-method (require :input-method))
(local display (require :display))
(local modes (require :lib.modes))
(local {: concat : logf : filter : identity : merge} (require :lib.functional))
(local {: desparsify} (require :lib.utils))

;; Actions ==========================================
;; ==================================================

(fn activate [app-name]
  (fn activate []
    (windows.activate-app app-name)))

;; General ==========================================
;; ==================================================


;; Windows ==========================================
;; ==================================================

(local window-bindings
       [{:key :hjkl :title :Focus}
        {:key :h :action "windows:jump-window-left" :repeatable true}
        {:key :j :action "windows:jump-window-below" :repeatable true}
        {:key :k :action "windows:jump-window-above" :repeatable true}
        {:key :l :action "windows:jump-window-right" :repeatable true}
        {:mods [:cmd] :key :hjkl :title "Top/Left Border"}
        {:mods [:cmd]
         :key :h
         :action "windows:left-border-left-repeatedly"
         :repeatable true}
        {:mods [:cmd]
         :key :j
         :action "windows:top-border-down-repeatedly"
         :repeatable true}
        {:mods [:cmd]
         :key :k
         :action "windows:top-border-up-repeatedly"
         :repeatable true}
        {:mods [:cmd]
         :key :l
         :action "windows:left-border-right-repeatedly"
         :repeatable true}
        {:mods [:ctrl] :key :hjkl :title "Bottom/Right Border"}
        {:mods [:ctrl]
         :key :h
         :action "windows:right-border-left-repeatedly"
         :repeatable true}
        {:mods [:ctrl]
         :key :j
         :action "windows:bottom-border-down-repeatedly"
         :repeatable true}
        {:mods [:ctrl]
         :key :k
         :action "windows:bottom-border-up-repeatedly"
         :repeatable true}
        {:mods [:ctrl]
         :key :l
         :action "windows:right-border-right-repeatedly"
         :repeatable true}
        {:mods [:alt] :key :hjkl :title :Move}
        {:mods [:alt] :key :h :action "windows:move-left-repeatedly" :repeatable true}
        {:mods [:alt] :key :j :action "windows:move-down-repeatedly" :repeatable true}
        {:mods [:alt] :key :k :action "windows:move-up-repeatedly" :repeatable true}
        {:mods [:alt] :key :l :action "windows:move-right-repeatedly" :repeatable true}
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
        {:key :d :title :Close :action "windows:close-window"}
        {:key :f :title "Full Screen" :action "windows:toggle-fullscreen"}
        {:key "=" :title :Balance :action "windows:tile-windows"}
        {:key :v :title "Split with Last" :action "windows:split-side-by-side"}
        {:key :s :title "Stack with Last" :action "windows:split-stacked"}
        {:key :p :title "Pair in Split View" :action "split-view:pair"}
        {:key "[" :title "Separator Left" :action "split-view:move-separator-left"
         :repeatable true}
        {:key "]" :title "Separator Right" :action "split-view:move-separator-right"
         :repeatable true}
        {:key :x :title "Swap Split View" :action "split-view:swap"}
        {:key :r :title :Rotate :action "windows:rotate-windows-down" :repeatable true}
        {:mods [:shift]
         :key :r
         :title "Rotate Back"
         :action "windows:rotate-windows-up"
         :repeatable true}
        {:key :u :title :Undo :action "windows:undo" :repeatable true}
        {:mods [:shift] :key :u :title :Redo :action "windows:redo" :repeatable true}])

;; Apps Menu ========================================
;; ==================================================

(fn app-binding [app-name key]
  {: key :title app-name :action (activate app-name)})

(fn app-binding-if-exists [app-name key]
  (when (hs.fs.displayName (.. :/Applications/ app-name :.app))
    {: key :title app-name :action (activate app-name)}))

(local radio-bindings
       [{:key :p :title "Play Station" :action "radio:choose-station"}
        {:key :s :title :Stop :action "radio:stop"}
        {:mods [:shift] :key :r :title :Toggle :action "radio:toggle"}])

(local app-bindings
       (desparsify
        [(app-binding :Emacs :e)
         (app-binding :Safari :f)
         (app-binding :Ghostty :i)
         (app-binding :Slack :s)
         (app-binding :Calendar :a)
         (app-binding :Zoom :z)
         (app-binding :Mail :m)
         (app-binding-if-exists "Visual Studio Code" :v)
         (app-binding-if-exists "Google Chrome" :c)
         (app-binding-if-exists "Brave Browser" :b)
         (app-binding-if-exists :KakaoTalk :k)
         {:mods [:shift] :key :r :title :Radio :items radio-bindings}]))

;; System media, as the media keys: they control what macOS shows in Now
;; Playing. EMMS has its own keys in Emacs' mode keymap (Cmd-Shift-, a m e).
(local media-bindings [{:key :h
                        :title "Prev Track"
                        :action "media:previous-track"
                        :repeatable true}
                       {:key :j
                        :title "Volume Down"
                        :action "media:volume-down"
                        :repeatable true}
                       {:key :k
                        :title "Volume Up"
                        :action "media:volume-up"
                        :repeatable true}
                       {:key :l
                        :title "Next Track"
                        :action "media:next-track"
                        :repeatable true}
                       {:key :s
                        :title "Play or Pause"
                        :action "media:play-or-pause"}
                       {:key :m :title :Mute :action "media:mute"}
                       {:key :n :title "Now Playing" :action "media:now-playing"}
                       {:key :a :title "Open mpv" :action "mpv:open"}])

;; Notification Center. The current notification is the one peeking in at
;; the top right; burying swipes it away but keeps it in Notification Center.
(local notification-bindings [{:key :n
                               :title "Notification Center"
                               :action "notifications:open"}
                              {:key :b
                               :title "Bury Current"
                               :action "notifications:bury-current"
                               :repeatable true}
                              {:key :x
                               :title "Clear Current"
                               :action "notifications:clear-current"
                               :repeatable true}
                              {:key :c
                               :title "Clear All"
                               :action "notifications:clear-all"}])

(local emacs-bindings [{:key :c
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
       [{:key :h
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

;; Emacs' C-h: describe keys, bindings and UI elements (describe.fnl).
(local help-bindings
       [{:key :k :title "Describe Key" :action "describe:describe-key"}
        {:key :b :title "Describe Bindings" :action "describe:describe-bindings"}
        {:key :w :title "Where Is" :action "describe:where-is"}
        {:key "." :title "Describe Element" :action "describe:describe-element"}
        {:key :p
         :title "Element at Pointer"
         :action "describe:describe-element-at-pointer"}
        {:key :m :title "Describe Mode" :action "describe:describe-mode"}
        {:key :f :title "Describe Function" :action "describe:describe-function"}
        {:key :a :title "Apropos Command" :action "describe:apropos-command"}
        {:key :e :title "Messages" :action "describe:view-echo-area-messages"}
        {:key "=" :title "Describe Char" :action "describe:describe-char"}
        {:mods [:shift]
         :key :a
         :title "Describe App"
         :action "describe:describe-app"}
        {:mods [:shift]
         :key :w
         :title "Describe Window"
         :action "describe:describe-window"}])

;; Clipboard history (clipboard.fnl).
(local clipboard-bindings
       [{:key :y :title "Clipboard History" :action "clipboard:show"}
        {:key :c :title "Clear History" :action "clipboard:confirm-clear"}
        {:key :p :title "Pause Recording" :action "clipboard:pause"}
        {:key :r :title "Resume Recording" :action "clipboard:resume"}
        {:mods [:shift]
         :key :y
         :title "Toggle Recording"
         :action "clipboard:toggle-pause"}])

(local buffer-bindings
       [{:key :b :title "Switch Window" :action "buffers:switch-to-window"}
        {:key :d :title "Close Window" :action "windows:close-window"}
        {:key :n :title "Next App Window" :action "buffers:next-window" :repeatable true}
        {:key :p :title "Previous App Window" :action "buffers:previous-window" :repeatable true}
        {:key :tab :title "Last Window" :action "buffers:last-window"}])

(local quit-bindings
       [{:key :q :title "Quit App" :action "quit:quit-app"}
        {:mods [:shift] :key :q :title "Force Quit App" :action "quit:force-quit-app"}
        {:key :r :title "Relaunch App" :action "quit:relaunch-app"}
        {:key :f :title "Close Window" :action "windows:close-window"}])

(local embark-bindings
       [{:key :e :title "Act on Selection" :action "embark:act"}])

(local toggle-bindings
       [{:mods [:shift] :key :f :title "Full Screen" :action "windows:toggle-fullscreen"}
        {:key :t :title "Dark Mode" :action "toggles:toggle-dark-mode"}
        {:key :d :title "Dock Hiding" :action "toggles:toggle-dock-autohide"}
        {:key :m :title "Menu Bar Hiding" :action "toggles:toggle-menu-bar-autohide"}
        {:key :h :title "Hidden Files" :action "toggles:toggle-hidden-files"}
        {:key :n :title "Do Not Disturb" :action "toggles:toggle-do-not-disturb"}
        {:key :a :title "Keep Awake" :action "toggles:toggle-keep-awake"}
        {:key :v :title "Mute" :action "toggles:toggle-mute"}
        {:key :w :title "Wi-Fi" :action "toggles:toggle-wifi"}
        {:key :b :title "Bluetooth" :action "toggles:toggle-bluetooth"}])

(local space-bindings
       [{:key :t :title "Switch Space" :action "spaces:list-spaces"}
        {:key :tab :title "Last Space" :action "spaces:last-space"}
        {:key :h :title "Space Left" :action "spaces:space-left" :repeatable true}
        {:key :l :title "Space Right" :action "spaces:space-right" :repeatable true}
        {:mods [:shift] :key :n :title "New Desktop" :action "spaces:new-space"}
        {:key :d :title "Close Desktop" :action "spaces:close-space"}
        {:key :1 :title "Space 1" :action "spaces:goto-space-1"}
        {:key :2 :title "Space 2" :action "spaces:goto-space-2"}
        {:key :3 :title "Space 3" :action "spaces:goto-space-3"}
        {:key :4 :title "Space 4" :action "spaces:goto-space-4"}
        {:key :5 :title "Space 5" :action "spaces:goto-space-5"}
        {:key :6 :title "Space 6" :action "spaces:goto-space-6"}
        {:key :7 :title "Space 7" :action "spaces:goto-space-7"}
        {:key :8 :title "Space 8" :action "spaces:goto-space-8"}
        {:key :9 :title "Space 9" :action "spaces:goto-space-9"}])

(local system-bindings
       [{:key :i :title "System Info" :action "sysinfo:info"}
        {:key :c :title "World Clock" :action "sysinfo:world-clock"}
        {:key :p :title :Processes :action "sysinfo:processes"}])

(local layout-bindings
       [{:key :s :title "Save Layout" :action "layouts:save"}
        {:key :r :title "Restore Layout" :action "layouts:restore"}
        {:key :d :title "Delete Layout" :action "layouts:delete"}
        {:mods [:shift] :key :r :title "Revert Layout" :action "layouts:revert"}])

(local voice-bindings
       [{:key :v :title :Dictate :action "voice:dictate"}
        {:key :l :title :Language :action "voice:choose-language"}
        {:key :c :title "Correct a Word" :action "voice:correct-dictation"}
        {:key :x :title :Cancel :action "voice:cancel-dictation"}
        {:key :e :title "Edit Corrections" :action "voice:edit-corrections"}])

(local menu-items
       [{:key "'" :title :Hammerspoon :action hs.toggleConsole}
        {:key :r :title :Relaunch :action hs.relaunch}
        {:key :space :title :Run :action "launcher:show"}
        {:key :y :title :Clipboard :items clipboard-bindings}
        {:key :w
         :title :Window
         :enter "windows:enter-window-menu"
         :exit "windows:exit-window-menu"
         :items window-bindings}
        {:key :b :title :Buffers :items buffer-bindings}
        {:key :a :title :Apps :items app-bindings}
        {:key :m :title :Media :items media-bindings}
        {:key :n :title :Notifications :items notification-bindings}
        {:key :x :title :Emacs :items emacs-bindings}
        {:key :d :title :Display :items display-bindings}
        {:key :k
         :title "Keyboard Shortcuts"
         :action "system-shortcuts:execute"}
        {:key :e :title :Embark :items embark-bindings}
        {:key :q :title :Quit :items quit-bindings}
        {:mods [:shift] :key :t :title :Toggles :items toggle-bindings}
        {:key :t :title :Spaces :items space-bindings}
        {:key "[" :title "Space Left" :action "spaces:space-left" :repeatable true}
        {:key "]" :title "Space Right" :action "spaces:space-right" :repeatable true}
        {:mods [:shift] :key ";" :title "Eval Fennel" :action "eval:eval-expression"}
        {:mods [:shift] :key :a :title :System :items system-bindings}
        {:mods [:shift] :key :d :title :Layouts :items layout-bindings}
        {:key :v :title :Voice :items voice-bindings}
        {:key :h :title :Help :items help-bindings}])

(local agnostic-keys [{:mods [:cmd]
                       :key :space
                       :action "lib.modal:activate-modal"}
                      {:mods [:cmd :shift]
                       :key ","
                       :action "lib.modal:activate-local-leader"}
                      {:mods [:cmd :ctrl] :key "`" :action hs.toggleConsole}
                      {:mods [:cmd :shift]
                       :key :space
                       :action "hints:show-hints"}
                      {:mods [:ctrl :alt]
                       :key :space
                       :action "hints:search"}
                      {:mods [:cmd :ctrl]
                       :key :delete
                       :action (activate :Brave)}
                      {:mods [:cmd :ctrl]
                       :key :o
                       :action "emacs:edit-with-emacs"}
                      ;; Same chord as voice-dictate (C-s-l) in Emacs, which
                      ;; it hands over to there.
                      {:mods [:cmd :ctrl]
                       :key :l
                       :action "voice:dictate"}])

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

(local chatgpt-config {:key :ChatGPT
                       :keys [{:mods [:ctrl]
                               :key :l
                               :action "chatgpt:focus-composer"}]})

(local apps [brave-config
             chrome-config
             firefox-config
             emacs-config
             hammerspoon-config
             slack-config
             chatgpt-config])

;; Application Modes ================================
;; ==================================================
;; `Cmd-Shift-,` opens the keymap of the frontmost app's mode (see lib/modes.fnl).
;; Keymaps are prefix tries: :items nodes are named prefixes, :command nodes
;; run a command registered by the mode's :commands table.

(local safari-mode
       {:id :safari-mode
        :title :Safari
        :bundle-ids [:com.apple.Safari]
        :commands (. (require :safari) :commands)
        :keymap [{:key :l :title :address :command :safari.focus-address}
                 {:key :f :title :find :command :safari.find}
                 {:key "[" :title :back :command :safari.back}
                 {:key "]" :title :forward :command :safari.forward}
                 {:key :b
                  :title :tabs
                  :items [{:key :n :title :new :command :safari.tab.new}
                          {:key :e :title :new-at-end :command :safari.tab.new-at-end}
                          {:key :d :title :close :command :safari.tab.close}
                          {:key :o :title :close-others :command :safari.tab.close-others}
                          {:key :y :title :duplicate :command :safari.tab.duplicate}
                          {:key :p :title :pin :command :safari.tab.pin}
                          {:key :m :title :mute :command :safari.tab.mute}
                          {:key :w :title :to-window :command :safari.tab.move-to-window}
                          {:key "[" :title :previous :command :safari.tab.previous}
                          {:key "]" :title :next :command :safari.tab.next}
                          {:key :v :title :overview :command :safari.tab.overview}
                          {:key :a
                           :title :arrange
                           :items [{:key :t :title :by-title :command :safari.tab.arrange.title}
                                   {:key :w :title :by-website :command :safari.tab.arrange.website}]}]}
                 {:key :w
                  :title :window
                  :items [{:key :n :title :new :command :safari.window.new}
                          {:key :p :title :private :command :safari.window.new-private}
                          {:key :d :title :close :command :safari.window.close}
                          {:key :m :title :minimize :command :safari.window.minimize}
                          {:key :z :title :zoom :command :safari.window.zoom}
                          {:key :f :title :fill :command :safari.window.fill}
                          {:key :c :title :center :command :safari.window.center}
                          {:key :g :title :merge :command :safari.window.merge}
                          {:key :s :title :full-screen :command :safari.window.full-screen}]}
                 {:key :t
                  :title :toolbar
                  :items [{:key :s :title :show :command :safari.toolbar.show}
                          {:key :c :title :customize :command :safari.toolbar.customize}
                          {:key :t :title :tab-bar :command :safari.toolbar.tab-bar}
                          {:key :f :title :favorites-bar :command :safari.toolbar.favorites-bar}
                          {:key :b :title :status-bar :command :safari.toolbar.status-bar}]}
                 {:key :s
                  :title :sidebar
                  :items [{:key :s :title :toggle :command :safari.sidebar.toggle}
                          {:key :b :title :bookmarks :command :safari.sidebar.bookmarks}
                          {:key :r :title :reading-list :command :safari.sidebar.reading-list}
                          {:key :y :title :shared :command :safari.sidebar.shared}]}
                 {:key :v
                  :title :view
                  :items [{:key :r :title :reader :command :safari.view.reader}
                          {:key :d :title :downloads :command :safari.view.downloads}]}
                 {:key :z
                  :title :zoom
                  :items [{:key :i :title :in :command :safari.zoom.in}
                          {:key :o :title :out :command :safari.zoom.out}
                          {:key :0 :title :reset :command :safari.zoom.reset}
                          {:key :b :title :text-bigger :command :safari.zoom.text-bigger}
                          {:key :s :title :text-smaller :command :safari.zoom.text-smaller}]}
                 {:key :r
                  :title :reload
                  :items [{:key :r :title :page :command :safari.reload.page}
                          {:key :o :title :from-origin :command :safari.reload.from-origin}
                          {:key :b :title :content-blockers :command :safari.reload.content-blockers}
                          {:key :s :title :stop :command :safari.reload.stop}]}
                 {:key :h
                  :title :history
                  :items [{:key :h :title :show :command :safari.history.show}
                          {:key :m :title :home :command :safari.history.home}
                          {:key :s :title :search-results :command :safari.history.search-results}
                          {:key :u :title :reopen :command :safari.history.reopen}
                          {:key :a :title :reopen-session :command :safari.history.reopen-session}
                          {:key :c :title :clear :command :safari.history.clear}]}
                 {:key :m
                  :title :bookmarks
                  :items [{:key :a :title :add :command :safari.bookmark.add}
                          {:key :l :title :reading-list :command :safari.bookmark.reading-list}
                          {:key :e :title :edit :command :safari.bookmark.edit}
                          {:key :s :title :show :command :safari.bookmark.show}
                          {:key :p :title :start-page :command :safari.bookmark.start-page}]}
                 {:key :d
                  :title :develop
                  :items [{:key :i :title :inspector :command :safari.develop.inspector}
                          {:key :c :title :console :command :safari.develop.console}
                          {:key :u :title :source :command :safari.develop.source}
                          {:key :e :title :element :command :safari.develop.element}
                          {:key :r :title :responsive :command :safari.develop.responsive}
                          {:key :x :title :empty-caches :command :safari.develop.empty-caches}]}]})

(local chatgpt-mode
       {:id :chatgpt-mode
        :title :ChatGPT
        :bundle-ids [:com.openai.codex]
        :commands (. (require :chatgpt) :commands)
        :keymap [{:key :i :title :input :command :chatgpt.focus-input}
                 {:key :n :title :new-chat :command :chatgpt.new-chat}
                 {:key :t
                  :title :temporary-chat
                  :command :chatgpt.new-temporary-chat}
                 {:key :s :title :sidebar :command :chatgpt.toggle-sidebar}]})

(local facetime-mode
       {:id :facetime-mode
        :title :FaceTime
        :bundle-ids [:com.apple.FaceTime]
        :commands (. (require :facetime) :commands)
        :keymap [{:key :n :title :new-call :command :facetime.new-call}
                 {:key :m :title :mute :command :facetime.mute}
                 {:key :t :title :on-top :command :facetime.toggle-on-top}]})

(local finder-mode
       {:id :finder-mode
        :title :Finder
        :bundle-ids [:com.apple.finder]
        :commands (. (require :finder) :commands)
        :keymap [{:key :l :title :go-to-folder :command :finder.go.folder}
                 {:key :f :title :find :command :finder.find}
                 {:key "[" :title :back :command :finder.back}
                 {:key "]" :title :forward :command :finder.forward}
                 {:key :u :title :up :command :finder.up}
                 {:key :o :title :open :command :finder.open}
                 {:key :i :title :info :command :finder.info}
                 {:key :y :title :quick-look :command :finder.quick-look}
                 {:key :g
                  :title :go
                  :items [{:key :h :title :home :command :finder.go.home}
                          {:key :d :title :desktop :command :finder.go.desktop}
                          {:key :o :title :documents :command :finder.go.documents}
                          {:key :l :title :downloads :command :finder.go.downloads}
                          {:key :a :title :applications :command :finder.go.applications}
                          {:key :u :title :utilities :command :finder.go.utilities}
                          {:key :x :title :library :command :finder.go.library}
                          {:key :r :title :recents :command :finder.go.recents}
                          {:key :i :title :icloud :command :finder.go.icloud}
                          {:key :c :title :computer :command :finder.go.computer}
                          {:key :n :title :network :command :finder.go.network}
                          {:key :s :title :shared :command :finder.go.shared}
                          {:key :w :title :airdrop :command :finder.go.airdrop}
                          {:key :k :title :server :command :finder.go.server}]}
                 {:key :e
                  :title :edit
                  :items [{:key :n :title :new-folder :command :finder.file.new-folder}
                          {:key :s :title :folder-with-selection :command :finder.file.new-folder-with}
                          {:key :r :title :rename :command :finder.file.rename}
                          {:key :d :title :duplicate :command :finder.file.duplicate}
                          {:key :a :title :alias :command :finder.file.alias}
                          {:key :c :title :compress :command :finder.file.compress}
                          {:key :o :title :original :command :finder.file.original}
                          {:key :p :title :copy-path :command :finder.file.copy-path}
                          {:key :k :title :copy-link :command :finder.file.copy-link}
                          {:key :x :title :trash :command :finder.file.trash}
                          {:key :e :title :empty-trash :command :finder.file.empty-trash}]}
                 {:key :v
                  :title :view
                  :items [{:key :i :title :icons :command :finder.view.icons}
                          {:key :l :title :list :command :finder.view.list}
                          {:key :c :title :columns :command :finder.view.columns}
                          {:key :g :title :gallery :command :finder.view.gallery}
                          {:key :u :title :groups :command :finder.view.groups}
                          {:key :k :title :clean-up :command :finder.view.clean-up}
                          {:key :o :title :options :command :finder.view.options}
                          {:key :s
                           :title :sort
                           :items [{:key :x :title :none :command :finder.sort.none}
                                   {:key :n :title :name :command :finder.sort.name}
                                   {:key :k :title :kind :command :finder.sort.kind}
                                   {:key :o :title :opened :command :finder.sort.opened}
                                   {:key :a :title :added :command :finder.sort.added}
                                   {:key :m :title :modified :command :finder.sort.modified}
                                   {:key :c :title :created :command :finder.sort.created}
                                   {:key :s :title :size :command :finder.sort.size}
                                   {:key :t :title :tags :command :finder.sort.tags}]}]}
                 {:key :t
                  :title :toolbar
                  :items [{:key :s :title :show :command :finder.toolbar.show}
                          {:key :c :title :customize :command :finder.toolbar.customize}
                          {:key :t :title :tab-bar :command :finder.toolbar.tab-bar}
                          {:key :p :title :path-bar :command :finder.toolbar.path-bar}
                          {:key :b :title :status-bar :command :finder.toolbar.status-bar}]}
                 {:key :s
                  :title :sidebar
                  :items [{:key :s :title :toggle :command :finder.sidebar.toggle}
                          {:key :p :title :preview :command :finder.sidebar.preview}]}
                 {:key :b
                  :title :tabs
                  :items [{:key :n :title :new :command :finder.tab.new}
                          {:key :o :title :open :command :finder.tab.open}
                          {:key :d :title :close :command :finder.tab.close}
                          {:key "[" :title :previous :command :finder.tab.previous}
                          {:key "]" :title :next :command :finder.tab.next}
                          {:key :w :title :to-window :command :finder.tab.move-to-window}
                          {:key :v :title :overview :command :finder.tab.overview}]}
                 {:key :w
                  :title :window
                  :items [{:key :n :title :new :command :finder.window.new}
                          {:key :d :title :close :command :finder.window.close}
                          {:key :a :title :close-all :command :finder.window.close-all}
                          {:key :m :title :minimize :command :finder.window.minimize}
                          {:key :z :title :zoom :command :finder.window.zoom}
                          {:key :f :title :fill :command :finder.window.fill}
                          {:key :c :title :center :command :finder.window.center}
                          {:key :g :title :merge :command :finder.window.merge}
                          {:key :s :title :full-screen :command :finder.window.full-screen}]}]})

(local netnewswire-mode
       {:id :netnewswire-mode
        :title :NetNewsWire
        :bundle-ids [:com.ranchero.NetNewsWire-Evergreen]
        :commands (. (require :netnewswire) :commands)
        :keymap [{:key :1 :title :feeds :command :netnewswire.pane.feeds}
                 {:key :2 :title :timeline :command :netnewswire.pane.timeline}
                 {:key :3 :title :article :command :netnewswire.pane.article}
                 {:key :f :title :search :command :netnewswire.search}
                 {:key :r :title :refresh :command :netnewswire.refresh}
                 {:key :n :title :next-unread :command :netnewswire.go.next-unread}
                 {:key :u :title :read/unread :command :netnewswire.article.read}
                 {:key :o :title :open-in-browser :command :netnewswire.article.browser}
                 {:key :a
                  :title :article
                  :items [{:key :u :title :read/unread :command :netnewswire.article.read}
                          {:key :s :title :star :command :netnewswire.article.star}
                          {:key :a :title :all-read :command :netnewswire.article.read-all}
                          {:key :k :title :above-read :command :netnewswire.article.read-above}
                          {:key :j :title :below-read :command :netnewswire.article.read-below}
                          {:key :v :title :reader-view :command :netnewswire.article.reader}
                          {:key :o :title :browser :command :netnewswire.article.browser}
                          {:key :b :title :background :command :netnewswire.article.background}
                          {:key :c :title :copy-url :command :netnewswire.article.copy-url}
                          {:key :x :title :copy-external-url :command :netnewswire.article.copy-external-url}]}
                 {:key :g
                  :title :go
                  :items [{:key :n :title :next-unread :command :netnewswire.go.next-unread}
                          {:key :t :title :today :command :netnewswire.go.today}
                          {:key :u :title :all-unread :command :netnewswire.go.unread}
                          {:key :s :title :starred :command :netnewswire.go.starred}]}
                 {:key :v
                  :title :view
                  :items [{:key :r :title :read-articles :command :netnewswire.view.read-articles}
                          {:key :f :title :read-feeds :command :netnewswire.view.read-feeds}
                          {:key :c :title :columns :command :netnewswire.view.columns}
                          {:key :k :title :clean-up :command :netnewswire.view.clean-up}
                          {:key :s
                           :title :sort
                           :items [{:key :d :title :date :command :netnewswire.sort.date}
                                   {:key :f :title :feed :command :netnewswire.sort.feed}
                                   {:key :t :title :title :command :netnewswire.sort.title}
                                   {:key :u :title :unread :command :netnewswire.sort.unread}
                                   {:key :s :title :starred :command :netnewswire.sort.starred}
                                   {:key :o :title :oldest-on-top :command :netnewswire.sort.oldest}
                                   {:key :n :title :newest-on-top :command :netnewswire.sort.newest}]}]}
                 {:key :t
                  :title :toolbar
                  :items [{:key :s :title :show :command :netnewswire.toolbar.show}
                          {:key :c :title :customize :command :netnewswire.toolbar.customize}]}
                 {:key :s
                  :title :sidebar
                  :items [{:key :s :title :toggle :command :netnewswire.sidebar.toggle}]}
                 {:key :e
                  :title :feeds
                  :items [{:key :n :title :new-feed :command :netnewswire.feed.new}
                          {:key :f :title :new-folder :command :netnewswire.feed.new-folder}
                          {:key :i :title :import :command :netnewswire.feed.import}
                          {:key :x :title :export :command :netnewswire.feed.export}]}
                 {:key :w
                  :title :window
                  :items [{:key :n :title :new :command :netnewswire.window.new}
                          {:key :d :title :close :command :netnewswire.window.close}
                          {:key :0 :title :main :command :netnewswire.window.main}
                          {:key :m :title :minimize :command :netnewswire.window.minimize}
                          {:key :z :title :zoom :command :netnewswire.window.zoom}
                          {:key :f :title :fill :command :netnewswire.window.fill}
                          {:key :c :title :center :command :netnewswire.window.center}
                          {:key :s :title :full-screen :command :netnewswire.window.full-screen}
                          {:key :i :title :info :command :netnewswire.window.info}
                          {:key :l :title :activity-log :command :netnewswire.window.activity}
                          {:key :e :title :error-log :command :netnewswire.window.errors}]}]})

;; Mirrors the SPC leader in ELispMachine's init.el, so the same keys work from
;; any evil state. A leaf :emacs.NAME runs the Emacs command NAME (emacs.fnl).
(local emacs-keymap
       [{:key :x :title :M-x :command :emacs.execute-extended-command}
        {:key :tab :title :last-buffer :command :emacs.evil-switch-to-windows-last-buffer}
        {:key :o :title :find-file :command :emacs.find-file}
        {:key :b
         :title :buffer
         :items [{:key :b :title :switch :command :emacs.consult-buffer}
                 {:mods [:shift] :key :b :title :ibuffer :command :emacs.ibuffer}
                 {:key :d :title :kill :command :emacs.kill-current-buffer}
                 {:key :n :title :next :command :emacs.next-buffer}
                 {:key :p :title :previous :command :emacs.previous-buffer}
                 {:key :r :title :rename :command :emacs.rename-buffer}
                 {:mods [:shift] :key :r :title :revert :command :emacs.revert-buffer}
                 {:key "-" :title :bury :command :emacs.bury-buffer}]}
        {:key :f
         :title :file
         :items [{:key :f :title :find :command :emacs.find-file}
                 {:key :r :title :recent :command :emacs.consult-recent-file}
                 {:key :s :title :save :command :emacs.save-buffer}
                 {:mods [:shift] :key :s :title :save-all :command :emacs.evil-write-all}
                 {:key :a :title :save-as :command :emacs.write-file}
                 {:mods [:shift] :key :r :title :rename :command :emacs.rename-visited-file}
                 {:key :j :title :dired :command :emacs.dired-jump}
                 {:key :d :title :directory :command :emacs.consult-dir}
                 {:key :p :title :project-buffer :command :emacs.consult-project-buffer}
                 {:key :g
                  :title :find/grep
                  :items [{:key :r :title :ripgrep :command :emacs.consult-ripgrep}
                          {:key :g :title :git-grep :command :emacs.consult-git-grep}
                          {:key :d :title :fd :command :emacs.consult-fd}]}
                 {:key :e
                  :title :emacs
                  :items [{:key :d :title :init :command :emacs.visit-init-dot-el}
                          {:mods [:shift] :key :r :title :reload-init :command :emacs.eval-init-dot-el}]}]}
        {:key :w
         :title :window
         :items [{:key :h :title :left :command :emacs.evil-window-left}
                 {:key :j :title :down :command :emacs.evil-window-down}
                 {:key :k :title :up :command :emacs.evil-window-up}
                 {:key :l :title :right :command :emacs.evil-window-right}
                 {:key :s :title :split :command :emacs.evil-window-split}
                 {:key :v :title :vsplit :command :emacs.evil-window-vsplit}
                 {:key :d :title :delete :command :emacs.delete-window}
                 {:mods [:shift] :key :d :title :ace-delete :command :emacs.ace-delete-window}
                 {:key :w :title :other :command :emacs.other-window}
                 {:key "=" :title :balance :command :emacs.balance-windows}
                 {:key :u :title :undo :command :emacs.winner-undo}
                 {:mods [:shift] :key :u :title :redo :command :emacs.winner-redo}
                 {:key :t :title :transpose :command :emacs.transpose-frame}
                 {:key :x :title :kill-buffer-and-window :command :emacs.kill-buffer-and-window}]}
        {:key :t
         :title :tab
         :items [{:key :n :title :next :command :emacs.tab-next}
                 {:key :p :title :previous :command :emacs.tab-previous}
                 {:mods [:shift] :key :n :title :new :command :emacs.tab-new}
                 {:key :d :title :close :command :emacs.tab-close}
                 {:mods [:shift] :key :d :title :close-others :command :emacs.tab-close-other}
                 {:key :r :title :rename :command :emacs.tab-rename}
                 {:key :t :title :list :command :emacs.tab-list}
                 {:key :tab :title :recent :command :emacs.tab-recent}
                 {:key :f :title :find-file :command :emacs.find-file-other-tab}
                 {:key :b :title :buffer :command :emacs.switch-to-buffer-other-tab}
                 {:key :y :title :duplicate :command :emacs.tab-duplicate}]}
        {:key :j
         :title :jump
         :items [{:key :j :title :char :command :emacs.avy-goto-char-timer}
                 {:key :l :title :link :command :emacs.ace-link}
                 {:mods [:shift] :key :l :title :lines :command :emacs.consult-line-multi}
                 {:key :u :title :url :command :emacs.elipsm/avy-goto-url}]}
        {:key :g
         :title :git
         :items [{:key :s :title :status :command :emacs.magit-status}
                 {:key :d :title :diff :command :emacs.magit-diff-dwim}
                 {:key :m :title :dispatch :command :emacs.magit-dispatch}
                 {:key :c :title :commit :command :emacs.magit-commit-create}
                 {:key :p :title :push :command :emacs.magit-push}
                 {:mods [:shift] :key :p :title :pull :command :emacs.magit-pull}
                 {:key :f :title :file-dispatch :command :emacs.magit-file-dispatch}]}
        {:mods [:shift]
         :key :p
         :title :project
         :items [{:key :p :title :switch :command :emacs.project-switch-project}
                 {:key :f :title :find-file :command :emacs.project-find-file}
                 {:key :b :title :buffer :command :emacs.project-switch-to-buffer}
                 {:key :g :title :regexp :command :emacs.project-find-regexp}
                 {:key :d :title :dired :command :emacs.project-dired}
                 {:key :c :title :compile :command :emacs.project-compile}
                 {:key :k :title :kill-buffers :command :emacs.project-kill-buffers}]}
        {:key :h
         :title :help
         :items [{:key :h :title :help :command :emacs.help}
                 {:key :f :title :function :command :emacs.helpful-function}
                 {:key :v :title :variable :command :emacs.helpful-variable}
                 {:key :k :title :key :command :emacs.helpful-key}
                 {:key :c :title :command :command :emacs.helpful-command}
                 {:key :s :title :symbol :command :emacs.helpful-symbol}
                 {:key "." :title :at-point :command :emacs.helpful-at-point}
                 {:key :m :title :mode :command :emacs.describe-mode}
                 {:key :b :title :bindings :command :emacs.describe-bindings}
                 {:mods [:shift] :key :m :title :keymap :command :emacs.describe-keymap}
                 {:key :x :title :char :command :emacs.describe-char}
                 {:key :p :title :package :command :emacs.describe-package}]}
        {:key :y
         :title :yank
         :items [{:key :y :title :pop :command :emacs.consult-yank-pop}
                 {:key :r :title :replace :command :emacs.consult-yank-replace}
                 {:key :i :title :from-history :command :emacs.consult-yank-from-kill-ring}]}
        {:key :a
         :title :utilities
         :items [{:key :m
                  :title :emms
                  :items [{:key :e
                           :title :emms
                           :items [{:key :e :title :emms :command :emacs.emms}
                                   {:key :s :title :play-pause :command :emms.play-or-pause}
                                   {:key :h :title :previous :command :emms.previous
                                    :repeatable true}
                                   {:key :l :title :next :command :emms.next
                                    :repeatable true}
                                   {:key :p :title :previous :command :emms.previous
                                    :repeatable true}
                                   {:key :n :title :next :command :emms.next
                                    :repeatable true}
                                   {:key "," :title :back-10s :command :emms.seek-backward
                                    :repeatable true}
                                   {:key "." :title :forward-10s :command :emms.seek-forward
                                    :repeatable true}
                                   {:mods [:shift] :key "," :title :back-1m
                                    :command :emms.seek-backward-more :repeatable true}
                                   {:mods [:shift] :key "." :title :forward-1m
                                    :command :emms.seek-forward-more :repeatable true}
                                   {:key :j :title :volume-down :command :emms.volume-down
                                    :repeatable true}
                                   {:key :k :title :volume-up :command :emms.volume-up
                                    :repeatable true}
                                   {:key :m :title :mute :command :emms.mute}
                                   {:key :i :title :now-playing :command :emms.now-playing}
                                   {:key :d :title :play-directory :command :emacs.emms-play-directory}
                                   {:key :f :title :play-file :command :emacs.emms-play-file}
                                   {:key :u :title :play-url :command :emacs.emms-play-url}]}]}
                 {:key :o
                  :title :org
                  :items [{:key :o :title :agenda :command :emacs.org-agenda}
                          {:key :a :title :agenda-list :command :emacs.org-agenda-list}
                          {:key :c :title :capture :command :emacs.org-capture}
                          {:key :t :title :todo-list :command :emacs.org-todo-list}
                          {:key :s :title :search :command :emacs.org-search-view}
                          {:key :m :title :tags :command :emacs.org-tags-view}
                          {:key :l :title :store-link :command :emacs.org-store-link}
                          {:mods [:shift]
                           :key :c
                           :title :clock
                           :items [{:key :i :title :in :command :emacs.org-clock-in}
                                   {:key :o :title :out :command :emacs.org-clock-out}
                                   {:key :g :title :goto :command :emacs.org-clock-goto}
                                   {:key :c :title :cancel :command :emacs.org-clock-cancel}]}
                          {:key :j
                           :title :journal
                           :items [{:key :j :title :new-entry :command :emacs.org-journal-new-entry}
                                   {:key :f :title :current :command :emacs.org-journal-open-current-journal-file}]}
                          {:key :r
                           :title :org-roam
                           :items [{:key :f :title :find :command :emacs.org-roam-node-find}
                                   {:key :c :title :capture :command :emacs.org-roam-capture}
                                   {:key :i :title :insert :command :emacs.org-roam-node-insert}
                                   {:key :l :title :buffer :command :emacs.org-roam-buffer-toggle}]}
                          {:key :d
                           :title :dailies
                           :items [{:key :c :title :capture-today :command :emacs.org-roam-dailies-capture-today}
                                   {:key :d :title :goto-date :command :emacs.org-roam-dailies-goto-date}
                                   {:key :g
                                    :title :goto
                                    :items [{:key "." :title :today :command :emacs.org-roam-dailies-goto-today}
                                            {:key :y :title :yesterday :command :emacs.org-roam-dailies-goto-yesterday}
                                            {:key :t :title :tomorrow :command :emacs.org-roam-dailies-goto-tomorrow}]}]}]}]}
        {:mods [:shift]
         :key :l
         :title :LLMs
         :items [{:key :l :title :chat :command :emacs.gptel}
                 {:key :s :title :send :command :emacs.gptel-send}
                 {:key :m :title :menu :command :emacs.gptel-menu}
                 {:key :q :title :abort :command :emacs.gptel-abort}
                 {:key :r :title :rewrite :command :emacs.gptel-rewrite}
                 {:key :a :title :add-file :command :emacs.gptel-add-file}
                 {:key "." :title :add :command :emacs.gptel-add}]}
        {:mods [:shift]
         :key :t
         :title :toggle
         :items [{:key :l :title :line-numbers :command :emacs.display-line-numbers-mode}
                 {:key :f :title :fullscreen :command :emacs.mac-non-native-toggle-frame-fullscreen}
                 {:key :t :title :theme :command :emacs.consult-theme}
                 {:key :w :title :writeroom :command :emacs.writeroom-mode}
                 {:key :m :title :mode-line :command :emacs.hide-mode-line-mode}
                 {:key :p :title :padding :command :emacs.spacious-padding-mode}
                 {:mods [:shift] :key :d :title :debug-on-error :command :emacs.toggle-debug-on-error}]}
        {:key :q
         :title :quit
         :items [{:key :f :title :delete-frame :command :emacs.delete-frame}]}])

(local emacs-mode
       {:id :emacs-mode
        :title :Emacs
        :bundle-ids [:org.gnu.Emacs]
        :commands (merge (emacs.commands-for emacs-keymap) (. (require :mpv) :commands))
        :keymap emacs-keymap})

(local config {:title "Main Menu"
               :items menu-items
               :keys agnostic-keys
               :local-leader modes.local-leader
               :enter (fn []
                        (windows.hide-display-numbers))
               :exit (fn []
                       (windows.hide-display-numbers))
               : apps
               :modes [safari-mode chatgpt-mode facetime-mode finder-mode netnewswire-mode
                       emacs-mode]
               :hyper {:key :F18}
               :grid {:size :4x2}
               :modules {:windows {:center-ratio "80:50"}}})

;; REPL =============================================
;; ==================================================

;; For editors to drive Hammerspoon (bin/hs-fennel-repl); loaded here to open
;; their REPLs again after a reload.
(require :repl)

;; Exports ==========================================
;; ==================================================

config
