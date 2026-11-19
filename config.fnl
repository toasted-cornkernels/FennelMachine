(require-macros :lib.macros)
(require-macros :lib.advice.macros)
(local windows (require :windows))
(local emacs (require :emacs))
(local slack (require :slack))
(local vim (require :vim))
(local input-method (require :input-method))
(local display (require :display))
(local modes (require :lib.modes))
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
        {:key :space :title :Run :action "launcher:show"}
        {:key :y :title "Clipboard History" :action "clipboard:show"}
        {:key :w
         :title :Window
         :enter "windows:enter-window-menu"
         :exit "windows:exit-window-menu"
         :items window-bindings}
        {:key :a :title :Apps :items app-bindings}
        {:key :p :title :Media :items media-bindings}
        {:key :m :title :Mode :menu-fn modes.local-leader}
        {:key :x :title :Emacs :items emacs-bindings}
        {:key :d :title :Display :items display-bindings}
        {:key :k
         :title "Keyboard Shortcuts"
         :action "system-shortcuts:execute"}])

(local agnostic-keys [{:mods [:cmd]
                       :key :space
                       :action "lib.modal:activate-modal"}
                      {:mods [:cmd :ctrl] :key "`" :action hs.toggleConsole}
                      {:mods [:cmd :shift]
                       :key :space
                       :action "hints:show-hints"}
                      {:mods [:cmd :shift]
                       :key :j
                       :action "hints:scroll-mode"}
                      {:mods [:ctrl :alt]
                       :key :space
                       :action "hints:search"}
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
;; `Cmd-SPC m` opens the keymap of the frontmost app's mode (see lib/modes.fnl).
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

(local config {:title "Main Menu"
               :items menu-items
               :keys agnostic-keys
               :enter (fn []
                        (windows.hide-display-numbers))
               :exit (fn []
                       (windows.hide-display-numbers))
               : apps
               :modes [safari-mode chatgpt-mode facetime-mode finder-mode netnewswire-mode]
               :hyper {:key :F18}
               :grid {:size :4x2}
               :modules {:windows {:center-ratio "80:50"}}})

;; nREPL server =====================================
;; ==================================================

;; (local repl (require :repl))
;; (repl.run (repl.start))

;; Exports ==========================================
;; ==================================================

config
