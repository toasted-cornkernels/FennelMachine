"
Emacs-style major modes for macOS applications.

A mode ties an application (matched by bundle id) to a keymap. The keymap is a
prefix trie in the same shape as the modal menus in config.fnl: nodes with
:items are named prefixes, nodes with :command are leaves that reference a
command in the global command registry.

  {:id :safari-mode
   :title :Safari
   :bundle-ids [:com.apple.Safari]
   :commands (. (require :safari) :commands)
   :keymap [{:key :l :title :address :command :safari.focus-address}
            {:key :t :title :toolbar
             :items [{:key :s :title :show :command :safari.toolbar.show}]}]}

Commands are {:title str :fn (fn [app] ...)} tables keyed by a dotted id. They
receive the application they act on and can be run without a key sequence via
execute-command.

`Cmd-Shift-,` (local-leader) resolves the frontmost app once, at entry, and opens
its keymap through lib.modal, which handles display, Back and Escape.
"
(local {: concat
        : contains?
        : filter
        : find
        : join
        : map
        : merge}
       (require :lib.functional))
(local modal (require :lib.modal))
(local which-key (require :lib.which-key))

(local log (hs.logger.new "modes.fnl" "info"))

(local leader-path "Cmd-Shift-,")

(local modes [])
(local commands {})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Registry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn notify
  [message]
  "Show MESSAGE after any pending modal cleanup has closed existing alerts."
  (hs.timer.doAfter 0 #(alert message)))

(fn mode-by-id
  [id]
  (find #(= $1.id id) modes))

(fn mode-for-app
  [app]
  (let [bundle-id (: app :bundleID)]
    (find #(contains? bundle-id $1.bundle-ids) modes)))

(fn running-app
  [mode]
  (var app nil)
  (each [_ bundle-id (ipairs mode.bundle-ids) &until app]
    (set app (. (hs.application.applicationsForBundleID bundle-id) 1)))
  app)

(fn check-keymap
  [mode nodes]
  (each [_ node (ipairs nodes)]
    (if node.items
        (check-keymap mode node.items)
        (not (. commands node.command))
        (log.wf "%s: key %s references unknown command %s"
                mode.id node.key (tostring node.command)))))

(fn register-mode
  [mode]
  "Add MODE and its commands to the registry, replacing a mode with its id."
  (each [i registered (ipairs modes)]
    (when (= registered.id mode.id)
      (table.remove modes i)))
  (table.insert modes mode)
  (each [id command (pairs (or mode.commands {}))]
    (when (and (. commands id) (not= (. commands id :mode) mode.id))
      (log.wf "Command %s from %s overrides %s"
              id mode.id (. commands id :mode)))
    (tset commands id (merge command {: id :mode mode.id})))
  (check-keymap mode (or mode.keymap [])))

(fn execute-command
  [id app]
  "
  Run the command registered as ID against APP, defaulting to the running
  application of the command's mode. Reports failures instead of raising.
  "
  (let [command (. commands id)
        app (or app (and command (running-app (mode-by-id command.mode))))]
    (if (not command)
        (notify (.. "Unknown command: " (tostring id)))
        (not app)
        (notify (.. id ": application is not running"))
        (let [(ok err) (pcall command.fn app)]
          (when (not ok)
            (log.ef "%s failed: %s" id err)
            (notify (.. id " failed")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn expand-path
  [path]
  "All concrete menu paths of PATH, whose elements may list alternative titles."
  (var paths [[]])
  (each [_ part (ipairs path)]
    (let [alternatives (if (= (type part) :table) part [part])
          extended []]
      (each [_ prefix (ipairs paths)]
        (each [_ title (ipairs alternatives)]
          (table.insert extended (concat prefix [title]))))
      (set paths extended)))
  paths)

(fn menu-path->string
  [path]
  (join " → " (map #(if (= (type $1) :table) (join "/" $1) $1) path)))

(fn menu-item
  [path]
  "
  Returns a command fn that selects the menu item at PATH, e.g.
  [\"View\" \"Customize Toolbar…\"]. An element may be a list of alternative
  titles for items that are renamed with their state, such as
  [\"View\" [\"Show Sidebar\" \"Hide Sidebar\"]]; whichever exists is selected,
  so such a command toggles. Checkbox items toggle as well.
  "
  (fn [app]
    (let [found (->> (expand-path path)
                     (map (fn [p] {:path p :item (: app :findMenuItem p)}))
                     (filter #$1.item))
          enabled (find #$1.item.enabled found)]
      (if enabled
          (: app :selectMenuItem enabled.path)
          (= (length found) 0)
          (notify (.. "Menu item not found: " (menu-path->string path)))
          (notify (.. "Unavailable now: " (menu-path->string path)))))))

(fn menu-item-matching
  [parent pattern]
  "
  Like menu-item for an item under the menu PARENT whose title embeds the
  selection, e.g. Copy “notes.txt” as Pathname. Selects the first enabled item
  whose title matches the Lua PATTERN, e.g. \"^Copy “.*” as Pathname$\".
  "
  (fn [app]
    (var items (: app :getMenuItems))
    (each [_ title (ipairs parent)]
      (set items (?. (find #(= $1.AXTitle title) (or items [])) :AXChildren 1)))
    (let [matches (filter #(string.find (or $1.AXTitle "") pattern)
                          (or items []))
          enabled (find #$1.AXEnabled matches)
          where (.. (menu-path->string parent) " → " pattern)]
      (if enabled
          (: app :selectMenuItem (concat parent [enabled.AXTitle]))
          (= (length matches) 0)
          (notify (.. "Menu item not found: " where))
          (notify (.. "Unavailable now: " where))))))

(fn find-element
  [root pred]
  "Depth-first search of an AX element tree. Returns the first match or nil."
  (if (pred root)
      root
      (do
        (var found nil)
        (each [_ child (ipairs (or (: root :attributeValue :AXChildren) []))
               &until found]
          (set found (find-element child pred)))
        found)))

(fn press-button
  [description]
  "
  Returns a command fn that sends AXPress to the button whose AXDescription is
  DESCRIPTION in the app's focused window.
  "
  (fn [app]
    (let [window (: app :focusedWindow)
          button (and window
                      (find-element
                       (hs.axuielement.windowElement window)
                       #(and (= (: $1 :attributeValue :AXRole) :AXButton)
                             (= (: $1 :attributeValue :AXDescription)
                                description))))]
      (if button
          (: button :performAction :AXPress)
          (notify (.. "Button not found: " description))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Undefined keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

"
hs.hotkey.modal only captures bound keys, so any other key would reach the
application while a mode menu is open. Like an undefined key in an Emacs prefix
map, an unbound key cancels the sequence instead: plain keys are swallowed and
Cmd chords pass through so shortcuts like Cmd-, keep working.
"

(local modifiers [:cmd :alt :shift :ctrl])
(local cancel-keys [{:key :escape :action :cancel}
                    {:mods [:ctrl] :key "[" :action :cancel}
                    {:mods [:ctrl] :key :g :action :back}])

(fn event->chord
  [event]
  (let [flags (: event :getFlags)]
    {:key (. hs.keycodes.map (: event :getKeyCode))
     :mods (filter #(. flags $1) modifiers)}))

(fn same-mods?
  [a b]
  (and (= (length a) (length b))
       (not (find #(not (contains? $1 b)) a))))

(fn bound?
  [items chord]
  (find #(and (or $1.action $1.items)
              (= (string.lower (tostring $1.key)) chord.key)
              (same-mods? (or $1.mods []) chord.mods))
        (concat items cancel-keys)))

(fn format-chord
  [{: key : mods}]
  (which-key.emacs-key mods key))

(fn guard-undefined-keys
  [path items]
  "Returns :enter and :exit lifecycle hooks guarding a menu's key sequence."
  (var tap nil)
  {:enter (fn []
            (set tap
                 (hs.eventtap.new
                  [hs.eventtap.event.types.keyDown]
                  (fn [event]
                    (let [chord (event->chord event)]
                      (if (bound? items chord)
                          false
                          (do
                            (hs.timer.doAfter 0 modal.deactivate-modal)
                            (if (contains? :cmd chord.mods)
                                false
                                (do
                                  (notify (.. path " " (format-chord chord)
                                              " is undefined"))
                                  true))))))))
            (: tap :start))
   :exit (fn []
           (when tap
             (: tap :stop)
             (set tap nil)))})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keymap -> modal menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn pad
  [str width]
  (.. str (string.rep " " (- width (length str)))))

(fn node->item
  [node width]
  "Title shown in the leader alert, derived from the trie and the registry."
  (if node.items
      (.. "+" node.title)
      (let [command (. commands node.command)]
        (.. (pad node.title width) "  "
            (if command command.title "(undefined command)")))))

(fn keymap->menu
  [mode app node path trail]
  "
  Convert keymap NODE into a lib.modal menu. Leaves run their command against
  APP, the application resolved when the local leader was entered.
  "
  (let [width (accumulate [w 0 _ child (ipairs node.items)]
                (math.max w (length child.title)))
        items (map (fn [child]
                     (if child.items
                         (keymap->menu mode app child
                                       (.. path " " child.key)
                                       (concat trail [child.title]))
                         {:key child.key
                          :mods child.mods
                          :title (node->item child width)
                          :command child.command
                          :repeatable child.repeatable
                          :action #(execute-command child.command app)}))
                   node.items)
        menu-items (concat [{:key "" :title (join " › " trail)}] items)
        guard (guard-undefined-keys path menu-items)]
    {:key node.key
     :mods node.mods
     :title (node->item node width)
     :mode mode.id
     :items menu-items
     :enter guard.enter
     :exit guard.exit}))

(fn local-leader
  [?quiet]
  "
  Menu resolver for the local leader, `Cmd-Shift-,`. Returns the frontmost application's mode
  keymap as a modal menu, or nil (after notifying, unless ?QUIET) when it has
  no mode.
  "
  (let [app (hs.application.frontmostApplication)
        mode (and app (mode-for-app app))]
    (if mode
        (keymap->menu mode app {:title mode.id :items mode.keymap}
                      leader-path [mode.id])
        (when (not ?quiet)
          (notify (if app
                      (.. "No mode for " (: app :name)
                          " (" (tostring (: app :bundleID)) ")")
                      "No frontmost application"))
          nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Introspection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn walk-keymap
  [nodes keys f]
  "Call (f keys node) for every node of a keymap trie, depth-first."
  (each [_ node (ipairs nodes)]
    (let [keys (concat keys [(which-key.emacs-key node.mods node.key)])]
      (f keys node)
      (when node.items
        (walk-keymap node.items keys f)))))

(fn describe-mode
  [?id]
  "
  Return a description of mode ?ID (default: the frontmost app's mode) listing
  every key sequence, prefix name and command.
  "
  (let [app (hs.application.frontmostApplication)
        mode (if ?id (mode-by-id ?id) (and app (mode-for-app app)))]
    (if (not mode)
        "No mode"
        (let [rows []]
          (walk-keymap mode.keymap []
                       (fn [keys node]
                         (table.insert
                          rows
                          [(join " " (concat [leader-path] keys))
                           (if node.items (.. "+" node.title) node.command)
                           (if node.items
                               ""
                               (or (?. commands node.command :title)
                                   "(undefined command)"))])))
          (let [width (fn [i]
                        (accumulate [w 0 _ row (ipairs rows)]
                          (math.max w (length (. row i)))))
                keys-width (width 1)
                command-width (width 2)]
            (join "\n"
                  (concat [(.. mode.id " (" (join ", " mode.bundle-ids) ")")]
                          (map (fn [[keys command title]]
                                 (.. "  " (pad keys keys-width)
                                     "  " (pad command command-width)
                                     "  " title))
                               rows))))))))

(fn describe-key
  [mode-id keys]
  "Return the keymap node bound to KEYS, e.g. [:t :s], in mode MODE-ID."
  (var nodes (?. (mode-by-id mode-id) :keymap))
  (var node nil)
  (each [_ key (ipairs keys) &until (not nodes)]
    (set node (find #(= $1.key key) nodes))
    (set nodes (and node node.items)))
  node)

(fn list-commands
  []
  "Return all registered commands sorted by id."
  (let [all (icollect [_ command (pairs commands)] command)]
    (table.sort all #(< $1.id $2.id))
    all))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn init
  [config]
  "Register the modes listed under :modes in config.fnl."
  (each [_ mode (ipairs (or config.modes []))]
    (register-mode mode))
  nil)

{: init
 : register-mode
 : execute-command
 : local-leader
 : menu-item
 : menu-item-matching
 : notify
 : press-button
 : describe-mode
 : describe-key
 : list-commands
 :all-modes (fn [] modes)}
