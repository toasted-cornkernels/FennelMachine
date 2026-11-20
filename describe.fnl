"
Emacs-style help commands, under `Cmd-SPC h`:

  k  describe-key        read a chord (or a whole leader sequence) and say what
                         it runs: a Hammerspoon binding, a menu item of the
                         frontmost app or a macOS keyboard shortcut
  b  describe-bindings   every key Hammerspoon, the frontmost app's menus and
                         macOS bind right now
  w  where-is            pick a command and show the keys that run it
  .  describe-element    the accessibility element that has keyboard focus
  p  describe-element-at-pointer   the one under the mouse pointer
  m  describe-mode       the frontmost app's mode keymap
  f  describe-function   a command, config function or Hammerspoon API item:
                         its documentation, source and keys
  a  apropos-command     commands whose name or documentation has some words
  e  view-echo-area-messages   past echo area messages and errors (*Messages*)
  =  describe-char       the character after the insertion point, or each one
                         of the selection: codepoint, Unicode name, UTF-8
  A  describe-app        the frontmost app: bundle, process, windows, mode
  W  describe-window     the focused window: frame, screen, Space, state

Short answers go to the echo area, long ones to the help viewer (lib/help.fnl).
"
(local hotkeys (require :lib.hotkeys))
(local help (require :lib.help))
(local minibuffer (require :lib.minibuffer))
(local apps (require :lib.apps))
(local modes (require :lib.modes))

(local leader-action "lib.modal:activate-modal")
(local local-leader-action "lib.modal:activate-local-leader")
(local read-seconds 10)
(local home (os.getenv :HOME))

(local layer-names {:global "Global Hammerspoon key"
                    :leader "Leader menu"
                    :hotkey "Hammerspoon hotkey"
                    :system "macOS Keyboard Shortcuts"})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn concat [xs ys]
  (let [acc (icollect [_ x (ipairs xs)] x)]
    (each [_ y (ipairs ys)] (table.insert acc y))
    acc))

(fn width [s] (or (utf8.len s) (length s)))

(fn pad [s w] (.. s (string.rep " " (math.max 0 (- w (width s))))))

(fn short-path [file]
  (if (= (file:sub 1 (length home)) home)
      (.. "~" (file:sub (+ (length home) 1)))
      file))

(fn location-string [loc]
  (when loc (.. (short-path loc.file) ":" loc.line)))

(fn first-line [s] (when s (s:match "^[^\n]*")))

(fn clean-title [title]
  (let [t (: (tostring (or title "")) :gsub "^%+" "")]
    (t:match "^%s*(.-)%s*$")))

(fn front-name []
  (let [app (hs.application.frontmostApplication)]
    (or (and app (: app :name)) "the frontmost app")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bindings
;;
;; Every binding is an entry {:keys :layer :title :name :fn :doc :source}.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn resolve-action [action]
  "The function ACTION (a function or a \"module:function\" string) runs."
  (if (= (type action) :function) action
      (= (type action) :string)
      (let [(module-name fn-name) (action:match "^(.-):(.+)$")]
        (when module-name
          (let [(ok module) (pcall require module-name)]
            (when (and ok (= (type module) :table))
              (. module fn-name)))))))

(fn function-name [f]
  "A name for F found by looking it up in the loaded modules, e.g.
  hs.toggleConsole or launcher:show."
  (when (= (type f) :function)
    (var found nil)
    (each [k v (pairs hs) &until found]
      (when (= v f) (set found (.. "hs." k))))
    (each [module-name module (pairs package.loaded) &until found]
      ;; Skip internal modules such as hs.libcaffeinate in favour of the public
      ;; one (hs.caffeinate) that exports the same function.
      (when (and (= (type module) :table)
                 (not (module-name:match "^hs%.lib"))
                 (not (module-name:match "%._")))
        (pcall #(each [k v (pairs module) &until found]
                  (when (and (= v f) (= (type k) :string))
                    (set found (if (= module-name :_G) k
                                   (module-name:match "^hs")
                                   (.. module-name "." k)
                                   (.. module-name ":" k))))))))
    found))

(fn fn-entry [keys layer title name f ?source]
  (let [source (or (hotkeys.fn-location f) ?source)]
    {: keys : layer : title :fn f
     :name (or name (function-name f) (location-string source)
               "anonymous function")
     :doc (hotkeys.docstring f)
     : source}))

(fn action-entry [keys layer title action]
  (fn-entry keys layer title (when (= (type action) :string) action)
            (resolve-action action)))

(fn chord-of [item]
  (let [mods (or item.mods [])]
    (if (hs.fnutils.contains mods :hyper)
        (.. (hotkeys.chord [] (or (?. (get-config) :hyper :key) :F18)) " "
            (hotkeys.chord (icollect [_ m (ipairs mods)] (when (not= m :hyper) m))
                           item.key))
        (hotkeys.chord mods item.key))))

(fn leader-chord [?action]
  (let [action (or ?action leader-action)
        item (hs.fnutils.find (or (. (get-config) :keys) [])
                              #(= $1.action action))]
    (when item (chord-of item))))

(fn global-entries []
  (icollect [_ item (ipairs (or (. (get-config) :keys) []))]
    (action-entry (chord-of item) :global
                  (or item.title (when (= item.action leader-action) "Leader menu"))
                  item.action)))

(fn app-entries []
  (let [app (apps.get-app)]
    (icollect [_ item (ipairs (or (and app app.keys) []))]
      (doto (action-entry (chord-of item) :app item.title item.action)
        (tset :app app.key)))))

(fn leader-root []
  (let [app (apps.get-app)]
    (if (and app app.items (> (length app.items) 0)) app (get-config))))

(fn bound? [item]
  (and item.key (not= item.key "") (or item.action item.items item.menu-fn)))

(fn submenu-of [item]
  "The menu ITEM opens: its :items, or what its :menu-fn builds (quietly)."
  (if item.items item
      item.menu-fn (let [(ok built) (pcall item.menu-fn true)]
                     (when ok built))))

(fn mode-command [id]
  (hs.fnutils.find (modes.list-commands) #(= $1.id id)))

(fn leaf-entry [keys path item]
  (if item.command
      (let [command (mode-command item.command)]
        (fn-entry keys :leader
                  (table.concat (concat path [(or (?. command :title) item.command)])
                                " › ")
                  item.command (?. command :fn)))
      (let [title (clean-title item.title)
            title (if (and (= title "") (= (type item.action) :string))
                      (or (item.action:match ":(.+)$") item.action)
                      title)]
        (action-entry keys :leader (table.concat (concat path [title]) " › ")
                      item.action))))

(fn leader-entries []
  (let [acc []
        chord (leader-chord)]
    (fn walk [menu prefix path]
      (each [_ item (ipairs (or menu.items []))]
        (when (and (bound? item) (not= item.action :previous))
          (let [keys (.. prefix " " (chord-of item))]
            (if (or item.items item.menu-fn)
                (let [sub (submenu-of item)]
                  (when sub
                    (walk sub keys (concat path [(clean-title item.title)]))))
                (table.insert acc (leaf-entry keys path item)))))))
    (when chord (walk (leader-root) chord []))
    acc))

(fn hotkey-entry [record]
  (fn-entry record.chord :hotkey record.message nil record.fn
            (or record.location record.caller)))

(fn other-hotkey-entries [known]
  "Enabled hotkeys not described by config.fnl, e.g. those bound in code."
  (icollect [_ record (ipairs (hotkeys.active))]
    (when (not (. known record.chord))
      (hotkey-entry record))))

;; Menu key equivalents arrive as a character, a Carbon menu glyph code or a
;; private-use function-key character.
(local glyph-keys {2 :tab 4 :padenter 9 :space 10 :forwarddelete 11 :return
                   23 :delete 27 :escape 98 :pageup 100 :left 101 :right
                   102 :home 104 :up 105 :end 106 :down 107 :pagedown})
(local function-chars {0xF700 :up 0xF701 :down 0xF702 :left 0xF703 :right
                       0xF728 :forwarddelete 0xF729 :home 0xF72B :end
                       0xF72C :pageup 0xF72D :pagedown})
(local control-chars {"\t" :tab "\r" :return "\3" :padenter " " :space
                      "\127" :delete "\8" :delete "\27" :escape})
(local shifted-chars {"!" "1" "@" "2" "#" "3" "$" "4" "%" "5" "^" "6" "&" "7"
                      "*" "8" "(" "9" ")" "0" "_" "-" "+" "=" "{" "[" "}" "]"
                      "|" "\\" ":" ";" "\"" "'" "<" "," ">" "." "?" "/" "~" "`"})

(fn glyph-key [glyph]
  (when (= (type glyph) :number)
    (or (. glyph-keys glyph)
        (when (<= 111 glyph 122) (.. "f" (- glyph 110)))
        (when (<= 135 glyph 137) (.. "f" (- glyph 122)))
        (when (<= 159 glyph 162) (.. "f" (- glyph 143))))))

(fn menu-chord [item]
  "The key equivalent of menu ITEM (from getMenuItems) in Emacs notation."
  (let [mods (icollect [_ m (ipairs (or item.AXMenuItemCmdModifiers []))] m)
        char item.AXMenuItemCmdChar
        code (when (and (= (type char) :string) (not= char ""))
               (let [(ok cp) (pcall utf8.codepoint char 1)] (when ok cp)))
        key (or (glyph-key item.AXMenuItemCmdGlyph)
                (when code
                  (or (. function-chars code)
                      (when (<= 0xF704 code 0xF71B) (.. "f" (- code 0xF703)))
                      (. control-chars char)
                      (let [base (. shifted-chars char)]
                        (when base
                          (table.insert mods :shift)
                          base))
                      (char:lower))))]
    (when key (hotkeys.chord mods key))))

(fn menu-items->entries [app-name items]
  (let [acc []]
    (fn walk [items path]
      (each [_ item (ipairs (or items []))]
        (let [title item.AXTitle
              label (if (and (= (type title) :string) (not= title ""))
                        (if path (.. path " › " title) title)
                        path)
              submenu (?. item :AXChildren 1)]
          (if submenu
              (walk submenu label)
              (let [keys (and label (menu-chord item))]
                (when keys
                  (table.insert acc {: keys :layer :menu :app app-name
                                     :title (if item.AXEnabled label
                                                (.. label " (disabled)"))
                                     :name label
                                     :enabled item.AXEnabled})))))))
    (walk items nil)
    acc))

(fn with-menu-entries [k]
  "Call K with the frontmost app's menu items that have a key equivalent.
  Reading big menus takes a second or two, so it happens on a timer to let the
  echo area show what is going on first. (getMenuItems' callback form does the
  same work on the main thread and does not always call back.)"
  (let [app (hs.application.frontmostApplication)]
    (if (not app)
        (k [])
        (hs.timer.doAfter
         0.05
         #(let [(ok entries) (pcall #(menu-items->entries (: app :name)
                                                          (: app :getMenuItems)))]
            (when (not ok)
              (print (.. "describe: reading menus failed: " (tostring entries))))
            (k (if ok entries [])))))))

(fn system-entries []
  (let [system-shortcuts (require :system-shortcuts)]
    (icollect [_ s (ipairs (system-shortcuts.shortcuts))]
      (when (and s.binding s.enabled)
        (let [title (if s.category (.. s.category " › " s.name) s.name)]
          {:keys s.binding :layer :system : title :name title})))))

(fn hammerspoon-entries []
  "Global keys, the frontmost app's keys, other hotkeys and the leader tree."
  (let [globals (global-entries)
        app-keys (app-entries)
        known {}]
    (each [_ e (ipairs (concat globals app-keys))]
      (tset known e.keys true))
    {:globals globals
     :app-keys app-keys
     :hotkeys (other-hotkey-entries known)
     :leader (leader-entries)}))

(fn layer-name [entry]
  (case entry.layer
    :app (.. entry.app " keys (Hammerspoon, while " entry.app " is frontmost)")
    :menu (.. entry.app "'s menu bar")
    layer (or (. layer-names layer) layer)))

(fn summary [entry]
  "A one-line description such as `s-SPC SPC runs launcher:show (Run)`."
  (.. entry.keys " runs "
      (case entry.layer
        :menu (.. "the menu item " entry.title " in " entry.app)
        :system (.. "the macOS shortcut " entry.title)
        _ (.. entry.name (if (and entry.title (not= entry.title entry.name))
                             (.. " (" entry.title ")")
                             "")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; describe-key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(var reader nil)

(fn stop-reading []
  (when reader
    (: reader.tap :stop)
    (: reader.timer :stop)
    (set reader nil)))

(fn read-chord [prompt k]
  "Echo PROMPT and call K with the next chord typed. The chord never reaches
  Hammerspoon's hotkeys, the app or macOS. C-g or a timeout quits."
  (stop-reading)
  (minibuffer.message prompt read-seconds)
  (let [tap (hs.eventtap.new
             [hs.eventtap.event.types.keyDown]
             (fn [event]
               (let [chord (hotkeys.event-chord event)]
                 (stop-reading)
                 (hs.timer.doAfter 0 #(if (= chord "C-g")
                                          (minibuffer.message "Quit")
                                          (k chord)))
                 true)))]
    (set reader {: tap
                 :timer (hs.timer.doAfter read-seconds
                                          #(do (stop-reading)
                                               (minibuffer.message "Quit")))})
    (: tap :start)))

(fn field [label value]
  [[(pad (.. "  " label) 14) :dim] [(tostring value) :fg]])

(fn show-entry [entry ?shadowed]
  "Describe ENTRY in the help viewer, or in the echo area if a line will do."
  (if (or entry.doc entry.source (and ?shadowed (> (length ?shadowed) 0)))
      (let [lines [[[entry.keys :key] [" runs " :fg] [entry.name :heading]]
                   ""
                   (field "Layer" (layer-name entry))]]
        (when (and entry.title (not= entry.title entry.name))
          (table.insert lines (field "Title" entry.title)))
        (when entry.source
          (table.insert lines (field "Source" (location-string entry.source))))
        (when entry.doc
          (table.insert lines "")
          (each [l (string.gmatch (.. entry.doc "\n") "(.-)\n")]
            (table.insert lines (.. "  " l))))
        (when (and ?shadowed (> (length ?shadowed) 0))
          (table.insert lines "")
          (table.insert lines [["Also bound to (shadowed by the above)" :heading]])
          (each [_ e (ipairs ?shadowed)]
            (table.insert lines [[(.. "  " (layer-name e) ": ") :dim]
                                 [e.title :fg]])))
        (minibuffer.clear)
        (help.show {:title (.. "describe-key: " entry.keys)
                    : lines
                    :source entry.source}))
      (minibuffer.message (summary entry))))

(fn find-keys [entries chord]
  "The entry bound to CHORD, preferring enabled ones."
  (or (hs.fnutils.find entries #(and (= $1.keys chord) (not= $1.enabled false)))
      (hs.fnutils.find entries #(= $1.keys chord))))

(fn describe-leader [menu keys path]
  (read-chord
   (.. "Describe key: " keys " -")
   (fn [chord]
     (let [item (hs.fnutils.find (or menu.items [])
                                 #(and (bound? $1) (= (chord-of $1) chord)))
           keys (.. keys " " chord)]
       (if (not item)
           (minibuffer.message (.. keys " is undefined"))
           (= item.action :previous)
           (minibuffer.message (.. keys " goes back to the previous menu"))
           (or item.items item.menu-fn)
           (let [sub (submenu-of item)]
             (if sub
                 (describe-leader sub keys (concat path [(clean-title item.title)]))
                 (minibuffer.message (.. keys " (" (clean-title item.title)
                                         ") has no keymap in " (front-name)))))
           (show-entry (leaf-entry keys path item)))))))

(fn describe-chord [chord]
  "Describe CHORD: Hammerspoon first, then macOS shortcuts, then the frontmost
  app's menus, which is the order they get to handle a key in."
  (if (and (hotkeys.lookup chord) (= chord (leader-chord)))
      (describe-leader (leader-root) chord [])
      (and (hotkeys.lookup chord) (= chord (leader-chord local-leader-action)))
      (let [menu (modes.local-leader true)]
        (if menu
            (describe-leader menu chord ["Mode"])
            (minibuffer.message (.. chord " opens the local leader; "
                                    (front-name) " has no mode"))))
      (do
        (minibuffer.message (.. "Looking up " chord "…") read-seconds)
        (with-menu-entries
         (fn [menus]
           (let [record (hotkeys.lookup chord)
                 hammerspoon (when record
                               (or (find-keys (concat (app-entries) (global-entries))
                                              chord)
                                   (hotkey-entry record)))
                 layers [hammerspoon
                         (find-keys (system-entries) chord)
                         (find-keys menus chord)]
                 [primary & shadowed] (fcollect [i 1 3] (. layers i))]
             (if primary
                 (show-entry primary shadowed)
                 (minibuffer.message
                  (.. chord " is not bound by Hammerspoon, macOS shortcuts or "
                      (front-name) "'s menus")))))))))

(fn describe-key []
  "Read a key (or a leader key sequence) and describe what it runs."
  (help.close)
  (read-chord "Describe key (C-g to quit): " describe-chord))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; describe-bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn section [lines title entries]
  (when (> (length entries) 0)
    (let [key-width (math.min 26 (accumulate [w 0 _ e (ipairs entries)]
                                   (math.max w (width e.keys))))]
      (when (> (length lines) 0) (table.insert lines ""))
      (table.insert lines [[title :heading]])
      (each [_ e (ipairs entries)]
        (let [desc (or e.title e.name)
              extra (if (and e.title e.name (not= e.title e.name)) e.name nil)]
          (table.insert lines [[(.. "  " (pad e.keys key-width) "  ") :key]
                               [desc :fg]
                               [(if extra (.. "  " extra) "") :dim]]))))))

(fn describe-bindings []
  "Show every key binding in effect: Hammerspoon's, the frontmost app's menu
  shortcuts and macOS keyboard shortcuts."
  (let [{: globals : app-keys :hotkeys others : leader} (hammerspoon-entries)
        app-name (front-name)
        lines []]
    (section lines "Global keys (Hammerspoon)" globals)
    (section lines (.. app-name " keys (Hammerspoon)") app-keys)
    (section lines "Other Hammerspoon hotkeys" others)
    (section lines "Leader menu" leader)
    (section lines "macOS Keyboard Shortcuts" (system-entries))
    (minibuffer.message (.. "Reading " app-name "'s menus…") read-seconds)
    (with-menu-entries
     (fn [menus]
       (section lines (.. app-name " menu bar") menus)
       (minibuffer.clear)
       (help.show {:title (.. "describe-bindings: " app-name) : lines})))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; where-is
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn other-mode-entries []
  "Leader keys of the modes of apps that are not frontmost."
  (let [chord (leader-chord)
        front (hs.application.frontmostApplication)
        bundle-id (and front (: front :bundleID))
        acc []]
    (each [_ mode (ipairs (modes.all-modes))]
      (when (not (hs.fnutils.contains mode.bundle-ids bundle-id))
        (fn walk [nodes prefix]
          (each [_ node (ipairs nodes)]
            (let [keys (.. prefix " " (hotkeys.chord node.mods node.key))]
              (if node.items
                  (walk node.items keys)
                  (let [command (mode-command node.command)]
                    (table.insert acc
                                  (doto (fn-entry keys :leader
                                                  (?. command :title)
                                                  node.command (?. command :fn))
                                    (tset :context (.. "in " mode.title)))))))))
        (walk (or mode.keymap []) (.. chord " m"))))
    acc))

(fn where-is-candidates [menus]
  (let [{: globals : app-keys :hotkeys others : leader} (hammerspoon-entries)
        groups {}
        order []]
    (each [_ entries (ipairs [globals app-keys others leader (other-mode-entries)
                              (system-entries) menus])]
      (each [_ e (ipairs entries)]
        (let [id (.. (if (= e.layer :menu) "menu:" (= e.layer :system) "macos:" "")
                     e.name)
              keys (if e.context (.. e.keys " (" e.context ")") e.keys)]
          (if (. groups id)
              (table.insert (. groups id :keys) keys)
              (do
                (tset groups id {:entry e :keys [keys]})
                (table.insert order id))))))
    (icollect [_ id (ipairs order)]
      (let [{: entry : keys} (. groups id)]
        {:text entry.name
         :keywords (or entry.title "")
         :annotation (.. (if (and entry.title (not= entry.title entry.name))
                             (.. entry.title "  ")
                             "")
                         (case entry.layer
                           :menu "menu  " :system "macOS  " _ "")
                         (table.concat keys ", "))
         : entry
         : keys}))))

(fn where-is []
  "Read a command in the minibuffer and show the keys that run it."
  (help.close)
  (minibuffer.completing-read
   "Where is command: " nil
   {:on-accept (fn [candidate input]
                 (if candidate
                     (minibuffer.message (.. candidate.entry.name " is on "
                                             (table.concat candidate.keys ", "))
                                         6)
                     (when (and input (not= input ""))
                       (minibuffer.message (.. input " is not on any key")))))})
  (hs.timer.doAfter
   0.01
   #(when (minibuffer.active?)
      (minibuffer.set-collection (where-is-candidates []))
      (with-menu-entries
       (fn [menus]
         (when (minibuffer.active?)
           (minibuffer.set-collection (where-is-candidates menus))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; describe-element
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn attr [element name]
  (let [(ok value) (pcall #(: element :attributeValue name))]
    (when ok value)))

(fn element-label [element]
  (let [role (or (attr element :AXRole) "?")
        title (or (attr element :AXTitle) (attr element :AXDescription))]
    (if (and (= (type title) :string) (not= title ""))
        (.. role " “" title "”")
        role)))

(fn one-line [s max]
  (let [s (: (tostring s) :gsub "\n" "⏎")]
    (if (> (width s) max)
        (.. (s:sub 1 (or (utf8.offset s max) (length s))) "…")
        s)))

(fn format-value [value]
  (if (= (type value) :userdata)
      (if (= (getmetatable value) (hs.getObjectMetatable "hs.axuielement"))
          (element-label value)
          (tostring value))
      (= (type value) :table)
      (if (and value.x value.y value.w value.h)
          (string.format "x=%d y=%d w=%d h=%d" value.x value.y value.w value.h)
          (and value.x value.y)
          (string.format "x=%d y=%d" value.x value.y)
          (and value.w value.h)
          (string.format "w=%d h=%d" value.w value.h)
          (= (length value) 0)
          (if (next value) (one-line (hs.inspect value {:newline " " :indent ""}) 120)
              "[]")
          (let [shown (icollect [i v (ipairs value) &until (> i 3)]
                        (format-value v))]
            (.. "[" (length value) "] " (table.concat shown ", ")
                (if (> (length value) 3) ", …" ""))))
      (= (type value) :string) (one-line (.. "\"" value "\"") 160)
      (tostring value)))

(fn element-lines [element]
  (let [(ok attr-names) (pcall #(: element :attributeNames))
        attr-names (if ok (or attr-names []) [])
        (_ actions) (pcall #(: element :actionNames))
        (_ params) (pcall #(: element :parameterizedAttributeNames))
        name-width (accumulate [w 0 _ n (ipairs attr-names)] (math.max w (width n)))
        lines [[["Attributes" :heading]]]]
    (table.sort attr-names)
    (each [_ name (ipairs attr-names)]
      (let [value (attr element name)]
        (table.insert lines [[(.. "  " (pad name name-width) "  ") :key]
                             [(if (= value nil) "nil" (format-value value))
                              (if (= value nil) :dim :fg)]])))
    (when (and actions (> (length actions) 0))
      (table.insert lines "")
      (table.insert lines [["Actions" :heading]])
      (each [_ action (ipairs actions)]
        (let [(_ desc) (pcall #(: element :actionDescription action))]
          (table.insert lines [[(.. "  " (pad action name-width) "  ") :key]
                               [(or desc "") :fg]]))))
    (when (and params (> (length params) 0))
      (table.insert lines "")
      (table.insert lines [["Parameterized attributes" :heading]])
      (table.insert lines (.. "  " (table.concat params ", "))))
    (table.insert lines "")
    (table.insert lines [["Ancestors" :heading]])
    (var parent (attr element :AXParent))
    (var depth 0)
    (while (and parent (< depth 30))
      (table.insert lines (.. "  " (element-label parent)))
      (set parent (attr parent :AXParent))
      (set depth (+ depth 1)))
    (let [(ok pid) (pcall #(: element :pid))
          app (and ok pid (hs.application.applicationForPID pid))]
      (when app
        (table.insert lines "")
        (table.insert lines (field "App" (.. (: app :name) " ("
                                             (tostring (: app :bundleID)) ", pid "
                                             pid ")")))))
    lines))

(fn show-element [element what]
  (if element
      (help.show {:title (.. "describe-element: " (element-label element) " (" what ")")
                  :lines (element-lines element)})
      (minibuffer.message (.. "No accessibility element " what))))

(fn focused-element []
  "The element with keyboard focus. The system-wide query sometimes comes back
  empty right after an app switch, so ask the frontmost app first."
  (let [front (hs.application.frontmostApplication)
        app (and front (hs.axuielement.applicationElement front))]
    (or (and app (attr app :AXFocusedUIElement))
        (attr (hs.axuielement.systemWideElement) :AXFocusedUIElement)
        (and app (attr app :AXFocusedWindow)))))

(fn describe-element []
  "Describe the accessibility element that has keyboard focus."
  (show-element (focused-element) "focused"))

(fn describe-element-at-pointer []
  "Describe the accessibility element under the mouse pointer."
  (show-element (hs.axuielement.systemElementAtPosition (hs.mouse.absolutePosition))
                "at pointer"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; describe-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn describe-mode []
  "Show the frontmost app's mode keymap."
  (let [description (modes.describe-mode)]
    (if (= description "No mode")
        (minibuffer.message (.. "No mode for " (front-name)))
        (help.show {:title (.. "describe-mode: " (front-name))
                    :lines [description]}))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; describe-function and apropos-command
;;
;; Functions are {:name :kind :fn :doc :source :title :keys} where :kind is
;; :command (bound to a key), :launcher, :function (exported by one of the
;; config's modules) or :api (Hammerspoon's reference documentation).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn own-file? [file]
  (let [dir (.. hs.configdir "/")]
    (and file (= (file:sub 1 (length dir)) dir))))

(fn module-functions []
  "Functions exported by the config's own modules, named like actions."
  (let [acc []]
    (each [module-name module (pairs package.loaded)]
      (when (and (= (type module) :table) (= (type module-name) :string)
                 (not (module-name:match "^hs[%._]")))
        (pcall #(each [k v (pairs module)]
                  (when (and (= (type k) :string) (= (type v) :function))
                    (let [loc (hotkeys.fn-location v)]
                      (when (and loc (own-file? loc.file))
                        (table.insert acc {:name (.. module-name ":" k) :kind :function
                                           :fn v :source loc
                                           :doc (hotkeys.docstring v)}))))))))
    (table.sort acc #(< $1.name $2.name))
    acc))

(fn launcher-commands []
  (let [(ok items) (pcall #((. (require :launcher) :system-commands)))]
    (icollect [_ c (ipairs (if ok items []))]
      {:name c.text :kind :launcher :fn c.run :keywords c.keywords
       :doc (hotkeys.docstring c.run) :source (hotkeys.fn-location c.run)})))

(fn commands []
  "Bound commands, launcher commands and the config's functions, merged by name
  and by function so each appears once with all its keys."
  (let [by-name {}
        by-fn {}
        order []]
    (fn add [c]
      (let [known (or (. by-name c.name) (and c.fn (. by-fn c.fn)))]
        (if known
            (do (each [_ k (ipairs (or c.keys []))]
                  (when (not (hs.fnutils.contains known.keys k))
                    (table.insert known.keys k)))
              (each [_ field (ipairs [:fn :doc :source :title :keywords])]
                (when (= nil (. known field))
                  (tset known field (. c field)))))
            (let [c (doto c (tset :keys (or c.keys [])))]
              (tset by-name c.name c)
              (when c.fn (tset by-fn c.fn c))
              (table.insert order c)))))
    (each [_ candidate (ipairs (where-is-candidates []))]
      (let [e candidate.entry]
        (when (not= e.layer :system)
          (add {:name e.name :kind :command :fn e.fn :doc e.doc :source e.source
                :title (when (not= e.title e.name) e.title)
                :keys (icollect [_ k (ipairs candidate.keys)] k)}))))
    (each [_ c (ipairs (launcher-commands))] (add c))
    (each [_ c (ipairs (module-functions))] (add c))
    order))

(var api-cache nil)

(fn api-item [path node module?]
  "An :api function for the hs.doc NODE at PATH. Its text starts with a
  `Type: signature` line, except for modules."
  (let [text (tostring node)
        (api-type signature) (text:match "^(%a+): ([^\n]*)")
        api-type (if module? :Module (or api-type :Item))
        name (if (= api-type :Method) (path:gsub "%.([^%.]+)$" ":%1") path)
        body (if module? text (or (text:match "^[^\n]*\n%s*\n(.*)$") ""))]
    {: name :kind :api : api-type
     :module (or (path:match "^(.+)%.[^%.]+$") path)
     :signature (when (not module?) signature)
     :desc (first-line (body:match "^%s*(.-)%s*$"))
     :doc (body:match "^(.-)%s*$")}))

(fn hs-api []
  "Every module and item of the reference documentation bundled with
  Hammerspoon (hs.doc), built once: it takes about half a second."
  (when (not api-cache)
    (set api-cache [])
    (let [tree (getmetatable hs.doc)]
      (fn walk [path node]
        (each [_ name (ipairs (or (tree._children path) []))]
          (let [full (.. path "." name)
                child (. node name)
                kids (tree._children full)]
            (when child
              (if (and kids (> (length kids) 0))
                  (do (table.insert api-cache (api-item full child true))
                    (walk full child))
                  (table.insert api-cache (api-item full child false)))))))
      (each [_ root (ipairs [:hs :spoon])]
        (let [(ok node) (pcall #(. hs.doc root))]
          (when (and ok node) (pcall walk root node)))))
    (table.sort api-cache #(< $1.name $2.name)))
  api-cache)

(fn api-function [item]
  "The Lua value ITEM documents, if it is a plain function reachable from _G."
  (when (not= item.api-type :Method)
    (var value _G)
    (each [part (item.name:gmatch "[^%.]+") &until (not= (type value) :table)]
      (set value (let [(ok v) (pcall #(. value part))] (when ok v))))
    (when (= (type value) :function) value)))

(fn kind-phrase [c]
  (case c.kind
    :command "a command"
    :launcher "a launcher command"
    :function (.. "a function in " (or (c.name:match "^(.-):") "the config"))
    :api (if (= c.api-type :Module) "a Hammerspoon module"
             (.. "a Hammerspoon " (c.api-type:lower) " in " c.module))
    _ "a function"))

(fn describe-function-lines [c ?bound]
  (let [keys (or (and (> (length (or c.keys [])) 0) c.keys)
                 (and ?bound ?bound.keys))
        lines [[[c.name :heading] [(.. " is " (kind-phrase c) ".") :fg]] ""]]
    (when c.signature
      (table.insert lines [[(.. "  " c.signature) :key]])
      (table.insert lines ""))
    (when c.title (table.insert lines (field "Title" c.title)))
    (table.insert lines (field "Keys" (if (and keys (> (length keys) 0))
                                          (table.concat keys ", ")
                                          "not on any key")))
    (when c.source
      (table.insert lines (field "Source" (location-string c.source))))
    (let [doc (or c.doc (and ?bound ?bound.doc))]
      (table.insert lines "")
      (if doc
          (each [l (string.gmatch (.. doc "\n") "(.-)\n")]
            (table.insert lines (.. "  " l)))
          (table.insert lines [["  Not documented." :dim]])))
    lines))

(fn show-function [c all]
  (let [bound (when (= c.kind :api)
                (let [f (api-function c)]
                  (when f (hs.fnutils.find all #(= $1.fn f)))))
        ;; A command that is a named function, e.g. Lock Screen runs
        ;; hs.caffeinate.lockScreen: describe that too.
        runs (when (and c.fn (not= c.kind :api))
               (let [name (function-name c.fn)]
                 (when (and name (not= name c.name)) name)))
        api (when runs (hs.fnutils.find (hs-api) #(= $1.name runs)))
        lines (describe-function-lines
               (if api
                   (setmetatable {:signature (or c.signature api.signature)
                                  :doc (or c.doc api.doc)}
                                 {:__index c})
                   c)
               bound)]
    (when runs
      (table.insert lines 3 (field "Runs" runs)))
    (help.show {:title (.. "describe-function: " c.name)
                : lines
                :source c.source})))

(fn function-annotation [c]
  (if (> (length (or c.keys [])) 0)
      (table.concat c.keys ", ")
      (one-line (or (first-line c.doc) c.desc c.title
                    (case c.kind :launcher "launcher command" _ c.kind))
                70)))

(fn describe-function []
  "Read a command, function or Hammerspoon API name and describe it."
  (help.close)
  (let [all (commands)]
    (minibuffer.completing-read
     "Describe function: " nil
     {:on-accept (fn [candidate input]
                   (if candidate
                       (show-function candidate.command all)
                       (when (and input (not= input ""))
                         (minibuffer.message (.. "No function named " input)))))})
    (hs.timer.doAfter
     0.01
     #(when (minibuffer.active?)
        (let [acc (icollect [_ c (ipairs all)]
                    {:text c.name
                     :keywords (.. (or c.title "") " " (or c.keywords ""))
                     :annotation (function-annotation c)
                     :command c})]
          (let [by-name (collect [_ c (ipairs all)] c.name c)]
            (each [_ c (ipairs (hs-api))]
              (let [command (. by-name c.name)]
                (if command
                    ;; A bound API function, e.g. hs.toggleConsole: one entry
                    ;; with both its keys and its reference documentation.
                    (do (set command.signature c.signature)
                      (when (not command.doc) (set command.doc c.doc)))
                    (table.insert acc {:text c.name
                                       :annotation (.. c.api-type "  "
                                                       (one-line (or c.desc "") 60))
                                       :command c})))))
          (minibuffer.set-collection acc))))))

(fn apropos-matches [words]
  (icollect [_ c (ipairs (commands))]
    (let [haystack (: (table.concat [c.name (or c.title "") (or c.doc "")
                                     (or c.keywords "")]
                                    " ")
                      :lower)]
      (when (accumulate [all true _ w (ipairs words)]
              (and all (not= nil (haystack:find w 1 true))))
        c))))

(fn apropos [input]
  (let [lower (input:lower)
        words (icollect [w (lower:gmatch "%S+")] w)
        matches (apropos-matches words)]
    (table.sort matches #(< $1.name $2.name))
    (if (= (length matches) 0)
        (minibuffer.message (.. "No command matches “" input "”"))
        (let [lines []]
          (each [_ c (ipairs matches)]
            (table.insert lines [[c.name :heading]
                                 [(if (> (length c.keys) 0)
                                      (.. "  " (table.concat c.keys ", "))
                                      "")
                                  :key]])
            (table.insert lines [[(.. "  " (or (first-line c.doc) c.title
                                               (.. (kind-phrase c) ", not documented")))
                                  (if (or c.doc c.title) :fg :dim)]]))
          (help.show {:title (.. "apropos-command: " input " (" (length matches) ")")
                      : lines})))))

(fn apropos-command []
  "Read words and list the commands whose name, title or documentation
  contains all of them."
  (help.close)
  (minibuffer.completing-read
   "Apropos command (words): " []
   {:on-accept (fn [_ input]
                 (if (and input (input:find "%S"))
                     (apropos input)
                     (minibuffer.message "Quit")))}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; view-echo-area-messages
;;
;; minibuffer.message prints every message to the console, which therefore
;; already is *Messages*; this reads the messages (and errors) back from it.
;; Like the console, it starts over when the config reloads.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local error-face "#f7768e")
(local max-messages 500)

(fn error-line? [text]
  (or (text:find "ERROR") (text:find "[Ee]rror:") (text:find "stack traceback")))

(fn messages []
  "{:time :text :error?} for the messages and errors in the console."
  (let [acc []]
    (var last nil)
    (each [line (string.gmatch (.. (or (hs.console.getConsole) "") "\n") "(.-)\n")]
      (let [(time rest) (line:match "^%d+%-%d+%-%d+ (%d+:%d+:%d+): (.*)$")]
        (if time
            (let [message (rest:match "^message: (.*)$")]
              (set last (if message {: time :text message}
                            (error-line? rest) {: time
                                                :text (-> rest
                                                          (: :gsub "^%s*%d+:%d+:%d+%s+" "")
                                                          (: :match "^%s*(.-)$"))
                                                :error? true}
                            nil))
              (when last (table.insert acc last)))
            (when (and last (line:find "%S"))
              (table.insert acc {:time "" :text line :error? last.error?})))))
    (if (> (length acc) max-messages)
        (fcollect [i (- (length acc) max-messages -1) (length acc)] (. acc i))
        acc)))

(fn view-echo-area-messages []
  "Show the echo area's past messages, and errors, like Emacs' *Messages*."
  (let [all (messages)]
    (if (= (length all) 0)
        (minibuffer.message "No messages yet")
        (do
          (minibuffer.clear)
          (help.show {:title (.. "*Messages* (" (length all) ")")
                      :at-end true
                      :lines (icollect [_ m (ipairs all)]
                               [[(pad m.time 8) :dim] ["  " :fg]
                                [m.text (if m.error? error-face :fg)]])})))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; describe-char
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local max-chars 300)

(fn utf16-offset->index [s units]
  "The byte index in S of the character UNITS UTF-16 code units in."
  (var u 0)
  (var found nil)
  (let [(ok) (pcall #(each [i c (utf8.codes s) &until found]
                       (if (>= u units)
                           (set found i)
                           (set u (+ u (if (>= c 0x10000) 2 1))))))]
    (when ok found)))

(fn text-at-point []
  "The focused element's selected text, or else the character after its
  insertion point, like Emacs' char-after. Also returns :selection or :point."
  (let [element (focused-element)
        selected (and element (attr element :AXSelectedText))]
    (if (and (= (type selected) :string) (not= selected ""))
        (values selected :selection)
        (let [range (and element (attr element :AXSelectedTextRange))
              value (and element (attr element :AXValue))
              location (and (= (type range) :table) (or range.location range.loc))
              i (and location (= (type value) :string)
                     (utf16-offset->index value location))]
          (when i
            (values (value:sub i (- (or (utf8.offset value 2 i) (+ (length value) 1)) 1))
                    :point))))))

(fn unicode-names [codepoints]
  "Unicode names of CODEPOINTS, from ICU's Any-Name transform."
  (let [js (.. "ObjC.import('Foundation');"
               "ObjC.unwrap($.NSString.stringWithString(String.fromCodePoint("
               (table.concat codepoints ",")
               ")).stringByApplyingTransformReverse('Any-Name', false))")
        (ok result) (hs.osascript.javascript js)
        names (if (and ok (= (type result) :string))
                  (icollect [name (result:gmatch "\\N{(.-)}")] name)
                  [])]
    (if (= (length names) (length codepoints)) names [])))

(fn shown-char [cp]
  "How to show codepoint CP: control characters as ^X, combining marks on ◌."
  (if (< cp 32) (.. "^" (utf8.char (+ cp 64)))
      (= cp 127) "^?"
      (or (<= 0x0300 cp 0x036F) (<= 0x20D0 cp 0x20FF)) (.. "◌" (utf8.char cp))
      (utf8.char cp)))

(fn char-info [cp name]
  {:char (shown-char cp)
   :code (string.format "U+%04X" cp)
   :name (or name "?")
   :utf8 (table.concat (icollect [_ b (ipairs [(string.byte (utf8.char cp) 1 -1)])]
                         (string.format "%02X" b))
                       " ")
   :decimal cp})

(fn describe-char []
  "Describe the character after the insertion point, or each character of the
  selection: its codepoint, Unicode name and UTF-8 bytes."
  (let [(text where) (text-at-point)]
    (if (not text)
        (minibuffer.message (.. "No text at point in " (front-name)))
        (let [codepoints (let [(ok cps) (pcall #(icollect [_ c (utf8.codes text)] c))]
                           (if ok cps []))
              shown (fcollect [i 1 (math.min max-chars (length codepoints))]
                      (. codepoints i))
              names (unicode-names shown)
              infos (icollect [i cp (ipairs shown)] (char-info cp (. names i)))]
          (if (= (length infos) 0)
              (minibuffer.message "Not valid UTF-8")
              (and (= (length infos) 1) (= where :point))
              (let [c (. infos 1)]
                (minibuffer.message
                 (.. "Char: " c.char "  " c.code "  " c.name "  (" c.decimal
                     ", UTF-8 " c.utf8 ")")
                 8))
              (let [lines [(.. (length codepoints) " characters, " (length text)
                               " bytes in UTF-8"
                               (if (> (length codepoints) max-chars)
                                   (.. "; the first " max-chars " are shown")
                                   ""))
                           ""]]
                (each [_ c (ipairs infos)]
                  (table.insert lines [[(pad (.. "  " c.char) 6) :fg]
                                       [(pad c.code 10) :key]
                                       [(pad c.name 44) :fg]
                                       [c.utf8 :dim]]))
                (help.show {:title (.. "describe-char: "
                                       (if (= where :selection) "selection" "point"))
                            : lines})))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; describe-app and describe-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn yes-no [x] (if x "yes" "no"))

(fn window-flags [win]
  (let [flags (icollect [_ [f label] (ipairs [[:isMinimized "minimized"]
                                               [:isFullScreen "full screen"]
                                               [:isStandard nil]])]
                (let [(ok v) (pcall #(: win f))]
                  (if (= f :isStandard)
                      (when (and ok (not v)) "not standard")
                      (when (and ok v) label))))]
    (if (> (length flags) 0) (.. "  (" (table.concat flags ", ") ")") "")))

(fn describe-app []
  "Describe the frontmost app: its bundle, process, windows, mode and keys."
  (let [app (hs.application.frontmostApplication)]
    (if (not app)
        (minibuffer.message "No frontmost app")
        (let [name (: app :name)
              path (: app :path)
              bundle-id (: app :bundleID)
              info (or (and path (hs.application.infoForBundlePath path)) {})
              mode (hs.fnutils.find (modes.all-modes)
                                    #(hs.fnutils.contains $1.bundle-ids bundle-id))
              windows (or (: app :allWindows) [])
              focused (: app :focusedWindow)
              lines [[[name :heading]
                      [(.. "  " (or info.CFBundleShortVersionString "")
                           (if info.CFBundleVersion (.. " (" info.CFBundleVersion ")") ""))
                       :dim]]
                     ""
                     (field "Bundle ID" (tostring bundle-id))
                     (field "Path" (if path (short-path path) "?"))
                     (field "PID" (: app :pid))
                     (field "Kind" (case (: app :kind)
                                     1 "regular (in the Dock)"
                                     0 "accessory (no Dock icon)"
                                     _ "background"))
                     (field "Hidden" (yes-no (: app :isHidden)))
                     (field "Mode" (if mode
                                       (.. mode.title "  (" (leader-chord) " m)")
                                       "none"))
                     (field "App keys" (.. (length (app-entries)) " Hammerspoon keys"))]]
          (table.insert lines "")
          (table.insert lines [[(.. "Windows (" (length windows) ")") :heading]])
          (each [_ win (ipairs windows)]
            (table.insert lines
                          [[(if (= win focused) "* " "  ") :key]
                           [(let [t (: win :title)]
                              (if (and t (not= t "")) t "(untitled)"))
                            :fg]
                           [(window-flags win) :dim]]))
          (minibuffer.message (.. "Reading " name "'s menus…") read-seconds)
          (with-menu-entries
           (fn [menus]
             (table.insert lines "")
             (table.insert lines (field "Menu keys"
                                        (.. (length menus) " (" (leader-chord)
                                            " h b lists them)")))
             (minibuffer.clear)
             (help.show {:title (.. "describe-app: " name) : lines})))))))

(fn describe-window []
  "Describe the focused window: its frame, screen, Space and state."
  (let [win (hs.window.focusedWindow)]
    (if (not win)
        (minibuffer.message (.. "No focused window in " (front-name)))
        (let [app (: win :application)
              frame (: win :frame)
              screen (: win :screen)
              (spaces-ok spaces) (pcall #(hs.spaces.windowSpaces win))
              (screen-spaces-ok screen-spaces) (pcall #(hs.spaces.spacesForScreen screen))
              space-text (if (and spaces-ok spaces (> (length spaces) 0))
                             (table.concat
                              (icollect [_ id (ipairs spaces)]
                                (let [index (and screen-spaces-ok screen-spaces
                                                 (hs.fnutils.indexOf screen-spaces id))]
                                  (if index
                                      (.. "Desktop " index " of " (length screen-spaces)
                                          " (id " id ")")
                                      (.. "id " id))))
                              ", ")
                             "?")
              title (: win :title)
              element (hs.axuielement.windowElement win)
              (tabs-ok tabs) (pcall #(: win :tabCount))
              lines [[[(if (and title (not= title "")) title "(untitled)") :heading]]
                     ""
                     (field "App" (if app (.. (: app :name) " ("
                                             (tostring (: app :bundleID)) ")")
                                      "?"))
                     (field "ID" (tostring (: win :id)))
                     (field "Role" (.. (tostring (: win :role)) " / "
                                       (tostring (: win :subrole))))
                     (field "Frame" (format-value frame))
                     (field "Screen" (if screen
                                         (.. (: screen :name) " ("
                                             (format-value (: screen :frame)) ")")
                                         "?"))
                     (field "Space" space-text)
                     (field "Standard" (yes-no (: win :isStandard)))
                     (field "Visible" (yes-no (: win :isVisible)))
                     (field "Minimized" (yes-no (: win :isMinimized)))
                     (field "Full screen" (yes-no (: win :isFullScreen)))]]
          (when (and tabs-ok tabs (> tabs 0))
            (table.insert lines (field "Tabs" tabs)))
          (when element
            (let [document (attr element :AXDocument)]
              (when (and (= (type document) :string) (not= document ""))
                (table.insert lines (field "Document" (one-line document 90))))))
          (help.show {:title (.. "describe-window: " (one-line (or title "") 60))
                      : lines})))))

{: describe-key
 : describe-bindings
 : where-is
 : describe-element
 : describe-element-at-pointer
 : describe-mode
 : describe-function
 : apropos-command
 : view-echo-area-messages
 : describe-char
 : describe-app
 : describe-window
 : summary
 : hammerspoon-entries
 : with-menu-entries
 : commands
 : hs-api
 : messages}
