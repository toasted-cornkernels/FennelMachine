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

Short answers go to the echo area, long ones to the help viewer (lib/help.fnl).
"
(local hotkeys (require :lib.hotkeys))
(local help (require :lib.help))
(local minibuffer (require :lib.minibuffer))
(local apps (require :lib.apps))
(local modes (require :lib.modes))

(local leader-action "lib.modal:activate-modal")
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
      (when (= (type module) :table)
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

(fn leader-chord []
  (let [item (hs.fnutils.find (or (. (get-config) :keys) [])
                              #(= $1.action leader-action))]
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
  [[(pad (.. "  " label) 10) :dim] [(tostring value) :fg]])

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

(fn describe-element []
  "Describe the accessibility element that has keyboard focus."
  (let [app (hs.axuielement.applicationElement (hs.application.frontmostApplication))]
    (show-element (or (attr app :AXFocusedUIElement)
                      (attr (hs.axuielement.systemWideElement) :AXFocusedUIElement)
                      (attr app :AXFocusedWindow))
                  "focused")))

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

{: describe-key
 : describe-bindings
 : where-is
 : describe-element
 : describe-element-at-pointer
 : describe-mode
 : summary
 : hammerspoon-entries
 : with-menu-entries}
