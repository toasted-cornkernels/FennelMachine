"
The commands of System Settings › Keyboard › Keyboard Shortcuts, runnable
from Hammerspoon whether or not they have a shortcut and whether or not it is
enabled.

The list comes from System Settings' own table in KeyboardSettings.appex, so
it follows the installed macOS and its language. Each command is a symbolic
hotkey. `run` temporarily binds it to a spare chord through the private CGS
API (called from JavaScript for Automation), posts that chord, then restores
the user's binding. These changes live in the WindowServer only; the
preferences are never written.

`execute` reads a command in the minibuffer, annotated with its category and
current shortcut. The Services of the frontmost app are offered too, since
they make up the Services category. App Shortcuts are ordinary menu items,
which `hints:search` already finds.
"

(local minibuffer (require :lib.minibuffer))
(local which-key (require :lib.which-key))

(local log (hs.logger.new "system-shortcuts.fnl" "info"))

(local resources
       "/System/Library/ExtensionKit/Extensions/KeyboardSettings.appex/Contents/Resources/")

;; Hyper-fn-F19: a chord nothing else uses.
(local spare-keycode 80)
(local spare-flags {:ctrl true :alt true :shift true :cmd true :fn true})
(local spare-mask (+ 0x40000 0x80000 0x20000 0x100000 0x800000))
(local restore-delay 0.25)

;; Entries for hardware this config doesn't run on.
(local skipped-identifiers {:touchbar.save-capture-as-file true
                            :touchbar.save-capture-to-clipboard true
                            :TrackpadHandwriting true})

(local cgs-prelude
       "ObjC.bindFunction('CGSIsSymbolicHotKeyEnabled', ['bool', ['int']]);
ObjC.bindFunction('CGSSetSymbolicHotKeyEnabled', ['int', ['int', 'bool']]);
ObjC.bindFunction('CGSGetSymbolicHotKeyValue',
  ['int', ['int', 'unsigned short *', 'unsigned short *', 'unsigned int *']]);
ObjC.bindFunction('CGSSetSymbolicHotKeyValue',
  ['int', ['int', 'unsigned short', 'unsigned short', 'unsigned int']]);
function get(id) {
  var c = Ref(), k = Ref(), m = Ref();
  $.CGSGetSymbolicHotKeyValue(id, c, k, m);
  return {enabled: $.CGSIsSymbolicHotKeyEnabled(id), char: c[0], key: k[0],
          mods: m[0]};
}
function set(id, v) {
  $.CGSSetSymbolicHotKeyValue(id, v.char, v.key, v.mods);
  $.CGSSetSymbolicHotKeyEnabled(id, v.enabled);
}
")

(fn cgs [body]
  "Run BODY after `cgs-prelude`, returning its value decoded from JSON."
  (let [(ok result raw) (hs.osascript.javascript (.. cgs-prelude body))]
    (if ok
        (hs.json.decode result)
        (log.e "CGS call failed:" (hs.inspect raw)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The table of commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn read-plist [path]
  (let [file (io.open path)]
    (when file
      (let [text (file:read :a)]
        (file:close)
        (hs.plist.readString text)))))

(fn localizations []
  "Names in the user's language, e.g. All windows → Mission Control."
  (let [tables (or (hs.plist.read (.. resources "DefaultShortcutsTable.loctable"))
                   {})
        langs (icollect [_ lang (ipairs (hs.host.locale.preferredLanguages))]
                (lang:gsub "-" "_"))]
    (var found nil)
    (each [_ lang (ipairs langs) &until found]
      (set found (or (. tables lang) (. tables (lang:match "^[^_]+")))))
    (or found tables.en {})))

(fn user-space-count []
  (accumulate [n 0 _ spaces (pairs (or (hs.spaces.allSpaces) {}))]
    (+ n (length (icollect [_ space (ipairs spaces)]
                   (when (= (hs.spaces.spaceType space) :user) space))))))

(var cache nil)

(fn commands []
  "Every command as {:id :name :category}, in System Settings' order."
  (when (not cache)
    (let [names (localizations)
          acc []]
      (fn name-of [entry]
        (let [raw (entry.name:gsub "^DO_NOT_LOCALIZE: " "")]
          (or (. names raw) raw)))
      (fn walk [entries category]
        (each [_ entry (ipairs entries)]
          (when (not (. skipped-identifiers entry.identifier))
            (if entry.elements
                (walk entry.elements
                      (if (or (not category) (= category (name-of entry)))
                          (or category (name-of entry))
                          (.. category " › " (name-of entry))))
                entry.sybmolichotkey
                (table.insert acc {:id entry.sybmolichotkey
                                   :name (name-of entry)
                                   :category category})))))
      (walk (or (read-plist (.. resources "en.lproj/DefaultShortcutsTable.xml"))
                [])
            nil)
      (let [spaces (or (read-plist (.. resources "DefaultSpacesShortcuts.xml"))
                       [])
            mission-control (or (. names "Expose and Spaces") "Mission Control")]
        (each [_ entry (ipairs spaces)]
          (table.insert acc {:id entry.sybmolichotkey
                             :name (name-of entry)
                             :category mission-control
                             :space true})))
      (set cache acc)))
  ;; Like System Settings, only offer the desktops that exist.
  (let [desktops (user-space-count)]
    (var n 0)
    (icollect [_ c (ipairs cache)]
      (if c.space
          (do (set n (+ n 1))
            (when (<= n desktops) c))
          c))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Running commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Original bindings of the commands whose spare binding is still in place.
(local pending {})

(fn post-spare-chord []
  (let [event hs.eventtap.event]
    (each [_ down? (ipairs [true false])]
      (: (doto (event.newKeyEvent spare-keycode down?)
           (: :setFlags spare-flags))
         :post))))

(fn restore [id]
  (let [p (. pending id)]
    (when p
      (tset pending id nil)
      (: p.timer :stop)
      (cgs (string.format "set(%d, %s); %s" id (hs.json.encode p.saved) "'{}'")))))

(fn run [id]
  "Run the symbolic hotkey ID as if its shortcut had been pressed."
  (let [saved (if (. pending id)
                  (. pending id :saved)
                  (cgs (string.format "var saved = get(%d);
                                       set(%d, {enabled: true, char: 65535,
                                                key: %d, mods: %d});
                                       JSON.stringify(saved)"
                                      id id spare-keycode spare-mask)))]
    (if (not saved)
        (minibuffer.message "Could not run the shortcut")
        (do
          (when (. pending id) (: (. pending id :timer) :stop))
          (tset pending id {: saved
                            :timer (hs.timer.doAfter restore-delay
                                                     #(restore id))})
          (post-spare-chord)))))

(fn find-command [name]
  (let [name (name:lower)]
    (var found nil)
    (each [_ c (ipairs (commands)) &until found]
      (when (= (c.name:lower) name) (set found c)))
    found))

(fn action [name]
  "An action running the command called NAME, for config.fnl."
  (fn []
    (let [c (find-command name)]
      (if c (run c.id) (minibuffer.message (.. "No such shortcut: " name))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading a command in the minibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local plain-keys {:space true :tab true :return true :escape true :delete true
                   :left true :right true :up true :down true})

(fn describe-binding [value]
  "A binding read through CGS in Emacs notation, e.g. C-<f2>; nil if unset."
  (let [key (and value (not= value.key 65535) (. hs.keycodes.map value.key))]
    (when key
      (let [mods (icollect [_ [bit mod] (ipairs [[0x40000 :ctrl] [0x80000 :alt]
                                                 [0x20000 :shift]
                                                 [0x100000 :cmd]])]
                   (when (not= 0 (band value.mods bit)) mod))
            ;; Function and arrow keys always carry the fn flag.
            fn-key? (and (not= 0 (band value.mods 0x800000))
                         (= (length key) 1))
            key (if (and (> (length key) 1) (not (. plain-keys key)))
                    (.. "<" key ">")
                    key)]
        (.. (if fn-key? "fn-" "") (which-key.emacs-key mods key))))))

(fn bindings [ids]
  (or (cgs (.. "JSON.stringify(" (hs.json.encode ids) ".map(get))")) []))

(fn services-menu-items [services-name]
  "Enabled items of the frontmost app's Services menu."
  (let [app (hs.application.frontmostApplication)
        bar (and app (: (hs.axuielement.applicationElement app)
                        :attributeValue :AXMenuBar))
        app-menu (and bar (. (or (: bar :attributeValue :AXChildren) []) 2))
        menu (and app-menu (. (or (: app-menu :attributeValue :AXChildren) []) 1))
        acc []]
    (each [_ item (ipairs (or (and menu (: menu :attributeValue :AXChildren)) []))]
      (when (hs.fnutils.contains [services-name :Services]
                                 (: item :attributeValue :AXTitle))
        (let [submenu (. (or (: item :attributeValue :AXChildren) []) 1)]
          (each [_ service (ipairs (or (and submenu
                                            (: submenu :attributeValue :AXChildren))
                                       []))]
            (let [title (: service :attributeValue :AXTitle)]
              (when (and (= (type title) :string) (not= title "")
                         (: service :attributeValue :AXEnabled)
                         (= 0 (length (or (: service :attributeValue :AXChildren)
                                          []))))
                (table.insert acc {: title :element service})))))))
    acc))

(fn candidates []
  (let [cmds (commands)
        current (bindings (icollect [_ c (ipairs cmds)] c.id))
        acc (icollect [i c (ipairs cmds)]
              (let [value (. current i)
                    binding (describe-binding value)
                    shortcut (if (and binding value.enabled) binding
                                 binding (.. binding " (off)")
                                 "")]
                {:text c.name
                 :annotation (.. c.category (if (= shortcut "") "" "  ")
                                 shortcut)
                 :command c}))
        services-name (or (. (localizations) :Services) :Services)]
    (each [_ s (ipairs (services-menu-items services-name))]
      (table.insert acc {:text s.title
                         :annotation services-name
                         :service s.element}))
    acc))

(fn accept [candidate input]
  (if (not candidate)
      (when (not= input "") (minibuffer.message (.. "No shortcut " input)))
      candidate.command (run candidate.command.id)
      candidate.service
      (when (not (pcall #(: candidate.service :performAction :AXPress)))
        (minibuffer.message (.. "Could not run " candidate.text)))))

(fn execute []
  "Read a Keyboard Shortcuts command or a Service in the minibuffer and run it."
  (minibuffer.completing-read "Shortcut: " nil {:on-accept accept})
  (hs.timer.doAfter 0.01
                    #(when (minibuffer.active?)
                       (minibuffer.set-collection (candidates)))))

{: commands
 : run
 : action
 : execute}
