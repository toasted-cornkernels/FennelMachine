"
Displays the menu modals, sub-menus, and application-specific modals if set
in config.fnl.

We define a state machine, which uses our local states to determine states, and
transitions. Then we can send actions that may transition between specific
states defined in the table.

Allows us to create the machinery for displaying, entering, exiting, and
switching menus in one place which is then powered by config.fnl.
"
(local atom (require :lib.atom))
(local statemachine (require :lib.statemachine))
(local apps (require :lib.apps))
(local {: butlast
        : call-when
        : concat
        : conj
        : find
        : filter
        : has-some?
        : identity
        : join
        : map
        : merge}
       (require :lib.functional))
(local which-key (require :lib.which-key))
(local {:action->fn action->fn
        :bind-keys bind-keys}
       (require :lib.bind))
(local lifecycle (require :lib.lifecycle))

(local log (hs.logger.new "modal.fnl" "debug"))
(var fsm nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn timeout
  [f]
  "
  Create a pre-set timeout task that takes a function to run later.
  Takes a function to call after 2 seconds.
  Returns a function to destroy the timeout task.
  "
  (let [task (hs.timer.doAfter 2 f)]
    (fn destroy-task
      []
      (when task
        (: task :stop)
        nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Action dispatch functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn activate-modal
  [menu-key]
  "
  API to enter the main menu. Only effective when in the idle state (with no
  modal on screen)
  Side effectful
  "
  (fsm.send :activate menu-key))

(fn enter-modal
  [menu-key]
  "
  API to transition to the active state of our modal finite state machine
  It is called by a trigger set on the outside world and provided relevant
  context to determine which menu modal to activate.
  Takes the name of a menu to activate or nil if it's the root menu.
  menu-key refers to either a submenu key in config.fnl or an application
  specific menu key.
  Side effectful
  "
  (fsm.send :enter menu-key))


(fn deactivate-modal
  []
  "
  API to transition to the idle state of our modal finite state machine.
  Takes no arguments.
  Side effectful
  "
  (fsm.send :deactivate))


(fn activate-local-leader
  []
  "
  The local leader: open the frontmost app's mode keymap on top of the leader
  menu (so `SPC` and C-g still go back to it).
  "
  (deactivate-modal)
  (activate-modal)
  (enter-modal :local-leader))


(fn previous-modal
  []
  "
  API to transition to the previous modal in our history. Useful for returning
  to the main menu when in the window modal for instance.
  "
  (fsm.send :previous))


(fn back-or-quit
  []
  "C-g, as in Emacs: go up one menu level, or close the modal at the top level."
  (let [history (. (atom.deref fsm.state) :context :history)]
    (if (and history (> (length history) 1))
        (previous-modal)
        (deactivate-modal))))


(var stop-repeat-timeout nil)

(fn stop-modal-timeout
  []
  (call-when stop-repeat-timeout)
  (set stop-repeat-timeout nil))

(fn start-modal-timeout
  []
  "
  API for starting a menu timeout. Some menu actions like the window navigation
  actions can be repeated without having to re-enter into the Menu
  Modal > Window but we don't want to be listening for key events indefinitely.
  This (re)starts a timeout that will close the modal and remove the key
  bindings after a time delay specified in the timout function.
  It deliberately does not go through the state machine: a transition would
  rebind the menu keys and cut off a held key's auto-repeat.
  Takes no arguments.
  Side effectful
  "
  (stop-modal-timeout)
  (set stop-repeat-timeout (timeout deactivate-modal)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set Key Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn create-action-trigger
  [{:action action :repeatable repeatable :timeout timeout}]
  "
  Creates a function to dispatch an action associated with a menu item defined
  by config.fnl.
  Takes a table defining the following:

  action :: function | string - Either a string like \"module:function-name\"
                                or a fennel function to call.
  repeatable :: bool | nil - If this action is repeatable like jumping between
                             windows where we might wish to jump 2 windows
                             left and it wouldn't want to re-enter the jump menu
  timeout :: bool | nil - If a timeout should be started. Defaults to true when
                          repeatable is true.

  Returns a function to execute the action-fn async.
  "
  (let [action-fn (action->fn action)]
    (fn []
      (if (and repeatable (~= timeout false))
          (start-modal-timeout)
          (not repeatable)
          (deactivate-modal))
      ;; Delay the action-fn ever so slightly
      ;; to speed up the closing of the menu
      ;; This makes the UI feel slightly snappier
      (hs.timer.doAfter 0.01 action-fn))))


(fn create-menu-trigger
  [{:key key :mods mods}]
  "
  Takes a config menu option and returns a function to enter that submenu when
  action is activated. The modifiers are passed along so Shift-t and t can
  open different submenus.
  Returns a function to activate submenu.
  "
  (fn []
    (enter-modal (if (has-some? mods) {: key : mods} key))))


(fn select-trigger
  [item]
  "
  Transform a menu item into an action to either call a function or enter a
  submenu.
  Takes a menu item from config.fnl
  Returns a function to perform the action associated with menu item.
  "
  (if (and item.action (= item.action :previous))
      previous-modal
      item.action
      (create-action-trigger item)
      (or item.items item.menu-fn)
      (create-menu-trigger item)
      (fn []
        (log.w "No trigger could be found for item: "
               (hs.inspect item)))))


(fn bind-item
  [item]
  "
  Create a bindspec to map modal menu items to actions and submenus.
  Takes a menu item
  Returns a table to create a hs key binding.
  "
  {:mods (or item.mods [])
   :key item.key
   ;; Holding a repeatable key fires it again with the system key-repeat rate.
   :repeat (or item.repeat item.repeatable)
   :action (select-trigger item)})


(fn bind-menu-keys
  [items]
  "
  Binds all actions and submenu items within a menu to VenueBook.
  Takes a list of modal menu items.
  Returns a function to remove menu key bindings for easy cleanup.
  "
  (-> items
      (->> (filter (fn [item]
                     (or item.action
                         item.items
                         item.menu-fn)))
           (map bind-item))
      (concat [{:key :ESCAPE
                :action deactivate-modal}
               {:mods [:ctrl]
                :key "["
                :action deactivate-modal}
                {:mods [:ctrl]
                :key :g
                :action back-or-quit}])
      (bind-keys)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display Modals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn modal-alert
  [menu history]
  "
  Display a menu modal as an Emacs which-key style panel at the bottom of the
  screen.
  Takes a menu table specified in config.fnl and the modal menu history
  Returns nil
  "
  (which-key.show menu history))

(fn show-modal-menu
  [state]
  "
  Main API to display a modal and run side-effects
    - Display the modal alert
  Takes current modal state from our modal statemachine
  Returns the function to cleanup everything it sets up
  "
  (lifecycle.enter-menu state.context.menu)
  (modal-alert state.context.menu state.context.history)
  (let [unbind-keys (bind-menu-keys state.context.menu.items)]
    (fn []
      (which-key.hide)
      (unbind-keys)
      (stop-modal-timeout)
      (lifecycle.exit-menu state.context.menu)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus, & Config Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn same-mods?
  [a b]
  (and (= (length a) (length b))
       (accumulate [same true _ mod (ipairs a)]
         (and same (hs.fnutils.contains b mod)))))


(fn by-key
  [target]
  "
  Checker function to filter menu items where key matches target
  Takes a target key string like \"w\", or a {:key :mods} table for a
  modified key such as Shift-t
  Returns true or false
  "
  (let [key (if (= (type target) :table) target.key target)
        mods (if (= (type target) :table) target.mods [])]
    (fn [item]
      (and (= (. item :key) key)
           (same-mods? (or item.mods []) mods)
           (or (has-some? item.items)
               item.menu-fn)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State Transition Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(fn local-leader-item
  [config]
  "
  The local leader isn't in the leader menu but is entered like one of its
  items, built by config's :local-leader and keyed by its own hotkey for the
  which-key echo line.
  "
  (let [hotkey (find #(= $1.action "lib.modal:activate-local-leader")
                     (or config.keys []))]
    {:key (and hotkey hotkey.key)
     :mods (and hotkey hotkey.mods)
     :menu-fn config.local-leader
     :local-leader? true}))


(fn ->menu
  [state action menu-key]
  "
  Enter a menu like entering into the Window menu from the default main menu.
  Enters the main menu when called from the idle state.
  Takes the current menu state table and the submenu key as 'extra'.
  A submenu item may provide :menu-fn instead of :items to build its menu when
  entered; if it returns nil the modal closes.
  Returns updated menu state
  "
  (let [{:config config
         :history history
         :menu prev-menu} state.context
        app-menu (apps.get-app)
        item (when menu-key
               (if (= menu-key :local-leader)
                   (local-leader-item config)
                   (find (by-key menu-key) prev-menu.items)))
        menu (if (not menu-key)
                 (if (and app-menu (has-some? app-menu.items))
                     app-menu
                     config)
                 (and item item.menu-fn)
                 (let [built (item.menu-fn)]
                   ;; Built menus don't know the key that opened them; record
                   ;; it so the which-key echo line can show e.g. `s-< b-`.
                   (when built
                     (set built.leader-key item.key)
                     (set built.leader-mods item.mods)
                     (set built.local-leader? item.local-leader?))
                   built)
                 item)]
    (if menu
        {:state {:current-state :active
                 :context (merge state.context
                                 {:menu menu
                                  :history (if menu-key
                                               (conj (or history []) menu)
                                               [menu])})}
         :effect :open-menu}
        {:state {:current-state :idle
                 :context (merge state.context {:menu :nil
                                                :history []})}
         :effect :close-modal-menu})))


(fn active->idle
  [state action extra]
  "
  Transition our modal state machine from the active, open state to idle.
  Takes the current modal state table.
  Kicks off an effect to close the modal, stop the timeout, and unbind keys
  Returns updated modal state machine state table.
  "
  {:state  {:current-state :idle
            :context (merge state.context {:menu :nil
                                           :history []})}
   :effect :close-modal-menu})


(fn ->enter-app
  [state action extra]
  "
  Transition our modal state machine the main menu to an app menu
  Takes the current modal state table and the app menu table.
  Displays updated modal menu if the current menu is different than the previous
  menu otherwise results in no operation. Application mode menus (lib.modes)
  belong to the app they were opened in, so switching apps cancels them.
  A submenu stays open, so a repeatable key that switched apps (like `t l`
  onto another app's Space) can be pressed again; only the root menu C-g
  returns to changes.
  Returns new modal state
  "
  (let [{:config config
         :history history
         :menu prev-menu} state.context
        app-menu (apps.get-app)
        menu (if (and app-menu (has-some? app-menu.items))
                 app-menu
                 config)
        root (. history 1)]
    (if prev-menu.mode
        (active->idle state)
        (= menu root)
        ; nil transition object means keep all state
        nil
        (> (length history) 1)
        ; no effect: the open submenu, its keys and its timeout stay
        {:state {:current-state :active
                 :context (merge state.context
                                 {:history (icollect [i m (ipairs history)]
                                             (if (= i 1) menu m))})}}
        (do
          ;; Re-rendering the root stops the timeout of a repeatable key.
          (when stop-repeat-timeout
            (hs.timer.doAfter 0 start-modal-timeout))
          {:state {:current-state :active
                   :context (merge state.context {:menu menu :history [menu]})}
           :effect :open-menu}))))


(fn active->leave-app
  [state action extra]
  "
  Transition to the regular menu when user removes focus (blurs) another app.
  If the leave event was fired for the app we are already in, do nothing.
  Takes the current modal state table.
  Returns new updated modal state if we are leaving the current app.
  "
  (let [{:config config
        :menu prev-menu} state.context]
    (if (= prev-menu.key config.key)
        nil
        (->menu state))))


(fn ->previous
  [state action extra]
  "
  Transition to the previous submenu. Like if you went into the window menu
  and wanted to go back to the main menu.
  Takes the modal state table.
  Returns a partial modal state table update.
  Dynamically calls another transition depending on history.
  "
  (let [{:config config
         :history hist
         :menu menu} state.context
        prev-menu (. hist (- (length hist) 1))]
    (if prev-menu
        {:state {:current-state :active
                 :context (merge state.context {:menu prev-menu
                                                :history (butlast hist)})}
         :effect :open-menu}
        (->menu state))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finite State Machine States
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; State machine states table. Maps states to actions to transition functions.
;; These transition functions return transition objects that contain the new
;; state key and context.
(local states
       {:idle   {:activate       ->menu}
        :active {:deactivate     active->idle
                 :enter          ->menu
                 :previous       ->previous
                 :enter-app      ->enter-app}})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Watchers, Dispatchers, & Logging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(fn start-logger
  [fsm]
  "
  Start logging the status of the modal state machine.
  Takes our finite state machine.
  Returns nil
  Creates a watcher of our state atom to log state changes reactively.
  "
  (atom.add-watch
   fsm.state :log-state
   (fn log-state
     [state]
     (when state.context.history
       (log.df (hs.inspect (map #(. $1 :title) state.context.history)))))))

(local modal-effect
       (let [handle (statemachine.effect-handler {:open-menu show-modal-menu})]
         (fn [transition]
           ;; A transition without an effect (including a nil one) only updates
           ;; the context; handling it would close the menu on screen while
           ;; the state machine stays active.
           (when transition.effect
             (handle transition)))))

(fn proxy-app-action
  [[action data]]
  "
  Provide a semi-public API function for other state machines to dispatch
  changes to the modal menu state. Currently used by the app state machine to
  tell the modal menu state machine when an app is launched, activated,
  deactivated, or exited.
  Executes a side-effect
  Returns nil
  "
  (fsm.send action data))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn init
  [config]
  "
  Initialize the modal state machine responsible for displaying modal alerts
  to the user to trigger actions defined by their config.fnl.
  Takes the config.fnl table.
  Causes side effects to start the state machine, show the modal, and logging.
  Returns a function to unsubscribe from the app state machine.
  "
  (let [initial-context {:config config
                         :history []
                         :menu :nil}
        template {:state {:current-state :idle
                          :context initial-context}
                  :states states
                  :log "modal"}
        unsubscribe (apps.subscribe proxy-app-action)]
    (set fsm (statemachine.new template))
    (fsm.subscribe modal-effect)
    (start-logger fsm)
    (fn cleanup []
      (unsubscribe))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


{: init
 : activate-modal
 : activate-local-leader
 : deactivate-modal}
