"Toggle system settings, like Spacemacs' SPC T: appearance, Dock and menu
bar hiding, hidden files, Do Not Disturb, keeping awake, mute, Wi-Fi and
Bluetooth. Each echoes the new state; turning a connection off asks first."

(local minibuffer (require :lib.minibuffer))
(local system-shortcuts (require :system-shortcuts))

(local blueutil
       (accumulate [found nil _ path (ipairs [(.. "/etc/profiles/per-user/"
                                                  (os.getenv :USER) "/bin/blueutil")
                                              "/run/current-system/sw/bin/blueutil"
                                              "/opt/homebrew/bin/blueutil"
                                              "/usr/local/bin/blueutil"])]
         (or found (when (hs.fs.attributes path) path))))
(local bluetooth-settings
       "x-apple.systempreferences:com.apple.preference.security?Privacy_Bluetooth")
;; "Turn Do Not Disturb On/Off" in Keyboard Shortcuts › Mission Control.
(local do-not-disturb-hotkey 175)

(fn on-off [on?] (if on? "on" "off"))

(fn system-events [script]
  "Run SCRIPT in System Events and return its result, or nil on failure."
  (let [(ok result) (hs.osascript.applescript
                     (.. "tell application \"System Events\"\n" script "\nend tell"))]
    (when ok result)))

(fn sh [command]
  "Run COMMAND in a shell: its output and whether it succeeded."
  (let [(output ok) (hs.execute command)]
    (values (or output "") ok)))

;; Toggles ==========================================
;; ==================================================

(fn toggle-dark-mode []
  (let [dark? (system-events
               "tell appearance preferences
                  set dark mode to not dark mode
                  return dark mode
                end tell")]
    (minibuffer.message (.. "Dark mode " (on-off dark?)))))

(fn toggle-dock-autohide []
  (let [hidden? (system-events
                 "tell dock preferences
                    set autohide to not autohide
                    return autohide
                  end tell")]
    (minibuffer.message (.. "Dock hiding " (on-off hidden?)))))

(fn toggle-menu-bar-autohide []
  (let [hidden? (system-events
                 "tell dock preferences
                    set autohide menu bar to not autohide menu bar
                    return autohide menu bar
                  end tell")]
    (minibuffer.message (.. "Menu bar hiding " (on-off hidden?)))))

(fn toggle-hidden-files []
  "Show or hide dotfiles in Finder, which has to relaunch to notice."
  (let [(current) (sh "defaults read com.apple.finder AppleShowAllFiles 2>/dev/null")
        show? (not (current:match "^%s*[1Yy]"))]
    (minibuffer.y-or-n-p
     (.. (if show? "Show" "Hide") " hidden files? Finder relaunches.")
     (fn []
       (sh (.. "defaults write com.apple.finder AppleShowAllFiles -bool "
               (tostring show?)))
       (sh "/usr/bin/killall Finder")
       (minibuffer.message (.. "Hidden files " (if show? "shown" "hidden")))))))

(fn do-not-disturb? []
  "Whether a Focus such as Do Not Disturb is on, going by its menu extra, which
  macOS shows while one is active (the Focus database itself needs Full Disk
  Access); nil if Control Center can't be read."
  (let [app (hs.application.get :com.apple.controlcenter)
        (ok on?) (pcall
                  #(accumulate [found false _ bar (ipairs (or (: (hs.axuielement.applicationElement app)
                                                                 :attributeValue :AXChildren) []))]
                     (or found
                         (accumulate [f false _ item (ipairs (or (bar:attributeValue :AXChildren) []))]
                           (or f (= (item:attributeValue :AXIdentifier)
                                    :com.apple.menuextra.focusmode))))))]
    (when (and app ok) on?)))

(fn toggle-do-not-disturb []
  (let [was-on? (do-not-disturb?)]
    (system-shortcuts.run do-not-disturb-hotkey)
    (minibuffer.message (if (= was-on? nil) "Do Not Disturb toggled"
                            (.. "Do Not Disturb " (on-off (not was-on?)))))))

(fn toggle-keep-awake []
  (minibuffer.message (if (hs.caffeinate.toggle :displayIdle)
                          "Keeping the Mac awake"
                          "The Mac may sleep again")))

(fn toggle-mute []
  (let [device (hs.audiodevice.defaultOutputDevice)]
    (if (not device) (minibuffer.message "No output device")
        (let [muted? (not (device:muted))]
          (device:setMuted muted?)
          (minibuffer.message (.. (device:name) (if muted? " muted" " unmuted")))))))

(fn wifi-device []
  (let [(ports) (sh "/usr/sbin/networksetup -listallhardwareports")]
    (ports:match "Hardware Port: Wi%-Fi\nDevice: (%S+)")))

(fn toggle-wifi []
  (let [device (wifi-device)]
    (if (not device)
        (minibuffer.message "No Wi-Fi device")
        (let [(power) (sh (.. "/usr/sbin/networksetup -getairportpower " device))
              on? (power:match ": On")
              set-power (fn [state]
                          (sh (.. "/usr/sbin/networksetup -setairportpower "
                                  device " " state))
                          (minibuffer.message (.. "Wi-Fi " state)))]
          (if on?
              (minibuffer.y-or-n-p "Turn Wi-Fi off?" #(set-power :off))
              (set-power :on))))))

(fn toggle-bluetooth []
  (let [(power ok) (if blueutil (sh (.. blueutil " -p 2>&1")) (values "" false))
        set-power (fn [state]
                    (sh (.. blueutil " -p " state))
                    (minibuffer.message (.. "Bluetooth " state)))]
    (if (not blueutil) (minibuffer.message "blueutil is not installed")
        (not ok)
        (do
          (minibuffer.message
           "Hammerspoon needs Bluetooth access: Privacy & Security › Bluetooth")
          (hs.urlevent.openURL bluetooth-settings))
        (power:match "^%s*1")
        (minibuffer.y-or-n-p
         "Turn Bluetooth off? Bluetooth keyboards and mice disconnect."
         #(set-power :off))
        (set-power :on))))

{: do-not-disturb?
 : toggle-dark-mode
 : toggle-dock-autohide
 : toggle-menu-bar-autohide
 : toggle-hidden-files
 : toggle-do-not-disturb
 : toggle-keep-awake
 : toggle-mute
 : toggle-wifi
 : toggle-bluetooth}
