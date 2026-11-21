;; System media controls, as the keyboard's media keys: they act on the app
;; macOS shows in Now Playing (Music, a browser, mpv, EMMS's mpv, ...).
;; EMMS's own controls are in Emacs' mode keymap (Cmd-Shift-, a m e).

(local minibuffer (require :lib.minibuffer))

(fn system-key [key]
  (each [_ down? (ipairs [true false])]
    (: (hs.eventtap.event.newSystemKeyEvent key down?) :post)))

(fn play-or-pause []
  "Play or pause what is playing (the play/pause media key)."
  (system-key :PLAY))

(fn next-track []
  "Next track (the next-track media key)."
  (system-key :NEXT))

(fn previous-track []
  "Previous track (the previous-track media key)."
  (system-key :PREVIOUS))

(fn volume-up []
  "Raise the system volume (the volume-up key)."
  (system-key :SOUND_UP))

(fn volume-down []
  "Lower the system volume (the volume-down key)."
  (system-key :SOUND_DOWN))

(fn mute []
  "Mute or unmute the system volume (the mute key)."
  (system-key :MUTE))

(fn menu-extra [id]
  "Control Center's menu bar item with identifier ID, if it is shown."
  (let [app (. (hs.application.applicationsForBundleID :com.apple.controlcenter) 1)
        bar (and app (: (hs.axuielement.applicationElement app) :attributeValue
                        :AXExtrasMenuBar))]
    (when bar
      (hs.fnutils.find (or (: bar :attributeValue :AXChildren) [])
                       #(= (: $1 :attributeValue :AXIdentifier) id)))))

(fn now-playing []
  "Show or hide macOS's Now Playing (art, title, controls, position), or
  Control Center when Now Playing isn't in the menu bar."
  (let [item (or (menu-extra :com.apple.menuextra.now-playing)
                 (menu-extra :com.apple.menuextra.controlcenter))]
    (if item
        (: item :performAction :AXPress)
        (minibuffer.message "Can't find Now Playing in the menu bar"))))

{: play-or-pause
 : next-track
 : previous-track
 : volume-up
 : volume-down
 : mute
 : now-playing}
