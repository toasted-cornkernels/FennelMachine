"
FaceTime commands for facetime-mode.

FaceTime has no menu item for starting a call. The toolbar's New button opens
the call composer with the recipient field focused, which is the reliable,
keyboard-friendly entry point. Placing a call to a particular contact is left to
that composer rather than pressing contact tiles.
"

(local {: menu-item : press-button} (require :lib.modes))

(local commands
       {:facetime.new-call {:title "New FaceTime (recipient field)"
                            :fn (press-button "New")}
        :facetime.mute {:title "Mute/unmute (during a call)"
                        :fn (menu-item ["Video" "Mute"])}
        :facetime.toggle-on-top {:title "Toggle Always on Top"
                                 :fn (menu-item ["Video" "Always on Top"])}})

{: commands}
