(global layout-watcher (: (hs.eventtap.new [hs.eventtap.event.types.flagsChanged]
                                           (fn [e]
                                             (let [flags (e:getFlags)
                                                   current-app-name (: (hs.application.frontmostApplication)
                                                                       :name)]
                                               (when (and flags.cmd
                                                          (not (or (or (or flags.alt
                                                                           flags.shift)
                                                                       flags.ctrl)
                                                                   flags.fn)))
                                                 (local key-code (e:getKeyCode))
                                                 (if (= key-code 54)
                                                     (match (hs.keycodes.currentLayout)
                                                       :U.S. (if (= current-app-name :Emacs)
                                                                 (hs.eventtap.keyStroke {} :f6)
                                                                 (hs.keycodes.setMethod "2-Set Korean"))
                                                       "2-Set Korean" (if (= current-app-name
                                                                             :Emacs)
                                                                          (hs.eventtap.keyStroke {}
                                                                                                 :f6)
                                                                          (hs.keycodes.setLayout :U.S.)))
                                                     )))))
                          :start))

