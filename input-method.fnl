(global e (: (hs.eventtap.new [hs.eventtap.event.types.flagsChanged
                               hs.eventtap.event.types.keyUp
                               hs.eventtap.event.types.keyDown]
                              (fn [event]
                                (let [raw-flags (band (. (. (event:getRawEventData)
                                                            :CGEventData)
                                                         :flags)
                                                      3758096127)
                                      cmd? (. (event:getFlags) :cmd)
                                      current-app-name (: (hs.application.frontmostApplication) :name)]
                                  ;; (hs.alert cmd?)
                                  (when (= raw-flags 1048592)
                                    (match (hs.keycodes.currentLayout)
                                      :U.S. (if (= current-app-name "Emacs")
                                                (hs.eventtap.keyStroke {} "f6")
                                                (hs.keycodes.setMethod "2-Set Korean"))
                                      "2-Set Korean" (if (= current-app-name "Emacs")
                                                         (hs.eventtap.keyStroke {} "f6")
                                                         (hs.keycodes.setLayout :U.S.)))
                                    (values true {}))))) :start))
