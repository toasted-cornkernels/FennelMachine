(global e (: (hs.eventtap.new [hs.eventtap.event.types.flagsChanged
                               hs.eventtap.event.types.keyUp
                               hs.eventtap.event.types.keyDown]
                              (fn [ev]
                                (let [raw-flags (& (. (. (ev:getRawEventData)
                                                         :CGEventData)
                                                      :flags)
                                                   0xdffffeff)
                                      regular-flags (ev:getFlags)]
                                  (when (= raw-flags 1048592)
                                    (print "righty!" (ev:getType))
                                    (values true {})))))
             start))

