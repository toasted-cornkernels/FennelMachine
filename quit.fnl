"Hammerspoon itself, as Emacs' SPC q: reload the config, relaunch or quit
Hammerspoon, clear its console"

(local minibuffer (require :lib.minibuffer))

(fn reload []
  "Reload the config, like restarting Emacs without leaving the process."
  (hs.console.clearConsole)
  (hs.reload))

(fn relaunch []
  "Quit Hammerspoon and open it again."
  (hs.relaunch))

(fn quit []
  "Quit Hammerspoon, after confirmation. Its keys are gone until it is
  opened again."
  (minibuffer.y-or-n-p "Quit Hammerspoon?"
                       #(: (hs.application.applicationForPID hs.processInfo.processID)
                           :kill)))

(fn clear-console []
  "Clear the Hammerspoon console."
  (hs.console.clearConsole)
  (minibuffer.message "Console cleared"))

{: reload
 : relaunch
 : quit
 : clear-console}
