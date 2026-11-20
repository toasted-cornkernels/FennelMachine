(local log (hs.logger.new "emacs.fnl" "warning"))

(fn find-emacsclient
  []
  "Homebrew's prefix is /opt/homebrew on Apple Silicon and /usr/local on Intel;
  Nix profiles live elsewhere. Fall back to the login shell's PATH."
  (or (hs.fnutils.find ["/opt/homebrew/bin/emacsclient"
                        "/usr/local/bin/emacsclient"
                        (.. "/etc/profiles/per-user/" (os.getenv "USER")
                            "/bin/emacsclient")
                        "/run/current-system/sw/bin/emacsclient"]
                       #(hs.fs.attributes $1))
      (let [(out ok) (hs.execute "command -v emacsclient" true)
            path (and ok (out:match "^%s*(.-)%s*$"))]
        (when (and path (not= path "")) path))))

(local emacsclient-path (find-emacsclient))

(fn elisp
  [form]
  "Wrap FORM so spacehammer.el (shipped with this config) is loaded first."
  (.. "(progn (unless (featurep 'spacehammer) (load "
      (string.format "%q" (.. hs.configdir "/spacehammer.el"))
      " nil t)) " form ")"))

(fn emacsclient
  [args]
  "Run emacsclient with ARGS (no shell involved); log failures to the console."
  (if (not emacsclient-path)
      (log.w "emacsclient not found; install Emacs or add it to PATH")
      (: (hs.task.new emacsclient-path
                      (fn [code stdout stderr]
                        (when (not= code 0)
                          (log.w (.. "emacsclient failed (" code "): " stderr
                                     stdout)))
                        true)
                      args)
         :start)))

(fn capture [is-note]
  "Activates org-capture"
  (let [window (hs.window.focusedWindow)
        pid (if window (tostring (: window :pid)) "")
        title (if window (: window :title) "")
        form (string.format "(spacehammer/activate-capture-frame %q %q %s)"
                            pid title (if is-note "\"z\"" "nil"))]
    (hs.timer.doAfter 0.1
                      #(emacsclient ["-c" "-F" "((name . \"capture\"))"
                                     "-e" (elisp form)]))))

(fn edit-with-emacs []
  "Executes emacsclient, evaluating a special elisp function in spacehammer.el
   (loaded on demand), passing PID, title and display-id of the caller."
  (let [current-app (: (hs.window.focusedWindow) :application)
        form (string.format "(spacehammer/edit-with-emacs %q %q %q)"
                            (tostring (: current-app :pid))
                            (: current-app :title)
                            (tostring (: (hs.screen.mainScreen) :id)))
        prev (hs.pasteboard.changeCount)
        _ (hs.eventtap.keyStroke [:cmd] :c)
        next (hs.pasteboard.changeCount)]
    (when (= prev next)         ; Pasteboard was not updated so no text was selected
      (hs.eventtap.keyStroke [:cmd] :a)  ; select all
      (hs.eventtap.keyStroke [:cmd] :c)  ; copy
      )
    (emacsclient ["-c" "-F" "((name . \"edit\"))" "-e" (elisp form)])))

(fn edit-with-emacs-callback [pid title screen]
  "Don't remove! - this is callable from Emacs
   See: `spacehammer/edit-with-emacs` in spacehammer.el"
  (let [emacs-app   (hs.application.find :Emacs)
        edit-window (: emacs-app :findWindow :edit)
        scr         (hs.screen.find (tonumber screen))
        windows     (require :windows)]
    (when (and edit-window scr)
      (: edit-window :moveToScreen scr)
      (: windows :center-window-frame))))

(fn run-emacs-fn
  [form]
  "Evaluates the elisp FORM (a string) in the running Emacs."
  (emacsclient ["-e" (elisp form)]))

(fn run-command
  [command]
  "
  Run the interactive Emacs COMMAND (a symbol name) as M-x would, in the
  selected frame, via spacehammer/run-command. spacehammer.el is reloaded if
  an older copy without that function is already loaded.
  "
  (emacsclient
   ["-e"
    (string.format "(progn (unless (fboundp 'spacehammer/run-command) (load %q nil t)) (spacehammer/run-command '%s))"
                   (.. hs.configdir "/spacehammer.el") command)]))

(fn commands-for
  [keymap]
  "
  Mode commands for every leaf of KEYMAP whose command id is emacs.NAME: each
  runs the Emacs command NAME. The keymap is the only list of commands.
  "
  (let [commands {}]
    (fn walk [nodes]
      (each [_ node (ipairs nodes)]
        (if node.items
            (walk node.items)
            (let [name (and node.command (node.command:match "^emacs%.(.+)$"))]
              (when name
                (tset commands node.command
                      {:title name :fn #(run-command name)}))))))
    (walk keymap)
    commands))

(fn full-screen
  []
  "Switches to current instance of GUI Emacs and makes its frame fullscreen"
  (hs.application.launchOrFocus :Emacs)
  (run-emacs-fn "(progn (set-frame-parameter nil 'fullscreen 'fullboth) (spacehammer/fix-frame))"))

(fn vertical-split-with-emacs
  []
  "Creates vertical split with Emacs window sitting next to the current app"
  (let [windows    (require :windows)
        cur-app    (-?> (hs.window.focusedWindow) (: :application) (: :name))
        rect-left  [0  0 .5  1]
        rect-right [.5 0 .5  1]]
    ;; A fullscreen (fullboth) frame snaps back when resized, so release it first.
    (run-emacs-fn "(set-frame-parameter nil 'fullscreen nil)")
    (hs.timer.doAfter
     .2
     (fn []
       (if (= cur-app :Emacs)
           (do
             (windows.rect rect-left)
             (windows.jump-to-last-window)
             (windows.rect rect-right))
           (do
             (windows.rect rect-right)
             (hs.application.launchOrFocus :Emacs)
             (windows.rect rect-left)))))))

(fn switch-to-app [pid]
  "Don't remove! - this is callable from Emacs See: `spacehammer/switch-to-app`
   in spacehammer.el "
  (let [app (hs.application.applicationForPID (tonumber pid))]
    (when app (: app :activate))))

(fn switch-to-app-and-paste-from-clipboard [pid]
  "Don't remove! - this is callable from Emacs See:
   `spacehammer/finish-edit-with-emacs` in spacehammer.el."
  (let [app (hs.application.applicationForPID (tonumber pid))]
    (when app
      (: app :activate)
      (hs.timer.doAfter
       0.001
       (fn [] (: app :selectMenuItem [:Edit :Paste]))))))

(fn maximize
  []
  "Maximizes Emacs GUI window after a short delay."
  (hs.timer.doAfter
   1.5
   (fn []
     (let [app     (hs.application.find :Emacs)
           windows (require :windows)
           modal   (require :lib.modal)]
       (when app
         (: app :activate)
         (windows.maximize-window-frame))))))

{:capture                          capture
 :commands-for                     commands-for
 :edit-with-emacs                  edit-with-emacs
 :editWithEmacsCallback            edit-with-emacs-callback
 :emacsclient                      emacsclient
 :full-screen                      full-screen
 :maximize                         maximize
 :note                             (fn [] (capture true))
 :run-command                      run-command
 :switchToApp                      switch-to-app
 :switchToAppAndPasteFromClipboard switch-to-app-and-paste-from-clipboard
 :vertical-split-with-emacs        vertical-split-with-emacs}
