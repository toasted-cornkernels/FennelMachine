"
Run a shell command and show its output, like async-shell-command: a short
result in the echo area, a longer one in the help viewer. Earlier commands
are offered as completions, most recent first.
"
(local minibuffer (require :lib.minibuffer))

(local history-key "shell.history")
(local history-limit 100)
(local echo-lines 3)

;; Running tasks, kept so they aren't garbage collected.
(local running {})

(fn history [] (or (hs.settings.get history-key) []))

(fn remember [command]
  (let [kept (icollect [_ c (ipairs (history))] (when (not= c command) c))]
    (table.insert kept 1 command)
    (hs.settings.set history-key
                     (icollect [i c (ipairs kept)] (when (<= i history-limit) c)))))

(fn lines-of [text]
  (let [text (text:gsub "%s+$" "")]
    (if (= text "") []
        (icollect [line (string.gmatch (.. text "\n") "(.-)\n")] line))))

(fn report [command code stdout stderr]
  (let [lines (lines-of (.. (or stdout "") (or stderr "")))
        status (if (= code 0) "" (.. " [exit " code "]"))]
    (if (= (length lines) 0)
        (minibuffer.message (.. "$ " command status
                                (if (= code 0) " (no output)" "")))
        (and (<= (length lines) echo-lines)
             (hs.fnutils.every lines #(<= (utf8.len $1) (minibuffer.max-chars))))
        (minibuffer.message (.. (table.concat lines "\n") status))
        ((. (require :lib.help) :show) {:title (.. "$ " command status)
                                        : lines}))))

(fn run [command]
  "Run COMMAND with the login shell in the home folder and report its output."
  (remember command)
  (let [shell (or (os.getenv :SHELL) "/bin/zsh")
        task (hs.task.new shell
                          (fn [code stdout stderr]
                            (tset running command nil)
                            (report command code stdout stderr))
                          ["-lc" command])]
    (task:setWorkingDirectory (os.getenv :HOME))
    (tset running command task)
    (when (not (task:start))
      (tset running command nil)
      (minibuffer.message (.. "Couldn't run " command)))))

(fn shell-command []
  "Read a shell command, with earlier ones to complete from, and run it."
  (minibuffer.completing-read
   "Shell command: "
   (icollect [_ c (ipairs (history))] {:text c :annotation "history" :command c})
   {:dynamic (fn [input]
               (let [input (input:match "^%s*(.-)%s*$")]
                 (if (= input "") {}
                     {:before [{:text input :annotation "run" :command input}]})))
    :on-accept (fn [c input]
                 (let [command (or (and c c.command) (input:match "^%s*(.-)%s*$"))]
                   (if (= command "")
                       (minibuffer.message "No command")
                       (run command))))}))

{: shell-command
 : run
 : lines-of}
