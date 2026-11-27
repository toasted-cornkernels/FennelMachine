"
Run a shell command and show its output, like async-shell-command: a short
result in the echo area, a longer one in the help viewer. Earlier commands
are offered as completions, most recent first; s-DEL forgets the selected one
and `clear-history` all of them.

Like a shell with HISTCONTROL=ignorespace, a command typed with a leading
space isn't kept, nor is one that looks like it holds a secret (see
clipboard.fnl), e.g. `export API_TOKEN=...`.
"
(local minibuffer (require :lib.minibuffer))

(local history-key "shell.history")
(local history-limit 100)
(local echo-lines 3)

;; Running tasks, kept so they aren't garbage collected.
(local running {})

(fn history [] (or (hs.settings.get history-key) []))

(fn forget [command]
  (hs.settings.set history-key
                   (icollect [_ c (ipairs (history))] (when (not= c command) c))))

(fn remember [command]
  (let [kept (icollect [_ c (ipairs (history))] (when (not= c command) c))]
    (table.insert kept 1 command)
    (hs.settings.set history-key
                     (icollect [i c (ipairs kept)] (when (<= i history-limit) c)))))

(fn history-count [] (length (history)))

(fn clear-history []
  "Forget every command in the history, and the minibuffer's repeat, which
  may hold them too."
  (hs.settings.clear history-key)
  (minibuffer.clear-history))

(fn confirm-clear []
  "Clear the shell command history after asking."
  (minibuffer.y-or-n-p "Clear the shell command history?"
                       #(do (clear-history)
                            (minibuffer.message "Shell command history cleared"))))

(fn lines-of [text]
  (let [text (text:gsub "%s+$" "")]
    (if (= text "") []
        (icollect [line (string.gmatch (.. text "\n") "(.-)\n")] line))))

(fn report [command code stdout stderr private?]
  (let [lines (lines-of (.. (or stdout "") (or stderr "")))
        status (if (= code 0) "" (.. " [exit " code "]"))
        shown (if private? "(private command)" command)]
    (if (= (length lines) 0)
        (minibuffer.message (.. "$ " shown status
                                (if (= code 0) " (no output)" ""))
                            nil private?)
        (and (<= (length lines) echo-lines)
             (hs.fnutils.every lines #(<= (utf8.len $1) (minibuffer.max-chars))))
        (minibuffer.message (.. (table.concat lines "\n") status) nil private?)
        ((. (require :lib.help) :show) {:title (.. "$ " shown status)
                                        : lines}))))

(fn run [command private?]
  "Run COMMAND with the login shell in the home folder and report its output.
  Unless PRIVATE? or COMMAND looks like it holds a secret, it is kept in the
  history; otherwise it is also left out of the minibuffer's repeat and the
  console."
  (let [private? (or private? ((. (require :clipboard) :secret?) command))
        shell (or (os.getenv :SHELL) "/bin/zsh")
        task (hs.task.new shell
                          (fn [code stdout stderr]
                            (tset running command nil)
                            (report command code stdout stderr private?))
                          ["-lc" command])]
    (if private?
        (minibuffer.clear-history)
        (remember command))
    (task:setWorkingDirectory (os.getenv :HOME))
    (tset running command task)
    (when (not (task:start))
      (tset running command nil)
      (minibuffer.message (.. "Couldn't run " (if private? "the command" command))
                          nil private?))))

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
                       (run command (and (not= (?. c :annotation) "history")
                                         (not= nil (input:find "^%s")))))))
    :actions {"s-DEL" (fn [c input]
                        (let [earlier? (and c (= c.annotation "history"))]
                          (when earlier? (forget c.command))
                          (shell-command)
                          (minibuffer.set-input (or input ""))
                          (when (not earlier?)
                            (minibuffer.message "Not an earlier command"))))}}))

{: shell-command
 : run
 : history-count
 : clear-history
 : confirm-clear
 : lines-of}
