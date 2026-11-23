"
Fennel REPLs in Hammerspoon, for an editor to drive it.

bin/hs-fennel-repl asks for one with `listen`, which opens a Unix socket of
its own in $TMPDIR, and relays its stdin and stdout to it with nc: what the
editor sends is read by Fennel's own REPL, and what the REPL prints goes
back. Emacs' fennel-mode runs the script as its `fennel-program`, with
`fennel-repl` or `fennel-proto-repl`, whose protocol (completion,
documentation, finding definitions) works too.

Each REPL has an environment of its own over Hammerspoon's globals: it
reads them there, `global` sets them, and its locals, `print` and `io.write`
stay its own.

Hammerspoon reloading, on saving a config file say, ends its REPLs. It
leaves a marker beside each socket as it goes, which the script waits on,
and once loaded again opens a fresh REPL on the same socket, replaying the
protocol upgrade if the editor had sent one. Config loads this module so.
"
(local fennel (require :fennel))
;; Required here, as hs.socket and the like would print that they're loading
;; when first used, and printing inside an `hs -c` can break hs.ipc.
(local fs (require :hs.fs))
(local socket (require :hs.socket))
(local timer (require :hs.timer))

(local connect-timeout 10)
(local poll-interval 0.05)
(local prefix "hs-fennel-repl-")

;; Open REPLs by their socket's path, so that they aren't collected.
(local sessions {})

(fn tmp-dir []
  (: (or (os.getenv :TMPDIR) "/tmp/") :gsub "/?$" "/"))

(fn alive? [id]
  "Whether the script with process ID is still running."
  (and (id:match "^%d+$") (os.execute (.. "kill -0 " id " 2>/dev/null")) true))

(fn leave-markers []
  "Mark the open sessions, beside their sockets, for their scripts to wait
  for Hammerspoon to open them again; the protocol upgrade is kept in it."
  (each [_ session (pairs sessions)]
    (when (not session.closed?)
      (case (io.open (.. session.path ".resume") :w)
        file (do (file:write (or session.upgrade ""))
                 (file:close))))))

(let [previous hs.shutdownCallback]
  (set hs.shutdownCallback (fn []
                             (leave-markers)
                             (when previous (previous)))))
(fn session-env [write read-line]
  "An environment over _G: reads fall through to it and so do writes, but
  for what the REPL and its protocol keep to themselves."
  (let [own {:___repl___ false :___replLocals___ false :_ false :__ false
             :io (collect [k v (pairs io)] k v)}]
    ;; All set before the metatable, which would send them on to _G.
    (fn own.print [...]
      (write (.. (table.concat (fcollect [i 1 (select :# ...)]
                                 (tostring (select i ...)))
                               "\t")
                 "\n"))
      (values))
    (fn own.io.write [...]
      (for [i 1 (select :# ...)]
        (write (tostring (select i ...))))
      io.stdout)
    (fn own.io.read [?format]
      (let [line (read-line)
            format (tostring (or ?format :l))]
        (if (not line) nil
            (format:match "^%*?L") line
            (format:match "^%*?n") (tonumber line)
            (pick-values 1 (line:gsub "\n$" "")))))
    (set own._G own)
    (setmetatable own
                  {:__index _G
                   :__newindex _G
                   ;; Fennel lists the globals a REPL allows by pairs.
                   :__pairs (fn []
                              (let [all (collect [k v (pairs _G)] k v)]
                                (each [k v (values next own)]
                                  (tset all k v))
                                (values next all nil)))})))

(fn on-error [write]
  (fn [kind message]
    (write (if (= kind :Runtime)
               (.. (fennel.traceback (tostring message) 4) "\n")
               (: "%s error: %s\n" :format kind (tostring message))))))

(fn close [session]
  "End SESSION's REPL and close its socket; the script, its other end,
  exits too."
  (when (not session.closed?)
    (set session.closed? true)
    (: session.timer :stop)
    (tset sessions session.path nil)
    ;; Its REPL is left waiting for input, never to come: told of the end of
    ;; it instead, the protocol's would complain, and read again, forever.
    (set session.co nil)
    (let [server session.server
          disconnect #(: server :disconnect)]
      (if (= 0 (: server :connections))
          (disconnect)
          ;; Once what was written has gone, the last of it a newline: its
          ;; callback is called on the next write done, maybe an earlier one.
          (: server :write "\n" 1 #(if (= $ 1)
                                        (disconnect)
                                        (timer.doAfter 0.5 disconnect)))))))

(fn resume [session]
  "Run SESSION's REPL until it waits for input."
  (case session.co
    co (when (= (coroutine.status co) :suspended)
         (let [(ok? err) (coroutine.resume co)]
           (when (not ok?)
             (print (.. "repl: " (tostring err))))
           (when (= (coroutine.status co) :dead)
             (close session))))))

(fn start-repl [session]
  (let [server session.server
        write #(when (not (or session.closed? session.muted?))
                 (: server :write $))
        read-line (fn []
                    (while (= 0 (length session.lines))
                      ;; What the replayed upgrade printed is done with.
                      (set session.muted? false)
                      (coroutine.yield))
                    (table.remove session.lines 1))
        env (session-env write read-line)]
    (set session.co
         (coroutine.create
          #(fennel.repl {: env
                         :readChunk (fn [{: stack-size}]
                                      (write (if (< 0 stack-size) ".. " ">> "))
                                      (read-line))
                         :onValues #(when (< 0 (length $))
                                     (write (.. (table.concat $ "\t") "\n")))
                         :onError (on-error write)
                         :message (if session.resumed?
                                      ";; Hammerspoon reloaded: a fresh REPL"
                                      (.. ";; Fennel " fennel.version " on "
                                          _VERSION " in Hammerspoon "
                                          hs.processInfo.version))})))
    (when session.upgrade
      (set session.muted? true)
      (table.insert session.lines session.upgrade))
    (: server :setCallback
       (fn [data]
         ;; fennel-proto-repl's first message upgrades the REPL to its
         ;; protocol; kept to replay should Hammerspoon reload.
         (when (data:match "^%s*%(%(fn protocol %[")
           (set session.upgrade data))
         (table.insert session.lines data)
         (: server :read "\n")
         (resume session)))
    (: server :read "\n")
    (resume session)))

(fn watch [session]
  "Start the REPL once the script connects, and close it once it goes."
  (let [started (timer.secondsSinceEpoch)]
    (set session.timer
         (timer.doEvery poll-interval
                           #(let [connections (: session.server :connections)]
                              (if session.co
                                  (when (= 0 connections) (close session))
                                  (> connections 0)
                                  (start-repl session)
                                  (> (- (timer.secondsSinceEpoch) started)
                                     connect-timeout)
                                  (close session)))))))

(fn open [id ?resumed? ?upgrade]
  (let [path (.. (tmp-dir) prefix id)]
    (os.remove path)
    (let [server (socket.server path)
          session {: id : path : server :lines [] :resumed? ?resumed?
                   :upgrade ?upgrade}]
      (tset sessions path session)
      (watch session)
      path)))

(fn listen [id]
  "Open a socket for a REPL in the session ID, the script's process ID, and
  return its path."
  (open (tostring id)))

(fn resume-sessions []
  "Open again the sessions Hammerspoon left marked as it reloaded."
  (let [dir (tmp-dir)]
    (each [name (fs.dir dir)]
      (case (name:match (.. "^" (prefix:gsub "%-" "%%-") "(%d+)%.resume$"))
        id (let [marker (.. dir name)
                 upgrade (case (io.open marker)
                           file (let [data (file:read :a)]
                                  (file:close)
                                  data))]
             (when (alive? id)
               (open id true (if (= "" upgrade) nil upgrade)))
             ;; After the socket is there: the script waits for this to go.
             (os.remove marker))))))

(resume-sessions)

{: listen : sessions}
