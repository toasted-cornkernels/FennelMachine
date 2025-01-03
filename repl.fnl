(local coroutine (require :coroutine))
(local fennel    (require :fennel))
(local jeejah    (require :jeejah))
(local {:merge merge}
       (require :lib.functional))

(fn fennel-middleware [f msg]
  (match msg.op
    "load-file" (let [f (assert (io.open msg.filename "rb"))]
                  (tset msg
                        :op "eval"
                        :code (-> f
                                  (: :read "*all")
                                  (: :gsub "^#![^\n]*\n" "")))
                  (: f :close))
    _ (f msg)))

(local default-opts {:port nil
                     :fennel true
                     :middleware fennel-middleware
                     :serialize hs.inspect})

(local repl-coro-freq 0.05)

(fn run [server]
  (let [repl-coro server
        repl-spin (fn [] (coroutine.resume repl-coro))
        repl-chk (fn [] (not= (coroutine.status repl-coro) "dead"))]
    (hs.timer.doWhile repl-chk repl-spin repl-coro-freq)))

(fn start [custom-opts]
  (let [opts (merge {} default-opts custom-opts)
        server (jeejah.start opts.port opts)]
    server))

(fn stop [server]
  (jeejah.stop server))

{: run
 : start
 : stop}
