"
Evaluate Fennel in Hammerspoon, like Emacs' M-: (eval-expression).

The value is shown in the echo area, or in the help viewer when it doesn't
fit on one line. Errors are echoed and their traceback is printed to the
console. Earlier expressions are offered as candidates: RET on an empty
input evaluates the selected one again; s-DEL forgets it and
`clear-history` forgets them all. An expression typed with a leading space,
or one that looks like it holds a secret (see clipboard.fnl), isn't kept.
"
(local fennel (require :fennel))
(local minibuffer (require :lib.minibuffer))
(local help (require :lib.help))

(local history-key :eval.history)
(local history-size 50)
(local echo-width 100)

(fn history []
  (or (hs.settings.get history-key) []))

(fn forget [code]
  (hs.settings.set history-key
                   (icollect [_ old (ipairs (history))] (when (not= old code) old))))

(fn remember [code]
  (let [entries [code]]
    (each [_ old (ipairs (history))]
      (when (and (not= old code) (< (length entries) history-size))
        (table.insert entries old)))
    (hs.settings.set history-key entries)))

(fn history-count [] (length (history)))

(fn clear-history []
  "Forget every expression in the history, and the minibuffer's repeat, which
  may hold them too."
  (hs.settings.clear history-key)
  (minibuffer.clear-history))

(fn confirm-clear []
  "Clear the eval history after asking."
  (minibuffer.y-or-n-p "Clear the eval history?"
                       #(do (clear-history)
                            (minibuffer.message "Eval history cleared"))))

(fn show-values [code vals private?]
  (let [shown (if (= vals.n 0)
                  ["nil"]
                  (fcollect [i 1 vals.n] (fennel.view (. vals i))))
        text (table.concat shown "\t")]
    (when (not private?) (print (.. "eval: " code "\n" text)))
    (if (and (not (text:find "\n")) (<= (utf8.len text) echo-width))
        (minibuffer.message text nil private?)
        (help.show {:title (.. "eval: " (if private? "(private)" code))
                    :lines [text]}))))

(fn eval-code [code private?]
  "Evaluate CODE and show its values. Unless PRIVATE? or CODE looks like it
  holds a secret, it is kept in the history; otherwise it is also left out of
  the minibuffer's repeat and the console."
  (let [private? (or private? ((. (require :clipboard) :secret?) code))]
    (if private?
        (minibuffer.clear-history)
        (remember code))
    (let [result (table.pack (xpcall #(fennel.eval code {:env _G :filename :eval})
                                     fennel.traceback))]
      (if (. result 1)
          (show-values code (table.pack (table.unpack result 2 result.n)) private?)
          (let [err (tostring (. result 2))]
            (when (not private?) (print (.. "eval: " code "\n" err)))
            (minibuffer.message (or (err:match "^[^\n]*") err) nil private?))))))

(fn eval-expression []
  "Read a Fennel expression in the minibuffer and evaluate it."
  (minibuffer.completing-read
   "Eval: "
   (icollect [_ code (ipairs (history))] {:text code})
   {:on-accept (fn [candidate input]
                 (let [code (if (input:match "%S") input
                                candidate candidate.text)]
                   (when code
                     (eval-code code (not= nil (input:find "^%s+%S"))))))
    :actions {"s-DEL" #(when $1
                         (forget $1.text)
                         (eval-expression))}}))

{: eval-expression
 : eval-code
 : history-count
 : clear-history
 : confirm-clear}
