"
Evaluate Fennel in Hammerspoon, like Emacs' M-: (eval-expression).

The value is shown in the echo area, or in the help viewer when it doesn't
fit on one line. Errors are echoed and their traceback is printed to the
console. Earlier expressions are offered as candidates: RET on an empty
input evaluates the selected one again.
"
(local fennel (require :fennel))
(local minibuffer (require :lib.minibuffer))
(local help (require :lib.help))

(local history-key :eval.history)
(local history-size 50)
(local echo-width 100)

(fn history []
  (or (hs.settings.get history-key) []))

(fn remember [code]
  (let [entries [code]]
    (each [_ old (ipairs (history))]
      (when (and (not= old code) (< (length entries) history-size))
        (table.insert entries old)))
    (hs.settings.set history-key entries)))

(fn show-values [code vals]
  (let [shown (if (= vals.n 0)
                  ["nil"]
                  (fcollect [i 1 vals.n] (fennel.view (. vals i))))
        text (table.concat shown "\t")]
    (print (.. "eval: " code "\n" text))
    (if (and (not (text:find "\n")) (<= (utf8.len text) echo-width))
        (minibuffer.message text)
        (help.show {:title (.. "eval: " code) :lines [text]}))))

(fn eval-code [code]
  (remember code)
  (let [result (table.pack (xpcall #(fennel.eval code {:env _G :filename :eval})
                                   fennel.traceback))]
    (if (. result 1)
        (show-values code (table.pack (table.unpack result 2 result.n)))
        (let [err (tostring (. result 2))]
          (print (.. "eval: " code "\n" err))
          (minibuffer.message (or (err:match "^[^\n]*") err))))))

(fn eval-expression []
  "Read a Fennel expression in the minibuffer and evaluate it."
  (minibuffer.completing-read
   "Eval: "
   (icollect [_ code (ipairs (history))] {:text code})
   {:on-accept (fn [candidate input]
                 (let [code (if (input:match "%S") input
                                candidate candidate.text)]
                   (when code (eval-code code))))}))

{: eval-expression : eval-code}
