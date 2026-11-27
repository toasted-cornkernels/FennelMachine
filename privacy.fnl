"
Clear what FennelMachine remembers of what you did: the histories that
commands recall from. `clear-history` picks one, or all of them, and asks
before clearing; `confirm-clear-all` clears all of them.

Bookmarks, saved layouts, named Spaces and voice corrections aren't here:
they are things you keep on purpose, and their own commands delete them.
"
(local minibuffer (require :lib.minibuffer))

(fn uses [module]
  "The count and clear functions of MODULE, required only when called."
  {:count #((. (require module) :history-count))
   :clear #((. (require module) :clear-history))})

(fn console-lines []
  (let [text (or (hs.console.getConsole) "")]
    (accumulate [n 0 _ (text:gmatch "[^\n]+")] (+ n 1))))

(local histories
       [(doto {:name "Clipboard history" :where "in memory"
               :count #(length ((. (require :clipboard) :history)))
               :clear #((. (require :clipboard) :clear))})
        (doto (uses :kmacro) (tset :name "Keyboard macros") (tset :where "on disk"))
        (doto (uses :shell) (tset :name "Shell commands") (tset :where "on disk"))
        (doto (uses :eval) (tset :name "Eval expressions") (tset :where "on disk"))
        (doto (uses :lib.minibuffer) (tset :name "Minibuffer repeat")
          (tset :where "in memory"))
        (doto (uses :launcher) (tset :name "Launcher usage counts")
          (tset :where "on disk"))
        {:name "Hammerspoon console" :where "in memory" :unit "line"
         :count console-lines
         :clear hs.console.clearConsole}
        (doto (uses :timeconvert) (tset :name "Recent time zones")
          (tset :where "on disk"))
        (doto (uses :spaces) (tset :name "Recent Spaces") (tset :where "on disk"))
        (doto (uses :radio) (tset :name "Last radio station") (tset :where "on disk"))])

(fn count-of [h]
  (let [(ok n) (pcall h.count)]
    (if ok n 0)))

(fn clear [h]
  "Clear H, and the minibuffer's repeat state, which may show its entries."
  (minibuffer.clear-history)
  (let [(ok err) (pcall h.clear)]
    (when (not ok) (print (.. "privacy: couldn't clear " h.name ": " (tostring err))))
    ok))

(fn clear-all []
  "Clear every history."
  (let [failed (icollect [_ h (ipairs histories)]
                 (when (not (clear h)) h.name))]
    (minibuffer.message
     (if (= (length failed) 0)
         "Cleared all history"
         (.. "Couldn't clear " (table.concat failed ", "))))))

(fn confirm-clear-all []
  "Clear every history after asking."
  (minibuffer.y-or-n-p "Clear all history?" clear-all))

(fn entries [n unit]
  (if (= unit "line") (.. n (if (= n 1) " line" " lines"))
      (= n 1) "1 entry"
      (.. n " entries")))

(fn clear-history []
  "Pick a history, or all of them, and clear it after asking."
  (let [total (accumulate [n 0 _ h (ipairs histories)]
                (if h.unit n (+ n (count-of h))))]
    (minibuffer.completing-read
     "Clear history: "
     (doto (icollect [_ h (ipairs histories)]
             (let [n (count-of h)]
               {:text h.name
                :annotation (.. (if (= n 0) "empty" (entries n h.unit))
                                "  " h.where)
                :history h}))
       (table.insert 1 {:text "Everything"
                        :annotation (.. (entries total) " and the console")}))
     {:no-repeat true
      :on-accept (fn [c]
                   (if (not c) nil
                       (not c.history) (confirm-clear-all)
                       (minibuffer.y-or-n-p
                        (.. "Clear " (c.text:lower) "?")
                        #(when (clear c.history)
                           (minibuffer.message (.. "Cleared " (c.text:lower)))))))})))

{: histories
 : clear-history
 : clear-all
 : confirm-clear-all}
