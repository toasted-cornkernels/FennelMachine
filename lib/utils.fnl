(fn global-filter
  []
  "
  Filter that includes full-screen apps
  "
  (let [filter (hs.window.filter.new)]
    (: filter :setAppFilter :Emacs {:allowRoles [:AXUnknown :AXStandardWindow :AXDialog :AXSystemDialog]})))

(fn desparsify [in-table]
  "Remove gaps in a possibly sparse table."
  (var count 0)
  (local out-table {})
  (each [key value (pairs in-table)] (set count (+ count 1))
    (tset out-table count value))
  out-table)

{:global-filter global-filter
 :desparsify    desparsify}
