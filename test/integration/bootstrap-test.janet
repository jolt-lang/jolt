(use ../../src/jolt/evaluator)
(use ../../src/jolt/types)
(use ../../src/jolt/reader)
(use ../../src/jolt/api)

(def ctx (init))
(def source (slurp "/Users/yogthos/src/sci/src/sci/impl/macros.cljc"))
(var s source)
(var count 0)

(while (> (length (string/trim s)) 0)
  (def [form rest] (parse-next s))
  (set s rest)
  (++ count)
  (if (not (nil? form))
    (do
      (printf "eval form %d..." count)
      (flush)
      (eval-form ctx @{} form)
      (printf " OK\n"))))

(printf "\n%d forms processed\n" count)
(printf "ns: %s\n" (ctx-current-ns ctx))

(let [ns (ctx-find-ns ctx "sci.impl.macros")]
  (printf "sci.impl.macros bindings:\n")
  (loop [[name v] :pairs (ns :mappings)]
    (printf "  %s: macro=%q\n" name (v :macro))))
