(use ../src/jolt/evaluator)
(use ../src/jolt/types)
(use ../src/jolt/reader)
(use ../src/jolt/api)
(use ../src/jolt/core)

(def ctx (init))

(defn load-file [ctx path]
  (var s (slurp path))
  (var count 0)
  (var ok 0)
  (var fail 0)
  (var failures @[])
  (while (> (length (string/trim s)) 0)
    (def [form rest] (parse-next s))
    (set s rest)
    (++ count)
    (if (not (nil? form))
      (do
        (printf "eval form %d..." count)
        (flush)
        (if (try
           (do (eval-form ctx @{} form) true)
           ([err]
            (printf " FAIL: %q\n" err)
            (array/push failures {:form-number count :error (string err) :form (string form)})
            false))
          (do
            (printf " OK\n")
            (++ ok))
          (++ fail)))))
  {:ok ok :fail fail :total count :failures failures})

(def sci-base "/Users/yogthos/src/sci/src/sci")

(def load-order @[
  ["impl/macros.cljc" nil]
  ["impl/protocols.cljc" nil]
  ["impl/types.cljc" nil]
  ["impl/unrestrict.cljc" nil]
  ["impl/vars.cljc" nil]
  ["lang.cljc" nil]
  ["impl/utils.cljc" nil]
  ["impl/namespaces.cljc" nil]
  ["core.cljc" nil]
])

(var total-ok 0)
(var total-fail 0)
(var all-failures @[])

(each [file expected-ns] load-order
  (def path (string sci-base "/" file))
  (printf "\n=== Loading %s ===\n" file)
  (def result (load-file ctx path))
  (printf "  Result: %d ok, %d fail, %d total\n" (result :ok) (result :fail) (result :total))
  (+= total-ok (result :ok))
  (+= total-fail (result :fail))
  (each f (result :failures)
    (array/push all-failures {:file file :form-number (f :form-number) :error (f :error) :form (f :form)})))

(printf "\n==============================\n")
(printf "TOTAL: %d ok, %d fail, %d total\n" total-ok total-fail (+ total-ok total-fail))
(printf "==============================\n")

# After loading, check what SCI namespaces exist
(printf "\ncurrent ns: %s\n" (ctx-current-ns ctx))
(printf "sci.core exists: %q\n" (not (nil? (ctx-find-ns ctx "sci.core"))))
(printf "total namespaces: %d\n" (length (keys ((ctx :env) :namespaces))))

# Initialize edamame shim (in core.janet) and test eval-string
(init-edamame-shim! ctx parse-string read-form)

# Check critical SCI namespaces
(printf "\n--- Critical SCI namespaces ---\n")
(def critical-ns ["sci.impl.interpreter" "sci.impl.parser" "sci.impl.analyzer" "sci.impl.opts"])
(each nsn critical-ns
  (def ns (ctx-find-ns ctx nsn))
  (printf "%s: %d bindings\n" nsn (if ns (length (keys (ns-map ns))) 0)))

(printf "\n--- Testing sci.core/eval-string ---\n")
(def core-ns (ctx-find-ns ctx "sci.core"))
(def ev (ns-find core-ns "eval-string"))
(if ev
  (do
    (printf "eval-string found, calling...\n")
    (flush)
    (def f (var-get ev))
    (def result (try (f "(+ 1 2 3)") ([err] (string "ERROR: " err))))
    (printf "eval-string result: %q\n" result))
  (printf "eval-string NOT found\n"))

(when (> (length all-failures) 0)
  (printf "\n=== FAILURES ===\n")
  (each f all-failures
    (printf "[%s:%d] %s\n" (f :file) (f :form-number) (f :error))
    (printf "  form: %s\n" (f :form))))
