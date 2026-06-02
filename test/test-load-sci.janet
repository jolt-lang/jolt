(use ../src/jolt/evaluator)
(use ../src/jolt/types)
(use ../src/jolt/reader)
(use ../src/jolt/api)

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

# After loading, replace sci.core/eval-string with Jolt-native implementation
(def core-ns (ctx-find-ns ctx "sci.core"))

# Replace eval-string with native Jolt version
(defn jolt-eval-string
  [s &opt opts]
  (def forms (parse-string s))
  (eval-form ctx @{} @[{:jolt/type :symbol :ns nil :name "do"} forms]))

(def ev-var (ns-find core-ns "eval-string"))
(var-set ev-var jolt-eval-string)

(printf "\n--- Testing sci.core/eval-string (Jolt-native) ---\n")
(def result (try (jolt-eval-string "(+ 1 2 3)") ([err] (string "ERROR: " err))))
(printf "eval-string result: %q\n" result)

(def result2 (try (jolt-eval-string "(def x 42) x") ([err] (string "ERROR: " err))))
(printf "eval-string def+ref: %q\n" result2)

(when (> (length all-failures) 0)
  (printf "\n=== FAILURES ===\n")
  (each f all-failures
    (printf "[%s:%d] %s\n" (f :file) (f :form-number) (f :error))
    (printf "  form: %s\n" (f :form))))
