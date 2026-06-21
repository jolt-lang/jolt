# jolt-r8ku (inc Y) — Chez-side Clojure data reader: read-string / read /
# read+string / with-in-str / clojure.edn. The reader (host/chez/reader.ss)
# produces jolt forms directly; the *in* family and
# clojure.edn are Clojure over the read-string / __parse-next seams.
#
# Outputs are kept order-stable (equality checks, scalars) so set/map iteration
# order — which is host-dependent — doesn't masquerade as a
# divergence.
#
#   janet test/chez/_reader.janet
(def jolt-bin (or (os/getenv "JOLT_BIN") "bin/joltc"))

(def cases
  [# --- scalars + collections (value equality, order-independent) ---
   ["(= 42 (read-string \"42\"))"                                  "true"]
   ["(= 42 (read-string \"0x2A\"))"                                "true"]
   # numeric tower (jolt-n6al): "1/2" reads as an exact Ratio (= JVM), so a
   # category-aware = against the double 0.5 is false; assert numeric value (==),
   # which holds in both the all-flonum and tower models.
   ["(== 0.5 (read-string \"1/2\"))"                               "true"]
   ["(= -3.5 (read-string \"-3.5\"))"                              "true"]
   ["(== 1000.0 (read-string \"1e3\"))"                            "true"]
   ["(= 1 (read-string \"1N\"))"                                   "true"]
   ["(integer? (read-string \"7\"))"                               "true"]
   ["(= :foo (read-string \":foo\"))"                              "true"]
   ["(= :a/b (read-string \":a/b\"))"                              "true"]
   ["(= :foo (read-string \"::foo\"))"                             "true"]
   ["(= (quote sym) (read-string \"sym\"))"                        "true"]
   ["(= (quote ns/sym) (read-string \"ns/sym\"))"                  "true"]
   ["(nil? (read-string \"nil\"))"                                 "true"]
   ["(true? (read-string \"true\"))"                               "true"]
   ["(false? (read-string \"false\"))"                             "true"]
   ["(= \\a (read-string \"\\\\a\"))"                              "true"]
   ["(= \\newline (read-string \"\\\\newline\"))"                  "true"]
   ["(= \\space (read-string \"\\\\space\"))"                      "true"]
   ["(= \"a\\nb\" (read-string \"\\\"a\\\\nb\\\"\"))"              "true"]
   ["(= [1 2 3] (read-string \"[1 2 3]\"))"                        "true"]
   ["(= (quote (+ 1 2)) (read-string \"(+ 1 2)\"))"               "true"]
   ["(= {:a 1 :b 2} (read-string \"{:a 1 :b 2}\"))"               "true"]
   # core read-string returns the raw set FORM (edn->value builds the real set)
   ["(:value (read-string \"#{1 2 3}\"))"                          "[1 2 3]"]
   ["(= :jolt/set (:jolt/type (read-string \"#{1 2 3}\")))"        "true"]
   ["(= [1 [2 3] {:k :v}] (read-string \"[1 [2 3] {:k :v}]\"))"   "true"]
   # --- reader macros ---
   ["(= (quote (quote x)) (read-string \"'x\"))"                  "true"]
   ["(= (quote (clojure.core/deref a)) (read-string \"@a\"))"     "true"]
   ["(= (quote (syntax-quote (a (unquote b)))) (read-string \"`(a ~b)\"))" "true"]
   ["(= (quote (unquote-splicing xs)) (read-string \"~@xs\"))"    "true"]
   # --- whitespace / comments / discard ---
   ["(= 42 (read-string \"; comment\\n42\"))"                     "true"]
   ["(= [1 2] (read-string \"[1 #_ 9 2]\"))"                       "true"]
   ["(nil? (read-string \"\"))"                                    "true"]
   ["(nil? (read-string \"  , ,\"))"                               "true"]
   # --- metadata ^ on a symbol ---
   ["(:tag (meta (read-string \"^String x\")))"                   "String"]
   ["(:foo (meta (read-string \"^:foo x\")))"                     "true"]
   # --- *in* reader family: read / read+string / with-in-str ---
   ["(= 42 (with-in-str \"42\" (read)))"                          "true"]
   ["(= (quote (+ 1 2)) (with-in-str \"(+ 1 2)\" (read)))"        "true"]
   ["(with-in-str \"1 2\" [(read) (read)])"                       "[1 2]"]
   ["(= :done (with-in-str \"\" (read *in* false :done)))"        "true"]
   ["(let [[v s] (with-in-str \"42 rest\" (read+string))] (and (= v 42) (string? s)))" "true"]
   ["(= [1 2] (with-in-str \"1 2\" [(first (read+string)) (first (read+string))]))" "true"]
   # --- clojure.edn (set/tagged forms built into real values) ---
   ["(do (require (quote [clojure.edn :as e0])) (= #{1 2} (e0/read-string \"#{1 2}\")))" "true"]
   ["(do (require (quote [clojure.edn :as e0])) (uuid? (e0/read-string \"#uuid \\\"550e8400-e29b-41d4-a716-446655440000\\\"\")))" "true"]
   ["(do (require (quote [clojure.edn :as e0])) (inst? (e0/read-string \"#inst \\\"2020-01-01T00:00:00Z\\\"\")))" "true"]
   ["(do (require (quote [clojure.edn :as e0])) (= :end (e0/read-string {:eof :end} \"\")))" "true"]
   ["(do (require (quote [clojure.edn :as e0])) (= [:custom 5] (e0/read-string {:readers {(quote custom) (fn [v] [:custom v])}} \"#custom 5\")))" "true"]
   ["(do (require (quote clojure.edn)) (= {:a 1 :b 2} (clojure.edn/read-string \"{:a 1\\n :b 2}\")))" "true"]])

(defn run-capture [bin expr]
  (def proc (os/spawn [bin "-e" expr] :p {:out :pipe :err :pipe}))
  (def out (ev/read (proc :out) 0x100000))
  (def err (ev/read (proc :err) 0x100000))
  (def code (os/proc-wait proc))
  (def lines (filter (fn [l] (not (empty? l)))
                     (string/split "\n" (string/trim (if out (string out) "")))))
  [code (if (empty? lines) "" (last lines)) (string/trim (if err (string err) ""))])

(var pass 0)
(def fails @[])
(each [expr expected] cases
  (def [code got err] (run-capture jolt-bin expr))
  (cond
    (not= code 0) (array/push fails [expr (string "exit " code "; err: " err)])
    (= got expected) (++ pass)
    (array/push fails [expr (string "want `" expected "`, got `" got "`")])))

(printf "\n_reader parity [%s]: %d/%d passed" jolt-bin pass (length cases))
(when (> (length fails) 0)
  (printf "%d FAIL(s):" (length fails))
  (each [e m] fails (printf "  FAIL %s\n    %s" e m)))
(flush)
(os/exit (if (empty? fails) 0 1))
