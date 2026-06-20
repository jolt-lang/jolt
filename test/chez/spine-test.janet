# Chez Phase 3 inc6 (jolt-hs9n) — the zero-Janet spine.
#
# Validates that the analyzer + emitter, cross-compiled to Scheme and run ON CHEZ
# over the host contract (host-contract.ss), compile and run macro-free Clojure
# from source with NO Janet in the loop: read (reader.ss) -> analyze (jolt.analyzer
# on Chez) -> IR -> emit (jolt.backend-scheme on Chez) -> eval.
#
# Oracle = the Janet-hosted analyzer through the SAME Chez emitter/RT/printer
# (d/eval-e-with-prelude): the only difference under test is WHERE analysis runs
# (Janet vs Chez), so equal stdout means moving analysis onto Chez is behavior-
# preserving. Macros (let/when/->/defn) are inc6b (jolt-r8ku, runtime macros).
#
#   janet test/chez/spine-test.janet
(import ../../src/jolt/api :as api)
(import ../../host/chez/driver :as d)
(import ../../host/chez/jolt-chez :as jc)

(var total 0) (var fails 0)
(defn ok [name pred &opt extra]
  (++ total)
  (if pred (printf "ok: %s" name)
    (do (++ fails) (printf "FAIL: %s   %s" name (or extra "")))))

(unless (d/chez-available?)
  (print "chez not on PATH — skipping spine-test")
  (os/exit 0))

(def ctx (d/make-ctx))
(def prelude-path (jc/ensure-prelude ctx))

# compiler image cache, keyed by the cross-compiled sources + the host contract.
(defn- image-fingerprint []
  (string/slice (string (hash (string/join
    (map slurp ["jolt-core/jolt/ir.clj" "jolt-core/jolt/analyzer.clj"
                "jolt-core/jolt/backend_scheme.clj" "host/chez/host-contract.ss"
                "host/chez/compile-eval.ss"])))) 0))
(def image-path
  (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-compiler-image-" (image-fingerprint) ".ss"))
(d/ensure-compiler-image ctx image-path)

# Each case: the Chez-hosted spine value must equal the Janet-hosted oracle value
# (both printed via jolt-final-str on Chez).
(defn check [src]
  (def [ocode oout oerr] (d/eval-e-with-prelude ctx src prelude-path))
  (def [acode aout aerr] (d/eval-zero-janet prelude-path image-path src))
  (cond
    (= ocode :emit-err) (ok src false (string "oracle emit-err: " oout))
    (not (zero? acode)) (ok src false (string "zero-janet exit " acode ": " aerr " | out=" aout))
    (ok src (= oout aout) (string "chez=" aout " oracle=" oout))))

# macro-free forms: handled specials (if/do/fn*), native ops, consts, invoke.
(each src
  ["(if true 10 20)"
   "(if false 10 20)"
   "(do 1 2 3)"
   "(+ 1 2)"
   "(- 10 3 2)"
   "((fn* [x] (* x x)) 7)"
   "((fn* [x] (+ x 1)) 5)"
   "((fn* [a b] (+ a b)) 3 4)"
   "(if (< 3 5) :yes :no)"
   "(if (> 3 5) :yes :no)"
   "((fn* [x] (if x :t :f)) true)"
   "(do (if true 1 2) (* 6 7))"
   "((fn* [n] (* n n n)) 4)"
   "(< 1 2)"
   "(= 5 5)"]
  (check src))

# inc6b (jolt-r9lm): runtime macros — the on-Chez analyzer expands core macros
# (emitted into the prelude as expander fns + a macro flag). Same oracle: the
# Janet analyzer expands them at analyze time, the value must match.
(each src
  ["(when true 1)"
   "(when false 1)"
   "(when true 1 2 3)"
   "(when-not false 5)"
   "(let [a 1] (+ a 2))"
   "(let [a 1 b 2] (+ a b))"
   "(let [a 1 b (+ a 1)] (* a b))"
   "(-> 1 inc inc)"
   "(-> 5 (- 2))"
   "(->> 3 (- 10))"
   "(and 1 2 3)"
   "(and 1 false 3)"
   "(or nil 5)"
   "(or false nil 7)"
   "(cond false 1 true 2)"
   "(cond false 1 :else 3)"
   "(if-not false :a :b)"
   "(do (defn f [x] (* x x)) (f 6))"
   "(do (defn g [x y] (+ x y 1)) (g 3 4))"
   "(let [a 1] (when (< a 5) (-> a inc inc)))"]
  (check src))

(printf "\n%d/%d ok" (- total fails) total)
(when (> fails 0) (os/exit 1))
