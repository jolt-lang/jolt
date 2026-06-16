# Predicate folding from inference (jolt-wcw): when the collection-type
# inference PROVES the argument's type, a type predicate (number?/string?/
# keyword?/nil?/some?/record?) folds to a compile-time boolean constant, which
# the trailing const-fold then propagates — collapsing any `if` it gates to the
# taken branch. Sound: only a provable answer folds, and only when the argument
# is side-effect-free (a local or const), so dropping its evaluation is a no-op.
# Mirrors type-infer-test.janet's harness (count a marker in the emitted IR to
# prove the optimization fired, then evaluate to prove it stayed correct).
(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/reader :as reader)

(print "Predicate folding (jolt-wcw)...")

(os/setenv "JOLT_DIRECT_LINK" "1")
(def ctx (api/init-cached {:compile? true}))
(api/eval-string ctx "(ns pf)")

(defn code [src]
  (string/format "%p" (backend/emit-ir ctx (backend/analyze-form ctx (reader/parse-string src)))))
(defn occurs [src needle] (length (string/find-all needle (code src))))
(defn ev [src] (api/eval-string ctx src))

# --- the predicate call is gone where the type is proven --------------------
(assert (= 0 (occurs "(fn [] (let [x (+ 1 2)] (number? x)))" "number?"))
        "number? on proven :num -> folded, call eliminated")
(assert (= 0 (occurs "(fn [] (let [x \"hi\"] (string? x)))" "string?"))
        "string? on proven :str -> folded")
(assert (= 0 (occurs "(fn [] (let [x :k] (keyword? x)))" "keyword?"))
        "keyword? on proven :kw -> folded")
(assert (= 0 (occurs "(fn [] (let [x (+ 1 2)] (nil? x)))" "nil?"))
        "nil? on a provably non-nil value -> folded")

# --- the folded constant collapses an if it gates --------------------------
# the dead branch's literal (200) must be gone after dead-branch removal
(assert (= 0 (occurs "(fn [] (let [x (+ 1 2)] (if (number? x) 100 200)))" "200"))
        "true predicate folds the if to its then-branch (dead 200 dropped)")
(assert (= 0 (occurs "(fn [] (let [x (+ 1 2)] (if (string? x) 100 200)))" "100"))
        "false predicate folds the if to its else-branch (dead 100 dropped)")

# --- sound fallback: unknown type or impure arg keeps the call -------------
# a param is :any (Phase 0 doesn't type it) -> no fold
(assert (= 1 (occurs "(fn [m] (number? m))" "number?"))
        "unknown-type arg keeps the predicate call")
# arg type is proven :num but the arg has side effects (a call) -> must NOT
# drop its evaluation, so the predicate is left in place
(assert (>= (occurs "(fn [g] (number? (+ (g) 1)))" "number?") 1)
        "impure arg (even with proven type) keeps the predicate call")

# --- correctness: folded path evaluates to the dispatched path -------------
(assert (= true  (ev "((fn [] (let [x (+ 1 2)] (number? x))))")) "number? true value")
(assert (= false (ev "((fn [] (let [x \"hi\"] (number? x))))")) "number? false value")
(assert (= true  (ev "((fn [] (let [x :k] (keyword? x))))")) "keyword? true value")
(assert (= false (ev "((fn [] (let [x 5] (nil? x))))")) "nil? false value")
(assert (= true  (ev "((fn [] (let [x 5] (some? x))))")) "some? true value")
(assert (= :yes  (ev "((fn [] (let [x 5] (if (number? x) :yes :no))))")) "gated if takes proven branch")
(assert (= :no   (ev "((fn [] (let [x 5] (if (string? x) :yes :no))))")) "gated if drops false branch")
# impure arg still runs its side effect and returns the right answer
(assert (= 6 (ev "((fn [g] (if (number? (+ (g) 1)) (+ (g) 5) 0)) (fn [] 1))")) "impure-arg predicate stays correct")

(print "Predicate folding (jolt-wcw) passed!")
