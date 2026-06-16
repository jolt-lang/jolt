# Scalar-replacement of short-lived RECORD allocations (jolt-15jq). The pass
# already folds the const-key MAP-literal form ((:k {:k a ..}) -> a and drops a
# non-escaping let-bound map); this extends it to record CONSTRUCTORS. A record
# ctor (->Rec a b ..) is a positional struct whose declared field order lives in
# the record-shapes registry, so a field read on a non-escaping ctor result folds
# to the corresponding positional arg and the allocation disappears.
#
# Probe: count occurrences of the ctor var "->V3" in the analyzed IR. Folded =>
# the ctor is gone (0). Mirrors type-infer-test's guard-counting harness.
(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/reader :as reader)

(print "Scalar-replace of records (jolt-15jq)...")

(os/setenv "JOLT_DIRECT_LINK" "1")
(def ctx (api/init-cached {:compile? true}))
(api/eval-string ctx "(ns srr)")
(api/eval-string ctx "(defrecord V3 [r g b])")

(defn ctors [src]
  (length (string/find-all "->V3"
                           (string/format "%p" (backend/analyze-form ctx (reader/parse-string src))))))
(defn ev [src] (api/eval-string ctx src))

# --- direct form: (:field (->V3 a b c)) -> the positional arg ----------------
(assert (= 0 (ctors "(fn [] (:r (->V3 1 2 3)))")) "direct record lookup :r -> arg, ctor gone")
(assert (= 0 (ctors "(fn [] (:g (->V3 1 2 3)))")) "direct record lookup :g -> arg, ctor gone")
(assert (= 0 (ctors "(fn [] (:b (->V3 1 2 3)))")) "direct record lookup :b -> arg, ctor gone")
# pure non-constant args fold too (each discarded sibling is pure)
(assert (= 0 (ctors "(fn [a b] (:r (->V3 (+ a 1) (* b 2) 7)))")) "direct fold with pure arith args")

# --- let form: non-escaping let-bound record, field reads -> args ------------
(assert (= 0 (ctors "(fn [a b c] (let [v (->V3 a b c)] (+ (:r v) (:g v) (:b v))))")) "let-bound record, all field reads folded")
(assert (= 0 (ctors "(fn [a b c] (let [v (->V3 a b c)] (:r v)))")) "let-bound record, single field read folded (siblings discarded, pure)")

# --- sound fallbacks: keep the allocation ------------------------------------
# escaping record (passed to a sink) must NOT be folded
(assert (>= (ctors "(fn [sink a b c] (let [v (->V3 a b c)] (sink v) (:r v)))") 1) "escaping record keeps the allocation")
# returning the record escapes it
(assert (>= (ctors "(fn [a b c] (->V3 a b c))") 1) "returned record keeps the allocation")
# a non-field key read (:jolt/deftype is a virtual key) -> not folded, keep alloc
(assert (>= (ctors "(fn [a b c] (let [v (->V3 a b c)] (:jolt/deftype v)))") 1) ":jolt/deftype lookup keeps the allocation")

# --- correctness: folded path evaluates identically -------------------------
(assert (= 1 (ev "((fn [] (:r (->V3 1 2 3))))")) "direct :r value")
(assert (= 2 (ev "((fn [] (:g (->V3 1 2 3))))")) "direct :g value")
(assert (= 3 (ev "((fn [] (:b (->V3 1 2 3))))")) "direct :b value")
(assert (= 6 (ev "((fn [a b c] (let [v (->V3 a b c)] (+ (:r v) (:g v) (:b v)))) 1 2 3)")) "let-bound sum value")
(assert (= 10 (ev "((fn [a b] (:r (->V3 (+ a 1) (* b 2) 7))) 9 5)")) "arith-arg direct value")
# correctness of the kept-allocation fallbacks
(assert (= 1 (ev "((fn [sink a b c] (let [v (->V3 a b c)] (sink v) (:r v))) (fn [_] nil) 1 2 3)")) "escaping record reads correctly")
(assert (= "srr.V3" (ev "((fn [a b c] (let [v (->V3 a b c)] (:jolt/deftype v))) 1 2 3)")) ":jolt/deftype reads the type tag")

# --- nested records fold compositionally (bottom-up) -------------------------
(api/eval-string ctx "(defrecord Ray [orig dir])")
# (:r (:orig (->Ray (->V3 a b c) d))): inner ctors both fold away
(assert (= 0 (ctors "(fn [a b c d] (:r (:orig (->Ray (->V3 a b c) d))))")) "nested record reads fold both ctors")
(assert (= 7 (ev "((fn [a b c d] (:r (:orig (->Ray (->V3 a b c) d)))) 7 8 9 0)")) "nested record fold value")

(print "Scalar-replace of records passed!")
