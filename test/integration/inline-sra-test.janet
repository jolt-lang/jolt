# Inline + scalar-replacement passes (jolt-87f, Route 1 AOT escape analysis).
# When a unit opts into direct-linking (:inline?, JOLT_DIRECT_LINK=1), the IR
# pipeline inlines small direct-linked fns and then scalar-replaces the now-
# exposed non-escaping map allocations: (:r {:r a ..}) -> a. This pins the
# transform (allocations actually vanish) AND that it stays semantics-preserving.
(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/reader :as reader)

(print "Inline + scalar replacement (jolt-87f)...")

# A ctx with inlining ON (independent of the build-time JOLT_DIRECT_LINK).
(def ctx (api/init {:compile? true}))
(put (ctx :env) :direct-linking? true)
(put (ctx :env) :inline? true)
(api/eval-string ctx "(ns rt)")
(each s ["(defn v3 [r g b] {:r r :g g :b b})"
         "(defn scale [l n] {:r (* (:r l) n) :g (* (:g l) n) :b (* (:b l) n)})"
         "(defn add [l r] {:r (+ (:r l) (:r r)) :g (+ (:g l) (:g r)) :b (+ (:b l) (:b r))})"
         "(defn dot [l r] (+ (+ (* (:r l) (:r r)) (* (:g l) (:g r))) (* (:b l) (:b r))))"
         "(defn sub [l r] {:r (- (:r l) (:r r)) :g (- (:g l) (:g r)) :b (- (:b l) (:b r))})"
         "(defn reflect [v n] (sub v (scale n (* 2.0 (dot v n)))))"
         # self-recursive: must NOT be inlined into callers (its body has a free
         # local — the fn-name self-reference — that would dangle when spliced).
         "(defn countdown [n] (if (< n 1) :done (countdown (- n 1))))"]
  (api/eval-string ctx s))

(defn alloc-count [src]
  # struct / build-map literal occurrences in the emitted Janet = surviving map
  # allocations (jolt builds a struct, falling back to build-map-literal).
  (def code (string/format "%p" (backend/emit-ir ctx (backend/analyze-form ctx (reader/parse-string src)))))
  [(length (string/find-all "struct " code))
   (length (string/find-all "build-map" code))])

# A vec3 chain whose intermediates never escape collapses to ONE result map.
(let [[s b] (alloc-count "(fn [v n] (reflect v n))")]
  (assert (= 1 s) (string "reflect keeps exactly one alloc, got " s " struct"))
  (assert (= 1 b) (string "reflect keeps exactly one build-map fallback, got " b)))

# A fully consumed chain (result not returned as a map) allocates NOTHING.
(let [[s b] (alloc-count "(fn [v n] (dot (reflect v n) (reflect v n)))")]
  (assert (= 0 s) (string "fully-consumed chain allocates no struct, got " s))
  (assert (= 0 b) (string "fully-consumed chain has no build-map fallback, got " b)))

# Loop bodies optimize too (recur is not a blanket escape).
(let [[s _] (alloc-count "(fn [k] (loop [i 0 acc 0.0] (if (< i k) (recur (inc i) (+ acc (dot (v3 1.0 2.0 3.0) (v3 0.1 0.2 0.3)))) acc)))")]
  (assert (= 0 s) (string "loop body allocates no struct, got " s)))

# Correctness: inlined results match the obvious computation.
(assert (= 32.0 (api/eval-string ctx "(dot (v3 1.0 2.0 3.0) (v3 4.0 5.0 6.0))")) "dot value")
(assert (= 9.0 (api/eval-string ctx "(:r (add (v3 1.0 0.0 0.0) (scale (v3 4.0 0.0 0.0) 2.0)))")) "add+scale value")
# the self-recursive fn still runs (the closed-body guard kept it un-inlined)
(assert (= :done (api/eval-string ctx "(countdown 5)")) "recursive fn still works")

# A redefinable (^:redef) callee must NOT be inlined — it stays a live var call.
(api/eval-string ctx "(defn ^:redef wobble [x] {:v x})")
(let [[s _] (alloc-count "(fn [] (:v (wobble 1)))")]
  # wobble is not inlined, so its map isn't visible to scalar replacement: the
  # lookup stays a call, and the (:v ...) result is whatever wobble returns.
  (assert (= 7 (do (api/eval-string ctx "(defn ^:redef wobble [x] {:v (+ x 6)})")
                   (api/eval-string ctx "(:v (wobble 1))")))
          "redef callee stays live (redefinition is visible)"))

(print "Inline + scalar replacement passed!")
