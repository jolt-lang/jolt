# ^:struct type hint (jolt-dad). A constant-keyword lookup on a local hinted
# ^:struct skips the :jolt/type guard and emits a bare get (~20ns vs ~36ns),
# the way Clojure type hints let the compiler specialize. The hint is a
# programmer assertion (a lie just makes the raw get return the wrong thing,
# same contract as ^String); these tests pin that an ACCURATE hint is
# correctness-preserving, that it drops the guard, and that it survives inlining.
(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/reader :as reader)

(print "Struct hint (jolt-dad)...")

(os/setenv "JOLT_DIRECT_LINK" "1")  # inline on, so hint-through-inline is exercised
(def ctx (api/init {:compile? true}))
(api/eval-string ctx "(ns sh)")
(each s ["(defn v3 [r g b] {:r r :g g :b b})"
         "(defn dot [^:struct l ^:struct r] (+ (+ (* (:r l) (:r r)) (* (:g l) (:g r))) (* (:b l) (:b r))))"
         "(defn sub [^:struct l ^:struct r] {:r (- (:r l) (:r r)) :g (- (:g l) (:g r)) :b (- (:b l) (:b r))})"
         "(defn lensq [^:struct v] (dot v v))"]
  (api/eval-string ctx s))

(defn guards [src]
  (def code (string/format "%p" (backend/emit-ir ctx (backend/analyze-form ctx (reader/parse-string src)))))
  (length (string/find-all ":jolt/type" code)))

# the guard is dropped for hinted subjects, kept for unhinted ones
(assert (= 1 (guards "(fn [v] (:r v))")) "unhinted (:r v) keeps the guard")
(assert (= 0 (guards "(fn [^:struct v] (:r v))")) "hinted (:r v) drops the guard")
(assert (= 0 (guards "(fn [^:struct v] (+ (+ (:r v) (:g v)) (:b v)))")) "all three hinted lookups bare")
(assert (= 0 (guards "(fn [^:struct v] (lensq v))")) "hint survives through an inlined call")

# accurate hints are correctness-preserving (value identical to the guarded path)
(assert (= 32 (api/eval-string ctx "(dot (v3 1 2 3) (v3 4 5 6))")) "hinted dot value")
(assert (= 14 (api/eval-string ctx "(lensq (v3 1 2 3))")) "hinted lensq (inline-flow) value")
(assert (= 7 (api/eval-string ctx "(:r (sub (v3 9 8 7) (v3 2 0 0)))")) "hinted sub field")
# a hinted value flowing through an inlined call still reads correctly
(api/eval-string ctx "(defn hit [^:struct ray ^:struct c] (lensq (sub (:origin ray) c)))")
(assert (= 48 (api/eval-string ctx "(hit {:origin (v3 5 5 5) :direction (v3 0 0 0)} (v3 1 1 1))"))
        "hinted value through nested inline reads correctly")

# a missing key on a hinted struct still reads nil (struct miss), like a guarded get
(assert (= nil (api/eval-string ctx "((fn [^:struct m] (:absent m)) (v3 1 2 3))")) "hinted struct miss -> nil")

(print "Struct hint passed!")
