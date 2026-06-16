# Type hints driving keyword-lookup specialization (jolt-94n). A local hinted
# ^:struct (a plain struct/record map) or ^Record (a defrecord/deftype) lets a
# constant-keyword lookup skip the :jolt/type guard and emit a bare get
# (~20ns vs ~36ns), the way Clojure type hints let the compiler specialize.
# Covers both (:k m) and (get m :k), hint propagation through inlining, the
# ^Record path, the JOLT_CHECK_HINTS dev aid, and that accurate hints preserve
# results. An inaccurate hint is a programmer error (like a wrong ^String): the
# raw get returns the wrong value, surfaced only under JOLT_CHECK_HINTS.
(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/reader :as reader)

(print "Type hints (jolt-94n)...")

(os/setenv "JOLT_DIRECT_LINK" "1")  # inline on, so hint-through-inline is exercised
(def ctx (api/init-cached {:compile? true}))
(api/eval-string ctx "(ns sh)")
(api/eval-string ctx "(defrecord Vec3r [r g b])")
(each s ["(defn v3 [r g b] {:r r :g g :b b})"
         "(defn dot [^:struct l ^:struct r] (+ (+ (* (:r l) (:r r)) (* (:g l) (:g r))) (* (:b l) (:b r))))"
         "(defn sub [^:struct l ^:struct r] {:r (- (:r l) (:r r)) :g (- (:g l) (:g r)) :b (- (:b l) (:b r))})"
         "(defn lensq [^:struct v] (dot v v))"]
  (api/eval-string ctx s))

(defn guards [src]
  (def code (string/format "%p" (backend/emit-ir ctx (backend/analyze-form ctx (reader/parse-string src)))))
  (length (string/find-all ":jolt/type" code)))

# --- guard removal ----------------------------------------------------------
(assert (= 1 (guards "(fn [v] (:r v))")) "unhinted (:r v) keeps the guard")
(assert (= 0 (guards "(fn [^:struct v] (:r v))")) "^:struct (:r v) drops the guard")
(assert (= 0 (guards "(fn [^Vec3r v] (:r v))")) "^Record (:r v) drops the guard")
(assert (= 1 (guards "(fn [^String v] (:r v))")) "^String (not a record) still guards")
(assert (= 0 (guards "(fn [^:struct v] (+ (+ (:r v) (:g v)) (:b v)))")) "all three hinted lookups bare")
(assert (= 0 (guards "(fn [^:struct v] (lensq v))")) "hint survives through an inlined call")
# hints work on let bindings too, not just params (init is a plain local here,
# so the only candidate guard is the hinted (:r v))
(assert (= 0 (guards "(fn [^:struct s] (let [^:struct v s] (:r v)))")) "^:struct on a let binding drops the guard")
(assert (= 1 (guards "(fn [s] (let [v s] (:r v)))")) "unhinted let binding keeps the guard")
# (get m :k) gets the same treatment as (:k m)
(assert (= 1 (guards "(fn [m] (get m :k))")) "unhinted (get m :k) is guarded-inline")
(assert (= 0 (guards "(fn [^:struct m] (get m :k))")) "^:struct (get m :k) drops the guard")
(assert (= 0 (guards "(fn [^Vec3r m] (get m :k 0))")) "^Record (get m :k d) drops the guard")
# a variable (non-constant) key isn't a keyword literal, so the inline doesn't
# fire — it falls through to core-get, which still indexes correctly.
(assert (= 2 (api/eval-string ctx "((fn [m kk] (get m kk)) {:a 2} :a)")) "variable-key get via core-get")
(assert (= 10 (api/eval-string ctx "((fn [m i] (get m i)) [10 20] 0)")) "variable-key get indexes a vector")

# --- correctness (accurate hints preserve results) --------------------------
(assert (= 32 (api/eval-string ctx "(dot (v3 1 2 3) (v3 4 5 6))")) "hinted dot value")
(assert (= 14 (api/eval-string ctx "(lensq (v3 1 2 3))")) "hinted lensq (inline-flow) value")
(assert (= 7 (api/eval-string ctx "(:r (sub (v3 9 8 7) (v3 2 0 0)))")) "hinted sub field")
(api/eval-string ctx "(defn hit [^:struct ray ^:struct c] (lensq (sub (:origin ray) c)))")
(assert (= 48 (api/eval-string ctx "(hit {:origin (v3 5 5 5) :direction (v3 0 0 0)} (v3 1 1 1))"))
        "hinted value through nested inline reads correctly")
(assert (= nil (api/eval-string ctx "((fn [^:struct m] (:absent m)) (v3 1 2 3))")) "hinted struct miss -> nil")
(assert (= 9 (api/eval-string ctx "((fn [^:struct m] (get m :absent 9)) (v3 1 2 3))")) "hinted get default")
# field access on a real record instance through a ^Record hint
(api/eval-string ctx "(defn vr-x [^Vec3r v] (:r v))")
(assert (= 5 (api/eval-string ctx "(vr-x (->Vec3r 5 6 7))")) "record field via ^Record hint")
# (get m :k) on assorted reps still matches core-get semantics (unhinted path)
(assert (= 2 (api/eval-string ctx "(get {:a 2} :a)")) "get struct present")
(assert (= nil (api/eval-string ctx "(get {:a 2} :z)")) "get struct miss")
(assert (= 1 (api/eval-string ctx "(get (hash-map :a 1 :x nil) :a)")) "get phm present")
(assert (= nil (api/eval-string ctx "(get (hash-map :a 1 :x nil) :x)")) "get phm nil value")
(assert (= 7 (api/eval-string ctx "(get (sorted-map :a 7) :a)")) "get sorted present")

# --- checked mode: a lying hint throws (separate ctx with the flag on) -------
(os/setenv "JOLT_CHECK_HINTS" "1")
(def cctx (api/init {:compile? true}))
(api/eval-string cctx "(ns ck)")
(api/eval-string cctx "(defn rd [^:struct m] (:a m))")
(assert (= 1 (api/eval-string cctx "(rd {:a 1 :b 2})")) "checked mode: accurate hint still works")
(let [r (protect (api/eval-string cctx "(rd (hash-map :a 1 :x nil))"))]
  (assert (not (r 0)) "checked mode: lying ^:struct hint throws")
  (assert (string/find "type hint violated" (string (r 1))) "checked-mode error is meaningful"))
(os/setenv "JOLT_CHECK_HINTS" nil)

(print "Type hints passed!")
