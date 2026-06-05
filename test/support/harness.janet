# Shared test harness for Jolt.
#
# Two complementary styles:
#
#   defspec — data-driven behavioral tables, for spec/ and integration batteries.
#     Each case is ["label" expected actual] where `expected` and `actual` are
#     Clojure source strings. Equality is checked with Jolt's own `=`, so it is
#     representation-agnostic (a vector result compares equal to a vector
#     literal regardless of the underlying Janet type). Use the :throws sentinel
#     in the `expected` position to assert that `actual` raises an error.
#
#       (defspec "clojure.core / seq"
#         ["first of vector" "1"       "(first [1 2 3])"]
#         ["rest is a seq"   "(2 3)"   "(rest [1 2 3])"]
#         ["nth out of range" :throws  "(nth [1] 5)"])
#
#   jeval / expect= / expect-throws — assertion helpers for white-box unit/ tests
#     that probe a single component and want Janet-level assertions.
#
# A failing suite prints every failing behavior, then raises — so `jpm test`
# reports a non-zero exit and the offending behaviors are visible.

(use ../../src/jolt/api)

(defn jeval
  "Evaluate a Clojure source string in a fresh context, normalizing persistent
  vectors/lists to Janet tuples so results compare with `deep=`/tuple literals."
  [s]
  (normalize-pvecs (eval-string (init) s)))

(defn- show [s]
  (let [r (protect (eval-string (init) s))]
    (if (= (r 0) true)
      (string/format "%q" (normalize-pvecs (r 1)))
      (string "<error: " (r 1) ">"))))

(defn run-spec
  "Run a data-driven behavioral suite. See `defspec`."
  [suite cases]
  (var pass 0)
  (def fails @[])
  (each case cases
    (def label (in case 0))
    (def expected (in case 1))
    (def actual (in case 2))
    (if (= expected :throws)
      (let [r (protect (eval-string (init) actual))]
        (if (= (r 0) false)
          (++ pass)
          (array/push fails [label "expected an error, got a value"])))
      (let [r (protect (eval-string (init) (string "(= " expected " " actual ")")))]
        (cond
          (not= (r 0) true) (array/push fails [label (string "errored: " (r 1))])
          (= (r 1) true) (++ pass)
          (array/push fails [label (string "want " expected ", got " (show actual))])))))
  (printf "  %s: %d/%d" suite pass (length cases))
  (each [l m] fails (printf "    FAIL [%s] %s" l m))
  (when (> (length fails) 0)
    (error (string suite ": " (length fails) " failing behavior(s)")))
  pass)

(defmacro defspec
  "Define and immediately run a behavioral suite of [label expected actual] cases."
  [suite & cases]
  ~(,run-spec ,suite [,;cases]))

# --- white-box assertion helpers (unit tests) ---

(defn expect=
  "Assert that evaluating Clojure `s` yields `expected` (a Janet value, compared
  with deep= after normalizing persistent collections to tuples)."
  [expected s]
  (let [got (jeval s)]
    (assert (deep= expected got)
            (string "expected " (string/format "%q" expected)
                    ", got " (string/format "%q" got) " for: " s))))

(defn expect-throws
  "Assert that evaluating Clojure `s` raises an error."
  [s]
  (let [r (protect (eval-string (init) s))]
    (assert (= (r 0) false) (string "expected an error for: " s))))
