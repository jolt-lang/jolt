# Stage 2 Task 3: the compile path's interpreter fallback is DELIBERATE-ONLY.
#
# compile-and-eval used to wrap the compile step in a blanket protect — ANY
# failure (including a genuine compiler bug) silently fell back to the
# interpreter, hiding the bug behind a correct-looking result. Now only the
# analyzer's deliberate punt signal ("jolt/uncompilable: …", raised for the
# curated stateful/letrec set) may fall back; any other compile-step error
# propagates. Verified here by stubbing jolt.analyzer/analyze.

(use ../../src/jolt/types)
(use ../../src/jolt/api)
(use ../../src/jolt/reader)
(import ../../src/jolt/backend :as backend)

(def ctx (init))

# 1. A deliberate punt (letfn needs letrec IR) falls back and evaluates correctly.
(assert (= 3 (backend/compile-and-eval ctx (parse-string "(letfn [(f [n] (+ n 1))] (f 2))")))
        "deliberate uncompilable punt falls back to the interpreter")

(assert (backend/analyzer-built? ctx) "analyzer built")
(def analyze-var (ns-find (ctx-find-ns ctx "jolt.analyzer") "analyze"))
(def real-analyze (var-get analyze-var))

# 2. A NON-punt compile error must propagate — even though the interpreter could
#    evaluate the form fine, it must NOT be silently used (that hides compiler bugs).
(var-set analyze-var (fn [ctx form] (error "boom: simulated compiler bug")))
(def r (protect (backend/compile-and-eval ctx (parse-string "(+ 1 2)"))))
(assert (not (r 0)) "non-uncompilable compile error must propagate, not silently interpret")
(assert (string/find "boom" (string (r 1))) "the original error is surfaced")

# 3. The punt marker is the one sanctioned fallback channel.
(var-set analyze-var (fn [ctx form] (error "jolt/uncompilable: stubbed")))
(assert (= 3 (backend/compile-and-eval ctx (parse-string "(+ 1 2)")))
        "uncompilable punt falls back to the interpreter")

# Restore the real analyzer and confirm the pipeline still works.
(var-set analyze-var real-analyze)
(assert (= 7 (backend/compile-and-eval ctx (parse-string "(+ 3 4)"))) "restored analyzer compiles")

(print "All compile-fallback tests passed!")
