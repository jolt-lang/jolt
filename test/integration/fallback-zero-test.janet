# Fallback-zero verification (Stage 1 Task 3).
#
# self-host-test.janet checks observable RESULTS but not WHICH path ran — a form
# that silently fell back to the interpreter still "passes" there. This harness
# checks the path: it runs the portable analyzer (jolt.analyzer/analyze, via
# backend/analyze-form) on a corpus of NON-STATEFUL forms and asserts NONE raise
# :jolt/uncompilable — i.e. the self-hosted analyzer actually COMPILED them.
#
# As analyzer↔compiler.janet parity grows (Stage 1), move forms from the
# "intentional fallback" sanity list into the must-compile corpus. The day the
# fallback set equals the frozen intentional stateful set, the Janet bootstrap
# compiler is retireable.
#
# Mechanism: backend/analyze-form throws (a "jolt/uncompilable: …" string) for a
# punted form; (protect …) turns that into [false msg]. [true ir] == compiled.

(import ../../src/jolt/backend :as backend)
(use ../../src/jolt/api)
(use ../../src/jolt/reader)

(def ctx (init))

(defn- analyzes? [s]
  # true if the analyzer produced IR (compiled), false if it punted/uncompilable.
  (def r (protect (backend/analyze-form ctx (parse-string s))))
  (and (r 0) true))

# --- Must compile: pure, non-stateful value production. NONE may punt. ---
(def must-compile
  [# set literals (Task 1)
   "#{1 2 3}" "#{}" "#{:a :b :c}" "#{(inc 0) 2}" "(conj #{1 2} 3)"
   "[#{1 2} {:s #{3}}]" "(let [x 5] #{x (inc x)})"
   # other literals
   "[1 2 3]" "{:a 1 :b 2}" "{:k (inc 0)}" "[[1] [2 3]]" "42" ":kw" "\"str\""
   # control flow + binding
   "(+ 1 2)" "(if true 1 2)" "(do 1 2 3)" "(let [a 1 b 2] (+ a b))"
   "(fn [x] (* x x))" "(fn ([a] a) ([a b] (+ a b)))"
   "(loop [i 0] (if (< i 3) (recur (inc i)) i))"
   "(quote (a b c))" "(throw (ex-info \"x\" {}))"
   "(try (inc 1) (catch :default e e))"
   # def + calls into core
   "(def answer 42)" "(map inc [1 2 3])" "(reduce + 0 [1 2 3])"
   "(get {:a 1} :a)" "(vec (range 5))"
   # set?/disj are plain fns now, not special forms (jolt-g3h)
   "(set? #{1 2})" "(disj #{1 2 3} 2)"
   # Stage 2 (jolt-eaa): stateful forms moved onto the compile path. (binding only
   # compiles over an INTERNED var; the built-in dynamic vars aren't interned yet,
   # so it's exercised end-to-end in the state spec instead.)
   "(require (quote [clojure.string :as s]))" "(in-ns (quote foo.bar))"
   "(ns foo.bar (:require [clojure.string :as s]))"
   "(defprotocol P (m [x]))" "(extend-type Long P (m [x] x))"
   "(reify P (m [this] 1))" "(var map)"
   # Stage 2 tier 5: type/dispatch definitional forms compile too
   "(deftype Pt [x y])" "(deftype Sq [s] P (m [this] s))"
   "(defrecord Rec [a b])" "(defmulti mf :k)" "(defmethod mf :a [x] x)"])

# --- Intentional fallback (sanity sample): these SHOULD punt to the interpreter.
# The remaining frozen/uncompiled set keeps the harness honest in the punt
# direction: defmacro + set! (frozen host-coupled), and letfn (needs letrec IR).
(def must-punt
  ["(defmacro m [x] x)"
   "(set! *warn-on-reflection* true)" "(letfn [(f [n] (g n)) (g [n] (f n))] (f 1))"])

(var fails @[])
(each s must-compile
  (unless (analyzes? s) (array/push fails (string "FALLBACK (should compile): " s))))
(each s must-punt
  (when (analyzes? s) (array/push fails (string "COMPILED (should punt): " s))))

(printf "fallback-zero: %d must-compile + %d must-punt — %d failures"
        (length must-compile) (length must-punt) (length fails))
(when (> (length fails) 0)
  (print "\nFailures:")
  (each f fails (printf "  %s" f))
  (os/exit 1))
(print "fallback-zero: OK (analyzer compiled the full non-stateful corpus)")
