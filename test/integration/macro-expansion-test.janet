# Compiled macro expansion in EVERY mode (jolt-tzo: the fast macro-expansion
# path that unblocks moving hot fns the 00-syntax expanders depend on).
#
# Macros are ordinary compiled fns in Clojure's model. Compile mode has had
# this since the staged bootstrap (ensure-macros-compiled! recompiles the
# early interpreted expanders once the analyzer is alive); interpret mode —
# the conformance battery's default — used to skip it, so every distinct
# (and ...) / (cond ...) / nested expansion ran an interpreted closure.
# Now interpret-mode init also builds the analyzer once at the end of the
# overlay load and compiles every stashed expander; JOLT_INTERPRET_MACROS=1
# opts back into the pure interpreted oracle.

(use ../../src/jolt/api)
(use ../../src/jolt/types)

(print "compiled macro expansion...")
(os/setenv "JOLT_INTERPRET_MACROS" nil)

(defn- macro-var [ctx nm] (ns-find (ctx-find-ns ctx "clojure.core") nm))
(defn- user-var [ctx nm] (ns-find (ctx-find-ns ctx "user") nm))

(def probes
  ["(= 3 (and 1 2 3))"
   "(= 1 (or nil false 1))"
   "(= :b (cond false :a :else :b))"
   "(= 4 (when-not nil 4))"
   "(= 6 (-> 1 inc (* 3)))"
   "(= 15 (->> [1 2] (map inc) (reduce +) (* 3)))"
   "(= 2 (if-let [x nil] 1 2))"
   "(= [0 1] (vec (for [i (range 2)] i)))"
   "(= 5 (case 2 1 :one 2 5 :dflt))"
   "(= 10 (loop [i 0 a 0] (if (< i 5) (recur (inc i) (+ a 2)) a)))"])

(defn- run-probes [ctx label]
  (each prog probes
    (def got (protect (eval-string ctx prog)))
    (assert (and (got 0) (= (got 1) true))
            (string label " probe failed: " prog " => "
                    (if (got 0) (string/format "%q" (got 1)) (string (got 1)))))))

# 1. Interpret mode: expanders are COMPILED after init (the new path).
(def ictx (init {}))
(run-probes ictx "interpret")
(each m ["when" "when-not" "and" "or" "cond" "->" "->>" "if-let" "case" "doseq"]
  (def v (macro-var ictx m))
  (assert v (string m " var exists"))
  (assert (get v :macro-compiled)
          (string m " expander is compiled in interpret mode")))

# 2. A USER defmacro in an interpret ctx gets a compiled expander too.
(eval-string ictx "(defmacro my-twice [x] `(* 2 ~x))")
(assert (= 10 (eval-string ictx "(my-twice 5)")) "user macro works")
(assert (get (user-var ictx "my-twice") :macro-compiled)
        "user macro expander compiled in interpret mode")

# 3. An expander whose body the analyzer can't compile (here: the `eval`
#    special form) falls back to the interpreted closure and still works.
(eval-string ictx "(defmacro evalish [x] (eval `(+ ~x 1)))")
(assert (= 3 (eval-string ictx "(evalish 2)")) "uncompilable expander works")
(assert (not (get (user-var ictx "evalish") :macro-compiled))
        "uncompilable expander stays interpreted")

# 4. Compile mode unchanged: expanders compiled (pre-existing behavior).
(def cctx (init {:compile? true}))
(run-probes cctx "compile")
(assert (get (macro-var cctx "cond") :macro-compiled) "compile-mode expanders compiled")

# 5. JOLT_INTERPRET_MACROS=1: the pure interpreted oracle — same semantics,
#    expanders NOT compiled.
(os/setenv "JOLT_INTERPRET_MACROS" "1")
(def octx (init {}))
(run-probes octx "oracle")
(assert (not (get (macro-var octx "cond") :macro-compiled))
        "oracle mode keeps interpreted expanders")
(os/setenv "JOLT_INTERPRET_MACROS" nil)

(print "compiled macro expansion passed!")
