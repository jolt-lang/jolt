# Phase 1 (jolt-cf1q.2) — live-analyzer -> Chez driver.
#
# Boots a real jolt ctx, runs the EXISTING Janet-hosted analyzer on actual
# Clojure source to produce host-neutral IR, feeds that IR to the Scheme emitter
# (emit.janet), and assembles a runnable Chez program. This is the Option-2
# backend swap end to end: same front end, Scheme back end, run on Chez.
#
# Analysis still happens on Janet here (the analyzer is portable Clojure but not
# yet bootstrapped onto Chez — that's Phase 2); EXECUTION happens on Chez. The
# point of this increment is to validate that the real IR the analyzer emits
# compiles to correct, fast Scheme.

(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/reader :as r)
(import ../../src/jolt/evaluator :as evlr)
(import ./emit :as emit)

(defn chez-available?
  "True when a `chez` binary is on PATH — lets the chez tests skip cleanly on
  hosts without it (CI without Chez), like the clojure-test-suite skips when its
  corpus dir is absent."
  []
  (def r (protect (let [p (os/spawn ["chez" "--version"] :p {:out :pipe :err :pipe})]
                    (ev/read (p :out) 1024)
                    (ev/read (p :err) 1024)
                    (os/proc-wait p))))
  (and (r 0) (zero? (r 1))))

(defn make-ctx []
  "A compile-mode jolt ctx (the analyzer pipeline is only built under :compile?)."
  (api/init {:compile? true}))

(defn- parse-all [src]
  (def out @[])
  (var s src)
  (while (> (length (string/trim s)) 0)
    (def parsed (r/parse-next s))
    (set s (in parsed 1))
    (def f (in parsed 0))
    (unless (nil? f) (array/push out f)))
  out)

(defn compile-program
  "Compile a Clojure program string to a runnable Chez program. Every top-level
  form is analyzed to real IR and emitted to Scheme; all but the LAST form are
  treated as defs (also interned in the ctx so later forms resolve their vars),
  and the last form is the expression whose value the program prints."
  [ctx src]
  (def forms (parse-all src))
  (assert (> (length forms) 0) "compile-program: empty program")
  (def n (length forms))
  (def def-scm @[])
  (for i 0 (- n 1)
    (def f (in forms i))
    # emit the def, then intern it (interpreted) so a later form's reference to
    # this var resolves to a :var node rather than an unresolved symbol.
    (array/push def-scm (emit/emit (backend/analyze-form ctx f)))
    (evlr/eval-form ctx @{} f))
  (def final-scm (emit/emit (backend/analyze-form ctx (in forms (- n 1)))))
  (emit/program def-scm final-scm))

(defn run-on-chez
  "Compile `src` and run it on Chez; returns [exit-code stdout stderr]."
  [ctx src &opt scheme-out]
  (def prog (compile-program ctx src))
  (def path (or scheme-out "/tmp/chez-jolt-prog.ss"))
  (spit path prog)
  (def proc (os/spawn ["chez" "--script" path] :p {:out :pipe :err :pipe}))
  (def out (ev/read (proc :out) 0x100000))
  (def err (ev/read (proc :err) 0x100000))
  (def code (os/proc-wait proc))
  [code (string/trim (if out (string out) "")) (string/trim (if err (string err) ""))])
