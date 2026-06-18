# Phase 2 (jolt-cf1q.3) — which clojure.core names resolve to jolt-nil on Chez?
#
# The parity gate buckets a missing native generically as "apply non-procedure
# jolt-nil" — it doesn't NAME the fn. This probe does: it assembles the prelude,
# enumerates every clojure.core var name, then runs ONE Chez program that
# var-derefs each name (after loading prelude + post-prelude) and prints the ones
# that are still nil. That list is the shim punch-list for the next increment.
#   JOLT_CHEZ_NIL_PROBE=1 janet test/chez/nil-names-probe.janet
(import ../../host/chez/driver :as d)
(import ../../src/jolt/types_ctx :as tctx)

(unless (os/getenv "JOLT_CHEZ_NIL_PROBE")
  (print "skip: set JOLT_CHEZ_NIL_PROBE=1 to run the nil-names probe")
  (os/exit 0))
(unless (d/chez-available?)
  (print "skip: chez not on PATH")
  (os/exit 0))

(def ctx (d/make-ctx))

# Collect every clojure.core var name (mapping keys are symbol structs).
(def names @{})
(each ns (tctx/all-ns ctx)
  (when (= (get ns :name) "clojure.core")
    (eachk sym (get ns :mappings)
      (def n (cond (struct? sym) (get sym :name)
                   (string? sym) sym
                   (symbol? sym) (string sym)
                   nil))
      (when n (put names n true)))))
(def name-list (sort (keys names)))
(printf "clojure.core has %d interned names" (length name-list))

# Assemble the prelude once.
(def [prelude-scm emitted total] (d/emit-core-prelude ctx))
(def prelude-path (string "/tmp/jolt-nil-probe-prelude-" (os/getpid) ".ss"))
(spit prelude-path prelude-scm)
(printf "prelude: %d/%d non-macro core forms emitted" emitted total)

# Build a Chez program that derefs each name and prints the nil ones.
(def buf @"")
(buffer/push buf "(import (chezscheme))\n(load \"host/chez/rt.ss\")\n")
(buffer/push buf "(set-chez-ns! \"clojure.core\")\n")
(buffer/push buf (string "(load " (string/format "%j" prelude-path) ")\n"))
(buffer/push buf "(load \"host/chez/post-prelude.ss\")\n")
(buffer/push buf "(set-chez-ns! \"user\")\n")
(each n name-list
  (buffer/push buf
    (string "(when (jolt-nil? (var-deref \"clojure.core\" " (string/format "%j" n)
            ")) (display " (string/format "%j" n) ") (newline))\n")))
(def prog-path (string "/tmp/jolt-nil-probe-" (os/getpid) ".ss"))
(spit prog-path buf)

(def proc (os/spawn ["chez" "--script" prog-path] :p {:out :pipe :err :pipe}))
(def out (ev/read (proc :out) 0x100000))
(def err (ev/read (proc :err) 0x100000))
(def code (os/proc-wait proc))
(def nils (filter (fn [s] (> (length s) 0)) (string/split "\n" (string/trim (if out (string out) "")))))
(printf "\n%d clojure.core names resolve to jolt-nil on Chez:" (length nils))
(each n (sort nils) (print "  " n))
(when (and err (> (length (string/trim (string err))) 0))
  (printf "\nstderr:\n%s" (string err)))
(printf "\n(probe exit %d)" code)
