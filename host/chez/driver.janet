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
(import ../../src/jolt/types_ctx :as tctx)
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
  "A compile-mode jolt ctx (the analyzer pipeline is only built under :compile?).
  Late-bind unresolved symbols: the Chez back end has no interpreter to punt to,
  so a forward reference to a runtime-interned var (defmulti/defmethod's setup
  call) lowers to a var-deref instead of failing to compile (jolt-9ls5)."
  (def ctx (api/init {:compile? true}))
  (put (get ctx :env) :late-bind-unresolved? true)
  ctx)

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

# Drain a pipe to EOF. A single (ev/read pipe N) can return BEFORE the child has
# flushed everything — a program with a stdout side effect (newline/print) flushes
# in two writes, and the first ev/read sometimes catches only the first chunk, so
# the trailing real value is lost (intermittent gate divergence). Loop until EOF.
(defn- drain [pipe]
  (def b @"")
  (var c (ev/read pipe 0x10000))
  (while c (buffer/push b c) (set c (ev/read pipe 0x10000)))
  (string b))

(defn run-on-chez
  "Compile `src` and run it on Chez; returns [exit-code stdout stderr]."
  [ctx src &opt scheme-out]
  (def prog (compile-program ctx src))
  (def path (or scheme-out "/tmp/chez-jolt-prog.ss"))
  (spit path prog)
  (def proc (os/spawn ["chez" "--script" path] :p {:out :pipe :err :pipe}))
  (def out (drain (proc :out)))
  (def err (drain (proc :err)))
  (def code (os/proc-wait proc))
  [code (string/trim out) (string/trim err)])

# --- clojure.core prelude assembly (jolt-9ziu) --------------------------------
# The -e-capable jolt-chez path: emit EVERY non-macro clojure.core form across
# the dependency-ordered tiers as a def-var! in prelude mode, concatenated into
# a Scheme prelude loaded before the user expression. var-deref then resolves any
# core fn at runtime from the prelude's own def-var! cells. Macros are skipped
# (analyze-time only — the Janet analyzer expands them before emit, so they have
# no runtime value). Each form is wrapped in a tolerant load guard so a form that
# fails to LOAD (currently only the Phase-2 multimethod defmulti/defmethod
# print-method forms) doesn't break the rest of the prelude; it logs to stderr
# and becomes a lazy gap rather than a hard prelude failure.

(def core-tier-files
  ["00-syntax" "00-kernel" "10-seq" "20-coll" "25-sorted" "30-macros" "40-lazy" "50-io"])

# stdlib namespaces (beyond clojure.core) emitted into the prelude as their own
# def-var! tier. Each is pure Clojure over clojure.core + host natives, so the
# same analyze->emit pipeline lowers it; an aliased ref resolves via var-deref at
# runtime once the alias is registered (the driver pre-evals requires). jolt-nfca.
(def stdlib-ns-files
  [["clojure.string" "src/jolt/clojure/string.clj"]
   ["clojure.walk" "src/jolt/clojure/walk.clj"]
   # clojure.template requires clojure.walk (apply-template over postwalk-replace)
   # — must follow it so the alias resolves at emit time.
   ["clojure.template" "src/jolt/clojure/template.clj"]
   # clojure.edn requires clojure.string; read-string/__read-tagged are the
   # reader.ss seams. The reader-arity's drain-reader is Janet-coupled (janet/type)
   # so it's a lazy gap on Chez — read-string/edn->value are the live path. jolt-r8ku.
   ["clojure.edn" "src/jolt/clojure/edn.clj"]])

(defn- sym-name [x]
  (when (and (struct? x) (= :symbol (get x :jolt/type))) (get x :name)))

(defn- macro-form? [f]
  (and (indexed? f) (> (length f) 0)
       (let [h (sym-name (in f 0))] (and h (or (= h "defmacro") (= h "definline"))))))

(defn- form-label [f]
  (if (and (indexed? f) (> (length f) 1))
    (let [h (or (sym-name (in f 0)) "?") n (sym-name (in f 1))] (if n (string h " " n) h))
    "?"))

(defn- require-head? [f]
  (and (indexed? f) (> (length f) 0)
       (let [h (sym-name (in f 0))] (and h (or (= h "require") (= h "use"))))))

(defn- scan-eval-requires! [ctx form]
  "Recursively eval any (require ...)/(use ...) sub-form against the ctx so the
  alias registers + the aliased ns loads BEFORE the AOT analyzer resolves its
  qualified refs — the whole user form is analyzed up front, before any require
  would run at eval time (jolt-nfca). Failures are swallowed (the ref then stays
  an emit-err, the prior behavior)."
  (when (indexed? form)
    (if (require-head? form)
      (protect (api/eval-one ctx form))
      (each sub form (scan-eval-requires! ctx sub)))))

(defn emit-core-prelude
  "Assemble the clojure.core prelude as a Scheme string. `ctx` must be a
  compile-mode ctx; its current ns is set to clojure.core for the duration.
  Returns [scheme emitted total skipped-load-guards-unknown]; `scheme` is the
  joined, guard-wrapped def-var! forms (no rt.ss load — add that at program
  assembly via emit/program or program-with-prelude)."
  [ctx &opt core-dir]
  (default core-dir "jolt-core/clojure/core/")
  (emit/set-prelude-mode! true)
  (def prev-ns (tctx/ctx-current-ns ctx))
  (tctx/ctx-set-current-ns ctx "clojure.core")
  (def out @[])
  (var total 0) (var emitted 0)
  (defn- emit-ns-forms [ns-name src]
    (tctx/create-ns ctx ns-name)
    (tctx/ctx-set-current-ns ctx ns-name)
    (each f (parse-all src)
      # Register any aliases this ns depends on before analyzing its forms, so an
      # aliased ref (e.g. clojure.template's walk/postwalk-replace) resolves at emit
      # time instead of lowering to an "Unknown class walk" host-static. The ns
      # form's :require is a keyword-headed clause that scan-eval-requires! (matching
      # the `require`/`use` symbol heads) doesn't catch, so eval the ns form whole.
      (def ns-form? (and (indexed? f) (> (length f) 0) (= "ns" (sym-name (in f 0)))))
      (if ns-form? (protect (api/eval-one ctx f)) (scan-eval-requires! ctx f))
      # Skip emitting ns forms: their only role here is alias registration, and a
      # runtime ns-switch would leak into the prelude's trailing *ns* (the def-var!s
      # already carry explicit ns names). Macros have no runtime value either.
      (unless (or ns-form? (macro-form? f))
        (++ total)
        (def res (protect (emit/emit (backend/analyze-form ctx f))))
        (when (res 0)
          (++ emitted)
          # Tolerant load guard: a form that fails to LOAD (currently only the 8
          # Phase-2 multimethod print-method forms in 50-io) is swallowed so it
          # doesn't break the rest of the prelude — it becomes a lazy gap (the var
          # cell stays nil; calling it surfaces in the parity gate's crash bucket).
          # Silent to keep a real -e's stderr clean; the known set is documented.
          (array/push out
            (string "(guard (e (#t #f))\n  " (res 1) ")"))))))
  (each tf core-tier-files
    (emit-ns-forms "clojure.core" (slurp (string core-dir tf ".clj"))))
  # stdlib namespaces beyond clojure.core that are pure Clojure over core/host
  # natives — emitted as their own def-var! tier so an aliased ref (e.g. s/split
  # after (require '[clojure.string :as s])) resolves at runtime (jolt-nfca).
  (each [ns-name path] stdlib-ns-files
    (emit-ns-forms ns-name (slurp path)))
  (tctx/ctx-set-current-ns ctx prev-ns)
  (emit/set-prelude-mode! false)
  [(string/join out "\n") emitted total])

(defn program-with-prelude
  "Assemble a runnable Chez program that loads rt.ss, loads the assembled core
  prelude from `prelude-path` (a file written once), then prints `final-scm`."
  [prelude-path final-scm]
  (string
    "(import (chezscheme))\n"
    "(load \"host/chez/rt.ss\")\n"
    # the prelude's defmultis (print-method/print-dup) must land in clojure.core,
    # not the default user ns (jolt-9ls5); set the multimethod current-ns around
    # the prelude load, then restore it to user for the program form.
    "(set-chez-ns! \"clojure.core\")\n"
    "(load " (string/format "%j" prelude-path) ")\n"
    # native-wins overrides for overlay predicates that read :jolt/type (char?,
    # atom?) — must load AFTER the prelude's own def-var! to take effect.
    "(load \"host/chez/post-prelude.ss\")\n"
    "(set-chez-ns! \"user\")\n"
    "(printf \"~a\\n\" (jolt-final-str " final-scm "))\n"))

(defn eval-e-with-prelude
  "Run a single user expression `src` on Chez with the full clojure.core prelude
  (loaded from `prelude-path`). Emits `src` in prelude mode so any core ref
  resolves via var-deref. Returns [code stdout stderr], or [:emit-err msg \"\"]
  if the user form itself can't be emitted."
  [ctx src prelude-path &opt scheme-out]
  (emit/set-prelude-mode! true)
  (def form (in (r/parse-next src) 0))
  (scan-eval-requires! ctx form)
  (def res (protect (emit/emit (backend/analyze-form ctx form))))
  (emit/set-prelude-mode! false)
  (if (not (res 0))
    [:emit-err (string (res 1)) ""]
    (let [prog (program-with-prelude prelude-path (res 1))
          # PID-unique default so concurrent processes (a foreground -e while the
          # parity gate runs) never read each other's half-written program file.
          path (or scheme-out (string "/tmp/jolt-chez-e-" (os/getpid) ".ss"))]
      (spit path prog)
      (def proc (os/spawn ["chez" "--script" path] :p {:out :pipe :err :pipe}))
      (def out (drain (proc :out)))
      (def err (drain (proc :err)))
      (def code (os/proc-wait proc))
      [code (string/trim out) (string/trim err)])))
