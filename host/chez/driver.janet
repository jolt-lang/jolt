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
(import ../../src/jolt/types_ns :as tns)
(import ../../src/jolt/types_var :as tvar)
(import ./emit :as emit)

# Chez Phase 3 (jolt-duot): the IR->Scheme emitter is now the PORTABLE Clojure
# jolt.backend-scheme (jolt-core), not emit.janet. It's loaded into the ctx and
# called from here the same way the analyzer is. emit.janet stays only as the
# program-string wrapper (emit/program) until program assembly ports to Clojure
# with compile-from-source. This is the step that takes the emitter off Janet.
(defn- ensure-clj-emitter [ctx]
  (def env (ctx :env))
  (unless (get env :clj-emit-fn)
    (def src (get (get env :embedded-sources @{}) "jolt.backend-scheme"))
    (assert src "jolt.backend-scheme not embedded (check stdlib_embed)")
    (backend/bootstrap-load-source ctx "jolt.backend-scheme" src)
    (def ns (tctx/ctx-find-ns ctx "jolt.backend-scheme"))
    (put env :clj-emit-fn (tvar/var-get (tns/ns-find ns "emit")))
    (put env :clj-set-prelude-fn (tvar/var-get (tns/ns-find ns "set-prelude-mode!"))))
  ctx)

# Emit IR -> Scheme via the Clojure emitter (returns a Janet string).
(defn- cemit [ctx ir] (string ((get (ctx :env) :clj-emit-fn) ir)))
(defn- cset-prelude! [ctx on] ((get (ctx :env) :clj-set-prelude-fn) on))

# Public: emit IR -> Scheme via the portable Clojure emitter (jolt.backend-scheme).
# The single seam tests use so emit.janet's emit fn is no longer exercised.
(defn scheme-emit [ctx ir] (ensure-clj-emitter ctx) (cemit ctx ir))

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
  (ensure-clj-emitter ctx)
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
  (ensure-clj-emitter ctx)
  (def forms (parse-all src))
  (assert (> (length forms) 0) "compile-program: empty program")
  (def n (length forms))
  (def def-scm @[])
  (for i 0 (- n 1)
    (def f (in forms i))
    # emit the def, then intern it (interpreted) so a later form's reference to
    # this var resolves to a :var node rather than an unresolved symbol.
    (array/push def-scm (cemit ctx (backend/analyze-form ctx f)))
    (evlr/eval-form ctx @{} f))
  (def final-scm (cemit ctx (backend/analyze-form ctx (in forms (- n 1)))))
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
   ["clojure.edn" "src/jolt/clojure/edn.clj"]
   # clojure.set / clojure.pprint: pure Clojure over core. set = relational ops
   # (union/intersection/difference/join/index/...); pprint = the minimal jolt
   # shim (pprint -> prn + recognized dispatch vars, with-pprint-dispatch macro).
   # jolt-j5vg, clojure.pprint Phase-2 parity.
   ["clojure.set" "src/jolt/clojure/set.clj"]
   ["clojure.pprint" "src/jolt/clojure/pprint.clj"]])

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
  (ensure-clj-emitter ctx)
  (cset-prelude! ctx true)
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
        (def res (protect (cemit ctx (backend/analyze-form ctx f))))
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
  (cset-prelude! ctx false)
  [(string/join out "\n") emitted total])

# --- analyzer/emitter cross-compile (jolt-hs9n, the zero-Janet spine) ---------
# Phase 3 inc6: cross-compile the PORTABLE compiler (jolt.ir + jolt.analyzer +
# jolt.backend-scheme) to Scheme def-var! forms so analyze->IR->emit runs ON CHEZ.
# Same emit pipeline as the core prelude, but for jolt-core/jolt/* namespaces
# rather than clojure.core: jolt.* refs lower to var-deref (the prelude-mode gate
# only rejects clojure.* refs), clojure.core refs resolve from the loaded prelude,
# and the jolt.host form-*/resolve-global/... refs resolve from host-contract.ss.

(defn- emit-ns-forms-list
  "Cross-compile one namespace's source to a list of guard-wrapped def-var! Scheme
  strings (prelude mode must already be ON). Registers the ns' requires/aliases in
  ctx first so cross-ns refs resolve at emit time; skips ns + macro forms (macros
  are analyze-time only, already expanded at their use sites)."
  [ctx ns-name src]
  (tctx/create-ns ctx ns-name)
  (tctx/ctx-set-current-ns ctx ns-name)
  (def out @[])
  (each f (parse-all src)
    (def ns-form? (and (indexed? f) (> (length f) 0) (= "ns" (sym-name (in f 0)))))
    (if ns-form? (protect (api/eval-one ctx f)) (scan-eval-requires! ctx f))
    (unless (or ns-form? (macro-form? f))
      (def res (protect (cemit ctx (backend/analyze-form ctx f))))
      (when (res 0)
        (array/push out (string "(guard (e (#t #f))\n  " (res 1) ")")))))
  out)

(def compiler-ns-files
  [["jolt.ir" "jolt-core/jolt/ir.clj"]
   ["jolt.analyzer" "jolt-core/jolt/analyzer.clj"]
   ["jolt.backend-scheme" "jolt-core/jolt/backend_scheme.clj"]])

(defn emit-compiler-image
  "Cross-compile the analyzer pipeline (jolt.ir + jolt.analyzer +
  jolt.backend-scheme) to a Scheme string of prelude-mode def-var! forms — the
  analyze->IR->emit spine running ON CHEZ (jolt-hs9n). Load AFTER rt.ss +
  host-contract.ss + the core prelude. Returns [scheme total]."
  [ctx]
  (ensure-clj-emitter ctx)
  # ensure-analyzer is lazy; a trivial analyze builds jolt.ir/jolt.analyzer/
  # jolt.passes in the Janet ctx so their vars resolve while we emit their source.
  (protect (backend/analyze-form ctx (in (r/parse-next "nil") 0)))
  (cset-prelude! ctx true)
  (def prev-ns (tctx/ctx-current-ns ctx))
  (def out @[])
  (each [ns-name path] compiler-ns-files
    (array/concat out (emit-ns-forms-list ctx ns-name (slurp path))))
  (tctx/ctx-set-current-ns ctx prev-ns)
  (cset-prelude! ctx false)
  [(string/join out "\n") (length out)])

(defn ensure-compiler-image
  "Build (once) and return the path to the cross-compiled compiler image — the
  jolt.ir/jolt.analyzer/jolt.backend-scheme def-var! forms (jolt-hs9n). Cached on
  disk keyed by the same fingerprint scheme as the prelude; pass an explicit path
  to control caching from the test harness."
  [ctx path]
  (unless (os/stat path)
    (def [img _] (emit-compiler-image ctx))
    (spit path img))
  path)

(defn program-zero-janet
  "Assemble a fully self-hosted Chez program: rt.ss + the core prelude +
  host-contract.ss + the cross-compiled compiler image + compile-eval.ss, then
  compile AND eval `src` ON CHEZ (read->analyze->emit->eval, no Janet). The
  zero-Janet spine (jolt-hs9n)."
  [prelude-path image-path src ns]
  (string
    "(import (chezscheme))\n"
    "(load \"host/chez/rt.ss\")\n"
    "(set-chez-ns! \"clojure.core\")\n"
    "(load " (string/format "%j" prelude-path) ")\n"
    "(load \"host/chez/post-prelude.ss\")\n"
    "(set-chez-ns! \"user\")\n"
    "(load \"host/chez/host-contract.ss\")\n"
    "(load " (string/format "%j" image-path) ")\n"
    "(load \"host/chez/compile-eval.ss\")\n"
    "(printf \"~a\\n\" (jolt-final-str (jolt-compile-eval "
    (string/format "%j" src) " " (string/format "%j" ns) ")))\n"))

(defn eval-zero-janet
  "Compile+run `src` through the ON-CHEZ analyzer/emitter (zero Janet). Needs a
  prebuilt core prelude (`prelude-path`) and compiler image (`image-path`).
  Returns [code stdout stderr]."
  [prelude-path image-path src &opt ns scheme-out]
  (default ns "user")
  (def prog (program-zero-janet prelude-path image-path src ns))
  (def path (or scheme-out (string "/tmp/jolt-zero-janet-" (os/getpid) ".ss")))
  (spit path prog)
  (def proc (os/spawn ["chez" "--script" path] :p {:out :pipe :err :pipe}))
  (def out (drain (proc :out)))
  (def err (drain (proc :err)))
  (def code (os/proc-wait proc))
  [code (string/trim out) (string/trim err)])

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
  (ensure-clj-emitter ctx)
  (cset-prelude! ctx true)
  (def form (in (r/parse-next src) 0))
  (scan-eval-requires! ctx form)
  (def res (protect (cemit ctx (backend/analyze-form ctx form))))
  (cset-prelude! ctx false)
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
