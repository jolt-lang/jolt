# Janet back end: host-neutral IR (from jolt.analyzer) -> Janet form -> bytecode.
#
# Host-specific by definition (it targets Janet). It resolves name-based :var
# nodes to Janet var cells and reuses runtime helpers (jolt-call, make-vec,
# build-map-literal). The portable front end (jolt.analyzer) never sees any of
# this; a different runtime provides its own back end against the same IR.
#
# In src/jolt/ (not host/janet/) for the same module-resolution reason as
# host_iface — see that file's header.

(use ./types)
(use ./core)
(use ./evaluator)
(import ./reader :as r)
(import ./phm :as phm)

# The IR is portable data; reading its representation is a host-layer concern.
# Most nodes are Janet structs (raw-readable), but a node carrying a nil-valued
# field — an anonymous fn's :name, a nil const's :val, a def with no :meta, an
# arity with no :rest — is a phm, whose fields live under :buckets, not as direct
# keys. Densify such a node to a struct: phm-to-struct drops exactly those
# nil-valued fields, which is what the back end wants (it already treats an absent
# field as nil). Structs (the common case) pass through untouched. Applied at the
# few points where a node first reaches the emitter, so the rest of the back end
# keeps using plain (node :key) access and the portable front end never sees this.
# --- Runtime kernel (absorbed from the retired bootstrap compiler) ----------

# The Janet env compiled code evaluates in. Captured at module load: backend's
# env chains types/core/evaluator/reader/phm, so emitted symbols (let/fn/in/
# var-get/tuple-slice/...) and jolt runtime helpers resolve by name.
(def jolt-runtime-env (curenv))

(defn ctx-janet-env
  "Lazily create/cache a per-context Janet environment for compiled code: a child
  of the runtime env (so core fns resolve) that holds this context's user defs.
  For a nil context (one-off compile/eval) returns a fresh child env."
  [ctx]
  (if (and ctx (table? (get ctx :env)))
    (or (get (ctx :env) :janet-rt)
        (let [e (make-env jolt-runtime-env)]
          (put (ctx :env) :janet-rt e)
          e))
    (make-env jolt-runtime-env)))

(defn build-map-literal
  "Build a map value from evaluated k v k v ... args. A phm (not a Janet struct)
  when a key is a collection (value hashing) or a key/value is nil (structs drop
  nil; phm preserves it, matching Clojure)."
  [& kvs]
  (var need-phm false)
  (var ki 0)
  (while (< ki (length kvs))
    (let [kk (in kvs ki) vv (in kvs (+ ki 1))]
      (when (or (table? kk) (array? kk) (nil? kk) (nil? vv)) (set need-phm true)))
    (+= ki 2))
  (if need-phm
    (do (var m (phm/make-phm)) (var j 0)
        (while (< j (length kvs)) (set m (phm/phm-assoc m (in kvs j) (in kvs (+ j 1)))) (+= j 2))
        m)
    (struct ;kvs)))

(defn- norm-node [n]
  (if (phm/phm? n) (phm/phm-to-struct n) n))

# Var late-binding: reads go through `(var-get cell)` with the cell embedded as a
# constant, so compiled code sees redefinition (Janet early-binds plain symbols)
# — var-get reads the cell's root live. Writes go through a memoized setter.
(defn- var-setter [cell]
  (or (get cell :jolt/setter)
      (let [s (fn [v] (bind-root cell v) cell)] (put cell :jolt/setter s) s)))

# Setter that also applies def metadata to the var (so ^:dynamic / ^:redef /
# ^:private survive compilation, matching the interpreter's def). Not memoized:
# the meta is specific to this def site.
(defn- var-setter-meta [cell meta]
  (fn [v]
    (bind-root cell v)
    (put cell :meta (merge (or (cell :meta) {}) meta))
    (when (get meta :dynamic) (put cell :dynamic true))
    cell))

(defn- cell-for [ctx ns-name nm]
  (ns-intern (ctx-find-ns ctx ns-name) nm))

# Direct-linking decision (call-site/unit property, Clojure-style). A var
# reference compiles to its embedded value (direct) iff:
#   - the compiling unit has direct-linking on (env :direct-linking?),
#   - the target opts in (NOT ^:redef / ^:dynamic — those force indirect),
#   - the target is already defined AND its root is a Janet function.
# The function? guard is essential: embedding a non-function value (a jolt
# collection/symbol) into the emitted form would make Janet evaluate it AS code.
# So we direct-link exactly the call-optimization case; everything else stays
# indirect (live var deref → redefinable). Default user/REPL units: flag off,
# so all user calls are indirect and redefinable with no annotation.
(defn- direct-var? [ctx cell]
  (and (get (ctx :env) :direct-linking?)
       (not (cell :dynamic))
       (not (let [m (cell :meta)] (and m (get m :redef))))
       (function? (cell :root))))

# Fresh Janet symbol for back-end-introduced bindings (arity dispatch). NOT
# Janet's `gensym` — `(use ./core)` shadows it with Jolt's, which returns a jolt
# symbol struct (invalid in a Janet param position).
(var- gsym-counter 0)
(defn- gsym [] (def s (symbol "_be$" gsym-counter)) (++ gsym-counter) s)

(var emit nil)

(defn- emit-seq [ctx node]
  (def out @['do])
  (each s (vview (node :statements)) (array/push out (emit ctx s)))
  (array/push out (emit ctx (node :ret)))
  (tuple/slice out))

(defn- emit-let [ctx node]
  (def binds @[])
  (each pair (vview (node :bindings))
    (def p (vview pair))
    (array/push binds (symbol (in p 0)))
    (array/push binds (emit ctx (in p 1))))
  ['let (tuple/slice binds) (emit ctx (node :body))])

# An arity compiles to a named Janet fn whose name is its recur target, so recur
# is a self-call (Janet tail-calls it). The rest param is an ORDINARY positional
# param holding a seq (not Janet `&`), so `(recur fixed... rest-seq)` re-enters
# the way Clojure recur into a variadic arity does (rebinds the rest seq directly,
# no re-collection). The dispatch wrapper (emit-fn-body) collects the call's args.
(defn- emit-arity-fn [ctx ar]
  (def ps @[])
  (each pn (vview (ar :params)) (array/push ps (symbol pn)))
  (when (ar :rest) (array/push ps (symbol (ar :rest))))
  ['fn (symbol (ar :recur-name)) (tuple/slice ps) (emit ctx (ar :body))])

# Invoke an arity's fn with args pulled from the dispatch tuple: fixed params by
# index, rest as a slice from n-fixed on.
(defn- emit-arity-invoke [ctx ar jargs]
  (def nfixed (length (vview (ar :params))))
  (def call @[(emit-arity-fn ctx ar)])
  (for i 0 nfixed (array/push call ['in jargs i]))
  (when (ar :rest) (array/push call ['tuple/slice jargs nfixed]))
  (tuple/slice call))

(defn- emit-loop [ctx node]
  (def L (symbol (node :recur-name)))
  (def params @[])
  # Initial inits bind SEQUENTIALLY (a later init can reference an earlier binding,
  # like let / Clojure's loop) — emit them in a Janet `let`, then enter the recur
  # target L with those values, rather than computing all inits in the outer scope.
  (def let-binds @[])
  (each pair (vview (node :bindings))
    (def p (vview pair))
    (def sym (symbol (in p 0)))
    (array/push params sym)
    (array/push let-binds sym)
    (array/push let-binds (emit ctx (in p 1))))
  ['do
   ['var L nil]
   ['set L ['fn (tuple/slice params) (emit ctx (node :body))]]
   ['let (tuple/slice let-binds) (tuple/slice (array/concat @[L] params))]])

(defn- emit-recur [ctx node]
  (tuple/slice (array/concat @[(symbol (node :recur-name))]
                            (map |(emit ctx $) (vview (node :args))))))

(defn- emit-try [ctx node]
  (def core
    (if (node :catch-sym)
      ['try (emit ctx (node :body))
       [[(symbol (node :catch-sym))] (emit ctx (node :catch-body))]]
      (emit ctx (node :body))))
  (if (node :finally)
    ['defer (emit ctx (node :finally)) core]
    core))

(defn- emit-fn-body [ctx node]
  (def arities (map norm-node (vview (node :arities))))
  (def multi (> (length arities) 1))
  (cond
    # Single fixed arity (the hot case): emit the arity fn directly — its name is
    # the recur target, no dispatch overhead.
    (and (not multi) (not ((first arities) :rest)))
    (emit-arity-fn ctx (first arities))
    # Single variadic arity: a thin wrapper collects the call's args so the rest
    # seq can be built, then hands off to the arity fn.
    (not multi)
    (let [jargs (gsym)]
      ['fn ['& jargs] (emit-arity-invoke ctx (first arities) jargs)])
    # Multi-arity: dispatch on arg count. Fixed arities match exactly; the (one)
    # variadic arity matches >= its fixed count.
    (let [jargs (gsym)
          nsym (gsym)
          cf @['cond]]
      (each ar arities
        (def nfixed (length (vview (ar :params))))
        (array/push cf (if (ar :rest) [>= nsym nfixed] [= nsym nfixed]))
        (array/push cf (emit-arity-invoke ctx ar jargs)))
      (array/push cf ['error "wrong number of args passed to fn"])
      ['fn ['& jargs]
       ['do ['def nsym ['length jargs]] (tuple/slice cf)]])))

# A named fn (fn self [..] .. (self ..)) references itself by name. The analyzer
# binds that name as a local; bind it here to the fn value via a var (set before
# any call, so the captured closure sees it — same scheme as emit-loop). recur
# stays a separate self-call to the arity fn; this only covers by-name self-refs.
(defn- emit-fn [ctx node]
  (def body (emit-fn-body ctx node))
  (if (node :name)
    (let [s (symbol (node :name))]
      ['do ['var s nil] ['set s body] s])
    body))

# A direct Janet call (f args) is only correct when the callee is definitely a
# function: Janet calling a pvec/keyword/etc. does get (or the wrong thing), not
# IFn dispatch. So only emit a direct call for :fn / :host (always functions) and
# a :var whose CURRENT root is a function (the common user/core-fn case). A :var
# holding an IFn COLLECTION (vector/keyword/set used as a fn) or a :local of
# unknown value falls through to jolt-call, which dispatches IFn correctly
# (function fast-path first). Trade-off, like direct-linking: a fn-var redefined
# to a collection after this call was compiled would still emit a direct call.
(defn- direct-call? [ctx fnode]
  (case (fnode :op)
    :fn true
    :host true
    :var (let [r (get (cell-for ctx (fnode :ns) (fnode :name)) :root)]
           (or (function? r) (cfunction? r)))
    false))

# Hot primitives emitted as native Janet ops (host-specific optimization): a
# call to clojure.core/+ etc. becomes (+ …) rather than a var deref + variadic
# core fn. Matches numeric semantics; relaxes the non-number checks (a documented
# perf-mode divergence, same as the bootstrap's core-renames).
(def- native-ops
  {"+" '+ "-" '- "*" '* "/" '/ "<" '< ">" '> "<=" '<= ">=" '>=
   "inc" '++ "dec" '--
   # verified semantic parity with the jolt fns (incl. negative operands):
   # mod is floored, rem (janet %) truncates, / is variadic with (/ x) -> 1/x.
   # quot is deliberately ABSENT: janet div floors where Clojure truncates.
   "mod" 'mod "rem" '%
   # jolt's bit fns are 2-arg (unlike Clojure's variadic), so these emit native
   # only at exactly the arity the interpreted fn accepts; bit-not is unary.
   "bit-and" 'band "bit-or" 'bor "bit-xor" 'bxor
   "bit-shift-left" 'blshift "bit-shift-right" 'brshift "bit-not" 'bnot})

(def- unary-ops {'++ true '-- true 'bnot true})
(def- binary-ops {'mod true '% true 'band true 'bor true 'bxor true
                  'blshift true 'brshift true})

(defn- native-op
  "If fnode is a clojure.core ref (or host ref) to a native-op primitive, return
  the Janet op symbol, else nil — only at an arity where the janet op and the
  jolt fn agree."
  [fnode nargs]
  (def nm (case (fnode :op)
            :var (when (= "clojure.core" (fnode :ns)) (fnode :name))
            :host (fnode :name)
            nil))
  (def op (and nm (get native-ops nm)))
  (cond
    (nil? op) nil
    (and (get unary-ops op) (not= nargs 1)) nil
    (and (get binary-ops op) (not= nargs 2)) nil
    op))

(defn- emit-invoke [ctx node]
  (def fnode (norm-node (node :fn)))
  (def args (map |(emit ctx $) (vview (node :args))))
  (def nop (native-op fnode (length args)))
  (cond
    nop (case nop
          '++ ['+ (in args 0) 1]
          '-- ['- (in args 0) 1]
          (tuple nop ;args))
    (direct-call? ctx fnode) (tuple (emit ctx fnode) ;args)
    (tuple jolt-call (emit ctx fnode) ;args)))

(defn- emit-vector [ctx node]
  (def items (map |(emit ctx $) (vview (node :items))))
  (tuple make-vec (tuple/slice (array/concat @['tuple] items))))

(defn- emit-map [ctx node]
  (def args @[build-map-literal])
  (each pair (vview (node :pairs))
    (def p (vview pair))
    (array/push args (emit ctx (in p 0)))
    (array/push args (emit ctx (in p 1))))
  (tuple/slice args))

# A set literal: build (make-phs e1 e2 …) so each element is evaluated at runtime
# then the persistent set is constructed — mirrors compiler.janet's emit-set-expr.
(defn- emit-set [ctx node]
  (def items (map |(emit ctx $) (vview (node :items))))
  (tuple/slice (array/concat @[phm/make-phs] items)))

(set emit
  (fn emit [ctx raw]
    (def node (norm-node raw))
    (case (node :op)
      :const (node :val)
      :local (symbol (node :name))
      :host (symbol (node :name))
      :var (let [cell (cell-for ctx (node :ns) (node :name))]
             (if (direct-var? ctx cell)
               (cell :root)                          # direct link: embed the fn value
               # Indirect: live deref, with the var-get FN CALL inlined away
               # (jolt-8sq): a non-dynamic var's value is always its root, so
               # the common case is two native table ops + a branch instead of
               # a function call. Dynamic vars take the full var-get (thread-
               # binding walk). The cell is quoted so it's embedded by
               # reference (a bare table in arg position would be re-evaluated
               # as a constructor — deep-copying it, and any atom in :root,
               # each call). Redefinition stays live: :root is read per call.
               # The :dynamic check must be PER CALL, not at emit: a
               # (def ^:dynamic x) in the same compiled unit marks the cell
               # dynamic only when the def RUNS, after this site was emitted —
               # the same reason JVM Clojure's Var.deref() checks the
               # thread-bound bit on every call. Non-dynamic vars (the vast
               # majority) pay two native table ops + a branch instead of a
               # function call.
               (let [qcell (tuple 'quote cell)]
                 ['if ['in qcell :dynamic]
                   (tuple var-get qcell)
                   ['in qcell :root]])))
      # (var x): the var object itself (not its value) — the embedded cell, by
      # reference. binding keys its thread-binding frame on this exact cell.
      :the-var (tuple 'quote (cell-for ctx (node :ns) (node :name)))
      :if ['if (emit ctx (node :test)) (emit ctx (node :then)) (emit ctx (node :else))]
      :do (emit-seq ctx node)
      :loop (emit-loop ctx node)
      :recur (emit-recur ctx node)
      :try (emit-try ctx node)
      :throw ['error (emit ctx (node :expr))]
      :def (let [cell (cell-for ctx (node :ns) (node :name))
                 meta (node :meta)]
             (tuple (if (and meta (not (empty? meta))) (var-setter-meta cell meta) (var-setter cell))
                    (emit ctx (node :init))))
      :let (emit-let ctx node)
      :fn (emit-fn ctx node)
      :invoke (emit-invoke ctx node)
      :vector (emit-vector ctx node)
      :map (emit-map ctx node)
      :set (emit-set ctx node)
      :quote ['quote (node :form)]
      (error (string "backend: unhandled op " (node :op))))))

(defn emit-ir
  "IR node -> Janet form (public entry for the back end)."
  [ctx node]
  (emit ctx node))

# --- pipeline wiring (the self-hosted compile path) ---

# Bootstrap-compile a source string into target-ns: each form is compiled via the
# bootstrap (native Janet) compiler and its defs interned in target-ns. This is
# the stage-1 builder — it runs BEFORE the self-hosted analyzer exists, so it's
# how both the compiler namespaces (jolt.ir/jolt.analyzer) and the clojure.core
# kernel tier (the structural fns the analyzer itself calls) get built. The
# analyzer uses unqualified referred names (jolt.host form-* + IR ctors), so the
# bootstrap's plain :var path compiles it; stateful forms fall back to interp.
(defn bootstrap-load-source
  "Stage-1 builder: load a source string into target-ns INTERPRETED. Runs before
  the self-hosted analyzer exists (it builds jolt.ir/jolt.analyzer and the kernel
  tier); self-compile-compiler! then re-runs those sources through the live
  analyzer so the steady-state compiler is compiled by itself — the retired
  bootstrap compiler's job, done by the interpreter + one fixpoint turn."
  [ctx target-ns src]
  (def saved (ctx-current-ns ctx))
  (ctx-set-current-ns ctx target-ns)
  (var s src)
  (while (> (length (string/trim s)) 0)
    (def parsed (r/parse-next s))
    (set s (in parsed 1))
    (def f (in parsed 0))
    (when (not (nil? f))
      (eval-form ctx @{} f)))
  (ctx-set-current-ns ctx saved))

# Compile-load an embedded jolt-core namespace by name (source from the stdlib map).
(defn- compile-load [ctx ns-name]
  (def src (get (get (ctx :env) :embedded-sources @{}) ns-name))
  (when src (bootstrap-load-source ctx ns-name src)))

# Build the self-hosted compiler (IR ctors + analyzer) via the bootstrap. The
# analyzer's references to clojure.core fns it uses (second/peek/subvec/mapv/
# update) resolve to whatever is interned in clojure.core at this point — so the
# kernel tier must already be loaded (see api/load-core-overlay!).
(defn- build-compiler! [ctx]
  (compile-load ctx "jolt.ir")
  (compile-load ctx "jolt.analyzer"))

(defn- ensure-analyzer [ctx]
  # Don't build until the kernel tier is loaded (see api/load-core-overlay! and
  # build-compiler!). Before then a compile request — e.g. a defn in a pre-kernel
  # tier — must fall back to the interpreter, not build the analyzer against a
  # core missing the fns it references (which would intern them as nil cells that
  # then shadow the real definitions on the self-rebuild). The flag is absent in
  # bare/test contexts that never load core; treat that as ready so those keep
  # building the analyzer lazily as before.
  (def env (ctx :env))
  (def gated (and (has-key? env :kernel-ready?) (not (get env :kernel-ready?))))
  (when (and (not gated)
             (= 0 (length ((ctx-find-ns ctx "jolt.analyzer") :mappings))))
    (build-compiler! ctx)))

(defn rebuild-compiler!
  "Recompile the self-hosted compiler (jolt.ir + jolt.analyzer) against the
  CURRENT clojure.core. The fractal turn: once a core tier supplies Clojure
  definitions the compiler itself uses, rebuilding makes the compiler run on
  them. Idempotent; re-interns the compiler namespaces over the existing cells."
  [ctx]
  (build-compiler! ctx))

(defn analyze-form
  "Run the portable Clojure analyzer (jolt.analyzer/analyze) on a reader form,
  returning host-neutral IR."
  [ctx form]
  (ensure-analyzer ctx)
  # Capture the real compile ns: the analyzer runs interpreted (defined in
  # jolt.analyzer), and the interpreter rebinds current-ns to a fn's defining ns
  # while it runs — so h/current-ns must read this instead of ctx-current-ns.
  (put (ctx :env) :compile-ns (ctx-current-ns ctx))
  (def saved-ns (ctx-current-ns ctx))
  (def av (ns-find (ctx-find-ns ctx "jolt.analyzer") "analyze"))
  # Pre-kernel bootstrap: ensure-analyzer is gated until the kernel tier loads
  # (see api/load-core-overlay!), so a compile request from an earlier tier (e.g.
  # 00-syntax's destructure defn) finds no analyzer. That fallback is DESIGNED —
  # route it through the sanctioned punt channel rather than crashing on a nil var.
  (unless av
    (put (ctx :env) :compile-ns nil)
    (error "jolt/uncompilable: analyzer not built (pre-kernel bootstrap)"))
  # The analyzer runs INTERPRETED; the interpreter rebinds current-ns to a fn's
  # defining ns (jolt.analyzer) while it runs and only restores on normal return.
  # A punt THROWS out of those frames, leaking jolt.analyzer as current-ns (and
  # :compile-ns stayed set) — the fallback interpretation then resolves user vars
  # against the wrong ns. Restore both on every exit.
  (def r (protect ((var-get av) ctx form)))
  (put (ctx :env) :compile-ns nil)
  (ctx-set-current-ns ctx saved-ns)
  (if (r 0) (r 1) (error (r 1))))

# The analyzer's deliberate punt signal — (uncompilable why) throws the string
# "jolt/uncompilable: <why>". Anything else escaping the compile step is an
# unexpected compiler error, not a punt.
(defn- uncompilable-error? [err]
  # The punt may arrive as a plain string (compiled analyzer) or wrapped in the
  # interpreter's exception struct {:jolt/type :jolt/exception :value s}
  # (interpreted analyzer — the stage-3 bootstrap path).
  (def msg (if (and (struct? err) (= :jolt/exception (get err :jolt/type)))
             (get err :value)
             err))
  (and (or (string? msg) (buffer? msg))
       (string/has-prefix? "jolt/uncompilable" (string msg))))

(defn compile-and-eval
  "Self-hosted compile path: analyze (portable Clojure) -> IR -> Janet -> eval.
  The interpreter fallback is DELIBERATE-ONLY (Stage 2): only an analyzer punt
  (jolt/uncompilable — the curated stateful/letrec set) falls back; any other
  compile-step error is a compiler bug and propagates rather than being silently
  hidden by interpretation. Runtime errors in compiled code propagate as before
  (no double-eval, no hidden errors)."
  [ctx form]
  (def compiled (protect (emit-ir ctx (analyze-form ctx form))))
  (if (compiled 0)
    (eval (compiled 1) (ctx-janet-env ctx))
    (if (uncompilable-error? (compiled 1))
      (eval-form ctx @{} form)
      (error (compiled 1)))))

(defn self-compile-compiler!
  "Stage 3 (interpreted bootstrap): once the overlay + interpreted analyzer are
  alive, run the kernel tier, jolt.ir, and jolt.analyzer back through the
  SELF-HOSTED pipeline — the analyzer compiles itself (and the kernel fns it
  uses), so by steady state the compiler runs compiled with no bootstrap
  compiler involved. Forms a punt can't compile stay interpreted (the
  deliberate channel)."
  [ctx]
  (def saved (ctx-current-ns ctx))
  (each [ns-name target] [["clojure.core.00-kernel" "clojure.core"]
                          ["jolt.ir" "jolt.ir"]
                          ["jolt.analyzer" "jolt.analyzer"]]
    (def src (get (get (ctx :env) :embedded-sources @{}) ns-name))
    (when src
      (ctx-set-current-ns ctx target)
      (var s src)
      (while (> (length (string/trim s)) 0)
        (def parsed (r/parse-next s))
        (set s (in parsed 1))
        (def f (in parsed 0))
        (when (not (nil? f))
          (def r (protect (compile-and-eval ctx f)))
          (unless (r 0) (eval-form ctx @{} f))))))
  (ctx-set-current-ns ctx saved))

(defn analyzer-built? [ctx]
  (> (length ((ctx-find-ns ctx "jolt.analyzer") :mappings)) 0))

(defn try-compile-fn
  "Compile a fn* form to a native Janet fn via the self-hosted pipeline, or nil if
  it can't be compiled (analyzer not yet built, or the body isn't compilable).
  Used to compile macro expanders for native-speed expansion."
  [ctx fn-form]
  (when (analyzer-built? ctx)
    (def compiled (protect (emit-ir ctx (analyze-form ctx fn-form))))
    (when (compiled 0)
      (def r (protect (eval (compiled 1) (ctx-janet-env ctx))))
      (when (r 0) (r 1)))))

# Wrap expanders in the `fn` MACRO, not the `fn*` primitive: `fn` desugars a
# destructured macro arglist (`[a & [b]]`, `[& {:keys [x]}]`) before lowering,
# whereas raw fn* punts on a destructuring rest param.
(def- fn-sym {:jolt/type :symbol :ns nil :name "fn"})

(defn recompile-macros!
  "Staged-bootstrap second pass: once the self-hosted analyzer is alive, replace
  every interpreted macro expander with a COMPILED one. The early macros (00-syntax
  etc.) are defined WHILE the analyzer is still being bootstrapped, so their
  expanders can't compile yet (the analyzer they'd compile through doesn't exist) —
  defmacro gives them an interpreted closure as a build-time crutch and stashes the
  source on the var (:macro-src). This pass compiles that source through the now-live
  analyzer and rebinds the var, so by steady state no macro expansion is interpreted
  — mirroring how a self-hosting compiler recompiles its seed once it can.

  Idempotent: a var compiled once is marked :macro-compiled and skipped (so the
  refer of a core macro into another ns, or a later rebuild, costs nothing). A macro
  whose body uses &env/&form keeps its interpreted closure (the compiled fn* has no
  such params). Returns the number of expanders compiled this pass."
  [ctx]
  (var n 0)
  (each ns (all-ns ctx)
    (each v (ns :mappings)
      (when (and (var? v) (var-macro? v)
                 (v :macro-src) (not (v :macro-compiled))
                 (not (v :macro-uses-env)))
        (def [args-form body] (v :macro-src))
        (def compiled
          (try-compile-fn ctx (array/concat @[fn-sym args-form] body)))
        (when compiled
          (bind-root v compiled)
          (put v :macro-compiled true)
          (++ n)))))
  n)

(defn recompile-defns!
  "Staged-bootstrap pass for early DEFNS (jolt-4j3) — the defn analog of
  recompile-macros!. Pre/at-kernel overlay defns (00-syntax's destructure,
  empty?/keys/vals, and the kernel tier in interpret mode) load as interpreted
  closures; the evaluator stashes their fn source on the var (:defn-src).
  Once the analyzer is alive, compile that source and swap the var's ROOT —
  callers go through the var, so they pick up the compiled fn. Skips vars
  already done; a body the analyzer can't compile stays interpreted."
  [ctx]
  (def mappings ((ctx-find-ns ctx "clojure.core") :mappings))
  (var n 0)
  (each nm (keys mappings)
    (def v (get mappings nm))
    (when (and (table? v) (get v :defn-src) (not (get v :defn-compiled)))
      (def compiled (try-compile-fn ctx (get v :defn-src)))
      (when compiled
        (put v :root compiled)
        (put v :defn-compiled true)
        (++ n))))
  n)

(defn ensure-macros-compiled!
  "Called once the overlay is fully loaded (api/load-core-overlay!): ensure the
  analyzer is built, then run the staged macro-recompile pass so the early
  (interpreted-during-bootstrap) macro expanders become compiled. Runs in EVERY
  mode — macro expansion is compiled code even when evaluation is interpreted
  (in interpret mode the tiers load fast interpreted, then this one pass builds
  the analyzer and compiles all stashed expanders; the analyzer itself stays
  interpreted there). :compile-macros? false (JOLT_INTERPRET_MACROS=1) skips it,
  keeping the fully-interpreted oracle. Cheap to call again (recompile-macros!
  skips already-compiled vars)."
  [ctx]
  (when (get (ctx :env) :compile-macros?)
    (ensure-analyzer ctx)
    (when (analyzer-built? ctx)
      # defns first: the expanders call them, and a recompiled expander that
      # ran before the defn pass still resolves through the var either way.
      (recompile-defns! ctx)
      (recompile-macros! ctx))))
