# Jolt Public API
# High-level interface for the Clojure-on-Janet interpreter.

(use ./types)
(use ./pv)
(use ./plist)
(use ./reader)
(use ./evaluator)
(use ./core)
(use ./loader)
(use ./async)
(import ./backend :as backend)
(import ./stdlib_embed :as stdlib-embed)
(import ./host_iface :as host)
(import ./javatime)   # java.time shims register into the evaluator at load

# Wire core's collection realizer into the evaluator's (.iterator coll) shim —
# late-bound here because the evaluator loads before core. Makes Java-Iterator-
# style loops (e.g. hiccup's iterate!) work over any jolt collection.
(set-coll-realizer! realize-for-iteration)

# A defmacro expander compiles to a native fn (built as (fn args body...) and run
# through the self-hosted pipeline) so macro expansion is COMPILED code, zero runtime
# cost — instead of an interpreted closure, mirroring Clojure (macros are ordinary
# compiled fns). Wrapped in the `fn` MACRO (not the `fn*` primitive) so a destructured
# macro arglist — `[a & [b]]`, `[& {:keys [x]}]`, nested — desugars before lowering;
# raw fn* punts on a destructuring rest param. Returns nil when the analyzer isn't
# built yet (the early macros, expanded WHILE the analyzer is being bootstrapped) or
# the body isn't compilable; in that case defmacro keeps an interpreted closure, and
# backend/recompile-macros! replaces it with a compiled expander once the analyzer
# comes alive (staged bootstrap — the interpreter is a build-time crutch, gone by
# steady state).
(set macro-compile-hook
  (fn [ctx args-form body]
    (backend/try-compile-fn ctx
      (array/concat @[{:jolt/type :symbol :ns nil :name "fn"} args-form] body))))

(defn normalize-pvecs
  "Deep-convert any sequential (pvec/tuple/array) to a Janet tuple. Test helper
  so Janet-level `=`/deep= can compare jolt collection results against Janet
  tuple literals regardless of representation — mirroring Clojure, where vectors
  and lists with the same elements are equal."
  [x]
  (cond
    # lazy-seq: realize to a tuple (map/filter/take now return lazy seqs).
    (and (table? x) (= (get x :jolt/type) :jolt/lazy-seq))
      (tuple ;(map normalize-pvecs (realize-for-iteration x)))
    (pvec? x) (tuple ;(map normalize-pvecs (pv->array x)))
    (plist? x) (tuple ;(map normalize-pvecs (pl->array x)))
    (tuple? x) (tuple ;(map normalize-pvecs x))
    (array? x) (tuple ;(map normalize-pvecs x))
    x))


# Ordered clojure.core tiers (embedded jolt-core/clojure/core/NN-*.clj). Each tier
# may reference only the Janet seed + earlier tiers. A :kernel tier holds the
# structural fns the self-hosted compiler itself uses (second/peek/subvec/mapv/
# update); in compile mode it must be bootstrap-compiled into clojure.core BEFORE
# the analyzer is built (the analyzer depends on it), so it bypasses the
# self-hosted pipeline. Non-kernel tiers route through eval-toplevel like any
# source (compiled when :compile?, interpreted otherwise — the analyzer, built
# lazily on the first such form, sees the kernel tier already in place).
(def- core-tiers
  [{:ns "clojure.core.00-syntax" :kernel false}
   {:ns "clojure.core.00-kernel" :kernel true}
   {:ns "clojure.core.10-seq"    :kernel false}
   {:ns "clojure.core.20-coll"   :kernel false}
   {:ns "clojure.core.25-sorted" :kernel false}
   {:ns "clojure.core.30-macros" :kernel false}
   {:ns "clojure.core.40-lazy"   :kernel false}
   {:ns "clojure.core.50-io"     :kernel false}])

(defn- eval-overlay-source [ctx src]
  (var s src)
  (while (> (length (string/trim s)) 0)
    (def [form rest] (parse-next s))
    (set s rest)
    (when (not (nil? form)) (eval-toplevel ctx form))))

(defn- load-core-overlay!
  "Load the Clojure portion of clojure.core in dependency-ordered tiers. See
  core-tiers and jolt-core/clojure/core/."
  [ctx]
  (def env (ctx :env))
  (def compile? (get env :compile?))
  # Core compiles with direct-linking on when :aot-core? (so core->core calls
  # are direct). The flag is restored to the user-code default afterward, so
  # user/REPL code stays indirect and fully redefinable.
  (def user-dl (get env :direct-linking?))
  (def core-dl (get env :aot-core?))
  (def saved (ctx-current-ns ctx))
  (ctx-set-current-ns ctx "clojure.core")
  # Gate the analyzer build until the kernel tier loads (see ensure-analyzer):
  # present-and-false here means pre-kernel compiles fall back to the interpreter.
  (put env :kernel-ready? false)
  # Pre/at-kernel defns load interpreted in some mode (00-syntax always; the
  # kernel tier too in interpret mode); stash their fn sources so the staged
  # recompile pass (backend/recompile-defns!) can compile them once the
  # analyzer is alive. Cleared after the kernel tier so later tiers and user
  # code don't stash.
  (put env :stash-defn-src? true)
  (each tier core-tiers
    (when-let [src (get stdlib-embed/sources (tier :ns))]
      (put env :direct-linking? core-dl)
      (if (and compile? (tier :kernel))
        (backend/bootstrap-load-source ctx "clojure.core" src)
        (eval-overlay-source ctx src))
      # The self-hosted compiler depends on the kernel tier (second/peek/mapv/...).
      # Mark it ready once that tier is in place so the analyzer can be built; a
      # pre-kernel tier that triggers a compile (e.g. a defn in 00-syntax) instead
      # falls back to the interpreter rather than building the analyzer against a
      # half-loaded core (which would forward-ref the missing kernel fns to nil).
      (when (tier :kernel)
        (put env :kernel-ready? true)
        (put env :stash-defn-src? false))))
  (put env :direct-linking? user-dl)
  (ctx-set-current-ns ctx saved)
  # Stage 3 interpreted bootstrap: the analyzer was loaded INTERPRETED (no
  # bootstrap compiler); have it compile itself + the kernel tier before the
  # macro pass, so steady-state compilation runs compiled.
  (when compile?
    (backend/self-compile-compiler! ctx))
  # Staged bootstrap: the early macros (00-syntax) were defined while the analyzer
  # was still being built, so their expanders are interpreted closures. Now that the
  # full overlay + analyzer are in place, recompile those expanders to native code —
  # by steady state no macro expansion runs interpreted (no-op in interpreter mode).
  (backend/ensure-macros-compiled! ctx)
  # print-method's record hook: only wirable once the overlay multimethod
  # exists (50-io tier), so it rides the end of overlay load.
  (install-print-method-cb! ctx))

# clojure.math (Clojure 1.11) backed directly by Janet's math natives
# (jolt-h79). The vars hold plain Janet functions, so compiled calls
# direct-link — unlike Math/sqrt interop forms, which are in the frozen
# interpret-only punt set (~5us/call vs ~30ns here). Installed as a
# populated namespace, so (require '[clojure.math :as m]) is a no-op pass.
(defn- install-clojure-math! [ctx]
  (def ns (ctx-find-ns ctx "clojure.math"))
  (def fns
    {"sqrt" math/sqrt "cbrt" math/cbrt "pow" math/pow
     "exp" math/exp "expm1" math/expm1
     "log" math/log "log10" math/log10 "log1p" math/log1p
     "sin" math/sin "cos" math/cos "tan" math/tan
     "asin" math/asin "acos" math/acos "atan" math/atan "atan2" math/atan2
     "sinh" math/sinh "cosh" math/cosh "tanh" math/tanh
     "floor" math/floor "ceil" math/ceil "rint" math/round
     "round" (fn cm-round [x] (math/round x))
     "signum" (fn cm-signum [x] (cond (< x 0) -1.0 (> x 0) 1.0 0.0))
     "to-degrees" (fn cm-to-degrees [r] (/ (* r 180.0) math/pi))
     "to-radians" (fn cm-to-radians [d] (/ (* d math/pi) 180.0))
     "hypot" (fn cm-hypot [a b] (math/sqrt (+ (* a a) (* b b))))
     "floor-div" (fn cm-floor-div [a b] (math/floor (/ a b)))
     "floor-mod" (fn cm-floor-mod [a b] (mod a b))
     "E" math/e "PI" math/pi})
  (eachp [nm f] fns (ns-intern ns nm f))
  # mark loaded so maybe-require-ns never goes looking for a source file
  (def loaded (or (get (ctx :env) :loaded-namespaces)
                  (let [t @{}] (put (ctx :env) :loaded-namespaces t) t)))
  (put loaded "clojure.math" true))

(defn init
  "Create a new Jolt evaluation context.
  opts may contain:
    :namespaces — map of {ns-name → {sym → value, ...}, ...}
    :mutable?   — use Janet mutable data structures instead of persistent
    :compile?   — compile Clojure forms via the self-hosted pipeline (analyzer ->
                  IR -> Janet back end), falling back to the interpreter as needed
    :paths      — extra source roots to search for namespaces (after the stdlib)"
  [&opt opts]
  (default opts {})
  (let [ctx (make-ctx opts)]
    # The .clj stdlib (clojure.string, jolt.http, …) baked into the image at build
    # time, so it loads from any directory; the loader falls back to this when a
    # namespace isn't found on disk. (See stdlib-embed.)
    (put (ctx :env) :embedded-sources stdlib-embed/sources)
    # Extra source roots: opts :paths, then JOLT_PATH (colon-separated). These are
    # searched after the stdlib so (require ...) finds deps.edn-resolved libs.
    (let [roots (get (ctx :env) :source-paths)]
      (each p (get opts :paths []) (array/push roots p))
      (when-let [jp (os/getenv "JOLT_PATH")]
        (each p (string/split ":" jp) (when (> (length p) 0) (array/push roots p)))))
    # Collection representation (persistent vs mutable) is selected at BUILD time
    # via JOLT_MUTABLE (see config.janet); init-core! registers vec/vector/conj/
    # etc. that produce the mode-appropriate values, so nothing extra to load.
    (init-core! ctx)
    # clojure.core.async (channels + go blocks on Janet fibers); pre-populated
    # so (require '[clojure.core.async ...]) finds it and applies :as/:refer.
    (install-async! ctx)
    # Host contract (ns jolt.host): the seam the portable jolt-core compiler calls.
    (host/install! ctx)
    (install-clojure-math! ctx)
    # require/maybe-require-ns route loaded namespaces through the loader's
    # compile-or-interpret eval-toplevel (the evaluator can't import the loader
    # — that would be circular — so it reads this hook). Without it, required
    # namespaces ran interpreted-only.
    (put (ctx :env) :toplevel-eval eval-toplevel)
    # Inter-procedural type-inference hook (jolt-767): the evaluator calls this
    # after a unit finishes loading (optimization mode only). Installed here to
    # avoid an evaluator->backend circular import.
    (put (ctx :env) :infer-unit! backend/infer-unit!)
    (put (ctx :env) :infer-program! backend/infer-program!)   # jolt-t34 whole-program
    # Stateful primitives as ctx-capturing clojure.core fns (protocol-dispatch,
    # register-method, …) — so the protocol macros compile to plain invokes. Must
    # precede the overlay (its defprotocol/extend-type expansions call these).
    (install-stateful-fns! ctx)
    # Clojure portion of clojure.core (jolt-core/clojure/core.clj): fns expressed
    # in plain Clojure on top of the Janet primitives interned above. Loaded into
    # clojure.core and compiled by the self-hosted pipeline (or interpreted when
    # :compile? is off). Phase 4 kernel-shrink seam — see that file.
    (load-core-overlay! ctx)
    # load-string and eval as VALUES need the loader's compile-or-interpret
    # routing, which lives above the evaluator — intern them here. (The eval
    # special form still handles direct calls; this covers value position,
    # e.g. (map eval forms).)
    (let [core (ctx-find-ns ctx "clojure.core")]
      (ns-intern core "load-string"
        (fn [s]
          (var cur s)
          (var result nil)
          (while (> (length (string/trim cur)) 0)
            (def [form rest-src] (parse-next cur))
            (set cur rest-src)
            (when (not (nil? form)) (set result (eval-toplevel ctx form))))
          result))
      (ns-intern core "eval" (fn [form] (eval-toplevel ctx form))))
    # Init is done: core + the self-hosted compiler are loaded with :inline? off
    # (so they compiled exactly as before). Flip inlining on for subsequent
    # user-code compilation iff user direct-linking is on (JOLT_DIRECT_LINK=1) —
    # the inline pass only inlines targets that won't be redefined, the same
    # safety the direct-linking flag asserts (jolt-87f).
    (put (ctx :env) :inline? (if (get (ctx :env) :direct-linking?) true false))
    # jolt-t34. Two shape gates:
    #  :shapes?     — shape-recs are active. Records use declared-shape layout +
    #                 bare-index reads here. ON wherever the inference that proves
    #                 reads runs = direct-linking. JOLT_NO_SHAPE force-disables.
    #  :map-shapes? — also shape generic const-key MAP literals. Opt-in (JOLT_SHAPE)
    #                 because shaping maps net-loses on unproven reads; records win.
    (put (ctx :env) :shapes?
      (and (get (ctx :env) :direct-linking?) (not (os/getenv "JOLT_NO_SHAPE"))))
    (put (ctx :env) :map-shapes?
      (and (os/getenv "JOLT_SHAPE") (not (os/getenv "JOLT_NO_SHAPE"))))
    # Whole-program (Stalin) mode (jolt-t34): opt-in, closed-world. Defers the
    # per-ns inference and runs one fixpoint over all units at the end (main, or a
    # harness calling infer-program!). Needs direct-linking (the closed-world
    # assumption); slow/memory-heavy builds are the documented trade-off.
    (put (ctx :env) :whole-program?
      (and (os/getenv "JOLT_WHOLE_PROGRAM") (get (ctx :env) :direct-linking?)))
    ctx))

# --- Context snapshot/fork (cheap isolated copies) --------------------------
#
# init is expensive (~50 ms interpreted, ~900 ms compiled: tier loading, analyzer
# build, macro recompilation). For workloads that need MANY isolated contexts —
# the test harnesses build a fresh ctx per case — snapshot a fully-built ctx once
# and fork cheap deep copies (~2 ms) from it via Janet marshal/unmarshal. A fork
# shares nothing mutable with the original: defs, protocol extensions, hierarchy
# changes, atom states in one fork are invisible to the others.
#
# The reverse-lookup dicts must be built from root-env (cfunctions and abstract
# values from the Janet runtime marshal by reference through them) BEFORE any ctx
# values exist in scope — module load time here, so user code can't leak into it.
(def- image-load-dict (env-lookup root-env))
(def- image-make-dict (invert image-load-dict))

(defn snapshot
  "Marshal a fully-built context into a buffer that fork can cheaply clone.
  Build the ctx (init), customize it if needed, then snapshot once."
  [ctx]
  (marshal ctx image-make-dict))

(defn fork
  "A fresh, fully-isolated deep copy of a snapshotted context (~2 ms, vs
  re-running init). (fork (snapshot ctx)) behaves exactly like ctx did at
  snapshot time; mutations to a fork never affect the original or other forks."
  [snap]
  (unmarshal snap image-load-dict))

# --- Disk-cached init (AOT context image) ------------------------------------
#
# init in compile mode is ~2.4 s (tier loading, analyzer self-compile, macro
# recompilation) — paid by every PROCESS that builds a ctx from source, e.g.
# each `jpm test` file. init-cached pays it once: the built ctx is snapshotted
# to an image file and later processes unmarshal it (~tens of ms). Marshaling
# is against root-env (same dicts as snapshot/fork), so core cfunctions ride by
# name and everything jolt-level rides by value — a loaded image needs nothing
# from the baking process. The cache key fingerprints everything a fresh build
# would read: the embedded .clj stdlib (which includes jolt-core — the
# analyzer, IR, and core tiers), the .janet seed sources next to this module,
# the janet version, and the init opts. Any change rebuilds; a corrupt or
# unreadable image silently rebuilds. JOLT_NO_IMAGE_CACHE=1 disables.

# Captured at module load: in source mode this is .../src/jolt/api.janet, so
# the seed sources can be fingerprinted; nil or stale in a built binary, where
# the baked-at-build-time ctx makes init-cached pointless anyway.
(def- api-module-file (dyn :current-file))

(defn- src-dir []
  (when api-module-file
    (let [idxs (string/find-all "/" api-module-file)]
      (when (not (empty? idxs))
        (string/slice api-module-file 0 (last idxs))))))

(defn- source-fingerprint
  "Hash + total length of every source a fresh init depends on. Two numbers, so
  a 32-bit hash collision alone can't alias two different source trees."
  []
  (def buf @"")
  (each k (sorted (keys stdlib-embed/sources))
    (buffer/push buf k "\x00" (get stdlib-embed/sources k) "\x00"))
  (def dir (src-dir))
  (when dir
    (each f (sorted (os/dir dir))
      (when (string/has-suffix? ".janet" f)
        (buffer/push buf f "\x00" (slurp (string dir "/" f)) "\x00"))))
  [(hash (string buf)) (length buf)])

(defn- image-cache-path [opts]
  (def dir (or (os/getenv "JOLT_IMAGE_CACHE_DIR")
               (os/getenv "TMPDIR")
               "/tmp"))
  (def [h len] (source-fingerprint))
  # Opts land in the key via their printed form; an opt that prints unstably
  # (e.g. a closure in :namespaces) just degrades to a cache miss, never to a
  # wrong hit. Runtime knobs that shape the ctx outside opts ride along too.
  (def key (string/format "%q|%q|%q|%q|%q|%q|%q|%q|%q|%q|%q|%q"
                          (string janet/version "-" janet/build)
                          opts
                          (os/getenv "JOLT_PATH")
                          (os/getenv "JOLT_MUTABLE")
                          (os/getenv "JOLT_AOT_CORE")
                          (os/getenv "JOLT_FEATURES")
                          (os/getenv "JOLT_INTERPRET_MACROS")
                          (os/getenv "JOLT_DIRECT_LINK")
                          (os/getenv "JOLT_NO_IR_PASSES")
                          (os/getenv "JOLT_CHECK_HINTS")
                          # :shapes? is baked into the image; key on every input
                          # to it so a cache hit never carries a wrong shape state
                          (os/getenv "JOLT_SHAPE")
                          (os/getenv "JOLT_NO_SHAPE")))
  (string dir "/jolt-ctx-" (band h 0x7FFFFFFF) "-" len "-" (band (hash key) 0x7FFFFFFF) ".jimg"))

(defn init-cached
  "init, but disk-cached: the first call builds the context and writes a
  bytecode image; later calls (any process, same sources) load the image
  instead of rebuilding. Same opts as init. JOLT_NO_IMAGE_CACHE=1 disables;
  JOLT_IMAGE_CACHE_DIR overrides the cache directory (default TMPDIR)."
  [&opt opts]
  (default opts {})
  (if (or (= "1" (os/getenv "JOLT_NO_IMAGE_CACHE")) (nil? (src-dir)))
    (init opts)
    (let [path (image-cache-path opts)
          loaded (when (os/stat path)
                   (let [r (protect (fork (slurp path)))]
                     # unmarshal of a corrupt image can also "succeed" with a
                     # non-ctx value, so check the shape, not just the throw.
                     (when (and (r 0) (ctx? (r 1)))
                       (r 1))))]
      (or (when loaded
            # per-PROCESS wiring an image restore skips: the renderer's
            # print-method hook lives in module state, not the marshaled ctx
            (install-print-method-cb! loaded)
            loaded)
          (let [ctx (init opts)
                tmp (string path "." (os/getpid) ".tmp")]
            # Atomic publish so concurrent cold starts never see a torn image.
            (when (protect (spit tmp (snapshot ctx)))
              (protect (os/rename tmp path)))
            ctx)))))

(defn eval-one
  "Evaluate a single already-parsed form. Routing (compile when :compile? is set,
  stateful forms interpret, interpreter fallback for forms the compiler can't
  handle) lives in loader/eval-toplevel so load-ns and eval-one stay in sync."
  [ctx form]
  (eval-toplevel ctx form))

(defn eval-string
  "Evaluate a Clojure source string in a Jolt context.
  When :compile? is enabled, compiles to Janet and evaluates.
  Macros are expanded at compile time.
  Context-modifying forms (ns, defmacro, deftype, require, in-ns, defmulti, defmethod)
  always use the interpreter."
  [ctx s]
  (eval-one ctx (parse-string s)))

(defn eval-string*
  "Evaluate a Clojure source string with explicit bindings."
  [ctx s bindings]
  (let [form (parse-string s)]
    (eval-form ctx bindings form)))

(defn load-string
  "Evaluate all forms from a Clojure source string.
  Uses parse-next to load every top-level form in sequence.
  Returns the result of the last form evaluated."
  [ctx s &opt file]
  (default file "<eval>")
  # record form positions so the checker can report file:line:col (jolt-fqy).
  # The checker is on when JOLT_TYPE_CHECK selects it, OR by default in
  # direct-link builds (where it piggybacks on inference for free).
  (when (or (checker-enabled?) (get (ctx :env) :inline?))
    (track-positions! true)
    (put (ctx :env) :tc-source s)
    (put (ctx :env) :tc-file file))
  (eval-forms-positioned ctx (parse-all-positioned s file) file))

