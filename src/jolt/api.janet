# Jolt Public API
# High-level interface for the Clojure-on-Janet interpreter.

(use ./types)
(use ./pv)
(use ./plist)
(use ./reader)
(use ./evaluator)
(use ./core)
(use ./compiler)
(use ./loader)
(use ./async)
(import ./backend :as backend)
(import ./stdlib_embed :as stdlib-embed)
(import ./host_iface :as host)

# A defmacro expander compiles to a native fn (built as (fn* args body...) and run
# through the self-hosted pipeline) so macro expansion is compiled, zero runtime
# cost — instead of an interpreted closure. Returns nil (interpreted fallback) when
# the analyzer isn't built yet or the body isn't compilable.
(set macro-compile-hook
  (fn [ctx args-form body]
    (backend/try-compile-fn ctx
      (array/concat @[{:jolt/type :symbol :ns nil :name "fn*"} args-form] body))))

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
   {:ns "clojure.core.30-macros" :kernel false}])

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
      (when (tier :kernel) (put env :kernel-ready? true))))
  (put env :direct-linking? user-dl)
  (ctx-set-current-ns ctx saved))

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
    # Clojure portion of clojure.core (jolt-core/clojure/core.clj): fns expressed
    # in plain Clojure on top of the Janet primitives interned above. Loaded into
    # clojure.core and compiled by the self-hosted pipeline (or interpreted when
    # :compile? is off). Phase 4 kernel-shrink seam — see that file.
    (load-core-overlay! ctx)
    ctx))

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
  [ctx s]
  (var cur s)
  (var result nil)
  (while (> (length (string/trim cur)) 0)
    (def [form rest] (parse-next cur))
    (set cur rest)
    (when (not (nil? form))
      (set result (eval-one ctx form))))
  result)

(defn compile-string
  "Compile a Clojure source string to Janet source.
  Returns the Janet source string."
  [s]
  (let [form (parse-string s)]
    (compile-form form)))

(defn compile-file
  "Compile a .clj file to Janet source and optionally eval it.
  When ctx has :compile? enabled, also evaluates the compiled forms.
  Returns the namespace name."
  [ctx filepath]
  (load-ns ctx filepath))
