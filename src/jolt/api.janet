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

(defn normalize-pvecs
  "Deep-convert any sequential (pvec/tuple/array) to a Janet tuple. Test helper
  so Janet-level `=`/deep= can compare jolt collection results against Janet
  tuple literals regardless of representation — mirroring Clojure, where vectors
  and lists with the same elements are equal."
  [x]
  (cond
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
  [{:ns "clojure.core.00-kernel" :kernel true}
   {:ns "clojure.core.10-seq"    :kernel false}])

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
  (def compile? (get (ctx :env) :compile?))
  (def saved (ctx-current-ns ctx))
  (ctx-set-current-ns ctx "clojure.core")
  (each tier core-tiers
    (when-let [src (get stdlib-embed/sources (tier :ns))]
      (if (and compile? (tier :kernel))
        (backend/bootstrap-load-source ctx "clojure.core" src)
        (eval-overlay-source ctx src))))
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
