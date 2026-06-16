# Jolt Evaluator — base: forward vars, syntax-quote, ns-loading, registries
# Extracted from evaluator.janet (jolt-oudv, phase 2a split).

(use ./types)
(use ./phm)
(use ./phs)
(use ./lazyseq)
(use ./pv)
(use ./plist)
(use ./config)
(use ./reader)
(use ./regex)

# Host PNG encoder, exposed to the overlay as `janet.png/encode` / `janet.png/write`
# (resolved through module-load-env below). Pure Janet, no jolt deps.
(import ./png :prefix "png/")


# The env this module was loaded under — proto-chains to the Janet root env;
# the janet/* interop bridge falls back to it inside env-less fibers.
(def module-load-env (fiber/getenv (fiber/current)))

# jpm-module autoload: a janet.<module>/<name> reference whose module isn't
# in the env is satisfied by requiring it from the jpm module path on first
# use — (janet.spork.http/server ...) just works when spork is installed,
# and the same goes for any jpm module. Loaded bindings are cached here
# (and failures negatively cached, so a missing module errors fast).
(def janet-bridge-extras @{})
(def janet-bridge-failed @{})
(defn bridge-autoload
  "jname is spork.http/server-shaped: require spork/http, cache its public
  bindings under the dotted prefix, return the one asked for (nil when the
  module is missing or has no such binding)."
  [jname]
  (def slash (string/find "/" jname))
  (when slash
    (def mod-ns (string/slice jname 0 slash))
    (unless (get janet-bridge-failed mod-ns)
      (def mod-path (string/replace-all "." "/" mod-ns))
      (def r (protect (require mod-path)))
      (if (r 0)
        (eachp [sym entry] (r 1)
          (when (and (symbol? sym) (table? entry) (not (get entry :private)))
            (put janet-bridge-extras (string mod-ns "/" sym) (get entry :value))))
        (put janet-bridge-failed mod-ns true))))
  (in janet-bridge-extras jname))

(defn sym-name?
  [sym-s name-str]
  (and (struct? sym-s) (= :symbol (sym-s :jolt/type)) (= name-str (sym-s :name))))

(defn- special-symbol?
  [name]
  (or (= name "quote") (= name "syntax-quote") (= name "unquote")
      (= name "unquote-splicing") (= name "do") (= name "if")
      (= name "def") (= name "defmacro") (= name "fn*") (= name "let*") (= name "loop*")
      (= name "recur") (= name "throw") (= name "try")
      (= name "set!") (= name "var")
      (= name "eval")
      (= name "new") (= name ".")
      # var-get/var-set/var?/alter-var-root/alter-meta!/reset-meta! are plain
      # clojure.core fns (core-bindings); find-var/intern are ctx-capturing fns
      # (install-stateful-fns!) — no longer special forms (Stage 2 tier 6).
      # locking/instance?/satisfies?/defonce/read-string/macroexpand-1 and the
      # multimethod table ops are overlay macros / clojure.core fns now
      # (Stage 2 tier 6c) — not special forms.
      ))

(var eval-form nil)

# Macro expansion cache (interpreter): a macro CALL form expands ONCE and the
# result is reused — macroexpansion is a compile-time step with zero runtime cost,
# the proper Lisp model. Keyed by the call form's identity (a fn body re-evaluates
# the same form arrays each call). Also gives compile-once gensym semantics (a
# foo# auto-gensym is fixed across calls, unlike per-call re-expansion). Cleared
# when a macro is (re)defined so stale expansions don't linger.
(def macro-cache @{})

# Compile hook for macro expanders: set by the api to (fn [ctx args-form body] ->
# compiled-janet-fn | nil). When set and the body is compilable (no &env/&form,
# analyzer available), defmacro uses the compiled expander instead of the
# interpreted closure — macro expansion at native speed, zero runtime cost.
(var macro-compile-hook nil)

(defn form-uses-sym? [form nm]
  (cond
    (and (struct? form) (= :symbol (form :jolt/type))) (= nm (form :name))
    (or (array? form) (tuple? form))
    (do (var found false) (each x form (when (form-uses-sym? x nm) (set found true) (break))) found)
    (and (struct? form) (nil? (form :jolt/type)))
    (do (var found false) (each k (keys form)
          (when (or (form-uses-sym? k nm) (form-uses-sym? (get form k) nm)) (set found true) (break))) found)
    false))

# A transient is a tagged mutable table @{:jolt/type :jolt/transient :kind ...}.
(defn- jolt-transient? [x]
  (and (table? x) (= :jolt/transient (get x :jolt/type))))

# Read-only lookup over a transient (vector index / map key / set membership),
# mirroring core-get. Map/set backing tables are keyed by the same canon used
# by phm, so canonicalize collection keys here too.
(defn- transient-lookup [t k default]
  (case (t :kind)
    :vector (let [a (t :arr)]
              (if (and (number? k) (= k (math/floor k)) (>= k 0) (< k (length a)))
                (in a k) default))
    :map (let [e (get (t :tbl) (canon k))] (if (nil? e) default (in e 1)))
    :set (if (nil? (get (t :tbl) (canon k))) default k)
    default))

(defn coll-lookup
  "Clojure `get` semantics over a jolt collection, used for collection-as-IFn."
  [coll k default]
  (cond
    (jolt-transient? coll) (transient-lookup coll k default)
    (shape-rec? coll) (shape-get coll k default)
    # sorted colls are tables — without this arm they fell into the raw
    # table-get branch and (:k (sorted-map ...)) was always nil (jolt-4vr spec)
    (and (table? coll) (or (= :jolt/sorted-map (coll :jolt/type))
                           (= :jolt/sorted-set (coll :jolt/type))))
      ((get (coll :ops) :get) coll k default)
    (phm? coll) (phm-get coll k default)
    (set? coll) (if (phs-contains? coll k) k default)
    (pvec? coll)
      (if (and (number? k) (= k (math/floor k)) (>= k 0) (< k (pv-count coll)))
        (pv-nth coll k) default)
    (or (tuple? coll) (array? coll))
      (if (and (number? k) (= k (math/floor k)) (>= k 0) (< k (length coll)))
        (in coll k) default)
    (or (struct? coll) (table? coll))
      (let [v (get coll k :jolt/not-found)]
        (if (= v :jolt/not-found) default v))
    (nil? coll) default
    default))

(defn jolt-invoke
  "Apply f to already-evaluated args. Handles real functions and Clojure's
  IFn collections: vectors (index lookup), maps/sets/keywords/symbols (get),
  and deftype/record values implementing IFn. `args` is an array."
  [ctx f args]
  (cond
    (or (function? f) (cfunction? f)) (apply f args)
    (jolt-transient? f) (transient-lookup f (get args 0) (get args 1))
    # a record shape-rec is callable: IFn impl if it has one, else map-like
    # field access. A plain (non-record) shape-rec is just field access.
    (shape-rec? f)
      (let [tag (record-tag f)
            ifn (when tag (find-protocol-method ctx tag "IFn" "-invoke"))]
        (if ifn (apply ifn f args) (shape-get f (get args 0) (get args 1))))
    (keyword? f) (coll-lookup (get args 0) f (get args 1))
    (and (struct? f) (= :symbol (f :jolt/type)))
      (coll-lookup (get args 0) f (get args 1))
    (and (table? f) (or (= :jolt/sorted-map (f :jolt/type))
                        (= :jolt/sorted-set (f :jolt/type))))
      # the overlay-attached :get op (comparator-based lookup, like Clojure)
      ((get (f :ops) :get) f (get args 0) (get args 1))
    (phm? f) (phm-get f (get args 0) (get args 1))
    (set? f) (if (phs-contains? f (get args 0)) (get args 0) (get args 1))
    (pvec? f)
      (let [k (get args 0)]
        (if (and (number? k) (= k (math/floor k)) (>= k 0) (< k (pv-count f)))
          (pv-nth f k)
          (error (string "Index " k " out of bounds for vector of length " (pv-count f)))))
    (or (tuple? f) (array? f))
      (let [k (get args 0)]
        (if (and (number? k) (= k (math/floor k)) (>= k 0) (< k (length f)))
          (in f k)
          (error (string "Index " k " out of bounds for vector of length " (length f)))))
    # Map literal only (struct with no :jolt/type). A tagged struct (char/etc.)
    # is not callable — symbols are handled above; chars fall through to the error.
    (and (struct? f) (nil? (get f :jolt/type)))
      (let [v (get f (get args 0) :jolt/not-found)]
        (if (= v :jolt/not-found) (get args 1) v))
    (and (table? f) (get f :jolt/deftype))
      (let [ifn-fn (find-protocol-method ctx (get f :jolt/deftype) "IFn" "-invoke")]
        (if ifn-fn (apply ifn-fn f args)
          (if (and (get f :jolt/protocol-methods) (get (f :jolt/protocol-methods) :-invoke))
            (apply (get (f :jolt/protocol-methods) :-invoke) f args)
            # No IFn impl: fall back to map-like field access, e.g. (point :x)
            (let [v (get f (get args 0) :jolt/not-found)]
              (if (= v :jolt/not-found) (get args 1) v)))))
    (and (table? f) (get f :jolt/protocol-methods))
      (let [invoke-fn (get (f :jolt/protocol-methods) :-invoke)]
        (if invoke-fn (apply invoke-fn f args)
          (error (string "Cannot call " (type f) " as a function"))))
    (error (string "Cannot call " (type f) " as a function"))))

(defn- sq-symbol
  "Resolve a symbol inside syntax-quote. `foo#` becomes a stable auto-gensym
  (per-expansion, via gsmap); special forms are left unqualified; a clojure.core
  name is fully qualified to clojure.core/ (matching Clojure, for hygiene); other
  symbols are qualified to the current namespace so they resolve when the macro is
  used elsewhere."
  [ctx form gsmap]
  (if (nil? (form :ns))
    (let [nm (form :name)]
      (cond
        (string/has-suffix? "#" nm)
          (or (get gsmap nm)
              (let [g {:jolt/type :symbol :ns nil
                       :name (string (string/slice nm 0 -2) "__" (string (gensym)) "__auto")}]
                (put gsmap nm g) g))
        (special-symbol? nm) form
        (ns-find (ctx-find-ns ctx "clojure.core") nm)
          {:jolt/type :symbol :ns "clojure.core" :name nm}
        # Unresolved -> qualify to the namespace being COMPILED when set (the
        # analyzer runs interpreted in jolt.analyzer, so ctx-current-ns is wrong
        # mid-compile — the same seam resolve-var/h-current-ns use). Matters when
        # a macro expander's template is lowered while a symbol it references is
        # not yet defined (deftype's extend-type, defined later in the same tier):
        # it must qualify to the macro's home ns, not jolt.analyzer.
        {:jolt/type :symbol
         :ns (or (get (ctx :env) :compile-ns) (ctx-current-ns ctx))
         :name nm}))
    # Alias-qualified (impl/foo): resolve the alias to its target namespace so the
    # emitted symbol resolves at the macro's USE site, which has no such alias
    # (jolt-9av). Matches Clojure's syntax-quote. A real ns name (not an alias)
    # has no entry and is left as written.
    (let [cur (ctx-find-ns ctx (or (get (ctx :env) :compile-ns) (ctx-current-ns ctx)))
          target (and cur (or (ns-alias-lookup cur (form :ns))
                              (ns-import-lookup cur (form :ns))))]
      (if target
        {:jolt/type :symbol :ns target :name (form :name)}
        form))))

(defn d-realize
  "Realize a lazy-seq to an array for positional destructuring / splicing; pass
  others (pvec/plist coerced to array, everything else unchanged). nil is an
  empty seq, as everywhere in Clojure — ~@nil splices nothing (an interpreted
  macro's empty & rest binds nil, which used to blow up `each`)."
  [val]
  (if (nil? val) @[]
  (if (pvec? val) (pv->array val)
  (if (plist? val) (pl->array val)
  (if (lazy-seq? val)
    (do
      (var items @[]) (var cur val) (var go true)
      (while go
        (let [cell (realize-ls cur)]
          (if (or (nil? cell) (= :jolt/pending cell) (= 0 (length cell)))
            (set go false)
            (do (array/push items (in cell 0))
                (let [rt (in cell 1)]
                  (if (nil? rt) (set go false) (set cur (ls-rest-cached cur rt))))))))
      items)
    val)))))

(defn syntax-quote*
  [ctx bindings form &opt gsmap]
  (default gsmap @{})
  (cond
    (and (array? form) (> (length form) 0) (sym-name? (first form) "unquote"))
    (eval-form ctx bindings (in form 1))
    (and (array? form) (> (length form) 0) (sym-name? (first form) "unquote-splicing"))
    (error "~@ used outside of a list or vector in syntax-quote")
    (or (number? form) (string? form) (keyword? form) (nil? form) (= true form) (= false form))
    form
    (and (struct? form) (= :symbol (form :jolt/type)))
    (sq-symbol ctx form gsmap)
    (tuple? form)
    (do (var result @[]) (var i 0) (while (< i (length form))
      (let [item (in form i)]
        (if (and (array? item) (> (length item) 0) (sym-name? (first item) "unquote-splicing"))
          (let [sv (eval-form ctx bindings (in item 1))]
            (each v (d-realize sv) (array/push result v)))
          (array/push result (syntax-quote* ctx bindings item gsmap))))
      (++ i)) (tuple ;result))
    (array? form)
    (do (var result @[]) (var i 0) (while (< i (length form))
      (let [item (in form i)]
        (if (and (array? item) (> (length item) 0) (sym-name? (first item) "unquote-splicing"))
          (let [sv (eval-form ctx bindings (in item 1))]
            (each v (d-realize sv) (array/push result v)))
          (array/push result (syntax-quote* ctx bindings item gsmap))))
      (++ i)) result)
    # set literal: lower each element (processing ~/~@) and rebuild a set.
    (and (struct? form) (= :jolt/set (form :jolt/type)))
    (do (var result @[])
      (each item (form :value)
        (if (and (array? item) (> (length item) 0) (sym-name? (first item) "unquote-splicing"))
          (let [sv (eval-form ctx bindings (in item 1))]
            (each v (d-realize sv) (array/push result v)))
          (array/push result (syntax-quote* ctx bindings item gsmap))))
      (make-phs ;result))
    (and (struct? form) (get form :jolt/type)) form
    (struct? form)
    (do (var kvs @[])
      (def order (form-kv-order form))
      (if order
        (each x order (array/push kvs (syntax-quote* ctx bindings x gsmap)))
        (each k (keys form)
          (array/push kvs (syntax-quote* ctx bindings k gsmap))
          (array/push kvs (syntax-quote* ctx bindings (get form k) gsmap))))
      # keep carrying source order through nested syntax-quote (jolt-p3c)
      (struct/with-proto (struct :jolt/kv-order (tuple/slice kvs)) ;kvs))
    form))

# Syntax-quote LOWERING: instead of evaluating a `(...) form to a value (what
# syntax-quote* does), produce equivalent CONSTRUCTION CODE so a backtick body is
# plain compilable code (read -> macroexpand -> compile, zero runtime cost).
# Mirrors syntax-quote*/sq-symbol exactly; the canonical algorithm is
# tools.reader's syntax-quote*/expand-list. List forms build via __sqcat (-> array),
# vectors via __sqvec (-> tuple), maps via __sqmap; symbols become (quote resolved);
# ~ leaves the expr in place, ~@ passes the seq straight to __sqcat for splicing.
(defn- sqsym* [nm] {:jolt/type :symbol :ns nil :name nm})

(var syntax-quote-lower nil)

(defn- sq-lower-part [ctx item gsmap]
  (if (and (array? item) (> (length item) 0) (sym-name? (first item) "unquote-splicing"))
    (in item 1)
    @[(sqsym* "__sq1") (syntax-quote-lower ctx item gsmap)]))

(set syntax-quote-lower
  (fn syntax-quote-lower [ctx form &opt gsmap]
    (default gsmap @{})
    (cond
      (and (array? form) (> (length form) 0) (sym-name? (first form) "unquote"))
      (in form 1)
      (and (array? form) (> (length form) 0) (sym-name? (first form) "unquote-splicing"))
      (error "~@ used outside of a list or vector in syntax-quote")
      (or (number? form) (string? form) (keyword? form) (nil? form) (= true form) (= false form))
      form
      (and (struct? form) (= :symbol (form :jolt/type)))
      @[(sqsym* "quote") (sq-symbol ctx form gsmap)]
      (array? form)
      (array/concat @[(sqsym* "__sqcat")] (map (fn [it] (sq-lower-part ctx it gsmap)) form))
      (tuple? form)
      (array/concat @[(sqsym* "__sqvec")] (map (fn [it] (sq-lower-part ctx it gsmap)) form))
      # set literal: lower each element (so ~/~@ are processed) and rebuild a set.
      (and (struct? form) (= :jolt/set (form :jolt/type)))
      (array/concat @[(sqsym* "__sqset")] (map (fn [it] (sq-lower-part ctx it gsmap)) (form :value)))
      # other tagged structs (chars): returned as-is (no recursion)
      (and (struct? form) (get form :jolt/type))
      @[(sqsym* "quote") form]
      (struct? form)
      (do (var parts @[(sqsym* "__sqmap")])
          (def order (form-kv-order form))
          (if order
            (each x order (array/push parts (syntax-quote-lower ctx x gsmap)))
            (each k (keys form)
              (array/push parts (syntax-quote-lower ctx k gsmap))
              (array/push parts (syntax-quote-lower ctx (get form k) gsmap))))
          parts)
      @[(sqsym* "quote") form])))

(defn resolve-var
  [ctx bindings sym-s]
  (let [name (sym-s :name) ns (sym-s :ns)]
    (if (not (nil? ns))
      # Resolve ns aliases (e.g. `p/thrown?` where `p` is a require :as alias) so
      # aliased refs/macros resolve. During compilation the analyzer (interpreted,
      # in jolt.analyzer) rebinds ctx-current-ns to its own ns, so look up the alias
      # against the COMPILE ns (:compile-ns, the user's ns) when set — otherwise an
      # aliased ref like g/foo wouldn't resolve mid-compile. Same ns h-current-ns uses.
      (let [cur-name (or (get (ctx :env) :compile-ns) (ctx-current-ns ctx))
            current-ns (ctx-find-ns ctx cur-name)
            aliased-ns (or (ns-alias-lookup current-ns ns) (ns-import-lookup current-ns ns))
            target-ns (ctx-find-ns ctx (or aliased-ns ns))]
        (ns-find target-ns name))
      (if (get bindings name) nil
        (let [current-ns (ctx-current-ns ctx)
              ns (ctx-find-ns ctx current-ns)
              v (ns-find ns name)]
          (if v v
            (let [core-ns (ctx-find-ns ctx "clojure.core")]
              (ns-find core-ns name))))))))

(defn sym-name-str
  [sym-s]
  (if (sym-s :ns) (string (sym-s :ns) "/" (sym-s :name)) (sym-s :name)))

(defn- ns->relpath
  "Namespace name to its file-relative path (dots->dirs, dashes->_), no extension."
  [ns-name]
  (string/replace-all "." "/" (string/replace-all "-" "_" ns-name)))

(defn- find-ns-file
  "Search the context's source roots (stdlib first, then deps.edn dirs) for the
  namespace's source, trying .clj then .cljc. Returns the path or nil."
  [ctx ns-name]
  (let [rel (ns->relpath ns-name)
        roots (or (get (ctx :env) :source-paths) @["src/jolt"])]
    (var found nil)
    (each root roots
      (each ext [".clj" ".cljc"]
        (when (nil? found)
          (let [p (string root "/" rel ext)]
            (when (os/stat p) (set found p))))))
    found))

(defn- load-ns-source
  "Parse and evaluate every form of a namespace's source in the given context.
  Routes through the loader's eval-toplevel when the api has installed it
  (the :toplevel-eval hook) so REQUIRED namespaces compile like everything
  else — without it they ran interpreted-only: slower, and their fns were
  anonymous closures in stack traces (jolt-2o7.1)."
  [ctx src &opt file]
  (default file "<source>")
  (def toplevel (get (ctx :env) :toplevel-eval))
  # a require runs nested inside an outer file's eval; save/restore the outer
  # checker source so its later forms still convert offsets correctly (jolt-fqy)
  (def checking (or (checker-enabled?) (get (ctx :env) :inline?)))
  (def saved-src (and checking (get (ctx :env) :tc-source)))
  (def saved-file (and checking (get (ctx :env) :tc-file)))
  (when checking
    (track-positions! true)
    (put (ctx :env) :tc-source src)
    (put (ctx :env) :tc-file file))
  (defer (when checking
           (put (ctx :env) :tc-source saved-src)
           (put (ctx :env) :tc-file saved-file))
  (each [f line] (parse-all-positioned src file)
    (try
      (if toplevel (toplevel ctx f) (eval-form ctx @{} f))
      ([err fib]
        # innermost failing form wins; files unwound through form the
        # 'while loading …' chain (mirrors loader/eval-forms-positioned,
        # which this can't import — circularity) (jolt-2o7.4)
        (def env (ctx :env))
        (when (nil? (get env :error-pos))
          (put env :error-pos {:file file :line line}))
        (when (nil? (get env :error-loading)) (put env :error-loading @[]))
        (def chain (get env :error-loading))
        (when (not= (last chain) file) (array/push chain file))
        (propagate err fib))))))

# jolt-87e: is a namespace loaded from `path` part of the APP (vs a dependency)?
# True when its file sits under one of the declared app source roots
# (:app-source-paths, from JOLT_APP_PATHS / jolt-deps). When NO app roots are
# declared (a bare program run, or jolt invoked without jolt-deps), everything
# counts as app so whole-program covers the whole program exactly as before.
# Only app namespaces defer into the one whole-program fixpoint; dependency
# namespaces infer per-ns at load, so a dep-heavy app's startup doesn't re-infer
# hundreds of transitive dependency namespaces in a single closed-world pass.
(defn- app-source-ns?
  [ctx path]
  (def roots (get (ctx :env) :app-source-paths))
  (if (or (nil? roots) (empty? roots))
    true
    (and path (truthy? (some |(string/has-prefix? $ path) roots)))))

(defn maybe-require-ns
  "If namespace ns-name isn't populated yet, load its source — from a file on the
  context's source roots, else from the stdlib baked into the image. Restores the
  current namespace afterwards (a library's own `ns` form, or our manual switch
  for ns-form-less stdlib files, changes it). No-op for already-loaded namespaces."
  [ctx ns-name]
  (let [ns (ctx-find-ns ctx ns-name)]
    (when (and (= 0 (length (ns :mappings)))
               (not (get (get (ctx :env) :loaded-namespaces @{}) ns-name))
               (not= ns-name "clojure.core"))
      (let [path (find-ns-file ctx ns-name)
            embedded (get (get (ctx :env) :embedded-sources @{}) ns-name)
            stdlib? (not (nil? embedded))]
        # Clojure throws FileNotFoundException here; succeeding silently leaves
        # an empty namespace behind and defers the failure to the first
        # unresolved symbol, far from the actual cause (a typo, a missing
        # JOLT_PATH root). Best-effort loaders (the SCI bootstrap, which loads
        # clj-targeted sources whose requires can't all exist on this host)
        # opt out via :lenient-require? on the env.
        (when (and (nil? path) (nil? embedded)
                   (not (get (ctx :env) :lenient-require?)))
          (error (string "Could not locate " ns-name
                         " on the context's source paths (JOLT_PATH / :paths)")))
        (when (or path embedded)
          (let [saved (ctx-current-ns ctx)
                # jolt-87e: is this an app namespace, or a dependency/library? Only
                # the app is the closed world the whole-program optimizer reasons
                # over; dependencies are open-world libraries.
                app? (app-source-ns? ctx path)
                # Whole-program optimize is active for this load.
                wp-active? (and (get (ctx :env) :inline?)
                                (get (ctx :env) :whole-program?)
                                (not (get (ctx :env) :infer-program-done?)))
                # A dependency under whole-program optimize compiles at DEFAULT
                # cost: :inline? off for its load, so the per-form inline +
                # inference passes — the bulk of optimize-mode startup — don't run
                # over hundreds of library forms. (Direct-linking + shape-recs stay
                # on, exactly like a non-optimized direct-link build.) This is what
                # makes JOLT_OPTIMIZE viable on dep-heavy apps; the app's own nses
                # below keep full optimization. (jolt-87e)
                dep-cheap? (and wp-active? (not app?))
                saved-inline (get (ctx :env) :inline?)]
            # Stdlib files have no `ns` form, so switch into the target ns first
            # (their defs intern there); a library's own `ns` form overrides this.
            (ctx-set-current-ns ctx ns-name)
            (when dep-cheap? (put (ctx :env) :inline? false))
            (if path
              (load-ns-source ctx (slurp path) path)
              (load-ns-source ctx embedded (string ns-name " (stdlib)")))
            (when dep-cheap? (put (ctx :env) :inline? saved-inline))
            # Inter-procedural collection-type inference (jolt-767): once the whole
            # unit is loaded, run the closed-world fixpoint + recompile so param-
            # dependent lookups specialize. Only in optimization mode; best-effort
            # (a failure here must not break loading). Hook installed by the api to
            # avoid an evaluator->backend circular import.
            (when (get (ctx :env) :inline?)
              (cond
                # whole-program (jolt-t34), APP namespace: defer — record the ns and
                # run ONE fixpoint over all app units later (the closed-world pass
                # sees every caller, so cross-ns param types propagate).
                (and wp-active? app?)
                (let [lst (or (get (ctx :env) :inferred-nses)
                              (let [a @[]] (put (ctx :env) :inferred-nses a) a))]
                  (array/push lst ns-name))
                # whole-program, DEPENDENCY namespace (jolt-87e): nothing to do —
                # it compiled cheaply above (no inference to run or defer).
                wp-active?
                nil
                # per-ns mode (whole-program off), or a lazy require AFTER the batch
                # ran: infer this unit on its own.
                (when-let [iu (get (ctx :env) :infer-unit!)]
                  (protect (iu ctx ns-name)))))
            # Record load order for tooling (uberscript): a dependency finishes
            # loading before its requirer, so this is topological. Skip the
            # baked-in stdlib — it's part of the runtime, not something to bundle.
            (when (and path (not stdlib?))
              (when-let [lf (get (ctx :env) :loaded-files)] (array/push lf path)))
            (ctx-set-current-ns ctx saved)))))))

(defn eval-require
  [ctx spec]
  (let [ns-sym (in spec 0)
        ns-name (sym-name-str ns-sym)]
    (var alias nil)
    (var refer-syms nil)
    (var i 1)
    (let [slen (length spec)]
      # Scan ALL options — a spec may carry both :as and :refer, e.g.
      # [clojure.string :as str :refer [blank?]]; don't stop at the first.
      (while (< i slen)
        (let [item (in spec i)]
          (cond
            (or (= item :as) (and (struct? item) (= :symbol (item :jolt/type)) (= "as" (item :name))))
              (do (set alias ((in spec (+ i 1)) :name)) (+= i 2))
            (or (= item :refer) (and (struct? item) (= :symbol (item :jolt/type)) (= "refer" (item :name))))
              (do (set refer-syms (in spec (+ i 1))) (+= i 2))
            (++ i)))))
    (maybe-require-ns ctx ns-name)
    (when alias
      (let [current-ns (ctx-find-ns ctx (ctx-current-ns ctx))]
        (ns-add-alias current-ns alias ns-name)))
    (when refer-syms
      (let [source-ns (ctx-find-ns ctx ns-name)
            target-ns (ctx-find-ns ctx (ctx-current-ns ctx))]
        (if (or (= refer-syms :all)
                (and (struct? refer-syms) (= :symbol (refer-syms :jolt/type))
                     (= "all" (refer-syms :name))))
          # :refer :all — share EVERY var (this used to each over the :all
          # keyword itself and silently refer nothing; selmer's
          # [selmer.util :refer :all] left *tag-open* & co unresolved)
          (eachp [nm v] (source-ns :mappings)
            (put (target-ns :mappings) nm v))
          (each refer-sym refer-syms
            (let [name (if (struct? refer-sym) (refer-sym :name) refer-sym)
                  v (ns-find source-ns name)]
              (when v
                # Share the SOURCE var (the Clojure model): macro-ness travels with
                # it and source-ns redefinitions propagate to the referer.
                (put (target-ns :mappings) name v)))))))
    nil))

(defn bind-put
  "Put a value into bindings. Uses :jolt/nil sentinel for nil values
  because Janet's (put table key nil) silently drops the key."
  [bindings key value]
  (put bindings key (if (nil? value) :jolt/nil value)))

(defn binding-get
  "Get a value from bindings, walking the prototype chain."
  [bindings name]
  (var result :jolt/not-found)
  (var t bindings)
  (while (not (nil? t))
    (when (in t name)
      (set result (in t name))
      (break))
    (set t (table/getproto t)))
  result)

# Pluggable host-class shims (java.time etc. register here at module load):
#   class-statics: "ClassName" -> {"member" value-or-fn}   (Foo/bar resolution)
#   tagged-methods: :jolt/tag -> {"method" (fn [self args...])}   ((.m obj) dispatch)
(def class-statics @{})
(def tagged-methods @{})
(defn register-class-statics! [class-name tbl] (put class-statics class-name tbl))
(defn register-tagged-methods! [tag tbl] (put tagged-methods tag tbl))
# Constructor shims: (ClassName. args) resolves ClassName as a value, so the
# ctor fns are interned as clojure.core vars at init (install-stateful-fns!).
(def class-ctors @{})
(defn register-class-ctor! [nm f] (put class-ctors nm f))

# java.util.Iterator shim: (.iterator coll) gives a jolt iterator over any
# seqable, with (.hasNext it) / (.next it). Some Clojure libs (e.g. hiccup's
# iterate!) loop with the Java Iterator protocol; this makes that work over jolt
# collections. The realizer (core/realize-for-iteration, which handles every
# collection type) is late-bound because core loads after this file.
(var coll-realizer nil)
(defn set-coll-realizer! [f] (set coll-realizer f))
# Late-bound (wired in api): routes a Java collection-interop method call
# (.nth/.count/.valAt/.seq …) on a jolt persistent collection to the clojure.core
# equivalent. Returns :jolt/ci-none when it doesn't apply. Lets clj-targeted libs
# (malli) that use .nth/.count on vectors/maps in their :clj branches work.
(var coll-interop nil)
(defn set-coll-interop! [f] (set coll-interop f))
