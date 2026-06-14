# Jolt Evaluator
# Direct interpreter for Clojure forms on Janet.

(use ./types)
(use ./phm)
(use ./pv)
(use ./plist)
(use ./config)
(use ./reader)
(use ./regex)

# The env this module was loaded under — proto-chains to the Janet root env;
# the janet/* interop bridge falls back to it inside env-less fibers.
(def- module-load-env (fiber/getenv (fiber/current)))

# jpm-module autoload: a janet.<module>/<name> reference whose module isn't
# in the env is satisfied by requiring it from the jpm module path on first
# use — (janet.spork.http/server ...) just works when spork is installed,
# and the same goes for any jpm module. Loaded bindings are cached here
# (and failures negatively cached, so a missing module errors fast).
(def- janet-bridge-extras @{})
(def- janet-bridge-failed @{})
(defn- bridge-autoload
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

(defn- sym-name?
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

(defn- form-uses-sym? [form nm]
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

(defn- coll-lookup
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

(defn- d-realize
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

(defn- syntax-quote*
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

(defn- sym-name-str
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

(defn- maybe-require-ns
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
          (let [saved (ctx-current-ns ctx)]
            # Stdlib files have no `ns` form, so switch into the target ns first
            # (their defs intern there); a library's own `ns` form overrides this.
            (ctx-set-current-ns ctx ns-name)
            (if path
              (load-ns-source ctx (slurp path) path)
              (load-ns-source ctx embedded (string ns-name " (stdlib)")))
            # Inter-procedural collection-type inference (jolt-767): once the whole
            # unit is loaded, run the closed-world fixpoint + recompile so param-
            # dependent lookups specialize. Only in optimization mode; best-effort
            # (a failure here must not break loading). Hook installed by the api to
            # avoid an evaluator->backend circular import.
            (when (get (ctx :env) :inline?)
              (if (get (ctx :env) :whole-program?)
                # whole-program (jolt-t34): defer — record the ns and run ONE
                # fixpoint over all units later (the closed-world pass sees every
                # caller, so cross-ns param types propagate)
                (let [lst (or (get (ctx :env) :inferred-nses)
                              (let [a @[]] (put (ctx :env) :inferred-nses a) a))]
                  (array/push lst ns-name))
                (when-let [iu (get (ctx :env) :infer-unit!)]
                  (protect (iu ctx ns-name)))))
            # Record load order for tooling (uberscript): a dependency finishes
            # loading before its requirer, so this is topological. Skip the
            # baked-in stdlib — it's part of the runtime, not something to bundle.
            (when (and path (not stdlib?))
              (when-let [lf (get (ctx :env) :loaded-files)] (array/push lf path)))
            (ctx-set-current-ns ctx saved)))))))

(defn- eval-require
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

(defn- bind-put
  "Put a value into bindings. Uses :jolt/nil sentinel for nil values
  because Janet's (put table key nil) silently drops the key."
  [bindings key value]
  (put bindings key (if (nil? value) :jolt/nil value)))

(defn- binding-get
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

(def- math-statics
  @{"sqrt" math/sqrt "pow" math/pow "floor" math/floor "ceil" math/ceil
    "abs" (fn [x] (if (< x 0) (- x) x))
    "round" (fn [x] (math/round x))
    "sin" math/sin "cos" math/cos "tan" math/tan
    "asin" math/asin "acos" math/acos "atan" math/atan
    "log" math/log "log10" math/log10 "exp" math/exp
    "max" (fn [a b] (if (> a b) a b)) "min" (fn [a b] (if (< a b) a b))
    "signum" (fn [x] (cond (< x 0) -1.0 (> x 0) 1.0 0.0))
    "PI" math/pi "E" math/e
    "random" (fn [&] (math/random))})

# Thread statics (the JVM shapes portable code actually uses). sleep parks the
# CURRENT thread's event loop — inside a future body that's the worker OS
# thread (ev/spawn-thread gives each worker its own loop), so a sleeping
# future doesn't block the parent.
(def- thread-statics
  {"sleep" (fn [ms] (ev/sleep (/ ms 1000)) nil)
   "yield" (fn [] (ev/sleep 0) nil)
   "interrupted" (fn [] false)
   "currentThread" (fn [] @{:jolt/type :jolt/thread :id "main"})})

# System statics (wall/monotonic clocks — what portable timing code uses).
(def- system-statics
  # realtime clock (sub-ms float epoch seconds) — os/time is whole seconds,
  # which quantized every elapsed-time measurement to 1000ms.
  {"currentTimeMillis" (fn [] (math/floor (* 1000 (os/clock :realtime))))
   "nanoTime" (fn [] (math/floor (* 1e9 (os/clock :monotonic))))
   "getProperty" (fn [k &opt dflt]
                   (case k
                     "os.name" (case (os/which)
                                 :windows "Windows" :macos "Mac OS X" "Linux")
                     "line.separator" "\n"
                     "file.separator" "/"
                     "user.dir" (os/cwd)
                     "user.home" (os/getenv "HOME")
                     "java.io.tmpdir" (or (os/getenv "TMPDIR") "/tmp")
                     dflt))
   # JOLT_BAKE_ENV_ALLOWLIST (jolt-s3j): during an image bake (jpm build of a
   # native executable, set by the project's build.sh) the env snapshot that
   # libraries like config.core capture at load gets MARSHALED INTO THE BINARY
   # — GitHub push protection once flagged real API tokens inside an example's
   # build output. With the var set, System/getenv serves only the listed
   # comma-separated names (single-var reads of unlisted names return nil), so
   # nothing secret can bake. Unset (the normal runtime case), reads are live
   # and unfiltered.
   "getenv" (fn [&opt k]
              (def allow (os/getenv "JOLT_BAKE_ENV_ALLOWLIST"))
              (if (nil? allow)
                (if k (os/getenv k) (os/environ))
                (let [names (string/split "," allow)
                      ok @{}]
                  (each n names (put ok (string/trim n) true))
                  (if k
                    (when (get ok k) (os/getenv k))
                    (let [e (os/environ) out @{}]
                      (eachp [ek ev] e (when (get ok ek) (put out ek ev)))
                      out)))))
   # the property subset getProperty serves, as an iterable map
   "getProperties" (fn []
                     {"os.name" (case (os/which)
                                  :windows "Windows" :macos "Mac OS X" "Linux")
                      "line.separator" "\n"
                      "file.separator" "/"
                      "user.dir" (os/cwd)
                      "user.home" (or (os/getenv "HOME") "")
                      "java.io.tmpdir" (or (os/getenv "TMPDIR") "/tmp")})})

# Long statics: sentinels portable code compares against. jolt numbers are
# doubles, so these are the f64 approximations.
(def- long-statics
  {"MAX_VALUE" 9223372036854775807
   "MIN_VALUE" -9223372036854775808
   "parseLong" (fn [s &opt radix]
                 (def n (scan-number (string/trim (string s)) (or radix 10)))
                 (if (and n (= n (math/floor n)))
                   n
                   (error (string "NumberFormatException: For input string: \"" s "\""))))
   "valueOf" (fn [s &opt radix]
               (def n (scan-number (string/trim (string s)) (or radix 10)))
               (if (and n (= n (math/floor n)))
                 n
                 (error (string "NumberFormatException: For input string: \"" s "\""))))})

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
# Class names evaluate to their CANONICAL NAME STRING — the same value
# core-class returns — so (defmethod m String ...) keys match a
# (defmulti m (comp class :body)) dispatch (ring.util.request does this).
# `new` resolves the actual constructor from class-ctors by short name.
(def- class-canonical-names
  @{"String" "java.lang.String" "Number" "java.lang.Number"
    "Boolean" "java.lang.Boolean" "Long" "java.lang.Long"
    "Integer" "java.lang.Integer" "Double" "java.lang.Double"
    "InputStream" "java.io.InputStream" "OutputStream" "java.io.OutputStream"
    "File" "java.io.File" "Reader" "java.io.Reader" "Writer" "java.io.Writer"
    "ISeq" "clojure.lang.ISeq" "Keyword" "clojure.lang.Keyword"
    "Symbol" "clojure.lang.Symbol" "MapEntry" "clojure.lang.MapEntry"
    "StringReader" "java.io.StringReader" "StringWriter" "java.io.StringWriter"
    "StringBuilder" "java.lang.StringBuilder"
    "StringTokenizer" "java.util.StringTokenizer"
    "Charset" "java.nio.charset.Charset" "Base64" "java.util.Base64"
    "Exception" "java.lang.Exception"
    "IllegalArgumentException" "java.lang.IllegalArgumentException"
    "InterruptedException" "java.lang.InterruptedException"
    "Throwable" "java.lang.Throwable"})
(defn- class-value-for
  "The value a class-name symbol evaluates to: its canonical name string."
  [nm]
  (or (get class-canonical-names nm)
      # qualified already, or unknown: the name itself is the token
      nm))
(defn- ctor-for-class-token
  "Constructor fn for a class token (a canonical-name string): try the full
  name, then the short name after the last dot."
  [tok]
  (or (in class-ctors tok)
      (let [parts (string/split "." tok)]
        (in class-ctors (last parts)))))

# java.lang.String method surface for clj-compat interop: (.toLowerCase s),
# (.indexOf s x), ... — the methods portable cljc libraries actually call.
# Case mapping is ASCII (the whole engine is byte-based); indexOf returns -1
# on miss, as on the JVM.
(defn- str-needle [x]
  (cond
    (and (struct? x) (= :jolt/char (get x :jolt/type))) (string/from-bytes (x :ch))
    # (.indexOf s 61): an int needle is a char CODE on the JVM, not its decimal
    # text (ring-codec splits k=v pairs this way)
    (number? x) (string/from-bytes (math/trunc x))
    (string x)))
# java.lang.Number surface (ring-codec: (.byteValue (Integer/valueOf s 16))).
(def- number-methods
  {"byteValue"   (fn [n] (let [b (band (math/trunc n) 0xff)] (if (> b 127) (- b 256) b)))
   "shortValue"  (fn [n] (let [v (band (math/trunc n) 0xffff)] (if (> v 32767) (- v 65536) v)))
   "intValue"    (fn [n] (math/trunc n))
   "longValue"   (fn [n] (math/trunc n))
   "floatValue"  (fn [n] (* 1.0 n))
   "doubleValue" (fn [n] (* 1.0 n))
   "toString"    (fn [n &opt radix] (if (= radix 16) (string/format "%x" (math/trunc n)) (string n)))})

# Universal java.lang.Object / exception / persistent-collection methods that
# reitit's :clj branches call on non-string targets: (.getMessage e),
# (.assoc m k v), (.get m k). Consulted in the method-dispatch fallthrough.
(def- object-methods
  {"getMessage"  (fn [e] (cond (and (table? e) (= :jolt/ex-info (get e :jolt/type))) (get e :message)
                               (string? e) e
                               (string e)))
   "getCause"    (fn [e] (and (table? e) (get e :cause)))
   "toString"    (fn [x] (string x))
   "equals"      (fn [a b] (deep= a b))
   "hashCode"    (fn [x] (hash x))})

(def- string-methods
  {"getBytes"    (fn [s &opt charset] (buffer s))
   "toString"    (fn [s] s)
   "toLowerCase" (fn [s] (string/ascii-lower s))
   "toUpperCase" (fn [s] (string/ascii-upper s))
   "trim"        (fn [s] (string/trim s))
   "intern"      (fn [s] s)
   # file-path surface: io/file returns plain path strings, so the java.io.File
   # / java.net.URL methods selmer's template cache calls land here
   "toURI"       (fn [s] s)
   "toURL"       (fn [s] s)
   "getPath"     (fn [s] s)
   "getName"     (fn [s] (if-let [i (string/find "/" (string/reverse s))]
                           (string/slice s (- (length s) i)) s))
   "exists"      (fn [s] (not (nil? (os/stat s))))
   "lastModified" (fn [s] (if-let [st (os/stat s)] (math/floor (* 1000 (st :modified))) 0))
   # JVM String.split takes a REGEX string; trailing empties dropped like the JVM
   "split"       (fn [s re &opt limit]
                   (def parts (re-split (re-pattern re) s))
                   (while (and (> (length parts) 0) (= "" (last parts)))
                     (array/pop parts))
                   parts)
   "length"      (fn [s] (length s))
   "isEmpty"     (fn [s] (= 0 (length s)))
   "charAt"      (fn [s i] {:jolt/type :jolt/char :ch (s i)})
   "codePointAt" (fn [s i] (s i))
   "indexOf"     (fn [s x &opt from] (or (string/find (str-needle x) s (or from 0)) -1))
   "lastIndexOf" (fn [s x]
                   (let [n (str-needle x)]
                     (var found -1) (var i 0)
                     (while (< i (length s))
                       (let [f (string/find n s i)]
                         (if f (do (set found f) (set i (+ f 1))) (set i (length s)))))
                     found))
   "substring"   (fn [s start &opt end] (string/slice s start end))
   "startsWith"  (fn [s p] (string/has-prefix? p s))
   "endsWith"    (fn [s p] (string/has-suffix? p s))
   "contains"    (fn [s sub] (not (nil? (string/find (str-needle sub) s))))
   "concat"      (fn [s o] (string s o))
    "replace"     (fn [s a b] (string/replace-all (str-needle a) (str-needle b) s))
    "replaceAll"  (fn [s regex replacement] (re-replace-all (re-pattern regex) s replacement))
    "replaceFirst" (fn [s regex replacement] (re-replace-first (re-pattern regex) s replacement))
    "matches"     (fn [s regex] (not (nil? (re-matches (re-pattern regex) s))))
   "compareTo"   (fn [s o] (cond (< s o) -1 (> s o) 1 0))
   "equalsIgnoreCase" (fn [s o] (= (string/ascii-lower s) (string/ascii-lower (string o))))})

(defn- resolve-sym
  [ctx bindings sym-s]
  (let [name (sym-s :name) ns (sym-s :ns)]
    (if (= ns "Math")
      (let [v (get math-statics name)]
        (if (nil? v) (error (string "Unsupported Math member: Math/" name)) v))
    (if (= ns "Thread")
      (let [v (get thread-statics name)]
        (if (nil? v) (error (string "Unsupported Thread member: Thread/" name)) v))
    (if (= ns "System")
      (let [v (get system-statics name)]
        (if (nil? v) (error (string "Unsupported System member: System/" name)) v))
    (if (= ns "Long")
      (let [v (get long-statics name)]
        (if (nil? v) (error (string "Unsupported Long member: Long/" name)) v))
    (if (get class-statics ns)
      (let [v (get (get class-statics ns) name)]
        (if (nil? v) (error (string "Unsupported member: " ns "/" name)) v))
    (if (not (nil? ns))
      (let [current-ns (ctx-find-ns ctx (ctx-current-ns ctx))
            aliased-ns (or (ns-alias-lookup current-ns ns) (ns-import-lookup current-ns ns))
            target-ns (ctx-find-ns ctx (or aliased-ns ns))
            v (and target-ns (ns-find target-ns name))]
        (if v (var-get v)
          # Explicit Janet interop. The `janet` namespace segment marks every
          # crossing into host code, where Clojure semantics no longer hold:
          #   janet/<name>          -> Janet root binding   (janet/slurp, janet/type)
          #   janet.<module>/<name> -> Janet module binding (janet.net/server,
          #                                                   janet.os/clock)
          # This makes the whole Janet stdlib reachable from Clojure while keeping
          # the interop boundary visible at the call site.
          (if (or (= ns "janet") (string/has-prefix? "janet." ns))
            (let [jname (if (= ns "janet") name (string (string/slice ns 6) "/" name))
                  # worker fibers may carry no env (fiber/new without :e inherit)
                  # — fall back to the env captured at module load
                  # four-step resolution: the runtime fiber's env (when it
                  # has one), the evaluator's module env (worker/connection
                  # fibers carry a foreign or empty env — net/server handler
                  # fibers resolve janet/struct through here), the autoload
                  # cache, then a jpm-module require on first miss
                  entry (or (when-let [fe (fiber/getenv (fiber/current))]
                              (in fe (symbol jname)))
                            (in module-load-env (symbol jname))
                            (in janet-bridge-extras jname)
                            (bridge-autoload jname))]
              (if (not (nil? entry))
                (if (table? entry) (entry :value) entry)
                (error (string "Unable to resolve Janet symbol: " jname))))
            # syntax-quote ns-qualifies bare class names inside macros
            # (selmer.util/StringBuilder); class names never belong to an ns —
            # fall back to the constructor / statics shims before giving up.
            (if (or (in class-ctors name) (get class-canonical-names name))
              (class-value-for name)
              (error (string "Unable to resolve symbol: " ns "/" name))))))
      # Use :jolt/not-found sentinel to distinguish nil binding from absent binding
      (let [local (get bindings name :jolt/not-found-1)
            local (if (= local :jolt/not-found-1) (binding-get bindings name) local)]
        (if (not= local :jolt/not-found)
          (if (= local :jolt/nil) nil local)
          (let [current-ns (ctx-current-ns ctx) ns (ctx-find-ns ctx current-ns) v (ns-find ns name)]
            (if v (var-get v)
              # Check clojure.core as auto-referred fallback
              (let [core-ns (ctx-find-ns ctx "clojure.core")
                    core-v (ns-find core-ns name)]
                (if core-v
                  (var-get core-v)
                  # Try class-name resolution: Foo.Bar.Baz -> ns "Foo.Bar", name "Baz"
                  (let [dot-idx (string/find "." name)]
                    (if dot-idx
                      (let [last-dot (do
                                       (var idx dot-idx)
                                       (var next-dot (string/find "." name (+ idx 1)))
                                       (while (not (nil? next-dot))
                                         (set idx next-dot)
                                         (set next-dot (string/find "." name (+ idx 1))))
                                       idx)
                            class-ns (string/slice name 0 last-dot)
                            class-name (string/slice name (+ last-dot 1))]
                        (let [target-ns (ctx-find-ns ctx class-ns) tv (ns-find target-ns class-name)]
                          (if tv (var-get tv) tv)))
                      # No implicit Janet fallback (Stage 3): an unresolved
                      # Clojure symbol is an error. Host access is the explicit
                      # janet/ prefix above.
                      (if (or (in class-ctors name) (get class-canonical-names name))
                        (class-value-for name)
                        (error (string "Unable to resolve symbol: " name " in this context")))))))))))))))))))
(defn- parse-arg-names
  "Parse a parameter vector, handling & rest args.
  Returns {:fixed [names...] :rest name-or-nil :all [names...]}"
  [args-form]
  (var fixed @[])
  (var rest-name nil)
  (var i 0)
  (while (< i (length args-form))
    (let [a (in args-form i)]
      (if (and (struct? a) (= :symbol (a :jolt/type)) (= "&" (a :name)))
        (do
          (+= i 1)
          (if (< i (length args-form))
            (do
              (set rest-name ((in args-form i) :name))
              (+= i 1))
            (error "& without argument in parameter list")))
        (do
          (if (and (struct? a) (= :symbol (a :jolt/type)))
            (array/push fixed (a :name))
            # destructuring form: recurse into it
            (when (indexed? a)
              (var di 0)
              (while (< di (length a))
                (def inner (in a di))
                (if (and (struct? inner) (= :symbol (inner :jolt/type)) (= "&" (inner :name)))
                  (do
                    (+= di 1)
                    (if (< di (length a))
                      (do
                        (set rest-name ((in a di) :name))
                        (+= di 1))
                      (error "& without argument in parameter list")))
                  (do
                    (if (and (struct? inner) (= :symbol (inner :jolt/type)))
                      (array/push fixed (inner :name))
                      # nested destructuring - extract names
                      (when (indexed? inner)
                        (each sym inner
                          (when (and (struct? sym) (= :symbol (sym :jolt/type)))
                            (array/push fixed (sym :name))))))
                    (+= di 1))))))
          (+= i 1)))))
  (var all @[])
  (each n fixed (array/push all n))
  (if rest-name (array/push all rest-name))
  {:fixed (tuple/slice (tuple ;fixed)) :rest rest-name :all (tuple/slice (tuple ;all))})

# ============================================================
# Destructuring (Clojure-compatible, recursive)
# ============================================================

(defn- parse-params
  "Parse a parameter vector into raw patterns: {:fixed [pat...] :rest pat-or-nil}.
  Unlike parse-arg-names, patterns are kept intact (not flattened) so they can
  be destructured against the corresponding argument."
  [args-form]
  (var fixed @[])
  (var rest-pat nil)
  (var i 0)
  (while (< i (length args-form))
    (let [a (in args-form i)]
      (if (and (struct? a) (= :symbol (a :jolt/type)) (= "&" (a :name)))
        (do (+= i 1)
            (when (< i (length args-form)) (set rest-pat (in args-form i)))
            (+= i 1))
        (do (array/push fixed a) (+= i 1)))))
  {:fixed (tuple/slice (tuple ;fixed)) :rest rest-pat})

(defn- rest-args-val
  "What a rest param binds to: nil when no args remain (Clojure semantics —
  (fn [& r]) called with nothing gives r = nil, never an empty seq)."
  [args i]
  (when (> (length args) i) (tuple/slice args i)))

(defn- plain-sym? [p] (and (struct? p) (= :symbol (p :jolt/type))))

(defn- require-symbol-params
  "fn* is a primitive: its params must be plain symbols. The fn/defn MACROS desugar
  destructuring into plain params + a body let before emitting fn*, so fn* never
  legitimately sees a pattern — matching Clojure, where (fn* [[a b]] ...) is the
  compile error 'fn params must be Symbols'. Enforcing it here keeps the interpreter
  consistent with the self-hosted analyzer (which also requires plain fn* params)
  and with Clojure, instead of leniently destructuring a form Clojure rejects."
  [param-info]
  (each p (param-info :fixed)
    (unless (plain-sym? p) (error "fn params must be Symbols")))
  (let [r (param-info :rest)]
    (when (and r (not (plain-sym? r))) (error "fn params must be Symbols"))))

(defn- d-get
  "Look up key k in a map-like value (phm/struct/table/nil)."
  [m k]
  (cond
    (phm? m) (phm-get m k)
    (or (struct? m) (table? m)) (get m k)
    true nil))

(defn- find-or-default
  "Find the :or default expression for binding name nm, or :jolt/none."
  [or-map nm]
  (var result :jolt/none)
  (when or-map
    (each k (keys or-map)
      (when (and (struct? k) (= :symbol (k :jolt/type)) (= nm (k :name)))
        (set result (get or-map k)))))
  result)

(var destructure-bind nil)
(set destructure-bind
  (fn dbind [ctx bindings pat val]
    (cond
      # plain symbol
      (and (struct? pat) (= :symbol (pat :jolt/type)))
        (bind-put bindings (pat :name) val)
      # sequential pattern (vector of sub-patterns)
      (indexed? pat)
        (let [rv (d-realize val)
              seqable? (indexed? rv)]
          (var di 0) (var vi 0)
          (def n (length pat))
          (while (< di n)
            (let [elem (in pat di)]
              (cond
                 # & rest
                 (and (struct? elem) (= :symbol (elem :jolt/type)) (= "&" (elem :name)))
                   (do
                     # rest binds a seq (jolt list = array), per Clojure semantics.
                     # For lazy-seqs, preserve laziness: walk vi steps via ls-rest
                     # instead of slicing the eagerly-realized array.
                     (destructure-bind ctx bindings (in pat (+ di 1))
                       (if (lazy-seq? val)
                         (do
                           (var c val) (var i 0)
                           (while (< i vi)
                             (let [nxt (ls-rest c)]
                               (if (nil? nxt) (break)
                                 (do (set c nxt) (++ i)))))
                           c)
                         (if (and seqable? (< vi (length rv)))
                           (array/slice (if (tuple? rv) (array/slice rv) rv) vi)
                           @[])))
                    (set di (+ di 2)))
                # :as whole
                (= elem :as)
                  (do
                    (destructure-bind ctx bindings (in pat (+ di 1)) val)
                    (set di (+ di 2)))
                # positional element
                true
                  (do
                    (destructure-bind ctx bindings elem
                      (if (and seqable? (< vi (length rv))) (in rv vi) nil))
                    (+= di 1) (+= vi 1))))))
      # map pattern (struct/table that isn't a symbol)
      (or (struct? pat) (table? pat))
        (let [rv (d-realize val)
              # Destructuring a sequential value as a map treats it as kwargs:
              # alternating k/v pairs, or a single trailing map (Clojure's
              # `[& {:keys ...}]`). A real map value is used as-is.
              mval (if (and (indexed? rv) (not (or (struct? rv) (table? rv))))
                     (if (and (= 1 (length rv))
                              (let [e (in rv 0)] (or (struct? e) (table? e) (phm? e))))
                       (in rv 0)
                       (let [m @{}]
                         (var i 0)
                         (while (< (+ i 1) (length rv))
                           (put m (in rv i) (in rv (+ i 1)))
                           (+= i 2))
                         m))
                     val)]
          (def or-map (get pat :or))
          (def as-sym (get pat :as))
          (when as-sym (destructure-bind ctx bindings as-sym mval))
          # :keys (keyword), :strs (string), :syms (symbol). A namespaced symbol
          # in :keys/:syms (x/y) looks up the namespaced key but binds local y.
          (each spec [[:keys :kw] [:strs :str] [:syms :sym]]
            (let [kw (in spec 0) kind (in spec 1) names (get pat kw)]
              (when (and names (indexed? names))
                (each s names
                  (let [sym? (and (struct? s) (= :symbol (s :jolt/type)))
                        local (if sym? (s :name) (string s))
                        nsp (and sym? (s :ns))
                        key (case kind
                              :kw (keyword (if nsp (string nsp "/" local) local))
                              :str local
                              :sym {:jolt/type :symbol :ns nsp :name local})
                        v (d-get mval key)
                        v (if (nil? v)
                            (let [d (find-or-default or-map local)]
                              (if (= d :jolt/none) nil (eval-form ctx bindings d)))
                            v)]
                    (bind-put bindings local v))))))
          # direct {local-pattern key-expr} entries (local may itself be a
          # nested vector/map pattern). Special keys are keywords; skip them.
          (each k (keys pat)
            (when (not (keyword? k))
              (let [key-val (eval-form ctx bindings (get pat k))
                    v (d-get mval key-val)]
                (if (and (struct? k) (= :symbol (k :jolt/type)))
                  # symbol target: apply :or default if missing
                  (let [nm (k :name)
                        v (if (nil? v)
                            (let [d (find-or-default or-map nm)]
                              (if (= d :jolt/none) nil (eval-form ctx bindings d)))
                            v)]
                    (bind-put bindings nm v))
                  # nested pattern target
                  (destructure-bind ctx bindings k v))))))
      true (error (string "Unsupported destructuring pattern: " (string/format "%q" pat))))))

# ---- host-type protocol extension (extend-protocol String/Number/... ) ----
(def- host-type-names
  {"Long" true "Integer" true "Short" true "Byte" true "BigInteger" true "BigInt" true
   "Double" true "Float" true "Number" true "BigDecimal" true "Ratio" true
   "String" true "CharSequence" true "Boolean" true "Character" true
   "Keyword" true "Symbol" true "Object" true "IFn" true "Fn" true
   "PersistentVector" true "PersistentList" true "PersistentHashMap" true
   "PersistentHashSet" true "IPersistentMap" true "IPersistentVector" true
   "IPersistentSet" true "IPersistentCollection" true "ISeq" true "Atom" true "nil" true
   # java.util interfaces + seq types ring & friends extend on
   "Map" true "Set" true "List" true "Collection" true "LazySeq" true
   "APersistentMap" true})

(defn- canonical-host-tag
  "If type-name names a host type (optionally java.*/clojure.lang.* qualified),
  return its bare canonical name; else nil (it's a deftype/record name)."
  [type-name]
  (let [base (cond
               (string/has-prefix? "java.lang." type-name) (string/slice type-name 10)
               (string/has-prefix? "java.util." type-name) (string/slice type-name 10)
               (string/has-prefix? "clojure.lang." type-name) (string/slice type-name 13)
               type-name)]
    (if (get host-type-names base) base nil)))

(defn- value-host-tags
  "Candidate host type-tags for a runtime value, most-specific first."
  [obj]
  (cond
    (number? obj) ["Long" "Integer" "Number" "Double" "Object"]
    (string? obj) ["String" "CharSequence" "Object"]
    (or (= true obj) (= false obj)) ["Boolean" "Object"]
    (keyword? obj) ["Keyword" "Object"]
    (and (struct? obj) (= :jolt/char (get obj :jolt/type))) ["Character" "Object"]
    (and (struct? obj) (= :symbol (get obj :jolt/type))) ["Symbol" "Object"]
    (plist? obj) ["PersistentList" "IPersistentList" "IPersistentCollection" "ISeq" "List" "Collection" "Object"]
    (lazy-seq? obj) ["LazySeq" "ISeq" "IPersistentCollection" "Collection" "Object"]
    # maps: phm / plain struct / sorted / records — java.util.Map covers them
    # all in ring-style extend-protocol clauses
    (or (phm? obj)
        (shape-rec? obj)   # plain shape maps AND records — both map-like
        (and (struct? obj) (nil? (get obj :jolt/type)))
        (and (table? obj) (or (get obj :jolt/deftype)
                              (= :jolt/sorted-map (get obj :jolt/type)))))
      ["PersistentHashMap" "APersistentMap" "IPersistentMap" "Map" "IPersistentCollection" "Object"]
    (or (set? obj) (and (table? obj) (= :jolt/sorted-set (get obj :jolt/type))))
      ["PersistentHashSet" "IPersistentSet" "Set" "IPersistentCollection" "Object"]
    (or (tuple? obj) (array? obj) (pvec? obj)) ["PersistentVector" "IPersistentVector" "IPersistentCollection" "ISeq" "Object"]
    (or (function? obj) (cfunction? obj)) ["IFn" "Fn" "Object"]
    (nil? obj) ["nil" "Object"]
    ["Object"]))

# ---------------------------------------------------------------------------
# Stateful primitives as ordinary fns (Stage 2 jolt-eaa). These mutate/read the
# per-ctx protocol registry, so they need ctx. They're interned into clojure.core
# as closures over ctx (install-stateful-fns!), which makes them resolve + COMPILE
# as plain :var invokes — the back end embeds the per-ctx var cell, and the closure
# captures ctx so a compiled protocol dispatcher works even when called later.
# Both the interpreter and compiled code call these same closures; there is no
# longer a special-form handler for them. proto/method/type names arrive as
# STRINGS (the defprotocol/extend-type macros pass (name sym), not the symbol).
(defn protocol-dispatch-impl [ctx proto-name method-name obj rest-args]
  # an empty jolt rest arg is NIL (Clojure semantics); janet apply needs a tuple
  (default rest-args [])
  (def type-tag (or (record-tag obj)
                    (if (and (table? obj) (get obj :jolt/protocol-methods)) (get obj :jolt/deftype))))
  (if (and (table? obj) (get obj :jolt/protocol-methods))
    (let [reified-fns (get obj :jolt/protocol-methods)
          f (get reified-fns (keyword method-name))]
      (if f (apply f obj rest-args)
        (error (string "No reified method " method-name " for " type-tag))))
    (if type-tag
      (let [f (find-protocol-method ctx type-tag proto-name method-name)]
        (if f (apply f obj rest-args)
          (error (string "No method " method-name " in " proto-name " for " type-tag))))
      # host value: try candidate host type-tags (Long/String/Object/...), with a
      # generation-guarded inline cache (same walk for every value of a host class).
      (let [env (ctx :env)
            reg-gen (or (get env :type-registry-gen) 0)
            pc (let [c (get env :proto-dispatch-cache)]
                 (if (and c (= (c :gen) reg-gen)) c
                   (let [n @{:gen reg-gen :map @{}}]
                     (put env :proto-dispatch-cache n) n)))
            cands (value-host-tags obj)
            ckey [(first cands) proto-name method-name]
            cached (get (pc :map) ckey)
            found (if (nil? cached)
                    (let [f (do (var r nil)
                              (each tag cands
                                (when (nil? r)
                                  (set r (find-protocol-method ctx tag proto-name method-name))))
                              r)]
                      (put (pc :map) ckey (if f f :jolt/none))
                      f)
                    (if (= cached :jolt/none) nil cached))]
        (if found (apply found obj rest-args)
          (error (string "No dispatch for " method-name " on " (type obj))))))))

(defn register-method-impl [ctx type-name proto-name method-name f]
  # host types register under a bare canonical tag; deftype/record names stay
  # namespace-qualified to the ns the (extend-)type form runs in.
  (def host (canonical-host-tag type-name))
  (def type-tag (if host host (string (ctx-current-ns ctx) "." type-name)))
  (register-protocol-method ctx type-tag proto-name method-name f))

(defn make-reified-impl [ctx proto-name methods-map]
  # methods-map is the EVALUATED {keyword fn} map (a phm when compiled, a struct/
  # table when interpreted) — the fn* literals are already fns, just store them.
  (def obj @{:jolt/deftype (string "reified-" proto-name) :jolt/protocol-methods @{}})
  (def pairs (if (phm? methods-map)
               (phm-entries methods-map)
               (map (fn [k] [k (get methods-map k)]) (keys methods-map))))
  (each p pairs (put (obj :jolt/protocol-methods) (in p 0) (in p 1)))
  obj)

(defn require-impl
  "(require '[ns :as a :refer [...]] ...) — load + alias/refer each spec. A fn, so
  the args (quoted specs) arrive evaluated. Varargs (Clojure-compatible); each spec
  is a vector [ns & opts] or a bare ns symbol (treated as [ns])."
  [ctx & specs]
  (each spec specs
    (let [s (if (pvec? spec) (pv->array spec) spec)]
      (cond
        (and (indexed? s) (> (length s) 0)) (eval-require ctx s)
        (and (struct? s) (= :symbol (s :jolt/type))) (eval-require ctx @[s])
        (error "require expects a vector spec or a namespace symbol"))))
  nil)

(defn in-ns-impl
  "(in-ns 'foo) — switch the current namespace (creating it if needed). A fn; the
  quoted symbol arrives evaluated."
  [ctx sym]
  (def ns-name (if (and (struct? sym) (= :symbol (sym :jolt/type))) (sym :name) (string sym)))
  (def the-ns-obj (ctx-find-ns ctx ns-name))
  # An ns entered in-session counts as loaded (Clojure's ns macro commutes the
  # name into *loaded-libs*), so a later require/use of it must not try to load
  # a file — see maybe-require-ns. Namespace objects are immutable structs, so
  # the set lives on the env.
  (def loaded (or (get (ctx :env) :loaded-namespaces)
                  (let [t @{}] (put (ctx :env) :loaded-namespaces t) t)))
  (put loaded ns-name true)
  (ctx-set-current-ns ctx ns-name)
  the-ns-obj)

(defn use-impl
  "(use '[ns ...] ...) — refer ALL public vars of each used ns into the CURRENT ns.
  A fn; quoted specs arrive evaluated. Each spec is a ns symbol or a [ns & opts]
  vector (a pvec/tuple, not a Janet array — coerce, then take the head as the ns)."
  [ctx & specs]
  (def target-ns (ctx-find-ns ctx (ctx-current-ns ctx)))
  (each s specs
    (let [spec (if (pvec? s) (pv->array s) s)
          ns-sym (if (indexed? spec) (in spec 0) spec)
          src-name (sym-name-str ns-sym)]
      (maybe-require-ns ctx src-name)
      (let [source-ns (ctx-find-ns ctx src-name)]
        # Refer maps the SOURCE VAR itself (the Clojure model): redefinitions in
        # the source ns propagate, the :macro flag travels for free, and
        # ns-refers can identify refers by the var's home :ns.
        (loop [[sym v] :pairs (source-ns :mappings)]
          (put (target-ns :mappings) sym v)))))
  nil)

(defn import-impl
  "(import 'pkg.Class ...) — register the short class name as an alias of the fully
  qualified name in the current ns. A fn; quoted class symbols arrive evaluated."
  [ctx & class-specs]
  (def ns (ctx-find-ns ctx (ctx-current-ns ctx)))
  (defn sym-name [x] (if (and (struct? x) (= :symbol (x :jolt/type))) (x :name) (string x)))
  (defn import-one [class-name &opt pkg]
    (def last-dot (do (var idx -1) (var pos 0)
                    (while (< pos (length class-name))
                      (when (= (class-name pos) 46) (set idx pos)) (++ pos))
                    idx))
    (def short-name (if (>= last-dot 0) (string/slice class-name (+ last-dot 1)) class-name))
    (def pkg-name (cond pkg pkg (>= last-dot 0) (string/slice class-name 0 last-dot) nil))
    (ns-import ns short-name class-name)
    # a deftype "class" lives as a ctor var in its defining jolt ns — share it
    # (the JVM import makes (TextNode. ...) resolvable; this is our analog)
    (when pkg-name
      (when-let [src-ns (get ((ctx :env) :namespaces) pkg-name)
                 v (ns-find src-ns short-name)]
        (put (ns :mappings) short-name v))))
  (each class-spec class-specs
    (if (or (array? class-spec) (tuple? class-spec)
            (and (table? class-spec) (= :jolt/pvec (class-spec :jolt/type))))
      # vector spec: [pkg Class1 Class2 ...]
      (let [items (if (table? class-spec) (pv->array class-spec) class-spec)
            pkg (sym-name (in items 0))]
        (for i 1 (length items)
          (import-one (string pkg "." (sym-name (in items i))) pkg)))
      (import-one (sym-name class-spec))))
  nil)

(defn refer-clojure-impl
  "(refer-clojure :exclude [a b]) — currently only :exclude is honored: unmap the
  excluded names from the current ns. A fn; quoted args arrive evaluated."
  [ctx & args]
  (when (and (>= (length args) 2) (= (in args 0) :exclude))
    (let [ns (ctx-find-ns ctx (ctx-current-ns ctx))
          excl (in args 1)]
      (each sym excl
        (ns-unmap ns (if (and (struct? sym) (= :symbol (sym :jolt/type))) (sym :name) (string sym))))))
  nil)

# Multimethod value -> its var. methods/get-method take the multimethod VALUE
# (Clojure semantics) and recover the var (hence :jolt/methods) through this,
# which works from a compiled fn in any namespace — resolving the symbol at call
# time in the current ns did not (a bare multifn ref in its defining ns saw an
# empty table once defmethods lived in other namespaces; migratus hit this).
(def multi-registry @{})

(defn defmulti-setup
  "(defmulti name dispatch & opts) — intern a multimethod var. A fn; name arrives
  quoted, dispatch + opts (:default key, :hierarchy h) arrive evaluated. The
  defmulti macro is the thin wrapper. Builds the dispatch closure over the method
  table (shared with the var's :jolt/methods so defmethod adds to it)."
  [ctx name-sym dispatch-raw & opts]
  (def dispatch-fn (if (keyword? dispatch-raw) (fn [x] (get x dispatch-raw)) dispatch-raw))
  (def default-key
    (do (var dv :default) (var i 0)
      (while (< i (length opts))
        (if (= :default (in opts i)) (do (set dv (in opts (+ i 1))) (set i (length opts))) (+= i 2)))
      dv))
  (def hierarchy
    (do (var h nil) (var i 0)
      (while (< i (length opts))
        (if (= :hierarchy (in opts i)) (do (set h (in opts (+ i 1))) (set i (length opts))) (+= i 2)))
      h))
  (def ns (ctx-find-ns ctx (ctx-current-ns ctx)))
  (def methods @{})
  (def isa-cache @[nil])
  (def dispatch-cache @{})
  # the prefers table, shared with the var (prefer-method-setup mutates it)
  (def v-box @[nil])
  (def mm-fn
    (fn [& args]
      (let [dv* (apply dispatch-fn args)
            dv (if (nil? dv*) :jolt/nil-sentinel dv*)
            method (get methods dv)]
        (if method
          (apply method args)
          (let [cached (get dispatch-cache dv)]
            (if cached
              (apply cached args)
              # isa? is the OVERLAY's (the hierarchy system is pure Clojure now,
              # stage 3); resolve its var lazily, once. A :hierarchy option is an
              # atom (deref per dispatch, like Clojure's var) or a plain map.
              (let [isa-fn (do
                             (when (nil? (isa-cache 0))
                               (put isa-cache 0
                                    (var-get (ns-find (ctx-find-ns ctx "clojure.core") "isa?"))))
                             (isa-cache 0))
                    h (if hierarchy
                        (if (and (table? hierarchy) (= :jolt/atom (get hierarchy :jolt/type)))
                          (hierarchy :value)
                          hierarchy)
                        nil)
                    # Collect EVERY isa-matching method key, then pick the
                    # dominant one: x dominates y when x is prefer-method'd
                    # over y (direct preference) or (isa? x y). Two matches
                    # with no dominant is an ambiguity ERROR, as in Clojure —
                    # this used to silently take whichever key the table
                    # yielded first, ignoring prefer-method (jolt-heo).
                    found (do
                            (def matches @[])
                            (each k (keys methods)
                              (when (if h (isa-fn h dv k) (isa-fn dv k))
                                (array/push matches k)))
                            (defn pref? [x y]
                              (def px (get (or (get v-box 0) @{}) x))
                              (and px (not (nil? (get px y)))))
                            (defn dom? [x y]
                              (or (pref? x y) (if h (isa-fn h x y) (isa-fn x y))))
                            (case (length matches)
                              0 nil
                              1 (get methods (in matches 0))
                              (do
                                (var best (in matches 0))
                                (var i 1)
                                (while (< i (length matches))
                                  (when (dom? (in matches i) best) (set best (in matches i)))
                                  (++ i))
                                (var amb nil)
                                (each k matches
                                  (when (and (nil? amb) (not (deep= k best)) (not (dom? best k)))
                                    (set amb k)))
                                (when amb
                                  (error (string "Multiple methods in multimethod '" (name-sym :name)
                                                 "' match dispatch value — neither is preferred")))
                                (get methods best))))]
                (if found
                  (do (put dispatch-cache dv found) (apply found args))
                  (let [dm (get methods default-key)]
                    (if dm (apply dm args)
                      (error (string "No method in multimethod " (name-sym :name)
                                     " for dispatch value: " dv))))))))))))
  (def v (ns-intern ns (name-sym :name) mm-fn))
  # pre-create the prefers store so the dispatch closure and
  # prefer-method-setup share one table
  (def prefs-tbl (or (get v :jolt/prefers)
                     (do (put v :jolt/prefers @{}) (get v :jolt/prefers))))
  (put v-box 0 prefs-tbl)
  (put v :jolt/methods methods)
  (put v :jolt/dispatch-cache dispatch-cache)
  (put v :jolt/default default-key)
  (when hierarchy (put v :jolt/hierarchy hierarchy))
  (put multi-registry mm-fn v)
  (var-get v))

(defn defmethod-setup
  "(defmethod mm dispatch-val impl) — add a method to a multimethod. A fn; mm
  arrives quoted, dispatch-val evaluated, impl is the COMPILED method fn (the
  defmethod macro builds (fn …)). Auto-creates the multimethod if it's missing."
  [ctx mm-sym dispatch-val impl]
  (def mm-var
    (or (resolve-var ctx @{} mm-sym)
        (let [ns (ctx-find-ns ctx (ctx-current-ns ctx))
              stub (fn [& args] nil)]
          (def v (ns-intern ns (mm-sym :name) stub))
          (put v :jolt/methods @{})
          (put multi-registry stub v)
          v)))
  (def methods (or (get mm-var :jolt/methods) (let [m @{}] (put mm-var :jolt/methods m) m)))
  # nil is a legal dispatch value (ring's body-string keys a method on it);
  # janet tables can't hold nil keys, so it rides the sentinel
  (put methods (if (nil? dispatch-val) :jolt/nil-sentinel dispatch-val) impl)
  (let [dc (get mm-var :jolt/dispatch-cache)]
    (when dc (each k (keys dc) (put dc k nil))))
  mm-var)

(defn- hint-cross-ns-key
  "Resolve a record-typed field hint (\"Vec3\", \"v/Vec3\", \"rt.vec/Vec3\") to the
  home namespace's ctor key (\"rt.vec/->Vec3\") when the type is defined in a
  DIFFERENT namespace and referred/aliased into the one being defined. The local
  current-ns/->Type lookup misses those; this resolves the hint name through the
  ns's :refer/:as bindings to the type var, then maps its root ctor value back to
  the home key via the ctor-value index. Using the ctor VALUE, not the var's :ns,
  is what makes :refer work — a :refer re-interns a fresh var whose :ns is the
  referring ns, but its root is the same shared ctor closure. nil if unresolved."
  [ctx t cix]
  # Resolve against the COMPILE ns (the user ns being analyzed), not ctx-current-ns
  # — during compilation the analyzer rebinds ctx-current-ns to jolt.analyzer, so a
  # bare referred name would otherwise miss. Qualified alias/Name resolves the alias
  # against the compile ns; a bare name looks up the compile ns's own mappings
  # (which include :refer-interned vars).
  (def cur-name (or (get (ctx :env) :compile-ns) (ctx-current-ns ctx)))
  (def cur-ns (ctx-find-ns ctx cur-name))
  (def slash (string/find "/" t))
  (def v (when cur-ns
           (if slash
             (let [a (string/slice t 0 slash) nm (string/slice t (inc slash))
                   home (or (ns-alias-lookup cur-ns a) (ns-import-lookup cur-ns a))]
               (when home (ns-find (ctx-find-ns ctx home) nm)))
             (ns-find cur-ns t))))
  (when (and v (table? v)) (get cix (v :root))))

(defn record-hint-ctor-key
  "Resolve a record-type hint NAME (as written on a ^Type field/param — bare,
  aliased, or fully qualified) to its home ctor key in the record-shapes registry
  (\"rt.vec/->Vec3\"), or nil if it is not a known record type. Local
  current-ns/->Name wins; otherwise cross-ns via the ctor-value index. Public so
  the analyzer (through jolt.host) can type a ^Type PARAM hint exactly as a field
  hint resolves, which is what carries a record param's type across a namespace
  boundary without whole-program inference."
  [ctx name]
  (def rs (get (ctx :env) :record-shapes))
  (when rs
    (def cur (or (get (ctx :env) :compile-ns) (ctx-current-ns ctx)))
    (def local (string cur "/->" name))
    (if (get rs local)
      local
      (let [cix (get (ctx :env) :record-ctor-index)]
        (when cix (hint-cross-ns-key ctx name cix))))))

(defn make-deftype-ctor-impl
  "Build a deftype constructor closure. The ns-qualified type tag is baked at
  definition time (this runs during the deftype's (def …), in the type's ns), so
  instances carry a stable tag matching what extend-type registers methods under.
  field-kws is the [:f1 :f2 …] keyword vector; the ctor maps positional args to
  those keys. A ctx-capturing closure (make-deftype-ctor) is the public handle."
  [ctx type-name-sym field-kws &opt field-tags]
  (def type-tag (string (ctx-current-ns ctx) "." (type-name-sym :name)))
  (def kws (d-realize field-kws))
  # per-field type hints (jolt-3ko): a tuple parallel to kws — "Vec3" (a record
  # type name), "num", or nil. The inference resolves these to the field's exact
  # type so reading a field back carries it (a nested record stays typed).
  (def tags (if field-tags (d-realize field-tags) (array/new-filled (length kws))))
  # The ctor closure itself. Built FIRST so it can be indexed by value below.
  # Records are shape-recs when shapes are active (:shapes? = direct-link, where
  # the inference proves the reads) — the whole field-access pipeline handles
  # them; otherwise the original :jolt/deftype tables. Read at ctor-BUILD time so
  # a type is consistently one representation or the other.
  (def the-ctor
    (if (get (ctx :env) :shapes?)
      (fn [& args] (make-record type-tag kws args))
      (fn [& args]
        (var inst @{:jolt/deftype type-tag})
        (var i 0) (each kw kws (put inst kw (in args i)) (++ i))
        inst)))
  # jolt-t34: register this record's ctor return shape (DECLARED field order) so
  # the inference types (->Name ...) as a struct of these fields and field reads
  # on the result bare-index. Keyed by the ctor var-key "ns/->Name" to match how
  # the IR names the call head. Harmless when records aren't shaped (sidx gated).
  (let [rs (or (get (ctx :env) :record-shapes)
               (let [t @{}] (put (ctx :env) :record-shapes t) t))
        # ctor-value index: maps each ctor closure to its rs key, so a ^Type hint
        # in another namespace can resolve home through the type var's root value
        # (jolt-3ko cross-ns hints; see hint-cross-ns-key).
        cix (or (get (ctx :env) :record-ctor-index)
                (let [t @{}] (put (ctx :env) :record-ctor-index t) t))
        # resolve a record-typed hint ("Vec3") to its ctor-key ("ns/->Vec3") so
        # the inference resolves it with a direct lookup. "num" stays as-is; a
        # local def wins; else try cross-ns resolution; an unresolved name (not a
        # known record type) stays bare -> :any.
        resolved (map (fn [t]
                        (cond (nil? t) nil
                              (= t "num") "num"
                              (let [ck (string (ctx-current-ns ctx) "/->" t)]
                                (if (get rs ck) ck
                                  (or (hint-cross-ns-key ctx t cix) t)))))
                      tags)]
    (put rs (string (ctx-current-ns ctx) "/->" (type-name-sym :name))
         {:fields (tuple ;kws) :type type-tag :tags (tuple ;resolved)})
    (put cix the-ctor (string (ctx-current-ns ctx) "/->" (type-name-sym :name))))
  the-ctor)

(defn install-stateful-fns!
  "Intern ctx-capturing closures for the stateful primitives into clojure.core, so
  both the interpreter and the compiler reach them as ordinary fns. Called by
  api/init after init-core! and before the overlay loads (the protocol macros
  expand to calls of these)."
  [ctx]
  (def core (ctx-find-ns ctx "clojure.core"))
  # current-ns get/set for compiled code (emit-try restores the ns on a caught
  # throw — an interpreted fn that throws leaves ctx-current-ns set to its
  # defining ns, since it can't restore on unwind; the interpreted try already
  # repairs this, the compiled try did not, leaking the ns past a catch).
  (ns-intern core "__current-ns" (fn [] (ctx-current-ns ctx)))
  (ns-intern core "__set-current-ns!" (fn [ns-sym] (ctx-set-current-ns ctx ns-sym) nil))
  (ns-intern core "protocol-dispatch"
    (fn [proto-name method-name obj rest-args]
      (protocol-dispatch-impl ctx proto-name method-name obj rest-args)))
  # Devirtualization registry (jolt-41m): defprotocol calls this at load so the
  # inference can recognize a protocol-method call site. Maps the method's
  # var-key "ns/method" -> [proto-name method-name].
  (ns-intern core "register-protocol-methods!"
    (fn [proto-name method-names]
      (def reg (or (get (ctx :env) :protocol-methods)
                   (let [t @{}] (put (ctx :env) :protocol-methods t) t)))
      (def ns (ctx-current-ns ctx))
      (each m (d-realize method-names) (put reg (string ns "/" m) (tuple proto-name m)))
      nil))
  (ns-intern core "extenders"
    (fn [proto]
      # All type-tags whose registry entry implements this protocol, as symbols
      # (closest analog to Clojure's class list); nil when none.
      (let [pname (get (get proto :name) :name)
            registry (get (ctx :env) :type-registry)
            out @[]]
        (each tag (keys registry)
          (when (get (get registry tag) pname)
            (array/push out {:jolt/type :symbol :ns nil :name tag})))
        (if (empty? out) nil (tuple ;out)))))
  (ns-intern core "register-method"
    (fn [type-name proto-name method-name f]
      (register-method-impl ctx type-name proto-name method-name f)))
  (ns-intern core "make-reified"
    (fn [proto-name methods-map] (make-reified-impl ctx proto-name methods-map)))
  # Host-class shim registration, exposed to Clojure so a library can mirror a
  # Java class jolt doesn't ship (e.g. reitit.Trie). __register-class-statics!
  # makes (Class/method ...) resolve; __register-class-methods! makes (.method
  # tagged-value ...) dispatch; __register-class-ctor! makes (Class. ...) build.
  # Reader-conditional feature toggle, exposed to Clojure so a namespace can
  # load a clj-targeted library (e.g. reitit, under :clj) WITHOUT forcing the
  # whole process to :clj — set features, require the lib, restore. Returns the
  # previous feature set (a list of name strings) for restoration.
  (ns-intern core "__reader-features"
    (fn [] (tuple ;(map (fn [k] (string k)) (keys reader-features)))))
  (ns-intern core "__reader-features-set!"
    (fn [names]
      # names arrives as a jolt vector (pvec) or list — coerce to a janet array
      (def arr (cond (pvec? names) (pv->array names)
                     (or (tuple? names) (array? names)) names
                     @[names]))
      (reader-features-set! (map (fn [n] (if (keyword? n) n (string n))) arr))
      nil))
  (ns-intern core "__register-class-statics!"
    (fn [nm tbl] (register-class-statics! nm tbl) nil))
  (ns-intern core "__register-class-methods!"
    (fn [tag tbl] (register-tagged-methods! tag tbl) nil))
  (ns-intern core "__register-class-ctor!"
    (fn [nm f] (register-class-ctor! nm f) (ns-intern core nm (class-value-for nm)) nil))
  (ns-intern core "require" (fn [& specs] (require-impl ctx ;specs)))
  (ns-intern core "in-ns" (fn [sym] (in-ns-impl ctx sym)))
  (ns-intern core "use" (fn [& specs] (use-impl ctx ;specs)))
  (ns-intern core "import" (fn [& specs] (import-impl ctx ;specs)))
  (ns-intern core "refer-clojure" (fn [& args] (refer-clojure-impl ctx ;args)))
  (ns-intern core "defmulti-setup" (fn [name-sym dispatch & opts] (defmulti-setup ctx name-sym dispatch ;opts)))
  (ns-intern core "defmethod-setup" (fn [mm-sym dval impl] (defmethod-setup ctx mm-sym dval impl)))
  (ns-intern core "make-deftype-ctor" (fn [name-sym field-kws &opt field-tags] (make-deftype-ctor-impl ctx name-sym field-kws field-tags)))
  # Var/namespace lookups that need the ctx (the rest of the var fns — var-get/
  # var-set/var?/alter-var-root/alter-meta!/reset-meta! — are plain core-bindings).
  (ns-intern core "find-var" (fn [sym] (find-var ctx sym)))
  # *ns*: the current-namespace dynamic var. Its root is kept in sync by
  # ctx-set-current-ns via the cached var table (env :ns-var); a thread
  # binding (binding [*ns* ...]) shadows the root through var-get as usual.
  (def ns-var (ns-intern core "*ns*" (ctx-find-ns ctx (ctx-current-ns ctx))))
  (put ns-var :dynamic true)
  (put (ctx :env) :ns-var ns-var)
  (ns-intern core "intern"
    (fn [ns-name sym-name &opt val]
      (def ns (ctx-find-ns ctx (if (struct? ns-name) (ns-name :name) ns-name)))
      (ns-intern ns (if (struct? sym-name) (sym-name :name) sym-name) val)))
  # --- ns introspection (Stage 2 tier 6b) — evaluated-arg Clojure semantics.
  # A namespace designator is an ns object (passes through) or a symbol/string
  # naming one. find-ns is a pure lookup (nil when absent); create-ns creates
  # (ctx-find-ns is create-on-demand). The optional-arg forms default to the
  # current ns, preserving the prior 0-arg interpreter behavior.
  (def ns-name-of (fn [x]
    (cond
      (and (struct? x) (= :symbol (x :jolt/type))) (x :name)
      (string? x) x
      (keyword? x) (string x)
      nil)))
  (def ns-of (fn [x]
    (if (= :jolt/namespace (get x :jolt/type))
      x
      (let [nm (ns-name-of x)]
        (if nm (get (get (ctx :env) :namespaces) nm) nil)))))
  (def ns-or-current (fn [x]
    (if (nil? x)
      (ctx-find-ns ctx (ctx-current-ns ctx))
      (or (ns-of x) (error (string "No namespace: " (ns-name-of x)))))))
  (ns-intern core "find-ns" (fn [x] (ns-of x)))
  (ns-intern core "create-ns" (fn [x] (ctx-find-ns ctx (ns-name-of x))))
  (ns-intern core "remove-ns" (fn [x] (remove-ns ctx (ns-name-of x))))
  (ns-intern core "all-ns" (fn [] (all-ns ctx)))
  (ns-intern core "the-ns" (fn [&opt x] (ns-or-current x)))
  # interns/imports return a jolt MAP (struct), not the live host table — so
  # count/seq/keys work on them, and callers can't mutate the ns through them.
  (ns-intern core "ns-interns" (fn [&opt x] (table/to-struct ((ns-or-current x) :mappings))))
  # {alias-symbol -> namespace object}, Clojure's shape, from the string store.
  (ns-intern core "ns-aliases"
    (fn [&opt x]
      (def ns (ns-or-current x))
      (def out @{})
      (eachp [a target] (ns :aliases)
        (put out {:jolt/type :symbol :ns nil :name a} (ctx-find-ns ctx target)))
      (table/to-struct out)))
  (ns-intern core "ns-imports" (fn [&opt x] (table/to-struct ((ns-or-current x) :imports))))
  # (ns-resolve ns sym) -> the var or nil. Unqualified syms look in ns's own
  # mappings; ns-qualified syms resolve through ns's aliases. (types/ns-resolve
  # keys ns-find with the symbol struct instead of its name string, so it never
  # finds anything — do the lookup here.)
  (ns-intern core "ns-resolve"
    (fn [ns-d sym]
      (def ns (ns-or-current ns-d))
      (def nm (if (struct? sym) (sym :name) (string sym)))
      (def nsp (if (struct? sym) (sym :ns) nil))
      (if nsp
        (let [target (or (ns-alias-lookup ns nsp) nsp)
              target-ns (ctx-find-ns ctx target)]
          (when target-ns (ns-find target-ns nm)))
        (ns-find ns nm))))
  (ns-intern core "resolve"
    (fn [sym]
      (when (and (struct? sym) (= :symbol (sym :jolt/type)))
        (def r (protect (resolve-var ctx @{} sym)))
        (if (r 0) (r 1) nil))))
  # refer: bring another ns's public vars into the current ns. Reuses use-impl's
  # refer-all behavior; the :only/:exclude/:rename filters are not yet honored.
  (ns-intern core "refer" (fn [ns-sym & filters] (use-impl ctx ns-sym)))
  # --- dispatch-table / type fns (Stage 2 tier 6c) ------------------------
  # A multimethod's method table lives on its VAR (the value is the dispatch
  # closure), so the overlay macros pass the NAME quoted — the defmulti/
  # defmethod pattern — and these resolve the var. prefer-method auto-creates
  # a missing multimethod (matching the prior interpreter arm).
  (def mm-var-of (fn [mm-sym auto-create?]
    (def r (protect (resolve-var ctx @{} mm-sym)))
    (def found (if (r 0) (r 1) nil))
    (if found
      found
      (when auto-create?
        (def ns (ctx-find-ns ctx (ctx-current-ns ctx)))
        (def stub (fn [& args] nil))
        (def nv (ns-intern ns (mm-sym :name) stub))
        (put nv :jolt/methods @{})
        (put multi-registry stub nv)
        nv))))
  (def clear-dispatch-cache! (fn [mm-var]
    (let [dc (get mm-var :jolt/dispatch-cache)]
      (when dc (each k (keys dc) (put dc k nil))))))
  (ns-intern core "prefer-method-setup"
    (fn [mm-sym dval-a dval-b]
      (def mm-var (mm-var-of mm-sym true))
      (def prefs (or (get mm-var :jolt/prefers)
                     (do (put mm-var :jolt/prefers @{}) (mm-var :jolt/prefers))))
      # {x -> {y true ...}}: x is preferred over each y (Clojure's {x #{y}})
      (def sub (or (get prefs dval-a)
                   (do (put prefs dval-a @{}) (get prefs dval-a))))
      (put sub dval-b true)
      (clear-dispatch-cache! mm-var)
      mm-var))
  (ns-intern core "remove-method-setup"
    (fn [mm-sym dval]
      (def dval (if (nil? dval) :jolt/nil-sentinel dval))
      (def mm-var (mm-var-of mm-sym false))
      (when mm-var
        (let [methods (get mm-var :jolt/methods)]
          (when methods (put methods dval nil)))
        (clear-dispatch-cache! mm-var))
      mm-var))
  (ns-intern core "remove-all-methods-setup"
    (fn [mm-sym]
      (def mm-var (mm-var-of mm-sym false))
      (when mm-var
        # clear IN PLACE: the dispatch closure captured this table at defmulti
        # time, so swapping in a fresh one leaves dispatch seeing stale methods
        (let [methods (get mm-var :jolt/methods)]
          (when methods (each k (keys methods) (put methods k nil))))
        (clear-dispatch-cache! mm-var))
      mm-var))
  (ns-intern core "prefers-setup"
    (fn [mm-sym]
      (def mm-var (mm-var-of mm-sym false))
      (or (and mm-var (get mm-var :jolt/prefers)) {})))
  # methods/get-method receive the multimethod VALUE (Clojure semantics): map it
  # back to its var via multi-registry. A symbol arg still works (mm-var-of), for
  # any caller that passes one.
  (def mm-var-of-val (fn [mm]
    (if (function? mm) (get multi-registry mm) (mm-var-of mm false))))
  (ns-intern core "get-method-setup"
    (fn [mm dval]
      (def dval (if (nil? dval) :jolt/nil-sentinel dval))
      (def mm-var (mm-var-of-val mm))
      (when mm-var
        (let [methods (get mm-var :jolt/methods)]
          (or (get methods dval) (get methods :default))))))
  (ns-intern core "methods-setup"
    (fn [mm]
      (def mm-var (mm-var-of-val mm))
      (when mm-var
        # a jolt map, not the live host table (and phm so vector dispatch
        # values look up by value, same reason build-eval-map promotes)
        (var m (make-phm))
        (let [tbl (get mm-var :jolt/methods)]
          (when tbl (each k (keys tbl) (set m (phm-assoc m k (get tbl k))))))
        m)))
  # satisfies?: evaluated protocol value + instance (matches the prior arm).
  (ns-intern core "satisfies?"
    (fn [proto obj]
      (def type-tag (or (record-tag obj)
                        (if (and (table? obj) (get obj :jolt/protocol-methods))
                          (get obj :jolt/deftype))))
      (if type-tag
        (let [pn (proto :name)
              pn-str (if (struct? pn) (pn :name) pn)]
          (type-satisfies? ctx type-tag pn-str))
        false)))
  # instance?: the overlay macro passes the TYPE NAME quoted (class names don't
  # evaluate to values on jolt); the value arg arrives evaluated.
  (ns-intern core "instance-check"
    (fn [type-sym val]
      (if (record-tag val)
        (let [type-tag (record-tag val)
              type-name (type-sym :name)]
          (or (= type-tag type-name)
              (and (> (length type-tag) (length type-name))
                   (= (string/slice type-tag (- (length type-tag) (length type-name)))
                      type-name))))
        (match (type-sym :name)
          "Number" (number? val)
          "java.lang.Number" (number? val)
          "Long" (number? val)
          "java.lang.Long" (number? val)
          "Integer" (number? val)
          "Double" (number? val)
          "String" (string? val)
          "java.lang.String" (string? val)
          "Boolean" (or (= true val) (= false val))
          "Keyword" (keyword? val)
          # regex patterns (cuerdas-style (instance? Pattern x) checks)
          "Pattern" (and (table? val) (= :jolt/regex (val :jolt/type)))
          "java.util.regex.Pattern" (and (table? val) (= :jolt/regex (val :jolt/type)))
          "Character" (and (struct? val) (= :jolt/char (get val :jolt/type)))
          "java.lang.Character" (and (struct? val) (= :jolt/char (get val :jolt/type)))
          # java.time shims (javatime.janet); #inst IS java.util.Date in Clojure
          "java.util.Date" (and (struct? val) (= :jolt/inst (get val :jolt/type)))
          "Date" (and (struct? val) (= :jolt/inst (get val :jolt/type)))
          "Instant" (and (table? val) (= :jolt/instant (get val :jolt/type)))
          "java.time.Instant" (and (table? val) (= :jolt/instant (get val :jolt/type)))
          "LocalDateTime" (and (table? val) (= :jolt/local-dt (get val :jolt/type)))
          "java.time.LocalDateTime" (and (table? val) (= :jolt/local-dt (get val :jolt/type)))
          "ZonedDateTime" (and (table? val) (= :jolt/zoned-dt (get val :jolt/type)))
          "java.time.ZonedDateTime" (and (table? val) (= :jolt/zoned-dt (get val :jolt/type)))
          "LocalTime" false
          "LocalDate" false
          "java.sql.Time" false
          "java.sql.Timestamp" false
          "java.sql.Date" false
          "DateTimeFormatter" (and (table? val) (= :jolt/dt-formatter (get val :jolt/type)))
          "URL" (and (table? val) (= :jolt/url (get val :jolt/type)))
          "java.net.URL" (and (table? val) (= :jolt/url (get val :jolt/type)))
          # next.jdbc host shim: a wrapped jdbc.core connection (core.janet).
          # migratus's do-commands only runs SQL through its (instance? Connection)
          # branch, so the wrapped conn must answer true here.
          "Connection" (and (table? val) (= :jolt/jdbc-conn (get val :jolt/type)))
          "java.sql.Connection" (and (table? val) (= :jolt/jdbc-conn (get val :jolt/type)))
          # java.io.File model (jolt-hjw): io/file and (File. …) build :jolt/file,
          # so migratus's (instance? File migration-dir) takes the filesystem path.
          "File" (and (table? val) (= :jolt/file (get val :jolt/type)))
          "java.io.File" (and (table? val) (= :jolt/file (get val :jolt/type)))
          # JVM char[] class — (Class/forName "[C"); jolt char arrays are Janet
          # arrays of char structs
          "[C" (and (array? val)
                    (or (= 0 (length val))
                        (and (struct? (val 0)) (= :jolt/char ((val 0) :jolt/type)))))
          "clojure.lang.Atom" (and (table? val) (= :jolt/atom (val :jolt/type)))
          "clojure.lang.Volatile" (and (table? val) (= :jolt/volatile (val :jolt/type)))
          "clojure.lang.Delay" (and (table? val) (= :jolt/delay (val :jolt/type)))
          "clojure.lang.IPersistentMap" (or (phm? val) (struct? val))
          "clojure.lang.IPersistentVector" (or (tuple? val) (pvec? val))
          "clojure.lang.IPersistentSet" (set? val)
          "Object" true
          false))))
  # Reader / expansion as plain fns: read-string parses one form; macroexpand-1
  # expands a (quoted, already-evaluated) call form once via its macro var.
  (ns-intern core "read-string" (fn [s] (parse-string s)))
  # The *in* reader family's host seams. __stdin-read-line: one line from real
  # stdin, newline stripped, nil at EOF. __parse-next: one form off a string ->
  # [form rest-of-string], nil when only whitespace remains. *in*, read-line,
  # read, with-in-str, and line-seq are Clojure over these (core/50-io.clj).
  # The loader's registered source roots (the closest thing to a classpath) —
  # io/resource searches these for relative resource paths.
  # registered constructor shims: the NAME evaluates to the canonical class
  # string (so class-dispatch defmultis match); `new` finds the ctor fn.
  (eachp [nm f] class-ctors (ns-intern core nm (class-value-for nm)))
  # dispatch-only type names (no ctor): InputStream, File, ISeq, ...
  (eachp [nm canon] class-canonical-names
    (unless (or (in class-ctors nm) (ns-find core nm))
      (ns-intern core nm canon)))
  (ns-intern core "__source-roots"
    (fn [] (tuple ;(get (ctx :env) :source-paths))))
  (ns-intern core "__stdin-read-line"
    (fn []
      (let [l (file/read stdin :line)]
        (if (nil? l) nil
          (let [s (string l)]
            (if (string/has-suffix? "\n" s) (string/slice s 0 -2) s))))))
  (ns-intern core "__parse-next"
    (fn [s]
      (if (= 0 (length (string/trim s))) nil
        (let [r (parse-next s)] (tuple (r 0) (r 1))))))
  (def expand-1 (fn [the-form]
    (if (and (array? the-form) (> (length the-form) 0)
             (struct? (first the-form)) (= :symbol ((first the-form) :jolt/type)))
      (let [v (resolve-var ctx @{} (first the-form))]
        (if (and v (var-macro? v))
          (apply (var-get v) (tuple/slice the-form 1))
          the-form))
      the-form)))
  (ns-intern core "macroexpand-1" expand-1)
  # Apply a registered data reader to an already-read form (EDN built-in tags
  # #uuid/#inst and any registered reader). Throws on an unknown tag.
  (ns-intern core "__read-tagged"
    (fn [tag form]
      (def data-readers (get (ctx :env) :data-readers))
      (def reader-fn (if data-readers (get data-readers tag)))
      (if reader-fn
        (reader-fn form)
        (error (string "No reader function for tag " tag)))))
  # macroexpand: expand repeatedly until the head is no longer a macro (the
  # form's SUBFORMS are not expanded, matching Clojure).
  (ns-intern core "macroexpand"
    (fn [the-form]
      (var cur the-form)
      (var nxt (expand-1 cur))
      (while (not= cur nxt) (set cur nxt) (set nxt (expand-1 cur)))
      cur))
  # alias bookkeeping is UNIFIED (jolt-ark): :aliases (alias-name string ->
  # ns-name string) is the one store, read by resolution and ns-aliases;
  # :imports holds class imports only.
  (ns-intern core "alias"
    (fn [alias-sym ns-sym]
      (def cur (ctx-find-ns ctx (ctx-current-ns ctx)))
      (ns-add-alias cur (alias-sym :name) (ns-sym :name))
      nil))
  (ns-intern core "ns-unalias"
    (fn [ns-d alias-sym]
      (def ns (ns-or-current ns-d))
      (put (ns :aliases) (alias-sym :name) nil)
      nil))
  # ns-publics: {symbol -> var} (jolt has no private vars, so publics = interns).
  # Keys are symbol structs (value-hashed), matching Clojure's symbol keys.
  (def mappings->symbol-map (fn [ns pred]
    (var m (make-phm))
    (loop [[nm v] :pairs (ns :mappings)]
      (when (pred nm v)
        (set m (phm-assoc m {:jolt/type :symbol :ns nil :name nm} v))))
    m))
  (ns-intern core "ns-publics"
    (fn [&opt ns-d]
      (mappings->symbol-map (ns-or-current ns-d) (fn [nm v] true))))
  # ns-map: all mappings (interns + refers; jolt has no class imports in maps).
  (ns-intern core "ns-map"
    (fn [&opt ns-d]
      (mappings->symbol-map (ns-or-current ns-d) (fn [nm v] true))))
  # ns-refers: mappings whose var's HOME ns differs from this ns (copied in by
  # refer/use/require :refer).
  (ns-intern core "ns-refers"
    (fn [&opt ns-d]
      (def ns (ns-or-current ns-d))
      (def my-name (ns :name))
      (mappings->symbol-map ns (fn [nm v]
        (and (table? v) (not= (get v :ns) my-name))))))
  (ns-intern core "ns-unmap"
    (fn [ns-d sym]
      (def ns (ns-or-current ns-d))
      (put (ns :mappings) (if (struct? sym) (sym :name) (string sym)) nil)
      nil))
  core)

# Dispatch a special form by its string name.
(defn- unwrap-meta-name
  "Recursively unwrap (with-meta sym meta) forms to extract the underlying symbol.
  Returns the symbol struct, or the original form if it's not a with-meta wrapper."
  [form]
  (if (and (array? form) (> (length form) 0)
           (struct? (in form 0))
           (= :symbol ((in form 0) :jolt/type))
           (= "with-meta" ((in form 0) :name)))
    (unwrap-meta-name (in form 1))
    form))

(defn- eval-list
  [ctx bindings form]
  (def first-form (first form))
  # Safe name extraction: non-symbol heads (e.g. keywords) fall through to default.
  # A head qualified to a NON-core namespace (e.g. clojure.edn/read-string) must
  # resolve to that var, not the like-named clojure.core special form — so only
  # unqualified or clojure.core-qualified heads dispatch as special forms.
  (def name (if (and (struct? first-form) (= :symbol (first-form :jolt/type)))
              (let [ns (first-form :ns)]
                (if (or (nil? ns) (= ns "clojure.core")) (first-form :name) nil))
              nil))
  (match name
    "quote" (in form 1)
    # Interpreter builds the form directly (self-contained, no core dependency).
    # The COMPILE path instead lowers syntax-quote to construction code (via
    # syntax-quote-lower) so a backtick body is compilable; the two are kept in
    # sync and cross-checked by conformance (interpret vs compile modes).
    "syntax-quote" (syntax-quote* ctx bindings (in form 1))
    "unquote" (error "Unquote not valid outside of syntax-quote")
    "unquote-splicing" (error "Unquote-splicing not valid outside of syntax-quote")
    "eval" (eval-form ctx bindings (eval-form ctx bindings (in form 1)))
    # read-string/macroexpand-1 are ctx-capturing clojure.core fns and defonce
    # an overlay macro now (Stage 2 tier 6c) — no special-form arms.
    "do" (do
           (var result nil)
           (var i 1)
           (let [len (length form)]
             (while (< i len)
               (set result (eval-form ctx bindings (in form i)))
               (++ i)))
           result)
    "if" (do
           # 2 or 3 argument forms only (spec 03-special-forms X1)
           (when (or (< (length form) 3) (> (length form) 4))
             (error (string "Wrong number of args (" (dec (length form)) ") passed to: if")))
           (let [test-val (eval-form ctx bindings (in form 1))]
             (if (and (not (nil? test-val)) (not (= false test-val)))
               (eval-form ctx bindings (in form 2))
               (if (> (length form) 3) (eval-form ctx bindings (in form 3)) nil))))
    "def" (let [raw-name (in form 1)
                name-sym (unwrap-meta-name raw-name)
                # Metadata on the name: keyword/type-hint metadata rides on the
                # symbol (:meta); a ^{:map} reads as a with-meta form we evaluate.
                sym-meta (or (and (struct? name-sym) (get name-sym :meta)) {})
                wm-meta (if (and (array? raw-name) (> (length raw-name) 0)
                                 (sym-name? (first raw-name) "with-meta"))
                          (let [mv (protect (eval-form ctx bindings (last raw-name)))]
                            (if (and (mv 0) (or (table? (mv 1)) (struct? (mv 1)))) (mv 1) {}))
                          {})
                name-meta (merge wm-meta sym-meta)
                dynamic? (truthy? (get name-meta :dynamic))
                ns-name (ctx-current-ns ctx)
                ns (ctx-find-ns ctx ns-name)
                # Create var first (unbound) so self-referencing defs resolve
                v (ns-intern ns (name-sym :name))]
            # (def name) with no init interns the var and leaves any existing
            # root binding alone (Clojure semantics — this is what declare
            # expands to, so compiled forward refs bind to the var instead of
            # falling through to a like-named host builtin).
            (if (= 2 (length form))
              (do
                (when (not (empty? name-meta))
                  (put v :meta (merge (or (get v :meta) {}) name-meta)))
                (when dynamic? (put v :dynamic true))
                v)
              (let [# (def name docstring value): docstring form 2, value form 3
                    has-doc (and (> (length form) 3) (string? (in form 2)))
                    val-form (in form (if has-doc 3 2))
                    val (eval-form ctx bindings val-form)]
                (bind-root v val)
                # Staged bootstrap (jolt-4j3): pre/at-kernel overlay defns load
                # interpreted; stash the fn source so backend/recompile-defns! can
                # compile them once the analyzer is alive — the defn analog of
                # :macro-src. Only set while api/load-core-overlay! loads the early
                # tiers (the flag scopes it away from user code).
                (when (and (get (ctx :env) :stash-defn-src?)
                           (function? val)
                           (array? val-form) (> (length val-form) 0)
                           (or (sym-name? (first val-form) "fn")
                               (sym-name? (first val-form) "fn*")))
                  (put v :defn-src val-form))
                (let [extra (if has-doc (merge name-meta {:doc (in form 2)}) name-meta)]
                  (when (not (empty? extra))
                    (put v :meta (merge (or (get v :meta) {}) extra))))
                (when dynamic?
                  (put v :dynamic true))
                # def returns the var (Clojure semantics); REPL prints #'ns/name
                v)))
    "defmacro" (let [# ^{:map} metadata on the name reads as a (with-meta sym …)
                     # form (jolt-8w2); unwrap to the bare symbol like def does.
                     name-sym (unwrap-meta-name (in form 1))
                     after-name (tuple/slice form 2)
                     # Skip an optional leading docstring (string) then an optional
                     # attr-map (a struct that is not a symbol — a map literal reads
                     # as a struct), matching defn. Real macros use both, e.g.
                     # (defmacro info "doc" {:arglists '(...)} [& args] …).
                     a1 (if (and (> (length after-name) 0) (string? (first after-name)))
                          (tuple/slice after-name 1) after-name)
                     after-meta (if (and (> (length a1) 0)
                                         (struct? (first a1))
                                         (not= :symbol (get (first a1) :jolt/type)))
                                  (tuple/slice a1 1) a1)
                     # What remains is either a params VECTOR (tuple) + body, or one
                     # or more arity CLAUSES (each a list, i.e. a janet array). Build
                     # a uniform arity list [{:params … :body …} …].
                     multi? (and (> (length after-meta) 0) (array? (first after-meta)))
                     arities (if multi?
                               (map (fn [cl] {:params (first cl) :body (tuple/slice cl 1)})
                                    after-meta)
                               @[{:params (first after-meta) :body (tuple/slice after-meta 1)}])
                     defining-ns (ctx-current-ns ctx)]
                 (def interp-fn (fn [& macro-args]
                   (def n (length macro-args))
                   # Pick the arity: an exact fixed-count match wins; otherwise the
                   # first variadic arity that accepts n args (Clojure fn dispatch).
                   (var chosen nil)
                   (each ar arities
                     (def pi (parse-params (ar :params)))
                     (when (and (nil? chosen) (not (pi :rest)) (= n (length (pi :fixed))))
                       (set chosen [pi (ar :body)])))
                   (when (nil? chosen)
                     (each ar arities
                       (def pi (parse-params (ar :params)))
                       (when (and (nil? chosen) (pi :rest) (>= n (length (pi :fixed))))
                         (set chosen [pi (ar :body)]))))
                   (when (nil? chosen)
                     (error (string "no matching arity for macro " (name-sym :name)
                                    " (" n " args)")))
                   (def pi (chosen 0))
                   (def body (chosen 1))
                   (var new-bindings @{})
                   (table/setproto new-bindings bindings)
                   (put new-bindings "&env" @{})  # implicit &env for macro bodies (table — nil-safe)
                   (var i 0)
                   # Destructure macro params (like fn), so [& [a & more :as all]]
                   # and {:keys …} rest forms work in macro arglists.
                   (each pat (pi :fixed)
                     (destructure-bind ctx new-bindings pat (macro-args i))
                     (++ i))
                   (when (pi :rest)
                     (destructure-bind ctx new-bindings (pi :rest) (rest-args-val macro-args i)))
                   # Use defining namespace for symbol resolution
                   (def saved-ns (ctx-current-ns ctx))
                   (ctx-set-current-ns ctx defining-ns)
                   # Plain trailing restore (NOT defer/try — those build a fiber per
                   # call and blow the C stack on deep interpreted recursion). An
                   # unwinding throw is repaired once at the TOP-LEVEL boundary
                   # (loader/eval-toplevel restores the ns on error).
                   (var result nil)
                   (each bf body
                     (set result (eval-form ctx new-bindings bf)))
                   (ctx-set-current-ns ctx saved-ns)
                   result))
                 # A COMPILED expander (native-speed) is only built for the
                 # single-arity case (the compile hook + recompile path take one
                 # [args body]); multi-arity macros use the interpreted expander.
                 (def single? (= 1 (length arities)))
                 (def args-form (and single? ((first arities) :params)))
                 (def body (and single? ((first arities) :body)))
                 (def uses-env (do (var u false)
                                   (each ar arities
                                     (when (or (form-uses-sym? (ar :body) "&env")
                                               (form-uses-sym? (ar :body) "&form"))
                                       (set u true)))
                                   u))
                 (def compiled-fn
                   (when (and macro-compile-hook single? (not uses-env))
                     (macro-compile-hook ctx args-form body)))
                 (def macro-fn (or compiled-fn interp-fn))
                  (let [ns-name (ctx-current-ns ctx)
                       ns (ctx-find-ns ctx ns-name)]
                   (def v (ns-intern ns (name-sym :name) macro-fn))
                   (put v :macro true)
                   # Stash the expander source so backend/recompile-macros! can
                   # compile it once the analyzer is alive (staged bootstrap): a
                   # macro defined WHILE the analyzer is still being built gets an
                   # interpreted closure now, a compiled expander later. uses-env
                   # macros stay interpreted (the compiled fn* has no &env/&form);
                   # multi-arity macros keep the interpreted dispatch (no single
                   # [args body] to recompile).
                   (when single? (put v :macro-src @[args-form body]))
                   (put v :macro-uses-env uses-env)
                   (when compiled-fn (put v :macro-compiled true))
                   # A (re)defined macro invalidates any cached expansions.
                   (table/clear macro-cache)
                   (var-get v)))
    # ns is now a macro (clojure.core, 30-macros) expanding to in-ns + require/use/
    # import/refer-clojure calls — all ctx-capturing fns — so it compiles. No
    # special-form arm; an (ns ...) head falls through to the macro-expansion path.
    # require / in-ns are now ordinary clojure.core fns (install-stateful-fns!) —
    # no special-form arm; they compile + interpret as plain invokes.
    # all-ns/the-ns/create-ns/remove-ns/ns-interns/ns-aliases/ns-imports/
    # ns-resolve/resolve/find-ns/refer are ctx-capturing clojure.core fns now
    # (install-stateful-fns!) with evaluated-arg Clojure semantics — they fall
    # through to the function-call default and compile as plain invokes
    # (Stage 2 tier 6b).
    "fn*" (let [# optional name: (fn* name [args] ...) / (fn* name ([args] ...)...)
                named? (and (struct? (in form 1)) (= :symbol ((in form 1) :jolt/type)))
                fn-name (if named? ((in form 1) :name) nil)
                form (if named? (array/concat @[(in form 0)] (tuple/slice form 2)) form)]
          (if (array? (in form 1))
             # Multi-arity: (fn* ([args] body...) ([args] body...)...)
             (let [pairs (tuple/slice form 1)
                   arities @{}
                   defining-ns (ctx-current-ns ctx)]
               (var self nil)
               # The (single) variadic clause is dispatched separately: it handles
               # any arg count >= its fixed count. Storing it in `arities` by
               # fixed-count would collide with a same-fixed-count fixed clause and
               # only match that exact count.
               (var variadic-fn nil)
               (var variadic-min 0)
               (each pair pairs
                 (let [args-form (in pair 0)
                       body (tuple/slice pair 1)
                       param-info (parse-params args-form)
                       _ (require-symbol-params param-info)
                       fixed-pats (param-info :fixed)
                       rest-pat (param-info :rest)
                       n-fixed (length fixed-pats)
                       # recur-entry: where (recur ...) re-enters THIS arity. For
                       # a fixed arity it's the dispatcher (exact count re-selects
                       # it). For the VARIADIC arity, recur takes n-fixed + 1 args
                       # with the LAST bound DIRECTLY as the rest seq (Clojure) —
                       # re-entering through the varargs collector would wrap it
                       # in a fresh 1-element rest list and the seq never empties
                       # (the jolt-4df hang).
                       recur-entry-box @[nil]
                       run-clause (fn [fn-bindings]
                          (put fn-bindings :jolt/loop-fn (in recur-entry-box 0))
                          (when fn-name (bind-put fn-bindings fn-name self))
                          # Use defining namespace for symbol resolution
                          (def saved-ns (ctx-current-ns ctx))
                          (ctx-set-current-ns ctx defining-ns)
                          # Plain trailing restore (NOT defer/try — those build a fiber per
                          # call and blow the C stack on deep interpreted recursion). An
                          # unwinding throw is repaired once at the TOP-LEVEL boundary
                          # (loader/eval-toplevel restores the ns on error).
                          (var result nil)
                          (each body-form body
                            (set result (eval-form ctx fn-bindings body-form)))
                          (ctx-set-current-ns ctx saved-ns)
                          result)
                       f (fn [& fn-args]
                          (var fn-bindings @{})
                          (table/setproto fn-bindings bindings)
                          (var i 0)
                          (each pat fixed-pats
                            (destructure-bind ctx fn-bindings pat (fn-args i))
                            (++ i))
                          (when rest-pat
                            (destructure-bind ctx fn-bindings rest-pat (rest-args-val fn-args i)))
                          (run-clause fn-bindings))]
                   (if rest-pat
                     (do
                       (put recur-entry-box 0
                            (fn [& recur-args]
                              (var fn-bindings @{})
                              (table/setproto fn-bindings bindings)
                              (var i 0)
                              (each pat fixed-pats
                                (destructure-bind ctx fn-bindings pat (recur-args i))
                                (++ i))
                              (destructure-bind ctx fn-bindings rest-pat (get recur-args i))
                              (run-clause fn-bindings)))
                       (set variadic-fn f) (set variadic-min n-fixed))
                     (do
                       (put recur-entry-box 0 (fn [& recur-args] (apply self recur-args)))
                       (put arities n-fixed f)))))
               (set self (fn [& fn-args]
                 (let [n (length fn-args)
                       f (get arities n)]
                   (cond
                     f (apply f fn-args)
                     (and variadic-fn (>= n variadic-min)) (apply variadic-fn fn-args)
                     (error (string "Wrong number of args (" n ") passed to: "
                                    (or fn-name "fn")))))))
               self)
             # Single-arity: (fn* [args] body...)
             (let [args-form (in form 1)
                   body (tuple/slice form 2)
                   param-info (parse-params args-form)
                   _ (require-symbol-params param-info)
                   fixed-pats (param-info :fixed)
                   rest-pat (param-info :rest)
                   defining-ns (ctx-current-ns ctx)]
               (var self nil)
               (var recur-entry nil)
               (def run-body (fn [fn-bindings]
                 (put fn-bindings :jolt/loop-fn recur-entry)
                 (when fn-name (bind-put fn-bindings fn-name self))
                 # Use defining namespace for symbol resolution
                 (def saved-ns (ctx-current-ns ctx))
                 (ctx-set-current-ns ctx defining-ns)
                 # Plain trailing restore (NOT defer/try — those build a fiber per
                 # call and blow the C stack on deep interpreted recursion). An
                 # unwinding throw is repaired once at the TOP-LEVEL boundary
                 # (loader/eval-toplevel restores the ns on error).
                 (var result nil)
                 (each body-form body
                   (set result (eval-form ctx fn-bindings body-form)))
                 (ctx-set-current-ns ctx saved-ns)
                 result))
               (def n-fixed (length fixed-pats))
               (set self (fn [& fn-args]
                 # ArityException semantics (jolt-6xn): a fixed arity takes
                 # exactly its params, a variadic one at least its fixed params.
                 # The compiled path enforces this natively (janet fn arity);
                 # this keeps the interpreter oracle in agreement.
                 (def n (length fn-args))
                 (when (if rest-pat (< n n-fixed) (not= n n-fixed))
                   (error (string "Wrong number of args (" n ") passed to: "
                                  (or fn-name "fn"))))
                 (var fn-bindings @{})
                 (table/setproto fn-bindings bindings)
                 (var i 0)
                 (each pat fixed-pats
                   (destructure-bind ctx fn-bindings pat (fn-args i))
                   (++ i))
                 (when rest-pat
                   (destructure-bind ctx fn-bindings rest-pat (rest-args-val fn-args i)))
                 (run-body fn-bindings)))
               # recur re-enters here: for a variadic fn it takes n-fixed + 1
               # args, the LAST bound DIRECTLY as the rest seq (Clojure) — going
               # back through the varargs collector wrapped the seq in a fresh
               # 1-element rest list, so it never emptied (the jolt-4df hang).
               (set recur-entry
                 (if rest-pat
                   (fn [& recur-args]
                     (var fn-bindings @{})
                     (table/setproto fn-bindings bindings)
                     (var i 0)
                     (each pat fixed-pats
                       (destructure-bind ctx fn-bindings pat (recur-args i))
                       (++ i))
                     (destructure-bind ctx fn-bindings rest-pat (get recur-args i))
                     (run-body fn-bindings))
                   self))
              self)))
    "let*" (let [bind-vec (in form 1)
                  body (tuple/slice form 2)]
              (var new-bindings @{})
              (table/setproto new-bindings bindings)
              (var i 0)
              (let [len (length bind-vec)]
                (while (< i len)
                  (let [pat (bind-vec i)]
                    # let* is a primitive (the let macro desugars destructuring);
                    # its binding names must be plain symbols, as in Clojure.
                    (unless (plain-sym? pat) (error "Bad binding form, expected symbol"))
                    (def val (eval-form ctx new-bindings (bind-vec (+ i 1))))
                    (destructure-bind ctx new-bindings pat val)
                    (+= i 2))))
             (var result nil)
             (each body-form body
               (set result (eval-form ctx new-bindings body-form)))
             result)
    "loop*" (let [bind-vec (in form 1)
                  body (tuple/slice form 2)
                  init-vals @[]
                  patterns @[]
                  # Inits are evaluated sequentially in an accumulating scope (like
                  # let*), so a later init can reference an earlier binding —
                  # matching Clojure's loop.
                  seq-bindings @{}]
              (table/setproto seq-bindings bindings)
              (var i 0)
              (while (< i (length bind-vec))
                # loop* is a primitive (the loop macro desugars destructuring);
                # its binding names must be plain symbols, as in Clojure.
                (unless (plain-sym? (bind-vec i)) (error "Bad binding form, expected symbol"))
                (def v (eval-form ctx seq-bindings (bind-vec (+ i 1))))
                (bind-put seq-bindings ((bind-vec i) :name) v)
                (array/push init-vals v)
                (array/push patterns (bind-vec i))
                (+= i 2))
              (var loop-fn nil)
              (set loop-fn (fn [& args]
                (var loop-bindings @{})
                (table/setproto loop-bindings bindings)
                (var j 0)
                (each pat patterns
                  (destructure-bind ctx loop-bindings pat (in args j))
                  (++ j))
                (put loop-bindings :jolt/loop-fn loop-fn)
                (var result nil)
                (each body-form body
                  (set result (eval-form ctx loop-bindings body-form)))
                result))
              (apply loop-fn init-vals))
    "recur" (let [loop-fn (get bindings :jolt/loop-fn)]
              (if (nil? loop-fn)
                (error "recur used outside of loop* or fn*")
                (let [args (map |(eval-form ctx bindings $) (tuple/slice form 1))]
                  (apply loop-fn args))))
    "throw" (let [val (eval-form ctx bindings (in form 1))]
              (error {:jolt/type :jolt/exception :value val}))
    "try" (let [# The body is EVERY form between `try` and the first catch/finally
                # clause (not just form 1 — a multi-form body before the clauses,
                # e.g. (try (foo) (bar) (catch …)), dropped all but the first).
                forms (tuple/slice form 1)
                clause? (fn [c]
                          (and (array? c) (> (length c) 0)
                               (struct? (first c)) (= :symbol ((first c) :jolt/type))
                               (or (= "catch" ((first c) :name))
                                   (= "finally" ((first c) :name)))))
                split (do (var k 0)
                          (while (and (< k (length forms)) (not (clause? (in forms k)))) (++ k))
                          k)
                body-forms (tuple/slice forms 0 split)
                clauses (tuple/slice forms split)
                # current-ns is dynamic state. The interpreter rebinds it to a
                # fn's defining ns while that fn runs and restores it on normal
                # return, but a fn that THROWS unwinds past its own restore — so
                # the ns can leak. try is the unwind boundary: restore the ns that
                # was current at try entry before running catch/finally, so caught
                # code (and the harness's is/thrown?) sees the right namespace.
                try-ns (ctx-current-ns ctx)]
            (var catch-sym nil)
            (var catch-body nil)
            (var finally-body nil)
            (each clause clauses
              (when (and (array? clause) (> (length clause) 0))
                (let [head (first clause)]
                  (when (and (struct? head) (= :symbol (head :jolt/type)))
                    (match (head :name)
                      "catch" (do
                        (set catch-sym (in clause 2))
                        (set catch-body (tuple/slice clause 3)))
                      "finally" (set finally-body (tuple/slice clause 1)))))))
            (defn eval-body []
              (var result nil)
              (each bf body-forms (set result (eval-form ctx bindings bf)))
              result)
            (defn run-finally []
              (when finally-body
                (each fb finally-body (eval-form ctx bindings fb))))
            (defn run-protected []
              (if catch-sym
                (try
                  (eval-body)
                  ([err]
                   (ctx-set-current-ns ctx try-ns)
                   (var new-bindings @{})
                   (table/setproto new-bindings bindings)
                   # bind the originally-thrown value (unwrap the :jolt/exception
                   # envelope) so (catch … e (throw e)) rethrows the same value
                   # rather than nesting another envelope
                   (def caught
                     (if (and (or (table? err) (struct? err)) (= :jolt/exception (get err :jolt/type)))
                       (get err :value)
                       err))
                   (put new-bindings (catch-sym :name) caught)
                   (var result nil)
                   (each cb catch-body
                     (set result (eval-form ctx new-bindings cb)))
                   result))
                # no catch: restore the ns on an unwinding error, then re-raise
                (try (eval-body) ([err] (ctx-set-current-ns ctx try-ns) (error err)))))
            # finally ALWAYS runs (success, caught error, or rethrow) — defer so it
            # fires even if a catch body throws. Without a finally, just run.
            (if finally-body
              (defer (run-finally) (run-protected))
              (run-protected)))
    "set!" (let [target (in form 1)
                  val (eval-form ctx bindings (in form 2))]
              # Handle (set! (.-field obj) val) — .-field shorthand as a list
              (if (and (array? target) (> (length target) 1)
                       (struct? (first target)) (= :symbol ((first target) :jolt/type))
                       (> (length ((first target) :name)) 1)
                       (= (string/slice ((first target) :name) 0 2) ".-"))
                (let [obj (eval-form ctx bindings (in target 1))
                      field-name (string/slice ((first target) :name) 2)
                      field-key (keyword field-name)]
                  (if (get obj :jolt/deftype)
                    (do (put obj field-key val) val)
                    (error (string "Can't set! field on non-deftype: " (type obj)))))
                # (set! (. obj -field) val) — instance field mutation
                (if (and (array? target) (> (length target) 0)
                         (struct? (first target))
                         (= :symbol ((first target) :jolt/type))
                         (= "." ((first target) :name)))
                  (let [obj (eval-form ctx bindings (in target 1))
                        field-sym (in target 2)
                        field-name (field-sym :name)
                        field-key (keyword (if (and (> (length field-name) 0) (= "-" (string/slice field-name 0 1)))
                                           (string/slice field-name 1)
                                           field-name))]
                    (if (get obj :jolt/deftype)
                      (do (put obj field-key val) val)
                      (error (string "Can't set! field on non-deftype: " (type obj)))))
                  # (set! var val) — normal var mutation
                  (let [target-sym target
                        v (resolve-var ctx bindings target-sym)]
                    (if v
                      (do (var-set v val) val)
                      # Auto-create var if it doesn't exist
                      (let [ns-name (ctx-current-ns ctx)
                            ns (ctx-find-ns ctx ns-name)]
                        (def new-v (ns-intern ns (target-sym :name) val))
                        val))))))
    "var" (let [target-sym (in form 1)
                 v (resolve-var ctx bindings target-sym)]
             (if v v (error (string "Unable to resolve var: " (sym-name-str target-sym) " in var"))))
    # var-get/var-set/var?/alter-var-root/alter-meta!/reset-meta! are plain
    # clojure.core fns; find-var/intern are ctx-capturing clojure.core fns
    # (install-stateful-fns!) — they fall through to the function-call default
    # and compile as ordinary invokes (Stage 2 tier 6).
    # set?/disj are plain clojure.core fns now (core-set?/core-disj) — no longer
    # special-cased here, the analyzer, or compiler.janet (jolt-g3h).
    # protocol-dispatch / register-method / make-reified are now ordinary
    # clojure.core fns (install-stateful-fns!) — the defprotocol/extend-type/reify
    # macros call them with name STRINGS, so they compile + interpret as plain
    # invokes (no special-form arms).
    # satisfies?/instance?/locking and the multimethod table ops
    # (prefer-method/remove-method/remove-all-methods/get-method/methods) are
    # clojure.core fns / overlay macros now (Stage 2 tier 6c) — no special arms.
    # deftype is now a macro (30-macros) over make-deftype-ctor + extend-type —
    # compiles as a plain (do …); no special-form arm.
    "new" (let [type-sym (in form 1)
                args (map |(eval-form ctx bindings $) (tuple/slice form 2))
                ctor (eval-form ctx bindings type-sym)
                ctor (if (string? ctor) (or (ctor-for-class-token ctor) ctor) ctor)]
            (apply ctor args))
    "." (let [target (eval-form ctx bindings (in form 1))
              member-raw (in form 2)
              # Resolve member name: symbols have :name, keywords use string, strings as-is
              member-name (if (and (struct? member-raw) (= :symbol (member-raw :jolt/type)))
                           (member-raw :name)
                           (if (keyword? member-raw)
                             (string member-raw)
                             member-raw))
              field-name (if (and (string? member-name) (> (length member-name) 0) (= "-" (string/slice member-name 0 1)))
                          (string/slice member-name 1)
                          member-name)]
          (if (> (length form) 3)
            # method call: (. obj method args...)
            (let [args (map |(eval-form ctx bindings $) (tuple/slice form 3))]
              (if (or (string? target) (buffer? target))
                (let [m (get string-methods field-name)]
                  (if m
                    (m (string target) ;args)
                    (if-let [om (get object-methods field-name)]
                      (om (string target) ;args)
                      (error (string "Unsupported String method ." field-name)))))
              (if (and (number? target) (get number-methods field-name))
                ((get number-methods field-name) target ;args)
              (if (and (get object-methods field-name)
                       (not (and (table? target) (get tagged-methods (get target :jolt/type)))))
                ((get object-methods field-name) target ;args)
              # registered shim objects (java.time etc.): tag-keyed method tables
              (if (and (or (table? target) (struct? target))
                       (get tagged-methods (get target :jolt/type)))
                (let [m (get (get tagged-methods (get target :jolt/type)) field-name)]
                  (if m
                    (m target ;args)
                    (error (string "Unsupported method ." field-name " on " (string (get target :jolt/type))))))
              (if (record-tag target)
                # deftype/reify methods live in the protocol registry (or the
                # instance's reified-fns table), not on the instance. get is safe
                # on a shape-rec tuple (returns nil for the method/protocol keys).
                (let [method-key (keyword field-name)
                      own (get target method-key)
                      reified (get (get target :jolt/protocol-methods) method-key)
                      m (cond
                          (or (function? own) (cfunction? own)) own
                          (or (function? reified) (cfunction? reified)) reified
                          (find-method-any-protocol ctx (record-tag target) field-name))]
                  (if m
                    (apply m target args)
                    (error (string "No method ." field-name " on " (record-tag target)))))
                # Janet-native interop: try field lookup + call
                (if (or (table? target) (struct? target))
                  (let [method (get target (keyword field-name))]
                    (if (or (function? method) (cfunction? method))
                      (method target ;args)
                      # If stored as fn* form (array), compile to function then call
                      (if (array? method)
                        (let [method-fn (eval-form ctx bindings method)]
                          (if (or (function? method-fn) (cfunction? method-fn))
                            (method-fn target ;args)
                            (error (string "Cannot call non-function " field-name " on " (type target)))))
                        (error (string "Cannot call non-function " field-name " on " (type target))))))
                  (error (string "Cannot call method " field-name " on " (type target))))))))))
            # (. obj member) with no extra args: a symbol member naming a
            # function is a zero-arg method call (receiver passed as self);
            # a keyword or `-field` member is plain field access. Strings get
            # the java.lang.String surface (clj-compat: (.toLowerCase s) ...).
            (if (or (string? target) (buffer? target))
              (let [m (get string-methods field-name)]
                (if m
                  (m (string target))
                  (if-let [om (get object-methods field-name)]
                    (om (string target))
                    (error (string "Unsupported String method ." field-name)))))
            (if (and (number? target) (get number-methods field-name))
              ((get number-methods field-name) target)
            (if (and (get object-methods field-name)
                     (not (and (table? target) (get tagged-methods (get target :jolt/type))
                               (get (get tagged-methods (get target :jolt/type)) field-name))))
              ((get object-methods field-name) target)
            (if (and (or (table? target) (struct? target))
                     (get tagged-methods (get target :jolt/type))
                     (get (get tagged-methods (get target :jolt/type)) field-name))
              ((get (get tagged-methods (get target :jolt/type)) field-name) target)
            (let [v (if (record-tag target)
                      (coll-lookup target (keyword field-name) nil)
                      (get target (keyword field-name)))]
              (if (and (struct? member-raw) (= :symbol (member-raw :jolt/type))
                       (not (string/has-prefix? "-" member-name)))
                (cond
                  (or (function? v) (cfunction? v)) (v target)
                  # zero-arg deftype/reify method via the protocol registry
                  (record-tag target)
                    (let [reified (get (get target :jolt/protocol-methods) (keyword field-name))
                          m (if (or (function? reified) (cfunction? reified)) reified
                              (find-method-any-protocol ctx (record-tag target) field-name))]
                      (if m (m target) v))
                  # value stored as an unevaluated fn* form: compile then call
                  (array? v) (let [f (eval-form ctx bindings v)]
                               (if (or (function? f) (cfunction? f)) (f target) f))
                  v)
                v))))))))
    # default: function application — check for macros
    (if (and (struct? first-form) (= :symbol (first-form :jolt/type)))
      (let [sym-name (first-form :name)]
        # Handle .-fieldName accessor: (.-cnt obj) → (. obj -cnt)
        (if (and (> (length sym-name) 1) (= (string/slice sym-name 0 2) ".-")
                 (> (length form) 1))
          (let [field-name (string/slice sym-name 2)
                target (eval-form ctx bindings (in form 1))]
            (get target (keyword field-name)))
        # (.method obj args...) sugar -> (. obj method args...): desugar and
        # re-enter the dot special form (which holds the String surface, the
        # deftype method path, and the map-fn fallback).
        (if (and (> (length sym-name) 1)
                 (= (string/slice sym-name 0 1) ".")
                 (not= sym-name "..")
                 (> (length form) 1))
          (eval-form ctx bindings
                     (array/concat @[{:jolt/type :symbol :ns nil :name "."}
                                     (in form 1)
                                     {:jolt/type :symbol :ns nil :name (string/slice sym-name 1)}]
                                   (tuple/slice form 2)))
        # Handle ClassName. constructor syntax (".." is the member-threading
        # macro, not a constructor named ".")
        (if (and (> (length sym-name) 1) (not= sym-name "..")
                 (= (sym-name (- (length sym-name) 1)) 46))
          (let [type-name (string/slice sym-name 0 (- (length sym-name) 1))
                type-sym {:jolt/type :symbol :ns (first-form :ns) :name type-name}
                ctor (eval-form ctx bindings type-sym)
                # class names evaluate to canonical-name STRINGS now; the
                # constructor itself comes from the ctor registry
                ctor (if (string? ctor) (or (ctor-for-class-token ctor) ctor) ctor)
                args (map |(eval-form ctx bindings $) (tuple/slice form 1))]
            (apply ctor args))
          (let [v (resolve-var ctx bindings first-form)]
            (if (and v (var-macro? v))
              # Expand once (cached by call-form identity), then evaluate the
              # macro-free expansion with the current bindings each call.
              (let [cached (in macro-cache form)]
                (if (not (nil? cached))
                  (eval-form ctx bindings cached)
                  (let [expanded (apply (var-get v) (tuple/slice form 1))]
                    (put macro-cache form expanded)
                    (eval-form ctx bindings expanded))))
              (let [f (eval-form ctx bindings first-form)
                    args (map |(eval-form ctx bindings $) (tuple/slice form 1))]
                (jolt-invoke ctx f args))))))))
      (let [f (eval-form ctx bindings first-form)
            args (map |(eval-form ctx bindings $) (tuple/slice form 1))]
        (jolt-invoke ctx f args)))))

# Build a map value from an array of evaluated [k v k v ...]. A phm (not a Janet
# struct) is used when a key is a collection (value-based hashing) OR a key/value
# is nil (Janet structs drop nil; phm preserves it, matching Clojure). The common
# scalar/nil-free case stays a struct.
(defn- map-needs-phm? [kvs]
  (var need false) (var i 0)
  (while (< i (length kvs))
    (let [k (in kvs i) v (in kvs (+ i 1))]
      (when (or (table? k) (array? k) (nil? k) (nil? v)) (set need true) (break)))
    (+= i 2))
  need)

(defn- build-eval-map [kvs]
  (if (map-needs-phm? kvs)
    (do (var m (make-phm)) (var j 0)
        (while (< j (length kvs)) (set m (phm-assoc m (in kvs j) (in kvs (+ j 1)))) (+= j 2)) m)
    (struct ;kvs)))

(set eval-form (fn [ctx bindings form]
  (cond
    (nil? form) nil
    (number? form) form
    (string? form) form
    (keyword? form) form
    (bytes? form) form
    (buffer? form) form
    (tuple? form)
      (let [els (map |(eval-form ctx bindings $) form)]
        (if mutable? (array ;els) (pv-from-indexed els)))
    (struct? form)
    (if (= :symbol (form :jolt/type))
      (resolve-sym ctx bindings form)
      (if (= :jolt/char (form :jolt/type))
        form
      # a UUID/inst value flowing back through eval (macro expansion, eval of a
      # read form) is a self-evaluating literal, like chars. A namespace object
      # does too: `~*ns*` in a syntax-quote (clojure.tools.logging) splices the
      # live ns into the expansion.
      (if (or (= :jolt/uuid (form :jolt/type)) (= :jolt/inst (form :jolt/type))
              (= :jolt/namespace (form :jolt/type)))
        form
      (if (= :jolt/set (form :jolt/type))
        # evaluate each element (set literals like #{(inc 1)} must compute)
        (apply make-phs (map |(eval-form ctx bindings $) (form :value)))
      (if (= :jolt/tagged (form :jolt/type))
        (let [tag (form :tag)
              data-readers (get (ctx :env) :data-readers)
              reader-fn (if data-readers (get data-readers tag))]
          (cond
            # #"..." regex literal -> a regex value (Janet PEG-backed)
            (= tag :regex) (compile-regex (form :form))
            reader-fn (reader-fn (form :form))
            (error (string "No reader function for tag " tag))))
      (if (get form :jolt/type)
        (error (string "Unexpected tagged form: " (form :jolt/type)))
        # plain map literal: evaluate keys and values in SOURCE order when
        # the reader order rides along (jolt-p3c), hash order otherwise
        (let [kvs @[]
              order (form-kv-order form)]
          (if order
            (each x order (array/push kvs (eval-form ctx bindings x)))
            (each k (keys form)
              (array/push kvs (eval-form ctx bindings k))
              (array/push kvs (eval-form ctx bindings (get form k)))))
          (build-eval-map kvs))))))))
    # A phm map-literal FORM (reader emits one for {:a nil} etc., which a struct
    # would have dropped): evaluate its key/value forms and rebuild, preserving nil.
    (phm? form)
    (let [kvs @[]
          order (form-kv-order form)]
      (if order
        (each x order (array/push kvs (eval-form ctx bindings x)))
        (each e (phm-entries form)
          (array/push kvs (eval-form ctx bindings (in e 0)))
          (array/push kvs (eval-form ctx bindings (in e 1)))))
      (build-eval-map kvs))
    (array? form)
    (if (= 0 (length form))
      @[]
      (eval-list ctx bindings form))
    form)))
