# Jolt Evaluator
# Direct interpreter for Clojure forms on Janet.

(use ./types)
(use ./phm)
(use ./pv)
(use ./plist)
(use ./config)
(use ./reader)
(use ./regex)

(defn- sym-name?
  [sym-s name-str]
  (and (struct? sym-s) (= :symbol (sym-s :jolt/type)) (= name-str (sym-s :name))))

(defn- special-symbol?
  [name]
  (or (= name "quote") (= name "syntax-quote") (= name "unquote")
      (= name "unquote-splicing") (= name "do") (= name "if")
      (= name "def") (= name "defmacro") (= name "fn*") (= name "let*") (= name "loop*")
      (= name "recur") (= name "throw") (= name "try")
      (= name "set!") (= name "var") (= name "locking")
      (= name "eval")
      (= name "instance?") (= name "defmulti") (= name "defmethod")
      (= name "deftype") (= name "new") (= name ".")
      (= name "var-get") (= name "var-set") (= name "var?")
      (= name "alter-var-root") (= name "find-var") (= name "intern")
      (= name "alter-meta!") (= name "reset-meta!")
      (= name "disj") (= name "set?")
      (= name "satisfies?")
      (= name "protocol-dispatch") (= name "register-method") (= name "make-reified")
      (= name "prefer-method") (= name "remove-method") (= name "remove-all-methods")
      (= name "get-method") (= name "methods")))

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
    (keyword? f) (coll-lookup (get args 0) f (get args 1))
    (and (struct? f) (= :symbol (f :jolt/type)))
      (coll-lookup (get args 0) f (get args 1))
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
    (struct? f)
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
  (per-expansion, via gsmap); special forms and clojure.core names are left
  unqualified (they resolve via the core fallback); other symbols are qualified
  to the current namespace so they resolve when the macro is used elsewhere."
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
        (ns-find (ctx-find-ns ctx "clojure.core") nm) form
        {:jolt/type :symbol :ns (ctx-current-ns ctx) :name nm}))
    form))

(defn- d-realize
  "Realize a lazy-seq to an array for positional destructuring / splicing; pass
  others (pvec/plist coerced to array, everything else unchanged)."
  [val]
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
                  (if (nil? rt) (set go false) (set cur (make-lazy-seq rt))))))))
      items)
    val))))

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
    (and (struct? form) (get form :jolt/type)) form
    (struct? form)
    (do (var kvs @[]) (each k (keys form)
      (array/push kvs (syntax-quote* ctx bindings k gsmap))
      (array/push kvs (syntax-quote* ctx bindings (get form k) gsmap))) (struct ;kvs))
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
      # tagged structs (sets/chars): syntax-quote* returns them as-is (no recursion)
      (and (struct? form) (get form :jolt/type))
      @[(sqsym* "quote") form]
      (struct? form)
      (do (var parts @[(sqsym* "__sqmap")])
          (each k (keys form)
            (array/push parts (syntax-quote-lower ctx k gsmap))
            (array/push parts (syntax-quote-lower ctx (get form k) gsmap)))
          parts)
      @[(sqsym* "quote") form])))

(defn resolve-var
  [ctx bindings sym-s]
  (let [name (sym-s :name) ns (sym-s :ns)]
    (if (not (nil? ns))
      # Resolve ns aliases (e.g. `p/thrown?` where `p` is a require :as alias)
      # so that aliased macros are recognized as macros, matching resolve-sym.
      (let [current-ns (ctx-find-ns ctx (ctx-current-ns ctx))
            aliased-ns (ns-import-lookup current-ns ns)
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
  "Parse and evaluate every form of a namespace's source in the given context."
  [ctx src]
  (var s src)
  (while (> (length (string/trim s)) 0)
    (def [f r] (parse-next s))
    (set s r)
    (when (not (nil? f)) (eval-form ctx @{} f))))

(defn- maybe-require-ns
  "If namespace ns-name isn't populated yet, load its source — from a file on the
  context's source roots, else from the stdlib baked into the image. Restores the
  current namespace afterwards (a library's own `ns` form, or our manual switch
  for ns-form-less stdlib files, changes it). No-op for already-loaded namespaces."
  [ctx ns-name]
  (let [ns (ctx-find-ns ctx ns-name)]
    (when (and (= 0 (length (ns :mappings))) (not= ns-name "clojure.core"))
      (let [path (find-ns-file ctx ns-name)
            embedded (get (get (ctx :env) :embedded-sources @{}) ns-name)
            stdlib? (not (nil? embedded))]
        (when (or path embedded)
          (let [saved (ctx-current-ns ctx)]
            # Stdlib files have no `ns` form, so switch into the target ns first
            # (their defs intern there); a library's own `ns` form overrides this.
            (ctx-set-current-ns ctx ns-name)
            (if path (load-ns-source ctx (slurp path)) (load-ns-source ctx embedded))
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
        (ns-import current-ns alias ns-name)))
    (when refer-syms
      (let [source-ns (ctx-find-ns ctx ns-name)
            target-ns (ctx-find-ns ctx (ctx-current-ns ctx))]
        (each refer-sym refer-syms
          (let [name (if (struct? refer-sym) (refer-sym :name) refer-sym)
                v (ns-find source-ns name)]
            (when v
              # Preserve macro-ness: a referred macro must stay a macro, so copy
              # the :macro flag onto the interned var (not just its value).
              (let [nv (ns-intern target-ns name (var-get v))]
                (when (get v :macro) (put nv :macro true))))))))
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

(defn- resolve-sym
  [ctx bindings sym-s]
  (let [name (sym-s :name) ns (sym-s :ns)]
    (if (= ns "Math")
      (let [v (get math-statics name)]
        (if (nil? v) (error (string "Unsupported Math member: Math/" name)) v))
    (if (not (nil? ns))
      (let [current-ns (ctx-find-ns ctx (ctx-current-ns ctx))
            aliased-ns (ns-import-lookup current-ns ns)
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
                  entry (in (fiber/getenv (fiber/current)) (symbol jname))]
              (if (not (nil? entry))
                (if (table? entry) (entry :value) entry)
                (error (string "Unable to resolve Janet symbol: " jname))))
            (error (string "Unable to resolve symbol: " ns "/" name)))))
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
                      # Fall back to Janet's global environment
                      (let [root-env (fiber/getenv (fiber/current))
                            entry (in root-env (symbol name))]
                        (if (not (nil? entry))
                          (if (table? entry) (entry :value) entry)
                          (error (string "Unable to resolve symbol: " name))))))))))))))))

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
   "IPersistentSet" true "IPersistentCollection" true "ISeq" true "Atom" true "nil" true})

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
    (plist? obj) ["PersistentList" "IPersistentList" "IPersistentCollection" "ISeq" "Object"]
    (or (tuple? obj) (array? obj) (pvec? obj)) ["PersistentVector" "IPersistentVector" "IPersistentCollection" "ISeq" "Object"]
    (or (function? obj) (cfunction? obj)) ["IFn" "Fn" "Object"]
    (nil? obj) ["nil" "Object"]
    ["Object"]))

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
    "read-string" (parse-string (eval-form ctx bindings (in form 1)))
    "defonce" (let [name-sym (unwrap-meta-name (in form 1))
                    ns (ctx-find-ns ctx (ctx-current-ns ctx))
                    existing (ns-find ns (name-sym :name))]
                (if (and existing (not (nil? (get existing :root))))
                  existing
                  (eval-form ctx bindings @[{:jolt/type :symbol :ns nil :name "def"}
                                            (in form 1) (in form 2)])))
    "macroexpand-1" (let [the-form (eval-form ctx bindings (in form 1))]
                      (if (and (array? the-form) (> (length the-form) 0)
                               (struct? (first the-form)) (= :symbol ((first the-form) :jolt/type)))
                        (let [v (resolve-var ctx bindings (first the-form))]
                          (if (and v (var-macro? v))
                            (apply (var-get v) (tuple/slice the-form 1))
                            the-form))
                        the-form))
    "do" (do
           (var result nil)
           (var i 1)
           (let [len (length form)]
             (while (< i len)
               (set result (eval-form ctx bindings (in form i)))
               (++ i)))
           result)
    "if" (let [test-val (eval-form ctx bindings (in form 1))]
           (if (and (not (nil? test-val)) (not (= false test-val)))
             (eval-form ctx bindings (in form 2))
             (if (> (length form) 3) (eval-form ctx bindings (in form 3)) nil)))
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
                v (ns-intern ns (name-sym :name))
                # (def name docstring value): docstring is form 2, value form 3
                has-doc (and (> (length form) 3) (string? (in form 2)))
                val (eval-form ctx bindings (in form (if has-doc 3 2)))]
            (bind-root v val)
            (let [extra (if has-doc (merge name-meta {:doc (in form 2)}) name-meta)]
              (when (not (empty? extra))
                (put v :meta (merge (or (get v :meta) {}) extra))))
            (when dynamic?
              (put v :dynamic true))
            # def returns the var (Clojure semantics); REPL prints #'ns/name
            v)
    "defmacro" (let [name-sym (in form 1)
                     rest-form (tuple/slice form 2)
                     # optional docstring
                     has-doc? (and (> (length rest-form) 0) (string? (first rest-form)))
                     args-form (if has-doc? (in rest-form 1) (first rest-form))
                     body (tuple/slice rest-form (if has-doc? 2 1))
                     param-info (parse-params args-form)
                     fixed-pats (param-info :fixed)
                     rest-pat (param-info :rest)
                     defining-ns (ctx-current-ns ctx)]
                 (def interp-fn (fn [& macro-args]
                   (var new-bindings @{})
                   (table/setproto new-bindings bindings)
                   (put new-bindings "&env" @{})  # implicit &env for macro bodies (table — nil-safe)
                   (var i 0)
                   # Destructure macro params (like fn), so [& [a & more :as all]]
                   # and {:keys …} rest forms work in macro arglists.
                   (each pat fixed-pats
                     (destructure-bind ctx new-bindings pat (macro-args i))
                     (++ i))
                   (when rest-pat
                     (destructure-bind ctx new-bindings rest-pat (tuple/slice macro-args i)))
                   # Use defining namespace for symbol resolution
                   (def saved-ns (ctx-current-ns ctx))
                   (ctx-set-current-ns ctx defining-ns)
                   (var result nil)
                   (each bf body
                     (set result (eval-form ctx new-bindings bf)))
                   (ctx-set-current-ns ctx saved-ns)
                   result))
                 # Prefer a COMPILED expander (native-speed expansion, zero runtime
                 # cost). Skip when the body uses &env/&form (the compiled fn has no
                 # such params) — those fall back to the interpreted closure.
                 (def uses-env (or (form-uses-sym? body "&env") (form-uses-sym? body "&form")))
                 (def compiled-fn
                   (when (and macro-compile-hook (not uses-env))
                     (macro-compile-hook ctx args-form body)))
                 (def macro-fn (or compiled-fn interp-fn))
                  (let [ns-name (ctx-current-ns ctx)
                       ns (ctx-find-ns ctx ns-name)]
                   (def v (ns-intern ns (name-sym :name) macro-fn))
                   (put v :macro true)
                   # A (re)defined macro invalidates any cached expansions.
                   (table/clear macro-cache)
                   (var-get v)))
    "ns" (let [raw-name (in form 1)
               name-sym (unwrap-meta-name raw-name)
               ns-name (sym-name-str name-sym)
               clauses (tuple/slice form 2)]
           (ctx-set-current-ns ctx ns-name)
           (ctx-find-ns ctx ns-name)
            (var result nil)
            (var i 0)
            (let [clen (length clauses)]
              (while (< i clen)
                (let [clause (in clauses i)
                      head (if (and (array? clause) (> (length clause) 0)) (first clause) nil)]
                  (if (nil? head)
                    (do (set result clause) (++ i))
                    (match head
                      :require (let [specs (tuple/slice clause 1)
                                     slen (length specs)]
                                 (var j 0)
                                 (while (< j slen)
                                   (let [s (in specs j)]
                                     (when s (eval-require ctx s)))
                                   (++ j))
                                 (set i (+ i 1)))
                      :use (let [specs (tuple/slice clause 1)
                                 slen (length specs)]
                             (var j 0)
                             (while (< j slen)
                               (let [s (in specs j)
                                     ns-sym (if (array? s) (in s 0) s)
                                     ns-name (sym-name-str ns-sym)
                                     source-ns (ctx-find-ns ctx ns-name)
                                     target-ns (ctx-find-ns ctx ns-name)]
                                 (loop [[sym v] :pairs (source-ns :mappings)]
                                   (ns-intern target-ns sym (var-get v))))
                               (++ j))
                             (set i (+ i 1)))
                      :refer-clojure (let [spec (in clause 1)]
                                       (when (and (array? spec) (= (first spec) :exclude))
                                         (let [ns (ctx-find-ns ctx ns-name)]
                                           (each sym (tuple/slice spec 1)
                                             (ns-unmap ns (if (struct? sym) (sym :name) sym)))))
                                       (set i (+ i 1)))
                      :import (let [specs (tuple/slice clause 1)
                                    slen (length specs)]
                                (var j 0)
                                (while (< j slen)
                                  (let [class-spec (in specs j)
                                        class-name (if (struct? class-spec) (class-spec :name) (string class-spec))
                                        last-dot (do
                                                  (var idx -1)
                                                  (var pos 0)
                                                  (while (< pos (length class-name))
                                                    (if (= (class-name pos) 46) (set idx pos))
                                                    (++ pos))
                                                  idx)
                                        short-name (if (>= last-dot 0)
                                                    (string/slice class-name (+ last-dot 1))
                                                    class-name)]
                                    (ns-import (ctx-find-ns ctx ns-name) short-name class-name))
                                  (++ j))
                                (set i (+ i 1)))
                      (do (set result clause) (++ i)))))))
           result)
    "require" (let [spec0 (eval-form ctx bindings (in form 1))
                    spec (if (pvec? spec0) (pv->array spec0) spec0)]
                 (if (and (indexed? spec) (> (length spec) 0))
                   (eval-require ctx spec)
                   (error "require expects a vector spec")))
    "all-ns" (all-ns ctx)
    "the-ns" (the-ns ctx)
    "create-ns" (create-ns ctx (sym-name-str (in form 1)))
    "remove-ns" (remove-ns ctx (sym-name-str (in form 1)))
    "ns-interns" (let [ns (ctx-find-ns ctx (ctx-current-ns ctx))] (ns :mappings))
    "ns-aliases" (let [ns (ctx-find-ns ctx (ctx-current-ns ctx))] (ns :aliases))
    "ns-imports" (let [ns (ctx-find-ns ctx (ctx-current-ns ctx))] (ns :imports))
    "ns-resolve" (ns-resolve (ctx-find-ns ctx (ctx-current-ns ctx)) (in form 1))
    "in-ns" (let [sym (eval-form ctx bindings (in form 1))
                  ns-name (if (and (struct? sym) (= :symbol (sym :jolt/type))) (sym :name) (string sym))]
              (ctx-find-ns ctx ns-name)
              (ctx-set-current-ns ctx ns-name)
              nil)
    "resolve" (let [sym (eval-form ctx bindings (in form 1))]
                (if (and (struct? sym) (= :symbol (sym :jolt/type)))
                  (let [r (protect (resolve-var ctx bindings sym))]
                    (if (= (r 0) true) (r 1) nil))
                  nil))
    "find-ns" (let [sym (eval-form ctx bindings (in form 1))
                    nm (if (and (struct? sym) (= :symbol (sym :jolt/type))) (sym :name) (string sym))]
                (get (get (ctx :env) :namespaces) nm))
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
                       fixed-pats (param-info :fixed)
                       rest-pat (param-info :rest)
                       n-fixed (length fixed-pats)
                       f (fn [& fn-args]
                          (var fn-bindings @{})
                          (table/setproto fn-bindings bindings)
                          (var i 0)
                          (each pat fixed-pats
                            (destructure-bind ctx fn-bindings pat (fn-args i))
                            (++ i))
                          (when rest-pat
                            (destructure-bind ctx fn-bindings rest-pat (tuple/slice fn-args i)))
                          (put fn-bindings :jolt/loop-fn self)
                          (when fn-name (bind-put fn-bindings fn-name self))
                          # Use defining namespace for symbol resolution
                          (def saved-ns (ctx-current-ns ctx))
                          (ctx-set-current-ns ctx defining-ns)
                          (var result nil)
                          (each body-form body
                            (set result (eval-form ctx fn-bindings body-form)))
                          (ctx-set-current-ns ctx saved-ns)
                          result)]
                   (if rest-pat
                     (do (set variadic-fn f) (set variadic-min n-fixed))
                     (put arities n-fixed f))))
               (set self (fn [& fn-args]
                 (let [n (length fn-args)
                       f (get arities n)]
                   (cond
                     f (apply f fn-args)
                     (and variadic-fn (>= n variadic-min)) (apply variadic-fn fn-args)
                     (error (string "Wrong number of args (" n ") passed to fn"))))))
               self)
             # Single-arity: (fn* [args] body...)
             (let [args-form (in form 1)
                   body (tuple/slice form 2)
                   param-info (parse-params args-form)
                   fixed-pats (param-info :fixed)
                   rest-pat (param-info :rest)
                   defining-ns (ctx-current-ns ctx)]
               (var self nil)
               (set self (fn [& fn-args]
                 (var fn-bindings @{})
                 (table/setproto fn-bindings bindings)
                 (var i 0)
                 (each pat fixed-pats
                   (destructure-bind ctx fn-bindings pat (fn-args i))
                   (++ i))
                 (when rest-pat
                   (destructure-bind ctx fn-bindings rest-pat (tuple/slice fn-args i)))
                 (put fn-bindings :jolt/loop-fn self)
                 (when fn-name (bind-put fn-bindings fn-name self))
                 # Use defining namespace for symbol resolution
                 (def saved-ns (ctx-current-ns ctx))
                 (ctx-set-current-ns ctx defining-ns)
                 (var result nil)
                 (each body-form body
                   (set result (eval-form ctx fn-bindings body-form)))
                 (ctx-set-current-ns ctx saved-ns)
                 result))
              self)))
    "let*" (let [bind-vec (in form 1)
                  body (tuple/slice form 2)]
              (var new-bindings @{})
              (table/setproto new-bindings bindings)
              (var i 0)
              (let [len (length bind-vec)]
                (while (< i len)
                  (let [pat (bind-vec i)]
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
                  patterns @[]]
              (var i 0)
              (while (< i (length bind-vec))
                (array/push init-vals (eval-form ctx bindings (bind-vec (+ i 1))))
                # keep the binding form (symbol OR destructuring pattern)
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
    "try" (let [body-form (in form 1)
                clauses (tuple/slice form 2)
                n (length clauses)
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
            (var i 0)
            (while (< i n)
              (let [clause (in clauses i)]
                (if (and (array? clause) (> (length clause) 0))
                  (let [head (first clause)]
                    (if (and (struct? head) (= :symbol (head :jolt/type)))
                      (match (head :name)
                        "catch" (do
                          (set catch-sym (in clause 2))
                          (set catch-body (tuple/slice clause 3)))
                        "finally" (set finally-body (tuple/slice clause 1)))))))
              (++ i))
            (defn run-finally [f]
              (when f
                (each fb f (eval-form ctx bindings fb))))
            (if catch-sym
              (try
                (eval-form ctx bindings body-form)
                ([err]
                 (ctx-set-current-ns ctx try-ns)
                 (var new-bindings @{})
                 (table/setproto new-bindings bindings)
                 # bind the originally-thrown value (unwrap the :jolt/exception
                 # envelope) so (catch ... e (throw e)) rethrows the same value
                 # rather than nesting another envelope
                 (def caught
                   (if (and (or (table? err) (struct? err)) (= :jolt/exception (get err :jolt/type)))
                     (get err :value)
                     err))
                 (put new-bindings (catch-sym :name) caught)
                 (var result nil)
                 (each cb catch-body
                   (set result (eval-form ctx new-bindings cb)))
                 (run-finally finally-body)
                 result))
              (if finally-body
                (try
                  (do
                    (def result (eval-form ctx bindings body-form))
                    (run-finally finally-body)
                    result)
                  ([err]
                   (ctx-set-current-ns ctx try-ns)
                   (run-finally finally-body)
                   (error err)))
                (eval-form ctx bindings body-form))))
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
    "var-get" (var-get (eval-form ctx bindings (in form 1)))
    "var-set" (var-set (eval-form ctx bindings (in form 1))
                       (eval-form ctx bindings (in form 2)))
    "var?" (var? (eval-form ctx bindings (in form 1)))
    "alter-var-root" (alter-var-root (eval-form ctx bindings (in form 1))
                                      (eval-form ctx bindings (in form 2)))
    "find-var" (find-var ctx (eval-form ctx bindings (in form 1)))
    "alter-meta!" (let [v (eval-form ctx bindings (in form 1))
                         f (eval-form ctx bindings (in form 2))
                         args (map |(eval-form ctx bindings $) (tuple/slice form 3))]
                    (apply alter-meta! v f args))
    "reset-meta!" (reset-meta! (eval-form ctx bindings (in form 1))
                                (eval-form ctx bindings (in form 2)))
    "intern" (let [ns-name (eval-form ctx bindings (in form 1))
                   sym-name (eval-form ctx bindings (in form 2))
                   val (eval-form ctx bindings (in form 3))
                   ns (ctx-find-ns ctx (if (struct? ns-name) (ns-name :name) ns-name))]
               (ns-intern ns (if (struct? sym-name) (sym-name :name) sym-name) val))
    "disj" (let [s (eval-form ctx bindings (in form 1))
                 ks (map |(eval-form ctx bindings $) (tuple/slice form 2))]
             (if (set? s)
               (apply phs-disj s ks)
               (error "disj expects a set")))
    "set?" (set? (eval-form ctx bindings (in form 1)))
    "protocol-dispatch" (let [proto-sym (in form 1)
                               method-sym (in form 2)
                               obj (eval-form ctx bindings (in form 3))
                               rest-args (eval-form ctx bindings (in form 4))
                               type-tag (if (and (table? obj) (get obj :jolt/deftype))
                                        (get obj :jolt/deftype)
                                        (if (get obj :jolt/protocol-methods)
                                          (get obj :jolt/deftype)))
                               proto-name (proto-sym :name)
                               method-name (method-sym :name)]
                          (if (and (table? obj) (get obj :jolt/protocol-methods))
                            (let [reified-fns (get obj :jolt/protocol-methods)
                                  fn (get reified-fns (keyword method-name))]
                              (if fn (apply fn obj rest-args)
                                (error (string "No reified method " method-name " for " type-tag))))
                            (if type-tag
                              (let [fn (find-protocol-method ctx type-tag proto-name method-name)]
                                (if fn (apply fn obj rest-args)
                                  (error (string "No method " method-name " in " proto-name " for " type-tag))))
                              # host value: try candidate host type-tags (Long/String/Object/...).
                              # Generation-guarded inline cache: the candidate
                              # walk (array alloc + up to ~15 registry lookups) is
                              # the same for every value of a given host class, so
                              # cache (most-specific-tag, proto, method) -> fn,
                              # invalidated when the registry generation bumps.
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
    "register-method" (let [type-sym (in form 1)
                            proto-sym (in form 2)
                            method-sym (in form 3)
                            fn (eval-form ctx bindings (in form 4))
                            ns-name (ctx-current-ns ctx)
                            type-name (type-sym :name)
                            host (canonical-host-tag type-name)
                            # host types register under a bare canonical tag;
                            # deftype/record names stay namespace-qualified
                            type-tag (if host host (string ns-name "." type-name))
                            proto-name (proto-sym :name)
                            method-name (method-sym :name)]
                       (register-protocol-method ctx type-tag proto-name method-name fn))
    "make-reified" (let [proto-sym (in form 1)
                         methods-map (eval-form ctx bindings (in form 2))
                         proto-name (proto-sym :name)
                         reified-tag (string "reified-" proto-name)]
                    (def obj @{:jolt/deftype reified-tag :jolt/protocol-methods @{}})
                    (loop [[k v] :pairs methods-map]
                      (let [fn-value (if (and (table? v) (get v :fn*))
                                     (let [args-vec (get v :args)
                                           body-forms (get v :body)]
                                       (eval-form ctx @{}
                                   @[{:jolt/type :symbol :ns nil :name "fn*"} args-vec ;body-forms]))
                                     v)]
                        (put (obj :jolt/protocol-methods) k fn-value)))
                    obj)
    "satisfies?" (let [proto-sym (eval-form ctx bindings (in form 1))
                       obj (eval-form ctx bindings (in form 2))
                       type-tag (if (and (table? obj) (get obj :jolt/deftype))
                                (get obj :jolt/deftype)
                                (if (get obj :jolt/protocol-methods)
                                  (get obj :jolt/deftype)))]
                  (if type-tag
                    (let [pn (proto-sym :name)
                          pn-str (if (struct? pn) (pn :name) pn)]
                      (type-satisfies? ctx type-tag pn-str))
                    false))
    "locking" (eval-form ctx bindings (in form 2))
    "instance?" (let [type-sym (in form 1)
                      val (eval-form ctx bindings (in form 2))]
                  (if (get val :jolt/deftype)
                    (let [type-tag (val :jolt/deftype)
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
                      "clojure.lang.Atom" (and (table? val) (= :jolt/atom (val :jolt/type)))
                      "clojure.lang.Volatile" (and (table? val) (= :jolt/volatile (val :jolt/type)))
                      "clojure.lang.Delay" (and (table? val) (= :jolt/delay (val :jolt/type)))
                      "clojure.lang.IPersistentMap" (or (phm? val) (struct? val))
                      "clojure.lang.IPersistentVector" (or (tuple? val) (pvec? val))
                      "clojure.lang.IPersistentSet" (set? val)
                      "Object" true
                      false)))
    "defmulti" (let [name-sym (in form 1)
                      dispatch-fn (do
                                    (def raw (eval-form ctx bindings (in form 2)))
                                    (if (keyword? raw)
                                      (fn [x] (get x raw))
                                      raw))
                      # Parse options: :default dispatch-key (defaults to :default)
                      # and :hierarchy h
                      opts (tuple/slice form 3)
                      default-key (do
                                    (var dv :default) (var i 0)
                                    (while (< i (length opts))
                                      (if (= :default (in opts i))
                                        (do (set dv (in opts (+ i 1))) (set i (length opts)))
                                        (+= i 2))) dv)
                      hierarchy (do
                                  (var h nil) (var i 0)
                                  (while (< i (length opts))
                                    (if (= :hierarchy (in opts i))
                                      (do (set h (eval-form ctx bindings (in opts (+ i 1)))) (set i (length opts)))
                                      (+= i 2))) h)
                      ns (ctx-find-ns ctx (ctx-current-ns ctx))
                      methods @{}
                      # Cache for hierarchy-resolved dispatch values: the isa? walk
                      # over every method key is the expensive path (derive-based
                      # dispatch). Direct (get methods dv) hits stay uncached (already
                      # fast). Cleared in place when methods/prefs change (defmethod,
                      # prefer-method, remove-method, …) so a redef can't be hidden.
                      dispatch-cache @{}
                      mm-fn (fn [& args]
                              (let [dv (apply dispatch-fn args)
                                    method (get methods dv)]
                                (if method
                                  (apply method args)
                                  (let [cached (get dispatch-cache dv)]
                                    (if cached
                                      (apply cached args)
                                      # hierarchy-based match (explicit :hierarchy or
                                      # the global hierarchy from derive)
                                      (let [h (or hierarchy the-global-hierarchy)
                                            found (do (var f nil) (var i 0)
                                                    (let [ks (keys methods)]
                                                      (while (and (nil? f) (< i (length ks)))
                                                        (if (isa? h dv (in ks i)) (set f (get methods (in ks i))))
                                                        (++ i))) f)]
                                        (if found
                                          (do (put dispatch-cache dv found) (apply found args))
                                          # fall back to the method registered under the default key
                                          (let [dm (get methods default-key)]
                                            (if dm (apply dm args)
                                              (error (string "No method in multimethod "
                                                             (name-sym :name) " for dispatch value: " dv))))))))))) ]
                 (def v (ns-intern ns (name-sym :name) mm-fn))
                 (put v :jolt/methods methods)
                 (put v :jolt/dispatch-cache dispatch-cache)
                 (put v :jolt/default default-key)
                 (when hierarchy (put v :jolt/hierarchy hierarchy))
                 (var-get v))
    "defmethod" (let [mm-sym (in form 1)
                      dispatch-val (eval-form ctx bindings (in form 2))
                      # (defmethod mm dispatch [args] body...) — single-arity, or
                      # (defmethod mm dispatch ([args] body)...) — multi-arity.
                      # Build a fn* form and evaluate it (reuses arity dispatch
                      # and destructuring).
                      impl (eval-form ctx bindings
                             @[{:jolt/type :symbol :ns nil :name "fn*"} ;(tuple/slice form 3)])
                      mm-var (resolve-var ctx bindings mm-sym)
                      # Auto-create multimethod if it doesn't exist
                      mm-var (if mm-var mm-var
                               (let [ns (ctx-find-ns ctx (ctx-current-ns ctx))
                                     dummy-fn (fn [& args] nil)]
                                 (def v (ns-intern ns (mm-sym :name) dummy-fn))
                                 (put v :jolt/methods @{})
                                 v))
                      # The resolved var may be a plain fn (e.g. a copy-core-var'd
                      # print-method) with no method table yet — initialize one.
                      methods (or (get mm-var :jolt/methods)
                                  (let [m @{}] (put mm-var :jolt/methods m) m))]
                  (put methods dispatch-val impl)
                  (let [dc (get mm-var :jolt/dispatch-cache)]
                    (when dc (each k (keys dc) (put dc k nil))))
                  mm-var)
    "prefer-method" (let [mm-arg (in form 1)
                          mm-var (if (and (struct? mm-arg) (= :symbol (mm-arg :jolt/type)))
                                  (resolve-var ctx bindings mm-arg)
                                  (eval-form ctx bindings mm-arg))
                          # Auto-create multimethod if it doesn't exist
                          mm-var (if mm-var mm-var
                                   (let [ns (ctx-find-ns ctx (ctx-current-ns ctx))
                                         dummy-fn (fn [& args] nil)]
                                     (def v (ns-intern ns (mm-arg :name) dummy-fn))
                                     (put v :jolt/methods @{})
                                     v))
                          dispatch-val-a (eval-form ctx bindings (in form 2))
                          dispatch-val-b (eval-form ctx bindings (in form 3))
                          prefs (or (get mm-var :jolt/prefers)
                                   (do (put mm-var :jolt/prefers @{}) (mm-var :jolt/prefers)))]
                     (put prefs dispatch-val-a dispatch-val-b)
                     (let [dc (get mm-var :jolt/dispatch-cache)]
                       (when dc (each k (keys dc) (put dc k nil))))
                     mm-var)
    # A multimethod's methods live on its VAR, but the value is the dispatch fn;
    # so resolve the var from the symbol rather than evaluating it.
    "get-method" (let [mm-arg (in form 1)
                       mm-var (if (and (struct? mm-arg) (= :symbol (mm-arg :jolt/type)))
                                (resolve-var ctx bindings mm-arg)
                                (eval-form ctx bindings mm-arg))
                       dispatch-val (eval-form ctx bindings (in form 2))]
                   (when mm-var
                     (let [methods (get mm-var :jolt/methods)]
                       (or (get methods dispatch-val) (get methods :default)))))
    "methods" (let [mm-arg (in form 1)
                    mm-var (if (and (struct? mm-arg) (= :symbol (mm-arg :jolt/type)))
                             (resolve-var ctx bindings mm-arg)
                             (eval-form ctx bindings mm-arg))]
                (and mm-var (get mm-var :jolt/methods)))
    "remove-method" (let [mm-arg (in form 1)
                          mm-var (if (and (struct? mm-arg) (= :symbol (mm-arg :jolt/type)))
                                   (resolve-var ctx bindings mm-arg)
                                   (eval-form ctx bindings mm-arg))
                          dispatch-val (eval-form ctx bindings (in form 2))]
                     (when mm-var
                       (let [methods (get mm-var :jolt/methods)]
                         (when methods (put methods dispatch-val nil)))
                       (let [dc (get mm-var :jolt/dispatch-cache)]
                         (when dc (each k (keys dc) (put dc k nil)))))
                     mm-var)
    "remove-all-methods" (let [mm-var (eval-form ctx bindings (in form 1))]
                          (when mm-var
                            (put mm-var :jolt/methods @{})
                            (let [dc (get mm-var :jolt/dispatch-cache)]
                              (when dc (each k (keys dc) (put dc k nil)))))
                          mm-var)
    "deftype" (let [raw-name (in form 1)
                    type-name (unwrap-meta-name raw-name)
                    fields-vec (in form 2)
                    field-names (map 
                      (fn [f]
                        # Handle ^:meta and ^Type annotations — extract the actual name
                        (let [f (unwrap-meta-name f)]
                          (if (and (struct? f) (= :symbol (f :jolt/type)))
                            (keyword (f :name))
                            (error (string "Unsupported deftype field: " (string f))))))
                      fields-vec)
                    ns-name (ctx-current-ns ctx)
                    type-tag (string ns-name "." (type-name :name))]
                (defn ctor [& args]
                  (var inst @{:jolt/deftype type-tag})
                  (var i 0)
                  (each fn field-names
                    (put inst fn (args i))
                    (++ i))
                  inst)
                (let [ns (ctx-find-ns ctx ns-name)
                      ctor-name (type-name :name)
                      arrow-name (string "->" ctor-name)]
                  (ns-intern ns ctor-name ctor)
                  (ns-intern ns arrow-name ctor)
                  # Process inline protocol/interface methods (like defrecord):
                  #   (deftype T [fs] Proto (m [this] body) Proto2 (m2 [this] body))
                  # Emit one extend-type per protocol, wrapping each method body in a
                  # let that binds the type's fields from the instance (first param),
                  # matching Clojure's field-in-scope semantics.
                  (let [body (tuple/slice form 3)
                        field-syms (map unwrap-meta-name fields-vec)]
                    (var bi 0)
                    (while (< bi (length body))
                      (def elem (in body bi))
                      (if (and (struct? elem) (= :symbol (elem :jolt/type)))
                        (let [proto-sym elem
                              et @[{:jolt/type :symbol :ns nil :name "extend-type"} type-name proto-sym]]
                          (++ bi)
                          (while (and (< bi (length body))
                                      (not (and (struct? (in body bi)) (= :symbol ((in body bi) :jolt/type)))))
                            (let [spec (in body bi)
                                  mname (in spec 0)
                                  argv (in spec 1)
                                  mbody (tuple/slice spec 2)
                                  instance (in argv 0)
                                  field-binds @[]
                                  _ (each f field-syms
                                      (array/push field-binds f)
                                      (array/push field-binds @[{:jolt/type :symbol :ns nil :name "get"}
                                                                instance (keyword (f :name))]))
                                  wrapped @[{:jolt/type :symbol :ns nil :name "let"}
                                            (tuple/slice (tuple ;field-binds)) ;mbody]]
                              (array/push et @[mname argv wrapped]))
                            (++ bi))
                          (eval-form ctx bindings et))
                        (++ bi))))
                  (var-get (ns-intern ns ctor-name))))
    "new" (let [type-sym (in form 1)
                args (map |(eval-form ctx bindings $) (tuple/slice form 2))
                ctor (eval-form ctx bindings type-sym)]
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
              (if (target :jolt/deftype)
                (let [method-key (keyword field-name)]
                  (apply (get target method-key) target ;args))
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
                  (error (string "Cannot call method " field-name " on " (type target))))))
            # (. obj member) with no extra args: a symbol member naming a
            # function is a zero-arg method call (receiver passed as self);
            # a keyword or `-field` member is plain field access.
            (let [v (get target (keyword field-name))]
              (if (and (struct? member-raw) (= :symbol (member-raw :jolt/type))
                       (not (string/has-prefix? "-" member-name)))
                (cond
                  (or (function? v) (cfunction? v)) (v target)
                  # value stored as an unevaluated fn* form: compile then call
                  (array? v) (let [f (eval-form ctx bindings v)]
                               (if (or (function? f) (cfunction? f)) (f target) f))
                  v)
                v))))
    # default: function application — check for macros
    (if (and (struct? first-form) (= :symbol (first-form :jolt/type)))
      (let [sym-name (first-form :name)]
        # Handle .-fieldName accessor: (.-cnt obj) → (. obj -cnt)
        (if (and (> (length sym-name) 1) (= (string/slice sym-name 0 2) ".-")
                 (> (length form) 1))
          (let [field-name (string/slice sym-name 2)
                target (eval-form ctx bindings (in form 1))]
            (get target (keyword field-name)))
        # Handle ClassName. constructor syntax
        (if (and (> (length sym-name) 0) (= (sym-name (- (length sym-name) 1)) 46))
          (let [type-name (string/slice sym-name 0 (- (length sym-name) 1))
                type-sym {:jolt/type :symbol :ns (first-form :ns) :name type-name}
                ctor (eval-form ctx bindings type-sym)
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
                (jolt-invoke ctx f args)))))))
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
        # plain map literal: evaluate keys and values
        (let [kvs @[]]
          (each k (keys form)
            (array/push kvs (eval-form ctx bindings k))
            (array/push kvs (eval-form ctx bindings (get form k))))
          (build-eval-map kvs)))))))
    # A phm map-literal FORM (reader emits one for {:a nil} etc., which a struct
    # would have dropped): evaluate its key/value forms and rebuild, preserving nil.
    (phm? form)
    (let [kvs @[]]
      (each e (phm-entries form)
        (array/push kvs (eval-form ctx bindings (in e 0)))
        (array/push kvs (eval-form ctx bindings (in e 1))))
      (build-eval-map kvs))
    (array? form)
    (if (= 0 (length form))
      @[]
      (eval-list ctx bindings form))
    form)))
