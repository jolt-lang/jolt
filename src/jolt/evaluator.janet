# Jolt Evaluator
# Direct interpreter for Clojure forms on Janet.

(use ./types)
(use ./phm)

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
      (= name "prefer-method") (= name "remove-method") (= name "remove-all-methods")))

(var eval-form nil)

(defn- coll-lookup
  "Clojure `get` semantics over a jolt collection, used for collection-as-IFn."
  [coll k default]
  (cond
    (phm? coll) (phm-get coll k default)
    (set? coll) (if (phs-contains? coll k) k default)
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
    (keyword? f) (coll-lookup (get args 0) f (get args 1))
    (and (struct? f) (= :symbol (f :jolt/type)))
      (coll-lookup (get args 0) f (get args 1))
    (phm? f) (phm-get f (get args 0) (get args 1))
    (set? f) (if (phs-contains? f (get args 0)) (get args 0) (get args 1))
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

(defn- syntax-quote*
  [ctx bindings form]
  (cond
    (and (array? form) (> (length form) 0) (sym-name? (first form) "unquote"))
    (eval-form ctx bindings (in form 1))
    (and (array? form) (> (length form) 0) (sym-name? (first form) "unquote-splicing"))
    (error "~@ used outside of a list or vector in syntax-quote")
    (or (number? form) (string? form) (keyword? form) (nil? form) (= true form) (= false form))
    form
    (and (struct? form) (= :symbol (form :jolt/type)))
    (if (nil? (form :ns))
      (if (special-symbol? (form :name)) form
        {:jolt/type :symbol :ns (ctx-current-ns ctx) :name (form :name)})
      form)
    (tuple? form)
    (do (var result @[]) (var i 0) (while (< i (length form))
      (let [item (in form i)]
        (if (and (array? item) (> (length item) 0) (sym-name? (first item) "unquote-splicing"))
          (each v (eval-form ctx bindings (in item 1)) (array/push result v))
          (array/push result (syntax-quote* ctx bindings item))))
      (++ i)) (tuple ;result))
    (array? form)
    (do (var result @[]) (var i 0) (while (< i (length form))
      (let [item (in form i)]
        (if (and (array? item) (> (length item) 0) (sym-name? (first item) "unquote-splicing"))
          (each v (eval-form ctx bindings (in item 1)) (array/push result v))
          (array/push result (syntax-quote* ctx bindings item))))
      (++ i)) result)
    (and (struct? form) (get form :jolt/type)) form
    (struct? form)
    (do (var kvs @[]) (each k (keys form)
      (array/push kvs (syntax-quote* ctx bindings k))
      (array/push kvs (syntax-quote* ctx bindings (get form k)))) (struct ;kvs))
    form))

(defn resolve-var
  [ctx bindings sym-s]
  (let [name (sym-s :name) ns (sym-s :ns)]
    (if (not (nil? ns))
      (let [target-ns (ctx-find-ns ctx ns)] (ns-find target-ns name))
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

(defn- eval-require
  [ctx spec]
  (let [ns-sym (in spec 0)
        ns-name (sym-name-str ns-sym)]
    (var alias nil)
    (var refer-syms nil)
    (var i 1)
    (let [slen (length spec)]
      (while (< i slen)
        (let [item (in spec i)]
          (if (or (= item :as) (and (struct? item) (= :symbol (item :jolt/type)) (= "as" (item :name))))
            (do
              (def alias-sym (in spec (+ i 1)))
              (set alias (alias-sym :name))
              (set i slen))
            (if (or (= item :refer) (and (struct? item) (= :symbol (item :jolt/type)) (= "refer" (item :name))))
              (do
                (set refer-syms (in spec (+ i 1)))
                (set i slen))
              (++ i))))))
    (ctx-find-ns ctx ns-name)
    (when alias
      (let [current-ns (ctx-find-ns ctx (ctx-current-ns ctx))]
        (ns-import current-ns alias ns-name)))
    (when refer-syms
      (let [source-ns (ctx-find-ns ctx ns-name)
            target-ns (ctx-find-ns ctx (ctx-current-ns ctx))]
        (each refer-sym refer-syms
          (let [name (if (struct? refer-sym) (refer-sym :name) refer-sym)
                v (ns-find source-ns name)]
            (when v (ns-intern target-ns name (var-get v)))))))
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

(defn- resolve-sym
  [ctx bindings sym-s]
  (let [name (sym-s :name) ns (sym-s :ns)]
    (if (not (nil? ns))
      (let [current-ns (ctx-find-ns ctx (ctx-current-ns ctx)) aliased-ns (ns-import-lookup current-ns ns)]
        (if aliased-ns
          (let [target-ns (ctx-find-ns ctx aliased-ns) v (ns-find target-ns name)]
            (if v (var-get v) (error (string "Unable to resolve symbol: " ns "/" name))))
          (let [target-ns (ctx-find-ns ctx ns) v (ns-find target-ns name)]
            (if v (var-get v) (error (string "Unable to resolve symbol: " ns "/" name))))))
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
                          (error (string "Unable to resolve symbol: " name)))))))))))))))

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

(defn- d-realize
  "Realize a lazy-seq to an array for positional destructuring; pass others through."
  [val]
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
    val))

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
                    # rest binds a seq (jolt list = array), per Clojure semantics
                    (destructure-bind ctx bindings (in pat (+ di 1))
                      (if (and seqable? (< vi (length rv)))
                        (array/slice (if (tuple? rv) (array/slice rv) rv) vi)
                        @[]))
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
        (do
          (def or-map (get pat :or))
          (def as-sym (get pat :as))
          (when as-sym (destructure-bind ctx bindings as-sym val))
          # :keys (keyword lookup), :strs (string lookup), :syms (symbol lookup)
          (each spec [[:keys (fn [nm] (keyword nm))]
                      [:strs (fn [nm] nm)]
                      [:syms (fn [nm] {:jolt/type :symbol :ns nil :name nm})]]
            (let [kw (in spec 0) keyf (in spec 1) names (get pat kw)]
              (when (and names (indexed? names))
                (each s names
                  (let [nm (if (and (struct? s) (= :symbol (s :jolt/type))) (s :name) (string s))
                        v (d-get val (keyf nm))
                        v (if (nil? v)
                            (let [d (find-or-default or-map nm)]
                              (if (= d :jolt/none) nil (eval-form ctx bindings d)))
                            v)]
                    (bind-put bindings nm v))))))
          # direct {local-pattern key-expr} entries (local may itself be a
          # nested vector/map pattern). Special keys are keywords; skip them.
          (each k (keys pat)
            (when (not (keyword? k))
              (let [key-val (eval-form ctx bindings (get pat k))
                    v (d-get val key-val)]
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
  # Safe name extraction: non-symbol heads (e.g. keywords) fall through to default
  (def name (if (and (struct? first-form) (= :symbol (first-form :jolt/type)))
              (first-form :name)
              nil))
  (match name
    "quote" (in form 1)
    "syntax-quote" (syntax-quote* ctx bindings (in form 1))
    "unquote" (error "Unquote not valid outside of syntax-quote")
    "unquote-splicing" (error "Unquote-splicing not valid outside of syntax-quote")
    "eval" (eval-form ctx bindings (eval-form ctx bindings (in form 1)))
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
                # Check for ^:dynamic metadata
                dynamic? (and (array? raw-name) (> (length raw-name) 0)
                             (sym-name? (first raw-name) "with-meta")
                             (= :dynamic (last raw-name)))
                ns-name (ctx-current-ns ctx)
                ns (ctx-find-ns ctx ns-name)
                # Create var first (unbound) so self-referencing defs resolve
                v (ns-intern ns (name-sym :name))
                val (eval-form ctx bindings (in form 2))]
            (bind-root v val)
            (when dynamic?
              (put v :dynamic true))
            (var-get v))
    "defmacro" (let [name-sym (in form 1)
                     rest-form (tuple/slice form 2)
                     # optional docstring
                     has-doc? (and (> (length rest-form) 0) (string? (first rest-form)))
                     args-form (if has-doc? (in rest-form 1) (first rest-form))
                     body (tuple/slice rest-form (if has-doc? 2 1))
                     arg-info (parse-arg-names args-form)
                     fixed-names (arg-info :fixed)
                     rest-name (arg-info :rest)
                     defining-ns (ctx-current-ns ctx)]
                 (def macro-fn (fn [& macro-args]
                   (var new-bindings @{})
                   (table/setproto new-bindings bindings)
                   (put new-bindings "&env" @{})  # implicit &env for macro bodies (table — nil-safe)
                   (var i 0)
                   (each a fixed-names
                     (bind-put new-bindings a (macro-args i))
                     (++ i))
                   (when rest-name
                     (put new-bindings rest-name (tuple/slice macro-args i)))
                   # Use defining namespace for symbol resolution
                   (def saved-ns (ctx-current-ns ctx))
                   (ctx-set-current-ns ctx defining-ns)
                   (var result nil)
                   (each bf body
                     (set result (eval-form ctx new-bindings bf)))
                   (ctx-set-current-ns ctx saved-ns)
                   result))
                  (let [ns-name (ctx-current-ns ctx)
                       ns (ctx-find-ns ctx ns-name)]
                   (def v (ns-intern ns (name-sym :name) macro-fn))
                   (put v :macro true)
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
    "require" (let [spec (eval-form ctx bindings (in form 1))]
                 (if (and (tuple? spec) (> (length spec) 0))
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
    "in-ns" (let [ns-name (sym-name-str (in form 1))]
              (ctx-set-current-ns ctx ns-name)
              (ctx-find-ns ctx ns-name)
              nil)
    "fn*" (if (array? (in form 1))
             # Multi-arity: (fn* ([args] body...) ([args] body...)...)
             (let [pairs (tuple/slice form 1)
                   arities @{}
                   defining-ns (ctx-current-ns ctx)]
               (var self nil)
               (each pair pairs
                 (let [args-form (in pair 0)
                       body (tuple/slice pair 1)
                       param-info (parse-params args-form)
                       fixed-pats (param-info :fixed)
                       rest-pat (param-info :rest)
                       n-fixed (length fixed-pats)]
                   (put arities n-fixed
                        (fn [& fn-args]
                          (var fn-bindings @{})
                          (table/setproto fn-bindings bindings)
                          (var i 0)
                          (each pat fixed-pats
                            (destructure-bind ctx fn-bindings pat (fn-args i))
                            (++ i))
                          (when rest-pat
                            (destructure-bind ctx fn-bindings rest-pat (tuple/slice fn-args i)))
                          (put fn-bindings :jolt/loop-fn self)
                          # Use defining namespace for symbol resolution
                          (def saved-ns (ctx-current-ns ctx))
                          (ctx-set-current-ns ctx defining-ns)
                          (var result nil)
                          (each body-form body
                            (set result (eval-form ctx fn-bindings body-form)))
                          (ctx-set-current-ns ctx saved-ns)
                          result))))
               (set self (fn [& fn-args]
                 (let [n (length fn-args)
                       f (get arities n)]
                   (if f
                     (apply f fn-args)
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
                 # Use defining namespace for symbol resolution
                 (def saved-ns (ctx-current-ns ctx))
                 (ctx-set-current-ns ctx defining-ns)
                 (var result nil)
                 (each body-form body
                   (set result (eval-form ctx fn-bindings body-form)))
                 (ctx-set-current-ns ctx saved-ns)
                 result))
              self))
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
                  sym-names @[]]
              (var i 0)
              (while (< i (length bind-vec))
                (array/push init-vals (eval-form ctx bindings (bind-vec (+ i 1))))
                (array/push sym-names ((bind-vec i) :name))
                (+= i 2))
              (var loop-fn nil)
              (set loop-fn (fn [& args]
                (var loop-bindings @{})
                (table/setproto loop-bindings bindings)
                (var j 0)
                (each sn sym-names
                  (bind-put loop-bindings sn (args j))
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
                n (length clauses)]
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
                 (var new-bindings @{})
                 (table/setproto new-bindings bindings)
                 (put new-bindings (catch-sym :name) err)
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
                              (error (string "No dispatch for " method-name " on " (type obj))))))
    "register-method" (let [type-sym (in form 1)
                            proto-sym (in form 2)
                            method-sym (in form 3)
                            fn (eval-form ctx bindings (in form 4))
                            ns-name (ctx-current-ns ctx)
                            type-name (type-sym :name)
                            type-tag (string ns-name "." type-name)
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
                      "String" (string? val)
                      "Boolean" (or (= true val) (= false val))
                      "Keyword" (keyword? val)
                      "Object" true
                      false)))
    "defmulti" (let [name-sym (in form 1)
                      dispatch-fn (do
                                    (def raw (eval-form ctx bindings (in form 2)))
                                    (if (keyword? raw)
                                      (fn [x] (get x raw))
                                      raw))
                      # Parse options: :default val and :hierarchy h
                      opts (tuple/slice form 3)
                      default-val (do
                                    (var dv nil) (var i 0)
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
                      mm-fn (fn [& args]
                              (let [dv (apply dispatch-fn args)
                                    method (get methods dv)]
                                (if method
                                  (apply method args)
                                  (if hierarchy
                                    (let [found (do
                                                 (var f nil) (var i 0)
                                                 (let [ks (keys methods)]
                                                   (while (and (nil? f) (< i (length ks)))
                                                     (if (isa? hierarchy dv (ks i)) (set f (get methods (ks i))))
                                                     (++ i))) f)]
                                      (if found (apply found args)
                                        (if (not (nil? default-val)) default-val
                                          (error (string "No method in multimethod "
                                                         (name-sym :name) " for dispatch value: " dv)))))
                                    (if (not (nil? default-val)) default-val
                                      (error (string "No method in multimethod "
                                                     (name-sym :name) " for dispatch value: " dv)))))))]
                 (def v (ns-intern ns (name-sym :name) mm-fn))
                 (put v :jolt/methods methods)
                 (when default-val (put v :jolt/default default-val))
                 (when hierarchy (put v :jolt/hierarchy hierarchy))
                 (var-get v))
    "defmethod" (let [mm-sym (in form 1)
                      dispatch-val (eval-form ctx bindings (in form 2))
                      arg-vec (in form 3)
                      body (tuple/slice form 4)
                      # Extract names, handling metadata-wrapped symbols
                      extract-name (fn [arg]
                                     (let [arg (unwrap-meta-name arg)]
                                       (arg :name)))
                      arg-names (tuple/slice (map extract-name arg-vec))
                      mm-var (resolve-var ctx bindings mm-sym)
                      # Auto-create multimethod if it doesn't exist
                      mm-var (if mm-var mm-var
                               (let [ns (ctx-find-ns ctx (ctx-current-ns ctx))
                                     dummy-fn (fn [& args] nil)]
                                 (def v (ns-intern ns (mm-sym :name) dummy-fn))
                                 (put v :jolt/methods @{})
                                 v))
                      methods (get mm-var :jolt/methods)
                      impl (fn [& args]
                             (var new-bindings @{})
                             (table/setproto new-bindings bindings)
                             (var i 0)
                             (each a arg-names
                                (bind-put new-bindings a (args i))
                               (++ i))
                             (var result nil)
                             (each bf body
                               (set result (eval-form ctx new-bindings bf)))
                             result)]
                  (put methods dispatch-val impl)
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
                     mm-var)
    "remove-method" (let [mm-var (eval-form ctx bindings (in form 1))
                          dispatch-val (eval-form ctx bindings (in form 2))]
                     (if mm-var
                       (let [methods (get mm-var :jolt/methods)]
                         (put methods dispatch-val nil)))
                     mm-var)
    "remove-all-methods" (let [mm-var (eval-form ctx bindings (in form 1))]
                          (if mm-var
                            (put mm-var :jolt/methods @{}))
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
            # field access: (. obj field) — works on tables, structs, and deftypes
            (get target (keyword field-name))))
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
              (let [macro-fn (var-get v)
                    args (tuple/slice form 1)]
                (eval-form ctx bindings (apply macro-fn args)))
              (let [f (eval-form ctx bindings first-form)
                    args (map |(eval-form ctx bindings $) (tuple/slice form 1))]
                (jolt-invoke ctx f args)))))))
      (let [f (eval-form ctx bindings first-form)
            args (map |(eval-form ctx bindings $) (tuple/slice form 1))]
        (jolt-invoke ctx f args)))))

(set eval-form (fn [ctx bindings form]
  (cond
    (nil? form) nil
    (number? form) form
    (string? form) form
    (keyword? form) form
    (bytes? form) form
    (buffer? form) form
    (tuple? form) (tuple/slice (map |(eval-form ctx bindings $) form))
    (struct? form)
    (if (= :symbol (form :jolt/type))
      (resolve-sym ctx bindings form)
      (if (= :jolt/set (form :jolt/type))
        (apply make-phs (form :value))
      (if (= :jolt/tagged (form :jolt/type))
        (let [tag (form :tag)
              data-readers (get (ctx :env) :data-readers)
              reader-fn (if data-readers (get data-readers tag))]
          (if reader-fn
            (reader-fn (form :form))
            (error (string "No reader function for tag " tag))))
      (if (get form :jolt/type)
        (error (string "Unexpected tagged form: " (form :jolt/type)))
        form))))
    (array? form)
    (if (= 0 (length form))
      @[]
      (eval-list ctx bindings form))
    form)))
