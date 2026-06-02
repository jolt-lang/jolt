# Jolt Evaluator
# Direct interpreter for Clojure forms on Janet.

(use ./types)

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
      (= name "instance?") (= name "defmulti") (= name "defmethod")
      (= name "deftype") (= name "new") (= name ".")))

(var eval-form nil)

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
                val (eval-form ctx bindings (in form 2))
                ns-name (ctx-current-ns ctx)
                ns (ctx-find-ns ctx ns-name)]
            (ns-intern ns (name-sym :name) val)
            (var-get (ns-intern ns (name-sym :name))))
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
                       arg-info (parse-arg-names args-form)
                       fixed-names (arg-info :fixed)
                       rest-name (arg-info :rest)
                       n-fixed (length fixed-names)]
                   (put arities n-fixed
                        (fn [& fn-args]
                          (var fn-bindings @{})
                          (table/setproto fn-bindings bindings)
                          (var i 0)
                          (each arg-name fixed-names
                            (bind-put fn-bindings arg-name (fn-args i))
                            (++ i))
                          (when rest-name
                            (put fn-bindings rest-name (tuple/slice fn-args i)))
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
                   arg-info (parse-arg-names args-form)
                   fixed-names (arg-info :fixed)
                   rest-name (arg-info :rest)
                   defining-ns (ctx-current-ns ctx)]
               (var self nil)
               (set self (fn [& fn-args]
                 (var fn-bindings @{})
                 (table/setproto fn-bindings bindings)
                 (var i 0)
                 (each arg-name fixed-names
                   (bind-put fn-bindings arg-name (fn-args i))
                   (++ i))
                 (when rest-name
                   (put fn-bindings rest-name (tuple/slice fn-args i)))
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
                    # Handle destructuring patterns
                    (if (struct? pat)
                      (let [keys-vec (get pat :keys)]
                        (if (and keys-vec (indexed? keys-vec))
                          (each k keys-vec
                            (def kname (if (keyword? k) (string k) (k :name)))
                            (bind-put new-bindings kname (get val (keyword kname))))
                          (bind-put new-bindings (pat :name) val)))
                      (if (indexed? pat)
                        # Sequential destructuring (vector pattern)
                        (do
                          (var di 0)
                          (while (< di (length pat))
                            (let [inner-pat (in pat di)]
                              (if (struct? inner-pat)
                                (bind-put new-bindings (inner-pat :name) (get val di))
                                (bind-put new-bindings inner-pat (get val di))))
                            (+= di 1)))
                        # Plain symbol binding
                        (bind-put new-bindings (pat :name) val)))
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
                  (eval-form ctx bindings body-form)
                  ([err]
                   (run-finally finally-body)
                   (error err)))
                (eval-form ctx bindings body-form))))
    "set!" (let [target (in form 1)
                 val (eval-form ctx bindings (in form 2))]
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
                     val)))))
    "var" (let [target-sym (in form 1)
                v (resolve-var ctx bindings target-sym)]
            (if v v (error (string "Unable to resolve var: " (sym-name-str target-sym) " in var"))))
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
                     dispatch-fn (eval-form ctx bindings (in form 2))
                     ns (ctx-find-ns ctx (ctx-current-ns ctx))
                     methods @{}
                     mm-fn (fn [& args]
                             (let [dv (apply dispatch-fn args)
                                   method (get methods dv)]
                               (if method
                                 (apply method args)
                                 (error (string "No method in multimethod "
                                                (name-sym :name) " for dispatch value: "
                                                dv)))))]
                 (def v (ns-intern ns (name-sym :name) mm-fn))
                 (put v :jolt/methods methods)
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
              member-sym (in form 2)
              member-name (member-sym :name)
              field-name (if (and (> (length member-name) 0) (= "-" (string/slice member-name 0 1)))
                          (string/slice member-name 1)
                          member-name)]
          (if (> (length form) 3)
            # method call: (. obj method args...)
            (let [args (map |(eval-form ctx bindings $) (tuple/slice form 3))]
              (if (target :jolt/deftype)
                (let [method-key (keyword field-name)]
                  (apply (get target method-key) target ;args))
                (error (string "Cannot call method " field-name " on non-deftype"))))
            # field access: (. obj field)
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
                (apply f args)))))))
      (let [f (eval-form ctx bindings first-form)
            args (map |(eval-form ctx bindings $) (tuple/slice form 1))]
        (if (function? f)
          (apply f args)
          (if (keyword? f)
            (get (first args) f)
            (error (string "Cannot call " (type f) " as a function"))))))))

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
      (if (get form :jolt/type)
        (error (string "Unexpected tagged form: " (form :jolt/type)))
        form))
    (array? form)
    (if (= 0 (length form))
      @[]
      (eval-list ctx bindings form))
    form)))
