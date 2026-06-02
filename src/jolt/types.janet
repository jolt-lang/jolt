# Jolt Types
# Core types for the Clojure-on-Janet interpreter.
#
# Types:
#   JoltVar        — mutable container with metadata (like Clojure Var)
#   JoltNamespace  — namespace with symbol→var mappings and imports
#   JoltContext     — evaluation context (env atom, namespaces)
#
# Symbols are represented as {:jolt/type :symbol :ns <string-or-nil> :name <string>}
# as produced by the reader.

# ============================================================
# Symbol helpers
# ============================================================

(defn sym?
  "Check if x is a Jolt symbol struct."
  [x]
  (and (struct? x) (= :symbol (x :jolt/type))))

# ============================================================
# Var
# ============================================================

(def- binding-stack @[])  # stack of {var → value} tables for thread-local bindings

(defn push-thread-bindings
  "Push a frame of dynamic var bindings. Takes a struct of var→value."
  [bindings]
  (array/push binding-stack bindings))

(defn pop-thread-bindings
  "Pop the most recent frame of dynamic var bindings."
  []
  (array/pop binding-stack))

(defn make-var
  "Create a new Jolt Var.
  (make-var name)           — unbound var
  (make-var name init-val)  — var with root binding
  (make-var name init-val meta) — var with root and metadata
  
  name is a symbol struct {:jolt/type :symbol ...}"
  [name &opt init-val meta]
  (default init-val nil)
  (default meta nil)
  (let [m (if meta meta {:name name})
        result @{:jolt/type :jolt/var
                 :name name
                 :root init-val
                 :meta m
                 :dynamic (if meta (get meta :dynamic) false)
                 :macro (if meta (get meta :macro) false)
                 :ns (if meta (get meta :ns) nil)}]
    result))

(defn var?
  "Check if x is a Jolt Var."
  [x]
  (and (table? x) (= :jolt/var (x :jolt/type))))

(defn var-dynamic?
  "Check if var is marked :dynamic."
  [v]
  (v :dynamic))

(defn var-macro?
  "Check if var is marked :macro."
  [v]
  (v :macro))

(defn var-name
  "Return the symbol name of the var."
  [v]
  (v :name))

(defn var-meta
  "Return the metadata of the var."
  [v]
  (v :meta))

(defn var-ns
  "Return the namespace of the var."
  [v]
  (v :ns))

(defn var-get
  "Deref the var. If the var is dynamic and has a thread-local binding, return that.
  Otherwise return the root binding."
  [v]
  # walk binding stack top-down for this var
  (var result nil)
  (var i (dec (length binding-stack)))
  (while (>= i 0)
    (let [frame (in binding-stack i)
          val (get frame v)]
      (if (not (nil? val))
        (do
          (set result (if (var? val) (var-get val) val))
          (set i -1))
        (-- i))))
  (if (not (nil? result)) result (v :root)))

(defn var-set
  "Set the root binding of a var."
  [v val]
  (put v :root val))

(defn alter-var-root
  "Atomically alter the root binding of v by applying f to current value plus args."
  [v f & args]
  (let [new-val (f (v :root) ;args)]
    (put v :root new-val)))

(defn with-meta
  "Return a new var with updated metadata. The original var is unchanged."
  [v meta]
  # build new meta as a table first (to allow adding keys), then convert
  (let [new-meta-table (merge @{} (v :meta) meta)
        # convert to struct by extracting all keys
        new-meta (table/to-struct new-meta-table)]
    @{:jolt/type :jolt/var
      :name (v :name)
      :root (v :root)
      :meta new-meta
      :dynamic (v :dynamic)
      :macro (v :macro)
      :ns (v :ns)}))

(defn bind-root
  "Set the root binding (internal, same as var-set)."
  [v val]
  (put v :root val))

# ============================================================
# Namespace
# ============================================================

(defn make-ns
  "Create a new namespace.
  (make-ns name) — empty namespace
  name is a symbol struct {:jolt/type :symbol ...}"
  [name]
  (struct
    :jolt/type :jolt/namespace
    :name name
    :mappings @{}
    :imports @{}
    :aliases @{}))

(defn ns?
  "Check if x is a Jolt Namespace."
  [x]
  (and (struct? x) (= :jolt/namespace (x :jolt/type))))

(defn ns-name
  "Return the name symbol of a namespace."
  [ns]
  (ns :name))

(defn ns-map
  "Return the mappings table (symbol → var) for a namespace."
  [ns]
  (ns :mappings))

(defn ns-intern
  "Find or create a var named by sym in namespace ns, setting root binding to val if given.
  (ns-intern ns sym)       — find or create unbound var
  (ns-intern ns sym val)   — find or create with root binding"
  [ns sym &opt val]
  (default val nil)
  (let [mappings (ns :mappings)
        existing (get mappings sym)]
    (if existing
      (do
        (when (not (nil? val))
          (bind-root existing val))
        existing)
      (let [v (make-var sym val {:ns ns :name sym})]
        (put mappings sym v)
        v))))

(defn ns-find
  "Find a var by symbol in the namespace. Returns nil if not found."
  [ns sym]
  (get (ns :mappings) sym))

(defn ns-unmap
  "Remove a mapping by symbol from the namespace."
  [ns sym]
  (put (ns :mappings) sym nil))

(defn ns-resolve
  "Resolve a symbol in a namespace. Looks in own mappings first,
  then aliases. Returns the var or nil."
  [ns sym]
  (or (ns-find ns sym)
      (let [qualified? (sym? sym)]
        (when qualified?
          # qualified symbol: look up via alias
          (let [alias-ns (get (ns :aliases) (sym :ns))]
            (when alias-ns
              (ns-find alias-ns (sym :name))))))))

(defn ns-import
  "Add an import to the namespace. class-name is local symbol, fq-class-name is the full qualified name."
  [ns class-name fq-class-name]
  (put (ns :imports) class-name fq-class-name))

(defn ns-import-lookup
  "Look up an import in the namespace. Returns the full qualified name or nil."
  [ns class-name]
  (get (ns :imports) class-name))

(defn ns-add-alias
  "Add an alias from alias-sym to target-ns."
  [ns alias-sym target-ns]
  (put (ns :aliases) alias-sym target-ns))

# ============================================================
# Context
# ============================================================

(defn ctx-find-ns
  "Find or create a namespace in the context by name symbol."
  [ctx ns-sym]
  (let [env (ctx :env)
        namespaces (env :namespaces)]
    (or (get namespaces ns-sym)
        (let [ns (make-ns ns-sym)]
          (put namespaces ns-sym ns)
          ns))))

(defn make-ctx
  "Create a new evaluation context.
  (make-ctx)       — empty context with 'user namespace
  (make-ctx opts)  — context with initial namespaces from opts
  
  opts may contain:
    :namespaces — struct of {ns-symbol → {sym → value, ...}, ...}"
  [&opt opts]
  (default opts nil)
  (let [compile? (if opts (get opts :compile?) false)
        env @{:namespaces @{}
              :class->opts @{}
              :current-ns "user"
              :compile? compile?
              :compiled-cache @{}}
        # create the user namespace via a partial context
        _ (ctx-find-ns {:env env} "user")]
    # initialize from opts
    (when opts
      (when-let [ns-opts (get opts :namespaces)]
        (loop [[ns-sym mappings] :pairs ns-opts]
          (let [ns (ctx-find-ns {:env env} ns-sym)]
            (loop [[sym val] :pairs mappings]
              (ns-intern ns sym val))))))
    {:jolt/type :jolt/context
     :env env}))

(defn ctx?
  "Check if x is a Jolt Context."
  [x]
  (and (struct? x) (= :jolt/context (x :jolt/type))))

(defn ctx-env
  "Return the env atom from the context."
  [ctx]
  (ctx :env))

(defn ctx-current-ns
  "Get the current namespace symbol."
  [ctx]
  (get (ctx :env) :current-ns))

(defn ctx-set-current-ns
  "Set the current namespace symbol."
  [ctx ns-sym]
  (put (ctx :env) :current-ns ns-sym))

(defn all-ns
  "Return a list of all namespaces in the context."
  [ctx]
  (let [namespaces (get (ctx :env) :namespaces)
        result @[]]
    (loop [[_ ns] :pairs namespaces]
      (array/push result ns))
    result))

(defn remove-ns
  "Remove a namespace from the context by name string."
  [ctx ns-name]
  (put (get (ctx :env) :namespaces) ns-name nil) nil)

(defn create-ns
  "Create a new namespace."
  [ctx ns-name]
  (ctx-find-ns ctx ns-name))

(defn the-ns
  "Return the current namespace object."
  [ctx]
  (ctx-find-ns ctx (ctx-current-ns ctx)))

(defn ns-interns
  "Return the map of all interned vars in the current namespace."
  [ctx]
  (let [ns (ctx-find-ns ctx (ctx-current-ns ctx))]
    (ns :mappings)))

(defn ns-aliases
  "Return the alias map of the current namespace."
  [ctx]
  (let [ns (ctx-find-ns ctx (ctx-current-ns ctx))]
    (ns :aliases)))

(defn ns-imports-fn
  "Return the import map of the current namespace."
  [ctx]
  (let [ns (ctx-find-ns ctx (ctx-current-ns ctx))]
    (ns :imports)))
