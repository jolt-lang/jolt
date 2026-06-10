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

# Characters are {:jolt/type :jolt/char :ch <codepoint>}, distinct from strings.
(defn make-char [code] {:jolt/type :jolt/char :ch code})

(def- char-named @{"newline" 10 "space" 32 "tab" 9 "return" 13
                   "formfeed" 12 "backspace" 8 "newpage" 12 "nul" 0})

(defn char-from-name
  "Resolve a reader char-literal name (\\a, \\newline, \\uNNNN, \\oNNN) to a char value."
  [name]
  (cond
    (= 1 (length name)) (make-char (in name 0))
    (get char-named name) (make-char (get char-named name))
    (and (> (length name) 1) (= (in name 0) (get "u" 0)))
      (make-char (scan-number (string "16r" (string/slice name 1))))
    (and (> (length name) 1) (= (in name 0) (get "o" 0)))
      (make-char (scan-number (string "8r" (string/slice name 1))))
    (error (string "Unsupported character: \\" name))))

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

# Dynamic-var binding stack. Stored fiber-locally (via Janet's dyn), so that
# concurrent go blocks — each a Janet fiber — don't interleave each other's
# dynamic bindings, and a go block conveys the bindings in effect when it was
# spawned (see snapshot-bindings/install-bindings). Each fiber lazily gets its
# own array on first use.
(defn cur-binding-stack []
  (or (dyn :jolt/binding-stack)
      (let [s @[]] (setdyn :jolt/binding-stack s) s)))

(defn push-thread-bindings
  "Push a frame of dynamic var bindings. Takes a struct of var→value."
  [bindings]
  (array/push (cur-binding-stack) bindings))

(defn pop-thread-bindings
  "Pop the most recent frame of dynamic var bindings."
  []
  (array/pop (cur-binding-stack)))

(defn snapshot-bindings
  "Shallow copy of the current binding stack (frames are immutable value maps).
  Captured by a go block at spawn time for binding conveyance."
  []
  (array/slice (cur-binding-stack)))

(defn install-bindings
  "Install a snapshot as this fiber's binding stack (a fresh copy, so the
  fiber's own push/pop/var-set don't mutate the snapshot's frames array)."
  [snap]
  (setdyn :jolt/binding-stack (array/slice snap)))

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
                 # Generation: bumped on every root change (redefinition). Call
                 # sites / dispatch caches keyed on this can detect a redef and
                 # invalidate; direct-linked (sealed) sites can detect staleness.
                 :gen 0
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
  # Fast path: no dynamic bindings are active (the common case — the stack is
  # only non-empty inside a `binding` block), so the value is just the root. This
  # is the hot path for every global deref; skip building the walk otherwise.
  (def bs (cur-binding-stack))
  (if (= 0 (length bs))
    (v :root)
    # walk binding stack top-down for this var
    (do
      (var result nil)
      (var i (dec (length bs)))
      (while (>= i 0)
        (let [frame (in bs i)
              val (get frame v)]
          (if (not (nil? val))
            (do
              (set result (if (var? val) (var-get val) val))
              (set i -1))
            (-- i))))
      (if (not (nil? result)) result (v :root)))))

(defn var-set
  "Set a var's value. If the var has a thread-local binding on the stack, update
  the innermost frame that binds it (matching Clojure, where var-set targets the
  current binding); otherwise set the root."
  [v val]
  (def bs (cur-binding-stack))
  (var i (dec (length bs)))
  (var done false)
  (while (and (not done) (>= i 0))
    (let [frame (in bs i)]
      (if (not (nil? (get frame v)))
        (do (put bs i (merge frame {v val})) (set done true))
        (-- i))))
  (unless done (do (put v :root val) (put v :gen (+ 1 (or (v :gen) 0)))))
  val)

(defn alter-var-root
  "Atomically alter the root binding of v by applying f to current value plus args."
  [v f & args]
  (let [new-val (f (v :root) ;args)]
    (put v :root new-val)
    (put v :gen (+ 1 (or (v :gen) 0)))
    new-val))

(defn alter-meta!
  "Atomically update a var's metadata via (apply f args)."
  [v f & args]
  (let [new-meta (apply f (var-meta v) args)]
    (put v :meta new-meta)
    new-meta))

(defn reset-meta!
  "Reset a var's metadata to the given value."
  [v meta]
  (put v :meta meta)
  meta)


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
      :gen (or (v :gen) 0)
      :dynamic (v :dynamic)
      :macro (v :macro)
      :ns (v :ns)}))

(defn bind-root
  "Set the root binding and bump the var's generation (the redefinition
  chokepoint: def, ns-intern-with-val, and the root-set paths all route here)."
  [v val]
  (put v :root val)
  (put v :gen (+ 1 (or (v :gen) 0)))
  val)

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
  (and (or (struct? x) (table? x)) (= :jolt/namespace (x :jolt/type))))

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
      # Store the namespace *name*, not the ns table: a back-pointer to the ns
      # would make the var cyclic (ns -> mappings -> var -> ns), and the compiler
      # embeds var cells as constants, which can't be cyclic.
      (let [v (make-var sym val {:ns (get ns :name) :name sym})]
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
          # qualified symbol: look up via alias (string-keyed store)
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
  "Add an alias: alias-name (string) -> target ns NAME (string). The ONE alias
  store (jolt-ark): resolution and ns-aliases both read it; :imports is class
  imports only."
  [ns alias-name target-ns-name]
  (put (ns :aliases) alias-name target-ns-name))

(defn ns-alias-lookup
  "The target ns NAME for alias-name, or nil."
  [ns alias-name]
  (get (ns :aliases) alias-name))

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

# Instant value: an immutable tagged struct keyed by epoch milliseconds, so
# equality and map-key hashing are by INSTANT (offset-normalized): two #inst
# literals with different offsets denoting the same moment are =.
(defn make-inst [ms]
  {:jolt/type :jolt/inst :ms ms})

(defn parse-inst
  "Parse an RFC3339 timestamp with Clojure's partial defaults
  (yyyy[-MM[-dd[Thh[:mm[:ss[.fff]]]]]][Z|+hh:mm|-hh:mm]) to an inst value.
  Errors on a malformed timestamp."
  [ts]
  (def pat (peg/compile
    ~(sequence
       (capture (repeat 4 :d))                                   # year
       (opt (sequence "-" (capture (repeat 2 :d))))               # month
       (opt (sequence "-" (capture (repeat 2 :d))))               # day
       (opt (sequence "T" (capture (repeat 2 :d))                 # hour
                      (opt (sequence ":" (capture (repeat 2 :d))  # min
                        (opt (sequence ":" (capture (repeat 2 :d)) # sec
                          (opt (sequence "." (capture (some :d)))))))))) # frac
       (opt (choice (capture "Z")
                    (sequence (capture (set "+-")) (capture (repeat 2 :d))
                              ":" (capture (repeat 2 :d)))))
       -1)))
  (def m (peg/match pat ts))
  (when (nil? m) (error (string "Unrecognized #inst timestamp: " ts)))
  # captures arrive positionally; classify by shape: digits runs + offset parts.
  (var year nil) (var month 1) (var day 1)
  (var hh 0) (var mm 0) (var ss 0) (var frac "0")
  (var off-sign nil) (var off-h 0) (var off-m 0)
  (var i 0)
  (def fields @[:year :month :day :hh :mm :ss])
  (var fi 0)
  (while (< i (length m))
    (def part (in m i))
    (cond
      (= part "Z") nil
      (or (= part "+") (= part "-"))
        (do (set off-sign part)
            (set off-h (scan-number (in m (+ i 1))))
            (set off-m (scan-number (in m (+ i 2))))
            (+= i 2))
      # fractional seconds arrive right after :ss was filled
      (and (>= fi 6))
        (set frac part)
      (do
        (def v (scan-number part))
        (case (in fields fi)
          :year (set year v) :month (set month v) :day (set day v)
          :hh (set hh v) :mm (set mm v) :ss (set ss v))
        (++ fi)))
    (++ i))
  (when (nil? year) (error (string "Unrecognized #inst timestamp: " ts)))
  (def base-s (os/mktime {:year year :month (- month 1) :month-day (- day 1)
                          :hours hh :minutes mm :seconds ss}))
  # fractional part -> milliseconds (truncate beyond 3 digits)
  (def frac3 (string/slice (string frac "000") 0 3))
  (def ms-frac (scan-number frac3))
  (def off-s (* (if (= off-sign "-") -1 1) (+ (* off-h 3600) (* off-m 60))))
  (make-inst (- (+ (* base-s 1000) ms-frac) (* off-s 1000))))

(defn inst->rfc3339
  "Canonical print form: yyyy-MM-ddThh:mm:ss.fff-00:00 (UTC, like Clojure)."
  [inst]
  (def ms (inst :ms))
  (def s (math/floor (/ ms 1000)))
  (def frac (- ms (* s 1000)))
  (def d (os/date s))
  (string/format "%04d-%02d-%02dT%02d:%02d:%02d.%03d-00:00"
                 (d :year) (+ 1 (d :month)) (+ 1 (d :month-day))
                 (d :hours) (d :minutes) (d :seconds) frac))

# UUID value: an immutable tagged struct. Lowercased at construction so
# equality and map-key hashing are case-insensitive by value (struct equality),
# matching Clojure (java.util.UUID equality / cljs UUID).
(defn make-uuid [s]
  {:jolt/type :jolt/uuid :str (string/ascii-lower s)})

(defn make-ctx
  "Create a new evaluation context.
  (make-ctx)       — empty context with 'user namespace
  (make-ctx opts)  — context with initial namespaces from opts
  
  opts may contain:
    :namespaces — struct of {ns-symbol → {sym → value, ...}, ...}"
  [&opt opts]
  (default opts nil)
  (let [compile? (if opts (get opts :compile?) false)
        # Direct-linking (call-site/unit property, like Clojure). :aot-core?
        # (default true; JOLT_AOT_CORE=0 disables) compiles the core tiers +
        # compiler with direct-linking on. :direct-linking? is the per-unit flag
        # the back end reads while emitting; it defaults to the user-code setting
        # (off unless opted in) and load-core-overlay! flips it on around core.
        aot-core? (let [o (if opts (get opts :aot-core?) nil)]
                    (if (nil? o) (not (= "0" (os/getenv "JOLT_AOT_CORE"))) o))
        # Macro expanders compile in EVERY mode (macros are ordinary compiled
        # fns, as in Clojure) — including interpret mode, where evaluation stays
        # interpreted but expansion runs native. :compile-macros? false (or
        # JOLT_INTERPRET_MACROS=1) opts back into the fully-interpreted oracle.
        compile-macros? (let [o (if opts (get opts :compile-macros?) nil)]
                          (if (nil? o)
                            (not (= "1" (os/getenv "JOLT_INTERPRET_MACROS")))
                            o))
        env @{:namespaces @{}
              :class->opts @{}
              :current-ns "user"
              :compile? compile?
              :aot-core? aot-core?
              :compile-macros? compile-macros?
              :direct-linking? (if opts (get opts :direct-linking?) nil)
              # Ordered roots searched (after the stdlib) to resolve a namespace
              # to a .clj/.cljc file. jolt-core holds the portable Clojure layer
              # (analyzer/IR/core); deps.edn resolution appends dep src dirs.
              :source-paths @["jolt-core" "src/jolt"]
              :type-registry @{}
              :data-readers (let [dr @{}]
                              (put dr (keyword "#inst") (fn [s] (parse-inst s)))
                              (put dr (keyword "#uuid") (fn [s] (make-uuid s)))
                              dr)}
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
  "Set the current namespace symbol. Also keeps the *ns* dynamic var's root in
  sync (the var table is cached on the env by install-stateful-fns! — one table
  put on this hot path, no ns lookup chain)."
  [ctx ns-sym]
  (put (ctx :env) :current-ns ns-sym)
  (when-let [v (get (ctx :env) :ns-var)]
    (put v :root (ctx-find-ns ctx ns-sym))))

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

(defn find-var
  "Resolve a symbol to a var in the current context.
  Looks in current namespace first, then clojure.core."
  [ctx sym-s]
  (let [name (sym-s :name)
        ns-sym (sym-s :ns)]
    (if ns-sym
      (let [ns (ctx-find-ns ctx ns-sym)]
        (ns-find ns name))
      (let [current-ns (ctx-find-ns ctx (ctx-current-ns ctx))
            v (ns-find current-ns name)]
        (if v v
          (let [core-ns (ctx-find-ns ctx "clojure.core")]
            (ns-find core-ns name)))))))


# ============================================================
# Protocol type registry
# ============================================================

(defn register-protocol-method
  "Register a protocol method implementation for a type."
  [ctx type-tag protocol-name method-name fn]
  (let [env (ctx :env)
        registry (get env :type-registry)
        type-impls (or (get registry type-tag)
                      (do (put registry type-tag @{}) (get registry type-tag)))
        proto-impls (or (get type-impls protocol-name)
                       (do (put type-impls protocol-name @{}) (get type-impls protocol-name)))]
    (put proto-impls method-name fn)
    # Bump the registry generation so any dispatch cache keyed on it invalidates.
    (put env :type-registry-gen (+ 1 (or (get env :type-registry-gen) 0)))))

(defn find-protocol-method
  "Find a protocol method implementation for a type."
  [ctx type-tag protocol-name method-name]
  (let [registry (get (ctx :env) :type-registry)
        type-impls (get registry type-tag)]
    (when type-impls
      (let [proto-impls (get type-impls protocol-name)]
        (when proto-impls
          (get proto-impls method-name))))))

(defn type-satisfies?
  "Check if a type satisfies a protocol."
  [ctx type-tag protocol-name]
  (let [registry (get (ctx :env) :type-registry)
        type-impls (get registry type-tag)]
    (if (and type-impls (get type-impls protocol-name)) true false)))
