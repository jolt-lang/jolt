# Jolt value layer — Namespace
# Extracted from types.janet (jolt-bvek phase 5a split).

(use ./types_symbols)
(use ./types_var)
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

