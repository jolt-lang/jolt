# Jolt Evaluator — protocols, multimethods, deftype/reify, stateful fn install
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
(use ./eval_base)
(use ./eval_resolve)
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

(defn make-reified-impl [ctx methods-map & rest-args]
  # methods-map is the EVALUATED {keyword fn} map (a phm when compiled, a struct/
  # table when interpreted) — the fn* literals are already fns, just store them.
  # proto-names are the (short) names of every protocol the reify implements.
  (def proto-names (if (and (= 1 (length rest-args)) (indexed? (in rest-args 0)))
                     (in rest-args 0)        # wiring passed the rest tuple as one arg
                     rest-args))
  (def obj @{:jolt/deftype (string "reified-" (if (> (length proto-names) 0) (in proto-names 0) ""))
             :jolt/protocols (tuple ;proto-names)
             :jolt/protocol-methods @{}})
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
  [ctx type-name-sym field-kws &opt field-tags field-muts]
  (def type-tag (string (ctx-current-ns ctx) "." (type-name-sym :name)))
  (def kws (d-realize field-kws))
  # per-field type hints (jolt-3ko): a tuple parallel to kws — "Vec3" (a record
  # type name), "num", or nil. The inference resolves these to the field's exact
  # type so reading a field back carries it (a nested record stays typed).
  (def tags (if field-tags (d-realize field-tags) (array/new-filled (length kws))))
  # jolt-c3q: a type with any ^:unsynchronized-mutable / ^:volatile-mutable field
  # is set!-able, so it CAN'T be an immutable shape-rec tuple. Such a type uses
  # the mutable :jolt/deftype table form regardless of :shapes? (set! mutates it,
  # field reads route through the tagged-table path), and is NOT registered as a
  # shape so the inference never emits a bare-index read against the table.
  (def mutable? (and field-muts (some |(identity $) (d-realize field-muts))))
  # The ctor closure itself. Built FIRST so it can be indexed by value below.
  # Records are shape-recs when shapes are active (:shapes? = direct-link, where
  # the inference proves the reads) — the whole field-access pipeline handles
  # them; otherwise (or when mutable) the original :jolt/deftype tables. Read at
  # ctor-BUILD time so a type is consistently one representation or the other.
  (def the-ctor
    (if (and (get (ctx :env) :shapes?) (not mutable?))
      (fn [& args] (make-record type-tag kws args))
      (fn [& args]
        (var inst @{:jolt/deftype type-tag})
        (var i 0) (each kw kws (put inst kw (in args i)) (++ i))
        inst)))
  # jolt-t34: register this record's ctor return shape (DECLARED field order) so
  # the inference types (->Name ...) as a struct of these fields and field reads
  # on the result bare-index. Keyed by the ctor var-key "ns/->Name" to match how
  # the IR names the call head. Harmless when records aren't shaped (sidx gated).
  # Skipped for mutable types — they're tables, not shape-recs (jolt-c3q).
  (unless mutable?
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
    (put cix the-ctor (string (ctx-current-ns ctx) "/->" (type-name-sym :name)))))
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
    (fn [methods-map & proto-names] (make-reified-impl ctx methods-map proto-names)))
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
  (ns-intern core "make-deftype-ctor" (fn [name-sym field-kws &opt field-tags field-muts] (make-deftype-ctor-impl ctx name-sym field-kws field-tags field-muts)))
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
  # satisfies?: evaluated protocol value + instance. Recognizes a reify the same
  # way instance? does — by the protocols it records on itself (a reify's methods
  # are instance-local, so they aren't in the global type registry that
  # type-satisfies? consults).
  (ns-intern core "satisfies?"
    (fn [proto obj]
      (def pn (proto :name))
      (def pn-str (if (struct? pn) (pn :name) pn))
      (def protos (if (table? obj) (get obj :jolt/protocols)))
      (def type-tag (or (record-tag obj)
                        (if (and (table? obj) (get obj :jolt/protocol-methods))
                          (get obj :jolt/deftype))))
      (cond
        (and protos (string? pn-str)
             (truthy? (some (fn [p] (= (last (string/split "." p))
                                       (last (string/split "." pn-str))))
                            protos))) true
        type-tag (type-satisfies? ctx type-tag pn-str)
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
                      type-name))
              # instance? of a PROTOCOL works like satisfies?: a reify implementing
              # it is an instance. The reify records every protocol it implements
              # (short names); (instance? a.b.Proto x) passes a qualified name, so
              # match by short name against any of them. (malli relies on this.)
              (let [protos (if (table? val) (get val :jolt/protocols))
                    tn-short (last (string/split "." type-name))]
                (and protos (truthy? (some (fn [p] (= (last (string/split "." p)) tn-short)) protos))))))
        (match (type-sym :name)
          "Number" (number? val)
          "java.lang.Number" (number? val)
          "Long" (number? val)
          "java.lang.Long" (number? val)
          "Integer" (number? val)
          "Double" (number? val)
          "String" (string? val)
          "java.lang.String" (string? val)
          # String implements CharSequence — malli's :re validator gates on
          # (instance? CharSequence x) before matching (jolt-ltwk).
          "CharSequence" (string? val)
          "java.lang.CharSequence" (string? val)
          "Boolean" (or (= true val) (= false val))
          "Keyword" (keyword? val)
          # regex patterns (cuerdas-style (instance? Pattern x) checks)
          "Pattern" (and (table? val) (= :jolt/regex (val :jolt/type)))
          "java.util.regex.Pattern" (and (table? val) (= :jolt/regex (val :jolt/type)))
          "Character" (and (struct? val) (= :jolt/char (get val :jolt/type)))
          "java.lang.Character" (and (struct? val) (= :jolt/char (get val :jolt/type)))
          # java.time shims (host_interop.janet); #inst IS java.util.Date in Clojure
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
