# Janet implementation of the Jolt host contract (ns `jolt.host`).
#
# This is the seam between the portable jolt-core (analyzer/IR/core, pure Clojure
# under jolt-core/) and the Janet runtime. jolt-core calls ONLY these functions —
# never Janet directly. Re-hosting Jolt to another runtime means reimplementing
# this contract (+ the back end and RT) for that runtime.
#
# Lives in src/jolt/ (with the rest of the Janet host) rather than a separate
# host/janet/ dir: Janet resolves relative imports per-file, so a host/janet
# module importing ../../src/jolt/* loads SECOND instances of compiler/types/core
# (inconsistent state). The portability boundary is the `jolt.host` namespace
# contract + jolt-core/, not the directory.
#
# Two groups:
#   1. Form introspection — reader forms are host-specific (the reader is the
#      host's), so shape predicates/accessors live here. Returns jolt values the
#      analyzer walks with ordinary Clojure.
#   2. Compile-time environment — resolve symbols to vars/macros, expand macros,
#      the current namespace. These take ctx (an opaque host handle).

(use ./types)
(use ./evaluator)
(use ./core)
(import ./phm :as phm)
(import ./reader :as rdr)

# ---------------------------------------------------------------------------
# Form introspection
# ---------------------------------------------------------------------------

(defn h-sym? [form] (and (struct? form) (= :symbol (form :jolt/type))))
(defn h-sym-name [form] (form :name))
(defn h-sym-ns [form] (form :ns))
# Reader metadata on a symbol (e.g. ^:dynamic / ^:redef / ^:private on a def
# name). Returns the meta map or nil. Lets the analyzer carry def metadata that
# the back end applies to the var — without it, compiled defs drop all var meta.
(defn h-sym-meta [form] (form :meta))

(defn h-list? [form] (array? form))          # a call / list (reader: array)
(defn h-vector? [form] (tuple? form))        # a vector literal (reader: tuple)
# A map-literal form is a plain struct, or a phm when the reader preserved a nil
# key/value (Janet structs drop nil). Sets/chars/symbols are tagged structs (have
# :jolt/type); phm carries :jolt/deftype, distinct from those.
(defn h-map? [form]
  (or (and (struct? form) (nil? (form :jolt/type)))
      (phm/phm? form)))
(defn h-set? [form] (and (struct? form) (= :jolt/set (form :jolt/type))))
(defn h-char? [form] (and (struct? form) (= :jolt/char (form :jolt/type))))
# A regex literal #"…" reads as a tagged form {:jolt/type :jolt/tagged :tag :regex
# :form "source"}. The analyzer lowers it to a :regex IR node (Chez emits a
# jolt-regex value; the Janet back end punts to the interpreter, which compiles it
# via the seed PEG engine).
(defn h-regex? [form]
  (and (struct? form) (= :jolt/tagged (form :jolt/type)) (= :regex (form :tag))))
(defn h-regex-source [form] (form :form))

(defn h-literal? [form]
  (or (nil? form) (boolean? form) (number? form) (string? form)
      (keyword? form) (h-char? form)))

# Items of a list/vector as a jolt vector, so the analyzer walks them with Clojure.
(defn h-elements [form] (make-vec form))
(defn h-vector-items [form] (make-vec form))
(defn h-map-pairs [form]
  # reader forms carry source order (jolt-p3c) — Clojure evaluates literal
  # entries left to right; hash order only for constructed maps
  (def order (rdr/form-kv-order form))
  (cond
    order
    (do (def out @[])
        (var i 0)
        (while (< i (length order))
          (array/push out (make-vec [(in order i) (in order (+ i 1))]))
          (+= i 2))
        (make-vec out))
    (phm/phm? form)
    (make-vec (map (fn [e] (make-vec [(in e 0) (in e 1)])) (phm/phm-entries form)))
    (make-vec (map (fn [k] (make-vec [k (get form k)])) (keys form)))))
(defn h-set-items [form] (make-vec (form :value)))

# ---------------------------------------------------------------------------
# Compile-time environment
# ---------------------------------------------------------------------------

# Names the analyzer must NOT treat as a function call: interpreter special forms
# plus definitional/host macros the compiler doesn't lower. The analyzer handles
# a subset (quote/if/do/def/fn*/let*/loop*/recur/throw/try) and falls back to the
# interpreter for the rest. Kept in sync with evaluator/special-symbol? and
# compiler/uncompilable-heads.
(def- special-names
  (let [t @{}]
    (each n ["quote" "syntax-quote" "unquote" "unquote-splicing" "do" "if" "def"
             "defmacro" "fn*" "let*" "loop*" "recur" "throw" "try" "set!"
             # defmulti/defmethod/deftype now compile (macros over *-setup fns).
             "eval" "new"
             # var-get/var-set/var?/alter-var-root/alter-meta!/reset-meta! are
             # plain core fns; find-var/intern are ctx-capturing core fns — all
             # compile as ordinary invokes now (Stage 2 tier 6).
             "."
             # satisfies?/instance?/locking/defonce/read-string/macroexpand-1 and
             # the multimethod table ops (prefer-method/remove-method/
             # remove-all-methods/get-method/methods) are clojure.core fns /
             # overlay macros now (Stage 2 tier 6c) — ordinary invokes.
             # create-ns/remove-ns/find-ns/all-ns/the-ns/resolve/ns-resolve/
             # ns-aliases/ns-imports/ns-interns/refer are ctx-capturing
             # clojure.core fns now (compile as plain invokes — tier 6b), like
             # ns/require/in-ns/use/import/refer-clojure before them.
             # defprotocol/extend-type/extend-protocol/reify/defrecord now expand to
             # plain def + protocol-dispatch/register-method/make-reified/deftype.
             "gen-class"
             # letfn stays: its let* expansion needs letrec semantics (mutual
             # recursion between the fns), which compiled sequential let* lacks.
             "monitor-enter" "monitor-exit" "letfn"]
      (put t n true))
    t))

# Interop-shaped heads the interpreter lowers but the back end doesn't model:
#   (.method obj …) / (.-field obj)  — member access (name starts with ".")
#   (Foo. …)                          — constructor (name ends with "." )
# Treated as special so the analyzer marks them uncompilable and falls back.
(defn- interop-head? [name]
  (def n (length name))
  (and (> n 1)
       (or (= (string/slice name 0 1) ".")
           (= (string/slice name (- n 1)) "."))))

(defn h-special? [name]
  (if (or (get special-names name) (interop-head? name)) true false))

# The namespace being compiled. NOT ctx-current-ns directly: the interpreter
# rebinds current-ns to a fn's defining ns while that fn runs, so an interpreted
# analyzer (defined in jolt.analyzer) would otherwise see jolt.analyzer. The back
# end stashes the real compile ns in :compile-ns before invoking the analyzer.
(defn h-current-ns [ctx] (or (get (ctx :env) :compile-ns) (ctx-current-ns ctx)))

(defn h-macro? [ctx sym]
  (let [v (resolve-var ctx @{} sym)]
    (if (and v (var-macro? v)) true false)))

(defn h-expand-1 [ctx form]
  (let [head (in form 0)
        v (resolve-var ctx @{} head)
        macro-fn (var-get v)]
    (apply macro-fn (tuple/slice form 1))))

# Classify a global (non-local) symbol reference:
#   {:kind :var  :ns NS :name NAME}  — a Jolt var (current ns / clojure.core)
#   {:kind :host :name NAME}         — resolves only via the host env (+, int?, …),
#                                      same fallback the interpreter's resolve-sym uses
#   {:kind :unresolved :name NAME}   — not yet defined (forward reference)
(defn h-resolve-global [ctx sym]
  # Unqualified syms must resolve against the COMPILE ns (h-current-ns), not
  # ctx-current-ns: the analyzer runs interpreted in jolt.analyzer, so during
  # analysis resolve-var's unqualified branch looks in the wrong namespace.
  # Before jolt-2o7.3 the analyzer's lenient fallthrough masked this (it
  # emitted a var-ref against compile-ns for everything 'unresolved').
  (def v (if (sym :ns)
           (resolve-var ctx @{} sym)
           (let [cns (ctx-find-ns ctx (h-current-ns ctx))]
             (or (and cns (ns-find cns (sym :name)))
                 (ns-find (ctx-find-ns ctx "clojure.core") (sym :name))))))
  (if v
    {:kind :var :ns (var-ns v) :name (var-name v)}
    (let [nm (sym :name)
          entry (in (fiber/getenv (fiber/current)) (symbol nm))]
      (if (not (nil? entry))
        {:kind :host :name nm}
        {:kind :unresolved :name nm}))))

(defn h-intern! [ctx ns-name nm]
  (ns-intern (ctx-find-ns ctx ns-name) nm)
  nil)

# Open-world (late-binding) analysis: when set on the ctx env, an unresolved
# symbol emits a var-ref against the compile ns (resolved at runtime) instead of
# punting to the interpreter. The Chez back end (host/chez/driver) sets it: it has
# no interpreter to punt to, and a forward reference to a runtime-interned var
# (e.g. defmulti's setup call) must lower to a var-deref. Off everywhere else, so
# the normal compiler keeps its strict 'Unable to resolve symbol' behavior.
(defn h-late-bind? [ctx] (truthy? (get (ctx :env) :late-bind-unresolved?)))

# ---------------------------------------------------------------------------
# Installation: bind these fns as vars in the `jolt.host` namespace so jolt-core
# can call them. Idempotent per context.
# ---------------------------------------------------------------------------

# Form predicates use `form-*` names (not list?/vector?/map?/set?/char?) so the
# analyzer can refer them unqualified without the bootstrap's core-renames
# intercepting them as the value-level predicates.
# Lower a syntax-quote's inner form to construction code (so the analyzer can
# compile it). The portable analyzer calls this and analyzes the result.
(defn h-syntax-quote-lower [ctx inner]
  (syntax-quote-lower ctx inner))

# Runtime host primitive: set a key on a mutable reference cell (an atom, the
# watches sub-table, ...). The minimal mutation kernel the overlay can't express
# over core fns — putting nil removes the key (Janet table semantics). Returns the
# table so callers can thread; overlay wrappers return the Clojure-meaningful value.
(defn h-ref-put! [tab key val] (put tab key val) tab)

# Runtime host primitive: mint a fresh tagged host table — the value-layer
# constructor the overlay uses to define host-dispatched values (sorted colls).
# Fields are attached with ref-put!.
(defn h-tagged-table [kw] @{:jolt/type kw})

# Raw field read on a host table, BYPASSING collection semantics. The overlay
# needs this to read its own wrappers' fields: plain (get sorted-coll k) is the
# comparator lookup (it dispatches back into the overlay), so reading :entries
# with it would recurse forever.
(defn h-ref-get [tab key] (get tab key))

# Absolute source offset of a list FORM (jolt-fqy), or nil. The analyzer stamps
# it onto :invoke nodes so the success checker can report file:line:col.
(defn h-form-position [form] (rdr/form-pos form))

# ---------------------------------------------------------------------------
# Inline registry (jolt-87f, Route 1 AOT escape analysis). The inline pass
# (jolt.passes) is portable Clojure and can't read Janet var cells, so it asks
# the host whether a given global is inline-eligible and, if so, for its body IR.
# ---------------------------------------------------------------------------

# Is inlining enabled for the unit currently being compiled? :inline? is OFF for
# all of init (core tiers + self-hosted compiler recompile) and flipped on at the
# end of init to the user direct-linking setting, so core compiles exactly as
# before (const-fold only) and only opted-in user code inlines.
(defn h-inline-enabled? [ctx] (if (get (ctx :env) :inline?) true false))

# The stashed inline body for global ns/name, or nil. Returns it only when the
# target is inline-SAFE: a defined var that won't be redefined (not ^:redef /
# ^:dynamic) and that carries a stashed :inline-ir (set by the back end when the
# var's defn compiled as a small, side-effect-bounded fn). The stash is
# {:params [name ...] :body <ir>} — a single fixed arity.
(defn h-inline-ir [ctx ns-name nm]
  (when (get (ctx :env) :inline?)
    (def cell (let [n (ctx-find-ns ctx ns-name)] (and n (ns-find n nm))))
    (when (and cell (table? cell)
               (not (cell :dynamic))
               (not (let [m (cell :meta)] (and m (get m :redef)))))
      (cell :inline-ir))))

# Is `name` (a bare type-name string, e.g. "Vec3") a defrecord/deftype? Both
# expand to define a ->Name positional constructor var (30-macros.clj), so its
# presence is the marker. Lets the analyzer resolve a ^Record type hint to the
# struct fast path: record instances are tables tagged :jolt/deftype (NOT
# :jolt/type), so a raw keyword get is correct for them (jolt-94n).
(defn h-record-ctor-key [ctx name]
  # The home ctor key ("ns/->Name") for a ^Type hint, resolving cross-ns
  # (referred/aliased) records too — nil if `name` is not a known record type.
  (record-hint-ctor-key ctx name))

(defn h-record-type? [ctx name]
  # A ^Type hint names a record iff it resolves to a ctor key (local OR cross-ns,
  # via record-hint-ctor-key — so a referred/aliased foreign record is recognized,
  # not just one whose ->Name happens to be interned in the current ns).
  (if (record-hint-ctor-key ctx name) true false))

# The record-ctor shape registry ("ns/->Name" -> {:fields (:k ..) :type tag ..}),
# or an empty table. Lets scalar-replace (jolt-15jq) recognize a (->Rec ..) call
# and map its positional args to the declared field order — the record analogue
# of the inline map-literal keys the map fold already sees in the IR.
(defn h-record-shapes [ctx]
  (or (get (ctx :env) :record-shapes) @{}))

(def- exports
  {"form-sym?" h-sym? "form-sym-name" h-sym-name "form-sym-ns" h-sym-ns
   "ref-put!" h-ref-put!
   "ref-get" h-ref-get
   "tagged-table" h-tagged-table
   "form-sym-meta" h-sym-meta
   "form-list?" h-list? "form-vec?" h-vector? "form-map?" h-map?
   "form-set?" h-set? "form-char?" h-char? "form-literal?" h-literal?
   "form-regex?" h-regex? "form-regex-source" h-regex-source
   "form-elements" h-elements "form-vec-items" h-vector-items
   "form-map-pairs" h-map-pairs "form-set-items" h-set-items
   "form-special?" h-special? "compile-ns" h-current-ns "form-macro?" h-macro?
   "form-expand-1" h-expand-1 "resolve-global" h-resolve-global
   "form-syntax-quote-lower" h-syntax-quote-lower
   "host-intern!" h-intern!
   "inline-enabled?" h-inline-enabled? "inline-ir" h-inline-ir
   "record-type?" h-record-type? "form-position" h-form-position
   "record-ctor-key" h-record-ctor-key "record-shapes" h-record-shapes
   "late-bind?" h-late-bind?})

(defn install! [ctx]
  (def ns (ctx-find-ns ctx "jolt.host"))
  (eachp [nm f] exports (ns-intern ns nm f))
  ns)
