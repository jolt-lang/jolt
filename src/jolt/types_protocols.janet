# Jolt value layer — protocol/type registry + shape-records
# Extracted from types.janet (jolt-bvek phase 5a split).

(use ./types_symbols)
(use ./types_var)
(use ./types_ns)
(use ./types_ctx)
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

(defn find-method-any-protocol
  "Find a method implementation for a type, searching every protocol it
  implements (dot calls name the method but not the protocol)."
  [ctx type-tag method-name]
  (let [type-impls (get (get (ctx :env) :type-registry) type-tag)]
    (when type-impls
      (var r nil)
      (eachp [_ proto-impls] type-impls
        (when (nil? r) (set r (get proto-impls method-name))))
      r)))

(defn type-satisfies?
  "Check if a type satisfies a protocol."
  [ctx type-tag protocol-name]
  (let [registry (get (ctx :env) :type-registry)
        type-impls (get registry type-tag)]
    (if (and type-impls (get type-impls protocol-name)) true false)))

# --- shape records (hidden classes, jolt-t34) -------------------------------
# A "shape record" is a cheap fixed-layout representation for a map literal
# whose keys are a known compile-time set (e.g. a vec3 {:r :g :b}). It is a
# Janet tuple [SHAPE v0 v1 ...] where SHAPE is an interned descriptor struct
# {:jolt/shape KEYS :idx {k->pos}}. Construction is a tuple (≈2x cheaper than a
# struct), const-keyword lookup compiles to an index, and general map ops below
# recognize it via shape-rec? and treat it as a map — so it is transparent
# wherever it flows. Created only by the backend when JOLT_SHAPE is on and the
# inference proves the shape; the runtime support here is always present so a
# shape value is handled correctly regardless.
(def- shape-cache @{})   # canonical-keys-tuple -> interned shape descriptor
# Canonical key order, OWNED BY THE RUNTIME (jolt-t34): every site that builds or
# reads a shape (shape-for, emit-map, emit-kw-lookup, build-map-literal) derives
# the layout from this one function, so they always agree regardless of what
# order the inference passed the keys in. Sorted by the keys' jdn print form —
# deterministic and total across keywords/strings/numbers/bools.
(defn shape-sort [ks]
  (sort (array ;ks) (fn [a b] (< (string/format "%j" a) (string/format "%j" b)))))
(defn shape-for
  "Interned shape descriptor for a key set. Keys are canonicalized internally,
  so callers need not pre-sort and any permutation yields the same descriptor."
  [keyv]
  (def sk (tuple ;(shape-sort keyv)))
  (or (get shape-cache sk)
      (let [idx @{}]
        (var i 0) (each k sk (put idx k i) (++ i))
        (def desc (struct :jolt/shape sk :idx (table/to-struct idx)))
        (put shape-cache sk desc)
        desc)))
(defn shape-rec? [x]
  (and (tuple? x) (> (length x) 0)
       (struct? (in x 0)) (not (nil? (in (in x 0) :jolt/shape)))))
(defn shape-keys [rec] ((in rec 0) :jolt/shape))
(defn shape-get [rec k default]
  (def desc (in rec 0))
  (def pos (get (desc :idx) k))
  (cond
    (not (nil? pos)) (in rec (+ pos 1))
    # records respond to the virtual :jolt/deftype key with their type tag, so
    # every existing (get obj :jolt/deftype) dispatch site keeps working
    (and (= k :jolt/deftype) (desc :type)) (desc :type)
    default))
(defn shape-assoc [rec k v]
  # assoc on a known key keeps the layout. A new key: a record keeps its type
  # and grows a slot (Clojure records stay records when extended); a plain
  # shape-rec falls back to a struct.
  (def desc (in rec 0))
  (def pos (get (desc :idx) k))
  (cond
    (not (nil? pos)) (let [a (array ;rec)] (put a (+ pos 1) v) (tuple ;a))
    (desc :type)
      (let [new-keys (array ;(desc :jolt/shape) k) idx @{}]
        (var i 0) (each kk new-keys (put idx kk i) (++ i))
        (def ndesc (struct :jolt/shape (tuple ;new-keys) :idx (table/to-struct idx) :type (desc :type)))
        (def out @[ndesc])
        (each kk (desc :jolt/shape) (array/push out (shape-get rec kk nil)))
        (array/push out v)
        (tuple ;out))
    (let [t @{}] (each kk (desc :jolt/shape) (put t kk (shape-get rec kk nil))) (put t k v) (table/to-struct t))))
(defn shape-count [rec] (- (length rec) 1))
(defn shape-contains? [rec k] (not (nil? (get ((in rec 0) :idx) k))))
# a struct snapshot of a shape-rec — the reusable bridge for ops that already
# handle structs (dissoc, vals, seq, equality, print, ...) without per-op code
(defn shape->struct [rec]
  (def desc (in rec 0)) (def t @{})
  (each kk (desc :jolt/shape) (put t kk (in rec (+ 1 (get (desc :idx) kk)))))
  (table/to-struct t))

# --- records as shapes (jolt-t34 R3) ----------------------------------------
# A user record (deftype/defrecord) is a shape-rec whose descriptor ALSO carries
# :type (the type tag). Field layout is the DECLARED field order (not sorted),
# so the positional ->Name constructor maps args to slots directly. The
# descriptor is interned per type tag, so all instances of a type share it.
# record-tag unifies the type accessor over both the new shape-rec records and
# the table form still used for reified protocol objects.
(def- record-desc-cache @{})
(defn record-desc [type-tag field-keys]
  "Build a record descriptor (interned in declared field order) for the given
  key set. Not cached — used for records extended past their declared fields."
  (let [idx @{}]
    (var i 0) (each k field-keys (put idx k i) (++ i))
    (struct :jolt/shape (tuple ;field-keys) :idx (table/to-struct idx) :type type-tag)))
# Interned per (type-tag, field-keys): keying on the tag alone would hand back a
# STALE descriptor after a record is redefined with different fields (a REPL
# redefine, or two same-named records in different test cases) — the new instance
# would carry the old layout. Old instances keep their own descriptor and stay
# valid; new ones get the new layout. (jolt-t34)
(defn record-shape-for [type-tag field-keys]
  (def ck (tuple type-tag (tuple ;field-keys)))
  (or (get record-desc-cache ck)
      (let [desc (record-desc type-tag field-keys)]
        (put record-desc-cache ck desc)
        desc)))
(defn make-record [type-tag field-keys args]
  (def out @[(record-shape-for type-tag field-keys)])
  (var i 0) (each k field-keys (array/push out (in args i)) (++ i))
  (tuple ;out))
(defn record-tag
  "The deftype/record type tag of x, or nil. Covers shape-rec records (descriptor
  :type) and the table form (reified objects, :jolt/deftype)."
  [x]
  (cond
    (and (tuple? x) (> (length x) 0) (struct? (in x 0))) (get (in x 0) :type)
    (and (table? x) (get x :jolt/deftype)) (get x :jolt/deftype)))

