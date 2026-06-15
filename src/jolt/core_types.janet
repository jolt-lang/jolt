# Jolt Core — vector helpers, predicates, math, comparison, equality
# Extracted from core.janet (jolt-nma8, phase 2b split).

(use ./types)
(use ./phm)
(use ./phs)
(use ./lazyseq)
(use ./regex)
(use ./config)
(use ./pv)
(use ./plist)

# ------------------------------------------------------------
# Vector representation helpers
#
# In immutable mode a vector value is a structural-sharing persistent vector
# (pvec); in mutable mode it is a plain Janet array. Janet tuples may also still
# appear (e.g. literals that have not been routed through make-vec), so the read
# helpers below accept tuple, pvec and (mutable mode) array uniformly.
# ------------------------------------------------------------

(defn jvec?
  "True when x is a vector VALUE. In immutable mode that is a persistent vector
  or tuple; in mutable mode vectors are plain arrays (so vectors and lists share
  one fast representation — `vector?` is true for both)."
  [x]
  (if mutable?
    (or (array? x) (tuple? x))
    (or (tuple? x) (pvec? x))))

(defn vcount [x] (if (pvec? x) (pv-count x) (length x)))
(defn vnth [x i] (if (pvec? x) (pv-nth x i) (in x i)))

(defn vview
  "An indexed (tuple/array) view of a vector value, for iteration/slicing."
  [x]
  (if (pvec? x) (pv->array x) x))

(defn make-vec
  "Build a vector value from a Janet array/tuple of elements, honoring the
  build-time collection mode."
  [xs]
  (if mutable? (array ;xs) (pv-from-indexed xs)))

(defn core-transient?
  "True when x is a transient (a mutable scratch collection). See `transient`."
  [x]
  (and (table? x) (= :jolt/transient (get x :jolt/type))))

# Sorted-coll tag checks + entries view, defined this early because canon-key,
# empty?, and jolt-equal? (all below) need them. The sorted-coll SEMANTICS are
# pure Clojure (core/25-sorted.clj); see the dispatch section further down.
# SEED-TWIN: sorted-map?/sorted-set?/sorted? also live in the overlay
# (jolt-core/clojure/core/25-sorted.clj); the overlay copies are the public ones
# (NOT in core-bindings). These seed copies exist only for earlier-tier seed
# dispatch. Change both copies together — docs/seed-overlay-registry.md.
(defn core-sorted-map? [x] (and (table? x) (= :jolt/sorted-map (x :jolt/type))))
(defn core-sorted-set? [x] (and (table? x) (= :jolt/sorted-set (x :jolt/type))))
(defn core-sorted? [x] (or (core-sorted-map? x) (core-sorted-set? x)))
# The :entries vector as a Janet array (entries are jolt vectors: pvecs in
# immutable mode, arrays in mutable mode) — for the seed's printers/equality.
(defn sorted-entries-arr [coll]
  (let [e (coll :entries)] (if (pvec? e) (pv->array e) e)))

# Lazy cell chain over an indexed (tuple/array) collection, walking by INDEX —
# O(1) per step. Slicing the remainder per step (the old shape) made every
# full walk over a concrete collection O(n^2).
(defn indexed-cells [t i]
  (if (>= i (length t)) nil
    @[(in t i) (fn [] (indexed-cells t (+ i 1)))]))

# Canonicalize a collection key/element to a value-hashable Janet struct/tuple so
# the PHM/PHS treat value-equal maps/vectors as the same key (Janet hashes tables
# by identity otherwise). Installed into phm via set-canonicalize-key!.
(var canon-key nil)
(set canon-key
  (fn [k]
    (cond
      (pvec? k) (tuple ;(map canon-key (pv->array k)))
      (plist? k) (tuple ;(map canon-key (pl->array k)))
      (set? k) (do (def t @{}) (each e (phs-seq k) (put t (canon-key e) true)) (table/to-struct t))
      (phm? k) (do (def t @{}) (each pair (phm-entries k) (put t (canon-key (in pair 0)) (canon-key (in pair 1)))) (table/to-struct t))
      # sorted colls canonicalize like their unsorted counterparts, so
      # (get {(sorted-map :a 1) :hit} {:a 1}) finds the key
      (core-sorted-map? k) (do (def t @{}) (each e (sorted-entries-arr k) (put t (canon-key (vnth e 0)) (canon-key (vnth e 1)))) (table/to-struct t))
      (core-sorted-set? k) (do (def t @{}) (each x (sorted-entries-arr k) (put t (canon-key x) true)) (table/to-struct t))
      (and (table? k) (get k :jolt/deftype))
        (do (def t @{}) (each kk (keys k) (when (not= kk :jolt/deftype) (put t kk (canon-key (get k kk))))) (table/to-struct t))
      (struct? k) (do (def t @{}) (each kk (keys k) (put t (canon-key kk) (canon-key (get k kk)))) (table/to-struct t))
      (array? k) (tuple ;(map canon-key k))
      (tuple? k) (tuple ;(map canon-key k))
      k)))
(set-canonicalize-key! canon-key)

# All [k v] entries of a map (struct or phm), nil-valued keys included. Use this
# instead of (keys (phm-to-struct m)) — phm-to-struct drops keys whose value is
# nil, which is exactly what Clojure maps must keep.
(defn map-entries-of [m]
  (if (phm? m) (phm-entries m) (map (fn [k] [k (in m k)]) (keys m))))

# assoc one entry onto a map value (struct or phm), preserving a nil key/value and
# value-comparing collection keys (promotes a struct to a phm when needed). A
# single-entry core-assoc usable by fns defined before core-assoc itself.
(defn map-assoc1 [m k v]
  (cond
    (phm? m) (phm-assoc m k v)
    (or (nil? k) (nil? v) (table? k) (array? k))
      (do (var p (make-phm)) (each ek (keys m) (set p (phm-assoc p ek (in m ek)))) (phm-assoc p k v))
    (do (def t (merge @{} m)) (put t k v) (table/to-struct t))))

# Build a map from a flat [k v k v ...] array: a phm when any key/value is nil or
# a key is a collection (value hashing); a struct otherwise. One O(n) pass.
(defn- kvs->map [kvs]
  (var need-phm false) (var i 0)
  (while (< i (length kvs))
    (let [k (in kvs i) v (in kvs (+ i 1))]
      (when (or (nil? k) (nil? v) (table? k) (array? k)) (set need-phm true)))
    (+= i 2))
  (if need-phm
    (do (var m (make-phm)) (var j 0)
        (while (< j (length kvs)) (set m (phm-assoc m (in kvs j) (in kvs (+ j 1)))) (+= j 2)) m)
    (struct ;kvs)))

(defn realize-for-iteration [c]
  "Normalize a seqable to a Janet array/tuple for iteration: pvec -> array,
  set -> seq, lazy-seq -> realized array; others pass through. Warning: will
  loop on infinite lazy-seqs. Terminates on the empty cell, not on nil."
  (cond
    # nil is an empty seq in Clojure — iterating it yields nothing.
    (nil? c) @[]
    (shape-rec? c) (map (fn [k] (tuple k (shape-get c k nil))) (shape-keys c))
    (pvec? c) (pv->array c)
    (plist? c) (pl->array c)
    (set? c) (phs-seq c)
    (phm? c) (phm-entries c)
    # sorted colls iterate their comparator-ordered entries/elements
    (core-sorted? c) (sorted-entries-arr c)
    # byte array (Janet buffer) -> array of byte values
    (buffer? c) (let [a @[]] (each x c (array/push a x)) a)
    # struct map literal (no :jolt/type marker — not a symbol/char) -> entries
    (and (struct? c) (nil? (get c :jolt/type))) (map (fn [k] (tuple k (get c k))) (keys c))
    # raw host table (System/getenv, os/environ) — also a map: entries
    (and (table? c) (nil? (get c :jolt/type)) (nil? (get c :jolt/deftype)))
      (map (fn [k] (tuple k (get c k))) (keys c))
    (lazy-seq? c)
    (do
      (var items @[])
      (var cur c)
      (var go true)
      (while go
        (let [cell (realize-ls cur)]
          (if (or (nil? cell) (= :jolt/pending cell) (= 0 (length cell)))
            (set go false)
            (do
              (array/push items (in cell 0))
              (let [rt (in cell 1)]
                (if (nil? rt) (set go false) (set cur (ls-rest-cached cur rt))))))))
      items)
    c))

# Syntax-quote form builders. The syntax-quote lowering (evaluator) emits calls to
# these so a `(...)/`[...] body is plain compilable code instead of an interpreted
# special form. A list FORM is a Janet array, a vector FORM a tuple (the reader's
# representation), so these build those types. Each concat part is either a 1-elem
# wrap (__sq1, a non-spliced item) or a spliced seq (~@), flattened in order.
(defn core-sq1 [x] @[x])

(defn core-sqcat [& parts]
  (def r @[])
  (each p parts (each x (realize-for-iteration p) (array/push r x)))
  r)

(defn core-sqvec [& parts]
  (def r @[])
  (each p parts (each x (realize-for-iteration p) (array/push r x)))
  (tuple/slice r))

# Map builder: parts are alternating k v (no splicing in map syntax-quote).
(defn core-sqmap [& parts]
  # A syntax-quoted map template is Clojure's array-map case: construction
  # order is source order and must survive into the built map, which usually
  # becomes a FORM whose entries the evaluator walks (jolt-p3c). Same
  # carriers as the reader: struct prototype / phm field.
  (def kvs (array ;parts))
  (def m (kvs->map kvs))
  (cond
    (struct? m) (struct/with-proto (struct :jolt/kv-order (tuple/slice kvs)) ;kvs)
    (table? m) (do (put m :jolt/kv-order (tuple/slice kvs)) m)
    m))

# Set builder: like core-sqvec but yields a set, so `#{~@a} splices into a set.
(defn core-sqset [& parts]
  (def r @[])
  (each p parts (each x (realize-for-iteration p) (array/push r x)))
  (apply make-phs r))

# ============================================================
# Predicates
# ============================================================

# SEED-TWIN: char? is also defined in the overlay (jolt-core/clojure/core/
# 20-coll.clj) and the overlay copy is the public one (NOT in core-bindings).
# This seed copy is internal type dispatch only. docs/seed-overlay-registry.md.
(defn core-char? [x] (and (struct? x) (= :jolt/char (x :jolt/type))))
(defn char-code [c] (c :ch))
(defn char->string [c] (string/from-bytes (c :ch)))

(defn core-nil? [x] (nil? x))
(defn core-not [x] (if x false true))
# some? / true? / false? now live in the Clojure collection tier.
(defn core-string? [x] (string? x))
(defn core-number? [x] (number? x))
(defn core-fn? [x] (or (function? x) (cfunction? x)))
(defn core-keyword? [x] (keyword? x))
(defn core-symbol? [x] (and (struct? x) (= :symbol (x :jolt/type))))
# A record shape-rec is a Janet tuple (jvec? true), but a record is NOT a vector
# in Clojure — `(vector? record)` is false, and so is `(sequential? record)`.
# Excluding it here keeps map-destructuring of a record off the `& {:keys}` kwargs
# coerce path (which does `(apply hash-map x)` for a sequential x). jvec? itself
# stays as-is for internal representation dispatch.
(defn core-vector? [x] (and (jvec? x) (not (shape-rec? x))))
# map? is STRICT: a plain struct map literal, a phm, a sorted map, or a record.
# Tagged structs (symbols/chars/uuids — anything with :jolt/type) are VALUES,
# not maps. (sorted-map? is defined later, so the table check is inlined.)
(defn core-map? [x]
  (or (shape-rec? x)
      (phm? x)
      (and (struct? x) (nil? (get x :jolt/type)))
      (and (table? x)
           (or (not (nil? (get x :jolt/deftype)))
               (= :jolt/sorted-map (get x :jolt/type))))))
# seq? is true only for actual sequences (lists, lazy-seqs) — NOT vectors, which
# are not ISeq in Clojure. (A Janet array represents a Clojure list/seq result.)
(defn core-seq? [x] (or (array? x) (plist? x) (lazy-seq? x)))
# coll? mirrors map?'s strictness for structs/tables, and includes the sorted
# collections and records (IPersistentCollection in Clojure).
(defn core-coll? [x]
  (or (array? x) (tuple? x) (pvec? x) (plist? x) (phm? x) (set? x) (lazy-seq? x)
      (and (struct? x) (nil? (get x :jolt/type)))
      (and (table? x)
           (or (not (nil? (get x :jolt/deftype)))
               (= :jolt/sorted-map (get x :jolt/type))
               (= :jolt/sorted-set (get x :jolt/type))))))



(defn core-identical? [a b] (= a b))

# Strictness helpers: like Clojure, numeric ops reject non-numbers, and the
# integer ops (odd?/even?) reject non-integers (incl. infinities, NaN, fractions).
(defn- finite-num? [x] (and (number? x) (= x x) (< (if (< x 0) (- x) x) math/inf)))
(defn- need-num [x op]
  (if (number? x) x (error (string op " requires a number, got " (type x)))))
(defn- need-int [x op]
  (if (and (number? x) (= x x) (< (if (< x 0) (- x) x) math/inf) (= x (math/floor x))) x
    (error (string op " requires an integer"))))

# zero? / pos? live in the syntax tier (core/00-syntax.clj) — empty? and the
# analyzer use them; neg? lives in the collection tier (20-coll.clj).
# even?/odd? are PERF-WALL residents: (filter even? ...) is idiomatic and the
# overlay versions cost an extra call layer per element (seq-pipe bench 4x).
(defn core-even? [n] (= 0 (% (need-int n "even?") 2)))
(defn core-odd? [n] (not= 0 (% (need-int n "odd?") 2)))

# Finite integral number: NaN and the infinities are NOT integers (floor of
# inf is inf, so the naive floor check wrongly accepted them).
(defn core-integer? [x]
  (and (number? x) (= x x)
       (< x math/inf) (> x (- math/inf))
       (= x (math/floor x))))
(defn core-list? [x] (or (plist? x) (and (array? x) (not (get x :jolt/type)))))

# empty? now lives in the syntax tier (core/00-syntax.clj): the expanders
# call it, so it must exist before the kernel tier compiles.

# every? lives in the syntax tier (core/00-syntax.clj) — the analyzer uses it;
# the canonical seq/first/next walk short-circuits lazy seqs the same way.

# ============================================================
# Math — Clojure semantics (variadic, / with one arg = reciprocal)
# ============================================================

(def core-+ (fn [& args] (if (= 0 (length args)) 0 (+ ;args))))

(def core-sub
  (fn [& args]
    (if (= 0 (length args))
      (error "Wrong number of args (0) passed to: -")
      (apply - args))))

(def core-* (fn [& args] (if (= 0 (length args)) 1 (* ;args))))

(def core-/
  (fn [& args]
    (case (length args)
      0 (error "Wrong number of args (0) passed to: /")
      1 (/ 1 (args 0))
      (apply / args))))

(def core-inc inc)
(def core-dec dec)
# Clojure integer division: quot truncates toward zero; rem matches the sign of
# the dividend; mod matches the sign of the divisor (floored).
(def core-quot (fn [n d]
  (when (or (not (finite-num? n)) (not (finite-num? d))) (error "quot requires finite numbers"))
  (when (= d 0) (error "Divide by zero"))
  (let [q (/ n d)] (if (< q 0) (math/ceil q) (math/floor q)))))
(def core-rem (fn [n d] (- n (* (core-quot n d) d))))
(def core-mod (fn [n d]
  (let [m (core-rem n d)]
    (if (or (= m 0) (= (> n 0) (> d 0))) m (+ m d)))))

# max / min now live in the Clojure collection tier (canonical pairwise
# >/<, so non-numbers throw and NaN behaves as on the JVM).


(defn core-rand [& n] (let [r (math/random)] (if (empty? n) r (* r (in n 0)))))
# rand-int / shuffle / random-uuid now live in the Clojure collection tier
# over the rand host seam (canonical: rand-int truncates toward zero).

# ============================================================
# Comparison
# ============================================================

(defn- eq-seqable
  "If x is a Clojure sequential (vector/list/lazy-seq), return its elements as
  an array; otherwise nil. Lets = compare across tuple/array/lazy-seq."
  [x]
  (cond
    # a shape-rec is a MAP, not a sequence, even though it is a tuple
    (shape-rec? x) nil
    (lazy-seq? x) (realize-for-iteration x)
    (pvec? x) (pv->array x)
    (plist? x) (pl->array x)
    (tuple? x) x
    (array? x) x
    nil))

(defn- eq-map-pairs
  "Return [k v] pairs for a map-like value (phm/sorted-map/struct/table), else nil."
  [x]
  (cond
    # a record shape-rec returns nil so equality falls to deep=, which is
    # type-aware (the descriptor is interned per type): a record equals only a
    # same-type record, never a plain map — mirroring the :jolt/deftype table
    # form below. A plain-map shape-rec compares by pairs.
    (shape-rec? x) (if (record-tag x) nil (map (fn [k] @[k (shape-get x k nil)]) (shape-keys x)))
    (phm? x) (phm-entries x)
    # sorted-map equals any map with the same pairs (representation-agnostic, as
    # in Clojure); sorted-set is handled by the set branch of jolt-equal?
    (core-sorted-map? x) (map (fn [e] @[(vnth e 0) (vnth e 1)]) (sorted-entries-arr x))
    (core-sorted-set? x) nil
    (and (table? x) (get x :jolt/deftype)) nil
    (struct? x) (pairs x)
    (table? x) (pairs x)
    nil))

# Elements of a set-like value (phs or sorted-set) as an array, else nil.
(defn- eq-set-elems [x]
  (cond
    (set? x) (phs-seq x)
    (core-sorted-set? x) (sorted-entries-arr x)
    nil))

(var jolt-equal? nil)
(set jolt-equal?
  (fn [a b]
    (let [sa (eq-seqable a) sb (eq-seqable b)]
      (cond
        # both sequential: compare element-wise (vectors/lists/lazy-seqs equal)
        (and sa sb)
          (if (= (length sa) (length sb))
            (do (var ok true) (var i 0)
              (while (and ok (< i (length sa)))
                (unless (jolt-equal? (in sa i) (in sb i)) (set ok false))
                (++ i))
              ok)
            false)
        (or sa sb) false
        # sets (phs or sorted-set, in any combination)
        (or (set? a) (set? b) (core-sorted-set? a) (core-sorted-set? b))
          # value-based: same size and every element of a is value-equal to some
          # element of b (so #{ {:a 1} } equals #{ (hash-map :a 1) } regardless of
          # the elements' underlying representations)
          (let [ea (eq-set-elems a) eb (eq-set-elems b)]
            (if (and ea eb (= (length ea) (length eb)))
              (do
                (var ok true)
                (each x ea
                  (unless (some (fn [y] (jolt-equal? x y)) eb) (set ok false)))
                ok)
              false))
        # maps: compare key/value pairs recursively, order-independent
        true
          (let [pa (eq-map-pairs a) pb (eq-map-pairs b)]
            (if (or pa pb)
              (if (and pa pb (= (length pa) (length pb)))
                (do (var ok true)
                  (each pair pa
                    (let [k (in pair 0) v (in pair 1)
                          found (do (var fv :jolt/none)
                                  (each p2 pb (when (jolt-equal? k (in p2 0)) (set fv (in p2 1))))
                                  fv)]
                      (unless (and (not= found :jolt/none) (jolt-equal? v found)) (set ok false))))
                  ok)
                false)
              (deep= a b)))))))

(defn core-= [& args]
  (if (< (length args) 2) true
    (do
      (var ok true)
      (var i 0)
      (while (and ok (< i (dec (length args))))
        (unless (jolt-equal? (args i) (args (+ i 1))) (set ok false))
        (++ i))
      ok)))

# not= lives in the syntax tier (core/00-syntax.clj) — the kernel uses it.

# Comparisons are variadic: (< a b c) means a < b < c.
(defn- chain-cmp [op opname xs]
  # 1-arity (e.g. (< x)) is true regardless of x and does no type check.
  (when (>= (length xs) 2) (each x xs (need-num x opname)))
  (var ok true) (var i 0)
  (while (and ok (< i (dec (length xs))))
    (unless (op (in xs i) (in xs (+ i 1))) (set ok false))
    (++ i))
  ok)
(defn core-< [& xs] (chain-cmp < "<" xs))
(defn core-> [& xs] (chain-cmp > ">" xs))
(defn core-<= [& xs] (chain-cmp <= "<=" xs))
(defn core->= [& xs] (chain-cmp >= ">=" xs))

# ============================================================
