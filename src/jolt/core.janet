# Jolt Core Library
# Clojure-compatible core functions for the Jolt interpreter.

(use ./types)
(use ./phm)
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
(defn- map-entries-of [m]
  (if (phm? m) (phm-entries m) (map (fn [k] [k (in m k)]) (keys m))))

# assoc one entry onto a map value (struct or phm), preserving a nil key/value and
# value-comparing collection keys (promotes a struct to a phm when needed). A
# single-entry core-assoc usable by fns defined before core-assoc itself.
(defn- map-assoc1 [m k v]
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
(defn core-sqmap [& parts] (kvs->map (array ;parts)))

# Set builder: like core-sqvec but yields a set, so `#{~@a} splices into a set.
(defn core-sqset [& parts]
  (def r @[])
  (each p parts (each x (realize-for-iteration p) (array/push r x)))
  (apply make-phs r))

# ============================================================
# Predicates
# ============================================================

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
(defn core-vector? [x] (jvec? x))
# map? is STRICT: a plain struct map literal, a phm, a sorted map, or a record.
# Tagged structs (symbols/chars/uuids — anything with :jolt/type) are VALUES,
# not maps. (sorted-map? is defined later, so the table check is inlined.)
(defn core-map? [x]
  (or (phm? x)
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
# Collections
# ============================================================

# Is x a map value (for conj/merge semantics: conj-ing a map merges its entries)?
(defn- map-value? [x]
  (or (phm? x) (and (struct? x) (nil? (get x :jolt/type)))))

# --- Sorted collections (sorted-map / sorted-set) -------------------------------
# Pure Clojure now (stage 3, jolt-0lj — jolt-core/clojure/core/25-sorted.clj).
# A sorted coll is a tagged table {:jolt/type .. :entries SORTED-VECTOR :cmp
# :ops {kw fn}} whose ops travel WITH the value, so the seed's dispatch
# branches below are each a one-line call through (coll :ops) — no module-level
# hooks, correct across contexts/forks/AOT images. The tag predicates and the
# entries view live near the top of this module (canon-key/empty?/equality
# need them); only this dispatch accessor is left here.
(defn sorted-op
  "The overlay-attached implementation of `op` for sorted coll `coll`."
  [coll op]
  (get (coll :ops) op))

(defn core-conj [& args]
  (if (= 0 (length args)) (make-vec @[])        # (conj) -> []
  (let [coll (first args) xs (tuple/slice args 1)]
  (if (nil? coll)
    # conj onto nil builds a list (prepends): (conj nil 1 2) -> (2 1)
    (do (var result nil) (each x xs (set result (pl-cons x result))) result)
  (if (core-sorted? coll)
    ((sorted-op coll :conj) coll xs)
  (if (pvec? coll)
    (do (var result coll) (each x xs (set result (pv-conj result x))) result)
  (if (plist? coll)
    # list: prepend, O(1) per element via structural sharing
    (do (var result coll) (each x xs (set result (pl-cons x result))) result)
  (if (tuple? coll)
    (tuple/slice (tuple ;(array/concat (array/slice coll) xs)))
    (if (array? coll)
      (if mutable?
        # mutable mode: arrays are vectors — append in place
        (do (each x xs (array/push coll x)) coll)
        # immutable mode: arrays are lists — prepend onto a persistent cons node,
        # sharing the original array as the tail (O(1) per element, no copy)
        (do (var result coll) (each x xs (set result (pl-cons x result))) result))
      (if (set? coll)
        (apply phs-conj coll xs)
        # conj onto a seq prepends (Clojure: a Cons cell). Without this branch a
        # lazy-seq fell into the MAP fallback below — clojure.data/diff relies on
        # (conj seq x) via set/union over (keys m), which is now a lazy seq.
        (if (lazy-seq? coll)
          (do (var result coll)
            (each x xs (set result (pl-cons x (realize-for-iteration result))))
            result)
        (if (phm? coll)
          (do
            (var result coll)
            (each x xs
              (cond
                # conj nil onto a map is a no-op (Clojure)
                (nil? x) nil
                (map-value? x)
                # conj a map -> merge its entries
                (each e (map-entries-of x)
                  (set result (phm-assoc result (in e 0) (in e 1))))
                # a [k v] entry: exactly a 2-element vector (Clojure throws
                # otherwise — and merge inherits this strictness through conj)
                (and (or (pvec? x) (tuple? x) (array? x)) (= 2 (vcount x)))
                (set result (phm-assoc result (vnth x 0) (vnth x 1)))
                (error "Vector arg to map conj must be a pair")))
            result)
          (do
            (var result coll)
            (each x xs
              (cond
                # conj nil onto a map is a no-op (Clojure)
                (nil? x) nil
                (map-value? x)
                # conj a map -> merge its entries
                (each e (map-entries-of x)
                  (set result (map-assoc1 result (in e 0) (in e 1))))
                # a [k v] entry: exactly a 2-element vector (Clojure throws
                # otherwise — and merge inherits this strictness through conj)
                (and (or (pvec? x) (tuple? x) (array? x)) (= 2 (vcount x)))
                (set result (map-assoc1 result (vnth x 0) (vnth x 1)))
                (error "Vector arg to map conj must be a pair")))
            result)))))))))))))

(defn core-assoc [m & kvs]
  (when (odd? (length kvs))
    (error "assoc expects an even number of key/value arguments"))
  # assoc is defined on maps, vectors and nil; reject other shapes
  (when (or (number? m) (string? m) (buffer? m) (keyword? m) (boolean? m)
            (plist? m) (set? m) (core-transient? m) (core-sorted-set? m)
            (and (struct? m) (get m :jolt/type)))
    (error (string "assoc requires a map or vector, got " (type m))))
  (cond
    (core-sorted-map? m) ((sorted-op m :assoc) m kvs)
    (phm? m)
      (do (var result m) (var i 0) (while (< i (length kvs)) (set result (phm-assoc result (kvs i) (kvs (+ i 1)))) (+= i 2)) result)
    (pvec? m)
      (do (var result m) (var i 0)
        (while (< i (length kvs))
          (let [idx (kvs i)]
            (when (not (and (number? idx) (= idx (math/floor idx)) (>= idx 0) (<= idx (pv-count result))))
              (error (string "Index " idx " out of bounds for assoc on a vector of length " (pv-count result))))
            (set result (pv-assoc result idx (kvs (+ i 1)))))
          (+= i 2)) result)
    # vector: assoc by integer index (appending at count is allowed); stays a vector
    (or (tuple? m) (array? m))
      (do (var result (array/slice m)) (var i 0)
        (while (< i (length kvs))
          (let [idx (kvs i) v (kvs (+ i 1))]
            (when (not (and (number? idx) (= idx (math/floor idx)) (>= idx 0) (<= idx (length result))))
              (error (string "Index " idx " out of bounds for assoc on a vector of length " (length result))))
            (if (= idx (length result)) (array/push result v) (put result idx v)))
          (+= i 2))
        (if (tuple? m) (tuple/slice (tuple ;result)) result))
    # map (struct/table). Promote to a phm when any new key is a collection (a
    # Janet struct/table would key it by identity) or any new key/value is nil (a
    # struct drops nil; phm preserves it, matching Clojure). m itself is a struct
    # here (phm handled above), so only the new kvs can introduce these.
    (let [coll-key (do (var c false) (var i 0)
                     (while (< i (length kvs))
                       (let [k (in kvs i) v (in kvs (+ i 1))]
                         (when (or (table? k) (array? k) (nil? k) (nil? v)) (set c true)))
                       (+= i 2)) c)]
      (if coll-key
        (do (var result (make-phm))
            (when m (each k (keys m) (set result (phm-assoc result k (get m k)))))
            (var i 0) (while (< i (length kvs)) (set result (phm-assoc result (in kvs i) (in kvs (+ i 1)))) (+= i 2))
            result)
        (do (var result @{}) (when m (each k (keys m) (put result k (get m k))))
          (var i 0) (while (< i (length kvs)) (let [k (kvs i) v (kvs (+ i 1))] (put result k v) (+= i 2)))
          (if (struct? m) (table/to-struct result) result))))))

(defn core-dissoc [m & ks]
  (cond
    (nil? m) nil
    (core-sorted-map? m) ((sorted-op m :dissoc) m ks)
    (phm? m) (do (var result m) (each k ks (set result (phm-dissoc result k))) result)
    # reject clearly non-map values (scalars, sequences, sets, symbol/char structs)
    (or (number? m) (string? m) (buffer? m) (keyword? m) (boolean? m)
        (pvec? m) (plist? m) (tuple? m) (array? m) (set? m) (core-transient? m)
        (and (struct? m) (get m :jolt/type)))
      (error (string "dissoc requires a map, got " (type m)))
    # struct map / sorted-map / record / meta-wrapped map
    (do (var result @{}) (each k (keys m) (var in-ks false) (each k2 ks (if (deep= k k2) (do (set in-ks true) (break)))) (if (not in-ks) (put result k (m k))))
      (if (struct? m) (table/to-struct result) result))))

(defn core-get [m k &opt default]
  (default default nil)
  (if (nil? m) default
    (if (core-sorted? m) ((sorted-op m :get) m k default)
    (if (core-transient? m)
      (case (m :kind)
        :vector (if (and (number? k) (>= k 0) (< k (length (m :arr)))) (in (m :arr) k) default)
        :map (let [p (get (m :tbl) (canon-key k))] (if p (in p 1) default))
        :set (if (nil? (get (m :tbl) (canon-key k))) default k))
    (if (set? m) (phs-get m k default)
      (if (phm? m) (phm-get m k default)
        (if (pvec? m)
          (if (and (number? k) (>= k 0) (< k (pv-count m))) (pv-nth m k) default)
        (if (or (struct? m) (table? m))
          (let [v (m k)]
            (if (nil? v) default v))
          (if (and (or (tuple? m) (array? m)) (number? k) (>= k 0) (< k (length m)))
            (in m k)
            default)))))))))

# Runtime invoke dispatch for COMPILED code (interpreter uses evaluator's
# jolt-invoke). Handles real functions plus Clojure IFn collections.
(defn jolt-call [f & args]
  (cond
    (or (function? f) (cfunction? f)) (apply f args)
    (keyword? f) (core-get (get args 0) f (get args 1))
    (and (struct? f) (= :symbol (f :jolt/type))) (core-get (get args 0) f (get args 1))
    (core-sorted? f) ((sorted-op f :get) f (get args 0) (get args 1))
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
    # Map literal (struct with no :jolt/type marker) or a record: callable as a
    # key lookup. A TAGGED struct (char/etc.) is NOT a fn — symbols are handled
    # above; everything else with a :jolt/type falls through to the error.
    (or (and (struct? f) (nil? (get f :jolt/type))) (and (table? f) (get f :jolt/deftype)))
      (let [v (get f (get args 0) :jolt/not-found)]
        (if (= v :jolt/not-found) (get args 1) v))
    (error (string "Cannot call " (type f) " as a function"))))

(defn core-apply
  "(apply f a b ... coll) — call f with the leading args plus the elements of
  the final collection spliced in. Materializes pvec/lazy-seq/set tails."
  [f & args]
  (let [n (length args)]
    (if (= n 0)
      (jolt-call f)
      (let [fixed (array/slice args 0 (- n 1))
            t (in args (- n 1))
            tail (cond (nil? t) []   # (apply f x nil) == (f x), as in Clojure
                       (set? t) (phs-seq t) (phm? t) (tuple ;(phm-entries t))
                       (realize-for-iteration t))]
        (jolt-call f ;fixed ;tail)))))

# get-in now lives in the Clojure collection tier (core/20-coll.clj).

(defn core-contains? [coll key]
  (if (core-sorted? coll) (if ((sorted-op coll :contains) coll key) true false)
  (if (core-transient? coll)
    (case (coll :kind)
      :vector (and (number? key) (>= key 0) (< key (length (coll :arr))))
      (not (nil? (get (coll :tbl) (canon-key key)))))
  (if (set? coll) (phs-contains? coll key)
    (if (phm? coll) (phm-contains? coll key)
      (if (pvec? coll) (and (number? key) (>= key 0) (< key (pv-count coll)))
      (if (struct? coll) (not (nil? (coll key)))
        (if (table? coll) (not (nil? (coll key)))
          (if (or (tuple? coll) (array? coll))
            (and (number? key) (>= key 0) (< key (length coll)))
            false)))))))))

# Coerce a Clojure IFn value to a Janet-callable fn for higher-order fns
# (map/filter/sort-by/group-by/...). Janet functions pass through; a keyword or
# symbol becomes a key lookup, a map a key lookup, a set a membership test — so
# (map :k coll), (sort-by :k coll), (filter a-set coll) work.
(defn- as-fn [f]
  (cond
    (or (function? f) (cfunction? f)) f
    (keyword? f) (fn [x &opt d] (core-get x f d))
    (core-symbol? f) (fn [x &opt d] (core-get x f d))
    (phm? f) (fn [k &opt d] (core-get f k d))
    (set? f) (fn [x &opt d] (if (core-contains? f x) x d))
    true f))

# Sorted collections — minimal: backed by a struct (map) / sorted array (set),
# ordered by key/element on read. Defined early so seq/count/get can dispatch.
# sorted-map/sorted-set predicates, constructors and ops live ABOVE core-conj so
# the collection fns (conj/assoc/get/contains?/…) can branch on them.

(defn core-count [coll]
  (cond
    (nil? coll) 0
    (core-transient? coll) (length (if (= :vector (coll :kind)) (coll :arr) (coll :tbl)))
    (core-sorted? coll) ((sorted-op coll :count) coll)
    (lazy-seq? coll) (ls-count coll)
    (pvec? coll) (pv-count coll)
    (plist? coll) (pl-count coll)
    (set? coll) (coll :cnt)
    (phm? coll) (coll :cnt)
    (and (table? coll) (get coll :jolt/deftype)) (- (length (keys coll)) 1)
    (or (string? coll) (buffer? coll) (struct? coll) (tuple? coll) (array? coll)) (length coll)
    # count is undefined on scalars (numbers/keywords/symbols/booleans/chars)
    (error (string "count not supported on " (type coll)))))

(defn core-first [coll]
  (cond
    (core-sorted? coll) ((sorted-op coll :first) coll)
    (lazy-seq? coll) (ls-first coll)
    (pvec? coll) (if (= 0 (pv-count coll)) nil (pv-nth coll 0))
    (plist? coll) (if (pl-empty? coll) nil (pl-first coll))
    # maps and sets: first of their seq (an entry / element)
    (phm? coll) (let [e (phm-entries coll)] (if (= 0 (length e)) nil (in e 0)))
    (set? coll) (let [s (phs-seq coll)] (if (= 0 (length s)) nil (in s 0)))
    (and (struct? coll) (nil? (get coll :jolt/type)))
      (let [ks (keys coll)] (if (= 0 (length ks)) nil (tuple (in ks 0) (get coll (in ks 0)))))
    (nil? coll) nil
    (string? coll) (if (= 0 (length coll)) nil (make-char (in coll 0)))
    # scalars aren't seqable
    (or (number? coll) (boolean? coll) (keyword? coll) (and (struct? coll) (get coll :jolt/type)))
      (error (string "first not supported on " (type coll)))
    (= 0 (length coll)) nil
    (in coll 0)))

(defn- seq-done?
  "True when cursor c (a lazy-seq or a concrete collection) is exhausted.
  Uses cell realization for lazy-seqs so nil elements don't end the seq early."
  [c]
  (if (lazy-seq? c)
    (let [cell (realize-ls c)]
      (or (nil? cell) (= :jolt/pending cell) (= 0 (length cell))))
    (or (nil? c) (= 0 (length c)))))

(defn core-rest [coll]
  (cond
    # rest never returns nil — Clojure's rest yields () on an exhausted seq.
    (lazy-seq? coll) (let [r (ls-rest coll)] (if (nil? r) @[] r))
    (plist? coll) (pl-rest coll)
    # Indexed collections: an O(1) lazy view from index 1 (Clojure: rest of a
    # vector is a seq, not a vector). Slicing per step made first/rest loops
    # over concrete collections O(n^2) — a 20k rest-loop took two seconds.
    # These stay ABOVE the set/map branches: rest-of-vector is every seq loop's
    # hot path and must not pay the wrapper-tag checks.
    (pvec? coll) (let [a (pv->array coll)]
                   (if (<= (length a) 1) @[]
                     (make-lazy-seq (fn [] (indexed-cells a 1)))))
    (or (nil? coll) (= 0 (length coll))) @[]
    (string? coll) (tuple ;(map make-char (string/bytes (string/slice coll 1))))
    (tuple? coll) (if (<= (length coll) 1) @[]
                    (make-lazy-seq (fn [] (indexed-cells coll 1))))
    # Sets, maps and sorted colls rest via their seq. Without these branches
    # they fell into the indexed fall-through, which walked the wrapper table's
    # INTERNAL fields — (next #{1 2}) was (nil nil) until the canonical every?
    # started seq-walking sets (seed-shrink round 4).
    (set? coll) (if (= 0 (coll :cnt)) @[] (core-rest (phs-seq coll)))
    (phm? coll) (if (= 0 (coll :cnt)) @[] (core-rest (tuple ;(phm-entries coll))))
    (core-sorted? coll) (core-rest ((sorted-op coll :seq) coll))
    # plain struct maps (untagged literals) rest via entries too
    (and (struct? coll) (nil? (get coll :jolt/type)))
      (core-rest (tuple ;(map-entries-of coll)))
    (if (<= (length coll) 1) @[]
      (make-lazy-seq (fn [] (indexed-cells coll 1))))))

(defn core-next [coll]
  # next is rest, but nil when the rest is empty. seq-done? realizes one lazy
  # cell so a lazy rest that turns out empty (length on the table won't tell us)
  # collapses to nil, matching Clojure.
  (let [r (core-rest coll)]
    (if (seq-done? r) nil r)))

(defn core-cons [x coll]
  "Prepend x onto coll. For concrete collections this is an O(1) persistent cons
  node; for lazy-seqs it stays a lazy cell so laziness is preserved."
  (cond
    # Lazy tail: return a LazySeq (NOT a bare cell), so a cons-of-a-cons stays a
    # proper lazy-seq and the rest-thunk never leaks as a plain array element.
    (lazy-seq? coll) (make-lazy-seq (fn [] @[x (fn [] coll)]))
    (or (nil? coll) (plist? coll) (array? coll) (tuple? coll)) (pl-cons x coll)
    # second arg must be seqable (a collection or string); reject scalars
    (not (or (core-coll? coll) (string? coll)))
      (error (string "Don't know how to create ISeq from: " (type coll)))
    (pl-cons x (realize-for-iteration coll))))

(defn core-seq [coll]
  (cond
    (core-sorted? coll) ((sorted-op coll :seq) coll)
    (or (nil? coll) (and (or (tuple? coll) (array? coll)) (= 0 (length coll)))) nil
    # Cell-based emptiness, NOT (nil? (ls-first)): a lazy-seq whose first element
    # is legitimately nil is non-empty, so (seq (cons nil ...)) must not be nil.
    (lazy-seq? coll) (let [cell (realize-ls coll)]
                       (if (or (nil? cell) (= :jolt/pending cell) (= 0 (length cell))) nil coll))
    (pvec? coll) (if (= 0 (pv-count coll)) nil (tuple ;(pv->array coll)))
    (plist? coll) (if (pl-empty? coll) nil (tuple ;(pl->array coll)))
    (buffer? coll) (if (= 0 (length coll)) nil (let [a @[]] (each x coll (array/push a x)) (tuple ;a)))
    # empty maps/sets seq to nil, as in Clojure ((seq {}) is nil, not ())
    (set? coll) (if (= 0 (coll :cnt)) nil (phs-seq coll))
    (phm? coll) (if (= 0 (coll :cnt)) nil (tuple ;(phm-entries coll)))
    (tuple? coll) (tuple/slice coll)
    (string? coll) (if (= 0 (length coll)) nil (tuple ;(map make-char (string/bytes coll))))
    (struct? coll) (if (= 0 (length coll)) nil (tuple ;(map (fn [k] (tuple k (get coll k))) (keys coll))))
    (array? coll) (tuple ;coll)
    (and (table? coll) (get coll :jolt/deftype)) coll
    # raw host table (System/getenv result) seqs like a map: kv entries
    (and (table? coll) (nil? (get coll :jolt/type)))
      (if (= 0 (length coll)) nil
        (tuple ;(map (fn [k] (tuple k (get coll k))) (keys coll))))
    # scalars/functions aren't seqable
    (error (string "seq not supported on " (type coll)))))

(defn core-vec [coll]
  (when (not (or (nil? coll) (core-coll? coll) (string? coll)))
    (error (string "Don't know how to create a vector from " (type coll))))
  (let [coll (realize-for-iteration coll)]
    (cond
      (array? coll) (make-vec coll)
      (tuple? coll) (make-vec coll)
      (struct? coll) (make-vec (map |(in (kvs coll) (+ (* $ 2) 1)) (range (/ (length (kvs coll)) 2))))
      (string? coll) (make-vec (map |(string/from-bytes $) (string/bytes coll)))
      (make-vec @[]))))

(defn- into-conj [to items]
  (cond
    (or (phm? to) (struct? to) (and (table? to) (get to :jolt/deftype)))
      (do (var result to)
        (each item items (set result (core-assoc result (vnth item 0) (vnth item 1))))
        result)
    (pvec? to) (do (var result to) (each x items (set result (pv-conj result x))) result)
    (array? to) (if mutable?
                  (do (each x items (array/push to x)) to)               # vector: append
                  (do (var result (array/slice to)) (each x items (array/insert result 0 x)) result))  # list: prepend
    (tuple? to) (tuple/slice (tuple ;(array/concat (array/slice to) (array/slice items))))
    # everything else conj-able (sets, sorted colls): fold conj — previously
    # this fell through to `to` unchanged, silently dropping all elements
    # ((into #{} [:a :b]) was #{}, jolt-h86)
    (do (var result to) (each x items (set result (core-conj result x))) result)))

# merge now lives in the Clojure collection tier (core/20-coll.clj).

# merge-with now lives in the Clojure collection tier (core/20-coll.clj).

# keys / vals now live in the syntax tier (core/00-syntax.clj) — canonical
# projections of (seq m), so sorted maps come back in comparator order.



# select-keys now lives in the Clojure collection tier (core/20-coll.clj).

# zipmap now lives in the Clojure collection tier (core/20-coll.clj).

# ============================================================
# Transducers
# ============================================================
# A transducer is (fn [rf] rf') where rf' is a reducing fn with arities
# []=init, [acc]=complete, [acc x]=step. map/filter/take/... return a
# transducer when called with no collection.

(defn core-reduced [x] @{:jolt/type :jolt/reduced :val x})
(defn core-reduced? [x] (and (table? x) (= :jolt/reduced (x :jolt/type))))
# unreduced lives in the syntax tier (core/00-syntax.clj) over reduced?/deref.
(defn- ensure-reduced [x] (if (core-reduced? x) x (core-reduced x)))

(defn td-map [f]
  (fn [rf] (fn [& a] (case (length a) 0 (rf) 1 (rf (a 0)) (rf (a 0) (f (a 1)))))))
(defn td-filter [pred]
  (fn [rf] (fn [& a] (case (length a) 0 (rf) 1 (rf (a 0))
                       (if (truthy? (pred (a 1))) (rf (a 0) (a 1)) (a 0))))))
(defn td-remove [pred] (td-filter (fn [x] (not (pred x)))))
# td-keep removed: keep (incl its transducer arity) lives in core/40-lazy.clj.
(defn td-take [n]
  (fn [rf]
    (var left n)
    (fn [& a] (case (length a) 0 (rf) 1 (rf (a 0))
                (if (<= left 0) (core-reduced (a 0))
                  (let [r (rf (a 0) (a 1))] (set left (dec left))
                    (if (<= left 0) (ensure-reduced r) r)))))))
(defn td-drop [n]
  (fn [rf]
    (var left n)
    (fn [& a] (case (length a) 0 (rf) 1 (rf (a 0))
                (if (> left 0) (do (set left (dec left)) (a 0)) (rf (a 0) (a 1)))))))
(defn td-take-while [pred]
  (fn [rf]
    (fn [& a] (case (length a) 0 (rf) 1 (rf (a 0))
                (if (truthy? (pred (a 1))) (rf (a 0) (a 1)) (core-reduced (a 0)))))))
(defn td-drop-while [pred]
  (fn [rf]
    (var dropping true)
    (fn [& a] (case (length a) 0 (rf) 1 (rf (a 0))
                (do (when (and dropping (not (truthy? (pred (a 1))))) (set dropping false))
                  (if dropping (a 0) (rf (a 0) (a 1))))))))
# td-map-indexed removed: map-indexed (incl transducer arity) lives in core/40-lazy.clj.

# Stateful windowing transducers. The 1-arg (completion) arity flushes a partial
# trailing window before delegating to rf's completion; matches Clojure.
# td-partition-all removed: partition-all (incl transducer arity) lives in core/40-lazy.clj.

# partition-by's transducer arity lives with its (lazy) collection arity in the
# overlay (10-seq tier), written in Clojure with volatiles.

(defn- reduce-with-reduced
  "Reduce coll with reducing fn rf and seed init, honoring `reduced`. Steps lazy
  seqs one cell at a time so a reducing fn that returns `reduced` (e.g. the
  `take`/`take-while` transducers) can short-circuit over an INFINITE seq instead
  of realizing it eagerly. Returns the final (unwrapped) accumulator."
  [rf init coll]
  (var acc init)
  (if (lazy-seq? coll)
    (do
      (var cur coll) (var go true)
      (while go
        (let [cell (realize-ls cur)]
          (if (or (nil? cell) (= :jolt/pending cell) (= 0 (length cell)))
            (set go false)
            (do
              (set acc (rf acc (in cell 0)))
              (if (core-reduced? acc)
                (do (set acc (acc :val)) (set go false))
                (let [rt (in cell 1)]
                  (if (nil? rt) (set go false) (set cur (ls-rest-cached cur rt))))))))))
    (do
      (var stop false)
      (each x (if (set? coll) (phs-seq coll) (realize-for-iteration coll))
        (when (not stop)
          (set acc (rf acc x))
          (when (core-reduced? acc) (set acc (acc :val)) (set stop true))))))
  acc)

(defn- transduce-reduce
  "Reduce coll with reducing fn rf and seed init, honoring `reduced`."
  [rf init coll]
  (reduce-with-reduced rf init coll))

(defn core-transduce
  "(transduce xform f coll) or (transduce xform f init coll)."
  [xform f & rest]
  (let [has-init (= 2 (length rest))
        init (if has-init (in rest 0) (f))
        coll (if has-init (in rest 1) (in rest 0))
        rf (xform f)]
    (rf (transduce-reduce rf init coll))))

(defn core-into
  "(into to from) or (into to xform from)."
  [to & rest]
  (if (= 2 (length rest))
    (let [xform (in rest 0) from (in rest 1)]
      (core-transduce xform (fn [& a] (case (length a) 0 to 1 (a 0) (core-conj (a 0) (a 1)))) to from))
    (into-conj to (realize-for-iteration (in rest 0)))))

(defn core-sequence
  "(sequence coll) -> a seq of coll. (sequence xform coll) -> a LAZY seq of coll
  transformed by xform: elements are pulled and pushed through the transducer one
  at a time, with outputs buffered and emitted lazily — so it works over infinite
  input (matching Clojure). Honors `reduced` (early stop) and runs the completion
  arity to flush stateful transducers (e.g. partition-all)."
  [a & rest]
  (if (= 0 (length rest))
    (core-seq a)
    (let [xform a
          coll (in rest 0)
          buf @[]
          state @{:stopped false :completed false}
          rf (fn [& args]
               (case (length args)
                 0 buf
                 1 (in args 0)
                 (do (array/push (in args 0) (in args 1)) (in args 0))))
          xf (xform rf)]
      # Pull/complete until buf holds an output or the source is fully drained.
      (defn ensure-buf [src]
        (var s src)
        (while (and (= 0 (length buf)) (not (state :stopped)) (not (seq-done? s)))
          (let [r (xf buf (core-first s))]
            (set s (core-rest s))
            (when (core-reduced? r) (put state :stopped true))))
        (when (and (= 0 (length buf)) (not (state :completed))
                   (or (state :stopped) (seq-done? s)))
          (put state :completed true)
          (xf buf))   # completion arity — flushes any buffered state
        s)
      (defn gen [src]
        (fn []
          (let [s (ensure-buf src)]
            (if (= 0 (length buf)) nil
              (let [val (in buf 0)]
                (array/remove buf 0 1)
                @[val (gen s)])))))
      # core-seq normalizes to a tuple / lazy-seq / nil — all walkable by
      # core-first/rest/seq-done?. (Walking a raw pvec/set would misfire:
      # seq-done? uses length, which counts a pvec table's KEYS, not elements.)
      (make-lazy-seq (gen (core-seq coll))))))


(defn coll->cells [c]
  "Convert a seqable to a lazy-seq cell chain: nil or [first, rest-thunk].
  A cons cell is a MUTABLE array `@[val rest-thunk]` (produced by `cons`/the lazy
  transformers); user collections (tuples, pvecs, lists) are immutable. We rely
  on that distinction: only a mutable 2-array whose tail is a function is treated
  as an already-built cell — a user vector like `[first last]` (tail is the fn
  `last`) is data and must NOT be misread as a cell. User data is recursed through
  immutable tuples so its tails never reach the cell-detection branch."
  (if (nil? c) nil
    (if (pvec? c) (coll->cells (tuple ;(pv->array c)))
    (if (plist? c) (coll->cells (tuple ;(pl->array c)))
    (if (function? c)
      (let [r (c)]
        (if (and (array? r) (= 2 (length r)) (function? (in r 1)))
          r
          (coll->cells r)))
      (if (lazy-seq? c)
        (let [cell (realize-ls c)]
          (if (= :jolt/pending cell) nil cell))
        (if (tuple? c)
          # user sequential data: every element is a value, no cell-detection.
          # indexed-cells walks by INDEX — the old (tuple/slice c 1) per cell
          # made any walk over a concrete collection O(n^2).
          (if (= 0 (length c)) nil (indexed-cells c 0))
        (if (array? c)
          # mutable array: a genuine cons cell, or an eager seq result.
          (if (= 0 (length c)) nil
            (if (and (= 2 (length c)) (function? (in c 1)))
              c  # already a cell [val, rest-thunk]
              (indexed-cells c 0)))
          # Other concrete seqables (set/map/sorted coll/string/buffer): coerce
          # to a tuple seq via core-seq, then recurse. (lazy/indexed above.)
          (if (or (set? c) (phm? c) (buffer? c) (string? c) (core-sorted? c)
                  (and (struct? c) (nil? (get c :jolt/type)))
                  # raw host table (System/getenv) — a map: kv entries
                  (and (table? c) (nil? (get c :jolt/type))
                       (nil? (get c :jolt/deftype))))
            (coll->cells (core-seq c))
            nil)))))))))

(defn lazy-from
  "Coerce any seqable to a uniform lazy view without forcing.
  Returns nil if coll is nil or empty, the LazySeq unchanged if already lazy,
  or a new LazySeq that walks element by element."
  [coll]
  (if (nil? coll) nil
    (if (lazy-seq? coll) coll
      (do
        # Reject non-seqable scalars (number/boolean/keyword, and tagged structs
        # like char/symbol) so a lazy transformer over bad input throws when
        # realized — matching Clojure — instead of silently yielding empty.
        (when (or (number? coll) (boolean? coll) (keyword? coll)
                  (and (struct? coll) (not (nil? (get coll :jolt/type)))))
          (error (string "Don't know how to create ISeq from: " (type coll))))
        (let [cell (coll->cells coll)]
          (if (nil? cell) nil
            (make-lazy-seq (fn [] cell))))))))

(defn core-map [f & colls]
  (def f (as-fn f))
  (if (= 0 (length colls))
    (td-map f)   # transducer arity
  (if (= 1 (length colls))
    (let [coll (colls 0)]
      # Option A: always lazy, even over concrete collections (matches Clojure —
      # map returns a seq, not a vector).
      (do
        (defn mstep [c]
          (fn []
            (if (seq-done? c) nil
              @[(f (core-first c)) (mstep (core-rest c))])))
        (make-lazy-seq (mstep (lazy-from coll)))))
    # Multi-collection: lazy-seq with per-element independent state
    (let [init-cs (array/new-filled (length colls) nil)
          init-idxs (array/new-filled (length colls) 0)
          init-reals (array/new-filled (length colls) nil)
          _ (do
              (var i 0)
              (while (< i (length colls))
                (let [c (in colls i)]
                  (if (lazy-seq? c)
                    (put init-cs i c)
                    (do (put init-cs i nil)
                        (put init-reals i (if (set? c) (phs-seq c) (realize-for-iteration c))))))
                (++ i))
              nil)]
      (defn step [cs idxs reals]
        "cs: current lazy-seq cursors, idxs: indices, reals: realized colls"
        (fn []
          (var args @[])
          (var next-cs (array/new-filled (length cs) nil))
          (var next-idxs (array/new-filled (length idxs) 0))
          (var next-reals (array/new-filled (length reals) nil))
          (var ok true)
          (var i 0)
          (while (< i (length cs))
            (let [cur (in cs i) ridx (in idxs i) real (in reals i)]
              (if (not (nil? cur))
                # Detect exhaustion with seq-done?, NOT (nil? (ls-first)): a
                # lazy-seq can legitimately contain nil elements, and treating the
                # first nil as end-of-seq truncates (e.g. mapping over a previous
                # map result that holds nils).
                (if (seq-done? cur) (do (set ok false) (break))
                    (do (array/push args (ls-first cur))
                        (put next-cs i (ls-rest cur))
                        (put next-idxs i (+ ridx 1))
                        (put next-reals i nil)))
                (let [c (if (nil? real)
                          (let [rc (realize-for-iteration (in colls i))]
                            (put next-reals i rc) rc)
                          real)]
                  (if (>= ridx (length c)) (do (set ok false) (break))
                    (do (array/push args (in c ridx))
                        (put next-cs i nil)
                        (put next-idxs i (+ ridx 1))
                        (put next-reals i c))))))
            (++ i))
          (if (and ok (= (length args) (length cs)))
            @[(apply f args) (step next-cs next-idxs next-reals)]
            nil)))
      (make-lazy-seq (step init-cs init-idxs init-reals))))))

(defn core-filter [pred & rest]
  (def pred (as-fn pred))
  (if (= 0 (length rest)) (td-filter pred)
   (let [coll (in rest 0)]
    # Option A: always lazy (matches Clojure — filter returns a seq).
    (do
      (defn fstep [c]
        (fn []
          (var cur c) (var hit nil) (var found false)
          (while (and (not found) (not (seq-done? cur)))
            (let [x (core-first cur)]
              (if (pred x) (do (set hit @[x (core-rest cur)]) (set found true))
                (set cur (core-rest cur)))))
          (if found @[(in hit 0) (fstep (in hit 1))] nil)))
      (make-lazy-seq (fstep (lazy-from coll)))))))

(defn core-remove [pred & rest]
  (def pred (as-fn pred))
  (if (= 0 (length rest)) (td-remove pred)
    (core-filter (fn [x] (not (pred x))) (in rest 0))))

(def core-reduce
  (fn [& args]
    (case (length args)
      # 2-arg: seed is the first element; reduce over the rest. Lazy seqs are
      # stepped incrementally (via reduce-with-reduced) so `reduced` can
      # short-circuit an infinite seq rather than realizing it.
      2 (let [f (args 0) coll (args 1)]
          (if (lazy-seq? coll)
            (let [cell (realize-ls coll)]
              (if (or (nil? cell) (= :jolt/pending cell) (= 0 (length cell)))
                (f)
                (let [rt (in cell 1)]
                  (if (nil? rt) (in cell 0)
                    (reduce-with-reduced f (in cell 0) (ls-rest-cached coll rt))))))
            (let [c (if (set? coll) (phs-seq coll) (realize-for-iteration coll))]
              (if (= 0 (length c)) (f)
                (reduce-with-reduced f (in c 0) (array/slice c 1))))))
      3 (let [f (args 0) val (args 1) coll (args 2)]
          (reduce-with-reduced f val coll))
      (error "Wrong number of args passed to: reduce"))))

(defn core-take [n & rest]
 # n is a count — reject non-numbers (e.g. a char/string) like Clojure, rather
 # than letting Janet's >= silently compare mixed types.
 (unless (number? n) (error (string "take: n must be a number, got " (type n))))
 (if (= 0 (length rest)) (td-take n)
  (let [coll (in rest 0)]
    # Option A: lazy take (returns a seq, not a vector, even over a vector).
    (defn tstep [c i]
      (fn []
        (if (or (>= i n) (seq-done? c)) nil
          @[(core-first c) (tstep (core-rest c) (+ i 1))])))
    (make-lazy-seq (tstep (lazy-from coll) 0)))))

(defn core-drop [n & rest]
 (if (= 0 (length rest)) (td-drop n)
  (let [coll (in rest 0)]
    # Option A: lazy drop — skip n (forcing only those), return the lazy tail.
    (make-lazy-seq
      (fn []
        (var cur (lazy-from coll))
        (var i 0)
        (while (and (< i n) (not (seq-done? cur)))
          (set cur (core-rest cur))
          (++ i))
        (coll->cells cur))))))

# ffirst/nfirst/fnext/nnext/last/butlast (seq tier) and second/peek/subvec/mapv/
# update (kernel tier) now live in the Clojure clojure.core tiers under
# jolt-core/clojure/core/. The kernel tier is bootstrap-compiled before the
# self-hosted analyzer is built, so the structural fns the analyzer uses come
# from Clojure, not Janet — see api/load-core-overlay! and core/00-kernel.clj.

(defn core-take-while [pred & rest]
 (def pred (as-fn pred))
 (if (= 0 (length rest)) (td-take-while pred)
  (let [coll (in rest 0)]
    # Option A: lazy take-while.
    (defn twstep [c]
      (fn []
        (if (seq-done? c) nil
          (let [x (core-first c)]
            (if (pred x) @[x (twstep (core-rest c))] nil)))))
    (make-lazy-seq (twstep (lazy-from coll))))))

(defn core-drop-while [pred & rest]
 (def pred (as-fn pred))
 (if (= 0 (length rest)) (td-drop-while pred)
  (let [coll (in rest 0)]
   (if (lazy-seq? coll)
     (do
       (defn dwstep [c]
         (fn []
           (var cur c)
           (while (and (not (seq-done? cur)) (pred (ls-first cur)))
             (set cur (ls-rest cur)))
           (if (seq-done? cur) nil (realize-ls cur))))
       (make-lazy-seq (dwstep coll)))
     (let [c (realize-for-iteration coll)]
       (var start 0)
       (while (and (< start (length c)) (pred (c start)))
         (++ start))
       (if (tuple? c)
         (tuple/slice c start)
         (array/slice c start)))))))

(defn core-concat [& colls]
  "Truly lazy concatenation. `step` returns a 0-arg thunk that is only forced
  when the consumer asks for the next cell, so nothing in `colls` is realized at
  construction time. This is essential for self-referential lazy seqs (e.g.
  (def fib (lazy-cat [0 1] (map + (rest fib) fib)))): the later colls must not be
  forced until after the surrounding `def` has bound the var."
  (if (= 0 (length colls)) @[]
    (let [colls (if (tuple? colls) (array/slice colls) colls)]
      (defn step [cs]
        (fn []
          (if (= 0 (length cs))
            nil
            (let [c (in cs 0)
                  remaining (array/slice cs 1)
                  cell (coll->cells c)]
              (if (nil? cell)
                # current coll is empty: advance to the next one
                ((step remaining))
                (let [val (in cell 0)
                      rest-fn (in cell 1)]
                  @[val (step (if (nil? rest-fn)
                                remaining
                                (array/insert remaining 0 rest-fn)))]))))))
      (make-lazy-seq (step colls)))))


(defn core-mapcat
  "(mapcat f & colls) — map then concat. (mapcat f) returns a transducer."
  [f & colls]
  (if (= 0 (length colls))
    # transducer: map f over each input, then splice (cat) the result
    (fn [rf]
      (fn [& a]
        (case (length a)
          0 (rf)
          1 (rf (a 0))
          (do (var acc (a 0))
              (each x (realize-for-iteration (f (a 1)))
                (set acc (rf acc x)))
              acc))))
    # collection arity: direct lazy implementation. Pull one element
    # from each input coll, apply f, then yield elements from f's result.
    # No apply-forcing — walk input colls lazily element-by-element.
    (do
      (var n (length colls))
      (var init-cs @[])
      (var i 0)
      (while (< i n)
        (array/push init-cs (lazy-from (in colls i)))
        (++ i))
      (defn step [cs res]
        (fn []
          (var cursors cs) (var cur-res res) (var hit nil) (var ok false)
          (while (not ok)
            (if (nil? cur-res)
              (do
                (var args @[]) (var next-cs @[]) (var exhausted false) (var j 0)
                (while (and (< j n) (not exhausted))
                  (let [c (in cursors j)]
                    (if (seq-done? c) (set exhausted true)
                      (do
                        (array/push args (ls-first c))
                        (array/push next-cs (ls-rest c)))))
                  (++ j))
                (if exhausted (break))
                (let [r (apply f args)]
                  (set cursors next-cs)
                  (set cur-res (if (or (nil? r) (tuple? r) (array? r)
                                       (lazy-seq? r) (pvec? r) (set? r) (plist? r))
                                 (lazy-from r)
                                 (lazy-from (tuple r))))))
              (if (seq-done? cur-res)
                (set cur-res nil)
                (let [val (ls-first cur-res) rest (ls-rest cur-res)]
                  (set hit @[val (step cursors rest)])
                  (set ok true)))))
          (if ok hit nil)))
      (make-lazy-seq (step init-cs nil)))))

# reverse now lives in the Clojure collection tier ((reduce conj () coll)).

(defn core-nth
  "Return the nth element of a sequential collection. With a not-found arg, return
  it when idx is out of bounds (even if it's nil); without one, throw — matching
  Clojure, where (nth coll i nil) returns nil rather than throwing."
  [coll idx & rest]
  (def has-default (> (length rest) 0))
  (def default (if has-default (in rest 0) nil))
  (defn oob [n] (if has-default default (error (string "Index " idx " out of bounds, length: " n))))
  (if (nil? coll) default      # (nth nil i) -> nil / default, never throws
  (if (core-transient? coll)
    (let [a (coll :arr)] (if (and (>= idx 0) (< idx (length a))) (in a idx) (oob (length a))))
  (if (plist? coll)
    (let [a (pl->array coll)]
      (if (and (>= idx 0) (< idx (length a))) (in a idx) (oob (length a))))
  (if (pvec? coll)
    (if (and (>= idx 0) (< idx (pv-count coll)))
      (pv-nth coll idx)
      (oob (pv-count coll)))
  (if (lazy-seq? coll)
    # Walk with seq-done?, NOT (ls-first cur): a lazy element may legitimately be
    # false or nil, which truthiness would mistake for end-of-seq.
    (if (< idx 0) (oob 0)
      (do
        (var cur coll)
        (var i 0)
        (while (and (< i idx) (not (seq-done? cur)))
          (set cur (core-rest cur))
          (++ i))
        (if (seq-done? cur) (oob i) (core-first cur))))
    (do
      (var c (realize-for-iteration coll))
      (if (and (>= idx 0) (< idx (length c)))
        (if (string? c) (make-char (in c idx)) (in c idx))
        (oob (length c))))))))))

(defn core-sort
  "(sort coll) or (sort comparator coll). Comparator may return a boolean or a
  Clojure-style negative/zero/positive number."
  [a & rest]
  (let [has-cmp (> (length rest) 0)
        cmp (if has-cmp a nil)
        coll (if has-cmp (first rest) a)]
    (if (nil? coll) @[]
      (let [arr (array/slice (realize-for-iteration coll))]
        (if has-cmp
          (sort arr (fn [x y] (let [r (cmp x y)] (if (number? r) (< r 0) (truthy? r)))))
          (sort arr))
        (tuple/slice (tuple ;arr))))))

# (sort-by keyfn coll) or (sort-by keyfn comparator coll). The comparator (when
# given) compares the KEYS and may return a boolean or a Clojure-style number.
# sort-by now lives in the Clojure collection tier — canonical: compare-
# defaulted (nil sorts first), comparator over KEYS, via the host sort seam.

# distinct now lives in the Clojure lazy tier (core/40-lazy.clj).
# group-by / frequencies now live in the Clojure collection tier
# (core/20-coll.clj).

(defn core-partition
  "(partition n coll), (partition n step coll), or (partition n step pad coll).
  Only complete partitions of size n are kept; with pad, the final partial
  partition is padded from pad (possibly to fewer than n if pad runs out)."
  [n & rest]
  (let [argc (length rest)
        step (if (>= argc 2) (first rest) n)
        pad  (if (>= argc 3) (in rest 1) nil)
        has-pad (>= argc 3)
        coll (case argc 1 (first rest) 2 (in rest 1) 3 (in rest 2))]
    # Option A: always lazy.
    (defn pstep [c]
      (fn []
        (if (seq-done? c) nil
          (do
            (var part @[]) (var cur c) (var i 0)
            (while (and (< i n) (not (seq-done? cur)))
              (array/push part (core-first cur))
              (set cur (core-rest cur))
              (++ i))
            (cond
              (= i n)
              (let [next-cur (if (= step n) cur (lazy-from (core-drop (- step n) cur)))]
                @[(tuple/slice (tuple ;part)) (pstep next-cur)])
              # partial final partition: pad it (last partition, then stop)
              (and has-pad (> i 0))
              (do
                (each x (realize-for-iteration pad)
                  (when (< (length part) n) (array/push part x)))
                @[(tuple/slice (tuple ;part)) (fn [] nil)])
              nil)))))
    (make-lazy-seq (pstep (lazy-from coll)))))

# partition-by now lives in the Clojure seq tier (core/10-seq.clj).

# partition-all now lives in the Clojure lazy tier (core/40-lazy.clj).


# keep-indexed / map-indexed / cycle now live in the Clojure lazy tier
# (core/40-lazy.clj).

# reduce-kv now lives in the Clojure collection tier (core/20-coll.clj).

# pop is defined only on stacks (vectors -> last end, lists -> front); Clojure
# throws on sets/maps/seqs/strings/scalars. (peek lives in the Clojure kernel
# tier — core/00-kernel.clj.)
# subvec lives in the Clojure kernel tier — core/00-kernel.clj.

# trampoline now lives in the Clojure collection tier (core/20-coll.clj).

(def core-format (fn [fmt & args] (string/format fmt ;args)))

# ============================================================
# Sequence generators
# ============================================================

(def core-range
  (fn [& args]
    (if (= 0 (length args))
      # (range) — infinite lazy sequence 0, 1, 2, ...
      (do
        (defn rstep [i] (fn [] @[i (rstep (+ i 1))]))
        (make-lazy-seq (rstep 0)))
      (let [start (if (> (length args) 1) (args 0) 0)
            end (if (> (length args) 1) (args 1) (args 0))
            step (if (> (length args) 2) (args 2) 1)]
        (var result @[])
        (var i start)
        (while (if (pos? step) (< i end) (> i end))
          (array/push result i)
          (+= i step))
        (tuple/slice (tuple ;result))))))

# repeat / iterate now live in the Clojure lazy tier (core/40-lazy.clj).

# repeatedly now lives in the Clojure lazy tier (core/40-lazy.clj).

# ============================================================
# Higher-order functions
# ============================================================

# identity / constantly live in the Clojure collection tier (core/20-coll.clj).

# complement now lives in the Clojure collection tier (core/20-coll.clj).

# inst?/inst-ms live in the Clojure collection tier (core/20-coll.clj).
# Jolt has no uri host type, so uri? is always false.
# uri? lives in the Clojure collection tier (no uri host type: always false).
# uuid? now lives in the Clojure collection tier (tagged-value predicate).
(defn core-bytes? [x] (buffer? x))
# tagged-literal? now lives in the Clojure collection tier (tagged-value predicate).

(defn core-meta [x]
  "Returns the metadata of x, or nil."
  (cond
    (var? x) (var-meta x)
    # symbols carry reader metadata (type hints etc.) in a :meta field
    (and (struct? x) (= :symbol (get x :jolt/type))) (get x :meta)
    (table? x) (or (get x :jolt/meta) (get x :meta))
    nil))

# every-pred now lives in the Clojure collection tier (core/20-coll.clj).

# Public comp lives in the overlay now (20-coll) — its stages can be any jolt
# IFn (keyword/map/set/vector), which raw Janet calls mishandle ((comp seq
# :content) returned nil: janet keyword-apply is not jolt invoke). This
# private composer remains ONLY for the transducer machinery below, where the
# stages are always real fns.
# (td-comp is gone: eduction — its last caller — lives in the overlay now.)

# partial now lives in the Clojure collection tier (canonical arities).

# juxt now lives in the Clojure collection tier (core/20-coll.clj).

# memoize now lives in the Clojure collection tier — find-based, so it
# caches nil results too (this kernel fn re-computed them).

# ============================================================
# Collection constructors
# ============================================================

(defn core-vector [& xs] (make-vec xs))
(defn core-hash-map [& kvs] (make-phm kvs))

(defn core-array-map [& kvs]
  (var result @{})
  (var i 0)
  (while (< i (length kvs))
    (put result (kvs i) (kvs (+ i 1)))
    (+= i 2))
  (table/to-struct result))

(defn core-hash-set [& xs]
  (apply make-phs xs))

# sorted sets are tagged tables the host set? predicate misses (jolt-dpn)
(defn core-set? [x] (or (set? x) (core-sorted-set? x)))
(defn core-disj [s & ks]
  (cond
    (core-sorted-set? s) ((sorted-op s :disj) s ks)
    (set? s) (apply phs-disj s ks)
    (error "disj expects a set")))

(defn core-set [coll]
  (apply core-hash-set (realize-for-iteration coll)))

(defn core-list [& xs]
  (array ;xs))

# ============================================================
# String functions
# ============================================================

# Readable rendering of a value (Clojure pr semantics): strings quoted,
# keywords with leading ':', symbols by name, collections with their reader
# syntax. Used by both pr-str (readable) and str (collection elements).
# A namespace's :name may be a string or a symbol struct depending on the
# creation path — normalize for display.
(defn- ns-display-name [ns]
  (def n (ns :name))
  (if (and (struct? n) (= :symbol (get n :jolt/type))) (n :name) (string n)))

# print-method callback (jolt-g1r): set by api/init AFTER the overlay loads,
# to a (fn [v emit] handled?) that looks for a USER-registered print-method
# multimethod entry for v's dispatch value and renders through it (emit takes
# string pieces). The renderer consults it only on the record/tagged
# fallthrough, so built-in rendering pays nothing.
(var print-method-cb nil)
(defn set-print-method-cb! [f] (set print-method-cb f))

(def- pr-char-escapes
  {34 "\\\"" 92 "\\\\" 10 "\\n" 9 "\\t" 13 "\\r" 12 "\\f" 8 "\\b"})
(var pr-render nil)

# Format a number the way Clojure prints it: infinity and NaN have named forms
# (Janet renders them "inf"/"-inf"/"nan").
(defn- fmt-number [v]
  (cond
    (not (number? v)) (string v)
    (= v math/inf) "Infinity"
    (= v (- math/inf)) "-Infinity"
    (not= v v) "NaN"
    (string v)))

(defn- pr-render-seq [buf items open close]
  (buffer/push-string buf open)
  (var first true)
  (each x items
    (if first (set first false) (buffer/push-string buf " "))
    (pr-render buf x))
  (buffer/push-string buf close))

(defn- pr-render-pairs [buf pairs]
  (buffer/push-string buf "{")
  (var first true)
  (each pair pairs
    (if first (set first false) (buffer/push-string buf ", "))
    (pr-render buf (in pair 0))
    (buffer/push-string buf " ")
    (pr-render buf (in pair 1)))
  (buffer/push-string buf "}"))

(defn- name-of
  "Extract a plain name string from a string, symbol struct, or a namespace/var
  table (reading its :name) — never recurses into the cyclic ns structure."
  [x]
  (cond
    (nil? x) nil
    (string? x) x
    (and (struct? x) (= :symbol (get x :jolt/type))) (x :name)
    (or (struct? x) (table? x)) (name-of (get x :name))
    (string x)))

(defn- var-display
  "Render a Jolt var as #'ns/name. A var's :meta/:ns refs are cyclic, so this
  reads only its :name and :ns name — printing the var's pairs would loop."
  [v]
  (let [nm (name-of (v :name))
        ns (name-of (v :ns))]
    (if ns (string "#'" ns "/" nm) (string "#'" nm))))

(defn- pr-push-escaped
  "Readable string body: escape per char-escapes (quote, backslash, \\n & co),
  so pr-str round-trips through the reader (this was unescaped, jolt pre-r6)."
  [buf s]
  (each c (string/bytes s)
    (if-let [esc (get pr-char-escapes c)]
      (buffer/push-string buf esc)
      (buffer/push-byte buf c))))

(set pr-render
  (fn [buf v]
    (cond
      (nil? v) (buffer/push-string buf "nil")
      (= true v) (buffer/push-string buf "true")
      (= false v) (buffer/push-string buf "false")
      (string? v) (do (buffer/push-string buf "\"") (pr-push-escaped buf v) (buffer/push-string buf "\""))
      (buffer? v) (do (buffer/push-string buf "\"") (pr-push-escaped buf (string v)) (buffer/push-string buf "\""))
      (keyword? v) (do (buffer/push-string buf ":") (buffer/push-string buf (string v)))
      (core-char? v) (do (buffer/push-string buf "\\")
                         (buffer/push-string buf
                           (case (v :ch)
                             10 "newline" 32 "space" 9 "tab" 13 "return"
                             12 "formfeed" 8 "backspace" 0 "nul"
                             (char->string v))))
      (number? v) (buffer/push-string buf (fmt-number v))
      (and (struct? v) (= :symbol (v :jolt/type)))
        (buffer/push-string buf (if (v :ns) (string (v :ns) "/" (v :name)) (v :name)))
      (and (struct? v) (= :jolt/inst (v :jolt/type)))
        (do (buffer/push-string buf "#inst \"") (buffer/push-string buf (inst->rfc3339 v))
            (buffer/push-string buf "\""))
      (= :jolt/namespace (get v :jolt/type))
        (do (buffer/push-string buf "#namespace[")
            (buffer/push-string buf (ns-display-name v))
            (buffer/push-string buf "]"))
      (and (table? v) (= :jolt/var (get v :jolt/type))) (buffer/push-string buf (var-display v))
      (core-sorted-map? v) (pr-render-pairs buf
                             (map (fn [e] [(vnth e 0) (vnth e 1)]) (sorted-entries-arr v)))
      (core-sorted-set? v) (pr-render-seq buf (sorted-entries-arr v) "#{" "}")
      (lazy-seq? v) (pr-render-seq buf (realize-for-iteration v) "(" ")")
      (set? v) (pr-render-seq buf (phs-seq v) "#{" "}")
      (phm? v) (pr-render-pairs buf (phm-entries v))
      (pvec? v) (pr-render-seq buf (pv->array v) "[" "]")
      (plist? v) (pr-render-seq buf (pl->array v) "(" ")")
      (and (table? v) (get v :jolt/deftype))
        (if (and print-method-cb (print-method-cb v (fn [piece] (buffer/push-string buf piece))))
          nil
          # Clojure's record syntax: #ns.Type{:k v, ...} (fields only, the
          # deftype tag elided). This used to print the raw janet table.
          (do
            (buffer/push-string buf (string "#" (get v :jolt/deftype)))
            (pr-render-pairs buf
              (filter (fn [pair] (not= :jolt/deftype (in pair 0))) (pairs v)))))
      (tuple? v) (pr-render-seq buf v "[" "]")
      # mutable mode: arrays are vectors -> print with [] (else lists -> ())
      (array? v) (if mutable? (pr-render-seq buf v "[" "]") (pr-render-seq buf v "(" ")"))
      # Any remaining TAGGED value dispatches through print-method when the
      # hook is wired: the io tier owns the cold renderings (uuid, regex,
      # transient, channel — branches that used to live here), and user
      # defmethods on any :jolt/* tag fire from inside nested values. Before
      # the overlay loads (init-time error messages) these fall through to
      # the raw pairs view below.
      (and print-method-cb (get v :jolt/type)
           (print-method-cb v (fn [piece] (buffer/push-string buf piece))))
        nil
      (struct? v) (pr-render-pairs buf (pairs v))
      (table? v) (pr-render-pairs buf (pairs v))
      true (buffer/push-string buf (string v)))))

(defn- str-render-one
  "Render one value with Clojure's `str`/.toString semantics (bare strings,
  nil -> empty, keywords/symbols by name, collections via pr-render)."
  [v]
  (cond
    (nil? v) ""
    (string? v) v
    (buffer? v) (string v)
    (core-char? v) (char->string v)
    (keyword? v) (string ":" (string v))
    (and (struct? v) (= :symbol (v :jolt/type)))
      (if (v :ns) (string (v :ns) "/" (v :name)) (v :name))
    (and (struct? v) (= :jolt/uuid (v :jolt/type))) (v :str)
    (and (struct? v) (= :jolt/inst (v :jolt/type))) (inst->rfc3339 v)
    (= :jolt/namespace (get v :jolt/type)) (ns-display-name v)
    (and (table? v) (= :jolt/var (get v :jolt/type))) (var-display v)
    (number? v) (fmt-number v)
    (= true v) "true"
    (= false v) "false"
    (let [buf @""] (pr-render buf v) (string buf))))

(defn core-str [& xs]
  (if (= 0 (length xs)) ""
    (do
      (var result @[])
      (each x xs (array/push result (str-render-one x)))
      (string/join result ""))))

(defn core-str-join
  "clojure.string/join: stringify each element (Clojure semantics), then join."
  [coll &opt sep]
  (default sep "")
  (let [items (realize-for-iteration coll)
        parts @[]]
    (each x items (array/push parts (str-render-one x)))
    (string/join parts (str-render-one sep))))

(defn core-name
  "Returns the name string of a keyword, symbol, or string (without namespace)."
  [x]
  (cond
    (keyword? x) (let [s (string x) i (string/find "/" s)] (if i (string/slice s (+ i 1)) s))
    (and (struct? x) (= :symbol (x :jolt/type))) (x :name)
    (string? x) x
    ""))

(defn core-namespace
  "Returns the namespace string of a keyword/symbol, or nil if none."
  [x]
  (cond
    (keyword? x) (let [s (string x) i (string/find "/" s)] (if i (string/slice s 0 i) nil))
    (and (struct? x) (= :symbol (x :jolt/type)))
      (if (x :ns) (if (struct? (x :ns)) ((x :ns) :name) (string (x :ns))) nil)
    nil))

(def core-subs
  (fn [& args]
    (when (not (or (= 2 (length args)) (= 3 (length args))))
      (error "Wrong number of args passed to: subs"))
    (let [s (args 0)
          start (get args 1)]
      (when (not (string? s)) (error (string "subs requires a string, got " (type s))))
      (let [len (length s)
            end (if (= 3 (length args)) (args 2) len)]
        # Clojure validates bounds (no negative/from-end/clamping like Janet):
        # 0 <= start <= end <= (count s).
        (when (not (and (number? start) (number? end)
                        (= start (math/floor start)) (= end (math/floor end))
                        (>= start 0) (<= start end) (<= end len)))
          (error "String index out of range"))
        (string/slice s start end)))))

# ============================================================
# I/O — minimal wrappers
# ============================================================

# print/println use str semantics (bare strings); pr/prn use readable (quoted).
# All space-separate their args, like Clojure.
# print/println live in the Clojure collection tier (core/20-coll.clj) over
# the __write / __pr-str1 host seams; str-render-one stays for core-str.
(defn core-write [s] (prin s) nil)

# newline lives in the Clojure collection tier (core/20-coll.clj).

# Clojure 1.11 string->scalar parsers: nil on malformed input, throw on a
# non-string. Validation is strict (scan-number alone accepts 0x10 etc.).
(defn- parse-arg-str [s who]
  (if (or (string? s) (buffer? s)) (string s)
    (error (string who " requires a string, got " (type s)))))

(defn core-parse-long [s]
  (def str* (parse-arg-str s "parse-long"))
  (def n (length str*))
  (def start (if (and (> n 0) (or (= 43 (in str* 0)) (= 45 (in str* 0)))) 1 0))
  (if (and (> n start)
           (do (var ok true)
               (for i start n (when (or (< (in str* i) 48) (> (in str* i) 57)) (set ok false)))
               ok))
    (scan-number str*)
    nil))

(defn core-parse-double [s]
  (def str* (parse-arg-str s "parse-double"))
  # strict float shape: [+-] digits [. digits] [eE [+-] digits] — at least one
  # digit overall; "Infinity"/"-Infinity"/"NaN" accepted like the reference.
  (cond
    (= str* "Infinity") math/inf
    (= str* "-Infinity") (- math/inf)
    (= str* "NaN") math/nan
    (do
      (def pat (peg/compile ~(sequence (opt (set "+-")) (choice (sequence (some :d) (opt (sequence "." (any :d)))) (sequence "." (some :d))) (opt (sequence (set "eE") (opt (set "+-")) (some :d))) -1)))
      (if (peg/match pat str*) (scan-number str*) nil))))

# parse-boolean lives in the Clojure collection tier (core/20-coll.clj).

# Host time source for the `time` macro (monotonic, milliseconds).
(defn core-current-time-ms [] (* 1000 (os/clock :monotonic)))

# Host IO (host-classified in the spec): path-based slurp/spit, *out* flush.
(defn core-slurp [path] (string (slurp path)))

(defn core-spit [path content & opts]
  (def append? (do (var a false) (var i 0)
                   (while (< i (length opts))
                     (when (and (= :append (in opts i)) (in opts (+ i 1))) (set a true))
                     (+= i 2))
                   a))
  (def f (file/open path (if append? :a :w)))
  (file/write f (str-render-one content))
  (file/close f)
  nil)

(defn core-flush []
  (def out (dyn :out))
  (when out (file/flush out))
  nil)

# Thread-binding introspection over the frame stack (types/cur-binding-stack).
(defn core-get-thread-bindings []
  # Innermost frame wins: merge frames oldest-first. The result is a Janet
  # STRUCT keyed by the var tables themselves — the exact frame representation
  # var-get reads (identity-keyed get) — so the map can be re-pushed by
  # with-bindings*/bound-fn* and remains lookup-able with (get m the-var).
  (def acc @{})
  (each frame (snapshot-bindings)
    (each entry (realize-for-iteration frame)
      (put acc (in entry 0) (in entry 1))))
  (table/to-struct acc))

(defn core-thread-bound?* [v]
  (var found false)
  (each frame (snapshot-bindings)
    (each entry (realize-for-iteration frame)
      (when (= (in entry 0) v) (set found true))))
  found)

# Directory primitives for file-seq (paths, not File objects — host-classified).
(defn core-dir? [path]
  (def st (os/stat path))
  (and st (= :directory (st :mode))))

(defn core-list-dir [path]
  (def entries (os/dir path))
  (map (fn [e] (string path "/" e)) (sort entries)))

# Clojure compare: a total order over comparable values. nil sorts first;
# numbers numerically; strings/keywords lexically; symbols by ns then name;
# booleans false<true; chars by codepoint; vectors by length then elementwise;
# uuids by canonical string; insts by epoch ms. Cross-type comparison throws
# (like Clojure's ClassCastException).
(var core-compare nil)
(set core-compare (fn ccompare [a b]
  (defn cmp3 [x y] (cond (< x y) -1 (> x y) 1 0))
  (cond
    (and (nil? a) (nil? b)) 0
    (nil? a) -1
    (nil? b) 1
    (and (number? a) (number? b)) (cmp3 a b)
    (and (or (string? a) (buffer? a)) (or (string? b) (buffer? b)))
      (cmp3 (string a) (string b))
    (and (keyword? a) (keyword? b)) (cmp3 (string a) (string b))
    (and (core-symbol? a) (core-symbol? b))
      (let [r (cmp3 (string (or (a :ns) "")) (string (or (b :ns) "")))]
        (if (= 0 r) (cmp3 (a :name) (b :name)) r))
    (and (boolean? a) (boolean? b))
      (cond (= a b) 0 (= a false) -1 1)
    (and (core-char? a) (core-char? b)) (cmp3 (a :ch) (b :ch))
    (and (struct? a) (= :jolt/uuid (get a :jolt/type))
         (struct? b) (= :jolt/uuid (get b :jolt/type)))
      (cmp3 (a :str) (b :str))
    (and (struct? a) (= :jolt/inst (get a :jolt/type))
         (struct? b) (= :jolt/inst (get b :jolt/type)))
      (cmp3 (a :ms) (b :ms))
    (and (jvec? a) (jvec? b))
      (let [la (vcount a) lb (vcount b)]
        (if (not= la lb)
          (cmp3 la lb)
          (do
            (var r 0) (var i 0)
            (while (and (= r 0) (< i la))
              (set r (ccompare (vnth a i) (vnth b i)))
              (++ i))
            r)))
    (error (string "Cannot compare " (type a) " with " (type b))))))

# Clojure type: the :type metadata when present, else the value's type. With no
# class objects on this host, the "class" is a symbol: a deftype/record value
# yields its type tag symbol; everything else a taxonomy keyword
# (host-classified — see spec coverage).
(defn core-type [x]
  (def m (core-meta x))
  (def override (and m (core-get m :type)))
  (if (not (nil? override))
    override
    (cond
      (and (table? x) (get x :jolt/deftype))
        {:jolt/type :symbol :ns nil :name (get x :jolt/deftype)}
      (nil? x) nil
      (boolean? x) :boolean
      (number? x) :number
      (or (string? x) (buffer? x)) :string
      (keyword? x) :keyword
      (core-symbol? x) :symbol
      (core-char? x) :char
      (and (struct? x) (get x :jolt/type)) (get x :jolt/type)
      (jvec? x) :vector
      (core-map? x) :map
      (set? x) :set
      (core-seq? x) :seq
      (or (function? x) (cfunction? x)) :fn
      (table? x) (or (get x :jolt/type) :table)
      :else (keyword (type x)))))

# Capture *out*: run thunk with Janet's :out dynamic bound to a buffer, so all
# print/println/pr/prn output (which go through `prin` -> (dyn :out)) is collected
# and returned as a string. The with-out-str macro (overlay) wraps a body thunk.
(defn core-with-out-str [thunk]
  (def buf @"")
  (with-dyns [:out buf] (thunk))
  (string buf))

# pr/prn/pr-str live in the Clojure collection tier (core/20-coll.clj); the
# renderer itself stays host (representation-coupled, shared with hot str).
(defn core-pr-str1 [x] (let [b @""] (pr-render b x) (string b)))

# ============================================================
# Java-style arrays — backed by Janet's C primitives. Byte arrays use Janet
# buffers (contiguous, O(1) indexed get/put — genuinely fast); object and
# numeric arrays use Janet arrays. aget/aset/alength/aclone work over both.
# ============================================================

# alength / aget / aset now live in the Clojure collection tier — count/nth reads
# and an aset write through jolt.host/ref-put!. The typed/object array constructors
# below stay native (they build the mutable backing).

(defn core-aclone [arr]
  (cond
    (buffer? arr) (buffer/slice arr)
    (pvec? arr) (array ;(pv->array arr))
    (array/slice arr)))

# Numeric / object arrays: (T-array size) | (T-array size init) | (T-array seq)
(defn- make-num-array [a rest init]
  (if (number? a)
    (array/new-filled a (if (> (length rest) 0) (in rest 0) init))
    (array ;(realize-for-iteration a))))
(defn core-object-array [a & rest] (make-num-array a rest nil))
(defn core-int-array [a & rest] (make-num-array a rest 0))
(defn core-long-array [a & rest] (make-num-array a rest 0))
(defn core-short-array [a & rest] (make-num-array a rest 0))
(defn core-double-array [a & rest] (make-num-array a rest 0))
(defn core-float-array [a & rest] (make-num-array a rest 0))
(defn core-char-array [a & rest]
  # JVM char-array also accepts a STRING/char-seq (char[] of its characters) —
  # selmer's parse-str does (char-array template).
  (cond
    (string? a) (map make-char (string/bytes a))
    (buffer? a) (map make-char (string/bytes (string a)))
    (make-num-array a rest (make-char 0))))
(defn core-boolean-array [a & rest] (make-num-array a rest false))

# Byte arrays — Janet buffers (each element a 0..255 byte).
(defn core-byte-array [a & rest]
  (if (number? a)
    (buffer/new-filled a (band (if (> (length rest) 0) (in rest 0) 0) 0xff))
    (let [b (buffer/new 0)]
      (each x (realize-for-iteration a) (buffer/push-byte b (band x 0xff)))
      b)))

(defn core-aset-byte [arr i v] (put arr i (band v 0xff)) v)
(defn core-aset-int [arr i v] (put arr i v) v)
(defn core-aset-long [arr i v] (put arr i v) v)
(defn core-aset-short [arr i v] (put arr i v) v)
(defn core-aset-double [arr i v] (put arr i v) v)
(defn core-aset-float [arr i v] (put arr i v) v)
(defn core-aset-char [arr i v] (put arr i v) v)
(defn core-aset-boolean [arr i v] (put arr i v) v)

(defn core-make-array [a & rest]
  # (make-array len) or (make-array type len ...); ignore the type tag
  (let [len (if (number? a) a (in rest 0))] (array/new-filled len nil)))

(defn core-into-array [a & rest]
  (let [s (if (> (length rest) 0) (in rest 0) a)]
    (array ;(realize-for-iteration s))))

(defn core-to-array [coll]
  (def arr @[]) (each x (realize-for-iteration coll) (array/push arr x)) arr)
# to-array-2d lives in the Clojure collection tier (core/20-coll.clj).

# Array-element casts — identity on arrays; `bytes` coerces to a byte buffer.
(defn core-bytes [x] (if (buffer? x) x (core-byte-array x)))
(defn core-booleans [x] x)
(defn core-ints [x] x)
(defn core-longs [x] x)
(defn core-shorts [x] x)
(defn core-doubles [x] x)
(defn core-floats [x] x)
(defn core-chars [x] x)

# Scalar numeric coercions
(defn core-byte [x] (let [b (band (math/trunc x) 0xff)] (if (>= b 128) (- b 256) b)))
(defn core-short [x] (let [s (band (math/trunc x) 0xffff)] (if (>= s 0x8000) (- s 0x10000) s)))
# The masking unchecked-byte/short/char and float/double coercions live in
# the Clojure collection tier (core/20-coll.clj).

# 64-bit integers (Janet int/s64 — C-backed)
(defn core-bigint [x] (int/s64 x))
(defn core-biginteger [x] (int/s64 x))
# bigdec now lives in the Clojure collection tier (no BigDecimal: a double).

# Chunked seqs — Jolt does not chunk, so these are simple eager equivalents.
(defn core-chunk-buffer [capacity] @[])
(defn core-chunk-append [b x] (array/push b x) b)
(defn core-chunk [b] b)
# chunked-seq? now lives in the Clojure collection tier (always false on Jolt).
(defn core-chunk-first [s] (core-first s))
(defn core-chunk-rest [s] (core-rest s))
(defn core-chunk-next [s] (core-next s))
(defn core-chunk-cons [chunk rest] (core-concat (realize-for-iteration chunk) rest))

# More clojure.core: real implementations backed by existing Jolt machinery.
(defn core-boolean [x] (if x true false))
(defn core-cat [rf]
  (fn [& a]
    (case (length a)
      0 (rf) 1 (rf (a 0))
      (do (var acc (a 0)) (each x (realize-for-iteration (a 1)) (set acc (rf acc x))) acc))))
(defn core-reader-conditional [form splicing?]
  @{:jolt/type :jolt/reader-conditional :form form :splicing? splicing?})
# reader-conditional? now lives in the Clojure collection tier (tagged-value predicate).
# sorted-map-by / sorted-set-by (and all other sorted-coll constructors and
# semantics) now live in the Clojure sorted tier (core/25-sorted.clj).
# array-seq / seque live in the Clojure collection tier (core/20-coll.clj).
# supers now lives in the Clojure collection tier (no class hierarchy: #{}).
(defn core-class [x]
  (cond
    (nil? x) nil (number? x) "java.lang.Number" (string? x) "java.lang.String"
    (boolean? x) "java.lang.Boolean" (keyword? x) "clojure.lang.Keyword"
    (function? x) "clojure.lang.IFn" (buffer? x) "[B"
    (string (type x))))
# clojure-version / munge / test now live in the Clojure collection tier
# (core/20-coll.clj).


# ============================================================
# Bit operations (needed for persistent data structures)  
# ============================================================

(def core-bit-and (fn [a b] (band a b)))
(def core-bit-or (fn [a b] (bor a b)))
(def core-bit-xor (fn [a b] (bxor a b)))
(def core-bit-not (fn [a] (bnot a)))
(def core-bit-shift-left (fn [x n] (blshift x n)))
(def core-bit-shift-right (fn [x n] (brshift x n)))
(def core-bit-clear (fn [x n] (band x (bnot (blshift 1 n)))))
(def core-bit-set (fn [x n] (bor x (blshift 1 n))))
(def core-bit-flip (fn [x n] (bxor x (blshift 1 n))))
(def core-bit-test (fn [x n] (not= 0 (band x (blshift 1 n)))))
(def core-bit-and-not (fn [a b] (band a (bnot b))))
(def core-unsigned-bit-shift-right (fn [x n] (brushift x n)))

# ============================================================
# Integer coercion
# ============================================================

(def core-int (fn [x] (if (core-char? x) (x :ch) (math/trunc x))))
(def core-long (fn [x] (if (core-char? x) (x :ch) (math/trunc x))))
(def core-double (fn [x] (* 1.0 (if (core-char? x) (x :ch) x))))
(def core-float core-double)
# num and the unchecked-*/promoting-' arithmetic live in the Clojure
# collection tier (core/20-coll.clj) — jolt numbers don't overflow.
(defn core-char [x]
  "(char code-or-char) -> a character value."
  (cond
    (core-char? x) x
    (number? x) (make-char (math/trunc x))
    (string? x) (make-char (in x 0))
    (error "char expects a number or character")))

# ============================================================
# Hash
# ============================================================

(def core-hash (fn [x] (hash x)))


# ============================================================
# Atom
# ============================================================

(defn core-atom
  "Create an atom. Accepts optional :validator fn and :meta map."
  [val & opts]
  (var atm @{:jolt/type :jolt/atom :value val :watches @{} :validator nil})
  (var i 0)
  (while (< i (length opts))
    (case (opts i)
      :validator (put atm :validator (opts (+ i 1)))
      :meta (let [m (opts (+ i 1))]
              (var meta-tab @{})
              (each k (keys m) (put meta-tab k (get m k)))
              (table/setproto atm meta-tab)
              (put atm :jolt/meta m)))
    (+= i 2))
  atm)

# atom? now lives in the Clojure collection tier (tagged-value predicate).

# Futures — run the body on a real OS thread (ev/thread) for true parallelism.
# Janet threads have separate heaps, so the thunk and the state it closes over are
# MARSHALLED (copied) to the worker thread and the result is marshalled back. A
# future therefore sees a *snapshot* of captured state and communicates only via
# its return value — mutating a captured atom does not propagate to the parent.
# Coordination uses two channels: a thread-chan carries the single [:ok v] /
# [:error e] result back, and a parent-local chan acts as a broadcast latch that
# is closed when the result lands so any number of deref-ers can unpark.
(defn core-future? [x] (and (table? x) (= :jolt/future (x :jolt/type))))

(defn core-future-call [thunk]
  (def tc (ev/thread-chan 1))          # worker thread -> collector (shared, thread-safe)
  (def latch (ev/chan))                # parent-local: closed when the result is in
  (def fut @{:jolt/type :jolt/future :latch latch :cached false :res nil :cancelled false})
  # Worker: compute on a fresh OS thread, send back a marshalled result. The give
  # is guarded so a non-marshallable value can't strand deref-ers forever.
  (ev/spawn-thread
    (def res (try [:ok (thunk)] ([e] [:error e])))
    (try (ev/give tc res)
      ([_] (ev/give tc [:error "future result is not marshallable across threads"]))))
  # Collector: a parent-side fiber bridges the single result into the box and
  # closes the latch to wake every waiter. If the future was already cancelled,
  # the box is finalized — drop the late result and don't re-close the latch.
  (ev/spawn
    (def res (ev/take tc))
    (when (not (fut :cancelled))
      (put fut :res res)
      (put fut :cached true)
      (try (ev/chan-close latch) ([_] nil))))
  fut)

(defn- future-result [fut]
  (def res (fut :res))
  (if (= :error (in res 0)) (error (in res 1)) (in res 1)))

# future-done? / future-cancelled? now live in the Clojure collection tier (pure
# reads of :cached/:cancelled). core-future? stays — deref/future-cancel call it.
# Janet OS threads can't be interrupted, so the worker still runs to completion
# in the background; we can only mark the *future* cancelled (done) so deref
# raises and realized?/future-done?/future-cancelled? reflect it. Returns false
# if the future has already completed (matching Clojure).
(defn core-future-cancel [x]
  (if (and (core-future? x) (not (x :cached)) (not (x :cancelled)))
    (do
      (put x :cancelled true)
      (put x :res [:error "future cancelled"])
      (put x :cached true)
      (try (ev/chan-close (x :latch)) ([_] nil))
      true)
    false))

# future macro: (future body...) -> (future-call (fn* [] body...))
(defn core-deref [ref & opts]
  (cond
    (and (table? ref) (= :jolt/reduced (ref :jolt/type)))
    (ref :val)
    (and (table? ref) (= :jolt/atom (ref :jolt/type)))
    (ref :value)
    (and (table? ref) (= :jolt/volatile (ref :jolt/type)))
    (ref :val)
    (and (table? ref) (= :jolt/delay (ref :jolt/type)))
    (if (ref :realized) (ref :val)
      (let [v ((ref :fn))] (put ref :val v) (put ref :realized true) v))
    (and (table? ref) (= :jolt/future (ref :jolt/type)))
    (if (empty? opts)
      (do (when (not (ref :cached)) (ev/take (ref :latch))) (future-result ref))
      # (deref future timeout-ms timeout-val): wait at most timeout-ms. The
      # deadline cancels the parked take; if the result still hasn't landed we
      # return the supplied timeout value (the future keeps running).
      (let [timeout-val (in opts 1)]
        (when (not (ref :cached))
          (try (ev/with-deadline (/ (in opts 0) 1000) (ev/take (ref :latch))) ([_] nil)))
        (if (ref :cached) (future-result ref) timeout-val)))
    (and (table? ref) (= :jolt/var (ref :jolt/type)))
    (ref :root)
    ref))

(defn- atom-validate
  "Call validator on atm. Returns the value if valid, errors otherwise."
  [atm val]
  (let [v (atm :validator)]
    (if v
      (if (v val) val
        (error "Validator rejected value"))
      val)))

(defn- atom-notify-watches
  [atm old-val new-val]
  (loop [[k w] :pairs (atm :watches)]
    (w k atm old-val new-val)))

(defn core-reset! [atm val]
  (let [old-val (atm :value)]
    (atom-validate atm val)
    (put atm :value val)
    (atom-notify-watches atm old-val val)
    val))

(defn core-swap! [atm f & args]
  (var old-val (atm :value))
  (var new-val (apply f old-val args))
  (atom-validate atm new-val)
  (put atm :value new-val)
  (atom-notify-watches atm old-val new-val)
  new-val)

# Atom peripheral ops (swap-vals!/reset-vals!/compare-and-set!/get-validator/
# add-watch/remove-watch/set-validator!) now live in the Clojure collection tier —
# composed over the native atom ops + jolt.host/ref-put!. atom/swap!/reset!/deref
# and the atom-validate/atom-notify-watches helpers stay native (compiler-critical).

# ============================================================
# Threading macros (as regular functions? No, as macros in Clojure)
# These need to be defined as macros in the Jolt namespace system.
# For now, skip — they need proper macro definition via the evaluator.
# ============================================================

# ============================================================
# Initialization — intern everything into a context's namespace
# ============================================================

(def gensym_counter @{:val 0})

(defn gensym
  "Returns a new symbol with a unique name."
  [&opt prefix-string]
  (default prefix-string "G__")
  (def n (get gensym_counter :val))
  (put gensym_counter :val (+ n 1))
  {:jolt/type :symbol :ns nil :name (string prefix-string n)})


# if-let/when-let/if-some/when-some now live in the Clojure overlay
# (core/30-macros.clj) as defmacros.

(defn core-push-thread-bindings [b] (push-thread-bindings b))
(defn core-pop-thread-bindings [] (pop-thread-bindings))

(defn core-var-get [v] (var-get v))
(defn core-var-set [v val] (var-set v val))
(defn core-var? [x] (var? x))
(defn core-alter-var-root [v f & args] (apply alter-var-root v f args))
(defn core-alter-meta! [v f & args] (apply alter-meta! v f args))
(defn core-reset-meta! [v meta] (reset-meta! v meta))

# intern is a ctx-capturing clojure.core fn now (install-stateful-fns!).

# get-method/methods/remove-method/remove-all-methods/prefer-method are
# overlay macros (core/30-macros.clj) over the evaluator's *-setup fns.

(defn core-with-meta [obj meta]
  # Functions and scalars can't carry metadata in Jolt's model — return as-is
  # rather than crashing (Clojure attaches meta only to IObj values).
  (if (or (function? obj) (cfunction? obj) (number? obj) (boolean? obj)
          (nil? obj) (string? obj) (keyword? obj) (buffer? obj))
    obj
    (do
      (var new-obj @{})
      (each k (keys obj)
        (put new-obj k (get obj k)))
      # table/setproto requires a table, convert struct meta to table. meta may
      # be nil (Clojure allows (with-meta obj nil) to clear metadata).
      (var meta-tab @{})
      (when meta (each k (keys meta) (put meta-tab k (get meta k))))
      (table/setproto new-obj meta-tab)
      (put new-obj :jolt/meta meta)
      new-obj)))

(defn core-var-dynamic? [v]
  (var-dynamic? v))

# Java interop stubs
(def core-Object (fn [] (struct ;[:jolt/type :jolt/java-object])))

# Volatiles — typed box so deref/volatile? can recognize them.
(defn core-volatile! [v] @{:jolt/type :jolt/volatile :val v})
# volatile? / vreset! / vswap! now live in the Clojure collection tier — vreset!
# over jolt.host/ref-put!, vswap! over vreset! + get. The constructor stays native.

# Delays — created lazily by the `delay` macro; forced once via force/deref.
(defn core-make-delay [thunk] @{:jolt/type :jolt/delay :fn thunk :realized false :val nil})
(defn core-delay? [x] (and (table? x) (= :jolt/delay (x :jolt/type))))
# Proxy stub — returns nil form (macro, args not evaluated)
# Thread stubs
(def core-Thread (fn [& args] (struct ;[:jolt/type :jolt/thread])))
(def core-ThreadLocal (fn [& args] (struct ;[:jolt/type :jolt/thread-local])))
(def core-IllegalStateException (fn [& args] (struct ;[:jolt/type :jolt/exception])))



# letfn — mutually-recursive local fns. Expands to let* of fn* bindings; jolt
# closures capture the (shared, mutable) bindings table, so forward references
# between the fns resolve at call time.

# doseq — like `for` but eager and returns nil. Reuse `for`, force realization
# with `count`, discard the result.
# assert — (assert x) / (assert x message). Throws when x is falsy.

# resolve stub — returns nil (symbols not found in Jolt's clojure.core)
(defn core-resolve [sym] nil)  # shadowed by the resolve special form (needs ctx)
# ns-name now lives in the Clojure collection tier (pure over get + symbol).

# update lives in the Clojure kernel tier — core/00-kernel.clj. update-in stays
# (it's recursive and has internal callers).
(defn- ks-rest [ks]
  (if (tuple? ks) (tuple/slice ks 1) (array/slice ks 1)))

# assoc-in / update-in now live in the Clojure collection tier (canonical
# recursive ports).



# fnil now lives in the Clojure collection tier (core/20-coll.clj), with
# Clojure's canonical 2/3/4-arity (patch the first 1-3 args only).

# copy-var stubs for sci.impl.copy-vars (used by sci.impl.namespaces)
(defn core-copy-core-var [sym] nil)
(defn core-copy-var [sym & args] nil)
(defn core-macrofy [sym fn & more] fn)
(defn core-new-var [sym & args] nil)
# A free-standing var cell (not interned anywhere): with-local-vars binds
# these as locals; var-get/var-set work on any cell.
(defn core-local-var [&opt val]
  @{:jolt/type :jolt/var :name "local" :ns nil :root val :gen 0})
# with-open's close seam: a map-like value closes via its :close fn, a host
# file via file/close. No .close interop on the Janet host.
(defn core-close-resource [x]
  (cond
    (and (or (table? x) (struct? x)) (function? (get x :close))) ((get x :close))
    (= :core/file (type x)) (file/close x)
    (error (string "with-open: don't know how to close " (type x)))))
# sci stub: pass the registry map through (it was @{} — a raw host table that
# strict map-conj rightly rejects; identity also keeps sci's registry intact).
(defn core-avoid-method-too-large [& args] (if (> (length args) 0) (in args 0) {}))

# declare macro — accepts symbols, does nothing (forward declaration)

# Build a protocol value (a self-evaluating tagged table). Exposed so the overlay
# `defprotocol` can construct one via a fn call rather than embedding a tagged
# struct literal (which the interpreter would try to re-evaluate). `methods` is a
# {kw {:name str}} map; only :name is consulted (by satisfies?).
(defn core-make-protocol [name-str methods]
  @{:jolt/type :jolt/protocol
    :name {:jolt/type :symbol :ns nil :name name-str}
    :methods methods})

(def core-satisfies? (fn [proto-sym obj] false))

# extends? is a real overlay fn now (30-macros, over extenders).
(def core-implements? (fn [& args] false))
(def core-type->str (fn [& args] ""))

# ============================================================
# Additional clojure.core functions (conformance batch)
# ============================================================

(defn core-keyword
  "(keyword name) or (keyword ns name). Namespaced keywords are `:ns/name`.
  (keyword nil) is nil; the 2-arg form requires string args (nil ns allowed)."
  [& args]
  (case (length args)
    1 (let [a (in args 0)]
        (cond
          (nil? a) nil
          (keyword? a) a
          (or (string? a) (core-symbol? a)) (keyword (core-name a))
          (error (string "keyword requires a string, symbol or keyword, got " (type a)))))
    2 (let [ns (in args 0) nm (in args 1)]
        (when (not (and (or (nil? ns) (string? ns)) (string? nm)))
          (error "keyword ns and name must be strings"))
        (keyword (if ns (string ns "/" nm) nm)))
    (keyword ;args)))

(defn core-symbol
  "(symbol name) or (symbol ns name) -> a jolt symbol struct. name/ns must be
  strings (a single symbol arg is returned as-is)."
  [& args]
  (case (length args)
    1 (let [a (in args 0)]
        (cond
          (core-symbol? a) a
          (or (string? a) (keyword? a)) {:jolt/type :symbol :ns nil :name (core-name a)}
          (error (string "symbol requires a string or symbol, got " (type a)))))
    2 (let [ns (in args 0) nm (in args 1)]
        (when (not (and (or (nil? ns) (string? ns)) (string? nm)))
          (error "symbol ns and name must be strings"))
        {:jolt/type :symbol :ns ns :name nm})
    (error "symbol expects 1 or 2 args")))

# (take-nth's transducer arity lives in the overlay now.)


# filterv now lives in the Clojure collection tier (core/20-coll.clj).

# mapv lives in the Clojure kernel tier — core/00-kernel.clj.

# (interpose's transducer arity lives in the overlay now.)
# interpose / take-nth now live in the Clojure lazy tier (core/40-lazy.clj),
# with the canonical transducer arities.

# keep now lives in the Clojure lazy tier (core/40-lazy.clj).

# empty now lives in the Clojure collection tier (core/20-coll.clj); a lazy
# seq empties to () there (this fn returned a host table for it).

# not-empty now lives in the Clojure collection tier (core/20-coll.clj).

# rseq is defined only on vectors and sorted collections (Reversible).
(defn core-rseq [coll]
  (cond
    (pvec? coll) (tuple/slice (tuple ;(reverse (pv->array coll))))
    (core-sorted? coll) ((sorted-op coll :rseq) coll)
    (error (string "rseq requires a vector or sorted collection, got " (type coll)))))



# some-fn now lives in the Clojure collection tier (core/20-coll.clj).

# Associative = maps and (real) vectors only. pvec is a literal/built vector;
# tuples and lists are seq results, not associative.
# ifn? now lives in the Clojure collection tier — canonical IFn set (fns,
# keywords, symbols, maps, sets, vectors, vars); lists are NOT IFn.
# With a single item, Clojure returns it WITHOUT calling f. On ties, the last
# extremal item wins (>=/<= update), matching Clojure.
# Clojure's min-key/max-key: the 2-arg base compares with strict < / > (so the
# second wins on ties/NaN), and each further item switches on <= / >=. This
# asymmetry reproduces the JVM's NaN-ordering behavior. Janet's < / > are used
# directly (NaN comparisons are false, never throwing).
# keys must be numbers (NaN allowed) — like Clojure, which compares them with </>.
# min-key / max-key now live in the Clojure collection tier (core/20-coll.clj).

# vary-meta / namespace-munge now live in the Clojure collection tier
# (core/20-coll.clj) — pure compositions of meta/with-meta and str/map.

# Exceptions (ex-info / ex-data / ex-message)
(defn core-ex-info [msg data & more]
  @{:jolt/type :jolt/ex-info :message msg :data data
    :cause (if (> (length more) 0) (in more 0) nil)})
# ex-data / ex-message / ex-cause now live in the Clojure collection tier
# (core/20-coll.clj) — pure over get on the tagged value the constructor builds.

# String split/replace that accept either a literal string or a regex value.
(defn core-str-split [pat s]
  (if (regex? pat)
    (re-split pat s)
    (string/split pat s)))
(defn core-str-replace-all [pat repl s]
  (if (regex? pat)
    (re-replace-all pat s repl)
    (string/replace-all pat repl s)))
(defn core-str-replace-first [pat repl s]
  (if (regex? pat)
    (re-replace-first pat s repl)
    (string/replace pat repl s)))

# Iterator/enumeration seqs — Jolt has no Java iterators, so adapt to plain seq.
# enumeration-seq / iterator-seq live in the Clojure collection tier.
# xml-seq now lives in the Clojure collection tier (core/20-coll.clj).
# line-seq now lives in the Clojure IO tier (core/50-io.clj), over the reader
# protocol of the *in* family.
(defn core-re-matcher [re s] @{:jolt/type :jolt/matcher :re re :s s :pos 0})

# bean / print-method / print-dup / the proxy surface live in the Clojure
# collection tier (JVM-shape stubs; print hooks inert until jolt-g1r).
# == lives in the Clojure collection tier (core/20-coll.clj); memfn is an
# overlay macro (core/30-macros.clj) over the .method call sugar.
# eduction / ->Eduction live in the Clojure collection tier (core/20-coll.clj).

(def- char-escapes
  {10 "\\n" 9 "\\t" 13 "\\r" 12 "\\f" 8 "\\b" 34 "\\\"" 92 "\\\\"})
(def- char-names
  {10 "newline" 9 "tab" 13 "return" 12 "formfeed" 8 "backspace" 32 "space"})
# char-escape-string / char-name-string now live in the Clojure collection
# tier as char-keyed maps. The CODE-keyed tables below stay: pr-render uses them.


# subseq / rsubseq over sorted collections
# subseq / rsubseq now live in the Clojure sorted tier (core/25-sorted.clj),
# along with the constructors and all sorted-coll semantics.

# ============================================================
# Additional clojure.core functions
# ============================================================

# Integer-valued: a finite number equal to its floor. Infinity floors to itself
# but is NOT integer-valued (so float?/double? are true for ##Inf, and int?/
# pos-int?/… are false), and NaN is excluded by the equality check.
(defn- intval? [x] (and (number? x) (< (math/abs x) math/inf) (= x (math/floor x))))

# Forcing lazy seqs
# Map entries (represented as 2-element vectors)
# key/val require a map entry (a 2-element vector/tuple in Jolt); Clojure throws
# otherwise. (Jolt can't distinguish a 2-vector from a real MapEntry.)
# A map entry is a 2-element tuple — Jolt produces tuples only from map
# iteration (first/seq/map over a map), while vector literals are pvecs and
# lists are arrays. So key/val/map-entry? accept a 2-tuple and reject a plain
# vector, matching Clojure (where a MapEntry is distinct from a vector).
(defn- entry-like? [x] (and (tuple? x) (= 2 (length x))))
# key / val now live in the Clojure collection tier (core/20-coll.clj),
# along with find (previously missing from jolt entirely).
(defn core-map-entry? [x] (entry-like? x))

# Reversible (supports rseq) = vectors and sorted collections.
# Numeric predicates (Jolt has no ratios/bigdec). nat-int?/pos-int?/neg-int?/
# ratio?/decimal?/rational? live in the Clojure collection tier (core/20-coll.clj).
# Jolt has no ratio type, so numerator/denominator have no valid input (Clojure
# requires a Ratio and throws otherwise).
# numerator / denominator now live in the Clojure collection tier (Jolt has
# no ratios; they throw, as on a non-ratio in Clojure).

# special-symbol? lives in the Clojure collection tier (a quoted symbol set).

# record? now lives in the Clojure collection tier (tagged-value predicate).

# Promise: single-threaded box backed by an atom (deref returns nil until set).
# promise / deliver live in the Clojure collection tier (an atom; deref of an
# undelivered promise is nil — single-threaded host, no blocking).

(defn core-tagged-literal [tag form] @{:jolt/type :jolt/tagged-literal :tag tag :form form})
# ensure-reduced / halt-when live in the Clojure collection tier
# (core/20-coll.clj) — halt-when is the canonical ::halt-map version there.
(defn core-re-groups [m] (error "re-groups: stateful matchers are not supported in Jolt"))

# Transients — real mutable scratch collections backed by Janet's native arrays
# and tables (host interop): O(1) conj!/assoc!/dissoc!/disj!/pop!, frozen back to
# a persistent value by persistent!. A transient is a tagged table holding either
# a Janet array (vectors) or a Janet table keyed by canonical key (maps/sets, so
# collection keys still compare by value). The mutating ops return the transient.
(defn core-transient [coll]
  (cond
    (pvec? coll)
      @{:jolt/type :jolt/transient :kind :vector :arr (pv->array coll)}
    (set? coll)
      (let [t @{}] (each e (phs-seq coll) (put t (canon-key e) e))
        @{:jolt/type :jolt/transient :kind :set :tbl t})
    (or (phm? coll) (and (struct? coll) (nil? (get coll :jolt/type))))
      (let [t @{}]
        (each pair (realize-for-iteration coll)
          (put t (canon-key (in pair 0)) @[(in pair 0) (in pair 1)]))
        @{:jolt/type :jolt/transient :kind :map :tbl t})
    # mutable-build arrays (vectors/lists) — copy into a transient vector
    (array? coll) @{:jolt/type :jolt/transient :kind :vector :arr (array/slice coll)}
    # tuples (reader vectors / map entries) are vectors too
    (tuple? coll) @{:jolt/type :jolt/transient :kind :vector :arr (array ;coll)}
    (error (string "Don't know how to create a transient from " (type coll)))))

# A transient is invalidated by persistent!; using it afterwards is a bug.
(defn- tr-check-active! [t]
  (when (get t :jolt/persistent)
    (error "Transient used after persistent! call")))

(defn- tr-conj! [t x]
  (tr-check-active! t)
  (case (t :kind)
    :vector (array/push (t :arr) x)
    :set    (put (t :tbl) (canon-key x) x)
    :map    (cond
              # a [k v] pair (map-entry / 2-vector)
              (and (or (pvec? x) (tuple? x) (array? x))
                   (= 2 (if (pvec? x) (pv-count x) (length x))))
                (put (t :tbl) (canon-key (vnth x 0)) @[(vnth x 0) (vnth x 1)])
              # a map: merge all its entries
              (or (phm? x) (and (struct? x) (nil? (get x :jolt/type))))
                (each e (map-entries-of x)
                  (put (t :tbl) (canon-key (in e 0)) @[(in e 0) (in e 1)]))
              (error "conj! on a transient map requires a [key value] pair or a map")))
  t)

(defn- tr-assoc! [t k v]
  (tr-check-active! t)
  (case (t :kind)
    :vector (let [a (t :arr)]
              (when (not (and (number? k) (= k (math/floor k)) (>= k 0) (<= k (length a))))
                (error (string "Index " k " out of bounds for assoc! on a transient vector of length " (length a))))
              (if (= k (length a)) (array/push a v) (put a k v)))
    :map    (put (t :tbl) (canon-key k) @[k v])
    (error "assoc! expects a transient vector or map"))
  t)

# The bang ops require a transient (Clojure throws otherwise); no lenient
# fallback to the persistent op.
(defn core-conj! [& args]
  (cond
    (= 0 (length args)) (core-transient (make-vec @[]))   # (conj!) -> (transient [])
    (= 1 (length args)) (first args)                      # (conj! coll) -> coll, as-is
    (let [t (first args) xs (tuple/slice args 1)]
      (if (core-transient? t)
        (do (each x xs (tr-conj! t x)) t)
        (error "conj! requires a transient")))))

(defn core-assoc! [t & kvs]
  # Unlike assoc, assoc! accepts an ODD number of args — a missing final value
  # is taken as nil (so (get kvs (+ i 1)) rather than (in ...), which would
  # error on the dangling key).
  (if (core-transient? t)
    (do (var i 0) (while (< i (length kvs)) (tr-assoc! t (in kvs i) (get kvs (+ i 1))) (+= i 2)) t)
    (error "assoc! requires a transient")))

(defn core-dissoc! [t & ks]
  (if (and (core-transient? t) (= :map (t :kind)))
    (do (tr-check-active! t) (each k ks (put (t :tbl) (canon-key k) nil)) t)
    (error "dissoc! requires a transient map")))

(defn core-disj! [t & xs]
  (if (and (core-transient? t) (= :set (t :kind)))
    (do (tr-check-active! t) (each x xs (put (t :tbl) (canon-key x) nil)) t)
    (error "disj! requires a transient set")))

(defn core-pop! [t]
  (if (and (core-transient? t) (= :vector (t :kind)))
    (do (tr-check-active! t)
        (when (= 0 (length (t :arr))) (error "Can't pop empty vector"))
        (array/pop (t :arr)) t)
    (error "pop! requires a transient vector")))

(defn core-persistent! [t]
  (if (core-transient? t)
    (do
      (tr-check-active! t)
      (def result
        (case (t :kind)
          :vector (make-vec (t :arr))
          :set (do (var s (make-phs)) (each [_ e] (pairs (t :tbl)) (set s (phs-conj s e))) s)
          :map (do (var m (make-phm)) (each [_ pair] (pairs (t :tbl)) (set m (phm-assoc m (in pair 0) (in pair 1)))) m)))
      # Invalidate: any further bang op (or a second persistent!) now throws.
      (put t :jolt/persistent true)
      result)
    (error "persistent! requires a transient")))

# Unchecked arithmetic — Jolt numbers don't overflow, so these are plain ops.
# unchecked-* arithmetic lives in the Clojure collection tier
# (core/20-coll.clj); only the masking byte/short/char coercions remain above.

# Hashing helpers
# Hashes are masked to 24 bits at each step so intermediate products stay within
# Janet's integer range (a float here would make band error).
(defn- h24 [x] (band (hash x) 0xffffff))
(defn core-hash-combine [a b] (band (bxor (h24 a) (+ (h24 b) 0x9e3779)) 0xffffff))
(defn core-hash-ordered-coll [coll]
  (var h 1) (each x (realize-for-iteration coll) (set h (band (+ (* 31 h) (h24 x)) 0xffffff))) h)
(defn core-hash-unordered-coll [coll]
  (var h 0) (each x (realize-for-iteration coll) (set h (band (+ h (h24 x)) 0xffffff))) h)

# prefers is a macro over prefers-setup now (the store lives on the VAR).



# parse-uuid lives in the Clojure collection tier (core/20-coll.clj) over
# re-matches + the __make-uuid host constructor (types.janet).

(def- core-bindings
  "Map of symbol name → function for all core functions."
  @{"nil?" core-nil?
    "string?" core-string?
    "number?" core-number?
    "fn?" core-fn?
    "keyword?" core-keyword?
    "symbol?" core-symbol?
    "vector?" core-vector?
    "map?" core-map?
    "seq?" core-seq?
    "coll?" core-coll?
    "identical?" core-identical?
    "integer?" core-integer?
    "list?" core-list?
    "+" core-+
    "-" core-sub
    "*" core-*
    "/" core-/
    "inc" core-inc
    "dec" core-dec
    "even?" core-even?
    "odd?" core-odd?
    "mod" core-mod
    "rem" core-rem
    "quot" core-quot
    "rand" core-rand
    "=" core-=
    "<" core-<
    ">" core->
    "<=" core-<=
    ">=" core->=
    "conj" core-conj
    "assoc" core-assoc
    "dissoc" core-dissoc
    "get" core-get
    "contains?" core-contains?
    "count" core-count
    "format" core-format
    "first" core-first
    "rest" core-rest
    "next" core-next
    "cons" core-cons
    "seq" core-seq
    "vec" core-vec
    "__sq1" core-sq1
    "__sqcat" core-sqcat
    "__sqvec" core-sqvec
    "__sqmap" core-sqmap
    "__sqset" core-sqset
    "into" core-into
    "with-meta" core-with-meta
    "map" core-map
    "filter" core-filter
    "remove" core-remove
    "reduce" core-reduce
    "apply" core-apply
    "map-entry?" core-map-entry?
    "future-call" core-future-call
    "future?" core-future?
    "future-cancel" core-future-cancel
    "tagged-literal" core-tagged-literal
    "re-groups" core-re-groups
    "transient" core-transient
    "transient?" core-transient?
    "persistent!" core-persistent!
    "conj!" core-conj!
    "assoc!" core-assoc!
    "dissoc!" core-dissoc!
    "pop!" core-pop!
    "hash-combine" core-hash-combine
    "hash-ordered-coll" core-hash-ordered-coll
    "hash-unordered-coll" core-hash-unordered-coll
    "gensym" gensym
    "__write" core-write
    "__pr-str1" core-pr-str1
    "__make-uuid" make-uuid
    "compare" core-compare
    "type" core-type
    "slurp" core-slurp
    "spit" core-spit
    "flush" core-flush
    "get-thread-bindings" core-get-thread-bindings
    "__thread-bound?" core-thread-bound?*
    "__dir?" core-dir?
    "__list-dir" core-list-dir
    "parse-long" core-parse-long
    "parse-double" core-parse-double
    "current-time-ms" core-current-time-ms
    "mapcat" core-mapcat
    "sequence" core-sequence
    "keyword" core-keyword
    "symbol" core-symbol
    "namespace" core-namespace
    "reduced" core-reduced
    "reduced?" core-reduced?
    "rseq" core-rseq
    "ex-info" core-ex-info
    "__with-out-str" core-with-out-str
    "delay?" core-delay?
    "make-delay" core-make-delay
    "take" core-take
    "drop" core-drop
    "take-while" core-take-while
    "drop-while" core-drop-while
    "concat" core-concat
    "nth" core-nth
    "sort" core-sort
    "partition" core-partition
    "range" core-range
    "vector" core-vector
    "hash-map" core-hash-map
    "array-map" core-array-map
    "hash-set" core-hash-set
    "set" core-set
    "list" core-list
    "set?" core-set?
    "disj" core-disj
    "coll->cells" coll->cells
    "make-lazy-seq" make-lazy-seq
    "lazy-cons" lazy-cons
    "lazy-from" lazy-from
    "str" core-str
    "name" core-name
    "subs" core-subs
    "str-trim" string/trim
    "str-upper" string/ascii-upper
    "str-lower" string/ascii-lower
    "str-find" string/find
    "str-replace" core-str-replace-first
    "str-replace-all" core-str-replace-all
    "str-reverse-b" string/reverse
    "str-join" core-str-join
    "str-split" core-str-split
    "re-pattern" re-pattern
    "re-find" re-find
    "re-matches" re-matches
    "re-seq" re-seq
    "regex?" regex?
    "str-triml" string/triml
    "str-trimr" string/trimr
    # Java-style arrays (buffers for bytes, arrays otherwise)
    "aclone" core-aclone
    "object-array" core-object-array
    "int-array" core-int-array
    "long-array" core-long-array
    "short-array" core-short-array
    "double-array" core-double-array
    "float-array" core-float-array
    "char-array" core-char-array
    "boolean-array" core-boolean-array
    "byte-array" core-byte-array
    "aset-byte" core-aset-byte
    "aset-int" core-aset-int
    "aset-long" core-aset-long
    "aset-short" core-aset-short
    "aset-double" core-aset-double
    "aset-float" core-aset-float
    "aset-char" core-aset-char
    "aset-boolean" core-aset-boolean
    "make-array" core-make-array
    "into-array" core-into-array
    "to-array" core-to-array
    "bytes" core-bytes
    "booleans" core-booleans
    "ints" core-ints
    "longs" core-longs
    "shorts" core-shorts
    "doubles" core-doubles
    "floats" core-floats
    "chars" core-chars
    "byte" core-byte
    "short" core-short
    "bigint" core-bigint
    "biginteger" core-biginteger
    "chunk-buffer" core-chunk-buffer
    "chunk-append" core-chunk-append
    "chunk" core-chunk
    "chunk-first" core-chunk-first
    "chunk-rest" core-chunk-rest
    "chunk-next" core-chunk-next
    "chunk-cons" core-chunk-cons
    "boolean" core-boolean
    "cat" core-cat
    "disj!" core-disj!
    "reader-conditional" core-reader-conditional
    "class" core-class
    "re-matcher" core-re-matcher
    # Bit operations
    "__bit-and" core-bit-and
    "__bit-or" core-bit-or
    "__bit-xor" core-bit-xor
    "bit-not" core-bit-not
    "bit-shift-left" core-bit-shift-left
    "bit-shift-right" core-bit-shift-right
    "bit-clear" core-bit-clear
    "bit-set" core-bit-set
    "bit-flip" core-bit-flip
    "bit-test" core-bit-test
    "__bit-and-not" core-bit-and-not
    "unsigned-bit-shift-right" core-unsigned-bit-shift-right
    # Integer coercion / unchecked math
    "int" core-int
    "long" core-long
    "double" core-double
    "float" core-float
    "char" core-char
    # Hash
    "hash" core-hash
    "atom" core-atom
    "deref" core-deref
    "reset!" core-reset!
    "swap!" core-swap!
    "not" core-not
    "Object" core-Object
    "make-protocol" core-make-protocol
    "satisfies?" core-satisfies?
    "implements?" core-implements?
    "type->str" core-type->str
    "volatile!" core-volatile!
    "Thread" core-Thread
    "ThreadLocal" core-ThreadLocal
    "IllegalStateException" core-IllegalStateException
    "resolve" core-resolve
    "copy-core-var" core-copy-core-var
    "copy-var" core-copy-var
    "macrofy" core-macrofy
    "new-var" core-new-var
    "__local-var" core-local-var
    "__close" core-close-resource
    "avoid-method-too-large" core-avoid-method-too-large
    "bytes?" core-bytes?
    "meta" core-meta
    "var-get" core-var-get
    "var-set" core-var-set
    "var?" core-var?
    "var-dynamic?" core-var-dynamic?
    "alter-var-root" core-alter-var-root
    "alter-meta!" core-alter-meta!
    "reset-meta!" core-reset-meta!
    "push-thread-bindings" core-push-thread-bindings
    "pop-thread-bindings" core-pop-thread-bindings
    # Dynamic vars — stubs for SCI bootstrap
    "*unchecked-math*" false
    "*clojure-version*" @{:major 1 :minor 11 :incremental 0 :qualifier nil}
    "*1" :jolt/nil-sentinel
    "*2" :jolt/nil-sentinel
    "*3" :jolt/nil-sentinel
    "*e" :jolt/nil-sentinel
    "*assert" true})

(defn core-macro-names
  "Set of core binding names that are macros. Empty now that every core macro
  lives in the Clojure overlay (clojure.core.*-syntax / *-macros tiers)."
  []
  @{})

# Wire the print-method callback once the overlay (and its print-method
# multimethod) exists: the renderer's record fallthrough consults the methods
# table on the var; only a USER-registered method fires — the multimethod's
# :default would bounce straight back into the renderer.
(defn install-print-method-cb! [ctx]
  (def core-ns (ctx-find-ns ctx "clojure.core"))
  (def pm-var (ns-find core-ns "print-method"))
  (when pm-var
    (set-print-method-cb!
      (fn [v emit]
        (def methods (get pm-var :jolt/methods))
        (when methods
          (def mt (core-meta v))
          (def t (and mt (core-get mt :type)))
          (def dval (if (keyword? t) t (core-type v)))
          (def m (get methods dval))
          (when m
            (m v @{:jolt/type :jolt/writer :sink emit})
            true))))))

(def init-core!
  (fn [& args]
    (case (length args)
      1 (let [ctx (args 0)
               ns (ctx-find-ns ctx "clojure.core")]
           (loop [[name fn] :pairs core-bindings]
             (def v (ns-intern ns name (if (= fn :jolt/nil-sentinel) nil fn)))
             (when (get (core-macro-names) name)
               (put v :macro true)))
           ns)
       2 (let [ctx (args 0) ns-name (args 1)
               ns (ctx-find-ns ctx ns-name)]
           (loop [[name fn] :pairs core-bindings]
             (def v (ns-intern ns name (if (= fn :jolt/nil-sentinel) nil fn)))
            (when (get (core-macro-names) name)
              (put v :macro true)))
          ns)
       (error "Wrong number of args passed to: init-core!"))))
