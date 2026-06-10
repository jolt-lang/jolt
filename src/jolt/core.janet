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
    # byte array (Janet buffer) -> array of byte values
    (buffer? c) (let [a @[]] (each x c (array/push a x)) a)
    # struct map literal (no :jolt/type marker — not a symbol/char) -> entries
    (and (struct? c) (nil? (get c :jolt/type))) (map (fn [k] (tuple k (get c k))) (keys c))
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
                (if (nil? rt) (set go false) (set cur (make-lazy-seq rt))))))))
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
(defn core-some? [x] (not (nil? x)))
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

(defn core-true? [x] (= true x))
(defn core-false? [x] (= false x))
(defn core-identical? [a b] (= a b))

# Strictness helpers: like Clojure, numeric ops reject non-numbers, and the
# integer ops (odd?/even?) reject non-integers (incl. infinities, NaN, fractions).
(defn- finite-num? [x] (and (number? x) (= x x) (< (if (< x 0) (- x) x) math/inf)))
(defn- need-num [x op]
  (if (number? x) x (error (string op " requires a number, got " (type x)))))
(defn- need-int [x op]
  (if (and (number? x) (= x x) (< (if (< x 0) (- x) x) math/inf) (= x (math/floor x))) x
    (error (string op " requires an integer"))))

(defn core-zero? [x] (= (need-num x "zero?") 0))
(defn core-pos? [x] (> (need-num x "pos?") 0))
(defn core-neg? [x] (< (need-num x "neg?") 0))
(defn core-even? [n] (= 0 (% (need-int n "even?") 2)))
(defn core-odd? [n] (not= 0 (% (need-int n "odd?") 2)))

(defn core-integer? [x] (and (number? x) (= x (math/floor x))))
(defn core-boolean? [x] (or (= x true) (= x false)))
(defn core-list? [x] (or (plist? x) (and (array? x) (not (get x :jolt/type)))))

(defn core-empty? [coll]
  (if (nil? coll) true
    (if (set? coll) (= 0 (coll :cnt))
      (if (phm? coll) (= 0 (coll :cnt))
        (if (pvec? coll) (= 0 (pv-count coll))
          (if (plist? coll) (pl-empty? coll)
          # Cell-based, NOT (nil? (ls-first)): a lazy-seq whose first element is
          # legitimately nil (e.g. a `nil` case-constant) is non-empty.
          (if (lazy-seq? coll)
            (let [cell (realize-ls coll)]
              (or (nil? cell) (= :jolt/pending cell) (= 0 (length cell))))
            (if (struct? coll) (= 0 (length (keys coll)))
              (= 0 (length coll))))))))))

(defn core-every? [pred coll]
  # Short-circuit on the first false — and pull lazily so an infinite seq with an
  # early false (e.g. (every? pos? (range))) returns rather than hanging. Walks
  # cells via realize-ls directly (core-first/lazy-from are defined later).
  (if (lazy-seq? coll)
    (do
      (var cur coll) (var result true) (var go true)
      (while (and result go)
        (let [cell (realize-ls cur)]
          (if (or (nil? cell) (= :jolt/pending cell) (= 0 (length cell)))
            (set go false)
            (if (pred (in cell 0))
              (let [rt (in cell 1)]
                (if (nil? rt) (set go false) (set cur (make-lazy-seq rt))))
              (set result false)))))
      result)
    (do
      (var result true)
      (each x (realize-for-iteration coll)
        (if (not (pred x)) (do (set result false) (break))))
      result)))

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

(defn core-max [& args] (each x args (need-num x "max")) (apply max args))
(defn core-min [& args] (each x args (need-num x "min")) (apply min args))

(defn core-rand [& n] (let [r (math/random)] (if (empty? n) r (* r (in n 0)))))
(defn core-rand-int [n] (math/floor (* (math/random) n)))

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
  "Return [k v] pairs for a map-like value (phm/struct/table), else nil."
  [x]
  (cond
    (phm? x) (phm-entries x)
    (and (table? x) (get x :jolt/deftype)) nil
    (struct? x) (pairs x)
    (table? x) (pairs x)
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
        # sets
        (or (set? a) (set? b))
          # value-based: same size and every element of a is value-equal to some
          # element of b (so #{ {:a 1} } equals #{ (hash-map :a 1) } regardless of
          # the elements' underlying representations)
          (if (and (set? a) (set? b) (= (a :cnt) (b :cnt)))
            (let [eb (phs-seq b)]
              (var ok true)
              (each x (phs-seq a)
                (unless (some (fn [y] (jolt-equal? x y)) eb) (set ok false)))
              ok)
            false)
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

(defn core-not= [& args] (not (apply core-= args)))

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
# Defined here (before the collection fns) so conj/assoc/get/contains?/keys/vals/
# disj can branch on them. A sorted-map is {:jolt/type :jolt/sorted-map :map STRUCT};
# a sorted-set is {:jolt/type :jolt/sorted-set :items SORTED-ARRAY}. Keys/elements
# are assumed Comparable scalars (the premise of a sorted coll); ops return a fresh
# wrapper (persistent — source unchanged). A wrapper may carry an optional :cmp
# (set by the by-comparator constructors) that all derived colls propagate.
(defn core-sorted-map? [x] (and (table? x) (= :jolt/sorted-map (x :jolt/type))))
(defn core-sorted-set? [x] (and (table? x) (= :jolt/sorted-set (x :jolt/type))))
(defn core-sorted? [x] (or (core-sorted-map? x) (core-sorted-set? x)))
# A sorted coll may carry a :cmp — a Janet 2-arg comparator returning a Clojure
# compare result (neg/0/pos). nil means natural order (Janet's < via sort). The
# by-comparator constructors install one (built from the user IFn); all derived
# colls (assoc/conj/...) propagate it so ordering stays consistent.
# A Clojure comparator is either a (neg/0/pos)-returning fn or a boolean predicate
# (true => a sorts before b, like <). Reduce both to a strict less-than for sort.
(defn- cmp-lt? [cmp a b]
  (let [r (cmp a b)]
    (if (boolean? r) r (if (number? r) (< r 0) (truthy? r)))))
(defn- sorted-by [cmp arr] (if cmp (sort arr (fn [a b] (cmp-lt? cmp a b))) (sort arr)))
(defn sm-make [m &opt cmp] @{:jolt/type :jolt/sorted-map :map m :cmp cmp})
(defn ss-make [items &opt cmp] @{:jolt/type :jolt/sorted-set :items items :cmp cmp})
(defn core-sorted-map [& kvs]
  (var m @{}) (var i 0)
  (while (< i (length kvs)) (put m (kvs i) (kvs (+ i 1))) (+= i 2))
  (sm-make (table/to-struct m)))
(defn core-sorted-set [& xs]
  (var seen @{}) (each x xs (put seen x true))
  (ss-make (sorted-by nil (array ;(keys seen)))))
(defn sorted-map-keys [sm] (sorted-by (sm :cmp) (array ;(keys (sm :map)))))
(defn sorted-map-entries [sm] (let [m (sm :map)] (map (fn [k] [k (get m k)]) (sorted-map-keys sm))))
(defn sm-assoc-many [sm kvs]
  (var m @{}) (each k (keys (sm :map)) (put m k (get (sm :map) k)))
  (var i 0) (while (< i (length kvs)) (put m (kvs i) (kvs (+ i 1))) (+= i 2))
  (sm-make (table/to-struct m) (sm :cmp)))
(defn sm-dissoc-many [sm ks]
  (def rm @{}) (each x ks (put rm x true))
  (var m @{}) (each k (keys (sm :map)) (unless (get rm k) (put m k (get (sm :map) k))))
  (sm-make (table/to-struct m) (sm :cmp)))
(defn ss-contains? [ss x] (var f false) (each e (ss :items) (when (deep= e x) (set f true) (break))) f)
(defn ss-conj-many [ss xs]
  (var seen @{}) (each e (ss :items) (put seen e true)) (each x xs (put seen x true))
  (ss-make (sorted-by (ss :cmp) (array ;(keys seen))) (ss :cmp)))
(defn ss-disj-many [ss xs]
  (def rm @{}) (each x xs (put rm x true))
  (ss-make (filter (fn [e] (not (get rm e))) (ss :items)) (ss :cmp)))

(defn core-conj [& args]
  (if (= 0 (length args)) (make-vec @[])        # (conj) -> []
  (let [coll (first args) xs (tuple/slice args 1)]
  (if (nil? coll)
    # conj onto nil builds a list (prepends): (conj nil 1 2) -> (2 1)
    (do (var result nil) (each x xs (set result (pl-cons x result))) result)
  (if (core-sorted-map? coll)
    # conj a [k v] entry (or merge a map) into a sorted-map
    (do (var m coll)
      (each x xs
        (if (map-value? x)
          (each e (map-entries-of x) (set m (sm-assoc-many m [(in e 0) (in e 1)])))
          (set m (sm-assoc-many m [(vnth x 0) (vnth x 1)]))))
      m)
  (if (core-sorted-set? coll)
    (ss-conj-many coll xs)
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
        (if (phm? coll)
          (do
            (var result coll)
            (each x xs
              (if (map-value? x)
                # conj a map -> merge its entries
                (each e (map-entries-of x)
                  (set result (phm-assoc result (in e 0) (in e 1))))
                (set result (phm-assoc result (vnth x 0) (vnth x 1)))))
            result)
          (do
            (var result coll)
            (each x xs
              (if (map-value? x)
                (each e (map-entries-of x)
                  (set result (map-assoc1 result (in e 0) (in e 1))))
                (set result (map-assoc1 result (vnth x 0) (vnth x 1)))))
            result)))))))))))))

(defn core-assoc [m & kvs]
  (when (odd? (length kvs))
    (error "assoc expects an even number of key/value arguments"))
  # assoc is defined on maps, vectors and nil; reject other shapes
  (when (or (number? m) (string? m) (buffer? m) (keyword? m) (boolean? m)
            (plist? m) (set? m) (core-transient? m)
            (and (struct? m) (get m :jolt/type)))
    (error (string "assoc requires a map or vector, got " (type m))))
  (cond
    (core-sorted-map? m) (sm-assoc-many m kvs)
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
    (core-sorted-map? m) (sm-dissoc-many m ks)
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
    (if (core-sorted-map? m) (let [v (get (m :map) k)] (if (nil? v) default v))
    (if (core-sorted-set? m) (if (ss-contains? m k) k default)
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
            default))))))))))

# Runtime invoke dispatch for COMPILED code (interpreter uses evaluator's
# jolt-invoke). Handles real functions plus Clojure IFn collections.
(defn jolt-call [f & args]
  (cond
    (or (function? f) (cfunction? f)) (apply f args)
    (keyword? f) (core-get (get args 0) f (get args 1))
    (and (struct? f) (= :symbol (f :jolt/type))) (core-get (get args 0) f (get args 1))
    (core-sorted-map? f) (let [v (get (f :map) (get args 0))] (if (nil? v) (get args 1) v))
    (core-sorted-set? f) (if (ss-contains? f (get args 0)) (get args 0) (get args 1))
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
            tail (cond (set? t) (phs-seq t) (phm? t) (tuple ;(phm-entries t))
                       (realize-for-iteration t))]
        (jolt-call f ;fixed ;tail)))))

(defn core-get-in [m ks &opt default]
  (default default nil)
  (def ks (vview ks))
  # Walk with a fresh sentinel so a PRESENT key whose value is nil is distinguished
  # from a missing key: only a genuinely-absent step falls back to default.
  (def absent @{})
  (var current m)
  (var i 0)
  (var missing false)
  (while (< i (length ks))
    (let [nxt (core-get current (ks i) absent)]
      (if (= nxt absent) (do (set missing true) (break)) (set current nxt)))
    (++ i))
  (if missing default current))

(defn core-contains? [coll key]
  (if (core-sorted-map? coll) (not (nil? (get (coll :map) key)))
  (if (core-sorted-set? coll) (ss-contains? coll key)
  (if (core-transient? coll)
    (case (coll :kind)
      :vector (and (number? key) (>= key 0) (< key (length (coll :arr))))
      (not (nil? (get (coll :tbl) (canon-key key)))))
  (if (set? coll) (phs-contains? coll key)
    (if (phm? coll) (let [b (get (coll :buckets) (phm-hash-key key))] (if b (phm-bucket-contains? b key) false))
      (if (pvec? coll) (and (number? key) (>= key 0) (< key (pv-count coll)))
      (if (struct? coll) (not (nil? (coll key)))
        (if (table? coll) (not (nil? (coll key)))
          (if (or (tuple? coll) (array? coll))
            (and (number? key) (>= key 0) (< key (length coll)))
            false))))))))))

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
    (core-sorted-map? coll) (length (keys (coll :map)))
    (core-sorted-set? coll) (length (coll :items))
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
    (core-sorted-map? coll) (let [e (sorted-map-entries coll)] (if (empty? e) nil (in e 0)))
    (core-sorted-set? coll) (let [i (coll :items)] (if (empty? i) nil (in i 0)))
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
    (pvec? coll) (let [a (pv->array coll)] (if (<= (length a) 1) @[] (array/slice a 1)))
    (or (nil? coll) (= 0 (length coll))) @[]
    (string? coll) (tuple ;(map make-char (string/bytes (string/slice coll 1))))
    (tuple? coll) (tuple/slice coll 1)
    (array/slice coll 1)))

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
    (core-sorted-map? coll) (let [e (sorted-map-entries coll)] (if (empty? e) nil (tuple ;e)))
    (core-sorted-set? coll) (let [i (coll :items)] (if (empty? i) nil (tuple ;i)))
    (or (nil? coll) (and (or (tuple? coll) (array? coll)) (= 0 (length coll)))) nil
    # Cell-based emptiness, NOT (nil? (ls-first)): a lazy-seq whose first element
    # is legitimately nil is non-empty, so (seq (cons nil ...)) must not be nil.
    (lazy-seq? coll) (let [cell (realize-ls coll)]
                       (if (or (nil? cell) (= :jolt/pending cell) (= 0 (length cell))) nil coll))
    (pvec? coll) (if (= 0 (pv-count coll)) nil (tuple ;(pv->array coll)))
    (plist? coll) (if (pl-empty? coll) nil (tuple ;(pl->array coll)))
    (buffer? coll) (if (= 0 (length coll)) nil (let [a @[]] (each x coll (array/push a x)) (tuple ;a)))
    (set? coll) (phs-seq coll)
    (phm? coll) (tuple ;(phm-entries coll))
    (tuple? coll) (tuple/slice coll)
    (string? coll) (if (= 0 (length coll)) nil (tuple ;(map make-char (string/bytes coll))))
    (struct? coll) (tuple ;(map (fn [k] (tuple k (get coll k))) (keys coll)))
    (array? coll) (tuple ;coll)
    (and (table? coll) (get coll :jolt/deftype)) coll
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
    to))

(defn core-merge [& maps]
  # Clojure: (when (some identity maps) (reduce conj (or (first maps) {}) (rest maps)))
  # - (merge) and (merge nil nil) -> nil; nil args elsewhere are no-ops.
  # - later args follow conj semantics (a map merges its entries; a [k v]
  #   vector/map-entry adds that entry).
  (var any false)
  (each m maps (when (not (nil? m)) (set any true)))
  (if (not any)
    nil
    (do
      (var result (let [f (in maps 0)] (if (nil? f) (struct) f)))
      (var i 1)
      (while (< i (length maps))
        (let [m (in maps i)]
          (cond
            (nil? m) nil
            (or (phm? m) (struct? m))
              (each e (map-entries-of m)
                (set result (core-assoc result (in e 0) (in e 1))))
            # a [k v] pair (map-entry / 2-vector), per conj
            (and (or (pvec? m) (tuple? m) (array? m))
                 (= 2 (if (pvec? m) (pv-count m) (length m))))
              (set result (core-assoc result (vnth m 0) (vnth m 1)))
            # scalars, sets, and wrong-length sequentials can't merge into a map
            # (a length-2 vector was handled above; anything else here is bad)
            (or (number? m) (string? m) (buffer? m) (keyword? m) (boolean? m)
                (set? m) (plist? m) (pvec? m) (tuple? m) (array? m)
                (and (struct? m) (get m :jolt/type)))
              (error (string "Can't merge " (type m) " into a map"))
            # other map-like tables (records, sorted-maps, host tables): lenient conj
            (set result (core-conj result m))))
        (++ i))
      result)))

(defn core-merge-with [f & maps]
  # Presence — not nil-of-value — decides whether to combine: a key present in the
  # accumulator with a nil value still triggers (f existing v), matching Clojure.
  (if (= 0 (length maps))
    nil
    (do
      (var result (first maps))
      (var mi 1)
      (while (< mi (length maps))
        (let [m (maps mi)]
          (when m
            (each e (map-entries-of m)
              (let [k (in e 0) v (in e 1)]
                (set result
                  (if (core-contains? result k)
                    (core-assoc result k (f (core-get result k) v))
                    (core-assoc result k v)))))))
        (++ mi))
      result)))

(defn core-keys [m]
  # phm-entries (not phm-to-struct) so keys mapped to nil values are not dropped.
  (if (core-sorted-map? m) (tuple ;(sorted-map-keys m))
  (if (phm? m) (tuple ;(map |(in $ 0) (phm-entries m))) (tuple ;(keys m)))))

(defn core-vals [m]
  (if (core-sorted-map? m) (tuple ;(map |(in $ 1) (sorted-map-entries m)))
  (if (phm? m) (tuple ;(map |(in $ 1) (phm-entries m))) (tuple ;(map |(m $) (keys m))))))

(defn core-select-keys [m ks]
  # Include a key when it is PRESENT (contains?), even if its value is nil — a
  # struct/table would drop a nil value, so collect entries and build via kvs->map.
  (def kvs @[])
  (each k (realize-for-iteration ks)
    (when (core-contains? m k)
      (array/push kvs k) (array/push kvs (core-get m k))))
  (kvs->map kvs))

(defn core-zipmap [ks vs]
  (let [ks (realize-for-iteration ks) vs (realize-for-iteration vs)]
    # collect pairs, then build once — a nil key/value must survive (kvs->map -> phm)
    (def kvs @[])
    (var i 0)
    (while (and (< i (length ks)) (< i (length vs)))
      (array/push kvs (in ks i)) (array/push kvs (in vs i))
      (++ i))
    (kvs->map kvs)))

# ============================================================
# Transducers
# ============================================================
# A transducer is (fn [rf] rf') where rf' is a reducing fn with arities
# []=init, [acc]=complete, [acc x]=step. map/filter/take/... return a
# transducer when called with no collection.

(defn core-reduced [x] @{:jolt/type :jolt/reduced :val x})
(defn core-reduced? [x] (and (table? x) (= :jolt/reduced (x :jolt/type))))
(defn core-unreduced [x] (if (core-reduced? x) (x :val) x))
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
                  (if (nil? rt) (set go false) (set cur (make-lazy-seq rt))))))))))
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
          (if (= 0 (length c)) nil
            @[(in c 0) (fn [] (coll->cells (tuple/slice c 1)))])
        (if (array? c)
          # mutable array: a genuine cons cell, or an eager seq result.
          (if (= 0 (length c)) nil
            (if (and (= 2 (length c)) (function? (in c 1)))
              c  # already a cell [val, rest-thunk]
              @[(in c 0) (fn [] (coll->cells (array/slice c 1)))]))
          # Other concrete seqables (set/map/string/buffer): coerce to a tuple
          # seq via core-seq, then recurse. (lazy/indexed handled above.)
          (if (or (set? c) (phm? c) (buffer? c) (string? c)
                  (and (struct? c) (nil? (get c :jolt/type))))
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
                    (reduce-with-reduced f (in cell 0) (make-lazy-seq rt))))))
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

(defn core-reverse [coll]
  (if (nil? coll) @[]
  (if (lazy-seq? coll)
    (do
      (var result @[])
      (var cur coll)
      # seq-done?, not (nil? (ls-first)): a nil element must not end the walk.
      (while (not (seq-done? cur))
        (array/push result (core-first cur))
        (set cur (core-rest cur)))
      (var reversed @[])
      (var i (dec (length result)))
      (while (>= i 0)
        (array/push reversed (in result i))
        (-- i))
      reversed)
    (let [c (realize-for-iteration coll)]
      (var result @[])
      (var i (dec (length c)))
      (while (>= i 0)
        (array/push result (in c i))
        (-- i))
      result))))

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
(defn core-sort-by [keyfn & rest]
  (def keyfn (as-fn keyfn))
  (let [has-cmp (> (length rest) 1)
        coll (if has-cmp (in rest 1) (first rest))]
    (if (nil? coll) (tuple)
      (let [c (realize-for-iteration coll)
            arr (if (tuple? c) (array/slice c) (array/slice c))]
        (if has-cmp
          (let [cmp (first rest)]
            (sort arr (fn [x y] (let [r (cmp (keyfn x) (keyfn y))]
                                  (if (number? r) (< r 0) (truthy? r))))))
          (sort-by keyfn arr))
        (tuple/slice (tuple ;arr))))))

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
(defn core-pop [coll]
  (cond
    (nil? coll) nil
    (plist? coll) (if (pl-empty? coll) (error "Can't pop empty list") (pl-rest coll))
    (pvec? coll) (if (= 0 (pv-count coll)) (error "Can't pop empty vector") (pv-pop coll))
    (tuple? coll) (if (= 0 (length coll)) (error "Can't pop empty vector") (tuple/slice coll 0 (- (length coll) 1)))
    (array? coll) (if (= 0 (length coll)) (error "Can't pop empty list") (array/slice coll 1))
    (error (string "pop not supported on " (type coll)))))

# subvec lives in the Clojure kernel tier — core/00-kernel.clj.

(defn core-trampoline [f & args]
  (var result (apply f args))
  (while (function? result) (set result (result)))
  result)

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

(def core-identity (fn [x] x))

(def core-constantly (fn [x] (fn [& _] x)))

(defn core-complement [f]
  (fn [& args] (not (apply f args))))

(defn core-qualified-symbol? [x]
  "Returns true if x is a symbol with a namespace."
  (and (struct? x) (= :symbol (x :jolt/type)) (not (nil? (x :ns)))))

(defn core-simple-symbol? [x]
  (and (struct? x) (= :symbol (x :jolt/type)) (nil? (x :ns))))
(defn core-qualified-keyword? [x]
  (and (keyword? x) (not (nil? (string/find "/" (string x))))))
(defn core-simple-keyword? [x]
  (and (keyword? x) (nil? (string/find "/" (string x)))))
# Jolt has no inst/uri/uuid host types, so these are always false; inst-ms has
# nothing valid to read.
(defn core-inst? [x] false)
(defn core-inst-ms [x] (error "Not an instant (no inst type in Jolt)"))
(defn core-uri? [x] false)
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

(def core-comp
  (fn [& fs]
    (case (length fs)
      0 identity
      1 (fs 0)
      2 (let [f (fs 0) g (fs 1)] (fn [& args] (f (apply g args))))
      (let [f (last fs)
            gs (array/slice fs 0 (dec (length fs)))]
        (fn [& args]
          (var result (apply (last gs) args))
          (var i (- (length gs) 2))
          (while (>= i 0)
            (set result ((gs i) result))
            (-- i))
          (f result))))))

(defn core-partial [f & args]
  (fn [& more] (apply f (array/concat (array/slice args) more))))

# juxt now lives in the Clojure collection tier (core/20-coll.clj).

(defn core-memoize [f]
  (var cache @{})
  (fn [& args]
    (let [key (tuple ;args)]
      (if-let [v (get cache key)]
        v
        (let [result (apply f args)]
          (put cache key result)
          result)))))

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

(defn core-set? [x] (set? x))
(defn core-disj [s & ks]
  (cond
    (core-sorted-set? s) (ss-disj-many s ks)
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

(set pr-render
  (fn [buf v]
    (cond
      (nil? v) (buffer/push-string buf "nil")
      (= true v) (buffer/push-string buf "true")
      (= false v) (buffer/push-string buf "false")
      (string? v) (do (buffer/push-string buf "\"") (buffer/push-string buf v) (buffer/push-string buf "\""))
      (buffer? v) (do (buffer/push-string buf "\"") (buffer/push-string buf (string v)) (buffer/push-string buf "\""))
      (keyword? v) (do (buffer/push-string buf ":") (buffer/push-string buf (string v)))
      (core-char? v) (do (buffer/push-string buf "\\")
                         (buffer/push-string buf
                           (case (v :ch)
                             10 "newline" 32 "space" 9 "tab" 13 "return"
                             12 "formfeed" 8 "backspace" 0 "nul"
                             (char->string v))))
      (regex? v) (do (buffer/push-string buf "#\"") (buffer/push-string buf (v :source)) (buffer/push-string buf "\""))
      (number? v) (buffer/push-string buf (fmt-number v))
      (and (struct? v) (= :symbol (v :jolt/type)))
        (buffer/push-string buf (if (v :ns) (string (v :ns) "/" (v :name)) (v :name)))
      (and (struct? v) (= :jolt/uuid (v :jolt/type)))
        (do (buffer/push-string buf "#uuid \"") (buffer/push-string buf (v :str))
            (buffer/push-string buf "\""))
      (and (table? v) (= :jolt/var (get v :jolt/type))) (buffer/push-string buf (var-display v))
      (core-sorted-map? v) (pr-render-pairs buf (sorted-map-entries v))
      (core-sorted-set? v) (pr-render-seq buf (v :items) "#{" "}")
      (lazy-seq? v) (pr-render-seq buf (realize-for-iteration v) "(" ")")
      (set? v) (pr-render-seq buf (phs-seq v) "#{" "}")
      (phm? v) (pr-render-pairs buf (phm-entries v))
      (core-transient? v) (buffer/push-string buf (string "#<transient " (v :kind) ">"))
      (and (table? v) (= :jolt/chan (get v :jolt/type))) (buffer/push-string buf "#<channel>")
      (pvec? v) (pr-render-seq buf (pv->array v) "[" "]")
      (plist? v) (pr-render-seq buf (pl->array v) "(" ")")
      (and (table? v) (get v :jolt/deftype)) (buffer/push-string buf (string v))
      (tuple? v) (pr-render-seq buf v "[" "]")
      # mutable mode: arrays are vectors -> print with [] (else lists -> ())
      (array? v) (if mutable? (pr-render-seq buf v "[" "]") (pr-render-seq buf v "(" ")"))
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
(defn core-print [& xs]
  (var i 0)
  (while (< i (length xs))
    (if (> i 0) (prin " "))
    (prin (str-render-one (xs i)))
    (++ i))
  nil)

(defn core-println [& xs]
  (apply core-print xs)
  (prin "\n")
  nil)

(defn core-newline [] (prin "\n") nil)

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

(defn core-parse-boolean [s]
  (def str* (parse-arg-str s "parse-boolean"))
  (case str* "true" true "false" false nil))

# Host time source for the `time` macro (monotonic, milliseconds).
(defn core-current-time-ms [] (* 1000 (os/clock :monotonic)))

# Capture *out*: run thunk with Janet's :out dynamic bound to a buffer, so all
# print/println/pr/prn output (which go through `prin` -> (dyn :out)) is collected
# and returned as a string. The with-out-str macro (overlay) wraps a body thunk.
(defn core-with-out-str [thunk]
  (def buf @"")
  (with-dyns [:out buf] (thunk))
  (string buf))

(defn core-pr [& xs]
  (var i 0)
  (while (< i (length xs))
    (if (> i 0) (prin " "))
    (let [b @""] (pr-render b (xs i)) (prin (string b)))
    (++ i))
  nil)

(defn core-prn [& xs]
  (apply core-pr xs)
  (prin "\n")
  nil)

(defn core-pr-str [& xs]
  (def buf @"")
  (var i 0)
  (let [n (length xs)]
    (while (< i n)
      (pr-render buf (xs i))
      (when (< (+ i 1) n) (buffer/push-string buf " "))
      (++ i)))
  (string buf))

# ============================================================
# Java-style arrays — backed by Janet's C primitives. Byte arrays use Janet
# buffers (contiguous, O(1) indexed get/put — genuinely fast); object and
# numeric arrays use Janet arrays. aget/aset/alength/aclone work over both.
# ============================================================

# alength / aget / aset now live in the Clojure collection tier — count/nth reads
# and an aset write through jolt.host/ref-put!. The typed/object array constructors
# below stay native (they build the mutable backing).

(defn core-aclone [arr]
  (if (buffer? arr) (buffer/slice arr) (array/slice arr)))

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
(defn core-char-array [a & rest] (make-num-array a rest (make-char 0)))
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
(defn core-to-array-2d [coll]
  (def arr @[]) (each row (realize-for-iteration coll) (array/push arr (core-to-array row))) arr)

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
(defn core-unchecked-byte [x] (band (math/trunc x) 0xff))
(defn core-unchecked-short [x] (band (math/trunc x) 0xffff))
(defn core-unchecked-char [x] (band (math/trunc x) 0xffff))
(defn core-unchecked-float [x] (* 1.0 x))
(defn core-unchecked-double [x] (* 1.0 x))

# 64-bit integers (Janet int/s64 — C-backed)
(defn core-bigint [x] (int/s64 x))
(defn core-biginteger [x] (int/s64 x))
(defn core-bigdec [x] (* 1.0 x))   # no BigDecimal; use a double

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
(defn core-random-sample [prob & rest]
  (if (= 0 (length rest))
    (core-filter (fn [_] (< (math/random) prob)))
    (core-filter (fn [_] (< (math/random) prob)) (in rest 0))))
(defn core-reader-conditional [form splicing?]
  @{:jolt/type :jolt/reader-conditional :form form :splicing? splicing?})
# reader-conditional? now lives in the Clojure collection tier (tagged-value predicate).
# The user comparator is a Clojure IFn; wrap it as a Janet 2-arg fn returning the
# numeric compare result, then thread it through the sorted wrapper.
(defn core-sorted-map-by [cmp & kvs]
  (let [jc (fn [a b] (jolt-call cmp a b))]
    (var m @{}) (var i 0)
    (while (< i (length kvs)) (put m (kvs i) (kvs (+ i 1))) (+= i 2))
    (sm-make (table/to-struct m) jc)))
(defn core-sorted-set-by [cmp & xs]
  (let [jc (fn [a b] (jolt-call cmp a b))]
    (var seen @{}) (each x xs (put seen x true))
    (ss-make (sorted-by jc (array ;(keys seen))) jc)))
(defn core-array-seq [arr & _] (core-seq arr))
(defn core-seque [& args] (in args (- (length args) 1)))
(defn core-supers [x] (make-phs))
(defn core-class [x]
  (cond
    (nil? x) nil (number? x) "java.lang.Number" (string? x) "java.lang.String"
    (boolean? x) "java.lang.Boolean" (keyword? x) "clojure.lang.Keyword"
    (function? x) "clojure.lang.IFn" (buffer? x) "[B"
    (string (type x))))
(defn core-clojure-version [] "1.11.0-jolt")
(defn core-munge [s]
  (string/replace-all "-" "_" (string s)))
(defn core-test [v]
  (let [t (and (core-meta v) (get (core-meta v) :test))]
    (if t (do (t) :ok) :no-test)))


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
(def core-num (fn [x] (if (number? x) x (error (string "num requires a number, got " (type x))))))
(defn core-char [x]
  "(char code-or-char) -> a character value."
  (cond
    (core-char? x) x
    (number? x) (make-char (math/trunc x))
    (string? x) (make-char (in x 0))
    (error "char expects a number or character")))
(def core-unchecked-inc (fn [x] (+ x 1)))
(def core-unchecked-dec (fn [x] (- x 1)))
(def core-unchecked-add (fn [& xs] (+ ;xs)))
(def core-unchecked-subtract (fn [& xs] (- ;xs)))

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

# Hierarchy stubs for sci bootstrap
(def core-make-hierarchy make-hierarchy)
(defn core-derive
  [& args]
  (case (length args)
    2 (let [[tag parent] args] (derive* the-global-hierarchy tag parent) nil)
    3 (let [[h tag parent] args] (derive* h tag parent))))
(defn core-isa?
  [& args]
  (case (length args)
    2 (let [[child parent] args] (isa? the-global-hierarchy child parent))
    3 (let [[h child parent] args] (isa? h child parent))))
(defn core-ancestors
  [& args]
  (case (length args)
    1 (apply make-phs (ancestors the-global-hierarchy (in args 0)))
    2 (let [[h tag] args] (apply make-phs (ancestors h tag)))))
(defn core-descendants
  [& args]
  (case (length args)
    1 (apply make-phs (descendants the-global-hierarchy (in args 0)))
    2 (let [[h tag] args] (apply make-phs (descendants h tag)))))
(defn core-parents
  [& args]
  (let [[h tag] (if (= 1 (length args)) [the-global-hierarchy (in args 0)] args)
        p (get (h :parents) tag)]
    (if p (make-phs p) (make-phs))))
(defn core-underive [& args]
  (case (length args)
    2 (let [[tag parent] args] (underive the-global-hierarchy tag parent) nil)
    3 (let [[h tag parent] args] (underive h tag parent))))
(def core-get-method (fn [mm-var dispatch-val]
  (let [methods (get mm-var :jolt/methods)]
    (or (get methods dispatch-val) (get methods :default)))))
(def core-methods (fn [mm-var] (get mm-var :jolt/methods)))
(def core-remove-method (fn [mm-var dispatch-val]
  (let [methods (get mm-var :jolt/methods)]
    (put methods dispatch-val nil) mm-var)))
(def core-remove-all-methods (fn [mm-var]
  (put mm-var :jolt/methods @{}) mm-var))
(defn core-prefer-method [mm-var dispatch-val-a dispatch-val-b]
  (let [prefs (or (get mm-var :jolt/prefers)
                 (do (put mm-var :jolt/prefers @{}) (mm-var :jolt/prefers)))]
    (put prefs dispatch-val-a dispatch-val-b) mm-var))

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
(defn core-force [x]
  (if (core-delay? x)
    (if (x :realized) (x :val)
      (let [v ((x :fn))] (put x :val v) (put x :realized true) v))
    x))
(defn core-realized? [x]
  (cond
    (core-delay? x) (x :realized)
    (core-future? x) (truthy? (x :cached))
    (lazy-seq? x) (truthy? (x :realized))
    (and (table? x) (= :jolt/atom (x :jolt/type))) true
    # Clojure's realized? is only defined on IPending; reject anything else.
    (error (string "realized? not supported on " (type x)))))


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

(defn core-assoc-in [m ks v]
  (let [ks (vview ks) k (in ks 0)]
    (if (<= (length ks) 1)
      (core-assoc m k v)
      (let [sub (core-get m k)]
        (core-assoc m k (core-assoc-in (if (nil? sub) {} sub) (ks-rest ks) v))))))

(defn core-update-in [m ks f & args]
  (let [ks (vview ks) k (in ks 0)]
    (if (<= (length ks) 1)
      (core-assoc m k (apply f (core-get m k) args))
      (let [sub (core-get m k)]
        (core-assoc m k (apply core-update-in (if (nil? sub) {} sub) (ks-rest ks) f args))))))

(defn core-fnil [f & defaults]
  (fn [& args]
    (def new-args (array/slice args))
    (var i 0)
    (each d defaults
      (when (and (< i (length new-args)) (nil? (in new-args i)))
        (put new-args i d))
      (++ i))
    (apply f new-args)))

# copy-var stubs for sci.impl.copy-vars (used by sci.impl.namespaces)
(defn core-copy-core-var [sym] nil)
(defn core-copy-var [sym & args] nil)
(defn core-macrofy [sym fn & more] fn)
(defn core-new-var [sym & args] nil)
(defn core-avoid-method-too-large [& args] @{})

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

(def core-extends? (fn [& args] false))
(def core-implements? (fn [& args] false))
(def core-type->str (fn [& args] ""))

# ============================================================
# Additional clojure.core functions (conformance batch)
# ============================================================

(defn core-find [m k]
  (cond
    (phm? m) (if (phm-contains? m k) [k (phm-get m k)] nil)
    (or (struct? m) (table? m)) (let [v (get m k :jolt/nf)] (if (= v :jolt/nf) nil [k v]))
    nil))

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

(defn- td-take-nth [n]
  (fn [rf]
    (var i 0)
    (fn [& a] (case (length a) 0 (rf) 1 (rf (a 0))
                (let [keep (= 0 (mod i n))] (++ i)
                  (if keep (rf (a 0) (a 1)) (a 0)))))))
(defn core-take-nth [n & rest]
  (if (= 0 (length rest)) (td-take-nth n)
    (let [coll (in rest 0)]
      # Option A: always lazy.
      (defn tstep [c]
        (fn []
          (if (seq-done? c) nil
            (let [drop-n (lazy-from (core-drop n c))]
              (if (seq-done? drop-n) @[(core-first c) nil]
                @[(core-first c) (tstep drop-n)])))))
      (make-lazy-seq (tstep (lazy-from coll))))))

# filterv now lives in the Clojure collection tier (core/20-coll.clj).

# mapv lives in the Clojure kernel tier — core/00-kernel.clj.

(defn- td-interpose [sep]
  (fn [rf]
    (var started false)
    (fn [& a] (case (length a) 0 (rf) 1 (rf (a 0))
                (if started (rf (rf (a 0) sep) (a 1))
                  (do (set started true) (rf (a 0) (a 1))))))))
(defn core-interpose [sep & rest]
  (if (= 0 (length rest)) (td-interpose sep)
    (let [coll (in rest 0)]
      # Option A: always lazy.
      (defn istep [c need-sep]
        (fn []
          (if (seq-done? c) nil
            (if need-sep
              @[sep (istep c false)]
              @[(core-first c) (istep (core-rest c) true)]))))
      (make-lazy-seq (istep (lazy-from coll) false)))))

# keep now lives in the Clojure lazy tier (core/40-lazy.clj).

(defn core-empty [coll]
  (cond
    (phm? coll) (make-phm)
    (set? coll) (make-phs)
    (plist? coll) EMPTY-PLIST
    (pvec? coll) (make-vec @[])
    (struct? coll) (struct)
    (tuple? coll) (make-vec @[])
    (array? coll) @[]
    (table? coll) @{}
    nil))

# not-empty now lives in the Clojure collection tier (core/20-coll.clj).

# rseq is defined only on vectors and sorted collections (Reversible).
(defn core-rseq [coll]
  (cond
    (pvec? coll) (tuple/slice (tuple ;(reverse (pv->array coll))))
    (core-sorted-map? coll) (tuple/slice (tuple ;(reverse (sorted-map-entries coll))))
    (core-sorted-set? coll) (tuple/slice (tuple ;(reverse (coll :items))))
    (error (string "rseq requires a vector or sorted collection, got " (type coll)))))

(defn core-shuffle [coll]
  (when (not (core-coll? coll)) (error (string "shuffle requires a collection, got " (type coll))))
  (let [c (array/slice (realize-for-iteration coll))]
    (var i (- (length c) 1))
    (while (> i 0)
      (let [j (math/floor (* (math/random) (+ i 1)))
            tmp (in c i)]
        (put c i (in c j)) (put c j tmp))
      (-- i))
    (tuple/slice (tuple ;c))))

# some-fn now lives in the Clojure collection tier (core/20-coll.clj).

(defn core-sequential? [x] (or (tuple? x) (array? x) (pvec? x) (plist? x) (lazy-seq? x)))
# Associative = maps and (real) vectors only. pvec is a literal/built vector;
# tuples and lists are seq results, not associative.
(defn core-associative? [x]
  (or (pvec? x) (phm? x) (core-sorted-map? x)
      (and (struct? x) (nil? (get x :jolt/type)))))
(defn core-ifn? [x]
  (or (function? x) (cfunction? x) (keyword? x) (phm? x) (set? x) (tuple? x) (array? x) (pvec? x)
      (and (struct? x) (= :symbol (x :jolt/type)))))
(defn core-indexed? [x] (or (tuple? x) (array? x) (pvec? x)))


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

(defn core-prn-str [& xs] (string (apply core-pr-str xs) "\n"))
(defn core-println-str [& xs]
  (var parts @[]) (each x xs (array/push parts (str-render-one x)))
  (string (string/join parts " ") "\n"))

# Iterator/enumeration seqs — Jolt has no Java iterators, so adapt to plain seq.
(defn core-enumeration-seq [x] (core-seq x))
(defn core-iterator-seq [x] (core-seq x))
# xml-seq now lives in the Clojure collection tier (core/20-coll.clj).
(defn core-line-seq [rdr]
  (if (string? rdr) (core-seq (string/split "\n" rdr)) nil))
(defn core-re-matcher [re s] @{:jolt/type :jolt/matcher :re re :s s :pos 0})

# JVM reflection / proxies — not applicable on a Janet host; resolve-only.
(defn core-bean [x] (if (core-map? x) x {}))
(defn core-print-method [x writer] nil)
(defn core-print-dup [x writer] nil)
(defn core-proxy-call-with-super [f proxy meth] (f))
(defn core-proxy-mappings [proxy] {})
(defn core-update-proxy [proxy mappings] proxy)
(defn core-numeric= [& args]
  (if (< (length args) 2) true
    (do (var ok true) (var i 0)
      (while (and ok (< i (dec (length args))))
        (unless (= (in args i) (in args (+ i 1))) (set ok false)) (++ i))
      ok)))
(defn core-print-str [& xs]
  (var parts @[]) (each x xs (array/push parts (str-render-one x)))
  (string/join parts " "))
(defn core-memfn [& args] (error "memfn: JVM method handles are not supported in Jolt"))
(defn core-eduction [& args]
  # (eduction xform* coll): apply the composed transducers eagerly to coll
  (let [n (length args)
        coll (in args (- n 1))
        xforms (array/slice args 0 (- n 1))
        xform (if (= 0 (length xforms)) (fn [rf] rf) (apply core-comp xforms))]
    (core-into (make-vec @[]) xform coll)))
(defn core->Eduction [xform coll] (core-into (make-vec @[]) xform coll))
(defn core-proxy-super [& args] (error "proxy-super: JVM proxies are not supported in Jolt"))
(defn core-construct-proxy [c & args] (error "construct-proxy: not supported in Jolt"))
(defn core-init-proxy [proxy mappings] proxy)
(defn core-get-proxy-class [& interfaces] (error "get-proxy-class: not supported in Jolt"))

(def- char-escapes
  {10 "\\n" 9 "\\t" 13 "\\r" 12 "\\f" 8 "\\b" 34 "\\\"" 92 "\\\\"})
(def- char-names
  {10 "newline" 9 "tab" 13 "return" 12 "formfeed" 8 "backspace" 32 "space"})
(defn core-char-escape-string [c]
  (get char-escapes (if (core-char? c) (char-code c) c)))
(defn core-char-name-string [c]
  (get char-names (if (core-char? c) (char-code c) c)))

# subseq / rsubseq over sorted collections
(defn- sorted-entries [sc]
  (cond
    (core-sorted-map? sc) (sorted-map-entries sc)
    (core-sorted-set? sc) (map (fn [x] x) (sc :items))
    (realize-for-iteration sc)))
(defn- sorted-key-of [sc e] (if (core-sorted-map? sc) (in e 0) e))
(defn core-subseq [sc & args]
  (let [es (sorted-entries sc)]
    (tuple ;(filter
      (fn [e] (let [k (sorted-key-of sc e)]
        (if (= 2 (length args))
          (truthy? ((args 0) k (args 1)))
          (and (truthy? ((args 0) k (args 1))) (truthy? ((args 2) k (args 3)))))))
      es))))
(defn core-rsubseq [sc & args]
  (tuple ;(reverse (apply core-subseq sc args))))

# ============================================================
# Additional clojure.core functions
# ============================================================

# Integer-valued: a finite number equal to its floor. Infinity floors to itself
# but is NOT integer-valued (so float?/double? are true for ##Inf, and int?/
# pos-int?/… are false), and NaN is excluded by the equality check.
(defn- intval? [x] (and (number? x) (< (math/abs x) math/inf) (= x (math/floor x))))

# Forcing lazy seqs
(defn core-doall [a & rest]
  (let [coll (if (= 0 (length rest)) a (in rest 0))]
    (realize-for-iteration coll) coll))
(defn core-dorun [a & rest]
  (let [coll (if (= 0 (length rest)) a (in rest 0))]
    (realize-for-iteration coll) nil))

# Map entries (represented as 2-element vectors)
# key/val require a map entry (a 2-element vector/tuple in Jolt); Clojure throws
# otherwise. (Jolt can't distinguish a 2-vector from a real MapEntry.)
# A map entry is a 2-element tuple — Jolt produces tuples only from map
# iteration (first/seq/map over a map), while vector literals are pvecs and
# lists are arrays. So key/val/map-entry? accept a 2-tuple and reject a plain
# vector, matching Clojure (where a MapEntry is distinct from a vector).
(defn- entry-like? [x] (and (tuple? x) (= 2 (length x))))
(defn core-key [e] (if (entry-like? e) (in e 0) (error "key requires a map entry")))
(defn core-val [e] (if (entry-like? e) (in e 1) (error "val requires a map entry")))
(defn core-map-entry? [x] (entry-like? x))

(defn core-rand-nth [coll]
  (let [c (realize-for-iteration coll)]
    (in c (math/floor (* (math/random) (length c))))))

(defn core-counted? [x]
  (or (pvec? x) (plist? x) (phm? x) (set? x) (tuple? x) (array? x) (string? x)))
# Reversible (supports rseq) = vectors and sorted collections.
(defn core-reversible? [x] (or (pvec? x) (core-sorted-map? x) (core-sorted-set? x)))
(defn core-seqable? [x]
  (or (nil? x) (tuple? x) (array? x) (pvec? x) (plist? x) (phm? x) (set? x)
      (struct? x) (lazy-seq? x) (string? x)
      (and (table? x) (or (get x :jolt/type) (get x :jolt/deftype)))))

# Numeric predicates (Jolt has no ratios/bigdec). nat-int?/pos-int?/neg-int?/
# ratio?/decimal?/rational? live in the Clojure collection tier (core/20-coll.clj).
(defn core-double? [x] (and (number? x) (not (intval? x))))
(defn core-float? [x] (and (number? x) (not (intval? x))))
(defn core-infinite? [x] (and (number? x) (= (math/abs x) math/inf)))
# Jolt has no ratio type, so numerator/denominator have no valid input (Clojure
# requires a Ratio and throws otherwise).
(defn core-numerator [x] (error "numerator requires a ratio (Jolt has no ratios)"))
(defn core-denominator [x] (error "denominator requires a ratio (Jolt has no ratios)"))

(defn core-list* [& args]
  (let [n (length args)]
    (if (= 0 n) nil
      (let [head (array/slice args 0 (- n 1))
            tail (realize-for-iteration (in args (- n 1)))]
        (var r (if (array? tail) tail (array ;tail)))
        (var i (- (length head) 1))
        (while (>= i 0) (set r (pl-cons (in head i) r)) (-- i))
        r))))

(def- special-syms
  {"if" true "do" true "let*" true "fn*" true "quote" true "var" true "def" true
   "loop*" true "recur" true "throw" true "try" true "catch" true "finally" true
   "new" true "set!" true "." true "monitor-enter" true "monitor-exit" true})
(defn core-special-symbol? [x]
  (and (core-symbol? x) (= true (get special-syms (x :name)))))

# record? now lives in the Clojure collection tier (tagged-value predicate).

# Promise: single-threaded box backed by an atom (deref returns nil until set).
(defn core-promise [] (core-atom nil))
(defn core-deliver [p v] (core-reset! p v) p)

(defn core-tagged-literal [tag form] @{:jolt/type :jolt/tagged-literal :tag tag :form form})
(defn core-ensure-reduced [x] (if (core-reduced? x) x (core-reduced x)))
(defn core-halt-when [pred & rest]
  (let [retf (if (> (length rest) 0) (in rest 0) nil)]
    (fn [rf]
      (fn [& a]
        (case (length a)
          0 (rf)
          1 (rf (in a 0))
          (if (truthy? (pred (in a 1)))
            (core-reduced (if retf (retf (rf (in a 0)) (in a 1)) (in a 1)))
            (rf (in a 0) (in a 1))))))))
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
(defn core-unchecked-add [a b] (+ a b))
(defn core-unchecked-subtract [a b] (- a b))
(defn core-unchecked-multiply [a b] (* a b))
(defn core-unchecked-negate [a] (- a))
(defn core-unchecked-inc [a] (+ a 1))
(defn core-unchecked-dec [a] (- a 1))
(defn core-unchecked-divide-int [a b] (math/trunc (/ a b)))
(defn core-unchecked-remainder-int [a b] (% a b))
(defn core-unchecked-int [a] (math/trunc a))

# Hashing helpers
# Hashes are masked to 24 bits at each step so intermediate products stay within
# Janet's integer range (a float here would make band error).
(defn- h24 [x] (band (hash x) 0xffffff))
(defn core-hash-combine [a b] (band (bxor (h24 a) (+ (h24 b) 0x9e3779)) 0xffffff))
(defn core-hash-ordered-coll [coll]
  (var h 1) (each x (realize-for-iteration coll) (set h (band (+ (* 31 h) (h24 x)) 0xffffff))) h)
(defn core-hash-unordered-coll [coll]
  (var h 0) (each x (realize-for-iteration coll) (set h (band (+ h (h24 x)) 0xffffff))) h)

(defn core-prefers [mm-var] (or (get mm-var :jolt/prefers) {}))

(defn core-random-uuid
  "A random version-4 UUID value: zero-padded hex groups (8-4-4-4-12), version
  nibble 4, variant nibble 8-b (RFC 4122)."
  []
  (defn hx4 [] (string/format "%04x" (math/floor (* (math/random) 0x10000))))
  (defn hx3 [] (string/format "%03x" (math/floor (* (math/random) 0x1000))))
  (def variant (string/format "%x" (+ 8 (math/floor (* (math/random) 4)))))
  (make-uuid (string (hx4) (hx4) "-" (hx4) "-4" (hx3)
                     "-" variant (hx3) "-" (hx4) (hx4) (hx4))))

(defn- hex-digit? [c]
  (or (and (>= c 48) (<= c 57))      # 0-9
      (and (>= c 97) (<= c 102))     # a-f
      (and (>= c 65) (<= c 70))))    # A-F

(defn core-parse-uuid
  "(parse-uuid s) -> the UUID, or nil when s is not a canonical 8-4-4-4-12 hex
  UUID string. Throws when s is not a string (Clojure 1.11)."
  [s]
  (when (not (or (string? s) (buffer? s)))
    (error (string "parse-uuid requires a string, got " (type s))))
  (def dash-at {8 true 13 true 18 true 23 true})
  (if (and (= 36 (length s))
           (do
             (var ok true)
             (for i 0 36
               (if (get dash-at i)
                 (when (not= 45 (in s i)) (set ok false))    # 45 = "-"
                 (when (not (hex-digit? (in s i))) (set ok false))))
             ok))
    (make-uuid s)
    nil))

(def- core-bindings
  "Map of symbol name → function for all core functions."
  @{"nil?" core-nil?
    "some?" core-some?
    "string?" core-string?
    "number?" core-number?
    "fn?" core-fn?
    "keyword?" core-keyword?
    "symbol?" core-symbol?
    "vector?" core-vector?
    "map?" core-map?
    "seq?" core-seq?
    "coll?" core-coll?
    "true?" core-true?
    "false?" core-false?
    "identical?" core-identical?
    "zero?" core-zero?
    "pos?" core-pos?
    "neg?" core-neg?
    "even?" core-even?
    "odd?" core-odd?
    "integer?" core-integer?
    "boolean?" core-boolean?
    "list?" core-list?
    "empty?" core-empty?
    "every?" core-every?
    "+" core-+
    "-" core-sub
    "*" core-*
    "/" core-/
    "inc" core-inc
    "dec" core-dec
    # auto-promoting variants — Jolt numbers don't overflow, so these are the
    # same as their non-quoted counterparts
    "+'" core-+
    "-'" core-sub
    "*'" core-*
    "inc'" core-inc
    "dec'" core-dec
    "mod" core-mod
    "rem" core-rem
    "quot" core-quot
    "max" core-max
    "min" core-min
    "rand" core-rand
    "rand-int" core-rand-int
    "=" core-=
    "not=" core-not=
    "<" core-<
    ">" core->
    "<=" core-<=
    ">=" core->=
    "conj" core-conj
    "assoc" core-assoc
    "dissoc" core-dissoc
    "get" core-get
    "get-in" core-get-in
    "contains?" core-contains?
    "count" core-count
    "pop" core-pop
    "trampoline" core-trampoline
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
    "merge" core-merge
    "merge-with" core-merge-with
    "keys" core-keys
    "vals" core-vals
    "select-keys" core-select-keys
    "with-meta" core-with-meta
    "zipmap" core-zipmap
    "map" core-map
    "filter" core-filter
    "remove" core-remove
    "reduce" core-reduce
    "apply" core-apply
    "doall" core-doall
    "dorun" core-dorun
    "key" core-key
    "val" core-val
    "map-entry?" core-map-entry?
    "rand-nth" core-rand-nth
    "counted?" core-counted?
    "reversible?" core-reversible?
    "seqable?" core-seqable?
    "double?" core-double?
    "float?" core-float?
    "infinite?" core-infinite?
    "numerator" core-numerator
    "denominator" core-denominator
    "list*" core-list*
    "special-symbol?" core-special-symbol?
    "promise" core-promise
    "deliver" core-deliver
    "future-call" core-future-call
    "future?" core-future?
    "future-cancel" core-future-cancel
    "tagged-literal" core-tagged-literal
    "ensure-reduced" core-ensure-reduced
    "unreduced" core-unreduced
    "halt-when" core-halt-when
    "re-groups" core-re-groups
    "transient" core-transient
    "transient?" core-transient?
    "persistent!" core-persistent!
    "conj!" core-conj!
    "assoc!" core-assoc!
    "dissoc!" core-dissoc!
    "pop!" core-pop!
    "unchecked-add" core-unchecked-add
    "unchecked-add-int" core-unchecked-add
    "unchecked-subtract" core-unchecked-subtract
    "unchecked-subtract-int" core-unchecked-subtract
    "unchecked-multiply" core-unchecked-multiply
    "unchecked-multiply-int" core-unchecked-multiply
    "unchecked-negate" core-unchecked-negate
    "unchecked-negate-int" core-unchecked-negate
    "unchecked-inc" core-unchecked-inc
    "unchecked-inc-int" core-unchecked-inc
    "unchecked-dec" core-unchecked-dec
    "unchecked-dec-int" core-unchecked-dec
    "unchecked-divide-int" core-unchecked-divide-int
    "unchecked-remainder-int" core-unchecked-remainder-int
    "unchecked-int" core-unchecked-int
    "unchecked-long" core-unchecked-int
    "hash-combine" core-hash-combine
    "hash-ordered-coll" core-hash-ordered-coll
    "hash-unordered-coll" core-hash-unordered-coll
    "prefers" core-prefers
    "random-uuid" core-random-uuid
    "parse-long" core-parse-long
    "parse-double" core-parse-double
    "parse-boolean" core-parse-boolean
    "newline" core-newline
    "current-time-ms" core-current-time-ms
    "parse-uuid" core-parse-uuid
    "interpose" core-interpose
    "mapcat" core-mapcat
    "find" core-find
    "transduce" core-transduce
    "sequence" core-sequence
    "eduction" core-sequence
    "unreduced" core-unreduced
    "keyword" core-keyword
    "symbol" core-symbol
    "namespace" core-namespace
    "sorted-map" core-sorted-map
    "sorted-set" core-sorted-set
    "sorted?" core-sorted?
    "reduced" core-reduced
    "reduced?" core-reduced?
    "take-nth" core-take-nth
    "empty" core-empty
    "rseq" core-rseq
    "shuffle" core-shuffle
    "sequential?" core-sequential?
    "associative?" core-associative?
    "ifn?" core-ifn?
    "indexed?" core-indexed?
    "ex-info" core-ex-info
    "prn-str" core-prn-str
    "println-str" core-println-str
    "__with-out-str" core-with-out-str
    "force" core-force
    "realized?" core-realized?
    "delay?" core-delay?
    "make-delay" core-make-delay
    "take" core-take
    "drop" core-drop
    "take-while" core-take-while
    "drop-while" core-drop-while
    "concat" core-concat
    "reverse" core-reverse
    "nth" core-nth
    "sort" core-sort
    "sort-by" core-sort-by
    "partition" core-partition
    "range" core-range
    "identity" core-identity
    "constantly" core-constantly
    "complement" core-complement
    "comp" core-comp
    "partial" core-partial
    "memoize" core-memoize
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
    "print" core-print
    "println" core-println
    "pr" core-pr
    "prn" core-prn
    "pr-str" core-pr-str
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
    "to-array-2d" core-to-array-2d
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
    "unchecked-byte" core-unchecked-byte
    "unchecked-short" core-unchecked-short
    "unchecked-char" core-unchecked-char
    "unchecked-float" core-unchecked-float
    "unchecked-double" core-unchecked-double
    "bigint" core-bigint
    "biginteger" core-biginteger
    "bigdec" core-bigdec
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
    "random-sample" core-random-sample
    "reader-conditional" core-reader-conditional
    "sorted-map-by" core-sorted-map-by
    "sorted-set-by" core-sorted-set-by
    "array-seq" core-array-seq
    "seque" core-seque
    "supers" core-supers
    "class" core-class
    "clojure-version" core-clojure-version
    "munge" core-munge
    "test" core-test
    "enumeration-seq" core-enumeration-seq
    "iterator-seq" core-iterator-seq
    "line-seq" core-line-seq
    "re-matcher" core-re-matcher
    "bean" core-bean
    "print-method" core-print-method
    "print-dup" core-print-dup
    "proxy-call-with-super" core-proxy-call-with-super
    "proxy-mappings" core-proxy-mappings
    "update-proxy" core-update-proxy
    "==" core-numeric=
    "print-str" core-print-str
    "memfn" core-memfn
    "eduction" core-eduction
    "->Eduction" core->Eduction
    "proxy-super" core-proxy-super
    "construct-proxy" core-construct-proxy
    "init-proxy" core-init-proxy
    "get-proxy-class" core-get-proxy-class
    "char-escape-string" core-char-escape-string
    "char-name-string" core-char-name-string
    "subseq" core-subseq
    "rsubseq" core-rsubseq
    # Bit operations
    "bit-and" core-bit-and
    "bit-or" core-bit-or
    "bit-xor" core-bit-xor
    "bit-not" core-bit-not
    "bit-shift-left" core-bit-shift-left
    "bit-shift-right" core-bit-shift-right
    "bit-clear" core-bit-clear
    "bit-set" core-bit-set
    "bit-flip" core-bit-flip
    "bit-test" core-bit-test
    "bit-and-not" core-bit-and-not
    "unsigned-bit-shift-right" core-unsigned-bit-shift-right
    # Integer coercion / unchecked math
    "int" core-int
    "long" core-long
    "double" core-double
    "float" core-float
    "num" core-num
    "char" core-char
    "char?" core-char?
    "unchecked-inc" core-unchecked-inc
    "unchecked-dec" core-unchecked-dec
    "unchecked-add" core-unchecked-add
    "unchecked-subtract" core-unchecked-subtract
    # Hash
    "hash" core-hash
    "atom" core-atom
    "deref" core-deref
    "reset!" core-reset!
    "swap!" core-swap!
    "not" core-not
    "derive" core-derive
    "isa?" core-isa?
    "parents" core-parents
    "ancestors" core-ancestors
    "descendants" core-descendants
    "make-hierarchy" core-make-hierarchy
    "underive" core-underive
    "get-method" core-get-method
    "methods" core-methods
    "remove-method" core-remove-method
    "remove-all-methods" core-remove-all-methods
    "prefer-method" core-prefer-method
    "Object" core-Object
    "make-protocol" core-make-protocol
    "satisfies?" core-satisfies?
    "extends?" core-extends?
    "implements?" core-implements?
    "type->str" core-type->str
    "volatile!" core-volatile!
    "Thread" core-Thread
    "ThreadLocal" core-ThreadLocal
    "IllegalStateException" core-IllegalStateException
    "resolve" core-resolve
    "update-in" core-update-in
    "assoc-in" core-assoc-in
    "fnil" core-fnil
    "copy-core-var" core-copy-core-var
    "copy-var" core-copy-var
    "macrofy" core-macrofy
    "new-var" core-new-var
    "avoid-method-too-large" core-avoid-method-too-large
    "qualified-symbol?" core-qualified-symbol?
    "simple-symbol?" core-simple-symbol?
    "qualified-keyword?" core-qualified-keyword?
    "simple-keyword?" core-simple-keyword?
    "inst?" core-inst?
    "inst-ms" core-inst-ms
    "uri?" core-uri?
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
