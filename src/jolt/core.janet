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

(defn realize-for-iteration [c]
  "Normalize a seqable to a Janet array/tuple for iteration: pvec -> array,
  set -> seq, lazy-seq -> realized array; others pass through. Warning: will
  loop on infinite lazy-seqs. Terminates on the empty cell, not on nil."
  (cond
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
(defn core-map? [x] (or (phm? x) (struct? x) (if (and (table? x) (get x :jolt/deftype)) true false)))
# seq? is true only for actual sequences (lists, lazy-seqs) — NOT vectors, which
# are not ISeq in Clojure. (A Janet array represents a Clojure list/seq result.)
(defn core-seq? [x] (or (array? x) (plist? x) (lazy-seq? x)))
(defn core-coll? [x] (or (array? x) (tuple? x) (pvec? x) (plist? x) (struct? x) (phm? x) (set? x) (lazy-seq? x)))

(defn core-true? [x] (= true x))
(defn core-false? [x] (= false x))
(defn core-identical? [a b] (= a b))

(defn core-zero? [x] (and (number? x) (= x 0)))
(defn core-pos? [x] (and (number? x) (> x 0)))
(defn core-neg? [x] (and (number? x) (< x 0)))
(defn core-even? [n] (= 0 (% n 2)))
(defn core-odd? [n] (not= 0 (% n 2)))

(defn core-integer? [x] (and (number? x) (= x (math/floor x))))
(defn core-boolean? [x] (or (= x true) (= x false)))
(defn core-list? [x] (or (plist? x) (and (array? x) (not (get x :jolt/type)))))

(defn core-empty? [coll]
  (if (nil? coll) true
    (if (set? coll) (= 0 (coll :cnt))
      (if (phm? coll) (= 0 (coll :cnt))
        (if (pvec? coll) (= 0 (pv-count coll))
          (if (plist? coll) (pl-empty? coll)
          (if (lazy-seq? coll) (nil? (ls-first coll))
            (if (struct? coll) (= 0 (length (keys coll)))
              (= 0 (length coll))))))))))

(defn core-every? [pred coll]
  (var result true)
  (each x (realize-for-iteration coll)
    (if (not (pred x)) (do (set result false) (break))))
  result)

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
(def core-quot (fn [n d] (let [q (/ n d)] (if (< q 0) (math/ceil q) (math/floor q)))))
(def core-rem (fn [n d] (- n (* (core-quot n d) d))))
(def core-mod (fn [n d]
  (let [m (core-rem n d)]
    (if (or (= m 0) (= (> n 0) (> d 0))) m (+ m d)))))

(defn core-max [& args] (apply max args))
(defn core-min [& args] (apply min args))

(defn core-abs [x] (if (neg? x) (- 0 x) x))
(defn core-rand [] (math/random))
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
(defn- chain-cmp [op xs]
  (var ok true) (var i 0)
  (while (and ok (< i (dec (length xs))))
    (unless (op (in xs i) (in xs (+ i 1))) (set ok false))
    (++ i))
  ok)
(defn core-< [& xs] (chain-cmp < xs))
(defn core-> [& xs] (chain-cmp > xs))
(defn core-<= [& xs] (chain-cmp <= xs))
(defn core->= [& xs] (chain-cmp >= xs))

# ============================================================
# Collections
# ============================================================

(defn core-conj [coll & xs]
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
            (var i 0)
            (while (< i (length xs))
              (let [pair (xs i)]
                (set result (phm-assoc result (vnth pair 0) (vnth pair 1))))
              (++ i))
            result)
          (do
            (var result coll)
            (var i 0)
            (while (< i (length xs))
              (let [pair (xs i)]
                (set result (merge result {(vnth pair 0) (vnth pair 1)})))
              (++ i))
            result))))))))

(defn core-assoc [m & kvs]
  (cond
    (phm? m)
      (do (var result m) (var i 0) (while (< i (length kvs)) (set result (phm-assoc result (kvs i) (kvs (+ i 1)))) (+= i 2)) result)
    (pvec? m)
      (do (var result m) (var i 0) (while (< i (length kvs)) (set result (pv-assoc result (kvs i) (kvs (+ i 1)))) (+= i 2)) result)
    # vector: assoc by integer index (appending at count is allowed); stays a vector
    (or (tuple? m) (array? m))
      (do (var result (array/slice m)) (var i 0)
        (while (< i (length kvs))
          (let [idx (kvs i) v (kvs (+ i 1))]
            (if (= idx (length result)) (array/push result v) (put result idx v)))
          (+= i 2))
        (if (tuple? m) (tuple/slice (tuple ;result)) result))
    (do (var result @{}) (when m (each k (keys m) (put result k (get m k))))
      (var i 0) (while (< i (length kvs)) (let [k (kvs i) v (kvs (+ i 1))] (put result k v) (+= i 2)))
      (if (struct? m) (table/to-struct result) result))))

(defn core-dissoc [m & ks]
  (if (phm? m)
    (do (var result m) (each k ks (set result (phm-dissoc result k))) result)
    (do (var result @{}) (each k (keys m) (var in-ks false) (each k2 ks (if (deep= k k2) (do (set in-ks true) (break)))) (if (not in-ks) (put result k (m k))))
      (if (struct? m) (table/to-struct result) result))))

(defn core-get [m k &opt default]
  (default default nil)
  (if (nil? m) default
    (if (set? m) (phs-get m k default)
      (if (phm? m) (phm-get m k default)
        (if (pvec? m)
          (if (and (number? k) (>= k 0) (< k (pv-count m))) (pv-nth m k) default)
        (if (or (struct? m) (table? m))
          (let [v (m k)]
            (if (nil? v) default v))
          (if (and (or (tuple? m) (array? m)) (number? k) (>= k 0) (< k (length m)))
            (in m k)
            default)))))))

# Runtime invoke dispatch for COMPILED code (interpreter uses evaluator's
# jolt-invoke). Handles real functions plus Clojure IFn collections.
(defn jolt-call [f & args]
  (cond
    (or (function? f) (cfunction? f)) (apply f args)
    (keyword? f) (core-get (get args 0) f (get args 1))
    (and (struct? f) (= :symbol (f :jolt/type))) (core-get (get args 0) f (get args 1))
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
    (or (struct? f) (and (table? f) (get f :jolt/deftype)))
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
  (var current m)
  (var i 0)
  (while (< i (length ks))
    (if (nil? current) (break))
    (set current (core-get current (ks i)))
    (++ i))
  (if (nil? current) default current))

(defn core-contains? [coll key]
  (if (set? coll) (phs-contains? coll key)
    (if (phm? coll) (let [b (get (coll :buckets) (phm-hash-key key))] (if b (phm-bucket-contains? b key) false))
      (if (pvec? coll) (and (number? key) (>= key 0) (< key (pv-count coll)))
      (if (struct? coll) (not (nil? (coll key)))
        (if (table? coll) (not (nil? (coll key)))
          (if (or (tuple? coll) (array? coll))
            (and (number? key) (>= key 0) (< key (length coll)))
            false)))))))

# Sorted collections — minimal: backed by a struct (map) / sorted array (set),
# ordered by key/element on read. Defined early so seq/count/get can dispatch.
(defn core-sorted-map? [x] (and (table? x) (= :jolt/sorted-map (x :jolt/type))))
(defn core-sorted-set? [x] (and (table? x) (= :jolt/sorted-set (x :jolt/type))))
(defn sm-make [m] @{:jolt/type :jolt/sorted-map :map m})
(defn ss-make [items] @{:jolt/type :jolt/sorted-set :items items})
(defn core-sorted-map [& kvs]
  (var m @{}) (var i 0)
  (while (< i (length kvs)) (put m (kvs i) (kvs (+ i 1))) (+= i 2))
  (sm-make (table/to-struct m)))
(defn core-sorted-set [& xs]
  (var seen @{}) (each x xs (put seen x true))
  (ss-make (sort (array ;(keys seen)))))
(defn sorted-map-keys [sm] (sort (array ;(keys (sm :map)))))
(defn sorted-map-entries [sm] (let [m (sm :map)] (map (fn [k] [k (get m k)]) (sorted-map-keys sm))))

(defn core-count [coll]
  (cond
    (nil? coll) 0
    (core-sorted-map? coll) (length (keys (coll :map)))
    (core-sorted-set? coll) (length (coll :items))
    (lazy-seq? coll) (ls-count coll)
    (pvec? coll) (pv-count coll)
    (plist? coll) (pl-count coll)
    (set? coll) (coll :cnt)
    (phm? coll) (coll :cnt)
    (and (table? coll) (get coll :jolt/deftype)) (- (length (keys coll)) 1)
    (length coll)))

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
    (struct? coll) (let [ks (keys coll)] (if (= 0 (length ks)) nil (tuple (in ks 0) (get coll (in ks 0)))))
    (or (nil? coll) (= 0 (length coll))) nil
    (string? coll) (make-char (in coll 0))
    (in coll 0)))

(defn core-rest [coll]
  (cond
    (lazy-seq? coll) (ls-rest coll)
    (plist? coll) (pl-rest coll)
    (pvec? coll) (let [a (pv->array coll)] (if (<= (length a) 1) @[] (array/slice a 1)))
    (or (nil? coll) (= 0 (length coll))) @[]
    (string? coll) (tuple ;(map make-char (string/bytes (string/slice coll 1))))
    (tuple? coll) (tuple/slice coll 1)
    (array/slice coll 1)))

(defn core-next [coll]
  (let [r (core-rest coll)]
    (if (= 0 (length r)) nil r)))

(defn core-cons [x coll]
  "Prepend x onto coll. For concrete collections this is an O(1) persistent cons
  node; for lazy-seqs it stays a lazy cell so laziness is preserved."
  (cond
    (lazy-seq? coll) @[x (fn [] coll)]
    (or (nil? coll) (plist? coll) (array? coll) (tuple? coll)) (pl-cons x coll)
    (pl-cons x (realize-for-iteration coll))))

(defn core-seq [coll]
  (cond
    (core-sorted-map? coll) (let [e (sorted-map-entries coll)] (if (empty? e) nil (tuple ;e)))
    (core-sorted-set? coll) (let [i (coll :items)] (if (empty? i) nil (tuple ;i)))
    (or (nil? coll) (and (or (tuple? coll) (array? coll)) (= 0 (length coll)))) nil
    (lazy-seq? coll) (ls-seq coll)
    (pvec? coll) (if (= 0 (pv-count coll)) nil (tuple ;(pv->array coll)))
    (plist? coll) (if (pl-empty? coll) nil (tuple ;(pl->array coll)))
    (buffer? coll) (if (= 0 (length coll)) nil (let [a @[]] (each x coll (array/push a x)) (tuple ;a)))
    (set? coll) (phs-seq coll)
    (phm? coll) (tuple ;(phm-entries coll))
    (tuple? coll) (tuple/slice coll)
    (string? coll) (if (= 0 (length coll)) nil (tuple ;(map make-char (string/bytes coll))))
    (struct? coll) (tuple ;(map (fn [k] (tuple k (get coll k))) (keys coll)))
    coll))

(defn core-vec [coll]
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
  (if (phm? (first maps))
    (do (var result (first maps)) (var mi 1) (while (< mi (length maps)) (let [m (maps mi)] (each k (if (phm? m) (keys (phm-to-struct m)) (keys m)) (set result (phm-assoc result k (if (phm? m) (phm-get m k) (m k))))) (++ mi))) result)
    (do (var result (struct)) (each m maps (set result (merge result m))) result)))

(defn core-merge-with [f & maps]
  (if (phm? (first maps))
    (do (var result (first maps)) (var mi 1) (while (< mi (length maps)) (let [m (maps mi)]
      (each k (if (phm? m) (keys (phm-to-struct m)) (keys m)) (let [existing (phm-get result k)
                                   val (if (phm? m) (phm-get m k) (m k))]
        (set result (phm-assoc result k (if (nil? existing) val (f existing val)))))) (++ mi))) result)
    (do (var result @{}) (each m maps (each k (if (phm? m) (keys (phm-to-struct m)) (keys m)) (let [existing (result k)] (put result k (if (nil? existing) (m k) (f existing (m k))))))) (table/to-struct result))))

(defn core-keys [m]
  (if (phm? m) (tuple ;(keys (phm-to-struct m))) (tuple ;(keys m))))

(defn core-vals [m]
  (if (phm? m) (do (def s (phm-to-struct m)) (tuple ;(map |(s $) (keys s)))) (tuple ;(map |(m $) (keys m)))))

(defn core-select-keys [m ks]
  (var result @{})
  (each k (realize-for-iteration ks)
    (let [v (core-get m k)]
      (if (not (nil? v)) (put result k v))))
  (if (struct? m) (table/to-struct result) result))

(defn core-zipmap [ks vs]
  (let [ks (realize-for-iteration ks) vs (realize-for-iteration vs)]
    (var result @{})
    (var i 0)
    (while (and (< i (length ks)) (< i (length vs)))
      (put result (in ks i) (in vs i))
      (++ i))
    (table/to-struct result)))

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
(defn td-keep [f]
  (fn [rf] (fn [& a] (case (length a) 0 (rf) 1 (rf (a 0))
                       (let [v (f (a 1))] (if (nil? v) (a 0) (rf (a 0) v)))))))
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
(defn td-map-indexed [f]
  (fn [rf]
    (var i -1)
    (fn [& a] (case (length a) 0 (rf) 1 (rf (a 0)) (do (++ i) (rf (a 0) (f i (a 1))))))))

(defn- transduce-reduce
  "Reduce coll with reducing fn rf and seed init, honoring `reduced`."
  [rf init coll]
  (var acc init) (var stop false)
  (each x (if (set? coll) (phs-seq coll) (realize-for-iteration coll))
    (when (not stop)
      (set acc (rf acc x))
      (when (core-reduced? acc) (set acc (acc :val)) (set stop true))))
  acc)

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
  "(sequence coll) or (sequence xform coll) — eager here (returns a seq/tuple)."
  [a & rest]
  (if (= 0 (length rest))
    (core-seq a)
    (tuple ;(core-transduce a (fn [& x] (case (length x) 0 @[] 1 (x 0) (do (array/push (x 0) (x 1)) (x 0)))) @[] (in rest 0)))))

(defn- seq-done?
  "True when cursor c (a lazy-seq or a concrete collection) is exhausted.
  Uses cell realization for lazy-seqs so nil elements don't end the seq early."
  [c]
  (if (lazy-seq? c)
    (let [cell (realize-ls c)]
      (or (nil? cell) (= :jolt/pending cell) (= 0 (length cell))))
    (or (nil? c) (= 0 (length c)))))

(defn core-map [f & colls]
  (if (= 0 (length colls))
    (td-map f)   # transducer arity
  (if (= 1 (length colls))
    (let [coll (colls 0)]
      (if (lazy-seq? coll)
        # Lazy input: stay lazy so infinite/self-referential seqs work.
        (do
          (defn mstep [c]
            (fn []
              (if (seq-done? c) nil
                @[(f (core-first c)) (mstep (core-rest c))])))
          (make-lazy-seq (mstep coll)))
        # Concrete collection: eager (preserves tuple/array representation).
        (let [c (if (set? coll) (phs-seq coll) (realize-for-iteration coll))
              result (do (var res @[]) (each x c (array/push res (f x))) res)]
          (if (jvec? coll) (make-vec result) result))))
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
                (let [val (ls-first cur)]
                  (if (nil? val) (do (set ok false) (break))
                    (do (array/push args val)
                        (put next-cs i (ls-rest cur))
                        (put next-idxs i (+ ridx 1))
                        (put next-reals i nil))))
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
  (if (= 0 (length rest)) (td-filter pred)
   (let [coll (in rest 0)]
   (if (lazy-seq? coll)
    # lazy input -> lazy output (supports infinite seqs)
    (do
      (defn fstep [c]
        (fn []
          (var cur c) (var hit nil) (var found false)
          (while (and (not found) (not (seq-done? cur)))
            (let [x (core-first cur)]
              (if (pred x) (do (set hit @[x (core-rest cur)]) (set found true))
                (set cur (core-rest cur)))))
          (if found @[(in hit 0) (fstep (in hit 1))] nil)))
      (make-lazy-seq (fstep coll)))
    (do
      (var result @[])
      (each x (if (set? coll) (phs-seq coll) (realize-for-iteration coll))
        (if (pred x) (array/push result x)))
      (if (jvec? coll) (make-vec result) result))))))

(defn core-remove [pred & rest]
  (if (= 0 (length rest)) (td-remove pred)
    (core-filter (fn [x] (not (pred x))) (in rest 0))))

(def core-reduce
  (fn [& args]
    (case (length args)
      2 (let [f (args 0) coll (args 1)
              coll (if (set? coll) (phs-seq coll) (realize-for-iteration coll))]
          (if (= 0 (length coll))
            (f)
            (do
              (var acc (coll 0))
              (var i 1)
              (while (< i (length coll))
                (set acc (f acc (coll i)))
                (if (core-reduced? acc) (do (set acc (acc :val)) (set i (length coll))) (++ i)))
              acc)))
      3 (let [f (args 0) val (args 1) coll (args 2)
              coll (if (set? coll) (phs-seq coll) (realize-for-iteration coll))]
          (var acc val) (var i 0)
          (while (< i (length coll))
            (set acc (f acc (in coll i)))
            (if (core-reduced? acc) (do (set acc (acc :val)) (set i (length coll))) (++ i)))
          acc)
      (error "Wrong number of args passed to: reduce"))))

(defn core-take [n & rest]
 (if (= 0 (length rest)) (td-take n)
  (let [coll (in rest 0)]
  (if (lazy-seq? coll)
    (do
      (var result @[])
      (var cur coll)
      (var i 0)
      (while (and (< i n) (not (nil? (ls-first cur))))
        (array/push result (ls-first cur))
        (set cur (ls-rest cur))
        (++ i))
      result)
    (let [c (realize-for-iteration coll)]
      (var result @[])
      (var i 0)
      (while (and (< i n) (< i (length c)))
        (array/push result (in c i))
        (++ i))
      (if (jvec? coll) (make-vec result) result))))))

(defn core-drop [n & rest]
 (if (= 0 (length rest)) (td-drop n)
  (let [coll (in rest 0)]
  (if (lazy-seq? coll)
    (do
      (var cur coll)
      (var i 0)
      (while (and (< i n) (ls-first cur))
        (set cur (ls-rest cur))
        (++ i))
      (if (nil? (ls-first cur)) nil cur))
    (let [c (realize-for-iteration coll)
          dropped (array/slice c (min n (length c)))]
      (if (jvec? coll) (make-vec dropped) dropped))))))

(defn core-second [coll] (core-first (core-rest coll)))
(defn core-ffirst [coll] (core-first (core-first coll)))
(defn core-nfirst [coll] (core-next (core-first coll)))
(defn core-fnext [coll] (core-first (core-next coll)))
(defn core-nnext [coll] (core-next (core-next coll)))

(defn core-last [coll]
  (let [c (realize-for-iteration coll)]
    (if (= 0 (length c)) nil (in c (- (length c) 1)))))

(defn core-drop-last [a & rest]
  (let [n (if (= 0 (length rest)) 1 a)
        coll (if (= 0 (length rest)) a (in rest 0))
        c (realize-for-iteration coll)
        end (max 0 (- (length c) n))]
    (tuple ;(array/slice c 0 end))))

(defn core-take-last [n coll]
  (let [c (realize-for-iteration coll)
        start (max 0 (- (length c) n))]
    (if (= 0 (length c)) nil (tuple ;(array/slice c start)))))

(defn core-take-while [pred & rest]
 (if (= 0 (length rest)) (td-take-while pred)
  (let [coll (in rest 0)]
  (if (lazy-seq? coll)
    (do
      (var result @[]) (var cur coll) (var go true)
      (while (and go (not (seq-done? cur)))
        (let [x (core-first cur)]
          (if (pred x) (do (array/push result x) (set cur (core-rest cur)))
            (set go false))))
      result)
    (do
      (var result @[])
      (each x (realize-for-iteration coll) (if (pred x) (array/push result x) (break)))
      (if (jvec? coll) (make-vec result) result))))))

(defn core-drop-while [pred & rest]
 (if (= 0 (length rest)) (td-drop-while pred)
  (let [coll (in rest 0)
        c (realize-for-iteration coll)]
    (var start 0)
    (while (and (< start (length c)) (pred (c start)))
      (++ start))
    (if (tuple? c)
      (tuple/slice c start)
      (array/slice c start)))))

(defn coll->cells [c]
  "Convert a seqable to lazy-seq cell chain: nil or [first, rest-thunk].
  If the value is a function, call it and use the result.
  If the result is already a cell (array of [val, function]), return it directly."
  (if (nil? c) nil
    (if (pvec? c) (coll->cells (pv->array c))
    (if (plist? c) (coll->cells (pl->array c))
    (if (function? c)
      (let [r (c)]
        (if (and (indexed? r) (= 2 (length r)) (function? (in r 1)))
          r
          (coll->cells r)))
      (if (lazy-seq? c)
        (let [cell (realize-ls c)]
          (if (= :jolt/pending cell) nil cell))
        (if (indexed? c)
          (if (= 0 (length c)) nil
            (if (and (= 2 (length c)) (function? (in c 1)))
              c  # already a cell [val, rest-thunk]
              (let [f (in c 0)
                    rest (if (> (length c) 1)
                           (if (tuple? c) (tuple/slice c 1) (array/slice c 1))
                           nil)]
                @[f (fn [] (coll->cells rest))])))
          nil)))))))

(defn core-concat [& colls]
  "Truly lazy concatenation. `step` returns a 0-arg thunk that is only forced
  when the consumer asks for the next cell, so nothing in `colls` is realized at
  construction time. This is essential for self-referential lazy seqs (e.g.
  (def fib (lazy-cat [0 1] (map + (rest fib) fib)))): the later colls must not be
  forced until after the surrounding `def` has bound the var."
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
  (make-lazy-seq (step (if (tuple? colls) (array/slice colls) colls))))

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
    # map, then concat; a non-seqable result counts as a single element (this
    # leniency is what jolt's `for` expansion relies on for :let on the last
    # binding, whose body yields a scalar rather than a seq).
    (let [mapped (realize-for-iteration (core-apply core-map f colls))
          seqs (map (fn [item]
                      (if (or (tuple? item) (array? item) (pvec? item)
                              (lazy-seq? item) (set? item))
                        item (tuple item)))
                    mapped)]
      (core-apply core-concat seqs))))

(defn core-reverse [coll]
  (if (nil? coll) @[]
  (if (lazy-seq? coll)
    (do
      (var result @[])
      (var cur coll)
      (while (not (nil? (ls-first cur)))
        (array/push result (ls-first cur))
        (set cur (ls-rest cur)))
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
  "Return the nth element of a sequential collection."
  [coll idx &opt default]
  (if (plist? coll)
    (let [a (pl->array coll)]
      (if (and (>= idx 0) (< idx (length a))) (in a idx)
        (if (nil? default) (error (string "Index " idx " out of bounds, length: " (length a))) default)))
  (if (pvec? coll)
    (if (and (>= idx 0) (< idx (pv-count coll)))
      (pv-nth coll idx)
      (if (nil? default) (error (string "Index " idx " out of bounds, length: " (pv-count coll))) default))
  (if (lazy-seq? coll)
    (do
      (var cur coll)
      (var i 0)
      (while (and (< i idx) (ls-first cur))
        (set cur (ls-rest cur))
        (++ i))
      (if (ls-first cur) (ls-first cur)
        (if (nil? default)
          (error (string "Index " idx " out of bounds"))
          default)))
    (do
      (var c (realize-for-iteration coll))
      (if (and (>= idx 0) (< idx (length c)))
        (if (string? c) (make-char (in c idx)) (in c idx))
        (if (nil? default)
          (error (string "Index " idx " out of bounds, length: " (length c)))
          default)))))))

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

(defn core-sort-by [keyfn coll]
  (if (nil? coll) (break @[]))
  (var c (realize-for-iteration coll))
  (let [arr (if (tuple? c) (array/slice c) c)
        sorted (sort-by keyfn arr)]
    (if (tuple? c) (tuple/slice (tuple ;sorted)) sorted)))

(defn core-distinct [coll]
  (if (nil? coll) @[]
  (if (lazy-seq? coll)
    (do
      (var seen @{})
      (var result @[])
      (var cur coll)
      (while (not (nil? (ls-first cur)))
        (let [x (ls-first cur)]
          (if (nil? (seen x))
            (do (put seen x true) (array/push result x))))
        (set cur (ls-rest cur)))
      result)
    (do
      (var seen @{})
      (var result @[])
      (each x (realize-for-iteration coll)
        (if (nil? (seen x))
          (do (put seen x true) (array/push result x))))
      (if (jvec? coll) (make-vec result) result)))))

(defn core-group-by [f coll]
  (var result @{})
  (var c (realize-for-iteration coll))
  (each x c
    (let [k (f x)]
      (put result k (array/push (core-get result k @[]) x))))
  result)

(defn core-frequencies [coll]
  (var result @{})
  (each x (realize-for-iteration coll)
    (put result x (+ 1 (get result x 0))))
  (table/to-struct result))

(defn core-partition
  "(partition n coll) or (partition n step coll). Only complete partitions of
  size n are kept (use partition-all to keep the trailing remainder)."
  [n & rest]
  (let [has-step (> (length rest) 1)
        step (if has-step (first rest) n)
        coll (realize-for-iteration (if has-step (in rest 1) (first rest)))]
    (var result @[]) (var i 0)
    (while (<= (+ i n) (length coll))
      (var part @[]) (var j 0)
      (while (< j n) (array/push part (in coll (+ i j))) (++ j))
      (array/push result (tuple/slice (tuple ;part)))
      (+= i step))
    result))

(defn core-partition-by [f coll]
  (var result @[])
  (var part @[])
  (var last-k nil)
  (each x (realize-for-iteration coll)
    (let [k (f x)]
      (if (and last-k (deep= k last-k))
        (array/push part x)
        (do
          (if (> (length part) 0) (array/push result (tuple/slice (tuple ;part))))
          (set part @[x])
          (set last-k k)))))
  (if (> (length part) 0) (array/push result (tuple/slice (tuple ;part))))
  result)

(defn core-partition-all [n coll]
  (let [c (realize-for-iteration coll)]
    (var result @[]) (var i 0)
    (while (< i (length c))
      (var part @[]) (var j 0)
      (while (and (< j n) (< (+ i j) (length c)))
        (array/push part (in c (+ i j))) (++ j))
      (array/push result (tuple/slice (tuple ;part)))
      (+= i n))
    result))

(defn core-reductions
  "(reductions f coll) or (reductions f init coll) -> seq of intermediate accs."
  [f init-or-coll &opt maybe-coll]
  (let [has-init (not (nil? maybe-coll))
        coll (realize-for-iteration (if has-init maybe-coll init-or-coll))
        result @[]]
    (if has-init
      (do (var acc init-or-coll) (array/push result acc)
          (each x coll (set acc (f acc x)) (array/push result acc)))
      (when (> (length coll) 0)
        (var acc (in coll 0)) (array/push result acc)
        (var i 1)
        (while (< i (length coll)) (set acc (f acc (in coll i))) (array/push result acc) (++ i))))
    (tuple/slice (tuple ;result))))

(defn core-dedupe [coll]
  (let [c (realize-for-iteration coll) result @[]]
    (var prev :jolt/none)
    (each x c
      (when (or (= prev :jolt/none) (not (deep= x prev)))
        (array/push result x))
      (set prev x))
    (tuple/slice (tuple ;result))))

(defn core-keep-indexed [f coll]
  (let [c (realize-for-iteration coll) result @[]]
    (var i 0)
    (each x c (let [v (f i x)] (when (not (nil? v)) (array/push result v))) (++ i))
    (tuple/slice (tuple ;result))))

(defn core-map-indexed [f & rest]
  (if (= 0 (length rest)) (td-map-indexed f)
    (let [c (realize-for-iteration (in rest 0)) result @[]]
      (var i 0)
      (each x c (array/push result (f i x)) (++ i))
      (tuple/slice (tuple ;result)))))

(defn core-cycle [coll]
  (let [c (realize-for-iteration coll)]
    (if (= 0 (length c))
      (make-lazy-seq (fn [] nil))
      (do
        (defn cstep [i] (fn [] @[(in c (% i (length c))) (cstep (+ i 1))]))
        (make-lazy-seq (cstep 0))))))

(defn core-reduce-kv [f init m]
  (var acc init)
  (cond
    (phm? m) (each k (keys (phm-to-struct m)) (set acc (f acc k (phm-get m k))))
    (or (struct? m) (table? m)) (each k (keys m) (set acc (f acc k (get m k))))
    (indexed? m) (do (var i 0) (each x m (set acc (f acc i x)) (++ i))))
  acc)

(defn core-peek [coll]
  (cond
    (nil? coll) nil
    (lazy-seq? coll) (ls-first coll)
    (plist? coll) (if (pl-empty? coll) nil (pl-first coll))   # list: first
    (pvec? coll) (if (= 0 (pv-count coll)) nil (pv-nth coll (- (pv-count coll) 1)))  # vector: last
    (= 0 (length coll)) nil
    (tuple? coll) (in coll (- (length coll) 1))   # vector: last
    (array? coll) (in coll 0)                      # list: first
    (in coll 0)))

(defn core-pop [coll]
  (cond
    (nil? coll) nil
    (plist? coll) (pl-rest coll)                              # list: drop first
    (pvec? coll) (pv-pop coll)                                # vector: drop last
    (tuple? coll) (tuple/slice coll 0 (- (length coll) 1))  # vector: drop last
    (array? coll) (array/slice coll 1)                       # list: rest
    coll))

(defn core-subvec [v start &opt end]
  (let [a (vview v)]
    (make-vec (tuple/slice a start (if (nil? end) (length a) end)))))

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

(defn core-repeat
  "(repeat x) -> infinite lazy seq of x; (repeat n x) -> n copies of x."
  [a & rest]
  (if (= 0 (length rest))
    (do (defn rstep [] (fn [] @[a (rstep)])) (make-lazy-seq (rstep)))
    (let [n a x (in rest 0)]
      (var result @[]) (var i 0)
      (while (< i n) (array/push result x) (++ i))
      result)))

(defn core-iterate [f x]
  "Lazy infinite sequence x, (f x), (f (f x)), ..."
  (defn istep [v] (fn [] @[v (istep (f v))]))
  (make-lazy-seq (istep x)))

(defn core-repeatedly
  "(repeatedly f) -> infinite lazy seq of (f) calls; (repeatedly n f) -> n calls."
  [a & rest]
  (if (= 0 (length rest))
    (do (defn rstep [] (fn [] @[(a) (rstep)])) (make-lazy-seq (rstep)))
    (let [n a f (in rest 0)]
      (var result @[]) (var i 0)
      (while (< i n) (array/push result (f)) (++ i))
      result)))

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
(defn core-ident? [x] (or (core-keyword? x) (core-symbol? x)))
(defn core-qualified-ident? [x]
  (or (core-qualified-symbol? x) (core-qualified-keyword? x)))
(defn core-simple-ident? [x]
  (or (core-simple-symbol? x) (core-simple-keyword? x)))
# Jolt has no inst/uri/uuid host types, so these are always false; inst-ms has
# nothing valid to read.
(defn core-inst? [x] false)
(defn core-inst-ms [x] (error "Not an instant (no inst type in Jolt)"))
(defn core-uri? [x] false)
(defn core-uuid? [x] false)
(defn core-bytes? [x] (buffer? x))
(defn core-tagged-literal? [x]
  (and (table? x) (= :jolt/tagged-literal (get x :jolt/type))))

(defn core-meta [x]
  "Returns the metadata of x, or nil."
  (if (var? x) (var-meta x)
    (if (table? x) (or (get x :jolt/meta) (get x :meta)) nil)))

(defn core-every-pred [& preds]
  (fn [& xs]
    (var ok true)
    (each p preds (each x xs (when (not (truthy? (p x))) (set ok false))))
    ok))

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

(defn core-juxt [& fs]
  (fn [& args]
    (tuple ;(map |(apply $ args) fs))))

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
  (if (set? s) (apply phs-disj s ks) (error "disj expects a set")))

(defn core-lazy-seq [& body]
  @[{:jolt/type :symbol :ns nil :name "make-lazy-seq"}
    @[{:jolt/type :symbol :ns nil :name "fn*"} []
      @[{:jolt/type :symbol :ns nil :name "coll->cells"}
        @[{:jolt/type :symbol :ns nil :name "do"} ;body]]]])

(defn core-lazy-cat [& colls]
  "Macro: (lazy-cat & colls) — concatenate lazy sequences, wrapping each coll in lazy-seq.
  concat is now lazy, so no outer make-lazy-seq wrapping is needed."
  (def concat-form @[])
  (array/push concat-form {:jolt/type :symbol :ns nil :name "concat"})
  (each c colls
    (array/push concat-form @[{:jolt/type :symbol :ns nil :name "lazy-seq"} c]))
  concat-form)

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
      (number? v) (buffer/push-string buf (string v))
      (and (struct? v) (= :symbol (v :jolt/type)))
        (buffer/push-string buf (if (v :ns) (string (v :ns) "/" (v :name)) (v :name)))
      (core-sorted-map? v) (pr-render-pairs buf (sorted-map-entries v))
      (core-sorted-set? v) (pr-render-seq buf (v :items) "#{" "}")
      (lazy-seq? v) (pr-render-seq buf (realize-for-iteration v) "(" ")")
      (set? v) (pr-render-seq buf (phs-seq v) "#{" "}")
      (phm? v) (pr-render-pairs buf (phm-entries v))
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
    (number? v) (string v)
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
    (case (length args)
      2 (string/slice (args 0) (args 1))
      3 (string/slice (args 0) (args 1) (args 2))
      (error "Wrong number of args passed to: subs"))))

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

(defn core-alength [arr] (length arr))

(defn core-aget [arr & idxs]
  # multi-dim: aget arr i j ... walks nested arrays
  (var v arr) (each i idxs (set v (in v i))) v)

(defn core-aset [arr & more]
  # (aset arr i v) or (aset arr i j ... v): last arg is the value
  (let [n (length more) val (in more (- n 1))]
    (var target arr) (var k 0)
    (while (< k (- n 2)) (set target (in target (in more k))) (++ k))
    (put target (in more (- n 2)) val) val))

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
(defn core-chunked-seq? [x] false)
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
(defn core-disj! [s & ks] (apply core-disj s ks))
(defn core-rationalize [x] x)
(defn core-random-sample [prob & rest]
  (if (= 0 (length rest))
    (core-filter (fn [_] (< (math/random) prob)))
    (core-filter (fn [_] (< (math/random) prob)) (in rest 0))))
(defn core-reader-conditional [form splicing?]
  @{:jolt/type :jolt/reader-conditional :form form :splicing? splicing?})
(defn core-reader-conditional? [x]
  (and (table? x) (= :jolt/reader-conditional (get x :jolt/type))))
(defn core-sorted-map-by [cmp & kvs] (apply core-sorted-map kvs))
(defn core-sorted-set-by [cmp & xs] (apply core-sorted-set xs))
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
(defn core-namespace-munge [s]
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
(def core-num (fn [x] (if (core-char? x) (x :ch) x)))
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

(defn core-atom? [x]
  (and (table? x) (= :jolt/atom (x :jolt/type))))

(defn core-deref [ref]
  (cond
    (and (table? ref) (= :jolt/atom (ref :jolt/type)))
    (ref :value)
    (and (table? ref) (= :jolt/volatile (ref :jolt/type)))
    (ref :val)
    (and (table? ref) (= :jolt/delay (ref :jolt/type)))
    (if (ref :realized) (ref :val)
      (let [v ((ref :fn))] (put ref :val v) (put ref :realized true) v))
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

(defn core-reset-vals! [atm val]
  (let [old-val (atm :value)]
    (atom-validate atm val)
    (put atm :value val)
    (atom-notify-watches atm old-val val)
    [old-val val]))

(defn core-swap-vals! [atm f & args]
  (var old-val (atm :value))
  (var new-val (apply f old-val args))
  (atom-validate atm new-val)
  (put atm :value new-val)
  (atom-notify-watches atm old-val new-val)
  [old-val new-val])

(defn core-compare-and-set! [atm old-val new-val]
  (if (= old-val (atm :value))
    (do
      (atom-validate atm new-val)
      (put atm :value new-val)
      (atom-notify-watches atm old-val new-val)
      true)
    false))

(defn core-set-validator! [atm validator-fn]
  (put atm :validator validator-fn)
  nil)

(defn core-get-validator [atm]
  (atm :validator))

(defn core-add-watch [atm key watch-fn]
  (let [watches (atm :watches)]
    (put watches key watch-fn)
    atm))

(defn core-remove-watch [atm key]
  (let [watches (atm :watches)]
    (put watches key nil)
    atm))

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

(defn core-cond
  "Macro: (cond test1 expr1 test2 expr2 ... :else default)
   -> (if test1 expr1 (if test2 expr2 ...))"
  [& clauses]
  (defn build [cls]
    (if (= 0 (length cls))
      nil
      (let [t (first cls)]
        (if (= :else t)
          (if (> (length cls) 1) (in cls 1) nil)
          (if (< (length cls) 2)
            (error "cond requires an even number of forms")
            (let [e (in cls 1)]
              @[{:jolt/type :symbol :ns nil :name "if"}
                t e
                (build (tuple/slice cls 2))]))))))
  (build clauses))

(defn core-case
  "Macro: (case expr val1 result1 ... default)
   Supports single values, lists of values (one-of-many), and symbols."
  [expr & clauses]
  (def g (gensym))
  (defn make-const [c]
    (if (and (struct? c) (= :symbol (c :jolt/type)))
      @[{:jolt/type :symbol :ns nil :name "quote"} c]
      c))
  (defn make-test [c]
    (if (array? c)
      (let [or-args @[{:jolt/type :symbol :ns nil :name "or"}]]
        (each v c
          (array/push or-args @[{:jolt/type :symbol :ns nil :name "="} g (make-const v)]))
        or-args)
      @[{:jolt/type :symbol :ns nil :name "="} g (make-const c)]))
  (defn build [cls]
    (if (= 0 (length cls))
      nil
      (if (= 1 (length cls))
        (first cls)
        (let [c (first cls)
              r (first (tuple/slice cls 1))]
          @[{:jolt/type :symbol :ns nil :name "if"}
            (make-test c)
            r
            (build (tuple/slice cls 2))]))))
  @[{:jolt/type :symbol :ns nil :name "let*"} @[g expr] (build clauses)])

(defn core-when
  "Macro: (when test & body) -> (if test (do body...))"
  [test & body]
  (def arr (array ;body))
  (array/insert arr 0 {:jolt/type :symbol :ns nil :name "do"})
  @[{:jolt/type :symbol :ns nil :name "if"}
    test
    arr])

(defn core-when-not
  "Macro: (when-not test & body) -> (when (not test) & body)"
  [test & body]
  (def not-form @[{:jolt/type :symbol :ns nil :name "not"} test])
  @[{:jolt/type :symbol :ns nil :name "if"} not-form
    @[{:jolt/type :symbol :ns nil :name "do"} ;body]])

(defn core-and
  "Macro: (and) -> true, (and x) -> x, (and x y ...) -> (if x (and y ...) x)"
  [& exprs]
  (if (= 0 (length exprs)) true
    (if (= 1 (length exprs)) (first exprs)
      @[{:jolt/type :symbol :ns nil :name "let*"}
        @[{:jolt/type :symbol :ns nil :name "and__x"} (first exprs)]
        @[{:jolt/type :symbol :ns nil :name "if"}
          {:jolt/type :symbol :ns nil :name "and__x"}
          @[{:jolt/type :symbol :ns nil :name "and"} ;(tuple/slice exprs 1)]
          {:jolt/type :symbol :ns nil :name "and__x"}]])))

(defn core-or
  "Macro: (or) -> nil, (or x) -> x, (or x y ...) -> (let [or__x x] (if or__x or__x (or y ...)))"
  [& exprs]
  (if (= 0 (length exprs)) nil
    (if (= 1 (length exprs)) (first exprs)
      @[{:jolt/type :symbol :ns nil :name "let*"}
        @[{:jolt/type :symbol :ns nil :name "or__x"} (first exprs)]
        @[{:jolt/type :symbol :ns nil :name "if"}
          {:jolt/type :symbol :ns nil :name "or__x"}
          {:jolt/type :symbol :ns nil :name "or__x"}
          @[{:jolt/type :symbol :ns nil :name "or"} ;(tuple/slice exprs 1)]]])))

(defn core-if-let
  "Macro: (if-let [binding val-expr] then else?)"
  [bindings then-form & else-forms]
  (def form-sym (in bindings 0))
  (def val-form (in bindings 1))
  @[{:jolt/type :symbol :ns nil :name "let*"}
    @[form-sym val-form]
    @[{:jolt/type :symbol :ns nil :name "if"}
      form-sym
      then-form
      ;else-forms]])

(defn core-when-let
  "Macro: (when-let [binding val-expr] & body)"
  [bindings & body]
  (def form-sym (in bindings 0))
  (def val-form (in bindings 1))
  @[{:jolt/type :symbol :ns nil :name "let*"}
    @[form-sym val-form]
    @[{:jolt/type :symbol :ns nil :name "when"}
      form-sym
      ;body]])

(defn core-if-some
  "Macro: (if-some [binding val-expr] then else?)"
  [bindings then-form & else-forms]
  (def form-sym (in bindings 0))
  (def val-form (in bindings 1))
  @[{:jolt/type :symbol :ns nil :name "let*"}
    @[form-sym val-form]
    @[{:jolt/type :symbol :ns nil :name "if"}
      @[{:jolt/type :symbol :ns nil :name "some?"} form-sym]
      then-form
      ;else-forms]])

(defn core-when-some
  "Macro: (when-some [binding val-expr] & body)"
  [bindings & body]
  (def form-sym (in bindings 0))
  (def val-form (in bindings 1))
  @[{:jolt/type :symbol :ns nil :name "let*"}
    @[form-sym val-form]
    @[{:jolt/type :symbol :ns nil :name "when"}
      @[{:jolt/type :symbol :ns nil :name "some?"} form-sym]
      ;body]])

(defn core-doto
  "Macro: (doto obj (method args)...) → let obj, call methods, return obj"
  [obj & forms]
  (def sym (gensym "doto"))
  (def result @[{:jolt/type :symbol :ns nil :name "let*"} 
                 @[sym obj]])
  (each f forms
    (if (array? f)
      # (doto x (f a b)) -> (f x a b)  (thread x as first arg, not a method call)
      (array/push result @[(first f) sym ;(tuple/slice f 1)])
      (array/push result @[f sym])))
  (array/push result sym)
  result)

(defn core-if-not
  "Macro: (if-not test then else?) -> (if (not test) then else?)"
  [test then-form & else-forms]
  @[{:jolt/type :symbol :ns nil :name "if"}
    @[{:jolt/type :symbol :ns nil :name "not"} test]
    then-form
    ;else-forms])

(defn core-when-first
  "Macro: (when-first [sym coll] & body) -> (when-let [sym (first coll)] body...)"
  [bindings & body]
  (def sym (in bindings 0))
  (def coll-form (in bindings 1))
  @[{:jolt/type :symbol :ns nil :name "when-let"}
    @[sym @[{:jolt/type :symbol :ns nil :name "first"} coll-form]]
    ;body])

(defn core-condp
  "Macro: (condp pred expr clause1 val1 ... default)"
  [pred expr & clauses]
  (def g (gensym))
  (defn build [cls]
    (if (= 0 (length cls))
      nil
      (if (= 1 (length cls))
        (first cls)
        (let [c (first cls)
              v (first (tuple/slice cls 1))]
          @[{:jolt/type :symbol :ns nil :name "if"}
            (if (and (struct? c) (= :symbol (c :jolt/type)) (= ":>>" (c :name)))
              @[v g]
              @[pred c g])
            v
            (build (tuple/slice cls 2))]))))
  @[{:jolt/type :symbol :ns nil :name "let*"} @[g expr] (build clauses)])

(defn core-dotimes
  "Macro: (dotimes [sym n] & body) -> loop from 0 to n-1"
  [bindings & body]
  (def sym (in bindings 0))
  (def n-form (in bindings 1))
  (def i (gensym))
  @[{:jolt/type :symbol :ns nil :name "let*"}
    @[i n-form]
    @[{:jolt/type :symbol :ns nil :name "loop*"}
      @[sym 0]
      @[{:jolt/type :symbol :ns nil :name "if"}
        @[{:jolt/type :symbol :ns nil :name "<"} sym i]
        @[{:jolt/type :symbol :ns nil :name "do"}
          ;body
          @[{:jolt/type :symbol :ns nil :name "recur"}
            @[{:jolt/type :symbol :ns nil :name "inc"} sym]]]
        nil]]])

(defn core-while
  "Macro: (while test & body) -> loop while test is truthy"
  [test & body]
  @[{:jolt/type :symbol :ns nil :name "loop*"}
    @[]
    @[{:jolt/type :symbol :ns nil :name "when"}
      test
      @[{:jolt/type :symbol :ns nil :name "do"} ;body]
      @[{:jolt/type :symbol :ns nil :name "recur"}]]])

(defn core-for
  "Macro: (for [binding-form coll :when test :let [bindings]] body)
   List comprehension. Basic support for :when and :let."
  [bindings body]
  (defn parse-groups [bvec]
    (var groups @[])
    (var i 0)
    (while (< i (length bvec))
      (def bind (bvec i))
      (var coll (bvec (+ i 1)))
      (def mods @[])
      (+= i 2)
      (while (and (< i (length bvec)) (keyword? (bvec i)))
        (case (bvec i)
          :when (do (array/push mods @[{:jolt/type :symbol :ns nil :name "when"} (bvec (+ i 1))]) (+= i 2))
          :let (do (array/push mods @[{:jolt/type :symbol :ns nil :name "let"} (bvec (+ i 1))]) (+= i 2))
          # :while terminates iteration of this binding's collection
          :while (do (set coll @[{:jolt/type :symbol :ns nil :name "take-while"}
                                 @[{:jolt/type :symbol :ns nil :name "fn"} [bind] (bvec (+ i 1))]
                                 coll])
                     (+= i 2))
          (do (+= i 1))))
      (array/push groups @[bind coll mods]))
    groups)
  (defn wrap-mods [mods inner-form]
    (if (= 0 (length mods))
      inner-form
      (let [m (in mods (- (length mods) 1))
            rest-mods (array/slice mods 0 (- (length mods) 1))
            kind (get (m 0) :name)]
        (wrap-mods rest-mods
          (if (= kind "when")
            @[{:jolt/type :symbol :ns nil :name "if"} (m 1)
              @[{:jolt/type :symbol :ns nil :name "list"} inner-form] @[]]
            @[{:jolt/type :symbol :ns nil :name "let*"} (m 1) inner-form])))))
  (defn build [group-idx groups]
    (if (>= group-idx (length groups))
      body
      (let [g (in groups group-idx)
            my-bind (in g 0)
            my-coll (in g 1)
            my-mods (in g 2)
            inner (build (+ group-idx 1) groups)
            inner-form (wrap-mods my-mods inner)
            is-last (= group-idx (- (length groups) 1))
            has-mods (> (length my-mods) 0)]
        (if (and is-last (not has-mods))
          @[{:jolt/type :symbol :ns nil :name "map"}
            @[{:jolt/type :symbol :ns nil :name "fn"} [my-bind] inner-form]
            my-coll]
          @[{:jolt/type :symbol :ns nil :name "mapcat"}
            @[{:jolt/type :symbol :ns nil :name "fn"} [my-bind] inner-form]
            my-coll]))))
  (if (>= (length bindings) 2)
    (build 0 (parse-groups bindings))
    body))

(defn core-thread-first
  "Macro: (-> x & forms) — thread first"
  [x & forms]
  (if (= 0 (length forms)) x
    (let [f (first forms)
          rest-forms (tuple/slice forms 1)]
      (if (array? f)
        (apply core-thread-first [(let [arr (array/slice f)]
                         (array/insert arr 1 x)
                         arr) ;rest-forms])
        (apply core-thread-first [@[f x] ;rest-forms])))))

(defn core-thread-last
  "Macro: (->> x & forms) — thread last"
  [x & forms]
  (if (= 0 (length forms)) x
    (let [f (first forms)
          rest-forms (tuple/slice forms 1)]
      (if (array? f)
        (apply core-thread-last [(let [arr (array/slice f)]
                          (array/push arr x)
                          arr) ;rest-forms])
        (apply core-thread-last [@[f x] ;rest-forms])))))

(defn core-some->
  "Macro: (some-> expr & forms) — thread first, stop at nil"
  [expr & forms]
  (if (= 0 (length forms)) expr
    (let [f (first forms)
          rest-forms (tuple/slice forms 1)]
      @[{:jolt/type :symbol :ns nil :name "let*"}
        @[{:jolt/type :symbol :ns nil :name "some->__x"} expr]
        @[{:jolt/type :symbol :ns nil :name "if"}
          @[{:jolt/type :symbol :ns nil :name "some?"}
            {:jolt/type :symbol :ns nil :name "some->__x"}]
          @[{:jolt/type :symbol :ns nil :name "let*"}
            @[{:jolt/type :symbol :ns nil :name "some->__x"}
              (if (array? f)
                (let [arr (array/slice f)]
                  (array/insert arr 1 {:jolt/type :symbol :ns nil :name "some->__x"})
                  arr)
                @[f {:jolt/type :symbol :ns nil :name "some->__x"}])]
            (apply core-some-> [{:jolt/type :symbol :ns nil :name "some->__x"} ;rest-forms])]
          nil]])))

(defn core-some->>
  "Macro: (some->> expr & forms) — thread last, stop at nil"
  [expr & forms]
  (if (= 0 (length forms)) expr
    (let [f (first forms)
          rest-forms (tuple/slice forms 1)]
      @[{:jolt/type :symbol :ns nil :name "let*"}
        @[{:jolt/type :symbol :ns nil :name "some->__x"} expr]
        @[{:jolt/type :symbol :ns nil :name "if"}
          @[{:jolt/type :symbol :ns nil :name "some?"}
            {:jolt/type :symbol :ns nil :name "some->__x"}]
          @[{:jolt/type :symbol :ns nil :name "let*"}
            @[{:jolt/type :symbol :ns nil :name "some->__x"}
              (if (array? f)
                (let [arr (array/slice f)]
                  (array/push arr {:jolt/type :symbol :ns nil :name "some->__x"})
                  arr)
                @[f {:jolt/type :symbol :ns nil :name "some->__x"}])]
            (apply core-some->> [{:jolt/type :symbol :ns nil :name "some->__x"} ;rest-forms])]
          nil]])))

(defn core-cond->
  "Macro: (cond-> expr test form ...) — thread first only when test is true"
  [expr & clauses]
  (def g (gensym))
  (defn build [cls result-form]
    (if (= 0 (length cls))
      result-form
      (let [t (first cls)
            f (in cls 1)
            f-call (if (array? f)
                     (let [arr (array/slice f)]
                       (array/insert arr 1 result-form)
                       arr)
                     @[f result-form])]
        (build (tuple/slice cls 2)
               @[{:jolt/type :symbol :ns nil :name "if"}
                 t
                 f-call
                 result-form]))))
  @[{:jolt/type :symbol :ns nil :name "let*"} @[g expr] (build clauses g)])

(defn core-cond->>
  "Macro: (cond->> expr test form ...) — thread last only when test is true"
  [expr & clauses]
  (def g (gensym))
  (defn build [cls result-form]
    (if (= 0 (length cls))
      result-form
      (let [t (first cls)
            f (in cls 1)
            f-call (if (array? f)
                     (let [arr (array/slice f)]
                       (array/push arr result-form)
                       arr)
                     @[f result-form])]
        (build (tuple/slice cls 2)
               @[{:jolt/type :symbol :ns nil :name "if"}
                 t
                 f-call
                 result-form]))))
  @[{:jolt/type :symbol :ns nil :name "let*"} @[g expr] (build clauses g)])

(defn core-as->
  "Macro: (as-> expr name & forms) — bind name to expr, thread through forms"
  [expr name & forms]
  (defn build [fs acc]
    (if (= 0 (length fs))
      acc
      (let [f (first fs)]
        @[{:jolt/type :symbol :ns nil :name "let*"}
          @[name acc]
          (build (tuple/slice fs 1) f)])))
  (build forms expr))

(defn core-push-thread-bindings [b] (push-thread-bindings b))
(defn core-pop-thread-bindings [] (pop-thread-bindings))

(defn core-var-get [v] (var-get v))
(defn core-var-set [v val] (var-set v val))
(defn core-var? [x] (var? x))
(defn core-alter-var-root [v f & args] (apply alter-var-root v f args))
(defn core-alter-meta! [v f & args] (apply alter-meta! v f args))
(defn core-reset-meta! [v meta] (reset-meta! v meta))

(defn core-intern [ns-name sym-name val] val)

(defn core-binding
  "Macro: (binding [var val ...] body...)
  Uses array-map (plain struct) to store binding frame
  to avoid PHM get() incompatibility with var-get."
  [bindings & body]
  (def frame-pairs @[])
  (var i 0)
  (let [n (length bindings)]
    (while (< i n)
      (array/push frame-pairs
        @[{:jolt/type :symbol :ns nil :name "var"} (in bindings i)])
      (array/push frame-pairs (in bindings (+ i 1)))
      (+= i 2)))
  (def hm-form (array/insert frame-pairs 0
    {:jolt/type :symbol :ns nil :name "array-map"}))
  @[{:jolt/type :symbol :ns nil :name "let*"}
    [{:jolt/type :symbol :ns nil :name "frame"} hm-form]
    @[{:jolt/type :symbol :ns nil :name "push-thread-bindings"}
      {:jolt/type :symbol :ns nil :name "frame"}]
    @[{:jolt/type :symbol :ns nil :name "try"}
      @[{:jolt/type :symbol :ns nil :name "do"} ;body]
      @[{:jolt/type :symbol :ns nil :name "finally"}
        @[{:jolt/type :symbol :ns nil :name "pop-thread-bindings"}]]]])


(defn- defn->def
  "Shared expansion for defn/defn-: (name doc-string? attr-map? params body...)
  or (name doc-string? attr-map? ([params] body)... attr-map?) -> (def name (fn* ...))."
  [fn-name rest]
  (var items (array/slice rest))
  # strip optional docstring
  (when (and (> (length items) 0) (string? (first items)))
    (set items (array/slice items 1)))
  # strip optional attr-map (a map literal, i.e. struct/table that isn't a symbol)
  (when (and (> (length items) 0)
             (let [x (first items)]
               (and (or (struct? x) (table? x))
                    (not (and (struct? x) (= :symbol (get x :jolt/type)))))))
    (set items (array/slice items 1)))
  (def fn-form @[{:jolt/type :symbol :ns nil :name "fn*"}])
  (if (and (> (length items) 0) (array? (first items)) (indexed? (first (first items))))
    # multi-arity: each remaining item is an ([params] body...) clause
    (each pair items (array/push fn-form pair))
    # single-arity: items = [params-vector body...]
    (do
      (array/push fn-form (first items))
      (each b (tuple/slice items 1) (array/push fn-form b))))
  @[{:jolt/type :symbol :ns nil :name "def"} fn-name fn-form])

(defn core-defn
  "Macro: (defn name doc-string? attr-map? [args] body...) (or multi-arity)
  -> (def name (fn* ...))"
  [fn-name & rest]
  (defn->def fn-name rest))

# defn- — same as defn (private not enforced in Jolt)
(defn core-defn- [fn-name & rest]
  (defn->def fn-name rest))

# Hierarchy stubs for sci bootstrap
(def core-make-hierarchy make-hierarchy)
(defn core-derive
  [& args]
  (case (length args)
    2 (let [[tag parent] args] (derive* (make-hierarchy) tag parent))
    3 (let [[h tag parent] args] (derive* h tag parent))))
(defn core-isa?
  [& args]
  (case (length args)
    1 false
    2 false
    3 (let [[h child parent] args] (isa? h child parent))))
(defn core-ancestors
  [& args]
  (case (length args)
    1 @[]
    2 (let [[h tag] args] (ancestors h tag))))
(defn core-descendants
  [& args]
  (case (length args)
    1 @[]
    2 (let [[h tag] args] (descendants h tag))))
(def core-underive underive)
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
      # table/setproto requires a table, convert struct meta to table
      (var meta-tab @{})
      (each k (keys meta) (put meta-tab k (get meta k)))
      (table/setproto new-obj meta-tab)
      (put new-obj :jolt/meta meta)
      new-obj)))

(defn core-var-dynamic? [v]
  (var-dynamic? v))

# Java interop stubs
(def core-Object (fn [] (struct ;[:jolt/type :jolt/java-object])))

# Volatiles — typed box so deref/volatile? can recognize them.
(defn core-volatile! [v] @{:jolt/type :jolt/volatile :val v})
(defn core-volatile? [x] (and (table? x) (= :jolt/volatile (x :jolt/type))))
(defn core-vswap! [vol f & args]
  (def new-val (apply f (vol :val) args))
  (put vol :val new-val)
  new-val)
(defn core-vreset! [vol val] (put vol :val val) val)

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
    (lazy-seq? x) (truthy? (x :realized))
    (and (table? x) (= :jolt/atom (x :jolt/type))) true
    false))

# delay macro: (delay body...) -> (make-delay (fn* [] body...))
(defn core-delay [& body]
  @[{:jolt/type :symbol :ns nil :name "make-delay"}
    @[{:jolt/type :symbol :ns nil :name "fn*"} [] ;body]])

# Proxy stub — returns nil form (macro, args not evaluated)
(defn core-proxy [& args] nil)

# Thread stubs
(def core-Thread (fn [& args] (struct ;[:jolt/type :jolt/thread])))
(def core-ThreadLocal (fn [& args] (struct ;[:jolt/type :jolt/thread-local])))
(def core-IllegalStateException (fn [& args] (struct ;[:jolt/type :jolt/exception])))

# definterface stub — JVM-only, emits def form
(defn core-definterface [name-sym & body]
  @[{:jolt/type :symbol :ns nil :name "def"}
    name-sym
    @{}])

# comment macro — ignores body, returns nil
(defn core-comment [& body]
  nil)

# defrecord — creates a proper type via deftype + factory functions
(defn core-defrecord [name-sym fields-vec & body]
  (def type-name (name-sym :name))
  (def type-name-dot (string type-name "."))
  (def arrow-name (string "->" type-name))
  (def map-name (string "map->" type-name))
  
  # (deftype TypeName [fields...])
  (def dt-form @[{:jolt/type :symbol :ns nil :name "deftype"} name-sym fields-vec])
  
  # Arrow factory: (def ->TypeName (fn [field1 field2 ...] (TypeName. field1 field2 ...)))
  (def arrow-call @[{:jolt/type :symbol :ns nil :name type-name-dot}])
  (each f fields-vec (array/push arrow-call f))
  (def arrow-sym {:jolt/type :symbol :ns nil :name arrow-name})
  (def arrow-body @[{:jolt/type :symbol :ns nil :name "fn"} fields-vec arrow-call])
  
  # map-> factory: (def map->TypeName (fn [m] (->TypeName (get m :field1) (get m :field2) ...)))
  (def map-call @[{:jolt/type :symbol :ns nil :name arrow-name}])
  (each f fields-vec
    (array/push map-call @[{:jolt/type :symbol :ns nil :name "get"} {:jolt/type :symbol :ns nil :name (string "m")} (keyword (f :name))]))
  (def map-sym {:jolt/type :symbol :ns nil :name map-name})
  # params must be a tuple (a vector), not an array — fn* treats an array
  # first-arg as multi-arity clauses
  (def map-body @[{:jolt/type :symbol :ns nil :name "fn"} [{:jolt/type :symbol :ns nil :name "m"}] map-call])
  
  (def out @[{:jolt/type :symbol :ns nil :name "do"}
    dt-form
    @[{:jolt/type :symbol :ns nil :name "def"} arrow-sym arrow-body]
    @[{:jolt/type :symbol :ns nil :name "def"} map-sym map-body]])
  # Process inline protocol/interface implementations:
  #   (defrecord T [fs] Proto (m [this] body) ... Proto2 (m2 [this] body))
  # Emit an extend-type per protocol. Each method body is wrapped in a let that
  # binds the record's fields from the instance (first method param), matching
  # Clojure's field-in-scope semantics for deftype/defrecord methods.
  (var i 0)
  (while (< i (length body))
    (def elem (in body i))
    (if (and (struct? elem) (= :symbol (elem :jolt/type)))
      # protocol name; collect following method specs
      (let [proto-sym elem
            et @[{:jolt/type :symbol :ns nil :name "extend-type"} name-sym proto-sym]]
        (++ i)
        (while (and (< i (length body)) (not (and (struct? (in body i)) (= :symbol ((in body i) :jolt/type)))))
          (let [spec (in body i)
                mname (spec 0)
                argv (spec 1)
                mbody (tuple/slice spec 2)
                instance (in argv 0)
                # (let [f0 (core-get instance :f0) ...] body...)
                field-binds @[]
                _ (each f fields-vec
                    (array/push field-binds f)
                    (array/push field-binds @[{:jolt/type :symbol :ns nil :name "get"}
                                              instance (keyword (f :name))]))
                wrapped @[{:jolt/type :symbol :ns nil :name "let"}
                          (tuple/slice (tuple ;field-binds)) ;mbody]]
            (array/push et @[mname argv wrapped]))
          (++ i))
        (array/push out et))
      (++ i)))
  out)


# letfn — mutually-recursive local fns. Expands to let* of fn* bindings; jolt
# closures capture the (shared, mutable) bindings table, so forward references
# between the fns resolve at call time.
(defn core-letfn [specs & body]
  (def binds @[])
  (each spec specs
    (let [fname (spec 0)
          rest (tuple/slice spec 1)]
      (array/push binds fname)
      # rest is either ([args] body...) for single-arity or a list of
      # ([args] body) clauses for multi-arity; (fn* ;rest) handles both.
      (array/push binds @[{:jolt/type :symbol :ns nil :name "fn*"} ;rest])))
  @[{:jolt/type :symbol :ns nil :name "let*"} (tuple/slice (tuple ;binds)) ;body])

# doseq — like `for` but eager and returns nil. Reuse `for`, force realization
# with `count`, discard the result.
(defn core-doseq [bindings & body]
  (def for-body @[{:jolt/type :symbol :ns nil :name "do"} ;body nil])
  @[{:jolt/type :symbol :ns nil :name "do"}
    @[{:jolt/type :symbol :ns nil :name "count"}
      @[{:jolt/type :symbol :ns nil :name "for"} bindings for-body]]
    nil])

# assert — (assert x) / (assert x message). Throws when x is falsy.
(defn core-assert [x & more]
  (def msg-form
    (if (> (length more) 0)
      (first more)
      (let [b @""] (pr-render b x) (string "Assert failed: " (string b)))))
  @[{:jolt/type :symbol :ns nil :name "if"}
    x
    nil
    @[{:jolt/type :symbol :ns nil :name "throw"}
      @[{:jolt/type :symbol :ns nil :name "ex-info"} msg-form {}]]])

# resolve stub — returns nil (symbols not found in Jolt's clojure.core)
(defn core-resolve [sym] nil)

# update — works on both structs and tables
(defn core-update [m k f & args]
  (core-assoc m k (apply f (core-get m k) args)))

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
(defn core-declare [& syms]
  @[{:jolt/type :symbol :ns nil :name "do"}])

(defn core-fn
  "Macro: (fn [args] body) → (fn* [args] body)"
  [& args]
  (def result @[])
  (array/push result {:jolt/type :symbol :ns nil :name "fn*"})
  (each a args (array/push result a))
  result)

(defn core-let
  "Macro: (let [bindings] body) → (let* [bindings] body)"
  [bindings & body]
  (def result @[])
  (array/push result {:jolt/type :symbol :ns nil :name "let*"})
  (array/push result bindings)
  (each b body (array/push result b))
  result)

(defn core-loop
  "Macro: (loop [bindings] body) → (loop* [bindings] body)"
  [bindings & body]
  (def result @[])
  (array/push result {:jolt/type :symbol :ns nil :name "loop*"})
  (array/push result bindings)
  (each b body (array/push result b))
  result)

# Protocol implementation — methods dispatch via type registry
(defn core-defprotocol [protocol-name & sigs]
  (def result @[])
  (array/push result {:jolt/type :symbol :ns nil :name "do"})
  (def methods @{})
  (each sig sigs
    (def method-name (first sig))
    (def arglists (tuple/slice sig 1))
    (put methods (keyword (if (struct? method-name) (method-name :name) method-name)) {:name method-name :arglists arglists}))
  (def proto-def @[])
  (array/push proto-def {:jolt/type :symbol :ns nil :name "def"})
  (array/push proto-def protocol-name)
  (array/push proto-def @{:jolt/type :jolt/protocol
                          :name {:jolt/type :symbol :ns nil :name (protocol-name :name)}
                          :methods methods})
  (array/push result proto-def)
  (each sig sigs
    (def method-name (first sig))
    (def method-def @[])
    (array/push method-def {:jolt/type :symbol :ns nil :name "def"})
    (array/push method-def method-name)
    (def fn-form @[])
    (array/push fn-form {:jolt/type :symbol :ns nil :name "fn*"})
    (array/push fn-form [{:jolt/type :symbol :ns nil :name "this"} {:jolt/type :symbol :ns nil :name "&"} {:jolt/type :symbol :ns nil :name "rest-args"}])
    (array/push fn-form @[
      {:jolt/type :symbol :ns nil :name "protocol-dispatch"}
      protocol-name
      method-name
      {:jolt/type :symbol :ns nil :name "this"}
      {:jolt/type :symbol :ns nil :name "rest-args"}])
    (array/push method-def fn-form)
    (array/push result method-def))
  result)

(defn core-extend-type [type-sym proto-sym & impls]
  (def result @[{:jolt/type :symbol :ns nil :name "do"}])
  (each method-spec impls
    (def method-name (method-spec 0))
    (def arg-vec (method-spec 1))
    (def body (tuple/slice method-spec 2))
    (def fn-form @[{:jolt/type :symbol :ns nil :name "fn*"} arg-vec ;body])
    (array/push result @[
      {:jolt/type :symbol :ns nil :name "register-method"}
      type-sym
      proto-sym
      method-name
      fn-form]))
  result)

(defn core-extend-protocol [proto-sym & type-impls]
  (def result @[{:jolt/type :symbol :ns nil :name "do"}])
  (var i 0)
  (while (< i (length type-impls))
    (let [type-sym (type-impls i)
          methods (type-impls (+ i 1))]
      # methods is a single method spec array or an array of method specs
      # If the first element is a symbol (method name), treat as single spec
      (if (and (struct? (methods 0)) (= :symbol ((methods 0) :jolt/type)))
        (let [method-spec methods]
          (def method-name (method-spec 0))
          (def arg-vec (method-spec 1))
          (def body (tuple/slice method-spec 2))
          (def fn-form @[{:jolt/type :symbol :ns nil :name "fn*"} arg-vec ;body])
          (array/push result @[
            {:jolt/type :symbol :ns nil :name "register-method"}
            type-sym
            proto-sym
            method-name
            fn-form]))
        (each method-spec methods
          (def method-name (method-spec 0))
          (def arg-vec (method-spec 1))
          (def body (tuple/slice method-spec 2))
          (def fn-form @[{:jolt/type :symbol :ns nil :name "fn*"} arg-vec ;body])
          (array/push result @[
            {:jolt/type :symbol :ns nil :name "register-method"}
            type-sym
            proto-sym
            method-name
            fn-form]))))
    (+= i 2))
  result)

(def core-extend (fn [& args] nil))

(defn core-reify [proto-sym & impls]
  (def result @[{:jolt/type :symbol :ns nil :name "do"}])
  (def methods @{})
  (var i 0)
  (while (< i (length impls))
    (let [method-spec (impls i)]
      (def method-name (method-spec 0))
      (def arg-vec (method-spec 1))
      (def body (tuple/slice method-spec 2))
      (put methods (keyword (if (struct? method-name) (method-name :name) method-name)) @{:fn* true :args arg-vec :body body})
      (+= i 2)))
  (array/push result @[
    {:jolt/type :symbol :ns nil :name "make-reified"}
    proto-sym
    methods])
  result)

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
  "(keyword name) or (keyword ns name). Namespaced keywords are `:ns/name`."
  [& args]
  (case (length args)
    1 (let [a (in args 0)] (if (keyword? a) a (keyword (core-name a))))
    2 (keyword (string (in args 0) "/" (in args 1)))
    (keyword ;args)))

(defn core-symbol
  "(symbol name) or (symbol ns name) -> a jolt symbol struct."
  [& args]
  (case (length args)
    1 (let [a (in args 0)]
        (if (and (struct? a) (= :symbol (a :jolt/type))) a
          {:jolt/type :symbol :ns nil :name (if (keyword? a) (string a) (string a))}))
    2 {:jolt/type :symbol :ns (in args 0) :name (in args 1)}
    (error "symbol expects 1 or 2 args")))

(defn core-split-at [n coll]
  (let [c (realize-for-iteration coll) m (min n (length c))]
    [(tuple/slice (tuple ;(array/slice c 0 m))) (tuple/slice (tuple ;(array/slice c m)))]))

(defn core-split-with [pred coll]
  (let [c (realize-for-iteration coll)]
    (var i 0)
    (while (and (< i (length c)) (truthy? (pred (in c i)))) (++ i))
    [(tuple/slice (tuple ;(array/slice c 0 i))) (tuple/slice (tuple ;(array/slice c i)))]))

(defn core-take-nth [n coll]
  (let [c (realize-for-iteration coll) r @[]]
    (var i 0) (while (< i (length c)) (array/push r (in c i)) (+= i n))
    (tuple/slice (tuple ;r))))

(defn core-nthrest [coll n]
  (let [c (realize-for-iteration coll)]
    (tuple/slice (tuple ;(array/slice c (min n (length c)))))))

(defn core-nthnext [coll n]
  (let [r (core-nthrest coll n)] (if (= 0 (length r)) nil r)))

(defn core-butlast [coll]
  (let [c (realize-for-iteration coll)]
    (if (<= (length c) 1) nil (tuple/slice (tuple ;(array/slice c 0 (- (length c) 1)))))))

(defn core-filterv [pred coll]
  (let [r @[]] (each x (realize-for-iteration coll) (when (truthy? (pred x)) (array/push r x)))
    (make-vec r)))

(defn core-mapv [f & colls]
  (let [r @[]]
    (if (= 1 (length colls))
      (each x (realize-for-iteration (colls 0)) (array/push r (f x)))
      (let [cs (map realize-for-iteration colls)
            n (min ;(map length cs))]
        (var i 0) (while (< i n) (array/push r (apply f (map (fn [c] (in c i)) cs))) (++ i))))
    (make-vec r)))

(defn core-interpose [sep coll]
  (let [items (realize-for-iteration coll) r @[]]
    (var first? true)
    (each x items (if first? (set first? false) (array/push r sep)) (array/push r x))
    (tuple ;r)))

(defn core-some-search
  "(some pred coll) — first truthy (pred x), else nil."
  [pred coll]
  (var result nil)
  (each x (realize-for-iteration coll)
    (let [r (pred x)] (when (truthy? r) (set result r) (break))))
  result)

(defn core-keep
  "(keep f coll) — (f x) for each x, dropping nils. (keep f) is a transducer."
  [f & rest]
  (if (= 0 (length rest))
    (td-keep f)
    (let [r @[]]
      (each x (realize-for-iteration (in rest 0))
        (let [v (f x)] (when (not (nil? v)) (array/push r v))))
      (tuple ;r))))

(defn core-interleave
  "(interleave & colls) — take one from each in turn until the shortest ends."
  [& colls]
  (if (= 0 (length colls)) (tuple)
    (let [cs (map realize-for-iteration colls)
          n (min ;(map length cs))
          r @[]]
      (var i 0)
      (while (< i n) (each c cs (array/push r (in c i))) (++ i))
      (tuple ;r))))

(defn core-flatten
  "(flatten coll) — fully flatten nested sequentials into one seq."
  [coll]
  (def r @[])
  (defn seqish? [x] (or (tuple? x) (array? x) (pvec? x) (lazy-seq? x)))
  (defn step [x] (each e (realize-for-iteration x) (if (seqish? e) (step e) (array/push r e))))
  (when (seqish? coll) (step coll))
  (tuple ;r))

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

(defn core-not-empty [coll]
  (if (or (nil? coll) (= 0 (core-count coll))) nil coll))

(defn core-rseq [coll]
  (let [c (realize-for-iteration coll)] (tuple/slice (tuple ;(reverse c)))))

(defn core-shuffle [coll]
  (let [c (array/slice (realize-for-iteration coll))]
    (var i (- (length c) 1))
    (while (> i 0)
      (let [j (math/floor (* (math/random) (+ i 1)))
            tmp (in c i)]
        (put c i (in c j)) (put c j tmp))
      (-- i))
    (tuple/slice (tuple ;c))))

(defn core-replace [smap coll]
  (let [c (realize-for-iteration coll) r @[]]
    (each x c (array/push r (let [v (core-get smap x :jolt/nf)] (if (= v :jolt/nf) x v))))
    (tuple/slice (tuple ;r))))

(defn core-some-fn [& preds]
  (fn [& xs]
    (var hit nil)
    (each p preds (each x xs (when (and (nil? hit) (truthy? (p x))) (set hit (p x)))))
    hit))

(defn core-sequential? [x] (or (tuple? x) (array? x) (pvec? x) (plist? x) (lazy-seq? x)))
(defn core-associative? [x] (or (phm? x) (struct? x) (tuple? x) (array? x) (pvec? x) (and (table? x) (not (set? x)))))
(defn core-ifn? [x]
  (or (function? x) (cfunction? x) (keyword? x) (phm? x) (set? x) (tuple? x) (array? x) (pvec? x)
      (and (struct? x) (= :symbol (x :jolt/type)))))
(defn core-indexed? [x] (or (tuple? x) (array? x) (pvec? x)))

(defn core-distinct? [& xs]
  (var seen @{}) (var ok true)
  (each x xs (if (get seen x) (set ok false) (put seen x true)))
  ok)

(defn core-min-key [f & xs]
  (var best (first xs)) (var bestv (f best))
  (each x (array/slice xs 1) (let [v (f x)] (when (< v bestv) (set best x) (set bestv v))))
  best)

(defn core-max-key [f & xs]
  (var best (first xs)) (var bestv (f best))
  (each x (array/slice xs 1) (let [v (f x)] (when (> v bestv) (set best x) (set bestv v))))
  best)

(defn core-not-every? [pred coll]
  (not (do (var ok true) (each x (realize-for-iteration coll) (when (not (truthy? (pred x))) (set ok false))) ok)))

(defn core-not-any? [pred coll]
  (do (var none true) (each x (realize-for-iteration coll) (when (truthy? (pred x)) (set none false))) none))

(defn core-vary-meta [obj f & args]
  (let [m (core-meta obj)] (core-with-meta obj (apply f m args))))

# Exceptions (ex-info / ex-data / ex-message)
(defn core-ex-info [msg data & more]
  @{:jolt/type :jolt/ex-info :message msg :data data
    :cause (if (> (length more) 0) (in more 0) nil)})
(defn core-ex-info? [x] (and (table? x) (= :jolt/ex-info (x :jolt/type))))
(defn- unwrap-ex [e]
  (if (and (or (table? e) (struct? e)) (= :jolt/exception (get e :jolt/type))) (get e :value) e))
(defn core-ex-data [e]
  (let [e (unwrap-ex e)] (if (core-ex-info? e) (e :data) nil)))
(defn core-ex-message [e]
  (let [e (unwrap-ex e)]
    (cond (core-ex-info? e) (e :message) (string? e) e nil)))

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
(defn core-xml-seq [root]
  (def out @[])
  (defn walk [n]
    (array/push out n)
    (when (and (core-map? n) (core-contains? n :content))
      (each c (realize-for-iteration (core-get n :content)) (walk c))))
  (walk root)
  (tuple ;out))
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
(defn core-seq-to-map-for-destructuring [s]
  # used by {:keys [...]} destructuring over a seq of k/v pairs
  (if (core-sequential? s)
    (let [items (realize-for-iteration s) m @{}]
      (var i 0)
      (while (< (+ i 1) (length items)) (put m (in items i) (in items (+ i 1))) (+= i 2))
      (table/to-struct m))
    s))
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
(defn core-undefined? [x] false)

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

(defn- intval? [x] (and (number? x) (= x (math/floor x))))

# Forcing lazy seqs
(defn core-doall [a & rest]
  (let [coll (if (= 0 (length rest)) a (in rest 0))]
    (realize-for-iteration coll) coll))
(defn core-dorun [a & rest]
  (let [coll (if (= 0 (length rest)) a (in rest 0))]
    (realize-for-iteration coll) nil))
(defn core-run! [f coll]
  (each x (realize-for-iteration coll) (f x)) nil)

(defn core-tree-seq [branch? children root]
  (def out @[])
  (defn walk [node]
    (array/push out node)
    (when (truthy? (branch? node))
      (each c (realize-for-iteration (children node)) (walk c))))
  (walk root)
  (tuple ;out))

# Map entries (represented as 2-element vectors)
(defn core-key [e] (core-nth e 0))
(defn core-val [e] (core-nth e 1))
(defn core-map-entry? [x] (and (or (pvec? x) (tuple? x)) (= 2 (core-count x))))

(defn core-rand-nth [coll]
  (let [c (realize-for-iteration coll)]
    (in c (math/floor (* (math/random) (length c))))))

(defn core-replicate [n x] (tuple ;(map (fn [_] x) (range n))))

(defn core-bounded-count [n coll]
  (let [c (realize-for-iteration coll)] (min n (length c))))

(defn core-counted? [x]
  (or (pvec? x) (plist? x) (phm? x) (set? x) (tuple? x) (array? x) (string? x)))
(defn core-reversible? [x] (or (pvec? x) (tuple? x) (array? x)))
(defn core-seqable? [x]
  (or (nil? x) (tuple? x) (array? x) (pvec? x) (plist? x) (phm? x) (set? x)
      (struct? x) (lazy-seq? x) (string? x)
      (and (table? x) (or (get x :jolt/type) (get x :jolt/deftype)))))

# Numeric predicates (Jolt has no ratios/bigdec, so those are always false)
(defn core-nat-int? [x] (and (intval? x) (>= x 0)))
(defn core-pos-int? [x] (and (intval? x) (> x 0)))
(defn core-neg-int? [x] (and (intval? x) (< x 0)))
(defn core-double? [x] (and (number? x) (not (intval? x))))
(defn core-float? [x] (and (number? x) (not (intval? x))))
(defn core-ratio? [x] false)
(defn core-decimal? [x] false)
(defn core-rational? [x] (intval? x))
(defn core-numerator [x] x)
(defn core-denominator [x] 1)

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

(defn core-record? [x] (and (table? x) (not (nil? (get x :jolt/deftype)))))

# Promise: single-threaded box backed by an atom (deref returns nil until set).
(defn core-promise [] (core-atom nil))
(defn core-deliver [p v] (core-reset! p v) p)

(defn core-comparator [pred]
  (fn [a b] (cond (truthy? (pred a b)) -1 (truthy? (pred b a)) 1 true 0)))
(defn core-completing [rf & cf]
  (let [c (if (> (length cf) 0) (in cf 0) (fn [x] x))]
    (fn [& a] (case (length a) 0 (rf) 1 (c (in a 0)) (rf (in a 0) (in a 1))))))
(defn core-keyword-identical? [a b] (= a b))
(defn core-object? [x] false)
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

# Transients — Jolt's collections are persistent, so transients are correctness-
# only aliases (no in-place optimization, but semantically correct).
(defn core-transient [coll] coll)
(defn core-persistent! [coll] coll)
(defn core-conj! [coll & xs] (apply core-conj coll xs))
(defn core-assoc! [coll & kvs] (apply core-assoc coll kvs))
(defn core-dissoc! [coll & ks] (apply core-dissoc coll ks))
(defn core-pop! [coll] (core-pop coll))

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

(defn core-ex-cause [e] (and (table? e) (get e :cause)))
(defn core-prefers [mm-var] (or (get mm-var :jolt/prefers) {}))

(defn core-random-uuid []
  (defn hx [n] (string/format "%x" (math/floor (* (math/random) n))))
  (string (hx 0x10000) (hx 0x10000) "-" (hx 0x10000) "-4" (hx 0x1000)
          "-" (hx 0x1000) "-" (hx 0x10000) (hx 0x10000) (hx 0x10000)))

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
    "abs" core-abs
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
    "partition-all" core-partition-all
    "reductions" core-reductions
    "dedupe" core-dedupe
    "keep-indexed" core-keep-indexed
    "map-indexed" core-map-indexed
    "cycle" core-cycle
    "reduce-kv" core-reduce-kv
    "peek" core-peek
    "pop" core-pop
    "subvec" core-subvec
    "trampoline" core-trampoline
    "format" core-format
    "letfn" core-letfn
    "doseq" core-doseq
    "assert" core-assert
    "first" core-first
    "rest" core-rest
    "next" core-next
    "cons" core-cons
    "seq" core-seq
    "vec" core-vec
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
    "second" core-second
    "doall" core-doall
    "dorun" core-dorun
    "run!" core-run!
    "tree-seq" core-tree-seq
    "key" core-key
    "val" core-val
    "map-entry?" core-map-entry?
    "rand-nth" core-rand-nth
    "replicate" core-replicate
    "bounded-count" core-bounded-count
    "counted?" core-counted?
    "reversible?" core-reversible?
    "seqable?" core-seqable?
    "nat-int?" core-nat-int?
    "pos-int?" core-pos-int?
    "neg-int?" core-neg-int?
    "double?" core-double?
    "float?" core-float?
    "ratio?" core-ratio?
    "decimal?" core-decimal?
    "rational?" core-rational?
    "numerator" core-numerator
    "denominator" core-denominator
    "list*" core-list*
    "special-symbol?" core-special-symbol?
    "record?" core-record?
    "promise" core-promise
    "deliver" core-deliver
    "comparator" core-comparator
    "completing" core-completing
    "keyword-identical?" core-keyword-identical?
    "object?" core-object?
    "tagged-literal" core-tagged-literal
    "ensure-reduced" core-ensure-reduced
    "unreduced" core-unreduced
    "halt-when" core-halt-when
    "re-groups" core-re-groups
    "transient" core-transient
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
    "ex-cause" core-ex-cause
    "prefers" core-prefers
    "random-uuid" core-random-uuid
    "ffirst" core-ffirst
    "nfirst" core-nfirst
    "fnext" core-fnext
    "nnext" core-nnext
    "last" core-last
    "drop-last" core-drop-last
    "take-last" core-take-last
    "interpose" core-interpose
    "mapcat" core-mapcat
    "some" core-some-search
    "keep" core-keep
    "interleave" core-interleave
    "flatten" core-flatten
    "every-pred" core-every-pred
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
    "sorted?" core-sorted-map?
    "reduced" core-reduced
    "reduced?" core-reduced?
    "split-at" core-split-at
    "split-with" core-split-with
    "take-nth" core-take-nth
    "nthrest" core-nthrest
    "nthnext" core-nthnext
    "butlast" core-butlast
    "filterv" core-filterv
    "mapv" core-mapv
    "empty" core-empty
    "not-empty" core-not-empty
    "rseq" core-rseq
    "shuffle" core-shuffle
    "replace" core-replace
    "some-fn" core-some-fn
    "sequential?" core-sequential?
    "associative?" core-associative?
    "ifn?" core-ifn?
    "indexed?" core-indexed?
    "distinct?" core-distinct?
    "min-key" core-min-key
    "max-key" core-max-key
    "not-every?" core-not-every?
    "not-any?" core-not-any?
    "vary-meta" core-vary-meta
    "ex-info" core-ex-info
    "ex-data" core-ex-data
    "ex-message" core-ex-message
    "prn-str" core-prn-str
    "println-str" core-println-str
    "volatile?" core-volatile?
    "force" core-force
    "realized?" core-realized?
    "delay?" core-delay?
    "make-delay" core-make-delay
    "delay" core-delay
    "take" core-take
    "drop" core-drop
    "take-while" core-take-while
    "drop-while" core-drop-while
    "concat" core-concat
    "reverse" core-reverse
    "nth" core-nth
    "sort" core-sort
    "sort-by" core-sort-by
    "distinct" core-distinct
    "group-by" core-group-by
    "frequencies" core-frequencies
    "partition" core-partition
    "partition-by" core-partition-by
    "range" core-range
    "repeat" core-repeat
    "iterate" core-iterate
    "repeatedly" core-repeatedly
    "identity" core-identity
    "constantly" core-constantly
    "complement" core-complement
    "comp" core-comp
    "partial" core-partial
    "juxt" core-juxt
    "memoize" core-memoize
    "vector" core-vector
    "hash-map" core-hash-map
    "array-map" core-array-map
    "hash-set" core-hash-set
    "set" core-set
    "list" core-list
    "set?" core-set?
    "disj" core-disj
    "lazy-seq" core-lazy-seq
    "lazy-cat" core-lazy-cat
    "coll->cells" coll->cells
    "make-lazy-seq" make-lazy-seq
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
    "alength" core-alength
    "aget" core-aget
    "aset" core-aset
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
    "chunked-seq?" core-chunked-seq?
    "chunk-first" core-chunk-first
    "chunk-rest" core-chunk-rest
    "chunk-next" core-chunk-next
    "chunk-cons" core-chunk-cons
    "boolean" core-boolean
    "cat" core-cat
    "disj!" core-disj!
    "rationalize" core-rationalize
    "random-sample" core-random-sample
    "reader-conditional" core-reader-conditional
    "reader-conditional?" core-reader-conditional?
    "sorted-map-by" core-sorted-map-by
    "sorted-set-by" core-sorted-set-by
    "array-seq" core-array-seq
    "seque" core-seque
    "supers" core-supers
    "class" core-class
    "clojure-version" core-clojure-version
    "munge" core-munge
    "namespace-munge" core-namespace-munge
    "test" core-test
    "enumeration-seq" core-enumeration-seq
    "iterator-seq" core-iterator-seq
    "xml-seq" core-xml-seq
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
    "seq-to-map-for-destructuring" core-seq-to-map-for-destructuring
    "eduction" core-eduction
    "->Eduction" core->Eduction
    "proxy-super" core-proxy-super
    "construct-proxy" core-construct-proxy
    "init-proxy" core-init-proxy
    "get-proxy-class" core-get-proxy-class
    "undefined?" core-undefined?
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
    "atom?" core-atom?
    "deref" core-deref
    "reset!" core-reset!
    "swap!" core-swap!
    "swap-vals!" core-swap-vals!
    "reset-vals!" core-reset-vals!
    "compare-and-set!" core-compare-and-set!
    "set-validator!" core-set-validator!
    "get-validator" core-get-validator
    "add-watch" core-add-watch
    "remove-watch" core-remove-watch
    "not" core-not
    "and" core-and
    "or" core-or
    "cond" core-cond
    "case" core-case
    "for" core-for
    "when" core-when
    "when-not" core-when-not
    "if-not" core-if-not
    "when-first" core-when-first
    "if-let" core-if-let
    "when-let" core-when-let
    "if-some" core-if-some
    "when-some" core-when-some
    "doto" core-doto
    "condp" core-condp
    "dotimes" core-dotimes
    "while" core-while
    "->" core-thread-first
    "->>" core-thread-last
    "some->" core-some->
    "some->>" core-some->>
    "cond->" core-cond->
    "cond->>" core-cond->>
    "as->" core-as->
    "defn" core-defn
    "defn-" core-defn-
    "derive" core-derive
    "isa?" core-isa?
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
    "declare" core-declare
    "fn" core-fn
    "let" core-let
    "loop" core-loop
    "defprotocol" core-defprotocol
    "extend-type" core-extend-type
    "extend-protocol" core-extend-protocol
    "extend" core-extend
    "reify" core-reify
    "satisfies?" core-satisfies?
    "extends?" core-extends?
    "implements?" core-implements?
    "type->str" core-type->str
    "volatile!" core-volatile!
    "vswap!" core-vswap!
    "vreset!" core-vreset!
    "proxy" core-proxy
    "Thread" core-Thread
    "ThreadLocal" core-ThreadLocal
    "IllegalStateException" core-IllegalStateException
    "definterface" core-definterface
    "defrecord" core-defrecord
    "comment" core-comment
    "resolve" core-resolve
    "update" core-update
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
    "ident?" core-ident?
    "qualified-ident?" core-qualified-ident?
    "simple-ident?" core-simple-ident?
    "inst?" core-inst?
    "inst-ms" core-inst-ms
    "uri?" core-uri?
    "uuid?" core-uuid?
    "bytes?" core-bytes?
    "tagged-literal?" core-tagged-literal?
    "meta" core-meta
    "var-get" core-var-get
    "var-set" core-var-set
    "var?" core-var?
    "var-dynamic?" core-var-dynamic?
    "alter-var-root" core-alter-var-root
    "alter-meta!" core-alter-meta!
    "reset-meta!" core-reset-meta!
    "intern" core-intern
    "binding" core-binding
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
  "Set of core binding names that are macros."
  []
  @{"and" true "or" true "cond" true "case" true "for" true "when" true "when-not" true "if-let" true "when-let" true "if-some" true "when-some" true "doto" true "defn" true "defn-" true "declare" true "fn" true "let" true "loop" true "defrecord" true "defprotocol" true "extend-type" true "extend-protocol" true "extend" true "reify" true "proxy" true "definterface" true "comment" true "binding" true "lazy-seq" true "lazy-cat" true "if-not" true "when-first" true "condp" true "dotimes" true "while" true "some->" true "some->>" true "cond->" true "cond->>" true "as->" true "->" true "->>" true "letfn" true "doseq" true "delay" true "assert" true})

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
