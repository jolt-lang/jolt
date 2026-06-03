# Jolt Core Library
# Clojure-compatible core functions for the Jolt interpreter.

(use ./types)
(use ./phm)

# ============================================================
# Predicates
# ============================================================

(defn core-nil? [x] (nil? x))
(defn core-not [x] (if x false true))
(defn core-some? [x] (not (nil? x)))
(defn core-string? [x] (string? x))
(defn core-number? [x] (number? x))
(defn core-fn? [x] (or (function? x) (cfunction? x)))
(defn core-keyword? [x] (keyword? x))
(defn core-symbol? [x] (and (struct? x) (= :symbol (x :jolt/type))))
(defn core-vector? [x] (tuple? x))
(defn core-map? [x] (or (phm? x) (struct? x) (if (and (table? x) (get x :jolt/deftype)) true false)))
(defn core-seq? [x] (or (array? x) (tuple? x)))
(defn core-coll? [x] (or (array? x) (tuple? x) (struct? x)))

(defn core-true? [x] (= true x))
(defn core-false? [x] (= false x))
(defn core-identical? [a b] (= a b))

(defn core-zero? [x] (and (number? x) (= x 0)))
(defn core-pos? [x] (and (number? x) (> x 0)))
(defn core-neg? [x] (and (number? x) (< x 0)))
(defn core-even? [n] (= 0 (% n 2)))
(defn core-odd? [n] (not= 0 (% n 2)))

(defn core-empty? [coll]
  (if (nil? coll) true
    (if (set? coll) (= 0 (coll :cnt))
      (if (phm? coll) (= 0 (coll :cnt))
        (if (struct? coll) (= 0 (length (keys coll)))
          (= 0 (length coll)))))))

(defn core-every? [pred coll]
  (var result true)
  (each x (if (set? coll) (phs-seq coll) coll)
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
(def core-mod %)
(def core-rem %)
(def core-quot (fn [n d] (math/floor (/ n d))))

(defn core-max [& args] (apply max args))
(defn core-min [& args] (apply min args))

# ============================================================
# Comparison
# ============================================================

(defn core-= [& args]
  (if (< (length args) 2) true
    (do
      (var ok true)
      (var i 0)
      (while (and ok (< i (dec (length args))))
        (let [a (args i) b (args (+ i 1))]
          (set ok
            (if (phm? a)
              (deep= (phm-to-struct a) (if (phm? b) (phm-to-struct b) b))
              (if (phm? b) (deep= a (phm-to-struct b))
                (if (set? a)
                  (deep= (phs-to-struct a) (if (set? b) (phs-to-struct b) b))
                  (if (set? b) (deep= a (phs-to-struct b)) (deep= a b)))))))
        (++ i))
      ok)))

(defn core-not= [& args] (not (apply core-= args)))

(defn core-< [a b] (< a b))
(defn core-> [a b] (> a b))
(defn core-<= [a b] (<= a b))
(defn core->= [a b] (>= a b))

# ============================================================
# Collections
# ============================================================

(defn core-conj [coll & xs]
  (if (tuple? coll)
    (tuple/slice (tuple ;(array/concat (array/slice coll) xs)))
    (if (array? coll)
      (do
        (var result coll)
        (var i 0)
        (while (< i (length xs))
          (set result (array/insert result 0 (xs i)))
          (++ i))
        result)
      (if (set? coll)
        (apply phs-conj coll xs)
        (if (phm? coll)
          (do
            (var result coll)
            (var i 0)
            (while (< i (length xs))
              (let [pair (xs i)]
                (set result (phm-assoc result (pair 0) (pair 1))))
              (++ i))
            result)
          (do
            (var result coll)
            (var i 0)
            (while (< i (length xs))
              (let [pair (xs i)]
                (set result (merge result {(pair 0) (pair 1)})))
              (++ i))
            result))))))

(defn core-assoc [m & kvs]
  (if (phm? m)
    (do (var result m) (var i 0) (while (< i (length kvs)) (set result (phm-assoc result (kvs i) (kvs (+ i 1)))) (+= i 2)) result)
    (do (var result @{}) (when m (each k (if (struct? m) (keys m) (keys (table ;(pairs m)))) (put result k (get m k))))
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
        (if (or (struct? m) (table? m))
          (let [v (m k)]
            (if (nil? v) default v))
          (if (and (or (tuple? m) (array? m)) (number? k) (>= k 0) (< k (length m)))
            (in m k)
            default))))))

(defn core-get-in [m ks &opt default]
  (default default nil)
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
      (if (struct? coll) (not (nil? (coll key)))
        (if (table? coll) (not (nil? (coll key)))
          (if (or (tuple? coll) (array? coll))
            (and (number? key) (>= key 0) (< key (length coll)))
            false))))))

(defn core-count [coll]
  (if (lazy-seq? coll) (ls-count coll)
    (if (set? coll) (coll :cnt)
      (if (phm? coll) (coll :cnt)
        (if (and (table? coll) (get coll :jolt/deftype)) (- (length (keys coll)) 1)
          (length coll))))))

(defn core-first [coll]
  (if (lazy-seq? coll) (ls-first coll)
    (if (or (nil? coll) (= 0 (length coll))) nil
      (in coll 0))))

(defn core-rest [coll]
  (if (lazy-seq? coll) (ls-rest coll)
    (if (or (nil? coll) (= 0 (length coll)))
      nil
      (if (tuple? coll)
        (tuple/slice coll 1)
        (array/slice coll 1)))))

(defn core-next [coll]
  (let [r (core-rest coll)]
    (if (= 0 (length r)) nil r)))

(defn core-cons [x coll]
  (if (nil? coll)
    @[x]
    (if (tuple? coll)
      (tuple/slice (tuple ;(array/insert (array/slice coll) 0 x)))
      (array/insert coll 0 x))))

(defn core-seq [coll]
  (if (or (nil? coll) (and (or (tuple? coll) (array? coll)) (= 0 (length coll))))
    nil
    (if (lazy-seq? coll) (ls-seq coll)
      (if (set? coll) (phs-seq coll)
        (if (phm? coll) (tuple ;(phm-entries coll))
          (if (tuple? coll) (tuple/slice coll)
            (if (string? coll) (map |(string/from-bytes $) (string/bytes coll))
              (if (struct? coll) (tuple ;(keys coll))
                coll))))))))

(defn core-vec [coll]
  (if (tuple? coll) coll
    (if (array? coll) (tuple ;coll)
      (if (struct? coll) (tuple ;(map |(in (kvs coll) (+ (* $ 2) 1)) (range (/ (length (kvs coll)) 2))))
        (tuple)))))

(defn core-into [to from]
  (if (tuple? to)
    (tuple/slice (tuple ;(array/concat (array/slice to) (if (tuple? from) from (array/slice from)))))
    (if (array? to)
      (array/concat to from)
      (if (struct? to)
        (do
          (var result to)
          (each [k v] (pairs from)
            (set result (merge result {k v})))
          result)
        to))))

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
  (each k ks
    (let [v (core-get m k)]
      (if (not (nil? v)) (put result k v))))
  (if (struct? m) (table/to-struct result) result))

(defn core-zipmap [ks vs]
  (var result @{})
  (var i 0)
  (while (and (< i (length ks)) (< i (length vs)))
    (put result (ks i) (vs i))
    (++ i))
  (table/to-struct result))

# ============================================================
# Sequence operations
# ============================================================

(defn core-map [f & colls]
  (let [first-coll (colls 0)
        result (if (= 1 (length colls))
                 (array ;(map f first-coll))
                 (do
                   (var res @[])
                   (var idxs @{})
                   (each _ first-coll (array/push idxs 0))
                   (var done false)
                   (while (not done)
                     (var args @[])
                     (var i 0)
                     (while (< i (length colls))
                       (let [c (colls i) j (idxs i)]
                         (if (>= j (length c))
                           (do (set done true) (break))
                           (array/push args (c j))))
                       (++ i))
                     (if (not done) (array/push res (apply f args)))
                     (var k 0)
                     (while (< k (length colls))
                       (set (idxs k) (+ (idxs k) 1))
                       (++ k)))
                   res))]
    (if (tuple? first-coll) (tuple/slice (tuple ;result)) result)))

(defn core-filter [pred coll]
  (var result @[])
  (each x (if (set? coll) (phs-seq coll) coll)
    (if (pred x) (array/push result x)))
  (if (tuple? coll) (tuple/slice (tuple ;result)) result))

(defn core-remove [pred coll]
  (core-filter (fn [x] (not (pred x))) coll))

(def core-reduce
  (fn [& args]
    (case (length args)
      2 (let [f (args 0) coll (args 1)
              coll (if (set? coll) (phs-seq coll) coll)]
          (if (= 0 (length coll))
            (f)
            (do
              (var acc (coll 0))
              (var i 1)
              (while (< i (length coll))
                (set acc (f acc (coll i)))
                (++ i))
              acc)))
      3 (let [f (args 0) val (args 1) coll (args 2)
              coll (if (set? coll) (phs-seq coll) coll)]
          (var acc val)
          (each x coll (set acc (f acc x)))
          acc)
      (error "Wrong number of args passed to: reduce"))))

(defn core-take [n coll]
  (var result @[])
  (var i 0)
  (while (and (< i n) (< i (length coll)))
    (array/push result (coll i))
    (++ i))
  (if (tuple? coll) (tuple/slice (tuple ;result)) result))

(defn core-drop [n coll]
  (if (tuple? coll)
    (tuple/slice coll (min n (length coll)))
    (array/slice coll (min n (length coll)))))

(defn core-take-while [pred coll]
  (var result @[])
  (each x coll
    (if (pred x) (array/push result x) (break)))
  (if (tuple? coll) (tuple/slice (tuple ;result)) result))

(defn core-drop-while [pred coll]
  (var start 0)
  (while (and (< start (length coll)) (pred (coll start)))
    (++ start))
  (if (tuple? coll)
    (tuple/slice coll start)
    (array/slice coll start)))

(defn core-concat [& colls]
  (var result @[])
  (each c colls
    (each x c (array/push result x)))
  result)

(defn core-reverse [coll]
  (var result @[])
  (var i (dec (length coll)))
  (while (>= i 0)
    (array/push result (coll i))
    (-- i))
  (if (tuple? coll) (tuple/slice (tuple ;result)) result))

(defn core-nth
  "Return the nth element of a sequential collection."
  [coll idx &opt default]
  (if (and (>= idx 0) (< idx (length coll)))
    (in coll idx)
    (if (nil? default)
      (error (string "Index " idx " out of bounds, length: " (length coll)))
      default)))

(defn core-sort [coll]
  (let [arr (if (tuple? coll) (array/slice coll) coll)
        sorted (sort arr)]
    (if (tuple? coll) (tuple/slice (tuple ;sorted)) sorted)))

(defn core-sort-by [keyfn coll]
  (let [arr (if (tuple? coll) (array/slice coll) coll)
        sorted (sort-by keyfn arr)]
    (if (tuple? coll) (tuple/slice (tuple ;sorted)) sorted)))

(defn core-distinct [coll]
  (var seen @{})
  (var result @[])
  (each x coll
    (if (nil? (seen x))
      (do
        (put seen x true)
        (array/push result x))))
  (if (tuple? coll) (tuple/slice (tuple ;result)) result))

(defn core-group-by [f coll]
  (var result @{})
  (each x coll
    (let [k (f x)]
      (put result k (array/push (core-get result k @[]) x))))
  result)

(defn core-frequencies [coll]
  (core-group-by identity coll))

(defn core-partition [n coll]
  (var result @[])
  (var i 0)
  (while (< i (length coll))
    (var part @[])
    (var j 0)
    (while (and (< j n) (< (+ i j) (length coll)))
      (array/push part (coll (+ i j)))
      (++ j))
    (if (= (length part) n) (array/push result (tuple/slice (tuple ;part))))
    (+= i n))
  result)

(defn core-partition-by [f coll]
  (var result @[])
  (var part @[])
  (var last-k nil)
  (each x coll
    (let [k (f x)]
      (if (and last-k (deep= k last-k))
        (array/push part x)
        (do
          (if (> (length part) 0) (array/push result (tuple/slice (tuple ;part))))
          (set part @[x])
          (set last-k k)))))
  (if (> (length part) 0) (array/push result (tuple/slice (tuple ;part))))
  result)

# ============================================================
# Sequence generators
# ============================================================

(def core-range
  (fn [& args]
    (let [start (if (> (length args) 1) (args 0) 0)
          end (if (> (length args) 1) (args 1) (args 0))
          step (if (> (length args) 2) (args 2) 1)]
      (var result @[])
      (var i start)
      (while (if (pos? step) (< i end) (> i end))
        (array/push result i)
        (+= i step))
      (tuple/slice (tuple ;result)))))

(def core-repeat (fn [n x]
  (var result @[])
  (var i 0)
  (while (< i n)
    (array/push result x)
    (++ i))
  result))

(defn core-iterate [f x]
  "Macro: (iterate f x) → lazy infinite sequence x, (f x), (f (f x)), ..."
  (def sym-x (gensym "x"))
  (def sym-f (gensym "f"))
  @[{:jolt/type :symbol :ns nil :name "lazy-seq"}
    @[{:jolt/type :symbol :ns nil :name "let*"}
      @[sym-x x sym-f f]
      @[{:jolt/type :symbol :ns nil :name "cons"}
        sym-x
        @[{:jolt/type :symbol :ns nil :name "iterate"}
          sym-f
          @[{:jolt/type :symbol :ns nil :name sym-f} sym-x]]]]])

(defn core-repeatedly [n f]
  (var result @[])
  (var i 0)
  (while (< i n)
    (array/push result (f))
    (++ i))
  result)

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

(defn core-meta [x]
  "Returns the metadata of x, or nil."
  (if (var? x) (var-meta x)
    (if (struct? x) (get x :meta) nil)))

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

(defn core-vector [& xs] (tuple ;xs))
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
    @[{:jolt/type :symbol :ns nil :name "fn*"} [] ;body]])

(defn core-set [coll]
  (apply core-hash-set (if (tuple? coll) (array/slice coll) coll)))

(defn core-list [& xs]
  (array ;xs))

# ============================================================
# String functions
# ============================================================

(defn core-str [& xs]
  (if (= 0 (length xs)) ""
    (do
      (var result @[])
      (each x xs
        (if (nil? x) nil  # skip nil
          (array/push result (if (string? x) x (string x)))))
      (string/join result ""))))

(defn core-name
  "Returns the name string of a keyword, symbol, or string."
  [x]
  (if (keyword? x) (string x)
    (if (and (struct? x) (= :symbol (x :jolt/type))) (x :name)
      (if (string? x) x
        ""))))

(defn core-namespace
  "Returns the namespace of a keyword, symbol, or nil if none."
  [x]
  (if (keyword? x) (string x)
    (if (and (struct? x) (= :symbol (x :jolt/type)))
      (if (x :ns) (if (struct? (x :ns)) ((x :ns) :name) (string (x :ns))) nil)
      nil)))

(def core-subs
  (fn [& args]
    (case (length args)
      2 (string/slice (args 0) (args 1))
      3 (string/slice (args 0) (args 1) (args 2))
      (error "Wrong number of args passed to: subs"))))

# ============================================================
# I/O — minimal wrappers
# ============================================================

(def core-print print)
(def core-println (fn [& xs] (apply print xs) (print "\n") nil))

(defn core-pr-str [& xs]
  "Returns a string representation of xs suitable for reading."
  (var buf @"")
  (var first? true)
  (each x xs
    (if first? (set first? false) (buffer/push buf " "))
    (if (nil? x) (buffer/push buf "nil")
      (= true x) (buffer/push buf "true")
      (= false x) (buffer/push buf "false")
      (keyword? x) (do (buffer/push buf ":") (buffer/push buf (string x)))
      (string? x) (do (buffer/push buf "\"") (buffer/push buf x) (buffer/push buf "\""))
      (buffer/push buf (string x))))
  (string buf))

(defn core-pr [& xs]
  (var i 0)
  (while (< i (length xs))
    (if (> i 0) (prin " "))
    (prin (xs i))
    (++ i))
  nil)

(defn core-prn [& xs]
  (apply core-pr xs)
  (print "\n")
  nil)

# ============================================================
# Array primitives (needed for persistent data structures)
# ============================================================

(def core-alength (fn [arr] (length arr)))
(def core-aget (fn [arr idx] (in arr idx)))
(def core-aset (fn [arr idx val] (put arr idx val) val))
(def core-aclone (fn [arr] (array/slice arr 0)))
(def core-object-array (fn [size] (array/new-filled size nil)))
(def core-int-array (fn [size] (array/new-filled size 0)))
(def core-to-array (fn [coll]
  (def arr @[])
  (each x coll (array/push arr x))
  arr))

# ============================================================
# Bit operations (needed for persistent data structures)  
# ============================================================

(def core-bit-and (fn [a b] (band a b)))
(def core-bit-or (fn [a b] (bor a b)))
(def core-bit-xor (fn [a b] (bxor a b)))
(def core-bit-not (fn [a] (bnot a)))
(def core-bit-shift-left (fn [x n] (blshift x n)))
(def core-bit-shift-right (fn [x n] (brshift x n)))
(def core-unsigned-bit-shift-right (fn [x n] (brushift x n)))

# ============================================================
# Integer coercion
# ============================================================

(def core-int (fn [x] (math/trunc x)))
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

(defn core-atom [val]
  @{:jolt/type :jolt/atom :value val :watches @{}})

(defn core-atom? [x]
  (and (table? x) (= :jolt/atom (x :jolt/type))))

(defn core-deref [ref]
  (cond
    # Jolt atom
    (and (table? ref) (= :jolt/atom (ref :jolt/type)))
    (ref :value)
    # Jolt var (from types.janet)
    (and (table? ref) (= :jolt/var (ref :jolt/type)))
    (ref :root)
    # default: return as-is
    ref))

(defn core-reset! [atm val]
  (put atm :value val)
  val)

(defn core-swap! [atm f & args]
  (let [new-val (apply f (atm :value) args)]
    (put atm :value new-val)
    new-val))

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
      (array/push result @[{:jolt/type :symbol :ns nil :name "."} sym (first f) ;(tuple/slice f 1)])
      (array/push result @[{:jolt/type :symbol :ns nil :name "."} sym f])))
  (array/push result sym)
  result)

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


(defn core-defn
  "Macro: (defn name [args] body) or (defn name ([args] body)...) 
  -> (def name (fn* ...) )"
  [fn-name & rest]
  # Multi-arity if rest starts with list of [args] pairs
  (if (and (> (length rest) 0) (array? (first rest)) (indexed? (first (first rest))))
    (let [pairs rest]
      (def fn-form @[])
      (array/push fn-form {:jolt/type :symbol :ns nil :name "fn*"})
      (each pair pairs (array/push fn-form pair))
      @[{:jolt/type :symbol :ns nil :name "def"} fn-name fn-form])
    # Single-arity: (defn name [args] body...)
    (let [args-form (first rest)
          body (tuple/slice rest 1)]
      (def fn-form @[])
      (array/push fn-form {:jolt/type :symbol :ns nil :name "fn*"})
      (array/push fn-form args-form)
      (each b body (array/push fn-form b))
      @[{:jolt/type :symbol :ns nil :name "def"} fn-name fn-form])))

# defn- — same as defn (private not enforced in Jolt)
(defn core-defn- [fn-name & rest]
  # Multi-arity if rest starts with list of [args] pairs
  (if (and (> (length rest) 0) (array? (first rest)) (indexed? (first (first rest))))
    (let [pairs rest]
      (def fn-form @[])
      (array/push fn-form {:jolt/type :symbol :ns nil :name "fn*"})
      (each pair pairs (array/push fn-form pair))
      @[{:jolt/type :symbol :ns nil :name "def"} fn-name fn-form])
    # Single-arity: (defn- name [args] body...)
    (let [args-form (first rest)
          body (tuple/slice rest 1)]
      (def fn-form @[])
      (array/push fn-form {:jolt/type :symbol :ns nil :name "fn*"})
      (array/push fn-form args-form)
      (each b body (array/push fn-form b))
      @[{:jolt/type :symbol :ns nil :name "def"} fn-name fn-form])))

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
(def core-remove-method (fn [mm-var dispatch-val]
  (let [methods (get mm-var :jolt/methods)]
    (put methods dispatch-val nil) mm-var)))
(def core-remove-all-methods (fn [mm-var]
  (put mm-var :jolt/methods @{}) mm-var))
(defn core-prefer-method [mm-var dispatch-val-a dispatch-val-b]
  (let [prefs (or (get mm-var :jolt/prefers)
                 (do (put mm-var :jolt/prefers @{}) (mm-var :jolt/prefers)))]
    (put prefs dispatch-val-a dispatch-val-b) mm-var))

# Java interop stubs
(def core-Object (fn [] (struct ;[:jolt/type :jolt/java-object])))

# Volatile stubs (minimal — use table as volatile box)
(defn core-volatile! [v] @{:val v})
(defn core-vswap! [vol f & args] 
  (def new-val (apply f (vol :val) args))
  (put vol :val new-val)
  new-val)
(defn core-vreset! [vol val] (put vol :val val) val)

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
    (array/push map-call @[{:jolt/type :symbol :ns nil :name "core-get"} {:jolt/type :symbol :ns nil :name (string "m")} (keyword (f :name))]))
  (def map-sym {:jolt/type :symbol :ns nil :name map-name})
  (def map-body @[{:jolt/type :symbol :ns nil :name "fn"} @[{:jolt/type :symbol :ns nil :name (string "m")}] map-call])
  
  @[{:jolt/type :symbol :ns nil :name "do"}
    dt-form
    @[{:jolt/type :symbol :ns nil :name "def"} arrow-sym arrow-body]
    @[{:jolt/type :symbol :ns nil :name "def"} map-sym map-body]])


# resolve stub — returns nil (symbols not found in Jolt's clojure.core)
(defn core-resolve [sym] nil)

# update — works on both structs and tables
(defn core-update [m k f & args]
  (let [current (get m k)
        new-val (apply f current args)]
    (put m k new-val)))

# copy-var stubs for sci.impl.copy-vars (used by sci.impl.namespaces)
(defn core-copy-core-var [sym] nil)
(defn core-copy-var [sym & args] nil)
(defn core-macrofy [sym fn] fn)
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
    "empty?" core-empty?
    "every?" core-every?
    "+" core-+
    "-" core-sub
    "*" core-*
    "/" core-/
    "inc" core-inc
    "dec" core-dec
    "mod" core-mod
    "rem" core-rem
    "quot" core-quot
    "max" core-max
    "min" core-min
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
    "zipmap" core-zipmap
    "map" core-map
    "filter" core-filter
    "remove" core-remove
    "reduce" core-reduce
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
    "make-lazy-seq" make-lazy-seq
    "str" core-str
    "name" core-name
    "subs" core-subs
    "str-trim" string/trim
    "str-upper" string/ascii-upper
    "str-lower" string/ascii-lower
    "str-find" string/find
    "str-replace" string/replace
    "str-replace-all" string/replace-all
    "str-reverse-b" string/reverse
    "str-join" string/join
    "str-split" string/split
    "str-triml" string/triml
    "str-trimr" string/trimr
    "print" core-print
    "println" core-println
    "pr" core-pr
    "prn" core-prn
    # Array primitives (for persistent data structures)
    "alength" core-alength
    "aget" core-aget
    "aset" core-aset
    "aclone" core-aclone
    "object-array" core-object-array
    "int-array" core-int-array
    "to-array" core-to-array
    # Bit operations
    "bit-and" core-bit-and
    "bit-or" core-bit-or
    "bit-xor" core-bit-xor
    "bit-not" core-bit-not
    "bit-shift-left" core-bit-shift-left
    "bit-shift-right" core-bit-shift-right
    "unsigned-bit-shift-right" core-unsigned-bit-shift-right
    # Integer coercion / unchecked math
    "int" core-int
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
    "not" core-not
    "and" core-and
    "or" core-or
    "when" core-when
    "when-not" core-when-not
    "if-let" core-if-let
    "when-let" core-when-let
    "if-some" core-if-some
    "when-some" core-when-some
    "doto" core-doto
    "defn" core-defn
    "defn-" core-defn-
    "derive" core-derive
    "isa?" core-isa?
    "ancestors" core-ancestors
    "descendants" core-descendants
    "make-hierarchy" core-make-hierarchy
    "underive" core-underive
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
    "copy-core-var" core-copy-core-var
    "copy-var" core-copy-var
    "macrofy" core-macrofy
    "new-var" core-new-var
    "avoid-method-too-large" core-avoid-method-too-large
    "qualified-symbol?" core-qualified-symbol?
    "meta" core-meta
    "var-get" core-var-get
    "var-set" core-var-set
    "var?" core-var?
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
  @{"and" true "or" true "when" true "when-not" true "if-let" true "when-let" true "if-some" true "when-some" true "doto" true "defn" true "defn-" true "declare" true "fn" true "let" true "loop" true "defrecord" true "defprotocol" true "extend-type" true "extend-protocol" true "extend" true "reify" true "proxy" true "definterface" true "comment" true "binding" true "lazy-seq" true})

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
