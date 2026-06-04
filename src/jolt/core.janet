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

(defn core-integer? [x] (and (number? x) (= x (math/floor x))))
(defn core-boolean? [x] (or (= x true) (= x false)))
(defn core-list? [x] (and (array? x) (not (get x :jolt/type))))

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

(defn core-abs [x] (if (neg? x) (- 0 x) x))
(defn core-rand [] (math/random))
(defn core-rand-int [n] (math/floor (* (math/random) n)))

# ============================================================
# Comparison
# ============================================================

(defn- norm-for-= [x]
  "Realize a lazy-seq to a tuple so it compares as a Clojure sequence."
  (if (lazy-seq? x)
    (let [s (ls-seq x)] (if (nil? s) [] (tuple/slice (tuple ;s))))
    x))

(defn core-= [& args]
  (if (< (length args) 2) true
    (do
      (var ok true)
      (var i 0)
      (while (and ok (< i (dec (length args))))
        (let [a (norm-for-= (args i)) b (norm-for-= (args (+ i 1)))]
          (set ok
            (if (and (tuple? a) (array? b))
              (deep= a (tuple/slice (tuple ;b)))
              (if (and (array? a) (tuple? b))
                (deep= (tuple/slice (tuple ;a)) b)
                (if (phm? a)
                  (deep= (phm-to-struct a) (if (phm? b) (phm-to-struct b) b))
                  (if (phm? b) (deep= a (phm-to-struct b))
                    (if (set? a)
                      (deep= (phs-to-struct a) (if (set? b) (phs-to-struct b) b))
                      (if (set? b) (deep= a (phs-to-struct b)) (deep= a b)))))))))
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
      @[]
      (if (tuple? coll)
        (tuple/slice coll 1)
        (array/slice coll 1)))))

(defn core-next [coll]
  (let [r (core-rest coll)]
    (if (= 0 (length r)) nil r)))

(defn core-cons [x coll]
  "Returns a lazy-seq compatible cons cell [first, rest-thunk]."
  @[x (fn [] coll)])

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

(defn realize-for-iteration [c]
  "If c is a lazy-seq, traverse and return all its elements as an array.
  Otherwise return c as-is. Warning: will loop on infinite lazy-seqs.
  Correctly handles nil elements (terminates on the empty cell, not on nil)."
  (if (lazy-seq? c)
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

(defn core-vec [coll]
  (let [coll (realize-for-iteration coll)]
    (if (tuple? coll) coll
      (if (array? coll) (tuple ;coll)
        (if (struct? coll) (tuple ;(map |(in (kvs coll) (+ (* $ 2) 1)) (range (/ (length (kvs coll)) 2))))
          (if (string? coll) (tuple ;(map |(string/from-bytes $) (string/bytes coll)))
            (tuple)))))))

(defn core-into [to from]
  (let [items (realize-for-iteration from)]
    (cond
      # map target: each item is a [k v] pair (or map entry) to assoc
      (or (phm? to) (struct? to) (and (table? to) (get to :jolt/deftype)))
        (do
          (var result to)
          (each item items
            (set result (core-assoc result (in item 0) (in item 1))))
          result)
      # list target (jolt lists are arrays): conj prepends -> reversed order
      (array? to)
        (do
          (var result (array/slice to))
          (each x items (array/insert result 0 x))
          result)
      # vector target (jolt vectors are tuples): conj appends
      (tuple? to)
        (tuple/slice (tuple ;(array/concat (array/slice to) (array/slice items))))
      to)))

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


(defn- seq-done?
  "True when cursor c (a lazy-seq or a concrete collection) is exhausted.
  Uses cell realization for lazy-seqs so nil elements don't end the seq early."
  [c]
  (if (lazy-seq? c)
    (let [cell (realize-ls c)]
      (or (nil? cell) (= :jolt/pending cell) (= 0 (length cell))))
    (or (nil? c) (= 0 (length c)))))

(defn core-map [f & colls]
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
        (let [c (if (set? coll) (phs-seq coll) coll)
              result (do (var res @[]) (each x c (array/push res (f x))) res)]
          (if (tuple? c) (tuple/slice (tuple ;result)) result))))
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
                    (do (put init-cs i nil) (put init-reals i c))))
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
      (make-lazy-seq (step init-cs init-idxs init-reals)))))

(defn core-filter [pred coll]
  (var result @[])
  (each x (if (set? coll) (phs-seq coll) (realize-for-iteration coll))
    (if (pred x) (array/push result x)))
  (if (tuple? coll) (tuple/slice (tuple ;result)) result))

(defn core-remove [pred coll]
  (core-filter (fn [x] (not (pred x))) coll))

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
                (++ i))
              acc)))
      3 (let [f (args 0) val (args 1) coll (args 2)
              coll (if (set? coll) (phs-seq coll) (realize-for-iteration coll))]
          (var acc val)
          (each x coll (set acc (f acc x)))
          acc)
      (error "Wrong number of args passed to: reduce"))))

(defn core-take [n coll]
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
    (do
      (var result @[])
      (var i 0)
      (while (and (< i n) (< i (length coll)))
        (array/push result (coll i))
        (++ i))
      (if (tuple? coll) (tuple/slice (tuple ;result)) result))))

(defn core-drop [n coll]
  (if (lazy-seq? coll)
    (do
      (var cur coll)
      (var i 0)
      (while (and (< i n) (ls-first cur))
        (set cur (ls-rest cur))
        (++ i))
      (if (nil? (ls-first cur)) nil cur))
    (do
      (if (tuple? coll)
        (tuple/slice coll (min n (length coll)))
        (array/slice coll (min n (length coll)))))))

(defn core-take-while [pred coll]
  (var result @[])
  (each x coll
    (if (pred x) (array/push result x) (break)))
  (if (tuple? coll) (tuple/slice (tuple ;result)) result))

(defn core-drop-while [pred coll]
  (var c (if (lazy-seq? coll) (realize-ls coll) coll))
  (var start 0)
  (while (and (< start (length c)) (pred (c start)))
    (++ start))
  (if (tuple? c)
    (tuple/slice c start)
    (array/slice c start)))

(defn coll->cells [c]
  "Convert a seqable to lazy-seq cell chain: nil or [first, rest-thunk].
  If the value is a function, call it and use the result.
  If the result is already a cell (array of [val, function]), return it directly."
  (if (nil? c) nil
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
          nil)))))

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
    (do
      (var result @[])
      (var i (dec (length coll)))
      (while (>= i 0)
        (array/push result (coll i))
        (-- i))
      (if (tuple? coll) (tuple/slice (tuple ;result)) result)))))

(defn core-nth
  "Return the nth element of a sequential collection."
  [coll idx &opt default]
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
      (var c (if (lazy-seq? coll) (realize-ls coll) coll))
      (if (and (>= idx 0) (< idx (length c)))
        (in c idx)
        (if (nil? default)
          (error (string "Index " idx " out of bounds, length: " (length c)))
          default)))))

(defn core-sort [coll]
  (if (nil? coll) @[]
  (if (lazy-seq? coll)
    (do
      (var items @[])
      (var cur coll)
      (while (not (nil? (ls-first cur)))
        (array/push items (ls-first cur))
        (set cur (ls-rest cur)))
      (sort items))
    (let [arr (if (tuple? coll) (array/slice coll) coll)
          sorted (sort arr)]
      (if (tuple? coll) (tuple/slice (tuple ;sorted)) sorted)))))

(defn core-sort-by [keyfn coll]
  (if (nil? coll) (break @[]))
  (var c (if (lazy-seq? coll) (realize-ls coll) coll))
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
      (each x coll
        (if (nil? (seen x))
          (do (put seen x true) (array/push result x))))
      (if (tuple? coll) (tuple/slice (tuple ;result)) result)))))

(defn core-group-by [f coll]
  (var result @{})
  (var c (if (lazy-seq? coll) (realize-ls coll) coll))
  (each x c
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

(def core-repeat (fn [n x]
  (var result @[])
  (var i 0)
  (while (< i n)
    (array/push result x)
    (++ i))
  result))

(defn core-iterate [f x]
  "Lazy infinite sequence x, (f x), (f (f x)), ..."
  (defn istep [v] (fn [] @[v (istep (f v))]))
  (make-lazy-seq (istep x)))

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
    (if (table? x) (or (get x :jolt/meta) (get x :meta)) nil)))

(defn core-every-pred [& preds]
  (fn [x]
    (var ok true)
    (var i 0)
    (while (and ok (< i (length preds)))
      (if (not ((preds i) x))
        (set ok false))
      (++ i))
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
  (apply core-hash-set (if (tuple? coll) (array/slice coll) coll)))

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
      (number? v) (buffer/push-string buf (string v))
      (and (struct? v) (= :symbol (v :jolt/type)))
        (buffer/push-string buf (if (v :ns) (string (v :ns) "/" (v :name)) (v :name)))
      (lazy-seq? v) (pr-render-seq buf (realize-for-iteration v) "(" ")")
      (set? v) (pr-render-seq buf (phs-seq v) "#{" "}")
      (phm? v) (pr-render-pairs buf (phm-entries v))
      (and (table? v) (get v :jolt/deftype)) (buffer/push-string buf (string v))
      (tuple? v) (pr-render-seq buf v "[" "]")
      (array? v) (pr-render-seq buf v "(" ")")
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
      (array/push result @[{:jolt/type :symbol :ns nil :name "."} sym (first f) ;(tuple/slice f 1)])
      (array/push result @[{:jolt/type :symbol :ns nil :name "."} sym f])))
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
      (def coll (bvec (+ i 1)))
      (def mods @[])
      (+= i 2)
      (while (and (< i (length bvec)) (keyword? (bvec i)))
        (case (bvec i)
          :when (do (array/push mods @[{:jolt/type :symbol :ns nil :name "when"} (bvec (+ i 1))]) (+= i 2))
          :let (do (array/push mods @[{:jolt/type :symbol :ns nil :name "let"} (bvec (+ i 1))]) (+= i 2))
          :while (do (+= i 2))
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
  (var new-obj @{})
  (each k (keys obj)
    (put new-obj k (get obj k)))
  # table/setproto requires a table, convert struct meta to table
  (var meta-tab @{})
  (each k (keys meta) (put meta-tab k (get meta k)))
  (table/setproto new-obj meta-tab)
  (put new-obj :jolt/meta meta)
  new-obj)

(defn core-var-dynamic? [v]
  (var-dynamic? v))

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
  (core-assoc m k (apply f (core-get m k) args)))

(defn- ks-rest [ks]
  (if (tuple? ks) (tuple/slice ks 1) (array/slice ks 1)))

(defn core-assoc-in [m ks v]
  (let [k (in ks 0)]
    (if (<= (length ks) 1)
      (core-assoc m k v)
      (let [sub (core-get m k)]
        (core-assoc m k (core-assoc-in (if (nil? sub) {} sub) (ks-rest ks) v))))))

(defn core-update-in [m ks f & args]
  (let [k (in ks 0)]
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
    "every-pred" core-every-pred
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
    "pr-str" core-pr-str
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
  @{"and" true "or" true "cond" true "case" true "for" true "when" true "when-not" true "if-let" true "when-let" true "if-some" true "when-some" true "doto" true "defn" true "defn-" true "declare" true "fn" true "let" true "loop" true "defrecord" true "defprotocol" true "extend-type" true "extend-protocol" true "extend" true "reify" true "proxy" true "definterface" true "comment" true "binding" true "lazy-seq" true "lazy-cat" true "if-not" true "when-first" true "condp" true "dotimes" true "while" true "some->" true "some->>" true "cond->" true "cond->>" true "as->" true "->" true "->>" true})

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
