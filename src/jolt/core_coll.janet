# Jolt Core — collections, transducers, seqs, HOFs, constructors
# Extracted from core.janet (jolt-nma8, phase 2b split).
#
# REP vs API: this file holds the Clojure-facing collection ops and dispatches
# on `:jolt/type` over the internal persistent structures, whose representations
# live elsewhere: persistent vector → pv.janet, list → plist.janet, hash map →
# phm.janet, set → phs.janet, lazy seq → lazyseq.janet. Grep a structure's file
# header for the primitive (pv-*/pl-*/phm-*/phs-*/ls-*) it exposes.

(use ./types)
(use ./phm)
(use ./phs)
(use ./lazyseq)
(use ./regex)
(use ./config)
(use ./pv)
(use ./plist)
(use ./core_types)
# Collections
# ============================================================

# Small maps are Janet structs (native, O(1) get) but assoc copies them whole
# (O(n)); past this many entries a map promotes to the phm HAMT (O(log n) assoc)
# so incremental building isn't O(n^2). Mirrors cljs PersistentArrayMap's
# HASHMAP-THRESHOLD (jolt-684u).
(def- map-array-threshold 8)

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

# Merge conj items onto a map receiver. assoc1 is phm-assoc for a phm receiver,
# map-assoc1 for a struct/host-table receiver — the only thing that differs
# between the two map-conj paths.
(defn- conj-into-map [coll xs assoc1]
  (var result coll)
  (each x xs
    (cond
      # conj nil onto a map is a no-op (Clojure)
      (nil? x) nil
      # conj a map -> merge its entries
      (map-value? x)
      (each e (map-entries-of x) (set result (assoc1 result (in e 0) (in e 1))))
      # a [k v] entry: exactly a 2-element vector (Clojure throws otherwise — and
      # merge inherits this strictness through conj)
      (and (or (pvec? x) (tuple? x) (array? x)) (= 2 (vcount x)))
      (set result (assoc1 result (vnth x 0) (vnth x 1)))
      (error "Vector arg to map conj must be a pair")))
  result)

# Dispatch is on :jolt/type via one case (the type is fetched once and the arm
# calls the concrete op directly) rather than a chain of (and (table? x) (= ..))
# predicates — same hot-path cost as the predicate chain for the common types,
# one place per op. Host values (tuple/array/nil) and tuple-based shape-recs
# carry no :jolt/type and stay in the per-op fallback.
(defn core-conj [& args]
  (if (= 0 (length args)) (make-vec @[])        # (conj) -> []
  (let [coll (first args) xs (tuple/slice args 1)]
    (if (table? coll)
      (case (get coll :jolt/type)
        :jolt/pvec (do (var r coll) (each x xs (set r (pv-conj r x))) r)
        :jolt/phm  (conj-into-map coll xs phm-assoc)
        # list: prepend, O(1) per element via structural sharing
        :jolt/plist (do (var r coll) (each x xs (set r (pl-cons x r))) r)
        # conj onto a seq prepends (Clojure: a Cons cell)
        :jolt/lazy-seq (do (var r coll) (each x xs (set r (pl-cons x (realize-for-iteration r)))) r)
        :jolt/set (apply phs-conj coll xs)
        :jolt/sorted-map ((sorted-op coll :conj) coll xs)
        :jolt/sorted-set ((sorted-op coll :conj) coll xs)
        # other tables (raw host table / deftype instance) conj like a map
        (conj-into-map coll xs map-assoc1))
      (cond
        # conj onto nil builds a list (prepends): (conj nil 1 2) -> (2 1)
        (nil? coll) (do (var r nil) (each x xs (set r (pl-cons x r))) r)
        (tuple? coll) (tuple/slice (tuple ;(array/concat (array/slice coll) xs)))
        (array? coll)
          (if mutable?
            # mutable mode: arrays are vectors — append in place
            (do (each x xs (array/push coll x)) coll)
            # immutable mode: arrays are lists — prepend onto a persistent cons
            # node, sharing the original array as the tail (O(1) per element)
            (do (var r coll) (each x xs (set r (pl-cons x r))) r))
        # struct map literal: merge entries
        (conj-into-map coll xs map-assoc1))))))

(defn core-assoc [m & kvs]
  (when (odd? (length kvs))
    (error "assoc expects an even number of key/value arguments"))
  # assoc is defined on maps, vectors and nil; reject other shapes
  (when (or (number? m) (string? m) (buffer? m) (keyword? m) (boolean? m)
            (plist? m) (set? m) (core-transient? m) (core-sorted-set? m)
            (and (struct? m) (get m :jolt/type)))
    (error (string "assoc requires a map or vector, got " (type m))))
  (cond
    (shape-rec? m)
      (do (var result m) (var i 0)
        (while (< i (length kvs)) (set result (shape-assoc result (kvs i) (kvs (+ i 1)))) (+= i 2))
        result)
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
    # map (struct/table). Promote to a phm when (a) any new key is a collection
    # (a Janet struct/table would key it by identity) or any new key/value is nil
    # (a struct drops nil; phm preserves it), or (b) the result would exceed the
    # small-map threshold — a Janet struct copies wholesale on assoc (O(n)), so a
    # growing map must ride the phm HAMT (O(log n)) past ~8 entries. Mirrors cljs
    # PersistentArrayMap -> PersistentHashMap (jolt-684u). m is a struct here
    # (phm handled above), so only the current size + new kvs matter.
    (let [coll-key (do (var c false) (var i 0)
                     (while (< i (length kvs))
                       (let [k (in kvs i) v (in kvs (+ i 1))]
                         (when (or (table? k) (array? k) (nil? k) (nil? v)) (set c true)))
                       (+= i 2)) c)
          promote (or coll-key
                      (> (+ (if m (length m) 0) (/ (length kvs) 2)) map-array-threshold))]
      (if promote
        (do (var result (make-phm))
            (when m (each k (keys m) (set result (phm-assoc result k (get m k)))))
            (var i 0) (while (< i (length kvs)) (set result (phm-assoc result (in kvs i) (in kvs (+ i 1)))) (+= i 2))
            result)
        (do (var result @{}) (when m (each k (keys m) (put result k (get m k))))
          (var i 0) (while (< i (length kvs)) (let [k (kvs i) v (kvs (+ i 1))] (put result k v) (+= i 2)))
          # nil assocs to a fresh immutable map ((assoc nil :a 1) => {:a 1}); a
          # raw table here would not count?/seq like a Clojure map (assoc-in into
          # an absent key recurses through nil — migratus's migration maps).
          (if (or (struct? m) (nil? m)) (table/to-struct result) result))))))

(defn core-dissoc [m & ks]
  (cond
    (nil? m) nil
    # dissoc loses a key -> the shape changes; bridge through a struct (cold op)
    (shape-rec? m) (core-dissoc (shape->struct m) ;ks)
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
    # inline the shape check (no fn call) so non-shape gets pay only a tuple? test
    (if (and (tuple? m) (> (length m) 0) (struct? (in m 0)) (not (nil? (in (in m 0) :jolt/shape))))
      (shape-get m k default)
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
        # Clojure's get indexes strings too (returns the char) — reitit's path
        # parser relies on (get path i). nth already did; get did not, so
        # (get "a:b" 1) was nil.
        (if (and (or (string? m) (buffer? m)) (number? k) (>= k 0) (< k (length m)))
          (make-char (in m k))
          default)))))))))))

# Runtime invoke dispatch for COMPILED code (interpreter uses evaluator's
# jolt-invoke). Handles real functions plus Clojure IFn collections.
(defn jolt-call [f & args]
  (cond
    (or (function? f) (cfunction? f)) (apply f args)
    (shape-rec? f) (core-get f (get args 0) (get args 1))
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
  (if (shape-rec? coll) (shape-contains? coll key)
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
  (if (table? coll)
    (case (get coll :jolt/type)
      :jolt/pvec (pv-count coll)
      :jolt/phm (coll :cnt)
      :jolt/plist (pl-count coll)
      :jolt/set (coll :cnt)
      :jolt/lazy-seq (ls-count coll)
      :jolt/sorted-map ((sorted-op coll :count) coll)
      :jolt/sorted-set ((sorted-op coll :count) coll)
      :jolt/transient (length (if (= :vector (coll :kind)) (coll :arr) (coll :tbl)))
      # other tables: a deftype record instance counts its fields; a raw host
      # table is unsupported (matches the original — seq handles it, count never did)
      (if (get coll :jolt/deftype) (- (length (keys coll)) 1)
        (error (string "count not supported on " (type coll)))))
    (cond
      (nil? coll) 0
      (shape-rec? coll) (shape-count coll)          # shape-recs are tuples, not tables
      (or (string? coll) (buffer? coll) (struct? coll) (tuple? coll) (array? coll)) (length coll)
      # count is undefined on scalars (numbers/keywords/symbols/booleans/chars)
      (error (string "count not supported on " (type coll))))))

(defn core-first [coll]
  (cond
    (shape-rec? coll) (core-first (shape->struct coll))
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
  (if (table? coll)
    (case (get coll :jolt/type)
      :jolt/pvec (if (= 0 (pv-count coll)) nil (tuple ;(pv->array coll)))
      # empty maps/sets seq to nil, as in Clojure ((seq {}) is nil, not ())
      :jolt/phm (if (= 0 (coll :cnt)) nil (tuple ;(phm-entries coll)))
      :jolt/plist (if (pl-empty? coll) nil (tuple ;(pl->array coll)))
      # Cell-based emptiness, NOT (nil? (ls-first)): a lazy-seq whose first
      # element is legitimately nil is non-empty, so (seq (cons nil ...)) is not nil.
      :jolt/lazy-seq (let [cell (realize-ls coll)]
                       (if (or (nil? cell) (= :jolt/pending cell) (= 0 (length cell))) nil coll))
      :jolt/set (if (= 0 (coll :cnt)) nil (phs-seq coll))
      :jolt/sorted-map ((sorted-op coll :seq) coll)
      :jolt/sorted-set ((sorted-op coll :seq) coll)
      # deftype instance seqs as itself; raw host table (System/getenv) as kv map
      (if (get coll :jolt/deftype) coll
        (if (= 0 (length coll)) nil
          (tuple ;(map (fn [k] (tuple k (get coll k))) (keys coll))))))
    (cond
      # shape-recs are tuples — must precede the tuple? branch
      (shape-rec? coll) (tuple ;(map (fn [k] (tuple k (shape-get coll k nil))) (shape-keys coll)))
      (nil? coll) nil
      (buffer? coll) (if (= 0 (length coll)) nil (let [a @[]] (each x coll (array/push a x)) (tuple ;a)))
      (tuple? coll) (if (= 0 (length coll)) nil (tuple/slice coll))
      (string? coll) (if (= 0 (length coll)) nil (tuple ;(map make-char (string/bytes coll))))
      (struct? coll) (if (= 0 (length coll)) nil (tuple ;(map (fn [k] (tuple k (get coll k))) (keys coll))))
      (array? coll) (if (= 0 (length coll)) nil (tuple ;coll))
      # scalars/functions aren't seqable
      (error (string "seq not supported on " (type coll))))))

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
      (cond
        # Indexed colls iterate in place — realize-for-iteration would copy a
        # pvec into a fresh array (alloc + pv-nth per element) on EVERY
        # reduce call, which dominates tight reduce-over-vector loops
        # (jolt-4vr). Also breaks at `reduced` instead of scanning the tail.
        (pvec? coll)
        (do (def n (pv-count coll)) (var i 0)
            (while (and (< i n) (not stop))
              (set acc (rf acc (pv-nth coll i)))
              (when (core-reduced? acc) (set acc (acc :val)) (set stop true))
              (++ i)))
        (or (tuple? coll) (array? coll))
        (do (def n (length coll)) (var i 0)
            (while (and (< i n) (not stop))
              (set acc (rf acc (in coll i)))
              (when (core-reduced? acc) (set acc (acc :val)) (set stop true))
              (++ i)))
        (each x (if (set? coll) (phs-seq coll) (realize-for-iteration coll))
          (when (not stop)
            (set acc (rf acc x))
            (when (core-reduced? acc) (set acc (acc :val)) (set stop true)))))))
  acc)

(defn- transduce-reduce
  "Reduce coll with reducing fn rf and seed init, honoring `reduced`."
  [rf init coll]
  (reduce-with-reduced rf init coll))

# SEED-TWIN: transduce is overlay-public (jolt-core/clojure/core/20-coll.clj);
# this seed copy is NOT registered in core-bindings. It survives only as the
# helper core-into calls below — user `transduce` resolves to the overlay. The
# asymmetry with `into` (seed-public) is intentional; docs/seed-overlay-registry.md.
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
          # reify clojure.lang.IReduceInit: the reified value carries its own
          # reduce — call it (ring.util.codec's tokenizer reduces this way)
          (if-let [m (and (table? coll)
                          (get coll :jolt/protocol-methods)
                          (get (get coll :jolt/protocol-methods) :reduce))]
            (m coll f val)
            (reduce-with-reduced f val coll)))
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
     # A string iterates as a seq of chars in Clojure; realize-for-iteration
     # passes strings through, so char-seq it here (as take-while/remove do) —
     # otherwise pred sees raw bytes and array/slice rejects the string.
     (let [c0 (realize-for-iteration coll)
           c (if (string? c0) (map make-char (string/bytes c0)) c0)]
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
