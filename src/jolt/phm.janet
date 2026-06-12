# PersistentHashMap implementation for Jolt
# Bucket-based hash map with copy-on-write semantics. The bucket array GROWS
# (doubling, rehash) when the load factor passes 2 entries/bucket, so lookups
# stay O(1)-ish at any size — with a fixed 8 buckets, a 100-entry map was a
# ~12-entry linear scan per get (the jolt-s3y map-read regression). The bucket
# count is derived from (length (m :buckets)), so marshaled images from before
# this change keep working.

(def- initial-buckets 8)

(defn phm? [x]
  (and (table? x)
       (= "jolt.lang.persistent-hash-map.PersistentHashMap" (x :jolt/deftype))))

# Keys are hashed and compared by VALUE. Scalars (keywords/strings/numbers) are
# value-hashable in Janet already, but collection keys (a phm/pvec/plist map or
# vector) are Janet tables hashed by identity — so they're canonicalized to a
# value-hashable struct/tuple first. `canonicalize-key` is injected by core (which
# knows the pvec/plist/phm types); phm stays dependency-free. Keys are still
# *stored* as-is, so retrieval and iteration return the original key objects.
(var canonicalize-key nil)
(defn set-canonicalize-key!
  "Install the value-canonicalizer for collection keys (called by core)."
  [f]
  (set canonicalize-key f))
(defn- ck [k]
  (if (and canonicalize-key (or (table? k) (struct? k) (array? k) (tuple? k)))
    (canonicalize-key k)
    k))
(defn canon
  "Public canonicalizer: maps a key to its value-hashable form (identity for
  scalars). Used by callers that index the same canonicalized tables phm uses
  (e.g. transient maps/sets)."
  [k] (ck k))
# Identity/scalar equality first — the common case — before paying for
# canonicalization of collection keys.
(defn- key= [a b] (or (= a b) (= (ck a) (ck b))))

(defn- hash-idx [m k]
  (if (nil? k) 0 (mod (hash (ck k)) (length (m :buckets)))))

(defn- phm-bucket-find [bucket k]
  (var i 0) (var n (length bucket)) (var found nil)
  (while (< i n)
    (if (key= k (in bucket i)) (do (set found (in bucket (+ i 1))) (break)))
    (+= i 2))
  found)

(defn phm-bucket-contains? [bucket k]
  (var i 0) (var n (length bucket)) (var found false)
  (while (< i n)
    (if (key= k (in bucket i)) (do (set found true) (break)))
    (+= i 2))
  found)

(defn- phm-bucket-assoc [bucket k v]
  (var i 0) (var n (length bucket)) (var found-i nil)
  (while (< i n)
    (if (key= k (in bucket i)) (do (set found-i i) (break)))
    (+= i 2))
  (if (not (nil? found-i))
    (let [nb @[]] (var j 0)
      (while (< j n) (array/push nb (if (= j (+ found-i 1)) v (in bucket j))) (++ j)) nb)
    (let [nb @[]] (var j 0)
      (while (< j n) (array/push nb (in bucket j)) (++ j))
      (array/push nb k) (array/push nb v) nb)))

(defn- phm-bucket-dissoc [bucket k]
  (var i 0) (var n (length bucket)) (var found-i nil)
  (while (< i n)
    (if (key= k (in bucket i)) (do (set found-i i) (break)))
    (+= i 2))
  (if (nil? found-i) bucket
    (if (= n 2) nil
      (let [nb @[]] (var j 0)
        (while (< j found-i) (array/push nb (in bucket j)) (++ j))
        (while (< j (- n 2)) (array/push nb (in bucket (+ j 2))) (++ j)) nb))))

(defn phm-get [m k &opt default]
  (default default nil)
  (let [bucket (get (m :buckets) (hash-idx m k))]
    # Single pass with a presence flag (not nil-of-value): a key mapped to nil
    # is still present, so return nil (not the default) when it exists.
    (if bucket
      (do
        (var i 0) (var n (length bucket)) (var result default)
        (while (< i n)
          (if (key= k (in bucket i)) (do (set result (in bucket (+ i 1))) (break)))
          (+= i 2))
        result)
      default)))

# Rehash every entry of `buckets` into a fresh array of `nb` buckets.
(defn- rehash [buckets nb]
  (def out (array/new-filled nb nil))
  (each bucket buckets
    (when bucket
      (var i 0) (var n (length bucket))
      (while (< i n)
        (let [k (in bucket i)
              idx (if (nil? k) 0 (mod (hash (ck k)) nb))]
          (when (nil? (in out idx)) (put out idx @[]))
          (array/push (in out idx) k)
          (array/push (in out idx) (in bucket (+ i 1))))
        (+= i 2))))
  out)

(defn phm-assoc [m k v]
  (let [cnt (m :cnt) idx (hash-idx m k)
        old-bucket (get (m :buckets) idx)
        had-key (if old-bucket (phm-bucket-contains? old-bucket k) false)
        new-bucket (phm-bucket-assoc (if old-bucket old-bucket @[]) k v)
        new-cnt (if had-key cnt (+ cnt 1))
        nbuckets (length (m :buckets))
        new-buckets (array/new nbuckets)]
    (var bi 0)
    (while (< bi nbuckets)
      (put new-buckets bi (if (= bi idx) new-bucket (get (m :buckets) bi))) (++ bi))
    # Grow past load factor 2 (doubling) so buckets stay short. Done on the
    # copy, so persistence is untouched.
    (def grown (if (> new-cnt (* 2 nbuckets))
                 (rehash new-buckets (* 2 nbuckets))
                 new-buckets))
    @{:jolt/type :jolt/phm :jolt/deftype "jolt.lang.persistent-hash-map.PersistentHashMap"
      :cnt new-cnt :buckets grown :_meta (m :_meta)}))

(defn phm-dissoc [m k]
  (let [idx (hash-idx m k) old-bucket (get (m :buckets) idx)]
    (if old-bucket
      (let [new-bucket (phm-bucket-dissoc old-bucket k)]
        (if (= new-bucket old-bucket) m
          (let [new-cnt (- (m :cnt) 1)
                nbuckets (length (m :buckets))
                new-buckets (array/new nbuckets)]
            (var bi 0)
            (while (< bi nbuckets)
              (put new-buckets bi (if (= bi idx) new-bucket (get (m :buckets) bi))) (++ bi))
            @{:jolt/type :jolt/phm :jolt/deftype "jolt.lang.persistent-hash-map.PersistentHashMap"
              :cnt new-cnt :buckets new-buckets :_meta (m :_meta)})))
      m)))

(defn phm-entries [m]
  (var result @[]) (var bi 0) (def nb (length (m :buckets)))
  (while (< bi nb)
    (let [bucket (get (m :buckets) bi)]
      (when bucket
        (var i 0) (var n (length bucket))
        (while (< i n) (array/push result [(in bucket i) (in bucket (+ i 1))]) (+= i 2))))
    (++ bi))
  result)

(defn phm-to-struct [m]
  (var result @{}) (var bi 0) (def nb (length (m :buckets)))
  (while (< bi nb)
    (let [bucket (get (m :buckets) bi)]
      (when bucket
        (var i 0) (var n (length bucket))
        (while (< i n) (put result (in bucket i) (in bucket (+ i 1))) (+= i 2))))
    (++ bi))
  (table/to-struct result))

(defn phm-count [m] (m :cnt))

(defn phm-contains? [m k]
  (let [bucket (get (m :buckets) (hash-idx m k))]
    (if bucket (phm-bucket-contains? bucket k) false)))

(defn make-phm [&opt kvs]
  (default kvs nil)
  (var m @{:jolt/type :jolt/phm :jolt/deftype "jolt.lang.persistent-hash-map.PersistentHashMap"
           :cnt 0 :buckets (array/new-filled initial-buckets nil) :_meta nil})
  (when kvs
    (var i 0) (var n (length kvs))
    (while (< i n) (set m (phm-assoc m (kvs i) (kvs (+ i 1)))) (+= i 2)))
  m)

# ============================================================
# LazySeq — cell-by-cell lazy sequence (Clojure-compatible)
# ============================================================
# Model: thunk returns nil (empty) or [first-val, rest-thunk] pair.
# Each step produces one element + thunk for the rest.
# Supports self-referencing sequences like fib-seq.

(defn lazy-seq?
  "Check if x is a LazySeq."
  [x]
  (and (table? x) (= :jolt/lazy-seq (x :jolt/type))))

(defn make-lazy-seq [thunk]
  @{:jolt/type :jolt/lazy-seq :fn thunk :realized false :val nil})

(defn realize-ls
  "Force a LazySeq cell. Returns nil (empty) or [first-val, rest-thunk].
  If the thunk returns another lazy-seq, recursively realize it.
  Uses :jolt/pending sentinel to detect self-referencing cycles."
  [ls]
  (if (get ls :realized)
    (ls :val)
    (do
      (put ls :val :jolt/pending)
      (put ls :realized true)
      (let [raw ((ls :fn))
            v (if (lazy-seq? raw) (realize-ls raw) raw)]
        (put ls :val v)
        v))))

(defn ls-first [ls]
  (let [cell (realize-ls ls)]
    (if (or (nil? cell) (= :jolt/pending cell) (= 0 (length cell))) nil (in cell 0))))

# The memoized rest wrapper for a node whose cell yielded rest-thunk rt.
# EVERY walk must go through this (not a fresh make-lazy-seq) or independent
# walks re-run the shared thunks and side effects duplicate.
(defn ls-rest-cached [ls rt]
  (or (get ls :rest-ls)
      (let [w (make-lazy-seq rt)]
        (put ls :rest-ls w)
        w)))

(defn ls-rest [ls]
  (let [cell (realize-ls ls)]
    (if (or (nil? cell) (= 0 (length cell))) nil
      (let [rt (in cell 1)]
        (if (nil? rt) nil
          # Memoized wrapper (see ls-rest-cached): a fresh table per call gave
          # every independent walk its own realization state, so the shared
          # rest-thunks re-ran — duplicating side effects (a doall'd seq of
          # futures re-spawned them on the deref walk, serializing pmap).
          (ls-rest-cached ls rt))))))

(defn ls-seq [ls]
  (var result @[])
  (var cur ls)
  (while (not (nil? cur))
    (let [cell (realize-ls cur)]
      (if (nil? cell) (break))
      (array/push result (in cell 0))
      (set cur (ls-rest cur))))
  (if (= 0 (length result)) nil result))

(defn ls-count [ls]
  (var cnt 0)
  (var cur ls)
  (while (not (nil? cur))
    (let [cell (realize-ls cur)]
      (if (nil? cell) (break))
      (++ cnt)
      (set cur (ls-rest cur))))
  cnt)

# ============================================================
# Lazy combinator — primitive for building lazy sequences
# ============================================================

(defn lazy-cons
  "Returns a LazySeq whose first element is x and whose rest is produced
  by rest-thunk (a 0-arg function returning nil or a LazySeq)."
  [x rest-thunk]
  (make-lazy-seq (fn [] @[x rest-thunk])))

# ============================================================
# PersistentHashSet — backed by PersistentHashMap
# ============================================================

(defn set?
  "Check if x is a PersistentHashSet."
  [x]
  (and (table? x) (= :jolt/set (x :jolt/type))))

(defn make-phs [& xs]
  "Create a PersistentHashSet from items."
  (var m (make-phm))
  (each x xs (set m (phm-assoc m x true)))
  @{:jolt/type :jolt/set :phm m :cnt (phm-count m)})

(defn phs-conj [s & xs]
  (var m (s :phm))
  (each x xs (set m (phm-assoc m x true)))
  @{:jolt/type :jolt/set :phm m :cnt (phm-count m)})

(defn phs-disj [s & xs]
  (var m (s :phm))
  (each x xs (set m (phm-dissoc m x)))
  @{:jolt/type :jolt/set :phm m :cnt (phm-count m)})

(defn phs-contains? [s x]
  (phm-contains? (s :phm) x))

(defn phs-count [s]
  (s :cnt))

(defn phs-empty? [s]
  (= 0 (s :cnt)))

(defn phs-seq [s]
  (tuple ;(keys (phm-to-struct (s :phm)))))

(defn phs-get [s x &opt default]
  (default default nil)
  (if (phm-contains? (s :phm) x) x default))

(defn phs-to-struct [s]
  (phm-to-struct (s :phm)))
