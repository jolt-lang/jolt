# PersistentHashMap implementation for Jolt
# Bucket-based hash map with copy-on-write semantics. The bucket array GROWS
# (doubling, rehash) when the load factor passes 2 entries/bucket, so lookups
# stay O(1)-ish at any size — with a fixed 8 buckets, a 100-entry map was a
# ~12-entry linear scan per get (the jolt-s3y map-read regression). The bucket
# count is derived from (length (m :buckets)), so marshaled images from before
# this change keep working.
#
# REP vs API: this file is ONLY the hash-map representation (phm-* primitives).
# The Clojure-facing map ops (assoc/dissoc/get/conj/count/seq dispatch, nil-key
# handling, merge) live in core_coll.janet / core_types.janet, which recognize
# phm by its `:jolt/deftype` string. PersistentHashSet is layered on top in
# phs.janet; LazySeq (historically here) now lives in lazyseq.janet.

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

# Index of key k in a bucket (entries are stored stride-2: k v k v ...), or nil.
# The single scan all the bucket ops share — keeping it one place stops the
# stride logic drifting between them.
(defn- bucket-index-of [bucket k]
  (var i 0) (def n (length bucket)) (var found nil)
  (while (< i n)
    (if (key= k (in bucket i)) (do (set found i) (break)))
    (+= i 2))
  found)

(defn- phm-bucket-find [bucket k]
  (let [i (bucket-index-of bucket k)]
    (if i (in bucket (+ i 1)) nil)))

(defn phm-bucket-contains? [bucket k]
  (not (nil? (bucket-index-of bucket k))))

(defn- phm-bucket-assoc [bucket k v]
  (def n (length bucket))
  (def found-i (bucket-index-of bucket k))
  (if (not (nil? found-i))
    (let [nb @[]] (var j 0)
      (while (< j n) (array/push nb (if (= j (+ found-i 1)) v (in bucket j))) (++ j)) nb)
    (let [nb @[]] (var j 0)
      (while (< j n) (array/push nb (in bucket j)) (++ j))
      (array/push nb k) (array/push nb v) nb)))

(defn- phm-bucket-dissoc [bucket k]
  (def n (length bucket))
  (def found-i (bucket-index-of bucket k))
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
      (let [i (bucket-index-of bucket k)]
        (if i (in bucket (+ i 1)) default))
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

