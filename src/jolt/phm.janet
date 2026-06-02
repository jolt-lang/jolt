# PersistentHashMap implementation for Jolt
# Bucket-based hash map with copy-on-write semantics.

(def- bucket-count 8)

(defn phm? [x]
  (and (table? x)
       (= "jolt.lang.persistent-hash-map.PersistentHashMap" (x :jolt/deftype))))

(defn phm-hash-key [k]
  (if (nil? k) 0 (mod (hash k) bucket-count)))

(defn- phm-bucket-find [bucket k]
  (var i 0) (var n (length bucket)) (var found nil)
  (while (< i n)
    (if (= k (in bucket i)) (do (set found (in bucket (+ i 1))) (break)))
    (+= i 2))
  found)

(defn phm-bucket-contains? [bucket k]
  (var i 0) (var n (length bucket)) (var found false)
  (while (< i n)
    (if (= k (in bucket i)) (do (set found true) (break)))
    (+= i 2))
  found)

(defn- phm-bucket-assoc [bucket k v]
  (var i 0) (var n (length bucket)) (var found-i nil)
  (while (< i n)
    (if (= k (in bucket i)) (do (set found-i i) (break)))
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
    (if (= k (in bucket i)) (do (set found-i i) (break)))
    (+= i 2))
  (if (nil? found-i) bucket
    (if (= n 2) nil
      (let [nb @[]] (var j 0)
        (while (< j found-i) (array/push nb (in bucket j)) (++ j))
        (while (< j (- n 2)) (array/push nb (in bucket (+ j 2))) (++ j)) nb))))

(defn phm-get [m k &opt default]
  (default default nil)
  (let [bucket (get (m :buckets) (phm-hash-key k))]
    (if bucket (let [v (phm-bucket-find bucket k)] (if (nil? v) default v)) default)))

(defn phm-assoc [m k v]
  (let [cnt (m :cnt) idx (phm-hash-key k)
        old-bucket (get (m :buckets) idx)
        had-key (if old-bucket (phm-bucket-contains? old-bucket k) false)
        new-bucket (phm-bucket-assoc (if old-bucket old-bucket @[]) k v)
        new-cnt (if had-key cnt (+ cnt 1))
        new-buckets (array/new bucket-count)]
    (var bi 0)
    (while (< bi bucket-count)
      (put new-buckets bi (if (= bi idx) new-bucket (get (m :buckets) bi))) (++ bi))
    @{:jolt/deftype "jolt.lang.persistent-hash-map.PersistentHashMap"
      :cnt new-cnt :buckets new-buckets :_meta (m :_meta)}))

(defn phm-dissoc [m k]
  (let [idx (phm-hash-key k) old-bucket (get (m :buckets) idx)]
    (if old-bucket
      (let [new-bucket (phm-bucket-dissoc old-bucket k)]
        (if (= new-bucket old-bucket) m
          (let [new-cnt (- (m :cnt) 1) new-buckets (array/new bucket-count)]
            (var bi 0)
            (while (< bi bucket-count)
              (put new-buckets bi (if (= bi idx) new-bucket (get (m :buckets) bi))) (++ bi))
            @{:jolt/deftype "jolt.lang.persistent-hash-map.PersistentHashMap"
              :cnt new-cnt :buckets new-buckets :_meta (m :_meta)})))
      m)))

(defn phm-entries [m]
  (var result @[]) (var bi 0)
  (while (< bi bucket-count)
    (let [bucket (get (m :buckets) bi)]
      (when bucket
        (var i 0) (var n (length bucket))
        (while (< i n) (array/push result [(in bucket i) (in bucket (+ i 1))]) (+= i 2))))
    (++ bi))
  result)

(defn phm-to-struct [m]
  (var result @{}) (var bi 0)
  (while (< bi bucket-count)
    (let [bucket (get (m :buckets) bi)]
      (when bucket
        (var i 0) (var n (length bucket))
        (while (< i n) (put result (in bucket i) (in bucket (+ i 1))) (+= i 2))))
    (++ bi))
  (table/to-struct result))

(defn make-phm [&opt kvs]
  (default kvs nil)
  (var m @{:jolt/deftype "jolt.lang.persistent-hash-map.PersistentHashMap"
           :cnt 0 :buckets (array/new bucket-count) :_meta nil})
  (when kvs
    (var i 0) (var n (length kvs))
    (while (< i n) (set m (phm-assoc m (kvs i) (kvs (+ i 1)))) (+= i 2)))
  m)
