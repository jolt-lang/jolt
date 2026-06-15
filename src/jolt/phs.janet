# PersistentHashSet — a set backed by a PersistentHashMap (members are keys
# mapped to true). Extracted from phm.janet (jolt-bvek) so phm.janet is purely
# the hash map; the set is a thin layer over it.

(use ./phm)

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
