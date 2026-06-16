# PersistentHashSet — a set backed by a PersistentHashMap (members are keys
# mapped to true). Extracted from phm.janet (jolt-bvek) so phm.janet is purely
# the hash map; the set is a thin layer over it.
#
# REP vs API: this file is ONLY the set representation (phs-* primitives). The
# Clojure-facing set ops (conj/disj/contains?/count/seq dispatch, set-as-fn
# membership) live in core_coll.janet / core_types.janet, branching on
# `:jolt/type :jolt/set`.

(use ./phm)

(defn set?
  "Check if x is a PersistentHashSet."
  [x]
  (and (table? x) (= :jolt/set (x :jolt/type))))

(defn phs-from-seq [xs]
  "Bulk-build a PersistentHashSet from an indexed collection of members. Members
  back a phm as keys mapped to true, so the bulk HAMT builder (phm-from-pairs,
  which dedups by canonical key) gives set semantics in one pass instead of an
  immutable phm-assoc per element."
  (def pairs @[])
  (each x xs (array/push pairs [x true]))
  (def m (phm-from-pairs pairs))
  @{:jolt/type :jolt/set :phm m :cnt (phm-count m)})

(defn make-phs [& xs]
  "Create a PersistentHashSet from items."
  (phs-from-seq xs))

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
