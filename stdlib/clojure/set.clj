;;   Portions of this file are from Clojure's src/clj/clojure/set.clj,
;;   Copyright (c) Rich Hickey. All rights reserved.
;;   The use and distribution terms for that software are covered by the
;;   Eclipse Public License 1.0 (licenses/EPL-1.0.txt).

; Jolt Standard Library: clojure.set
; Set operations.

(ns clojure.set)

(defn union
  ([] #{})
  ([s1] s1)
  ([s1 s2] (if (< (count s1) (count s2)) (reduce conj s2 s1) (reduce conj s1 s2)))
  ([s1 s2 & sets] (reduce union (union s1 s2) sets)))

;; s without the members keep? rejects, dropped by disj-ing each one out of s
;; itself (the reference's idiom: it keeps s's type, comparator and meta, and
;; costs only the drops). disj answers by value, so a member not equal to
;; itself (##NaN, or a collection holding one) can't be found that way -- the
;; JVM's disj gets it by reference identity. A disj that misses answers the set
;; itself, and the first such miss rebuilds from (empty s) instead, which keeps
;; the same type, comparator and meta.
(defn- keep-members [keep? s]
  (let [r (reduce (fn [acc item]
                    (if (keep? item)
                      acc
                      (let [acc' (disj acc item)]
                        (if (identical? acc' acc) (reduced ::rebuild) acc'))))
                  s s)]
    (if (identical? r ::rebuild) (into (empty s) (filter keep?) s) r)))

(defn intersection
  ([s1] s1)
  ([s1 s2]
   ;; walk the SMALLER side (the reference's swap): probing is O(1) either way,
   ;; but the reduce walks and rebuilds from its first argument, so
   ;; intersecting a million-element set with a three-element one must cost 3,
   ;; not a million.
   (if (< (count s2) (count s1))
     (recur s2 s1)
     (keep-members #(contains? s2 %) s1)))
  ([s1 s2 & sets] (reduce intersection (intersection s1 s2) sets)))

(defn difference
  ([s1] s1)
  ([s1 s2]
   ;; walk the SMALLER side (the reference's branch): disj-ing a big s2 out of a
   ;; three-element s1 must cost 3, so a smaller s1 keeps what s2 lacks
   (let [n1 (count s1)
         n2 (count s2)]
     (if (< n1 n2)
       ;; This branch walks the left and resolves disj at each matching item.
       (reduce (fn [result item] (if (contains? s2 item) (disj result item) result)) s1 s1)
       ;; The right reduction reads its callback once. Use that same value if
       ;; the host declines, including when disj is rebound.
       (let [drop-member disj]
         (or (when (>= n2 32)
               (jolt.host/try-set-difference s1 s2 drop-member))
             (reduce drop-member s1 s2))))))
  ([s1 s2 & sets] (reduce difference (difference s1 s2) sets)))

(defn select
  [pred s]
  (keep-members pred s))

(defn project
  [xrel ks]
  (set (map #(select-keys % ks) xrel)))

(defn rename-keys
  [map kmap]
  (reduce (fn [m [old new]]
            (if (contains? map old)
              (assoc m new (get map old))
              m))
          (apply dissoc map (keys kmap))
          kmap))

(defn map-invert
  [m]
  (reduce (fn [acc [k v]] (assoc acc v k)) {} m))

(defn rename
  [xrel kmap]
  (set (map (fn [m]
    (reduce (fn [acc [old new]] (if (contains? m old) (assoc acc new (get m old)) acc))
            (apply dissoc m (keys kmap)) kmap)) xrel)))

(defn index
  [xrel ks]
  (reduce (fn [m x] (let [ik (select-keys x ks)] (assoc m ik (conj (get m ik #{}) x)))) {} xrel))

(defn join
  "When passed 2 rels, returns the rel corresponding to the natural join.
  When passed an additional keymap, joins on the corresponding keys."
  ([xrel yrel]
   (if (and (seq xrel) (seq yrel))
     (let [ks (intersection (set (keys (first xrel))) (set (keys (first yrel))))
           [r s] (if (<= (count xrel) (count yrel)) [xrel yrel] [yrel xrel])
           idx (index r ks)]
       (reduce (fn [ret x]
                 (let [found (idx (select-keys x ks))]
                   (if found
                     (reduce (fn [acc y] (conj acc (merge y x))) ret found)
                     ret)))
               #{} s))
     #{}))
  ([xrel yrel km]
   (let [[r s k] (if (<= (count xrel) (count yrel))
                   [xrel yrel (map-invert km)]
                   [yrel xrel km])
         idx (index r (vals k))]
     (reduce (fn [ret x]
               (let [found (idx (rename-keys (select-keys x (keys k)) k))]
                 (if found
                   (reduce (fn [acc y] (conj acc (merge y x))) ret found)
                   ret)))
             #{} s))))

(defn subset?
  [set1 set2]
  (and (<= (count set1) (count set2))
       (every? #(contains? set2 %) set1)))

(defn superset?
  [set1 set2]
  (and (>= (count set1) (count set2))
       (every? #(contains? set1 %) set2)))
