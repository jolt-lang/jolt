; Jolt Standard Library: clojure.set
; Set operations (union, intersection, difference, subset?, superset?, etc.)

(defn union
  "Return a set that is the union of the input sets."
  ([s1] s1)
  ([s1 s2]
   (if (< (count s1) (count s2))
     (reduce conj s2 s1)
     (reduce conj s1 s2)))
  ([s1 s2 & sets]
   (reduce union (union s1 s2) sets)))

(defn intersection
  "Return a set that is the intersection of the input sets."
  ([s1] s1)
  ([s1 s2]
   (reduce (fn [acc item]
             (if (contains? s2 item) acc (disj acc item)))
           s1 s1))
  ([s1 s2 & sets]
   (reduce intersection (intersection s1 s2) sets)))

(defn difference
  "Return a set that is the first set without elements of the other sets."
  ([s1] s1)
  ([s1 s2]
   (reduce disj s1 s2))
  ([s1 s2 & sets]
   (reduce difference (difference s1 s2) sets)))

(defn select
  "Returns a set of the elements for which pred is true."
  [pred s]
  (reduce (fn [acc item]
            (if (pred item) acc (disj acc item)))
          s s))

(defn project
  "Returns a rel of the elements of xrel with only the keys in ks."
  [xrel ks]
  (set (map #(select-keys % ks) xrel)))

(defn rename
  "Returns a rel with the maps in xrel renamed according to kmap argument,
  which is a map from original to new key name."
  [xrel kmap]
  (set
   (map
    (fn [m]
      (reduce (fn [acc [old new]]
                (if (contains? m old)
                  (assoc acc new (get m old))
                  acc))
              (apply dissoc m (keys kmap))
              kmap))
    xrel)))

(defn rename-keys
  "Returns the map with the keys in kmap renamed to the values in kmap."
  [map kmap]
  (reduce
   (fn [m [old new]]
     (if (contains? m old)
       (assoc m new (get m old) old nil)
       m))
   map kmap))

(defn map-invert
  "Returns the map with the vals mapped to the keys."
  [m]
  (reduce (fn [acc [k v]] (assoc acc v k)) {} m))

(defn join
  "When passed 2 rels, returns the rel corresponding to the natural
  join. When passed an additional keymap, joins on the corresponding
  keys."
  ([xrel yrel]
   (if (and (seq xrel) (seq yrel))
     (let [ks (intersection (set (keys (first xrel)))
                            (set (keys (first yrel))))
           idx (map-invert (zipmap (range) yrel))]
       (reduce (fn [acc x]
                 (reduce (fn [acc y]
                           (if (= (select-keys x ks)
                                  (select-keys y ks))
                             (conj acc (merge x y))
                             acc))
                         acc yrel))
               #{} xrel))
     #{}))
  ([xrel yrel kmap]
   (let [kmap (if (map? kmap) kmap (zipmap kmap kmap))
         idx (reduce (fn [m y]
                       (assoc m (select-keys y (vals kmap)) y))
                     {} yrel)]
     (reduce
      (fn [acc x]
        (let [found (get idx (select-keys x (keys kmap)))]
          (if found
            (conj acc (merge x (rename-keys found kmap)))
            acc)))
      #{} xrel))))

(defn index
  "Returns a map of the distinct values of ks in the xrel mapped to a
  set of the maps in xrel with the corresponding values of ks."
  [xrel ks]
  (reduce (fn [m x]
            (let [ik (select-keys x ks)]
              (assoc m ik (conj (get m ik #{}) x))))
          {} xrel))

(defn subset?
  "Is set1 a subset of set2?"
  [set1 set2]
  (and (<= (count set1) (count set2))
       (every? #(contains? set2 %) set1)))

(defn superset?
  "Is set1 a superset of set2?"
  [set1 set2]
  (and (>= (count set1) (count set2))
       (every? #(contains? set1 %) set2)))
