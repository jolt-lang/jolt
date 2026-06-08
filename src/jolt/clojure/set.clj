; Jolt Standard Library: clojure.set
; Set operations. Note: no & rest arities (evaluator limitation).

(defn union
  ([s1] s1)
  ([s1 s2] (reduce conj s2 s1)))

(defn intersection
  ([s1] s1)
  ([s1 s2]
   (reduce (fn [acc item] (if (contains? s2 item) acc (disj acc item))) s1 s1)))

(defn difference
  ([s1] s1)
  ([s1 s2] (reduce disj s1 s2)))

(defn select
  [pred s]
  (reduce (fn [acc item] (if (pred item) acc (disj acc item))) s s))

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
