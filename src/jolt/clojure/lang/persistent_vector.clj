(ns jolt.lang.persistent-vector
  "PersistentVector: 32-way branching trie with tail optimization.")

(def branch-factor 32)
(def shift-increment 5)
(def tail-max 31)

(deftype VectorNode [^:volatile-mutable arr])
(deftype PersistentVector [cnt shift root tail _meta])

(def empty-array (object-array 0))
(def EMPTY (PersistentVector. 0 shift-increment nil empty-array nil))

(defn- tailoff [pv]
  (int (- (.-cnt pv) (unsigned-bit-shift-right (.-cnt pv) shift-increment))))

(defn- new-path [level node]
  (if (= level 0)
    node
    (let [arr (object-array branch-factor)]
      (aset arr 0 (new-path (int (- level shift-increment)) node))
      (VectorNode. arr))))

(defn- push-tail [parent level tailnode cnt]
  (let [subidx (int (bit-and (unsigned-bit-shift-right (int cnt) (int level)) tail-max))
        ret (VectorNode. (aclone (.-arr parent)))]
    (if (= level shift-increment)
      (do (aset (.-arr ret) subidx tailnode) ret)
      (let [child (aget (.-arr parent) subidx)]
        (aset (.-arr ret) subidx
              (if child
                (push-tail child (int (- level shift-increment)) tailnode cnt)
                (new-path (int (- level shift-increment)) tailnode)))
        ret))))

(defn- do-assoc [level node i val]
  (let [ret (VectorNode. (aclone (.-arr node)))]
    (if (= level 0)
      (do (aset (.-arr ret) (int (bit-and i tail-max)) val) ret)
      (let [subidx (int (bit-and (unsigned-bit-shift-right (int i) (int level)) tail-max))]
        (aset (.-arr ret) subidx
              (do-assoc (int (- level shift-increment)) (aget (.-arr node) subidx) i val))
        ret))))

(defn- array-for [pv i]
  (if (and (<= 0 i) (< i (.-cnt pv)))
    (if (>= i (tailoff pv))
      (.-tail pv)
      (loop [node (.-root pv) level (.-shift pv)]
        (if (> level 0)
          (recur (aget (.-arr node)
                       (int (bit-and (unsigned-bit-shift-right (int i) (int level)) tail-max)))
                 (int (- level shift-increment)))
          (.-arr node))))
    nil))

(defn pv-conj [pv val]
  (let [cnt (.-cnt pv)]
    (if (< (- cnt (tailoff pv)) branch-factor)
      (let [old-len (alength (.-tail pv))
            new-tail (object-array (+ old-len 1))]
        (loop [i 0]
          (if (< i old-len)
            (do (aset new-tail i (aget (.-tail pv) i)) (recur (unchecked-inc i)))
            (do (aset new-tail i val)
                (PersistentVector. (unchecked-inc cnt) (.-shift pv) (.-root pv) new-tail (.-_meta pv))))))
      (let [tail-node (VectorNode. (.-tail pv))
            root-overflow? (> (unchecked-inc (unsigned-bit-shift-right cnt shift-increment))
                              (bit-shift-left 1 (.-shift pv)))]
        (if root-overflow?
          (let [nr (object-array branch-factor)]
            (aset nr 0 (.-root pv))
            (aset nr 1 (new-path (.-shift pv) tail-node))
            (let [new-root (VectorNode. nr)
                  new-shift (+ (.-shift pv) shift-increment)
                  new-tail (object-array 1)]
              (aset new-tail 0 val)
              (PersistentVector. (unchecked-inc cnt) new-shift new-root new-tail (.-_meta pv))))
          (let [new-root (push-tail (.-root pv) (.-shift pv) tail-node cnt)
                new-tail (object-array 1)]
            (aset new-tail 0 val)
            (PersistentVector. (unchecked-inc cnt) (.-shift pv) new-root new-tail (.-_meta pv))))))))

(defn pv-nth [pv i]
  (let [node (array-for pv i)]
    (if node
      (aget node (int (bit-and i tail-max)))
      (throw (str "Index out of bounds: " i)))))

(defn pv-assoc [pv i val]
  (let [cnt (.-cnt pv)]
    (if (and (<= 0 i) (< i cnt))
      (if (>= i (tailoff pv))
        (let [new-tail (object-array (alength (.-tail pv)))]
          (loop [j 0]
            (if (< j (alength new-tail))
              (do (aset new-tail j
                        (if (= j (int (bit-and i tail-max))) val (aget (.-tail pv) j)))
                  (recur (unchecked-inc j)))
              (PersistentVector. cnt (.-shift pv) (.-root pv) new-tail (.-_meta pv)))))
        (PersistentVector. cnt (.-shift pv) (do-assoc (.-shift pv) (.-root pv) i val) (.-tail pv) (.-_meta pv)))
      (if (= i cnt)
        (pv-conj pv val)
        (throw (str "Index out of bounds: " i))))))

(defn- pop-tail [level node cnt]
  (let [subidx (int (bit-and (unsigned-bit-shift-right (int (- cnt 2)) (int level)) tail-max))]
    (if (> level shift-increment)
      (let [new-child (pop-tail (int (- level shift-increment)) (aget (.-arr node) subidx) cnt)]
        (if (and (nil? new-child) (zero? subidx))
          nil
          (let [ret (VectorNode. (aclone (.-arr node)))]
            (aset (.-arr ret) subidx new-child)
            ret)))
      (if (zero? subidx)
        nil
        (let [ret (VectorNode. (aclone (.-arr node)))]
          (aset (.-arr ret) subidx nil)
          ret)))))

(defn- pv-nth-internal [cnt shift root i]
  (if (and (<= 0 i) (< i cnt))
    (if (>= i (- cnt (int (bit-and cnt tail-max))))
      nil
      (loop [node root level shift]
        (if (> level 0)
          (recur (aget (.-arr node) (int (bit-and (unsigned-bit-shift-right (int i) (int level)) tail-max)))
                 (int (- level shift-increment)))
          (aget (.-arr node) (int (bit-and i tail-max))))))
    nil))

(defn pv-pop [pv]
  (let [cnt (.-cnt pv)]
    (cond
      (zero? cnt) (throw "Can't pop empty vector")
      (= cnt 1) EMPTY
      (> (- cnt (tailoff pv)) 1)
      (let [old-tail (.-tail pv)
            new-tail (object-array (dec (alength old-tail)))]
        (loop [i 0]
          (if (< i (alength new-tail))
            (do (aset new-tail i (aget old-tail i)) (recur (unchecked-inc i)))
            (PersistentVector. (dec cnt) (.-shift pv) (.-root pv) new-tail (.-_meta pv)))))
      :else
      (let [new-root (pop-tail (.-shift pv) (.-root pv) cnt)
            new-cnt (dec cnt)
            new-tail-len (int (bit-and new-cnt tail-max))
            tail-len (if (zero? new-tail-len) branch-factor new-tail-len)
            new-tail (object-array tail-len)]
        (loop [i 0]
          (if (< i tail-len)
            (let [idx (+ (- new-cnt tail-len) i)]
              (aset new-tail i (pv-nth-internal new-cnt (.-shift pv) new-root idx))
              (recur (unchecked-inc i)))
            (PersistentVector. new-cnt (.-shift pv) new-root new-tail (.-_meta pv))))))))

(defn pv-empty [_] EMPTY)

(defn vector [& args]
  (loop [acc EMPTY items (seq args)]
    (if (seq items)
      (recur (pv-conj acc (first items)) (rest items))
      acc)))

(defn vector? [x] (instance? PersistentVector x))
