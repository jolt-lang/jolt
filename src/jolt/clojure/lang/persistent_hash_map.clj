(ns jolt.lang.persistent-hash-map
  "PersistentHashMap: HAMT persistent hash map.")

(def branch-factor 32)
(def shift-increment 5)

(deftype BitmapIndexedNode [bitmap array])
(deftype PersistentHashMap [count root has-nil? nil-value _meta])

(def not-found (Object.))

(defn- hash-mix [h]
  (mod h 1000000))
(def EMPTY (PersistentHashMap. 0 nil false nil nil))

(defn- mask [h sh]
  (int (bit-and (unsigned-bit-shift-right h sh) 31)))

(defn- bitpos [h sh]
  (bit-shift-left 1 (mask h sh)))

(defn- bit-count [n]
  (loop [n n c 0]
    (if (zero? n) c (recur (bit-and n (dec n)) (inc c)))))

(defn- index [bm bit]
  (bit-count (bit-and bm (dec bit))))

;; Copy entries before idx into new array
(defn- copy-before [src dst idx]
  (loop [i 0]
    (if (< i idx)
      (do (aset dst i (aget src i))
          (aset dst (inc i) (aget src (inc i)))
          (recur (+ i 2))))))

;; Copy entries from src-idx onwards into dst at shifted position 
(defn- copy-after [src dst src-start dst-start end]
  (loop [i src-start]
    (if (< i end)
      (do (aset dst (+ dst-start (- i src-start)) (aget src i))
          (recur (inc i))))))

(defn- bmn-assoc [node shift h key val added?]
  (let [bit (bitpos h shift)
        bm (.-bitmap node)
        arr (.-array node)]
    (if (zero? (bit-and bm bit))
      ;; Insert new entry at this level
      (let [idx (* 2 (index bm bit))
            n (bit-count bm)
            new-len (* 2 (inc n))
            a (object-array new-len)]
        (loop [i 0]
          (if (< i idx)
            (do (aset a i (aget arr i))
                (aset a (inc i) (aget arr (inc i)))
                (recur (+ i 2)))))
        (loop [i idx]
          (if (< i (* 2 n))
            (do (aset a (+ i 2) (aget arr i))
                (aset a (+ i 3) (aget arr (inc i)))
                (recur (+ i 2))))))
        (aset a idx key)
        (aset a (inc idx) val)
        (aset added? 0 true)
        (BitmapIndexedNode. (bit-or bm bit) a))
      ;; Position occupied — just replace value (no recursion for now)
      (let [idx (* 2 (index bm bit))
            ek (aget arr idx)]
        (if (identical? ek key)
          (let [a (aclone arr)]
            (aset a (inc idx) val)
            (BitmapIndexedNode. bm a))
          ;; Different key at same position — use linear chaining in array
          (let [n (bit-count bm)
                new-len (* 2 (inc n))
                a (object-array new-len)]
            (loop [i 0]
              (if (< i (* 2 n))
                (do (aset a i (aget arr i))
                    (recur (inc i)))))
            (aset a (* 2 n) key)
            (aset a (inc (* 2 n)) val)
            (aset added? 0 true)
            (BitmapIndexedNode. bm a))))))

(defn- bmn-find [node shift h key]
  (let [bit (bitpos h shift)
        bm (.-bitmap node)
        arr (.-array node)]
    (if (zero? (bit-and bm bit))
      not-found
      (let [idx (* 2 (index bm bit))
            k (aget arr idx)]
        (if (nil? k)
          (bmn-find (aget arr (inc idx)) (+ shift shift-increment) h key)
          (if (identical? k key)
            (aget arr (inc idx))
            not-found))))))

(defn- bmn-without [node shift h key]
  (let [bit (bitpos h shift)
        bm (.-bitmap node)
        arr (.-array node)]
    (if (zero? (bit-and bm bit))
      node
      (let [idx (* 2 (index bm bit))
            k (aget arr idx)]
        (if (nil? k)
          (let [sub (aget arr (inc idx))
                ns (bmn-without sub (+ shift shift-increment) h key)]
            (if (identical? ns sub)
              node
              (let [a (aclone arr)]
                (aset a (inc idx) ns)
                (BitmapIndexedNode. bm a))))
          (if (identical? k key)
            (let [n (bit-count bm)
                  a (object-array (max 2 (* 2 (dec n))))]
              (loop [i 0]
                (if (< i idx)
                  (do (aset a i (aget arr i))
                      (aset a (inc i) (aget arr (inc i)))
                      (recur (+ i 2)))))
              (loop [i (+ idx 2)]
                (if (< i (* 2 n))
                  (do (aset a (- i 2) (aget arr i))
                      (aset a (- i 1) (aget arr (inc i)))
                      (recur (+ i 2))))))
              (BitmapIndexedNode. (bit-xor bm bit) a))
            node)))))

(defn phm-assoc [m key val]
  (if (nil? key)
    (PersistentHashMap.
      (if (.-has-nil? m) (.-count m) (inc (.-count m)))
      (.-root m) true val (.-_meta m))
    (let [added? (object-array 1)
          h (hash key)
          r (if (nil? (.-root m))
              (bmn-assoc (BitmapIndexedNode. 0 (object-array 2)) 0 h key val added?)
              (bmn-assoc (.-root m) 0 h key val added?))]
      (PersistentHashMap.
        (if (aget added? 0) (inc (.-count m)) (.-count m))
        r (.-has-nil? m) (.-nil-value m) (.-_meta m)))))

(defn phm-without [m key]
  (if (nil? key)
    (if (.-has-nil? m)
      (PersistentHashMap. (dec (.-count m)) (.-root m) false nil (.-_meta m))
      m)
    (if (nil? (.-root m))
      m
      (let [nr (bmn-without (.-root m) 0 (hash-mix (hash key)) key)]
        (if (identical? nr (.-root m))
          m
          (PersistentHashMap. (dec (.-count m)) nr
                             (.-has-nil? m) (.-nil-value m) (.-_meta m)))))))

(defn phm-get
  ([m key] (phm-get m key nil))
  ([m key not-found-val]
   (if (nil? key)
     (if (.-has-nil? m) (.-nil-value m) not-found-val)
     (if (nil? (.-root m))
       not-found-val
       (let [val (bmn-find (.-root m) 0 (hash-mix (hash key)) key)]
         (if (identical? val not-found) not-found-val val))))))

(defn phm-contains? [m key]
  (if (nil? key)
    (.-has-nil? m)
    (if (nil? (.-root m))
      false
      (not (identical? (bmn-find (.-root m) 0 (hash-mix (hash key)) key) not-found)))))

(defn phm-count [m] (.-count m))
(defn phm-empty [m] EMPTY)

(defn hash-map [& kvs]
  (if (nil? kvs)
    EMPTY
    (loop [m EMPTY xs (seq kvs)]
      (if (and xs (seq (rest xs)))
        (recur (phm-assoc m (first xs) (first (rest xs)))
               (rest (rest xs)))
        m))))
