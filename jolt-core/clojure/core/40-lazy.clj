;; clojure.core — lazy tier. Canonical CLJS-based lazy seq fns.
;; Loaded after 30-macros.clj, so lazy-seq macro is available.
;;
;; Each fn ported from CLJS core.cljs, stripped of chunked-seq branches.

;; --- distinct ---
(defn distinct [coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                 ((fn [[f :as xs] seen]
                    (when-let [s (seq xs)]
                      (if (contains? seen f)
                        (recur (rest s) seen)
                        (cons f (step (rest s) (conj seen f))))))
                   xs seen)))]
    (step coll #{})))

;; --- dedupe (lazy, canonical) ---
(defn dedupe [coll]
  (let [step (fn step [s prev]
               (lazy-seq
                 (let [s (seq s)]
                   (when s
                     (let [x (first s)]
                       (if (= x prev)
                         (step (rest s) prev)
                         (cons x (step (rest s) x))))))))]
    (let [s (seq coll)]
      (if s
        (lazy-seq (cons (first s) (step (rest s) (first s))))
        ()))))

;; --- keep ---
(defn keep
  ([f]
   (fn [rf]
     (fn ([] (rf)) ([result] (rf result))
       ([result input]
        (let [v (f input)]
          (if (nil? v) result (rf result v)))))))
  ([f coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (let [x (f (first s))]
        (if (nil? x)
          (keep f (rest s))
          (cons x (keep f (rest s)))))))))

;; --- keep-indexed ---
(defn keep-indexed
  ([f]
   (fn [rf]
     (let [ia (volatile! -1)]
       (fn ([] (rf)) ([result] (rf result))
         ([result input]
          (let [i (vswap! ia inc)
                v (f i input)]
            (if (nil? v) result (rf result v))))))))
  ([f coll]
   (letfn [(keepi [idx coll]
             (lazy-seq
               (when-let [s (seq coll)]
                 (let [x (f idx (first s))]
                   (if (nil? x)
                     (keepi (inc idx) (rest s))
                     (cons x (keepi (inc idx) (rest s))))))))]
     (keepi 0 coll))))

;; --- map-indexed ---
(defn map-indexed
  ([f]
   (fn [rf]
     (let [i (volatile! -1)]
       (fn ([] (rf)) ([result] (rf result))
         ([result input] (rf result (f (vswap! i inc) input)))))))
  ([f coll]
   (letfn [(mapi [idx coll]
             (lazy-seq
               (when-let [s (seq coll)]
                 (cons (f idx (first s)) (mapi (inc idx) (rest s))))))]
     (mapi 0 coll))))

;; --- cycle ---
(defn cycle [coll]
  (if-let [vals (seq coll)]
    (let [n (count vals)]
      (letfn [(cstep [i]
                (lazy-seq
                  (cons (nth vals (mod i n)) (cstep (inc i)))))]
        (cstep 0)))
    ()))

;; --- repeatedly ---
(defn repeatedly
  ([f] (lazy-seq (cons (f) (repeatedly f))))
  ([n f] (take n (repeatedly f))))

;; --- repeat ---
(defn repeat
  ([x] (lazy-seq (cons x (repeat x))))
  ([n x] (take n (repeat x))))

;; --- iterate ---
(defn iterate [f x]
  (lazy-seq (cons x (iterate f (f x)))))


;; --- partition-all ---
(defn partition-all
  ([n coll] (partition-all n n coll))
  ([n step coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (cons (take n s) (partition-all n step (nthrest coll step)))))))
