;; One transient/persistent round trip per timed operation.
(require '[jolt.profile :as profile])

(defn batch [source keys op]
  (let [n (count keys)]
    (loop [i 0 total 0]
      (if (== i n) total
          (let [k (nth keys i)
                t (transient source)
                edited (if (= op :replace) (assoc! t k 1) (dissoc! t k))]
            (recur (inc i) (+ total (count (persistent! edited)))))))))

(defn replace-batch [source keys writes stride]
  (let [n (count keys) size (count source)]
    (loop [i 0 total 0]
      (if (== i n) total
          (let [k (nth keys i) t (transient source)]
            (dotimes [j writes]
              (assoc! t (mod (+ k (* stride j)) size) (- -1 j)))
            (recur (inc i) (+ total (count (persistent! t)))))))))

(defn median [xs] (nth (sort xs) (quot (count xs) 2)))

(doseq [size [8 1024 32768]
        [op writes stride] (concat [[:replace 1 0] [:delete 1 0]]
                                   (when (= size 32768)
                                     [[:replace-twice 2 0] [:replace-two-keys 2 317]
                                      [:replace-eight-keys 8 317]]))]
  (let [source (zipmap (range size) (range size))
        keys (mapv #(mod (* 859 %) size) (range 8192))
        k (peek keys)
        delete? (= op :delete)
        t (transient source)
        expected-map (if delete? (dissoc source k)
                         (reduce (fn [m j] (assoc m (mod (+ k (* stride j)) size)
                                                   (if (= writes 1) 1 (- -1 j))))
                                 source (range writes)))
        expected (* (count keys) (if delete? (dec size) size))
        run (if (= writes 1) #(batch source keys op) #(replace-batch source keys writes stride))]
    (if delete? (dissoc! t k)
        (dotimes [j writes]
          (assoc! t (mod (+ k (* stride j)) size) (if (= writes 1) 1 (- -1 j)))))
    (assert (= expected-map (persistent! t)))
    (assert (= k (get source k)))
    (dotimes [_ 4] (assert (= expected (run))))
    (let [samples (repeatedly 7 #(profile/measure (run)))]
      (assert (every? #(= expected (:result %)) samples))
      (println size (name op)
               (/ (* 1e6 (median (map :real-ms samples))) (count keys))
               (/ (double (median (map :allocated-bytes samples))) (count keys))))))
