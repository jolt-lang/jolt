;; One edit to a fresh transient of the same persistent map each time.
(require '[jolt.profile :as profile])

(defn batch [source keys op]
  (let [n (count keys)]
    (loop [i 0 total 0]
      (if (== i n) total
          (let [k (nth keys i)
                t (transient source)
                edited (if (= op :replace) (assoc! t k 1) (dissoc! t k))]
            (recur (inc i) (+ total (count (persistent! edited)))))))))

(defn median [xs] (nth (sort xs) (quot (count xs) 2)))

(doseq [size [8 1024 32768] op [:replace :delete]]
  (let [source (zipmap (range size) (range size))
        keys (mapv #(mod (* 859 %) size) (range 8192))
        k (peek keys)
        replace? (= op :replace)
        edited (persistent! (if replace? (assoc! (transient source) k 1)
                                (dissoc! (transient source) k)))
        expected (* (count keys) (if replace? size (dec size)))
        run #(batch source keys op)]
    (assert (= edited (if replace? (assoc source k 1) (dissoc source k))))
    (assert (= k (get source k)))
    (dotimes [_ 4] (assert (= expected (run))))
    (let [samples (repeatedly 7 #(profile/measure (run)))]
      (assert (every? #(= expected (:result %)) samples))
      (println size (name op)
               (/ (* 1e6 (median (map :real-ms samples))) (count keys))
               (/ (double (median (map :allocated-bytes samples))) (count keys))))))
