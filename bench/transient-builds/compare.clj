;; Run from the generated collection benchmark project. The argument rotates
;; the profiles between fresh launches. Each line is one process median.
(require '[workload.collections :as w]
         '[jolt.profile :as profile])

(def sizes [8 9 32 33 512 1024 16384 32768])
(def launch (parse-long (or (first *command-line-args*) "0")))
(defn median [xs] (nth (sort xs) (quot (count xs) 2)))

(doseq [n (take (count sizes) (drop (mod (* launch 3) (count sizes)) (cycle sizes)))]
  (let [calls (quot 65536 n)
        elements (* n calls)
        keys (vec (range n))
        pairs (mapv (fn [k] [k (inc k)]) keys)
        indices (mapv #(mod (+ 47 (* % 7)) n) (range calls))
        probes (long-array indices)]
    (doseq [[kind kernel items expected]
            [["map" w/collections-map-build-transient pairs (reduce + (map inc indices))]
             ["set" w/collections-set-build-transient keys calls]]]
      (let [batch #(kernel nil items n probes calls)]
        (dotimes [_ 10] (assert (= expected (batch))))
        (let [samples (repeatedly 7 #(profile/measure (batch)))]
          (assert (every? #(= expected (:result %)) samples))
          (println kind n
                   (/ (* 1000000.0 (median (map :real-ms samples))) elements)
                   (/ (double (median (map :allocated-bytes samples))) elements)))))))
