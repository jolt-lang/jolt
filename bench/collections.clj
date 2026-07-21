;; collections — PERSISTENT-COLLECTION churn. Builds and reads persistent maps
;; and vectors (32-way hash/array tries) under heavy assoc/update/conj/lookup, a
;; word-count-style workload (cf. CLBG k-nucleotide), then consumes the built
;; vector with a map/filter/take + reduce pipeline. Covers persistent map,
;; reduce, and map/filter/take over materialized collections — an axis the ray
;; tracer (fixed records, no collections in the hot loop) doesn't touch.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh collections 200000
(ns collections)

;; map churn: accumulate a frequency map over a stream of keys, then sum it back
(defn freq-map [n buckets]
  (loop [i 0 m {}]
    (if (< i n)
      (recur (inc i)
             (let [k (mod (* i 2654435761) buckets)]
               (assoc m k (+ 1 (get m k 0)))))
      m)))

;; reduce over a persistent map: sum the values by looking each key back up.
(defn sum-vals [m]
  (reduce (fn [acc k] (+ acc (get m k))) 0 (keys m)))

;; vector churn: conj many into a persistent vector.
(defn build-vec [n]
  (loop [i 0 v []] (if (< i n) (recur (inc i) (conj v (mod i 1000))) v)))

;; map/filter/take pipeline over the built vector, then reduce — the read-side
;; consumption of a materialized persistent collection (distinct from `seqs`,
;; whose source is an unbounded lazy range).
(defn transform [v]
  (reduce + 0
          (take (quot (count v) 2)
                (map (fn [x] (* x 3))
                     (filter even? v)))))

(defn run [n]
  (let [m (freq-map n 4096)
        v (build-vec (quot n 4))]
    (+ (sum-vals m)                                          ; persistent map + reduce
       (reduce + 0 v)                                        ; vector reduce
       (transform v))))                                      ; map/filter/take + reduce

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 200000)]
    (dotimes [_ 2] (run (quot n 4)))                     ; warmup
    (let [runs 3
          times (mapv (fn [_]
                        (let [t0 (System/nanoTime)
                              r (run n)
                              ms (/ (- (System/nanoTime) t0) 1000000.0)]
                          [ms r]))
                      (range runs))
          mss (mapv first times)
          mean (/ (reduce + mss) runs)]
      (println "collections n" n "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
