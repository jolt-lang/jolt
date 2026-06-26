;; collections — PERSISTENT-COLLECTION churn. Builds and reads persistent maps
;; and vectors (32-way hash/array tries) under heavy assoc/update/conj/lookup, a
;; word-count-style workload (cf. CLBG k-nucleotide). Exercises jolt's persistent
;; data structures and (eventually) transients — an axis the ray tracer (fixed
;; records, no collections in the hot loop) doesn't touch.
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

(defn sum-vals [m]
  (reduce (fn [acc k] (+ acc (get m k))) 0 (keys m)))

;; vector churn: conj many, then reduce
(defn vec-sum [n]
  (let [v (loop [i 0 v []] (if (< i n) (recur (inc i) (conj v (mod i 1000))) v))]
    (reduce + 0 v)))

(defn run [n]
  (let [m (freq-map n 4096)]
    (+ (sum-vals m) (vec-sum (quot n 4)))))

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
