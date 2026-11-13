;; transient-builds — `(reduce conj! (transient coll) v)` where `v` is a
;; persistent VECTOR: a vector of elements into a transient vector and a
;; transient set, and a vector of [k v] pairs into a transient map.
;;
;; `transients` builds through `into` and per-element loops over seqs. This is
;; the vector-source shape that `vec`/`mapv`/`zipmap` results feed, and reduce
;; hands it to the transient's bulk inserter: a vector destination reserves
;; once and copies whole leaves, a map or set walks the source leaves and
;; inserts straight into hash slots. Integer keys take the fixnum key-hash
;; path. If this row moves and `transients` does not, look at the reduce arm in
;; seq.ss and the pvec inserters in transients.ss.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh transient-builds 32768
(ns transient-builds)

(defn- build-all [items pairs]
  (+ (count (persistent! (reduce conj! (transient []) items)))
     (count (persistent! (reduce conj! (transient {}) pairs)))
     (count (persistent! (reduce conj! (transient #{}) items)))))

(defn run [n]
  (let [items (vec (range n))
        pairs (mapv (fn [k] [k (inc k)]) items)]
    (loop [i 0 total 0]
      (if (== i 8) total (recur (inc i) (+ total (build-all items pairs)))))))

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 32768)]
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
      (println "transient-builds n" n "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
