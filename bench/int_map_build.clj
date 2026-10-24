;; int-map-build — building maps keyed by integers: `into {}` from pairs,
;; `assoc` one key at a time, `assoc!` on a transient, and `zipmap`, then a
;; `get` of every key. The shape of an id-indexed table (entity ids, row ids)
;; built from a query result.
;;
;; What it watches: long hashing (hasheq of a long is not its value), HAMT node
;; growth past the array-map threshold, and the transient edit path, all with
;; long keys rather than the keywords `transients` and `keyed-lookup` use.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh int-map-build 50000
(ns int-map-build)

(defn- ids [n] (map (fn [i] (* i 31)) (range n)))

(defn run [n]
  (let [a (into {} (map (fn [k] [k k]) (ids n)))
        b (reduce (fn [m k] (assoc m k k)) {} (ids n))
        c (persistent! (reduce (fn [m k] (assoc! m k k)) (transient {}) (ids n)))
        d (zipmap (ids n) (range n))
        hits (reduce (fn [s k] (+ s (get a k) (get d k))) 0 (ids n))]
    (+ (count a) (count b) (count c) (count d) hits)))

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 50000)]
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
      (println "int-map-build n" n "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
