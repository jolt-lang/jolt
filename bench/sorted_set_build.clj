;; sorted-set-build — building sorted sets the ways app code does: `into` from
;; scrambled longs, `conj` one at a time in ascending order, `into` from strings
;; (compare goes through String.compareTo), and `disj` of every other element.
;; `sorted-build` folds one set build into a map-heavy row; this is the set
;; alone, at a size where the tree depth is real.
;;
;; What it watches: tree-ins and rebalancing on the set's tree, the default
;; comparator's dispatch on longs and strings, and tree removal.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh sorted-set-build 50000
(ns sorted-set-build)

(defn- scrambled [n] (map (fn [i] (mod (* i 7919) n)) (range n)))

(defn run [n]
  (let [a (into (sorted-set) (scrambled n))
        b (loop [i 0 s (sorted-set)] (if (< i n) (recur (inc i) (conj s i)) s))
        c (into (sorted-set) (map (fn [i] (str "s" i)) (scrambled (quot n 4))))
        d (reduce disj a (range 0 n 2))]
    (+ (count a) (count b) (count c) (count d) (first a) (first d))))

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
      (println "sorted-set-build n" n "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
