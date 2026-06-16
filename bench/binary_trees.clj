;; binary-trees (Computer Language Benchmarks Game) — an ALLOCATION/GC stress
;; test. Builds and discards millions of short-lived `Node` records; the nodes
;; ESCAPE (stored in the tree, walked later), so this is the regime jolt-8flj
;; (escape analysis) targets and the ray tracer never exercises (~7% alloc).
;;
;; Portable Clojure: runs on jolt and JVM Clojure for cross-impl comparison.
;;   jolt -m binary-trees 14        (JOLT_DIRECT_LINK=1 JOLT_WHOLE_PROGRAM=1)
;;   clojure -M -m binary-trees 14
(ns binary-trees)

(defrecord Node [left right])

(defn make-tree [depth]
  (if (zero? depth)
    (->Node nil nil)
    (->Node (make-tree (dec depth)) (make-tree (dec depth)))))

(defn check-tree [node]
  (let [l (:left node)]
    (if (nil? l)
      1
      (+ (+ 1 (check-tree l)) (check-tree (:right node))))))

(defn run [max-depth]
  (let [min-depth 4
        stretch-depth (inc max-depth)
        _ (check-tree (make-tree stretch-depth))
        long-lived (make-tree max-depth)]
    (loop [d min-depth acc 0]
      (if (<= d max-depth)
        (let [iterations (bit-shift-left 1 (+ (- max-depth d) min-depth))
              sum (loop [i 0 s 0]
                    (if (< i iterations)
                      (recur (inc i) (+ s (check-tree (make-tree d))))
                      s))]
          (recur (+ d 2) (+ acc sum)))
        ;; touch the long-lived tree so it isn't dead-code-eliminated
        (+ acc (check-tree long-lived))))))

(defn -main [& args]
  (let [max-depth (if (seq args) (Integer/parseInt (first args)) 14)]
    (dotimes [_ 2] (run (min max-depth 10)))            ; warmup
    (let [runs 3
          times (mapv (fn [_]
                        (let [t0 (System/nanoTime)
                              r (run max-depth)
                              ms (/ (- (System/nanoTime) t0) 1000000.0)]
                          [ms r]))
                      (range runs))
          mss (mapv first times)
          mean (/ (reduce + mss) runs)]
      (println "binary-trees depth" max-depth "checksum" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
