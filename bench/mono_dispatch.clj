;; mono-dispatch — protocol dispatch where every call site sees ONE record type
;; (monomorphic). This is the regime where devirtualization and a
;; call-site inline cache CAN fire — the megamorphic `dispatch` bench deliberately
;; defeats them, so this is its complement: it measures how close a proven/cached
;; monomorphic dispatch gets to a direct call. Same per-call work as `dispatch`.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh mono-dispatch 20000
(ns mono-dispatch)

(defprotocol Shape
  (area [s])
  (sides [s]))

(defrecord Circle [r] Shape (area [_] (* (* 3.14159 r) r)) (sides [_] 0))

;; homogeneous: every element is a Circle -> the call site is monomorphic
(defn build-shapes [n]
  (mapv (fn [i] (->Circle (+ 1 (mod i 7)))) (range n)))

(defn sum-area [shapes]
  (reduce (fn [acc s] (+ (+ acc (area s)) (sides s))) 0.0 shapes))

(defn run [iters]
  (let [shapes (build-shapes 1000)]
    (loop [i 0 acc 0.0]
      (if (< i iters)
        (recur (inc i) (+ acc (sum-area shapes)))
        acc))))

(defn -main [& args]
  (let [iters (if (seq args) (Integer/parseInt (first args)) 20000)]
    (dotimes [_ 2] (run (quot iters 4)))                 ; warmup
    (let [runs 3
          times (mapv (fn [_]
                        (let [t0 (System/nanoTime)
                              r (run iters)
                              ms (/ (- (System/nanoTime) t0) 1000000.0)]
                          [ms r]))
                      (range runs))
          mss (mapv first times)
          mean (/ (reduce + mss) runs)]
      (println "mono-dispatch iters" iters "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
