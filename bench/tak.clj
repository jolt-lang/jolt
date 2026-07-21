;; tak — the Takeuchi function: deeply recursive three-way self-recursion with
;; only integer comparison and subtraction, no allocation. A denser call-overhead
;; probe than `fib` (up to three recursive calls per non-base frame, nested as
;; arguments to a fourth), the classic Gabriel/Larceny benchmark. Isolates
;; function-call throughput and native integer arith, the target of small-fn
;; inlining and self-call direct-linking.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh tak 24
(ns tak)

(defn tak [x y z]
  (if (< y x)
    (tak (tak (- x 1) y z)
         (tak (- y 1) z x)
         (tak (- z 1) x y))
    z))

;; scale the classic (tak n 2n/3 n/3); n=24 -> (tak 24 16 8), a few seconds.
(defn run [n]
  (tak n (quot (* n 2) 3) (quot n 3)))

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 24)]
    (dotimes [_ 2] (run (- n 6)))                           ; warmup
    (let [runs 3
          times (mapv (fn [_]
                        (let [t0 (System/nanoTime)
                              r (run n)
                              ms (/ (- (System/nanoTime) t0) 1000000.0)]
                          [ms r]))
                      (range runs))
          mss (mapv first times)
          mean (/ (reduce + mss) runs)]
      (println "tak n" n "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
