;; fib — naive recursive Fibonacci: pure function-call + integer-arithmetic
;; throughput, with no allocation, dispatch, or collections. Isolates call
;; overhead and native integer arith, and is the natural target for
;; single-call-site / small-fn inlining and self-call direct-linking.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh fib 32
(ns fib)

(defn fib [n]
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

(defn run [n] (fib n))

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 32)]
    (dotimes [_ 2] (run (- n 6)))                        ; warmup
    (let [runs 3
          times (mapv (fn [_]
                        (let [t0 (System/nanoTime)
                              r (run n)
                              ms (/ (- (System/nanoTime) t0) 1000000.0)]
                          [ms r]))
                      (range runs))
          mss (mapv first times)
          mean (/ (reduce + mss) runs)]
      (println "fib n" n "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
