;; arrays — primitive double-array throughput: fill an array with aset and read
;; it back with aget in tight loops, no boxing, no collections, no dispatch. This
;; isolates the unboxed primitive-array path (a ^doubles array is a Chez flvector;
;; aget/aset lower to flvector-ref/-set! and the surrounding arith to fl+/fl*), so
;; it tracks that codegen directly and guards it against regression. mandelbrot
;; covers scalar double arith; this covers indexed array reads/writes.
;;
;; Portable Clojure (jolt + JVM Clojure) — ^doubles/aget/aset hit primitive arrays
;; on both.
;;   bench/run.sh arrays 40000
(ns arrays)

(defn fill! [^doubles a ^long n]
  (loop [i 0]
    (when (< i n)
      (aset a i (double (+ (* i 0.5) 1.0)))
      (recur (inc i))))
  a)

(defn dot ^double [^doubles a ^doubles b ^long n]
  (loop [i 0 acc 0.0]
    (if (< i n)
      (recur (inc i) (+ acc (* (aget a i) (aget b i))))
      acc)))

;; `passes` dot products over two n-element arrays; each pass reads 2n elements
;; unboxed and does n fused multiply-adds. The arrays are filled once (the aset
;; path) then read every pass (the aget path).
(defn run ^double [^long passes]
  (let [n 1000
        a (fill! (double-array n) n)
        b (fill! (double-array n) n)]
    (loop [p 0 acc 0.0]
      (if (< p passes)
        (recur (inc p) (+ acc (dot a b n)))
        acc))))

(defn -main [& args]
  (let [passes (if (seq args) (Integer/parseInt (first args)) 40000)]
    (dotimes [_ 2] (run (quot passes 2)))                ; warmup
    (let [runs 3
          times (mapv (fn [_]
                        (let [t0 (System/nanoTime)
                              r (run passes)
                              ms (/ (- (System/nanoTime) t0) 1000000.0)]
                          [ms r]))
                      (range runs))
          mss (mapv first times)
          mean (/ (reduce + mss) runs)]
      (println "arrays passes" passes "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
