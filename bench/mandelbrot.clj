;; mandelbrot — pure floating-point compute: for each point of an NxN grid,
;; iterate z = z^2 + c up to a cap and count iterations. No allocation, no
;; dispatch, no collections in the hot loop — just double arithmetic and tight
;; recur loops. This isolates the irreducible-math axis the ray tracer is bound
;; on (where devirt/alloc passes measured flat), so it tracks native-arith codegen
;; and loop quality directly.
;;
;; Portable Clojure (jolt + JVM Clojure). The jolt.png picture demo lives in
;; mandelbrot_png.clj so this file stays portable for the JVM reference run.
;;   bench/run.sh mandelbrot 1000
(ns mandelbrot)

(defn count-point [cr ci cap]
  (loop [i 0 zr 0.0 zi 0.0]
    (if (or (>= i cap) (> (+ (* zr zr) (* zi zi)) 4.0))
      i
      (recur (inc i)
             (+ (- (* zr zr) (* zi zi)) cr)
             (+ (* 2.0 (* zr zi)) ci)))))

(defn run [n]
  (let [cap 200
        nd (* 1.0 n)]
    (loop [y 0 acc 0]
      (if (< y n)
        (let [ci (- (/ (* 2.0 y) nd) 1.0)
              row (loop [x 0 a 0]
                    (if (< x n)
                      (let [cr (- (/ (* 2.0 x) nd) 1.5)]
                        (recur (inc x) (+ a (count-point cr ci cap))))
                      a))]
          (recur (inc y) (+ acc row)))
        acc))))

(defn- run-bench [args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 1000)]
    (dotimes [_ 2] (run (quot n 2)))                     ; warmup
    (let [runs 3
          times (mapv (fn [_]
                        (let [t0 (System/nanoTime)
                              r (run n)
                              ms (/ (- (System/nanoTime) t0) 1000000.0)]
                          [ms r]))
                      (range runs))
          mss (mapv first times)
          mean (/ (reduce + mss) runs)]
      (println "mandelbrot n" n "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))

(defn -main [& args]
  (run-bench args))
