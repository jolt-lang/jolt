;; mathfns — transcendental math throughput: a tight loop over doubles that calls
;; java.lang.Math (sqrt/sin/cos/log/pow/atan2) and accumulates. When the operands
;; are proven flonums, each call lowers to the native Chez op (flsqrt/flsin/…) and
;; types double, so the whole accumulator stays unboxed. This isolates the native
;; Math path and guards it against falling back to the generic string-keyed
;; host-static dispatch (which boxes and re-dispatches per call).
;;
;; The INTEGER half is a separate kernel. java.lang.Math is not only the
;; transcendental surface — abs/min/max/floorDiv/floorMod are integer-in,
;; integer-out, and where the flonum side had been lowering to a native op all
;; along, the integer side had no such path: a Math call on a proven long went
;; through host-static-call, which finds the class's method table and then the
;; method by name, per invocation. `int-kernel` makes that the dominant cost.
;;
;; Portable Clojure (jolt + JVM Clojure) — the JVM turns these into intrinsics, so
;; it's an honest reference.
;;   bench/run.sh mathfns 1000000
(ns mathfns)

(defn kernel ^double [^long n]
  (loop [i 1 acc 0.0]
    (if (<= i n)
      (let [x (* i 1.0e-6)]
        (recur (inc i)
               (+ acc
                  (Math/sqrt x)
                  (Math/sin x)
                  (Math/cos x)
                  (Math/log (+ x 1.0))
                  (Math/pow x 2.0)
                  (Math/atan2 x 1.0))))
      acc)))

;; integer Math over proven longs: no flonums anywhere, so nothing here can take
;; the flonum path and the integer lowering is what is being timed.
(defn int-kernel ^long [^long n]
  (loop [i 1 acc 0]
    (if (<= i n)
      (recur (inc i)
             (unchecked-add
              acc
              (unchecked-add
               (unchecked-add (Math/abs (- i 7)) (Math/min i 1000))
               (unchecked-add (Math/max i 3)
                              (unchecked-add (Math/floorDiv i 3)
                                             (Math/floorMod i 7))))))
      acc)))

(defn run ^double [^long n]
  (+ (kernel n) (* 1.0e-9 (int-kernel n))))

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 1000000)]
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
      (println "mathfns n" n "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
