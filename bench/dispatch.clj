;; dispatch — a POLYMORPHIC-DISPATCH stress test. A protocol method is called in
;; a hot loop over a heterogeneous (megamorphic) collection of record types, with
;; minimal per-call work, so protocol dispatch dominates. This is the regime
;; devirtualization and the inline-cache target, and the one the ray
;; tracer can't reveal — its dispatch is monomorphic and a small fraction of the
;; float-math cost (devirt measured FLAT there).
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh dispatch 20000
(ns dispatch)

(defprotocol Shape
  (area [s])
  (sides [s]))

(defrecord Circle [r]    Shape (area [_] (* (* 3.14159 r) r))      (sides [_] 0))
(defrecord Square [s]    Shape (area [_] (* s s))                  (sides [_] 4))
(defrecord Triangle [b h] Shape (area [_] (* (* 0.5 b) h))         (sides [_] 3))
(defrecord Rect [w h]    Shape (area [_] (* w h))                  (sides [_] 4))

(defn build-shapes [n]
  (mapv (fn [i]
          (let [k (mod i 4)]
            (cond
              (= k 0) (->Circle (+ 1 (mod i 7)))
              (= k 1) (->Square (+ 1 (mod i 5)))
              (= k 2) (->Triangle (+ 1 (mod i 3)) (+ 2 (mod i 6)))
              :else   (->Rect (+ 1 (mod i 4)) (+ 1 (mod i 8))))))
        (range n)))

;; megamorphic: every element may be a different type -> the call site sees all 4
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
      (println "dispatch iters" iters "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
