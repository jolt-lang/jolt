;; seqs — LAZY-SEQ + higher-order-function pipelines: the cost of idiomatic
;; Clojure seq code (range -> map -> filter -> reduce, short-circuiting every?,
;; unbounded iterate/take, mapcat realization). Isolates lazy-seq allocation and
;; per-element HOF call overhead, an axis distinct from the persistent-collection
;; churn `collections` measures and the tight-loop arithmetic `fib`/`mandelbrot`
;; measure. jolt is at or ahead of the JVM on tight loops but pays on lazy seqs,
;; so this is the axis idiomatic pipelines (and ys-style programs) actually hit.
;;
;; All arithmetic is kept inside fixnum range with `mod MOD`, so the checksum is
;; identical on jolt, JVM Clojure, and babashka (no long-overflow divergence) and
;; the time is seq machinery, not bignum promotion.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh seqs 20000
(ns seqs)

(def MOD 1000000007)

;; range -> map -> filter -> map -> reduce: a classic lazy pipeline, every stage
;; a fresh lazy seq with a per-element closure call.
(defn pipeline [n]
  (reduce (fn [a x] (mod (+ a x) MOD)) 0
          (map (fn [x] (mod (* x 7) MOD))
               (filter odd?
                       (map inc (range n))))))

;; short-circuiting every? over many small ranges — the `prime?` shape, where
;; every?'s lazy scan and early exit dominate.
(defn scan [n]
  (reduce (fn [a k]
            (if (every? (fn [d] (not (zero? (rem k d)))) (range 2 (min k 40)))
              (mod (+ a k) MOD) a))
          0 (range 2 n)))

;; unbounded lazy seq realized with take — iterate builds cells on demand.
(defn realize [n]
  (reduce (fn [a x] (mod (+ a x) MOD)) 0
          (take n (iterate (fn [x] (mod (+ x 3) MOD)) 1))))

;; mapcat expansion realized into a vector — flattening + collection build.
(defn expand [n]
  (count (into [] (mapcat (fn [x] (list x (inc x))) (range n)))))

(defn run [n]
  (mod (+ (pipeline (* n 40))
          (scan n)
          (realize (* n 40))
          (expand (* n 20)))
       MOD))

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 20000)]
    (dotimes [_ 2] (run (quot n 4)))                        ; warmup
    (let [runs 3
          times (mapv (fn [_]
                        (let [t0 (System/nanoTime)
                              r (run n)
                              ms (/ (- (System/nanoTime) t0) 1000000.0)]
                          [ms r]))
                      (range runs))
          mss (mapv first times)
          mean (/ (reduce + mss) runs)]
      (println "seqs n" n "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
