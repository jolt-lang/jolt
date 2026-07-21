;; transducers — TRANSDUCER pipelines: the same map/filter/take shapes as `seqs`,
;; but composed as transducers (`comp`) and driven allocation-free through
;; `transduce` / `into` / `eduction`. Isolates the transducer machinery (composed
;; reducing functions, one accumulator, no per-stage lazy-seq cells) against the
;; lazy pipeline `seqs` measures on the same source data — the read-side pattern
;; idiomatic Clojure reaches for when it wants a pipeline without the thunk chain.
;;
;; All arithmetic is kept in fixnum range with `mod MOD`, so the checksum is
;; identical on jolt, JVM Clojure, and babashka.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh transducers 20000
(ns transducers)

(def MOD 1000000007)

(def xform
  (comp (map inc)
        (filter odd?)
        (map (fn [x] (mod (* x 7) MOD)))))

;; transduce: fold the composed xform straight into an accumulator, no seq built.
(defn td [n]
  (transduce xform
             (fn ([a] a) ([a x] (mod (+ a x) MOD)))
             0
             (range n)))

;; into with a transducer (incl. take): build a vector through the xform, which
;; runs transient-backed, then reduce it.
(defn into-xf [n]
  (reduce + 0 (into [] (comp xform (take (quot n 2))) (range n))))

;; eduction: a reducible/seqable view over the xform, realized by reduce.
(defn edu [n]
  (reduce (fn [a x] (mod (+ a x) MOD)) 0
          (eduction xform (range n))))

(defn run [n]
  (mod (+ (td (* n 40))
          (into-xf (* n 20))
          (edu (* n 40)))
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
      (println "transducers n" n "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
