;; loop-recur — tight LOOP/RECUR iteration: jolt's core counted-loop construct
;; with no seq or collection allocation in the hot path. Isolates the recur
;; back-edge and fixnum arithmetic, including nested loops and a data-dependent
;; branch in the body (the shape idiomatic imperative-style Clojure compiles to).
;; Complements `fib`/`tak` (self-recursion via the call stack) and `seqs` (the
;; lazy-seq alternative to an explicit loop) — this is the allocation-free loop.
;;
;; All arithmetic is kept in fixnum range with `mod MOD`, so the checksum is
;; identical on jolt, JVM Clojure, and babashka and the time is loop machinery.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh loop-recur 20000
(ns loop-recur)

(def MOD 1000000007)

;; single counted loop: accumulate i*i, the plain recur back-edge + fixnum arith.
(defn sum-squares [n]
  (loop [i 0 acc 0]
    (if (< i n)
      (recur (inc i) (mod (+ acc (* i i)) MOD))
      acc)))

;; nested loop/recur: an inner accumulate inside an outer counted loop.
(defn nested [n]
  (loop [i 0 acc 0]
    (if (< i n)
      (recur (inc i)
             (loop [j 0 a acc]
               (if (< j 64)
                 (recur (inc j) (mod (+ a (bit-xor i j)) MOD))
                 a)))
      acc)))

;; data-dependent branch in the loop body: Collatz step counts, where each
;; iteration picks one of two recur arms. Trajectories stay well within fixnum.
(defn collatz [n]
  (loop [k 1 total 0]
    (if (> k n)
      total
      (recur (inc k)
             (+ total
                (loop [x k steps 0]
                  (if (== x 1)
                    steps
                    (recur (if (even? x) (quot x 2) (inc (* 3 x)))
                           (inc steps)))))))))

(defn run [n]
  (mod (+ (sum-squares (* n 20))
          (nested n)
          (collatz n))
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
      (println "loop-recur n" n "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
