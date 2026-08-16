;; nth — INDEXED ELEMENT ACCESS, and specifically its constant cost.
;;
;; RT.nth tests Indexed first and returns immediately. jolt's jolt-nth is
;; set!-wrapped several times, with the array shim outermost, so before the pvec
;; case was hoisted a plain vector read paid a chain of extension-type probes
;; (jolt-array?, the ArrayList/StringBuilder shim, lazyseq, transient) and three
;; chained calls to reach the arm that answers it.
;;
;; This is a CONSTANT factor, not a complexity shape, which is why it is a
;; benchmark and not a CI gate: stating it in-process means calibrating nth
;; against some other operation, and that ratio moves with the machine. Two CI
;; runners once measured 2.79x and 5.14x for the same commit against a 5.0
;; ceiling, while the regression being watched for lands where those ranges
;; overlap. See the note in test/complexity_test.clj.
;;
;; Small vectors are the shape that matters: red-black tree nodes and tuples are
;; five elements, and nth was ~20% of the time in tree code.
;;
;; Reference figures, one machine, forced rebuilds on both arms:
;;
;;   nth small vector      34.34 ns with the chain   15.95 hoisted
;;   nth large vector      47.44                     29.38
;;   nth with a default    27.22                      9.68
;;   count (control)        4.96                      5.09
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh nth-access 1000000
(ns nth-access)

(defn- reads [small large reps]
  (loop [i 0 acc 0]
    (if (< i reps)
      (recur (inc i)
             (-> acc
                 (+ (nth small 2))            ; tuple / tree-node shape
                 (+ (nth large 90000))        ; trie descent
                 (+ (nth small 9 0))          ; miss, with a default
                 (+ (count small))))          ; control: same record, inline arm
      acc)))

;; Collections are built ONCE, outside the measured region — this is about the
;; cost of READING them.
(defn build [_]
  [(vec [10 20 30 40 50]) (vec (range 200000))])

(defn run [state reps]
  (let [[small large] state]
    (reads small large reps)))

(defn -main [& args]
  (let [reps (if (seq args) (Integer/parseInt (first args)) 1000000)
        state (build reps)]
    (dotimes [_ 2] (run state (quot reps 4)))               ; warmup
    (let [runs 3
          times (mapv (fn [_]
                        (let [t0 (System/nanoTime)
                              r (run state reps)
                              ms (/ (- (System/nanoTime) t0) 1000000.0)]
                          [ms r]))
                      (range runs))
          mss (mapv first times)
          mean (/ (reduce + mss) runs)]
      (println "nth-access reps" reps "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
