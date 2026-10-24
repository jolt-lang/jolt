;; pool-tasks — fan out 64 small tasks and join them, over and over: once onto
;; a fixed pool of 8 through submit/.get, once through `future`/`deref`. Each
;; task does a little real work (a 200-element reduce) so the pool has something
;; to overlap, but the cost per round is dominated by the dispatch and the join.
;;
;; What it watches: what `executors` does not — a burst of tasks onto an
;; already-warm pool of fixed size, the queue handoff to workers that are
;; parked rather than created, and the future's latch on the way back. It prints
;; the pool/future split above `mean:`; `mean:` is the row.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh pool-tasks 400
(ns pool-tasks
  (:import [java.util.concurrent Executors ExecutorService Callable Future]))

(defn- work [i] (reduce + i (range 200)))

(defn pool-rounds [^ExecutorService ex rounds]
  (loop [r 0 acc 0]
    (if (< r rounds)
      (let [fs (mapv (fn [i] (.submit ex ^Callable (fn [] (work i)))) (range 64))]
        (recur (inc r) (reduce (fn [s ^Future f] (+ s (long (.get f)))) acc fs)))
      acc)))

(defn future-rounds [rounds]
  (loop [r 0 acc 0]
    (if (< r rounds)
      (let [fs (mapv (fn [i] (future (work i))) (range 64))]
        (recur (inc r) (reduce (fn [s f] (+ s (long @f))) acc fs)))
      acc)))

(defn run [ex rounds]
  (+ (pool-rounds ex rounds) (future-rounds rounds)))

(defn- ms-since [t0] (/ (- (System/nanoTime) t0) 1000000.0))
(defn- r1 [x] (/ (Math/round (* x 10.0)) 10.0))

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 400)
        ex (Executors/newFixedThreadPool 8)]
    (dotimes [_ 2] (run ex (quot n 4)))                  ; warmup
    (let [runs 3
          ;; the phase split, for reading by hand — `mean:` below is the row
          tp (let [t0 (System/nanoTime)] (pool-rounds ex n) (ms-since t0))
          tf (let [t0 (System/nanoTime)] (future-rounds n) (ms-since t0))
          times (mapv (fn [_]
                        (let [t0 (System/nanoTime)
                              r (run ex n)]
                          [(ms-since t0) r]))
                      (range runs))
          mss (mapv first times)
          mean (/ (reduce + mss) runs)]
      (.shutdown ex)
      (println "pool-tasks n" n "result" (second (first times)))
      (println "phases: pool" (r1 tp) "ms  future" (r1 tf) "ms")
      (println "runs:" (mapv r1 mss))
      (println "mean:" (r1 mean) "ms")
      (shutdown-agents))))
