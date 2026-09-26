;; Lazy realization must cost the same whether or not a thread has ever existed.
;;
;; A lazy cell (host/chez/lazy-bridge.ss) and a seq cell with a deferred tail
;; (host/chez/seq.ss) publish their result through ONE field: the tail slot holds
;; the thunk until it is forced and the seq after, told apart by type. A reader
;; therefore never locks: it loads the slot and either has its answer or takes the
;; slow path. Exclusion is needed only to run a thunk once, and the mutex for that
;; is borrowed from a pool for the duration of the force, so a program pays no
;; per-cell mutex — a Chez mutex is a finalized object the collector has to visit,
;; and paying one per lazy element made every collection ~120x dearer the moment a
;; single thread had been forked, for the rest of the process.
;;
;; Two assertions:
;;
;;   ONCE-ONLY — eight threads walk the same unrealized seqs at once. Every
;;   producer runs exactly once per element, every walker sees the same values,
;;   and a producer that throws throws the SAME failure to every walker. This is
;;   the contract the lock-free read path must not weaken.
;;
;;   SCALING — one workload, timed before any thread exists and again after one
;;   has existed, in ONE process. The ratio is the judge: the per-cell mutex
;;   design measures ~5 (the second arm is mostly collector), the claim ~1.5.
;;   Only the ratio is read, so machine speed and load do not matter.

(ns lazyseq-mt-scaling-test)

(def ^:private walkers 8)
(def ^:private n 20000)
;; The claim design measures ~1.5 (the release fence and the counted claim on
;; every first force) and a mutex per cell ~5; the line sits well above the
;; first with room for a loaded CI runner, and well below the failure it guards.
(def ^:private max-ratio 2.5)

(defn- fail [msg]
  (println (str "FAIL lazyseq-mt-scaling: " msg))
  (System/exit 1))

(defn- gen
  "A user lazy-seq producer: one thunk per element, counted."
  [calls i]
  (lazy-seq
    (swap! calls inc)
    (when (< i n) (cons i (gen calls (inc i))))))

(defn- run-walkers
  "Start `walkers` threads that each run f, return their results in order."
  [f]
  (let [fs (doall (repeatedly walkers #(future (f))))]
    (mapv deref fs)))

(defn- check-once-only []
  ;; a user lazy-seq chain: every cell forced by 8 racing threads
  (let [calls (atom 0)
        s (gen calls 0)
        sums (run-walkers #(reduce + 0 s))]
    (when-not (apply = sums) (fail (str "walkers disagree over a lazy-seq chain: " sums)))
    (when-not (= (inc n) @calls)
      (fail (str "lazy-seq bodies ran " @calls " times for " (inc n) " cells — a thunk ran twice"))))
  ;; a native producer chain (map over an unchunked source): a cseq tail thunk per element
  (let [calls (atom 0)
        s (map (fn [x] (swap! calls inc) x) (take n (iterate inc 0)))
        sums (run-walkers #(reduce + 0 s))]
    (when-not (apply = sums) (fail (str "walkers disagree over a map chain: " sums)))
    (when-not (= n @calls)
      (fail (str "map's fn ran " @calls " times for " n " elements — a tail thunk ran twice"))))
  ;; a chunked source: the vector-backed tails are computed, not run, and must
  ;; still agree
  (let [s (map inc (vec (range n)))
        sums (run-walkers #(reduce + 0 s))]
    (when-not (apply = sums) (fail (str "walkers disagree over a chunked chain: " sums))))
  ;; a failing producer: every racing walker gets the exception, none hangs on
  ;; the claim the failed force leaves behind, and none sees an empty seq. The
  ;; body runs again for each force, as the reference's LazySeq keeps fn until
  ;; invoke returns (a ^:once body's captured locals are cleared by then, so this
  ;; one captures nothing and fails the same way every time).
  (let [s (lazy-seq (throw (ex-info "boom" {:once true})))
        msgs (run-walkers #(try (doall s) :no-throw
                                (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    (when-not (every? #(= {:once true} %) msgs)
      (fail (str "a failing lazy-seq did not fail every walker the same way: " msgs))))
  (println "lazyseq-mt-scaling once-only: 8 racing walkers, every producer ran once, a failure reached all"))

(defn- work []
  ;; allocation-heavy lazy walking: a few lazy cells per iteration, many iterations
  (loop [i 0 acc 0]
    (if (< i 300000)
      (recur (inc i) (+ acc (count (vec (map inc (take 3 (iterate inc i)))))))
      acc)))

(defn- time-ms [f]
  (let [t0 (System/nanoTime)]
    (f)
    (/ (- (System/nanoTime) t0) 1000000.0)))

(defn- check-scaling []
  (work)                                                ; warm
  (let [before (time-ms work)
        t (Thread. (fn [] nil))]
    (.start t) (.join t)                               ; a thread has EXISTED; it need not be alive
    (let [after (time-ms work)
          ratio (/ after before)]
      (println (format "lazyseq-mt-scaling: %.0fms before any thread, %.0fms after one existed, ratio %.2f (ceiling %.1f)"
                       before after ratio max-ratio))
      (when (> ratio max-ratio)
        (fail (str "lazy realization slows down once a thread has existed — a mutex is being "
                   "allocated per lazy cell again (host/chez/seq.ss seq-more / "
                   "host/chez/lazy-bridge.ss force-lazyseq)."))))))

(defn -main [& _]
  ;; scaling first: it needs the single-threaded arm, and once-only forks threads
  (check-scaling)
  (check-once-only)
  (println "lazyseq-mt-scaling: passed"))

(-main)
