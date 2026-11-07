;; chunk-append must cost O(1) amortized per item, not O(items-buffered).
;;
;; The chunk-builder API (clojure.core/chunk-buffer + chunk-append + chunk,
;; jolt-chunkbuf in host/chez/natives-transduce.ss) buffered items as a plain
;; list re-copied per append ((append items (list x))), so filling a buffer of n
;; cost O(n^2) — and the cap argument was silently ignored. The fix stores a
;; vector + count (growing when appends outrun the declared cap), which also
;; makes cap mean something.
;;
;; Same judgment as read_scaling_test.clj: the SHAPE, in one run. Quadrupling
;; the buffer size must not quadruple the per-item cost; only the in-process
;; ratio of two balanced arms is judged (below). Sealed-chunk contents are
;; verified per build here, and the JVM-portable behavior rows live in the corpus
;; ("chunk-builder API"); cap overflow (a jolt superset — the JVM throws) is
;; pinned in test/chez/unit.edn.

(ns chunk-scaling-test)

(defn- build
  "Fill a chunk-buffer of capacity n, seal it, spot-check the contents."
  [n]
  (let [b (chunk-buffer n)]
    (dotimes [i n] (chunk-append b i))
    (let [c (chunk b)]
      (and (= (count c) n) (= (nth c 0) 0) (= (nth c (dec n)) (dec n))))))

;; This thread's CPU time, not the wall clock: under make -j beside other gates, a
;; preempted sample reads as a regression. The work timed here all runs on this
;; thread, collections included.
(def ^:private thread-mx (java.lang.management.ManagementFactory/getThreadMXBean))
(defn- cpu-ns [] (.getCurrentThreadCpuTime thread-mx))

(defn- timed [f]
  (let [t (cpu-ns)]
    (f)
    (/ (- (cpu-ns) t) 1e6)))

;; Best of 3 per arm, the samples ALTERNATING arms and each from a fresh
;; collection, as hotpath_scaling_test.clj's judge-staged does, so a burst on a
;; shared runner lands on both arms. Milliseconds from the nanosecond clock.
(defn- best-pair [f1 f4]
  (let [ps (doall (repeatedly 3 #(vector (do (System/gc) (timed f1))
                                         (do (System/gc) (timed f4)))))]
    [(max 0.05 (reduce min (map first ps))) (reduce min (map second ps))]))

;; The arms are balanced to the same wall time for a linear implementation, as
;; hotpath_scaling_test.clj's judge-scaling: 4*reps builds of n1 against reps
;; builds of 4*n1, so linear reads ~1 and quadratic ~4. A single build of 4000
;; is ~0.06ms, which the millisecond clock this gate used read as 0 on both
;; arms; repeated, each arm is ~1ms. Small enough that the REGRESSED quadratic
;; (~128M conses per 4x build) still finishes and fails rather than hanging.
(def ^:private n1 4000)
(def ^:private factor 4)
(def ^:private reps 4)
(def ^:private max-ratio 2.0)

(defn -main [& _]
  (when-not (and (build n1) (build (* factor n1)))
    (println "FAIL chunk-scaling: sealed chunk has wrong count or contents")
    (System/exit 1))
  (let [report (fn [[t1 t4] note]
                 (let [ratio (/ t4 t1)]
                   (println (format "chunk-scaling: %dx%d appends %.2fms, %dx%d appends %.2fms, ratio %.2f (linear ~1.0, quadratic ~%.1f, ceiling %.1f)%s"
                                    (* factor reps) n1 t1 reps (* factor n1) t4 ratio
                                    (double factor) max-ratio note))
                   ratio))
        measure #(best-pair (fn [] (dotimes [_ (* factor reps)] (build n1)))
                            (fn [] (dotimes [_ reps] (build (* factor n1)))))
        m (measure)
        ;; a miss is measured once more before it counts
        ratio (if (<= (/ (second m) (first m)) max-ratio)
                (report m "")
                (do (report m " — over the ceiling, measuring again")
                    (report (measure) "")))]
    (if (> ratio max-ratio)
      (do (println (str "FAIL chunk-scaling: chunk-append scaled worse than linearly in the buffer size. "
                        "na-chunk-append is re-copying the buffered items per append "
                        "(jolt-chunkbuf in host/chez/natives-transduce.ss)."))
          (System/exit 1))
      (println "chunk-scaling: passed"))))

(-main)
