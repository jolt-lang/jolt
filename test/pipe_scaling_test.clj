;; Writing to a piped stream must cost O(1) per chunk, not O(chunks-queued).
;;
;; The pipe buffer (jpipe in host/chez/java/io-streams.ss) was a plain list and
;; every write re-copied it ((append chunks (list chunk))), so piping N chunks
;; cost O(N^2) cons work plus N^2 GC pressure — all under the pipe mutex, which
;; is the buffer every ring.util.io/piped-input-stream response body flows
;; through. The fix keeps a front/rear list pair (the standard amortized-O(1)
;; queue) and a running byte count so .available is O(1) too.
;;
;; Like read_scaling_test.clj this asserts the SHAPE in one run: quadrupling the
;; chunk count must not quadruple the per-chunk cost. Both arms are measured
;; best-of-3 in this process and only their ratio is judged — linear lands near
;; 4, the old quadratic near 16, and nothing here is an absolute time. The
;; write-then-drain order is deterministic (the pipe queue is unbounded), so no
;; second thread is involved. Behavior — byte order through the queue, available,
;; end-of-stream — is pinned in test/chez/unit.edn suite "piped-streams"; this
;; file is only about cost.

(ns pipe-scaling-test)

(def ^:private chunk-bytes 64)

(defn- pump
  "Write n 64-byte chunks, close the write end, drain. [total-bytes first-byte]"
  [n]
  (let [in (java.io.PipedInputStream.)
        out (java.io.PipedOutputStream. in)
        chunk (byte-array chunk-bytes (byte 7))
        buf (byte-array 4096)]
    (dotimes [_ n] (.write out chunk))
    (.close out)
    (loop [total 0 probe -2]
      (let [r (.read in buf)]
        (if (neg? r)
          [total probe]
          (recur (+ total r) (if (= probe -2) (aget buf 0) probe)))))))

;; This thread's CPU time, not the wall clock: under make -j beside other gates, a
;; preempted sample reads as a regression. The work timed here all runs on this
;; thread, collections included.
(def ^:private thread-mx (java.lang.management.ManagementFactory/getThreadMXBean))
(defn- cpu-ns [] (.getCurrentThreadCpuTime thread-mx))

(defn- timed [f]
  (let [t (cpu-ns)]
    (f)
    (/ (- (cpu-ns) t) 1e6)))

;; Best of 3 per arm, the samples ALTERNATING arms (hotpath_scaling_test.clj's
;; judge-staged), so a burst on a shared runner lands on both arms rather than
;; on the 4x arm's whole block. Three, not five, because a regressed quadratic
;; 4x arm costs seconds per sample. No collection before each sample, unlike
;; the sibling gates: here it moved the 4x arm from ~4.6 to 5.1-6.1.
(defn- best-pair [f1 f4]
  (let [ps (doall (repeatedly 3 #(vector (timed f1) (timed f4))))]
    [(max 0.05 (reduce min (map first ps))) (reduce min (map second ps))]))

;; n1 is big enough that the linear implementation still measures above timer
;; noise (~10ms of writes+reads) and small enough that the REGRESSED quadratic
;; finishes and fails rather than hanging — a gate that hangs reports nothing.
(def ^:private n1 8000)
(def ^:private factor 4)
(def ^:private max-ratio 8.0)

(defn -main [& _]
  (let [[c1 p1] (pump n1)              ; warm + verify small
        [c4 p4] (pump (* factor n1))]  ; verify big
    (when-not (and (= c1 (* n1 chunk-bytes)) (= c4 (* factor n1 chunk-bytes))
                   (= p1 7) (= p4 7))
      (println (str "FAIL pipe-scaling: wrong bytes through the pipe — got "
                    [c1 p1] " and " [c4 p4]))
      (System/exit 1))
    (let [report (fn [[t1 t4] note]
                   (let [ratio (/ t4 t1)]
                     (println (format "pipe-scaling: %d chunks %.2fms, %d chunks %.2fms, ratio %.2f (linear ~%.1f, quadratic ~%.1f, ceiling %.1f)%s"
                                      n1 t1 (* factor n1) t4 ratio
                                      (double factor) (double (* factor factor)) max-ratio note))
                     ratio))
          measure #(best-pair (fn [] (pump n1)) (fn [] (pump (* factor n1))))
          m (measure)
          ;; a miss is measured once more before it counts: quadratic reads
          ;; ~16 both times, a burst rarely spoils two measurements in a row
          ratio (if (<= (/ (second m) (first m)) max-ratio)
                  (report m "")
                  (do (report m " — over the ceiling, measuring again")
                      (report (measure) "")))]
      (if (> ratio max-ratio)
        (do (println (str "FAIL pipe-scaling: piping scaled worse than linearly in the chunk count. "
                          "pipe-write! is re-copying the queued chunk list per write "
                          "(jpipe front/rear queue in host/chez/java/io-streams.ss)."))
            (System/exit 1))
        (println "pipe-scaling: passed")))))

(-main)
