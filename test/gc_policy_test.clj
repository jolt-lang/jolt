;; The collector policy (host/chez/rt.ss jolt-install-gc-policy!): the nursery
;; (the trip threshold) grows while collections take a large share of the time
;; and stays at its 16MB floor for a program that allocates little, and
;; JOLT_GC_TRIP_BYTES pins it. Run by `make gcpolicy` as three processes, one per
;; mode, each printing what its nursery ended at.
;;
;; A fixed 16MB nursery made writ's prover spend 40% of its time collecting:
;; medium-lived data (live across a 16MB window, dead soon after) was copied on
;; every young collection and again through the older generations. The shape here
;; is the same: a working set rebuilt constantly, retained just long enough.
(ns gc-policy-test)

(defn churn []
  ;; a working set of 200k small maps, rebuilt from scratch each round and kept
  ;; until the next is built: a fixed ~40MB live, several GB allocated in all.
  ;; (Built from the index, not from the previous round's values, so it cannot
  ;; grow: a (str x) of the previous element doubles every round.)
  (loop [i 0 window []]
    (if (< i 60)
      (recur (inc i) (mapv (fn [j] {:i j :s (str j "-" i)}) (range 200000)))
      (count window))))

;; For the ceiling: 600k small maps held (~100MB, promoted through the
;; generations by scheduled collections, which is what copies them) while the
;; churn runs beside them.
(def ^:private held (atom nil))
(defn- hold-and-churn []
  (reset! held (vec (map (fn [i] {:i i :s (str "held-" i)}) (range 600000))))
  (jolt.host/reset-maximum-memory-bytes!)
  (loop [i 0 window []]
    (if (< i 40)
      (recur (inc i) (mapv (fn [j] {:i j :s (str j "-" i)}) (range 200000)))
      (+ (count window) (count @held)))))

(defn -main [mode]
  (case mode
    "churn" (churn)
    "light" (reduce + (range 1000))
    "pinned" (churn)
    ;; the heap ceiling bounds the TOTAL heap, as -Xmx does: live data, nursery
    ;; and the free memory kept (the gate reads the high-water mark and the GC log)
    "ceiling" (hold-and-churn))
  (println "trip" (jolt.host/gc-trip-bytes))
  (when (= mode "ceiling")
    (println "peak" (jolt.host/maximum-memory-bytes) "max" (.maxMemory (Runtime/getRuntime)))))

(apply -main *command-line-args*)
