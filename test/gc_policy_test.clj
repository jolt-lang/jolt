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

;; For the refresh: a full collection the program asks for measures the live set
;; as the policy's own do, so the older generations' allowance afterwards is sized
;; from what is live NOW. Held data grows the allowance past its 64MB minimum;
;; dropped and collected, it shrinks with the live set. It need not land on the
;; minimum: building the held data can cross the old-generation limit, and a
;; full collection the policy runs then may raise its growth factor. Before,
;; System/gc left the policy's figure where its last own full collection put it.
(defn- refresh []
  (reset! held (vec (map (fn [i] {:i i :s (str "held-" i)}) (range 600000))))
  (System/gc)
  (let [with-held (jolt.host/gc-old-growth-bytes)]
    (reset! held nil)
    (System/gc)
    (println "growth" with-held (jolt.host/gc-old-growth-bytes))))

;; For nepotism: a walk down one long lazy seq keeps nothing it has passed, so
;; what the heap holds after each young collection stays flat. With Chez's own
;; schedule the cell the walk was on at a collection was promoted, then its
;; tail was realized into it, and the dead promoted cell rooted every cell
;; realized after it: the heap after each collection climbed by most of a
;; nursery until a full collection.
(defn- walk []
  (reduce (fn [a x] (mod (+ a x) 1000000007)) 0
          (take 6000000 (iterate (fn [x] (mod (+ x 3) 1000000007)) 1))))

;; For the startup reading: a built binary's first collection comes within a
;; millisecond of the policy starting and reads as most of that millisecond. The
;; share averaged each collection's own ratio, so that one reading doubled the
;; nursery, and the next doubled it again though collection took 2% of it: a 9ms
;; benchmark ran in a 42MB nursery. Fed the same readings, the policy now keeps
;; the floor (a share of summed times, and nothing resized before five
;; collections, as the JVM's AdaptiveSizePolicyReadyThreshold). The live policy
;; is reset first; the readings are nanoseconds (gc, elapsed).
(defn- startup []
  (jolt.host/scheme-eval-string
   "(begin (set! gc-share-gc 0.0) (set! gc-share-el 0.0) (set! gc-share 0.0) (set! gc-seen 0)
           (set! gc-probe-from #f) (set! gc-growth-hold #f) (gc-win-reset!)
           (sa-gc-trip-bytes! gc-trip-floor)
           (gc-size-nursery! 400000 500000)
           (for-each (lambda (i) (gc-size-nursery! 120000 6000000)) '(1 2 3 4 5 6)))"))

;; For the live-set bound and the check on growth past it, decided on readings
;; the test supplies rather than on this machine's timing. The churn loop's own
;; nursery is a timing outcome: past its bound the policy probes (double, and if
;; the share of time rose, one jump to 8x the base, then back and hold), the run
;; is only ~40 collections long, and it ends wherever the probe stands. Main read
;; 84MB, 168MB or 670MB for the same binary (about one run in seven at 670MB,
;; which the old "under 512MB" check failed). Here each scenario resets the
;; policy, takes ~64MB as the live set (so the bound is 64MB) and feeds 120
;; collections whose share of time is a function of the nursery they ran in;
;; the path of sizes it leaves is exact. Run with JOLT_MAX_HEAP=off, so the cap
;; is the 1GB one on every machine. Interrupts are off while it runs, so no real
;; collection feeds the policy in between.
(def ^:private replay-src
  "(lambda (live n share-at)
     (with-interrupts-disabled
       (set! gc-share-gc 0.0) (set! gc-share-el 0.0) (set! gc-share 0.0) (set! gc-seen 0)
       (set! gc-probe-from #f) (set! gc-probe-jumped? #f) (set! gc-growth-hold #f) (set! gc-hold-n 0)
       (set! gc-hot-at-bound 0) (gc-win-reset!) (sa-gc-trip-bytes! gc-trip-floor)
       (set! gc-live-after-full live)
       (let loop ((i 0) (path (list (sa-gc-trip-bytes))))
         (if (= i n)
             (let ((ms (map (lambda (b) (number->string (quotient b 1048576))) (reverse path))))
               (apply string-append (car ms) (map (lambda (s) (string-append \",\" s)) (cdr ms))))
             (begin
               (gc-size-nursery! (* (share-at (quotient (sa-gc-trip-bytes) 1048576)) 1000000) 100000000)
               (loop (+ i 1) (if (= (sa-gc-trip-bytes) (car path)) path (cons (sa-gc-trip-bytes) path))))))))")

(defn- decide []
  (let [replay (jolt.host/scheme-eval-string replay-src)
        path (fn [label share-at]
               (println "path" label (replay (* 64 1024 1024) 120 share-at)))]
    ;; collection over the 10% target but never dominating: up to the bound, no further
    (path "over-target" (fn [_] 15))
    ;; dominating at the bound, and a doubling past it pays: kept
    (path "paid" (fn [mb] (if (<= mb 64) 30 15)))
    ;; a doubling that costs more, then 8x that costs more too: back to the bound, held
    (path "costlier" (fn [mb] (cond (<= mb 64) 30 (<= mb 128) 40 :else 45)))
    ;; a doubling that costs more, then 8x that pays: kept at 8x
    (path "big-window" (fn [mb] (cond (<= mb 64) 30 (<= mb 128) 40 :else 15)))))

(defn -main [mode]
  (case mode
    "decide" (decide)
    "refresh" (refresh)
    "churn" (churn)
    "light" (reduce + (range 1000))
    "walk" (walk)
    "startup" (startup)
    "pinned" (churn)
    ;; the heap ceiling bounds the TOTAL heap, as -Xmx does: live data, nursery
    ;; and the free memory kept (the gate reads the high-water mark and the GC log)
    "ceiling" (hold-and-churn))
  (println "trip" (jolt.host/gc-trip-bytes))
  (when (= mode "churn")
    (println "bound" (jolt.host/scheme-eval-string "(gc-trip-limit)")))
  (when (= mode "ceiling")
    (println "peak" (jolt.host/maximum-memory-bytes) "max" (.maxMemory (Runtime/getRuntime)))))

(apply -main *command-line-args*)
