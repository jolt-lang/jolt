;; bench-perf.clj — micro-benchmarks for the perf/audit-tail optimizations.
;; Run: bin/joltc test/chez/bench-perf.clj

(require '[clojure.string :as str])

(defn now-ms []
  (let [t (System/currentTimeMillis)]
    (double t)))

(defn bench [label n f]
  (let [t0 (now-ms)]
    (dotimes [_ n] (f))
    (let [elapsed (- (now-ms) t0)]
      (printf "%s: %.2f ms (%d iters, %.1f us/op)\n"
              label elapsed n (/ (* elapsed 1000.0) n)))))

(def runs 10000)

;; --- Stage B: subseq / rsubseq ------------------------------------------------
(let [m (apply sorted-map (interleave (range 500) (range 500)))]
  ;; subseq with bound
  (bench "subseq < 250" runs
    #(dorun (subseq m < 250)))
  ;; subseq with range
  (bench "subseq >= 100 < 200" runs
    #(dorun (subseq m >= 100 < 200)))
  ;; rsubseq
  (bench "rsubseq > 250" runs
    #(dorun (rsubseq m > 250)))
  ;; subseq on small range (fast path)
  (bench "subseq >= 0 < 5" runs
    #(dorun (subseq m >= 0 < 5))))

;; --- Stage C: natives-str case + replace --------------------------------------
;; uppercase — no change (all uppercase input, allocation-free fast path)
(bench "upper-case all UP" runs
  #(str/upper-case "HELLO WORLD THIS IS A TEST"))

;; uppercase — needs change (mixed case, must allocate)
(bench "upper-case mixed" runs
  #(str/upper-case "hello world this is a test"))

;; lowercase — no change
(bench "lower-case all low" runs
  #(str/lower-case "hello world this is a test"))

;; lowercase — needs change
(bench "lower-case mixed" runs
  #(str/lower-case "HELLO WORLD THIS IS A TEST"))

;; replace — needle not found (allocation-free fast path)
(bench "replace not found" runs
  #(str/replace "hello world" "xyz" "abc"))

;; replace — needle found
(bench "replace found" runs
  #(str/replace "hello world hello" "hello" "bye"))

(println "\nDone.")
