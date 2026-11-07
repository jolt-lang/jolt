;; doseq-perf.ss — doseq over a chunked seq must not cost more than over a list.
;; Run: chez --script test/chez/doseq-perf.ss (or `make doseqperf`).
;;
;; doseq walked every seq with first/next, and next on a chunked seq (a vector's,
;; a range's, a chunked map's) makes a new cell per element, so a 100-element
;; vector took 3.5x a list of the same elements. The JVM's doseq walks a chunk
;; by index and only steps the seq once per chunk; so does jolt's now.
;;
;; The assertion is a RATIO measured in this one process — the vector walk
;; against the list walk over the same elements — so it holds on a loaded
;; machine where a wall-clock floor would not. The bar is 1.5x: a chunk walk is
;; at least as cheap as a list walk; the old cell-per-element walk was 3.5x.
;; Timing stays out of the default gate, as printperf and sbperf do.
(import (chezscheme))
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

;; This thread's CPU time, not the wall clock: a preempted sample reads as a regression.
(define (now-ms)
  (let ((t (current-time 'time-thread)))
    (+ (* 1000.0 (time-second t)) (/ (time-nanosecond t) 1000000.0))))
(define (best-ms expr)
  (let loop ((i 0) (best +inf.0))
    (if (fx>= i 5)
        best
        (let ((t0 (now-ms)))
          (jolt-compile-eval expr "user")
          (loop (fx+ i 1) (min best (- (now-ms) t0)))))))

(define failed? #f)
(define (fail! msg) (set! failed? #t) (printf "  FAIL: ~a\n" msg))
(define (check! label expr want)
  (let ((got (jolt-compile-eval expr "user")))
    (unless (equal? got want) (fail! (format "~a: expected ~s, got ~s" label want got)))))

(jolt-compile-eval "(def dsv (vec (range 100000)))" "user")
(jolt-compile-eval "(def dsl (apply list (range 100000)))" "user")
(jolt-compile-eval "(defn ds-walk [xs] (let [a (volatile! 0)] (dotimes [_ 20] (doseq [x xs] (vswap! a + x))) @a))" "user")
;; correctness first: a walk that skipped elements would time beautifully
(check! "the vector walk sums every element" "(str (ds-walk dsv))" (number->string (* 20 4999950000)))
(check! "the list walk sums every element" "(str (ds-walk dsl))" (number->string (* 20 4999950000)))

(let* ((tl (best-ms "(ds-walk dsl)")) (tv (best-ms "(ds-walk dsv)")) (ratio (/ tv tl)))
  (printf "doseq over a vector ~,1fms, over a list ~,1fms: ~,2fx\n" tv tl ratio)
  (when (> ratio 1.5)
    (fail! (format "doseq over a vector is ~,2fx a list of the same elements (bar 1.5x)" ratio))))

(if failed?
    (begin (printf "doseq perf: FAILED\n") (exit 1))
    (printf "doseq perf: passed\n"))
