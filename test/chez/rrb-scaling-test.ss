;; RRB complexity gate: catvec and slice must not be linear in the vector size.
;; Run:  chez --script test/chez/rrb-scaling-test.ss
;;
;; Guards the complexity class, not the machine: both sizes are measured in
;; ONE process (same heap, same JIT-less substrate), and the assertion is the
;; n-vs-4n RATIO. An O(log n) op holds the ratio near 1; a rebuild-both-halves
;; regression puts it near 4. The bound is generous so only the class trips it
;; (see the readscaling precedent — never assert absolute times).

(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define (fresh n)
  (let loop ((i 0) (p empty-pvec)) (if (fx=? i n) p (loop (fx+ i 1) (pvec-conj p i)))))

;; Timed the way test/complexity_test.clj times its rows: each op runs in a
;; batch sized to clear 1ms (a fixed 400 reps measured 1-2ms on the millisecond
;; clock, so one tick moved the ratio from 1.0 to 2.0 against the 2.5 ceiling,
;; and a fixed rep count big enough to fix that makes a regressed rebuild run
;; for minutes). An arm's cost is its fastest batch over 5 rounds of a 20ms
;; budget each; the arms alternate order every round and each round starts from
;; a full collection. A linear rebuild reads ~4 in every batch.
;; This thread's CPU time, not the wall clock: a preempted sample reads as a regression.
(define (now-ns)
  (let ((t (current-time 'time-thread)))
    (+ (* 1000000000 (time-second t)) (time-nanosecond t))))

(define (timed-batch thunk b)
  (let ((t0 (now-ns)))
    (do ((i 0 (fx+ i 1))) ((fx=? i b)) (thunk))
    (- (now-ns) t0)))

(define (batch-size thunk)
  (let loop ((b 1))
    (if (or (>= (timed-batch thunk b) 1000000) (>= b 1048576)) b (loop (* 2 b)))))

;; ns per op of the fastest batch run within the budget, and of at least five:
;; a regressed op is one ~20ms batch, and two of those filter nothing
(define (per-op thunk b)
  (let loop ((spent 0) (best #f) (k 0))
    (if (and (>= spent 20000000) (>= k 5))
        (/ (exact->inexact best) b)
        (let ((t (timed-batch thunk b)))
          (loop (+ spent t) (if best (min best t) t) (fx+ k 1))))))

;; -> (ns/op at n, ns/op at 4n, ratio), each arm its fastest batch over all
;; rounds. Keeping the round with the smallest ratio instead only ever errs
;; toward passing, and under load it did: a regressed catvec read 1.45.
(define (measure f1 f4)
  (f1) (f4)
  (let ((b1 (batch-size f1)) (b4 (batch-size f4)))
    (let loop ((i 0) (t1 #f) (t4 #f))
      (if (fx=? i 5)
          (list t1 t4 (/ t4 (max t1 1.0)))
          (begin
            (collect (collect-maximum-generation))
            (let* ((pair (if (even? i)
                             (let* ((x (per-op f1 b1)) (y (per-op f4 b4))) (cons x y))
                             (let* ((y (per-op f4 b4)) (x (per-op f1 b1))) (cons x y)))))
              (loop (fx+ i 1)
                    (if t1 (min t1 (car pair)) (car pair))
                    (if t4 (min t4 (cdr pair)) (cdr pair)))))))))

;; A miss is measured once more before it counts: a linear rebuild reads ~4 on
;; both measurements, while a burst rarely spoils two in a row.
(define (judge f1 f4)
  (let ((m (measure f1 f4)))
    (if (< (caddr m) 2.5) m (measure f1 f4))))

(define n1 50000)
(define a1 (fresh n1)) (define b1 (fresh n1))
(define a4 (fresh (* 4 n1))) (define b4 (fresh (* 4 n1)))

;; warmup
(pvec-catvec a1 b1) (pvec-catvec a4 b4)

(define cat (judge (lambda () (pvec-catvec a1 b1)) (lambda () (pvec-catvec a4 b4))))
(define cat-ratio (caddr cat))

(define (slicer p)
  (let ((n (pvec-cnt p)))
    (lambda () (pvec-slice p (fxquotient n 3) (fx* 2 (fxquotient n 3))))))
(define sl (judge (slicer (pvec-catvec a1 b1)) (slicer (pvec-catvec a4 b4))))
(define sl-ratio (caddr sl))

(define (ns x) (exact (round x)))
(printf "rrbscaling: catvec ~ans vs ~ans per op (x4 size) ratio ~,2f; slice ~ans vs ~ans ratio ~,2f\n"
        (ns (car cat)) (ns (cadr cat)) cat-ratio (ns (car sl)) (ns (cadr sl)) sl-ratio)

(define fails 0)
(when (>= cat-ratio 2.5)
  (set! fails (+ fails 1))
  (printf "FAIL: catvec scales with size (ratio ~a >= 2.5) — linear rebuild is back\n" cat-ratio))
(when (>= sl-ratio 2.5)
  (set! fails (+ fails 1))
  (printf "FAIL: slice scales with size (ratio ~a >= 2.5) — linear rebuild is back\n" sl-ratio))
(unless (= fails 0) (exit 1))
(printf "RRB-SCALING OK\n")
