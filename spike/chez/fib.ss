;; fib spike — translated from bench/fib.clj. Pure call + integer arith.
;;   chez --script fib.ss [n=30] [optlevel=2]
(import (chezscheme))
(optimize-level
  (let ((a (command-line-arguments)))
    (if (and (pair? a) (pair? (cdr a))) (string->number (cadr a)) 2)))

(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(define (run n) (fib n))

(define (now-ns)
  (let ((t (current-time 'time-monotonic)))
    (+ (* (time-second t) 1000000000) (time-nanosecond t))))

(let* ((a (command-line-arguments))
       (n (if (pair? a) (string->number (car a)) 30)))
  (run (- n 6)) (run (- n 6))                              ; warmup
  (let loop ((k 0) (acc '()))
    (if (< k 3)
        (let* ((t0 (now-ns)) (r (run n)) (ms (/ (- (now-ns) t0) 1000000.0)))
          (loop (+ k 1) (cons ms acc)))
        (begin
          (printf "fib n ~a result ~a\n" n (run n))
          (printf "runs: ~a\n" (reverse acc))
          (printf "mean: ~a ms\n" (exact->inexact (/ (apply + acc) 3.0)))))))
