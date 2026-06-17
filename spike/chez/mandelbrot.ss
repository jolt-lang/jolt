;; mandelbrot spike — translated from bench/mandelbrot.clj. Pure float compute,
;; tight recur loops (here named-let tail loops). cap=200 like the .clj.
;;   chez --script mandelbrot.ss [n=200] [optlevel=2]
(import (chezscheme))
(optimize-level
  (let ((a (command-line-arguments)))
    (if (and (pair? a) (pair? (cdr a))) (string->number (cadr a)) 2)))

(define (count-point cr ci cap)
  (let loop ((i 0) (zr 0.0) (zi 0.0))
    (if (or (>= i cap) (> (+ (* zr zr) (* zi zi)) 4.0))
        i
        (loop (+ i 1)
              (+ (- (* zr zr) (* zi zi)) cr)
              (+ (* 2.0 (* zr zi)) ci)))))

(define (run n)
  (let ((cap 200) (nd (* 1.0 n)))
    (let loopy ((y 0) (acc 0))
      (if (< y n)
          (let* ((ci (- (/ (* 2.0 y) nd) 1.0))
                 (row (let loopx ((x 0) (a 0))
                        (if (< x n)
                            (let ((cr (- (/ (* 2.0 x) nd) 1.5)))
                              (loopx (+ x 1) (+ a (count-point cr ci cap))))
                            a))))
            (loopy (+ y 1) (+ acc row)))
          acc))))

(define (now-ns)
  (let ((t (current-time 'time-monotonic)))
    (+ (* (time-second t) 1000000000) (time-nanosecond t))))

(let* ((a (command-line-arguments))
       (n (if (pair? a) (string->number (car a)) 200)))
  (run n) (run n)                                          ; warmup
  (let loop ((k 0) (acc '()))
    (if (< k 3)
        (let* ((t0 (now-ns)) (r (run n)) (ms (/ (- (now-ns) t0) 1000000.0)))
          (loop (+ k 1) (cons ms acc)))
        (begin
          (printf "mandelbrot n ~a result ~a\n" n (run n))
          (printf "runs: ~a\n" (reverse acc))
          (printf "mean: ~a ms\n" (exact->inexact (/ (apply + acc) 3.0)))))))
