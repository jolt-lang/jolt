;; mandelbrot, flonum-specialized — what a type-aware jolt->Chez backend would
;; emit (fl*/fl+/fl< unbox; fx ops for the integer counter). This is the real
;; substrate ceiling vs the generic version (which boxes every flonum).
;;   chez --script mandelbrot-fl.ss [n=200] [optlevel=3]
(import (chezscheme))
(optimize-level
  (let ((a (command-line-arguments)))
    (if (and (pair? a) (pair? (cdr a))) (string->number (cadr a)) 3)))

(define (count-point cr ci cap)
  (let loop ((i 0) (zr 0.0) (zi 0.0))
    (if (or (fx>= i cap) (fl> (fl+ (fl* zr zr) (fl* zi zi)) 4.0))
        i
        (loop (fx+ i 1)
              (fl+ (fl- (fl* zr zr) (fl* zi zi)) cr)
              (fl+ (fl* 2.0 (fl* zr zi)) ci)))))

(define (run n)
  (let ((cap 200) (nd (fixnum->flonum n)))
    (let loopy ((y 0) (acc 0))
      (if (fx< y n)
          (let* ((ci (fl- (fl/ (fl* 2.0 (fixnum->flonum y)) nd) 1.0))
                 (row (let loopx ((x 0) (a 0))
                        (if (fx< x n)
                            (let ((cr (fl- (fl/ (fl* 2.0 (fixnum->flonum x)) nd) 1.5)))
                              (loopx (fx+ x 1) (fx+ a (count-point cr ci cap))))
                            a))))
            (loopy (fx+ y 1) (fx+ acc row)))
          acc))))

(define (now-ns)
  (let ((t (current-time 'time-monotonic)))
    (+ (* (time-second t) 1000000000) (time-nanosecond t))))

(let* ((a (command-line-arguments))
       (n (if (pair? a) (string->number (car a)) 200)))
  (run n) (run n)
  (let loop ((k 0) (acc '()))
    (if (< k 3)
        (let* ((t0 (now-ns)) (r (run n)) (ms (/ (- (now-ns) t0) 1000000.0)))
          (loop (+ k 1) (cons ms acc)))
        (begin
          (printf "mandelbrot-fl n ~a result ~a\n" n (run n))
          (printf "runs: ~a\n" (reverse acc))
          (printf "mean: ~a ms\n" (exact->inexact (/ (apply + acc) 3.0)))))))
