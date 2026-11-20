;; A double's printed form (number-print.ss). Run from repo root:
;;   chez --script test/chez/flonum-print-test.ss
;;
;; jolt-flonum->string answers a double some decimal of at most 15 significant
;; digits reads back as from flonum arithmetic (flonum-fast-digits), and every
;; other double from the host's shortest digits (flonum-host->string). The two
;; must print every double alike: the fast path is checked against the host one
;; over random bit patterns and over short decimals at every magnitude, then
;; pinned to Double.toString's text, then timed against it.
(import (chezscheme))
(load "host/chez/number-print.ss")

(define fails 0)
(define (fail! fmt . args)
  (set! fails (+ fails 1))
  (when (< fails 20) (apply printf fmt args)))
(define (agree x0)
  (let* ((x (inexact x0))
         (a (flonum-host->string x)) (b (jolt-flonum->string x)))
    (unless (string=? a b) (fail! "FAIL: ~s host=~a fast=~a\n" x a b))))

(define (random-double)          ; any finite double, from a random bit pattern
  (let ((bv (make-bytevector 8)))
    (bytevector-u32-native-set! bv 0 (random (expt 2 32)))
    (bytevector-u32-native-set! bv 4 (random (expt 2 32)))
    (let ((x (bytevector-ieee-double-native-ref bv 0)))
      (if (and (fl= x x) (not (fl= (flabs x) +inf.0))) x (random-double)))))

(random-seed 20261120)
(do ((i 0 (+ i 1))) ((= i 200000))
  (agree (random-double))
  ;; short decimals across magnitudes: m x 10^-j and m x 10^j, m up to 17 digits
  (let ((m (random (expt 10 (+ 1 (random 17))))) (j (random 25)))
    (agree (/ m (expt 10 j)))
    (agree (- (/ m (expt 10 j))))
    (agree (* m (expt 10 (random 8)))))
  (agree (* 1.1 i)) (agree (* 0.1 i)) (agree (/ i 3.0))
  (agree (* (random 1.0) (expt 10.0 (- (random 50) 25)))))

;; Double.toString's text, around the fast path's edges: its bounds (1e-22,
;; 1e15), the 15-digit limit, k = 0 trailing zeros, the plain/E layout switch
(for-each
  (lambda (p)
    (let ((s (jolt-flonum->string (car p))))
      (unless (string=? s (cdr p)) (fail! "FAIL: ~s printed ~a, Double.toString ~a\n" (car p) s (cdr p)))))
  '((1.1 . "1.1") (0.1 . "0.1") (1200.0 . "1200.0") (100.0 . "100.0") (-2.5 . "-2.5")
    (0.001 . "0.001") (9.99e-4 . "9.99E-4") (1e-5 . "1.0E-5") (1e7 . "1.0E7")
    (9999999.0 . "9999999.0") (12345678.0 . "1.2345678E7")
    (123456789012345.0 . "1.23456789012345E14") (1e15 . "1.0E15")
    (1234567890123456.0 . "1.234567890123456E15") (1.5e-22 . "1.5E-22") (1e-22 . "1.0E-22")
    (1e-23 . "1.0E-23") (0.30000000000000004 . "0.30000000000000004")
    (0.0 . "0.0") (-0.0 . "-0.0") (5e-324 . "4.9E-324") (1.7976931348623157e308 . "1.7976931348623157E308")))

;; The fast path is the point: a short decimal must print well under the host
;; path's time. Ratio in one process, fastest of several rounds each.
(define shorts
  (let loop ((i 0) (acc '()))
    (if (= i 20000) acc (loop (+ i 1) (cons (inexact (/ (random 100000000) 1000)) acc)))))
(define (best f)
  (let loop ((r 0) (b #f))
    (if (= r 7)
        b
        (let ((t0 (current-time 'time-thread)))
          (for-each f shorts)
          (let* ((t1 (current-time 'time-thread))
                 (d (time-difference t1 t0))
                 (ns (+ (* (time-second d) 1000000000) (time-nanosecond d))))
            (loop (+ r 1) (if b (min b ns) ns)))))))
(let* ((host (best flonum-host->string))
       (fast (best jolt-flonum->string))
       (ratio (/ (inexact fast) host)))
  (printf "flonum-print: short decimals print in ~,2fx the host path's time (ceiling 0.50)\n" ratio)
  (when (> ratio 0.5) (fail! "FAIL: the fast path is not taking short decimals\n")))

(if (= fails 0)
    (printf "flonum-print: ok\n")
    (begin (printf "flonum-print: ~a failures\n" fails) (exit 1)))
