;; s64-wrap-test.ss — 64-bit wraparound add, subtract and multiply (#1649,
;; host/chez/chez-patches/0008-s64-wraparound-arithmetic.patch).
;;
;; unchecked-add/-subtract/-multiply (and +, -, * under *unchecked-math*) wrap
;; to a signed long. Past the fixnum range seq.ss hands them to kernel entries,
;; bound in rt.ss as jolt-s64-{add,sub,mul}-wrap, that return the low 64 bits of
;; the exact result without building it. Every answer here is checked against
;; the definition: the exact result, masked to 64 bits and sign-folded.
;;
;; The rows:
;;   a. the patched kernel has the three entries
;;   b. each entry answers the definition over edge operands (fixnum and bignum
;;      limits, 2^63, 2^64, bignums far past 64 bits) and randomized ones, a
;;      fixnum whenever the result fits, and #f for a non-integer
;;   c. jolt-uncadd2/sub2/mul2/inc/dec/neg answer the definition over the same
;;      operands, on the kernel path and again with the entries unbound (the
;;      stock-Chez and Gambit path): a wrap for Long operands, the exact op once
;;      an operand is past the long range (a BigInt)
;;   d. a flonum or ratio operand is an ordinary op, not a wrap, and negate
;;      keeps -0.0
;;
;; (a) and (b) need the patched kernel. On a stock one they report and pass,
;; unless JOLT_REQUIRE_LOCKFREE_CHEZ=1 says the kernel must carry jolt's patches
;; (CI).
;;
;;   chez --script test/chez/s64-wrap-test.ss
(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (if pred (printf "PASS: ~a\n" name)
      (begin (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name))))

(define 2^63 (expt 2 63))
(define 2^64 (expt 2 64))
(define (wrap r)
  (let ((m (bitwise-and r (- 2^64 1))))
    (if (>= m 2^63) (- m 2^64) m)))

(define edges
  ;; 2^30 and 1e9 straddle the fixnum multiply's two fits tests (both factors
  ;; under 2^30, a flonum estimate under 1e18), and 1073741824 squared is 2^60
  (let ((base (list 0 1 2 3 31 1000000007 (- (expt 2 30) 1) (expt 2 30) (+ (expt 2 30) 1)
                    999999999 1000000000 1000000001 1152921504 1152921505 1073741823999
                    (expt 2 31) (expt 2 32) (- (expt 2 32) 1)
                    (expt 2 59) (most-positive-fixnum) (+ (most-positive-fixnum) 1)
                    (expt 2 62) (- 2^63 1) 2^63 (- 2^64 1) 2^64 (+ 2^64 5)
                    (+ (expt 2 100) 12345) (- (expt 2 127) 1))))
    (append base (map - base) (list (most-negative-fixnum) (- (most-negative-fixnum) 1)))))

;; biased toward the boundaries the C reads bigits at
(define (rand-operand)
  (case (random 6)
    ((0) (- (random 2^64) 2^63))
    ((1) (- (expt 2 (random 64)) (random 3)))
    ((2) (- (random 3) (expt 2 (random 64))))
    ((3) (- (random (expt 2 40)) (expt 2 39)))
    ((4) (- (random (expt 2 130)) (expt 2 129)))
    (else (list-ref edges (random (length edges))))))

(define pairs
  (let loop ((i 0) (acc (apply append (map (lambda (x) (map (lambda (y) (cons x y)) edges)) edges))))
    (if (= i 100000) acc (loop (+ i 1) (cons (cons (rand-operand) (rand-operand)) acc)))))

;; equal values, and a fixnum exactly where the definition's value is one: the
;; kernel must hand back a normalized integer, never a bignum in fixnum range
(define (same? r e)
  (if (pair? r)
      (and (pair? e) (same? (car r) (car e)) (same? (cdr r) (cdr e)))
      (and (equal? r e) (eq? (fixnum? r) (fixnum? e)))))
;; the first pair whose answer is wrong, or #f
(define (first-bad f expect)
  (let loop ((ps pairs))
    (cond ((null? ps) #f)
          ((let ((r (f (caar ps) (cdar ps))))
             (same? r (expect (caar ps) (cdar ps))))
           (loop (cdr ps)))
          (else (car ps)))))
(define (check name f expect)
  (let ((bad (first-bad f expect)))
    (ok name (not bad))
    (when bad (printf "  first wrong pair: ~s\n" bad))))

(define required? (equal? (getenv "JOLT_REQUIRE_LOCKFREE_CHEZ") "1"))
(define patched? (and jolt-s64-add-wrap jolt-s64-sub-wrap jolt-s64-mul-wrap #t))

;; --- a. the entries ----------------------------------------------------------
(if (or patched? required?)
    (ok "a. the kernel has (cs)s64_add_wrap, (cs)s64_sub_wrap and (cs)s64_mul_wrap" patched?)
    (printf "SKIP: a/b need the patched Chez kernel (chez-patches/0008); this one is stock\n"))

;; --- b. the entries themselves -----------------------------------------------
(when patched?
  (check "b. (cs)s64_add_wrap is the wrapped exact sum" jolt-s64-add-wrap (lambda (x y) (wrap (+ x y))))
  (check "b. (cs)s64_sub_wrap is the wrapped exact difference" jolt-s64-sub-wrap (lambda (x y) (wrap (- x y))))
  (check "b. (cs)s64_mul_wrap is the wrapped exact product" jolt-s64-mul-wrap (lambda (x y) (wrap (* x y))))
  (ok "b. a non-integer operand answers #f"
      (andmap (lambda (f)
                (andmap (lambda (p) (eq? (f (car p) (cdr p)) #f))
                        (list (cons 1.5 2) (cons 2 1.5) (cons 1/3 2) (cons (expt 2 70) 1/3)
                              (cons #\a 1) (cons "x" 1) (cons 1 '()) (cons +nan.0 2^63))))
              (list jolt-s64-add-wrap jolt-s64-sub-wrap jolt-s64-mul-wrap))))

;; --- c. the unchecked ops, on both paths -------------------------------------
;; Only a Long pair wraps. An operand outside signed 64 bits is a BigInt, and
;; the JVM's unchecked ops take BigInt contagion from it like + does: the exact
;; result (jolt-cdjgi).
(define (long? x) (and (>= x (- 2^63)) (< x 2^63)))
(define (unc op)
  (lambda (x y) (if (and (long? x) (long? y)) (wrap (op x y)) (op x y))))
(define (unc1 op)
  (lambda (x) (if (long? x) (wrap (op x)) (op x))))
(define (check-ops label)
  (check (format "c. jolt-uncadd2 wraps a Long pair (~a)" label) jolt-uncadd2 (unc +))
  (check (format "c. jolt-uncsub2 wraps a Long pair (~a)" label) jolt-uncsub2 (unc -))
  (check (format "c. jolt-uncmul2 wraps a Long pair (~a)" label) jolt-uncmul2 (unc *))
  (check (format "c. jolt-uncinc, -dec and -neg wrap a Long (~a)" label)
         (lambda (x y) (list (jolt-uncinc x) (jolt-uncdec x) (jolt-uncneg x)))
         (lambda (x y) (list ((unc1 (lambda (v) (+ v 1))) x) ((unc1 (lambda (v) (- v 1))) x)
                             ((unc1 -) x))))
  ;; unchecked-add/-subtract/-multiply take two args, as on the JVM; a longer
  ;; (+ a b c) under *unchecked-math* is the nested pairs below, and wraps the same
  (ok (format "c. nested pairs wrap (~a)" label)
      (equal? (list (jolt-uncmul2 (jolt-uncmul2 31 (- 2^63 1)) 7)
                    (jolt-uncadd2 (jolt-uncadd2 (- 2^63 1) 1) 1)
                    (jolt-uncsub2 (jolt-uncsub2 (- 2^63) 1) 1) (jolt-uncneg (- 2^63)))
              (list (wrap (* 31 (- 2^63 1) 7)) (- 1 2^63) (- 2^63 2) (- 2^63)))))

(check-ops (if patched? "kernel path" "stock path"))
(when patched?
  (let ((add jolt-s64-add-wrap) (sub jolt-s64-sub-wrap) (mul jolt-s64-mul-wrap))
    (set! jolt-s64-add-wrap #f) (set! jolt-s64-sub-wrap #f) (set! jolt-s64-mul-wrap #f)
    (check-ops "entries unbound, the stock path")
    (set! jolt-s64-add-wrap add) (set! jolt-s64-sub-wrap sub) (set! jolt-s64-mul-wrap mul)))

;; --- d. non-long operands ----------------------------------------------------
(ok "d. a flonum or ratio operand is an ordinary op"
    (equal? (list (jolt-uncmul2 1.5 2.0) (jolt-uncmul2 2^63 2.0) (jolt-uncadd2 2/3 2/3)
                  (jolt-uncsub2 1/2 (expt 2 70)) (jolt-uncinc 1.5) (jolt-uncneg 1/3))
            (list 3.0 (* 2^63 2.0) 4/3 (- 1/2 (expt 2 70)) 2.5 -1/3)))
(ok "d. negate keeps the sign of a flonum zero"
    (and (eqv? (jolt-uncneg 0.0) -0.0) (eqv? (jolt-uncneg -0.0) 0.0)))

(printf "\n~a/~a passed\n" (- total fails) total)
(exit (if (zero? fails) 0 1))
