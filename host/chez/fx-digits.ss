;; fx-digits.ss — decimal digits of small non-negative fixnums without a
;; hardware divide, for jolt-fixnum->string (values.ss) and the double printer
;; (number-print.ss). Its own file because both load it -- and so do the Gambit
;; boot and the flonumprint gate, which loads number-print.ss without
;; values.ss. Needs nothing but Scheme fixnum and string primitives.

;; #1713: division by 10 and by 100 is a multiply and a
;; shift below a bound, as compilers do for a constant divisor: a hardware divide
;; is ~30 cycles of latency, and a digit loop is a chain of them (a five-digit
;; (str n) was ~10 dependent divides, 208 ns). Both constants were checked
;; against fxquotient for every m below their bounds, and their products stay
;; under 2^60; past a bound the divide is fxquotient.
(define-syntax fx-quot10
  (syntax-rules ()
    ((_ m0) (let ((m m0)) (if (sa-ufx<? m 100000000) (sa-ufxsra (sa-ufx* m 429496730) 32) (fxquotient m 10))))))
(define-syntax fx-quot100
  (syntax-rules ()
    ((_ m0) (let ((m m0)) (if (sa-ufx<? m 10000000) (sa-ufxsra (sa-ufx* m 42949673) 32) (fxquotient m 100))))))
;; "000102...99": pair i at 2i
(define fx-digit-pairs
  (let ((s (make-string 200 #\0)))
    (do ((i 0 (fx+ i 1))) ((fx=? i 100) s)
      (string-set! s (fx* 2 i) (integer->char (fx+ 48 (fxquotient i 10))))
      (string-set! s (fx+ 1 (fx* 2 i)) (integer->char (fx+ 48 (fxremainder i 10)))))))
(define fx-pow10
  (let ((v (make-vector 19 1)))
    (do ((k 1 (fx+ k 1))) ((fx=? k 19) v) (vector-set! v k (* 10 (vector-ref v (fx- k 1)))))))
;; how many decimal digits positive fixnum M has: a binary search over the
;; powers of ten, five compares where a linear count took up to nineteen
(define (fx-decimal-length m)
  (let search ((lo 1) (hi 19))          ; 10^(lo-1) <= m, and m < 10^hi or hi = 19
    (if (sa-ufx>=? lo hi)
        lo
        (let ((mid (sa-ufxsra (sa-ufx+ lo hi) 1)))
          (if (sa-ufx<? m (sa-uvector-ref fx-pow10 mid))
              (search lo mid)
              (search (sa-ufx+ mid 1) hi))))))
;; M's digits (M >= 0) into S, the last one at index END - 1. Seven at a time
;; from the low end, each block split off with one fxquotient and filled two
;; digits per multiply-shift; the top block (below 10^7) the same way without
;; padding.
(define (fx-fill-digits! s m end)
  (if (fx>=? m 10000000)
      (let* ((q (fxquotient m 10000000)) (r (fx- m (fx* q 10000000))))
        (fx-fill-7! s r end)
        (fx-fill-digits! s q (fx- end 7)))
      (fx-fill-small! s m end)))
;; M below 10^7, its digits only
(define (fx-fill-small! s m end)
  (let fill ((m m) (i (sa-ufx- end 1)))
    (cond ((sa-ufx>=? m 100)
           (let* ((q (fx-quot100 m)) (r (sa-ufx* 2 (sa-ufx- m (sa-ufx* q 100)))))
             (sa-ustring-set! s i (sa-ustring-ref fx-digit-pairs (sa-ufx+ r 1)))
             (sa-ustring-set! s (sa-ufx- i 1) (sa-ustring-ref fx-digit-pairs r))
             (fill q (sa-ufx- i 2))))
          ((sa-ufx>=? m 10)
           (let ((r (sa-ufx* 2 m)))
             (sa-ustring-set! s i (sa-ustring-ref fx-digit-pairs (sa-ufx+ r 1)))
             (sa-ustring-set! s (sa-ufx- i 1) (sa-ustring-ref fx-digit-pairs r))))
          (else (sa-ustring-set! s i (integer->char (sa-ufx+ 48 m)))))))
;; R below 10^7 as exactly seven digits, zero-padded, ending at END - 1
(define (fx-fill-7! s r end)
  (let loop ((r r) (i (sa-ufx- end 1)) (pairs 3))
    (if (sa-ufx=? pairs 0)
        (sa-ustring-set! s i (integer->char (sa-ufx+ 48 r)))
        (let* ((q (fx-quot100 r)) (p (sa-ufx* 2 (sa-ufx- r (sa-ufx* q 100)))))
          (sa-ustring-set! s i (sa-ustring-ref fx-digit-pairs (sa-ufx+ p 1)))
          (sa-ustring-set! s (sa-ufx- i 1) (sa-ustring-ref fx-digit-pairs p))
          (loop q (sa-ufx- i 2) (sa-ufx- pairs 1))))))
