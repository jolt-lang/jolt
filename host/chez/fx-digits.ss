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
    ((_ m0) (let ((m m0)) (if (fx<? m 100000000) (fxsra (fx* m 429496730) 32) (fxquotient m 10))))))
(define-syntax fx-quot100
  (syntax-rules ()
    ((_ m0) (let ((m m0)) (if (fx<? m 10000000) (fxsra (fx* m 42949673) 32) (fxquotient m 100))))))
;; "000102...99": pair i at 2i
(define fx-digit-pairs
  (let ((s (make-string 200 #\0)))
    (do ((i 0 (fx+ i 1))) ((fx=? i 100) s)
      (string-set! s (fx* 2 i) (integer->char (fx+ 48 (fxquotient i 10))))
      (string-set! s (fx+ 1 (fx* 2 i)) (integer->char (fx+ 48 (fxremainder i 10)))))))
(define fx-pow10
  (let ((v (make-vector 19 1)))
    (do ((k 1 (fx+ k 1))) ((fx=? k 19) v) (vector-set! v k (* 10 (vector-ref v (fx- k 1)))))))
;; how many decimal digits positive fixnum M has
(define (fx-decimal-length m)
  (let count ((k 1))
    (if (or (fx=? k 19) (fx<? m (vector-ref fx-pow10 k))) k (count (fx+ k 1)))))
;; M's digits (M >= 0) into S, the last one at index END - 1, two at a time
(define (fx-fill-digits! s m end)
  (let fill ((m m) (i (fx- end 1)))
    (cond ((fx>=? m 100)
           (let* ((q (fx-quot100 m)) (r (fx* 2 (fx- m (fx* q 100)))))
             (string-set! s i (string-ref fx-digit-pairs (fx+ r 1)))
             (string-set! s (fx- i 1) (string-ref fx-digit-pairs r))
             (fill q (fx- i 2))))
          ((fx>=? m 10)
           (let ((r (fx* 2 m)))
             (string-set! s i (string-ref fx-digit-pairs (fx+ r 1)))
             (string-set! s (fx- i 1) (string-ref fx-digit-pairs r))))
          (else (string-set! s i (integer->char (fx+ 48 m)))))))
