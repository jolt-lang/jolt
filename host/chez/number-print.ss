;; number-print.ss — how jolt prints a number: Double.toString's layout over the
;; host's shortest round-trip digits. Portable (R7RS strings and arithmetic), so
;; Chez (rt.ss loads it) and Gambit (rt-core.ss includes it) print alike.

;; jolt has a numeric tower (exact integer / ratio / double, distinguished by
;; class). Exact integer-valued values print without a ".0" ((+ 1 2) -> "3");
;; a double prints with one ((* 1.0 5) -> "5.0", as the JVM does).

;; Double.toString layout: plain decimal when 1e-3 <= |x| < 1e7, otherwise
;; scientific d.dddE±x with one digit before the point; the mantissa always
;; carries a decimal point ("1.0E100", "2.3E-4", "1.2345678E7"). The host's
;; shortest-round-trip digits are kept and only the layout is rearranged, except
;; for a subnormal (see flonum-two-digits).
;; The two significant digits nearest the finite, positive double X, as
;; (digits . point) in jolt-flonum->string's terms (value = 0.DIGITS x 10^POINT,
;; trailing zero dropped), or #f when that decimal does not read back as X.
(define (flonum-two-digits x)
  (let* ((v (exact x))
         ;; e: 10^e <= v < 10^(e+1)
         (e (let loop ((e (exact (floor (/ (log x) (log 10.0))))))
              (cond ((> (expt 10 e) v) (loop (- e 1)))
                    ((<= (expt 10 (+ e 1)) v) (loop (+ e 1)))
                    (else e))))
         (n (round (/ v (expt 10 (- e 1)))))              ; 10..100
         (point (if (= n 100) (+ e 2) (+ e 1)))
         (n (if (= n 100) 10 n)))
    (and (= (exact->inexact (* n (expt 10 (- point 2)))) x)
         (let ((s (number->string n)))
           (cons (if (char=? (string-ref s 1) #\0) (substring s 0 1) s) point)))))

;; At 16 or 17 significant digits the double can lie exactly halfway between
;; two decimals of that length that both read back as it, and the host and the
;; JVM may break that tie differently: Chez rounds it up, Double.toString to the
;; even digit (1338163280546708.25 is 1.3381632805467082E15 on the JVM,
;; -650605845684.15625 is -6.506058456841562E11). Only an odd last digit D can
;; be the wrong side of a tie, with the double exactly half a unit of the last
;; place from D; that needs x * 2^(1-p) to be an odd integer, which is checked
;; first in flonum arithmetic, so an ordinary long double costs a parity test
;; and a few float operations, never the exact arithmetic. Answers the even neighbour's digits, or
;; #f to keep the host's.
(define (flonum-long-digits x digits point)
  (let ((dlen (string-length digits)))
    (and (odd? (char->integer (string-ref digits (fx- dlen 1))))   ; #\1 is 49
         (let ((p (- point dlen)))                 ; the last digit's place
           ;; x * 2^(1-p) is an odd integer: exact in flonum arithmetic, since
           ;; scaling by a power of two only moves the exponent
           (and (if (< p 0)
                    (let ((y (* x (expt 2.0 (- 1 p)))))
                      (and (= y (round y)) (not (= (* y 0.5) (round (* y 0.5))))))
                    (and (> p 0) (integer? x)))
                (let* ((v (exact x))
                       (d (string->number digits))
                       (unit (expt 10 p))
                       (even (cond ((= v (* (- d 1/2) unit)) (- d 1))
                                   ((= v (* (+ d 1/2) unit)) (+ d 1))
                                   (else #f)))
                       ;; and only when that neighbour reads back as x: a unit
                       ;; wider than the double's spacing leaves the host's
                       ;; digits the only ones that do
                       (s (and even (= (exact->inexact (* even unit)) x)
                               (number->string even))))
                  (and s (fx=? (string-length s) dlen)
                       (let loop ((k dlen))
                         (if (and (fx>? k 1) (char=? (string-ref s (fx- k 1)) #\0))
                             (loop (fx- k 1))
                             (substring s 0 k))))))))))

;; The fast path: a double that some decimal of at most 15 significant digits
;; reads back as -- 1.1, 3.25, 0.001, what EDN and JSON mostly carry -- has its
;; digits found in flonum arithmetic, never the host's arbitrary-precision
;; shortest-digits search (number->string was ~0.5 us a double, most of printing one).
;;
;; For k = 0, 1, ... take y = x * 10^k and n = round(y). With n < 2^53 and
;; 10^k <= 10^22 both exact, the division is correctly rounded, so (= (/ n 10^k) x)
;; is exactly "the decimal n x 10^-k reads back as x". The first k with a hit has
;; the fewest fractional digits, so the fewest significant ones: the shortest
;; decimal. No other integer can hit: below 10^15 a double's spacing scaled by
;; 10^k is under 0.23, so a k-place decimal that reads back as x lies within 0.12
;; of x * 10^k, and y is within 0.12 of that; one integer, round(y), is that close,
;; so there is no nearest-or-even choice to make. Past 15 digits the host path
;; answers, which also keeps the 16-17 digit ties (flonum-long-digits) off this one.
;; Answers (digits . point) for a positive X, or #f.
(define flonum-pow10
  (let ((v (make-vector 23 1.0)))
    (do ((k 1 (fx+ k 1))) ((fx= k 23) v)
      (vector-set! v k (fl* 10.0 (vector-ref v (fx- k 1)))))))
;; (digits . point) for the decimal N x 10^-K, N a positive fixnum: N's digits
;; with its trailing zeros dropped (k = 0 leaves them: 1200.0), written by hand
;; because number->string of a fixnum costs ~0.2 us, most of a short double's print
(define (flonum-fixnum-digits n k)
  (let strip ((m n) (z 0))
    (if (fx=? 0 (fxremainder m 10))
        (strip (fxquotient m 10) (fx+ z 1))
        (let* ((len (let count ((q m) (c 0)) (if (fx=? q 0) c (count (fxquotient q 10) (fx+ c 1)))))
               (s (make-string len)))
          (let fill ((q m) (i (fx- len 1)))
            (if (fx<? i 0)
                (cons s (fx- (fx+ len z) k))
                (begin (string-set! s i (integer->char (fx+ 48 (fxremainder q 10))))
                       (fill (fxquotient q 10) (fx- i 1)))))))))
(define (flonum-fast-digits x)
  (and (fl>= x 1e-22) (fl< x 1e15)
       (let loop ((k 0))
         (and (fx< k 23)
              (let* ((p (vector-ref flonum-pow10 k))
                     (y (fl* x p)))
                (and (fl< y 1e15)
                     (let ((n (flround y)))
                       (if (and (fl>= n 1.0) (fl= (fl/ n p) x))
                           (flonum-fixnum-digits (exact n) k)
                           (loop (fx+ k 1))))))))))

;; Double.toString's layout of the decimal 0.DIGITS x 10^POINT
(define (flonum-layout neg? digits point)
  (let* ((dlen (string-length digits))
         (res (cond
                ((string=? digits "0") "0.0")
                ((and (>= point -2) (<= point 7))   ; 1e-3 <= |x| < 1e7
                 (cond
                   ((<= point 0)
                    (string-append "0." (make-string (- point) #\0) digits))
                   ((>= point dlen)
                    (string-append digits (make-string (- point dlen) #\0) ".0"))
                   (else (string-append (substring digits 0 point) "."
                                        (substring digits point dlen)))))
                (else
                 (string-append (substring digits 0 1) "."
                                (if (fx>? dlen 1) (substring digits 1 dlen) "0")
                                "E" (number->string (- point 1)))))))
    (if neg? (string-append "-" res) res)))

(define (jolt-flonum->string x)
  (let ((fast (flonum-fast-digits (flabs x))))
    (if fast
        (flonum-layout (fl< x 0.0) (car fast) (cdr fast))
        (flonum-host->string x))))

(define (flonum-host->string x)
  (let* ((s (number->string x))
         (neg? (char=? (string-ref s 0) #\-))
         (body0 (if neg? (substring s 1 (string-length s)) s))
         ;; Chez appends a "|prec" suffix to subnormal strings (e.g. "5e-324|1").
         ;; Strip it before the exponent substring is parsed, else string->number
         ;; misreads "-324|1" as a precision-qualified flonum (-256.0) and corrupts
         ;; the value.
         (bar (let loop ((i 0))
                (cond ((fx>=? i (string-length body0)) #f)
                      ((char=? (string-ref body0 i) #\|) i)
                      (else (loop (fx+ i 1))))))
         (body (if bar (substring body0 0 bar) body0))
         (blen (string-length body))
         (epos (let loop ((i 0))
                 (cond ((fx>=? i blen) #f)
                       ((memv (string-ref body i) '(#\e #\E)) i)
                       (else (loop (fx+ i 1))))))
         (mant (if epos (substring body 0 epos) body))
         (eexp (if epos (string->number (substring body (fx+ epos 1) blen)) 0))
         (mlen (string-length mant))
         (dot (let loop ((i 0))
                (cond ((fx>=? i mlen) #f)
                      ((char=? (string-ref mant i) #\.) i)
                      (else (loop (fx+ i 1))))))
         (digits (if dot
                     (string-append (substring mant 0 dot) (substring mant (fx+ dot 1) mlen))
                     mant))
         (point (+ (if dot dot mlen) eexp)))
    ;; normalize: drop leading zeros (adjusting the point), then trailing zeros
    (let* ((dlen0 (string-length digits))
           (lead (let loop ((i 0))
                   (if (and (fx<? i (fx- dlen0 1)) (char=? (string-ref digits i) #\0))
                       (loop (fx+ i 1)) i)))
           (digits (substring digits lead dlen0))
           (point (- point lead))
           (dlen (let loop ((i (string-length digits)))
                   (if (and (fx>? i 1) (char=? (string-ref digits (fx- i 1)) #\0))
                       (loop (fx- i 1)) i)))
           (digits (substring digits 0 dlen))
           (digits (or (and (fx>=? dlen 16) (flonum-long-digits (flabs x) digits point)) digits))
           (dlen (string-length digits))
           ;; Double.toString never prints fewer than two significant digits, and
           ;; it picks the two-digit decimal NEAREST the double rather than padding
           ;; the shortest one with a 0. For a normal double that is the same
           ;; thing; a subnormal has so few bits that it is not: MIN_VALUE is
           ;; 4.94...E-324, shortest "5" but printed 4.9E-324, and twice it is
           ;; 9.88...E-324, shortest "1" (1.0E-323) but printed 9.9E-324.
           (two (and (fx=? dlen 1) (not (string=? digits "0"))
                     (fl< (flabs x) 2.2250738585072014e-308)   ; subnormal
                     (flonum-two-digits (flabs x))))
           (digits (if two (car two) digits))
           (point (if two (cdr two) point))
           (dlen (string-length digits)))
      (flonum-layout neg? digits point))))

(define (jolt-num->string x)
  (cond
    ;; the -e / element printer renders the infinities and NaN in READABLE form
    ;; (##Inf reads back, like Clojure's REPL/pr); str/print uses "Infinity"/"NaN"
    ;; (see jolt-str-render-one in converters.ss).
    ((and (flonum? x) (fl= x +inf.0)) "##Inf")
    ((and (flonum? x) (fl= x -inf.0)) "##-Inf")
    ((and (flonum? x) (not (fl= x x))) "##NaN")
    ;; str of a bigint has NO N suffix (BigInt.toString); only the readable
    ;; printer adds it (see jolt-pr-readable-base).
    ((fixnum? x) (jolt-fixnum->string x))
    ((and (exact? x) (integer? x)) (number->string x))
    ((flonum? x) (jolt-flonum->string x))
    (else (number->string x))))
