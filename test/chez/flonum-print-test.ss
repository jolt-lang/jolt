;; A double's printed form (number-print.ss). Run from repo root:
;;   chez --script test/chez/flonum-print-test.ss
;;
;; jolt-flonum->string writes Double.toString's digits two ways: a search in
;; flonum arithmetic for a double some decimal of at most 15 significant digits
;; reads back as (flonum-fast-digits), and the JDK's Schubfach for every other
;; one (flonum-schubfach-digits). Both are checked against the REFERENCE below,
;; over random bit patterns and over short decimals at every magnitude, then
;; pinned to Double.toString's text, then timed against the reference.
;;
;; The reference is the printer jolt had before Schubfach: Chez's shortest
;; round-trip digits re-laid out, with the two places Chez and the JDK disagree
;; patched (the even digit on a 16-17 digit tie, two digits for a subnormal).
;; It is slow and independent of the code under test, which is what an oracle
;; needs. When Schubfach landed, jolt-flonum->string matched JDK 21's
;; Double.toString on 10^6 doubles (random bit patterns, short decimals,
;; subnormals, scaled uniforms), so the reference and the JDK agree there too.
(import (chezscheme))
(load "host/chez/scheme-adapter-runtime.ss")
(load "host/chez/fx-digits.ss")
(load "host/chez/number-print.ss")

;; --- reference ---------------------------------------------------------------
;; The two significant digits nearest the finite, positive double X, as
;; (digits . point) in jolt-flonum->string's terms (value = 0.DIGITS x 10^POINT,
;; trailing zero dropped), or #f when that decimal does not read back as X.
(define (ref-two-digits x)
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
(define (ref-long-digits x digits point)
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

(define (ref-flonum->string x)
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
           (digits (or (and (fx>=? dlen 16) (ref-long-digits (flabs x) digits point)) digits))
           (dlen (string-length digits))
           ;; Double.toString never prints fewer than two significant digits, and
           ;; it picks the two-digit decimal NEAREST the double rather than padding
           ;; the shortest one with a 0. For a normal double that is the same
           ;; thing; a subnormal has so few bits that it is not: MIN_VALUE is
           ;; 4.94...E-324, shortest "5" but printed 4.9E-324, and twice it is
           ;; 9.88...E-324, shortest "1" (1.0E-323) but printed 9.9E-324.
           (two (and (fx=? dlen 1) (not (string=? digits "0"))
                     (fl< (flabs x) 2.2250738585072014e-308)   ; subnormal
                     (ref-two-digits (flabs x))))
           (digits (if two (car two) digits))
           (point (if two (cdr two) point))
           (dlen (string-length digits)))
      (flonum-layout neg? digits point))))


;; --- checks ------------------------------------------------------------------
(define fails 0)
(define (fail! fmt . args)
  (set! fails (+ fails 1))
  (when (< fails 20) (apply printf fmt args)))
(define (agree x0)
  (let* ((x (inexact x0))
         (a (ref-flonum->string x)) (b (jolt-flonum->string x)))
    (unless (string=? a b) (fail! "FAIL: ~s reference=~a printed=~a\n" x a b))))

(define (random-double)          ; any finite double, from a random bit pattern
  (let ((bv (make-bytevector 8)))
    (bytevector-u32-native-set! bv 0 (random (expt 2 32)))
    (bytevector-u32-native-set! bv 4 (random (expt 2 32)))
    (let ((x (bytevector-ieee-double-native-ref bv 0)))
      (if (and (fl= x x) (not (fl= (flabs x) +inf.0))) x (random-double)))))

;; the table's first entry, g(-324), is the JDK's MathUtils.g[0..1]
(unless (= (sf-g -324) (+ (* #x4F0CEDC95A718DD4 (expt 2 63)) #x5B01E8B09AA0D1B5))
  (fail! "FAIL: g(-324) is not the JDK's\n"))

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
;; subnormals (the scaled C_TINY arm and the two-digit rule), the irregular
;; spacing at a power of two, and the binade ends
(do ((i 1 (+ i 1))) ((= i 3000))
  (agree (* i 4.9e-324))
  (agree (expt 2.0 (- i 1100)))
  (agree (* (expt 2.0 (- (random 2098) 1074)) (- 2.0 (expt 2.0 -52)))))

;; Double.toString's text (JDK 21), around both paths' edges: the fast path's
;; bounds (1e-22, 1e15) and 15-digit limit, k = 0 trailing zeros, the plain/E
;; layout switch, the 16-17 digit even tie, subnormals, the extremes
(for-each
  (lambda (p)
    (let ((s (jolt-flonum->string (car p))))
      (unless (string=? s (cdr p)) (fail! "FAIL: ~s printed ~a, Double.toString ~a\n" (car p) s (cdr p)))))
  '((1.1 . "1.1") (0.1 . "0.1") (1200.0 . "1200.0") (100.0 . "100.0") (-2.5 . "-2.5")
    (0.001 . "0.001") (9.99e-4 . "9.99E-4") (1e-5 . "1.0E-5") (1e7 . "1.0E7")
    (9999999.0 . "9999999.0") (12345678.0 . "1.2345678E7")
    (123456789012345.0 . "1.23456789012345E14") (1e15 . "1.0E15")
    (1234567890123456.0 . "1.234567890123456E15") (1.5e-22 . "1.5E-22") (1e-22 . "1.0E-22")
    (1e-23 . "1.0E-23") (0.30000000000000004 . "0.30000000000000004") (1e23 . "1.0E23")
    (1338163280546708.25 . "1.3381632805467082E15") (-650605845684.15625 . "-6.506058456841562E11")
    (0.0 . "0.0") (-0.0 . "-0.0") (5e-324 . "4.9E-324") (1e-323 . "9.9E-324")
    (2.2250738585072014e-308 . "2.2250738585072014E-308")
    (1.7976931348623157e308 . "1.7976931348623157E308") (9007199254740993.0 . "9.007199254740992E15")))

;; Each path must beat the reference by a margin: a short decimal on the fast
;; path, a full-precision double on Schubfach. Ratio in one process, fastest of
;; several rounds each, on this thread's CPU clock.
(define (sample n f) (let loop ((i 0) (acc '())) (if (= i n) acc (loop (+ i 1) (cons (inexact (f)) acc)))))
(define (best xs f)
  (let loop ((r 0) (b #f))
    (if (= r 7)
        b
        (let ((t0 (current-time 'time-thread)))
          (for-each f xs)
          (let* ((d (time-difference (current-time 'time-thread) t0))
                 (ns (+ (* (time-second d) 1000000000) (time-nanosecond d))))
            (loop (+ r 1) (if b (min b ns) ns)))))))
(define (gate label xs ceiling)
  (let ((ratio (/ (inexact (best xs jolt-flonum->string)) (best xs ref-flonum->string))))
    (printf "flonum-print: ~a print in ~,2fx the reference's time (ceiling ~,2f)\n" label ratio ceiling)
    (when (> ratio ceiling) (fail! "FAIL: ~a are not taking their fast path\n" label))))
(gate "short decimals" (sample 20000 (lambda () (/ (random 100000000) 1000))) 0.5)
(gate "full-precision doubles" (sample 20000 (lambda () (random 1.0))) 0.7)

(if (= fails 0)
    (printf "flonum-print: ok\n")
    (begin (printf "flonum-print: ~a failures\n" fails) (exit 1)))
