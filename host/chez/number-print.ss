;; number-print.ss — how jolt prints a number, as Double.toString does. Portable
;; (R7RS strings and arithmetic, the fx/bitwise spellings both hosts bind), so
;; Chez (rt.ss loads it) and Gambit (rt-core.ss includes it) print alike.

;; jolt has a numeric tower (exact integer / ratio / double, distinguished by
;; class). Exact integer-valued values print without a ".0" ((+ 1 2) -> "3");
;; a double prints with one ((* 1.0 5) -> "5.0", as the JVM does).

;; Double.toString layout: plain decimal when 1e-3 <= |x| < 1e7, otherwise
;; scientific d.dddE±x with one digit before the point; the mantissa always
;; carries a decimal point ("1.0E100", "2.3E-4", "1.2345678E7").
;;
;; The digits are the JDK's own (JDK 19+, jdk.internal.math.DoubleToDecimal):
;; Raffaello Giulietti's Schubfach, ported below. A double some decimal of at
;; most 15 significant digits reads back as is answered first by a cheaper
;; search in flonum arithmetic (flonum-fast-digits); both give the shortest
;; decimal, the nearest of those, the even one on a tie, and at least two
;; digits for a subnormal (4.9E-324). The host's number->string is not used:
;; its digit search is arbitrary-precision and its ties go the other way.


;; The fast path: a double that some decimal of at most 15 significant digits
;; reads back as -- 1.1, 3.25, 0.001, what EDN and JSON mostly carry -- has its
;; digits found in flonum arithmetic, about a third of Schubfach's time, which
;; pays a 126-bit multiply.
;;
;; For k = 0, 1, ... take y = x * 10^k and n = round(y). With n < 2^53 and
;; 10^k <= 10^22 both exact, the division is correctly rounded, so (= (/ n 10^k) x)
;; is exactly "the decimal n x 10^-k reads back as x". The first k with a hit has
;; the fewest fractional digits, so the fewest significant ones: the shortest
;; decimal. No other integer can hit: below 10^15 a double's spacing scaled by
;; 10^k is under 0.23, so a k-place decimal that reads back as x lies within 0.12
;; of x * 10^k, and y is within 0.12 of that; one integer, round(y), is that close,
;; so there is no nearest-or-even choice to make. Past 15 digits Schubfach answers.
;; Answers (digits . point) for a positive X, or #f.
(define flonum-pow10
  (let ((v (make-vector 23 1.0)))
    (do ((k 1 (fx+ k 1))) ((fx= k 23) v)
      (vector-set! v k (fl* 10.0 (vector-ref v (fx- k 1)))))))
;; (digits . point) for the decimal N x 10^-K, N a positive fixnum: N's digits
;; with its trailing zeros dropped (k = 0 leaves them: 1200.0), written by hand
;; because number->string of a fixnum costs ~0.2 us, most of a short double's print
;; The digit producers below answer (M . POINT): M the digits as a fixnum with
;; its trailing zeros dropped, value = 0.M x 10^POINT. jolt-flonum->string lays
;; M out straight into its result (flonum-layout/fx); the string forms, (DIGITS
;; . POINT), are built from these for the callers that want text (#1713).
(define (fx-digits->string m)
  (let* ((len (fx-decimal-length m)) (s (make-string len #\0)))
    (fx-fill-digits! s m len)
    s))
(define (fx-digits-pair->strings d) (cons (fx-digits->string (car d)) (cdr d)))
(define (flonum-fixnum-digits/fx n k)
  ;; fx-digits.ss's divide-free helpers: each fxquotient here was a dependent
  ;; hardware divide, ~110 ns for the "15" of 1.5
  (let strip ((m n) (z 0))
    (let ((q (fx-quot10 m)))
      (if (fx=? m (fx* q 10))
          (strip q (fx+ z 1))
          (cons m (fx- (fx+ (fx-decimal-length m) z) k))))))
(define (flonum-fixnum-digits n k) (fx-digits-pair->strings (flonum-fixnum-digits/fx n k)))
(define (flonum-fast-digits/fx x)
  (and (fl>= x 1e-22) (fl< x 1e15)
       (let loop ((k 0))
         (and (fx< k 23)
              (let* ((p (vector-ref flonum-pow10 k))
                     (y (fl* x p)))
                (and (fl< y 1e15)
                     (let ((n (flround y)))
                       (if (and (fl>= n 1.0) (fl= (fl/ n p) x))
                           (flonum-fixnum-digits/fx (flonum->fixnum n) k)
                           (loop (fx+ k 1))))))))))
(define (flonum-fast-digits x) (let ((d (flonum-fast-digits/fx x))) (and d (fx-digits-pair->strings d))))

;; --- Schubfach --------------------------------------------------------------
;; DoubleToDecimal.toDecimal, ported. The paper: R. Giulietti, "The Schubfach way
;; to render doubles" (2020). Names follow the JDK source; vb, vbl and vbr are the
;; double and its rounding interval's ends scaled by 10^-k, with a sticky low bit.
;; g(k) is floor(10^-k / 2^r) + 1 for the r putting it in [2^125, 2^126); the
;; table is the JDK's MathUtils.g, computed on first use rather than spelled out
;; (617 entries; the first is checked by make flonumprint against the JDK's).
;; The JDK splits g in two 63-bit halves and multiplies in longs; here g is one
;; exact integer, and rop takes the product whole: its floor over 2^127 with bits
;; 64..126 as the sticky bit is exactly what the JDK's rop computes.
;; fixnum arithmetic: E is a binary exponent (|e| <= 1074) or a decimal one
;; (|e| <= 324), so every product stays below 2^50 (#1713)
(define (sf-flog10pow2 e) (fxsra (fx* e 661971961083) 41))
(define (sf-flog10-3q-pow2 e) (fxsra (fx+ (fx* e 661971961083) -274743187321) 41))
(define (sf-flog2pow10 e) (fxsra (fx* e 913124641741) 38))
(define sf-g-table #f)
(define (sf-g k)
  (let ((v (or sf-g-table
               (let ((v (make-vector 617)))
                 (do ((k -324 (+ k 1))) ((> k 292))
                   (let ((r (- (sf-flog2pow10 (- k)) 125)))
                     (vector-set! v (+ k 324) (+ 1 (floor (/ (expt 10 (- k)) (expt 2 r)))))))
                 ;; filling is idempotent: a racing first use builds an equal table
                 (set! sf-g-table v)
                 v))))
    (vector-ref v (+ k 324))))
(define (sf-rop p)
  (let ((v (bitwise-arithmetic-shift-right p 127)))
    (if (eqv? 0 (bitwise-bit-field p 64 127)) v (fxior v 1))))
;; The same rop over g's 28-bit limbs and a multiplier CP below 2^59, in
;; fixnums (#1713). The JDK takes g as two 63-bit halves and multiplies in longs
;; with multiplyHigh; a Chez fixnum is 61 bits, so the product is formed limb by
;; limb instead: every limb product is below 2^56 and every column sum below
;; 2^58. Bits 127 and up are the value, bits 64..126 the sticky bit, exactly as
;; sf-rop reads the bignum product (checked against it for every k and five
;; million multipliers). The bignum multiply, shifts and bit-field read cost
;; ~100 ns a rop, three per double.
(define sf-m28 #xFFFFFFF)
(define (sf-rop/fx gl cp)
  (let* ((c0 (sa-ufxand cp sf-m28)) (c1 (sa-ufxand (sa-ufxsra cp 28) sf-m28)) (c2 (sa-ufxsra cp 56))
         (g0 (sa-uvector-ref gl 0)) (g1 (sa-uvector-ref gl 1)) (g2 (sa-uvector-ref gl 2))
         (g3 (sa-uvector-ref gl 3)) (g4 (sa-uvector-ref gl 4))
         (t1 (sa-ufx+ (sa-ufxsra (sa-ufx* g0 c0) 28) (sa-ufx+ (sa-ufx* g1 c0) (sa-ufx* g0 c1))))
         (t2 (sa-ufx+ (sa-ufxsra t1 28) (sa-ufx+ (sa-ufx* g2 c0) (sa-ufx+ (sa-ufx* g1 c1) (sa-ufx* g0 c2)))))
         (t3 (sa-ufx+ (sa-ufxsra t2 28) (sa-ufx+ (sa-ufx* g3 c0) (sa-ufx+ (sa-ufx* g2 c1) (sa-ufx* g1 c2)))))
         (t4 (sa-ufx+ (sa-ufxsra t3 28) (sa-ufx+ (sa-ufx* g4 c0) (sa-ufx+ (sa-ufx* g3 c1) (sa-ufx* g2 c2)))))
         (t5 (sa-ufx+ (sa-ufxsra t4 28) (sa-ufx+ (sa-ufx* g4 c1) (sa-ufx* g3 c2))))
         (t6 (sa-ufx+ (sa-ufxsra t5 28) (sa-ufx* g4 c2)))
         (v (sa-ufxior (sa-ufxsra (sa-ufxand t4 sf-m28) 15)
                       (sa-ufxior (sa-ufxsll (sa-ufxand t5 sf-m28) 13) (sa-ufxsll t6 41))))
         (sticky (sa-ufxior (sa-ufxsra (sa-ufxand t2 sf-m28) 8)
                            (sa-ufxior (sa-ufxand t3 sf-m28) (sa-ufxand t4 #x7FFF)))))
    (if (sa-ufx=? sticky 0) v (sa-ufxior v 1))))
;; g(k) as its five 28-bit limbs, low first, built with the g table
(define sf-gl-table #f)
(define (sf-gl k)
  (let ((v (or sf-gl-table
               (let ((v (make-vector 617)))
                 (do ((k -324 (+ k 1))) ((> k 292))
                   (let ((g (sf-g k)))
                     (vector-set! v (+ k 324)
                                  (vector (bitwise-and g sf-m28)
                                          (bitwise-and (bitwise-arithmetic-shift-right g 28) sf-m28)
                                          (bitwise-and (bitwise-arithmetic-shift-right g 56) sf-m28)
                                          (bitwise-and (bitwise-arithmetic-shift-right g 84) sf-m28)
                                          (bitwise-arithmetic-shift-right g 112)))))
                 (set! sf-gl-table v)
                 v))))
    (vector-ref v (+ k 324))))
;; (digits . point) of F x 10^E, F a positive fixnum: F's digits with its
;; trailing zeros dropped, two per step (values.ss fx-fill-digits!)
(define (sf-to-digits/fx f e)
  (let strip ((m f) (z 0))
    (let ((q (fx-quot10 m)))
      (if (fx=? m (fx* q 10))
          (strip q (fx+ z 1))
          (cons m (+ (fx-decimal-length m) z e))))))
(define (sf-to-decimal q c dk)
  (let* ((out (fxand c 1))
         (cb (fxsll c 2))
         (regular (or (not (fx=? c 4503599627370496)) (fx=? q -1074)))   ; C_MIN = 2^52, Q_MIN
         (k (if regular (sf-flog10pow2 q) (sf-flog10-3q-pow2 q)))
         (h (+ q (sf-flog2pow10 (- k)) 2))
         ;; the multipliers of cb and its interval's ends, cbl = cb - 2 (- 1
         ;; at C_MIN) and cbr = cb + 2, shifted by h; below 2^59 for every
         ;; double, which sf-rop/fx needs, with the bignum rop kept for any other
         (cpr (fxsll (fx+ cb 2) h))
         (cp (fxsll cb h))
         (cpl (fxsll (fx- cb (if regular 2 1)) h))
         (fixnum-rop? (fx<? cpr 576460752303423488))   ; 2^59
         (gl (and fixnum-rop? (sf-gl k)))
         (vb (if gl (sf-rop/fx gl cp) (sf-rop (* (sf-g k) cp))))
         (vbl (if gl (sf-rop/fx gl cpl) (sf-rop (* (sf-g k) cpl))))
         (vbr (if gl (sf-rop/fx gl cpr) (sf-rop (* (sf-g k) cpr))))
         (s (fxsra vb 2)))
    (or (and (fx>=? s 100)
             (let* ((sp10 (fx* 10 (fxquotient s 10)))
                    (tp10 (fx+ sp10 10))
                    (upin (fx<=? (fx+ vbl out) (fxsll sp10 2)))
                    (wpin (fx<=? (fx+ (fxsll tp10 2) out) vbr)))
               (and (not (eq? upin wpin)) (sf-to-digits/fx (if upin sp10 tp10) k))))
        (let* ((t (fx+ s 1))
               (uin (fx<=? (fx+ vbl out) (fxsll s 2)))
               (win (fx<=? (fx+ (fxsll t 2) out) vbr)))
          (if (not (eq? uin win))
              (sf-to-digits/fx (if uin s t) (+ k dk))
              (let ((cmp (fx- vb (fxsll (fx+ s t) 1))))
                (sf-to-digits/fx (if (or (fx<? cmp 0) (and (fx=? cmp 0) (fx=? 0 (fxand s 1)))) s t)
                             (+ k dk))))))))
;; (digits . point) for a finite, nonzero double X, from its bit pattern
(define (flonum-schubfach-digits/fx x)
  (let* ((hi (sa-flonum-hi32 x))
         (bq (fxand (fxsra hi 20) #x7ff))
         (t (fxior (fxsll (fxand hi #xfffff) 32) (sa-flonum-lo32 x))))
    (if (fx>? bq 0)
        (let ((mq (fx- 1075 bq)) (c (fxior 4503599627370496 t)))
          ;; an integer below 2^53 is its own digits (the JDK's fast path)
          (or (and (fx>? mq 0) (fx<? mq 53)
                   (let ((f (fxsra c mq)))
                     (and (fx=? (fxsll f mq) c) (sf-to-digits/fx f 0))))
              (sf-to-decimal (fx- 0 mq) c 0)))
        ;; subnormal: below C_TINY the JDK scales by 10 for a second digit
        (if (fx<? t 3) (sf-to-decimal -1074 (fx* 10 t) -1) (sf-to-decimal -1074 t 0)))))
(define (flonum-schubfach-digits x) (fx-digits-pair->strings (flonum-schubfach-digits/fx x)))

;; Double.toString's layout of the decimal 0.DIGITS x 10^POINT
;; Double.toString's layout of (digits . point). The plain decimal forms
;; (1e-3 <= |x| < 1e7) are written straight into one string of their final
;; length (#1713): building "1.5" out of two substrings and two appends was
;; ~70 ns of a (str 1.5).
(define (flonum-copy-digits! s at digits from to)
  (do ((i from (fx+ i 1)) (j at (fx+ j 1))) ((fx=? i to)) (string-set! s j (string-ref digits i))))
(define (flonum-layout neg? digits point)
  (let ((dlen (string-length digits)) (sign (if neg? 1 0)))
    (cond
      ((string=? digits "0") (if neg? "-0.0" "0.0"))
      ((and (fixnum? point) (fx>=? point -2) (fx<=? point 7))
       (let ((s (cond
                  ((fx<=? point 0)              ; 0.00ddd
                   (let ((s (make-string (fx+ sign 2 (fx- 0 point) dlen) #\0)))
                     (string-set! s (fx+ sign 1) #\.)
                     (flonum-copy-digits! s (fx+ sign 2 (fx- 0 point)) digits 0 dlen)
                     s))
                  ((fx>=? point dlen)           ; ddd00.0
                   (let ((s (make-string (fx+ sign point 2) #\0)))
                     (flonum-copy-digits! s sign digits 0 dlen)
                     (string-set! s (fx+ sign point) #\.)
                     s))
                  (else                         ; dd.ddd
                   (let ((s (make-string (fx+ sign dlen 1) #\.)))
                     (flonum-copy-digits! s sign digits 0 point)
                     (flonum-copy-digits! s (fx+ sign point 1) digits point dlen)
                     s)))))
         (when neg? (string-set! s 0 #\-))
         s))
      (else
       (let ((res (string-append (substring digits 0 1) "."
                                 (if (fx>? dlen 1) (substring digits 1 dlen) "0")
                                 "E" (number->string (- point 1)))))
         (if neg? (string-append "-" res) res))))))

;; Double.toString's digits of a finite double X's magnitude, as (digits .
;; point): value = 0.DIGITS x 10^POINT, no trailing zero, ("0" . 1) for zero.
;; java.util.Formatter rounds these same digits (FormattedFPDecimal, JDK 21),
;; so natives-format.ss starts from them too.
(define (jolt-flonum-digits x)
  (if (fl= x 0.0)
      (cons "0" 1)
      (or (flonum-fast-digits (flabs x)) (flonum-schubfach-digits (flabs x)))))
(define (jolt-flonum->string x)
  (cond ((fl= x 0.0) (if (fx=? 0 (sa-flonum-hi32 x)) "0.0" "-0.0"))
        ;; NaN and the infinities print as jolt-num->string decides, before here
        ((not (fl= (fl- x x) 0.0)) (number->string x))
        (else
         (let* ((ax (flabs x))
                (d (or (flonum-fast-digits/fx ax) (flonum-schubfach-digits/fx ax))))
           (flonum-layout/fx (fl<? x 0.0) (car d) (cdr d))))))
;; flonum-layout's plain decimal forms with the digits M (a fixnum, no trailing
;; zero) written once, into the result itself: the digit string and the copy
;; out of it were a third of a 17-digit double's print. The scientific form,
;; rare, goes through the string layout.
(define (flonum-layout/fx neg? m point)
  (let ((dlen (fx-decimal-length m)) (sign (if neg? 1 0)))
    (if (and (fixnum? point) (fx>=? point -2) (fx<=? point 7))
        (let ((s (cond
                   ((fx<=? point 0)              ; 0.00ddd
                    (let* ((len (fx+ sign 2 (fx- 0 point) dlen)) (s (make-string len #\0)))
                      (string-set! s (fx+ sign 1) #\.)
                      (fx-fill-digits! s m len)
                      s))
                   ((fx>=? point dlen)           ; ddd00.0
                    (let ((s (make-string (fx+ sign point 2) #\0)))
                      (fx-fill-digits! s m (fx+ sign dlen))
                      (string-set! s (fx+ sign point) #\.)
                      s))
                   (else                         ; dd.ddd: the digits, then the
                    (let* ((len (fx+ sign dlen 1)) (s (make-string len #\0)))
                      (fx-fill-digits! s m len)   ; integer part moved left one
                      (do ((i sign (fx+ i 1))) ((fx=? i (fx+ sign point)))
                        (string-set! s i (string-ref s (fx+ i 1))))
                      (string-set! s (fx+ sign point) #\.)
                      s)))))
          (when neg? (string-set! s 0 #\-))
          s)
        (flonum-layout neg? (fx-digits->string m) point))))

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
