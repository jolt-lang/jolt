;; BigDecimal. A jbigdec is {unscaled, scale} over Chez arbitrary-precision exact
;; integers; its value is unscaled * 10^-scale (1.5M = {15,1}, 1.00M = {100,2},
;; 3M = {3,0}). M-suffix literals read to a :bigdec form that the back end lowers
;; to jolt-bigdec-from-string; bigdec coerces a number/string. Equality is by
;; value (1.0M = 1.00M), str drops the M, pr keeps it, class is
;; java.math.BigDecimal.
;;
;; Arithmetic follows java.math.BigDecimal's scale rules: add/sub align to the
;; larger scale; multiply adds scales; divide gives the exact quotient at minimal
;; scale or throws ArithmeticException on a non-terminating expansion (a bound
;; *math-context* rounds instead). Clojure contagion: a bigdec mixed with an
;; integer or ratio stays a bigdec; a flonum operand wins (the result is a
;; double). jbd-add/-sub/-mul/-div, jbd-min/-max, the jbd-lt?/…/zero? helpers,
;; and jbd-quot/-rem are the shared engine. Two paths reach it, both leaving the
;; inlined fast path untouched:
;;   - the seq.ss binary dispatch: every generic op (any position — (+ (bigdec x)
;;     1), (reduce + bigs), (quot 10.0 3M)) whose operand is outside Chez's tower
;;     falls to the jolt-*-slow hooks extended below.
;;   - static call position ((+ 1.5M 2.5M), (< a b), (zero? b)): jolt.passes.numeric
;;     tags the invoke :num-kind :bigdec when every operand is statically a bigdec
;;     (M literal or a let-bound copy, integer literals allowed), and the back end
;;     lowers it directly to the jbd op.

(define-record-type jbigdec (fields unscaled scale) (nongenerative chez-jbigdec-v1))


;; "1.50" -> {150,2}; "3" -> {3,0}; "-0.0" -> {0,1}; ".5" -> {5,1};
;; "1.0E300" -> {10,-299}; "1.5E-7" -> {15,8}. Throws NumberFormatException on
;; anything that isn't an optional sign + decimal mantissa (>=1 digit) + optional
;; signed exponent — i.e. the grammar java BigDecimal(String) accepts.
(define (jolt-bigdec-from-string s)
  (define n (string-length s))
  (define (fail)
    (throw-jvm (quote NumberFormatException)
      (string-append "bigdec: cannot parse \"" s "\"")))
  (define (digit? c) (and (char>=? c #\0) (char<=? c #\9)))
  (when (= n 0) (fail))
  ;; split mantissa / exponent at the first e/E
  (let* ((epos (let loop ((i 0))
                 (cond ((= i n) #f)
                       ((memv (string-ref s i) '(#\e #\E)) i)
                       (else (loop (+ i 1))))))
         (mend (or epos n))
         (expstr (and epos (substring s (+ epos 1) n))))
    ;; parse the exponent (optional sign + >=1 digit), 0 if absent
    (define exp
      (if (not expstr) 0
          (let* ((en (string-length expstr)))
            (when (= en 0) (fail))
            (let* ((c0 (string-ref expstr 0))
                   (signed (or (char=? c0 #\+) (char=? c0 #\-)))
                   (body (if signed (substring expstr 1 en) expstr)))
              (when (= (string-length body) 0) (fail))
              (let loop ((i 0))
                (cond ((= i (string-length body)))
                      ((digit? (string-ref body i)) (loop (+ i 1)))
                      (else (fail))))
              (* (if (and signed (char=? c0 #\-)) -1 1) (string->number body))))))
    ;; mantissa: optional sign, >=1 digit, at most one '.'
    (let* ((m0 (cond ((and (> mend 0) (char=? (string-ref s 0) #\-)) 1)
                     ((and (> mend 0) (char=? (string-ref s 0) #\+)) 1)
                     (else 0)))
           (msign (if (= m0 1) (if (char=? (string-ref s 0) #\-) -1 1) 1))
           (dot (let loop ((i m0) (d #f))
                  (cond ((= i mend) d)
                        ((char=? (string-ref s i) #\.) (if d (fail) (loop (+ i 1) i)))
                        ((digit? (string-ref s i)) (loop (+ i 1) d))
                        (else (fail))))))
      (unless dot (unless (> (- mend m0) 0) (fail)))   ; need >=1 char
      (let* ((intp (substring s m0 (or dot mend)))
             (fracp (if dot (substring s (+ dot 1) mend) ""))
             (mant (string-append intp fracp)))
        ;; at least one digit somewhere in the mantissa
        (let loop ((i 0) (any #f))
          (cond ((= i (string-length mant))
                 (unless any (fail)))
                ((digit? (string-ref mant i)) (loop (+ i 1) #t))
                (else (loop (+ i 1) any))))
        (make-jbigdec (* msign (if (= (string-length mant) 0) 0 (string->number mant)))
                      (- (string-length fracp) exp))))))

;; bigdec coercion: a bigdec is itself; an exact integer keeps scale 0; a ratio
;; expands to its exact decimal (throwing ArithmeticException if non-terminating);
;; a string parses as a BigDecimal literal; a flonum routes through its Double.toString
;; text (BigDecimal/valueOf semantics). Inf/NaN/garbage raise NumberFormatException.
(define (jolt-bigdec x)
  (cond
    ((jbigdec? x) x)
    ((and (number? x) (exact? x) (integer? x)) (make-jbigdec x 0))
    ((and (number? x) (exact? x) (rational? x)) (jbd-rational->bigdec x))
    ((string? x) (jolt-bigdec-from-string x))
    ((number? x) (jolt-bigdec-from-string (jolt-num->string x)))
    (else (throw-jvm (if (string? x) (quote NumberFormatException) (quote IllegalArgumentException))
                 (string-append "bigdec: cannot coerce " (jolt-final-str x))))))

;; value equality: unscaled_a * 10^scale_b == unscaled_b * 10^scale_a.
(define (jbigdec=? a b)
  (= (* (jbigdec-unscaled a) (expt 10 (jbigdec-scale b)))
     (* (jbigdec-unscaled b) (expt 10 (jbigdec-scale a)))))

;; render the decimal text (no M), matching java.math.BigDecimal.toString: plain
;; decimal when the adjusted exponent (precision-1-scale) is >= -6 and scale >= 0,
;; else scientific d(.ddd)E+/-exp. Zero prints "0" / "0.0" / "0.00" ...
(define (jbigdec->string bd)
  (let* ((u (jbigdec-unscaled bd)) (sc (jbigdec-scale bd))
         (neg (< u 0)) (digs (number->string (abs u))))
    (define (prefix body) (if neg (string-append "-" body) body))
    (if (= u 0)
        (prefix (if (<= sc 0) "0" (string-append "0." (make-string sc #\0))))
        (let* ((dlen (string-length digs))
               (adjexp (- (+ dlen -1) sc)))
          (prefix
            (if (or (< sc 0) (< adjexp -6))
                (string-append
                  (if (= dlen 1) digs
                      (string-append (substring digs 0 1) "." (substring digs 1 dlen)))
                  "E" (if (>= adjexp 0) "+" "-") (number->string (abs adjexp)))
                (cond ((= sc 0) digs)
                      ((<= dlen sc) (string-append "0." (make-string (- sc dlen) #\0) digs))
                      (else (string-append (substring digs 0 (- dlen sc)) "."
                                           (substring digs (- dlen sc) dlen))))))))))

;; value as a Chez flonum (for double contagion: a flonum operand wins).
(define (jbigdec->flonum b)
  (exact->inexact (/ (jbigdec-unscaled b) (expt 10 (jbigdec-scale b)))))

;; coerce an exact operand to a bigdec; pass a bigdec through. Used on the
;; non-flonum mixed path (bigdec + long -> bigdec). A Ratio converts like
;; Numbers.toBigDecimal — exact decimal expansion or throw on non-terminating.
(define (jbd-coerce x)
  (cond ((jbigdec? x) x)
        ((and (number? x) (exact? x) (integer? x)) (make-jbigdec x 0))
        ((and (number? x) (exact? x) (rational? x)) (jbd-rational->bigdec x))
        (else (throw-jvm (if (string? x) (quote NumberFormatException) (quote IllegalArgumentException))
               (string-append "bigdec arithmetic: cannot coerce operand " (jolt-final-str x))))))

;; --- core arithmetic on the {unscaled, scale} pair --------------------------
;; align two bigdecs to a common scale, returning (unscaled-a unscaled-b scale).
(define (jbd-align a b)
  (let ((sa (jbigdec-scale a)) (sb (jbigdec-scale b)))
    (cond
      ((= sa sb) (values (jbigdec-unscaled a) (jbigdec-unscaled b) sa))
      ((> sa sb) (values (jbigdec-unscaled a)
                         (* (jbigdec-unscaled b) (expt 10 (- sa sb))) sa))
      (else      (values (* (jbigdec-unscaled a) (expt 10 (- sb sa)))
                         (jbigdec-unscaled b) sb)))))

(define (jbd2+ a b) (let-values (((ua ub s) (jbd-align a b))) (make-jbigdec (+ ua ub) s)))
(define (jbd2- a b) (let-values (((ua ub s) (jbd-align a b))) (make-jbigdec (- ua ub) s)))
(define (jbd2* a b) (make-jbigdec (* (jbigdec-unscaled a) (jbigdec-unscaled b))
                                  (+ (jbigdec-scale a) (jbigdec-scale b))))
(define (jbd-negate a) (make-jbigdec (- (jbigdec-unscaled a)) (jbigdec-scale a)))

;; exact rational -> bigdec at minimal scale, or throw if non-terminating. den must
;; factor into 2s and 5s; scale = max(count2, count5).
(define (jbd-rational->bigdec r)
  (let ((p (numerator r)) (q (denominator r)))
    (let loop ((d q) (c2 0) (c5 0))
      (cond
        ((= d 1) (let ((sc (max c2 c5)))
                   (make-jbigdec (* p (quotient (expt 10 sc) q)) sc)))
        ((= 0 (modulo d 2)) (loop (quotient d 2) (+ c2 1) c5))
        ((= 0 (modulo d 5)) (loop (quotient d 5) c2 (+ c5 1)))
        (else (jolt-throw (jolt-host-throwable
                           "java.lang.ArithmeticException"
                           "Non-terminating decimal expansion; no exact representable decimal result.")))))))

;; floor(log10 |r|) for a nonzero exact rational.
(define (jbd-exp10 r)
  (let ((n (abs (numerator r))) (d (denominator r)))
    (if (>= n d)
        (- (jbd-digits (quotient n d)) 1)
        (let loop ((x (* n 10)) (e -1))
          (if (>= x d) e (loop (* x 10) (- e 1)))))))
;; round an exact rational to `prec` significant digits (the MathContext divide).
(define (jbd-rational-prec r prec mode)
  (if (= r 0)
      (make-jbigdec 0 0)
      (let* ((neg (< r 0)) (ar (abs r))
             (s (- prec 1 (jbd-exp10 ar)))
             (scaled (* ar (expt 10 s)))
             (q (floor scaled)) (frac (- scaled q))
             (q2 (if (jbd-round-inc? q frac 1 mode neg) (+ q 1) q))
             (res (make-jbigdec (if neg (- q2) q2) s)))
        ;; a carry can add a digit (9.99 -> 10.0); re-normalizing drops an exact
        ;; trailing zero, never re-rounds.
        (if (> (jbd-digits q2) prec) (jbd-round-prec res prec mode) res))))

(define (jbd2-div a b)
  (when (= 0 (jbigdec-unscaled b))
    (jolt-throw (jolt-host-throwable "java.lang.ArithmeticException" "Divide by zero")))
  ;; a/b = (ua * 10^sb) / (ub * 10^sa) as an exact rational. Unlimited context:
  ;; exact result at minimal scale or throw on a non-terminating expansion. A
  ;; bound *math-context* instead rounds to its precision.
  (let ((r (/ (* (jbigdec-unscaled a) (expt 10 (jbigdec-scale b)))
              (* (jbigdec-unscaled b) (expt 10 (jbigdec-scale a)))))
        (mc (jbd-math-context)))
    (if mc
        (jbd-rational-prec r (jbd-mc-precision mc) (jbd-mc-mode mc))
        (jbd-rational->bigdec r))))

;; integer-division semantics (quot/rem): truncate toward zero, scale 0.
(define (jbd-int-quot a b)
  (when (= 0 (jbigdec-unscaled b))
    (jolt-throw (jolt-host-throwable "java.lang.ArithmeticException" "Divide by zero")))
  (let-values (((ua ub s) (jbd-align a b))) (make-jbigdec (quotient ua ub) 0)))
(define (jbd-int-rem a b)
  (when (= 0 (jbigdec-unscaled b))
    (jolt-throw (jolt-host-throwable "java.lang.ArithmeticException" "Divide by zero")))
  (let-values (((ua ub s) (jbd-align a b)))
    (make-jbigdec (remainder ua ub) (max (jbigdec-scale a) (jbigdec-scale b)))))

;; scale-independent ordering: compare unscaled values aligned to a common scale.
(define (jbd-compare2 a b)
  (let-values (((ua ub s) (jbd-align a b))) (cond ((< ua ub) -1) ((> ua ub) 1) (else 0))))

;; --- *math-context* (with-precision) -----------------------------------------
;; with-precision binds clojure.core/*math-context* to {:precision N :rounding
;; MODE}; every exact bigdec result rounds through it (java.math.MathContext).
(define jbd-kw-precision (keyword #f "precision"))
(define jbd-kw-rounding (keyword #f "rounding"))
(define (jbd-math-context)
  (let ((mc (var-deref "clojure.core" "*math-context*")))
    (if (jolt-nil? mc) #f mc)))
(define (jbd-mc-precision mc) (jolt-get mc jbd-kw-precision))
(define (jbd-mc-mode mc)
  (let ((r (jolt-get mc jbd-kw-rounding)))
    (cond ((symbol-t? r) (symbol-t-name r))
          ((string? r) r)
          (else "HALF_UP"))))

;; should |value| = q + r/div (0 <= r < div) round up in magnitude? neg is the
;; value's sign; r/div may be exact rationals (the division path).
(define (jbd-round-inc? q r div mode neg)
  (cond ((= r 0) #f)
        ((string=? mode "UP") #t)
        ((string=? mode "DOWN") #f)
        ((string=? mode "CEILING") (not neg))
        ((string=? mode "FLOOR") neg)
        ((string=? mode "HALF_DOWN") (> (* 2 r) div))
        ((string=? mode "HALF_EVEN")
         (let ((c (- (* 2 r) div)))
           (cond ((> c 0) #t) ((< c 0) #f) (else (odd? q)))))
        ((string=? mode "UNNECESSARY")
         (jolt-throw (jolt-host-throwable "java.lang.ArithmeticException" "Rounding necessary")))
        (else (>= (* 2 r) div))))     ; HALF_UP, the MathContext default

(define (jbd-digits n) (string-length (number->string (abs n))))
;; round a bigdec to `prec` significant digits with `mode` (a RoundingMode name).
(define (jbd-round-prec bd prec mode)
  (let ((u (jbigdec-unscaled bd)) (s (jbigdec-scale bd)))
    (if (= u 0)
        bd
        (let ((digs (jbd-digits u)))
          (if (<= digs prec)
              bd
              (let* ((drop (- digs prec)) (div (expt 10 drop))
                     (neg (< u 0)) (au (abs u))
                     (q (quotient au div)) (r (remainder au div))
                     (q2 (if (jbd-round-inc? q r div mode neg) (+ q 1) q))
                     (res (make-jbigdec (if neg (- q2) q2) (- s drop))))
                ;; a carry can add a digit back (99 -> 100 at precision 2)
                (if (> (jbd-digits q2) prec) (jbd-round-prec res prec mode) res)))))))
(define (jbd-mc-round x)
  (let ((mc (and (jbigdec? x) (jbd-math-context))))
    (if mc (jbd-round-prec x (jbd-mc-precision mc) (jbd-mc-mode mc)) x)))

;; A binary op over operands that may mix bigdec / integer / flonum. flonum-op is
;; the native fallback for the double-contagion path; bd-op is the exact bigdec op
;; (its result rounds through a bound *math-context*).
(define (jbd-binop flonum-op bd-op a b)
  (if (or (flonum? a) (flonum? b))
      (flonum-op (if (jbigdec? a) (jbigdec->flonum a) a)
                 (if (jbigdec? b) (jbigdec->flonum b) b))
      (jbd-mc-round (bd-op (jbd-coerce a) (jbd-coerce b)))))

;; --- variadic engine ops (Phase-2 emit targets + value-position folds) -------
(define (jbd-fold flonum-op bd-op init xs)
  (let loop ((acc init) (rest xs))
    (if (null? rest) acc (loop (jbd-binop flonum-op bd-op acc (car rest)) (cdr rest)))))

(define (jbd-add . xs)
  (cond ((null? xs) (make-jbigdec 0 0))
        ((null? (cdr xs)) (car xs))
        (else (jbd-fold + jbd2+ (car xs) (cdr xs)))))
(define (jbd-sub . xs)
  (cond ((null? xs) (throw-jvm (quote ArityException) "Wrong number of args (0) passed to: -"))
        ((null? (cdr xs)) (if (jbigdec? (car xs)) (jbd-negate (car xs)) (- (car xs))))
        (else (jbd-fold - jbd2- (car xs) (cdr xs)))))
(define (jbd-mul . xs)
  (cond ((null? xs) (make-jbigdec 1 0))
        ((null? (cdr xs)) (car xs))
        (else (jbd-fold * jbd2* (car xs) (cdr xs)))))
(define (jbd-div . xs)
  (cond ((null? xs) (throw-jvm (quote ArityException) "Wrong number of args (0) passed to: /"))
        ((null? (cdr xs)) (jbd-binop / jbd2-div (make-jbigdec 1 0) (car xs)))
        (else (jbd-fold / jbd2-div (car xs) (cdr xs)))))

;; comparison / predicate helpers (Phase-2 emit targets). A flonum operand demotes
;; to the native comparison on the flonum values.
(define (jbd-cmp-num op flop a b)
  (if (or (flonum? a) (flonum? b))
      (flop (if (jbigdec? a) (jbigdec->flonum a) a) (if (jbigdec? b) (jbigdec->flonum b) b))
      (op (jbd-compare2 (jbd-coerce a) (jbd-coerce b)) 0)))
(define (jbd-lt? a b) (jbd-cmp-num < < a b))
(define (jbd-gt? a b) (jbd-cmp-num > > a b))
(define (jbd-le? a b) (jbd-cmp-num <= <= a b))
(define (jbd-ge? a b) (jbd-cmp-num >= >= a b))
(define (jbd-zero? a) (= 0 (jbigdec-unscaled a)))
(define (jbd-pos? a) (> (jbigdec-unscaled a) 0))
(define (jbd-neg? a) (< (jbigdec-unscaled a) 0))
(define (jbd-quot a b) (jbd-int-quot (jbd-coerce a) (jbd-coerce b)))
(define (jbd-rem a b) (jbd-int-rem (jbd-coerce a) (jbd-coerce b)))

;; min/max compare by value but return the ORIGINAL operand (its type and scale
;; unchanged), matching java/Clojure: (min 1M 2.0) -> 1M, (max 1M 2.0) -> 2.0,
;; (min 1.50M 2M) -> 1.50M. Comparison handles a bigdec mixed with an int / flonum.
(define (jbd-value-compare a b)
  (if (or (flonum? a) (flonum? b))
      (let ((fa (if (jbigdec? a) (jbigdec->flonum a) a)) (fb (if (jbigdec? b) (jbigdec->flonum b) b)))
        (cond ((< fa fb) -1) ((> fa fb) 1) (else 0)))
      (jbd-compare2 (jbd-coerce a) (jbd-coerce b))))
;; strict comparison so a tie keeps the second operand, like Clojure's
;; (if (< x y) x y) / (if (> x y) x y): (max 1.5M 1.50M) -> 1.50M.
(define (jbd-min2 a b) (if (< (jbd-value-compare a b) 0) a b))
(define (jbd-max2 a b) (if (> (jbd-value-compare a b) 0) a b))
(define (jbd-min x . xs) (fold-left jbd-min2 x xs))
(define (jbd-max x . xs) (fold-left jbd-max2 x xs))

;; --- wire into the value model ----------------------------------------------
(def-var! "clojure.core" "bigdec" jolt-bigdec)

;; The seq.ss binary numeric dispatch (jolt-add2/… and the jolt-n* macros) routes
;; any op whose operand is outside Chez's tower to the *-slow hooks; extend each
;; with a bigdec arm. Every arithmetic position (call, value, higher-order)
;; funnels through these, so contagion and *math-context* rounding apply
;; uniformly. min/max need no arm: the generic jolt-min2 compares through
;; jolt-num-cmp-slow and returns the original operand.
;; Slow-hook arms are registered through the core's register-num-arm! (seq.ss) —
;; bigdec never mutates a core var directly. jbd-num-arm registers a binary arm:
;; handler runs when either operand is a BigDecimal, otherwise the chain
;; declines to prev.
(register-num-arm! 'num-slow?
  (lambda (prev) (lambda (x) (or (jbigdec? x) (prev x)))))
(define (jbd-num-arm op handler)
  (register-num-arm! op
    (lambda (prev)
      (lambda (a b)
        (if (or (jbigdec? a) (jbigdec? b)) (handler a b) (prev a b))))))
(jbd-num-arm 'add-slow (lambda (a b) (jbd-binop + jbd2+ a b)))
(jbd-num-arm 'sub-slow (lambda (a b) (jbd-binop - jbd2- a b)))
(jbd-num-arm 'mul-slow (lambda (a b) (jbd-binop * jbd2* a b)))
(jbd-num-arm 'div-slow (lambda (a b) (jbd-binop / jbd2-div a b)))
(register-num-arm! 'num-cmp-slow
  (lambda (prev)
    (lambda (a b)
      (if (and (or (jbigdec? a) (jbigdec? b)) (jbd-numberish? a) (jbd-numberish? b))
          (jbd-value-compare a b)
          (prev a b)))))
;; quot/rem/mod: a double operand demotes to the double path; exact operands use
;; the integer-division bigdec ops (mod = rem, floor-adjusted to the divisor's sign).
(define (jbd->num x) (if (jbigdec? x) (jbigdec->flonum x) x))
(jbd-num-arm 'quot-slow
  (lambda (a b) (if (or (flonum? a) (flonum? b))
                    (jolt-quot (jbd->num a) (jbd->num b))
                    (jbd-int-quot (jbd-coerce a) (jbd-coerce b)))))
(jbd-num-arm 'rem-slow
  (lambda (a b) (if (or (flonum? a) (flonum? b))
                    (jolt-rem (jbd->num a) (jbd->num b))
                    (jbd-int-rem (jbd-coerce a) (jbd-coerce b)))))
(jbd-num-arm 'mod-slow
  (lambda (a b)
    (if (or (flonum? a) (flonum? b))
        (jolt-mod (jbd->num a) (jbd->num b))
        (let* ((bb (jbd-coerce b))
               (m (jbd-int-rem (jbd-coerce a) bb)))
          (if (or (jbd-zero? m) (eq? (jbd-neg? m) (jbd-neg? bb))) m (jbd2+ m bb))))))
;; unary shims: inc/dec and the sign predicates take a bigdec arm. Registration
;; updates call-position references; the re-def-var! updates the var cell AND
;; claims the wrapped proc's class name before the prelude's inc'/dec' aliases
;; are defined ((type inc) stays clojure.core$inc — first def wins in the class
;; registry).
(define jbd-one (make-jbigdec 1 0))
(register-num-arm! 'inc (lambda (prev) (lambda (x) (if (jbigdec? x) (jbd-mc-round (jbd2+ x jbd-one)) (prev x)))))
(register-num-arm! 'dec (lambda (prev) (lambda (x) (if (jbigdec? x) (jbd-mc-round (jbd2- x jbd-one)) (prev x)))))
(register-num-arm! 'zero? (lambda (prev) (lambda (x) (if (jbigdec? x) (jbd-zero? x) (prev x)))))
(register-num-arm! 'pos? (lambda (prev) (lambda (x) (if (jbigdec? x) (jbd-pos? x) (prev x)))))
(register-num-arm! 'neg? (lambda (prev) (lambda (x) (if (jbigdec? x) (jbd-neg? x) (prev x)))))
;; a BigDecimal IS a number (java.lang.Number): extend the number? native so the
;; predicate — and everything defined over it (num, =='s guard) — accepts it.
;; The compiled fast paths test Chez number? directly and are unaffected.
(register-num-arm! 'number? (lambda (prev) (lambda (x) (if (jbigdec? x) #t (prev x)))))
(def-var! "clojure.core" "number?" jolt-number?)
(def-var! "clojure.core" "inc" jolt-inc)
(def-var! "clojure.core" "dec" jolt-dec)
(def-var! "clojure.core" "zero?" jolt-zero?)
(def-var! "clojure.core" "pos?" jolt-pos?)
(def-var! "clojure.core" "neg?" jolt-neg?)

;; rationalize: reference Clojure goes through BigDecimal.valueOf(double) — the
;; SHORTEST decimal print of the double, not its exact binary value — so
;; (rationalize 1.1) is 11/10. A bigdec is exact already; other exacts pass through.
(define (jolt-rationalize x)
  (cond ((jbigdec? x) (/ (jbigdec-unscaled x) (expt 10 (jbigdec-scale x))))
        ((flonum? x)
         (if (or (nan? x) (infinite? x))
             (jolt-throw (jolt-host-throwable "java.lang.NumberFormatException"
                                              (string-append "Invalid input: " (number->string x))))
             (let ((bd (jolt-bigdec-from-string (jolt-num->string x))))
               (/ (jbigdec-unscaled bd) (expt 10 (jbigdec-scale bd))))))
        ((number? x) x)
        (else (jolt-num-cast-throw x))))
(def-var! "clojure.core" "rationalize" jolt-rationalize)

;; double/float of a bigdec is its flonum value.
(register-num-arm! 'double-slow
  (lambda (prev)
    (lambda (x) (if (jbigdec? x) (jbigdec->flonum x) (prev x)))))

;; narrow casts truncate a bigdec like Number.longValue.
(register-num-arm! 'cast-truncate-slow
  (lambda (prev)
    (lambda (x)
      (if (jbigdec? x)
          (truncate (/ (jbigdec-unscaled x) (expt 10 (jbigdec-scale x))))
          (prev x)))))

;; compare: a bigdec arm on the core's arm list (enables compare / sort /
;; sorted collections). A bigdec vs a plain number compares by value; bigdec vs
;; bigdec is scale-independent.
(define (jbd-numberish? x) (or (jbigdec? x) (number? x)))
(register-compare-arm!
  (lambda (a b) (and (or (jbigdec? a) (jbigdec? b)) (jbd-numberish? a) (jbd-numberish? b)))
  (lambda (a b)
    (if (or (flonum? a) (flonum? b))
        (let ((fa (if (jbigdec? a) (jbigdec->flonum a) a))
              (fb (if (jbigdec? b) (jbigdec->flonum b) b)))
          (cond ((< fa fb) -1) ((> fa fb) 1) (else 0)))
        (jbd-compare2 (jbd-coerce a) (jbd-coerce b)))))

;; equality: a bigdec equals only another bigdec, by value (matching (= 3M 3) = false).
(register-eq-arm! (lambda (a b) (or (jbigdec? a) (jbigdec? b)))
                  (lambda (a b) (and (jbigdec? a) (jbigdec? b) (jbigdec=? a b))))

;; == value-equality across the tower — with a double both sides compare as
;; doubles, otherwise as bigdec ((== 3M 3) is true while (= 3M 3) stays false).
(define (jbd-equiv2 a b)
  (cond
    ((or (flonum? a) (flonum? b))
     (let ((fa (if (jbigdec? a) (jbigdec->flonum a) (if (flonum? a) a (exact->inexact a))))
           (fb (if (jbigdec? b) (jbigdec->flonum b) (if (flonum? b) b (exact->inexact b)))))
       (= fa fb)))
    (else (jbigdec=? (jbd-coerce a) (jbd-coerce b)))))
(register-num-arm! 'num-equiv-slow
  (lambda (prev)
    (lambda (a b)
      (if (or (jbigdec? a) (jbigdec? b)) (jbd-equiv2 a b) (prev a b)))))

;; str drops the M; pr/pr-str keep it.
(register-str-render! jbigdec? jbigdec->string)
(register-pr-arm! jbigdec? (lambda (x) (string-append (jbigdec->string x) "M")))

;; hasheq: Clojure Numbers.hasheq(BigDecimal) — strip trailing zeros, then
;; strippedUnscaled.hashCode() * 31 + stripped.scale() (int32). Matches JVM so
;; bigdec keys collide correctly and (= 1.5M 1.50M) implies equal hashes.
(define (jbigdec-hasheq bd)
  (let loop ((u (jbigdec-unscaled bd)) (sc (jbigdec-scale bd)))
    (if (or (<= sc 0) (= u 0) (not (= 0 (modulo u 10))))
        (i32 (+ (* 31 (big-integer-hashcode u)) sc))
        (loop (quotient u 10) (- sc 1)))))
(register-hash-arm! jbigdec? jbigdec-hasheq)

;; class / decimal?
(register-class-arm! jbigdec? (lambda (x) "java.math.BigDecimal"))
(register-num-arm! 'decimal? (lambda (prev) (lambda (x) (or (jbigdec? x) (prev x)))))
(def-var! "clojure.core" "decimal?" jolt-decimal?)

;; --- java.math.BigDecimal as a host class -----------------------------------
;; The bigdec VALUE model above is complete (literals, arithmetic, class, hash);
;; what was missing is the class itself as a construction target. Clojure code
;; that wants an exact scale writes (BigDecimal. "1.50") rather than calling
;; bigdec — tools.reader's own number reader does, which is why reading "1M"
;; through it failed with "No matching ctor found for class BigDecimal".
;; A trailing MathContext argument is accepted and ignored: rounding here follows
;; *math-context*, as everywhere else in this file.
(define (jbd-class-ctor x . _)
  (if (string? x)
      (jolt-bigdec-from-string x)
      (jolt-bigdec x)))
(define jbd-class-statics
  ;; BigDecimal.valueOf(long unscaled, int scale) is unscaled x 10^-scale — the
  ;; scale is the value, not a formatting hint. Dropping it returned 50 where the
  ;; JVM returns 0.050.
  (list (cons "valueOf"
              (lambda (x . rest)
                (if (null? rest)
                    (jolt-bigdec x)
                    (make-jbigdec (jnum->exact x) (jnum->exact (car rest))))))
        (cons "ZERO" (jolt-bigdec-from-string "0"))
        (cons "ONE" (jolt-bigdec-from-string "1"))
        (cons "TEN" (jolt-bigdec-from-string "10"))))
(for-each
  (lambda (n)
    (register-class-ctor! n jbd-class-ctor)
    (register-class-statics! n jbd-class-statics))
  '("BigDecimal" "java.math.BigDecimal"))
