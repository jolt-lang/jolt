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
    ;; BigDecimal.valueOf of the double a Float widens to, as clojure.core/bigdec
    ((jfloat? x) (jolt-bigdec (jfloat-fl x)))
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
        ;; Numbers.toBigDecimal casts anything else to Number: nil is the
        ;; NullPointerException, a string or other non-number the
        ;; ClassCastException, as for any other arithmetic operand.
        (else (jolt-num-cast-throw x))))

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
          ((jbd-rounding-mode-constant? r) (jhost-state r))
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
  (cond ((jfloat? x) (jolt-rationalize (jfloat-fl x)))
        ((jbigdec? x) (/ (jbigdec-unscaled x) (expt 10 (jbigdec-scale x))))
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

;; --- java.math.BigDecimal's INSTANCE members ---------------------------------
;; The value model above is complete and the class answers its ctor and statics,
;; but a jbigdec is a RECORD, not a jhost, so none of the shim method tables
;; reach it: every (.scale b), (.movePointLeft b 6), (.setScale b 4 …) walked the
;; whole dispatch chain and ended at "No matching method … for class
;; java.math.BigDecimal" — or, for a zero-argument member, at the field spelling
;; of the same miss. The JDK's instance API is defined entirely over the unscaled
;; value and the scale, which is exactly what the record carries, so the members
;; are that arithmetic under the Java names, with Java's scale rules — those
;; rules being the whole reason a caller reaches for .movePointLeft or .setScale
;; instead of ordinary arithmetic.

;; RoundingMode is an enum (java/enums.ss) whose constant carries its NAME, the
;; same string *math-context*'s :rounding carries and jbd-round-inc? dispatches
;; on. BigDecimal's pre-Java-5 ROUND_* ints are registered beside them because
;; plenty of ported code still passes those.
(define jbd-rounding-mode-names
  '("UP" "DOWN" "CEILING" "FLOOR" "HALF_UP" "HALF_DOWN" "HALF_EVEN" "UNNECESSARY"))
(define (jbd-rounding-mode-constant? x) (and (jhost? x) (string=? (jhost-tag x) "rounding-mode")))
(define jbd-rounding-mode-constants
  (map (lambda (n) (cons n (make-jhost "rounding-mode" n))) jbd-rounding-mode-names))
;; RoundingMode.valueOf(int): the legacy BigDecimal.ROUND_* number
(register-enum-class! "java.math.RoundingMode" jbd-rounding-mode-constants
  (lambda (n)
    (let ((i (and (number? n) (exact? n) (integer? n) n)))
      (if (and i (<= 0 i 7))
          (cdr (list-ref jbd-rounding-mode-constants i))
          (throw-jvm 'IllegalArgumentException "argument out of range")))))
(class-statics-merge! "java.math.BigDecimal"
  (let loop ((ns jbd-rounding-mode-names) (i 0) (acc '()))
    (if (null? ns) (reverse acc)
        (loop (cdr ns) (+ i 1) (cons (cons (string-append "ROUND_" (car ns)) i) acc)))))

;; Normalize whatever spelling arrived — the enum name, a keyword/symbol, or one
;; of the legacy ints — to the mode name jbd-round-inc? reads. An unknown
;; spelling is the JVM's IllegalArgumentException rather than a silent HALF_UP.
(define (jbd-mode-normalize s)
  (list->string (map (lambda (c) (if (char=? c #\-) #\_ (char-upcase c))) (string->list s))))
(define (jbd-rounding-mode x)
  (let ((name (cond ((jbd-rounding-mode-constant? x) (jhost-state x))
                    ((string? x) (jbd-mode-normalize x))
                    ((symbol-t? x) (jbd-mode-normalize (symbol-t-name x)))
                    ((keyword? x) (jbd-mode-normalize (keyword-t-name x)))
                    ((and (number? x) (exact? x) (integer? x) (<= 0 x 7))
                     (list-ref jbd-rounding-mode-names x))
                    (else #f))))
    (if (and name (member name jbd-rounding-mode-names))
        name
        (throw-jvm (quote IllegalArgumentException)
          (string-append "Invalid rounding mode: " (jolt-final-str x))))))

;; setScale: scaling UP multiplies the unscaled value and is always exact;
;; scaling DOWN divides by the dropped power of ten and rounds. The 1-argument
;; form is RoundingMode.UNNECESSARY, so it raises ArithmeticException exactly
;; when the digits it would drop are not all zero — jbd-round-inc? owns that
;; throw already.
(define (jbd-set-scale bd new-scale mode)
  (let ((u (jbigdec-unscaled bd)) (s (jbigdec-scale bd)))
    (cond
      ((= new-scale s) bd)
      ((> new-scale s) (make-jbigdec (* u (expt 10 (- new-scale s))) new-scale))
      (else
       (let* ((div (expt 10 (- s new-scale)))
              (neg (< u 0)) (au (abs u))
              (q (quotient au div)) (r (remainder au div))
              (q2 (if (jbd-round-inc? q r div mode neg) (+ q 1) q)))
         (make-jbigdec (if neg (- q2) q2) new-scale))))))

;; movePointLeft(n) is this x 10^-n at scale max(scale+n, 0), and movePointRight
;; is the same with -n — so each accepts a negative argument and becomes the
;; other, as on the JVM. A scale that would go negative is folded back into the
;; unscaled value, which is what keeps the pair exact inverses.
(define (jbd-move-point bd n)
  (let ((s (+ (jbigdec-scale bd) n)) (u (jbigdec-unscaled bd)))
    (if (>= s 0) (make-jbigdec u s) (make-jbigdec (* u (expt 10 (- s))) 0))))

;; stripTrailingZeros drops trailing zero digits from the unscaled value,
;; lowering the scale past zero when they sit left of the point (Java 8's
;; behaviour: 600 -> 6E+2). Zero strips to 0 at scale 0.
(define (jbd-strip-trailing-zeros bd)
  (let loop ((u (jbigdec-unscaled bd)) (sc (jbigdec-scale bd)))
    (cond ((= u 0) (make-jbigdec 0 0))
          ((= 0 (remainder u 10)) (loop (quotient u 10) (- sc 1)))
          (else (make-jbigdec u sc)))))

;; toPlainString never uses exponent notation, however extreme the scale.
(define (jbd->plain-string bd)
  (let ((u (jbigdec-unscaled bd)) (sc (jbigdec-scale bd)))
    (if (<= sc 0)
        (number->string (* u (expt 10 (- sc))))
        (let* ((neg (< u 0)) (digs (number->string (abs u))) (dlen (string-length digs))
               (body (if (<= dlen sc)
                         (string-append "0." (make-string (- sc dlen) #\0) digs)
                         (string-append (substring digs 0 (- dlen sc))
                                        "." (substring digs (- dlen sc) dlen)))))
          (if neg (string-append "-" body) body)))))

;; value truncated toward zero, the JVM's toBigInteger / longValue / intValue.
;; jolt has ONE integer type (see :integer-box-model), so the narrowing
;; projections answer the same exact integer rather than wrapping to a width.
(define (jbd->integer bd)
  (truncate (/ (jbigdec-unscaled bd) (expt 10 (jbigdec-scale bd)))))
(define (jbd->integer-exact bd)
  (let ((v (jbd->integer bd)))
    (if (jbigdec=? bd (make-jbigdec v 0))
        v
        (jolt-throw (jolt-host-throwable "java.lang.ArithmeticException"
                                         "Rounding necessary")))))

;; a MathContext argument — the {:precision N :rounding MODE} map
;; with-precision binds — rounds the result to its significant digits. Java
;; overloads several members on it; jolt reads it off the map the same way
;; jbd-mc-round does for *math-context*.
(define (jbd-math-context-arg? x) (and (jolt-map? x) (not (jolt-nil? (jolt-get x jbd-kw-precision)))))
(define (jbd-round-mc bd mc)
  (let ((prec (jnum->exact (jbd-mc-precision mc))))
    ;; precision 0 is java.math.MathContext.UNLIMITED — no rounding at all.
    (if (<= prec 0) bd (jbd-round-prec bd prec (jbd-mc-mode mc)))))

;; divide's three shapes: exact (or *math-context*-rounded, or ArithmeticException
;; on a non-terminating expansion), rounded at THIS value's scale, and rounded at
;; a caller-named scale.
(define (jbd-divide bd args)
  (let ((d (jbd-coerce (car args))) (rest (cdr args)))
    (cond
      ((null? rest) (jbd-mc-round (jbd2-div bd d)))
      ((jbd-math-context-arg? (car rest)) (jbd-round-mc (jbd2-div bd d) (car rest)))
      ((null? (cdr rest))
       (jbd-divide-scaled bd d (jbigdec-scale bd) (jbd-rounding-mode (car rest))))
      (else
       (jbd-divide-scaled bd d (jnum->exact (car rest)) (jbd-rounding-mode (cadr rest)))))))
(define (jbd-divide-scaled bd d scale mode)
  (when (= 0 (jbigdec-unscaled d))
    (jolt-throw (jolt-host-throwable "java.lang.ArithmeticException" "Divide by zero")))
  ;; the exact quotient as a rational, then one rescale — the same rounding step
  ;; setScale takes, so both spellings round identically.
  (let* ((r (/ (* (jbigdec-unscaled bd) (expt 10 (jbigdec-scale d)))
               (* (jbigdec-unscaled d) (expt 10 (jbigdec-scale bd)))))
         (scaled (* r (expt 10 scale)))
         (neg (< scaled 0)) (a (abs scaled))
         (q (floor a)) (rem (- a q))
         (q2 (if (jbd-round-inc? q rem 1 mode neg) (+ q 1) q)))
    (make-jbigdec (if neg (- q2) q2) scale)))

;; precision: significant digits in the unscaled value; zero has precision 1.
(define (jbd-precision bd)
  (if (= 0 (jbigdec-unscaled bd)) 1 (jbd-digits (jbigdec-unscaled bd))))

(define jbd-instance-members
  (list
   ;; --- scale and rounding ---------------------------------------------------
   (cons "scale" (lambda (b) (->num (jbigdec-scale b))))
   (cons "precision" (lambda (b) (->num (jbd-precision b))))
   (cons "unscaledValue" (lambda (b) (jbigdec-unscaled b)))
   (cons "signum" (lambda (b) (->num (let ((u (jbigdec-unscaled b))) (cond ((< u 0) -1) ((> u 0) 1) (else 0))))))
   (cons "setScale"
         (lambda (b n . rest)
           (jbd-set-scale b (jnum->exact n)
                          (if (null? rest) "UNNECESSARY" (jbd-rounding-mode (car rest))))))
   (cons "movePointLeft" (lambda (b n) (jbd-move-point b (jnum->exact n))))
   (cons "movePointRight" (lambda (b n) (jbd-move-point b (- (jnum->exact n)))))
   (cons "scaleByPowerOfTen"
         (lambda (b n) (make-jbigdec (jbigdec-unscaled b) (- (jbigdec-scale b) (jnum->exact n)))))
   (cons "stripTrailingZeros" jbd-strip-trailing-zeros)
   (cons "round" (lambda (b mc) (if (jbd-math-context-arg? mc) (jbd-round-mc b mc) b)))
   ;; --- sign and arithmetic --------------------------------------------------
   (cons "negate" (lambda (b . mc) (jbd-negate b)))
   (cons "abs" (lambda (b . mc) (if (< (jbigdec-unscaled b) 0) (jbd-negate b) b)))
   (cons "plus" (lambda (b . mc) b))
   (cons "add" (lambda (b x . mc) (jbd-mc-round (jbd2+ b (jbd-coerce x)))))
   (cons "subtract" (lambda (b x . mc) (jbd-mc-round (jbd2- b (jbd-coerce x)))))
   (cons "multiply" (lambda (b x . mc) (jbd-mc-round (jbd2* b (jbd-coerce x)))))
   (cons "divide" (lambda (b . args) (jbd-divide b args)))
   (cons "remainder" (lambda (b x . mc) (jbd-int-rem b (jbd-coerce x))))
   (cons "pow"
         (lambda (b n . mc)
           (let ((k (jnum->exact n)))
             (when (< k 0)
               (jolt-throw (jolt-host-throwable "java.lang.ArithmeticException" "Invalid operation")))
             (make-jbigdec (expt (jbigdec-unscaled b) k) (* (jbigdec-scale b) k)))))
   (cons "min" (lambda (b x) (jbd-min2 b (jbd-coerce x))))
   (cons "max" (lambda (b x) (jbd-max2 b (jbd-coerce x))))
   ;; --- comparison, conversion, identity ------------------------------------
   (cons "compareTo" (lambda (b x) (->num (jbd-compare2 b (jbd-coerce x)))))
   ;; jolt's bigdec equality is by VALUE throughout — (= 1.5M 1.50M) is true, the
   ;; hash strips trailing zeros to match — so .equals answers what = answers
   ;; rather than contradicting it with the JVM's scale-sensitive test.
   (cons "equals" (lambda (b x) (and (jbigdec? x) (jbigdec=? b x))))
   (cons "hashCode" (lambda (b) (->num (jbigdec-hasheq b))))
   (cons "toBigInteger" jbd->integer)
   (cons "toBigIntegerExact" jbd->integer-exact)
   (cons "intValue" jbd->integer)
   (cons "longValue" jbd->integer)
   (cons "shortValue" jbd->integer)
   (cons "byteValue" jbd->integer)
   (cons "intValueExact" jbd->integer-exact)
   (cons "longValueExact" jbd->integer-exact)
   (cons "doubleValue" jbigdec->flonum)
   (cons "floatValue" jbigdec->flonum)
   (cons "toString" jbigdec->string)
   (cons "toPlainString" jbd->plain-string)))

(define jbd-instance-tbl (make-hashtable string-hash string=?))
(for-each (lambda (p) (hashtable-set! jbd-instance-tbl (car p) (cdr p))) jbd-instance-members)

(register-method-arm! arm-priority-bigdec
  (lambda (obj method-name rest-args)
    (if (jbigdec? obj)
        (let ((f (hashtable-ref jbd-instance-tbl method-name #f)))
          (if f
              (apply f obj (if (jolt-nil? rest-args) '() (seq->list rest-args)))
              'pass))
        'pass)))
