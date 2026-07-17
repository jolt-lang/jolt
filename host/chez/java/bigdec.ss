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

(define (bd-index-char s ch)
  (let loop ((i 0))
    (cond ((>= i (string-length s)) #f)
          ((char=? (string-ref s i) ch) i)
          (else (loop (+ i 1))))))

;; "1.50" -> {150,2}; "3" -> {3,0}; "-0.0" -> {0,1}; ".5" -> {5,1}.
(define (jolt-bigdec-from-string s)
  (let* ((neg (and (> (string-length s) 0) (char=? (string-ref s 0) #\-)))
         (sgn (and (> (string-length s) 0) (or neg (char=? (string-ref s 0) #\+))))
         (s1 (if sgn (substring s 1 (string-length s)) s))
         (sign (if neg -1 1))
         (dot (bd-index-char s1 #\.)))
    (if dot
        (let* ((intp (substring s1 0 dot))
               (fracp (substring s1 (+ dot 1) (string-length s1)))
               (digs (string-append intp fracp))
               (unscaled (if (= 0 (string-length digs)) 0 (string->number digs))))
          (make-jbigdec (* sign unscaled) (string-length fracp)))
        (make-jbigdec (* sign (string->number s1)) 0))))

;; bigdec coercion: a bigdec is itself; an exact integer keeps scale 0; a string
;; or any other number routes through its decimal text.
(define (jolt-bigdec x)
  (cond
    ((jbigdec? x) x)
    ((and (number? x) (exact? x) (integer? x)) (make-jbigdec x 0))
    ((string? x) (jolt-bigdec-from-string x))
    ((number? x) (jolt-bigdec-from-string (jolt-num->string x)))
    (else (throw-jvm (if (string? x) (quote NumberFormatException) (quote IllegalArgumentException))
                (string-append "bigdec: cannot coerce " (jolt-final-str x))))))

;; value equality: unscaled_a * 10^scale_b == unscaled_b * 10^scale_a.
(define (jbigdec=? a b)
  (= (* (jbigdec-unscaled a) (expt 10 (jbigdec-scale b)))
     (* (jbigdec-unscaled b) (expt 10 (jbigdec-scale a)))))

;; render the decimal text (no M): insert the point `scale` digits from the right.
(define (jbigdec->string bd)
  (let* ((u (jbigdec-unscaled bd)) (sc (jbigdec-scale bd))
         (neg (< u 0)) (digs (number->string (abs u))))
    (string-append
      (if neg "-" "")
      (if (<= sc 0)
          digs
          (let* ((padded (if (<= (string-length digs) sc)
                             (string-append (make-string (- (+ sc 1) (string-length digs)) #\0) digs)
                             digs))
                 (pl (string-length padded)))
            (string-append (substring padded 0 (- pl sc)) "." (substring padded (- pl sc) pl)))))))

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
(set! jolt-num-slow?
  (let ((prev jolt-num-slow?)) (lambda (x) (or (jbigdec? x) (prev x)))))
(define (jbd-extend-hook prev bd-op)
  (lambda (a b)
    (if (or (jbigdec? a) (jbigdec? b)) (bd-op a b) (prev a b))))
(set! jolt-add-slow (jbd-extend-hook jolt-add-slow (lambda (a b) (jbd-binop + jbd2+ a b))))
(set! jolt-sub-slow (jbd-extend-hook jolt-sub-slow (lambda (a b) (jbd-binop - jbd2- a b))))
(set! jolt-mul-slow (jbd-extend-hook jolt-mul-slow (lambda (a b) (jbd-binop * jbd2* a b))))
(set! jolt-div-slow (jbd-extend-hook jolt-div-slow (lambda (a b) (jbd-binop / jbd2-div a b))))
(set! jolt-num-cmp-slow
  (let ((prev jolt-num-cmp-slow))
    (lambda (a b)
      (if (and (or (jbigdec? a) (jbigdec? b)) (jbd-numberish? a) (jbd-numberish? b))
          (jbd-value-compare a b)
          (prev a b)))))
;; quot/rem/mod: a double operand demotes to the double path; exact operands use
;; the integer-division bigdec ops (mod = rem, floor-adjusted to the divisor's sign).
(define (jbd->num x) (if (jbigdec? x) (jbigdec->flonum x) x))
(set! jolt-quot-slow
  (jbd-extend-hook jolt-quot-slow
    (lambda (a b) (if (or (flonum? a) (flonum? b))
                      (jolt-quot (jbd->num a) (jbd->num b))
                      (jbd-int-quot (jbd-coerce a) (jbd-coerce b))))))
(set! jolt-rem-slow
  (jbd-extend-hook jolt-rem-slow
    (lambda (a b) (if (or (flonum? a) (flonum? b))
                      (jolt-rem (jbd->num a) (jbd->num b))
                      (jbd-int-rem (jbd-coerce a) (jbd-coerce b))))))
(set! jolt-mod-slow
  (jbd-extend-hook jolt-mod-slow
    (lambda (a b)
      (if (or (flonum? a) (flonum? b))
          (jolt-mod (jbd->num a) (jbd->num b))
          (let* ((bb (jbd-coerce b))
                 (m (jbd-int-rem (jbd-coerce a) bb)))
            (if (or (jbd-zero? m) (eq? (jbd-neg? m) (jbd-neg? bb))) m (jbd2+ m bb)))))))
;; unary shims: inc/dec and the sign predicates take a bigdec arm. set! updates
;; call-position references; the re-def-var! updates the var cell AND claims the
;; wrapped proc's class name before the prelude's inc'/dec' aliases are defined
;; ((type inc) stays clojure.core$inc — first def wins in the class registry).
(define jbd-one (make-jbigdec 1 0))
(set! jolt-inc (let ((prev jolt-inc)) (lambda (x) (if (jbigdec? x) (jbd-mc-round (jbd2+ x jbd-one)) (prev x)))))
(set! jolt-dec (let ((prev jolt-dec)) (lambda (x) (if (jbigdec? x) (jbd-mc-round (jbd2- x jbd-one)) (prev x)))))
(set! jolt-zero? (let ((prev jolt-zero?)) (lambda (x) (if (jbigdec? x) (jbd-zero? x) (prev x)))))
(set! jolt-pos? (let ((prev jolt-pos?)) (lambda (x) (if (jbigdec? x) (jbd-pos? x) (prev x)))))
(set! jolt-neg? (let ((prev jolt-neg?)) (lambda (x) (if (jbigdec? x) (jbd-neg? x) (prev x)))))
;; a BigDecimal IS a number (java.lang.Number): extend the number? native so the
;; predicate — and everything defined over it (num, =='s guard) — accepts it.
;; The compiled fast paths test Chez number? directly and are unaffected.
(set! jolt-number? (let ((prev jolt-number?)) (lambda (x) (if (jbigdec? x) #t (prev x)))))
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
(set! jolt-double-slow
  (let ((prev jolt-double-slow))
    (lambda (x) (if (jbigdec? x) (jbigdec->flonum x) (prev x)))))

;; narrow casts truncate a bigdec like Number.longValue.
(set! jolt-cast-truncate-slow
  (let ((prev jolt-cast-truncate-slow))
    (lambda (x)
      (if (jbigdec? x)
          (truncate (/ (jbigdec-unscaled x) (expt 10 (jbigdec-scale x))))
          (prev x)))))

;; compare: add a bigdec arm (enables compare / sort / sorted collections). A
;; bigdec vs a plain number compares by value; bigdec vs bigdec is scale-independent.
(define jbd-prev-compare jolt-compare)
(define (jbd-numberish? x) (or (jbigdec? x) (number? x)))
(set! jolt-compare
  (lambda (a b)
    (if (and (or (jbigdec? a) (jbigdec? b)) (jbd-numberish? a) (jbd-numberish? b))
        (if (or (flonum? a) (flonum? b))
            (let ((fa (if (jbigdec? a) (jbigdec->flonum a) a))
                  (fb (if (jbigdec? b) (jbigdec->flonum b) b)))
              (cond ((< fa fb) -1) ((> fa fb) 1) (else 0)))
            (jbd-compare2 (jbd-coerce a) (jbd-coerce b)))
        (jbd-prev-compare a b))))
(def-var! "clojure.core" "compare" jolt-compare)

;; equality: a bigdec equals only another bigdec, by value (matching (= 3M 3) = false).
(register-eq-arm! (lambda (a b) (or (jbigdec? a) (jbigdec? b)))
                  (lambda (a b) (and (jbigdec? a) (jbigdec? b) (jbigdec=? a b))))

;; str drops the M; pr/pr-str keep it.
(register-str-render! jbigdec? jbigdec->string)
(register-pr-arm! jbigdec? (lambda (x) (string-append (jbigdec->string x) "M")))

;; class / decimal?
(register-class-arm! jbigdec? (lambda (x) "java.math.BigDecimal"))
(set! jolt-decimal? (lambda (x) (jbigdec? x)))
(def-var! "clojure.core" "decimal?" jolt-decimal?)
