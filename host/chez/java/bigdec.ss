;; BigDecimal. A jbigdec is {unscaled, scale} over Chez arbitrary-precision exact
;; integers; its value is unscaled * 10^-scale (1.5M = {15,1}, 1.00M = {100,2},
;; 3M = {3,0}). M-suffix literals read to a :bigdec form that the back end lowers
;; to jolt-bigdec-from-string; bigdec coerces a number/string. Equality is by
;; value (1.0M = 1.00M), str drops the M, pr keeps it, class is
;; java.math.BigDecimal.
;;
;; Arithmetic follows java.math.BigDecimal's scale rules: add/sub align to the
;; larger scale; multiply adds scales; divide gives the exact quotient at minimal
;; scale or throws ArithmeticException on a non-terminating expansion. Clojure
;; contagion: a bigdec mixed with an integer stays a bigdec; a flonum operand wins
;; (the result is a double). jbd-add/-sub/-mul/-div, jbd-min/-max, the jbd-lt?/…
;; /zero? helpers, and jbd-quot/-rem are the shared engine. Two paths reach it, both
;; leaving the inlined native hot path untouched:
;;   - value position ((reduce + bigs)/(apply * bigs)): the jolt-add/-sub/-mul/-div
;;     and compare shims dispatch here when a bigdec operand is present.
;;   - call position ((+ 1.5M 2.5M), (< a b), (zero? b)): jolt.passes.numeric tags
;;     the invoke :num-kind :bigdec when every operand is statically a bigdec (M
;;     literal or a let-bound copy, integer literals allowed), and the back end
;;     lowers it to the jbd op. Non-bigdec code is unaffected.
;; Gaps (a runtime bigdec the analyzer can't see statically): a bigdec mixed with a
;; flonum in call position ((+ 1.5M 2.0)) and arithmetic over a bigdec the analyzer
;; types as :any ((+ (bigdec x) 1)) fall through to the raw op and throw; use value
;; position or a literal-typed let.

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
    (else (error #f "bigdec: cannot coerce" x))))

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

;; coerce an exact integer to a scale-0 bigdec; pass a bigdec through. Used on the
;; non-flonum mixed path (bigdec + long -> bigdec).
(define (jbd-coerce x)
  (cond ((jbigdec? x) x)
        ((and (number? x) (exact? x) (integer? x)) (make-jbigdec x 0))
        (else (error #f "bigdec arithmetic: cannot coerce operand" x))))

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

(define (jbd2-div a b)
  (when (= 0 (jbigdec-unscaled b))
    (jolt-throw (jolt-host-throwable "java.lang.ArithmeticException" "Divide by zero")))
  ;; a/b = (ua * 10^sb) / (ub * 10^sa) as an exact rational.
  (jbd-rational->bigdec (/ (* (jbigdec-unscaled a) (expt 10 (jbigdec-scale b)))
                           (* (jbigdec-unscaled b) (expt 10 (jbigdec-scale a))))))

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

;; A binary op over operands that may mix bigdec / integer / flonum. flonum-op is
;; the native fallback for the double-contagion path; bd-op is the exact bigdec op.
(define (jbd-binop flonum-op bd-op a b)
  (if (or (flonum? a) (flonum? b))
      (flonum-op (if (jbigdec? a) (jbigdec->flonum a) a)
                 (if (jbigdec? b) (jbigdec->flonum b) b))
      (bd-op (jbd-coerce a) (jbd-coerce b))))

;; --- variadic engine ops (Phase-2 emit targets + value-position folds) -------
(define (jbd-fold flonum-op bd-op init xs)
  (let loop ((acc init) (rest xs))
    (if (null? rest) acc (loop (jbd-binop flonum-op bd-op acc (car rest)) (cdr rest)))))

(define (jbd-add . xs)
  (cond ((null? xs) (make-jbigdec 0 0))
        ((null? (cdr xs)) (car xs))
        (else (jbd-fold + jbd2+ (car xs) (cdr xs)))))
(define (jbd-sub . xs)
  (cond ((null? xs) (error #f "- needs at least 1 arg"))
        ((null? (cdr xs)) (if (jbigdec? (car xs)) (jbd-negate (car xs)) (- (car xs))))
        (else (jbd-fold - jbd2- (car xs) (cdr xs)))))
(define (jbd-mul . xs)
  (cond ((null? xs) (make-jbigdec 1 0))
        ((null? (cdr xs)) (car xs))
        (else (jbd-fold * jbd2* (car xs) (cdr xs)))))
(define (jbd-div . xs)
  (cond ((null? xs) (error #f "/ needs at least 1 arg"))
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

;; Value-position arithmetic: (reduce + bigs) / (apply * bigs) pass +/*/- // AS A
;; VALUE, which lowers to these shims (NOT the inlined hot-path native op). Extend
;; them to dispatch to the bigdec engine when a bigdec operand is present; ordinary
;; numeric folds hit the captured native path unchanged.
(define jbd-prev-add jolt-add)
(define jbd-prev-sub jolt-sub)
(define jbd-prev-mul jolt-mul)
(define jbd-prev-div jolt-div)
(define jbd-prev-min jolt-min)
(define jbd-prev-max jolt-max)
(define (jbd-any? xs) (and (pair? xs) (or (jbigdec? (car xs)) (jbd-any? (cdr xs)))))
(set! jolt-add (lambda xs (if (jbd-any? xs) (apply jbd-add xs) (apply jbd-prev-add xs))))
(set! jolt-sub (lambda xs (if (jbd-any? xs) (apply jbd-sub xs) (apply jbd-prev-sub xs))))
(set! jolt-mul (lambda xs (if (jbd-any? xs) (apply jbd-mul xs) (apply jbd-prev-mul xs))))
(set! jolt-div (lambda xs (if (jbd-any? xs) (apply jbd-div xs) (apply jbd-prev-div xs))))
(set! jolt-min (lambda xs (if (jbd-any? xs) (apply jbd-min xs) (apply jbd-prev-min xs))))
(set! jolt-max (lambda xs (if (jbd-any? xs) (apply jbd-max xs) (apply jbd-prev-max xs))))

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
