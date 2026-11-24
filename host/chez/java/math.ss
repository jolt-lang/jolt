;; clojure.math and java.lang.Math — one implementation for both, over the
;; host's numeric procedures. Portable: R7RS arithmetic plus jolt's own runtime
;; names, so the Chez and Gambit hosts include the same file.
;;
;; clojure.math is registered as native bindings, NOT a .clj file — so there's no
;; source tier to emit. The def-var! shims here back each clojure.math fn. The
;; analyzer knows the clojure.math ns exists, so a ref like clojure.math/sqrt
;; lowers to a var-deref; these cells back it at runtime.
;;
;; Every clojure.math result is a double (inputs arrive as flonums; the host's
;; sqrt/sin/expt/... return flonums for flonum args). Semantics match
;; Clojure 1.11 clojure.math: round = floor(x+0.5), rint = round-half-even,
;; floor/ceil/floor-div return doubles, to-degrees/to-radians via PI.

;; clojure.math's PI/E are java.lang.Math's, which the JDK defines as compile-time
;; double literals — not the host libm's atan(1)/exp(1). Pinned as literals so a
;; host whose libm rounds exp(1) one ulp high (bionic's) still answers the JVM's
;; value. java.lang.Math's PI and E below are these.
(define jolt-math-pi 3.141592653589793)
(define jolt-math-e 2.718281828459045)

;; Math.cbrt is fdlibm's cbrt (the JDK's FdLibm.Cbrt), ported over the high
;; word of the double so every host answers the JVM's digits. The host libm's
;; pow does not: bionic's pow(2.5, 1/3) lands one ulp from glibc's and the
;; JVM's, and with 1/3 already rounded a tiny input drifts further. fdlibm is
;; itself not always correctly rounded (cbrt 2.5 is one ulp low), and that is
;; the answer the JVM gives.
(define (cbrt-with-hi x h)                          ; the subnormal path keeps the low word
  (sa-bits->flonum (+ (* h 4294967296) (bitwise-and (sa-flonum->bits x) #xffffffff))))
(define (jolt-math-cbrt x)
  (let* ((x (exact->inexact x))
         (hx0 (sa-flonum-hi32 x))
         (sign (bitwise-and hx0 #x80000000))
         (hx (bitwise-xor hx0 sign)))
    (cond
      ((>= hx #x7ff00000) (fl+ x x))                       ; NaN, Inf
      ((fl= x 0.0) x)                                     ; +-0
      (else
       (let* ((x (flabs x))
              (t (if (< hx #x00100000)                      ; subnormal
                     (let ((t (fl* 18014398509481984.0 x))) ; 2^54
                       (cbrt-with-hi t (+ (quotient (sa-flonum-hi32 t) 3) 696219795)))
                     (sa-hi32->flonum (+ (quotient hx 3) 715094163))))
              (r (fl/ (fl* t t) x))
              (s (fl+ 5.42857142857142815906e-01 (fl* r t)))
              (t (fl* t (fl+ 3.57142857142857150787e-01
                             (fl/ 1.60714285714285720630e+00
                                  (fl+ s 1.41428571428571436819e+00
                                       (fl/ -7.05306122448979611050e-01 s))))))
              ;; chop t to 20 bits and make it larger than cbrt(x)
              (t (sa-hi32->flonum (+ (sa-flonum-hi32 t) 1)))
              (s (fl* t t))
              (r (fl/ x s))
              (w (fl+ t t))
              (r (fl/ (fl- r t) (fl+ w r)))
              (t (fl+ t (fl* t r))))
         (if (= sign 0) t (fl- t)))))))

;; java.lang.Math.round(double) -> long: NaN->0, +Inf->Long/MAX_VALUE, -Inf->
;; Long/MIN_VALUE, out-of-long-range saturates, and the greatest double below 0.5
;; (0.49999999999999994) rounds to 0 — its x+0.5 sums to exactly 1.0. Else (long)
;; floor(x + 0.5). clojure.math/round and Math/round both route here.
(define jolt-math-long-max 9223372036854775807)
(define jolt-math-long-min -9223372036854775808)
(define (jolt-math-round x)
  (let ((d (if (and (number? x) (real? x)) (exact->inexact x) x)))
    (cond
      ((nan? d) 0)
      ((infinite? d) (if (fl> d 0.0) jolt-math-long-max jolt-math-long-min))
      ;; 0.49999999999999994: largest double < 0.5, whose +0.5 rounds up to 1.0
      ((and (fl< d 0.5) (fl>= (fl+ d 0.5) 1.0)) 0)
      (else
       (let ((r (floor (+ d 0.5))))
         (cond ((> r jolt-math-long-max) jolt-math-long-max)
               ((< r jolt-math-long-min) jolt-math-long-min)
               (else (exact r))))))))
(define (jolt-math-to-degrees r) (/ (* r 180.0) jolt-math-pi))
(define (jolt-math-to-radians d) (/ (* d jolt-math-pi) 180.0))
;; java.lang.Math.hypot — scale by a power of 2 (exact) so a^2+b^2 can't overflow
;; before the sqrt; the only rounding is the correctly-rounded sqrt. NaN->NaN,
;; either Inf -> +Inf. (Naive sqrt(a^2+b^2) returns Inf for 3e200,4e200.)
(define (jolt-math-hypot a b)
  ;; Java Math.hypot: Inf if either is Inf (even if the other is NaN), else NaN if
  ;; either is NaN. Otherwise la * sqrt(1 + (sm/la)^2), factoring out the larger
  ;; magnitude so the squares never overflow (sm/la <= 1). This is exact for the
  ;; scaled Pythagorean cases (e.g. 3e200,4e200 -> 5e200) and within 1 ULP
  ;; elsewhere, matching Java's correctly-rounded result to ~15-16 digits.
  (cond
    ((or (infinite? a) (infinite? b)) +inf.0)
    ((or (nan? a) (nan? b)) +nan.0)
    (else
     (let* ((ax (abs a)) (bx (abs b))
            (la (max ax bx)) (sm (min ax bx)))
       (if (= la 0.0)
           0.0
           (let ((r (/ sm la)))
             (* la (sqrt (+ 1.0 (* r r))))))))))
;; java.lang.Math.expm1 — Taylor series for |x|<0.5 (where exp(x)-1 cancels badly),
;; else exp(x)-1. +Inf->+Inf, -Inf->-1.0, NaN->NaN.
(define (jolt-math-expm1 x)
  (let ((ax (abs x)))
    (cond
      ((nan? x) x)
      ((infinite? x) (if (> x 0.0) x -1.0))
      ((= x 0.0) x)                     ; expm1(0) = 0 (also ±0.0 passthrough)
      ((< ax 0.5)
       (let loop ((term x) (n 2) (acc x))
         (let* ((nt (* term (/ x n))) (acc2 (+ acc nt)))
           ;; terminate on a term that adds nothing, or is negligible relative to
           ;; the accumulator. Guard the threshold with (max 1.0 …) so an acc that
           ;; rounds toward 0 can't make the bound 0 and spin forever (expm1 0).
           (if (or (= nt 0.0) (< (abs nt) (* 1e-18 (max 1.0 (abs acc2))))) acc2
               (loop nt (+ n 1) acc2)))))
      (else (- (exp x) 1.0)))))
;; java.lang.Math.log1p — alternating series for |x|<0.3 (where 1+x rounds to 1),
;; else log(1+x). log1p(-1)->-Inf, log1p(<-1)->NaN.
(define (jolt-math-log1p x)
  (cond
    ((nan? x) x)
    ((= x -1.0) -inf.0)
    ((< x -1.0) +nan.0)
    ((< (abs x) 0.3)
     (let loop ((n 1) (xp x) (acc 0.0) (sign 1))
       (let* ((term (* sign (/ xp n))) (acc2 (+ acc term)))
         (if (< (abs term) (* 1e-18 (max 1.0 (abs acc2)))) acc2
             (loop (+ n 1) (* xp x) acc2 (- sign))))))
    (else (real-or-nan (log (+ 1.0 x))))))
;; floor-div/floor-mod take ^long args and return a long. Coerce each operand
;; toward zero (Java's ^long cast) so a double like 7.0 becomes 7, then compute
;; on exact integers so the result is a long, not a double.
(define (jolt-math-floor-div a b)
  (let ((a (exact (truncate a))) (b (exact (truncate b))))
    (if (eqv? b 0) (jolt-l-div0-throw) (floor (/ a b)))))
(define (jolt-math-floor-mod a b)
  (let ((a (exact (truncate a))) (b (exact (truncate b))))
    (if (eqv? b 0) (jolt-l-div0-throw) (- a (* b (floor (/ a b)))))))

;; --- IEEE 754 bit-level ops ---------------------------------------------------
;; The JDK defines copySign/nextUp/nextDown/nextAfter/ulp/getExponent on the raw
;; bit pattern, so -0.0 keeps its sign and a step is exactly one representable
;; double. The pattern comes from the host (sa-flonum->bits): an unsigned 64-bit
;; integer, sign bit on top, 11 exponent bits, 52 mantissa bits. For a negative
;; double a larger pattern is a larger magnitude. A NaN reads as the JVM's
;; canonical one, as doubleToLongBits answers it; dbl->raw-bits keeps its sign
;; and payload, as doubleToRawLongBits does (a flonum carries both through
;; every move and the host's bit cast).
(define dbl-sign-bit (expt 2 63))
(define dbl-mant-unit (expt 2 52))
(define dbl-min-value 4.9406564584124654e-324)
(define (dbl-negative? x) (or (< x 0.0) (eqv? x -0.0)))
;; e with 2^e <= m < 2^(e+1), for an exact positive rational m
(define (dbl-binary-exponent m)
  (let loop ((e (exact (floor (log (inexact m) 2)))))
    (cond ((> (expt 2 e) m) (loop (- e 1)))
          ((<= (expt 2 (+ e 1)) m) (loop (+ e 1)))
          (else e))))
(define (dbl->bits x)
  (let ((x (exact->inexact x)))
    (if (nan? x)
        (+ (* 2047 dbl-mant-unit) (expt 2 51))
        (sa-flonum->bits x))))
(define (dbl->raw-bits x) (sa-flonum->bits (exact->inexact x)))
(define (bits->dbl b) (sa-bits->flonum b))

(define (jolt-math-copy-sign m s)
  (let* ((m (exact->inexact (jolt-need-num m)))
         (a (if (dbl-negative? m) (- m) m)))
    (if (dbl-negative? (exact->inexact (jolt-need-num s))) (- a) a)))

;; Math.signum: NaN and both zeros come back as themselves
(define (jolt-math-signum x)
  (let ((x (exact->inexact (jolt-need-num x))))
    (cond ((nan? x) x) ((> x 0.0) 1.0) ((< x 0.0) -1.0) (else x))))

;; one representable double up / down; d+0.0 folds -0.0 into 0.0 first, as the
;; JDK does, so both zeros step to the same neighbour
(define (jolt-math-next-up x)
  (let ((d (+ (exact->inexact (jolt-need-num x)) 0.0)))
    (cond ((or (nan? d) (= d +inf.0)) d)
          ((= d 0.0) dbl-min-value)
          (else (bits->dbl (if (> d 0.0) (+ (dbl->bits d) 1) (- (dbl->bits d) 1)))))))
(define (jolt-math-next-down x)
  (let ((d (+ (exact->inexact (jolt-need-num x)) 0.0)))
    (cond ((or (nan? d) (= d -inf.0)) d)
          ((= d 0.0) (- dbl-min-value))
          (else (bits->dbl (if (> d 0.0) (- (dbl->bits d) 1) (+ (dbl->bits d) 1)))))))
(define (jolt-math-next-after start direction)
  (let ((s (exact->inexact (jolt-need-num start)))
        (d (exact->inexact (jolt-need-num direction))))
    (cond ((> s d) (jolt-math-next-down s))
          ((< s d) (jolt-math-next-up s))
          ((= s d) d)
          (else (+ s d)))))                        ; a NaN

;; Math.getExponent: the unbiased exponent field; 1024 for Inf/NaN, -1023 for
;; zero and subnormals
(define (jolt-math-get-exponent x)
  (- (quotient (modulo (dbl->bits (jolt-need-num x)) dbl-sign-bit) dbl-mant-unit) 1023))

;; Math.ulp: the gap to the next double away from zero
(define (jolt-math-ulp x)
  (let* ((d (exact->inexact (jolt-need-num x))) (e (jolt-math-get-exponent d)))
    (cond ((= e 1024) (abs d))                     ; Inf -> Inf, NaN -> NaN
          ((= e -1023) dbl-min-value)              ; zero, subnormal
          (else (exact->inexact (expt 2 (- e 52)))))))

;; Math.scalb: d * 2^n rounded once, in exact arithmetic so a result that lands
;; among the subnormals rounds as the JDK's does. n is clamped first: past
;; +/-2200 every finite non-zero d has already overflowed or underflowed.
(define (jolt-math-scalb x n)
  (let ((d (exact->inexact (jolt-need-num x)))
        (n (max -2200 (min 2200 (exact (truncate (jolt-need-num n)))))))
    (if (or (nan? d) (infinite? d) (= d 0.0))
        d
        (let ((r (exact->inexact (* (exact d) (expt 2 n)))))
          (if (= r 0.0) (jolt-math-copy-sign 0.0 d) r)))))

;; Math.IEEEremainder: x - y*n with n = x/y rounded half-even, exactly. A zero
;; result takes x's sign.
(define (jolt-math-ieee-remainder x y)
  (let ((x (exact->inexact (jolt-need-num x))) (y (exact->inexact (jolt-need-num y))))
    (cond ((or (nan? x) (nan? y) (infinite? x) (= y 0.0)) +nan.0)
          ((infinite? y) x)
          (else
           (let* ((ex (exact x)) (ey (exact y))
                  (r (exact->inexact (- ex (* ey (round (/ ex ey)))))))
             (if (= r 0.0) (jolt-math-copy-sign 0.0 x) r))))))

;; Math.max / Math.min: a double argument makes it the double overload, where NaN
;; wins and -0.0 is below 0.0; two integers compare as longs. Two distinct
;; ordered doubles take one compare; the zero and NaN rules only run on a tie or
;; an unordered pair.
(define (jolt-math-max a b)
  (if (and (flonum? a) (flonum? b))
      (cond ((fl> a b) a)
            ((fl< a b) b)
            ((fl= a b) (if (dbl-negative? a) b a))        ; equal: only ±0.0 differ
            ((nan? a) a)
            (else b))
      (if (or (flonum? a) (flonum? b))
          (jolt-math-max (exact->inexact a) (exact->inexact b))
          (if (> a b) a b))))
(define (jolt-math-min a b)
  (if (and (flonum? a) (flonum? b))
      (cond ((fl< a b) a)
            ((fl> a b) b)
            ((fl= a b) (if (dbl-negative? a) a b))
            ((nan? a) a)
            (else b))
      (if (or (flonum? a) (flonum? b))
          (jolt-math-min (exact->inexact a) (exact->inexact b))
          (if (< a b) a b))))

;; Math.addExact and kin over longs: the exact result, or ArithmeticException
;; "long overflow" when it leaves the long range
(define (jolt-math-long-exact r)
  (if (and (>= r -9223372036854775808) (<= r 9223372036854775807))
      r
      (throw-jvm 'ArithmeticException "long overflow")))
(define (exact-long x) (exact (truncate (jolt-need-num x))))
(define (jolt-math-add-exact a b) (jolt-math-long-exact (+ (exact-long a) (exact-long b))))
(define (jolt-math-subtract-exact a b) (jolt-math-long-exact (- (exact-long a) (exact-long b))))
(define (jolt-math-multiply-exact a b) (jolt-math-long-exact (* (exact-long a) (exact-long b))))
(define (jolt-math-increment-exact a) (jolt-math-long-exact (+ (exact-long a) 1)))
(define (jolt-math-decrement-exact a) (jolt-math-long-exact (- (exact-long a) 1)))
(define (jolt-math-negate-exact a) (jolt-math-long-exact (- (exact-long a))))

;; clojure.math fns take ^double parameters, so an argument is a DOUBLE before the
;; op sees it. Coercing only the result was wrong at the poles: Chez raises on
;; exact (atan 0 0), (log 0) and (expt 0 -1) where the doubles answer 0.0, ##-Inf
;; and ##Inf. The result coerce stays for the fns that keep a flonum exact-free.
(define (->dbl x) (exact->inexact (jolt-need-num x)))
(define (m1 f) (lambda (x) (exact->inexact (f (->dbl x)))))
(define (m2 f) (lambda (a b) (exact->inexact (f (->dbl a) (->dbl b)))))
;; a real result stays a flonum; a complex result becomes +nan.0. Chez extends
;; several real-domain ops (sqrt/expt/log/asin/acos, and log's kin log10/log1p)
;; onto the complex plane for out-of-domain real inputs, but Java/clojure.math
;; returns NaN there. real? is #t for a flonum and #f for a Chez complex, so this
;; guards exactly the complex leak; NaN/Inf are real and pass through unchanged.
(define (real-or-nan x) (if (and (number? x) (real? x)) (exact->inexact x) +nan.0))
(define (m1c f) (lambda (x) (real-or-nan (f (->dbl x)))))
(define (m2c f) (lambda (a b) (real-or-nan (f (->dbl a) (->dbl b)))))
;; Math.pow, and clojure.math/pow which is it: Java's special cases, not C's. A
;; NaN exponent is NaN even over a base of 1.0, and a base of magnitude 1.0 to an
;; infinite power is NaN, where expt (C's pow) answers 1.0 for both. The proven-
;; double lowering, jolt-fl-pow in seq.ss, makes the same two checks.
(define (jolt-math-pow a b)
  (if (or (nan? b) (and (infinite? b) (= (abs a) 1.0)))
      +nan.0
      (real-or-nan (expt a b))))
;; the clojure.math fns implemented above take their numbers as given; these
;; take any jolt number first, as the m1/m2 wrappers do (a Float is the double
;; it widens to, which is what clojure.math's ^double parameters receive)
(define (n1 f) (lambda (x) (f (jolt-need-num x))))
(define (n2 f) (lambda (a b) (f (jolt-need-num a) (jolt-need-num b))))
(def-var! "clojure.math" "sqrt" (m1c sqrt))
(def-var! "clojure.math" "cbrt" (n1 jolt-math-cbrt))
(def-var! "clojure.math" "pow" (lambda (a b) (jolt-math-pow (->dbl a) (->dbl b))))
(def-var! "clojure.math" "exp" (m1 exp))
(def-var! "clojure.math" "expm1" (n1 jolt-math-expm1))
(def-var! "clojure.math" "log" (m1c log))
;; base-10 log via Chez's base-arg log — also backs java.lang.Math/log10 so the
;; two never disagree.
(define (jolt-math-log10 x) (real-or-nan (log x 10.0)))
(def-var! "clojure.math" "log10" (n1 jolt-math-log10))
(def-var! "clojure.math" "log1p" (n1 jolt-math-log1p))
(def-var! "clojure.math" "sin" (m1 sin))
(def-var! "clojure.math" "cos" (m1 cos))
(def-var! "clojure.math" "tan" (m1 tan))
(def-var! "clojure.math" "asin" (m1c asin))
(def-var! "clojure.math" "acos" (m1c acos))
(def-var! "clojure.math" "atan" (m1 atan))
;; clojure.math/atan2 is atan2(y, x); Chez's 2-arg atan is (atan y x).
(def-var! "clojure.math" "atan2" (m2 atan))
(def-var! "clojure.math" "sinh" (m1 sinh))
(def-var! "clojure.math" "cosh" (m1 cosh))
(def-var! "clojure.math" "tanh" (m1 tanh))
(def-var! "clojure.math" "floor" (m1 floor))
(def-var! "clojure.math" "ceil" (m1 ceiling))
(def-var! "clojure.math" "rint" (m1 round))
(def-var! "clojure.math" "round" (n1 jolt-math-round))
(def-var! "clojure.math" "signum" jolt-math-signum)
(def-var! "clojure.math" "to-degrees" (n1 jolt-math-to-degrees))
(def-var! "clojure.math" "to-radians" (n1 jolt-math-to-radians))
(def-var! "clojure.math" "hypot" (n2 jolt-math-hypot))
(def-var! "clojure.math" "floor-div" (n2 jolt-math-floor-div))
(def-var! "clojure.math" "floor-mod" (n2 jolt-math-floor-mod))
(def-var! "clojure.math" "E" jolt-math-e)
(def-var! "clojure.math" "PI" jolt-math-pi)
(def-var! "clojure.math" "IEEE-remainder" jolt-math-ieee-remainder)
(def-var! "clojure.math" "copy-sign" jolt-math-copy-sign)
(def-var! "clojure.math" "get-exponent" jolt-math-get-exponent)
(def-var! "clojure.math" "next-after" jolt-math-next-after)
(def-var! "clojure.math" "next-up" jolt-math-next-up)
(def-var! "clojure.math" "next-down" jolt-math-next-down)
(def-var! "clojure.math" "ulp" jolt-math-ulp)
(def-var! "clojure.math" "scalb" jolt-math-scalb)
(def-var! "clojure.math" "random" (lambda () (jolt-random 1.0)))
(def-var! "clojure.math" "add-exact" jolt-math-add-exact)
(def-var! "clojure.math" "subtract-exact" jolt-math-subtract-exact)
(def-var! "clojure.math" "multiply-exact" jolt-math-multiply-exact)
(def-var! "clojure.math" "increment-exact" jolt-math-increment-exact)
(def-var! "clojure.math" "decrement-exact" jolt-math-decrement-exact)
(def-var! "clojure.math" "negate-exact" jolt-math-negate-exact)

;; ---- java.lang.Math -------------------------------------------------------
;; java.lang.Math: sqrt/pow/floor/ceil/trig/log/exp always return a DOUBLE on the
;; JVM (Chez's sqrt/expt return EXACT for exact args, e.g. (sqrt 9) -> 3), so coerce
;; to flonum. round -> long (exact); abs/max/min preserve the argument's type.
;; Every Math method takes numbers (PI/E are values, not methods), and each one
;; hands its argument to a host numeric primitive. Check at the boundary: the
;; condition Chez raises for a wrong-typed operand carries no class, so it would
;; escape as #object[:object] with no catch clause able to select it. The hot path
;; does not come through here — a Math call over proven flonums lowers to a native
;; Chez flonum op (jolt.passes.numeric).
;; Every method but the long ones takes doubles, so its arguments become doubles
;; here, as clojure.math's do above (exact (Math/atan2 0 0) raised in Chez).
(define math-long-methods
  '("floorDiv" "floorMod" "addExact" "subtractExact" "multiplyExact"
    "incrementExact" "decrementExact" "negateExact"))
(define (math-checked entry)
  (let ((f (cdr entry)))
    (if (procedure? f)
        (let ((coerce (if (member (car entry) math-long-methods) jolt-need-num ->dbl)))
          (cons (car entry)
                (host-arity-like f (lambda args (apply f (map coerce args))))))
        entry)))
;; --- java.lang.Math's float overloads ------------------------------------------
;; A Float argument picks Math's float overload where there is one, so the answer
;; is a Float computed at single precision: (Math/ulp (float 2.5)) is the gap to
;; the next FLOAT, 2.3841858E-7, not the next double. round(float) is an int.
;; Anything else (a double among the arguments, or a method with no float
;; overload) keeps the double path in java/math.ss.
(define flt-min-value 1.401298464324817e-45)
(define (flt-of-bits b) (jfloat-fl (jolt-int-bits->float b)))
(define (flt-bits x) (flt->bits x))
;; the unbiased exponent field of a float's pattern: 128 for Inf/NaN, -127 for
;; zero and subnormals
(define (flt-exponent d)
  (- (bitwise-and (bitwise-arithmetic-shift-right (flt-bits d) 23) #xff) 127))
(define (flt-next-up d)
  (let ((d (+ d 0.0)))
    (cond ((or (nan? d) (= d +inf.0)) d)
          ((= d 0.0) flt-min-value)
          ((> d 0.0) (flt-of-bits (+ (flt-bits d) 1)))
          (else (flt-of-bits (- (flt-bits d) 1))))))
(define (flt-next-down d)
  (let ((d (+ d 0.0)))
    (cond ((or (nan? d) (= d -inf.0)) d)
          ((= d 0.0) (- flt-min-value))
          ((> d 0.0) (flt-of-bits (- (flt-bits d) 1)))
          (else (flt-of-bits (+ (flt-bits d) 1))))))
(define (flt-ulp d)
  (let ((e (flt-exponent d)))
    (cond ((= e 128) (abs d))
          ((= e -127) flt-min-value)
          (else (exact->inexact (expt 2 (- e 23)))))))
(define (flt-result d) (make-jfloat (jolt-unchecked-float->flonum d)))
(define (math-float-overload name double-impl float-impl)
  (cons name
        (lambda (x)
          (if (jfloat? x) (float-impl (jfloat-fl x)) (double-impl (jolt-need-num x))))))
(define (math-float-overload2 name double-impl float-impl)
  (cons name
        (lambda (x y)
          (if (jfloat? x) (float-impl (jfloat-fl x) y) (double-impl (jolt-need-num x) (jolt-need-num y))))))
(define (both-floats name double-impl float-impl)
  (cons name
        (lambda (a b)
          (if (and (jfloat? a) (jfloat? b))
              (make-jfloat (float-impl (jfloat-fl a) (jfloat-fl b)))
              (double-impl (jolt-need-num a) (jolt-need-num b))))))

(define math-float-overloads
  (list
    (math-float-overload "abs" abs (lambda (d) (make-jfloat (abs d))))
    (math-float-overload "signum" jolt-math-signum (lambda (d) (make-jfloat (jolt-math-signum d))))
    (math-float-overload "ulp" jolt-math-ulp (lambda (d) (make-jfloat (flt-ulp d))))
    (math-float-overload "nextUp" jolt-math-next-up (lambda (d) (make-jfloat (flt-next-up d))))
    (math-float-overload "nextDown" jolt-math-next-down (lambda (d) (make-jfloat (flt-next-down d))))
    (math-float-overload "getExponent" jolt-math-get-exponent flt-exponent)
    ;; Math.round(float): an int, so past the int range it saturates there
    (math-float-overload "round" jolt-math-round
      (lambda (d) (max -2147483648 (min 2147483647 (jolt-math-round d)))))
    ;; nextAfter(float, double) and scalb(float, int): the second argument is
    ;; whatever it is, the result a float
    (math-float-overload2 "nextAfter" jolt-math-next-after
      (lambda (d dir)
        (let ((dir (jolt-double dir)))
          (make-jfloat
            (cond ((> d dir) (flt-next-down d))
                  ((< d dir) (flt-next-up d))
                  ((= d dir) (jolt-unchecked-float->flonum dir))
                  (else (+ d dir)))))))
    (math-float-overload2 "scalb" jolt-math-scalb
      (lambda (d n) (flt-result (jolt-math-scalb d n))))
    (both-floats "max" jolt-math-max jolt-math-max)
    (both-floats "min" jolt-math-min jolt-math-min)
    (both-floats "copySign" jolt-math-copy-sign jolt-math-copy-sign)))

(register-class-statics! "Math"
  (append math-float-overloads
  (map math-checked
  (list (cons "sqrt" (lambda (x) (real-or-nan (sqrt x))))
        ;; cbrt/log10 (and hypot/expm1/log1p below) share the clojure.math impls
        ;; above so Math/x and clojure.math/x never diverge.
        (cons "cbrt" (lambda (x) (jolt-math-cbrt (->dbl x))))
        (cons "pow" (lambda (a b) (jolt-math-pow a b)))
        ;; hypot/expm1/log1p route to the numerically-stable clojure.math impls
        ;; so Math/hypot doesn't overflow and expm1/log1p keep
        ;; full precision near zero.
        (cons "hypot" (lambda (a b) (jolt-math-hypot (->dbl a) (->dbl b))))
        (cons "floor" (lambda (x) (->dbl (floor x))))
        (cons "ceil" (lambda (x) (->dbl (ceiling x))))
        (cons "rint" (lambda (x) (->dbl (round x))))            ; round-half-even -> double
        ;; Math.floorDiv/floorMod: integer floor division / modulus (long -> long).
        (cons "floorDiv" (lambda (a b) (if (zero? b) (jolt-l-div0-throw) (exact (floor (/ a b))))))
        (cons "floorMod" (lambda (a b) (if (zero? b) (jolt-l-div0-throw) (exact (- a (* b (floor (/ a b))))))))
        (cons "sin" (lambda (x) (->dbl (sin x)))) (cons "cos" (lambda (x) (->dbl (cos x))))
        (cons "tan" (lambda (x) (->dbl (tan x)))) (cons "asin" (lambda (x) (real-or-nan (asin x))))
        (cons "acos" (lambda (x) (real-or-nan (acos x)))) (cons "atan" (lambda (x) (->dbl (atan x))))
        ;; Math.atan2(y, x) — Chez's 2-arg atan is (atan y x).
        (cons "atan2" (lambda (y x) (->dbl (atan y x))))
        (cons "sinh" (lambda (x) (->dbl (sinh x)))) (cons "cosh" (lambda (x) (->dbl (cosh x))))
        (cons "tanh" (lambda (x) (->dbl (tanh x))))
        (cons "log" (lambda (x) (real-or-nan (log x)))) (cons "log10" (lambda (x) (jolt-math-log10 (->dbl x))))
        (cons "log1p" (lambda (x) (jolt-math-log1p (->dbl x))))
        (cons "exp" (lambda (x) (->dbl (exp x))))
        (cons "expm1" (lambda (x) (jolt-math-expm1 (->dbl x))))
        (cons "toRadians" (lambda (d) (->dbl (/ (* d jolt-math-pi) 180.0))))
        (cons "toDegrees" (lambda (r) (->dbl (/ (* r 180.0) jolt-math-pi))))
        ;; the IEEE 754 bit-level ops and the exact long arithmetic, shared with
        ;; clojure.math; test.check's double generator uses
        ;; getExponent and scalb
        (cons "IEEEremainder" (lambda (x y) (jolt-math-ieee-remainder x y)))
        (cons "addExact" (lambda (a b) (jolt-math-add-exact a b)))
        (cons "subtractExact" (lambda (a b) (jolt-math-subtract-exact a b)))
        (cons "multiplyExact" (lambda (a b) (jolt-math-multiply-exact a b)))
        (cons "incrementExact" (lambda (a) (jolt-math-increment-exact a)))
        (cons "decrementExact" (lambda (a) (jolt-math-decrement-exact a)))
        (cons "negateExact" (lambda (a) (jolt-math-negate-exact a)))
        (cons "PI" jolt-math-pi) (cons "E" jolt-math-e)
        (cons "random" (lambda args (jolt-random 1.0)))))))

;; ---- Double / Float bit casts -----------------------------------------------
;; Over the bit patterns above, as the JVM's signed long/int. doubleToLongBits
;; folds every NaN into the canonical 0x7ff8000000000000; doubleToRawLongBits
;; keeps a NaN's sign and payload.
(define (unsigned->signed u width)
  (if (>= u (expt 2 (- width 1))) (- u (expt 2 width)) u))
(define (jolt-double->long-bits x)
  (unsigned->signed (dbl->bits (jolt-need-num x)) 64))
(define (jolt-double->raw-long-bits x)
  (unsigned->signed (dbl->raw-bits (jolt-need-num x)) 64))
(define (jolt-long-bits->double n)
  (bits->dbl (modulo (exact (jolt-need-num n)) (expt 2 64))))

;; Float: the argument is cast like (float x) (RT.floatCast: past Float/MAX_VALUE,
;; an infinity included, is IllegalArgumentException), rounded once to single
;; precision (8 exponent bits, 23 mantissa bits, half-even), then encoded.
(define (flt->bits x)
  (let ((x (jolt-double x)))
    (+ (if (dbl-negative? x) (expt 2 31) 0)
       (cond ((nan? x) #x7fc00000)
             ((infinite? x) #x7f800000)
             ((= x 0.0) 0)
             (else (flt-mag->bits (exact (abs x))))))))
;; floatToRawIntBits: a NaN keeps its sign and payload. A float NaN is held as
;; the double it widens to, its 23-bit payload in the top of the double's 52
;; (bits->flt below builds it that way, flsingle leaves a narrowed NaN that way),
;; so the payload is the double's shifted back down. A double NaN with nothing
;; in those bits narrows to a quiet NaN, as the JVM's d2f does.
(define (flt->raw-bits x)
  (let ((d (jolt-double x)))
    (if (nan? d)
        (let* ((b (sa-flonum->bits d))
               (m (bitwise-and (bitwise-arithmetic-shift-right b 29) #x7fffff)))
          (+ (if (>= b dbl-sign-bit) (expt 2 31) 0) #x7f800000 (if (= m 0) #x400000 m)))
        (flt->bits d))))
;; The pattern, sign clear, of an exact positive rational M rounded once to
;; single precision. Taking M exact rather than a double is what lets a caller
;; ask about a decimal without rounding it to a double first (byte-buffer.ss
;; finding the shortest digits of a float).
(define (flt-mag->bits m)
  (let ((e (dbl-binary-exponent m)))
    (if (>= e -126)
        (let* ((q (round (* m (expt 2 (- 23 e)))))   ; 2^23..2^24
               (e (if (= q (expt 2 24)) (+ e 1) e))
               (q (if (= q (expt 2 24)) (expt 2 23) q)))
          (if (> e 127)
              #x7f800000
              (+ (* (+ e 127) (expt 2 23)) (- q (expt 2 23)))))
        ;; subnormal; a round up to 2^23 is the smallest normal's
        ;; pattern, which the sum already spells
        (round (* m (expt 2 149))))))
(define (bits->flt b)
  (let* ((r (bitwise-and b #x7fffffff))
         (field (bitwise-arithmetic-shift-right r 23))
         (mant (bitwise-and r #x7fffff)))
    (if (and (= field 255) (not (= mant 0)))
        ;; a NaN, built bit by bit so its sign and payload survive (a hardware
        ;; widening would quiet a signaling one)
        (sa-bits->flonum (+ (if (>= b (expt 2 31)) dbl-sign-bit 0)
                            (* 2047 dbl-mant-unit)
                            (* mant (expt 2 29))))
        (let ((mag (cond ((= field 255) +inf.0)
                         ((= field 0) (exact->inexact (* mant (expt 2 -149))))
                         (else (exact->inexact (* (+ (expt 2 23) mant) (expt 2 (- field 150))))))))
          (if (>= b (expt 2 31)) (- mag) mag)))))
(define (jolt-float->int-bits x) (unsigned->signed (flt->bits (jolt-float->flonum x)) 32))
(define (jolt-float->raw-int-bits x) (unsigned->signed (flt->raw-bits (jolt-float->flonum x)) 32))
(define (jolt-int-bits->float n)
  (make-jfloat (bits->flt (modulo (exact (jolt-need-num n)) (expt 2 32)))))

;; Double.compare: numeric order, except that -0.0 sorts below 0.0 and NaN above
;; everything, itself included equal to itself; that tie-break is the bit
;; patterns compared as signed longs.
(define (jolt-double-compare a b)
  (let ((a (exact->inexact (jolt-need-num a))) (b (exact->inexact (jolt-need-num b))))
    (cond ((< a b) -1)
          ((> a b) 1)
          (else (let ((x (jolt-double->long-bits a)) (y (jolt-double->long-bits b)))
                  (cond ((= x y) 0) ((< x y) -1) (else 1)))))))

(register-class-statics! "Double"
  (list (cons "compare" (lambda (a b) (jolt-double-compare a b)))
        (cons "doubleToLongBits" (lambda (x) (jolt-double->long-bits x)))
        (cons "doubleToRawLongBits" (lambda (x) (jolt-double->raw-long-bits x)))
        (cons "longBitsToDouble" (lambda (n) (jolt-long-bits->double n)))))
;; Float.toString: the shortest decimal that reads back as the float, which is
;; not a double's shortest digits now that (float 0.1) holds the float's exact
;; value 0.10000000149011612. Float.toString writes at least one digit after
;; the point, so in its scientific range (below 10^-3, from 10^7) the shortest
;; form has two significant digits: Float.MIN_VALUE is 1.4E-45, never 1.0E-45.
;; Answers the decimal as a double, whose own printing then spells it.
(define (flt-shortest x)
  (let ((x (jolt-unchecked-float->flonum x)))
    (if (or (nan? x) (infinite? x) (= x 0.0))
        x
        (let* ((m (exact (abs x)))
               (bits (flt-mag->bits m))
               (e10 (let loop ((e (exact (floor (/ (log (inexact m)) (log 10))))))
                      (cond ((> (expt 10 e) m) (loop (- e 1)))
                            ((<= (expt 10 (+ e 1)) m) (loop (+ e 1)))
                            (else e))))
               (d (let loop ((n (if (or (< m 1/1000) (>= m 10000000)) 2 1)))
                    (if (> n 9)
                        (inexact m)
                        (let* ((s (expt 10 (- e10 (- n 1))))
                               (cand (* (round (/ m s)) s)))
                          (if (and (> cand 0) (= (flt-mag->bits cand) bits))
                              (inexact cand)
                              (loop (+ n 1))))))))
          (if (< x 0.0) (- d) d)))))
;; Float.parseFloat/valueOf: the parsed double rounded to single precision, past
;; the float range an infinity (a parse does not range-check like the cast).
(define (jolt-parse-float s)
  (if (or (number? s) (jfloat? s))
      (make-jfloat (jolt-unchecked-float->flonum s))
      (make-jfloat (flsingle (parse-double-or-throw s)))))
;; Float.compare: numeric order, then -0.0 below 0.0 and NaN above everything,
;; which is the order of the signed bit patterns.
(define (jolt-float-compare a b)
  (let ((x (jolt-float->flonum a)) (y (jolt-float->flonum b)))
    (cond ((< x y) -1)
          ((> x y) 1)
          (else (let ((bx (unsigned->signed (flt->bits x) 32))
                      (by (unsigned->signed (flt->bits y) 32)))
                  (cond ((= bx by) 0) ((< bx by) -1) (else 1)))))))

(register-class-statics! "Float"
  (list (cons "parseFloat" jolt-parse-float) (cons "valueOf" jolt-parse-float)
        (cons "toString" (lambda (x) (jolt-str-render-one (flt-shortest x))))
        (cons "compare" jolt-float-compare)
        (cons "isNaN" (lambda (x) (nan? (jolt-double x))))
        (cons "isInfinite" (lambda (x) (infinite? (jolt-double x))))
        ;; Floats, as on the JVM: (str Float/MAX_VALUE) is 3.4028235E38
        (cons "MAX_VALUE" (make-jfloat 3.4028234663852886e38))
        (cons "MIN_VALUE" (make-jfloat 1.401298464324817e-45))
        (cons "MIN_NORMAL" (make-jfloat 1.1754943508222875e-38))
        (cons "POSITIVE_INFINITY" (make-jfloat +inf.0))
        (cons "NEGATIVE_INFINITY" (make-jfloat -inf.0))
        (cons "NaN" (make-jfloat +nan.0))
        (cons "floatToIntBits" (lambda (x) (jolt-float->int-bits x)))
        (cons "floatToRawIntBits" (lambda (x) (jolt-float->raw-int-bits x)))
        (cons "intBitsToFloat" (lambda (n) (jolt-int-bits->float n)))))
