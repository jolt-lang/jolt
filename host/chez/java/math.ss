;; clojure.math — host shim over native flonum math.
;;
;; clojure.math is registered as native bindings, NOT a .clj file — so there's no
;; source tier to emit. The def-var! shims here back each clojure.math fn over
;; Chez's native procedures. The analyzer knows the clojure.math ns exists, so a
;; ref like clojure.math/sqrt lowers to a var-deref; these cells back it at
;; runtime.
;;
;; jolt is all-flonum, so every result is a flonum (inputs arrive as flonums; Chez
;; sqrt/sin/expt/... return flonums for flonum args). Semantics match
;; Clojure 1.11 clojure.math: round = floor(x+0.5), rint = round-half-even,
;; floor/ceil/floor-div return doubles, to-degrees/to-radians via PI.

(define jolt-math-pi (acos -1.0))
(define jolt-math-e (exp 1.0))

(define (jolt-math-cbrt x)
  ;; sign-aware so negative inputs stay real (expt of a negative flonum to a
  ;; fractional power goes complex).
  (if (< x 0.0)
      (- (expt (- x) (/ 1.0 3.0)))
      (expt x (/ 1.0 3.0))))

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
(define (jolt-math-signum x) (cond ((< x 0.0) -1.0) ((> x 0.0) 1.0) (else 0.0)))
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
      ((< ax 0.5)
       (let loop ((term x) (n 2) (acc x))
         (let* ((nt (* term (/ x n))) (acc2 (+ acc nt)))
           (if (< (abs nt) (* 1e-18 (abs acc2))) acc2
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
  (let ((a (exact (truncate a))) (b (exact (truncate b)))) (floor (/ a b))))
(define (jolt-math-floor-mod a b)
  (let ((a (exact (truncate a))) (b (exact (truncate b)))) (- a (* b (floor (/ a b))))))

;; clojure.math fns always return a DOUBLE; Chez's sqrt/expt/sin/floor/... return
;; EXACT for exact args ((sqrt 9) -> 3, (sin 0) -> 0), so coerce.
(define (m1 f) (lambda (x) (exact->inexact (f x))))
(define (m2 f) (lambda (a b) (exact->inexact (f a b))))
;; a real result stays a flonum; a complex result becomes +nan.0. Chez extends
;; several real-domain ops (sqrt/expt/log/asin/acos, and log's kin log10/log1p)
;; onto the complex plane for out-of-domain real inputs, but Java/clojure.math
;; returns NaN there. real? is #t for a flonum and #f for a Chez complex, so this
;; guards exactly the complex leak; NaN/Inf are real and pass through unchanged.
(define (real-or-nan x) (if (and (number? x) (real? x)) (exact->inexact x) +nan.0))
(define (m1c f) (lambda (x) (real-or-nan (f x))))
(define (m2c f) (lambda (a b) (real-or-nan (f a b))))
(def-var! "clojure.math" "sqrt" (m1c sqrt))
(def-var! "clojure.math" "cbrt" jolt-math-cbrt)
(def-var! "clojure.math" "pow" (m2c expt))
(def-var! "clojure.math" "exp" (m1 exp))
(def-var! "clojure.math" "expm1" jolt-math-expm1)
(def-var! "clojure.math" "log" (m1c log))
(def-var! "clojure.math" "log10" (lambda (x) (real-or-nan (log x 10.0))))
(def-var! "clojure.math" "log1p" jolt-math-log1p)
(def-var! "clojure.math" "sin" (m1 sin))
(def-var! "clojure.math" "cos" (m1 cos))
(def-var! "clojure.math" "tan" (m1 tan))
(def-var! "clojure.math" "asin" (m1c asin))
(def-var! "clojure.math" "acos" (m1c acos))
(def-var! "clojure.math" "atan" (m1 atan))
;; clojure.math/atan2 is atan2(y, x); Chez's 2-arg atan is (atan y x).
(def-var! "clojure.math" "atan2" (lambda (y x) (exact->inexact (atan y x))))
(def-var! "clojure.math" "sinh" (m1 sinh))
(def-var! "clojure.math" "cosh" (m1 cosh))
(def-var! "clojure.math" "tanh" (m1 tanh))
(def-var! "clojure.math" "floor" (m1 floor))
(def-var! "clojure.math" "ceil" (m1 ceiling))
(def-var! "clojure.math" "rint" (m1 round))
(def-var! "clojure.math" "round" jolt-math-round)
(def-var! "clojure.math" "signum" jolt-math-signum)
(def-var! "clojure.math" "to-degrees" jolt-math-to-degrees)
(def-var! "clojure.math" "to-radians" jolt-math-to-radians)
(def-var! "clojure.math" "hypot" jolt-math-hypot)
(def-var! "clojure.math" "floor-div" jolt-math-floor-div)
(def-var! "clojure.math" "floor-mod" jolt-math-floor-mod)
(def-var! "clojure.math" "E" jolt-math-e)
(def-var! "clojure.math" "PI" jolt-math-pi)
