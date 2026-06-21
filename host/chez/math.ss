;; clojure.math (jolt-22vo) — Chez host shim over native flonum math.
;;
;; On the Janet seed clojure.math is registered as native math/ bindings
;; (api.janet install-clojure-math!, jolt-h79), NOT a .clj file — so there's no
;; source tier to emit. Chez provides its own def-var! shims here, one per
;; clojure.math fn, over Chez's native procedures. The analyzer already knows the
;; clojure.math ns exists (init interns the same fns on the Janet side), so a ref
;; like clojure.math/sqrt lowers to a var-deref; these cells back it at runtime.
;;
;; jolt is all-flonum, so every result is a flonum (inputs arrive as flonums; Chez
;; sqrt/sin/expt/... return flonums for flonum args). Semantics match the seed
;; (Clojure 1.11 clojure.math): round = floor(x+0.5), rint = round-half-even,
;; floor/ceil/floor-div return doubles, to-degrees/to-radians via PI.

(define jolt-math-pi (acos -1.0))
(define jolt-math-e (exp 1.0))

(define (jolt-math-cbrt x)
  ;; sign-aware so negative inputs stay real (expt of a negative flonum to a
  ;; fractional power goes complex).
  (if (< x 0.0)
      (- (expt (- x) (/ 1.0 3.0)))
      (expt x (/ 1.0 3.0))))

;; clojure.math/round returns a long (exact); floor/ceil/signum/rint return doubles.
(define (jolt-math-round x) (exact (floor (+ x 0.5))))
(define (jolt-math-signum x) (cond ((< x 0.0) -1.0) ((> x 0.0) 1.0) (else 0.0)))
(define (jolt-math-to-degrees r) (/ (* r 180.0) jolt-math-pi))
(define (jolt-math-to-radians d) (/ (* d jolt-math-pi) 180.0))
(define (jolt-math-hypot a b) (sqrt (+ (* a a) (* b b))))
(define (jolt-math-floor-div a b) (floor (/ a b)))
(define (jolt-math-floor-mod a b) (- a (* b (floor (/ a b)))))

;; clojure.math fns always return a DOUBLE; Chez's sqrt/expt/sin/floor/... return
;; EXACT for exact args ((sqrt 9) -> 3, (sin 0) -> 0), so coerce.
(define (m1 f) (lambda (x) (exact->inexact (f x))))
(define (m2 f) (lambda (a b) (exact->inexact (f a b))))
(def-var! "clojure.math" "sqrt" (m1 sqrt))
(def-var! "clojure.math" "cbrt" jolt-math-cbrt)
(def-var! "clojure.math" "pow" (m2 expt))
(def-var! "clojure.math" "exp" (m1 exp))
(def-var! "clojure.math" "expm1" (lambda (x) (- (exp x) 1.0)))
(def-var! "clojure.math" "log" (m1 log))
(def-var! "clojure.math" "log10" (lambda (x) (exact->inexact (log x 10.0))))
(def-var! "clojure.math" "log1p" (lambda (x) (log (+ 1.0 x))))
(def-var! "clojure.math" "sin" (m1 sin))
(def-var! "clojure.math" "cos" (m1 cos))
(def-var! "clojure.math" "tan" (m1 tan))
(def-var! "clojure.math" "asin" (m1 asin))
(def-var! "clojure.math" "acos" (m1 acos))
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
