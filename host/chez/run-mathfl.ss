;; run-mathfl.ss — java.lang.Math over proven flonum operands lowers to the native
;; Chez flonum op (flsqrt/flatan/…) instead of the generic string-keyed
;; host-static-call, and keeps flonum contagion in the surrounding arithmetic.
;;
;;   chez --script host/chez/run-mathfl.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")

(define analyze (var-deref "jolt.analyzer" "analyze"))
(define numeric-annotate (var-deref "jolt.passes.numeric" "annotate"))
(define emit (var-deref "jolt.backend-scheme" "emit"))
(define U ((var-deref "jolt.passes.types" "new-unit")))
((var-deref "jolt.backend-scheme" "set-emit-unit!") U)
((var-deref "jolt.backend-scheme" "set-prelude-mode!") #t)
(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))
(define (emit-num src) (emit (numeric-annotate (anode src))))

;; (1) Math/sqrt over a ^double arg -> flsqrt
(let ((e (emit-num "(def _ (fn [^double x] (Math/sqrt x)))")))
  (gate-check "(1) Math/sqrt ^double -> flsqrt" (gate-sub? e "flsqrt") #t)
  (gate-check "(1) ...not the generic host-static-call" (gate-sub? e "host-static-call") #f))

;; (2) contagion: (+ a (Math/sqrt a)) stays fl+ AND flsqrt (the whole point)
(let ((e (emit-num "(def _ (fn [^double a] (+ a (Math/sqrt a))))")))
  (gate-check "(2) surrounding add stays fl+" (gate-sub? e "fl+") #t)
  (gate-check "(2) inner Math/sqrt is flsqrt" (gate-sub? e "flsqrt") #t))

;; (3) atan2(y,x) -> (flatan y x)
(let ((e (emit-num "(def _ (fn [^double y ^double x] (Math/atan2 y x)))")))
  (gate-check "(3) Math/atan2 -> flatan" (gate-sub? e "flatan") #t))

;; (4) floor / pow
(let ((e (emit-num "(def _ (fn [^double x] (Math/floor x)))")))
  (gate-check "(4) Math/floor -> flfloor" (gate-sub? e "flfloor") #t))
(let ((e (emit-num "(def _ (fn [^double b] (Math/pow b 2)))")))
  (gate-check "(4) Math/pow (int literal coerced) -> flexpt" (gate-sub? e "flexpt") #t))

;; (5) untyped arg -> NOT specialized (stays host-static-call)
(let ((e (emit-num "(def _ (fn [x] (Math/sqrt x)))")))
  (gate-check "(5) untyped Math/sqrt stays generic host-static-call" (gate-sub? e "host-static-call") #t)
  (gate-check "(5) ...and is NOT flsqrt" (gate-sub? e "flsqrt") #f))

;; (6) all-integer-literal Math/abs keeps its (generic) result — the guard requires a
;; genuine :double operand, so (Math/abs 5) is NOT coerced to flabs on a flonum.
(let ((e (emit-num "(def _ (fn [] (Math/abs 5)))")))
  (gate-check "(6) (Math/abs 5) stays generic (no flabs)" (gate-sub? e "flabs") #f))

(gate-summary "mathfl")
