; Range facts (stdlib/jolt/native/range.clj, r-add/r-sub/fits?): for x in [a,b] and
; y in [c,d], x+y lies in [a+c, b+d] and x-y in [a-d, b-c] (exact integers). When
; that interval fits the long range the emitter drops the overflow check, so the
; 64-bit result must equal the exact one. Expect: unsat.
(declare-const a Int) (declare-const b Int) (declare-const c Int) (declare-const d Int)
(declare-const x Int) (declare-const y Int)
(define-fun LMIN () Int (- 9223372036854775808))
(define-fun LMAX () Int 9223372036854775807)
(define-fun long? ((v Int)) Bool (and (<= LMIN v) (<= v LMAX)))
(assert (and (long? x) (long? y) (<= a x) (<= x b) (<= c y) (<= y d)))
(define-fun add-ok () Bool
  (=> (and (long? (+ a c)) (long? (+ b d)))
      (and (long? (+ x y)) (<= (+ a c) (+ x y)) (<= (+ x y) (+ b d)))))
(define-fun sub-ok () Bool
  (=> (and (long? (- a d)) (long? (- b c)))
      (and (long? (- x y)) (<= (- a d) (- x y)) (<= (- x y) (- b c)))))
(assert (not (and add-ok sub-ok)))
(check-sat)
