; Range facts (range.clj r-mul): for x in [a,b] and y in [c,d], x*y lies between
; the least and greatest of the four corner products. When both fit the long
; range the multiply's overflow check is dropped. Expect: unsat.
(declare-const a Int) (declare-const b Int) (declare-const c Int) (declare-const d Int)
(declare-const x Int) (declare-const y Int)
(assert (and (<= a x) (<= x b) (<= c y) (<= y d)))
(define-fun lo () Int
  (let ((p (* a c)) (q (* a d)) (r (* b c)) (s (* b d)))
    (let ((m1 (ite (< p q) p q)) (m2 (ite (< r s) r s))) (ite (< m1 m2) m1 m2))))
(define-fun hi () Int
  (let ((p (* a c)) (q (* a d)) (r (* b c)) (s (* b d)))
    (let ((m1 (ite (> p q) p q)) (m2 (ite (> r s) r s))) (ite (> m1 m2) m1 m2))))
(assert (not (and (<= lo (* x y)) (<= (* x y) hi))))
(check-sat)
