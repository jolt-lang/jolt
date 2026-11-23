; Branch narrowing (range.clj relate/narrow): in the branch where x < y holds,
; x <= hi(y) - 1 and y >= lo(x) + 1; where x <= y holds, x <= hi(y) and
; y >= lo(x). A false test is the opposite comparison with the operands
; swapped: not (x < y) is y <= x, not (x <= y) is y < x. Expect: unsat.
(declare-const x Int) (declare-const y Int)
(declare-const xl Int) (declare-const xh Int) (declare-const yl Int) (declare-const yh Int)
(assert (and (<= xl x) (<= x xh) (<= yl y) (<= y yh)))
(define-fun strict-ok () Bool (=> (< x y) (and (<= x (- yh 1)) (>= y (+ xl 1)))))
(define-fun loose-ok () Bool (=> (<= x y) (and (<= x yh) (>= y xl))))
(define-fun neg-ok () Bool (and (= (not (< x y)) (<= y x)) (= (not (<= x y)) (< y x))))
(assert (not (and strict-ok loose-ok neg-ok)))
(check-sat)
