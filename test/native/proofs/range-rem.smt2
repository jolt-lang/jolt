; rem's range (range.clj divide), for a divisor proven positive, b in [1, bh]:
; (rem a b) lies in [1-bh, bh-1], and in [0, bh-1] when a >= 0. (divide gives a
; rem over any other divisor the full long range.) 64-bit operands.
; Expect: unsat.
(declare-const a (_ BitVec 64)) (declare-const b (_ BitVec 64)) (declare-const bh (_ BitVec 64))
(define-fun Z () (_ BitVec 64) #x0000000000000000)
(define-fun ONE () (_ BitVec 64) #x0000000000000001)
(assert (bvsgt b Z))
(assert (bvsle b bh))
(define-fun r () (_ BitVec 64) (bvsrem a b))
(assert (not (and (bvsle (bvsub ONE bh) r) (bvslt r bh)
                  (=> (bvsge a Z) (bvsge r Z)))))
(check-sat)
