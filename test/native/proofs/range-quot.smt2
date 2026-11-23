; quot's range (range.clj divide): with b nonzero and not the MIN / -1 case
; (which raises), |(quot a b)| <= |a|. Compared at 65 bits so |MIN| is exact.
; Expect: unsat.
(declare-const a (_ BitVec 64)) (declare-const b (_ BitVec 64))
(define-fun Z () (_ BitVec 64) #x0000000000000000)
(define-fun absw ((v (_ BitVec 64))) (_ BitVec 65)
  (ite (bvslt v Z) (bvneg ((_ sign_extend 1) v)) ((_ sign_extend 1) v)))
(assert (not (= b Z)))
(assert (not (and (= a #x8000000000000000) (= b #xffffffffffffffff))))
(assert (not (bvsle (absw (bvsdiv a b)) (absw a))))
(check-sat)
