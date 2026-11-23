; mod's range (range.clj divide) for a positive divisor b <= bh: clojure.core/mod,
; (let [r (rem a b)] (if (or (zero? r) (= (pos? a) (pos? b))) r (+ r b))),
; lies in [0, bh-1]. 64-bit operands. Expect: unsat.
(declare-const a (_ BitVec 64)) (declare-const b (_ BitVec 64)) (declare-const bh (_ BitVec 64))
(define-fun Z () (_ BitVec 64) #x0000000000000000)
(assert (bvsgt b Z))
(assert (bvsle b bh))
(define-fun r () (_ BitVec 64) (bvsrem a b))
(define-fun md () (_ BitVec 64)
  (ite (or (= r Z) (= (bvsgt a Z) (bvsgt b Z))) r (bvadd r b)))
(assert (not (and (bvsge md Z) (bvslt md bh))))
(check-sat)
