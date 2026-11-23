; Every aget/aset in a native region (emit-array-index in stdlib/jolt/native/emit.clj)
; is guarded:  if (i < INT32_MIN || i > INT32_MAX) raise;  if (i < 0 || i >= len) raise;
; then data[i]. With len the array's element count (0 <= len), the access is in
; bounds whenever both guards pass. 64-bit signed arithmetic. Expect: unsat.
(declare-const i (_ BitVec 64)) (declare-const len (_ BitVec 64))
(define-fun I32MIN () (_ BitVec 64) (bvneg #x0000000080000000))
(define-fun I32MAX () (_ BitVec 64) #x000000007fffffff)
(assert (bvsge len #x0000000000000000))
(define-fun guards-pass () Bool
  (and (not (or (bvslt i I32MIN) (bvsgt i I32MAX)))
       (not (or (bvslt i #x0000000000000000) (bvsge i len)))))
(define-fun in-bounds () Bool (and (bvsge i #x0000000000000000) (bvslt i len)))
(assert (and guards-pass (not in-bounds)))
(check-sat)
