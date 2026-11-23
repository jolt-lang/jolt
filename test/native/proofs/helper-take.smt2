; Inferred ownership at a native helper call (stdlib/jolt/native/memory.clj :call
; and check-body). The caller's live set is L. T = the arguments the helper takes
; (moved out of L at the call); B = the arrays it only borrows (must be live, stay
; the caller's). The helper frees what it took and did not return (HF = T \ R)
; and returns R (a subset of T, possibly empty) or a fresh array as its result,
; which the caller then owns. Never-taken parameters are stripped from the
; helper's free sets, so HF excludes B. Every taken value is freed exactly once,
; by the helper or by the caller through the result; the caller frees nothing
; it gave away; nothing it lent is freed by the helper. Expect: unsat.
(declare-const L (_ BitVec 16)) (declare-const T (_ BitVec 16)) (declare-const B (_ BitVec 16))
(declare-const R (_ BitVec 16))
(define-fun sub ((x (_ BitVec 16)) (y (_ BitVec 16))) (_ BitVec 16) (bvand x (bvnot y)))
(define-fun disj ((x (_ BitVec 16)) (y (_ BitVec 16))) Bool (= (bvand x y) #x0000))
(define-fun subset ((x (_ BitVec 16)) (y (_ BitVec 16))) Bool (= (sub x y) #x0000))
(assert (subset T L)) (assert (subset R T))
; borrowed arguments are checked live after the taken ones moved
(assert (subset B (sub L T)))
(define-fun HF () (_ BitVec 16) (sub T R))
(define-fun caller-after () (_ BitVec 16) (bvor (sub L T) R))
(assert (not (and (disj HF caller-after)          ; nothing freed twice
                  (= (bvor HF R) T) (disj HF R)    ; each taken value accounted once
                  (disj HF B)                      ; lent arrays survive the call
                  (subset B caller-after))))
(check-sat)
