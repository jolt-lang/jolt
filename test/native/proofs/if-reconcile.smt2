; Soundness of the if-branch deleter reconciliation in stdlib/jolt/native/memory.clj
; (ported from Carp src/Memory.hs IfPat). Expect: unsat.
; pre = owned vars live before the if; dT/dF = pre vars consumed in each branch;
; cT/cF = owned values created in a branch and alive at its end; self = the result.
; Sets are 16-bit vectors; every operation is element-wise, so this covers all cases.
(declare-const pre (_ BitVec 16)) (declare-const dT (_ BitVec 16)) (declare-const dF (_ BitVec 16))
(declare-const cT (_ BitVec 16)) (declare-const cF (_ BitVec 16)) (declare-const self (_ BitVec 16))
(define-fun sub ((a (_ BitVec 16)) (b (_ BitVec 16))) (_ BitVec 16) (bvand a (bvnot b)))
(define-fun subset ((a (_ BitVec 16)) (b (_ BitVec 16))) Bool (= (sub a b) #x0000))
(define-fun disj ((a (_ BitVec 16)) (b (_ BitVec 16))) Bool (= (bvand a b) #x0000))
(assert (subset dT pre)) (assert (subset dF pre))
(assert (disj cT pre)) (assert (disj cF pre)) (assert (disj self pre))
(declare-const both (_ BitVec 16)) (assert (= both (bvand dT dF)))
(declare-const delsT (_ BitVec 16)) (assert (= delsT (bvor (sub dF both) (sub cT self))))
(declare-const delsF (_ BitVec 16)) (assert (= delsF (bvor (sub dT both) (sub cF self))))
(declare-const after (_ BitVec 16)) (assert (= after (sub pre (bvor dT dF))))
; no double free; outer values freed exactly once per path; branch temporaries freed, result kept
(declare-const ok Bool)
(assert (= ok (and (disj dT delsT) (disj dF delsF)
                   (= (bvor dT (bvand delsT pre)) (sub pre after))
                   (= (bvor dF (bvand delsF pre)) (sub pre after))
                   (= (bvand delsT (bvnot pre)) (sub cT self))
                   (= (bvand delsF (bvnot pre)) (sub cF self))
                   (disj delsT self) (disj delsF self))))
(assert (not ok))
(check-sat)
