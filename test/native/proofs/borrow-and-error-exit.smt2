; Two rules of stdlib/jolt/native/memory.clj, as sets of owned values (16-bit).
; Borrow at use: an op borrows a (live when its array argument ran, L0); its
; other arguments then run, taking the live set to L1 (they may move a). The
; checker refuses unless a is in L1 at the op, so the block the op touches is
; never one freed in between (freed = L0 \ L1).
; Error exit: a fallible node records live = L1 (snapshot), and on failure the
; function frees exactly that set and returns. Values freed earlier on the path
; (E) left the live set when they were freed, so nothing is freed twice, and
; nothing live is leaked. Expect: unsat.
(declare-const L0 (_ BitVec 16)) (declare-const L1 (_ BitVec 16)) (declare-const a (_ BitVec 16))
(declare-const E (_ BitVec 16))
(define-fun sub ((x (_ BitVec 16)) (y (_ BitVec 16))) (_ BitVec 16) (bvand x (bvnot y)))
(define-fun disj ((x (_ BitVec 16)) (y (_ BitVec 16))) Bool (= (bvand x y) #x0000))
(define-fun subset ((x (_ BitVec 16)) (y (_ BitVec 16))) Bool (= (sub x y) #x0000))
; a is one value, live when the array argument ran
(assert (and (not (= a #x0000)) (= (bvand a (bvsub a #x0001)) #x0000) (subset a L0)))
; the checker's condition at the op
(assert (subset a L1))
; values freed before the failing node are not live there
(assert (disj E L1))
(define-fun freed-between () (_ BitVec 16) (sub L0 L1))
(define-fun F () (_ BitVec 16) L1)
(assert (not (and (disj a freed-between)
                  (disj F E)
                  (= (bvor E F) (bvor E L1)))))
(check-sat)
