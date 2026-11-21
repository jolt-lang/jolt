; The let rule of stdlib/jolt/native/memory.clj (Carp Memory.hs LetPat).
; pre = owned values live before the let; post = live after the body, once the
; body's value has been moved out (unmanage!), so the result r is not in post;
; diff = post \ pre is freed when the let ends; survivors = post \ diff.
; A let's end frees only live values, never an outer one, never its result, and
; what survives is exactly the outer values the let did not consume. Expect: unsat.
(declare-const pre (_ BitVec 16)) (declare-const post (_ BitVec 16)) (declare-const r (_ BitVec 16))
(define-fun sub ((a (_ BitVec 16)) (b (_ BitVec 16))) (_ BitVec 16) (bvand a (bvnot b)))
(define-fun disj ((a (_ BitVec 16)) (b (_ BitVec 16))) Bool (= (bvand a b) #x0000))
(define-fun subset ((a (_ BitVec 16)) (b (_ BitVec 16))) Bool (= (sub a b) #x0000))
(assert (disj r post))
(define-fun diff () (_ BitVec 16) (sub post pre))
(define-fun survivors () (_ BitVec 16) (sub post diff))
(assert (not (and (subset diff post) (disj diff pre) (disj diff r)
                  (= survivors (bvand post pre)))))
(check-sat)
