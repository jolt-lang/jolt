; loop/recur ownership rule in stdlib/jolt/native/memory.clj (no Carp counterpart:
; Carp has while). One iteration that continues, from the loop head. Expect: unsat.
; O = values live at the loop head other than loop vars; V = owned loop-var values;
; C = owned values created during the iteration; D = values the iteration consumes
; (moves into a let, returns from a nested form...); A = values the recur passes
; as the next loop-var values. The checker requires O subset-of L at the recur and
; frees F = L \ O there.
(declare-const O (_ BitVec 16)) (declare-const V (_ BitVec 16)) (declare-const C (_ BitVec 16))
(declare-const D (_ BitVec 16)) (declare-const A (_ BitVec 16))
(define-fun sub ((a (_ BitVec 16)) (b (_ BitVec 16))) (_ BitVec 16) (bvand a (bvnot b)))
(define-fun subset ((a (_ BitVec 16)) (b (_ BitVec 16))) Bool (= (sub a b) #x0000))
(define-fun disj ((a (_ BitVec 16)) (b (_ BitVec 16))) Bool (= (bvand a b) #x0000))
(define-fun all () (_ BitVec 16) (bvor O V C))
(assert (disj O V)) (assert (disj C (bvor O V)))
(assert (subset D all))
(assert (subset A (sub all D)))
(define-fun L () (_ BitVec 16) (sub all (bvor D A)))
(assert (subset O L))                     ; the checker's recur condition
(define-fun F () (_ BitVec 16) (sub L O))  ; what the recur frees
; every value accounted for exactly once; nothing outer consumed; nothing freed twice
(assert (not (and (disj D A) (disj D F) (disj A F) (disj O D) (disj O A) (disj O F)
                  (= (bvor D A F O) all)
                  ; the next head is O plus the passed values: the invariant shape holds
                  (disj O A))))
(check-sat)
