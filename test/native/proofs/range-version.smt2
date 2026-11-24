; The fast copy of a function (emit-versioned in emit.clj) runs only when its
; guard holds: each array parameter's len <= INT32_MAX, and len > h for the
; largest index h the facts proved for it. An access whose index lies in [0, h]
; is then in bounds, and an index proven below an array's length (i < (alength
; a) on the path) with i >= 0 is in bounds and fits an int. So the bounds and
; int-cast checks the fast copy drops could not have fired. Expect: unsat.
(declare-const i Int) (declare-const h Int) (declare-const len Int)
(define-fun I32MIN () Int (- 2147483648))
(define-fun I32MAX () Int 2147483647)
(define-fun guard () Bool (and (<= len I32MAX) (> len h)))
(define-fun fires () Bool (or (< i I32MIN) (> i I32MAX) (< i 0) (>= i len)))
(define-fun versioned-ok () Bool (=> (and guard (<= 0 i) (<= i h)) (not fires)))
(define-fun lt-len-ok () Bool (=> (and (<= len I32MAX) (<= 0 i) (< i len)) (not fires)))
(assert (>= len 0))
(assert (not (and versioned-ok lt-len-ok)))
(check-sat)
