; A store into a borrowed long-array (a Chez fxvector, slot = value * 8): the
; region raises unless -2^60 <= v <= 2^60-1, then writes (int64_t)((uint64_t)v * 8),
; and a read is slot / 8 (C division truncates; the slot is a multiple of 8).
; Within the range the encoding never wraps and decoding gives v back; the
; result _copy_out stores under the same check. Expect: unsat.
(declare-const v (_ BitVec 64))
(define-fun FXMIN () (_ BitVec 64) (bvneg #x1000000000000000))
(define-fun FXMAX () (_ BitVec 64) #x0fffffffffffffff)
(define-fun in-range () Bool (and (bvsge v FXMIN) (bvsle v FXMAX)))
(define-fun slot () (_ BitVec 64) (bvmul v #x0000000000000008))
; exact product in 128 bits, signed
(define-fun exact () (_ BitVec 128) (bvmul ((_ sign_extend 64) v) #x00000000000000000000000000000008))
(assert (and in-range
             (not (and (= ((_ sign_extend 64) slot) exact)
                       (= (bvsdiv slot #x0000000000000008) v)))))
(check-sat)
