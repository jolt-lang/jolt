; (double-array n) / (long-array n) in a native region: the emitted size is
;   lo = n & 0xffffffff;  len = lo >= 0x80000000 ? lo - 0x100000000 : lo
; which must be Number.intValue (the low 32 bits, signed: JVM Numbers.T_array),
; and when len >= 0 the byte counts calloc(len > 0 ? len : 1, 8), malloc and
; memcpy(len * 8) are exact (no wrap of the 64-bit size_t). Expect: unsat.
(declare-const n (_ BitVec 64))
(define-fun lo () (_ BitVec 64) (bvand n #x00000000ffffffff))
(define-fun len () (_ BitVec 64)
  (ite (bvuge lo #x0000000080000000) (bvsub lo #x0000000100000000) lo))
(define-fun intValue () (_ BitVec 64) ((_ sign_extend 32) ((_ extract 31 0) n)))
; bytes as a 128-bit product, so a wrap of the 64-bit computation would show
(define-fun bytes128 () (_ BitVec 128) (bvmul ((_ zero_extend 64) len) #x00000000000000000000000000000008))
(define-fun bytes64 () (_ BitVec 64) (bvmul len #x0000000000000008))
(assert (not (and (= len intValue)
                  (=> (bvsge len #x0000000000000000)
                      (and (= ((_ zero_extend 64) bytes64) bytes128)
                           (bvule bytes64 #x0000000400000000))))))
(check-sat)
