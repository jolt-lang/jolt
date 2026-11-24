; (long d) inside a native region: the emitted C guard must agree with Jolt's
; RT.longCast(double) and never perform an undefined C cast. Expect: unsat.
; Jolt (= JVM): NaN -> 0; d < -2^63 or d > 2^63 -> IllegalArgumentException;
; d == 2^63 -> Long/MAX_VALUE; otherwise truncate toward zero.
; C: (d != d) -> 0; (d >= -2^63 && d < 2^63) -> (int64_t)d; (d == 2^63) -> INT64_MAX; else error.
(declare-const d (_ FloatingPoint 11 53))
(define-fun P63 () (_ FloatingPoint 11 53) ((_ to_fp 11 53) RNE 9223372036854775808.0))
(define-fun N63 () (_ FloatingPoint 11 53) (fp.neg P63))
(define-fun spec_err () Bool (and (not (fp.isNaN d)) (or (fp.lt d N63) (fp.gt d P63))))
(define-fun c_nan () Bool (fp.isNaN d))
(define-fun c_cast () Bool (and (not c_nan) (fp.geq d N63) (fp.lt d P63)))
(define-fun c_max () Bool (and (not c_nan) (fp.eq d P63)))
(define-fun c_err () Bool (and (not c_nan) (not c_cast) (not c_max)))
(define-fun tr () (_ FloatingPoint 11 53) (fp.roundToIntegral RTZ d))
(define-fun cast_defined () Bool (and (fp.geq tr N63) (fp.lt tr P63)))
(assert (not (and (= c_err spec_err) (=> c_cast cast_defined))))
(check-sat)
