;; bit ops + string->number parsers — host-coupled natives (bit family,
;; parse-long/double). jolt models every number as a double, so bit ops coerce
;; to an exact integer, operate, and return a flonum. parse-* use strict shapes
;; (Clojure 1.11: nil on malformed, throw on a non-string).

;; bit ops return EXACT integers (= JVM long). ->int coerces the operand.
(define (->int x) (exact (truncate x)))
;; Mask shift count to low 6 bits (JVM long shift semantics), then wrap result
;; to 64-bit signed two's complement.
(define (shift-mask n) (bitwise-and (->int n) 63))
(define (wrap64 x)
  (let ((m (bitwise-and x #xFFFFFFFFFFFFFFFF)))
    (if (>= m #x8000000000000000) (- m #x10000000000000000) m)))
(define (jolt-bit-and a b)     (bitwise-and (->int a) (->int b)))
(define (jolt-bit-or a b)      (bitwise-ior (->int a) (->int b)))
(define (jolt-bit-xor a b)     (bitwise-xor (->int a) (->int b)))
(define (jolt-bit-and-not a b) (bitwise-and (->int a) (bitwise-not (->int b))))
(define (jolt-bit-not a)       (bitwise-not (->int a)))
(define (jolt-bit-shift-left x n)  (wrap64 (bitwise-arithmetic-shift-left (->int x) (shift-mask n))))
(define (jolt-bit-shift-right x n) (wrap64 (bitwise-arithmetic-shift-right (->int x) (shift-mask n))))
(define (bit-mask n) (bitwise-arithmetic-shift-left 1 (->int n)))
(define (jolt-bit-set x n)   (bitwise-ior (->int x) (bit-mask n)))
(define (jolt-bit-clear x n) (bitwise-and (->int x) (bitwise-not (bit-mask n))))
(define (jolt-bit-flip x n)  (bitwise-xor (->int x) (bit-mask n)))
(define (jolt-bit-test x n)  (not (zero? (bitwise-and (->int x) (bit-mask n)))))
;; unsigned-bit-shift-right: LOGICAL right shift over a 64-bit long (Java >>>),
;; so a negative operand shifts in zeros from its 64-bit two's-complement window
;; ((>>> -1 1) = 2^63-1), not the sign. The shift count is taken mod 64.
(define (jolt-unsigned-bit-shift-right x n)
  (bitwise-arithmetic-shift-right (bitwise-and (->int x) #xFFFFFFFFFFFFFFFF)
                                  (bitwise-and (->int n) 63)))

;; ---- string->scalar parsers -------------------------------------------------
(define (ascii-digit? c) (and (char>=? c #\0) (char<=? c #\9)))
(define (skip-digits s i n) (let loop ((i i)) (if (and (< i n) (ascii-digit? (string-ref s i))) (loop (+ i 1)) i)))
(define (sign-at? s i n) (and (< i n) (let ((c (string-ref s i))) (or (char=? c #\+) (char=? c #\-)))))

(define (parse-long-shape? s)
  (let* ((n (string-length s)) (i0 (if (sign-at? s 0 n) 1 0)))
    (and (> n i0) (= (skip-digits s i0 n) n))))

(define (jolt-parse-long s)
  (if (not (string? s)) (error #f "parse-long requires a string" s)
      (if (parse-long-shape? s) (string->number s) jolt-nil)))   ; exact long

;; strict float shape: [+-]? ( D+ (. D*)? | . D+ ) ([eE][+-]? D+)?  fully anchored.
(define (parse-double-shape? s)
  (let ((n (string-length s)))
    (and (> n 0)
      (call/cc
        (lambda (no)
          (let* ((i0 (if (sign-at? s 0 n) 1 0))
                 (after-int (skip-digits s i0 n))
                 (had-int (> after-int i0))
                 ;; mantissa end
                 (jm (cond
                       ((and had-int (< after-int n) (char=? (string-ref s after-int) #\.))
                        (skip-digits s (+ after-int 1) n))                 ; D+ . D*
                       ((and (not had-int) (< i0 n) (char=? (string-ref s i0) #\.))
                        (let ((k (skip-digits s (+ i0 1) n)))              ; . D+
                          (if (> k (+ i0 1)) k (no #f))))
                       (had-int after-int)
                       (else (no #f))))
                 ;; optional exponent
                 (je (if (and (< jm n) (let ((c (string-ref s jm))) (or (char=? c #\e) (char=? c #\E))))
                         (let* ((es (if (sign-at? s (+ jm 1) n) (+ jm 2) (+ jm 1)))
                                (ee (skip-digits s es n)))
                           (if (> ee es) ee (no #f)))
                         jm)))
            (= je n)))))))

(define (jolt-parse-double s)
  (if (not (string? s)) (error #f "parse-double requires a string" s)
      (cond
        ((string=? s "Infinity") +inf.0)
        ((string=? s "-Infinity") -inf.0)
        ((string=? s "NaN") +nan.0)
        ((parse-double-shape? s) (exact->inexact (string->number s)))
        (else jolt-nil))))

(def-var! "clojure.core" "__bit-and" jolt-bit-and)
(def-var! "clojure.core" "__bit-or" jolt-bit-or)
(def-var! "clojure.core" "__bit-xor" jolt-bit-xor)
(def-var! "clojure.core" "__bit-and-not" jolt-bit-and-not)
(def-var! "clojure.core" "bit-not" jolt-bit-not)
(def-var! "clojure.core" "bit-shift-left" jolt-bit-shift-left)
(def-var! "clojure.core" "bit-shift-right" jolt-bit-shift-right)
(def-var! "clojure.core" "bit-set" jolt-bit-set)
(def-var! "clojure.core" "bit-clear" jolt-bit-clear)
(def-var! "clojure.core" "bit-flip" jolt-bit-flip)
(def-var! "clojure.core" "bit-test" jolt-bit-test)
(def-var! "clojure.core" "unsigned-bit-shift-right" jolt-unsigned-bit-shift-right)
(def-var! "clojure.core" "parse-long" jolt-parse-long)
(def-var! "clojure.core" "parse-double" jolt-parse-double)
