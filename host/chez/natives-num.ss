;; bit ops + string->number parsers (jolt-cf1q.3 Phase 2 inc C) — host-coupled
;; seed natives (core_refs.janet bit family, core_io.janet parse-long/double) that
;; resolved to jolt-nil. jolt models every number as a double, so bit ops coerce
;; to an exact integer, operate, and return a flonum. parse-* match the seed's
;; strict shapes (Clojure 1.11: nil on malformed, throw on a non-string).

(define (->int x) (exact (truncate x)))
(define (jolt-bit-and a b)     (exact->inexact (bitwise-and (->int a) (->int b))))
(define (jolt-bit-or a b)      (exact->inexact (bitwise-ior (->int a) (->int b))))
(define (jolt-bit-xor a b)     (exact->inexact (bitwise-xor (->int a) (->int b))))
(define (jolt-bit-and-not a b) (exact->inexact (bitwise-and (->int a) (bitwise-not (->int b)))))
(define (jolt-bit-not a)       (exact->inexact (bitwise-not (->int a))))
(define (jolt-bit-shift-left x n)  (exact->inexact (bitwise-arithmetic-shift-left (->int x) (->int n))))
(define (jolt-bit-shift-right x n) (exact->inexact (bitwise-arithmetic-shift-right (->int x) (->int n))))
(define (bit-mask n) (bitwise-arithmetic-shift-left 1 (->int n)))
(define (jolt-bit-set x n)   (exact->inexact (bitwise-ior (->int x) (bit-mask n))))
(define (jolt-bit-clear x n) (exact->inexact (bitwise-and (->int x) (bitwise-not (bit-mask n)))))
(define (jolt-bit-flip x n)  (exact->inexact (bitwise-xor (->int x) (bit-mask n))))
(define (jolt-bit-test x n)  (not (zero? (bitwise-and (->int x) (bit-mask n)))))
;; unsigned-bit-shift-right: logical shift over 64-bit longs. For the common
;; non-negative operand it equals the arithmetic shift; the negative-operand
;; 64-bit-window case is not modeled (no fixed-width longs on the all-flonum side).
(define (jolt-unsigned-bit-shift-right x n)
  (exact->inexact (bitwise-arithmetic-shift-right (->int x) (->int n))))

;; ---- string->scalar parsers -------------------------------------------------
(define (ascii-digit? c) (and (char>=? c #\0) (char<=? c #\9)))
(define (skip-digits s i n) (let loop ((i i)) (if (and (< i n) (ascii-digit? (string-ref s i))) (loop (+ i 1)) i)))
(define (sign-at? s i n) (and (< i n) (let ((c (string-ref s i))) (or (char=? c #\+) (char=? c #\-)))))

(define (parse-long-shape? s)
  (let* ((n (string-length s)) (i0 (if (sign-at? s 0 n) 1 0)))
    (and (> n i0) (= (skip-digits s i0 n) n))))

(define (jolt-parse-long s)
  (if (not (string? s)) (error #f "parse-long requires a string" s)
      (if (parse-long-shape? s) (exact->inexact (string->number s)) jolt-nil)))

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
