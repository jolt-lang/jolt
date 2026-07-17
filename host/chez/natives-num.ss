;; bit ops + string->number parsers — host-coupled natives (bit family,
;; parse-long/double). jolt models every number as a double, so bit ops coerce
;; to an exact integer, operate, and return a flonum. parse-* use strict shapes
;; (Clojure 1.11: nil on malformed, throw on a non-string).

;; bit ops require a long operand. The JVM throws IllegalArgumentException for a
;; double, ratio, or an integer outside signed 64-bit range (a BigInt); ->int
;; enforces that. jolt's unified integer model can't distinguish (bigint 5) from
;; 5, so any exact integer in [Long/MIN, Long/MAX] is accepted; only non-integers
;; and out-of-range magnitudes are rejected (matching the JVM's "bit operation
;; not supported for" as closely as the model allows).
(define ->int-long-min -9223372036854775808)
(define ->int-long-max 9223372036854775807)
(define (->int x)
  (if (and (number? x) (exact? x) (integer? x)
           (>= x ->int-long-min) (<= x ->int-long-max))
      x
      (throw-jvm (quote IllegalArgumentException)
                 (string-append "bit operation not supported for: " (jolt-final-str x)))))
;; Mask shift count to low 6 bits (JVM long shift semantics), then wrap result
;; to 64-bit signed two's complement.
(define (shift-mask n) (bitwise-and (->int n) 63))
(define (wrap64 x)
  (let ((m (bitwise-and x #xFFFFFFFFFFFFFFFF)))
    (if (>= m #x8000000000000000) (- m #x10000000000000000) m)))
(define (jolt-bit-and a b)     (bitwise-and (->int a) (->int b)))
;; strict variadic twins (min arity 2, like clojure.core) — the backend emits
;; these when a bit op is a VALUE or called at a non-open-coded arity, so
;; (bit-and 5) raises like the JVM instead of hitting the identity of the raw
;; variadic Chez prim (jolt-mw44.52).
(define (jolt-bit-and* a b . more)
  (fold-left (lambda (acc x) (bitwise-and acc (->int x))) (jolt-bit-and a b) more))
(define (jolt-bit-or* a b . more)
  (fold-left (lambda (acc x) (bitwise-ior acc (->int x))) (jolt-bit-or a b) more))
(define (jolt-bit-xor* a b . more)
  (fold-left (lambda (acc x) (bitwise-xor acc (->int x))) (jolt-bit-xor a b) more))
(define (jolt-bit-or a b)      (bitwise-ior (->int a) (->int b)))
(define (jolt-bit-xor a b)     (bitwise-xor (->int a) (->int b)))
(define (jolt-bit-and-not a b) (bitwise-and (->int a) (bitwise-not (->int b))))
(define (jolt-bit-not a)       (bitwise-not (->int a)))
(define (jolt-bit-shift-left x n)  (wrap64 (bitwise-arithmetic-shift-left (->int x) (shift-mask n))))
(define (jolt-bit-shift-right x n) (wrap64 (bitwise-arithmetic-shift-right (->int x) (shift-mask n))))
(define (bit-mask n) (bitwise-arithmetic-shift-left 1 (->int n)))
;; set/flip can turn on bit 63, producing 2^63 (out of long range) — wrap to
;; 64-bit signed so (bit-set 0 63) is Long/MIN, like the JVM. clear only turns
;; bits off, so it stays in range.
(define (jolt-bit-set x n)   (wrap64 (bitwise-ior (->int x) (bit-mask n))))
(define (jolt-bit-clear x n) (bitwise-and (->int x) (bitwise-not (bit-mask n))))
(define (jolt-bit-flip x n)  (wrap64 (bitwise-xor (->int x) (bit-mask n))))
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
  (if (not (string? s)) (throw-jvm (quote IllegalArgumentException) (string-append "parse-long requires a string: " (jolt-final-str s)))
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

;; Double.parseDouble trims surrounding whitespace and accepts a trailing float/
;; double type suffix (1.5f / 1.5d / 1.5F / 1.5D). Strip both before the shape
;; check so parse-double matches the JVM on those forms.
(define (pd-ws? c) (or (char=? c #\space) (char=? c #\tab) (char=? c #\newline) (char=? c #\return)))
(define (pd-normalize s)
  (let* ((n (string-length s))
         (a (let loop ((i 0)) (if (and (< i n) (pd-ws? (string-ref s i))) (loop (+ i 1)) i)))
         (b (let loop ((j n)) (if (and (> j a) (pd-ws? (string-ref s (- j 1)))) (loop (- j 1)) j)))
         (t (substring s a b))
         (tn (string-length t)))
    ;; strip ONE trailing f/F/d/D suffix, but only when a digit precedes it
    (if (and (> tn 1)
             (let ((c (string-ref t (- tn 1)))) (memv c '(#\f #\F #\d #\D)))
             (char-numeric? (string-ref t (- tn 2))))
        (substring t 0 (- tn 1))
        t)))
(define (jolt-parse-double s)
  (if (not (string? s)) (throw-jvm (quote IllegalArgumentException) (string-append "parse-double requires a string: " (jolt-final-str s)))
      (let ((s (pd-normalize s)))
        (cond
          ((string=? s "Infinity") +inf.0)
          ((string=? s "-Infinity") -inf.0)
          ((string=? s "NaN") +nan.0)
          ((parse-double-shape? s) (exact->inexact (string->number s)))
          (else jolt-nil)))))

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
