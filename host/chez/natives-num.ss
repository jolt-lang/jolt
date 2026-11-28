;; bit ops + string->number parsers — host-coupled natives (bit family,
;; parse-long/double). Bit ops coerce to an exact integer, operate with 64-bit
;; wrapping, and return an exact integer (jolt's fixnum/bignum integer model,
;; per seq.ss). parse-* use strict shapes (Clojure 1.11: nil on malformed,
;; throw on a non-string).

;; bit ops require a long operand. The JVM throws IllegalArgumentException for a
;; double, ratio, or an integer outside signed 64-bit range (a BigInt); ->int
;; enforces that. jolt's unified integer model can't distinguish (bigint 5) from
;; 5, so any exact integer in [Long/MIN, Long/MAX] is accepted; only non-integers
;; and out-of-range magnitudes are rejected (matching the JVM's "bit operation
;; not supported for" as closely as the model allows).
(define ->int-long-min -9223372036854775808)
(define ->int-long-max 9223372036854775807)
;; A fixnum is already an exact integer inside signed 64-bit range — Chez's
;; fixnum width is 61 here, so it cannot reach ±2^63 — and it is what every
;; ordinary bit operation is handed. Test it first: the bounds are BIGNUMS on a
;; 61-bit tower, so the general path pays two fixnum-vs-bignum compares per
;; operand, four per (bit-xor a b). That was ~22ms per 1.28M bit ops, i.e. all
;; of the loop-recur benchmark's regression when the bit ops moved off the raw
;; Chez primitives onto these helpers. Same fix the int/long/unchecked-* casts
;; already carry.
;; The reference is Numbers.bitOpsCast, whose message is built from
;; x.getClass() — so it names the CLASS, not the value, and on nil the
;; getClass() call NPEs before the IllegalArgumentException is ever built.
;; Both are modelled here. Kept out of ->int's body so the hot path stays small.
;;
;; jolt's unified integer model has no separate bigint type to ask jolt-class-name
;; for, but it does not need one: ->int only rejects an exact integer when it is
;; outside signed 64-bit range, and a value the reference cannot hold in a long is
;; a clojure.lang.BigInt by construction. Every other operand kind answers through
;; jolt-class-name directly.
(define (->int-throw x)
  (if (jolt-nil? x)
      (jolt-throw (jolt-host-throwable "java.lang.NullPointerException"
                                       "bit operation not supported for: nil"))
      (throw-jvm (quote IllegalArgumentException)
                 (string-append "bit operation not supported for: class "
                                (if (and (number? x) (exact? x) (integer? x))
                                    "clojure.lang.BigInt"
                                    (jolt-class-name x))))))
(define (->int x)
  (if (fixnum? x)
      x
      (if (and (number? x) (exact? x) (integer? x)
               (>= x ->int-long-min) (<= x ->int-long-max))
          x
          (->int-throw x))))
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
;; wrap64 like set/flip: clearing bit 63 of a negative operand leaves the
;; infinite sign extension above bit 63 set, so (bit-clear Long/MIN_VALUE 63)
;; read -2^64 instead of 0 and (bit-clear -1 63) read -(2^63)-1 instead of
;; Long/MAX_VALUE. AND cannot set a bit its operands lack, but it can leave the
;; ones two's complement puts above the word.
(define (jolt-bit-clear x n) (wrap64 (bitwise-and (->int x) (bitwise-not (bit-mask n)))))
(define (jolt-bit-flip x n)  (wrap64 (bitwise-xor (->int x) (bit-mask n))))
(define (jolt-bit-test x n)  (not (zero? (bitwise-and (->int x) (bit-mask n)))))
;; unsigned-bit-shift-right: LOGICAL right shift over a 64-bit long (Java >>>),
;; so a negative operand shifts in zeros from its 64-bit two's-complement window
;; ((>>> -1 1) = 2^63-1), not the sign. The shift count is taken mod 64.
;; A zero shift preserves the signed input. A positive shift clears the
;; sign bit, so the result already fits a signed long.
(define (jolt-unsigned-bit-shift-right x n)
  (let ((v (->int x)) (count (shift-mask n)))
    (if (zero? count) v
        (bitwise-arithmetic-shift-right
          (if (negative? v) (+ v #x10000000000000000) v) count))))

;; ---- string->scalar parsers -------------------------------------------------
(define (ascii-digit? c) (and (char>=? c #\0) (char<=? c #\9)))
(define (skip-digits s i n) (let loop ((i i)) (if (and (< i n) (ascii-digit? (string-ref s i))) (loop (+ i 1)) i)))
(define (sign-at? s i n) (and (< i n) (let ((c (string-ref s i))) (or (char=? c #\+) (char=? c #\-)))))

;; ---- the Java integer grammar ------------------------------------------------
;; THE one place jolt reads a Java integer out of a string. Every java.lang
;; integer parser is this function at a different width -- Long/parseLong,
;; Integer/parseInt, Short/parseShort, Byte/parseByte, each one's valueOf, the
;; (Long. s) and (Integer. s) constructors, BigInteger unbounded -- and
;; clojure.core/parse-long is Long/valueOf with the throw caught to nil, so it is
;; this function too.
;;
;; It exists because the grammar is NOT Scheme's, and handing the string to
;; string->number spoke Scheme: (Long/parseLong "1e3") read the FLOAT 1000.0 out
;; of a method whose return type is long, "5.0" read 5.0, "#xff" / "#b101" /
;; "#o17" read Scheme radix prefixes whatever radix was asked for, " 5" parsed
;; because the string had been trimmed first, an out-of-range value came back as
;; a wider number instead of failing, and a radix outside 2..36 escaped as a Chez
;; "not a valid radix" condition. Java's grammar is only this: an optional + or
;; -, then one or more digits of RADIX, and nothing else.
;;
;; Answers the integer, or one of three SYMBOLS the caller renders as its own
;; failure -- 'radix (outside Character.MIN_RADIX..MAX_RADIX), 'shape (not a Java
;; integer in this radix), 'range (one that does not fit MN..MX). A parsed value
;; is never a symbol, so the two are told apart by symbol?. MN #f is the
;; unbounded parse.
(define (java-digit-value c radix)
  (let* ((i (char->integer c))
         (v (cond ((and (fx>=? i 48) (fx<=? i 57))  (fx- i 48))        ; 0-9
                  ((and (fx>=? i 97) (fx<=? i 122)) (fx+ 10 (fx- i 97)))  ; a-z
                  ((and (fx>=? i 65) (fx<=? i 90))  (fx+ 10 (fx- i 65)))  ; A-Z
                  (else #f))))
    (and v (fx<? v radix) v)))

(define (java-int-parse s radix mn mx)
  (if (or (fx<? radix 2) (fx>? radix 36))
      (quote radix)
      (let* ((n (string-length s))
             (c0 (and (fx>? n 0) (string-ref s 0)))
             (neg? (eqv? c0 #\-))
             (i0 (if (or neg? (eqv? c0 #\+)) 1 0)))
        (if (fx=? i0 n)
            (quote shape)                       ; "", "+", "-" -- no digits
            (let loop ((i i0) (acc 0))
              (if (fx=? i n)
                  (let ((v (if neg? (- acc) acc)))
                    (if (or (not mn) (and (>= v mn) (<= v mx))) v (quote range)))
                  (let ((d (java-digit-value (string-ref s i) radix)))
                    (if d (loop (fx+ i 1) (+ (* acc radix) d)) (quote shape)))))))))

;; clojure.core/parse-long: Long/valueOf with NumberFormatException caught to
;; nil, so a non-nil result is always a LONG and every failure -- bad shape or a
;; decimal outside signed 64-bit range -- is nil rather than a wider number.
;; (parse-long "9223372036854775808") read 9223372036854775808N before the range
;; half existed, so a caller branching on the nil to catch the overflow took the
;; wrong arm and got a clojure.lang.BigInt out of a fn documented to return Long.
(define (jolt-parse-long s)
  (if (not (string? s)) (throw-jvm (quote IllegalArgumentException) (string-append "parse-long requires a string: " (jolt-final-str s)))
      (let ((v (java-int-parse s 10 ->int-long-min ->int-long-max)))
        (if (symbol? v) jolt-nil v))))

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
;; the trim is String.trim(): every character at or below U+0020, not just the
;; four common blanks (a form feed or a NUL ahead of the digits is trimmed too)
(define (pd-ws? c) (char<=? c #\space))
(define (pd-normalize s)
  (let* ((n (string-length s))
         (a (let loop ((i 0)) (if (and (< i n) (pd-ws? (string-ref s i))) (loop (+ i 1)) i)))
         (b (let loop ((j n)) (if (and (> j a) (pd-ws? (string-ref s (- j 1)))) (loop (- j 1)) j)))
         (t (substring s a b))
         (tn (string-length t)))
    ;; strip ONE trailing f/F/d/D suffix, but only after a digit or a point:
    ;; "1.f" is 1.0 on the JVM, "f" and "xf" are not numbers (and ".f", left as
    ;; ".", fails the shape check below as it should)
    (if (and (> tn 1)
             (let ((c (string-ref t (- tn 1)))) (memv c '(#\f #\F #\d #\D)))
             (let ((b (string-ref t (- tn 2)))) (or (char-numeric? b) (char=? b #\.))))
        (substring t 0 (- tn 1))
        t)))
;; Java's HEXADECIMAL floating-point form, the one shape of Double.parseDouble
;; that is not a decimal: 0x HexDigits . HexDigitsopt p Signopt Digits. The
;; binary exponent is REQUIRED, which is what tells (Double/parseDouble "0x1fp0")
;; -- 31.0 -- from "0x1f", which is not a double on the JVM either. #f when the
;; string is not one.
(define (hex-digit-value c)
  (let ((i (char->integer c)))
    (cond ((and (fx>=? i 48) (fx<=? i 57))  (fx- i 48))
          ((and (fx>=? i 97) (fx<=? i 102)) (fx+ 10 (fx- i 97)))
          ((and (fx>=? i 65) (fx<=? i 70))  (fx+ 10 (fx- i 65)))
          (else #f))))
(define (skip-hex-digits s i n)
  (let loop ((i i)) (if (and (< i n) (hex-digit-value (string-ref s i))) (loop (+ i 1)) i)))
(define (hex-digits-value s i j)
  (let loop ((i i) (acc 0))
    (if (= i j) acc (loop (+ i 1) (+ (* acc 16) (hex-digit-value (string-ref s i)))))))

(define (java-hex-double s)
  (let* ((n (string-length s))
         (neg? (and (> n 0) (char=? (string-ref s 0) #\-)))
         (i0 (if (or neg? (and (> n 0) (char=? (string-ref s 0) #\+))) 1 0)))
    (and (<= (+ i0 2) n)
         (char=? (string-ref s i0) #\0)
         (memv (string-ref s (+ i0 1)) (quote (#\x #\X)))
         (let* ((ds (+ i0 2))
                (ip (skip-hex-digits s ds n))
                (dot? (and (< ip n) (char=? (string-ref s ip) #\.)))
                (fs (if dot? (+ ip 1) ip))
                (fp (if dot? (skip-hex-digits s fs n) fs)))
           (and (or (> ip ds) (> fp fs))        ; at least one hex digit, either side of the point
                (< fp n)
                (memv (string-ref s fp) (quote (#\p #\P)))
                (let* ((es (if (sign-at? s (+ fp 1) n) (+ fp 2) (+ fp 1)))
                       (ee (skip-digits s es n)))
                  (and (> ee es) (= ee n)
                       (let* ((mant (+ (hex-digits-value s ds ip)
                                       (if (> fp fs)
                                           (/ (hex-digits-value s fs fp) (expt 16 (- fp fs)))
                                           0)))
                              (ex (string->number (substring s (+ fp 1) n)))
                              (v (exact->inexact (* mant (expt 2 ex)))))
                         (if neg? (- v) v)))))))))

;; THE one place jolt reads a Java double out of a string, the floating half of
;; java-int-parse. clojure.core/parse-double is Double/parseDouble with the throw
;; caught to nil, so both doors are this function -- the Java one used to hand
;; the string to string->number instead and so spoke SCHEME, reading
;; (Double/parseDouble "#xff") as 255.0 and "1/2" as 0.5, neither of which is a
;; double on the JVM; and both doors were missing the hex form, "-NaN" and
;; "+Infinity". #f when the string is not a Java double.
(define (java-double-parse s0)
  (let* ((s (pd-normalize s0))
         (n (string-length s))
         (neg? (and (> n 0) (char=? (string-ref s 0) #\-)))
         (body (if (and (> n 0) (memv (string-ref s 0) (quote (#\- #\+)))) (substring s 1 n) s)))
    (cond
      ;; the sign is accepted and IGNORED on NaN, as the JVM does: "-NaN" is NaN
      ((string=? body "NaN") +nan.0)
      ((string=? body "Infinity") (if neg? -inf.0 +inf.0))
      ((java-hex-double s))
      ((parse-double-shape? s) (exact->inexact (string->number s)))
      (else #f))))

(define (jolt-parse-double s)
  (if (not (string? s)) (throw-jvm (quote IllegalArgumentException) (string-append "parse-double requires a string: " (jolt-final-str s)))
      (or (java-double-parse s) jolt-nil)))

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
