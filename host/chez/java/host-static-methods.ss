;; host-static-methods.ss — the `Class/member` static surface: java.lang.Math,
;; System (properties/env), Thread, the Long/Integer/Double/Character/String static
;; methods, java.text.NumberFormat, and the Class registry. Registers into
;; host-static.ss's class-statics table (loaded just before this); instantiable host
;; object classes (ArrayList, StringBuilder, …) live in host-static-classes.ss.

;; ---- java.lang statics ------------------------------------------------------
;; java.lang.Math: sqrt/pow/floor/ceil/trig/log/exp always return a DOUBLE on the
;; JVM (Chez's sqrt/expt return EXACT for exact args, e.g. (sqrt 9) -> 3), so coerce
;; to flonum. round -> long (exact); abs/max/min preserve the argument's type.
(define (->dbl x) (exact->inexact x))
(define math-pi (acos -1.0))
;; sign-aware cube root: expt of a negative flonum to 1/3 goes complex.
(define (math-cbrt x)
  (let ((x (exact->inexact x)))
    (if (< x 0.0) (- (expt (- x) (/ 1.0 3.0))) (expt x (/ 1.0 3.0)))))
(register-class-statics! "Math"
  (list (cons "sqrt" (lambda (x) (->dbl (sqrt x))))
        (cons "cbrt" (lambda (x) (math-cbrt x)))
        (cons "pow" (lambda (a b) (->dbl (expt a b))))
        (cons "hypot" (lambda (a b) (->dbl (sqrt (+ (* a a) (* b b))))))
        (cons "floor" (lambda (x) (->dbl (floor x))))
        (cons "ceil" (lambda (x) (->dbl (ceiling x))))
        (cons "round" (lambda (x) (exact (floor (+ x 1/2)))))   ; JVM round-half-up -> long
        (cons "rint" (lambda (x) (->dbl (round x))))            ; round-half-even -> double
        ;; Math.floorDiv/floorMod: integer floor division / modulus (long -> long).
        (cons "floorDiv" (lambda (a b) (exact (floor (/ a b)))))
        (cons "floorMod" (lambda (a b) (exact (- a (* b (floor (/ a b)))))))
        (cons "abs" (lambda (x) (abs x)))
        (cons "sin" (lambda (x) (->dbl (sin x)))) (cons "cos" (lambda (x) (->dbl (cos x))))
        (cons "tan" (lambda (x) (->dbl (tan x)))) (cons "asin" (lambda (x) (->dbl (asin x))))
        (cons "acos" (lambda (x) (->dbl (acos x)))) (cons "atan" (lambda (x) (->dbl (atan x))))
        ;; Math.atan2(y, x) — Chez's 2-arg atan is (atan y x).
        (cons "atan2" (lambda (y x) (->dbl (atan y x))))
        (cons "sinh" (lambda (x) (->dbl (sinh x)))) (cons "cosh" (lambda (x) (->dbl (cosh x))))
        (cons "tanh" (lambda (x) (->dbl (tanh x))))
        (cons "log" (lambda (x) (->dbl (log x)))) (cons "log10" (lambda (x) (->dbl (/ (log x) (log 10)))))
        (cons "log1p" (lambda (x) (->dbl (log (+ 1.0 x)))))
        (cons "exp" (lambda (x) (->dbl (exp x))))
        (cons "expm1" (lambda (x) (->dbl (- (exp x) 1.0))))
        (cons "toRadians" (lambda (d) (->dbl (/ (* d math-pi) 180.0))))
        (cons "toDegrees" (lambda (r) (->dbl (/ (* r 180.0) math-pi))))
        (cons "copySign" (lambda (m s) (->dbl (if (< s 0.0) (- (abs m)) (abs m)))))
        ;; getExponent: the unbiased binary exponent of a double (floor(log2|x|));
        ;; scalb: x * 2^n. test.check's double generator uses both.
        (cons "getExponent" (lambda (x) (if (= x 0.0) -1023
                                            (exact (floor (/ (log (abs (exact->inexact x))) (log 2.0)))))))
        (cons "scalb" (lambda (x n) (->dbl (* (exact->inexact x) (expt 2.0 (jnum->exact n))))))
        (cons "max" (lambda (a b) (if (> a b) a b))) (cons "min" (lambda (a b) (if (< a b) a b)))
        (cons "signum" (lambda (x) (cond ((< x 0) -1.0) ((> x 0) 1.0) (else 0.0))))
        (cons "PI" (->dbl (* 4 (atan 1)))) (cons "E" (->dbl (exp 1)))
        (cons "random" (lambda args (random 1.0)))))

;; Thread: real OS threads back futures/promises.
;;  - sleep parks the calling thread for `ms` ms (a worker sleeping doesn't block
;;    the parent).
;;  - yield hands the CPU to another runnable thread (libc sched_yield).
;;  - each thread carries an interrupt flag; interrupted (static) reads AND clears
;;    the current thread's flag, matching the JVM. currentThread / .interrupt /
;;    .isInterrupted are wired in io.ss, where the thread handle is built.

;; Per-thread interrupt flag, lazily allocated so each OS thread gets its own box.
;; A thread handle (from currentThread) captures this box, so .interrupt from
;; another thread sets the target thread's flag.
(define thread-interrupt-box (make-thread-parameter #f))
(define (current-interrupt-box)
  (or (thread-interrupt-box)
      (let ((b (box #f))) (thread-interrupt-box b) b)))
(define (clear-thread-interrupt!) (set-box! (current-interrupt-box) #f))

;; libc sched_yield, resolved once; fall back to a zero-length park if the symbol
;; isn't available.
(define thread-yield!
  (let ((fp #f) (tried? #f))
    (lambda ()
      (unless tried?
        (set! tried? #t)
        (set! fp (jolt-foreign-proc-safe "sched_yield" '() 'int)))
      (if fp (fp) (sleep (make-time 'time-duration 0 0)))
      jolt-nil)))

(define thread-statics
  (list (cons "sleep" (lambda (ms . _)
                        (let* ((ms* (exact (floor ms)))
                               (secs (quotient ms* 1000))
                               (nanos (* (remainder ms* 1000) 1000000)))
                          (sleep (make-time 'time-duration nanos secs)))
                        jolt-nil))
        (cons "yield" (lambda _ (thread-yield!)))
        (cons "interrupted" (lambda _ (let* ((b (current-interrupt-box)) (v (unbox b)))
                                        (set-box! b #f) (and v #t))))))
(register-class-statics! "Thread" thread-statics)
(register-class-statics! "java.lang.Thread" thread-statics)

;; clojure.lang.LockingTransaction: jolt refs with serialized transactions.
;; isRunning -> true when a transaction is active on this thread.
(register-class-statics! "LockingTransaction" (list (cons "isRunning" (lambda () (and (*txn*) #t)))))
(register-class-statics! "clojure.lang.LockingTransaction" (list (cons "isRunning" (lambda () (and (*txn*) #t)))))

;; clojure.lang.LazilyPersistentVector/createOwning: build a vector from an array
;; (malli's -vmap fills an object-array then hands it over). jolt has no array
;; ownership transfer, so copy the array's elements into a persistent vector.
(define (lpv-create-owning arr) (apply jolt-vector (seq->list (jolt-seq arr))))
(register-class-statics! "LazilyPersistentVector" (list (cons "createOwning" lpv-create-owning)))
(register-class-statics! "clojure.lang.LazilyPersistentVector" (list (cons "createOwning" lpv-create-owning)))

;; clojure.lang.PersistentArrayMap/createWithCheck: build a map from a [k v k v…]
;; array, throwing on a duplicate key. malli's eager entry parser relies on the
;; throw to report ::duplicate-keys, so a missing class would mis-fire on every
;; map. Build the map and signal if a key collapsed (count*2 < array length).
(define (pam-create-with-check arr)
  (let ((items (seq->list (jolt-seq arr))))
    (let loop ((xs items) (m (jolt-hash-map)))
      (if (null? xs) m
          (if (null? (cdr xs)) (error #f "PersistentArrayMap: odd key/value count")
              (let ((k (car xs)))
                (if (jolt-contains? m k) (error #f "Duplicate key")
                    (loop (cddr xs) (jolt-assoc m k (cadr xs))))))))))
(register-class-statics! "PersistentArrayMap" (list (cons "createWithCheck" pam-create-with-check)))
(register-class-statics! "clojure.lang.PersistentArrayMap" (list (cons "createWithCheck" pam-create-with-check)))

;; clojure.lang.RT/map: build a map from a [k v k v…] array/seq (RT.map). Small
;; maps keep insertion order (PersistentArrayMap). tools.reader builds map and
;; namespaced-map literals this way.
(define (rt-map arr)
  (let loop ((xs (if (jolt-nil? arr) '() (seq->list (jolt-seq arr)))) (m (jolt-hash-map)))
    (cond ((null? xs) m)
          ((null? (cdr xs)) (error #f "RT/map: odd key/value count"))
          (else (loop (cddr xs) (jolt-assoc m (car xs) (cadr xs)))))))
(register-class-statics! "RT" (list (cons "map" rt-map)))
(register-class-statics! "clojure.lang.RT" (list (cons "map" rt-map)))

;; clojure.lang.PersistentList/create: a list (in order) from a seq; empty -> ().
(define (plist-create x)
  (let ((items (seq->list (jolt-seq x))))
    (if (null? items) jolt-empty-list (list->cseq items))))
(register-class-statics! "PersistentList" (list (cons "create" plist-create)))
(register-class-statics! "clojure.lang.PersistentList" (list (cons "create" plist-create)))

;; clojure.lang.PersistentHashSet/createWithCheck: a set from a seq, throwing on a
;; duplicate element (tools.reader's #{…} reader reports the dup).
(define (phs-create-with-check x)
  (let loop ((xs (seq->list (jolt-seq x))) (s (jolt-hash-set)))
    (if (null? xs) s
        (let ((e (car xs)))
          (if (jolt-truthy? (jolt-contains? s e))
              (jolt-throw (jolt-ex-info (string-append "Duplicate key: " (jolt-str-render-one e)) (jolt-hash-map)))
              (loop (cdr xs) (jolt-conj1 s e)))))))
(register-class-statics! "PersistentHashSet" (list (cons "createWithCheck" phs-create-with-check)))
(register-class-statics! "clojure.lang.PersistentHashSet" (list (cons "createWithCheck" phs-create-with-check)))

;; java.lang.Character statics. digit(ch, radix) -> the digit value or -1; ch may
;; be a char or an int codepoint (tools.reader passes (int c)). isDigit/
;; isWhitespace take a char; valueOf boxes a char (identity on jolt).
(define (char->cp x) (if (char? x) (char->integer x) (jnum->exact x)))
(define (char-digit-value cp radix)
  (let ((d (cond ((and (fx>=? cp 48) (fx<=? cp 57)) (fx- cp 48))            ; 0-9
                 ((and (fx>=? cp 97) (fx<=? cp 122)) (fx+ 10 (fx- cp 97)))  ; a-z
                 ((and (fx>=? cp 65) (fx<=? cp 90)) (fx+ 10 (fx- cp 65)))   ; A-Z
                 (else 99))))
    (if (fx<? d radix) d -1)))
(define character-statics
  (list (cons "digit" (lambda (ch radix) (->num (char-digit-value (char->cp ch) (jnum->exact radix)))))
        (cons "toChars" (lambda (cp) (na-char-array (jolt-vector (integer->char (char->cp cp))))))
        (cons "isDigit" (lambda (ch) (let ((cp (char->cp ch))) (and (fx>=? cp 48) (fx<=? cp 57)))))
        (cons "isWhitespace" (lambda (ch) (char-whitespace? (integer->char (char->cp ch)))))
        (cons "valueOf" (lambda (ch) (if (char? ch) ch (integer->char (char->cp ch)))))))
(register-class-statics! "Character" character-statics)
(register-class-statics! "java.lang.Character" character-statics)

;; java.util.regex.Pattern/compile: a regex value from a string pattern.
(define pattern-statics (list (cons "compile" (lambda (s) (jolt-regex (jolt-str-render-one s))))))
(register-class-statics! "Pattern" pattern-statics)
(register-class-statics! "java.util.regex.Pattern" pattern-statics)

;; clojure.lang.BigInt / clojure.lang.Numbers: jolt has one exact-integer type
;; (Chez bignums auto-reduce), so BigInt.fromBigInteger and Numbers.reduceBigInt
;; are identity. tools.reader's number parser threads integers through these.
(define identity-num-statics (list (cons "fromBigInteger" (lambda (x) x))))
(register-class-statics! "BigInt" identity-num-statics)
(register-class-statics! "clojure.lang.BigInt" identity-num-statics)
(register-class-statics! "Numbers"
  (list (cons "reduceBigInt" (lambda (x) x)) (cons "toRatio" (lambda (x) x))))
(register-class-statics! "clojure.lang.Numbers"
  (list (cons "reduceBigInt" (lambda (x) x)) (cons "toRatio" (lambda (x) x))))

(define (now-millis)
  (let ((t (current-time 'time-utc)))
    (+ (* 1000 (time-second t)) (quotient (time-nanosecond t) 1000000))))

;; clojure.core/current-time-ms — epoch milliseconds; backs the `time` macro.
(def-var! "clojure.core" "current-time-ms" (lambda () (->num (now-millis))))
(register-class-statics! "System"
  (list (cons "currentTimeMillis" (lambda () (->num (now-millis))))
        (cons "nanoTime" (lambda () (->num (* 1000000 (now-millis)))))
        (cons "exit" (lambda args (exit (if (null? args) 0 (jnum->exact (car args))))))
        ;; System/gc -> a full Chez collection (so weak references clear and their
        ;; guardians fire); Runtime.gc() routes here too.
        (cons "gc" (lambda _ (collect (collect-maximum-generation)) jolt-nil))
        ;; wrapped in lambdas: the helpers are defined below, resolved at call time.
        (cons "getProperty" (lambda (k . d) (apply sys-get-property k d)))
        (cons "setProperty" (lambda (k v) (sys-set-property k v)))
        (cons "clearProperty" (lambda (k) (sys-clear-property k)))
        (cons "getProperties" (lambda () (sys-properties-map)))
        (cons "getenv" (lambda k (apply sys-getenv k)))
        ;; System/console is nil when there is no attached terminal (piped /
        ;; redirected) — the safe default here; libraries (pretty) use it to
        ;; decide whether to emit ANSI, and a nil means "not a tty".
        (cons "console" (lambda _ jolt-nil))
        (cons "lineSeparator" (lambda _ "\n"))
        (cons "identityHashCode" (lambda (x) (->num (equal-hash x))))))
(register-class-statics! "java.lang.System"
  (list (cons "console" (lambda _ jolt-nil))
        (cons "lineSeparator" (lambda _ "\n"))))

;; java.lang.Long.bitCount: the population count of the value's 64-bit two's-
;; complement (mask to 64 bits so a negative long counts like the JVM, e.g.
;; bitCount(-1) = 64). test.check's splittable PRNG uses it.
(define long-mask64 #xFFFFFFFFFFFFFFFF)
(define long-2^63 (expt 2 63))
(define long-2^64 (expt 2 64))
;; interpret a 64-bit value as a signed long (top bit = sign), like the JVM.
(define (as-signed64 v) (if (>= v long-2^63) (- v long-2^64) v))
(define (long-nlz n) (- 64 (integer-length (bitwise-and (jnum->exact n) long-mask64))))
(define (long-reverse n)
  (let ((v (bitwise-and (jnum->exact n) long-mask64)))
    (let loop ((i 0) (r 0))
      (if (fx=? i 64) (as-signed64 r)
          (loop (fx+ i 1)
                (bitwise-ior (bitwise-arithmetic-shift-left r 1)
                             (bitwise-and (bitwise-arithmetic-shift-right v i) 1)))))))
(register-class-statics! "Long"
  (list (cons "TYPE" "long")
        (cons "MAX_VALUE" (->num 9223372036854775807))
        (cons "MIN_VALUE" (->num -9223372036854775808))
        (cons "bitCount" (lambda (n) (->num (bitwise-bit-count (bitwise-and (jnum->exact n) long-mask64)))))
        (cons "numberOfLeadingZeros" (lambda (n) (->num (long-nlz n))))
        (cons "reverse" (lambda (n) (->num (long-reverse n))))
        (cons "parseLong" (lambda (s . r) (parse-int-or-throw s (if (null? r) 10 (jnum->exact (car r))) "parseLong")))
        (cons "valueOf" (lambda (s . r) (parse-int-or-throw s (if (null? r) 10 (jnum->exact (car r))) "valueOf")))))

;; JVM Integer.toHexString/etc. treat the int as 32-bit unsigned.
(define (int->u32 n) (if (< n 0) (+ n 4294967296) n))
(register-class-statics! "Integer"
  (list (cons "MAX_VALUE" (->num 2147483647)) (cons "MIN_VALUE" (->num -2147483648))
        ;; the primitive class token (int.class); jolt models a class as its name
        (cons "TYPE" "int")
        (cons "valueOf" (lambda (x . r)
                          (if (number? x) (->num x)
                              (parse-int-or-throw x (if (null? r) 10 (jnum->exact (car r))) "valueOf"))))
        (cons "parseInt" (lambda (x . r) (parse-int-or-throw x (if (null? r) 10 (jnum->exact (car r))) "parseInt")))
        ;; lowercase, like the JVM; a negative int is the 32-bit unsigned form.
        (cons "toHexString" (lambda (x) (string-downcase (number->string (int->u32 (jnum->exact x)) 16))))
        (cons "toOctalString" (lambda (x) (number->string (int->u32 (jnum->exact x)) 8)))
        (cons "toBinaryString" (lambda (x) (number->string (int->u32 (jnum->exact x)) 2)))
        (cons "toString" (lambda (x . r) (number->string (jnum->exact x) (if (null? r) 10 (jnum->exact (car r))))))))

;; Byte / Short bounds (their values are plain integers on jolt; the statics let
;; libraries reference the JVM ranges — clojure.test.check generates over them).
(register-class-statics! "Byte"
  (list (cons "TYPE" "byte")
        (cons "MAX_VALUE" (->num 127)) (cons "MIN_VALUE" (->num -128))
        (cons "valueOf" (lambda (x . r) (->num (if (number? x) x (parse-int-or-throw x 10 "valueOf")))))
        (cons "parseByte" (lambda (x . r) (parse-int-or-throw x (if (null? r) 10 (jnum->exact (car r))) "parseByte")))
        ;; interpret the low 8 bits as unsigned (0..255): a signed byte -1 -> 255.
        (cons "toUnsignedLong" (lambda (x) (->num (bitwise-and (jnum->exact x) #xFF))))
        (cons "toUnsignedInt" (lambda (x) (->num (bitwise-and (jnum->exact x) #xFF))))
        (cons "toString" (lambda (x . r) (number->string (jnum->exact x))))))
(register-class-statics! "Short"
  (list (cons "TYPE" "short")
        (cons "MAX_VALUE" (->num 32767)) (cons "MIN_VALUE" (->num -32768))
        (cons "valueOf" (lambda (x . r) (->num (if (number? x) x (parse-int-or-throw x 10 "valueOf")))))
        (cons "parseShort" (lambda (x . r) (parse-int-or-throw x (if (null? r) 10 (jnum->exact (car r))) "parseShort")))
        (cons "toString" (lambda (x . r) (number->string (jnum->exact x))))))


(register-class-statics! "Boolean"
  (list (cons "TYPE" "boolean")
        (cons "parseBoolean" (lambda (s) (string=? "true" (ascii-string-down (if (string? s) s (jolt-str-render-one s))))))
        (cons "TRUE" #t) (cons "FALSE" #f)))

(register-class-ctor! "Double" ->double)
(register-class-ctor! "Float" ->double)
(register-class-statics! "Double"
  (list (cons "TYPE" "double")
        (cons "parseDouble" parse-double-or-throw)
        (cons "valueOf" ->double)
        (cons "toString" (lambda (x) (jolt-str-render-one (->double x))))
        (cons "isNaN" (lambda (x) (and (flonum? x) (nan? x))))
        (cons "isInfinite" (lambda (x) (and (flonum? x) (infinite? x))))
        (cons "MAX_VALUE" 1.7976931348623157e308) (cons "MIN_VALUE" 4.9e-324)
        (cons "POSITIVE_INFINITY" +inf.0) (cons "NEGATIVE_INFINITY" -inf.0) (cons "NaN" +nan.0)))
(register-class-statics! "Float"
  (list (cons "TYPE" "float")
        (cons "parseFloat" parse-double-or-throw) (cons "valueOf" ->double)))

;; Character: ASCII predicates (the engine is byte/ASCII oriented).
(register-class-statics! "Character"
  (list (cons "TYPE" "char")
        (cons "isUpperCase" (lambda (c) (let ((n (char-code c))) (and (>= n 65) (<= n 90)))))
        (cons "isLowerCase" (lambda (c) (let ((n (char-code c))) (and (>= n 97) (<= n 122)))))
        (cons "isDigit" (lambda (c) (let ((n (char-code c))) (and (>= n 48) (<= n 57)))))
        ;; JVM Character.isWhitespace: Unicode whitespace (so U+2028 line separator
        ;; counts, like the JVM) MINUS the no-break spaces the JVM excludes
        ;; (U+00A0/U+2007/U+202F). char<=?space missed everything above ASCII.
        (cons "isWhitespace" (lambda (c) (let ((cp (char-code c)))
                                           (and (char-whitespace? (integer->char cp))
                                                (not (fx=? cp #xA0)) (not (fx=? cp #x2007)) (not (fx=? cp #x202F))))))))

;; String/valueOf(Object): "null" for nil, else jolt's str semantics.
;; String/format(fmt args…) / (locale fmt args…) -> the clojure.core format engine.
(register-class-statics! "String"
  (list (cons "valueOf" (lambda (x . _) (if (jolt-nil? x) "null" (jolt-str-render-one x))))
        (cons "format" (lambda (a . rest)
                         (if (and (jhost? a) (string=? (jhost-tag a) "locale"))
                             (apply jolt-format (car rest) (cdr rest))
                             (apply jolt-format a rest))))))

;; ---- java.text.NumberFormat -------------------------------------------------
;; A grouping decimal formatter (selmer number-format / cuerdas). state:
;; #(grouping? min-frac max-frac). .format groups the integer part with commas.
(define (nf-make grouping? minf maxf) (make-jhost "numberformat" (vector grouping? minf maxf)))
(define (group-int-str s)               ; "1234567" -> "1,234,567"
  (let* ((neg (and (> (string-length s) 0) (char=? (string-ref s 0) #\-)))
         (digs (if neg (substring s 1 (string-length s)) s))
         (n (string-length digs)) (out '()))
    (let loop ((i 0))
      (when (< i n)
        (when (and (> i 0) (= 0 (modulo (- n i) 3))) (set! out (cons #\, out)))
        (set! out (cons (string-ref digs i) out)) (loop (+ i 1))))
    (string-append (if neg "-" "") (list->string (reverse out)))))
(define (nf-format self x)
  (let* ((grouping? (vector-ref (jhost-state self) 0))
         (minf (vector-ref (jhost-state self) 1)) (maxf (vector-ref (jhost-state self) 2))
         (neg (< x 0)) (ax (abs (exact->inexact x)))
         (scale (expt 10 maxf))
         (scaled (exact (round (* ax scale))))
         (ipart (quotient scaled scale)) (fpart (remainder scaled scale))
         (istr (number->string ipart))
         (fstr0 (if (> maxf 0) (let ((s (number->string fpart)))
                                 (string-append (make-string (max 0 (- maxf (string-length s))) #\0) s)) ""))
         ;; trim trailing zeros down to minf
         (fstr (let loop ((s fstr0)) (if (and (> (string-length s) minf)
                                              (char=? (string-ref s (- (string-length s) 1)) #\0))
                                         (loop (substring s 0 (- (string-length s) 1))) s))))
    (string-append (if neg "-" "") (if grouping? (group-int-str istr) istr)
                   (if (> (string-length fstr) 0) (string-append "." fstr) ""))))
(register-host-methods! "numberformat"
  (list (cons "format" (lambda (self n) (nf-format self n)))
        (cons "setMaximumFractionDigits" (lambda (self d) (vector-set! (jhost-state self) 2 (jnum->exact d)) jolt-nil))
        (cons "setMinimumFractionDigits" (lambda (self d) (vector-set! (jhost-state self) 1 (jnum->exact d)) jolt-nil))
        (cons "setGroupingUsed" (lambda (self b) (vector-set! (jhost-state self) 0 (jolt-truthy? b)) jolt-nil))))
(let ((nf-statics (list (cons "getInstance" (lambda _ (nf-make #t 0 3)))
                        (cons "getNumberInstance" (lambda _ (nf-make #t 0 3)))
                        (cons "getIntegerInstance" (lambda _ (nf-make #t 0 0))))))
  (register-class-statics! "NumberFormat" nf-statics)
  (register-class-statics! "java.text.NumberFormat" nf-statics))

;; Class.forName: an array descriptor ("[C") is its own class token; a class Jolt
;; can back (registered statics/ctor, or a java.*/clojure.* core class) yields a
;; class object; anything else throws a catchable ClassNotFoundException, like the
;; JVM — so the common `(try (Class/forName "optional.Dep") (catch …))` probe a
;; library uses to detect an absent dependency works (e.g. ring's joda-time check).
;; java.* / clojure.* packages jolt models in the class graph are known;
;; optional backends a library feature-probes with (Class/forName …) (e.g.
;; tools.logging's java.util.logging) are absent from the graph and get
;; a definitive ClassNotFoundException.
(define (forname-known? nm)
  ;; exact FQN lookups only — no suffix matching. The JVM throws
  ;; ClassNotFoundException for any unknown FQN even when the last segment
  ;; matches a known class (e.g. "com.acme.String" when "java.lang.String"
  ;; exists). jch-known? does last-segment matching and is NOT used here;
  ;; it lives on for jch-isa?'s suffix matching (round-6 territory).
  (or (hashtable-ref class-statics-tbl nm #f)
      (hashtable-ref class-ctors-tbl nm #f)
      (hashtable-ref jvm-class-parents nm #f)))
(register-class-statics! "Class"
  (list (cons "forName"
              (lambda (nm . _)
                (cond
                  ((and (> (string-length nm) 0) (char=? (string-ref nm 0) #\[)) nm)
                  ((forname-known? nm) (make-class-obj nm))
                  (else (jolt-throw (jolt-host-throwable "java.lang.ClassNotFoundException" nm))))))))

;; ---- System helpers (defined before use above via top-level order) ----------
;; os.name reflects the actual platform (Chez's machine-type names it): a *osx
;; machine is macOS, otherwise Linux. Code that branches on the OS (socket struct
;; layout, path handling) needs the truth, not a fixed value.
(define (substring-index needle hay)
  (let ((nl (string-length needle)) (hl (string-length hay)))
    (let loop ((i 0)) (cond ((> (+ i nl) hl) #f)
                            ((string=? (substring hay i (+ i nl)) needle) i)
                            (else (loop (+ i 1)))))))
(define sys-os-name
  (let ((m (symbol->string (machine-type))))
    (cond ((or (substring-index "osx" m) (substring-index "macos" m)) "Mac OS X")
          ((or (substring-index "nt" m) (substring-index "windows" m)) "Windows")
          (else "Linux"))))
;; runtime-settable system properties (System/setProperty). A set value wins over
;; the built-in defaults below; clearProperty removes it.
(define sys-prop-table (make-hashtable string-hash string=?))
(define (sys-set-property k v)
  (let ((prev (hashtable-ref sys-prop-table k jolt-nil)))
    (hashtable-set! sys-prop-table k (if (string? v) v (jolt-str-render-one v)))
    prev))
(define (sys-clear-property k)
  (let ((prev (hashtable-ref sys-prop-table k jolt-nil)))
    (hashtable-delete! sys-prop-table k) prev))
(define (sys-get-property k . dflt)
  (let ((set-val (hashtable-ref sys-prop-table k #f)))
    (cond (set-val set-val)
          ((string=? k "os.name") sys-os-name)
          ((string=? k "line.separator") "\n")
          ((string=? k "file.separator") "/")
          ((string=? k "path.separator") ":")
          ((string=? k "user.dir") (or (getenv "PWD") "."))
          ((string=? k "user.home") (or (getenv "HOME") ""))
          ((string=? k "java.io.tmpdir") (or (getenv "TMPDIR") "/tmp"))
          ((pair? dflt) (car dflt))
          (else jolt-nil))))
(define (sys-properties-map)
  (jolt-hash-map "os.name" sys-os-name "line.separator" "\n" "file.separator" "/"
                 "user.dir" (or (getenv "PWD") ".") "user.home" (or (getenv "HOME") "")
                 "java.io.tmpdir" (or (getenv "TMPDIR") "/tmp")))

;; full environment as an alist of (name . value), via spawning `env`.
(define (all-env-pairs)
  (call-with-values
    (lambda () (open-process-ports "env" (buffer-mode block) (native-transcoder)))
    (lambda (stdin stdout stderr pid)
      (let loop ((acc '()))
        (let ((l (get-line stdout)))
          (if (eof-object? l) (reverse acc)
              (let ((eq (let scan ((i 0)) (cond ((= i (string-length l)) #f)
                                                ((char=? (string-ref l i) #\=) i)
                                                (else (scan (+ i 1)))))))
                (loop (if eq (cons (cons (substring l 0 eq) (substring l (+ eq 1) (string-length l))) acc) acc)))))))))
;; JOLT_BAKE_ENV_ALLOWLIST: when set, only the listed comma-separated
;; names are served; unset (the normal case) reads are live and unfiltered.
(define (env-allowlist)
  (let ((a (getenv "JOLT_BAKE_ENV_ALLOWLIST")))
    (and a (map str-trim (str-literal-split a ",")))))
(define (sys-getenv . k)
  (let ((allow (env-allowlist)))
    (if (null? k)
        (apply jolt-hash-map
          (let loop ((ps (all-env-pairs)) (acc '()))
            (cond ((null? ps) (reverse acc))
                  ((and allow (not (member (caar ps) allow))) (loop (cdr ps) acc))
                  (else (loop (cdr ps) (cons (cdar ps) (cons (caar ps) acc)))))))
        (let ((name (car k)))
          (if (and allow (not (member name allow))) jolt-nil
              (let ((v (getenv name))) (if v v jolt-nil)))))))

;; ---- StringBuilder ----------------------------------------------------------
;; state: a box (1-vector) holding the accumulated string.
(define (sb-str self) (vector-ref (jhost-state self) 0))
(define (sb-set! self s) (vector-set! (jhost-state self) 0 s))
(define (render-piece x)
  (cond ((jolt-nil? x) "null") ((char? x) (string x)) ((string? x) x)
        (else (jolt-str-render-one x))))
;; (Object.) — a fresh value with distinct identity (libraries use it as a lock
;; or a unique sentinel). Each call returns a new jhost so identical?/= separate.
(register-class-ctor! "Object" (lambda _ (make-jhost "object" (vector))))

