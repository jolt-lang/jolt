;; natives-format.ss — the %-format engine for clojure.core `format`, over the
;; all-flonum number model. java.util.Formatter's grammar
;; (%[index$][flags][width][.prec]conv) and its conversions: %d %x %X %o (integer,
;; the radix ones unsigned), %f %e %E %g %G (decimal float), %a %A (hexadecimal
;; float), %s %S (str), %b %B (boolean), %h %H (hashcode hex), %c (char), %t %T
;; (date-time, rendered by the java.util date layer through set-format-datetime!),
;; %% and %n. A flag or precision a conversion cannot take is the JVM's refusal,
;; not a silent drop. Loaded after natives-misc.ss (uses jolt-str-render-one via
;; converters + jolt-truthy?).

(define (->long x) (exact (truncate x)))

;; Guard for the conversions that need a number. The JVM renders a nil argument as
;; "null" whatever the conversion, and rejects one it cannot take with
;; IllegalFormatConversionException, whose message names the conversion and the
;; argument's class ("d != java.lang.String"). Without this the argument reached a
;; Chez numeric primitive and the raw condition escaped with no class, so no catch
;; clause could select it. %c also takes a char.
(define (fmt-numeric d a f)
  (cond ((jolt-nil? a) "null")
        ((or (number? a) (and (char? a) (char=? d #\c))) (f a))
        (else (jolt-throw (jolt-host-throwable "java.util.IllegalFormatConversionException"
                (string-append (string d) " != " (jolt-class-name a)))))))
;; %x / %X / %o are UNSIGNED conversions on the JVM: a negative argument prints the
;; two's complement of its integer type, not a signed magnitude ("%x" -1 was "-1"
;; here, which is wrong under any width). The WIDTH is the argument's Java type —
;; Byte 8 bits, Short 16, Integer 32, Long 64 — and jolt unifies every integer as
;; one type, so it cannot read the width off the argument and one of the two ends
;; must diverge. jolt takes the NARROWEST width that holds the value, which is the
;; JVM's answer whenever the value's origin type is the narrowest that holds it:
;; a byte out of a byte[] prints two digits and an int-sized hash prints eight,
;; so the hex-dump and percent-encode idioms match the JVM unmasked. The cost is a
;; long whose value fits narrower — (format "%x" (long -1)) is "ff" here and
;; "ffffffffffffffff" on the JVM — allowlisted under :integer-box-model. Masking
;; ((bit-and b 0xff)) pins the width explicitly and is identical on both.
;; A value outside the signed-64 range has no fixed-width two's complement to print
;; — the JVM would be holding a BigInteger there, whose %x is a signed magnitude
;; with a leading minus — so that is what it gets. Masking it to 64 bits, as the
;; width search would, silently rendered -(2^70) as "0".
(define (fmt-radix v radix)
  (let ((n (->long v)))
    (cond
      ((>= n 0) (number->string n radix))
      ((< n (- (expt 2 63))) (string-append "-" (number->string (- n) radix)))
      (else
       (let loop ((bits 8))
         (if (>= n (- (expt 2 (- bits 1))))
             (number->string (bitwise-and n (- (expt 2 bits) 1)) radix)
             (loop (fx* bits 2))))))))
(define (pad-left s n c) (if (fx>=? (string-length s) n) s (string-append (make-string (fx- n (string-length s)) c) s)))
;; The decimal separator %f renders. "." everywhere except inside a
;; String.format(Locale, …) call, which binds this to the locale's separator —
;; the JVM formats 123.045 as "123,045" under de. Bound rather than passed so
;; every directive path picks it up without threading an argument through.
(define format-decimal-sep (make-parameter "."))
;; --- the decimal digits of a number ---------------------------------------------
;; (num-digits x) -> (values digits expo) for a finite x >= 0: digits are the
;; significant decimal digits with no leading or trailing zeros ("0" for zero),
;; and x = 0.DIGITS x 10^expo. A flonum's digits are Chez's shortest round-trip
;; print, which is the digit string java.util.Formatter rounds too: it formats
;; from FloatingDecimal's digits, not from the binary value, so 1.005 rounds to
;; 1.01 at two places where the binary value 1.00499… would round down, and
;; (format "%.20f" 0.1) pads zeros rather than printing the expansion. A
;; BigDecimal's digits are its unscaled value against its scale, exact.
(define (str-index s c)
  (let loop ((i 0))
    (cond ((fx=? i (string-length s)) #f) ((char=? (string-ref s i) c) i) (else (loop (fx+ i 1))))))
(define (digits-normalize raw expo)      ; drop leading zeros (each lowers expo) and trailing ones
  (let lead ((i 0))
    (if (and (fx<? i (string-length raw)) (char=? (string-ref raw i) #\0))
        (lead (fx+ i 1))
        (let trail ((j (string-length raw)))
          (cond ((fx=? j i) (values "0" 1))
                ((char=? (string-ref raw (fx- j 1)) #\0) (trail (fx- j 1)))
                (else (values (substring raw i j) (- expo i))))))))
(define (num-digits x)
  (if (jbigdec? x)
      (let ((u (number->string (abs (jbigdec-unscaled x)))))
        (digits-normalize u (- (string-length u) (jbigdec-scale x))))
      (let* ((s (number->string (inexact x)))   ; "123.45" | "1e100" | "1.5e-7" | "123456789.0"
             ;; a denormal prints with its bit count after a bar ("1e-320|11"):
             ;; cut there before splitting the exponent off
             (bar (or (str-index s #\|) (string-length s)))
             (s (substring s 0 bar))
             (epos (or (str-index s #\e) (string-length s)))
             (mant (substring s 0 epos))
             (e10 (if (fx<? epos (string-length s))
                      (string->number (substring s (fx+ epos 1) (string-length s)))
                      0))
             (dot (str-index mant #\.))
             (int-part (if dot (substring mant 0 dot) mant))
             (frac-part (if dot (substring mant (fx+ dot 1) (string-length mant)) "")))
        (digits-normalize (string-append int-part frac-part) (+ e10 (string-length int-part))))))
;; (digits-round digits n) -> (cons digits* carry?): the first n digits, rounded
;; half up on the one after them -- java.util.Formatter's applyPrecision looks
;; at exactly that one digit. carry? says the round went past the top digit
;; (99.5 -> 100): digits* is then "1" and the caller's exponent grows by one.
(define (digits-round digits n)
  (let ((len (string-length digits)))
    (cond
      ((< n 0) (cons "" #f))
      ((>= n len) (cons digits #f))
      ((char<? (string-ref digits n) #\5) (cons (substring digits 0 n) #f))
      (else
       (let ((s (number->string (+ 1 (if (fx=? n 0) 0 (string->number (substring digits 0 n)))))))
         (if (fx>? (string-length s) n) (cons "1" #t) (cons s #f)))))))
;; the k-th digit (0-based from the top) of a digit string, 0 past either end
(define (digit-at d k)
  (if (and (fx>=? k 0) (fx<? k (string-length d))) (string-ref d k) #\0))
;; %f: prec fraction digits. The digits kept are the expo integer ones plus prec.
(define (fmt-fixed digits expo prec)
  (let* ((r (digits-round digits (+ expo prec)))
         (d (car r)) (expo (if (cdr r) (+ expo 1) expo))
         (int (if (<= expo 0)
                  "0"
                  (let ((s (make-string expo)))
                    (do ((k 0 (fx+ k 1))) ((fx=? k expo) s) (string-set! s k (digit-at d k))))))
         (frac (let ((s (make-string prec)))
                 (do ((i 0 (fx+ i 1))) ((fx=? i prec) s) (string-set! s i (digit-at d (+ expo i)))))))
    (if (fx>? prec 0) (string-append int (format-decimal-sep) frac) int)))
;; %e: d.dddddde+xx, prec fraction digits, the exponent at least two digits with
;; a sign. prec+1 significant digits are kept.
(define (fmt-sci digits expo prec)
  (let* ((r (digits-round digits (fx+ prec 1)))
         (d (car r)) (expo (if (cdr r) (+ expo 1) expo))
         (e (- expo 1))
         (frac (let ((s (make-string prec)))
                 (do ((i 0 (fx+ i 1))) ((fx=? i prec) s) (string-set! s i (digit-at d (fx+ i 1))))))
         (es (number->string (abs e))))
    (string-append (string (digit-at d 0))
                   (if (fx>? prec 0) (string-append (format-decimal-sep) frac) "")
                   "e" (if (< e 0) "-" "+")
                   (if (fx<? (string-length es) 2) (string-append "0" es) es))))
;; %g: prec significant digits (a 0 reads as 1); fixed notation when the ROUNDED
;; value is in [10^-4, 10^prec), scientific otherwise -- Java's rule, which is
;; why (format "%.3g" 1234.5) is 1.23e+03 and 0.00001234 is 1.23400e-05. The
;; digits are rounded once, here; the notation only lays them out.
(define (fmt-general digits expo prec)
  (let* ((prec (if (fx=? prec 0) 1 prec))
         (r (digits-round digits prec))
         (d (car r)) (expo (if (cdr r) (+ expo 1) expo))
         (e (- expo 1)))
    (if (and (>= e -4) (< e prec))
        (fmt-fixed d expo (fx- prec (fx+ e 1)))
        (fmt-sci d expo (fx- prec 1)))))
;; digit grouping for the , flag: the integer part in threes, the fraction as is
(define (fmt-group s)
  (let* ((sep (format-decimal-sep))
         (dot (str-index s (string-ref sep 0)))
         (int (if dot (substring s 0 dot) s))
         (rest (if dot (substring s dot (string-length s)) "")))
    (let loop ((i (string-length int)) (acc '()))
      (if (fx<=? i 3)
          (apply string-append (substring int 0 i) (append acc (list rest)))
          (loop (fx- i 3) (cons (string-append "," (substring int (fx- i 3) i)) acc))))))

;; --- %a: the hexadecimal-float conversion ----------------------------------------
;; With NO precision %a is Double.toHexString's spelling: 0x1.<13 hex mantissa
;; digits>p<exponent> with trailing zeros dropped (one digit always kept), and a
;; SUBNORMAL left denormalized as 0x0.<digits>p-1022.
;;
;; With a precision it is exactly p hex fraction digits, always NORMALIZED to a
;; leading 0x1. (so a subnormal gains an exponent below -1022: %.1a of
;; Double/MIN_VALUE is 0x1.0p-1074, not 0x0.0000000000001p-1022) and zero-padded
;; out past the 13 digits a double can hold. The significand rounds HALF TO EVEN
;; at 1+4p bits, which is what java.util.Formatter does — 0x1.28p0 at one digit
;; is 0x1.2p0 and 0x1.38p0 is 0x1.4p0 — and a round that carries past the leading
;; digit raises the exponent (0x1.f81p0 -> 0x1.0p1).
;;
;; Both read the flonum's EXACT rational value (its denominator is a power of
;; two), so there is no bit twiddling and a denormal needs no special case.
(define (fmt-hex-digits v n)             ; the integer v as n hex digits, zero-padded
  ;; Chez spells hex digits upper case; %a is the lower-case conversion and %A
  ;; upcases the whole rendering.
  (let ((s (string-downcase (number->string v 16))))
    (string-append (make-string (max 0 (- n (string-length s))) #\0) s)))
(define (fmt-hex-strip s)                ; drop trailing zeros, keep one digit
  (let loop ((j (string-length s)))
    (cond ((fx<=? j 1) (substring s 0 1))
          ((char=? (string-ref s (fx- j 1)) #\0) (loop (fx- j 1)))
          (else (substring s 0 j)))))
(define (fmt-hex-float x prec)
  (let ((r (exact (abs x))))
    (if (= r 0)
        (if prec
            (string-append "0x0." (make-string (max 1 prec) #\0) "p0")
            "0x0.0p0")
        ;; e = floor(log2 r), so v = r/2^e is in [1,2)
        (let loop ((e 0) (v r))
          (cond
            ((>= v 2) (loop (+ e 1) (/ v 2)))
            ((< v 1) (loop (- e 1) (* v 2)))
            (prec
             (let* ((p (max 1 prec))
                    (unit (expt 16 p))
                    (scaled (* v unit))
                    (q (floor scaled))
                    (rem (- scaled q))
                    (m (cond ((> rem 1/2) (+ q 1))
                             ((< rem 1/2) q)
                             ((even? q) q)
                             (else (+ q 1))))
                    (carry? (>= m (* 2 unit)))
                    (m (if carry? unit m))
                    (e (if carry? (+ e 1) e)))
               (string-append "0x1." (fmt-hex-digits (- m unit) p) "p" (number->string e))))
            ((>= e -1022)                ; normal: 1.<52 stored mantissa bits>
             (string-append "0x1." (fmt-hex-strip (fmt-hex-digits (* (- v 1) (expt 2 52)) 13))
                            "p" (number->string e)))
            (else                        ; subnormal: 0.<the 52 bits>p-1022
             (string-append "0x0." (fmt-hex-strip (fmt-hex-digits (* r (expt 2 1074)) 13))
                            "p-1022")))))))
;; %a's 0 flag pads AFTER the "0x" prefix, where the JVM puts it ("0x01.0p0"),
;; and its ( and , flags are refused outright (checked by the flag table), so it
;; cannot share fmt-sign-pad.
(define (fmt-hex-float-pad neg? body flags width)
  (let* ((prefix (cond (neg? "-")
                       ((fmt-flag? flags #\+) "+")
                       ((fmt-flag? flags #\space) " ")
                       (else "")))
         (n (fx+ (string-length prefix) (string-length body))))
    (cond ((or (not width) (fx>=? n width)) (string-append prefix body))
          ((fmt-flag? flags #\-) (string-append prefix body (make-string (fx- width n) #\space)))
          ((fmt-flag? flags #\0)
           (string-append prefix "0x" (make-string (fx- width n) #\0)
                          (substring body 2 (string-length body))))
          (else (string-append (make-string (fx- width n) #\space) prefix body)))))

;; --- one conversion --------------------------------------------------------------
;; A signed numeric conversion renders (vector neg? magnitude zero-pad?) and then
;; shares the sign, the grouping and the padding with every other one: the sign
;; is "-" (or "(…)" under the ( flag), "+" under +, " " under space; a 0 flag
;; pads with zeros AFTER the sign, as the JVM does ("-0003.00", not "000-3.00");
;; NaN and Infinity take the sign but never the zeros.
(define (fmt-flag? flags c) (and (memv c flags) #t))
(define (fmt-sign-pad neg? body flags width zero-ok?)
  (let* ((prefix (cond (neg? (if (fmt-flag? flags #\() "(" "-"))
                       ((fmt-flag? flags #\+) "+")
                       ((fmt-flag? flags #\space) " ")
                       (else "")))
         (suffix (if (and neg? (fmt-flag? flags #\()) ")" ""))
         (n (fx+ (string-length prefix) (fx+ (string-length body) (string-length suffix)))))
    (cond ((or (not width) (fx>=? n width)) (string-append prefix body suffix))
          ((fmt-flag? flags #\-) (string-append prefix body suffix (make-string (fx- width n) #\space)))
          ((and zero-ok? (fmt-flag? flags #\0)) (string-append prefix (make-string (fx- width n) #\0) body suffix))
          (else (string-append (make-string (fx- width n) #\space) prefix body suffix)))))
;; pad to width: left-justify with spaces, else right-justify (zero-pad only
;; where the caller says the conversion takes it)
(define (fmt-pad s flags width zero-ok?)
  (if (and width (fx<? (string-length s) width))
      (let ((p (fx- width (string-length s))))
        (cond ((fmt-flag? flags #\-) (string-append s (make-string p #\space)))
              (else (string-append (make-string p (if (and zero-ok? (fmt-flag? flags #\0)) #\0 #\space)) s))))
      s))

;; --- the conditions java.util.Formatter raises -----------------------------------
;; Every one is a java.util.IllegalFormatException (class-hierarchy.ss), so a
;; (catch IllegalFormatException …) over a bad format string selects all of them
;; the way it does on the JVM.
(define (fmt-jvm-throw cls msg) (jolt-throw (jolt-host-throwable cls msg)))
(define (fmt-unknown-conversion s)
  (fmt-jvm-throw "java.util.UnknownFormatConversionException"
                 (string-append "Conversion = '" s "'")))
(define (fmt-conversion-throw d a)
  (fmt-jvm-throw "java.util.IllegalFormatConversionException"
                 (string-append (string d) " != " (jolt-class-name a))))
(define (fmt-precision-throw p)
  (fmt-jvm-throw "java.util.IllegalFormatPrecisionException" (number->string p)))
(define (fmt-width-throw w)
  (fmt-jvm-throw "java.util.IllegalFormatWidthException" (number->string w)))
(define (fmt-flag-mismatch d c)
  (fmt-jvm-throw "java.util.FormatFlagsConversionMismatchException"
                 (string-append "Conversion = " (string d) ", Flags = " (string c))))
(define (fmt-missing-arg spec)
  (fmt-jvm-throw "java.util.MissingFormatArgumentException"
                 (string-append "Format specifier '" spec "'")))
(define (fmt-arg-index-throw n)
  (fmt-jvm-throw "java.util.IllegalFormatArgumentIndexException"
                 (string-append "Illegal format argument index = " (number->string n))))

;; --- which flags and precision each conversion takes ------------------------------
;; The JVM rejects a flag a conversion cannot use rather than ignoring it, and
;; the refusal is part of the contract a caller catches: %#d and %,g are
;; FormatFlagsConversionMismatchException, %.2d is IllegalFormatPrecisionException.
;; Silently dropping them let a typo'd format string render, which is worse than
;; the throw — a %,d that meant %,f printed ungrouped and looked fine.
;;
;; # (alternate form) prefixes a radix conversion and is otherwise a no-op on the
;; float conversions; , groups the integer part; ( parenthesizes a negative.
(define (fmt-alt-ok? d) (and (memv d '(#\x #\X #\o #\a #\A #\e #\E #\f)) #t))
(define (fmt-group-ok? d) (and (memv d '(#\d #\f #\g #\G)) #t))
(define (fmt-paren-ok? d) (and (memv d '(#\d #\o #\x #\X #\e #\E #\f #\g #\G)) #t))
;; + and space force a sign, so they are the SIGNED conversions only: %x %X %o
;; are unsigned and refuse them, as do the general ones.
(define (fmt-sign-ok? d) (and (memv d '(#\d #\e #\E #\f #\g #\G #\a #\A)) #t))
;; Precision means fraction/significant digits on the float conversions and
;; TRUNCATION on the general ones (%.3s of "abcdef" is "abc"); the integer, char
;; and date conversions refuse it.
(define (fmt-prec-ok? d) (and (memv d '(#\s #\S #\b #\B #\h #\H #\e #\E #\f #\g #\G #\a #\A)) #t))
;; A DATE conversion takes only - and a width: java.util.Formatter's checkDateTime
;; refuses a precision outright and rejects # + space 0 , and ( — and its message
;; names the SUB-conversion (%#tF is "Conversion = F"), not the t.
(define (fmt-check-date-flags sub flags prec)
  (for-each (lambda (c) (when (fmt-flag? flags c) (fmt-flag-mismatch sub c)))
            '(#\# #\+ #\space #\0 #\, #\())
  (when prec (fmt-precision-throw prec)))
(define (fmt-check-flags d flags width prec)
  (when (and (fmt-flag? flags #\#) (not (fmt-alt-ok? d))) (fmt-flag-mismatch d #\#))
  (when (and (fmt-flag? flags #\,) (not (fmt-group-ok? d))) (fmt-flag-mismatch d #\,))
  (when (and (fmt-flag? flags #\() (not (fmt-paren-ok? d))) (fmt-flag-mismatch d #\())
  (when (and (fmt-flag? flags #\+) (not (fmt-sign-ok? d))) (fmt-flag-mismatch d #\+))
  (when (and (fmt-flag? flags #\space) (not (fmt-sign-ok? d))) (fmt-flag-mismatch d #\space))
  (when (and prec (not (fmt-prec-ok? d))) (fmt-precision-throw prec))
  (when (and width (memv d '(#\n))) (fmt-width-throw width)))
;; the # flag's radix prefix, empty when the flag is absent
(define (fmt-alt-prefix d flags)
  (if (fmt-flag? flags #\#)
      (case d ((#\x) "0x") ((#\X) "0X") ((#\o) "0") (else ""))
      ""))
;; precision TRUNCATES a general conversion's rendering
(define (fmt-truncate s prec)
  (if (and prec (fx<? prec (string-length s))) (substring s 0 prec) s))

;; %h is Integer.toHexString(arg.hashCode()) — the argument's JAVA hashCode (not
;; its Clojure hasheq, which is a different number for a string) as UNSIGNED
;; 32-bit hex, and "null" for nil. It reads that off the same .hashCode dispatch
;; a (.hashCode x) call takes, so the two always agree; where jolt's hashCode
;; itself diverges from the JVM's (a double, under the all-flonum number model)
;; %h carries that same divergence and no new one. Chez spells hex digits upper
;; case and %h is the lower-case conversion.
(define (fmt-hash a)
  (if (jolt-nil? a)
      "null"
      (string-downcase
       (number->string (bitwise-and (->long (record-method-dispatch a "hashCode" jolt-nil))
                                    #xffffffff)
                       16))))

;; %t / %T render the fields of a date-time value, which live in the java.util
;; date layer (java/inst-time.ss) — loaded well after this file, and the layer
;; that owns the default zone, the epoch-ms projection and the locale month/day
;; names. It installs the renderer here on load; until it does, a %t is the
;; UnknownFormatConversionException a host with no date layer should give.
;; The renderer takes (sub-conversion-char argument) and returns the LOWER-case
;; rendering; %T upcases the whole result, exactly as the JVM does.
(define format-datetime-hook #f)
(define (set-format-datetime! f) (set! format-datetime-hook f))
(define (fmt-datetime d sub a)
  (or (and format-datetime-hook (format-datetime-hook sub a))
      (fmt-unknown-conversion (string d sub))))

;; %d %x %X %o take an integer -- Byte through BigInteger on the JVM, never a
;; Double or a Ratio (IllegalFormatConversionException there, so here too).
(define (fmt-integer? a) (and (number? a) (exact? a) (integer? a)))
;; %f %e %g %a take a Float, a Double or a BigDecimal; an integer or a ratio is
;; the same refusal. NaN and the infinities print as the JVM prints them.
(define (fmt-real d a flags width render)
  (cond
    ((jolt-nil? a) (fmt-pad "null" flags width #f))
    ((flonum? a)
     (cond ((nan? a) (fmt-sign-pad #f "NaN" flags width #f))
           ((infinite? a) (fmt-sign-pad (< a 0) "Infinity" flags width #f))
           (else (let-values (((ds ex) (num-digits (abs a))))
                   (fmt-sign-pad (or (< a 0) (eqv? a -0.0)) (render ds ex) flags width #t)))))
    ((jbigdec? a)
     (let-values (((ds ex) (num-digits a)))
       (fmt-sign-pad (< (jbigdec-unscaled a) 0) (render ds ex) flags width #t)))
    (else (fmt-conversion-throw d a))))
;; %a takes the same argument types but renders from the flonum itself, not from
;; its decimal digits, and pads its own way.
(define (fmt-hex-real d a flags width prec)
  (cond
    ((jolt-nil? a) (fmt-pad "null" flags width #f))
    ((flonum? a)
     (cond ((nan? a) (fmt-sign-pad #f "NaN" flags width #f))
           ((infinite? a) (fmt-sign-pad (< a 0) "Infinity" flags width #f))
           (else (fmt-hex-float-pad (or (< a 0) (eqv? a -0.0))
                                    (let ((s (fmt-hex-float a prec)))
                                      (if (char=? d #\A) (string-upcase s) s))
                                    flags width))))
    (else (fmt-conversion-throw d a))))
(define (fmt-directive d a flags width prec)
  (fmt-check-flags d flags width prec)
  (let ((grouped (lambda (s) (if (fmt-flag? flags #\,) (fmt-group s) s)))
        (up (lambda (f) (lambda (ds ex) (string-upcase (f ds ex))))))
    (case d
      ((#\d) (cond ((jolt-nil? a) (fmt-pad "null" flags width #f))
                   ((fmt-integer? a) (fmt-sign-pad (< a 0) (grouped (number->string (abs a))) flags width #t))
                   (else (fmt-conversion-throw d a))))
      ((#\f) (fmt-real d a flags width (lambda (ds ex) (grouped (fmt-fixed ds ex (or prec 6))))))
      ((#\e) (fmt-real d a flags width (lambda (ds ex) (fmt-sci ds ex (or prec 6)))))
      ((#\E) (fmt-real d a flags width (up (lambda (ds ex) (fmt-sci ds ex (or prec 6))))))
      ((#\g) (fmt-real d a flags width (lambda (ds ex) (grouped (fmt-general ds ex (or prec 6))))))
      ((#\G) (fmt-real d a flags width (up (lambda (ds ex) (grouped (fmt-general ds ex (or prec 6)))))))
      ((#\a #\A) (fmt-hex-real d a flags width prec))
      ((#\x #\X #\o)
       (cond ((jolt-nil? a) (fmt-pad "null" flags width #f))
             ((fmt-integer? a)
              ;; Chez spells hex digits in upper case; %x is the lower-case conversion
              (let* ((s (fmt-radix a (if (char=? d #\o) 8 16)))
                     (s (cond ((char=? d #\X) (string-upcase s))
                              ((char=? d #\x) (string-downcase s))
                              (else s)))
                     (pfx (fmt-alt-prefix d flags)))
                ;; the 0 flag's zeros go between the # prefix and the digits, as
                ;; the JVM's do ("0x000000ff", not "000000 0xff")
                (if (and width (fmt-flag? flags #\0) (not (fmt-flag? flags #\-))
                         (fx<? (fx+ (string-length pfx) (string-length s)) width))
                    (string-append pfx (make-string (fx- width (fx+ (string-length pfx) (string-length s))) #\0) s)
                    (fmt-pad (string-append pfx s) flags width #f))))
             (else (fmt-conversion-throw d a))))
      ((#\s) (fmt-pad (fmt-truncate (if (jolt-nil? a) "null" (jolt-str-render-one a)) prec) flags width #f))
      ((#\S) (fmt-pad (string-upcase (fmt-truncate (if (jolt-nil? a) "null" (jolt-str-render-one a)) prec)) flags width #f))
      ((#\b) (fmt-pad (fmt-truncate (if (jolt-truthy? a) "true" "false") prec) flags width #f))
      ((#\B) (fmt-pad (string-upcase (fmt-truncate (if (jolt-truthy? a) "true" "false") prec)) flags width #f))
      ((#\h) (fmt-pad (fmt-truncate (fmt-hash a) prec) flags width #f))
      ((#\H) (fmt-pad (string-upcase (fmt-truncate (fmt-hash a) prec)) flags width #f))
      ((#\c) (fmt-pad (fmt-numeric d a (lambda (n) (if (char? n) (string n) (string (integer->char (->long n))))))
                      flags width #f))
      (else (fmt-unknown-conversion (string d))))))

;; --- the format string -----------------------------------------------------------
;; A directive is %[argument_index$][flags][width][.precision][t|T]conversion,
;; java.util.Formatter's grammar. The argument index and the width are both
;; digits, and only the '$' tells them apart, so the index is scanned ahead and
;; rolled back when there is none: "%12s" is width 12, "%12$s" is argument 12.
(define (fmt-digit? c) (and (char>=? c #\0) (char<=? c #\9)))
;; An index, width or precision is a Java int. A digit run longer than that is
;; not representable, and the JVM says so with the conversion's own exception
;; (IllegalFormatArgumentIndexException / …WidthException / …PrecisionException)
;; rather than a numeric fault — so the scanner SATURATES one digit past int-max
;; instead of growing a bignum, which used to reach fx* and escape as a raw
;; "fixnum overflow" ArithmeticException that no catch clause could select.
(define fmt-int-max 2147483647)
(define (jolt-format fmt . args)
  (let* ((fmt (jolt-need-string fmt))
         (n (string-length fmt))
         (argv (list->vector args))
         (nargs (vector-length argv))
         (out (open-output-string)))
    ;; DIGITS from i: (cons value next-index), or #f when there are none. The
    ;; value saturates at fmt-int-max + 1, so "not representable as an int" is
    ;; visible to the caller without any bignum arithmetic.
    (define (scan-digits i)
      (let loop ((j i) (acc 0) (any #f))
        (if (and (fx<? j n) (fmt-digit? (string-ref fmt j)))
            (loop (fx+ j 1)
                  (let ((v (fx+ (fx* (fxmin acc (fx+ fmt-int-max 1)) 10)
                                (fx- (char->integer (string-ref fmt j)) 48))))
                    (if (fx>? v fmt-int-max) (fx+ fmt-int-max 1) v))
                  #t)
            (and any (cons acc j)))))
    ;; DIGITS '$' from i -> (cons index next-index), else #f
    (define (scan-index i)
      (let ((ds (scan-digits i)))
        (and ds (fx<? (cdr ds) n) (char=? (string-ref fmt (cdr ds)) #\$)
             (cons (car ds) (fx+ (cdr ds) 1)))))
    ;; the flags, in any order; a 0 is a flag only ahead of the width
    (define (scan-flags i)
      (let loop ((j i) (acc '()))
        (if (and (fx<? j n) (memv (string-ref fmt j) '(#\- #\# #\+ #\space #\0 #\, #\( #\<)))
            (loop (fx+ j 1) (cons (string-ref fmt j) acc))
            (cons acc j))))
    ;; '.' DIGITS from i -> (cons precision next-index); a bare '.' is precision 0
    (define (scan-prec i)
      (if (and (fx<? i n) (char=? (string-ref fmt i) #\.))
          (let ((ds (scan-digits (fx+ i 1))))
            (if ds (cons (car ds) (cdr ds)) (cons 0 (fx+ i 1))))
          (cons #f i)))
    ;; a spec that runs off the end of the format string is the JVM's
    ;; UnknownFormatConversionException naming the character after the '%'
    ;; ("abc%" -> '%', "%1$" -> '1')
    (define (unterminated i)
      (fmt-unknown-conversion (if (fx<? (fx+ i 1) n) (string (string-ref fmt (fx+ i 1))) "%")))
    (let loop ((i 0) (ordinary 0) (last -1))
      (if (fx>=? i n)
          (get-output-string out)
          (let ((c (string-ref fmt i)))
            (if (not (char=? c #\%))
                (begin (write-char c out) (loop (fx+ i 1) ordinary last))
                (let* ((idx (scan-index (fx+ i 1)))
                       (fl (scan-flags (if idx (cdr idx) (fx+ i 1))))
                       (flags (car fl))
                       (w (scan-digits (cdr fl)))
                       (pr (scan-prec (if w (cdr w) (cdr fl))))
                       (width (and w (car w)))
                       (prec (car pr))
                       (j (cdr pr)))
                  (when (fx>=? j n) (unterminated i))
                  (when (and width (fx>? width fmt-int-max)) (fmt-width-throw width))
                  (when (and prec (fx>? prec fmt-int-max)) (fmt-precision-throw prec))
                  (let ((d (string-ref fmt j)))
                    (cond
                      ;; %%: a literal percent, taking a width but no argument
                      ((char=? d #\%)
                       (fmt-check-flags d flags #f prec)
                       (display (fmt-pad "%" flags width #f) out)
                       (loop (fx+ j 1) ordinary last))
                      ;; %n: the line separator, taking neither width nor argument
                      ((char=? d #\n)
                       (fmt-check-flags d flags width prec)
                       (write-char #\newline out)
                       (loop (fx+ j 1) ordinary last))
                      (else
                       (let* ((date? (or (char=? d #\t) (char=? d #\T)))
                              (sub (and date?
                                        (begin (when (fx>=? (fx+ j 1) n) (unterminated i))
                                               (string-ref fmt (fx+ j 1)))))
                              (end (if date? (fx+ j 2) (fx+ j 1)))
                              ;; the argument: an explicit index (1-based), the
                              ;; previous one under the < flag, else the next
                              ;; un-indexed one
                              (k (cond (idx (when (or (fx=? (car idx) 0)
                                                     (fx>? (car idx) fmt-int-max))
                                              (fmt-arg-index-throw (car idx)))
                                            (fx- (car idx) 1))
                                       ((fmt-flag? flags #\<) last)
                                       (else ordinary)))
                              (spec (substring fmt i end)))
                         (when (or (fx<? k 0) (fx>=? k nargs)) (fmt-missing-arg spec))
                         (let ((a (vector-ref argv k)))
                           (display (if date?
                                        (begin (fmt-check-date-flags sub flags prec)
                                               (let ((s (fmt-datetime d sub a)))
                                                 (fmt-pad (if (char=? d #\T) (string-upcase s) s)
                                                          flags width #f)))
                                        (fmt-directive d a flags width prec))
                                    out))
                         ;; only an un-indexed directive advances the ordinary
                         ;; cursor, and every one of them remembers its argument
                         ;; for a following %<
                         (loop end
                               (if (or idx (fmt-flag? flags #\<)) ordinary (fx+ ordinary 1))
                               k))))))))))))
(def-var! "clojure.core" "format" jolt-format)
