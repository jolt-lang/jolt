;; java-parse.ss — the java.lang number-parsing family and the numeric
;; coercion helpers the interop layer hands results through. Shared by every
;; target: the grammar is java-int-parse / java-double-parse (natives-num.ss),
;; and what lives here is the JVM's NumberFormatException, in the message the
;; caller's own class uses. Long/parseLong, Integer/decode, Double/parseDouble
;; and their siblings are these procedures registered under their class
;; (host-static-methods.ss on Chez, host-statics.ss on Gambit), and
;; clojure.core/parse-long is the same parse with the throw caught.
;;
;; Needs jolt-throw / jolt-host-throwable, jolt-str-render-one and jnum->exact;
;; loads before the statics registries that reference it.

;; numeric tower: currentTimeMillis/nanoTime are exact longs (JVM).
;; (jnum->exact lives in seq.ss: the shared dispatch arms read it too.)
(define (->num x) x)
;; ---- java.lang integer parsing ----------------------------------------------
;; The grammar and the width check are java-int-parse (natives-num.ss), shared
;; with clojure.core/parse-long, which is Long/valueOf with the throw caught.
;; What lives HERE is the throw: the JVM's NumberFormatException, in the message
;; the caller's own class uses.
;;
;; TYPE is the target integer type -- the same name its TYPE static carries --
;; and is what makes a width check possible at all: every parser below is this
;; one function at a different width, and a new one cannot be wired without
;; saying which type it parses. The name used to be a free-form label for the
;; error text alone, was "valueOf" for all four classes, and nothing checked the
;; width, so Byte/parseByte answered 128 and Long/parseLong a bignum.
;;
;; The JVM has TWO out-of-range messages and jolt has to pick per type: Long and
;; Integer reuse the ordinary "For input string:" one, Short and Byte have their
;; own "Value out of range." A bad SHAPE is always the first, and both it and the
;; range message name a radix other than 10 (Integer.parseInt appends "under
;; radix N"; Short/Byte always spell "Radix:N").
(define java-int-types
  ;; name -> (min max value-out-of-range-message?); #f bounds is the unbounded
  ;; parse, which is BigInteger's and clojure.core/bigint's.
  (list (list "long"  -9223372036854775808 9223372036854775807 #f)
        (list "int"   -2147483648          2147483647          #f)
        (list "short" -32768               32767               #t)
        (list "byte"  -128                 127                 #t)
        (list "big"   #f                   #f                  #f)))

(define (java-int-input-msg str radix)
  (string-append "For input string: \"" str "\""
                 (if (= radix 10) "" (string-append " under radix " (number->string radix)))))

(define (parse-int-or-throw s radix type)
  (let* ((str (if (string? s) s (jolt-str-render-one s)))
         (row (assoc type java-int-types))
         (v (java-int-parse str radix (cadr row) (caddr row))))
    (if (symbol? v)
        (jolt-throw
         (jolt-host-throwable
          "java.lang.NumberFormatException"
          (cond
            ((eq? v (quote radix))
             (string-append "radix " (number->string radix)
                            (if (< radix 2) " less than Character.MIN_RADIX"
                                " greater than Character.MAX_RADIX")))
            ((and (eq? v (quote range)) (cadddr row))
             (string-append "Value out of range. Value:\"" str "\" Radix:"
                            (number->string radix)))
            (else (java-int-input-msg str radix)))))
        (->num v))))

;; Integer.decode(String) and its three siblings: the same grammar with a RADIX
;; PREFIX in front of it -- 0x / 0X / # for hex, a bare leading 0 for octal,
;; nothing for decimal -- and the sign OUTSIDE the prefix, as in "-0x1f". Not a
;; fourth parser: strip the sign and the prefix, hand the digits to
;; java-int-parse at the radix they named, and put the sign back. It was missing
;; entirely, which is how it stayed out of the parse family's reach; adding it
;; anywhere but here would have started that family over.
;;
;; The messages are Integer.decode's, which names the digits AFTER the prefix and
;; the radix they resolved to -- (Integer/decode "08") is `For input string: "8"
;; under radix 8`, not a complaint about "08" -- and, for the two narrow types, a
;; range message of its own that quotes the value and the ORIGINAL string.
(define (java-decode-split str)
  ;; -> (values sign digits radix), digits after sign and prefix
  (let* ((n (string-length str))
         (c0 (and (fx>? n 0) (string-ref str 0)))
         (neg? (eqv? c0 #\-))
         (i (if (or neg? (eqv? c0 #\+)) 1 0)))
    (cond
      ((and (fx<=? (fx+ i 2) n)
            (char=? (string-ref str i) #\0)
            (memv (string-ref str (fx+ i 1)) (quote (#\x #\X))))
       (values neg? (substring str (fx+ i 2) n) 16))
      ((and (fx<? i n) (char=? (string-ref str i) #\#))
       (values neg? (substring str (fx+ i 1) n) 16))
      ((and (fx<? (fx+ i 1) n) (char=? (string-ref str i) #\0))
       (values neg? (substring str (fx+ i 1) n) 8))
      (else (values neg? (substring str i n) 10)))))

(define (decode-or-throw s type)
  (let ((str (if (string? s) s (jolt-str-render-one s))))
    (if (fx=? 0 (string-length str))
        (jolt-throw (jolt-host-throwable "java.lang.NumberFormatException" "Zero length string"))
        (let-values (((neg? digits radix) (java-decode-split str)))
          (let* ((row (assoc type java-int-types))
                 ;; unbounded first: the narrow types' range message quotes the
                 ;; VALUE, so it has to exist before the width is applied.
                 (mag (java-int-parse digits radix #f #f))
                 (v (and (not (symbol? mag)) (if neg? (- mag) mag))))
            (cond
              ((symbol? mag)
               (jolt-throw (jolt-host-throwable "java.lang.NumberFormatException"
                             ;; a sign with nothing after it reports the sign, as
                             ;; decode's own retry with the re-attached sign does
                             (java-int-input-msg (if (and neg? (fx=? 0 (string-length digits))) "-" digits)
                                                 radix))))
              ((and (>= v (cadr row)) (<= v (caddr row))) (->num v))
              ((cadddr row)
               (jolt-throw (jolt-host-throwable "java.lang.NumberFormatException"
                             (string-append "Value " (number->string v)
                                            " out of range from input " str))))
              (else
               (jolt-throw (jolt-host-throwable "java.lang.NumberFormatException"
                             (java-int-input-msg digits radix))))))))))
(define (char-code c) (if (char? c) (char->integer c) (jnum->exact c)))

;; Double/parseDouble, Float/parseFloat, Double/valueOf, (Double. s) — the
;; floating half of parse-int-or-throw, and the same story: the grammar is
;; java-double-parse (natives-num.ss), shared with clojure.core/parse-double,
;; which is this method with the throw caught. It used to hand the string
;; straight to string->number and so spoke Scheme, reading "#xff" as 255.0 and
;; "1/2" as 0.5 — neither one a double on the JVM — while missing the hex
;; significand form ("0x1fp0" is 31.0 there) that a Scheme reader has no
;; spelling for. #f on failure.
(define (parse-double-str s)
  (java-double-parse (if (string? s) s (jolt-str-render-one s))))
(define (parse-double-or-throw s)
  (or (parse-double-str s)
      (jolt-throw (jolt-host-throwable "java.lang.NumberFormatException"
                    (string-append "For input string: \""
                                   (if (string? s) s (jolt-str-render-one s)) "\"")))))
(define (->double x) (if (number? x) (exact->inexact x) (parse-double-or-throw x)))

