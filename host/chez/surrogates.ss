;; surrogates.ss — a UTF-16 surrogate code unit as a jolt value (the jsurr
;; record, values.ss), and the hooks that make it a java.lang.Character.
;;
;; jolt strings hold code points, so half of a surrogate pair cannot live in
;; one. It can live outside one, as it does on the JVM: (char 0xD83D), a char[]
;; element and Character/toChars hand one out (#1454). What a
;; program can observe of it, registered with each hook that owns it, as
;; float.ss does for java.lang.Float:
;;   - char? is true and (class u) is java.lang.Character; (int u) is the unit.
;;   - = and identical? hold between two of the same unit, since values.ss
;;     makes each one once; hash is Character.hashCode, the unit.
;;   - compare orders it among chars by its unit, as Character.compareTo does.
;;   - pr prints \uD83D. str renders a LONE unit as U+FFFD, since finished jolt
;;     text cannot hold one. That is what the JDK's UTF-8 encoder writes for
;;     one, so text that leaves the process reads the same. A high unit
;;     followed by a low one, in str's arguments, a StringBuilder, a Writer or a
;;     char[] made into a String, is the astral character the two spell.
;; Shared by the Chez and Gambit hosts.

(register-hash-arm! jsurr? jsurr-unit)
(register-compare-arm! (lambda (a b) (or (jsurr? a) (jsurr? b)))
  (lambda (a b)
    (if (and (or (char? a) (jsurr? a)) (or (char? b) (jsurr? b)))
        (- (char-or-unit->integer a) (char-or-unit->integer b))
        (jolt-cast-throw (if (jsurr? a) b a) "java.lang.Character"))))
(register-str-render! jsurr? (lambda (u) jolt-replacement-string))
(define (jsurr-pr u)
  (let ((h (string-upcase (number->string (jsurr-unit u) 16))))
    (if (jolt-pr-readable?) (string-append "\\u" h) jolt-replacement-string)))
(register-pr-arm! jsurr? jsurr-pr)
(register-class-arm! jsurr? (lambda (u) "java.lang.Character"))

;; java.lang.Character's own instance methods over a unit.
(define jsurr-instance-tbl (make-hashtable string-hash string=?))
(for-each (lambda (p) (hashtable-set! jsurr-instance-tbl (car p) (cdr p)))
  (list (cons "charValue" (lambda (u) u))
        (cons "hashCode" jsurr-unit)
        (cons "equals" (lambda (u o) (eq? u o)))
        (cons "compareTo" (lambda (u o) (- (jsurr-unit u) (char-or-unit->integer o))))
        (cons "toString" (lambda (u) jolt-replacement-string))))
(register-method-arm! arm-priority-float
  (lambda (obj method-name rest-args)
    (if (jsurr? obj)
        (let ((f (hashtable-ref jsurr-instance-tbl method-name #f)))
          (if f
              (apply f obj (if (jolt-nil? rest-args) '() (seq->list rest-args)))
              'pass))
        'pass)))
