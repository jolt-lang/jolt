;; misc scalar natives — UUID, tagged-literal, bigint, and the hash API. (format /
;; printf moved to natives-format.ss.)
;;
;; Loaded after the printers (pr-str of a uuid is #uuid "…") and converters
;; (jolt-str-render-one for %s / str of a uuid).

;; --- UUID --------------------------------------------------------------------
;; A uuid is a record wrapping its canonical 36-char lowercase string. str -> the
;; bare string; pr-str -> #uuid "…"; not map?/coll?.
(define-record-type juuid (fields s) (nongenerative chez-juuid-v1))
(define (jolt-uuid-pred? x) (juuid? x))

(define hexd "0123456789abcdef")
(define (rand-hex) (string-ref hexd (random 16)))
;; v4: 8-4-4-4-12, version nibble (index 14) = 4, variant nibble (index 19) in 8-b.
(define (random-uuid-str)
  (let ((cs (make-string 36)))
    (let loop ((i 0))
      (if (fx=? i 36) cs
          (begin
            (string-set! cs i
              (cond ((or (fx=? i 8) (fx=? i 13) (fx=? i 18) (fx=? i 23)) #\-)
                    ((fx=? i 14) #\4)
                    ((fx=? i 19) (string-ref "89ab" (random 4)))
                    (else (rand-hex))))
            (loop (fx+ i 1)))))))
(define (jolt-random-uuid) (make-juuid (random-uuid-str)))

;; #uuid literal -> a uuid value (the emitter lowers the :uuid node to this). The
;; reader already validated the shape; lowercase for value equality.
(define (jolt-uuid-from-string s) (make-juuid (string-downcase s)))

;; parse-uuid: validate the canonical shape (8-4-4-4-12 hex), lowercase, -> uuid;
;; nil if the string doesn't conform (Clojure parse-uuid), error on a non-string.
(define (hex-char? c) (or (and (char>=? c #\0) (char<=? c #\9))
                          (and (char>=? c #\a) (char<=? c #\f))
                          (and (char>=? c #\A) (char<=? c #\F))))
(define (uuid-shape? s)
  (and (string? s) (fx=? (string-length s) 36)
       (let loop ((i 0))
         (if (fx=? i 36) #t
             (let ((c (string-ref s i)))
               (cond ((or (fx=? i 8) (fx=? i 13) (fx=? i 18) (fx=? i 23)) (and (char=? c #\-) (loop (fx+ i 1))))
                     ((hex-char? c) (loop (fx+ i 1)))
                     (else #f)))))))
(define (jolt-parse-uuid s)
  (cond ((not (string? s)) (error #f "parse-uuid: not a string" s))
        ((uuid-shape? s) (make-juuid (string-downcase s)))
        (else jolt-nil)))

;; uuid? / random-uuid / parse-uuid are OVERLAY fns (they read :jolt/type), so
;; the prelude would clobber a def-var! here — they're asserted in post-prelude.ss.

;; str of a uuid -> the bare 36-char string; pr-str -> #uuid "…".
(register-str-render! juuid? juuid-s)
(define (juuid-pr u) (string-append "#uuid \"" (juuid-s u) "\""))
(register-pr-arm! juuid? juuid-pr)
;; two uuids are = iff same string.
(register-eq-arm! (lambda (a b) (or (juuid? a) (juuid? b)))
                  (lambda (a b) (and (juuid? a) (juuid? b) (string=? (juuid-s a) (juuid-s b)))))

;; --- bigint / biginteger -----------------------------------------------------
;; JVM bigint/biginteger coerce: string → parsed integer, double/float →
;; truncated integer, ratio → quotient, integer → exact integer.
(define (jolt-bigint x)
  (cond ((string? x) (parse-int-or-throw x 10 "bigint"))
        ((flonum? x)
         (if (or (finite? x) (zero? x))
             (inexact->exact (truncate x))
             (jolt-throw (jolt-host-throwable "java.lang.NumberFormatException"
                           (string-append "For input string: \""
                                          (jolt-str-render-one x) "\"")))))
        (else (inexact->exact (truncate x)))))
(def-var! "clojure.core" "bigint" jolt-bigint)
(def-var! "clojure.core" "biginteger" jolt-bigint)

;; --- tagged-literal ----------------------------------------------------------
;; (tagged-literal tag form): a tagged value with :tag / :form. tagged-literal? is
;; overlay (reads :jolt/type) so it's overridden in post-prelude.ss.
(define-record-type jtagged (fields tag form) (nongenerative chez-jtagged-v1))
(define (jolt-tagged-literal tag form) (make-jtagged tag form))
(define (jolt-tagged-literal-pred? x) (jtagged? x))
(define kw-tl-tag (keyword #f "tag"))
(define kw-tl-form (keyword #f "form"))
(register-get-arm! jtagged?
  (lambda (coll k d)
    (cond ((jolt=2 k kw-tl-tag) (jtagged-tag coll))
          ((jolt=2 k kw-tl-form) (jtagged-form coll))
          (else d))))
(define (jtagged-pr t) (string-append "#" (jolt-pr-str (jtagged-tag t)) " " (jolt-pr-readable (jtagged-form t))))
(register-pr-arm! jtagged? jtagged-pr)
;; two tagged literals are = iff same tag and (recursively) = form, like the JVM's
;; TaggedLiteral — so they work as map keys / set members. (jolt-hash already
;; hashes the fields structurally, so eq/hash stay consistent.)
(register-eq-arm! (lambda (a b) (or (jtagged? a) (jtagged? b)))
                  (lambda (a b) (and (jtagged? a) (jtagged? b)
                                     (jolt=2 (jtagged-tag a) (jtagged-tag b))
                                     (jolt=2 (jtagged-form a) (jtagged-form b)))))
(def-var! "clojure.core" "tagged-literal" jolt-tagged-literal)
;; tagged-literal? is OVERLAY (reads :jolt/type) — asserted in post-prelude.ss.

;; --- hash family (24-bit masked so int? holds) -------------------------------
;; The public hash API over jolt-hash (values.ss). hash-ordered/-unordered-coll
;; fold the element hashes the way Clojure's IHash mixers do.
(define (nm-h24 x) (bitwise-and (jolt-hash x) #xffffff))
(define (nm-hash x) (nm-h24 x))
(define (nm-hash-combine a b)
  (bitwise-and (bitwise-xor (nm-h24 a) (+ (nm-h24 b) #x9e3779)) #xffffff))
(define (nm-hash-ordered-coll coll)
  (let loop ((xs (seq->list (jolt-seq coll))) (h 1))
    (if (null? xs) h (loop (cdr xs) (bitwise-and (+ (* 31 h) (nm-h24 (car xs))) #xffffff)))))
(define (nm-hash-unordered-coll coll)
  (let loop ((xs (seq->list (jolt-seq coll))) (h 0))
    (if (null? xs) h (loop (cdr xs) (bitwise-and (+ h (nm-h24 (car xs))) #xffffff)))))
(def-var! "clojure.core" "hash" nm-hash)
(def-var! "clojure.core" "hash-combine" nm-hash-combine)
(def-var! "clojure.core" "hash-ordered-coll" nm-hash-ordered-coll)
(def-var! "clojure.core" "hash-unordered-coll" nm-hash-unordered-coll)
