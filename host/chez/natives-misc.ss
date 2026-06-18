;; misc scalar natives (jolt-cf1q.3) — UUID, format/printf, tagged-literal,
;; bigint. Seed natives that were jolt-nil on the prelude.
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
(define %m-str-render-one jolt-str-render-one)
(set! jolt-str-render-one (lambda (x) (if (juuid? x) (juuid-s x) (%m-str-render-one x))))
(define (juuid-pr u) (string-append "#uuid \"" (juuid-s u) "\""))
(define %m-pr-str jolt-pr-str)
(set! jolt-pr-str (lambda (x) (if (juuid? x) (juuid-pr x) (%m-pr-str x))))
(define %m-pr-readable jolt-pr-readable)
(set! jolt-pr-readable (lambda (x) (if (juuid? x) (juuid-pr x) (%m-pr-readable x))))
;; two uuids are = iff same string.
(define %m-=2 jolt=2)
(set! jolt=2 (lambda (a b)
  (cond ((juuid? a) (and (juuid? b) (string=? (juuid-s a) (juuid-s b))))
        ((juuid? b) #f)
        (else (%m-=2 a b)))))

;; --- bigint / biginteger -----------------------------------------------------
;; jolt models every number as a double; an integer-valued double prints without
;; a ".0" (jolt-num->string), so bigint is just the number for the corpus range.
;; (Arbitrary-precision beyond 2^53 is a separate concern.)
(define (jolt-bigint x) x)
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
(define %m-get jolt-get)
(set! jolt-get (case-lambda
  ((coll k)   (if (jtagged? coll) (jolt-get coll k jolt-nil) (%m-get coll k)))
  ((coll k d) (if (jtagged? coll)
                  (cond ((jolt=2 k kw-tl-tag) (jtagged-tag coll))
                        ((jolt=2 k kw-tl-form) (jtagged-form coll))
                        (else d))
                  (%m-get coll k d)))))
(define (jtagged-pr t) (string-append "#" (jolt-pr-str (jtagged-tag t)) " " (jolt-pr-readable (jtagged-form t))))
(define %m2-pr-str jolt-pr-str)
(set! jolt-pr-str (lambda (x) (if (jtagged? x) (jtagged-pr x) (%m2-pr-str x))))
(define %m2-pr-readable jolt-pr-readable)
(set! jolt-pr-readable (lambda (x) (if (jtagged? x) (jtagged-pr x) (%m2-pr-readable x))))
(def-var! "clojure.core" "tagged-literal" jolt-tagged-literal)
;; tagged-literal? is OVERLAY (reads :jolt/type) — asserted in post-prelude.ss.

;; --- format / printf ---------------------------------------------------------
;; A small %-format engine over the all-flonum number model: %d (integer), %s
;; (str), %f / %.Nf (fixed-point), %x/%X (hex int), %o (octal), %c (char int),
;; %b (boolean), %% (literal). Enough for the corpus; not the full Java spec.
(define (->long x) (exact (truncate x)))
(define (pad-left s n c) (if (fx>=? (string-length s) n) s (string-append (make-string (fx- n (string-length s)) c) s)))
(define (fmt-float x prec)
  (let* ((neg (< x 0)) (ax (abs x))
         (scale (expt 10 prec))
         (scaled (round (* (inexact ax) scale)))
         (i (exact (truncate (/ scaled scale))))
         (frac (exact (truncate (- scaled (* i scale))))))
    (string-append (if neg "-" "")
                   (number->string i)
                   (if (fx>? prec 0) (string-append "." (pad-left (number->string frac) prec #\0)) ""))))
(define (jolt-format fmt . args)
  (let ((out (open-output-string)))
    (let loop ((i 0) (as args))
      (if (fx>=? i (string-length fmt))
          (get-output-string out)
          (let ((c (string-ref fmt i)))
            (if (char=? c #\%)
                ;; parse a directive: %[.prec]conv
                (let scan ((j (fx+ i 1)) (prec #f) (seen-dot #f))
                  (let ((d (string-ref fmt j)))
                    (cond
                      ((char=? d #\%) (write-char #\% out) (loop (fx+ j 1) as))
                      ((char=? d #\.) (scan (fx+ j 1) 0 #t))
                      ((and (char>=? d #\0) (char<=? d #\9))
                       (scan (fx+ j 1) (fx+ (fx* (or prec 0) 10) (fx- (char->integer d) 48)) seen-dot))
                      (else
                       (let ((a (if (null? as) jolt-nil (car as)))
                             (rest (if (null? as) '() (cdr as))))
                         (display
                           (case d
                             ((#\d) (number->string (->long a)))
                             ((#\s) (jolt-str-render-one a))
                             ((#\f) (fmt-float a (or prec 6)))
                             ((#\x) (number->string (->long a) 16))
                             ((#\X) (string-upcase (number->string (->long a) 16)))
                             ((#\o) (number->string (->long a) 8))
                             ((#\b) (if (jolt-truthy? a) "true" "false"))
                             ((#\c) (string (integer->char (->long a))))
                             (else (string #\% d)))
                           out)
                         (loop (fx+ j 1) rest))))))
                (begin (write-char c out) (loop (fx+ i 1) as))))))))
(def-var! "clojure.core" "format" jolt-format)
