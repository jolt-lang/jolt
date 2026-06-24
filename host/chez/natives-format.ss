;; natives-format.ss — a small %-format engine for clojure.core `format` over the
;; all-flonum number model: %d (integer), %s (str), %f / %.Nf (fixed-point), %x/%X
;; (hex int), %o (octal), %c (char int), %b (boolean), %% (literal). Enough for the
;; corpus, not the full Java Formatter spec. Loaded after natives-misc.ss (uses
;; jolt-str-render-one via converters + jolt-truthy?).

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
                ;; parse a directive: %[-][0][width][.prec]conv
                (let scan ((j (fx+ i 1)) (left #f) (zero #f) (width #f) (prec #f) (seen-dot #f))
                  (let ((d (string-ref fmt j)))
                    (cond
                      ((char=? d #\%) (write-char #\% out) (loop (fx+ j 1) as))
                      ((and (not seen-dot) (not width) (char=? d #\-))
                       (scan (fx+ j 1) #t zero width prec seen-dot))
                      ((and (not seen-dot) (not width) (char=? d #\0))
                       (scan (fx+ j 1) left #t width prec seen-dot))
                      ((char=? d #\.) (scan (fx+ j 1) left zero width 0 #t))
                      ((and (char>=? d #\0) (char<=? d #\9))
                       (if seen-dot
                           (scan (fx+ j 1) left zero width (fx+ (fx* (or prec 0) 10) (fx- (char->integer d) 48)) seen-dot)
                           (scan (fx+ j 1) left zero (fx+ (fx* (or width 0) 10) (fx- (char->integer d) 48)) prec seen-dot)))
                      (else
                       (let* ((a (if (null? as) jolt-nil (car as)))
                              (rest (if (null? as) '() (cdr as)))
                              (s (case d
                                   ((#\d) (number->string (->long a)))
                                   ((#\s) (jolt-str-render-one a))
                                   ((#\f) (fmt-float a (or prec 6)))
                                   ((#\x) (number->string (->long a) 16))
                                   ((#\X) (string-upcase (number->string (->long a) 16)))
                                   ((#\o) (number->string (->long a) 8))
                                   ((#\b) (if (jolt-truthy? a) "true" "false"))
                                   ((#\c) (string (integer->char (->long a))))
                                   (else (string #\% d))))
                              ;; pad to width: left-justify with spaces, else right-justify
                              ;; (zero-pad only a right-justified number).
                              (s (if (and width (fx<? (string-length s) width))
                                     (let ((p (fx- width (string-length s))))
                                       (if left (string-append s (make-string p #\space))
                                           (string-append (make-string p (if (and zero (memv d '(#\d #\f #\x #\X #\o))) #\0 #\space)) s)))
                                     s)))
                         (display s out)
                         (loop (fx+ j 1) rest))))))
                (begin (write-char c out) (loop (fx+ i 1) as))))))))
(def-var! "clojure.core" "format" jolt-format)
