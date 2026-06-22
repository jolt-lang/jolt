;; BigDecimal (jolt-i2jm). A jbigdec is {unscaled, scale} over Chez arbitrary-
;; precision exact integers; its value is unscaled * 10^-scale (1.5M = {15,1},
;; 1.00M = {100,2}, 3M = {3,0}). M-suffix literals read to a :bigdec form that the
;; back end lowers to jolt-bigdec-from-string; bigdec coerces a number/string.
;; Equality is by value (1.0M = 1.00M), str drops the M, pr keeps it, class is
;; java.math.BigDecimal. Arithmetic contagion is not modelled (jolt-i2jm scope).

(define-record-type jbigdec (fields unscaled scale) (nongenerative chez-jbigdec-v1))

(define (bd-index-char s ch)
  (let loop ((i 0))
    (cond ((>= i (string-length s)) #f)
          ((char=? (string-ref s i) ch) i)
          (else (loop (+ i 1))))))

;; "1.50" -> {150,2}; "3" -> {3,0}; "-0.0" -> {0,1}; ".5" -> {5,1}.
(define (jolt-bigdec-from-string s)
  (let* ((neg (and (> (string-length s) 0) (char=? (string-ref s 0) #\-)))
         (sgn (and (> (string-length s) 0) (or neg (char=? (string-ref s 0) #\+))))
         (s1 (if sgn (substring s 1 (string-length s)) s))
         (sign (if neg -1 1))
         (dot (bd-index-char s1 #\.)))
    (if dot
        (let* ((intp (substring s1 0 dot))
               (fracp (substring s1 (+ dot 1) (string-length s1)))
               (digs (string-append intp fracp))
               (unscaled (if (= 0 (string-length digs)) 0 (string->number digs))))
          (make-jbigdec (* sign unscaled) (string-length fracp)))
        (make-jbigdec (* sign (string->number s1)) 0))))

;; bigdec coercion: a bigdec is itself; an exact integer keeps scale 0; a string
;; or any other number routes through its decimal text.
(define (jolt-bigdec x)
  (cond
    ((jbigdec? x) x)
    ((and (number? x) (exact? x) (integer? x)) (make-jbigdec x 0))
    ((string? x) (jolt-bigdec-from-string x))
    ((number? x) (jolt-bigdec-from-string (jolt-num->string x)))
    (else (error #f "bigdec: cannot coerce" x))))

;; value equality: unscaled_a * 10^scale_b == unscaled_b * 10^scale_a.
(define (jbigdec=? a b)
  (= (* (jbigdec-unscaled a) (expt 10 (jbigdec-scale b)))
     (* (jbigdec-unscaled b) (expt 10 (jbigdec-scale a)))))

;; render the decimal text (no M): insert the point `scale` digits from the right.
(define (jbigdec->string bd)
  (let* ((u (jbigdec-unscaled bd)) (sc (jbigdec-scale bd))
         (neg (< u 0)) (digs (number->string (abs u))))
    (string-append
      (if neg "-" "")
      (if (<= sc 0)
          digs
          (let* ((padded (if (<= (string-length digs) sc)
                             (string-append (make-string (- (+ sc 1) (string-length digs)) #\0) digs)
                             digs))
                 (pl (string-length padded)))
            (string-append (substring padded 0 (- pl sc)) "." (substring padded (- pl sc) pl)))))))

;; --- wire into the value model ----------------------------------------------
(def-var! "clojure.core" "bigdec" jolt-bigdec)

;; equality: a bigdec equals only another bigdec, by value (matching (= 3M 3) = false).
(define %bd-jolt=2 jolt=2)
(set! jolt=2 (lambda (a b)
  (cond ((and (jbigdec? a) (jbigdec? b)) (jbigdec=? a b))
        ((or (jbigdec? a) (jbigdec? b)) #f)
        (else (%bd-jolt=2 a b)))))

;; str drops the M; pr/pr-str keep it.
(define %bd-str-render jolt-str-render-one)
(set! jolt-str-render-one (lambda (x) (if (jbigdec? x) (jbigdec->string x) (%bd-str-render x))))
(define %bd-pr-str jolt-pr-str)
(set! jolt-pr-str (lambda (x) (if (jbigdec? x) (string-append (jbigdec->string x) "M") (%bd-pr-str x))))
(define %bd-pr-readable jolt-pr-readable)
(set! jolt-pr-readable (lambda (x) (if (jbigdec? x) (string-append (jbigdec->string x) "M") (%bd-pr-readable x))))

;; class / decimal?
(define %bd-class jolt-class)
(set! jolt-class (lambda (x) (if (jbigdec? x) "java.math.BigDecimal" (%bd-class x))))
(def-var! "clojure.core" "class" jolt-class)
(set! jolt-decimal? (lambda (x) (jbigdec? x)))
(def-var! "clojure.core" "decimal?" jolt-decimal?)
