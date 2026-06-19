;; natives-str.ss (jolt-nfca) — java.lang.String method interop on Chez.
;;
;; (.method s arg*) on a string target lowers to record-method-dispatch (emit.ss),
;; which falls through to jolt-string-method here when the target is a string.
;; Ported from the seed surface (src/jolt/eval_resolve.janet string-methods): the
;; portable java.lang.String/CharSequence methods cljc libraries actually call.
;; Case mapping is ASCII (the whole engine is byte-oriented), indexOf returns -1
;; on miss as on the JVM, indices come in as flonums, char results are Scheme
;; chars, and numeric results are flonums to match jolt's number model.
;;
;; Loaded from rt.ss AFTER regex.ss (the regex methods reuse jolt-re-pattern /
;; regex-t-irx) and records.ss (which calls jolt-string-method).

;; --- ASCII case mapping (match the seed's byte-oriented string/ascii-*) -------
(define (ascii-up-char c)
  (if (and (char<=? #\a c) (char<=? c #\z))
      (integer->char (fx- (char->integer c) 32)) c))
(define (ascii-down-char c)
  (if (and (char<=? #\A c) (char<=? c #\Z))
      (integer->char (fx+ (char->integer c) 32)) c))
(define (ascii-string-up s) (list->string (map ascii-up-char (string->list s))))
(define (ascii-string-down s) (list->string (map ascii-down-char (string->list s))))

;; --- ASCII trim: drop leading/trailing chars with code <= space (JVM .trim) ---
(define (str-trim s)
  (let ((len (string-length s)))
    (let scan-l ((i 0))
      (cond ((fx=? i len) "")
            ((char<=? (string-ref s i) #\space) (scan-l (fx+ i 1)))
            (else (let scan-r ((j (fx- len 1)))
                    (if (char<=? (string-ref s j) #\space)
                        (scan-r (fx- j 1))
                        (substring s i (fx+ j 1)))))))))
(define (str-triml s)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (cond ((fx=? i len) "")
            ((char<=? (string-ref s i) #\space) (loop (fx+ i 1)))
            (else (substring s i len))))))
(define (str-trimr s)
  (let loop ((j (fx- (string-length s) 1)))
    (cond ((fx<? j 0) "")
          ((char<=? (string-ref s j) #\space) (loop (fx- j 1)))
          (else (substring s 0 (fx+ j 1))))))

;; --- substring search: first index of `needle` in `s` at/after `from`, or -1 --
(define (str-index-of s needle from)
  (let ((nlen (string-length needle)) (slen (string-length s)))
    (let loop ((i (max 0 from)))
      (cond ((fx>? (fx+ i nlen) slen) -1)
            ((string=? (substring s i (fx+ i nlen)) needle) i)
            (else (loop (fx+ i 1)))))))
(define (str-last-index-of s needle)
  (let ((nlen (string-length needle)) (slen (string-length s)))
    (let loop ((i (fx- slen nlen)) (found -1))
      (cond ((fx<? i 0) found)
            ((string=? (substring s i (fx+ i nlen)) needle) i)
            (else (loop (fx- i 1) found))))))

;; A needle arg: a char value -> its 1-char string; a number -> the char at that
;; code point (JVM treats an int arg to indexOf as a char code); else a string.
(define (str-needle x)
  (cond ((char? x) (string x))
        ((number? x) (string (integer->char (exact (truncate x)))))
        ((string? x) x)
        (else (jolt-str x))))

;; literal replace-all (JVM String.replace(CharSequence,CharSequence)).
(define (str-replace-literal s a b)
  (let ((alen (string-length a)) (slen (string-length s)))
    (if (fx=? alen 0) s
        (let loop ((i 0) (acc '()))
          (cond ((fx>? (fx+ i alen) slen)
                 (apply string-append (reverse (cons (substring s i slen) acc))))
                ((string=? (substring s i (fx+ i alen)) a)
                 (loop (fx+ i alen) (cons b acc)))
                (else (loop (fx+ i 1) (cons (substring s i (fx+ i 1)) acc))))))))

;; A compiled irregex for a plain-string Java-regex pattern (or a jolt-regex).
(define (str-irx pat) (regex-t-irx (jolt-re-pattern pat)))

;; JVM String.split: split fully, then drop trailing empty strings.
(define (str-split-drop-trailing parts)
  (let loop ((p (reverse parts)))
    (if (and (pair? p) (string=? (car p) "")) (loop (cdr p)) (reverse p))))

(define (jolt-string-method method s rest)
  (define (arg n) (list-ref rest n))
  (cond
    ((string=? method "toString") s)
    ((string=? method "toLowerCase") (ascii-string-down s))
    ((string=? method "toUpperCase") (ascii-string-up s))
    ((string=? method "trim") (str-trim s))
    ((string=? method "length") (exact->inexact (string-length s)))
    ((string=? method "isEmpty") (fx=? (string-length s) 0))
    ((string=? method "charAt") (string-ref s (jolt->idx (arg 0))))
    ((string=? method "substring")
     (substring s (jolt->idx (arg 0))
                (if (fx>? (length rest) 1) (jolt->idx (arg 1)) (string-length s))))
    ((string=? method "indexOf")
     (exact->inexact
      (str-index-of s (str-needle (arg 0))
                    (if (fx>? (length rest) 1) (jolt->idx (arg 1)) 0))))
    ((string=? method "lastIndexOf")
     (exact->inexact (str-last-index-of s (str-needle (arg 0)))))
    ((string=? method "startsWith")
     (let ((p (arg 0))) (and (fx>=? (string-length s) (string-length p))
                             (string=? (substring s 0 (string-length p)) p))))
    ((string=? method "endsWith")
     (let ((p (arg 0)) (slen (string-length s)))
       (and (fx>=? slen (string-length p))
            (string=? (substring s (fx- slen (string-length p)) slen) p))))
    ((string=? method "contains")
     (fx>=? (str-index-of s (str-needle (arg 0)) 0) 0))
    ((string=? method "concat") (string-append s (arg 0)))
    ((string=? method "replace") (str-replace-literal s (str-needle (arg 0)) (str-needle (arg 1))))
    ((string=? method "equalsIgnoreCase")
     (string=? (ascii-string-down s) (ascii-string-down (arg 0))))
    ((string=? method "compareTo")
     (let ((o (arg 0))) (cond ((string<? s o) -1.0) ((string>? s o) 1.0) (else 0.0))))
    ((string=? method "getBytes") (string->utf8 s))
    ((string=? method "matches") (if (irregex-match (str-irx (arg 0)) s) #t #f))
    ((string=? method "replaceAll") (irregex-replace/all (str-irx (arg 0)) s (arg 1)))
    ((string=? method "replaceFirst") (irregex-replace (str-irx (arg 0)) s (arg 1)))
    ((string=? method "split")
     (apply jolt-vector (str-split-drop-trailing (irregex-split (str-irx (arg 0)) s))))
    (else (error #f (string-append "No method " method " for value")))))
