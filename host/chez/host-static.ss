;; host-static.ss — host class statics + constructors on Chez.
;;
;; The analyzer lowers `Class/member` to a :host-static node and `(Class. ...)` /
;; `(new Class ...)` to a :host-new node (jolt-core/jolt/analyzer.clj); the Chez
;; emit lowers a value ref to (host-static-ref "Class" "member"), a
;; call head to (host-static-call "Class" "member" args...), and a constructor to
;; (host-new "Class" args...). This file is the runtime registry those three
;; resolve against — the class-statics / class-ctors /
;; tagged-methods registries,
;; restricted to the java.lang/util/net/io surface portable cljc code calls.
;; (java.time formatting is a separate increment.)
;;
;; Constructed host objects are `jhost` records (a tag + mutable state); their
;; (.method ...) calls reach record-method-dispatch (records.ss), extended below
;; with a jhost arm that dispatches through host-tagged-methods.
;;
;; Loaded from rt.ss LAST (after natives-str.ss / records.ss): it extends
;; record-method-dispatch and reuses jolt-str-render-one / jolt-re-pattern.

;; ---- registries -------------------------------------------------------------
(define class-statics-tbl (make-hashtable string-hash string=?))   ; "Class" -> (member-ht)
(define class-ctors-tbl   (make-hashtable string-hash string=?))   ; "Class" -> ctor proc
(define host-methods-tbl  (make-hashtable string-hash string=?))   ; tag -> (method-ht)

;; A class token may arrive fully qualified (java.io.StringReader) or short
;; (StringReader). Register both; resolve by exact then by last dotted segment.
(define (short-class-name s)
  (let loop ((i (- (string-length s) 1)))
    (cond ((< i 0) s)
          ((char=? (string-ref s i) #\.) (substring s (+ i 1) (string-length s)))
          (else (loop (- i 1))))))

(define (register-class-statics! name members)  ; members: list of (str . val/proc)
  (let ((h (or (hashtable-ref class-statics-tbl name #f)
               (let ((h (make-hashtable string-hash string=?)))
                 (hashtable-set! class-statics-tbl name h) h))))
    (for-each (lambda (p) (hashtable-set! h (car p) (cdr p))) members)))

(define (register-class-ctor! name proc) (hashtable-set! class-ctors-tbl name proc))

(define (register-host-methods! tag members)
  (let ((h (or (hashtable-ref host-methods-tbl tag #f)
               (let ((h (make-hashtable string-hash string=?)))
                 (hashtable-set! host-methods-tbl tag h) h))))
    (for-each (lambda (p) (hashtable-set! h (car p) (cdr p))) members)))

(define (lookup-class h-tbl name)
  (or (hashtable-ref h-tbl name #f)
      (hashtable-ref h-tbl (short-class-name name) #f)))

;; ---- host object ------------------------------------------------------------
(define-record-type jhost (fields tag (mutable state)) (nongenerative chez-jhost-v1))

;; record-method-dispatch (records.ss) gets a jhost arm: dispatch (.method obj a*)
;; through the tag's method table.
(define %hs-record-method-dispatch record-method-dispatch)
(set! record-method-dispatch
  (lambda (obj method-name rest-args)
    (cond
      ;; (.getClass x) is universal — the class token for any value (incl. numbers
      ;; / jhost) — before the per-type arms that would otherwise reject it.
      ((string=? method-name "getClass") (jolt-class obj))
      ((jhost? obj)
       (let ((mh (hashtable-ref host-methods-tbl (jhost-tag obj) #f)))
         (let ((f (and mh (hashtable-ref mh method-name #f))))
           (if f
               (apply f obj (if (jolt-nil? rest-args) '() (seq->list rest-args)))
               (error #f (string-append "No method " method-name " on host " (jhost-tag obj)))))))
      ((number? obj) (number-method method-name obj))
      (else (%hs-record-method-dispatch obj method-name rest-args)))))

;; java.lang.Number method surface (the boxed-number methods cljc code calls). The
;; integer projections wrap modulo their width (ring-codec relies on byteValue
;; overflow: (.byteValue 255) => -1); the float projections are identity flonums.
(define (number-method method n)
  (cond
    ((string=? method "byteValue") (let ((b (modulo (jnum->exact n) 256))) (->num (if (>= b 128) (- b 256) b))))
    ((string=? method "shortValue") (let ((b (modulo (jnum->exact n) 65536))) (->num (if (>= b 32768) (- b 65536) b))))
    ((string=? method "intValue") (->num (jnum->exact n)))
    ((string=? method "longValue") (->num (jnum->exact n)))
    ((string=? method "doubleValue") (->num n))
    ((string=? method "floatValue") (->num n))
    ((string=? method "toString") (jolt-num->string n))
    ((string=? method "hashCode") (->num (jnum->exact n)))
    (else (error #f (string-append "No method " method " for number")))))

;; ---- emit entry points ------------------------------------------------------
(define (host-static-ref class member)
  (let ((h (lookup-class class-statics-tbl class)))
    (if h
        (let ((v (hashtable-ref h member #f)))
          (if v v (error #f (string-append "No static " class "/" member))))
        (error #f (string-append "Unknown class " class)))))

(define (host-static-call class member . args)
  (apply (host-static-ref class member) args))

(define (host-new class . args)
  (let ((ctor (lookup-class class-ctors-tbl class)))
    (cond
      (ctor (apply ctor args))
      ;; deftype/defrecord: the type name is bound as a VAR (the
      ;; make-deftype-ctor closure) in its defining ns, not a registered host class.
      ;; Resolve it in the current ns / clojure.core and invoke it — so (P. args)
      ;; works the same as the ->P factory.
      (else
       (let ((cell (or (var-cell-lookup (chez-current-ns) class)
                       (var-cell-lookup "clojure.core" class))))
         (if (and cell (var-cell-defined? cell) (procedure? (var-cell-root cell)))
             (apply (var-cell-root cell) args)
             (error #f (string-append "No constructor for class " class))))))))

;; ---- coercion helpers -------------------------------------------------------
;; numeric tower: currentTimeMillis/nanoTime are exact longs (JVM).
(define (->num x) x)
(define (jnum->exact n) (exact (truncate n)))
;; parse an integer string in radix; #f on failure
(define (parse-int-str s radix)
  (let ((n (string->number (str-trim (if (string? s) s (jolt-str-render-one s))) radix)))
    (and n (integer? n) (->num n))))
(define (parse-int-or-throw s radix what)
  (or (parse-int-str s radix)
      (error #f (string-append "NumberFormatException: For input string: \""
                               (if (string? s) s (jolt-str-render-one s)) "\""))))
(define (char-code c) (if (char? c) (char->integer c) (jnum->exact c)))

;; parse a double string (Double/parseDouble, (Double. s)); JVM accepts NaN /
;; Infinity / decimal / scientific. #f on failure.
(define (parse-double-str s)
  (let ((t (str-trim (if (string? s) s (jolt-str-render-one s)))))
    (cond
      ((or (string=? t "NaN") (string=? t "+NaN") (string=? t "-NaN")) +nan.0)
      ((or (string=? t "Infinity") (string=? t "+Infinity")) +inf.0)
      ((string=? t "-Infinity") -inf.0)
      (else (let ((n (string->number t))) (and n (real? n) (exact->inexact n)))))))
(define (parse-double-or-throw s)
  (or (parse-double-str s)
      (error #f (string-append "NumberFormatException: For input string: \""
                               (if (string? s) s (jolt-str-render-one s)) "\""))))
(define (->double x) (if (number? x) (exact->inexact x) (parse-double-or-throw x)))

