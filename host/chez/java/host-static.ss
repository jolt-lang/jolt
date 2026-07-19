;; host-static.ss — the host-interop registry core: the class-statics / class-ctors
;; / tagged-methods tables, the jhost record, and the coercion helpers. The actual
;; entries are registered by host-static-methods.ss (Class/member statics) and
;; host-static-classes.ss (instantiable object classes), loaded after this.
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

;; Does `nm` name a registered host class (has statics or a constructor)? The
;; analyzer's contract layer asks this to treat a bare Capitalized symbol as a
;; class; exposing it keeps the registry tables private to the java layer.
(define (host-class-registered? nm)
  (or (and (hashtable-ref class-statics-tbl nm #f) #t)
      (and (hashtable-ref class-ctors-tbl nm #f) #t)))
;; narrower: registered with STATICS (not just a constructor) — an imported class
;; short name used as a static-call target, distinct from a deftype's bare name.
(define (host-class-has-statics? nm) (and (hashtable-ref class-statics-tbl nm #f) #t))

;; A class token may arrive fully qualified (java.io.StringReader) or short
;; (StringReader). Register both; resolve by exact then by last dotted segment.
(define (short-class-name s)
  (let loop ((i (- (string-length s) 1)))
    (cond ((< i 0) s)
          ((char=? (string-ref s i) #\.) (substring s (+ i 1) (string-length s)))
          (else (loop (- i 1))))))

;; A member re-registered with a DIFFERENT value across files is drift (two
;; sources fighting over one static, last-wins silently deciding). This is a
;; diagnostic for the Pattern/compile+quote class of bug, but it ALSO fires when
;; two libraries legitimately shim the same class (jolt-crypto + http-client both
;; provide javax.crypto.Cipher/getInstance, etc.) — routine, not a bug. Gate it
;; behind JOLT_DEBUG so a normal run stays quiet (issue #422); set JOLT_DEBUG to
;; surface real drift. Registering the same member object twice (the FQN+short
;; double-register below, or a value equal? to the prior one) is never a collision.
(define (registry-collision! kind class member old new)
  (when (and (getenv "JOLT_DEBUG") (not (eq? old new)) (not (equal? old new)))
    (fprintf (current-error-port)
             "warning: ~a member ~a/~a registered twice with different values\n"
             kind class member)))

(define (register-class-statics! name members)  ; members: list of (str . val/proc)
  (let* ((short (short-class-name name))
         (h (or (hashtable-ref class-statics-tbl name #f)
                (hashtable-ref class-statics-tbl short #f)
                (let ((h (make-hashtable string-hash string=?)))
                  h))))
    ;; Both the FQN and short name share the same member table — registration
    ;; under either name lands in the merged table, so re-registrations under one
    ;; name are visible through the other.
    (hashtable-set! class-statics-tbl name h)
    (unless (string=? name short)
      (hashtable-set! class-statics-tbl short h))
    (for-each (lambda (p)
                (let ((old (hashtable-ref h (car p) #f)))
                  (when old (registry-collision! "static" name (car p) old (cdr p))))
                (hashtable-set! h (car p) (cdr p)))
              members)))

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
;; clojure.lang.Sorted on jolt's sorted-map / sorted-set: comparator / entryKey /
;; seqFrom / seq. data.priority-map's subseq/rsubseq reach for these (its
;; PersistentPriorityMap delegates .comparator to the backing sorted-map). The
;; comparator is returned as a small Comparator object whose .compare runs the
;; map's 3-way fn, since (.. sc comparator (compare a b)) is the calling form.
(define sorted-cmp-kw (keyword #f "cmp"))
(register-host-methods! "jolt-comparator"
  (list (cons "compare" (lambda (self a b) (jolt-invoke (jhost-state self) a b)))))
(define (sorted-comparator-of sc)
  (let ((c (jolt-ref-get sc sorted-cmp-kw)))
    (make-jhost "jolt-comparator" (if (jolt-nil? c) jolt-compare c))))
(define (sorted-iface-method? m)
  (or (string=? m "comparator") (string=? m "entryKey")
      (string=? m "seqFrom") (string=? m "seq")))
(define (sorted-iface-dispatch obj method rest)
  (cond
    ((string=? method "comparator") (sorted-comparator-of obj))
    ((string=? method "entryKey") (jolt-first (car rest)))   ; map entry -> its key
    ((string=? method "seq")                                 ; (.seq sc) or (.seq sc ascending?)
     (if (or (null? rest) (jolt-truthy? (car rest))) (jolt-seq obj) (jolt-rseq obj)))
    ;; (.seqFrom sc k ascending?) — the entries from k onward, in order. Done with a
    ;; comparator filter over the seq (jolt has no tree cursor), like subseq.
    ((string=? method "seqFrom")
     (let* ((k (car rest)) (asc (jolt-truthy? (cadr rest)))
            (cmp (jolt-ref-get obj sorted-cmp-kw))
            (cmpf (if (jolt-nil? cmp) jolt-compare cmp))
            (es (seq->list (jolt-seq obj)))
            (keep (filter (lambda (e)
                            (let ((c (jnum->exact (jolt-invoke cmpf (jolt-first e) k))))
                              (if asc (>= c 0) (<= c 0))))
                          es)))
       (list->cseq (if asc keep (reverse keep)))))
    (else (throw-jvm (quote IllegalArgumentException) (string-append "No matching method " method " on sorted collection")))))

(register-method-arm! arm-priority-host-type
  (lambda (obj method-name rest-args)
    (cond
      ((jhost? obj)
       (let ((mh (hashtable-ref host-methods-tbl (jhost-tag obj) #f)))
         (let ((f (and mh (hashtable-ref mh method-name #f))))
           (if f
               (apply f obj (if (jolt-nil? rest-args) '() (seq->list rest-args)))
               (throw-jvm (quote IllegalArgumentException) (string-append "No matching method " method-name " for " (jhost-tag obj)))))))
      ((number? obj) (apply number-method method-name obj (if (jolt-nil? rest-args) '() (seq->list rest-args))))
      (else 'pass))))

;; java.lang.Number method surface (the boxed-number methods cljc code calls). The
;; integer projections wrap modulo their width (ring-codec relies on byteValue
;; overflow: (.byteValue 255) => -1); the float projections are identity flonums.
(define (number-method method n . args)
  (cond
    ((string=? method "byteValue") (let ((b (modulo (jnum->exact n) 256))) (->num (if (>= b 128) (- b 256) b))))
    ((string=? method "shortValue") (let ((b (modulo (jnum->exact n) 65536))) (->num (if (>= b 32768) (- b 65536) b))))
    ((string=? method "intValue") (->num (jnum->exact n)))
    ((string=? method "longValue") (->num (jnum->exact n)))
    ((string=? method "doubleValue") (->num n))
    ((string=? method "floatValue") (->num n))
    ;; .toString(radix) — BigInteger/Integer render in a base, lowercase like the
    ;; JVM (rewrite-clj's integer node reconstructs 0xff / 0377 / 2r1001 this way).
    ((string=? method "toString")
     (if (pair? args)
         (string-downcase (number->string (jnum->exact n) (jnum->exact (car args))))
         (jolt-num->string n)))
    ((string=? method "hashCode") (->num (jnum->exact n)))
    ;; Double/Float .isNaN / .isInfinite (a non-flonum is neither).
    ((string=? method "isNaN") (and (flonum? n) (not (= n n))))
    ((string=? method "isInfinite") (and (flonum? n) (infinite? n)))
    ;; BigInteger interop: .negate / .bitLength / .signum / .abs. A jolt integer is
    ;; a Chez exact integer, so these are native (integer-length = JVM bitLength,
    ;; matching for negative values too). tools.reader's number parser uses them.
    ((string=? method "negate") (->num (- (jnum->exact n))))
    ((string=? method "abs") (->num (abs (jnum->exact n))))
    ((string=? method "bitLength") (->num (integer-length (jnum->exact n))))
    ((string=? method "signum") (->num (let ((e (jnum->exact n))) (cond ((> e 0) 1) ((< e 0) -1) (else 0)))))
    ;; BigInteger.shiftLeft/shiftRight (test.check's size-bounded-bigint): arbitrary
    ;; precision, so an arithmetic shift by the (positive) amount.
    ((string=? method "shiftLeft") (->num (bitwise-arithmetic-shift-left (jnum->exact n) (jnum->exact (car args)))))
    ((string=? method "shiftRight") (->num (bitwise-arithmetic-shift-right (jnum->exact n) (jnum->exact (car args)))))
    (else (throw-jvm (quote IllegalArgumentException) (string-append "No matching method " method " for Number")))))

;; Mutable static fields: "Class" -> (member -> 1-vector cell). A library that
;; writes a static field — clojure.spec.alpha's (set! (. clojure.lang.RT
;; checkSpecAsserts) flag) — lands here; the analyzer lowers the set! to a
;; set-static-field! call and a plain Class/member read consults the cell first.
(define mutable-statics-tbl (make-hashtable string-hash string=?))
(define (mutable-static-cell class member create?)
  (let ((h (or (hashtable-ref mutable-statics-tbl class #f)
               (and create? (let ((nh (make-hashtable string-hash string=?)))
                              (hashtable-set! mutable-statics-tbl class nh) nh)))))
    (and h (or (hashtable-ref h member #f)
               (and create? (let ((c (vector jolt-nil))) (hashtable-set! h member c) c))))))
(def-var! "jolt.host" "set-static-field!"
  (lambda (class member val)
    (vector-set! (mutable-static-cell class member #t) 0 val)
    val))
;; clojure.lang.RT.checkSpecAsserts — a JVM-internal flag clojure.spec.alpha reads
;; and writes; default false. Pre-seed the cell so a read before any write works.
(vector-set! (mutable-static-cell "clojure.lang.RT" "checkSpecAsserts" #t) 0 #f)

;; ---- emit entry points ------------------------------------------------------
(define (host-static-ref class member)
  (let ((cell (mutable-static-cell class member #f)))
    (if cell
        (vector-ref cell 0)
        (let ((h (lookup-class class-statics-tbl class)))
          (if h
              (let ((v (hashtable-ref h member #f)))
                (if v v (throw-jvm (quote IllegalArgumentException) (string-append "No matching field or method: " class "/" member))))
              (throw-jvm (quote IllegalArgumentException) (string-append "Unknown class " class)))))))

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
             (throw-jvm (quote IllegalArgumentException) (string-append "No matching ctor found for class " class))))))))

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
      (jolt-throw (jolt-host-throwable "java.lang.NumberFormatException"
                    (string-append "For input string: \""
                                   (if (string? s) s (jolt-str-render-one s)) "\"")))))
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
      (jolt-throw (jolt-host-throwable "java.lang.NumberFormatException"
                    (string-append "For input string: \""
                                   (if (string? s) s (jolt-str-render-one s)) "\"")))))
(define (->double x) (if (number? x) (exact->inexact x) (parse-double-or-throw x)))

