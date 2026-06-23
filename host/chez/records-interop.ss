;; records-interop.ss — JVM-emulation taxonomy split out of records.ss: the
;; ex-info class accessors, the exception supertype hierarchy, and instance-check
;; / case-string (the (instance? Class x) decision table). Loaded right after
;; records.ss; instance-check forward-refs nothing in records.ss at load time.

;; pmap? guard: ex-info maps are plain hash-maps, never sorted-map htables — and a
;; bare jolt-get on a sorted-map would invoke its comparator on :jolt/type and throw.
(define (ex-info-map? v)
  (and (pmap? v) (jolt=2 (jolt-get v jolt-kw-ex-type jolt-nil) jolt-kw-ex-info)))
(define (ex-info-class v)
  (let ((c (jolt-get v jolt-kw-class jolt-nil)))
    (if (string? c) c "clojure.lang.ExceptionInfo")))
;; immediate-parent chain of the JVM exception hierarchy (simple names). Drives
;; instance? across exception supertypes — (instance? Throwable (ex-info …)) etc.
(define exception-parent
  '(("ExceptionInfo" . "RuntimeException")
    ("RuntimeException" . "Exception")
    ("IllegalArgumentException" . "RuntimeException")
    ("NumberFormatException" . "IllegalArgumentException")
    ("IllegalStateException" . "RuntimeException")
    ("UnsupportedOperationException" . "RuntimeException")
    ("ArithmeticException" . "RuntimeException")
    ("NullPointerException" . "RuntimeException")
    ("ClassCastException" . "RuntimeException")
    ("IndexOutOfBoundsException" . "RuntimeException")
    ("ConcurrentModificationException" . "RuntimeException")
    ("NoSuchElementException" . "RuntimeException")
    ("UncheckedIOException" . "RuntimeException")
    ("InterruptedException" . "Exception")
    ("IOException" . "Exception")
    ("FileNotFoundException" . "IOException")
    ("UnsupportedEncodingException" . "IOException")
    ("UnknownHostException" . "IOException")
    ("SocketException" . "IOException")
    ("ConnectException" . "IOException")
    ("SocketTimeoutException" . "IOException")
    ("MalformedURLException" . "IOException")
    ("SSLException" . "IOException")
    ("Exception" . "Throwable")
    ("Error" . "Throwable")
    ("AssertionError" . "Error")
    ("Throwable" . "Object")))
;; Is `wanted` (simple name) `cls` or a supertype of it? ExceptionInfo also
;; implements the IExceptionInfo interface.
(define (exception-isa? cls wanted)
  (let loop ((c cls))
    (cond ((not c) #f)
          ((string=? c wanted) #t)
          ((and (string=? c "ExceptionInfo") (string=? wanted "IExceptionInfo")) #t)
          (else (let ((p (assoc c exception-parent))) (loop (and p (cdr p))))))))

;; instance-check: (type-sym val) — type/protocol membership. Host shims loaded
;; later (io, inst-time, natives-array, natives-queue, host-static-objects)
;; register an arm with register-instance-check-arm! instead of set!-wrapping
;; instance-check; an arm returns #t/#f to decide or 'pass to defer to the next.
;; Newest arm is checked first (matches the old outermost-wins set! order).
;; instance-check-base is the JVM taxonomy fallback when no arm decides.
(define instance-check-registry '())
(define (register-instance-check-arm! f)   ; f: (type-sym val) -> #t | #f | 'pass
  (set! instance-check-registry (cons f instance-check-registry)))

(define (instance-check-base type-sym val)
  (let ((tname (symbol-t-name type-sym)))
    (cond
      ((jrec? val)
       (let ((tag (jrec-tag val)))
         (or (string=? tag tname)
             (and (> (string-length tag) (string-length tname))
                  (string=? (substring tag (- (string-length tag) (string-length tname)) (string-length tag)) tname)))))
      ((jreify? val) (let ((short (last-dot tname)))
                       (and (memp (lambda (p) (string=? (last-dot p) short)) (jreify-protos val)) #t)))
      ((ex-info-map? val) (exception-isa? (last-dot (ex-info-class val)) (last-dot tname)))
      (else (case-string tname val)))))

(define (instance-check type-sym val)
  ;; normalize a bare (non-array) string class token to a symbol so every arm and
  ;; the base table can read its name; array tokens ("[I") stay strings for the
  ;; natives-array arm.
  (let ((ts (if (and (string? type-sym)
                     (or (= 0 (string-length type-sym))
                         (not (char=? (string-ref type-sym 0) #\[))))
                (jolt-symbol #f type-sym)
                type-sym)))
    (let loop ((rs instance-check-registry))
      (if (null? rs)
          (instance-check-base ts val)
          (let ((r ((car rs) ts val)))
            (if (eq? r 'pass) (loop (cdr rs)) r))))))
(define (case-string tname val)
  (cond
    ((member tname '("Number" "java.lang.Number")) (number? val))
    ((member tname '("Long" "java.lang.Long" "Integer" "java.lang.Integer"))
     (and (number? val) (exact? val) (integer? val)))
    ((member tname '("Double" "java.lang.Double" "Float" "java.lang.Float")) (and (number? val) (flonum? val)))
    ((member tname '("Ratio" "clojure.lang.Ratio")) (and (number? val) (exact? val) (rational? val) (not (integer? val))))
    ((member tname '("String" "java.lang.String" "CharSequence" "java.lang.CharSequence")) (string? val))
    ((member tname '("Boolean" "java.lang.Boolean")) (boolean? val))
    ((member tname '("Character" "java.lang.Character")) (char? val))
    ((member tname '("Keyword" "clojure.lang.Keyword")) (keyword? val))
    ((member tname '("Symbol" "clojure.lang.Symbol")) (jolt-symbol? val))
    ((member tname '("Atom" "clojure.lang.Atom")) (jolt-atom? val))
    ((member tname '("IFn" "clojure.lang.IFn" "Fn" "clojure.lang.Fn")) (procedure? val))
    ((member tname '("Pattern" "java.util.regex.Pattern")) (regex-t? val))
    ((member tname '("URI" "java.net.URI"))
     (and (jhost? val) (string=? (jhost-tag val) "uri")))
    ((member tname '("File" "java.io.File")) (jfile? val))
    ((member tname '("UUID" "java.util.UUID")) (juuid? val))
    (else #f)))

;; str of a record uses a custom (Object toString) impl if the type defines one
;; (deftype with no default toString relies on this); otherwise the map form
;; without the leading # (Clojure's record .toString). converters.ss loads before
;; records.ss, so this set! sees the registry — forward refs resolve at call time.

(def-var! "clojure.core" "instance-check" instance-check)
