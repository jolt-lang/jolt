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
;; Is `wanted` (simple name) `cls` or a supertype of it? The exception hierarchy
;; lives in the one class graph (class-hierarchy.ss) — resolve the simple name to
;; its graph key and ask jch-isa?, so exceptions and every other class share a
;; single source of truth (ExceptionInfo -> IExceptionInfo is a graph edge).
(define (exception-isa? cls wanted)
  (jch-isa? (jch-fqn-of-simple cls) wanted))

;; A raw Chez condition (an arity or non-seqable error Chez itself raised, not a
;; jolt ex-info) carries no jolt exception class. Map the ones Clojure raises a
;; specific class for, by message, so (class e) and (instance? C e) match the JVM.
;; Returns a simple class name or #f.
(define (ri-substring? needle hay)
  (let ((nl (string-length needle)) (hl (string-length hay)))
    (let loop ((i 0))
      (cond ((> (+ i nl) hl) #f)
            ((string=? needle (substring hay i (+ i nl))) #t)
            (else (loop (+ i 1)))))))
(define (chez-condition-exc-class v)
  (and (condition? v) (message-condition? v)
       (let ((m (condition-message v)))
         (and (string? m)
              (cond ((ri-substring? "incorrect number of arguments" m) "ArityException")
                    ((ri-substring? "not seqable" m) "IllegalArgumentException")
                    ;; Chez's numeric ops raise "~s is not a real number" on a bad
                    ;; operand. The JVM throws NullPointerException for a nil operand
                    ;; (null deref) and ClassCastException for a non-number (can't
                    ;; cast to Number) — clojure.spec.alpha's conform-explain relies
                    ;; on the distinction. The offending value rides in the irritants.
                    ((or (ri-substring? "is not a real number" m)
                         (ri-substring? "is not a number" m))
                     (if (and (irritants-condition? v)
                              (let loop ((xs (condition-irritants v)))
                                (and (pair? xs) (or (jolt-nil? (car xs)) (loop (cdr xs))))))
                         "NullPointerException"
                         "ClassCastException"))
                    (else #f))))))

;; instance-check: (type-sym val) — type/protocol membership. Host shims loaded
;; later (io, inst-time, natives-array, natives-queue, host-static-classes)
;; register an arm with register-instance-check-arm! instead of set!-wrapping
;; instance-check; an arm returns #t/#f to decide or 'pass to defer to the next.
;; Newest arm is checked first (matches the old outermost-wins set! order).
;; instance-check-base is the JVM taxonomy fallback when no arm decides.
(define instance-check-registry '())
(define (register-instance-check-arm! f)   ; f: (type-sym val) -> #t | #f | 'pass
  (set! instance-check-registry (cons f instance-check-registry)))

;; (instance? C raw-condition): match when C is the condition's mapped class or a
;; supertype of it (ArityException is also an IllegalArgumentException, etc.).
(register-instance-check-arm!
  (lambda (type-sym val)
    (let ((k (chez-condition-exc-class val)))
      (if k (if (exception-isa? k (last-dot (symbol-t-name type-sym))) #t #f) 'pass))))

;; Object / java.lang.Object is the root of the type hierarchy: every non-nil
;; value is an instance of Object; nil is not an instance of anything.
(register-instance-check-arm!
  (lambda (type-sym val)
    (let ((tn (symbol-t-name type-sym)))
      (if (or (string=? tn "Object") (string=? tn "java.lang.Object"))
          (not (jolt-nil? val))
          'pass))))

(define (instance-check-base type-sym val)
  (let ((tname (symbol-t-name type-sym)))
    (cond
      ((jrec? val)
       (let ((tag (jrec-tag val)))
         (or (string=? tag tname)
             ;; a simple name matches a qualified tag only at a `.` boundary:
             ;; "a.b.IntervalFD" is an IntervalFD, but "a.b.MultiIntervalFD" is NOT
             ;; (a raw string-suffix would wrongly match the latter).
             (let ((tl (string-length tag)) (nl (string-length tname)))
               (and (fx>? tl nl)
                    (char=? (string-ref tag (fx- (fx- tl nl) 1)) #\.)
                    (string=? (substring tag (fx- tl nl) tl) tname)))
             ;; a protocol/interface the type implements (defprotocol generates an
             ;; interface; (instance? SomeProtocol record) is true when the record
             ;; implements it — core.match dispatches on instance? IPatternCompile).
             (type-satisfies? tag tname)
             (type-satisfies? tag (last-dot tname)))))
      ((jreify? val) (let ((short (last-dot tname)))
                       ;; every Clojure reify implements IObj/IMeta (carries metadata).
                       (or (member short '("IObj" "IMeta"))
                           (and (memp (lambda (p) (string=? (last-dot p) short)) (jreify-protos val)) #t))))
      ((ex-info-map? val) (exception-isa? (last-dot (ex-info-class val)) (last-dot tname)))
      (else (case-string tname val)))))

(define (instance-check type-sym0 val)
  ;; a Class value as the type arg (instance? (class x) y) -> use its name string.
  (let* ((type-sym (if (jclass? type-sym0) (jclass-name type-sym0) type-sym0))
         (ts (if (and (string? type-sym)
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
    ;; clojure.lang.IPending — the realized?-able types (Promise/Future/Delay/
    ;; LazySeq all implement isRealized on the JVM). A tap> that hands a promise to
    ;; a tap fn relies on this so the fn can deliver it.
    ((member tname '("IPending" "clojure.lang.IPending"))
     (or (jolt-promise? val) (jolt-future? val) (jolt-delay? val) (jolt-lazyseq? val)))
    (else #f)))

;; str of a record uses a custom (Object toString) impl if the type defines one
;; (deftype with no default toString relies on this); otherwise the map form
;; without the leading # (Clojure's record .toString). converters.ss loads before
;; records.ss, so this set! sees the registry — forward refs resolve at call time.

(def-var! "clojure.core" "instance-check" instance-check)

;; Broad-catch fallback for catch-clause dispatch (analyze-try desugars
;; (catch C e …) to (or (instance? C e) (__catch-broad? "C" e))). A jolt host
;; condition or a raw raised value carries no jolt exception class, so instance?
;; can't place it; a Clojure (catch C e) over such a value matches when C is
;; RuntimeException (or a subclass) / Exception / Throwable — most host runtime
;; errors are RuntimeExceptions. Typed throwables (ex-info, (SomeException. …)) are
;; recognized by instance? as Throwable, so untyped? is false and they dispatch
;; precisely through the instance? arm instead.
(define throwable-type-sym (jolt-symbol #f "Throwable"))
(define (simple-class-name nm)
  (let loop ((i (- (string-length nm) 1)))
    (cond ((< i 0) nm)
          ((char=? (string-ref nm i) #\.) (substring nm (+ i 1) (string-length nm)))
          (else (loop (- i 1))))))
(define (jolt-catch-broad? nm v)
  (and (not (instance-check throwable-type-sym v))
       (let ((s (simple-class-name nm)))
         (or (exception-isa? s "RuntimeException")
             (string=? s "Exception")
             (string=? s "Throwable")))))
(def-var! "clojure.core" "__catch-broad?"
  (lambda (nm v) (if (jolt-catch-broad? nm v) #t #f)))
