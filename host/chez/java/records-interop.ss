;; records-interop.ss — JVM-emulation taxonomy split out of records.ss: the
;; ex-info class accessors, the exception supertype hierarchy, and instance-check
;; / case-string (the (instance? Class x) decision table). Loaded right after
;; records.ss; instance-check forward-refs nothing in records.ss at load time.

;; jolt-ex-info-record predicate (defined in rt.ss).
(define (ex-info-map? v)
  (jolt-ex-info-record? v))
(define (ex-info-class v)
  (jolt-ex-info-record-class-name v))
;; Is `wanted` (simple name) `cls` or a supertype of it? The exception hierarchy
;; lives in the one class graph (class-hierarchy.ss) — resolve the simple name to
;; its graph key and ask jch-isa?, so exceptions and every other class share a
;; single source of truth (ExceptionInfo -> IExceptionInfo is a graph edge).
(define (exception-isa? cls wanted)
  (jch-isa? (jch-fqn-of-simple cls) wanted))

;; A raw Chez condition (an arity or non-seqable error Chez itself raised) carries
;; no jolt exception class. All operation sites now throw typed jolt throwables
;; (ArityException, IllegalArgumentException, ClassCastException, etc.) BEFORE
;; Chez can raise a raw condition. Any raw condition that still escapes is a
;; runtime error — classify it as RuntimeException.
;; instance-check: (type-sym val) — type/protocol membership. Host shims loaded
;; later (io, inst-time, natives-array, natives-queue, host-static-classes)
;; register an arm with register-instance-check-arm! instead of set!-wrapping
;; instance-check; an arm returns #t/#f to decide or 'pass to defer to the next.
;; Newest arm is checked first (matches the old outermost-wins set! order).
;; instance-check-base is the JVM taxonomy fallback when no arm decides.
(define instance-check-registry '())
(define (register-instance-check-arm! f)   ; f: (type-sym val) -> #t | #f | 'pass
  (set! instance-check-registry (cons f instance-check-registry)))

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
             (type-implements-class? tag tname)
             ;; the class graph: a declared interface's own ancestry answers too
             ;; (IPersistentMap is an Associative is an IPersistentCollection).
             (jch-isa? tag tname))))
      ((jreify? val) (let ((short (last-dot tname)))
                       ;; every Clojure reify implements IObj/IMeta (carries metadata).
                       (or (member short '("IObj" "IMeta"))
                           (and (memp (lambda (p) (proto-class-match? p tname))
                                      (jreify-protos val))
                                #t))))
      ((ex-info-map? val) (exception-isa? (last-dot (ex-info-class val)) (last-dot tname)))
      (else (case-string tname val)))))

(define (instance-check type-sym0 val)
  ;; a Class value as the type arg (instance? (class x) y) -> use its name string.
  ;; A deftype/defrecord type token is its make-deftype-ctor closure; use the tag
  ;; ("ns.Name") it carries so (instance? Bar x) works when Bar is passed by value
  ;; (schema's record*/class-schema hold the type as a value, not a literal symbol).
  (let* ((type-sym (cond ((jclass? type-sym0) (jclass-name type-sym0))
                         ((and (procedure? type-sym0)
                               (hashtable-ref chez-deftype-ctor-tag type-sym0 #f)))
                         (else type-sym0)))
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

;; ---- java.lang.Throwable: the surface EVERY throwable inherits ---------------
;; On the JVM these are declared on Throwable itself, so every exception class has
;; them by inheritance and no shim ever needs to restate them. jolt had no such
;; place: the method bodies were duplicated across the `condition?` arm in
;; records.ss and dot-object-method in dot-forms.ss, which is exactly how the two
;; drifted — .printStackTrace existed on a raw Chez condition and was missing on
;; every ex-info / (Exception. …) / typed host throwable, and .getLocalizedMessage
;; the other way round. This is the single table both paths call.
;;
;; jolt models a throwable two ways: a jolt-ex-info-record (ex-info, and every
;; typed host throwable via jolt-host-throwable) or a raw Chez condition (an error
;; the host itself raised). Both answer here.
;;
;; Returns a BOXED result (a one-element list) or #f for "not a Throwable method",
;; matching dot-object-method — a legitimate nil/#f result has to stay
;; distinguishable from "no such method".
(define (jolt-throwable-value? v)
  (or (jolt-ex-info-record? v) (condition? v)))

(define (jolt-throwable-message v)
  (cond ((jolt-ex-info-record? v) (jolt-ex-info-record-message v))
        ((condition? v) (condition->message-string v))
        (else jolt-nil)))

;; "class: message", the JVM Throwable.toString. jolt-str-render-one already
;; renders an ex-info record that way (source-registry.ss); a raw condition has no
;; class of its own, so its message stands alone.
(define (jolt-throwable-tostring v)
  (if (condition? v) (condition->message-string v) (jolt-str-render-one v)))

;; Throwable.printStackTrace: the header line, then the same Clojure backtrace the
;; uncaught reporter prints. Called from a catch clause the throw's captured
;; continuation is still live (the emitted catch runs jolt-catch-complete! only
;; AFTER the body), so the trace is the one that led to this throwable.
;; jolt-backtrace-string lives in source-registry.ss, which loads after this file —
;; a top-level forward reference, resolved at call time.
(define (jolt-throwable-print-stack-trace v port)
  (display (jolt-throwable-tostring v) port)
  (newline port)
  (let ((bt (guard (e (#t #f)) (jolt-backtrace-string v))))
    (when bt (display bt port)))
  jolt-nil)

;; The target of a 1-arg .printStackTrace — a PrintStream / PrintWriter shim. Route
;; through the target's own .write, so any writer (io.ss's, a library's) works.
;; rest-args is a JOLT seq, not a Scheme list: passing a raw list made every
;; dispatch miss. A target with no .write raises from there, which is the right
;; report — swallowing it and quietly printing to stderr instead is what hid this.
(define (jolt-throwable-print-to v target)
  (let ((s (let ((p (open-output-string)))
             (jolt-throwable-print-stack-trace v p)
             (get-output-string p))))
    (record-method-dispatch target "write" (jolt-list s))
    jolt-nil))

(define (throwable-method obj name args)
  (cond
    ((or (string=? name "getMessage") (string=? name "getLocalizedMessage"))
     (list (jolt-throwable-message obj)))
    ((string=? name "toString") (list (jolt-throwable-tostring obj)))
    ((string=? name "getCause")
     (list (if (jolt-ex-info-record? obj) (jolt-ex-info-record-cause obj) jolt-nil)))
    ;; java.sql.SQLException chaining — jolt throwables don't chain.
    ((string=? name "getNextException") (list jolt-nil))
    ;; java.text.ParseException.getErrorOffset — the int its ctor stashed.
    ((string=? name "getErrorOffset")
     (list (if (jolt-ex-info-record? obj) (jolt-ex-info-record-error-offset obj) 0)))
    ;; jolt reifies no StackTraceElement array: TCO erases caller frames, so there
    ;; is no faithful per-frame array to hand back. Empty, like a JVM throwable
    ;; whose stack trace has been stripped. The real frames are what
    ;; printStackTrace renders, and what an uncaught error reports.
    ((string=? name "getStackTrace") (list (jolt-vector)))
    ;; jolt never suppresses: an empty array is the JVM's own answer for a
    ;; throwable with nothing suppressed, so this is exact rather than a stand-in.
    ((string=? name "getSuppressed") (list (jolt-vector)))
    ;; JVM contract is "returns this"; jolt has no stack to refill.
    ((string=? name "fillInStackTrace") (list obj))
    ((string=? name "printStackTrace")
     (list (if (pair? args)
               (jolt-throwable-print-to obj (car args))
               (jolt-throwable-print-stack-trace obj (current-error-port)))))
    (else #f)))

;; Broad-catch fallback for catch-clause dispatch (analyze-try desugars
;; (catch C e …) to (or (instance? C e) (__catch-broad? "C" e))). A jolt host
;; condition or a raw raised value carries no jolt exception class, so instance?
;; can't place it; a Clojure (catch C e) over such a value matches when C is
;; RuntimeException (or a subclass) / Exception / Throwable — most host runtime
;; errors are RuntimeExceptions. Typed throwables (ex-info records, (SomeException. …))
;; are recognized by instance? as Throwable, so untyped? is false and they dispatch
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
