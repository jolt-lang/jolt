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

;; Names the HOST registered (io.ss, io-streams.ss, …), as opposed to a library
;; registering a class jolt does not model. Only the host's own boot-time calls
;; land here — the Clojure-visible hook goes through register-class-ctor-user!.
(define host-class-ctors-tbl (make-hashtable string-hash string=?))

(define (register-class-ctor! name proc)
  (hashtable-set! host-class-ctors-tbl name #t)
  (hashtable-set! class-ctors-tbl name proc))

;; clojure.core/__register-class-ctor! lands here. Registering a class jolt does
;; not model is the intended use; REPLACING one it does is a process-wide
;; substitution that every other namespace silently inherits, and the symptoms
;; are remote from the cause — jolt-lang/http-client swaps its own tagged-table
;; shim in for java.io.ByteArrayInputStream, and any library loaded alongside it
;; then finds (.readAllBytes body) unresolvable and io/copy ~3600x slower, with
;; nothing pointing at the override. Report it under JOLT_DEBUG the way
;; register-class-statics! reports a colliding static, so the cause is one env
;; var away instead of a bisect.
(define (register-class-ctor-user! name proc)
  (when (and (getenv "JOLT_DEBUG") (hashtable-ref host-class-ctors-tbl name #f))
    (fprintf (current-error-port)
             "warning: a library replaced the host constructor for ~a — every (~a. ...) in this process now builds its shim, including in namespaces that never asked for it\n"
             name name))
  (hashtable-set! class-ctors-tbl name proc))

(define (register-host-methods! tag members)
  (let ((h (or (hashtable-ref host-methods-tbl tag #f)
               (let ((h (make-hashtable string-hash string=?)))
                 (hashtable-set! host-methods-tbl tag h) h))))
    (for-each (lambda (p) (hashtable-set! h (car p) (cdr p))) members)))

;; The comparator seam (natives-seq.ss jolt-comparator-fn) asks whether a value
;; is a shim object whose tag registers a `compare` method — a Comparator held
;; by the host (String/CASE_INSENSITIVE_ORDER) rather than by a deftype/reify.
(set! jhost-compare-method?
  (lambda (x)
    (and (jhost? x)
         (let ((h (hashtable-ref host-methods-tbl (jhost-tag x) #f)))
           (and h (hashtable-ref h "compare" #f) #t)))))

;; Point a second tag at an existing tag's method table, sharing the table itself
;; rather than copying it. A shim that differs from another ONLY in the class it
;; reports — clojure.lang.LineNumberingPushbackReader over java.io.PushbackReader
;; — needs its own tag so value-host-tags names the right class, but must not get
;; its own copy of the methods: a later register-host-methods! on either tag would
;; then reach only one of them, and the two would silently drift apart.
(define (alias-host-methods! tag from)
  (let ((h (or (hashtable-ref host-methods-tbl from #f)
               (error 'alias-host-methods! "no methods registered for tag" from))))
    (hashtable-set! host-methods-tbl tag h)))

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
    (else (dispatch-miss obj method rest))))

(register-method-arm! arm-priority-host-type
  (lambda (obj method-name rest-args)
    (cond
      ((jhost? obj)
       (let* ((mh (hashtable-ref host-methods-tbl (jhost-tag obj) #f))
              (f (and mh (hashtable-ref mh method-name #f)))
              (args (if (jolt-nil? rest-args) '() (seq->list rest-args))))
         (cond
           (f (apply f obj args))
           ;; (. Foo bar args) where Foo names a class is a STATIC call — that is
           ;; what the form means on the JVM. A dotted name resolves as a class at
           ;; analysis time, but an IMPORTED simple name evaluates to a class
           ;; token and arrives here as a method call on it, so anything Class
           ;; itself does not answer is a static of the class the token names.
           ;; Routing it through host-static-ref also picks up the on-demand class
           ;; autoload, which the slash form (Foo/bar) always had and this form did
           ;; not: (. LocalDate parse s) threw "No matching method parse for class"
           ;; unless some earlier slash-form call happened to have loaded the
           ;; provider. time-literals' data readers are written in exactly this
           ;; form, so #time/date could not read at all.
           ((string=? (jhost-tag obj) "class")
            ;; A leading dash is the explicit FIELD spelling — (. Token -MIN)
            ;; lands here when the token came through a local or a cold import,
            ;; and used to look up "-MIN" verbatim. And a field VALUE answers a
            ;; zero-argument access instead of being applied as a procedure —
            ;; the same rule jolt.host/static-member and host-static-call use.
            (let* ((mname (if (and (> (string-length method-name) 1)
                                   (char=? (string-ref method-name 0) #\-))
                              (substring method-name 1 (string-length method-name))
                              method-name))
                   (v (host-static-ref (jclass-name obj) mname)))
              (cond ((procedure? v) (apply v args))
                    ((null? args) v)
                    (else (throw-jvm (quote IllegalArgumentException)
                            (string-append (jclass-name obj) "/" mname
                                           " is a static field; it takes no arguments"))))))
           ;; the shared end of the chain, so a host object reports its real class
           ;; rather than its internal tag, and a (.-x obj) read that no registered
           ;; member claimed reads as a missing FIELD like it does everywhere else
           (else (dispatch-miss obj method-name args)))))
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
    (else (dispatch-miss n method args))))

;; Mutable static fields: "Class" -> (member -> 1-vector cell). A library that
;; writes a static field — clojure.spec.alpha's (set! (. clojure.lang.RT
;; checkSpecAsserts) flag) — lands here; the analyzer lowers the set! to a
;; set-static-field! call and a plain Class/member read consults the cell first.
;; A set! of a mutable static runs at RUN time from any thread. Both
;; check-then-creates go under one mutex: split, two threads setting different
;; members of the same class each build their own inner table and one member's
;; cell is dropped with it, so a later read of that member sees nil forever. The
;; cell itself is a mutable vector whose slot is written outside the lock, which
;; is a whole-value write and needs none — this only has to guarantee that both
;; writers reach the SAME cell.
(define mutable-statics-mu (make-mutex))
(define mutable-statics-tbl (make-hashtable string-hash string=?))
(define (mutable-static-cell class member create?)
  (if create?
      (jolt-with-mutex mutable-statics-mu
        (let ((h (or (hashtable-ref mutable-statics-tbl class #f)
                     (let ((nh (make-hashtable string-hash string=?)))
                       (hashtable-set! mutable-statics-tbl class nh) nh))))
          (or (hashtable-ref h member #f)
              (let ((c (vector jolt-nil))) (hashtable-set! h member c) c))))
      (let ((h (hashtable-ref mutable-statics-tbl class #f)))
        (and h (hashtable-ref h member #f)))))
(def-var! "jolt.host" "set-static-field!"
  (lambda (class member val)
    (vector-set! (mutable-static-cell class member #t) 0 val)
    val))
;; clojure.lang.RT.checkSpecAsserts — a JVM-internal flag clojure.spec.alpha reads
;; and writes; default false. Pre-seed the cell so a read before any write works.
(vector-set! (mutable-static-cell "clojure.lang.RT" "checkSpecAsserts" #t) 0 #f)

;; ---- autoload the java.time base on first use -------------------------------
;; ---- host-class providers (RFC 0014) ----------------------------------------
;; A JVM library reaches for MessageDigest/getInstance or java.sql.ResultSet
;; without requiring an install namespace first, because on the JVM the class is
;; simply there. Jolt has no such class, so something has to load the namespace
;; that installs it — on the FIRST reference, before anything of the provider has
;; loaded. That ordering is why a provider cannot just register itself as it
;; loads: nothing has loaded it yet.
;;
;; So the mapping class -> provider arrives as DATA, ahead of the load. A library
;; declares it in its own deps.edn and jolt.deps collects it through the walk it
;; already does for :jolt/native:
;;
;;     :jolt/provides {jolt.crypto ["java.security.MessageDigest" ...]}
;;
;; The runtime keeps no list of libraries. What it keeps below is jolt's OWN
;; stdlib — core declaring the classes core implements, which is a different
;; thing from core knowing what jolt-lang/db is. Everything else is registered
;; through register-class-provider! from the resolved dependency graph.
;;
;; Each entry is #(install-ns coordinate (class-name ...) done-latch); coordinate
;; is #f for jolt's own stdlib, which needs no dependency to be added.
(define (class-simple-name c)
  (let loop ((i (- (string-length c) 1)))
    (cond ((< i 0) c)
          ((char=? (string-ref c i) #\.) (substring c (+ i 1) (string-length c)))
          (else (loop (- i 1))))))
;; A class is claimed under BOTH spellings: a token arrives fully qualified
;; (java.time.LocalDate) or simple (LocalDate) — jolt has no import map, so both
;; reach here. Deriving the simple form is what removes the hand-sync failure the
;; old hardcoded table kept hitting: it listed both spellings by hand, and a name
;; present only as the qualified one failed for the IMPORTED SIMPLE form, which is
;; how libraries actually write it.
(define (class-spellings classes)
  (let loop ((cs classes) (acc '()))
    (if (null? cs)
        (reverse acc)
        (let* ((c (car cs)) (simple (class-simple-name c)))
          (loop (cdr cs)
                (if (string=? simple c) (cons c acc) (cons simple (cons c acc))))))))

;; Core's own stdlib. jolt.time.base is the base java.time VALUE types (RFC 0008)
;; that must resolve with no explicit require; jolt.socket is java.net over POSIX
;; sockets. Both ship with jolt, so neither names a coordinate: there is no
;; dependency for a caller to add, and the autoload always finds them.
;;
;; Everything that FORMATS or names a zone — DateTimeFormatter, ZoneId,
;; ZonedDateTime, Locale, ... — is the jolt-lang/time LIBRARY and is declared by
;; that library, not here.
(define core-class-providers
  (list
   (vector "jolt.time.base" #f
           (class-spellings
            '("java.time.Instant" "java.time.LocalDate" "java.time.LocalTime"
              "java.time.LocalDateTime" "java.time.Duration" "java.time.Period"
              "java.time.Year" "java.time.YearMonth" "java.time.MonthDay"
              "java.time.Month" "java.time.DayOfWeek"
              "java.time.temporal.ChronoUnit" "java.time.temporal.ChronoField"
              "java.time.temporal.ValueRange" "java.time.temporal.TemporalAdjusters"))
           (box #f))
   (vector "jolt.socket" #f
           (class-spellings
            '("java.net.InetAddress" "java.net.Inet4Address"
              "java.net.NetworkInterface" "java.net.Socket"
              "java.net.ServerSocket" "java.net.InetSocketAddress"))
           (box #f))))
(define lib-class-providers core-class-providers)

;; Declared providers from the dependency graph, installed at startup by
;; jolt.deps. A claim on a class the runtime already IMPLEMENTS is refused: a
;; dependency does not get to redefine what String means, and the same posture is
;; why extend-class! will not replace a built-in.
;;
;; "Implements" is the statics/ctor tables, not the class hierarchy. The
;; hierarchy knows names it does not implement — java.time.ZoneId is in it so
;; isa?/instance? answer correctly, while the implementation is jolt-lang/time's
;; to install. Checking the hierarchy rejected every provider for the classes it
;; exists to provide.
(define (register-class-provider! install-ns coordinate classes)
  (let* ((cs (class-spellings classes))
         (taken (filter (lambda (c) (or (hashtable-ref class-statics-tbl c #f)
                                        (hashtable-ref class-ctors-tbl c #f)))
                        cs)))
    (if (pair? taken)
        (jolt-throw
         (jolt-host-throwable
          "java.lang.IllegalArgumentException"
          (string-append install-ns " claims host " (if (null? (cdr taken)) "class " "classes ")
                         (fold-left (lambda (a c) (if (string=? a "") c (string-append a ", " c)))
                                    "" taken)
                         ", which the runtime already provides.")))
        (set! lib-class-providers
              (append lib-class-providers
                      (list (vector install-ns coordinate cs (box #f))))))))

;; The Clojure-facing seam. jolt.deps calls this once per declared provider after
;; it resolves the dependency graph, before any user code compiles — which is the
;; ordering the whole mechanism depends on: the table has to be complete before
;; the first class reference can miss.
(def-var! "jolt.host" "register-class-provider!"
  (lambda (install-ns coordinate classes)
    (register-class-provider! install-ns
                              (if (jolt-nil? coordinate) #f coordinate)
                              (seq->list classes))
    jolt-nil))

(define (lib-provider-for class)
  (let loop ((ps lib-class-providers))
    (cond ((null? ps) #f)
          ((member class (vector-ref (car ps) 2)) (car ps))
          (else (loop (cdr ps))))))
;; The latch holds #f (not attempted), 'ok, or 'failed — the install namespace was
;; on the source roots and raised while loading. 'failed is what separates a
;; dependency the caller forgot to declare from one that is declared and broken;
;; see unknown-class-message.
(define (lib-try-autoload! class)
  (let ((p (lib-provider-for class)))
    (and p
         (not (unbox (vector-ref p 3)))
         (begin (set-box! (vector-ref p 3) 'ok)
                (and (find-ns-file (vector-ref p 0))
                     (begin (guard (c (#t (set-box! (vector-ref p 3) 'failed)
                                          (raise c)))
                              (load-namespace (vector-ref p 0)))
                            #t))))))

;; A provider that is on the source roots but raised while loading leaves the
;; class unregistered exactly like an undeclared dependency does — but the fix is
;; the opposite one. Telling someone to add a dependency their deps.edn already
;; declares sends them the wrong way, so say which case it is.
(define (provider-load-failed-message class coordinate install-ns)
  (string-append class " is provided by the " coordinate " library, which is on "
                 "the source roots but failed to load — see the earlier error "
                 "from " install-ns ". This is not a missing dependency."))

;; A JDK-shaped name jolt has no implementation for. The message does NOT name a
;; library: which one supplies java.sql or java.time is not the runtime's to say,
;; and a caller is free to write the shim themselves — declaring it through
;; :jolt/provides is the whole point. Naming a specific library here would put
;; back, as a string, the coupling RFC 0014 removed.
(define (jdk-class-name? class)
  (or (and (>= (string-length class) 5) (string=? (substring class 0 5) "java."))
      (and (>= (string-length class) 6) (string=? (substring class 0 6) "javax."))
      ;; A SIMPLE name carries no package to test — a token arrives as ZoneOffset
      ;; as often as java.time.ZoneOffset. The hierarchy answers it: reaching this
      ;; message means the class has no implementation, so a name the hierarchy
      ;; models is one jolt describes but nothing supplies. jch-known? matches the
      ;; last segment, which is exactly the simple-name case.
      (jch-known? class)))

(define (unknown-class-message class)
  (cond
    ;; A provider CLAIMS this class and is on the source roots, but raised while
    ;; loading. Naming it is not a catalogue — it is the dependency the caller
    ;; declared themselves, read back from their own deps.edn — and the fix is the
    ;; opposite of adding something, so the two cases must not read alike.
    ((and (lib-provider-for class)
          (eq? (unbox (vector-ref (lib-provider-for class) 3)) 'failed))
     => (lambda (_)
          (let ((p (lib-provider-for class)))
            (provider-load-failed-message class (or (vector-ref p 1) "declared")
                                          (vector-ref p 0)))))
    ((jdk-class-name? class)
     (string-append "No dependency provides " class
                    " — a concrete implementation of the JDK classes must be "
                    "provided. A library supplies one by declaring :jolt/provides "
                    "in its deps.edn (RFC 0014)."))
    (else (string-append "Unknown class " class))))

;; ---- emit entry points ------------------------------------------------------
;; A qualified reference whose namespace segment names a live namespace — directly
;; or through a require :as alias — is a missing VAR, not a missing class. The
;; analyzer reads any unresolved ns/name as a host static, so a typo in a
;; clojure.string call used to report "Unknown class s", naming the alias as a
;; class and sending the reader looking in the wrong place.
(define (static-miss-message class member)
  (let ((target (or (chez-resolve-alias (chez-current-ns) class) class)))
    (if (chez-ns-exists? target)
        (string-append "No such var: " class "/" member)
        (unknown-class-message class))))

;; JVM Clojure resolves (.getName String) — an instance member on a class
;; token — as a call on the java.lang.Class OBJECT when the class has no such
;; static. Mirror it: a static-member miss consults the Class instance table
;; (the "class" tag) and applies the method to the interned class object. Only
;; the miss paths reach here, so a real static always wins; the unknown-class
;; arm additionally requires jch-known?, so a typo'd class name still reports
;; Unknown class instead of answering reflection calls. jolt-class-for is
;; defined in host-static-classes.ss (loads after us) — resolved at call time.
(define (class-instance-fallback class member)
  (let ((h (hashtable-ref host-methods-tbl "class" #f)))
    (and h
         (let ((m (hashtable-ref h member #f)))
           (and m (lambda args (apply m (jolt-class-for class) args)))))))

;; Unique miss marker: the registry holds fields and methods in one table, and a
;; field may legitimately hold a falsy value (Boolean/FALSE is #f), so absence
;; cannot be read off a #f result.
(define host-static-miss (list 'host-static-miss))
(define (host-static-ref class member)
  (let ((cell (mutable-static-cell class member #f)))
    (if cell
        (vector-ref cell 0)
        (let ((h (lookup-class class-statics-tbl class)))
          (if h
              (let ((v (hashtable-ref h member host-static-miss)))
                (if (eq? v host-static-miss)
                    (or (class-instance-fallback class member)
                        (throw-jvm (quote IllegalArgumentException) (string-append "No matching field or method: " class "/" member)))
                    v))
              ;; class miss — autoload a provider (the java.time base, or a
              ;; first-party library that installs the class) and retry once
              (if (lib-try-autoload! class)
                  (host-static-ref class member)
                  (or (and (jch-known? class) (class-instance-fallback class member))
                      (throw-jvm (quote IllegalArgumentException) (static-miss-message class member)))))))))

(define (host-static-call class member . args)
  ;; the registry's one rule: a procedure is a method to call, anything else is
  ;; a field value — which answers a zero-argument access and nothing more.
  ;; Applying the field's value used to raise Chez's bare "attempt to apply
  ;; non-procedure" with no message.
  (let ((v (host-static-ref class member)))
    (cond ((procedure? v) (apply v args))
          ((null? args) v)
          (else (throw-jvm (quote IllegalArgumentException)
                  (string-append class "/" member " is a static field; it takes no arguments"))))))

;; (. Class member) with no arguments is ambiguous on the JVM too: it reads a
;; static FIELD when one exists and otherwise calls a no-arg static method. jolt
;; keeps one registry for both, so the decision is by what is registered — a
;; procedure is a method to call, anything else is a field value. Without this
;; the dot form applied a field's value as a zero-arg procedure.
(def-var! "jolt.host" "static-member"
  (lambda (class member)
    (let ((v (host-static-ref class member)))
      (if (procedure? v) (v) v))))

(define (host-new class . args)
  (let ((ctor (lookup-class class-ctors-tbl class)))
    (cond
      (ctor (apply ctor args))
      ;; the constructor may live in a not-yet-loaded provider (the java.time base,
      ;; or a first-party library) — autoload and retry once before falling through
      ;; to the var / no-ctor paths.
      ((lib-try-autoload! class) (apply host-new class args))
      ;; deftype/defrecord: the type name is bound as a VAR (the
      ;; make-deftype-ctor closure) in its defining ns, not a registered host class.
      ;; Resolve it in the current ns / clojure.core and invoke it — so (P. args)
      ;; works the same as the ->P factory.
      (else
       (let ((cell (or (var-cell-lookup (chez-current-ns) class)
                       (var-cell-lookup "clojure.core" class))))
         (if (and cell (var-cell-defined? cell) (procedure? (var-cell-root cell)))
             (apply (var-cell-root cell) args)
             ;; a ctor for a class some provider CLAIMS, that never resolved,
             ;; is that provider's absence — name it; otherwise it is a genuine
             ;; missing ctor on a class jolt does have.
             (throw-jvm (quote IllegalArgumentException)
               (if (lib-provider-for class)
                   (unknown-class-message class)
                   (string-append "No matching ctor found for class " class)))))))))

;; ---- coercion helpers -------------------------------------------------------
;; numeric tower: currentTimeMillis/nanoTime are exact longs (JVM).
(define (->num x) x)
(define (jnum->exact n) (exact (truncate (jolt-need-num n))))
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

