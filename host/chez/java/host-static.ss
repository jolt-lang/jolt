;; host-static.ss — the host-interop registry core: the class-statics / class-ctors
;; / tagged-methods tables, the jhost record, and the emit entry points. The actual
;; entries are registered by host-static-methods.ss (Class/member statics) and
;; host-static-classes.ss (instantiable object classes), loaded after this; the
;; number-parsing family the statics hand results through is java-parse.ss,
;; loaded before.
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

;; The host's own boot-time registrations (io.ss, host-static-classes.ss, …), the
;; statics counterpart of host-class-ctors-tbl. Together they are what "the
;; runtime provides this class" means: register-class-provider! drops a claim on
;; a class already in the tables, and at declaration time — jolt.deps, before any
;; library code runs — the only entries there are these. Both spellings, because a
;; claim is tested under both (class-spellings).
(define host-class-statics-tbl (make-hashtable string-hash string=?))

(define (register-class-statics! name members)
  (hashtable-set! host-class-statics-tbl name #t)
  (hashtable-set! host-class-statics-tbl (short-class-name name) #t)
  (class-statics-merge! name members))

;; Would register-class-provider! drop a :jolt/provides claim on this class?
;; Answered from the HOST tables and not the live ones: a class ANOTHER library
;; registered is still free to be declared — nothing has claimed it — and a
;; registration that just landed must not make itself the reason.
(define (runtime-provides-class? name)
  (let ((short (short-class-name name)))
    (and (or (hashtable-ref host-class-statics-tbl name #f)
             (hashtable-ref host-class-statics-tbl short #f)
             (hashtable-ref host-class-ctors-tbl name #f)
             (hashtable-ref host-class-ctors-tbl short #f))
         #t)))

;; ---- arity, before the member runs ------------------------------------------
;; The JVM resolves a call by its ARITY before it runs anything: an extra trailing
;; argument is "No matching method X found taking N args", never a silently
;; dropped value. A table entry written with a fixed signature already says which
;; arities it has — procedure-arity-mask reads them straight off the procedure —
;; so the only thing missing was asking. A `. rest` entry says nothing, which is
;; how (String/valueOf ca 1 2) came back "abc": valueOf's one arm took the array
;; and never looked at the offset and count it was handed.
;;
;; So a member with overloads writes them as a case-lambda rather than as one
;; rest-taking arm that picks arguments out of a list — the mask then carries the
;; overload set, and the two invocation sites below (host-static-call for a
;; static, the jhost arm for an instance method) enforce it with one bit test. A
;; member that is genuinely variadic on the JVM — String/format, String/join —
;; keeps its rest arg and stays unchecked, which is correct rather than lax.
;;
;; N is the JAVA argument count; an instance method's procedure also takes the
;; receiver, which is not one of them.
(define (host-arity-ok? f n self?)
  (bitwise-bit-set? (procedure-arity-mask f) (if self? (fx+ n 1) n)))

;; A wrapper that takes `. args` to do something for every call — coerce the
;; operands, check the receiver, count the arguments itself — reads as "any
;; arity" to the bit test above, and so hid its member's overload set: every
;; Math entry went through one such wrapper and (Math/abs 1 2) reached Chez's
;; own arity error, which names an anonymous procedure. These give a wrapper the
;; mask it actually honours, so the check keeps working through it.
;;
;;   (host-arity-like f wrapper)            ; wrapper answers exactly f's arities
;;   (host-arity-of arities self? wrapper)  ; ARITIES are Java argument counts
;;   (host-arity-over prior arities wrapper) ; an instance method that answers
;;                                           ; ARITIES and hands every other
;;                                           ; receiver to PRIOR
(define (host-arities->mask arities self?)
  (fold-left (lambda (m n) (bitwise-ior m (bitwise-arithmetic-shift-left 1 (if self? (fx+ n 1) n))))
             0 arities))
(define (host-arity-like f wrapper)
  (make-arity-wrapper-procedure wrapper (procedure-arity-mask f) #f))
(define (host-arity-of arities self? wrapper)
  (make-arity-wrapper-procedure wrapper (host-arities->mask arities self?) #f))
(define (host-arity-over prior arities wrapper)
  (make-arity-wrapper-procedure
    wrapper
    (bitwise-ior (procedure-arity-mask prior) (host-arities->mask arities #t))
    #f))

;; ---- the JVM's overload arities, for members that cannot state their own ------
;; A member written `(lambda (self . args) ...)` picks its overload out of the
;; argument list, so its procedure takes any count and the arity check above
;; passes everything: (Integer/parseInt "1" 10 3) answered 1 and dropped the 3,
;; (Thread/sleep 1 0 1) slept and answered nil. Rewriting ~170 such members as
;; case-lambdas would restate each body once per overload, so they declare the
;; JVM's arities here instead, and registration gives the procedure that mask.
;;
;; Each row is (key member count ...): the counts are the Java argument counts of
;; the member's public overloads, read by reflection from the JDK, never the
;; receiver. A static's key is the class's simple name (the FQN and the simple
;; name share one member table); an instance method's key is its jhost tag, and
;; a tag that serves several classes (in-stream, out-stream) takes the union.
;; `varargs` marks a member with a trailing `...` parameter: jolt accepts its
;; elements loose as well as in an array, so it stays open on purpose.
;;
;; A member with a fixed signature, or a wrapper built with host-arity-like /
;; host-arity-of, already answers its own arities and needs no row. `make
;; hostarity` fails on any member that still takes any count without a row here,
;; and on a row that names nothing registered.
(define host-static-arity-rows
  '(
    ("Array" "newInstance" varargs)
    ("Arrays" "asList" varargs)
    ("Base64" "getMimeEncoder" 0 2)
    ("BigDecimal" "valueOf" 1 2)
    ("Byte" "parseByte" 1 2)
    ("Byte" "toString" 1)
    ("Byte" "valueOf" 1 2)
    ("ByteBuffer" "wrap" 1 3)
    ("Calendar" "getInstance" 0 1 2)
    ("Class" "forName" 1 2 3)
    ("Collections" "emptyList" 0)
    ("Collections" "emptyMap" 0)
    ("Collections" "synchronizedList" 1)
    ("Collections" "synchronizedMap" 1)
    ("Collections" "synchronizedSet" 1)
    ("Collections" "unmodifiableList" 1)
    ("Collections" "unmodifiableMap" 1)
    ("Collections" "unmodifiableSet" 1)
    ("Compiler" "eval" 1 2)
    ("Executors" "newCachedThreadPool" 0 1)
    ("Executors" "newFixedThreadPool" 1 2)
    ("Executors" "newScheduledThreadPool" 1 2)
    ("Executors" "newSingleThreadExecutor" 0 1)
    ("Executors" "newSingleThreadScheduledExecutor" 0 1)
    ("Executors" "newVirtualThreadPerTaskExecutor" 0)
    ("Executors" "newWorkStealingPool" 0 1)
    ("File" "createTempFile" 2 3)
    ("FileChannel" "open" varargs)
    ("FileTime" "from" 1 2)
    ("Files" "copy" varargs)
    ("Files" "createDirectories" varargs)
    ("Files" "createDirectory" varargs)
    ("Files" "createFile" varargs)
    ("Files" "createLink" 2)
    ("Files" "createSymbolicLink" varargs)
    ("Files" "createTempDirectory" varargs)
    ("Files" "createTempFile" varargs)
    ("Files" "exists" varargs)
    ("Files" "getAttribute" varargs)
    ("Files" "getLastModifiedTime" varargs)
    ("Files" "getOwner" varargs)
    ("Files" "getPosixFilePermissions" varargs)
    ("Files" "isDirectory" varargs)
    ("Files" "isExecutable" 1)
    ("Files" "isHidden" 1)
    ("Files" "isReadable" 1)
    ("Files" "isRegularFile" varargs)
    ("Files" "isSymbolicLink" 1)
    ("Files" "isWritable" 1)
    ("Files" "move" varargs)
    ("Files" "newDirectoryStream" 1 2)
    ("Files" "newInputStream" varargs)
    ("Files" "newOutputStream" varargs)
    ("Files" "notExists" varargs)
    ("Files" "readAllLines" 1 2)
    ("Files" "readAttributes" varargs)
    ("Files" "setAttribute" varargs)
    ("Files" "setPosixFilePermissions" 2)
    ("Files" "size" 1)
    ("Files" "write" varargs)
    ("GregorianCalendar" "getInstance" 0 1 2)
    ("Integer" "parseInt" 1 2 4)
    ("Integer" "toString" 1 2)
    ("Integer" "valueOf" 1 2)
    ("Keyword" "find" 1 2)
    ("Keyword" "intern" 1 2)
    ("Long" "parseLong" 1 2 4)
    ("Long" "toString" 1 2)
    ("Long" "valueOf" 1 2)
    ("Math" "random" 0)
    ("NumberFormat" "getCurrencyInstance" 0 1)
    ("NumberFormat" "getInstance" 0 1)
    ("NumberFormat" "getIntegerInstance" 0 1)
    ("NumberFormat" "getNumberInstance" 0 1)
    ("Numbers" "equiv" 2)
    ("Objects" "hash" varargs)
    ("Optional" "empty" 0)
    ("Path" "of" varargs)
    ("Paths" "get" varargs)
    ("Pattern" "compile" 1 2)
    ("RT" "aget" 2)
    ("RT" "alength" 1)
    ("RT" "assoc" 3)
    ("RT" "classForName" 1 3)
    ("RT" "classForNameNonLoading" 1)
    ("RT" "conj" 2)
    ("RT" "dissoc" 2)
    ("RT" "find" 2)
    ("RT" "list" 0 1 2 3 4 5)
    ("RT" "subvec" 3)
    ("SecureRandom" "getInstance" 1 2 3)
    ("SecureRandom" "getInstanceStrong" 0)
    ("Short" "parseShort" 1 2)
    ("Short" "toString" 1)
    ("Short" "valueOf" 1 2)
    ("String" "format" varargs)
    ("String" "join" varargs)
    ("Symbol" "create" 1 2)
    ("Symbol" "intern" 1 2)
    ("System" "console" 0)
    ("System" "exit" 1)
    ("System" "gc" 0)
    ("System" "getProperty" 1 2)
    ("System" "lineSeparator" 0)
    ("System" "runFinalization" 0)
    ("Thread" "interrupted" 0)
    ("Thread" "sleep" 1 2)
    ("Thread" "yield" 0)
    ("URLDecoder" "decode" 1 2)
    ("URLEncoder" "encode" 1 2)
    ("Var" "intern" 2 3 4)))
(define host-method-arity-rows
  '(
    ("abq" "offer" 1 3)
    ("abq" "poll" 0 2)
    ("abq" "toArray" 0 1)
    ("arraydeque" "add" 1)
    ("arraydeque" "addAll" 1)
    ("arraydeque" "toArray" 0 1)
    ("arraylist" "add" 1 2)
    ("arraylist" "addAll" 1 2)
    ("arraylist" "toArray" 0 1)
    ("arrays-aslist" "toArray" 0 1)
    ("byte-buffer" "get" 0 1 2 3 4)
    ("byte-buffer" "getChar" 0 1)
    ("byte-buffer" "getInt" 0 1)
    ("byte-buffer" "getLong" 0 1)
    ("byte-buffer" "getShort" 0 1)
    ("byte-buffer" "limit" 0 1)
    ("byte-buffer" "position" 0 1)
    ("byte-buffer" "put" 1 2 3 4)
    ("byte-buffer" "putChar" 1 2)
    ("byte-buffer" "putInt" 1 2)
    ("byte-buffer" "putLong" 1 2)
    ("byte-buffer" "putShort" 1 2)
    ("calendar" "set" 2 3 5 6)
    ("char-buffer" "get" 0 1 2 3 4)
    ("char-buffer" "limit" 0 1)
    ("char-buffer" "position" 0 1)
    ("char-buffer" "put" 1 2 3 4)
    ("char-reader" "mark" 1)
    ("char-reader" "read" 0 1 3)
    ("char-writer" "append" 1 3)
    ("char-writer" "write" 1 3)
    ("charset-decoder" "decode" 1 3)
    ("class" "getConstructor" varargs)
    ("class" "getDeclaredConstructor" varargs)
    ("class" "getDeclaredMethod" varargs)
    ("class" "getMethod" varargs)
    ("class-ctor" "newInstance" varargs)
    ("count-down-latch" "await" 0 2)
    ("executor-service" "awaitTermination" 2)
    ("executor-service" "invokeAll" 1 3)
    ("executor-service" "invokeAny" 1 3)
    ("file-writer" "append" 1 3)
    ("file-writer" "write" 1 3)
    ("future-task" "cancel" 1)
    ("future-task" "get" 0 2)
    ("hashset" "toArray" 0 1)
    ("in-stream" "mark" 1)
    ("in-stream" "read" 0 1 3)
    ("in-stream" "readNBytes" 1 3)
    ("in-stream" "skip" 1)
    ("in-stream" "unread" 1 3)
    ("j-future" "cancel" 1)
    ("j-future" "get" 0 2)
    ("jolt-runtime" "exec" 1 2 3)
    ("linkedlist" "add" 1 2)
    ("linkedlist" "addAll" 1 2)
    ("linkedlist" "toArray" 0 1)
    ("nio-filesystem" "getPath" varargs)
    ("out-stream" "finish" 0)
    ("out-stream" "toString" 0 1)
    ("out-stream" "write" 1 3)
    ("port-writer" "append" 1 3)
    ("port-writer" "println" 0 1)
    ("port-writer" "write" 1 3)
    ("print-stream" "append" 1 3)
    ("print-stream" "format" varargs)
    ("print-stream" "printf" varargs)
    ("print-stream" "println" 0 1)
    ("print-stream" "write" 1 3)
    ("print-writer" "append" 1 3)
    ("print-writer" "format" varargs)
    ("print-writer" "printf" varargs)
    ("print-writer" "println" 0 1)
    ("print-writer" "write" 1 3)
    ("print-writer-on" "append" 1 3)
    ("print-writer-on" "write" 1 3)
    ("process" "waitFor" 0 1 2)
    ("process-builder" "command" varargs)
    ("process-builder" "redirectError" 0 1)
    ("process-builder" "redirectErrorStream" 0 1)
    ("process-builder" "redirectInput" 0 1)
    ("process-builder" "redirectOutput" 0 1)
    ("properties" "getProperty" 1 2)
    ("pushback-reader" "read" 0 1 3)
    ("pushback-reader" "unread" 1 3)
    ("random" "nextInt" 0 1 2)
    ("splittable-random" "nextDouble" 0 1 2)
    ("splittable-random" "nextInt" 0 1 2)
    ("splittable-random" "nextLong" 0 1 2)
    ("reader-adapter" "mark" 1)
    ("reader-adapter" "read" 0 1 3)
    ("reentrant-lock" "tryLock" 0 2)
    ("ref-queue" "poll" 0)
    ("ref-queue" "remove" 0 1)
    ("reflect-method" "invoke" varargs)
    ("securerandom" "nextInt" 0 1 2)
    ("securerandom" "setSeed" 1)
    ("string-reader" "mark" 1)
    ("string-reader" "read" 0 1 3)
    ("url" "openConnection" 0 1)
    ("user-thread" "interrupt" 0)
    ("user-thread" "join" 0 1 2)
    ("writer" "append" 1 3)
    ("writer" "write" 1 3)
    ("zip-adler32" "update" 1 3)
    ("zip-crc32" "update" 1 3)))
(define (host-arity-rows->table rows)
  (let ((t (make-hashtable string-hash string=?)))
    (for-each (lambda (r) (hashtable-set! t (string-append (car r) "/" (cadr r)) (cddr r))) rows)
    t))
(define host-static-arities (host-arity-rows->table host-static-arity-rows))
(define host-method-arities (host-arity-rows->table host-method-arity-rows))

;; A member procedure as registration stores it: an open procedure with a fixed
;; row gets the row's mask, narrowed to what the procedure itself accepts.
;;
;; One wrapper per procedure and mask: a class registered under both spellings
;; (Thread and java.lang.Thread) hands the same procedure over twice, and the
;; second registration must store the SAME value, or registry-collision!
;; reports the boot as drifting. The mask is part of the key because one
;; procedure can serve two members with different overloads (SecureRandom's
;; getInstance and getInstanceStrong).
(define host-arity-wrappers (make-weak-eq-hashtable))   ; f -> ((mask . wrapper) ...)
(define (host-arity-declared table key member f self?)
  (if (and (procedure? f) (< (procedure-arity-mask f) 0))
      (let ((row (hashtable-ref table (string-append key "/" member) #f)))
        (if (and row (not (eq? (car row) 'varargs)))
            (let ((mask (bitwise-and (procedure-arity-mask f) (host-arities->mask row self?))))
              (let* ((known (hashtable-ref host-arity-wrappers f '()))
                     (hit (assv mask known)))
                (if hit
                    (cdr hit)
                    (let ((w (make-arity-wrapper-procedure f mask #f)))
                      (hashtable-set! host-arity-wrappers f (cons (cons mask w) known))
                      w))))
            f))
      f))

;; ---- what a static site may cache ------------------------------------------
;; A `Class/member` site caches what the name resolved to (host-static-site
;; below). That answer holds until the registry changes, and the registry changes
;; in exactly two ways: a member is written into a class's member table, or a
;; mutable cell is created that now shadows the table. Both bump this epoch, and
;; a site revalidates against it — so every write to a member table goes through
;; class-statics-member-set!, never a bare hashtable-set!.
;; A box moved by CAS, not a set!: two writers bumping at once must move it twice,
;; or a site that read the value between them would validate against a write it
;; never saw. The release orders the table write before the new value.
(define host-static-epoch-box (box 0))
(define (host-static-epoch) (unbox host-static-epoch-box))
(define (host-static-epoch-bump!)
  (memory-order-release)
  (let retry ()
    (let ((e (unbox host-static-epoch-box)))
      (unless (box-cas! host-static-epoch-box e (fx+ e 1)) (retry)))))
(define (class-statics-member-set! h member v)
  (hashtable-set! h member v)
  (host-static-epoch-bump!))

;; The merge itself: also the LIBRARY path (register-class-statics-owned!,
;; extend-class!), which adds members without making the class the runtime's.
(define (class-statics-merge! name members)  ; members: list of (str . val/proc)
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
                (let ((v (host-arity-declared host-static-arities short (car p) (cdr p) #f))
                      (old (hashtable-ref h (car p) #f)))
                  (when old (registry-collision! "static" name (car p) old v))
                  (class-statics-member-set! h (car p) v)))
              members)
    ;; a class name newly resolving is a change too: a site that missed on it
    ;; answers from the table now
    (host-static-epoch-bump!)))

;; Names the HOST registered (io.ss, io-streams.ss, …), as opposed to a library
;; registering a class jolt does not model. Only the host's own boot-time calls
;; land here — the Clojure-visible hook goes through register-class-ctor-user!.
(define host-class-ctors-tbl (make-hashtable string-hash string=?))

(define (register-class-ctor! name proc)
  (hashtable-set! host-class-ctors-tbl name #t)
  (class-ctor-set! name proc))

;; The plain table write, for a ctor that is NOT the runtime coming to provide a
;; host class: every deftype and defrecord binds (Name. …) through this table
;; (protocols.ss), under its "ns.Name" tag AND its simple name. Recording those
;; as the host's would make host-class-ctors-tbl — which is what
;; runtime-provides-class? and the constructor-override warning read — answer
;; yes for any class whose simple name a user type happens to share.
;; Bumped by every write to class-ctors-tbl, so host-new's front cache below can
;; tell a ctor it resolved from one a redefinition replaced.
(define class-ctor-epoch 0)
(define (class-ctor-set! name proc)
  (set! class-ctor-epoch (fx+ class-ctor-epoch 1))
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
  ;; A constructor is one value, so there is no additive half to let through the
  ;; way register-class-statics-user! does with members.
  (cond
    ((lib-pending-claimer name)
     => (lambda (pending)
          (provider-claim-hold! name pending '())
          (lib-defer-registration! pending (lambda () (register-class-ctor-user! name proc)))))
    ((lib-provider-owner-elsewhere name)
     => (lambda (owner) (provider-claim-drop! name owner '())))
    (else
     (provider-claim-note! name)
     (when (and (getenv "JOLT_DEBUG") (hashtable-ref host-class-ctors-tbl name #f))
       (fprintf (current-error-port)
                "warning: a library replaced the host constructor for ~a — every (~a. ...) in this process now builds its shim, including in namespaces that never asked for it\n"
                name name))
     (lib-note-provider-registration! name)
     (set! class-ctor-epoch (fx+ class-ctor-epoch 1))
     (hashtable-set! class-ctors-tbl name proc))))

;; clojure.core/__register-class-statics! lands here — the statics counterpart of
;; register-class-ctor-user!, and under the same provider guard. The host's own
;; boot-time registrations go straight to register-class-statics! and are not
;; subject to it: nothing has declared anything yet when they run.
(define (register-class-statics-user! name members)
  (let ((pending (lib-pending-claimer name)))
    (if pending
        ;; The claimer has not spoken yet, so there is nothing to compare this
        ;; against — and letting it land would put an entry in the registry for a
        ;; class whose provider has not loaded, which is exactly what stops the
        ;; autoload from ever running (jolt#914). Hold it until the claim settles.
        (begin (provider-claim-hold! name pending (map car members))
               (lib-defer-registration!
                pending (lambda () (register-class-statics-user! name members))))
        (register-class-statics-owned! name members))))

;; The claim on `name` has settled (or there never was one): what the class's
;; provider registered is authority, everything else is additive or refused.
(define (register-class-statics-owned! name members)
  (let ((owner (lib-provider-owner-elsewhere name)))
    (if owner
        ;; A provider owns the members it registered, not the whole name. Adding a
        ;; member its shim does not answer is the additive case extend-class! exists
        ;; for and stays allowed; REPLACING one is the substitution that made
        ;; resolution depend on load order, and that is what is refused.
        (let* ((h (lookup-class class-statics-tbl name))
               (taken (if h (filter (lambda (m) (hashtable-contains? h (car m))) members) '()))
               (fresh (if h (filter (lambda (m) (not (hashtable-contains? h (car m)))) members) members)))
          (when (pair? taken) (provider-claim-drop! name owner (map car taken)))
          (when (pair? fresh) (class-statics-merge! name fresh)))
        (begin
          (provider-claim-note! name)
          (lib-note-provider-registration! name)
          (class-statics-merge! name members)))))

(define (register-host-methods! tag members)
  (let ((h (or (hashtable-ref host-methods-tbl tag #f)
               (let ((h (make-hashtable string-hash string=?)))
                 (hashtable-set! host-methods-tbl tag h) h))))
    (for-each (lambda (p)
                (hashtable-set! h (car p)
                  (host-arity-declared host-method-arities tag (car p) (cdr p) #t)))
              members)))

;; The comparator seam (natives-seq.ss jolt-comparator-fn) asks whether a value
;; is a shim object whose tag registers a `compare` method — a Comparator held
;; by the host (String/CASE_INSENSITIVE_ORDER) rather than by a deftype/reify.
(set! jhost-compare-method?
  (lambda (x)
    (and (jhost? x) (host-method-ref (jhost-tag x) "compare") #t)))

;; ---- how two tags relate ----------------------------------------------------
;; A tag is a REPRESENTATION: the procedures in its table read the state vector
;; its constructor builds. The class a tag reports is a separate fact, kept in
;; ONE place (jhost-tag->fqn, class-hierarchy.ss) and answered from the class
;; graph — instance?, class, the protocol tags. The two relations below are
;; between representations, and the second is checked against the graph so a
;; layout claim cannot contradict the class claim.
;;
;; alias: a second tag over the SAME table object, for a shim that differs from
;; another only in the class it reports — clojure.lang.LineNumberingPushbackReader
;; over java.io.PushbackReader. Sharing rather than copying is the point: a later
;; register-host-methods! on either tag reaches both, where a copy would let the
;; two silently drift apart. The alias inherits the original's parent link too,
;; so an alias of a derived tag answers what the derived tag answers.
(define host-methods-parent (make-hashtable string-hash string=?))   ; tag -> parent tag
(define (host-tag-parent tag) (hashtable-ref host-methods-parent tag #f))
(define (alias-host-methods! tag from)
  (let ((h (or (hashtable-ref host-methods-tbl from #f)
               (error 'alias-host-methods! "no methods registered for tag" from))))
    (hashtable-set! host-methods-tbl tag h)
    (let ((p (host-tag-parent from)))
      (when p (hashtable-set! host-methods-parent tag p)))))

;; derive: a tag whose LAYOUT EXTENDS another's — the scheduled future carries the
;; j-future's five slots and five more — answers every member the parent tag
;; answers plus its own. Neither an alias (the members differ) nor a copy (a copy
;; freezes the parent's table as it stood when it was taken, the drift alias
;; avoids): the child keeps a table of its own and names its parent, and
;; host-method-ref walks the chain on a MISS only, so a hit on the tag's own table
;; costs what it always did.
;;
;; CHECKED AGAINST THE CLASS GRAPH, because a parent's procedures reading the
;; child's state is a claim about the layout, and the layout follows the class:
;; the child's class must be a strict descendant of the parent's in the modeled
;; hierarchy, so the graph — already the single answer to instance? — is also the
;; single answer to "may this tag inherit that one". Two tags of ONE class are
;; two layouts of it (future-task and j-future are both FutureTask), which is
;; exactly why the graph cannot drive inheritance by itself: it names classes, not
;; layouts, and would let the scheduled future reach the future-task table whose
;; procedures read a different vector. A tag with no class row cannot derive: the
;; check would have nothing to hold it to.
(define (derive-host-methods! tag from members)
  (unless (hashtable-ref host-methods-tbl from #f)
    (error 'derive-host-methods! "no methods registered for tag" from))
  (let ((child (jhost-fqn tag)) (parent (jhost-fqn from)))
    (unless (and child parent)
      (error 'derive-host-methods! "both tags must name a class (jhost-tag->fqn)" tag from))
    (unless (and (not (string=? child parent)) (jch-isa? child parent))
      (error 'derive-host-methods! "the child's class must extend the parent's" child parent))
    ;; and the chain must end: a cycle would make host-method-ref loop
    (let walk ((t from))
      (when t
        (when (string=? t tag) (error 'derive-host-methods! "derivation cycle" tag from))
        (walk (host-tag-parent t)))))
  (hashtable-set! host-methods-parent tag from)
  (register-host-methods! tag members))

;; The two resolvers, and the only readers of the chain. host-method-ref: the
;; member NAME resolves to on TAG — its own table first, then each parent's — or
;; #f; every dispatch goes through it. host-method-entries: every (name . proc)
;; TAG answers, the child's first and a parent's member SHADOWED by the child's
;; left out, as getMethods lists an override once; reflection (natives-array.ss)
;; reads this rather than the tables.
(define (host-method-ref tag name)
  (let loop ((tag tag))
    (and tag
         (let ((h (hashtable-ref host-methods-tbl tag #f)))
           (or (and h (hashtable-ref h name #f))
               (loop (host-tag-parent tag)))))))
(define (host-method-entries tag)
  (let loop ((tag tag) (seen '()) (acc '()))
    (if (not tag)
        (reverse acc)
        (let ((h (hashtable-ref host-methods-tbl tag #f)))
          (if (not h)
              (loop (host-tag-parent tag) seen acc)
              (let-values (((names procs) (hashtable-entries h)))
                (let walk ((i 0) (seen seen) (acc acc))
                  (if (fx=? i (vector-length names))
                      (loop (host-tag-parent tag) seen acc)
                      (let ((n (vector-ref names i)))
                        (if (member n seen)
                            (walk (fx+ i 1) seen acc)
                            (walk (fx+ i 1) (cons n seen)
                                  (cons (cons n (vector-ref procs i)) acc))))))))))))

(define (lookup-class h-tbl name)
  (or (hashtable-ref h-tbl name #f)
      (hashtable-ref h-tbl (short-class-name name) #f)))

;; ---- the concrete methods of an abstract host class -------------------------
;; (proxy [java.io.InputStream] [] (read …)) extends the class on the JVM and
;; inherits every method it does not name — readAllBytes, skip, available,
;; close — each written against the abstract read() the proxy supplies and
;; reaching it by virtual dispatch. jolt generates no class: a proxy over a class
;; with no constructor is a bare reify, and a method it did not write was a
;; miss ("No matching field found: available"), so slurp, io/copy and a
;; PushbackInputStream over such a proxy all failed. These tables ARE that
;; inheritance. A reify's method miss (records-dispatch.ss) asks here; the
;; classes the reify declares and their modeled ancestry are walked in order, and
;; the method found runs against the reify itself, so its own calls back into
;; read() reach the override as the JVM's would. A method the class leaves
;; abstract is in its table too, raising what the proxy macro's stub raises for a
;; method the body omits: UnsupportedOperationException naming the method.
;; Keyed by the class's qualified name; io-streams.ss registers the java.io four.
(define abstract-methods-tbl (make-hashtable string-hash string=?))   ; FQN -> (method-ht)
(define (register-abstract-methods! class members)
  (let ((h (or (hashtable-ref abstract-methods-tbl class #f)
               (let ((h (make-hashtable string-hash string=?)))
                 (hashtable-set! abstract-methods-tbl class h) h))))
    (for-each (lambda (p) (hashtable-set! h (car p) (cdr p))) members)))
(define (abstract-class-method obj method-name)
  (let loop ((tags (jreify-host-tags obj)))
    (cond ((null? tags) #f)
          ((hashtable-ref abstract-methods-tbl (car tags) #f)
           => (lambda (h) (or (hashtable-ref h method-name #f) (loop (cdr tags)))))
          (else (loop (cdr tags))))))
(set-abstract-class-method-hook! abstract-class-method)

;; ---- host object ------------------------------------------------------------
(define-record-type jhost (fields tag (mutable state)) (nongenerative chez-jhost-v1))

;; record-method-dispatch (records.ss) gets a jhost arm: dispatch (.method obj a*)
;; through the tag's method table.
(register-method-arm! arm-priority-host-type
  (lambda (obj method-name rest-args)
    (cond
      ((jhost? obj)
       (let* ((f (host-method-ref (jhost-tag obj) method-name))
              (args (method-rest-args->list rest-args)))
         (cond
           ;; A member whose arities do not include this one is not this member:
           ;; fall to dispatch-miss, so a library extension still gets its say and
           ;; the report is the JVM's "No matching method … taking N args" rather
           ;; than whatever the body faulted on reading an argument it wasn't given
           ;; ((.append sb "x" 1) used to surface Chez's "cadr: incorrect list
           ;; structure").
           (f (if (host-arity-ok? f (length args) #t)
                  (apply f obj args)
                  (dispatch-miss obj method-name args)))
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
;;
;; The arity table is the same device jolt-string-method uses (natives-str.ss) and
;; is here for the same reason: these arms take `. args` and most never read it, so
;; (.intValue (int 5) 1) answered 5 where the JVM raises "No matching method
;; intValue found taking 1 args". A name absent from the table is unchecked.
(define number-method-arities
  (let ((h (make-hashtable string-hash string=?))
        (mask (lambda (ns) (fold-left (lambda (m k) (bitwise-ior m (bitwise-arithmetic-shift-left 1 k))) 0 ns))))
    (for-each (lambda (e) (hashtable-set! h (car e) (mask (cdr e))))
      '(("byteValue" 0) ("shortValue" 0) ("intValue" 0) ("longValue" 0)
        ("doubleValue" 0) ("floatValue" 0) ("toString" 0 1) ("hashCode" 0)
        ("isNaN" 0) ("isInfinite" 0) ("negate" 0) ("abs" 0) ("bitLength" 0)
        ("signum" 0) ("shiftLeft" 1) ("shiftRight" 1)))
    h))
(define (number-method method n . args)
  ;; Same shape as jolt-string-method's check, including the (pair? args) guard:
  ;; too few arguments already faults in the arm that reads one, and the 0-arg
  ;; projections are what this is called with almost every time.
  (if (and (pair? args)
           (let ((m (hashtable-ref number-method-arities method #f)))
             (and m (not (bitwise-bit-set? m (length args))))))
      (dispatch-miss n method args)
      (number-method-arms method n args)))

(define (number-method-arms method n args)
  (cond
    ((string=? method "byteValue") (let ((b (modulo (jnum->exact n) 256))) (->num (if (>= b 128) (- b 256) b))))
    ((string=? method "shortValue") (let ((b (modulo (jnum->exact n) 65536))) (->num (if (>= b 32768) (- b 65536) b))))
    ((string=? method "intValue") (->num (jnum->exact n)))
    ((string=? method "longValue") (->num (jnum->exact n)))
    ((string=? method "doubleValue") (->num n))
    ((string=? method "floatValue") (->num n))
    ;; .toString(radix) — BigInteger/Integer render in a base, lowercase like the
    ;; JVM (rewrite-clj's integer node reconstructs 0xff / 0377 / 2r1001 this way).
    ;; It is BigInteger's overload, the one jolt's single integer type can take, so
    ;; it follows BigInteger: a radix outside 2..36 renders in base 10 rather than
    ;; raising, and a double has no such overload.
    ((string=? method "toString")
     (cond ((null? args) (jolt-num->string n))
           ((and (exact? n) (integer? n))
            (let ((radix (jnum->exact (car args))))
              (string-downcase
                (number->string n (if (and (fixnum? radix) (fx<=? 2 radix 36)) radix 10)))))
           (else (dispatch-miss n method args))))
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
              (let ((c (vector jolt-nil)))
                (hashtable-set! h member c)
                ;; the cell shadows whatever the member table held for this name
                (host-static-epoch-bump!)
                c))))
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

;; ---- claim index: which claims have not had their chance yet ----------------
;; One entry per SPELLING for the claims of providers that have not been
;; autoloaded yet; entries leave the moment their provider is attempted. Two
;; questions read it, and neither is on the hot path: which provider to autoload
;; when a class reference MISSES (lib-try-autoload!), and whether a registration
;; is about to squat on a class whose claimer has not spoken (lib-pending-claimer).
;; The flag keeps the second one — asked once per registration — a boolean test
;; when nothing is declared.
(define lib-pending-claims-tbl (make-hashtable string-hash string=?))
(define lib-any-pending-claims? #f)
;; Autoload is one-shot per provider, and two threads reaching a claimed class at
;; once must not both load the install namespace. The latch flip, the index purge
;; and the held-registration list go under this mutex; the load itself does NOT,
;; because an install namespace requires others and a nested autoload would
;; deadlock on a non-recursive mutex.
(define lib-claims-mu (make-mutex))
(define (lib-claim-pending! p)
  (for-each (lambda (c) (hashtable-set! lib-pending-claims-tbl c p)) (vector-ref p 2))
  (set! lib-any-pending-claims? #t))
(define (lib-claim-settled! p)
  (for-each (lambda (c) (hashtable-delete! lib-pending-claims-tbl c)) (vector-ref p 2))
  (set! lib-any-pending-claims? (> (hashtable-size lib-pending-claims-tbl) 0)))
(for-each lib-claim-pending! core-class-providers)

;; Registrations HELD because the class's declared provider has not loaded yet.
;; Letting one land would put an entry in the registry for a class whose claimer
;; has not spoken — and a registry HIT is exactly what stops the autoload, which
;; is how resolution came to depend on compile order (jolt#914). They are replayed
;; the moment the claim settles, through the same guard as any other registration:
;; what the provider implements wins, what it left unanswered still lands. Keyed
;; by provider, because the provider is what settles.
(define lib-deferred-tbl (make-eq-hashtable))
(define (lib-defer-registration! p thunk)
  (jolt-with-mutex lib-claims-mu
    (hashtable-set! lib-deferred-tbl p (cons thunk (hashtable-ref lib-deferred-tbl p '())))))
;; Called for every settled claim, including one whose install namespace was off
;; the source roots or raised: a provider that cannot deliver must not swallow
;; somebody else's registration with it. Answers whether anything was held, which
;; is a change to what the registry says and so a reason for the caller to retry.
(define (lib-replay-deferred! p)
  (let ((held (jolt-with-mutex lib-claims-mu
                (let ((h (hashtable-ref lib-deferred-tbl p '())))
                  (hashtable-delete! lib-deferred-tbl p)
                  h))))
    (for-each (lambda (t) (t)) (reverse held))
    (pair? held)))

;; The provider whose install namespace is loading on THIS thread, or #f. A
;; provider registers its classes as its install namespace loads, and the
;; registration guard (lib-provider-owner-elsewhere) has to tell that registration
;; apart from another library squatting on the same class.
;;
;; Whoever LOADS an install namespace sets the mark (loader.ss load-namespace*,
;; through lib-with-install-ns-mark) — not the autoload alone. A provider whose
;; install namespace requires a SECOND provider's, which is how kmet reached
;; jolt.crypto, registers that second provider's classes under a plain require;
;; marking the whole load with the outer provider left the inner one's classes
;; owned by nobody, so a later registration for a member it answers was accepted
;; instead of dropped — jolt#914 one level down — and JOLT_DEBUG named the wrong
;; namespace (jolt#926).
(define lib-loading-provider (make-thread-parameter #f))

(define (lib-provider-by-install-ns ns)
  (let loop ((ps lib-class-providers))
    (cond ((null? ps) #f)
          ((string=? ns (vector-ref (car ps) 0)) (car ps))
          (else (loop (cdr ps))))))

;; Run `thunk` marked with the provider `ns` installs, if it installs one. A
;; namespace that is NOT an install namespace leaves the mark alone rather than
;; clearing it: a provider whose install! calls a helper namespace of its own is
;; still that provider registering, and only another DECLARED provider is a
;; different registrant.
(define (lib-with-install-ns-mark ns thunk)
  (let ((p (lib-provider-by-install-ns ns)))
    (if p (parameterize ((lib-loading-provider p)) (thunk)) (thunk))))

;; Classes a declared provider actually registered as its install namespace
;; loaded. This is what the registration guard protects: a class whose provider
;; has spoken for it is that provider's, and a later registration from anywhere
;; else is dropped rather than allowed to win by being last (jolt#914). Filled at
;; REGISTRATION time, not from the declaration, so a class a provider declares and
;; never registers stays open — the declaration alone is not an implementation.
(define lib-provider-owned-tbl (make-hashtable string-hash string=?))
;; jolt.time.base and jolt.socket are the runtime's own BASE tier, and a base tier
;; exists to be extended: jolt-lang/time declares only the formatting classes
;; (DateTimeFormatter, ZoneId, ...) and adds a DateTimeFormatter arm to
;; java.time.LocalDate/from, a class the base declares. That is the arrangement
;; working, not a squat — the base ships the value types that must resolve with no
;; dependency, and the library completes them.
;;
;; So what jolt ships claims a NAME, not the implementation of every member under
;; it. RFC 0014's guard is about two DEPENDENCIES disagreeing over who implements a
;; class (jolt#914), and the base tier is not one of them. Without this the tick
;; suite lost five parse tests to "dropping a registration for LocalDate/from".
(define (lib-core-provider? p) (and (memq p core-class-providers) #t))

;; Every SPELLING of the class goes in, not just the one written: the statics
;; table keys the fully-qualified and the simple name to ONE member table
;; (register-class-statics!), so a registration under either spelling reaches the
;; same members and both have to be covered.
(define (lib-note-provider-registration! name)
  (let ((p (lib-loading-provider)))
    (when (and p (not (lib-core-provider? p)) (member name (vector-ref p 2)))
      (let ((short (short-class-name name)))
        (for-each (lambda (c)
                    (when (string=? (short-class-name c) short)
                      (hashtable-set! lib-provider-owned-tbl c p)))
                  (vector-ref p 2))))))

;; Declared providers from the dependency graph, installed at startup by
;; jolt.deps. A claim on a class the runtime already IMPLEMENTS is dropped with
;; a warning, and the rest of the claim stands: a dependency does not get to
;; redefine what String means — the same posture is why extend-class! will not
;; replace a built-in — but a claim like that is a library from before the
;; runtime grew the class, not two dependencies disagreeing (RFC 0014's case,
;; jolt#914). It used to be refused outright, and when java.util.zip moved into
;; the runtime (jolt#916) every project pinned to an older jolt-lang/http-client
;; release stopped at resolution with nothing in its own deps.edn to change. The
;; runtime's class answers; the library's install namespace still autoloads for
;; whatever it alone provides, and its registrations for the overtaken class land
;; as any library's on a runtime class do (register-class-ctor-user!). The
;; warning names what to upgrade.
;;
;; "Implements" is the statics/ctor tables, not the class hierarchy. The
;; hierarchy knows names it does not implement — java.time.ZoneId is in it so
;; isa?/instance? answer correctly, while the implementation is jolt-lang/time's
;; to install. Checking the hierarchy rejected every provider for the classes it
;; exists to provide.
(define (register-class-provider! install-ns coordinate classes)
  (let* ((cs (class-spellings classes))
         (taken? (lambda (c) (or (hashtable-ref class-statics-tbl c #f)
                                 (hashtable-ref class-ctors-tbl c #f))))
         ;; the fully-qualified spellings the runtime provides, as declared
         (stale (filter (lambda (c) (taken? c)) classes))
         (kept (filter (lambda (c) (not (taken? c))) cs)))
    (when (pair? stale)
      (fprintf (current-error-port)
               "warning: ~a claims ~a, which this jolt provides; the runtime's ~a and the ~a dropped — upgrade ~a\n"
               install-ns
               (fold-left (lambda (a c) (if (string=? a "") c (string-append a ", " c))) "" stale)
               (if (null? (cdr stale)) "class answers" "classes answer")
               (if (null? (cdr stale)) "claim is" "claims are")
               (if coordinate (jolt-str-render-one coordinate) "the library")))
    (when (pair? kept)
      (let ((p (vector install-ns coordinate kept (box #f))))
        (set! lib-class-providers (append lib-class-providers (list p)))
        (lib-claim-pending! p)))))

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
(define (lib-load-provider! p)
  ;; claim the load: whoever flips the latch from #f does it, everyone else sees
  ;; a provider that has already had its chance and moves on.
  (and (jolt-with-mutex lib-claims-mu
         (and (not (unbox (vector-ref p 3)))
              (begin (set-box! (vector-ref p 3) 'ok)
                     (lib-claim-settled! p)
                     #t)))
       ;; The claim is settled from here whatever happens next, so registrations
       ;; held against it are replayed on every exit — including the install
       ;; namespace being off the roots, and the one that raises. A provider that
       ;; cannot deliver leaves the class to whoever else registered it, which is
       ;; what happened before the claim was honoured at all.
       ;;
       ;; Either half is a reason for the caller to look again, so the answer is
       ;; their OR: a class can become resolvable through a replay alone.
       (let* ((loaded (and (find-ns-file (vector-ref p 0))
                           (begin (guard (c (#t (set-box! (vector-ref p 3) 'failed)
                                                (lib-replay-deferred! p)
                                                (raise c)))
                                    ;; the mark comes from load-namespace* itself
                                    ;; (lib-with-install-ns-mark), which is the
                                    ;; only way it can also cover an install
                                    ;; namespace reached by a plain require.
                                    (load-namespace (vector-ref p 0)))
                                  #t)))
              (replayed (lib-replay-deferred! p)))
         (or loaded replayed))))

;; RFC 0014's resolution step: a class reference that MISSES the registry
;; autoloads the provider that declares the class, and retries.
;;
;; A miss is enough because a claimed class cannot be a HIT before its claimer has
;; loaded — register-class-provider! drops a claim on a class the runtime
;; already implements, and lib-pending-claimer holds any other library's
;; registration until the claim settles. That is what makes resolution a property
;; of the dependency graph rather than of compile order (jolt#914): the table hit
;; that used to serve an undeclared registration — jolt.crypto registers an
;; EC-only java.security.Signature while declaring only the symmetric classes —
;; never forms, so the claimer still autoloads and still wins.
;;
;; Keeping it on the miss path is also what keeps it off the hot one: every
;; static reference and every (Class. ...) would otherwise pay a lookup here, and
;; jolt.time.base / jolt.socket leave a claim pending in almost every program, so
;; there is no steady state in which that lookup goes away.
(define (lib-try-autoload! class)
  (and lib-any-pending-claims?
       (let ((p (hashtable-ref lib-pending-claims-tbl class #f)))
         (and p (lib-load-provider! p)))))

;; The provider that DECLARES this class and has not had its chance yet — meaning
;; the registration about to happen is somebody else's, and must wait. #f when
;; nothing claims the class, when the claim has already settled, or when this IS
;; the claimer registering.
(define (lib-pending-claimer name)
  (and lib-any-pending-claims?
       (let ((p (hashtable-ref lib-pending-claims-tbl name #f)))
         (and p
              (not (eq? p (lib-loading-provider)))
              ;; An install namespace pulled in by a plain require rather than by
              ;; the autoload carries no lib-loading-provider mark, and its own
              ;; registrations must not be held against it.
              (not (ns-dedup-loaded? (vector-ref p 0)))
              p))))

;; ---- the registration guard -------------------------------------------------
;; A class a dependency DECLARES is that dependency's to implement, and RFC 0014
;; already refuses two libraries claiming one class (jolt.deps host-class-providers)
;; — so a claimed class has exactly one implementor and the only thing that can
;; take it away is an undeclared side-effect registration from someone else's
;; install!. That is what made resolution depend on compile order: whoever
;; registered last decided what java.security.Signature meant, and who registered
;; last depended on which namespace happened to compile first.
;;
;; So once the declared provider has registered a class member, a registration of
;; that member from anywhere else is dropped. Loudly: the library asked for
;; something it did not get, and the symptom otherwise shows up somewhere else
;; entirely. Members the provider does NOT answer still go through — a shim with a
;; gap in it is the case class-extensions.ss exists for, and a claim is authority
;; over what the provider implements, not a reservation on the name.
(define (lib-provider-owner-elsewhere name)
  (let ((p (hashtable-ref lib-provider-owned-tbl name #f)))
    (and p (not (eq? p (lib-loading-provider))) p)))

;; One warning per class, not per registration: an install namespace registers
;; the fully-qualified name, the simple name, and a constructor for the same
;; class, and four copies of one message reads like four problems.
(define lib-claim-warned-tbl (make-hashtable string-hash string=?))
(define (claim-warn-once? tag name)
  (let ((k (string-append tag "/" (short-class-name name))))
    (and (not (hashtable-ref lib-claim-warned-tbl k #f))
         (begin (hashtable-set! lib-claim-warned-tbl k #t) #t))))

(define (provider-claim-drop! name owner members)
  (when (claim-warn-once? "drop" name)
    (fprintf (current-error-port)
             "warning: dropping ~a — ~a declares that class (:jolt/provides, RFC 0014) and implements ~a; a library may only register the classes it declares\n"
             (if (null? members)
                 (string-append "a constructor registration for " name)
                 (string-append "a registration for "
                                (fold-left (lambda (a m)
                                             (let ((one (string-append name "/" m)))
                                               (if (string=? a "") one (string-append a ", " one))))
                                           "" members)))
             (or (vector-ref owner 1) (vector-ref owner 0))
             (cond ((null? members) "it")
                   ((null? (cdr members)) "that member")
                   (else "those members")))))

;; The same registration BEFORE the claimer has loaded costs nobody the class:
;; it is held, the claimer autoloads on the first reference and registers, and
;; whatever the claimer left unanswered lands after it. So this is a note, not the
;; refusal provider-claim-drop! reports — but it is still a library registering a
;; class it did not declare, which is the thing to fix at the source, so say so
;; under JOLT_DEBUG the way the other registry diagnostics do.
(define (provider-claim-hold! name pending members)
  (when (and (getenv "JOLT_DEBUG") (claim-warn-once? "hold" name))
    (fprintf (current-error-port)
             "warning: holding ~a — ~a declares that class (:jolt/provides, RFC 0014) and has not loaded yet; it loads on the first reference to ~a, and what it does not implement is registered after it\n"
             (if (null? members)
                 (string-append "a constructor registration for " name)
                 (string-append "a registration for "
                                (fold-left (lambda (a m)
                                             (let ((one (string-append name "/" m)))
                                               (if (string=? a "") one (string-append a ", " one))))
                                           "" members)))
             (vector-ref pending 0) name)))

;; The contract from the other side: an install namespace registering a class it
;; does not declare. Nothing autoloads a provider for a class it never claimed, so
;; whether that class resolves at all depends on what else happens to pull the
;; namespace in first — which is how a reference to java.security.KeyPairGenerator
;; reported "No dependency provides" in one namespace and answered an EC-only shim
;; in the next (jolt#914).
(define (provider-claim-note! name)
  (when (getenv "JOLT_DEBUG")
    (let ((self (lib-loading-provider)))
      (when (and self (not (member name (vector-ref self 2)))
                 ;; ...but not for a class the RUNTIME implements. There the
                 ;; declaration the note asks for is refused ("which the runtime
                 ;; already provides"), so the advice cannot be taken: registering
                 ;; the members at install IS the route, and it is the additive
                 ;; case class-extensions.ss exists for. Nothing autoloads for a
                 ;; class that is already there either, so there is no order
                 ;; dependence left to warn about (jolt#926).
                 (not (runtime-provides-class? name))
                 (claim-warn-once? "note" name))
        (fprintf (current-error-port)
                 "warning: ~a registers ~a without declaring it in :jolt/provides (RFC 0014); nothing autoloads ~a for a class it does not declare, so whether ~a resolves depends on what else pulls that namespace in\n"
                 (vector-ref self 0) name (vector-ref self 0) name)))))

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

;; A simple name the current namespace IMPORTED names the class its import
;; bound — (:import (java.security.cert CertificateFactory)) binds
;; CertificateFactory to that class token — so the message is about
;; java.security.cert.CertificateFactory and says a library provides it, not
;; "Unknown class CertificateFactory", which reads as a typo.
(define (imported-class-fqn class)
  (let ((c (var-cell-lookup (chez-current-ns) class)))
    (and c (var-cell-defined? c)
         (let ((root (var-cell-root c)))
           (and (jhost? root) (string=? (jhost-tag root) "class")
                (vector-ref (jhost-state root) 0))))))

;; The class a static call names, spelled as the JVM reports it: a qualified
;; name as written, an imported simple name through its import, and one of the
;; default java.lang imports (Math, String, Thread) through ns.ss's canonical
;; table.
(define (static-class-fqn class)
  (cond ((memv #\. (string->list class)) class)
        ((imported-class-fqn class))
        ((member class jolt-default-import-names) (jolt-default-import-canonical class))
        (else class)))

(define (unknown-class-message class)
  (let ((class (or (imported-class-fqn class) class)))
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
    (else (string-append "Unknown class " class)))))

;; ---- emit entry points ------------------------------------------------------
;; A qualified reference whose namespace segment names a live namespace — directly
;; or through a require :as alias — is a missing VAR, not a missing class. The
;; analyzer reads any unresolved ns/name as a host static, so a typo in a
;; clojure.string call used to report "Unknown class s", naming the alias as a
;; class and sending the reader looking in the wrong place.
(define (static-miss-message class member)
  (let ((target (or (chez-resolve-alias (chez-current-ns) class) class)))
    (cond
      ((chez-ns-exists? target)
       (string-append "No such var: " class "/" member))
      ;; A class the RUNTIME implements is not a missing dependency, whatever the
      ;; member lookup did: jolt registers a constructor and an instance method
      ;; table for java.lang.StringBuilder and no statics at all, so the first
      ;; StringBuilder/... reference fell off the end of the class table and
      ;; reported "No dependency provides java.lang.StringBuilder" — advice that
      ;; cannot be taken, since register-class-provider! drops a claim on a
      ;; class the runtime already provides. The miss is the MEMBER's, and that is
      ;; the same message the class-with-statics path already gives (jolt#983).
      ((runtime-provides-class? (or (imported-class-fqn class) class))
       (string-append "No matching field or method: " class "/" member))
      (else (unknown-class-message class)))))

;; A member the class does not have is a miss, including a java.lang.Class
;; instance method: (Math/getName) and (. Math getName) have no static to call on
;; the JVM either. (.getName Math) is the Class-object call, and it reaches the
;; class value through the instance path, not through here. This used to fall
;; back to the Class instance table, which answered "Math" for (Math/getName)
;; where the JVM refuses the call.
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
                    (throw-jvm (quote IllegalArgumentException) (string-append "No matching field or method: " class "/" member))
                    v))
              ;; class miss — autoload the provider that declares the class (the
              ;; java.time base, jolt.socket, or a library that installs it) and
              ;; retry once. A claimed class cannot be a hit before its claimer
              ;; has loaded, so the miss is where resolution belongs (jolt#914).
              (if (lib-try-autoload! class)
                  (host-static-ref class member)
                  (throw-jvm (quote IllegalArgumentException) (static-miss-message class member))))))))

(define (host-static-call class member . args)
  ;; the registry's one rule: a procedure is a method to call, anything else is
  ;; a field value — which answers a zero-argument access and nothing more.
  ;; Applying the field's value used to raise Chez's bare "attempt to apply
  ;; non-procedure" with no message.
  (let ((v (host-static-ref class member)))
    (cond ((procedure? v)
           (if (host-arity-ok? v (length args) #f)
               (apply v args)
               ;; the compiler's miss for a static call with no overload of that
               ;; arity — (String/valueOf ca 0) is "No matching method valueOf
               ;; found taking 2 args for class java.lang.String", not
               ;; valueOf(char[]) with the 0 thrown away.
               (throw-jvm (quote IllegalArgumentException)
                 (string-append "No matching method " member " found taking "
                                (number->string (length args)) " args for class "
                                (static-class-fqn class)))))
          ((null? args) v)
          (else (throw-jvm (quote IllegalArgumentException)
                  (string-append class "/" member " is a static field; it takes no arguments"))))))

;; ---- per-site caches -----------------------------------------------------------
;; The emitter hoists one of these per `Class/member` site (backend_scheme.clj),
;; the same shape as jolt-instance-site. Without it every evaluation hashed the
;; class and member strings two or three times over — mutable-static-cell, then
;; lookup-class, then the member table — which put Long/MIN_VALUE at ~58 ns and a
;; Long/numberOfLeadingZeros call at ~142 ns against the JVM's ~2, and made
;; test.check's generators spend a fifth of their time in string-hash.
;;
;; A site holds #f or #(epoch kind value): kind 'cell is a mutable static's cell
;; (read through on every hit, so a set-static-field! is seen), 'val a registered
;; value, 'proc the procedure a call site applies. It is valid while the registry
;; epoch has not moved (host-static-epoch above). A miss that raises or autoloads
;; caches nothing and takes the uncached path, so its errors and its provider
;; loading stay exactly host-static-ref's.
(define (host-static-site-make) (vector #f))
(define (host-static-site-hit site)
  (let ((st (vector-ref site 0)))
    (and st (fx=? (vector-ref st 0) (host-static-epoch)) st)))
;; What CLASS/MEMBER resolves to without raising or loading anything:
;; (values kind value), or (values #f #f) when only the slow path can answer.
(define (host-static-resolve class member)
  (let ((cell (mutable-static-cell class member #f)))
    (if cell
        (values 'cell cell)
        (let ((h (lookup-class class-statics-tbl class)))
          (if h
              (let ((v (hashtable-ref h member host-static-miss)))
                (if (eq? v host-static-miss) (values #f #f) (values 'val v)))
              (values #f #f))))))

(define (host-static-ref-site site class member)
  (let ((st (host-static-site-hit site)))
    (if st
        (let ((v (vector-ref st 2)))
          (if (eq? (vector-ref st 1) 'cell) (vector-ref v 0) v))
        (host-static-ref-site-miss site class member))))
(define (host-static-ref-site-miss site class member)
  ;; the epoch is read BEFORE resolving: a write landing after it leaves the entry
  ;; already stale rather than valid for a table it never saw
  (let ((e (host-static-epoch)))
    (memory-order-acquire)
    (let-values (((kind v) (host-static-resolve class member)))
      (if kind
          ;; a release before the store that publishes the entry: on a weakly
          ;; ordered machine (ARM64) another thread could otherwise see the new
          ;; entry before its slots, and apply a stale procedure
          (begin (memory-order-release)
                 (vector-set! site 0 (vector e kind v))
                 (if (eq? kind 'cell) (vector-ref v 0) v))
          (host-static-ref class member)))))

;; A call site: (host-static-proc-site site class member n) answers the procedure
;; the n-argument call applies, so the call itself is a plain application with no
;; rest list. A field read with no arguments answers a thunk over the field. What
;; host-static-call would raise (an arity miss, a field given arguments, an
;; unknown member) is never cached: the answer is a procedure that makes the
;; uncached call, so the error, and any provider load, happen exactly as before.
(define (host-static-proc-site site class member n)
  (let ((st (host-static-site-hit site)))
    (if st
        (vector-ref st 2)
        (host-static-proc-site-miss site class member n))))
(define (host-static-proc-site-miss site class member n)
  (let ((e (host-static-epoch)))
    (memory-order-acquire)
    (let-values (((kind v) (host-static-resolve class member)))
      (let ((p (cond
                 ((not kind) #f)
                 ;; a mutable static's value can change between calls, and whether
                 ;; it is a procedure decides call-or-field per call; leave it on
                 ;; the uncached path (they are clojure.lang.RT/Compiler flags)
                 ((eq? kind 'cell) #f)
                 ((procedure? v) (and (host-arity-ok? v n #f) v))
                 ((fx=? n 0) (lambda () v))
                 (else #f))))
        (if p
            (begin (memory-order-release) (vector-set! site 0 (vector e 'proc p)) p)
            (lambda args (apply host-static-call class member args)))))))

;; (. Class member) with no arguments is ambiguous on the JVM too: it reads a
;; static FIELD when one exists and otherwise calls a no-arg static method. jolt
;; keeps one registry for both, so the decision is by what is registered — a
;; procedure is a method to call, anything else is a field value. Without this
;; the dot form applied a field's value as a zero-arg procedure.
(def-var! "jolt.host" "static-member"
  (lambda (class member)
    (let ((v (host-static-ref class member)))
      (if (procedure? v) (v) v))))

;; The ctor for CLASS, cached eq? on the name object: (Name. …) compiles to
;; (host-new "ns.Name" …) with the name as a literal, so every construction at a
;; site passes the same string, and lookup-class hashed it — twice on a miss of
;; the qualified spelling — per object built. core.logic builds a Substitutions
;; per binding step. Stamped with class-ctor-epoch, read BEFORE the lookup so a
;; racing registration can only understamp; a miss is not cached, so the
;; autoload and var paths below still run every time they are needed.
(define host-new-cache (make-weak-eq-hashtable))
(define host-new-cache-mu (make-mutex))
(define (host-new-ctor class)
  (let ((e (hashtable-ref host-new-cache class #f)))
    (if (and e (fx= (car e) class-ctor-epoch))
        (cdr e)
        (let* ((epoch class-ctor-epoch)
               (ctor (lookup-class class-ctors-tbl class)))
          (when ctor
            (jolt-with-mutex host-new-cache-mu
              (hashtable-set! host-new-cache class (cons epoch ctor))))
          ctor))))
(define (host-new class . args)
  (let ((ctor (host-new-ctor class)))
    (cond
      (ctor (apply ctor args))
      ;; the constructor may live in a provider that has not loaded yet — autoload
      ;; and retry once before falling through to the var / no-ctor paths.
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
