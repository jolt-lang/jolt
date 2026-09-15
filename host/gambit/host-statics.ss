;; host-statics.ss — the Gambit target's host-interop tier: the jhost record,
;; the method registry and the (.method obj) arm over it, the entry points the
;; emitter lowers Class/member and (new Class) to, and the statics and
;; constructors the SEED reaches.
;;
;; On Chez this is host-static.ss (registries, jhost, entry points),
;; host-static-methods.ss (every Class/member) and host-static-classes.ss
;; (every instantiable class) — the java.lang/util/io surface portable cljc
;; code calls. This target boots none of those; what it carries is the
;; registries in the same shape, so the shared java/ files that register into
;; them (class-model.ss, string-builder.ss) load unchanged, plus the members
;; clojure.core and the embedded stdlib call — the analyzer emits them as
;; Scheme-level (host-static-call "Class" "member" …) forms, so each is either
;; registered here or a raise naming the class. `make gambitstatics` pins the
;; set: every static and constructor the seed emits resolves, or is a
;; classified line in seed-statics-allowlist.txt.
;;
;; Loaded before the prelude, after records-gambit.ss (the dispatch-arm
;; registry) and java-parse.ss (the NumberFormatException family); the shared
;; class-model.ss and string-builder.ss follow it.

;; ---- host object --------------------------------------------------------------
;; The generic carrier for a host value: a tag naming its method table, and
;; mutable state. A Class object is a jhost tagged "class" (class-model.ss), a
;; StringBuilder one tagged "string-builder" (string-builder.ss).
(define-record-type jhost (fields tag (mutable state)) (nongenerative gambit-jhost-v1))

;; ---- registries ---------------------------------------------------------------
;; class-ctors-tbl / register-class-ctor! are rt-core.ss's (a deftype registers
;; its ctor there under its tag); class-statics-tbl / register-class-statics!
;; are prelude-shims.ss's (compile-eval.ss registers the Compiler statics into
;; it at load). The method registry is this file's.
(define host-methods-tbl (make-hashtable string-hash string=?))   ; tag -> (method-ht)
(define (register-host-methods! tag members)
  (let ((h (or (hashtable-ref host-methods-tbl tag #f)
               (let ((h (make-hashtable string-hash string=?)))
                 (hashtable-set! host-methods-tbl tag h) h))))
    (for-each (lambda (p) (hashtable-set! h (car p) (cdr p))) members)))
;; A class token may arrive fully qualified (java.io.StringReader) or short
;; (StringReader): exact first, then by last dotted segment.
(define (lookup-class h-tbl name)
  (or (hashtable-ref h-tbl name #f)
      (hashtable-ref h-tbl (short-class-name name) #f)))

;; (.method obj …) on a jhost: the tag's table, else — for a Class token — the
;; statics of the class it names, which is what the form means on the JVM for
;; an imported simple name; a leading dash is the explicit FIELD spelling.
(register-method-arm! arm-priority-host-type
  (lambda (obj method-name rest-args)
    (cond
      ((jhost? obj)
       (let* ((mh (hashtable-ref host-methods-tbl (jhost-tag obj) #f))
              (f (and mh (hashtable-ref mh method-name #f)))
              (args (if (jolt-nil? rest-args) '() (seq->list rest-args))))
         (cond
           (f (apply f obj args))
           ((string=? (jhost-tag obj) "class")
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
           (else (dispatch-miss obj method-name args)))))
      (else 'pass))))

;; ---- emit entry points --------------------------------------------------------
;; (Class/member args) is emitted as (host-static-call "Class" "member" args), a
;; static field read as (host-static-ref …), (new Class args) as (host-new
;; "Class" args) — Scheme-level calls in the seed, not var references, so the
;; three are GLOBALS here as well as clojure.core vars. A miss raises a named
;; UnsupportedOperationException: the member is one this target does not shim,
;; and the call site says which.
(define host-static-miss (list 'host-static-miss))
(define (host-static-lookup class member)
  (let ((h (lookup-class class-statics-tbl class)))
    (if h (hashtable-ref h member host-static-miss) host-static-miss)))
;; (.getName String) — an instance member on a class token — is a call on the
;; java.lang.Class OBJECT when the class has no such static, as on the JVM.
(define (class-instance-fallback class member)
  (let ((h (hashtable-ref host-methods-tbl "class" #f)))
    (and h
         (let ((m (hashtable-ref h member #f)))
           (and m (lambda args (apply m (jolt-class-for class) args)))))))
(define (host-static-ref class member)
  (let ((v (host-static-lookup class member)))
    (if (eq? v host-static-miss)
        (or (and (jch-known? class) (class-instance-fallback class member))
            (jolt-throw
              (jolt-host-throwable
                "java.lang.UnsupportedOperationException"
                (string-append class "/" member
                               " is unsupported on the gambit target: no shim registers it"
                               " (host/gambit/host-statics.ss)"))))
        v)))
(define (host-static-call class member . args)
  ;; a procedure is a method to call, anything else a field value — which
  ;; answers a zero-argument access and nothing more
  (let ((v (host-static-ref class member)))
    (cond ((procedure? v) (apply v args))
          ((null? args) v)
          (else (throw-jvm (quote IllegalArgumentException)
                  (string-append class "/" member " is a static field; it takes no arguments"))))))
(def-var! "clojure.core" "host-static-call" host-static-call)
(def-var! "clojure.core" "host-static-ref" host-static-ref)
;; (Class/member) with no arguments is a field read when a field is registered
;; and a no-arg call otherwise — the analyzer's jolt.host/static-member.
(def-var! "jolt.host" "static-member"
  (lambda (class member)
    (let ((v (host-static-ref class member)))
      (if (procedure? v) (v) v))))

;; Two kinds of class exist on this target beyond the shims registered below:
;; the exception hierarchy, which every core throw site constructs ((new
;; ClassCastException msg) is what (zero? "a") raises), and a deftype/defrecord,
;; whose type name is a var holding its ctor — the same two arms Chez's host-new
;; (host-static.ss) takes. The throwable ctors are derived from the ONE
;; hierarchy in class-hierarchy.ss the way host-static-classes.ss derives them,
;; so (E. msg), (E. msg cause), (E. cause) and (E.) all build the typed
;; throwable. Anything else names a class this target has no shim for, and says so.
(define (gambit-exc-ctor canonical)
  (lambda args
    (let* ((a0 (if (pair? args) (car args) jolt-nil))
           (rest (if (pair? args) (cdr args) '()))
           (cause (if (pair? rest) (car rest) jolt-nil)))
      (cond
        ((string? a0) (jolt-host-throwable canonical a0 cause))
        ((jolt-nil? a0) (jolt-host-throwable canonical jolt-nil))
        ((and (null? rest) (ex-info-map? a0)) (jolt-host-throwable canonical jolt-nil a0))
        (else (jolt-host-throwable canonical (jolt-str-render-one a0) cause))))))
(let-values (((keys vals) (hashtable-entries jvm-class-parents)))
  (vector-for-each
    (lambda (canonical supers)
      (when (jch-isa? canonical "Throwable")
        (let ((short (jch-last-segment canonical)))
          (register-class-ctor! short (gambit-exc-ctor canonical))
          (unless (string=? short canonical)
            (register-class-ctor! canonical (gambit-exc-ctor canonical))))))
    keys vals))
(define (host-new class . args)
  (let ((ctor (lookup-class class-ctors-tbl class)))
    (cond
      (ctor (apply ctor args))
      (else
       (let ((cell (or (var-cell-lookup (chez-current-ns) class)
                       (var-cell-lookup "clojure.core" class))))
         (if (and cell (var-cell-defined? cell) (procedure? (var-cell-root cell)))
             (apply (var-cell-root cell) args)
             (jolt-throw
               (jolt-host-throwable
                 "java.lang.UnsupportedOperationException"
                 (string-append "(new " class ") is unsupported on the gambit target: "
                                "no shim registers the class (host/gambit/host-statics.ss)")))))))))
(def-var! "clojure.core" "host-new" (lambda (c . a) (apply host-new c a)))

;; ---- the statics the seed reaches -----------------------------------------------
;; Each is the member Chez registers in host-static-methods.ss, with the same
;; body, so a call answers the same on both targets. Long/parseLong is
;; cl-format's number parser (java-parse.ss carries the throw); Math/floor and
;; Math/abs are cl-format's float formatting; Character/isWhitespace and
;; String/join are clojure.main's; the clojure.lang.Util family, MapEntry/create
;; and Murmur3/hashOrdered are the gvec's (clojure/core/60-gvec.clj);
;; PersistentList/EMPTY the gvec's empty; RT/REQUIRE_LOCK the object
;; serialized-require locks. The Compiler statics (munge, demunge, eval) are
;; registered by compile-eval.ss at its load, into the same table.
(register-class-statics! "Long"
  (list (cons "parseLong" (lambda (s . r) (parse-int-or-throw s (if (null? r) 10 (jnum->exact (car r))) "long")))))
(register-class-statics! "Math"
  (list (cons "floor" (lambda (x) (exact->inexact (floor (jolt-need-num x)))))
        (cons "abs" (lambda (x) (abs x)))))
;; JVM Character.isWhitespace: Unicode whitespace MINUS the no-break spaces the
;; JVM excludes (U+00A0/U+2007/U+202F).
(register-class-statics! "Character"
  (list (cons "isWhitespace" (lambda (c) (let ((cp (char-code c)))
                                           (and (char-whitespace? (integer->char cp))
                                                (not (fx=? cp #xA0)) (not (fx=? cp #x2007)) (not (fx=? cp #x202F))))))))
;; String.join(delim, elems) — elems as a collection or spread as varargs
(register-class-statics! "String"
  (list (cons "join" (lambda (delim . parts)
                       (let ((items (if (and (fx=? (length parts) 1)
                                             (not (string? (car parts))))
                                        (seq->list (jolt-seq (car parts)))
                                        parts)))
                         (jolt-str-join-strs (map jolt-str-render-one items)
                                             (jolt-str-render-one delim)))))))
(let ((util-statics
       (list (cons "hash" (lambda (x) (if (jolt-nil? x) 0 (record-method-dispatch x "hashCode" jolt-nil))))
             (cons "hasheq" (lambda (x) (jolt-hash x)))
             (cons "equiv" (lambda (a b) (if (jolt= a b) #t #f)))
             (cons "identical" (lambda (a b) (if (eq? a b) #t #f)))
             (cons "compare" (lambda (a b) (jolt-compare a b)))
             (cons "isInteger" (lambda (x) (if (and (number? x) (exact? x) (integer? x)) #t #f)))
             (cons "equals" (lambda (a b) (if (jolt= a b) #t #f))))))
  (register-class-statics! "Util" util-statics)
  (register-class-statics! "clojure.lang.Util" util-statics))
(register-class-statics! "Murmur3" (list (cons "hashOrdered" (lambda (xs) (hash-ordered (jolt-seq xs))))))
(register-class-statics! "MapEntry" (list (cons "create" (lambda (k v) (make-map-entry k v)))))
(register-class-statics! "clojure.lang.MapEntry" (list (cons "create" (lambda (k v) (make-map-entry k v)))))
(register-class-statics! "PersistentList" (list (cons "EMPTY" jolt-empty-list)))
(register-class-statics! "clojure.lang.PersistentList" (list (cons "EMPTY" jolt-empty-list)))
;; (Object.) — a fresh value with distinct identity: libraries use it as a lock
;; or a unique sentinel, and RT/REQUIRE_LOCK is one.
(register-class-ctor! "Object" (lambda _ (make-jhost "object" (vector))))
(register-class-arm! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "object")))
                     (lambda (x) "java.lang.Object"))
(define rt-require-lock (make-jhost "object" (vector)))
(register-class-statics! "RT" (list (cons "REQUIRE_LOCK" rt-require-lock)))
(register-class-statics! "clojure.lang.RT" (list (cons "REQUIRE_LOCK" rt-require-lock)))
;; System/getProperty: the JVM's spellings, over what the adapter can answer.
;; os.name follows sa-os-family like Chez's (host-static-methods.ss); a key
;; nothing here knows answers the default, else nil.
(define (gambit-get-property k . dflt)
  (let ((k (jolt-need-string k)))
    (cond ((string=? k "os.name") (case (sa-os-family)
                                    ((macos) "Mac OS X")
                                    ((windows) "Windows")
                                    (else "Linux")))
          ((string=? k "line.separator") "\n")
          ((string=? k "file.separator") "/")
          ((string=? k "path.separator") ":")
          ((string=? k "user.dir") (current-directory))
          ((string=? k "user.home") (or (getenv "HOME" #f) ""))
          ((string=? k "java.io.tmpdir") (or (getenv "TMPDIR" #f) "/tmp"))
          ((pair? dflt) (car dflt))
          (else jolt-nil))))
(register-class-statics! "System" (list (cons "getProperty" gambit-get-property)))
