;; class-extensions.ss — the library seam for extending a class jolt already
;; part-shims (jolt#575).
;;
;; jolt models the java.* surface with hand-written shims (io.ss, process.ss,
;; inst-time.ss, host-static-classes.ss, …) and each one covers the methods
;; jolt's own code and the ported libraries have needed so far. A library that
;; needs a method a shim does not have had exactly one way out before this:
;; __register-class-ctor! its OWN replacement for the whole class, which every
;; other namespace in the process then silently inherits — the failure mode
;; register-class-ctor-user! warns about, where swapping in a ByteArrayInputStream
;; shim makes (.readAllBytes body) unresolvable for code that never asked for it.
;; This file is the additive alternative: a library registers just the members it
;; needs against the class, and jolt's own shim keeps answering everything else.
;;
;; Two tiers, because "the shim is missing a method" and "the shim's method is
;; wrong for me" are different asks with different blast radius:
;;
;;   extend (the default)  consulted at the END of the dispatch chain, where the
;;                         call would otherwise raise "No matching method"
;;                         (dispatch-miss, records-dispatch.ss). It cannot change
;;                         the behaviour of anything jolt already answers, so two
;;                         libraries extending the same class only collide if
;;                         they claim the same missing method.
;;   override              consulted BEFORE every built-in arm
;;                         (arm-priority-user-override). It replaces jolt's
;;                         method for every caller in the process, so it is
;;                         spelled out at the call site and reported under
;;                         JOLT_DEBUG.
;;
;; Both are keyed by CLASS NAME and resolved through the modeled class graph
;; (jch-tags, class-hierarchy.ss), so a registration on java.io.Reader answers
;; for a StringReader receiver, and either spelling — java.io.File or File —
;; matches. The class name of a receiver comes from jolt-class-name, the one
;; place a value's class is decided, so this seam sees exactly the classes
;; (class x) and instance? report — which is also how a library's OWN value type
;; becomes extensible: teach (class x) about it with clojure.core/__register-class!
;; and it is addressable here by that name like any modeled class.
;;
;; Loaded LAST (after bigdec.ss): it only registers into existing registries, and
;; loading last means every class-arm that decides a value's class name is in
;; place before the first lookup can run.

;; ---- registries -------------------------------------------------------------
;; class name -> (method-name -> proc). Separate tables, not one table with a
;; per-member flag, because the two tiers are consulted from different places in
;; the chain and a lookup must never consider the other tier's members.
(define class-ext-override-tbl (make-hashtable string-hash string=?))
(define class-ext-extend-tbl   (make-hashtable string-hash string=?))
;; Registration is a probe-then-create on a shared table, so it takes a lock for
;; the same reason register-tagged-methods! does: two namespaces loading in
;; parallel and registering against one class each built their own inner table
;; and published it over the other's. Reads stay unlocked (strong hashtables).
(define class-ext-mu (make-mutex))
;; The override arm is registered on FIRST use, not at load: a process that never
;; extends a class must not pay a per-.method-call arm walk for a feature it does
;; not use. The arm still tests the table size on every call after that, because
;; an arm cannot be unregistered and the unit gate clears these tables between
;; cases.
(define class-ext-arm-registered? #f)

(define (class-ext-table-put! tbl name members)
  (jolt-with-mutex class-ext-mu
    (let ((h (or (hashtable-ref tbl name #f)
                 (let ((nh (make-hashtable string-hash string=?)))
                   (hashtable-set! tbl name nh) nh))))
      (for-each (lambda (p) (hashtable-set! h (car p) (cdr p))) members))))

;; Clear both tiers. Only the unit gate calls this (per-case isolation): a
;; registration is process-wide by design, and nothing in a normal run undoes one.
(define (class-ext-reset!)
  (jolt-with-mutex class-ext-mu
    (hashtable-clear! class-ext-override-tbl)
    (hashtable-clear! class-ext-extend-tbl)))

;; The member registered for OBJ's class (or the nearest ancestor that has one),
;; or #f. jch-tags is the class's own name, its simple name, then each ancestor
;; with its simple name, ending at Object — most specific first, which is the
;; resolution order a JVM override has. It is memoized behind the graph epoch, so
;; the steady-state cost here is one hashtable-ref per tag until a hit.
(define (class-ext-find tbl obj method-name)
  (and (fx>? (hashtable-size tbl) 0)
       (let ((cn (jolt-class-name obj)))
         (and (string? cn)
              (let loop ((tags (jch-tags cn)))
                (cond
                  ((null? tags) #f)
                  ((let ((h (hashtable-ref tbl (car tags) #f)))
                     (and h (hashtable-ref h method-name #f))) => values)
                  (else (loop (cdr tags)))))))))

;; ---- the two dispatch points -------------------------------------------------
;; Both handlers are named top-level procedures rather than lambdas written at
;; the registration site: the registration happens under class-ext-mu, and a
;; handler body that calls into library code (jolt-invoke) inside a locked region
;; is exactly what the park/lock check forbids — the body runs long after the
;; lock is released, but nothing in the text says so.
(define (class-ext-override-arm obj method-name rest-args)
  (let ((f (class-ext-find class-ext-override-tbl obj method-name)))
    (if f
        (apply jolt-invoke f obj (if (jolt-nil? rest-args) '() (seq->list rest-args)))
        'pass)))
(define (class-ext-fallback-lookup obj method-name)
  (class-ext-find class-ext-extend-tbl obj method-name))

;; Registered once, on the first override registration. Under the same mutex as
;; the tables: register-method-arm! rebuilds the arm list with a set!, so two
;; namespaces loading in parallel and each registering their first override
;; would otherwise publish two lists and lose one of the arms.
(define (class-ext-arm!)
  (jolt-with-mutex class-ext-mu
    (unless class-ext-arm-registered?
      (set! class-ext-arm-registered? #t)
      (register-method-arm! arm-priority-user-override class-ext-override-arm))))

;; The extend tier hangs off dispatch-miss (records-dispatch.ss). Installed on
;; the first extend registration for the same reason the arm is: until then the
;; miss path is byte-for-byte what it was.
(define class-ext-fallback-installed? #f)
(define (class-ext-install-fallback!)
  (jolt-with-mutex class-ext-mu
    (unless class-ext-fallback-installed?
      (set! class-ext-fallback-installed? #t))))

;; Every value is a java.lang.Object, so the methods every class inherits from
;; it answer for a value no tier claimed, after the extend tier: hashCode and
;; equals by identity, toString as Class@hash, getClass. Without this a host
;; value with no method table of its own -- a java.util.regex.Pattern -- refused
;; (.hashCode p) with "No matching field found", and so did the str of a record
;; holding one, whose hash asks each field for its hashCode. A value whose class
;; overrides one of these answers through its own table before this is reached.
(define (object-method-fallback method-name)
  (define (miss name)
    (lambda (o . args) (no-method-throw name o (length args))))
  (cond ((string=? method-name "hashCode")
         (case-lambda ((o) (->num (jolt-identity-hasheq o))) ((o . r) (apply (miss "hashCode") o r))))
        ((string=? method-name "equals")
         (case-lambda ((o x) (eq? o x)) ((o . r) (apply (miss "equals") o r))))
        ((string=? method-name "toString")
         (case-lambda ((o) (string-append (jolt-class-name o) "@" (jolt-identity-hex o)))
                      ((o . r) (apply (miss "toString") o r))))
        ((string=? method-name "getClass")
         (case-lambda ((o) (jolt-class o)) ((o . r) (apply (miss "getClass") o r))))
        (else #f)))
(set-class-ext-fallback-hook!
  (lambda (obj method-name)
    (or (and class-ext-fallback-installed? (class-ext-fallback-lookup obj method-name))
        (object-method-fallback method-name))))

;; ---- the Clojure-facing seam -------------------------------------------------
;; (jolt.host/extend-class! class spec)
;;
;;   class  a class-name string ("java.io.File" or "File"), a Class value
;;          (the bare token java.io.File evaluates to one), or a symbol.
;;   spec   {:methods  {"name" (fn [self & args] …)}   instance methods
;;           :statics  {"name" val-or-fn}              Class/member statics
;;           :ctor     (fn [& args] …)                 (Class. args)
;;           :override true}                           methods win over jolt's
;;
;; A method name with a leading dash — "-size" — answers the FIELD spelling
;; (.-size x), the same distinction the rest of the dot-form surface keeps.
(define kw-ce-methods  (keyword #f "methods"))
(define kw-ce-statics  (keyword #f "statics"))
(define kw-ce-ctor     (keyword #f "ctor"))
(define kw-ce-override (keyword #f "override"))

(define (class-ext-bad! msg) (throw-jvm (quote IllegalArgumentException) msg))

;; class -> canonical name string. A Class value carries its name; a symbol or
;; keyword is taken at its name; a string is used as given (either spelling
;; resolves, see class-ext-find).
(define (class-ext-name cls)
  (cond
    ((string? cls) cls)
    ((jclass? cls) (jclass-name cls))
    ((symbol-t? cls) (symbol-t-name cls))
    ((keyword-t? cls) (keyword-t-name cls))
    (else (class-ext-bad!
            (string-append "extend-class!: expected a class or class name, got: "
                           (jolt-final-str cls))))))

;; A member map -> an alist of (name-string . value), with the name coerced from
;; whichever of string / symbol / keyword the caller wrote it as. `check` names
;; what the value has to be, or #f to accept anything (statics hold values as
;; well as procedures).
(define (class-ext-members what cls m check)
  (unless (jolt-map? m)
    (class-ext-bad! (string-append "extend-class!: " what " for " cls " must be a map")))
  (let loop ((s (jolt-seq m)) (acc '()))
    (if (jolt-nil? s)
        (reverse acc)
        (let* ((e (jolt-first s))
               (k (jolt-nth e 0))
               (v (jolt-nth e 1))
               (name (cond ((string? k) k)
                           ((symbol-t? k) (symbol-t-name k))
                           ((keyword-t? k) (keyword-t-name k))
                           (else (class-ext-bad!
                                   (string-append "extend-class!: " what " key for " cls
                                                  " must be a name, got: " (jolt-final-str k)))))))
          (when (and check (not (procedure? v)))
            (class-ext-bad! (string-append "extend-class!: " what " " cls "/" name
                                           " must be a function, got: " (jolt-final-str v))))
          (loop (jolt-seq (jolt-rest s)) (cons (cons name v) acc))))))

;; Classes whose methods the BACKEND lowers directly at proven call sites: the
;; :target-type stamp (jolt.backend-scheme string-direct-emit /
;; keyword-direct-emit / sb-direct-emit) turns (.length s) on a receiver proven
;; to be one of these into a Chez primitive, and that call never reaches
;; record-method-dispatch. An override on such a class would therefore be
;; honoured at the sites that went through dispatch and silently skipped at the
;; sites that did not — one method with two behaviours in the same program.
;; Refuse the registration rather than ship that split.
;;
;; The refusal covers any class one of them INHERITS from, not just the three
;; names: overriding java.lang.CharSequence/length or java.lang.Object/toString
;; would reach a proven String receiver by the same graph walk class-ext-find
;; uses. Asking the graph rather than listing the ancestors is what keeps the two
;; from drifting when a row moves in class-hierarchy.ss.
;;
;; EXTENDING these is unaffected, and deliberately so: direct emit only covers
;; methods jolt already implements, and the extend tier only runs where nothing
;; answered — so no extend registration can ever be the one a direct emit
;; shadowed.
(define class-ext-direct-emit-classes
  '("java.lang.String" "clojure.lang.Keyword" "java.lang.StringBuilder"
    "java.lang.StringBuffer"))

(define (class-ext-check-override! cls)
  (let loop ((cs class-ext-direct-emit-classes))
    (cond
      ((null? cs) #t)
      ((jch-isa? (car cs) cls)
       (class-ext-bad!
         (string-append "extend-class!: cannot override methods on " cls
                        " — the compiler lowers " (car cs)
                        " receivers directly at proven call sites, so the override"
                        " would apply at some call sites and not others."
                        " Adding a method jolt does not have is allowed.")))
      (else (loop (cdr cs))))))

(define (jolt-extend-class! cls spec)
  (let ((name (class-ext-name cls)))
    (unless (jolt-map? spec)
      (class-ext-bad! (string-append "extend-class!: spec for " name " must be a map, got: "
                                     (jolt-final-str spec))))
    (let* ((methods  (jolt-get spec kw-ce-methods jolt-nil))
           (statics  (jolt-get spec kw-ce-statics jolt-nil))
           (ctor     (jolt-get spec kw-ce-ctor jolt-nil))
           (override (jolt-truthy? (jolt-get spec kw-ce-override #f))))
      (unless (jolt-nil? methods)
        (let ((ms (class-ext-members "methods" name methods #t)))
          (cond
            (override
             (class-ext-check-override! name)
             ;; Same reporting contract as a replaced constructor: routine when
             ;; two libraries deliberately shim one class, a bisect otherwise.
             (when (getenv "JOLT_DEBUG")
               (for-each
                 (lambda (p)
                   (fprintf (current-error-port)
                            "warning: a library overrode ~a/~a — every (.~a …) on a ~a in this process now runs its method, including in namespaces that never asked for it\n"
                            name (car p) (car p) name))
                 ms))
             (class-ext-table-put! class-ext-override-tbl name ms)
             (class-ext-arm!))
            (else
             (class-ext-table-put! class-ext-extend-tbl name ms)
             (class-ext-install-fallback!)))))
      (unless (jolt-nil? statics)
        ;; class-statics-merge! already merges into the class's existing member
        ;; table and reports a differing re-registration under JOLT_DEBUG. The
        ;; merge and not register-class-statics!: extending a class is not the
        ;; runtime coming to provide it (host-static.ss host-class-statics-tbl).
        (class-statics-merge! name (class-ext-members "statics" name statics #f)))
      (unless (jolt-nil? ctor)
        (unless (procedure? ctor)
          (class-ext-bad! (string-append "extend-class!: :ctor for " name
                                         " must be a function, got: " (jolt-final-str ctor))))
        (register-class-ctor-user! name ctor))
      jolt-nil)))

(def-var! "jolt.host" "extend-class!" jolt-extend-class!)
