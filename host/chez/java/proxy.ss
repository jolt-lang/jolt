;; clojure.core/proxy over a concrete host class.
;;
;; The JVM generates a class extending the base and implementing the interfaces.
;; jolt generates no classes, so a proxy EXTENDS BY DELEGATION: it constructs a
;; real instance of the base, answers the methods the proxy body declares, and
;; forwards everything else to that instance (the delegate field on jreify,
;; records.ss). proxy-super calls the base's own implementation.
;;
;; Where the first super names an interface — anything with no registered
;; constructor — there is nothing to construct and a proxy is exactly a reify of
;; that interface's methods, which is what it has always been.
;;
;; Delegation is not inheritance in one respect, and it is the respect worth
;; stating: the base instance holds no reference back to the proxy, so a base
;; method that calls an overridden method reaches the BASE's version, where the
;; JVM's virtual dispatch would re-enter the override — unless the base's shim
;; asks to be told its proxy (proxy-attach-hook!, below), as LinkedHashMap does
;; for removeEldestEntry.

;; A class is constructible when something registered a constructor for it. Probe
;; rather than calling host-new, so a genuine error raised INSIDE a real
;; constructor is not mistaken for "this name is an interface".
(define (proxy-class-constructible? class)
  (and (string? class)
       (or (and (lookup-class class-ctors-tbl class) #t)
           ;; the constructor may live in a provider that has not loaded yet
           (and (lib-try-autoload! class)
                (and (lookup-class class-ctors-tbl class) #t)))))

(define (proxy-name->string p)
  (cond ((symbol-t? p) (symbol-t-name p))
        ((keyword? p) (keyword-t-name p))
        ((string? p) p)
        (else (jolt-str-render-one p))))

;; (make-proxy {method fn} [ctor-args] Super Iface...) — the shape clojure.core's
;; proxy macro expands to. The base is built from the FIRST super when that names
;; a constructible class, matching the JVM, where only the first may be a class.
(define (jolt-make-proxy methods-map ctor-args . proto-names)
  (jolt-make-proxy* methods-map ctor-args proto-names))
;; The proxy macro hands its supers as ONE vector of names, a constant of the
;; site, so they are turned into the interned list a reify carries once per site
;; rather than once per instance (memoized by eq on the vector; weak, so a site
;; that is gone takes its entry with it).
(define proxy-site-protos-memo (make-weak-eq-hashtable))
(define proxy-site-protos-mu (make-mutex))
(define (proxy-site-protos v)
  (or (hashtable-ref proxy-site-protos-memo v #f)
      (let ((ps (reify-proto-names (map proxy-name->string (seq->list v)))))
        (jolt-with-mutex proxy-site-protos-mu (hashtable-set! proxy-site-protos-memo v ps))
        ps)))
(define (jolt-make-proxy* methods-map ctor-args proto-names)
  (let* (;; a lone argument may be a COLLECTION of names rather than one name;
         ;; a string is seqable, so exclude it or a single class name unravels
         ;; into its characters
         (names (cond ((and (pair? proto-names) (null? (cdr proto-names)) (pvec? (car proto-names)))
                       (proxy-site-protos (car proto-names)))
                      ((and (pair? proto-names) (null? (cdr proto-names))
                            (not (string? (car proto-names)))
                            (not (symbol-t? (car proto-names)))
                            (jolt-coll-pred? (car proto-names)))
                       (reify-proto-names (map proxy-name->string (seq->list (car proto-names)))))
                      (else (reify-proto-names (map proxy-name->string proto-names)))))
         (args (if (or (jolt-nil? ctor-args) (jolt-nil? (jolt-seq ctor-args)))
                   '()
                   (seq->list (jolt-seq ctor-args))))
         (base (and (pair? names)
                    (proxy-class-constructible? (car names))
                    (apply host-new (car names) args)))
         (p (make-reified-delegating/protos methods-map base names)))
    (proxy-attach! base p)
    p))


(def-var! "clojure.core" "make-proxy" jolt-make-proxy)

;; a reify form's class in the class graph: the interfaces it implements by their
;; full names (a protocol's key is ns/Name, not a class) and IObj
(set! reify-class-graph-hook
  (lambda (name ifaces)
    (jch-register-leaf! name (append (map (lambda (i) (if (memv #\. (string->list i)) i (jch-fqn-of-simple i)))
                                            (filter (lambda (i) (not (memv #\/ (string->list i)))) ifaces))
                                       '("clojure.lang.IObj")))))

;; The class the JVM generates for a proxy, named as clojure.core/proxy-name
;; names it: the defining ns ('-' as '_'), "proxy", the superclass, each
;; interface's simple name in sorted order, and the hex of the hash of the sorted
;; interface names — so (proxy [Object] []) in user is
;; user.proxy$java.lang.Object$ff19274a. The super is the first name when it is a
;; class, else Object with every name an interface. One name per (ns, supers),
;; memoized; its place in the class graph is the super, the interfaces and IProxy.
(define proxy-class-names (make-hashtable equal-hash equal?))
(define proxy-class-names-mu (make-mutex))
(define (proxy-fqn n) (if (memv #\. (string->list n)) n (jch-fqn-of-simple n)))
;; CLASS? is whether the first name is the superclass: one jolt could construct
;; (the proxy got a base), or one the class graph knows that is no interface (an
;; abstract class such as java.util.AbstractList). Decided when the proxy is
;; built, so naming it never loads anything.
(define (proxy-first-is-class? p names)
  (and (pair? names)
       (or (and (reify-delegate p) #t)
           (let ((f (proxy-fqn (car names))))
             (and (not (jch-interface? f)) (or (pair? (jch-direct-supers f)) (jch-jdk-class? f)))))))
(define (proxy-class-name ns names class?)
  (let ((key (cons ns names)))
    (or (hashtable-ref proxy-class-names key #f)
        (let* (
               (super (if class? (proxy-fqn (car names)) "java.lang.Object"))
               (ifaces (map proxy-fqn (if class? (cdr names) names)))
               ;; Clojure hashes the sorted set of the names; a set's hash is
               ;; the same whatever its order, so a hash-set's is that hash
               (sorted (list-sort string<? (fold-left (lambda (acc n) (if (member n acc) acc (cons n acc))) '() ifaces)))
               (inames (apply jolt-hash-set sorted))
               (simple (lambda (n) (let loop ((i (fx- (string-length n) 1)))
                                     (cond ((fx<? i 0) n)
                                           ((char=? (string-ref n i) #\.) (substring n (fx+ i 1) (string-length n)))
                                           (else (loop (fx- i 1)))))))
               (name (apply string-append
                            (append (list (list->string (map (lambda (c) (if (char=? c #\-) #\_ c)) (string->list ns)))
                                          ".proxy$" super)
                                    (map (lambda (n) (string-append "$" (simple n))) sorted)
                                    (list "$" (jolt-hex32 (jolt-hash inames)))))))
          ;; the JDK lists IProxy first among the interfaces
          (jch-register-leaf! name (cons super (cons "clojure.lang.IProxy" ifaces)))
          (jolt-with-mutex proxy-class-names-mu (hashtable-set! proxy-class-names key name))
          name))))
;; A proxy records the ns its form is in (as #(ns) in its cname field); the class
;; name is worked out the first time it is asked for, per (ns, supers), and kept
;; in the field, so building one pays a single field store.
(define proxy-lazy-names (make-weak-eq-hashtable))   ; interned supers list -> ((ns . name) ...)
(define (jolt-make-proxy-in ns methods-map ctor-args . proto-names)
  (let ((p (jolt-make-proxy* methods-map ctor-args proto-names)))
    (jreify-cname-set! p (vector ns #f))
    p))
(define (proxy-class-name-of p)
  (let ((c (jreify-cname p)))
    (cond ((string? c) c)
          ((vector? c)
           (let* ((ns (vector-ref c 0)) (ps (jreify-protos p))
                  (hit (assoc ns (hashtable-ref proxy-lazy-names ps '())))
                  (name (if hit (cdr hit)
                            (let ((name (proxy-class-name ns ps (proxy-first-is-class? p ps))))
                              (jolt-with-mutex proxy-class-names-mu
                                (hashtable-set! proxy-lazy-names ps (cons (cons ns name) (hashtable-ref proxy-lazy-names ps '()))))
                              name))))
             (jreify-cname-set! p name)
             name))
          (else #f))))
(set! proxy-class-name-hook proxy-class-name-of)
(def-var! "jolt.host" "make-proxy-in" jolt-make-proxy-in)

;; (proxy-super meth args...) inside a proxy body: the base's own implementation,
;; bypassing the override that is currently running.
(def-var! "jolt.host" "proxy-super-call"
  (lambda (self method . args)
    (let ((d (reify-delegate self))
          (m (proxy-name->string method)))
      (if d
          (record-method-dispatch d m (if (null? args) jolt-nil (list->cseq args)))
          (throw-jvm (quote IllegalArgumentException)
            (string-append "proxy-super: " m
                           " has no superclass to call - this proxy's first super"
                           " names an interface, not a constructible class"))))))

;; A proxy IS an instance of its base and reports the base's class and host tags,
;; so instance?, class and extend-protocol all see through the delegation. The JVM
;; names the generated subclass here; jolt has none, so the base's own name is the
;; closest honest answer.
(define (proxy-value? v) (and (jreify? v) (reify-delegate v) #t))
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (proxy-value? val)
        (let ((tname (if (symbol-t? type-sym) (symbol-t-name type-sym) type-sym)))
          (if (and (string? tname) (or (reify-declares-class? (jreify-protos val) tname)
                                       (reify-class-isa? val tname)))
              #t
              (if (jolt-truthy? (instance-check type-sym (reify-delegate val))) #t #f)))
        (quote pass))))
(register-class-arm! proxy-value? (lambda (v) (or (reify-class-name v) (jolt-class-name (reify-delegate v)))))
(let ((prev value-host-tags))
  (set! value-host-tags
    (lambda (obj)
      (if (proxy-value? obj) (value-host-tags (reify-delegate obj)) (prev obj)))))
