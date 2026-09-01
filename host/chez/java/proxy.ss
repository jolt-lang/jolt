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
;; JVM's virtual dispatch would re-enter the override. Overriding a method the
;; base calls internally is the case this does not model.

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
  (let* (;; a lone argument may be a COLLECTION of names rather than one name;
         ;; a string is seqable, so exclude it or a single class name unravels
         ;; into its characters
         (names (map proxy-name->string
                     (if (and (pair? proto-names) (null? (cdr proto-names))
                              (not (string? (car proto-names)))
                              (not (symbol-t? (car proto-names)))
                              (jolt-coll-pred? (car proto-names)))
                         (seq->list (car proto-names))
                         proto-names)))
         (args (if (or (jolt-nil? ctor-args) (jolt-nil? (jolt-seq ctor-args)))
                   '()
                   (seq->list (jolt-seq ctor-args))))
         (base (and (pair? names)
                    (proxy-class-constructible? (car names))
                    (apply host-new (car names) args))))
    (make-reified-delegating methods-map base names)))
(def-var! "clojure.core" "make-proxy" jolt-make-proxy)

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
          (if (and (string? tname)
                   (or (member (last-dot tname) '("IObj" "IMeta"))
                       (memp (lambda (p) (proto-class-match? p tname)) (jreify-protos val))))
              #t
              (if (jolt-truthy? (instance-check type-sym (reify-delegate val))) #t #f)))
        (quote pass))))
(register-class-arm! proxy-value? (lambda (v) (jolt-class-name (reify-delegate v))))
(let ((prev value-host-tags))
  (set! value-host-tags
    (lambda (obj)
      (if (proxy-value? obj) (value-host-tags (reify-delegate obj)) (prev obj)))))
