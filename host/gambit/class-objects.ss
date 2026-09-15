;; class-objects.ss — the Gambit target's java.lang.Class values and the class
;; model core reads off them (jolt-mj95; jolt-cw2p).
;;
;; On Chez these live in java/host-static-classes.ss, which this target does
;; not boot: the jhost record a host value rides in, the interned Class OBJECT
;; a class token evaluates to ((class x) returns one; (= (class x) String) is
;; jclass identity), the class-token vars, and the jolt.host/class-* answers
;; isa? / class? / supers / bases / instance? read. Same record shape and the
;; same graph (class-hierarchy.ss, booted) as Chez, so the shared class and
;; printing code reads them unchanged.
;;
;; Loaded BEFORE the prelude, unlike host-vars.ss: clojure.core's own source
;; runs (import '(clojure.lang Sequential …)) at load, and an import interns
;; through jolt-class-for — with this after the prelude that form failed under
;; the seed's guard and clojure.core/Sequential stayed unbound.

;; jhost is the generic carrier for a host value (java/host-static.ss on Chez);
;; a Class object is a jhost tagged "class". Without it (class e) died on an
;; unbound make-class-obj.
(define-record-type jhost
  (fields tag (mutable state))
  (nongenerative gambit-jhost-v1))

(define (make-class-obj name) (make-jhost "class" (vector name)))
(define (jclass? x) (and (jhost? x) (string=? (jhost-tag x) "class")))
(define (jclass-name x) (vector-ref (jhost-state x) 0))

;; Class tokens intern per name so identity, = and defmethod keys are stable.
(define jolt-class-for-tbl (make-hashtable string-hash string=?))
(define (jolt-class-for name)
  (or (hashtable-ref jolt-class-for-tbl name #f)
      (let ((obj (make-class-obj name)))
        (hashtable-set! jolt-class-for-tbl name obj)
        obj)))
(register-str-render! jclass? (lambda (x) (string-append "class " (jclass-name x))))
(register-pr-arm! jclass? jclass-name)
(register-eq-arm! (lambda (a b) (and (jclass? a) (jclass? b)))
                  (lambda (a b) (string=? (jclass-name a) (jclass-name b))))

(def-var! "jolt.host" "jolt-class-for" jolt-class-for)

;; The class tokens as clojure.core vars, the three lists Chez's
;; host-static-classes.ss binds in this order: compiled code reads `Throwable`
;; as (var-deref "clojure.core" "Throwable") and clojure.lang.Symbol as its FQN
;; var, and the compiler's own diagnostic wrapper does (instance? Throwable e)
;; — so with these unbound every analysis error became "symbol-t-name: not an
;; instance of this record type" on the Unbound sentinel, and the eval gate
;; could not report what actually failed. The lists live in host-class.ss and
;; ns.ss, both booted here.
(for-each
  (lambda (pair) (def-var! "clojure.core" (car pair) (jolt-class-for (cdr pair))))
  class-token-alist)
(for-each
  (lambda (nm) (def-var! "clojure.core" nm (jolt-class-for nm)))
  class-fqn-list)
(for-each
  (lambda (n)
    (let ((fqn (jolt-default-import-canonical n)))
      (when (jch-known? fqn) (def-var! "clojure.core" n (jolt-class-for fqn)))))
  jolt-default-import-names)

;; ---- the class model core reads (isa?, class?, supers, bases, instance?) -----
;; Mirrors of host-static-classes.ss over the class graph (class-hierarchy.ss,
;; booted here). isa? asks class-isa? for every non-equal pair and class-value?
;; for its hierarchy tags, so with these unbound every multimethod dispatch
;; through a hierarchy died, not just the class-keyed ones.
(define (hsc-last-segment s)
  (let loop ((i (- (string-length s) 1)))
    (cond ((< i 0) s)
          ((char=? (string-ref s i) #\.) (substring s (+ i 1) (string-length s)))
          (else (loop (- i 1))))))
(define (class-key x)
  (cond ((jclass? x) (jclass-name x))
        ((string? x) x)
        ;; a deftype/defrecord NAME var holds its ctor; treat it as the class
        ((procedure? x) (deftype-ctor-tag x))
        (else #f)))
(define (hsc-class-known? name)
  (or (string=? name "java.lang.Object")
      (jch-known? name)
      (str-has-dollar? name)))
;; a Class OBJECT specifically ((class x) result) or a deftype/defrecord type
;; token — what clojure.core/class? and the instance? macro ask.
(def-var! "jolt.host" "class-object?"
  (lambda (x) (if (or (jclass? x)
                      (and (procedure? x) (deftype-ctor-tag x) #t))
                  #t #f)))
(def-var! "jolt.host" "class-value?"
  (lambda (x)
    (if (jclass? x)
        #t
        (let ((n (class-key x)))
          (if (and n (hsc-class-known? n)) #t jolt-nil)))))
(def-var! "jolt.host" "class-isa?"
  (lambda (child parent)
    (let ((cc (class-key child)) (pp (class-key parent)))
      (if (and cc pp)
          (let ((pseg (hsc-last-segment pp)))
            (if (let loop ((names (cons cc (jch-closure cc))))
                  (cond ((string=? pp "java.lang.Object") #t)
                        ((null? names) #f)
                        ((or (string=? pp (car names))
                             (string=? pseg (hsc-last-segment (car names)))) #t)
                        (else (loop (cdr names)))))
                #t jolt-nil))
          jolt-nil))))
(define (gambit-class-supers x)
  (let ((name (class-key x)))
    (if name
        (let ((as (jch-ancestors-rooted name)))
          (if (null? as) jolt-nil (list->cseq (map jolt-class-for as))))
        jolt-nil)))
(def-var! "jolt.host" "class-supers" gambit-class-supers)
(def-var! "jolt.host" "class-ancestors" gambit-class-supers)
(define (jolt-class-bases x)
  (let ((name (class-key x)))
    (if name
        (let* ((ds (jch-direct-supers name))
               (ds (if (and (equal? (jch-superclass name) "java.lang.Object")
                            (not (member "java.lang.Object" ds)))
                       (cons "java.lang.Object" ds)
                       ds)))
          (if (null? ds) jolt-nil (list->cseq (map jolt-class-for ds))))
        jolt-nil)))
(def-var! "jolt.host" "class-bases" jolt-class-bases)
(def-var! "clojure.core" "bases" jolt-class-bases)

;; ---- answers the core predicates read ---------------------------------------
;; No array can be built here (every constructor is degraded below), so nothing
;; is one; the host callables are multimethods, since no promise exists either.
(def-var! "jolt.host" "array-value?" (lambda (x) jolt-nil))
(def-var! "jolt.host" "callable-host?"
  (lambda (x) (if (jolt-multifn? x) #t jolt-nil)))
;; Per-object identity for the back end's constant pool (rt.ss on Chez).
(def-var! "jolt.host" "identity-hash" (lambda (x) (jolt-identity-hasheq x)))
