;; enums.ss — the one model of a JVM enum class.
;;
;; Every enum the runtime or a library provides is declared here with
;; (register-enum-class! fqn constants), CONSTANTS the JDK-ordered list of
;; (name . value). The value is whatever object the declaring shim already uses —
;; a jhost with its own tag and state, a library's tagged table — so the shim's
;; own instance methods keep working; the model only adds what HotSpot
;; synthesizes for an enum and what java.lang.Enum provides:
;;
;;   - the statics: each constant, values() (a fresh [L<fqn>; array per call) and
;;     valueOf(String), registered under the FQN and the short name
;;   - the class facts: java.lang.Enum as a super (Comparable, Serializable and
;;     Constable through it), the ENUM and FINAL modifier bits, Class.isEnum and
;;     getEnumConstants, Enum/valueOf
;;   - Enum's instance methods — name, ordinal, toString, compareTo,
;;     getDeclaringClass, equals, hashCode — for any constant whose shim does not
;;     define its own: registered on a jhost constant's tag, and answered at the
;;     end of the dispatch chain (dispatch-miss) for any other representation
;;   - the JVM print form, #object[<fqn> 0xHASH "NAME"]
;;
;; A jhost constant's tag reports the enum's class: registration writes the
;; jhost-tag->fqn row, and refuses a tag already reporting another class.
;;
;; Loaded from rt.ss after host-static.ss (the statics tables, jhost) and before
;; the first shim that declares an enum.

;; The two tables readers consult with no lock. A Chez hashtable is not safe to
;; read while another thread resizes it, so neither published table is ever
;; written: a declaration copies both under enum-mu, fills the copies and
;; publishes each with one store (tools/enum-registry-model.pl checks the
;; protocol: no torn read, no lost declaration).
(define enum-mu (make-mutex))
;; class name (FQN and short) -> #(fqn names consts by-name)
(define enum-class-tbl (make-hashtable string-hash string=?))
;; constant -> (info . ordinal)
(define enum-const-tbl (make-eq-hashtable))

(define (enum-info-fqn i) (vector-ref i 0))
(define (enum-info-names i) (vector-ref i 1))
(define (enum-info-consts i) (vector-ref i 2))
(define (enum-info-by-name i) (vector-ref i 3))

(define (enum-of x) (hashtable-ref enum-const-tbl x #f))
(define (enum-constant? x) (and (enum-of x) #t))
(define (enum-entry-name e) (vector-ref (enum-info-names (car e)) (cdr e)))
(define (enum-name x) (enum-entry-name (enum-of x)))

;; The info for a class given as a token or a name, or #f for a non-enum. A
;; class a library declares (the java.time base) loads its provider on a miss,
;; as a static reference to it does.
(define (enum-class-info c)
  (let ((n (cond ((string? c) c)
                 ((jclass? c) (jclass-name c))
                 (else (class-key c)))))
    (and (string? n)
         (or (hashtable-ref enum-class-tbl n #f)
             (and (lib-try-autoload! n) (hashtable-ref enum-class-tbl n #f))))))

(define (enum-values-array info)
  (make-jolt-array (vector-copy (enum-info-consts info)) (na-name-kind (enum-info-fqn info))))

;; Enum.valueOf's checks, in its order: the name must be a String, then present.
(define (enum-value-of info nm)
  (cond ((jolt-nil? nm) (throw-jvm 'NullPointerException "Name is null"))
        ((not (string? nm))
         (throw-jvm 'ClassCastException
                    (string-append "class " (jolt-class-name nm)
                                   " cannot be cast to class java.lang.String")))
        ((hashtable-ref (enum-info-by-name info) nm #f))
        (else (throw-jvm 'IllegalArgumentException
                         (string-append "No enum constant "
                                        (hsc-canonical-name (enum-info-fqn info)) "." nm)))))

;; Enum.valueOf(Class, String): the class must be an enum before the name is
;; looked at.
(define (enum-valueof-class cls nm)
  (when (jolt-nil? cls) (throw-jvm 'NullPointerException jolt-nil))
  (let ((info (enum-class-info cls)))
    (unless info
      (throw-jvm 'IllegalArgumentException
                 (string-append (if (jclass? cls) (jclass-jvm-name cls) (jolt-final-str cls))
                                " is not an enum class")))
    (enum-value-of info nm)))

;; ---- Enum's instance methods ---------------------------------------------------
(define (enum-self-info self)
  (or (enum-of self)
      (throw-jvm 'IllegalArgumentException
                 (string-append "not an enum constant: " (jolt-class-name self)))))
(define enum-instance-methods
  (list (cons "name" (lambda (self) (enum-entry-name (enum-self-info self))))
        (cons "ordinal" (lambda (self) (cdr (enum-self-info self))))
        (cons "toString" (lambda (self) (enum-entry-name (enum-self-info self))))
        ;; Enum.compareTo: the ordinal difference, and a bare ClassCastException
        ;; for a constant of another enum
        (cons "compareTo"
              (lambda (self other)
                (let ((a (enum-self-info self)))
                  (when (jolt-nil? other)
                    (throw-jvm 'NullPointerException
                               "Cannot invoke \"Object.getClass()\" because \"other\" is null"))
                  (let ((b (enum-of other)))
                    (unless (and b (eq? (car a) (car b)))
                      (throw-jvm 'ClassCastException jolt-nil))
                    (fx- (cdr a) (cdr b))))))
        (cons "getDeclaringClass"
              (lambda (self) (jolt-class-for (enum-info-fqn (car (enum-self-info self))))))
        (cons "equals" (lambda (self o) (eq? self o)))
        (cons "hashCode" (lambda (self) (->num (jolt-identity-hasheq self))))))
(define enum-instance-method-tbl
  (let ((h (make-hashtable string-hash string=?)))
    (for-each (lambda (p) (hashtable-set! h (car p) (cdr p))) enum-instance-methods)
    h))

;; compare and sort reach a constant's compareTo through jolt-compare-miss
;; (host-class.ss), which asks it only of a pair of one class; a constant is
;; asked whatever it is compared with, so another enum's constant is refused by
;; compareTo's own bare ClassCastException.
(set! jolt-compare-miss
  (let ((prior jolt-compare-miss))
    (lambda (a b)
      (if (enum-of a)
          (jnum->exact (record-method-dispatch a "compareTo" (jolt-list b)))
          (prior a b)))))

;; The end of the dispatch chain for a constant that is not a jhost (a library's
;; value): the method, or #f.
(set-enum-method-hook!
  (lambda (obj method-name argc)
    (and (enum-of obj)
         (let ((f (hashtable-ref enum-instance-method-tbl method-name #f)))
           (and f (host-arity-ok? f argc #t) f)))))

;; str of a jhost constant is its toString: the shim's, else the name. Answered
;; ahead of the str registry (converters.ss) by the constant's tag OBJECT: each
;; tag an enum's constants carry is recorded as the enum is declared, with the
;; toString its table answered at the last host-methods-epoch it was read under.
;; #(tag (epoch . toString)), the pair replaced whole so a reader never sees one
;; epoch's procedure under another's number.
(define enum-jhost-tags '())
(define enum-generic-tostring (cdr (assoc "toString" enum-instance-methods)))
(define (enum-tag-tostring entry)
  (let ((c (vector-ref entry 1)) (epoch host-methods-epoch))
    (if (fx=? (car c) epoch)
        (cdr c)
        (let ((f (host-method-ref (vector-ref entry 0) "toString")))
          (vector-set! entry 1 (cons epoch f))
          f))))
;; A record constant (a library's deftype) renders its own toString, else its
;; name; records pay nothing for this until one is declared.
(define enum-jrec-declared? #f)
(define (enum-jrec-render x)
  (let ((e (enum-of x)))
    (and e
         (let ((f (find-protocol-method (jrec-rtag x) "Object" "toString")))
           (if f (jolt-invoke f x) (enum-entry-name e))))))
(set-str-render-by-identity!
  (lambda (x)
    (if (jhost? x)
         (let ((t (jhost-tag x)))
           (let find ((es enum-jhost-tags))
             (and (pair? es)
                  (if (eq? (vector-ref (car es) 0) t)
                      (let ((e (enum-of x)))
                        (and e
                             (let ((f (enum-tag-tostring (car es))))
                               (if (or (not f) (eq? f enum-generic-tostring))
                                   (enum-entry-name e)
                                   (f x)))))
                      (find (cdr es))))))
         (and enum-jrec-declared? (jrec? x) (enum-jrec-render x)))))
(set-jrec-enum-constant-test! (lambda (x) (and enum-jrec-declared? (enum-of x) #t)))

;; ---- registration ---------------------------------------------------------------
;; What a value shared by every equal value is, for the refusal below, or #f.
(define (enum-shared-value-kind c)
  (cond ((jolt-nil? c) "nil")
        ((boolean? c) "a boolean")
        ((number? c) "a number")
        ((char? c) "a character")
        ((string? c) "a string")
        ((keyword? c) "a keyword")
        ((jolt-symbol? c) "a symbol")
        (else #f)))
(define (enum-register! fqn constants statics! value-of-other)
  (let* ((names (list->vector (map car constants)))
         (consts (list->vector (map cdr constants)))
         (by-name (make-hashtable string-hash string=?))
         (info (vector fqn names consts by-name))
         (short (short-class-name fqn)))
    (vector-for-each
      (lambda (nm c)
        (unless (string? nm)
          (error 'register-enum-class! "a constant's name must be a string" fqn nm))
        (when (hashtable-ref by-name nm #f)
          (error 'register-enum-class! "duplicate constant name" fqn nm))
        ;; a constant is ONE object that answers for the enum: an immediate or
        ;; interned value (a keyword, a number) would make every equal value
        ;; everywhere one of its constants
        (let ((kind (enum-shared-value-kind c)))
          (when kind
            (throw-jvm 'IllegalArgumentException
                       (string-append fqn "." nm " is " kind
                                      ": an enum constant must be an object of its own"))))
        (hashtable-set! by-name nm c)
        (let ((prior (enum-of c)))
          (when (and prior (not (string=? (enum-info-fqn (car prior)) fqn)))
            (error 'register-enum-class! "value is already a constant of another enum"
                   fqn nm (enum-info-fqn (car prior)))))
        (when (jhost? c)
          (let ((reported (hashtable-ref jhost-tag->fqn (jhost-tag c) #f)))
            (when (and reported (not (string=? reported fqn)))
              (error 'register-enum-class! "the constant's tag reports another class"
                     fqn (jhost-tag c) reported)))))
      names consts)
    (jolt-with-mutex enum-mu
      (let ((classes (hashtable-copy enum-class-tbl #t))
            (consts-tbl (hashtable-copy enum-const-tbl #t)))
        (hashtable-set! classes fqn info)
        (unless (and (hashtable-ref classes short #f)
                     (not (string=? (enum-info-fqn (hashtable-ref classes short #f)) fqn)))
          (hashtable-set! classes short info))
        (let loop ((i 0))
          (when (fx<? i (vector-length consts))
            (hashtable-set! consts-tbl (vector-ref consts i) (cons info i))
            (when (jrec? (vector-ref consts i)) (set! enum-jrec-declared? #t))
            (loop (fx+ i 1))))
        ;; the copies are complete before either becomes visible
        (memory-order-release)
        (set! enum-class-tbl (hashtable-copy classes #f))
        (set! enum-const-tbl (hashtable-copy consts-tbl #f))))
    ;; valueOf(String) is the class's own; valueOf(Class, String) is Enum's,
    ;; which every enum class inherits. A class with a further overload of its
    ;; own (RoundingMode.valueOf(int)) hands it in as VALUE-OF-OTHER, called for
    ;; a one-argument call that is no String.
    (statics! fqn (append constants
                          (list (cons "values" (lambda () (enum-values-array info)))
                                (cons "valueOf"
                                      (case-lambda
                                        ((nm) (if (and value-of-other (not (string? nm)) (not (jolt-nil? nm)))
                                                  (value-of-other nm)
                                                  (enum-value-of info nm)))
                                        ((cls nm) (enum-valueof-class cls nm)))))))
    (jch-register-supers! fqn '("java.lang.Enum"))
    (jch-mark-enum! fqn)
    (jch-mark-final! fqn)
    ;; a jhost constant's tag reports the class and answers Enum's methods it
    ;; does not define itself
    (let ((tags '()))
      (vector-for-each
        (lambda (c)
          (when (and (jhost? c) (not (memq (jhost-tag c) tags)))
            (set! tags (cons (jhost-tag c) tags))))
        consts)
      (jolt-with-mutex enum-mu
        (for-each (lambda (tag)
                    (unless (exists (lambda (e) (eq? (vector-ref e 0) tag)) enum-jhost-tags)
                      (set! enum-jhost-tags (cons (vector tag (cons -1 #f)) enum-jhost-tags))))
                  tags))
      (for-each
        (lambda (tag)
          (host-registry-write! (lambda () (hashtable-set! jhost-tag->fqn tag fqn)))
          (register-host-methods! tag
            (filter (lambda (p) (not (host-method-ref tag (car p)))) enum-instance-methods)))
        tags))
    info))

;; The host's own enums: their statics are the runtime's (register-class-statics!).
(define register-enum-class!
  (case-lambda
    ((fqn constants) (enum-register! fqn constants register-class-statics! #f))
    ((fqn constants value-of-other) (enum-register! fqn constants register-class-statics! value-of-other))))

;; A library's enum: its statics go through the provider guard like any other
;; library registration (register-class-statics-user!).
(def-var! "jolt.host" "register-enum!"
  (lambda (fqn constants)
    (enum-register! fqn
                    (map (lambda (p) (cons (jolt-final-str (jolt-nth p 0)) (jolt-nth p 1)))
                         (seq->list (jolt-seq constants)))
                    register-class-statics-user! #f)
    jolt-nil))

;; ---- Class and java.lang.Enum --------------------------------------------------
(register-host-methods! "class"
  (list (cons "isEnum" (lambda (self) (and (enum-class-info self) #t)))
        (cons "getEnumConstants"
              (lambda (self)
                (let ((info (enum-class-info self)))
                  (if info (enum-values-array info) jolt-nil))))))

;; java.lang.Enum's one static
(register-class-statics! "java.lang.Enum" (list (cons "valueOf" enum-valueof-class)))
