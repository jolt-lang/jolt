;; class-model.ss — java.lang.Class as a value, and the class model core reads
;; off it. Shared by every target: it reads only the class graph
;; (class-hierarchy.ss), the jhost record and the token lists (host-class.ss,
;; ns.ss), so the Chez and Gambit boots load this one file instead of each
;; keeping a copy.
;;
;; What lives here: the jclass value ((class x) returns one, a class token
;; evaluates to one — interned per name by jolt-class-for, so identity, = and
;; defmethod keys are stable), its three JVM spellings and the Class methods
;; over the graph, and the answers clojure.core's isa? / class? / supers / bases
;; / instance? read through jolt.host (class-isa?, class-value?, class-object?,
;; class-supers, class-ancestors, class-bases). Last, the class tokens
;; themselves as clojure.core vars — the short names, the FQN value classes and
;; the java.lang auto-imports.
;;
;; Ordering. After host-class.ss (class-token-alist, class-munge-name), ns.ss
;; (jolt-default-import-names), records-interop.ss (instance-check) and the
;; jhost record with its method registry (host-static.ss on Chez,
;; host-statics.ss on Gambit); after host-static-methods.ss on Chez, whose
;; Long/TYPE and siblings the promotion loop below reads. Before the prelude on
;; Gambit: clojure.core's own (import …) interns through jolt-class-for. The
;; Class methods that answer with an array (getConstructors, the annotations)
;; are host-static-classes.ss's, joined to the same table there.

;; hsc-mu covers the two global registries written after boot: jolt-class-for-tbl's
;; intern below, and the tagged-methods registry in host-static-classes.ss
;; (reachable from Clojure as clojure.core/__register-class-methods!, so from a
;; namespace load, which is now parallel). Single-key reads of both stay
;; unlocked — strong hashtables, per var-table.
(define hsc-mu (make-mutex))

;; (instance? clojure.lang.IFoo x) for the core clojure.lang interfaces libraries
;; branch on — jolt's value model satisfies them, so report it. Matched by the
;; interface's last dotted segment, so "clojure.lang.IObj" and "IObj" both hit.
(define (hsc-last-segment s)
  (let loop ((i (- (string-length s) 1)))
    (cond ((< i 0) s)
          ((char=? (string-ref s i) #\.) (substring s (+ i 1) (string-length s)))
          (else (loop (- i 1))))))
;; Where a class name's nesting boundary is, or #f if it names a top-level class.
;; The JVM reads nesting off the class file's enclosing-class attribute, never off
;; the name, so a `$` does not by itself mean nested: java.util.Map$Entry is Map's
;; member class, while clojure.core$inc is a TOP-LEVEL class whose name merely
;; contains one — which is why the JVM answers "Entry" for the first and
;; "core$inc", not "inc", for the second. Derive that from the class model: a `$`
;; is a nesting boundary only when the name before it is itself a class jolt
;; knows. Scanning stops at the last dot, since a `$` can only appear in the
;; final segment.
(define (hsc-nesting-dollar cn)
  (let loop ((i (- (string-length cn) 1)))
    (cond ((fx<? i 0) #f)
          ((char=? (string-ref cn i) #\.) #f)
          ((and (char=? (string-ref cn i) #\$) (jch-known? (substring cn 0 i))) i)
          (else (loop (fx- i 1))))))
;; Class.getSimpleName drops the package and, for a nested class, the enclosing
;; class too. Class.getCanonicalName spells nesting with a dot instead of a `$`.
(define (hsc-simple-name cn)
  (let ((i (hsc-nesting-dollar cn)))
    (if i (substring cn (fx+ i 1) (string-length cn)) (hsc-last-segment cn))))
(define (hsc-canonical-name cn)
  (let ((i (hsc-nesting-dollar cn)))
    (if i
        (string-append (hsc-canonical-name (substring cn 0 i))
                       "." (substring cn (fx+ i 1) (string-length cn)))
        cn)))

;; java.lang.Class value: (class x) / (.getClass x) return one. It renders like
;; the JVM — str/.toString -> "class <name>", pr -> "<name>", .getName -> "<name>".
;; A class token (java.util.Date) now evaluates to a Class object (not a name
;; string), so (= (class x) java.util.Date) works by jclass identity.
(define (make-class-obj name) (make-jhost "class" (vector name)))
(define (jclass? x) (and (jhost? x) (string=? (jhost-tag x) "class")))
(define (jclass-name x) (vector-ref (jhost-state x) 0))
;; The name a class token PRESENTS — .getName, toString, print: the JVM spelling.
;; A deftype's registered name is its namespace as written (rf.def-two.R3); the
;; JVM munges the dash (rf.def_two.R3), and that is the spelling a record literal
;; must print in for the JVM's reader to find the class. Every other name is
;; already its JVM spelling (jch-munge-segments is the identity on it). Lookups
;; keep asking jclass-name: the registered name is the identity.
(define (jclass-jvm-name x) (jch-munge-segments (jclass-name x)))

;; Global interner: class tokens resolve to the same eq? object per name, so
;; identity, =, and defmethod table keys are stable. Called by the analyzer for
;; every class-name symbol (java.util.Date, clojure.lang.Atom) at evaluation time.
(define jolt-class-for-tbl (make-hashtable string-hash string=?))
;; Double-checked, like every other interner in the runtime. The hit path — every
;; class name after the first — is the bare hashtable-ref it always was, which
;; matters because the analyzer calls this per class-name symbol, and only a
;; first-ever intern takes the lock. The blast radius of a lost intern is smaller
;; here than for keywords: the eq-arm below compares jclass by NAME and the hash
;; arm hashes the name, so a duplicate still answers = and still keys a defmethod
;; table (new-mm-table is keyed by jolt=). It is the eq?-identity this comment
;; promises that a race would take away.
(define (jolt-class-for name)
  (or (hashtable-ref jolt-class-for-tbl name #f)
      (jolt-with-mutex hsc-mu
        (or (hashtable-ref jolt-class-for-tbl name #f)
            ;; First sight of a name asks the class graph for its registered
            ;; spelling, so the JVM spelling of a deftype in a dashed namespace
            ;; (rf.def_two.R3 for rf.def-two.R3 — what resolve, :import, a class
            ;; symbol in code and Class/forName all arrive with) interns to the
            ;; ONE token the type's values report; = on tokens compares names.
            ;; A name the graph does not know is its own token: the syntactic
            ;; class model, unchanged.
            (let* ((reg (jch-registered-name name))
                   (canon (if (and reg (not (string=? reg name))) reg name))
                   (obj (or (hashtable-ref jolt-class-for-tbl canon #f)
                            (let ((o (make-class-obj canon)))
                              (hashtable-set! jolt-class-for-tbl canon o)
                              o))))
              (hashtable-set! jolt-class-for-tbl name obj)
              obj)))))
(def-var! "jolt.host" "jolt-class-for" jolt-class-for)

;; Long/TYPE and its eight siblings are the primitive CLASSES, not their names.
;; They are declared beside the rest of each wrapper's statics
;; (host-static-methods.ss), which loads before the interner exists, so they are
;; registered as the primitive's name and promoted here in one pass — one
;; declaration site, one place that knows the interner is now available.
;; Promoting them is what lets a Class method reach them at all: .isPrimitive on
;; a bare string is a field miss, and typedclojure's symbol->Class asserts its
;; result is a class?.
(for-each
  (lambda (cls)
    (let ((h (hashtable-ref class-statics-tbl cls #f)))
      (when h
        (let ((v (hashtable-ref h "TYPE" #f)))
          (when (string? v) (hashtable-set! h "TYPE" (jolt-class-for v)))))))
  '("Long" "Integer" "Short" "Byte" "Character" "Boolean" "Double" "Float" "Void"))
;; A deftype registered AFTER its JVM spelling was interned — an :import or a
;; class symbol compiled ahead of the defining namespace, which the JVM rejects
;; but jolt's syntactic model lets through — left a token under that spelling
;; that is not the type's. Drop it, so the next lookup re-interns through the
;; graph. Runs outside the graph's mutex (class-hierarchy.ss), so the
;; hsc-mu -> jch-cache-mutex order jolt-class-for takes is never inverted.
(set! jch-class-registered-hook
  (lambda (name)
    (let ((m (jch-munge-segments name)))
      (unless (string=? m name)
        (jolt-with-mutex hsc-mu
          (let ((tok (hashtable-ref jolt-class-for-tbl m #f)))
            (when (and tok (not (string=? (jclass-name tok) name)))
              (hashtable-delete! jolt-class-for-tbl m))))))))

(define (class-key x)
  (cond ((jclass? x) (jclass-name x))
        ((string? x) x)
        ;; a deftype/defrecord NAME var holds its ctor; treat it as the class
        ((procedure? x) (deftype-ctor-tag x))
        (else #f)))
;; = compares jclass values by name (stable interning makes this eq?-level);
;; strings are no longer = to a jclass — class-key survives for internal
;; dispatch boundaries only (multimethod tables, catch dispatch, isa?).
(register-eq-arm! (lambda (a b) (and (jclass? a) (jclass? b)))
                  (lambda (a b) (let ((ka (class-key a)) (kb (class-key b)))
                                  (and ka kb (string=? ka kb) #t))))
;; A deftype/defrecord TYPE TOKEN and (class inst) are the same Class object on
;; the JVM — identical?, not merely =. jolt spells them differently (the token is
;; the ctor procedure, so it stays callable) and they compared unequal, so
;; (= Rec (class (->Rec 1))) was false and the two spellings were distinct keys in
;; the same map. =/hash stay in step: the token's identity hash is seeded from its
;; class name at registration (records.ss deftype-ctor-tag-set!), which costs the
;; procedure-hash fast path nothing. Half of this pair alone would be worse than
;; neither — that is the shape that makes a hash container answer nil for a key it
;; contains.
(register-eq-arm! (lambda (a b)
                    (or (and (jclass? a) (procedure? b) (deftype-ctor-tag b) #t)
                        (and (jclass? b) (procedure? a) (deftype-ctor-tag a) #t)))
                  (lambda (a b) (let ((ka (class-key a)) (kb (class-key b)))
                                  (and ka kb (string=? ka kb) #t))))
(register-hash-arm! jclass? (lambda (x) (jolt-hash (jclass-name x))))
;; The nine primitive classes. jolt names them exactly as the JVM does — the
;; class `long` is spelled "long" — and they are in no class graph row, so this
;; literal set is what tells them from a reference class.
(define jclass-primitive-names
  '("boolean" "byte" "char" "short" "int" "long" "float" "double" "void"))
(define (jclass-primitive? x)
  (and (member (jclass-name x) jclass-primitive-names) #t))

;; Class.toString says which kind it is: "interface java.util.List",
;; "class java.lang.String" — and a primitive, alone, is just its own name
;; ("long"). ONE renderer, so (str c) and (.toString c) cannot drift: they used
;; to, with str reading the graph for the interface case and the method always
;; saying "class".
(define (jclass-tostring x)
  (cond ((jclass-primitive? x) (jclass-jvm-name x))
        ((jch-interface? (jclass-name x)) (string-append "interface " (jclass-jvm-name x)))
        (else (string-append "class " (jclass-jvm-name x)))))
(register-str-render! jclass? jclass-tostring)
(register-pr-arm! jclass? (lambda (x) (jclass-jvm-name x)))
;; print/println of a Class prints the bare name (getName), like pr — the JVM's
;; print-method for Class ignores *print-readably*. Only str is "class <name>".
(let ((prev (var-deref "clojure.core" "__print1")))
  (def-var! "clojure.core" "__print1"
    (lambda (x) (if (jclass? x) (jclass-jvm-name x) (jolt-invoke1 prev x)))))
(register-host-methods! "class"
  (list (cons "getName" (lambda (self) (jclass-jvm-name self)))
        (cons "getCanonicalName" (lambda (self) (hsc-canonical-name (jclass-jvm-name self))))
        (cons "getSimpleName" (lambda (self) (hsc-simple-name (jclass-jvm-name self))))
        (cons "toString" jclass-tostring)
        (cons "isArray" (lambda (self) (let ((n (jclass-name self)))
                                         (and (fx>? (string-length n) 0) (char=? (string-ref n 0) #\[)))))
        ;; Class.getComponentType: for an array class returns the element class;
        ;; for a non-array returns nil. JVM: [Ljava.lang.Long; → java.lang.Long.
        (cons "getComponentType" (lambda (self)
                                   (let ((n (jclass-name self)))
                                     (cond ((and (fx>? (string-length n) 2) (char=? (string-ref n 0) #\[)
                                                 (char=? (string-ref n 1) #\L) (char=? (string-ref n (- (string-length n) 1)) #\;))
                                            (jolt-class-for (substring n 2 (- (string-length n) 1))))
                                           ((and (fx>? (string-length n) 1) (char=? (string-ref n 0) #\[))
                                            (cond ((char=? (string-ref n 1) #\B) (jolt-class-for "byte"))
                                                  ((char=? (string-ref n 1) #\C) (jolt-class-for "char"))
                                                  ((char=? (string-ref n 1) #\D) (jolt-class-for "double"))
                                                  ((char=? (string-ref n 1) #\F) (jolt-class-for "float"))
                                                  ((char=? (string-ref n 1) #\I) (jolt-class-for "int"))
                                                  ((char=? (string-ref n 1) #\J) (jolt-class-for "long"))
                                                  ((char=? (string-ref n 1) #\S) (jolt-class-for "short"))
                                                  ((char=? (string-ref n 1) #\Z) (jolt-class-for "boolean"))
                                                  (else jolt-nil)))
                                           (else jolt-nil)))))
        ;; Class.isInstance(o) == (instance? class o); core.logic's deftype .equals
        ;; uses (.. this getClass (isInstance o)).
        (cons "isInstance" (lambda (self o) (if (instance-check self o) #t #f)))
        ;; --- reflection over the jch graph (epic jolt-of08.3) -----------------
        ;; getSuperclass: the graph's class edge — nil for Object, for an
        ;; interface (the JVM's null), and for a name the graph does not model
        ;; (statics-only shims like Math; recorded divergence).
        (cons "getSuperclass" (lambda (self)
                                (let ((s (jch-superclass (jclass-name self))))
                                  (if s (jolt-class-for s) jolt-nil))))
        ;; getInterfaces: the DIRECT super-interfaces, as a seqable of Class
        ;; values (the JVM's Class[] surfaces to Clojure as a seq anyway).
        (cons "getInterfaces" (lambda (self)
                                (list->cseq
                                 (map jolt-class-for
                                      (filter jch-interface?
                                              (jch-direct-supers (jclass-name self)))))))
        (cons "isInterface" (lambda (self) (if (jch-interface? (jclass-name self)) #t #f)))
        (cons "isPrimitive" (lambda (self) (jclass-primitive? self)))
        ;; isAssignableFrom: the graph's isa?, JVM argument order — self is the
        ;; wanted supertype. class-key so a deftype ctor or a name string on
        ;; either side answers too.
        (cons "isAssignableFrom" (lambda (self other)
                                   (let ((ka (class-key self)) (kb (class-key other)))
                                     (if (and ka kb (jch-isa? kb ka)) #t #f))))
        ;; Class.cast: the JVM's checked narrowing — the value back when it is
        ;; already an instance, a ClassCastException otherwise. A reflective
        ;; interpreter casts every argument to its parameter type before the
        ;; call (SCI's box-arg does), and jolt reports every parameter as
        ;; Object, where the cast IS the identity. Without an arm the lookup fell
        ;; through to resolving the class by name, which raised for a name no
        ;; provider supplies (java.lang.Object) — every interpreted call failed
        ;; there before reaching the method.
        ;; null casts to anything: Class.cast(null) is null for every target
        ;; class on the JVM, and instance-check answers false for nil, so the
        ;; nil arm comes first — the failure branch would otherwise ask
        ;; (jolt-class nil) for a jhost it is not and die on the accessor.
        (cons "cast" (lambda (self o)
                       (if (or (jolt-nil? o) (instance-check self o))
                           o
                           (throw-jvm
                            'ClassCastException
                            (string-append "class " (jclass-jvm-name (jolt-class o))
                                           " cannot be cast to class "
                                           (jclass-jvm-name self))))))
        ;; getModifiers: the JVM bitmask, derived from the class graph (jolt has
        ;; no bytecode to read one out of). Modifier's predicates read it.
        (cons "getModifiers" (lambda (self) (->num (jch-modifiers (jclass-name self)))))
        ;; ---- annotations ----------------------------------------------------
        ;; jolt models no annotations: the class graph records supertypes and
        ;; modifiers, and nothing anywhere carries an annotation to report. So the
        ;; whole surface answers "none" — which is a real answer, not a gap, and
        ;; the JVM's own answer for every class that carries no annotation.
        ;;
        ;; It has to be answered HERE rather than left to the miss path, because a
        ;; Class value that does not recognise a member falls through to the
        ;; STATICS of the class it names (host-static.ss, the imported-token arm):
        ;; (.isAnnotationPresent c FunctionalInterface) became a lookup for a
        ;; static named isAnnotationPresent on java.lang.StringBuilder, which
        ;; reported "No dependency provides java.lang.StringBuilder" for a class
        ;; jolt fully supplies. SCI asks exactly this of every ^Hint it resolves —
        ;; sci.impl.reflector/maybe-fi-method, on the way to deciding whether the
        ;; hinted target is a functional interface to adapt — so a hinted instance
        ;; call inside SCI could not analyze at all (jolt#983).
        (cons "isAnnotationPresent" (lambda (self ann) #f))
        (cons "getAnnotation" (lambda (self ann) jolt-nil))
        ;; interned like every other Class token, so (identical? (.getClass String) Class)
        (cons "getClass" (lambda (self) (jolt-class-for "java.lang.Class")))))


;; JVM class assignability for isa? (20-coll): true when child and parent are both
;; class values and parent is child, java.lang.Object (every class's root), or a
;; modeled ancestor of child (full name or last segment). nil for non-class args, so
;; isa? falls through to its hierarchy/vector logic.
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

;; is NAME a class the host models (registered in the class graph, or a fn class)?
;; Object itself is modeled.
(define (hsc-class-known? name)
  (or (string=? name "java.lang.Object")
      (jch-known? name)
      (str-has-dollar? name)))

;; (jolt.host/class-supers name) / (jolt.host/class-ancestors name) — a jolt seq of
;; super / ancestor class-name strings (transitive, Object-rooted), or nil when
;; jolt models no hierarchy for it. class-bases is the DIRECT supers (clojure.core
;; `bases` / the class arm of `parents`). Each result element is an interned jclass
;; so (= (first (parents Long)) Number) and contains? work against class tokens.
(def-var! "jolt.host" "class-supers"
  (lambda (x)
    (let ((name (class-key x)))
      (if name
          (let ((as (jch-ancestors-rooted name)))
            (if (null? as) jolt-nil (list->cseq (map jolt-class-for as))))
          jolt-nil))))
(def-var! "jolt.host" "class-ancestors"
  (lambda (x)
    (let ((name (class-key x)))
      (if name
          (let ((as (jch-ancestors-rooted name)))
            (if (null? as) jolt-nil (list->cseq (map jolt-class-for as))))
          jolt-nil))))
;; The direct bases of a class as Class objects, superclass first the way the
;; JVM's `bases` orders them: a concrete class whose row names no concrete super
;; extends Object, so Object leads its list (interfaces have no superclass and
;; Object itself none at all). This is clojure.core/bases too — it answered name
;; STRINGS where supers answered Class objects, so (.getName (first (bases c)))
;; failed on every class, and typed.clojure's RClass ancestry (Class->symbol
;; over (bases cls)) with it.
(define (jolt-class-bases x)
  (let ((name (class-key x)))
    (if name
        (let* ((ds (jch-direct-supers name))
               ;; jch-superclass is "java.lang.Object" exactly for a known
               ;; concrete class with no modeled concrete super; #f for Object,
               ;; an interface, or a name the graph does not model (a fn class
               ;; keeps its AFunction row and nothing else, as on the JVM).
               (ds (if (and (equal? (jch-superclass name) "java.lang.Object")
                            (not (member "java.lang.Object" ds)))
                       (cons "java.lang.Object" ds)
                       ds)))
          (if (null? ds) jolt-nil (list->cseq (map jolt-class-for ds))))
        jolt-nil)))
(def-var! "jolt.host" "class-bases" jolt-class-bases)
(def-var! "clojure.core" "bases" jolt-class-bases)
;; is X a class value — a jclass, a deftype ctor, or a name string the host
;; graph models?
(def-var! "jolt.host" "class-value?"
  (lambda (x)
    (if (jclass? x)
        #t
        (let ((n (class-key x)))
          (if (and n (hsc-class-known? n)) #t jolt-nil)))))

;; a Class OBJECT specifically ((class x) result) — narrower than class-value?,
;; which also admits deftype ctors and modeled name strings. The instance?
;; macro needs exactly this: evaluate a var-held Class, keep quoting record names.
;; class? is true for a modeled host Class value AND for a deftype/defrecord type
;; token — jolt represents a record type by its make-deftype-ctor closure (the
;; same value instance?/ancestors dispatch on), so (class? Bar) holds like the JVM.
;; (jolt unifies Bar with ->Bar, so (class? ->Bar) also holds — a record's name and
;; its positional ctor are one value here.)
(def-var! "jolt.host" "class-object?"
  (lambda (x) (if (or (jclass? x)
                      (and (procedure? x) (deftype-ctor-tag x) #t))
                  #t #f)))

;; --- class-token def-vars as Class objects -----------------------------------
;; Short names (String, Long, HashMap) and FQN value-class names (java.lang.Long,
;; clojure.lang.Atom) evaluate to interned Class objects via the global interner
;; (jolt-class-for), so (= (class x) String) and (instance? Long x) work without
;; a string=class bridge. class-token-alist and class-fqn-list come from
;; host-class.ss (loaded earlier).
(for-each
  (lambda (pair) (def-var! "clojure.core" (car pair) (jolt-class-for (cdr pair))))
  class-token-alist)
(for-each
  (lambda (nm) (def-var! "clojure.core" nm (jolt-class-for nm)))
  class-fqn-list)
;; ...and the 96 auto-imports get theirs pinned to the canonical name, last, so
;; the mapping every namespace has cannot be decided by which class the graph
;; happened to enumerate first: class-token-alist is first-simple-name-wins over
;; an unordered hashtable walk, so a same-named class anywhere else in the graph
;; could otherwise take `Process` or `Package` from java.lang.
(for-each
  (lambda (n)
    (let ((fqn (jolt-default-import-canonical n)))
      (when (jch-known? fqn) (def-var! "clojure.core" n (jolt-class-for fqn)))))
  jolt-default-import-names)
