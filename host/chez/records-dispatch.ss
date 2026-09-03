;; records-dispatch.ss — the .method interop dispatcher: record-method-dispatch
;; with its priority arm registry, the base surface (deftype/record methods,
;; MultiFn / Keyword / Symbol / Namespace / Var / Throwable / Character interop,
;; the universal Object methods), jreify (reify/proxy instances) + the iterator
;; seq arms, satisfies? / extenders, and the def-var! surface for the records
;; subsystem.
;;
;; Loaded after protocols.ss, last of the four records files.

;; dot-dispatch fallback used by emit for (.method record args): find the method
;; in ANY protocol the record's type implements.
;; java.util.Iterator over a jolt seqable: (.iterator coll) returns a jiterator
;; holding a mutable cursor over (seq coll); (.hasNext it)/(.next it) walk it.
;; hiccup/compiler's run! loop iterates collections this way.
(define-record-type jiterator (fields (mutable cur)) (nongenerative jolt-iterator-v1))
;; (seq an-iterator) / (iterator-seq it): a jiterator wraps the remaining seq in
;; cur, so seq just yields it — clojure.test's (iterator-seq (.iterator coll)).
(register-seq-arm! jiterator? jiterator-cur)

;; A Chez condition's message: "who: text", where text is the &message template
;; with each ~s / ~a directive filled by the matching irritant printed as a jolt
;; value — "string-append: nil is not a string", not "#[jolt-nil-v1] is not a
;; string". This is Throwable .getMessage on a raw condition, jolt.host/
;; condition-message, and the message of the throwable a fault becomes at the
;; catch boundary (java/host-faults.ss). A template with any other directive
;; (open-input-file's "failed for ~a: ~(~a~)") is left to Chez's format, its
;; irritants appended when format rejects it; a message with no directive gets
;; its irritants appended. A condition with no message at all renders through
;; display-condition.
(define (condition->message-string c)
  (if (message-condition? c)
      (let* ((m (condition-message c))
             (irr (if (irritants-condition? c) (condition-irritants c) '()))
             (irr (if (list? irr) irr '()))
             (who (and (who-condition? c) (condition-who c)))
             (text (if (string? m)
                       (condition-template-fill m irr)
                       (with-output-to-string (lambda () (display m))))))
        (cond ((symbol? who) (string-append (symbol->string who) ": " text))
              ((string? who) (string-append who ": " text))
              (else text)))
      (with-output-to-string (lambda () (display-condition c)))))
;; The directive at position i of template m: (next-index . kind) — kind is
;; #\s / #\a for a plain (or ~:s) print directive, #\~ for a literal tilde, #f
;; for anything else. i indexes the ~ itself.
(define (condition-directive m i)
  (let* ((n (string-length m))
         (j (if (and (fx<? (fx+ i 1) n) (char=? (string-ref m (fx+ i 1)) #\:)) (fx+ i 2) (fx+ i 1)))
         (d (and (fx<? j n) (char-downcase (string-ref m j)))))
    (cond ((memv d '(#\s #\a)) (cons (fx+ j 1) d))
          ((eqv? d #\~) (cons (fx+ j 1) #\~))
          (else (cons (fx+ j 1) #f)))))
(define (condition-append-irritants s irr)
  (let loop ((xs irr) (acc s))
    (if (null? xs) acc
        (loop (cdr xs) (string-append acc " " (condition-irritant-string (car xs) #t))))))
(define (condition-template-fill m irr)
  (let ((n (string-length m)))
    (let scan ((i 0) (simple? #t))
      (cond
        ((fx>=? i n)
         (if simple?
             (condition-fill-simple m irr)
             (guard (e (#t (condition-append-irritants m irr)))
               (apply format m irr))))
        ((char=? (string-ref m i) #\~)
         (let ((d (condition-directive m i)))
           (scan (car d) (and simple? (cdr d) #t))))
        (else (scan (fx+ i 1) simple?))))))
;; An irritant as text: ~s prints it readably (a string keeps its quotes), ~a
;; displays it. A host value jolt's printer has no class for (a Chez flvector
;; behind a ^doubles array) would print as #object[:object]; Chez's own writer
;; names it instead.
(define (condition-irritant-string x readable?)
  (let ((s (if readable? (jolt-pr-readable x) (jolt-str-render-one x))))
    (if (string=? s "#object[:object]")
        (with-output-to-string (lambda () (write x)))
        s)))
;; Every directive is ~s / ~a: substitute in order and append any left over.
(define (condition-fill-simple m irr)
  (let ((n (string-length m)))
    (let loop ((i 0) (start 0) (irr irr) (acc '()))
      (cond
        ((fx>=? i n)
         (condition-append-irritants
           (apply string-append (reverse (cons (substring m start n) acc)))
           irr))
        ((char=? (string-ref m i) #\~)
         (let* ((d (condition-directive m i))
                (next (car d)) (kind (cdr d))
                (acc (cons (substring m start i) acc)))
           (cond ((eqv? kind #\~) (loop next next irr (cons "~" acc)))
                 ((null? irr) (loop next next irr acc))
                 (else (loop next next (cdr irr)
                             (cons (condition-irritant-string (car irr) (not (eqv? kind #\a)))
                                   acc))))))
        (else (loop (fx+ i 1) start irr acc))))))
;; expose a Chez condition's message to Clojure (ex-message returns nil for raw
;; host conditions): the nREPL eval handler surfaces it instead of an opaque
;; "#<compound condition>".
(def-var! "jolt.host" "condition-message"
  (lambda (c) (if (condition? c) (condition->message-string c) jolt-nil)))
;; Set by the java layer once java.lang.Class has a method table (this file loads
;; first). Takes (tag method-name args) and returns a wrapped result, or #f when
;; the table has no such method so the caller can fall through.
(define rd-class-method-hook #f)
(define (set-rd-class-method-hook! f) (set! rd-class-method-hook f))

;; jolt's own immutable collections that are a java.util.List / Set on the JVM:
;; the receivers of the SequencedCollection accessors and the mutator refusal in
;; the base below. A map is not here — its `.name` reads stay the documented
;; map-as-object superset — and neither is a deftype.
(define (rd-persistent-coll? obj)
  (or (pvec? obj) (pset? obj) (cseq? obj) (empty-list-t? obj) (jolt-lazyseq? obj)))
(define (rd-coll-last obj)
  (if (pvec? obj)
      (jolt-nth obj (fx- (jolt-count obj) 1))
      (let loop ((s (jolt-seq obj)))
        (let ((n (jolt-seq (seq-more s))))
          (if (jolt-nil? n) (seq-first s) (loop n))))))
(define rd-java-util-mutator-names
  '("add" "addAll" "addFirst" "addLast" "clear" "remove" "removeAll" "removeFirst"
    "removeLast" "removeIf" "replaceAll" "retainAll" "set" "sort"))
(define (rd-java-util-mutator? m) (and (member m rd-java-util-mutator-names) #t))

(define (record-method-dispatch-base obj method-name rest-args)
  (let ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args))))
    (cond
      ;; a deftype/defrecord TYPE token (its make-deftype-ctor closure) answers the
      ;; java.lang.Class reflection methods off the "ns.Name" tag it carries, so
      ;; (.getName Bar)/(.getSimpleName Bar) work when the type is held by value —
      ;; schema resolves class schemas by calling these on the record class.
      ((and (procedure? obj) (deftype-ctor-tag obj))
       => (lambda (tag)
            ;; Delegate to the java.lang.Class method table for the tag rather
            ;; than re-listing a subset here: the two spellings of "the class" —
            ;; the type token and (class inst) — must answer the same questions,
            ;; and a hand-kept list here silently answered fewer. Every Class
            ;; method the table grows is reachable through the token from the
            ;; moment it is added.
            (let ((hit (and rd-class-method-hook (rd-class-method-hook tag method-name rest))))
              (if (pair? hit)
                  (car hit)                       ; the hook wraps, so a nil/#f answer is still a hit
                  (cond ((or (string=? method-name "getName") (string=? method-name "getCanonicalName")
                             (string=? method-name "getTypeName")) tag)
                        ((string=? method-name "getSimpleName") (last-dot tag))
                        ((string=? method-name "toString") (string-append "class " tag))
                        (else (dispatch-miss obj method-name rest)))))))
      ;; clojure.lang.MultiFn interop on a defmulti value: addMethod/removeMethod/
      ;; getMethod/getMethodTable, the same table (defmethod) fills — schema's
      ;; abstract-map registers dispatch methods by calling .addMethod directly.
      ((jolt-multifn? obj)
       (cond
         ;; these mutate the same table defmethod does, so they take the same
         ;; mutex AND bump the same epoch. The bump was missing outright: a
         ;; .addMethod left every multifn's dispatch cache stamped current, so a
         ;; value already resolved through isa? kept its old method for good.
         ((string=? method-name "addMethod")
          (jolt-with-mutex mm-tbl-mu
            (hashtable-set! (jolt-multifn-methods obj) (car rest) (cadr rest))
            (set! jolt-mm-epoch (fx+ jolt-mm-epoch 1)))
          obj)
         ((string=? method-name "removeMethod")
          (jolt-with-mutex mm-tbl-mu
            (hashtable-delete! (jolt-multifn-methods obj) (car rest))
            (set! jolt-mm-epoch (fx+ jolt-mm-epoch 1)))
          obj)
         ((string=? method-name "getMethod")
          (or (hashtable-ref (jolt-multifn-methods obj) (car rest) #f) jolt-nil))
         ;; keys AND values in one critical section, like jolt-methods-setup —
         ;; snapshotting only the keys and reffing each one afterwards let a
         ;; remove-method landing in between answer #f, and that raw Scheme false
         ;; went into the returned map as the value for a dispatch value that no
         ;; longer has a method.
         ((string=? method-name "getMethodTable")
          (let* ((tbl (jolt-multifn-methods obj))
                 (kv (jolt-with-mutex mm-tbl-mu
                       (let-values (((ks vs) (hashtable-entries tbl))) (cons ks vs))))
                 (ks (car kv)) (vs (cdr kv)))
            (let loop ((i 0) (m (jolt-hash-map)))
              (if (fx>=? i (vector-length ks))
                  m
                  (loop (fx+ i 1) (jolt-assoc1 m (vector-ref ks i) (vector-ref vs i)))))))
         ((string=? method-name "toString") (jolt-str-render-one obj))
         (else (dispatch-miss obj method-name rest))))
      ((and (jrec? obj) (find-method-any-protocol-arity (jrec-tag obj) method-name (+ 1 (length rest))))
       => (lambda (f) (apply jolt-invoke f obj rest)))
      ;; (.field inst): a deftype/record field read with no matching method.
      ;; Clojure reads the field for (.q x) just like (.-q x); a declared method
      ;; (above) wins, this is the field-accessor fallback.
      ((and (jrec? obj) (null? rest) (jrec-has? obj (keyword #f method-name)))
       (jrec-lookup obj (keyword #f method-name) jolt-nil))
      ;; a defrecord is Associative / ILookup / IPersistentMap / Seqable / Counted,
      ;; so its clojure.lang interface methods delegate to the map fns when not
      ;; overridden by a declared method — reitit's impl calls (.assoc match k v),
      ;; (.valAt …), (.without …) directly. A bare deftype implements these via its
      ;; own declared methods (handled above), so this is record-only.
      ((and (jrec-record? obj)
            (member method-name '("valAt" "assoc" "without" "containsKey" "cons"
                                  "count" "seq" "equiv" "entryAt" "empty")))
       (cond
         ((string=? method-name "valAt")
          (if (null? (cdr rest)) (jolt-get obj (car rest) jolt-nil) (jolt-get obj (car rest) (cadr rest))))
         ((string=? method-name "assoc") (jolt-assoc1 obj (car rest) (cadr rest)))
         ((string=? method-name "without") (jolt-dissoc obj (car rest)))
         ((string=? method-name "containsKey") (if (jolt-truthy? (jolt-contains? obj (car rest))) #t #f))
         ((string=? method-name "cons") (jolt-conj1 obj (car rest)))
         ((string=? method-name "count") (jolt-count obj))
         ((string=? method-name "seq") (jolt-seq obj))
         ((string=? method-name "equiv") (if (jolt= obj (car rest)) #t #f))
         ((string=? method-name "entryAt")
          (if (jolt-truthy? (jolt-contains? obj (car rest)))
              (make-map-entry (car rest) (jolt-get obj (car rest) jolt-nil)) jolt-nil))
         (else jolt-nil)))   ; .empty of a record is nil on the JVM
      ((reified-methods obj)
       => (lambda (rm)
            (let ((f (hashtable-ref rm method-name #f))
                  (d (jreify-delegate obj)))
              (cond
                (f (apply jolt-invoke f obj rest))
                ;; a proxy forwards what it does not override to the base
                ;; instance, through the full dispatcher so the delegate gets
                ;; whatever method resolution its own kind of value has.
                ;; rest-args, not rest: the dispatcher takes a jolt seq
                (d (record-method-dispatch d method-name rest-args))
                (else (dispatch-miss obj method-name rest))))))
      ;; java.lang.String interop: defined in natives-str.ss, loaded
      ;; after this file (free reference, resolved at call time).
      ((string? obj) (jolt-string-method method-name obj rest))
      ((jiterator? obj)
       (cond ((string=? method-name "hasNext") (not (jolt-nil? (jolt-seq (jiterator-cur obj)))))
             ((string=? method-name "next")
              (let ((s (jolt-seq (jiterator-cur obj))))
                (if (jolt-nil? s) (throw-jvm (quote NoSuchElementException) "iterator exhausted")
                    (let ((v (jolt-first s))) (jiterator-cur-set! obj (jolt-rest s)) v))))
             (else (dispatch-miss obj method-name rest))))
      ((string=? method-name "iterator") (make-jiterator (jolt-seq obj)))
      ;; clojure.lang.Keyword interop: a Keyword carries an interned `sym` field
      ;; (the symbol form, ns + name) plus the Named methods. honeysql/reitit read
      ;; (.sym k) on their :clj branch to recover the symbol without the colon.
      ((keyword-t? obj)
       (cond ((string=? method-name "sym")
              (jolt-symbol (keyword-t-ns obj) (keyword-t-name obj)))
             ((string=? method-name "getName") (keyword-t-name obj))
             ((string=? method-name "getNamespace") (or (keyword-t-ns obj) jolt-nil))
             ((string=? method-name "toString")
              (string-append ":" (if (keyword-t-ns obj) (string-append (keyword-t-ns obj) "/") "")
                             (keyword-t-name obj)))
             ;; Keyword.hashCode() is sym.hashCode() + 0x9e3779b9 — the JAVA
             ;; hash of the symbol, not its hasheq. keyword-t-khash is the hasheq
             ;; (murmur-based), which .hashCode answered with, so a keyword's
             ;; .hashCode disagreed with the JVM while a symbol's agreed.
             ((string=? method-name "hashCode")
              (jolt-s32 (+ (java-symbol-hash (keyword-t-name obj) (keyword-t-ns obj))
                           #x9e3779b9)))
             ((string=? method-name "equals") (and (pair? rest) (eq? obj (car rest))))
             (else (dispatch-miss obj method-name rest))))
      ;; clojure.lang.Symbol interop: the Named methods + getName/getNamespace.
      ((symbol-t? obj)
       (cond ((string=? method-name "getName") (symbol-t-name obj))
             ((string=? method-name "getNamespace") (or (symbol-t-ns obj) jolt-nil))
             ((string=? method-name "toString")
              (string-append (if (symbol-t-ns obj) (string-append (symbol-t-ns obj) "/") "")
                             (symbol-t-name obj)))
             ((string=? method-name "equals") (and (pair? rest) (jolt=2 obj (car rest))))
             ((string=? method-name "hashCode")
              (java-symbol-hash (symbol-t-name obj) (symbol-t-ns obj)))
             (else (dispatch-miss obj method-name rest))))
      ;; clojure.lang.Namespace: name/getName yield the ns name as a Symbol (JVM:
      ;; Namespace.name is a Symbol). clojure.spec.alpha reads (.name *ns*).
      ((jns? obj)
       (cond ((or (string=? method-name "name") (string=? method-name "getName"))
              (jolt-symbol #f (jns-name obj)))
             ((string=? method-name "toString") (jns-name obj))
             (else (dispatch-miss obj method-name rest))))
      ;; clojure.lang.Var: ns -> its Namespace, sym -> the simple-name Symbol.
      ;; clojure.spec.alpha's ->sym reads (.name (.ns v)) and (.sym v).
      ((var-cell? obj)
       (cond ((string=? method-name "ns") (intern-ns! (var-cell-ns obj)))
             ((or (string=? method-name "sym") (string=? method-name "name"))
              (jolt-symbol #f (var-cell-name obj)))
             ((string=? method-name "getName")
              (jolt-symbol (var-cell-ns obj) (var-cell-name obj)))
             ((string=? method-name "toString") (string-append "#'" (var-cell-ns obj) "/" (var-cell-name obj)))
             ;; getRawRoot is the ROOT value, past any thread binding — how
             ;; fully-satisfies' requiring-resolve reads the global
             ;; clojure.core/*loaded-libs* rather than whatever a load has bound
             ;; over it. deref would answer the binding.
             ((string=? method-name "getRawRoot") (var-cell-root obj))
             (else (dispatch-miss obj method-name rest))))
      ;; java.lang.Throwable interop over a Chez condition. A jolt host error
      ;; (`error`/`assertion-violationf`) raises a Chez condition; Clojure code
      ;; that catches it as a Throwable reads (.getMessage e) / (.toString e).
      ;; The surface itself is the ONE shared Throwable table (throwable-method,
      ;; records-interop.ss), so a raw condition and an ex-info answer exactly the
      ;; same set of methods — restating them here is what let the two drift.
      ((condition? obj)
       (cond ((throwable-method obj method-name rest) => car)
             (else (dispatch-miss obj method-name rest))))
      ;; java.lang.Character interop: (.toString \+) -> "+", etc.
      ((char? obj)
       (cond ((string=? method-name "toString") (string obj))
             ((string=? method-name "charValue") obj)
             ((string=? method-name "hashCode") (char->integer obj))
             ((string=? method-name "equals") (and (pair? rest) (char? (car rest)) (char=? obj (car rest))))
             ((string=? method-name "compareTo")
              (let ((o (car rest))) (cond ((char<? obj o) -1) ((char>? obj o) 1) (else 0))))
             (else (dispatch-miss obj method-name rest))))
      ;; java.util.SequencedCollection (JDK 21) over jolt's own persistent
      ;; collections — vector / list / seq / set are java.util.List or Set on the
      ;; JVM and carry these. getFirst / getLast raise NoSuchElementException on
      ;; an empty one; reversed() is a reverse-order VIEW there, and for an
      ;; immutable collection a copy is that view.
      ((and (string=? method-name "getFirst") (rd-persistent-coll? obj))
       (let ((s (jolt-seq obj)))
         (if (jolt-nil? s) (throw-jvm 'NoSuchElementException "") (seq-first s))))
      ((and (string=? method-name "getLast") (rd-persistent-coll? obj))
       (if (jolt-nil? (jolt-seq obj)) (throw-jvm 'NoSuchElementException "") (rd-coll-last obj)))
      ((and (string=? method-name "reversed") (rd-persistent-coll? obj))
       (let ((items (reverse (seq->list (jolt-seq obj)))))
         (if (pvec? obj) (apply jolt-vector items) (list->cseq items))))
      ;; The java.util.Collection / List / Set mutators: an immutable collection
      ;; refuses every one with UnsupportedOperationException, as on the JVM. It
      ;; used to fall to dispatch-miss — an IllegalArgumentException "no matching
      ;; method" that a (catch UnsupportedOperationException …) does not see. A
      ;; deftype is not a persistent collection here: its own methods answered
      ;; above, and an interface method it does not declare stays its own miss.
      ((and (rd-java-util-mutator? method-name) (rd-persistent-coll? obj))
       (throw-jvm 'UnsupportedOperationException ""))
      ;; java.util.List .indexOf / .lastIndexOf over any seqable (vector / list /
      ;; seq) — -1 when absent, like the JVM (medley/index-of reads this).
      ((or (string=? method-name "indexOf") (string=? method-name "lastIndexOf"))
       (let ((target (car rest)) (last? (string=? method-name "lastIndexOf")))
         (let loop ((s (jolt-seq obj)) (i 0) (found -1))
           (cond ((jolt-nil? s) found)
                 ((jolt=2 (seq-first s) target)
                  (if last? (loop (jolt-seq (seq-more s)) (fx+ i 1) i) i))
                 (else (loop (jolt-seq (seq-more s)) (fx+ i 1) found))))))
      ;; java.util.Collection.contains over a list/seq (vectors/sets handle it in
      ;; dot-coll-method): value membership, like the JVM.
      ((string=? method-name "contains")
       (let ((target (car rest)))
         (let loop ((s (jolt-seq obj)))
           (cond ((jolt-nil? s) #f)
                 ((jolt=2 (seq-first s) target) #t)
                 (else (loop (jolt-seq (seq-more s))))))))
      ;; universal Object methods on any remaining value (boolean, etc.).
      ((string=? method-name "toString") (jolt-str-render-one obj))
      ((string=? method-name "hashCode") (jolt-hash obj))
      ((string=? method-name "equals") (and (pair? rest) (if (jolt= obj (car rest)) #t #f)))
      ;; __methodImplCache is the JVM's per-fn protocol-method cache. jolt does not
      ;; cache protocol dispatch, so a read is nil (and the paired set! is a no-op):
      ;; libraries that wrap protocol methods sync this cache (schema's fn
      ;; instrumentation) and a consistent nil makes that a safe no-op.
      ((string=? method-name "__methodImplCache") jolt-nil)
      ;; Java interface default methods (isEmpty, size, contains, iterator, entrySet,
      ;; seq, …) for a deftype that implements java.util.Map / java.util.Collection:
      ;; dispatch through dot-coll-method, which delegates to the method-first
      ;; jolt-empty?/jolt-count/jolt-seq — so the type answers from its OWN seq/count.
      ;; A deftype that implements none of them still reaches the error below, via
      ;; the throw those dispatchers raise. dot-coll-method BOXES its result (so a
      ;; legitimate #f is distinguishable from "no such method"); unbox it, or every
      ;; caller gets a one-element list instead of the value.
      ((jrec? obj)
       (let ((boxed (dot-coll-method obj method-name rest)))
         (if boxed (car boxed) (dispatch-miss obj method-name rest))))
      (else (dispatch-miss obj method-name rest)))))

;; The end of the .method dispatch chain: the library extension tier, then the
;; throw. EVERY "this receiver has no such method" path routes here — the
;; per-type arms and the base's per-type conds each used to raise their own
;; throw-jvm with their own wording, which split the error surface (a File said
;; "No matching method for File: x" while a String said "No matching field
;; found: x for class java.lang.String", and neither applied the nil -> NPE
;; rule) and, more to the point, put those receivers out of reach of the
;; extension tier below.
;;
;; class-extensions.ss sets the hook the first time a library registers a
;; non-override extension; it is #f until then, so a process that never uses the
;; seam pays nothing and misses throw exactly as before. The hook answers
;; (obj method-name) -> proc | #f.
(define class-ext-fallback-hook #f)
(define (set-class-ext-fallback-hook! f) (set! class-ext-fallback-hook f))
(define (dispatch-miss obj method-name args)
  (let ((f (and class-ext-fallback-hook (class-ext-fallback-hook obj method-name))))
    (if f
        (apply jolt-invoke f obj args)
        (no-method-throw method-name obj (length args)))))

;; The end of the dispatch chain. A method call on nil is the JVM's
;; NullPointerException; anything else is its IllegalArgumentException ("No
;; matching method"). Raising a raw host error here left the value classless, so
;; a catch clause could not select it and (class e) read :object.
;;
;; Which of the two it is follows the JVM's reflector. A no-arg member read tries
;; the method and then the FIELD, so (.-x obj) and a missed 0-arg (.x obj) both
;; end as "No matching field found" — getting here with a dash at all means no arm
;; claimed it, since dot-forms.ss answers a field read only for a declared
;; deftype/defrecord slot. A miss with arguments can only have been a method.
;; Reading the dash back off here keeps the spellings apart without every arm
;; having to thread the distinction through.
(define (no-method-throw method-name obj . maybe-argc)
  (let* ((argc (if (null? maybe-argc) 0 (car maybe-argc)))
         (dashed? (and (> (string-length method-name) 1)
                       (char=? (string-ref method-name 0) #\-)))
         (bare (if dashed? (substring method-name 1 (string-length method-name)) method-name)))
    (cond
      ((jolt-nil? obj)
       (throw-jvm (quote NullPointerException)
                  (string-append "Cannot invoke \"" method-name "\" because the target is null")))
      ((or dashed? (fx=? argc 0))
       (throw-jvm (quote IllegalArgumentException)
                  (string-append "No matching field found: " bare " for class "
                                 (guard (e (#t "?")) (jolt-class-name obj)))))
      (else
       (throw-jvm (quote IllegalArgumentException)
                  (string-append "No matching method " method-name " found taking "
                                 (number->string argc) " args for class "
                                 (guard (e (#t "?")) (jolt-class-name obj))))))))

;; ---- method-dispatch arm registry ------------------------------------------
;; A .method call (record-method-dispatch) is resolved by an ordered list of arms
;; (ascending priority), each (obj method-name rest-args) -> result | 'pass.
;; This replaces a stack of (set! record-method-dispatch ...) rebindings across
;; six files whose precedence was implicit in load order — priority is now
;; explicit data. record-method-dispatch-base is the final fallback (the
;; string/keyword/symbol/Object-method surface). A host shim / library registers
;; an arm with register-method-arm! instead of set!-wrapping the dispatcher.
(define method-dispatch-arms '())   ; list of (priority . arm), ascending priority
(define (register-method-arm! priority arm)
  (set! method-dispatch-arms
    (let ins ((as method-dispatch-arms))
      (cond ((null? as) (list (cons priority arm)))
            ((< priority (caar as)) (cons (cons priority arm) as))
            (else (cons (car as) (ins (cdr as))))))))
;; Named priorities for register-method-arm!, in ascending dispatch order
;; (lowest is tried first — see record-method-dispatch). Each name mirrors its
;; arm's role; two disjoint-type arms may share a tier (regex-t and nio-path
;; both sit just above jfile at 42). Values are unchanged from the prior magic
;; numbers — this is a readability rename only.
;; Library overrides sit above every built-in arm: the whole point of the tier is
;; that jolt's own method for the class does not get a say. Registered lazily by
;; java/class-extensions.ss, so a process that never calls jolt.host/extend-class!
;; has no such arm in the chain at all.
(define arm-priority-user-override 1)
(define arm-priority-getclass 5)      ; .getClass — universal Object method, first
(define arm-priority-string 6)       ; string receivers — the base's string? case hoisted
(define arm-priority-dotform 30)      ; -field accessor + dot-form method dispatch
(define arm-priority-date 40)         ; java.util.Date (jinst) method surface
(define arm-priority-file 41)         ; java.io.File (jfile) methods
(define arm-priority-regex 42)        ; regex-t (Pattern) .split/.matcher surface
(define arm-priority-nio-path 42)     ; java.nio.file.Path methods (above jfile)
(define arm-priority-htable 43)       ; tagged htable method registry
(define arm-priority-host-type 44)    ; jhost/number/string per-type dispatch
(define (record-method-dispatch obj method-name rest-args)
  (let loop ((as method-dispatch-arms))
    (if (null? as)
        (record-method-dispatch-base obj method-name rest-args)
        (let ((r ((cdar as) obj method-name rest-args)))
          (if (eq? r 'pass) (loop (cdr as)) r)))))

;; Strings are the most common interop receiver in library code (honeysql's
;; format path alone is .charAt/.length/.indexOf/.toString per entity), and the
;; base's string? case sat BELOW every arm — each call walked getclass, dotform
;; (whose let* seq->list-converts the rest args even to pass), date/file/regex/
;; nio/htable/host-type before reaching it. Claim string receivers right after
;; getClass so they pay one type test instead. Same handler as the base case —
;; jolt-string-method — so an unknown method throws the identical error, and a
;; non-string still 'passes on unchanged.
;; The rest args arrive as a jolt-vector the call site built, never wider than
;; a method's arity, so it is all tail: read the tail vector straight into a
;; list instead of seq->list, which allocated a seq cell per argument plus the
;; vec->seq dispatch (about half of a 116 ns unhinted .charAt).
(define (method-rest-args->list rest-args)
  (cond ((jolt-nil? rest-args) '())
        ((and (pvec? rest-args)
              (fx=? (pvec-cnt rest-args) (vector-length (pvec-tail rest-args))))
         (vector->list (pvec-tail rest-args)))
        (else (seq->list rest-args))))
(register-method-arm! arm-priority-string
  (lambda (obj method-name rest-args)
    (if (string? obj)
        (jolt-string-method method-name obj (method-rest-args->list rest-args))
        'pass)))


;; (.getClass x): a universal Object method reached by EVERY value before any
;; per-type arm — the class token for the value (jolt has no Class objects; the
;; token is the canonical name string, on which .getName/.getSimpleName work).
;; One arm, so a type arm that only whitelists its own methods can't steal it.
(register-method-arm! arm-priority-getclass
  (lambda (obj method-name rest-args)
    (if (string=? method-name "getClass") (jolt-class obj) 'pass)))

;; reify: instance-local method table. obj is a jreify carrying a method ht +
;; the protocol short-names it implements (for satisfies?/instance?).
;; A reify may carry a DELEGATE: an object that answers any method the reify's own
;; table does not. clojure.core/proxy over a concrete class builds one that way —
;; see java/proxy.ss. A plain reify has no delegate and a method miss still throws.
(define-record-type jreify (fields methods protos delegate) (nongenerative chez-jreify-v2))
;; likewise a reify: (def r (reify ...)) is code the restoring build already has.
(register-code-value! jreify?)
(define (reified-methods obj) (and (jreify? obj) (jreify-methods obj)))
(define (reify-delegate obj) (and (jreify? obj) (jreify-delegate obj)))
;; (get reify k) / (:k reify) routes to a reify's ILookup valAt — clojure.spec.alpha
;; reifies fspec/regex specs as clojure.lang.ILookup and reads (:args spec) off them.
(register-get-arm! jreify?
  (lambda (coll k d)
    (let ((m (and (reified-methods coll) (hashtable-ref (reified-methods coll) "valAt" #f))))
      (if m (jolt-invoke m coll k d) d))))
(define (make-reified-delegating methods-map delegate proto-names)
  (let ((ht (make-hashtable string-hash string=?))
        (protos (if (and (pair? proto-names) (null? (cdr proto-names)) (jolt-coll-pred? (car proto-names)))
                    (seq->list (car proto-names)) proto-names)))
    (for-each (lambda (p) (hashtable-set! ht (if (keyword? p) (keyword-t-name p) p)
                                          (jolt-get methods-map p jolt-nil)))
              (seq->list (jolt-keys methods-map)))
    (make-jreify ht (map (lambda (p) (if (symbol-t? p) (symbol-t-name p) p)) protos) delegate)))
(define (make-reified methods-map . proto-names)
  (make-reified-delegating methods-map #f proto-names))
;; A deftype or reify that DECLARES java.lang.Iterable or java.util.Iterator is
;; seqable, as on the JVM: seq of an Iterable walks its iterator, and seq of an
;; Iterator walks what it has left. ring's multipart middleware hands its item
;; iterator to `sequence` wrapped in (reify Iterable (iterator [_] ...)), which
;; used to fail "Don't know how to create ISeq from: ...$reify__0".
;;
;; The walk is LAZY, one element per forced cell: an iterator is a cursor over
;; something being produced, and realizing it eagerly would both change when the
;; producer runs and defeat any caller that stops early.
;; Seqable wins over Iterable, as in RT.seqFrom: a type declaring both is seqed
;; through its own seq method, not its iterator. Arms are consulted newest-first
;; and these are registered last, so without the check they would shadow the
;; coll-interface arm above for every deftype that declares both.
(define (iface-prefers-seq? v)
  (or (jrec-declares-coll-iface? v) (and (iface-method v "seq" #f) #t)))
(define (iface-iterator-obj v)
  (and (or (jrec? v) (jreify? v)) (not (iface-prefers-seq? v)) (iface-method v "iterator" #f)))
(define (iface-iterator-cursor v)
  (and (or (jrec? v) (jreify? v)) (not (iface-prefers-seq? v)) (iface-method v "hasNext" #f)))
(define (iterator-cursor->seq it)
  (jolt-make-lazy-seq
   (lambda ()
     (if (jolt-truthy? (record-method-dispatch it "hasNext" jolt-nil))
         (let ((v (record-method-dispatch it "next" jolt-nil)))
           (jolt-cons v (iterator-cursor->seq it)))
         jolt-nil))))
(register-seq-arm! iface-iterator-cursor
                   (lambda (x) (jolt-seq (iterator-cursor->seq x))))
(register-seq-arm! (lambda (x) (and (iface-iterator-obj x) (not (iface-iterator-cursor x))))
                   (lambda (x) (jolt-seq (record-method-dispatch x "iterator" jolt-nil))))


;; satisfies?: does obj's type implement the protocol? proto is a defprotocol
;; value (a map with a :name). A host Class or interface answers instance?: jolt
;; takes :bb reader branches, and code written for babashka asks
;; (satisfies? clojure.lang.IObj x) where its JVM branch asks instance? — the
;; JVM raises on the class form, babashka answers false for everything, jolt
;; answers the question the code means. Any other non-protocol throws, with a
;; message naming what was passed.
(define (jolt-satisfies? proto obj)
  (if (jclass? proto)
      (if (instance-check proto obj) #t #f)
      (jolt-satisfies-protocol? proto obj)))
(define (jolt-satisfies-protocol? proto obj)
  (let* ((pn (jolt-get proto (keyword #f "name") jolt-nil))
         (pn-str (if (symbol-t? pn) (symbol-t-name pn) pn)))
    (unless (string? pn-str)
      (throw-jvm (quote IllegalArgumentException)
        (string-append "satisfies? expects a protocol, got: "
          (cond ((jclass? proto) (jclass-name proto))
                ((jolt-nil? proto) "nil")
                (else (jolt-final-str proto))))))
    (or
      ;; direct: a record type's own registry, a reify's declared list.
      (cond
        ((jrec? obj) (and (type-satisfies? (jrec-tag obj) pn-str) #t))
        ((jreify? obj)
         (and (memp (lambda (p) (or (string=? p pn-str) (proto-class-match? p pn-str)))
                    (jreify-protos obj))
              #t))
        (else #f))
      ;; extended: the protocol may be extended to an interface or class the
      ;; value reports — value-host-tags includes a deftype/reify's declared
      ;; interfaces — the same walk dispatch takes. On the JVM one instanceof
      ;; answers both the direct and the extended case.
      (let loop ((tags (value-host-tags obj)))
        (cond ((null? tags) #f)
              ((type-satisfies? (car tags) pn-str) #t)
              (else (loop (cdr tags))))))))
(define (last-dot s)
  (let loop ((i (- (string-length s) 1)))
    (cond ((< i 0) s) ((char=? (string-ref s i) #\.) (substring s (+ i 1) (string-length s))) (else (loop (- i 1))))))
(define (memp pred lst) (cond ((null? lst) #f) ((pred (car lst)) lst) (else (memp pred (cdr lst)))))

;; extenders: type-tags that extend a protocol via extend/extend-type/extend-
;; protocol, as symbols (extends? reads this). Inline deftype/defrecord impls are
;; excluded — only tags carrying the extend mark count, matching the JVM.
(define (extenders proto)
  (let* ((pn (jolt-get proto (keyword #f "name") jolt-nil))
         (pn-str (if (symbol-t? pn) (symbol-t-name pn) pn))
         (out '()))
    (vector-for-each
      (lambda (tag)
        (let ((ti (hashtable-ref type-registry tag #f)))
          (when ti (let ((pi (hashtable-ref ti pn-str #f)))
                     (when (and pi (hashtable-ref pi extend-mark #f))
                       (set! out (cons (jolt-symbol jolt-nil tag) out)))))))
      (jolt-with-mutex rec-tbl-mu (hashtable-keys type-registry)))
    (if (null? out) jolt-nil (list->cseq out))))

;; jolt exception values (ex-info + host-constructed throwables) are ex-info-shaped
;; maps tagged :jolt/type :jolt/ex-info; (class …)/instance? read the JVM class off
;; the optional :jolt/class key, defaulting to clojure.lang.ExceptionInfo.
;; str of a jrec with no toString of its own is its print form. The JVM answers
;; Object.toString there ("user.Rec@3c6f0c28"); jolt has no identity hash to
;; print, so it renders the value and drops the leading # of the record marker.
;; A COLLECTION deftype prints in its collection's shape, which carries no such
;; marker — dropping a character there turned "(1 2 3)" into "1 2 3)".
(register-str-render! jrec?
  (lambda (v)
    (let ((f (find-protocol-method (jrec-tag v) "Object" "toString")))
      (cond (f (jolt-invoke f v))
            ((jrec-coll-print-shape v) => (lambda (shape) (jrec-coll-pr v shape)))
            (else (let ((s (jrec-field-pr v))) (substring s 1 (string-length s))))))))

;; a reify with a toString method renders through it, like the JVM.
(register-str-render! (lambda (v) (and (jreify? v) (reified-methods v)
                                       (hashtable-ref (reified-methods v) "toString" #f) #t))
  (lambda (v) (jolt-invoke (hashtable-ref (reified-methods v) "toString" #f) v)))

;; `type` lives in natives-meta.ss: it needs jolt-meta for the :type
;; override and a total value->taxonomy mapping, so it sits with meta — a record
;; yields (jolt-symbol #f (jrec-tag x)), the ns.Name class-name symbol.

(def-var! "clojure.core" "make-deftype-ctor" make-deftype-ctor)

;; defrecord marks its type a record (deftype does not), keyed by the same
;; "ns.Name" tag make-deftype-ctor bakes — so jrec-record? distinguishes the two.
(define (register-record-type! name-sym)
  (let ((tag (string-append (chez-current-ns) "." (symbol-t-name name-sym))))
    (jolt-with-mutex rec-tbl-mu (hashtable-set! chez-record-type-tbl tag #t))
    ;; a defrecord's class ancestry: replace the deftype IType row with the
    ;; record interfaces (their closure supplies Associative/Seqable/ILookup/…),
    ;; keeping any protocol interfaces already grafted by the inline
    ;; registrations that ran between the deftype ctor and this call.
    (let ((protos (filter (lambda (s) (not (string=? s "clojure.lang.IType")))
                          (jch-direct-supers tag))))
      (jch-set-supers! tag (append protos
                                   '("clojure.lang.IRecord" "clojure.lang.IObj"
                                     "clojure.lang.IPersistentMap" "java.util.Map"
                                     "clojure.lang.IHashEq" "java.io.Serializable"))))
    ;; every defrecord gets a static create(map) on the JVM — it is what the
    ;; #ns.Rec{…} literal is read through, positionally or by key.
    (let ((ctor (hashtable-ref class-ctors-tbl tag #f))
          (shape (hashtable-ref chez-record-shapes-tbl
                                (string-append (chez-current-ns) "/->" (symbol-t-name name-sym)) #f)))
      (when (and ctor shape)
        (register-class-statics! tag
          (list (cons "create"
                      (lambda (m)
                        ;; declared fields positionally, anything else assoc'd on —
                        ;; a record keeps unknown keys in its extension map.
                        (let ((kws (vector-ref shape 0)))
                          (let loop ((rec (apply ctor (map (lambda (k) (jolt-get m k)) kws)))
                                     (ks (seq->list (jolt-seq (jolt-keys m)))))
                            (cond ((null? ks) rec)
                                  ((member (car ks) kws) (loop rec (cdr ks)))
                                  (else (loop (jolt-assoc rec (car ks) (jolt-get m (car ks)))
                                              (cdr ks)))))))))))))
  jolt-nil)
(def-var! "clojure.core" "register-record-type!" register-record-type!)
(def-var! "clojure.core" "make-protocol" make-protocol)
(def-var! "clojure.core" "register-protocol-methods!" register-protocol-methods!)
(def-var! "clojure.core" "register-method" register-method)
(def-var! "clojure.core" "register-inline-method" register-inline-method)
(def-var! "clojure.core" "register-inline-protocol!" register-inline-protocol!)
(def-var! "jolt.host" "set-field!" jolt-set-field!)
(def-var! "clojure.core" "protocol-dispatch" (lambda (pn mn obj rest) (protocol-dispatch pn mn obj rest)))
(def-var! "clojure.core" "protocol-dispatch1" (lambda (pn mn obj) (protocol-dispatch1 pn mn obj)))
(def-var! "clojure.core" "protocol-dispatch2" (lambda (pn mn obj a) (protocol-dispatch2 pn mn obj a)))
(def-var! "clojure.core" "protocol-dispatch3" (lambda (pn mn obj a b) (protocol-dispatch3 pn mn obj a b)))
(def-var! "clojure.core" "satisfies?" jolt-satisfies?)
(def-var! "clojure.core" "extenders" extenders)
(def-var! "jolt.host" "type-satisfies?" type-satisfies?)
;; The dispatch key for the protocol SYM names — resolved the way any other
;; reference resolves, through :refer and :as — or nil when the symbol names no
;; protocol (a host class or interface, which keeps its bare name). deftype /
;; defrecord / reify / extend-type call this at macroexpansion so an impl is
;; filed under the protocol's own identity rather than the name it was spelled
;; with at the use site.
(def-var! "jolt.host" "protocol-key-of"
  (lambda (sym)
    (or (and (symbol-t? sym)
             (let ((v (jolt-resolve sym)))
               (and (var-cell? v) (protocol-value-key (var-cell-root v)))))
        ;; the dotted CLASS spelling of a protocol: a deftype/reify may name it
        ;; by its class — mulog's ConsolePublisher implements
        ;; com.brunobonacci.mulog.publisher.PPublisher — where the last segment
        ;; is the protocol name and the demunged prefix its namespace. Without
        ;; this the methods filed as interface methods and protocol dispatch
        ;; answered "No method ...".
        (and (symbol-t? sym) (not (symbol-t-ns sym))
             (let* ((nm (symbol-t-name sym))
                    (n (string-length nm))
                    (i (let loop ((k (- n 1)))
                         (cond ((< k 1) #f)
                               ((char=? (string-ref nm k) #\.) k)
                               (else (loop (- k 1)))))))
               (and i (< (+ i 1) n)
                    (let* ((ns-part (list->string
                                     (map (lambda (c) (if (char=? c #\_) #\- c))
                                          (string->list (substring nm 0 i)))))
                           (name-part (substring nm (+ i 1) n))
                           (cell (var-cell-lookup ns-part name-part)))
                      (and cell (protocol-value-key (var-cell-root cell)))))))
        jolt-nil)))
(def-var! "clojure.core" "make-reified" (lambda (mm . rest) (apply make-reified mm rest)))
(def-var! "clojure.core" "record-method-dispatch" (lambda (obj m rest) (record-method-dispatch obj m rest)))
