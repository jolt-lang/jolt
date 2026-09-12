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
;;
;; List and Set are kept APART, because the two carry different methods and a
;; single predicate for both answered List's on a set: (.getFirst #{1 2}) read 1
;; and (.reversed #{1 2}) a reversed seq, where the JVM has neither — a
;; PersistentHashSet is a java.util.Set, which is not a SequencedCollection, so
;; both are "No matching method" there. A SORTED set is the same answer for a
;; different reason: it is a PersistentTreeSet, which is not a SequencedSet
;; either. It is an htable rather than a pset in jolt, which is why it needs
;; naming here at all — without it every Collection mutator on a sorted set fell
;; through to a miss instead of being refused.
(define (rd-java-list? obj)
  (or (pvec? obj) (cseq? obj) (empty-list-t? obj) (jolt-lazyseq? obj)))
(define (rd-java-set? obj)
  (or (pset? obj) (htable-sorted-set? obj)))
(define (rd-persistent-coll? obj)
  (or (rd-java-list? obj) (rd-java-set? obj)))
(define (rd-coll-last obj)
  (if (pvec? obj)
      (jolt-nth obj (fx- (jolt-count obj) 1))
      (let loop ((s (jolt-seq obj)))
        (let ((n (jolt-seq (seq-more s))))
          (if (jolt-nil? n) (seq-first s) (loop n))))))
;; The java.util mutators an immutable collection refuses, keyed by name AND
;; ARITY — because a name on its own is not a method. java.util.List/set is
;; set(int,E), so a one-argument (.set [1] 2) matches nothing on the JVM and is
;; its IllegalArgumentException "No matching method set found taking 1 args".
;; Refusing it with UnsupportedOperationException, as a bare list of names did,
;; both named a method the class does not have and put the call out of reach of
;; the (catch IllegalArgumentException …) a caller writes; a no-argument .add and
;; a one-argument .clear were the same mistake (jolt-8oa).
;;
;; Two surfaces, because a set carries fewer of them. A vector / list / seq is a
;; java.util.List AND, on JDK 21, a SequencedCollection: it has the positional
;; overloads (add/2, addAll/2, set/2), the four ends (addFirst, addLast,
;; removeFirst, removeLast) and List's bulk ops (replaceAll, sort). jolt's set is
;; a plain java.util.Set — a SORTED set too, which is PersistentTreeSet and not a
;; SequencedSet on the JVM, checked — so it has Collection's surface and nothing
;; more, and every List-only name above is "No matching method" on one rather
;; than a refusal.
(define rd-collection-mutators
  '(("add" 1) ("addAll" 1) ("clear" 0) ("remove" 1) ("removeAll" 1)
    ("removeIf" 1) ("retainAll" 1)))
(define rd-list-mutators
  '(("add" 2) ("addAll" 2) ("set" 2) ("addFirst" 1) ("addLast" 1)
    ("removeFirst" 0) ("removeLast" 0) ("replaceAll" 1) ("sort" 1)))
(define (rd-mutator-in? table name argc)
  (let loop ((t table))
    (cond ((null? t) #f)
          ((and (string=? (caar t) name) (fx=? (cadar t) argc)) #t)
          (else (loop (cdr t))))))
(define (rd-coll-mutator? obj name argc)
  (or (and (rd-persistent-coll? obj) (rd-mutator-in? rd-collection-mutators name argc))
      (and (rd-java-list? obj) (rd-mutator-in? rd-list-mutators name argc))))

;; clojure.lang.Var meta reads for the instance arm below. A cell's meta is a
;; pmap, or #f / nil before anything attached one (state-image rebuilds a cell with
;; #f), so both empty shapes read as "no such key".
(define (rd-var-meta obj)
  (let ((m (var-cell-meta obj))) (and m (not (jolt-nil? m)) m)))
(define (rd-var-meta-get obj key)
  (let ((m (rd-var-meta obj))) (if m (jolt-get m (keyword #f key) jolt-nil) jolt-nil)))
(define (rd-var-meta-flag? obj key) (jolt-truthy? (rd-var-meta-get obj key)))
(define (rd-args->list x) (let ((s (jolt-seq x))) (if (jolt-nil? s) '() (seq->list s))))

;; clojure.lang.ARef's watch and validator methods, for the base's reference arm
;; below. Keyed by name AND arity so a wrong-arity call falls through to
;; dispatch-miss and reports the JVM's "No matching method" instead of faulting
;; inside the native on a short `rest`. Each body is the same procedure the
;; clojure.core fn of that name is def-var!'d to (atoms.ss), so a watch
;; registered through interop and one registered through add-watch are one list.
(define (rd-iref-method name argc)
  (cond ((string=? name "addWatch")      (and (fx=? argc 2) jolt-add-watch))
        ((string=? name "removeWatch")   (and (fx=? argc 1) jolt-remove-watch))
        ((string=? name "getWatches")    (and (fx=? argc 0) jolt-get-watches))
        ((string=? name "notifyWatches") (and (fx=? argc 2) jolt-notify-watches))
        ((string=? name "setValidator")  (and (fx=? argc 1) jolt-set-validator!))
        ((string=? name "getValidator")  (and (fx=? argc 0) jolt-get-validator))
        (else #f)))

;; clojure.lang.IDeref, the root every reference shares. On the JVM it is one
;; real method plus five DEFAULT ones: get() is deref(), and getAsBoolean /
;; getAsInt / getAsLong / getAsDouble are the java.util.function bridges, each
;; the matching RT cast of deref(). The casts here are the same natives
;; clojure.core/boolean, int, long and double are bound to, so (.getAsInt (atom
;; 3.9)) truncates to 3 exactly as (int 3.9) does.
;;
;; The two-argument deref is IBlockingDeref's (ms, timeout-val), and only a
;; promise and a future declare it. That arity has to be screened HERE rather than
;; left to jolt-deref, which refuses it with the ClassCastException @ raises: the
;; JVM never gets that far, because there is no two-argument deref on a Delay to
;; reflect onto in the first place, so (.deref (delay 1) 50 :to) is "No matching
;; method deref found taking 2 args" there. Hence a KIND rather than a predicate.
;;
;; rd-deref-kind knows the deref-able types THIS file can see. future / promise /
;; agent / delay live in java/concurrency.ss, which loads long after this file and
;; is no part of the Gambit host at all, so it classifies its own four through the
;; hook — the same shape as rd-class-method-hook above. A var is deliberately
;; absent: the Var arm below answers deref/get off var-cell-deref, which hands
;; back the Unbound object where jolt-deref would throw, and it keeps that.
(define rd-extra-deref-hook #f)
(define (set-rd-extra-deref-hook! f) (set! rd-extra-deref-hook f))
(define (rd-deref-kind x)
  (cond ((or (jolt-atom? x) (jolt-ref? x) (jvol? x) (jolt-reduced? x)) (quote ideref))
        (rd-extra-deref-hook (rd-extra-deref-hook x))
        (else #f)))
(define (rd-derefable? x) (and (rd-deref-kind x) #t))
(define (rd-blocking-derefable? x) (eq? (quote iblocking) (rd-deref-kind x)))
(define (rd-ideref-method x name argc)
  (cond ((string=? name "deref")
         (and (or (fx=? argc 0)
                  (and (fx=? argc 2) (rd-blocking-derefable? x)))
              jolt-deref))
        ((string=? name "get")          (and (fx=? argc 0) jolt-deref))
        ((string=? name "getAsBoolean") (and (fx=? argc 0) rd-deref-as-boolean))
        ((string=? name "getAsInt")     (and (fx=? argc 0) rd-deref-as-int))
        ((string=? name "getAsLong")    (and (fx=? argc 0) rd-deref-as-long))
        ((string=? name "getAsDouble")  (and (fx=? argc 0) rd-deref-as-double))
        (else #f)))
(define (rd-deref-as-boolean x) (jolt-boolean (jolt-deref x)))
(define (rd-deref-as-int x) (jolt-int-cast (jolt-deref x)))
(define (rd-deref-as-long x) (jolt-long-cast (jolt-deref x)))
(define (rd-deref-as-double x) (jolt-double (jolt-deref x)))

;; clojure.lang.IAtom / IAtom2 — the five mutators, each the native the
;; clojure.core fn of the same meaning is bound to, so the CAS retry, the
;; validator and the watch notification are the ones swap!/reset! already do.
;;
;; swap and swapVals have a fourth arity that is the JVM's (f, x, y, ISeq args)
;; spread form — RT.listStar there, the trailing seq spliced onto the tail here.
;; Every shorter arity is already flat, which is why the spread only fires at 4.
(define (rd-atom-method name argc)
  (cond ((string=? name "swap")          (and (memv argc (quote (1 2 3 4))) rd-atom-swap))
        ((string=? name "swapVals")      (and (memv argc (quote (1 2 3 4))) rd-atom-swap-vals))
        ((string=? name "reset")         (and (fx=? argc 1) jolt-reset!))
        ((string=? name "resetVals")     (and (fx=? argc 1) jolt-reset-vals!))
        ((string=? name "compareAndSet") (and (fx=? argc 2) jolt-compare-and-set!))
        (else #f)))
(define (rd-spread-tail args)
  (if (fx=? (length args) 4)
      (cons (car args) (cons (cadr args) (cons (caddr args) (rd-args->list (cadddr args)))))
      args))
(define (rd-atom-swap a . args) (apply jolt-swap! a (rd-spread-tail args)))
(define (rd-atom-swap-vals a . args) (apply jolt-swap-vals! a (rd-spread-tail args)))

;; clojure.lang.Ref. set / alter / commute are the transaction mutators, and the
;; natives raise IllegalStateException off a transaction exactly as Ref does;
;; alter and commute take the JVM's (fn, ISeq args) rather than a spread arglist.
;; touch is what clojure.core/ensure calls and is VOID there, so the value ensure
;; answers is dropped. jolt keeps no ref history — ref-history-count is 0 by
;; construction — so getHistoryCount answers 0 and trimHistory is the no-op it
;; already is, while the min/max knobs are the same side tables ref-min-history
;; and ref-max-history read (each native is getter and setter by arity, and the
;; setter answers the ref, as Ref.setMinHistory does). deref is not here: a ref
;; is rd-derefable?, so the IDeref table above claims it.
(define (rd-ref-method name argc)
  (cond ((string=? name "set")             (and (fx=? argc 1) jolt-ref-set))
        ((string=? name "alter")           (and (fx=? argc 2) rd-ref-alter))
        ((string=? name "commute")         (and (fx=? argc 2) rd-ref-commute))
        ((string=? name "touch")           (and (fx=? argc 0) rd-ref-touch))
        ((string=? name "getMinHistory")   (and (fx=? argc 0) jolt-ref-min-history))
        ((string=? name "setMinHistory")   (and (fx=? argc 1) jolt-ref-min-history))
        ((string=? name "getMaxHistory")   (and (fx=? argc 0) jolt-ref-max-history))
        ((string=? name "setMaxHistory")   (and (fx=? argc 1) jolt-ref-max-history))
        ((string=? name "getHistoryCount") (and (fx=? argc 0) jolt-ref-history-count))
        ((string=? name "trimHistory")     (and (fx=? argc 0) rd-ref-trim-history))
        (else #f)))
(define (rd-ref-alter r f args) (apply jolt-alter r f (rd-args->list args)))
(define (rd-ref-commute r f args) (apply jolt-commute r f (rd-args->list args)))
(define (rd-ref-touch r) (jolt-ensure r) jolt-nil)
(define (rd-ref-trim-history r) jolt-nil)

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
                  ;; presented in the JVM spelling of the tag (my_app.core.Foo for
                  ;; a type in my-app.core), as the class token's own methods are
                  (let ((jvm (jch-munge-segments tag)))
                    (cond ((or (string=? method-name "getName") (string=? method-name "getCanonicalName")
                               (string=? method-name "getTypeName")) jvm)
                          ((string=? method-name "getSimpleName") (last-dot jvm))
                          ((string=? method-name "toString") (string-append "class " jvm))
                          (else (dispatch-miss obj method-name rest))))))))
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
                ;; a concrete method of an abstract class the reify declares —
                ;; (proxy [java.io.InputStream] …) inheriting readAllBytes — runs
                ;; against the reify itself, so its calls back into the
                ;; abstract method reach the override (the hook below).
                ((and abstract-class-method-hook (abstract-class-method-hook obj method-name))
                 => (lambda (m) (apply m obj rest)))
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
      ;; clojure.lang.ARef's watch/validator surface, answered for all four
      ;; watchable reference types at once — atom, var, ref, agent. Each already
      ;; IS an IRef by class ((instance? clojure.lang.IRef a) is true and
      ;; (supers clojure.lang.Atom) lists ARef), but the METHODS were missing, so
      ;; a library reaching the seam through interop rather than
      ;; clojure.core/add-watch got "No matching method addWatch found taking 2
      ;; args for class clojure.lang.Atom".
      ;;
      ;; It sits above the Var arm because a Var answers these too, and the
      ;; receiver guard runs FIRST: a deftype spelling a method the same way is
      ;; not watchable, so it never gets here (its own methods answered in the
      ;; dot-form arm anyway), and a plain value falls through to dispatch-miss.
      ((and (jolt-iref-watchable? obj) (rd-iref-method method-name (length rest)))
       => (lambda (f) (apply f obj rest)))
      ;; ...and the interface each reference type declares BELOW ARef: IDeref for
      ;; every one of them, IAtom/IAtom2 for an atom, Ref's transaction and
      ;; history surface for a ref. Agent's own half is a registered arm in
      ;; java/concurrency.ss, where its natives live. Same shape as the watch arm
      ;; above — receiver first, then name and arity — so a wrong arity or a
      ;; receiver of the wrong kind falls through to dispatch-miss and reports the
      ;; JVM's "No matching method" rather than faulting inside a native.
      ((and (jolt-atom? obj) (rd-atom-method method-name (length rest)))
       => (lambda (f) (apply f obj rest)))
      ((and (jolt-ref? obj) (rd-ref-method method-name (length rest)))
       => (lambda (f) (apply f obj rest)))
      ((and (rd-derefable? obj) (rd-ideref-method obj method-name (length rest)))
       => (lambda (f) (apply f obj rest)))
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
             ;; The rest of Var's surface, each reading the state the clojure.core
             ;; fn of the same meaning reads: isMacro the cell's macro flag (which
             ;; alter-meta! keeps in step with meta :macro), isBound hasRoot OR a
             ;; thread binding (bound?), hasRoot the root alone, isDynamic /
             ;; isPublic / getTag the meta, deref / get the thread binding then
             ;; the root — an unbound root answers its Unbound object rather than
             ;; throwing, as Var.deref does. These used to fall to "No matching
             ;; field", and typedclojure reads (.isMacro v) at every def it checks.
             ((string=? method-name "isMacro") (var-cell-macro? obj))
             ((string=? method-name "isBound") (jolt-var-bound-one? obj))
             ((string=? method-name "hasRoot") (not (jolt-var-unbound? (var-cell-root obj))))
             ((string=? method-name "isDynamic") (var-cell-dynamic? obj))
             ((string=? method-name "isPublic") (not (rd-var-meta-flag? obj "private")))
             ((string=? method-name "getTag") (rd-var-meta-get obj "tag"))
             ((or (string=? method-name "deref") (string=? method-name "get")) (var-cell-deref obj))
             ;; setMacro sets the flag AND meta :macro, the pair alter-meta! keeps
             ;; together. bindRoot / alterRoot go through alter-var-root so the
             ;; validator and watches see the change; bindRoot also clears the
             ;; macro flag, as Var.bindRoot does (a def over a macro un-macros it).
             ;; setDynamic writes the FLAG and leaves the metadata alone, as the
             ;; JVM does — the two are separate there, and (:dynamic (meta v))
             ;; after a bare .setDynamic is nil on both now. It answers the var,
             ;; and the boolean overload turns the flag off.
             ((string=? method-name "setDynamic")
              (var-cell-dynamic?-set! obj (if (pair? rest) (jolt-truthy? (car rest)) #t))
              obj)
             ((string=? method-name "setMacro")
              (var-cell-macro?-set! obj #t)
              (var-cell-meta-set! obj (jolt-assoc (or (rd-var-meta obj) (jolt-hash-map)) jolt-kw-var-macro #t))
              jolt-nil)
             ((string=? method-name "bindRoot")
              (jolt-alter-var-root obj (lambda (_) (car rest)))
              (var-cell-macro?-set! obj #f)
              (let ((m (rd-var-meta obj)))
                (when m (var-cell-meta-set! obj (jolt-dissoc2 m jolt-kw-var-macro))))
              jolt-nil)
             ((string=? method-name "alterRoot")
              (apply jolt-alter-var-root obj (car rest) (rd-args->list (cadr rest))))
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
      ;; collections — a vector / list / seq is a java.util.List, which is one.
      ;; A SET is not, sorted or otherwise (see rd-java-list? above), so these
      ;; three stay off it. getFirst / getLast raise NoSuchElementException on an
      ;; empty one; reversed() is a reverse-order VIEW there, and for an
      ;; immutable collection a copy is that view.
      ((and (string=? method-name "getFirst") (rd-java-list? obj))
       (let ((s (jolt-seq obj)))
         (if (jolt-nil? s) (throw-jvm 'NoSuchElementException "") (seq-first s))))
      ((and (string=? method-name "getLast") (rd-java-list? obj))
       (if (jolt-nil? (jolt-seq obj)) (throw-jvm 'NoSuchElementException "") (rd-coll-last obj)))
      ((and (string=? method-name "reversed") (rd-java-list? obj))
       (let ((items (reverse (seq->list (jolt-seq obj)))))
         (if (pvec? obj) (apply jolt-vector items) (list->cseq items))))
      ;; The java.util.Collection / List / Set mutators: an immutable collection
      ;; refuses every one it HAS with UnsupportedOperationException, as on the
      ;; JVM — these used to fall to dispatch-miss, an IllegalArgumentException
      ;; "no matching method" that a (catch UnsupportedOperationException …) does
      ;; not see. Which ones it has is rd-coll-mutator? above; one it merely
      ;; SPELLS goes back to being that miss. A deftype is not a persistent
      ;; collection here: its own methods answered above, and an interface method
      ;; it does not declare stays its own miss.
      ;;
      ;; On an EMPTY collection three of them never reach a mutation to refuse,
      ;; because they are DEFAULT methods that walk the elements first and so the
      ;; JVM has answered before it can throw: removeFirst / removeLast raise
      ;; NoSuchElementException (the same empty check getFirst / getLast make
      ;; above), removeIf answers false — it removed nothing — and replaceAll and
      ;; sort are void and do nothing at all. removeIf's default walks the
      ;; elements and calls remove() only for one the predicate MATCHES, so on a
      ;; non-empty collection it is the predicate that decides: no match is
      ;; false, a match is the refusal. (replaceAll and sort call set() for
      ;; every element and refuse on any non-empty one.)
      ((rd-coll-mutator? obj method-name (length rest))
       (let ((empty? (jolt-nil? (jolt-seq obj))))
         (cond
           ((string=? method-name "removeIf")
            (if (and (not empty?)
                     (let loop ((s (jolt-seq obj)))
                       (and (not (jolt-nil? s))
                            (or (jolt-truthy? (jolt-invoke (car rest) (seq-first s)))
                                (loop (jolt-seq (seq-more s)))))))
                (throw-jvm 'UnsupportedOperationException "")
                #f))
           ((not empty?) (throw-jvm 'UnsupportedOperationException ""))
           ((or (string=? method-name "removeFirst") (string=? method-name "removeLast"))
            (throw-jvm 'NoSuchElementException ""))
           ((or (string=? method-name "replaceAll") (string=? method-name "sort")) jolt-nil)
           (else (throw-jvm 'UnsupportedOperationException "")))))
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

;; The concrete methods of an abstract host class, for a reify that declares the
;; class and did not write the method: (obj method-name) -> proc | #f, the proc
;; taking the reify as its first argument. host-static.ss installs it over its
;; per-class tables (register-abstract-methods!); until then a reify's method
;; miss is a miss, as it was.
(define abstract-class-method-hook #f)
(define (set-abstract-class-method-hook! f) (set! abstract-class-method-hook f))
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
;; java/concurrency.ss registers clojure.lang.Agent's own methods here rather
;; than in the base above: the agent natives are in that file, it loads long
;; after this one, and the Gambit host does not include it at all. Nothing else
;; claims those names, so the tier is only about where the code can live.
(define arm-priority-agent 45)      ; clojure.lang.Agent's own method surface
;; A nil receiver is a NullPointerException before any arm looks: the JVM
;; cannot invoke anything on null. (.toString nil) used to answer "" and
;; (.equals nil 1) false through the universal Object arm.
(define (record-method-dispatch obj method-name rest-args)
  (when (jolt-nil? obj)
    (no-method-throw method-name obj (if (jolt-nil? rest-args) 0 (jolt-count rest-args))))
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

;; ...and the PREDICATE half of the same question, which has to answer for
;; exactly the values the arms above accept. seqable? on the JVM is an instance?
;; test over Seqable / ISeq / Iterable (plus arrays, CharSequence and Map, which
;; jhost-seqable-shim? covers), so a bare deftype or reify declaring one is
;; seqable even though it is not coll?. jolt's seqable? is built out of coll?,
;; so it said false for every such value while `seq` worked on it — including
;; clojure.core.Eduction, the one in core: malli's :every schema tests
;; seqability before it walks, so (m/validate [:every :int] (eduction …)) was
;; false. Reading the same iface-method probes the arms read is what keeps the
;; two answers from drifting apart again.
;; The probe is per METHOD, not jrec-declares-coll-iface?: that name list is the
;; collection-behaviour set (ILookup, Counted, Associative are in it) and none of
;; those is Seqable on the JVM — an ILookup-only deftype is not seqable there.
;; A type declaring Seqable or ISeq has a `seq` method by construction, and a
;; type that is coll? here is already seqable through the predicate this wraps.
;; A bare Iterator (hasNext/next, no iterator method) is NOT in the JVM's set --
;; RT.canSeq names Iterable, never Iterator -- so it stays out here too, even
;; though the cursor arm above lets `seq` walk one; that arm is a jolt superset,
;; and the predicate answers the JVM's question.
(define (iface-seqable? v)
  (and (or (jrec? v) (jreify? v))
       (or (and (iface-method v "seq" #f) #t)
           (and (iface-method v "iterator" #f) #t))))


;; satisfies?: does obj's type implement the protocol? proto is a defprotocol
;; value (a map with a :name). A host Class or interface answers instance?:
;; ported code asks (satisfies? clojure.lang.IObj x) where the shape it means is
;; instance? — the JVM raises on the class form, so jolt answers the question the
;; code means rather than the error. Any other non-protocol throws, with a
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
    ;;
    ;; class-statics-merge! and not register-class-statics!: this runs when USER
    ;; code defines a record, not at boot, and register-class-statics! also
    ;; records the class in host-class-statics-tbl — "the runtime provides this
    ;; class", the table runtime-provides-class? answers from. A record's tag is
    ;; not the runtime's: register-class-provider! runs at deps time, before any
    ;; defrecord exists, so it would happily accept a :jolt/provides claim on
    ;; that name, and the predicate has to agree. Both spellings would be marked
    ;; too, so a bare `Widget` record shadowed every com.acme.Widget.
    (let ((ctor (hashtable-ref class-ctors-tbl tag #f))
          (shape (hashtable-ref chez-record-shapes-tbl
                                (string-append (chez-current-ns) "/->" (symbol-t-name name-sym)) #f)))
      (when (and ctor shape)
        (class-statics-merge! tag
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
;; extends? asks with the name a class token PRESENTS (.getName: the JVM spelling,
;; my_app.core.Foo), and the registry is keyed by the tag (my-app.core.Foo). Map
;; it back here, at the jolt-visible entry only — the Scheme callers above hand
;; over tags already, and this keeps the satisfies? path at its measured cost.
(def-var! "jolt.host" "type-satisfies?"
  (lambda (type-tag proto)
    (type-satisfies? (or (deftype-tag-for-jvm-name type-tag) type-tag) proto)))
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
