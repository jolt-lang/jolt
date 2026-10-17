;; dot-forms.ss — generic dispatch for the `.` special-form / `.-field` desugar.
;; The analyzer lowers (. target member arg*) and (.-field target)
;; to a :host-call; the Chez emit routes a non-shimmed :host-call through
;; record-method-dispatch. This file extends that dispatcher with the collection
;; arms the interpreter's dispatch-member covers but the record/string base does
;; not, with this precedence:
;;
;;   * collection interop wins first — count/seq/nth/get/valAt/containsKey on a
;;     vector/map/set/seq/record (so (. {:count 9} count) is the entry count, 1,
;;     NOT the :count field).
;;   * field access — a "-name" member with NO arguments reads the field (records
;;                    and maps); with arguments it is a call of the member named
;;                    "-name", which is the base dispatcher's (a dashed protocol method).
;;   * map member   — a stored fn is a method (called with self + args); any
;;                    other value is returned as a field.
;;
;; Anything not recognized falls through to the previous dispatcher (jhost /
;; number / regex / jrec protocol / string). Loaded after the jhost record and
;; its method registry (host-static.ss on Chez, host-statics.ss on Gambit —
;; this file is shared with the Gambit boot). A record (jrec) is jolt-map? here
;; (records.ss makes it so) and a collection, so its protocol method (no dash,
;; not a coll method) lands in the base.

;; Vectors / maps / sets only (records are jolt-map? here). Raw seqs are excluded:
;; coll-interop accepts some seq representations and not others (a
;; plain (seq v) returns nil from .count, a lazy-seq returns the count), an
;; inconsistency Chez's normalized cseq can't mirror — so a raw seq target falls
;; through to the base dispatcher rather than risk a divergence the corpus would
;; never exercise but a future case might.
(define (dot-coll? obj)
  (or (jolt-vector? obj) (jolt-map? obj) (pset? obj)))

;; (jolt-java-hashcode — Java .hashCode() for a collection — lives in
;; natives-misc.ss beside the hash API, where hash-combine reads it;
;; dot-coll-method — the java.util.Map/Collection/List surface of a collection
;; — in records-dispatch.ss, where record-method-dispatch reaches it for a
;; deftype built on the clojure.lang interfaces.)

;; Universal object-methods: on a
;; non-record map these win OVER a field lookup, like dispatch-member. getMessage
;; on an ex-info reads its :message (the one the corpus exercises); getCause reads
;; :cause; toString/hashCode/equals round out the set. Returns a boxed result or
;; #f. Strings/numbers/records/jhost keep the base dispatcher (it shims them).
(define (dot-object-method obj name args)
  (cond
    ;; A throwable answers the full java.lang.Throwable surface from the one shared
    ;; table (records-interop.ss) — the same one the raw-condition path uses.
    ((and (jolt-ex-info-record? obj) (throwable-method obj name args)) => values)
    ;; getMessage/toString on a plain map: legacy exception-as-map interop, kept
    ;; because a non-record map reaches this table too.
    ((string=? name "getMessage") (list (jolt-str-render-one obj)))
    ((string=? name "getCause")  (list jolt-nil))
    ((string=? name "getNextException") (list jolt-nil))
    ((string=? name "getStackTrace") (list (jolt-vector)))
    ((string=? name "toString")  (list (jolt-str-render-one obj)))
    ((string=? name "hashCode")  (list (jolt-hash obj)))
    ((string=? name "equals")    (list (if (jolt= obj (car args)) #t #f)))
    (else #f)))

;; The record class's own two public fields on the JVM, boxed, or #f: __extmap
;; is the extension keys (nil when there are none, as a dissoc back to the
;; declared fields leaves it), __meta the metadata. Read by code that rebuilds
;; a record without the positional factory (typed.clojure's update-expr), as
;; (.-__extmap e) or the reflector's (. e __extmap), which tries a method first
;; and then the field.
(define (jrec-class-field obj mname)
  (and (jrec-record? obj)
       (cond ((string=? mname "__extmap")
              (let ((ext (jrec-ext obj)))
                (list (if (or (jolt-nil? ext) (fx=? 0 (jolt-count ext))) jolt-nil ext))))
             ((string=? mname "__meta") (list (jolt-meta obj)))
             (else #f))))

;; clojure.lang.Sorted on jolt's sorted-map / sorted-set: comparator / entryKey /
;; seqFrom / seq. data.priority-map's subseq/rsubseq reach for these (its
;; PersistentPriorityMap delegates .comparator to the backing sorted-map). The
;; comparator is returned as a small Comparator object whose .compare runs the
;; map's 3-way fn, since (.. sc comparator (compare a b)) is the calling form.
(define sorted-cmp-kw (keyword #f "cmp"))
(register-host-methods! "jolt-comparator"
  (list (cons "compare" (lambda (self a b) (jolt-invoke (jhost-state self) a b)))))
(define (sorted-comparator-of sc)
  (let ((c (jolt-ref-get sc sorted-cmp-kw)))
    (make-jhost "jolt-comparator" (if (jolt-nil? c) jolt-compare c))))
(define (sorted-iface-method? m)
  (or (string=? m "comparator") (string=? m "entryKey")
      (string=? m "seqFrom") (string=? m "seq")))
(define (sorted-iface-dispatch obj method rest)
  (cond
    ((string=? method "comparator") (sorted-comparator-of obj))
    ((string=? method "entryKey") (jolt-first (car rest)))   ; map entry -> its key
    ((string=? method "seq")                                 ; (.seq sc) or (.seq sc ascending?)
     (if (or (null? rest) (jolt-truthy? (car rest))) (jolt-seq obj) (jolt-rseq obj)))
    ;; (.seqFrom sc k ascending?) — the entries (a set's elements) from k onward, in
    ;; order: the tree walk seeked to k in O(log n), as PersistentTreeMap.seqFrom.
    ((string=? method "seqFrom")
     (let ((cmp (jolt-ref-get obj sorted-cmp-kw)))
       (sc-tree-seq-from (jolt-ref-get obj kw-tree) (jolt-truthy? (cadr rest))
                         (if (htable-sorted-map? obj) 0 1)
                         (if (jolt-nil? cmp) jolt-compare cmp) (car rest))))
    (else (dispatch-miss obj method rest))))

;; The receivers the arm below answers for — every branch of its cond tests
;; one of these (a field read tests the receiver inside its own branch). The
;; arm sits at priority 30, so every receiver the string/getclass arms did not
;; claim passes through it: a Matcher, a StringBuilder, a File, a Date. It used
;; to convert the rest args to a list BEFORE the cond — a seq walk, a cseq plus
;; a cons per argument — so every one of those receivers paid for a conversion
;; whose result the arm then threw away with 'pass (448 bytes of the 576 an
;; unhinted (.region m a b) allocated in dispatch). Test the receiver first, and
;; convert with method-rest-args->list, which reads the argument vector's tail
;; straight into conses. test/chez/method-dispatch-alloc-test.ss pins it.
(define (dotform-receiver? obj)
  (or (jrec? obj) (jolt-multifn? obj) (procedure? obj) (jolt-transient? obj)
      (dot-coll? obj) (htable-sorted? obj) (jolt-map? obj) (jolt-ex-info-record? obj)))
(register-method-arm! arm-priority-dotform
  (lambda (obj method-name rest-args)
    (let* ((dashed? (and (> (string-length method-name) 0)
                         (char=? (string-ref method-name 0) #\-)))
           ;; A dashed name reads a FIELD only with no arguments. With arguments
           ;; it is a call of the member named "-name" — the analyzer's rule and
           ;; the JVM's, which munges it to _name and looks for a method — so it
           ;; is not this arm's at all: it passes with its dash intact, for the
           ;; base's protocol-method lookup (a deftype's own -go, which is how a
           ;; dash-named protocol method is invoked through interop) or the
           ;; end-of-chain method miss. A declared slot used to answer a
           ;; (.-fld x 1) with the argument silently dropped.
           (field? (and dashed? (method-rest-args-empty? rest-args)))
           (mname (if field?
                      (substring method-name 1 (string-length method-name))
                      method-name)))
     (if (or (and dashed? (not field?))
             (not (or field? (dotform-receiver? obj))))
      'pass
      (let ((rest (method-rest-args->list rest-args)))
      (cond
        ;; A FIELD read. Checked FIRST, so no method arm below can claim a dashed
        ;; name — (.-count [1 2]) is not the count, the way the JVM's
        ;; Reflector.getInstanceField is not the method surface.
        ;;
        ;; What answers is a declared deftype/defrecord slot, plus the map-as-object
        ;; read this file has always supported (see the precedence note above): a
        ;; key the map HAS reads as a field. What does not answer is the case that
        ;; used to return a silent nil — an undeclared record slot, a key the map
        ;; does not have, a string/vector/set with no field concept at all. Those
        ;; pass, and the end of the chain (no-method-throw) reads the leading dash
        ;; back off to raise "No matching field found". A nil there was the bad
        ;; shape: it reads as a field that is present and nil, so a caller testing
        ;; it took the wrong branch instead of failing.
        (field?
         (let ((kw (keyword #f mname)))
           (cond
             ((jrec? obj)
              (cond ((jrec-member-field obj kw) => (lambda (k) (jrec-lookup obj k jolt-nil)))
                    ((jrec-class-field obj mname) => car)
                    (else 'pass)))
             ((and (jolt-map? obj) (jolt-truthy? (jolt-contains? obj kw)))
              (jolt-get obj kw jolt-nil))
             (else 'pass))))
        ;; (. rec __extmap) with no args: no method by that name, then the field
        ((and (null? rest) (jrec? obj) (jrec-class-field obj mname)) => car)
        ;; (.invoke f a ...) and (.applyTo f args): IFn's own methods, for the
        ;; IFn receivers this arm admits (fns, multimethods, maps, sets,
        ;; vectors). A deftype that forwards through a ^clojure.lang.IFn field
        ;; calls it this way (yaml-parser's Rule); clojure.spec's instrument
        ;; uses applyTo. Ahead of the map-as-object read below, which answered
        ;; (.applyTo {:a 1} '(:a)) with nil. A deftype's own invoke is answered
        ;; further down.
        ((and (or (string=? mname "invoke")
                  (and (string=? mname "applyTo") (pair? rest) (null? (cdr rest))))
              (or (procedure? obj) (jolt-multifn? obj) (jolt-map? obj) (jolt-set? obj)
                  (jolt-vector? obj)))
         (if (string=? mname "invoke")
             (apply jolt-invoke obj rest)
             (apply jolt-invoke obj (seq->list (jolt-seq (car rest))))))
        ;; clojure.lang.MultiFn .dispatchFn / .getMethod — clojure.spec.alpha's
        ;; multi-spec walks a multimethod through these.
        ((jolt-multifn? obj)
         (cond
           ((string=? mname "dispatchFn") (jolt-multifn-dispatch-fn obj))
           ((string=? mname "getMethod")
            (let ((methods (jolt-multifn-methods obj)) (dv (car rest)))
              (or (hashtable-ref methods dv #f)
                  (mm-find-isa obj dv)
                  (hashtable-ref methods (jolt-multifn-default obj) #f)
                  jolt-nil)))
           (else 'pass)))
        ;; (.compare f a b): a fn is a java.util.Comparator — AFunction.compare
        ;; invokes it — so code holding a fn as a Comparator calls it this way.
        ((and (procedure? obj) (string=? mname "compare") (pair? rest) (pair? (cdr rest)))
         (jolt-invoke obj (car rest) (cadr rest)))
        ;; (.run f) / (.call f): a fn is a Runnable and a Callable — AFn.run and
        ;; AFn.call invoke it with no arguments — so an Executor handed a fn as
        ;; its task runs it this way. run answers nil, call the fn's value.
        ((and (procedure? obj) (null? rest) (string=? mname "run"))
         (jolt-invoke obj) jolt-nil)
        ((and (procedure? obj) (null? rest) (string=? mname "call"))
         (jolt-invoke obj))
        ;; a transient (ITransientCollection/Set/Map): .contains / .valAt / .count —
        ;; test.check's distinct-collection gen uses (.contains transient-set k).
        ((jolt-transient? obj)
         (cond
           ((string=? mname "contains") (if (jolt-truthy? (t-contains? obj (car rest))) #t #f))
           ((or (string=? mname "valAt") (string=? mname "get"))
            (t-get obj (car rest) (if (null? (cdr rest)) jolt-nil (cadr rest))))
           ((string=? mname "count") (t-count obj))
           ;; ITransient{Collection,Vector,Map,Set} mutators — a deftype built on
           ;; the clojure.lang transient interfaces calls these on its native
           ;; transient backing (flatland.ordered's TransientOrderedMap/Set).
           ((string=? mname "conj") (apply jolt-conj! obj rest))
           ((or (string=? mname "assoc") (string=? mname "assocN"))
            (jolt-assoc! obj (car rest) (cadr rest)))
           ((string=? mname "without") (jolt-dissoc! obj (car rest)))
           ((string=? mname "disjoin") (jolt-disj! obj (car rest)))
           ((string=? mname "pop") (jolt-pop! obj))
           ((string=? mname "persistent") (jolt-persistent! obj))
           (else 'pass)))
        ;; a deftype/record's OWN declared method (matched by name AND arity) wins
        ;; over the generic collection interop below — e.g. data.priority-map
        ;; declares both seq[this] (Seqable) and seq[this ascending] (Sorted), and
        ;; (.seq pm false) must reach the 2-arg one, not dot-coll's plain seq.
        ((and (jrec? obj)
              (jrec-method-arity obj mname (+ 1 (length rest))))
         => (lambda (f) (invoke-declared-method f obj mname rest)))
        ;; collection interop first (entry count / seq / nth / get / containsKey).
        ((and (dot-coll? obj) (dot-coll-method obj mname rest))
         => (lambda (box) (car box)))
        ;; a sorted set is a java.util.Set: Set.hashCode, not its hasheq (a
        ;; sorted map is a dot-coll and gets Map.hashCode above)
        ((and (htable-sorted? obj) (string=? mname "hashCode") (null? rest))
         (jolt-java-hashcode obj))
        ;; clojure.lang.Sorted (comparator / entryKey / seqFrom) on a sorted
        ;; map/set, before the map arm below reads the method name as a key.
        ;; data.priority-map's subseq/rsubseq reach for these.
        ((and (htable-sorted? obj) (sorted-iface-method? mname))
         (sorted-iface-dispatch obj mname rest))
        ;; non-record map: a universal object-method (getMessage/...) wins first,
        ;; then a stored procedure is a method (call with self), else the field.
        ((and (jolt-map? obj) (not (jrec? obj)))
         (cond
           ((dot-object-method obj mname rest) => car)
           (else
            (let ((v (jolt-get obj (keyword #f mname) jolt-nil)))
              (if (procedure? v) (apply jolt-invoke v obj rest) v)))))
        ;; ex-info record: universal object-methods (getMessage/getCause/toString/...)
        ;; only — NO field lookup (ExceptionInfo is not ILookup on the JVM).
        ((jolt-ex-info-record? obj)
         (cond
           ((dot-object-method obj mname rest) => car)
           (else 'pass)))
        (else 'pass)))))))
