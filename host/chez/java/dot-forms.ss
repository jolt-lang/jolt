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
;;   * field access — a "-name" member reads the field (records and maps).
;;   * map member   — a stored fn is a method (called with self + args); any
;;                    other value is returned as a field.
;;
;; Anything not recognized falls through to the previous dispatcher (jhost /
;; number / regex / jrec protocol / string). Loaded LAST (after host-static.ss).
;; A record (jrec) is jolt-map? here (records.ss makes it so) and a collection,
;; so its protocol method (no dash, not a coll method) lands in the base.

;; Vectors / maps / sets only (records are jolt-map? here). Raw seqs are excluded:
;; coll-interop accepts some seq representations and not others (a
;; plain (seq v) returns nil from .count, a lazy-seq returns the count), an
;; inconsistency Chez's normalized cseq can't mirror — so a raw seq target falls
;; through to the base dispatcher rather than risk a divergence the corpus would
;; never exercise but a future case might.
(define (dot-coll? obj)
  (or (jolt-vector? obj) (jolt-map? obj) (pset? obj)))

;; Java .hashCode() for a collection (java.util.Map/Set/List semantics), NOT the
;; Murmur3 hasheq that clojure.core/hash uses. A library computing .hashCode on its
;; own collection type (flatland's OrderedMap via APersistentMap/mapHash, OrderedSet
;; summing element .hashCodes) must agree with jolt's builtins, so map/set/vector
;; .hashCode go here. Recursive: a nested collection element hashes the same way; a
;; scalar routes to its own .hashCode. Sums use exact ints (jolt + is unbounded, as
;; a Clojure (reduce + …) over element hashCodes is) except the map form, which
;; mirrors APersistentMap.mapHash's 32-bit int accumulation.
(define (jolt-java-hashcode x)
  (cond
    ((jolt-nil? x) 0)
    ((pmap? x)
     (pmap-fold x (lambda (k v a)
                    (i32 (+ a (bitwise-xor (jolt-java-hashcode k) (jolt-java-hashcode v))))) 0))
    ((pset? x)
     (pset-fold x (lambda (e a) (if (jolt-nil? e) a (+ a (jolt-java-hashcode e)))) 0))
    ((pvec? x)
     (let ((n (pvec-count x)))
       (let loop ((i 0) (h 1))
         (if (fx>=? i n) h
             (loop (fx+ i 1) (i32 (+ (* 31 h) (jolt-java-hashcode (pvec-nth-d x i jolt-nil)))))))))
    ((or (cseq? x) (empty-list-t? x) (jolt-lazyseq? x))
     (let loop ((s (jolt-seq x)) (h 1))
       (if (jolt-nil? s) h
           (loop (jolt-seq (seq-more s)) (i32 (+ (* 31 h) (jolt-java-hashcode (seq-first s))))))))
    ;; a jrec is jolt-map? (so dot-coll?) — route it here directly, NOT through
    ;; record-method-dispatch, which would re-enter the .hashCode arm and loop. A
    ;; declared hashCode governs (flatland's types via APersistentMap/mapHash);
    ;; else the structural record hash.
    ((jrec? x) (let ((m (find-method-any-protocol (jrec-tag x) "hashCode")))
                 (if m (jolt-invoke m x) (jrec-hash x))))
    (else (record-method-dispatch x "hashCode" jolt-nil))))
(def-var! "jolt.host" "java-hashcode" jolt-java-hashcode)

;; Mirror coll-interop: return a one-element list boxing the result (so a jolt-nil
;; result is still distinguishable from "not a collection method"), or #f.
(define (dot-coll-method obj name args)
  (cond
    ((string=? name "count") (list (jolt-count obj)))
    ((string=? name "seq")   (list (jolt-seq obj)))
    ((string=? name "nth")   (list (apply jolt-nth obj args)))
    ((or (string=? name "get") (string=? name "valAt"))
     (list (apply jolt-get obj args)))
    ((string=? name "containsKey") (list (jolt-contains? obj (car args))))
    ;; java.util.Collection.contains(o): VALUE membership (a set is O(1) via
    ;; contains?; a list/vector/seq is a linear scan — contains? on a vector tests
    ;; an index, so it is wrong here).
    ((string=? name "contains")
     (list (if (pset? obj)
               (jolt-contains? obj (car args))
               (let ((x (car args)))
                 (let loop ((s (jolt-seq obj)))
                   (cond ((jolt-nil? s) #f)
                         ((jolt=2 (seq-first s) x) #t)
                         (else (loop (jolt-seq (seq-more s))))))))))
    ((string=? name "size")    (list (jolt-count obj)))
    ((string=? name "isEmpty") (list (jolt-empty? obj)))
    ;; java.util.{Map,Set,List}.hashCode — the Java collection hashCode, so a
    ;; jolt builtin matches a library's own type computing the same (flatland).
    ((string=? name "hashCode") (list (jolt-java-hashcode obj)))
    ;; IPersistentCollection / Associative / IPersistentVector / IPersistentMap /
    ;; IPersistentSet mutators — a deftype built on the clojure.lang interfaces
    ;; (e.g. flatland.ordered) calls these directly on its native backing
    ;; map/vector/set. Each maps to the persistent op of the same meaning.
    ((string=? name "cons")    (list (jolt-conj obj (car args))))
    ((or (string=? name "assoc") (string=? name "assocN"))
     (list (jolt-assoc obj (car args) (cadr args))))
    ((string=? name "without") (list (jolt-dissoc obj (car args))))
    ((string=? name "disjoin") (list (jolt-disj obj (car args))))
    ((string=? name "pop")     (list (jolt-pop obj)))
    ((string=? name "peek")    (list (jolt-peek obj)))
    ((string=? name "equiv")   (list (if (jolt= obj (car args)) #t #f)))
    ;; IEditableCollection.asTransient — hand back a transient over this coll.
    ((string=? name "asTransient") (list (jolt-transient-new obj)))
    ;; IObj — meta / withMeta thread metadata through the backing coll.
    ((string=? name "meta")    (list (jolt-meta obj)))
    ((string=? name "withMeta") (list (jolt-with-meta obj (car args))))
    ;; MapEntry.key/val/getKey/getValue on a 2-elem entry (a flagged pvec).
    ((and (jolt-map-entry? obj) (or (string=? name "key") (string=? name "getKey")))
     (list (jolt-nth obj 0)))
    ((and (jolt-map-entry? obj) (or (string=? name "val") (string=? name "getValue")))
     (list (jolt-nth obj 1)))
    ;; java.util.Map views: keySet (a Set), values (a Collection), entrySet.
    ((and (jolt-map? obj) (string=? name "keySet"))
     (list (apply jolt-hash-set (seq->list (jolt-keys obj)))))
    ((and (jolt-map? obj) (string=? name "values"))
     (list (apply jolt-vector (seq->list (jolt-vals obj)))))
    ((and (jolt-map? obj) (string=? name "entrySet")) (list (jolt-seq obj)))
    ;; (.iterator coll): a java.util.Iterator over the seq — for a map this is the
    ;; entry iterator. Without this a map's .iterator falls into the map-as-object
    ;; branch and is mis-read as a missing :iterator key (nil). Some libraries
    ;; (e.g. malli's -vmap) iterate a map this way.
    ((string=? name "iterator") (list (make-jiterator (jolt-seq obj))))
    ;; (.reduce coll f) / (.reduce coll f init): clojure.lang.IReduce — every
    ;; persistent collection reduces itself on the JVM.
    ((string=? name "reduce")
     (list (if (pair? (cdr args))
               (jolt-reduce (car args) (cadr args) obj)
               (jolt-reduce (car args) obj))))
    (else #f)))

;; Universal object-methods: on a
;; non-record map these win OVER a field lookup, like dispatch-member. getMessage
;; on an ex-info reads its :message (the one the corpus exercises); getCause reads
;; :cause; toString/hashCode/equals round out the set. Returns a boxed result or
;; #f. Strings/numbers/records/jhost keep the base dispatcher (it shims them).
(define (dot-object-method obj name args)
  (cond
    ((string=? name "getMessage")
     (list (if (jolt-ex-info-record? obj)
               (jolt-ex-info-record-message obj)
               (jolt-str-render-one obj))))
    ((string=? name "getCause")  (list (if (jolt-ex-info-record? obj) (jolt-ex-info-record-cause obj) jolt-nil)))
    ;; java.text.ParseException.getErrorOffset — the int offset stashed by its ctor.
    ((string=? name "getErrorOffset") (list (if (jolt-ex-info-record? obj) (jolt-ex-info-record-error-offset obj) 0)))
    ;; java.sql.SQLException chaining — ex-info / host throwables don't chain.
    ((string=? name "getNextException") (list jolt-nil))
    ((string=? name "getStackTrace") (list (jolt-vector)))
    ((string=? name "toString")  (list (jolt-str-render-one obj)))
    ((string=? name "hashCode")  (list (jolt-hash obj)))
    ((string=? name "equals")    (list (if (jolt= obj (car args)) #t #f)))
    (else #f)))

(register-method-arm! arm-priority-dotform
  (lambda (obj method-name rest-args)
    (let* ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args)))
           (field? (and (> (string-length method-name) 0)
                        (char=? (string-ref method-name 0) #\-)))
           (mname (if field?
                      (substring method-name 1 (string-length method-name))
                      method-name)))
      (cond
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
        ;; (.applyTo f args): apply a fn to a seq of args (clojure.spec instrument).
        ((and (procedure? obj) (string=? mname "applyTo"))
         (apply jolt-invoke obj (seq->list (jolt-seq (car rest)))))
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
        ((and (not field?) (jrec? obj)
              (find-method-any-protocol-arity (jrec-tag obj) mname (+ 1 (length rest))))
         => (lambda (f) (apply jolt-invoke f obj rest)))
        ;; collection interop first (entry count / seq / nth / get / containsKey).
        ((and (dot-coll? obj) (dot-coll-method obj mname rest))
         => (lambda (box) (car box)))
        ;; clojure.lang.Sorted (comparator / entryKey / seqFrom) on a sorted
        ;; map/set, before the map arm below reads the method name as a key.
        ;; data.priority-map's subseq/rsubseq reach for these.
        ((and (not field?) (htable-sorted? obj) (sorted-iface-method? mname))
         (sorted-iface-dispatch obj mname rest))
        ;; (.-field obj) / (. obj -field): field read on a record or map.
        (field? (jolt-get obj (keyword #f mname) jolt-nil))
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
        (else 'pass)))))
