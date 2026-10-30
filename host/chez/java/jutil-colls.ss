;; jutil-colls.ss — what makes a mutable java.util collection shim a
;; java.util.Map / Set / List to the rest of the runtime: = against the
;; persistent collections, hash, pr and str, reduce-kv, seqable?. Shared by
;; every target. Each shim registers its tag here with its KIND and a procedure
;; listing its live elements in iteration order; nothing below knows how a shim
;; stores them. host-static-classes.ss registers HashMap / HashSet / the
;; ArrayList family, tree-map.ss the TreeMap and TreeSet views.
;;
;; The JVM answers these questions from the interfaces, not the classes, which
;; is why one registry serves them all:
;;   - APersistentMap/Set/Vector.equiv accept any java.util.Map/Set/List, and a
;;     java.util collection's own equals accepts a persistent one, so (= hm {..})
;;     holds from both sides. Only a List is sequential: an ArrayDeque is a
;;     Collection and compares by identity.
;;   - hash of a non-IHashEq object is its hashCode, the java.util contract
;;     (Map: sum of key^value, Set: sum, List: 31*h+e), NOT the hasheq of the
;;     equal persistent collection.
;;   - pr prints a Map as a map literal, a Set as #{}, a RandomAccess List as
;;     [], any other List as (); print (readably false) and anything else is
;;     #object[class toString]; str is toString — {k=v, …} or [a, b].
;;
;; Needs the jhost record and register-host-methods! (host-static.ss on Chez,
;; host/gambit/host-statics.ss), the arm registries (values.ss, printing.ss,
;; converters.ss) and jolt-java-hashcode (natives-misc.ss). Loads before
;; host-static-classes.ss, which registers into it.

;; ---- the registry -------------------------------------------------------------
;; tag -> #(kind elems). kind is one of
;;   map     java.util.Map; elems answers map entries
;;   set     java.util.Set
;;   entries a Map's entrySet: a Set whose members are map entries, which
;;           hash and print the way Map.Entry does (k^v, k=v)
;;   ralist  a RandomAccess java.util.List (ArrayList, Arrays$ArrayList)
;;   list    any other java.util.List (LinkedList)
;;   coll    a Collection that is none of those (ArrayDeque): str only
;; The Map$Entry host objects below share the table under the kind entry, so
;; =, hash and str each walk ONE arm for every java.util host value — the arm
;; walk is paid by every value no fast path claims (a uuid's hash, an atom's =).
(define jutil-colls-tbl (make-hashtable string-hash string=?))
(define (register-jutil-coll! tag kind elems)
  (hashtable-set! jutil-colls-tbl tag (vector kind elems))
  (register-host-methods! tag (jutil-object-methods kind)))
;; The last tag looked up, as one (tag-object . entry) pair: a pair is published
;; by a single reference write, so a racing reader sees the old pair or the new
;; one, never half of either. A pr or = over a run of the same shim then skips
;; the string hash, which is most of what the arm costs.
(define jutil-tag-cache (cons #f #f))
(define (jutil-tag-entry x)
  (and (jhost? x)
       (let ((tag (jhost-tag x)) (c jutil-tag-cache))
         (if (eq? (car c) tag)
             (cdr c)
             (let ((e (hashtable-ref jutil-colls-tbl tag #f)))
               (set! jutil-tag-cache (cons tag e))
               e)))))
(define (jutil-any-kind x)
  (let ((e (jutil-tag-entry x))) (and e (vector-ref e 0))))
;; the collection kind, or #f — never entry
(define (jutil-coll-kind x)
  (let ((k (jutil-any-kind x))) (and k (not (eq? k 'entry)) k)))
(define (jutil-coll-elems x) ((vector-ref (jutil-tag-entry x) 1) x))
;; a shim the JVM compares by contents (everything but a bare Collection)
(define (jutil-equiv-coll? x)
  (let ((k (jutil-coll-kind x))) (and k (not (eq? k 'coll)))))
;; what the = and hash arms claim: a content-compared shim or an entry
(define (jutil-equiv-value? x)
  (let ((k (jutil-any-kind x))) (and k (not (eq? k 'coll)))))
;; Iterable on the JVM: seq, seqable?, reduce walk it. post-prelude's seqable?
;; consults this.
(define (jhost-seqable-shim? x) (and (jutil-coll-kind x) #t))

;; ---- equality -----------------------------------------------------------------
;; A shim compares as the persistent collection with the same contents, which is
;; exactly the java.util contract the persistent side checks on the JVM (a Map's
;; entries, a Set's members, a List's elements in order). The converted value is
;; never a shim, so the re-dispatch cannot come back here.
(define (jutil-coll->persistent x)
  (case (jutil-coll-kind x)
    ((map) (fold-left (lambda (m e) (pmap-assoc m (jolt-nth e 0) (jolt-nth e 1)))
                      empty-pmap (jutil-coll-elems x)))
    ((set entries) (fold-left pset-conj empty-pset (jutil-coll-elems x)))
    ((ralist list) (apply jolt-vector (jutil-coll-elems x)))
    (else x)))
(define (jutil-plain x) (if (jutil-equiv-coll? x) (jutil-coll->persistent x) x))
;; An IdentityHashMap compares by reference, which the plain copies below would
;; lose: its own rule answers (jutil-concurrent.ss sets this).
(define jutil-identity-map? (lambda (x) #f))
(define (jutil-equals? a b)
  (cond ((or (jutil-identity-map? a) (jutil-identity-map? b)) (jolt=2 a b))
        ((or (eq? (jutil-any-kind a) 'entry) (eq? (jutil-any-kind b) 'entry))
         (jutil-entry-equals? a b))
        ((jolt=2 (jutil-plain a) (jutil-plain b)) #t)
        (else #f)))
(register-eq-arm! (lambda (a b) (or (and (jhost? a) (jutil-equiv-value? a))
                                    (and (jhost? b) (jutil-equiv-value? b))))
                  jutil-equals?)

;; ---- hashCode -----------------------------------------------------------------
(define (jutil-hash-code x)
  (case (jutil-any-kind x)
    ((entry) (jutil-entry-hash x))
    ((map entries) (fold-left (lambda (h e)
                        (i32 (+ h (bitwise-xor (jolt-java-hashcode (jolt-nth e 0))
                                               (jolt-java-hashcode (jolt-nth e 1))))))
                      0 (jutil-coll-elems x)))
    ((set) (fold-left (lambda (h e) (i32 (+ h (jolt-java-hashcode e)))) 0 (jutil-coll-elems x)))
    (else (fold-left (lambda (h e) (i32 (+ (* 31 h) (jolt-java-hashcode e)))) 1
                     (jutil-coll-elems x)))))
;; jhost? is asked inline: this arm sits in the walk every unclaimed value's
;; hash pays (a uuid's, a record's), so a non-host value must leave in one test.
(register-hash-arm! (lambda (x) (and (jhost? x) (jutil-equiv-value? x))) jutil-hash-code)

;; ---- toString -----------------------------------------------------------------
;; String.valueOf of each element: null is "null", a collection holding itself
;; renders "(this Map)" / "(this Collection)" instead of recursing forever.
(define (jutil-elem-string self x)
  (cond ((eq? x self) (if (eq? (jutil-coll-kind self) 'map) "(this Map)" "(this Collection)"))
        ((string? x) x)
        ((jolt-nil? x) "null")
        (else (jolt-str-one x))))
(define (jutil-ek e) (if (pvec? e) (pvec-nth-d e 0 jolt-nil) (jolt-nth e 0)))
(define (jutil-ev e) (if (pvec? e) (pvec-nth-d e 1 jolt-nil) (jolt-nth e 1)))
(define (jutil-entry-string self e)
  (string-append (jutil-elem-string self (jutil-ek e)) "=" (jutil-elem-string self (jutil-ev e))))
(define (jutil-to-string x)
  (let* ((te (jutil-tag-entry x)) (kind (vector-ref te 0)))
    (case kind
      ((entry) (jutil-entry-string-of x))
      ((map) (string-append "{"
               (jolt-str-join-comma (map (lambda (e) (jutil-entry-string x e)) ((vector-ref te 1) x)))
               "}"))
      (else (string-append "["
              (jolt-str-join-comma
                (map (if (eq? kind 'entries)
                         (lambda (e) (jutil-entry-string x e))
                         (lambda (e) (jutil-elem-string x e)))
                     ((vector-ref te 1) x)))
              "]")))))
(register-str-render! jutil-any-kind jutil-to-string)

;; ---- pr -----------------------------------------------------------------------
;; The readable printer's map/set/list shapes, honoring *print-length* and
;; *print-level* like the persistent ones. A java.util.Map does not lift a shared
;; namespace (print-map, not the IPersistentMap method). Printed with
;; *print-readably* off — print / println — the JVM falls back to #object, which
;; is jolt-object-repr over the toString above.
(define (jutil-pr x)
  (if (not (jolt-pr-readable?))
      (jolt-object-repr x #f)
      (let* ((te (jutil-tag-entry x)) (kind (vector-ref te 0)))
        (if (jolt-print-hash?) "#"
            (with-deeper-print
              (let ((xs ((vector-ref te 1) x)))
                (case kind
                  ((map) (string-append "{"
                           (jolt-str-join-comma
                             (jutil-limited-strs xs
                               (lambda (e) (string-append (jolt-pr-readable (jutil-ek e)) " "
                                                          (jolt-pr-readable (jutil-ev e))))))
                           "}"))
                  ((set entries) (string-append "#{" (jolt-str-join (jutil-limited-strs xs jolt-pr-readable)) "}"))
                  ((ralist) (string-append "[" (jolt-str-join (jutil-limited-strs xs jolt-pr-readable)) "]"))
                  (else (string-append "(" (jolt-str-join (jutil-limited-strs xs jolt-pr-readable)) ")")))))))))
;; render at most *print-length* of a list, then "..."
(define (jutil-limited-strs xs render)
  (let ((lim (jolt-print-length)))
    (let loop ((xs xs) (i 0) (acc '()))
      (cond ((null? xs) (reverse acc))
            ((and lim (fx>=? i lim)) (reverse (cons "..." acc)))
            (else (loop (cdr xs) (fx+ i 1) (cons (render (car xs)) acc)))))))
(register-pr-readable-arm! jutil-equiv-coll? jutil-pr)

;; ---- reduce-kv ----------------------------------------------------------------
;; IKVReduce is extended to java.util.Map: (reduce-kv f init a-HashMap) walks its
;; entries (seq.ss consults this hook before refusing the value).
(set! jolt-reduce-kv-entries
  (lambda (x) (and (eq? (jutil-coll-kind x) 'map) (jutil-coll-elems x))))

;; ---- the Object methods every registered shim answers --------------------------
(define (jutil-object-methods kind)
  (cons (cons "toString" (lambda (self) (jutil-to-string self)))
        ;; a Collection that is neither a List, a Set nor a Map keeps Object's
        ;; identity equals and hashCode, as AbstractCollection does
        (if (eq? kind 'coll)
            (list (cons "equals" (lambda (self o) (eq? self o)))
                  (cons "hashCode" (lambda (self) (jolt-identity-hasheq self))))
            (list (cons "equals" (lambda (self o) (jutil-equals? self o)))
                  (cons "hashCode" (lambda (self) (jutil-hash-code self)))))))

;; ---- a proxy over a shim ---------------------------------------------------------
;; A proxy over a java.util collection is that collection to count, seq, get and
;; contains?, which reach it through the methods the JVM's RT does — size,
;; containsKey / contains, get — so an override in the proxy body answers.
(define (proxy-coll-kind x)
  (let ((d (and (jreify? x) (jreify-delegate x))))
    (and d (jutil-coll-kind d))))
(define (proxy-call x m . args) (record-method-dispatch x m (if (null? args) jolt-nil (list->cseq args))))
(register-count-arm! proxy-coll-kind (lambda (x) (jnum->exact (proxy-call x "size"))))
(register-seq-arm! proxy-coll-kind (lambda (x) (jolt-seq (jreify-delegate x))))
(register-get-arm! (lambda (x) (eq? (proxy-coll-kind x) 'map))
  (lambda (x k d) (if (jolt-truthy? (proxy-call x "containsKey" k)) (proxy-call x "get" k) d)))
(register-contains-arm! (lambda (x) (memq (proxy-coll-kind x) '(map set)))
  (lambda (x k) (jolt-truthy? (proxy-call x (if (eq? (proxy-coll-kind x) 'map) "containsKey" "contains") k))))
;; …and to =, hash, str and pr, which on the JVM reach it through the
;; collection's own equals / hashCode / toString and print-method's
;; java.util.Map / Set / List arms (jolt-iad8f). hash and str call the proxy's
;; method, so an override answers; = and pr read the elements the proxy's seq
;; hands out. A Collection that is none of those (ArrayDeque) keeps identity.
(define (proxy-equiv-kind x)
  (let ((k (proxy-coll-kind x))) (and k (not (eq? k 'coll)) k)))
(define (proxy-plain x) (if (proxy-equiv-kind x) (jreify-delegate x) x))
(register-eq-arm-last! (lambda (a b) (or (proxy-equiv-kind a) (proxy-equiv-kind b)))
  (lambda (a b) (jutil-equals? (proxy-plain a) (proxy-plain b))))
(register-hash-arm-last! proxy-equiv-kind
  (lambda (x) (jnum->exact (proxy-call x "hashCode"))))
(register-str-render! proxy-coll-kind (lambda (x) (proxy-call x "toString")))
(register-pr-readable-arm! proxy-equiv-kind
  (lambda (x) (if (jolt-pr-readable?) (jutil-pr (jreify-delegate x)) (jolt-object-repr x #f))))

;; A base whose own methods call one a subclass overrides — LinkedHashMap's put
;; asking removeEldestEntry — is told which proxy stands over it, so that call
;; reaches the override as the JVM's virtual dispatch would. tag -> (base proxy)
(define proxy-attach-hooks '())
(define (proxy-attach-hook! tag f) (set! proxy-attach-hooks (cons (cons tag f) proxy-attach-hooks)))
(define (proxy-attach! base p)
  (when (jhost? base)
    (let ((h (assoc (jhost-tag base) proxy-attach-hooks)))
      (when h ((cdr h) base p)))))

;; ---- modCount ----------------------------------------------------------------
;; A collection that counts structural changes (the ArrayList family, the
;; HashMap / LinkedHashMap / TreeMap families and the sets and views over them)
;; registers how to read the count, so the code they share — an iterator,
;; subList, the spliterators, a stream's source — can watch any of them the same
;; way. A view registers its backing map's count. jutil-modcount answers the
;; count, or #f for a collection that keeps none.
(define jutil-modcount-tbl (make-hashtable string-hash string=?))
(define (register-modcount! tag f) (hashtable-set! jutil-modcount-tbl tag f))
(define (jutil-modcount-reader x)
  (and (jhost? x) (hashtable-ref jutil-modcount-tbl (jhost-tag x) #f)))
(define (jutil-modcount x)
  (let ((f (jutil-modcount-reader x))) (and f (f x))))
(define (jutil-cme!) (throw-jvm 'ConcurrentModificationException jolt-nil))

;; ---- a map's views, made once ---------------------------------------------------
;; A HashMap, TreeMap, LinkedHashMap, EnumMap, … hands out ONE keySet, values and
;; entrySet view and answers it again on every later call (the JDK keeps each in
;; a field), so (identical? (.keySet m) (.keySet m)) and a values view's
;; identity equals hold. (jutil-view m slot make) is the view in SLOT of M,
;; made by MAKE the first time: slot 0 the key set, 1 values, 2 the entry set,
;; 3 the descending map. The table is an ephemeron one, so a view (which holds
;; its map) does not keep the map alive; writes take the mutex, reads do not.
(define jutil-views-tbl (make-ephemeron-eq-hashtable))
(define jutil-views-mu (make-mutex))
(define (jutil-view m slot make)
  (let ((v (hashtable-ref jutil-views-tbl m #f)))
    (or (and v (vector-ref v slot))
        (let ((x (make)))
          (jolt-with-mutex jutil-views-mu
            (let ((v (or (hashtable-ref jutil-views-tbl m #f)
                         (let ((v (make-vector 4 #f))) (hashtable-set! jutil-views-tbl m v) v))))
              (or (vector-ref v slot) (begin (vector-set! v slot x) x))))))))

;; ---- an Iterator whose remove writes through ---------------------------------------
;; (make-jutil-iterator tag items remove! [project]): walks ITEMS, a list taken
;; when the iterator is made; next() answers (project item) and remove() hands
;; the last item to REMOVE!, once — a second remove, or one before any next, is
;; the JDK's IllegalStateException. An item is whatever identifies an element to
;; its collection (an EnumMap's slot index, say). Each tag is one JDK iterator
;; class (classes follow tags); register-jutil-iterator! declares one.
;;
;; (make-jutil-ff-iterator tag items remove! project mcv mci) is the fail-fast one
;; HashMap$HashIterator and LinkedHashMap$LinkedHashIterator are: it remembers
;; the modCount its collection keeps in slot MCI of the vector MCV (the hash
;; table's state, a LinkedHashMap's chain), and next checks it before anything
;; else (then the end),
;; remove checks it after the IllegalStateException and takes the new count
;; after removing. hasNext never checks. The walk is the snapshot ITEMS, which is
;; what the live walk hands out for as long as the count holds, and next throws
;; the first time it does not (tools/failfast-model.pl checks the protocol).
;; State: #(remaining last remove! project mcv mci expected); mcv is #f for an
;; iterator that watches no count.
(define jutil-iterator-none (list 'none))
(define (jutil-identity x) x)
(define (make-jutil-iterator tag items remove! . project)
  (make-jhost tag (vector items jutil-iterator-none remove!
                          (if (pair? project) (jutil-project-slot (car project)) #f) #f 0 0)))
(define (make-jutil-ff-iterator tag items remove! project mcv mci)
  (make-jhost tag (vector items jutil-iterator-none remove! (jutil-project-slot project) mcv mci (vector-ref mcv mci))))
;; slot 3 holds #f for the identity projection, which next then skips calling
(define (jutil-project-slot p) (if (eq? p jutil-identity) #f p))
(define-syntax jutil-project
  (syntax-rules ()
    ((_ st x) (let ((p (sa-uvector-ref st 3))) (if p (p x) x)))))
;; is the count still the one expected (always, for one that watches none);
;; inline, as next pays it per element. Unchecked reads (sa-uvector-ref): ST is
;; the 7-slot vector the two constructors above build, and slot MCI of MCV is
;; the count's slot in a state its collection's constructor sized for it (2 of
;; a HashMap / HashSet state, 5 of a LinkedHashMap chain).
(define-syntax jutil-iterator-current?
  (syntax-rules ()
    ((_ st) (let ((v (sa-uvector-ref st 4)))
              (or (not v) (fx=? (sa-uvector-ref st 6) (sa-uvector-ref v (sa-uvector-ref st 5))))))))
(define (jutil-iterator-next self)
  (let* ((st (jhost-state self)) (xs (sa-uvector-ref st 0)))
    (unless (jutil-iterator-current? st) (jutil-cme!))
    (when (null? xs) (throw-jvm 'NoSuchElementException jolt-nil))
    (let ((x (car xs)))
      (sa-uvector-set! st 0 (cdr xs))
      (sa-uvector-set! st 1 x)
      (jutil-project st x))))
(define jutil-iterator-methods
  (list (cons "hasNext" (lambda (self) (pair? (vector-ref (jhost-state self) 0))))
        (cons "next" jutil-iterator-next)
        (cons "remove" (lambda (self)
                         (let* ((st (jhost-state self)) (x (vector-ref st 1)))
                           (when (eq? x jutil-iterator-none) (throw-jvm 'IllegalStateException jolt-nil))
                           (unless (jutil-iterator-current? st) (jutil-cme!))
                           (vector-set! st 1 jutil-iterator-none)
                           ((vector-ref st 2) x)
                           (let ((v (vector-ref st 4)))
                             (when v (vector-set! st 6 (vector-ref v (vector-ref st 5)))))
                           jolt-nil)))
        ;; Iterator's default: hasNext and next until the end, so a change the
        ;; action makes is caught at the next element
        (cons "forEachRemaining" (lambda (self f)
                                   (when (jolt-nil? f) (throw-jvm 'NullPointerException jolt-nil))
                                   (let loop ()
                                     (when (pair? (vector-ref (jhost-state self) 0))
                                       (jolt-fi-call f "accept" (jutil-iterator-next self))
                                       (loop)))
                                   jolt-nil))))
(define jutil-iterator-tags '())
(define (jutil-iterator? x) (and (jhost? x) (member (jhost-tag x) jutil-iterator-tags) #t))
;; iterator-seq's chunk: up to 32 nexts at once; a count that moved is the
;; loop's to report
(define (jutil-iterator-fill it buf)
  (let* ((st (jhost-state it)) (project (or (vector-ref st 3) jutil-identity)))
    (and (jutil-iterator-current? st)
         (let loop ((j 0) (xs (vector-ref st 0)) (last (vector-ref st 1)))
           (if (or (fx=? j 32) (null? xs))
               (begin (vector-set! st 0 xs) (vector-set! st 1 last) j)
               (begin (vector-set! buf j (project (car xs)))
                      (loop (fx+ j 1) (cdr xs) (car xs))))))))
(define (register-jutil-iterator! tag)
  (set! jutil-iterator-tags (cons tag jutil-iterator-tags))
  (register-host-methods! tag jutil-iterator-methods)
  (register-iterator-seq-fill! (list tag) jutil-iterator-fill jutil-iterator-next))
;; (seq it): what the iterator has not handed out yet
(register-seq-arm! jutil-iterator?
  (lambda (it)
    (let ((st (jhost-state it)))
      (if (null? (vector-ref st 0)) jolt-nil (list->cseq (map (or (vector-ref st 3) jutil-identity) (vector-ref st 0)))))))

;; (jutil-ff-seq coll xs): (seq coll) over XS, the elements COLL's fail-fast
;; iterator hands out, 32 at a time as RT.chunkIteratorSeq realizes them: the
;; first chunk now (a jolt seq is realized at its head), each later one when the
;; walk reaches it — a ConcurrentModificationException then if COLL's modCount
;; has moved since the seq was made, as the iterator's next would raise.
(define (jutil-ff-seq coll xs) (jutil-ff-seq/mc coll xs (jutil-modcount-reader coll)))
;; …with the count's reader MC (#f for none) supplied by a caller that knows it
(define (jutil-ff-seq/mc coll xs mc)
  (let ((e (and mc (mc coll))))
    ;; 32 cells; the 32nd's tail, when more follow, is read when the walk asks
    ;; for it. Plain cells: (seq coll) is no chunked seq on the JVM (it is the
    ;; LazySeq over RT.chunkIteratorSeq).
    (define (chunk xs)
      (let loop ((xs xs) (i 1))
        (let ((rest (cdr xs)))
          (cond ((null? rest) (cseq-realized (car xs) jolt-nil))
                ((fx=? i 32)
                 (cseq-lazy (car xs)
                            (lambda ()
                              (when (and mc (not (eqv? e (mc coll)))) (jutil-cme!))
                              (chunk rest))))
                (else (cseq-realized (car xs) (loop rest (fx+ i 1))))))))
    (if (null? xs) jolt-nil (chunk xs))))

;; ---- the default Collection walks over a fail-fast iterator ----------------------
;; Iterable.forEach and Collection.removeIf as the JDK's defaults run them: one
;; iterator, hasNext / next per element, removeIf removing through it. A change
;; the action makes is the next next()'s ConcurrentModificationException.
(define (jutil-walk-for-each it f)
  (when (jolt-nil? f) (throw-jvm 'NullPointerException jolt-nil))
  (let ((next (host-method-ref (jhost-tag it) "next")) (has? (host-method-ref (jhost-tag it) "hasNext")))
    (let loop () (when (has? it) (jolt-fi-call f "accept" (next it)) (loop))))
  jolt-nil)
(define (jutil-walk-remove-if it pred)
  (when (jolt-nil? pred) (throw-jvm 'NullPointerException jolt-nil))
  (let ((next (host-method-ref (jhost-tag it) "next")) (has? (host-method-ref (jhost-tag it) "hasNext"))
        (rm (host-method-ref (jhost-tag it) "remove")))
    (let loop ((removed #f))
      (if (has? it)
          (if (jolt-truthy? (jolt-fi-call pred "test" (next it)))
              (begin (rm it) (loop #t))
              (loop removed))
          removed))))

;; ---- java.util.Map$Entry host objects ----------------------------------------------
;; An entry a java.util shim hands out (TreeMap$Entry, SimpleImmutableEntry)
;; registers its tag with a procedure answering its (key . value). That is what
;; makes it a Map.Entry to the rest of the runtime: map-entry?, key / val, nth
;; and destructuring, count, conj onto a map (collections.ss jolt-host-entry),
;; = against another Map.Entry by key and value (and never against a vector:
;; a MapEntry's equiv wants a List), hash as Map.Entry.hashCode (k ^ v), and
;; str as k=v. pr is the #object form over that toString, as on the JVM.
(define (register-jutil-entry! tag kv)
  (hashtable-set! jutil-colls-tbl tag (vector 'entry kv))
  (register-host-methods! tag
    (list (cons "getKey" (lambda (self) (car (kv self))))
          (cons "getValue" (lambda (self) (cdr (kv self))))
          (cons "toString" (lambda (self) (jutil-entry-string-of self)))
          (cons "equals" (lambda (self o) (jutil-entry-equals? self o)))
          (cons "hashCode" (lambda (self) (jutil-entry-hash self))))))
(define (jutil-entry? x) (eq? (jutil-any-kind x) 'entry))
(define (jutil-entry-kv x)
  (let ((e (jutil-tag-entry x)))
    (and e (eq? (vector-ref e 0) 'entry) ((vector-ref e 1) x))))
(set! jolt-host-entry jutil-entry-kv)
(define (jutil-entry-string-of x)
  (let ((kv (jutil-entry-kv x)))
    (string-append (jutil-elem-string x (car kv)) "=" (jutil-elem-string x (cdr kv)))))
(define (jutil-entry-equals? a b)
  (let ((ka (jutil-entry-kv a)) (kb (jutil-entry-kv b)))
    (and ka kb (jolt=2 (car ka) (car kb)) (jolt=2 (cdr ka) (cdr kb)) #t)))
(define (jutil-entry-hash x)
  (let ((kv (jutil-entry-kv x)))
    (i32 (bitwise-xor (jolt-java-hashcode (car kv)) (jolt-java-hashcode (cdr kv))))))
(register-count-arm! jutil-entry? (lambda (x) 2))
;; map-entry? is (instance? java.util.Map$Entry x) on the JVM
(def-var! "clojure.core" "map-entry?"
  (lambda (x) (or (jolt-map-entry? x) (jutil-entry? x))))

;; ---- host Comparator objects -------------------------------------------------------
;; The comparator seam (natives-seq.ss jolt-comparator-fn) asks whether a value
;; is a shim object whose tag registers a `compare` method — a Comparator held
;; by the host (String/CASE_INSENSITIVE_ORDER, Comparator/reverseOrder) rather
;; than by a deftype/reify. Both targets carry jhost, so both answer it.
(set! jhost-compare-method?
  (lambda (x)
    (and (jhost? x) (host-method-ref (jhost-tag x) "compare") #t)))
