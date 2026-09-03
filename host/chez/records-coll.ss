;; records-coll.ss — the jrec arms on the collection dispatchers: equality/hash,
;; count/contains?/seq/conj/assoc/dissoc/keys/vals/nth/peek/pop, plus the
;; per-descriptor derived-interface cache (jrdesc-ifc-of) they read.
;;
;; Loaded after records.ss (the jrec layout) and before protocols.ss /
;; records-dispatch.ss. Form order across the four records files is
;; load-bearing: arms dispatch newest-first, so registration order here is
;; dispatch precedence.

;; ---- extend the collection dispatchers with a jrec arm ----------------------
;; equality for a jrec: a deftype implementing IPersistentCollection/equiv (e.g.
;; core.cache's caches, which equiv to their backing map) compares through that
;; method, so (= cache {…}) works; a plain record has no equiv and falls back to
;; field-wise jrec=? (and a record is never = a plain map).
(register-eq-arm! (lambda (a b) (or (jrec? a) (jrec? b)))
                  (lambda (a b)
                    (cond ((and (jrec? a) (jrec-cl a "equiv")) => (lambda (m) (if (jolt-truthy? (jolt-invoke m a b)) #t #f)))
                          ((and (jrec? b) (jrec-cl b "equiv")) => (lambda (m) (if (jolt-truthy? (jolt-invoke m b a)) #t #f)))
                          ;; a deftype with a custom Object.equals (but no equiv) governs
                          ;; its own value equality and map-key identity — core.logic's
                          ;; LVar/LCons key substitutions on id, ignoring metadata, so
                          ;; structural jrec=? (which sees the meta field) is wrong here.
                          ((and (jrec? a) (jrec-cl a "equals")) => (lambda (m) (if (jolt-truthy? (jolt-invoke m a b)) #t #f)))
                          ((and (jrec? b) (jrec-cl b "equals")) => (lambda (m) (if (jolt-truthy? (jolt-invoke m b a)) #t #f)))
                          ;; A deftype declaring clojure.lang.Sequential compares
                          ;; ELEMENT-WISE against any other sequential, in either
                          ;; direction. The JVM reaches the same answer through
                          ;; APersistentVector/ASeq .equiv, which test `instanceof
                          ;; Sequential` rather than a concrete collection type — so
                          ;; (= (eduction (map inc) [1 2]) [2 3]) is true there.
                          ((or (jrec-sequential-decl? a) (jrec-sequential-decl? b))
                           (and (seq-eq-candidate? a) (seq-eq-candidate? b) (seq=? a b)))
                          ;; field-wise equality is a RECORD's semantics (the
                          ;; JVM defrecord's equals); a plain deftype without a
                          ;; declared equiv/equals is Object.equals — IDENTITY.
                          ;; Two equal-field deftype instances used to compare
                          ;; = here (and collapse as map keys), which the JVM
                          ;; never does.
                          ((and (jrec-record? a) (jrec-record? b)) (jrec=? a b))
                          (else (eq? a b)))))
;; jrec hashing is a fast clause in jolt-hash / jolt-hasheq (a jrec probe in
;; hash-fast-probes keeps any arm from claiming one), field-first: the hasheq
;; slot answers a repeat hash in one read. 0 = unset routes here.
;;   - a declared hasheq governs the value hash (clojure.core/hash is IHashEq
;;     first, so (hash a-record) == (hash an-equal-map) for flatland's types),
;;     then a declared hashCode; both are consulted on EVERY call — the JVM
;;     does not cache custom methods, and one may read mutable state — so
;;     these types never fill the slot.
;;   - a defrecord caches its structural hash in the slot (the __hasheq field);
;;     a plain deftype caches its identity hash there (Object.hashCode), each
;;     paired with the matching equality above so the hash/eq contract holds.
(define (jrec-hasheq-slow x)
  (cond ((jrec-cl x "hasheq") => (lambda (m) (jolt-invoke m x)))
        ((jrec-cl x "hashCode") => (lambda (m) (jolt-invoke m x)))
        ((jrec-record? x)
         (let ((h (jrec-hash x))) (jrec-hasheq-set! x h) h))
        (else
         (let ((h (jolt-identity-hasheq x))) (jrec-hasheq-set! x h) h))))
(define (jrec-hasheq-fast x)
  (let ((h (jrec-hasheq x)))
    (if (eqv? h 0) (jrec-hasheq-slow x) h)))
;; get on a jrec: a real field reads raw (so a deftype method's own field bindings,
;; compiled to (get inst :field), never recurse); a NON-field key on a deftype that
;; implements clojure.lang.ILookup routes to its valAt (core.match's pattern types
;; compute ::tag in valAt), else the default.
;; jrec is the hottest get target (every record field read), so jolt-get-dispatch
;; (collections.ss) checks jrec? directly and calls jrec-ref before the arm walk.
;; There is NO get-arm registration for jrec: jolt-get-arms has exactly one
;; consumer — the else branch of that same cond — so an arm on jrec? could never
;; run. register-get-arm! now rejects one at registration rather than accepting a
;; handler it would silently never call.
;; A jrec is a defrecord (map of fields) by default, BUT a deftype that
;; implements a clojure.lang collection interface carries the op as an inline
;; method — prefer that method, else fall back to the field/map behavior. (jrec-cl
;; finds the method; find-method-any-protocol / jolt-invoke resolve at call time.)
;; Same lookup as collections.ss rec-coll-method — one definition, aliased here.
(define jrec-cl rec-coll-method)

;; Does this deftype/record implement INTERFACE, directly or through one of the
;; interfaces it declares? register-inline-protocol! files each declared interface
;; as a super of the type's own tag in the class graph, so the graph answers
;; transitively and by either spelling: a type declaring
;; clojure.lang.IPersistentVector is an Associative and a Sequential too, exactly
;; as instanceof is on the JVM.
(define (jrec-declares? x interface)
  (and (jrec? x) (jch-isa? (jrec-tag x) interface)))

;; Everything about a type that follows from its DECLARED interfaces, derived once
;; per descriptor: is it a defrecord, does it declare a collection interface, which
;; collection shape does it print in, is it a CharSequence. These sit on hot paths —
;; every count of a record, every print of one — where the per-value cost is real:
;; asking the class graph each time cost 1.25x on (count deftype), and
;; jrec-declares-coll-iface? allocated a fresh key list on every call.
;;
;; Revalidated against the two epochs that between them cover every way the answers
;; can change: the class graph's (a deftype's interfaces land AFTER its descriptor
;; exists) and the protocol registry's (extend-type can add one later). Both only
;; increase, so their sum increases whenever either does and one compare suffices.
;;
;; Held in a table keyed BY the descriptor rather than in a field OF it: the
;; descriptor's layout is image-format surface (see make-jrdesc), and a table read
;; costs 2 ns more than a field read (4.97 vs 2.86 ns, Chez 10.4.1) — nothing next
;; to the 200 ns count path it saves. Weak, so a redefined type's descriptor is
;; still collectable. A miss inserts under the mutex; reads stay lock-free, which
;; a Chez hashtable survives (writer-vs-writer is what corrupts one).
(define jrdesc-ifc-tbl (make-weak-eq-hashtable))
(define jrdesc-ifc-mutex (make-mutex))
(define (jrdesc-ifc d) (hashtable-ref jrdesc-ifc-tbl d #f))
(define (jrdesc-ifc-set! d v) (jolt-with-mutex jrdesc-ifc-mutex (hashtable-set! jrdesc-ifc-tbl d v)))
(define (jrdesc-ifc-epoch) (fx+ jch-graph-epoch jolt-proto-epoch))
;; The shape order is the JVM's print-method precedence, verified against it: ISeq
;; outranks IPersistentVector / IPersistentSet / IPersistentMap, and IRecord
;; outranks IPersistentMap (prefer-method), so a defrecord has no shape at all.
;; Exactly these four have a print-method there — a type declaring only
;; IPersistentList or IPersistentCollection falls through to #object[…], so neither
;; is listed.
;; The epoch is read BEFORE the graph is asked, and deliberately not inline in the
;; vector: Chez evaluates arguments right to left, so an inline read would happen
;; last and stamp answers derived from the OLD graph with the epoch of the new one
;; — a stale entry that revalidation could never catch. Reading it first can only
;; understamp, which costs one re-derivation.
(define (jrdesc-derive-ifc d record?)
  (let ((tag (jrdesc-tag d))
        (epoch (jrdesc-ifc-epoch)))
    (vector epoch
            (and (not record?)
                 (cond ((jch-isa? tag "clojure.lang.ISeq") 'seq)
                       ((jch-isa? tag "clojure.lang.IPersistentVector") 'vec)
                       ((jch-isa? tag "clojure.lang.IPersistentSet") 'set)
                       ((jch-isa? tag "clojure.lang.IPersistentMap") 'map)
                       (else #f)))
            (jch-isa? tag "java.lang.CharSequence")
            record?
            (and (not record?) (tag-declares-coll-iface? tag))
            ;; slot 5: declares clojure.lang.Sequential. Derived here with the
            ;; rest because it is asked on the EQUALITY path — every (= record x)
            ;; consults it for both operands — and answering it from the protocol
            ;; table meant a mutex and a key-vector allocation per comparison.
            (and (not record?) (tag-declares-sequential? tag)))))
(define (jrdesc-ifc-of x)
  (let* ((d (jrec-desc x))
         (c (jrdesc-ifc d)))
    (if (and c (fx=? (vector-ref c 0) (jrdesc-ifc-epoch)))
        c
        (let ((fresh (jrdesc-derive-ifc d (jrec-record?-uncached x))))
          (jrdesc-ifc-set! d fresh)
          fresh))))

;; A CharSequence is not a collection, but three of RT's entry points name one
;; anyway — RT.count is its length, RT.seq walks its characters, RT.nth reads one
;; by index. A deftype presenting a WINDOW over a string (instaparse's Segment)
;; relies on all three. The cached flag is checked BEFORE the method lookup, not
;; after: these sit on the count path, where find-method-any-protocol has to walk
;; the type's protocol tables, and asking it first cost 1.26x on (count deftype).
;; A type that is not a CharSequence now pays a vector-ref and a fixnum compare.
(define (jrec-charseq? x)
  (and (jrec? x) (vector-ref (jrdesc-ifc-of x) 2)))
(define (jrec-charseq-method x name)
  (and (jrec-charseq? x) (jrec-cl x name)))
;; length is the half of the pair every CharSequence path needs, and a deftype can
;; declare the interface without implementing it — the JVM answers AbstractMethodError
;; there, so invoking #f (a cast error naming Boolean) must not be what happens.
(define (jrec-charseq-length-method x)
  (or (jrec-cl x "length") (jrec-abstract-method-error x "length")))
;; The characters of a CharSequence deftype, lazily, through length + charAt —
;; the pair the interface guarantees, and what StringSeq reads on the JVM. Not
;; via toString: a seq must not depend on a method seq does not use.
(define (jrec-charseq->seq x len charat)
  (let ((n (->idx (jolt-invoke len x))))
    (let build ((i 0))
      (if (fx>=? i n)
          jolt-nil
          (cseq-lazy (jolt-invoke charat x i) (lambda () (build (fx+ i 1))))))))
;; The content of a CharSequence deftype as one string, for the callers that need
;; a whole string rather than a walk (the regex entry points). toString is what the
;; interface guarantees and what a window type implements, so prefer it; a type
;; that declares the interface without one is assembled from charAt. #f when the
;; value is not a CharSequence deftype at all.
(define (jrec-charseq->string x)
  (cond ((jrec-charseq-method x "toString")
         => (lambda (m) (jolt-need-str (jolt-invoke m x))))
        ((jrec-charseq-method x "charAt")
         => (lambda (m) (list->string (seq->list (jrec-charseq->seq x (jrec-charseq-length-method x) m)))))
        (else #f)))

;; A deftype that DECLARES a clojure.lang collection interface but leaves one of
;; its methods unimplemented throws AbstractMethodError when a core fn reaches
;; for it, like the JVM — it must not fall back to the bare-deftype
;; fields-as-map behavior (fireworks renders such types by catching this).
;; Records are exempt: defrecord generates the full map implementation.
(define jrec-coll-iface-names
  '("IPersistentCollection" "IPersistentMap" "IPersistentVector" "IPersistentSet"
    "IPersistentStack" "IPersistentList" "ISeq" "Seqable" "Indexed" "Counted"
    "Associative" "ILookup" "Reversible" "Sorted"))
;; Keyed by TAG so jrdesc-derive-ifc can compute it once per type; the walk builds
;; a key list, which is why answering it per call was worth caching.
(define (tag-declares-coll-iface? tag)
  (let ((ti (hashtable-ref type-registry tag #f)))
    (and ti
         (let loop ((ps (vector->list (jolt-with-mutex rec-tbl-mu (hashtable-keys ti)))))
           (cond ((null? ps) #f)
                 ((member (jch-last-segment (car ps)) jrec-coll-iface-names) #t)
                 (else (loop (cdr ps))))))))
(define (jrec-declares-coll-iface? x)
  (and (jrec? x) (vector-ref (jrdesc-ifc-of x) 4)))
;; Does this deftype DECLARE clojure.lang.Sequential? On the JVM that marker is
;; what makes a value participate in sequential value equality — the collection
;; side's .equiv tests `instanceof Sequential` and then compares element-wise —
;; so it governs (= some-vector an-eduction) in both directions. Records are
;; excluded: a defrecord is a map, not a sequential.
;;
;; Keyed by TAG, like tag-declares-coll-iface? beside it, so jrdesc-derive-ifc
;; answers it once per type per epoch instead of once per comparison.
(define (tag-declares-sequential? tag)
  (let ((ti (hashtable-ref type-registry tag #f)))
    (and ti
         (let loop ((ps (vector->list (jolt-with-mutex rec-tbl-mu (hashtable-keys ti)))))
           (cond ((null? ps) #f)
                 ((string=? (jch-last-segment (car ps)) "Sequential") #t)
                 (else (loop (cdr ps))))))))
(define (jrec-sequential-decl? x)
  (and (jrec? x) (vector-ref (jrdesc-ifc-of x) 5)))
;; Both sides must be seq-comparable for the element-wise path; anything else
;; (a number, a map, a bare deftype) is simply not equal to a sequential.
(define (seq-eq-candidate? x)
  (or (jolt-sequential? x) (jolt-lazyseq? x) (jrec-sequential-decl? x)))

(define (jrec-abstract-method-error x method)
  (jolt-throw (jolt-host-throwable "java.lang.AbstractMethodError"
    (string-append "Method " (jrec-tag x) "/" method "() is abstract"))))

;; iface-method: the single deftype/reify interface-method lookup. Returns the
;; impl fn for METHOD declared by V (a deftype/record OR a reify), or #f. NARGS
;; (including `this`) selects the matching arity for a deftype; #f means any
;; arity. Core fns route interface dispatch through this instead of each
;; re-deriving jrec-vs-reify lookup and arity handling.
(define (iface-method v method nargs)
  (cond ((jrec? v)
         (if nargs (find-method-any-protocol-arity (jrec-tag v) method nargs)
             (find-method-any-protocol (jrec-tag v) method)))
        ((jreify? v) (let ((rm (reified-methods v))) (and rm (hashtable-ref rm method #f))))
        (else #f)))
;; a record counts its declared fields plus anything assoc'd on beyond them
(define (jrec-field-count coll)
  (+ (jrec-nfields coll)
     (let ((ext (jrec-ext coll))) (if (jolt-nil? ext) 0 (jolt-count ext)))))
(register-count-arm! (lambda (coll) (or (jrec? coll) (jolt-transient? coll)))
  (lambda (coll)
    (cond
      ((jolt-transient? coll) (t-count coll))
      ((jrec-cl coll "count") => (lambda (m) (jolt-invoke m coll)))
      ;; One cached read answers the rest. A defrecord IS a map of its fields, and
      ;; is the common case here, so it is settled before anything else.
      (else
       (let ((ifc (jrdesc-ifc-of coll)))
         (cond
           ((vector-ref ifc 3) (jrec-field-count coll))
           ((vector-ref ifc 4) (jrec-abstract-method-error coll "count"))
           ;; RT.count reaches CharSequence.length once Counted and
           ;; IPersistentCollection have both missed — a declared `count` above
           ;; therefore outranks `length`, as it does on the JVM.
           ((vector-ref ifc 2) (jolt-invoke (jrec-charseq-length-method coll) coll))
           (else (jrec-field-count coll))))))))
;; contains?: a deftype implementing Associative/containsKey (e.g. core.cache's
;; caches) answers through that; a plain defrecord checks its fields.
(register-contains-arm! (lambda (coll) (jrec-cl coll "containsKey"))
  (lambda (coll k) (if (jolt-truthy? (jolt-invoke (jrec-cl coll "containsKey") coll k)) #t #f)))
;; a deftype implementing clojure.lang.IPersistentSet/Set.contains (a set-like type
;; has membership, not keys) — (contains? an-ordered-set k) routes to it.
(register-contains-arm! (lambda (coll) (and (jrec? coll) (jrec-cl coll "contains")))
  (lambda (coll k) (if (jolt-truthy? (jolt-invoke (jrec-cl coll "contains") coll k)) #t #f)))
;; a plain defrecord (no containsKey/contains of its own) checks its fields;
;; guarded so the containsKey- and contains-method arms above (registered first,
;; checked after this one in the newest-first walk) win for a deftype that declares
;; either — else contains?/find on a map-like (OrderedMap: containsKey) or set-like
;; (OrderedSet: contains) deftype reads field presence, not the type's membership.
(register-contains-arm! (lambda (coll) (and (jrec? coll)
                                            (not (jrec-cl coll "containsKey"))
                                            (not (jrec-cl coll "contains"))))
  (lambda (coll k) (jrec-has? coll k)))
(register-contains-arm! jolt-transient? t-contains?)
;; empty?: a transient is empty when its count is 0 (transients gained empty?/
;; bounded-count support in Clojure 1.12). Without this arm empty? fell through to
;; (jolt-seq coll), which throws on a transient.
(register-empty-arm! jolt-transient? (lambda (t) (zero? (t-count t))))
;; nth: transient unwrapping (vec→direct buf access, other→fallback), then original
(define %r-jolt-nth jolt-nth)
(set! jolt-nth
  (case-lambda
     ((coll i)
      (if (jolt-transient? coll)
          (begin
            (jolt-trans-check coll "nth")
            (if (eq? (jolt-transient-kind coll) 'vec)
                (let ((idx (->idx i)))
                  (if (tvec-in-bounds? coll idx) (vector-ref (jolt-transient-buf coll) idx) (error 'nth "index out of bounds")))
                (%r-jolt-nth (jolt-transient-buf coll) i)))
          (%r-jolt-nth coll i)))
    ((coll i d)
     (if (jolt-transient? coll)
         (if (eq? (jolt-transient-kind coll) 'vec)
             (let ((idx (->idx i))) (if (tvec-in-bounds? coll idx) (vector-ref (jolt-transient-buf coll) idx) d))
             (%r-jolt-nth (jolt-transient-buf coll) i d))
         (%r-jolt-nth coll i d)))))
;; assoc: replacing a declared field copies the value vector; any other key grows
;; the extension map (the value vector is shared — fields are immutable).
(define %r-jolt-assoc1 jolt-assoc1)
(set! jolt-assoc1 (lambda (coll k v)
  (cond ((jrec-cl coll "assoc") => (lambda (m) (jolt-invoke m coll k v)))
        ((jrec? coll)
         (let ((i (and (keyword? k) (jrec-field-index coll k))))
            (if i
                (let ((v2 (let ((flags (hashtable-ref chez-record-dbl-tbl (jrec-tag coll) #f)))
                            (if (and flags (fx< i (vector-length flags)) (vector-ref flags i)
                                     (number? v) (not (flonum? v)))
                                (exact->inexact v) v))))
                  (make-jrec-from-existing coll i v2 (jrec-ext coll)))
                (let ((ext (jrec-ext coll)))
                  (make-jrec-from-existing coll #f #f
                             (%r-jolt-assoc1 (if (jolt-nil? ext) empty-pmap ext) k v))))))
        (else (%r-jolt-assoc1 coll k v)))))
;; dissoc: a deftype implementing IPersistentMap/without answers through it.
;; Removing a declared field downgrades a plain record to a map (JVM parity); an
;; extension key drops from the ext map (normalized back to jolt-nil when empty).
(define (jrec->map-without r drop-k)
  (let* ((fkeys (jrdesc-fkeys (jrec-desc r))) (n (vector-length fkeys)))
    (let loop ((i 0) (m empty-pmap))
      (if (= i n)
          (let ((ext (jrec-ext r)))
            (if (jolt-nil? ext) m
                (fold-left (lambda (mm p) (%r-jolt-assoc1 mm (car p) (cdr p))) m (jrec-ext-pairs ext))))
          (let ((fk (vector-ref fkeys i)))
            (loop (+ i 1) (if (eq? fk drop-k) m (%r-jolt-assoc1 m fk (jrec-field-ref r i)))))))))
(define %r-jolt-dissoc jolt-dissoc)
(define %r-jolt-dissoc2 jolt-dissoc2)
(define (jrec-dissoc1 coll k)
  (if (not (jrec? coll))
      (%r-jolt-dissoc coll k)            ; an earlier declared-field dissoc downgraded it
      (let ((i (and (keyword? k) (jrec-field-index coll k))))
        (if i (jrec->map-without coll k)
            (let ((ext (jrec-ext coll)))
              (if (jolt-nil? ext) coll
                  (let ((ne (%r-jolt-dissoc ext k)))
                    (make-jrec-from-existing coll #f #f
                                (if (= 0 (jolt-count ne)) jolt-nil ne)))))))))
(set! jolt-dissoc (lambda (coll . ks)
  (cond ((jrec-cl coll "without")
         => (lambda (m) (fold-left (lambda (c k) (jolt-invoke m c k)) coll ks)))
        ((jrec? coll) (fold-left jrec-dissoc1 coll ks))
        (else (apply %r-jolt-dissoc coll ks)))))
(set! jolt-dissoc2
  (lambda (coll k)
    (cond ((jrec-cl coll "without") => (lambda (m) (jolt-invoke m coll k)))
          ((jrec? coll) (jrec-dissoc1 coll k))
          (else (%r-jolt-dissoc2 coll k)))))
;; keys/vals over a jrec read its entry seq (jolt-seq is method-first, so a
;; map-like deftype delegates to its Seqable; a defrecord's seq is its fields, so
;; the result is unchanged for records).
(define (jrec-seq-col m which)
  (let loop ((s (jolt-seq m)) (acc '()))
    (if (jolt-nil? s) (list->cseq (reverse acc))
        (loop (jolt-seq (seq-more s)) (cons (jolt-nth (seq-first s) which) acc)))))
(define %r-jolt-keys jolt-keys)
(set! jolt-keys (lambda (m) (if (jrec? m) (jrec-seq-col m 0) (%r-jolt-keys m))))
(define %r-jolt-vals jolt-vals)
(set! jolt-vals (lambda (m) (if (jrec? m) (jrec-seq-col m 1) (%r-jolt-vals m))))
;; a record's seq is its field map-entries in declared order, then any extensions.
(define (jrec-entry-list r)
  (let* ((fkeys (jrdesc-fkeys (jrec-desc r))) (n (vector-length fkeys)))
    (let loop ((i 0) (acc '()))
      (if (= i n)
          (let ((ext (jrec-ext r)))
            (append (reverse acc)
                    (if (jolt-nil? ext) '()
                        (map (lambda (p) (make-map-entry (car p) (cdr p))) (jrec-ext-pairs ext)))))
          (loop (+ i 1) (cons (make-map-entry (vector-ref fkeys i) (jrec-field-ref r i)) acc))))))
;; seq over a jrec stays METHOD-first: a declared seq wins, a defrecord seqs its
;; entries, a deftype that declares a collection interface without implementing
;; seq is an abstract-method error, and anything else falls through to jolt-seq's
;; "Don't know how to create ISeq from" — the JVM answer for a type that is not
;; Seqable, whatever its field count. Deciding emptiness from the jrec's own field
;; count instead would answer for the WRAPPER rather than the collection: a
;; deftype holding a backing map has one field, so it would never read as empty.
;; Arms dispatch newest-first, so these four are registered in reverse precedence:
;; a declared seq, then a record's entries, then the characters of a CharSequence,
;; then the abstract-method error. That is RT.seqFrom's order — Seqable first,
;; CharSequence after it, and everything else an IllegalArgumentException — which
;; is why the error arm is registered FIRST and reached LAST. Only the names that
;; imply Seqable would beat CharSequence there; Counted or Indexed alone do not,
;; and jrec-coll-iface-names deliberately covers more than Seqable.
(register-seq-arm! (lambda (x) (and (jrec? x) (jrec-declares-coll-iface? x)))
  (lambda (x) (jrec-abstract-method-error x "seq")))
(register-seq-arm! (lambda (x) (jrec-charseq-method x "charAt"))
  (lambda (x) (jrec-charseq->seq x (jrec-charseq-length-method x) (jrec-cl x "charAt"))))
(register-seq-arm! jrec-record?
  (lambda (x) (list->cseq (jrec-entry-list x))))
;; A deftype that IS a seq (clojure.lang.ISeq: its seq method answers itself)
;; walks through its own first and next into a lazy cseq — nil from next ends
;; it, and a `more` without a `next` ends on an empty rest. Re-asking the
;; answer for its seq, as the general arm below does, would ask forever; and
;; only the cseq shape gives count/nth/reduce and the printer their walk.
(define (jrec-iseq->cseq x)
  (let ((first-m (jrec-cl x "first"))
        (next-m (jrec-cl x "next"))
        (more-m (jrec-cl x "more")))
    (define (step s)
      (jolt-make-lazy-seq
        (lambda ()
          (let ((tail (cond (next-m (jolt-invoke next-m s))
                            (more-m (let ((r (jolt-invoke more-m s)))
                                      (if (jolt-nil? (jolt-seq r)) jolt-nil r)))
                            (else jolt-nil))))
            (jolt-cons (jolt-invoke first-m s)
                       (if (jolt-nil? tail) jolt-nil
                           (if (and (jrec? tail) (eq? (jrec-tag tail) (jrec-tag x)))
                               (step tail)
                               (jolt-seq tail))))))))
    (if first-m (jolt-seq (step x)) (jrec-abstract-method-error x "first"))))
(register-seq-arm! (lambda (x) (jrec-cl x "seq"))
  (lambda (x)
    (let ((r (jolt-invoke (jrec-cl x "seq") x)))
      (if (eq? r x) (jrec-iseq->cseq x) (jolt-seq r)))))
(register-conj-arm! (lambda (coll) (jrec-cl coll "cons"))
  (lambda (coll x) (jolt-invoke (jrec-cl coll "cons") coll x)))
;; A plain defrecord (no IPersistentCollection.cons of its own) conjs a [k v] pair
;; or map. Guarded to skip a deftype that declares its own cons — that method wins
;; (registered above but checked after this newer arm), so the guard preserves the
;; deftype's collection semantics (flatland.ordered's OrderedSet conjs a scalar).
(register-conj-arm! (lambda (coll) (and (jrec? coll) (not (jrec-cl coll "cons"))))
  (lambda (coll x)
    (if (pmap? x)
        ;; a map folds its entries (JVM parity): (conj record {:k v ...}) merges them.
        ;; Otherwise x is a [k v] pair / MapEntry — assoc the two elements.
        (pmap-fold-fwd x (lambda (k v c) (jolt-assoc1 c k v)) coll)
        (jolt-assoc1 coll (jolt-nth x 0) (jolt-nth x 1)))))
;; peek/pop on a deftype implementing IPersistentStack (data.priority-map, which
;; core.cache's LRU/LU caches lean on) dispatch to its methods.
;; empty? over a jrec: a map-like deftype is empty iff its entry seq is (data
;; .priority-map's peek calls (.isEmpty this) -> empty?). jolt-seq is method-first,
;; so this asks the type's own seq/count rather than counting the jrec's fields.
(register-empty-arm! jrec-collection? (lambda (coll) (jolt-nil? (jolt-seq coll))))
(define %r-jolt-peek jolt-peek)
(set! jolt-peek (lambda (coll)
  (cond ((jrec-cl coll "peek") => (lambda (m) (jolt-invoke m coll)))
        (else (%r-jolt-peek coll)))))
(define %r-jolt-pop jolt-pop)
(set! jolt-pop (lambda (coll)
  (cond ((jrec-cl coll "pop") => (lambda (m) (jolt-invoke m coll)))
        (else (%r-jolt-pop coll)))))
(register-pr-arm! jrec? jrec-pr)

;; records are map? and coll? (Clojure: a record IS an associative map). The
;; predicates.ss vars hold a snapshot, so re-def-var! after extending. record? is
;; the overlay's (some? (get x :jolt/deftype)) — works for free since the get
;; override returns the tag for that key.
;; only a defrecord is a map (Clojure: a record IS an associative map); a bare
;; deftype is not. coll? additionally covers a deftype implementing a collection
;; interface. predicates.ss vars hold a snapshot, so re-def-var! after extending.
(register-map-pred-arm! jrec-maplike?)
(def-var! "clojure.core" "map?" jolt-map?)
(def-var! "clojure.core" "coll?" (lambda (x) (or (jrec-collection? x) (jolt-coll-pred? x))))
