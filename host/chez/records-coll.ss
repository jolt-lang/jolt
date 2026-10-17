;; records-coll.ss — the jrec arms on the collection dispatchers: equality/hash,
;; count/contains?/seq/conj/assoc/dissoc/keys/vals/nth/peek/pop, plus the
;; per-descriptor derived-interface cache (jrdesc-ifc-of) they read.
;;
;; Loaded after records.ss (the jrec layout) and before protocols.ss /
;; records-dispatch.ss. Form order across the four records files is
;; load-bearing: arms dispatch newest-first, so registration order here is
;; dispatch precedence.

;; ---- extend the collection dispatchers with a jrec arm ----------------------
;; Equality with a jrec on either side is clojure.lang.Util/equiv, step for step:
;; the LEFT side's equiv when it is an IPersistentCollection, else the right
;; side's, else the left side's equals.
;;   - a deftype's equiv is the one it declares (core.cache's caches equiv to
;;     their backing map, so (= cache {…}) holds); a defrecord's is field-wise
;;     jrec=? against the same record type, so a record is never = a plain map.
;;   - a jolt collection's equiv against a jrec: a sequential one compares
;;     element-wise with a type that declares clojure.lang.Sequential (the JVM's
;;     `instanceof Sequential` test, which is why (= [2 3] (eduction (map inc)
;;     [1 2])) holds in either order); a map or set never equals one. It does NOT
;;     ask the jrec's equiv: (= {:a 1} cache) is false on the JVM.
;;   - equals is the declared Object.equals (core.logic's LVar keys its
;;     substitutions on id, ignoring metadata, so structural jrec=? would be
;;     wrong), a defrecord's field-wise compare, or IDENTITY — two equal-field
;;     plain deftypes are not =, and neither are two that declare only Sequential.
;;
;; Two jrecs are answered from jolt=2 ahead of the arm walk (values.ss): this arm
;; registers first, so it was the LAST one asked, and every record compare paid
;; every other arm's predicate before reaching it. The jrec pair is in
;; eq-fast-probes, so no arm may claim one; this arm owns the MIXED pairs, which
;; still walk so a library arm for the other side's type is asked first.
;;
;; What a type contributes is read from its derived-interface vector (IA/IB
;; below, #f for a side that is not a jrec): slot 12 is #(equiv equals ipc?
;; list? map? set?),
;; slot 5 its declared Sequential, slot 3 whether it is a defrecord — where the
;; arm used to look equiv and equals up by name on every compare.
(define (jrec-equiv=? a b)
  (let* ((ia (and (jrec? a) (jrdesc-ifc-of a)))
         (ib (and (jrec? b)
                  (if (and ia (eq? (jrec-desc a) (jrec-desc b))) ia (jrdesc-ifc-of b)))))
    (cond ((if ia (vector-ref (vector-ref ia 12) 2) (jrec-eq-other-coll? a))
           (jrec-ipc-equiv a ia b ib))
          ((if ib (vector-ref (vector-ref ib 12) 2) (jrec-eq-other-coll? b))
           (jrec-ipc-equiv b ib a ia))
          (ia (jrec-equals a ia b ib))
          (else #f))))
;; the other side is one of jolt's own collections (it is not a jrec, and no
;; library arm claimed the pair)
(define (jrec-eq-other-coll? x)
  (or (jolt-sequential? x) (jolt-lazyseq? x) (jolt-coll? x)))
;; x.equiv(o); IX is #f when x is a jolt collection (o is a jrec then), and the
;; collection's own equiv decides by what o is: APersistentVector/ASeq compare
;; element-wise with a Sequential or java.util.List, APersistentMap entry by
;; entry with a java.util.Map it accepts, APersistentSet member by member with a
;; java.util.Set.
(define (jrec-ipc-equiv x ix o io)
  (cond ((not ix)
         (let ((po (vector-ref io 12)))
           (cond ((or (jolt-sequential? x) (jolt-lazyseq? x))
                  (and (vector-ref po 3) (seq=? x o)))
                 ((jolt-map? x)
                  (and (vector-ref po 4)
                       (= (jolt-count x) (jolt-count o))
                       (let loop ((s (jolt-seq x)))
                         (or (jolt-nil? s)
                             (let* ((e (jolt-first s)) (k (jolt-nth e 0 jolt-nil)))
                               (and (jolt-truthy? (jolt-contains? o k))
                                    (jolt=2 (jolt-nth e 1 jolt-nil) (jolt-get o k jolt-nil))
                                    (loop (jolt-next s))))))))
                 ((or (jolt-set? x) (htable-sorted-set? x))
                  (and (vector-ref po 5)
                       (= (jolt-count x) (jolt-count o))
                       (let loop ((s (jolt-seq o)))
                         (or (jolt-nil? s)
                             (and (jolt-truthy? (jolt-contains? x (jolt-first s)))
                                  (loop (jolt-next s)))))))
                 (else #f))))
        ((vector-ref (vector-ref ix 12) 0)
         => (lambda (m) (if (jolt-truthy? (jolt-invoke m x o)) #t #f)))
        (else (jrec-equals x ix o io))))
;; x.equals(o)
(define (jrec-equals x ix o io)
  (cond ((vector-ref (vector-ref ix 12) 1)
         => (lambda (m) (if (jolt-truthy? (jolt-invoke m x o)) #t #f)))
        ((vector-ref ix 3) (and io (vector-ref io 3) (jrec=? x o)))
        (else (eq? x o))))
(register-eq-arm! (lambda (a b) (not (eq? (jrec? a) (jrec? b)))) jrec-equiv=?)
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
;;
;; The slot is the cache because the jrec LAYOUT is image-format surface — adding
;; a field means bumping every family tag — so there is nowhere else to put it.
;; A type with a declared hasheq/hashCode never fills it. The write is a plain
;; fixnum store with no lock: racing writers compute the same value (a structural
;; hash is deterministic, and the identity table answers one id per object), so a
;; double store is benign. The slot never travels — the image dump starts it
;; unset (state-image.ss), the way the JVM marks __hasheq transient.
(define (jrec-hasheq-slow x)
  (cond ((jrec-cl x "hasheq") => (lambda (m) (call-declared-hash m x)))
        ((jrec-cl x "hashCode") => (lambda (m) (call-declared-hash m x)))
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
            (and (not record?) (tag-declares-sequential? tag))
            ;; slot 6: a bare deftype's DECLARED ILookup, as (3-arity . 2-arity);
            ;; either half may be #f. The JVM gives a bare deftype no key lookup
            ;; of its own, so a declared valAt IS the lookup and answers for
            ;; every key — a field-named one included, which is the whole point
            ;; when the slot holds something valAt is there to transform
            ;; (typed.clojure keeps a lazy thunk in one and forces it here).
            ;; A defrecord keeps its generated field-first lookup: the JVM will
            ;; not compile one that declares another valAt.
            ;; Derived here, so the get path pays the vector-ref it already pays
            ;; for the flags above rather than two string hashes per lookup.
            (and (not record?)
                 (let ((m3 (find-method-any-protocol-arity tag "valAt" 3))
                       (m2 (find-method-any-protocol-arity tag "valAt" 2)))
                   ;; -arity falls back to ANY same-named impl when no arity
                   ;; matches, so each half is confirmed against the procedure
                   ;; before it is kept — a type declaring only (valAt [_ k])
                   ;; must not be called with a not-found argument.
                   (let ((m3 (and m3 (proc-accepts? m3 3) m3))
                         (m2 (and m2 (proc-accepts? m2 2) m2)))
                     (and (or m3 m2) (cons m3 m2)))))
            ;; slot 7: the type's declared impl per method NAME (jrec-method),
            ;; filled on first ask. Here, and not a table of its own, so it is
            ;; retired with the rest of this vector when either epoch moves.
            (make-weak-eq-hashtable)
            ;; slot 8: instance?'s own-type answer per class NAME
            ;; (jrec-declares-class?, records-interop.ss), keyed eq? like slot 7.
            (make-weak-eq-hashtable)
            ;; slot 9: jrec-method-arity's answers, method name -> ((nargs . impl) …)
            (make-weak-eq-hashtable)
            ;; slot 10: satisfies?'s answer per protocol NAME (records-dispatch.ss)
            (make-weak-eq-hashtable)
            ;; slot 11: jrec-type-isa?'s answer per interface NAME
            (make-weak-eq-hashtable)
            ;; slot 12: what jrec-equiv=? decides equality by — the declared
            ;; equiv and equals impls (either #f), and whether the type is an
            ;; IPersistentCollection: a defrecord always is, and so is a type
            ;; with an equiv impl, which an extend-type can add without a
            ;; class-graph edge
            ;; The last three are what a jolt collection's own equiv asks of the
            ;; other side (jrec-ipc-equiv): is it a java.util.List or Sequential,
            ;; a java.util.Map that is not an IPersistentMap or also declares
            ;; MapEquivalence (APersistentMap.equiv's test — flatland's ordered
            ;; map passes it, a defrecord does not), a java.util.Set.
            (let ((equiv (find-method-any-protocol tag "equiv")))
              (vector equiv
                      (find-method-any-protocol tag "equals")
                      (and (or record? equiv (jch-isa? tag "clojure.lang.IPersistentCollection")) #t)
                      (and (not record?)
                           (or (tag-declares-sequential? tag) (jch-isa? tag "java.util.List")) #t)
                      (and (jch-isa? tag "java.util.Map")
                           (or (not (jch-isa? tag "clojure.lang.IPersistentMap"))
                               (jch-isa? tag "clojure.lang.MapEquivalence"))
                           #t)
                      (and (jch-isa? tag "java.util.Set") #t)))
            ;; slot 13: jrec-dash-field-index's answers, "-name" -> slot or #f
            (make-weak-eq-hashtable))))
(define (jrdesc-ifc-of x)
  (let* ((d (jrec-desc x))
         (c (jrdesc-ifc d)))
    (if (and c (fx=? (vector-ref c 0) (jrdesc-ifc-epoch)))
        c
        (let ((fresh (jrdesc-derive-ifc d (jrec-record?-uncached x))))
          (jrdesc-ifc-set! d fresh)
          fresh))))
;; The impl a record type declares for METHOD (any protocol), or #f. Every
;; collection op on a record asks this first — equality asks it for equiv and
;; equals on both operands, hashing for hasheq and hashCode, meta/assoc/nth/count
;; each for their own — and find-method-any-protocol answers from two string-keyed
;; tables per call. core.logic keys its substitution maps on LVars, which declare
;; equals and hashCode, so every map probe paid four such lookups per key compared.
;; Keyed eq? on the name: callers pass literals, so a repeat is one eq?-ref; a
;; name spelled by a different string object just fills its own entry.
(define (jrec-method x method)
  (let* ((t (vector-ref (jrdesc-ifc-of x) 7))
         (hit (hashtable-ref t method 'none)))
    (if (eq? hit 'none)
        (let ((m (find-method-any-protocol (jrec-tag x) method)))
          (jolt-with-mutex jrdesc-ifc-mutex (hashtable-set! t method m))
          m)
        hit)))
;; Does a record TYPE implement IFACE — its own tag, an interface or protocol it
;; declares, or their ancestry in the class graph? The question map?/coll?/
;; vector?/… ask of a deftype, answered from the type alone and memoized per type
;; (slot 11, keyed eq? on the literal interface name).
;;
;; It must NOT be instance?: instance? consults the library arms, and a library's
;; value-tags arm (__register-class!) is a predicate over arbitrary values that
;; may well call map? — which asked instance? of the same deftype, which asked the
;; arm again: an unbounded recursion that hung any (satisfies? P x) or
;; (instance? P x) on such a value once a library had registered one.
(define (jrec-type-isa? x iface)
  (let* ((t (vector-ref (jrdesc-ifc-of x) 11))
         (hit (hashtable-ref t iface 'none)))
    (if (eq? hit 'none)
        (let* ((tag (jrec-tag x))
               (ans (or (jrec-declares-class? tag iface) (jch-isa? tag iface))))
          (jolt-with-mutex jrdesc-ifc-mutex (hashtable-set! t iface ans))
          ans)
        hit)))
;; The declared slot a (.-name x) field read names, or #f: METHOD is the dashed
;; name the call site passes as a literal, so a repeat is one eq?-ref (slot 13)
;; where the read built the undashed substring, interned it as a keyword and
;; looked that up per call — ~70 ns of a deftype equals that reads the other
;; instance's field, which every map probe keyed on such a type runs.
(define (jrec-dash-field-index x method)
  (let* ((t (vector-ref (jrdesc-ifc-of x) 13))
         (hit (hashtable-ref t method 'none)))
    (if (eq? hit 'none)
        (let ((i (and (fx>? (string-length method) 1)
                      (char=? (string-ref method 0) #\-)
                      (jrec-field-index x (keyword #f (substring method 1 (string-length method)))))))
          (jolt-with-mutex jrdesc-ifc-mutex (hashtable-set! t method i))
          i)
        hit)))
;; ...and by name AND arity (NARGS counts `this`), for the calls that pick one
;; arity of a method a type declares at several — every (.method rec …) interop
;; call and iface-method. Same cache, same keying; find-method-any-protocol-arity
;; decides, including its any-arity fallback.
(define (jrec-method-arity x method nargs)
  (let* ((t (vector-ref (jrdesc-ifc-of x) 9))
         (hit (assv nargs (hashtable-ref t method '()))))
    (if hit
        (cdr hit)
        (let ((m (find-method-any-protocol-arity (jrec-tag x) method nargs)))
          (jolt-with-mutex jrdesc-ifc-mutex
            (hashtable-set! t method (cons (cons nargs m) (hashtable-ref t method '()))))
          m))))

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
;; answers it once per type per epoch (slot 5, read by jrec-equiv=?) instead of
;; once per comparison.
(define (tag-declares-sequential? tag)
  (let ((ti (hashtable-ref type-registry tag #f)))
    (and ti
         (let loop ((ps (vector->list (jolt-with-mutex rec-tbl-mu (hashtable-keys ti)))))
           (cond ((null? ps) #f)
                 ((string=? (jch-last-segment (car ps)) "Sequential") #t)
                 (else (loop (cdr ps))))))))

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
         (if nargs (jrec-method-arity v method nargs) (jrec-method v method)))
        ((jreify? v) (reify-method-ref v method))
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
                  (if (tvec-in-bounds? coll idx)
                      (tvec-ref coll idx)
                      (begin (index-int-check! idx) (throw-jvm 'IndexOutOfBoundsException jolt-nil))))
                (%r-jolt-nth (jolt-transient-buf coll) i)))
          (%r-jolt-nth coll i)))
    ((coll i d)
     (if (jolt-transient? coll)
         (if (eq? (jolt-transient-kind coll) 'vec)
             (let ((idx (->idx i)))
               (if (tvec-in-bounds? coll idx) (tvec-ref coll idx) (begin (index-int-check! idx) d)))
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
