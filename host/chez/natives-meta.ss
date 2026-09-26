;; metadata — meta / with-meta.
;;
;; A collection carries its metadata in a field of its own record, the JVM's Obj
;; shape (PersistentVector, PersistentHashMap, PersistentHashSet, PersistentList,
;; Cons, LazySeq and EmptyList all extend Obj and keep an IPersistentMap _meta):
;; pvec, pmap, pset, a seq cell, () and a lazy-seq node each end in a `meta`
;; slot (collections.ss, seq.ss, lazy-bridge.ss). Reading meta is a field read
;; and attaching it is one allocation — with-meta returns a fresh copy with the
;; new meta (Clojure's immutable-with-meta), and an op that threads its
;; receiver's meta forward (conj/assoc/dissoc/pop/into) copies it onto the
;; result. No table, no lock, on any thread.
;;
;; The slot is mutable for one reason: the copy with-meta and meta-carry build,
;; and the object the image walker rebuilds, are filled in before anyone else
;; can see them. It is never written on a published value — that would change
;; the meta of every holder of the value, which with-meta returning a copy
;; exists to prevent. coll-meta-set! is the only writer, and every caller of it
;; holds a fresh instance.
;;
;; Everything else that can carry meta — a defrecord/deftype instance, a reify,
;; a fn, a sorted collection (host-table.ss) — keeps it in the identity-keyed
;; side table below: with-meta returns a fresh COPY of the value (new identity)
;; and records its meta there, so the original is unchanged and a copy made by
;; a later op drops it. Symbols carry meta in their own field. meta on a value
;; that carries none (number/string/keyword) is nil.
;;
;; Loaded after records.ss (jrec) + collections/seq/values (the ctors it copies).

;; Weak so a value's metadata is reclaimed with the value.
;;
;; A Chez hashtable is NOT thread-safe, and this one is written from whatever
;; thread calls with-meta on a record, reify or fn, or an op that carries such
;; meta forward. Unsynchronized mutation corrupts the table's internals, and the
;; corruption surfaces later as a SIGSEGV inside the collector (`nonrecoverable
;; invalid memory reference`, faulting in S_do_gc) or as a hang — never as an
;; error naming this table. So every WRITE goes through meta-table-mu.
;;
;; The READ does not take the mutex: assoc/conj on a record threads its meta
;; forward through here, so a lock on the read is a lock on every record op in
;; every thread (the collections used to read this table on every op too — eight
;; threads doing assoc on their own maps ran 32x slower per thread than one).
;;
;; A lock-free read of an eq-hashtable racing a locked writer is memory-safe (a
;; resize re-threads the existing cells; a walker mid-chain lands in a valid
;; chain and terminates) but it can MISS the key it is looking for while the
;; buckets are being rebuilt, and a missed entry here is metadata silently
;; dropped from a conj. So the read is a seqlock: meta-gen is odd while a writer
;; is inside and even otherwise, and a reader whose generation changed across
;; its lookup, or that saw an odd one, repeats the lookup under the mutex. The
;; fences order the generation stores against the table mutation on a weakly
;; ordered machine (arm64): without them a reader could see the new generation
;; before the bucket the writer moved. A writer's fences are unconditional (a
;; with-meta is rare); a reader's are skipped until a second thread exists.
;;
;; The empty-table fast path reads hashtable-size, which Chez keeps current for
;; a weak table as the collector drops entries (100k dead keys read 0 after a
;; collection), so a program whose record metadata is all gone again skips the
;; lookup.
(define meta-table (make-weak-eq-hashtable))
(define meta-table-mu (make-mutex))
(define meta-gen (box 0))
(define (meta-gen-bump!) (set-box! meta-gen (fx+ 1 (unbox meta-gen))))
(define (meta-table-set! k v)
  (jolt-with-mutex meta-table-mu
    (meta-gen-bump!) (memory-order-release)
    (hashtable-set! meta-table k v)
    (memory-order-release) (meta-gen-bump!)))
(define (meta-table-del! k)
  (jolt-with-mutex meta-table-mu
    (meta-gen-bump!) (memory-order-release)
    (hashtable-delete! meta-table k)
    (memory-order-release) (meta-gen-bump!)))
;; Single-threaded (jolt-mt? #f, values.ss) there is no writer to race and the
;; plain lookup is the whole read; the fences and the generation check are for
;; a process that has started a second thread.
(define (meta-table-get k)
  (if (not jolt-mt?)
      (hashtable-ref meta-table k #f)
      (let ((g (unbox meta-gen)))
        (if (fxodd? g)
            (jolt-with-mutex meta-table-mu (hashtable-ref meta-table k #f))
            (begin
              (memory-order-acquire)
              (let ((v (hashtable-ref meta-table k #f)))
                (memory-order-acquire)
                (if (fx=? g (unbox meta-gen))
                    v
                    (jolt-with-mutex meta-table-mu (hashtable-ref meta-table k #f)))))))))
(define (meta-table-empty?) (fx=? 0 (hashtable-size meta-table)))

;; The meta slot of a collection that has one: jolt-nil or a map. #f when X is
;; not one of those kinds — jolt-nil is a record and so truthy, which is what
;; lets a cond arm test the slot and read it in one step. Kinds in order of how
;; often an op carries: this runs on every assoc/conj/into.
(define (coll-meta x)
  (cond ((pmap? x) (pmap-meta x))
        ((pvec? x) (pvec-meta x))
        ((cseq? x) (cseq-meta x))
        ((pset? x) (pset-meta x))
        ((jolt-lazyseq? x) (jolt-lazyseq-meta x))
        ((empty-list-t? x) (empty-list-t-meta x))
        (else #f)))
;; Fill the slot of an instance nobody else holds yet (see the header). The
;; callers: coll-with-meta's copy below, and state-image.ss's rebuilt objects.
(define (coll-meta-set! x m)
  (cond ((pvec? x) (pvec-meta-set! x m))
        ((pmap? x) (pmap-meta-set! x m))
        ((pset? x) (pset-meta-set! x m))
        ((cseq? x) (cseq-meta-set! x m))
        ((empty-list-t? x) (empty-list-t-meta-set! x m))
        ((jolt-lazyseq? x) (jolt-lazyseq-meta-set! x m))
        (else (error 'coll-meta-set! "not a collection with a meta slot" x))))
;; A fresh copy of X carrying M — the withMeta constructors. The copy shares the
;; structure (trie, slot vector, cell head and tail) and the cached hasheq (meta
;; does not hash): one allocation, no traversal.
(define (coll-with-meta x m)
  (cond
    ((pvec? x) (%mk-pvec (pvec-cnt x) (pvec-shift x) (pvec-root x) (pvec-tail x) (pvec-ent x) (pvec-hasheq x) m))
    ((pmap? x) (%mk-pmap (pmap-root x) (pmap-cnt x) (pmap-hasheq x) m))
    ((pset? x) (%mk-pset (pset-m x) (pset-hasheq x) m))
    ;; Cons.withMeta is new Cons(meta, _first, _more): the copy SHARES the rest.
    ;; A cell's tail is its own published word (seq.ss), so a tail still pending
    ;; -- a thunk or a lazy-src descriptor -- cannot be copied as it stands:
    ;; each cell would run it, and (with-meta (seq (map f xs)) m) then called f
    ;; once per element for the original and again for the copy. The copy's
    ;; tail is instead a descriptor that forces X and takes ITS answer (lz-rest,
    ;; the producer jolt-rest already registers, so the copy still dumps to an
    ;; image), and the thunk runs once, in X, whichever cell is walked first. A
    ;; realized tail, or a cvec cell's #f (computed from its own fields, no
    ;; thunk to run twice), is shared as it is. A vector-backed cell stays one, with
    ;; its chunk fields, so the copy walks the same way.
    ((cseq? x)
     (let* ((t (cseq-tail x))
            (t2 (if (force-pending? t) (make-lazy-src lz-rest x #f) t)))
       (if (cseqv? x)
           (make-cseqv (cseq-head x) (if t t2 t) (cseq-kind x) m (cseq-cvec x) (cseq-ci x) (cseq-crest x))
           (make-cseq (cseq-head x) t2 (cseq-kind x) m))))
    ((empty-list-t? x) (make-empty-list-t m))
    ;; LazySeq.withMeta is new LazySeq(meta, seq()): the copy is REALIZED and
    ;; shares the forced seq, so the body runs once for both, and forcing X here
    ;; raises whatever its body raises, as seq() would. The copy is taken after
    ;; the force, when the thunk word holds the answer and the mirror fields
    ;; agree with it (lazy-bridge.ss).
    ((jolt-lazyseq? x)
     (jolt-seq x)
     (make-jolt-lazyseq (jolt-lazyseq-thunk x) (jolt-lazyseq-val x) #f m))
    (else (error 'coll-with-meta "not a collection with a meta slot" x))))

(define (jolt-meta x)
  (cond
    ((symbol-t? x) (let ((m (symbol-t-meta x))) (if (jolt-nil? m) jolt-nil m)))
    ((coll-meta x) => (lambda (m) m))
    ;; a var's meta is {:ns :name} (derived from the cell) + :macro true for a
    ;; macro var (derived from the cell's macro? field, like Var.isMacro reading meta on
    ;; the JVM) + any def-time user meta from the cell's meta field. :ns is the
    ;; Namespace VALUE (intern-ns! of the cell's ns string), like the JVM, so
    ;; (class (:ns (meta #'x))) is clojure.lang.Namespace; :name is the symbol.
     ((var-cell? x)
      (let* ((user (var-cell-meta x))
             (base (jolt-assoc (if user user (jolt-hash-map))
                               jolt-kw-var-ns (intern-ns! (var-cell-ns x))
                               jolt-kw-var-name (jolt-symbol #f (var-cell-name x)))))
       (if (macro-var? x)
           (jolt-assoc base jolt-kw-var-macro #t)
           base)))
    ;; a deftype implementing clojure.lang.IObj stores meta in a field and threads
    ;; it through its own assoc/withMeta (core.logic's Substitutions/LVar/LCons),
    ;; so dispatch to its meta method rather than the identity side-table — which
    ;; the deftype's reconstructed instances would not share.
    ((and (jrec? x) (jrec-cl x "meta")) => (lambda (m) (jolt-invoke m x)))
    ;; everything else (records, fns, reify, atoms/agents and any reference type)
    ;; reads the identity side-table; a value with no entry is nil meta.
    ;; no such metadata anywhere: skip the table entirely
    ((meta-table-empty?) jolt-nil)
    (else (or (meta-table-get x) jolt-nil))))

;; fresh-identity copy of a side-table value (so attaching meta doesn't mutate
;; the original). The copy only needs a distinct identity for the side-table;
;; a jrec shares its internal structure with the source (one allocation, no
;; traversal). host-table.ss extends this to a sorted collection's table. A
;; procedure can't be copied meaningfully — keyed in place.
(define (meta-copy x)
  (cond
    ((jrec? x) (make-jrec-from-existing x #f #f (jrec-ext x)))
    ;; a reify shares its (read-only) method table + protos but gets a fresh
    ;; identity, so attaching meta leaves the original's meta untouched. Every
    ;; Clojure reify implements IObj.
    ((jreify? x) (make-jreify (jreify-methods x) (jreify-protos x) (jreify-delegate x)))
    (else x)))                          ; procedure

;; Obj.withMeta returns `this` when the metadata is unchanged, and the JVM
;; compares it by IDENTITY, not by =: (with-meta v (meta v)) is v, and so is
;; (with-meta v nil) for a v with no metadata, but (with-meta v {:a 1}) on a v
;; whose meta is an equal-but-distinct {:a 1} still allocates. Without the
;; shortcut every (with-meta coll nil) was a fresh value, which is what made
;; clojure.core/set's (with-meta coll nil) fast path unusable here.
;;
;; The test is per-arm rather than a leading clause: a non-IObj value also has
;; nil metadata, so a leading (eq? (jolt-meta x) m) would return 5 from
;; (with-meta 5 nil) instead of throwing.
(define (jolt-with-meta x m)
  (cond
    ((symbol-t? x) (if (eq? (jolt-meta x) m) x (symbol-t-with-meta x m)))
    ((coll-meta x) => (lambda (cur) (if (eq? cur m) x (coll-with-meta x m))))
    ;; a deftype with an explicit clojure.lang.IObj withMeta carries meta in a
    ;; field; dispatch to it (see jolt-meta) so the meta survives reconstruction.
    ((and (jrec? x) (jrec-cl x "withMeta")) => (lambda (meth) (jolt-invoke meth x m)))
    ((or (jrec? x) (jreify? x) (procedure? x))
     (if (eq? (jolt-meta x) m)
         x
         (let ((c (meta-copy x)))
           (if (jolt-nil? m) (meta-table-del! c) (meta-table-set! c m))
           c)))
    (else (throw-jvm (quote ClassCastException) (string-append (jolt-final-str x) " cannot be cast to clojure.lang.IObj")))))

(def-var! "clojure.core" "meta" jolt-meta)
(def-var! "clojure.core" "with-meta" jolt-with-meta)

;; Carry SRC's metadata onto DST (a freshly-built collection of the same kind),
;; as Clojure's ops do — each new collection threads its receiver's meta()
;; forward. Returns the collection to use: DST itself when there is nothing to
;; carry or it already carries it (an op that answered its receiver, or a
;; shared empty), else DST with the meta attached. For a collection with a meta
;; slot that is a copy: DST may be shared — the () singleton, a list's tail
;; node — and a copy is what keeps an op from changing what everyone else
;; holding DST reads. A side-table DST (a record, a sorted collection) is keyed
;; as it is, as it always was.
;;
;; This runs on every assoc/conj/into, and on this path a call to a top-level
;; procedure is 4-6 ns (measured: a carry through coll-meta cost a list conj
;; 12 ns, the same test open-coded 5). So the no-meta answer is ONE call: the
;; kind test is the record predicate (open-coded by the compiler), the slot
;; read a field, in order of how often each kind carries; only the side-table
;; kinds and the rare attach go on to a second call.
(define-syntax slot-carry
  (syntax-rules ()
    ((_ slot dst) (let ((m slot)) (if (eq? m jolt-nil) dst (meta-attach dst m))))))
(define (meta-carry src dst)
  (cond ((pmap? src) (slot-carry (pmap-meta src) dst))
        ((pvec? src) (slot-carry (pvec-meta src) dst))
        ((cseq? src) (slot-carry (cseq-meta src) dst))
        ((pset? src) (slot-carry (pset-meta src) dst))
        ((jolt-lazyseq? src) (slot-carry (jolt-lazyseq-meta src) dst))
        ((empty-list-t? src) (slot-carry (empty-list-t-meta src) dst))
        ((meta-table-empty?) dst)
        (else (let ((m (meta-table-get src))) (if m (meta-attach dst m) dst)))))
(define (meta-attach dst m)
  (let ((cur (coll-meta dst)))
    (cond (cur (if (eq? cur m) dst (coll-with-meta dst m)))
          (else (meta-table-set! dst m) dst))))
;; conj's carry: the receiver kinds whose JVM cons threads meta are the
;; collections and a PersistentList — a list cell, and (), whose cons builds a
;; list with its meta. A Cons, a LazySeq or a vector's seq cons through
;; ASeq.cons -> new Cons(o, this), with none. Same one-call shape as above; the
;; cell arm leads because conj onto a list has the lowest floor (16 ns), where
;; one failed test ahead of it was a measured 6 ns.
(define (meta-carry-conj src dst)
  (cond ((cseq? src)
         ;; the slot first: the kind only matters once there is meta to carry
         (let ((m (cseq-meta src)))
           (if (or (eq? m jolt-nil) (not (fx=? (cseq-kind src) sk-list))) dst (meta-attach dst m))))
        ((pvec? src) (slot-carry (pvec-meta src) dst))
        ((pmap? src) (slot-carry (pmap-meta src) dst))
        ((pset? src) (slot-carry (pset-meta src) dst))
        ((jolt-lazyseq? src) dst)
        ((empty-list-t? src) (slot-carry (empty-list-t-meta src) dst))
        (else (meta-carry src dst))))

;; (type x) — Clojure's (or (:type (meta x)) (class x)). With no JVM classes the
;; "class" is a host taxonomy: a record yields its ns-qualified class-name SYMBOL
;; (user.TyR), everything else a keyword (:number/:vector/:seq/…).
;; MUST be total — a non-record value
;; falling through to a crash would read as a divergence, not the right keyword.
;; Forward refs (jolt-lazyseq?, the sorted-htable / wrapper predicates) all bind by
;; call time (every host .ss loads before any user expr runs).
(define ty-kw-type (keyword #f "type"))           ; the :type meta key
(define ty-kw-jtype (keyword "jolt" "type"))       ; tagged-map discriminator (ex-info)
(define ty-number (keyword #f "number"))
(define ty-string (keyword #f "string"))
(define ty-keyword (keyword #f "keyword"))
(define ty-symbol (keyword #f "symbol"))
(define ty-boolean (keyword #f "boolean"))
(define ty-char (keyword #f "char"))
(define ty-vector (keyword #f "vector"))
(define ty-map (keyword #f "map"))
(define ty-set (keyword #f "set"))
(define ty-seq (keyword #f "seq"))
(define ty-fn (keyword #f "fn"))
(define ty-atom (keyword "jolt" "atom"))
(define ty-volatile (keyword "jolt" "volatile"))
(define ty-regex (keyword "jolt" "regex"))
(define ty-var (keyword "jolt" "var"))
(define ty-transient (keyword "jolt" "transient"))
(define ty-uuid (keyword "jolt" "uuid"))
(define ty-sorted-set (keyword "jolt" "sorted-set"))
(define ty-object (keyword #f "object"))

;; Arm registry for host-type extensions (jinst, jolt-array, jfile, etc.)
;; A host shim registers its type's tag via register-type-arm! instead of
;; set!-wrapping jolt-type — disjoint types, checked before the base cases,
;; so the full behavior is gathered here plus the registry rather than
;; scattered across a set! chain (cf. register-hash-arm!).
;; Arms dispatch newest-registration-first: a later-loaded type's predicate
;; wins when predicates overlap (transients/records both answer some ops).
(define jolt-type-arms '())
(define (register-type-arm! pred handler)
  (set! jolt-type-arms (cons (cons pred handler) jolt-type-arms)))
(define (jolt-type-base x)
  (let* ((m (jolt-meta x))
         (override (if (jolt-nil? m) jolt-nil (jolt-get m ty-kw-type jolt-nil))))
    (cond
      ((not (jolt-nil? override)) override)            ; :type meta wins
      ;; record -> its ns-qualified class-name STRING (= (class x)). jolt models
      ;; classes as strings, so (symbol (str (type r))) is NOT (type r) — as on the
      ;; JVM where type is a Class, not a Symbol.
      ((jrec? x) (jrec-tag x))
      ((jolt-nil? x) jolt-nil)
      ((boolean? x) ty-boolean)
      ((number? x) ty-number)
      ((string? x) ty-string)
      ((keyword? x) ty-keyword)
      ((symbol-t? x) ty-symbol)
      ((char? x) ty-char)
      ;; host wrappers — keyed by their :jolt/* tags (checked before the
      ;; collection arms; none of these are pvec/pmap/pset).
      ((jolt-atom? x) ty-atom)
      ((jvol? x) ty-volatile)
      ((jolt-regex? x) ty-regex)
      ((var-cell? x) ty-var)
      ((jolt-transient? x) ty-transient)
      ((juuid? x) ty-uuid)
      ((htable-sorted-set? x) ty-sorted-set)
      ((htable-sorted-map? x) ty-map)
      ;; collections — pvec INCLUDES map entries (:vector).
      ((pvec? x) ty-vector)
      ((pmap? x)                                        ; a :jolt/type-tagged map (ex-info) -> its tag
       (let ((t (jolt-get x ty-kw-jtype jolt-nil))) (if (jolt-nil? t) ty-map t)))
      ((pset? x) ty-set)
      ((or (cseq? x) (empty-list-t? x) (jolt-lazyseq? x)) ty-seq)
      ((procedure? x) ty-fn)
      (else ty-object))))
(define (jolt-type x)
  (let* ((m (jolt-meta x))
         (override (if (jolt-nil? m) jolt-nil (jolt-get m ty-kw-type jolt-nil))))
    (cond
      ((not (jolt-nil? override)) override)             ; :type meta wins
      (else (let loop ((as jolt-type-arms))
              (cond ((null? as) (jolt-type-base x))
                    (((caar as) x) ((cdar as) x))
                    (else (loop (cdr as)))))))))

;; jolt-type is the keyword TAXONOMY (:string/:set/:jolt/inst/…) — jolt's native
;; value model, with no JVM in it. print-method/print-dup dispatch on it (via
;; __type-tag). The PUBLIC clojure.core/type is Clojure's (or (:type meta) (class
;; x)) — a JVM class — but that mapping belongs to the java host layer (host-class.ss
;; rebinds `type` next to `class`), so this core layer stays JVM-free.
(def-var! "clojure.core" "__type-tag" jolt-type)
(def-var! "clojure.core" "type" jolt-type)
