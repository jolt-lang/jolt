;; transients — mutable backing per collection kind, snapshotted to the immutable
;; collection on persistent!. conj!/assoc!/dissoc!/disj!/pop! mutate in place
;; (amortized O(1)); persistent! converts back to a pvec / pmap / pset once.
;;
;;   vec : the SOURCE pvec as a shared, untouched base (in `ord`) + a growable
;;         Scheme vector of the elements conj!ed past it + the total count `n`.
;;         Creating the transient is O(1) like the JVM's TransientVector, so
;;         (into big-v small-xs) costs the xs, not the whole of big-v; conj! is
;;         O(1) amortized and persistent! appends the buffer onto the base a
;;         32-chunk at a time. assoc!/pop! inside the base are O(log32 n)
;;         persistent ops on it. A source that fits in one tail chunk is
;;         copied into the buffer outright (base #f).
;;   map : an EDITABLE HAMT (collections.ss enode) sharing the source map's root
;;         — the first edit to a populated trie copies one persistent path.
;;         Further writes claim nodes and mutate in place. An
;;         array-mode map rides a k/v slot buffer instead; see below.
;;   set : the same editable HAMT, each element stored as both key and value, so
;;         a lookup answers with the stored element (which carries the metadata)
;;         and not the equal key it was probed with — the JVM's
;;         ITransientMap-of-val->val.
;;   cow : fallback for a list, a seq or a record — copy-on-write over the
;;         persistent ops, preserving jolt's superset of Clojure's transients.
;;
;; get/count/contains?/nth see THROUGH a transient (frequencies/group-by read a
;; transient map; a transient is callable). vector? on a transient is false (it's
;; this record, not a pvec), which group-by relies on. Loaded after collections.ss
;; (persistent ops + key-hash) and converters.ss.

;; A transient MAP has two modes, and `buf` says which: a HAMT root (an enode
;; or a compact hnode) means HASH mode; anything else is the k/v slot buffer
;; of ARRAY mode — the source
;; map's slots copied with spare capacity, TransientArrayMap's Object[]. (The
;; test is on the node types, never vector?: a target may represent records
;; as vectors.) In both `n` is the entry count (array mode uses slots 0 .. 2n-1). A
;; transient SET is always hash mode, like PersistentHashSet. For a vector `n`
;; is the element count and `ord` the base pvec (or #f). A hash transient uses
;; `ord` to request a compact first edit when created from a populated trie.
;;
;; Array mode promotes to hash mode when a new key would grow the map past its
;; CAPACITY — TransientArrayMap's rule: max(8 entries, the source map's size),
;; fixed at creation and consulted regardless of the key's type (the keyword
;; extension is the persistent assoc path only, so (into {} kw-pairs) gives a
;; hash map at 9 entries like the JVM). Promotion is ONE-WAY, like
;; TransientArrayMap -> TransientHashMap. Deciding lazily at persistent! from
;; the final count instead let a transient that grew past the limit and shrank
;; back come down an array map, where the JVM gives a hash map.
(define-record-type jolt-transient
  (fields kind (mutable buf) (mutable n) (mutable active) (mutable ord))
  (nongenerative jolt-transient-v3))

(define tvec-min-cap 8)

(define (jolt-transient-new coll)
  (cond
    ((pvec? coll)
     (let ((cnt (pvec-cnt coll)))
       (if (fx<=? cnt pv-width)
           (let ((buf (make-vector (fxmax tvec-min-cap cnt) jolt-nil)))
             (sa-vector-copy-range! buf 0 (pvec-v coll) 0 cnt)
             (make-jolt-transient 'vec buf cnt #t #f))
           (make-jolt-transient 'vec (make-vector tvec-min-cap jolt-nil) cnt #t coll))))
    ((pmap? coll)
     ;; The source's mode rides along. An array-mode map's slots are copied
     ;; into a buffer of max(16, its own length) slots and it comes back an
     ;; array map; a hash-mode map hands its ROOT straight over — nothing is
     ;; copied until the first edit.
     (let ((root (pmap-root coll)))
       (if (hnode? root)
           (make-jolt-transient 'map root (pmap-cnt coll) #t (fx>? (pmap-cnt coll) 0))
           (let* ((len (vector-length root))
                  (buf (make-vector (fxmax (fx* 2 array-map-limit) len) #f)))
             (sa-vector-copy-range! buf 0 root 0 len)
             (make-jolt-transient 'map buf (pmap-cnt coll) #t #f)))))
    ;; A set stores each element as both key and value, so a lookup answers with
    ;; the element the set HOLDS rather than the equal key it was probed with
    ;; (JVM TransientHashSet is likewise an ITransientMap of val->val), and
    ;; conj! of an equal element keeps the stored key while overwriting the
    ;; value — which is what enode-assoc!'s leaf replace does.
    ((pset? coll)
     (make-jolt-transient 'set (pmap-root (pset-m coll)) (pset-count coll) #t (fx>? (pset-count coll) 0)))
    ;; a deftype implementing clojure.lang.IEditableCollection.asTransient
    ;; (flatland's OrderedMap/OrderedSet) returns its OWN transient type, which
    ;; drives its declared ITransient* methods — not the copy-on-write wrapper.
    ;; find-method-any-protocol is a forward ref to records.ss (bound by call time).
    ((and (jrec? coll) (find-method-any-protocol (jrec-tag coll) "asTransient"))
     => (lambda (m) (jolt-invoke m coll)))
    ;; RFC 0003: a list or seq transients too (jolt's superset rides the
    ;; copy-on-write fallback); anything else is the JVM's cast failure. That
    ;; includes a sorted map or set: PersistentTreeMap/Set are not
    ;; IEditableCollection, and the reference has no sorted transient.
    ((or (cseq? coll) (empty-list-t? coll) (jolt-lazyseq? coll) (jrec? coll))
     (make-jolt-transient 'cow coll 0 #t #f))
    (else
     (jolt-throw (jolt-host-throwable
                  "java.lang.ClassCastException"
                  (string-append "class " (guard (e (#t "?")) (jolt-class-name coll))
                                 " cannot be cast to class clojure.lang.IEditableCollection"))))))

(define-syntax thash-count-add!
  (syntax-rules ()
    ((_ t delta) (jolt-transient-n-set! t (fx+ (jolt-transient-n t) delta)))))

;; One persistent path copy needs no freeze. After the first write, subsequent
;; edits use the usual editable trie. Empty builds enter that path immediately.
(define (thash-edit-compact! t root h k v put?)
  (let* ((changed (box #f))
         (next (if put? (node-assoc root 0 h k v changed) (node-dissoc root 0 h k changed))))
    (cond
      ;; A user equality/hash method may have edited t during the traversal.
      ;; Apply the outer edit to its current root, retaining the computed hash.
      ((not (eq? root (jolt-transient-buf t)))
       (if put? (thash-put-hashed! t h k v) (thash-del-hashed! t h k)))
      (else
       (jolt-transient-buf-set! t next)
       (jolt-transient-ord-set! t #f)
       (when (unbox changed) (thash-count-add! t (if put? 1 -1)))))))

(define (thash-put-hashed! t h k v)
  (let ((root (jolt-transient-buf t)))
    (cond
      ((enode? root)
       (when (enode-assoc! root 0 h k v) (thash-count-add! t 1)))
      ((jolt-transient-ord t) (thash-edit-compact! t root h k v #t))
      (else
       (let ((editable (hnode->enode root)))
         (jolt-transient-buf-set! t editable)
         (when (enode-assoc! editable 0 h k v) (thash-count-add! t 1)))))))
(define (thash-put! t k v) (thash-put-hashed! t (key-hash k) k v))

(define (thash-del-hashed! t h k)
  (let ((root (jolt-transient-buf t)))
    (if (jolt-transient-ord t)
        (thash-edit-compact! t root h k #f #f)
        (let ((editable (enode-claim root)) (removed (box #f)))
          (jolt-transient-buf-set! t editable)
          (enode-dissoc! editable 0 h k removed)
          (when (unbox removed) (thash-count-add! t -1))))))
(define (thash-del! t k) (thash-del-hashed! t (key-hash k) k))
(define (thash-get t k d) (enode-get (jolt-transient-buf t) 0 (key-hash k) k d))

;; --- array mode, and the one-way promotion out of it -------------------------
(define (tmap-array? t)
  (let ((b (jolt-transient-buf t))) (not (or (enode? b) (hnode? b)))))
(define (jolt-transient-array-map? t)
  (and (eq? (jolt-transient-kind t) 'map) (tmap-array? t)))
;; the used slots of an array-mode buffer -> an editable root; `n` stays the
;; entry count, the keys are distinct already
(define (tmap-promote! t)
  (let ((buf (jolt-transient-buf t)) (len (fx* 2 (jolt-transient-n t)))
        (root (enode-empty)))
    (let loop ((i 0))
      (when (fx<? i len)
        (let ((k (vector-ref buf i)))
          (enode-assoc! root 0 (key-hash k) k (vector-ref buf (fx+ i 1))))
        (loop (fx+ i 2))))
    (jolt-transient-buf-set! t root)))

;; map put/delete: hash mode goes straight to the HAMT; array mode writes the
;; slot buffer in place — a held key takes the new value, a new key appends
;; while the buffer has room, and one that does not fit promotes first.
(define (tmap-put! t k v)
  (if (tmap-array? t)
      (let* ((buf (jolt-transient-buf t)) (n (jolt-transient-n t)) (len (fx* 2 n))
             (i (amap-index buf len k)))
        (cond
          ((fx>=? i 0) (vector-set! buf (fx+ i 1) v))
          ((fx<? len (vector-length buf))
           (vector-set! buf len k) (vector-set! buf (fx+ len 1) v)
           (jolt-transient-n-set! t (fx+ n 1)))
          (else (tmap-promote! t) (thash-put! t k v))))
      (thash-put! t k v)))
;; TransientArrayMap.doWithout moves the LAST entry into the removed slot (array
;; compaction), so removal is order-visible.
(define (tmap-del! t k)
  (if (tmap-array? t)
      (let* ((buf (jolt-transient-buf t)) (n (jolt-transient-n t))
             (i (amap-index buf (fx* 2 n) k)))
        (when (fx>=? i 0)
          (let ((last (fx* 2 (fx- n 1))))
            (vector-set! buf i (vector-ref buf last))
            (vector-set! buf (fx+ i 1) (vector-ref buf (fx+ last 1)))
            (vector-set! buf last #f)
            (vector-set! buf (fx+ last 1) #f)
            (jolt-transient-n-set! t (fx- n 1)))))
      (thash-del! t k)))

(define (jolt-trans-check t who)
  (unless (jolt-transient? t) (throw-jvm (quote ClassCastException) (string-append who ": not a transient")))
  (unless (jolt-transient-active t)
    (jolt-throw (jolt-host-throwable "java.lang.IllegalAccessError"
                  (string-append who ": transient used after persistent!")))))

;; --- persistent! : snapshot back to the immutable collection -----------------
;; A deftype implementing the clojure.lang.ITransient* interfaces (flatland's
;; TransientOrderedMap/Set) is a plain jrec, not a jolt-transient — the transient
;; ops route to its declared methods. jrec?/find-method-any-protocol/jolt-invoke
;; are forward refs bound by call time.
(define (jrec-trans-method t name) (and (jrec? t) (find-method-any-protocol (jrec-tag t) name)))

(define (jolt-persistent! t)
  (cond
    ((jrec-trans-method t "persistent") => (lambda (m) (jolt-invoke m t)))
    (else
  (jolt-trans-check t "persistent!")
  (jolt-transient-active-set! t #f)
  (case (jolt-transient-kind t)
    ((vec)
     (let ((buf (jolt-transient-buf t)) (cnt (jolt-transient-n t)) (base (jolt-transient-ord t)))
       (cond
         ;; based: the buffered tail goes onto the shared trie; the result is a
         ;; plain vector without the source's metadata or kind (the JVM's
         ;; persistent() builds a fresh PersistentVector, meta null)
         (base
          (let ((p (pvec-append-flat base buf (fx- cnt (pvec-cnt base)))))
            (%mk-pvec (pvec-cnt p) (pvec-shift p) (pvec-root p) (pvec-tail p) #f 0 jolt-nil)))
         ;; exact fit: hand off the buffer (no other reference exists, and the
         ;; transient is now inactive so it can't mutate it). Else trim to size —
         ;; a pvec's backing length must equal its count.
         ((fx=? cnt (vector-length buf)) (make-pvec buf))
         (else (make-pvec (vec-copy-range buf 0 cnt))))))
      ((map)
       (let ((buf (jolt-transient-buf t)) (n (jolt-transient-n t)))
         (if (not (or (enode? buf) (hnode? buf)))
             ;; still in array mode, so an array map is what it is — no threshold
             ;; check here any more: promotion already happened at write time.
             ;; Exact fit hands the buffer off (the transient is inactive now);
             ;; else the used slots are trimmed into the map's own vector.
             (make-pmap (if (fx=? (fx* 2 n) (vector-length buf)) buf (vec-copy-range buf 0 (fx* 2 n))) n)
             ;; hash mode: freeze the edited spine. No rehashing and no path
             ;; copying — untouched subtrees stay shared with the source map.
             (make-pmap (enode-freeze buf) n))))
    ;; The element/lookup-value split survives because the tree already holds it:
    ;; conj! of an element equal to one present keeps the stored key and
    ;; overwrites the value, so seq yields the first element and get the second.
    ((set)
     (make-pset (make-pmap (enode-freeze (jolt-transient-buf t)) (jolt-transient-n t))))
    (else (jolt-transient-buf t))))))

;; --- in-place mutation -------------------------------------------------------
;; A transient vector's index space: [0, off) is the base pvec, [off, n) the
;; buffer from slot 0.
(define (tvec-off t) (let ((base (jolt-transient-ord t))) (if base (pvec-cnt base) 0)))
(define (tvec-ref t i)                   ; i already in bounds
  (let ((off (tvec-off t)))
    (if (fx<? i off)
        (pvec-nth-in-range (jolt-transient-ord t) i)
        (vector-ref (jolt-transient-buf t) (fx- i off)))))
(define (tvec-ensure! t need)            ; grow capacity to >= need by doubling
  (let ((buf (jolt-transient-buf t)))
    (when (fx>? need (vector-length buf))
      (let* ((ncap (let grow ((c (fxmax tvec-min-cap (vector-length buf)))) (if (fx>=? c need) c (grow (fx* 2 c)))))
             (nbuf (make-vector ncap jolt-nil)))
        (sa-vector-copy-range! nbuf 0 buf 0 (fx- (jolt-transient-n t) (tvec-off t)))
        (jolt-transient-buf-set! t nbuf)))))
(define (tvec-conj1! t x)
  (let ((cnt (jolt-transient-n t)) (off (tvec-off t)))
    (tvec-ensure! t (fx+ (fx- cnt off) 1))
    (vector-set! (jolt-transient-buf t) (fx- cnt off) x)
    (jolt-transient-n-set! t (fx+ cnt 1))))

;; (reduce conj! t v) appends every element of a persistent vector. Keep the
;; shared base in place and reserve space once after the already buffered tail.
(define (tvec-append-pvec! t v)
  (let* ((cnt (jolt-transient-n t)) (used (fx- cnt (tvec-off t))) (n (pvec-cnt v)))
    (tvec-ensure! t (fx+ used n))
    (pvec-copy-into! (jolt-transient-buf t) used v)
    (jolt-transient-n-set! t (fx+ cnt n))
    t))

(define (tvec-assoc1! t i x)
  (let ((i (pvec-key-index! i)) (cnt (jolt-transient-n t)) (off (tvec-off t)))
    (cond ((and (fixnum? i) (fx>=? i 0) (fx<? i off))
           (jolt-transient-ord-set! t (pvec-assoc (jolt-transient-ord t) i x)))
          ((and (fixnum? i) (fx>=? i off) (fx<? i cnt)) (vector-set! (jolt-transient-buf t) (fx- i off) x))
          ((and (fixnum? i) (fx=? i cnt)) (tvec-conj1! t x))
          (else (throw-jvm (quote IndexOutOfBoundsException) "assoc!: index out of bounds")))))
(define (tvec-pop1! t)
  (let ((cnt (jolt-transient-n t)))
    (cond ((fx=? cnt 0) (throw-jvm (quote IllegalStateException) "pop!: can't pop empty transient vector"))
          ;; nothing buffered: the last element is the base's
          ((fx=? cnt (tvec-off t)) (jolt-transient-ord-set! t (pvec-pop (jolt-transient-ord t))))
          (else (vector-set! (jolt-transient-buf t) (fx- (fx- cnt (tvec-off t)) 1) jolt-nil)))
    (jolt-transient-n-set! t (fx- cnt 1))))
;; p with buf[0, k) appended: top the tail up to a full chunk in one copy, and
;; let pvec-conj push it into the trie (one path copy per 32 elements) — never
;; the per-element tail copy a conj loop would pay.
(define (pvec-append-flat p buf k)
  (let loop ((p p) (i 0))
    (if (fx=? i k)
        p
        (let* ((tail (pvec-tail p)) (tl (vector-length tail)))
          (if (fx<? tl pv-width)
              (let* ((m (fxmin (fx- pv-width tl) (fx- k i))) (nt (make-vector (fx+ tl m))))
                (sa-vector-copy-range! nt 0 tail 0 tl)
                (sa-vector-copy-range! nt tl buf i (fx+ i m))
                (loop (mk-pvec (fx+ (pvec-cnt p) m) (pvec-shift p) (pvec-root p) nt (pv-derived-ent p))
                      (fx+ i m)))
              (loop (pvec-conj p (vector-ref buf i)) (fx+ i 1)))))))
;; conj! onto a transient map: a [k v] pair (vector/map-entry) or a whole map.
(define (tmap-conj-entry! t x)
  (cond
    ((jolt-nil? x) #t)
    ;; a vector is one entry, so it must be a pair — the persistent conj's rule
    ((pvec? x)
     (unless (fx=? 2 (pvec-count x))
       (throw-jvm 'IllegalArgumentException "Vector arg to map conj must be a pair"))
     (let ((tail (pvec-tail x)))
       (if (fx=? 2 (vector-length tail))
           (tmap-put! t (vector-ref tail 0) (vector-ref tail 1))
           (tmap-put! t (pvec-nth-in-range x 0) (pvec-nth-in-range x 1)))))
    ((pmap? x) (pmap-fold-fwd x (lambda (k v acc) (tmap-put! t k v) acc) 0))
    ((jolt-host-entry x) => (lambda (kv) (tmap-put! t (car kv) (cdr kv))))
    (else (throw-jvm (quote IllegalArgumentException) "conj!: a transient map takes a map entry or a map"))))

(define-syntax define-pvec-inserter
  (syntax-rules ()
    ((_ (name t v x) insert!)
     (define (name t v)
       (let ((n (pvec-cnt v)))
         (let outer ((i 0))
           (if (fx=? i n) t
               (let-values (((leaf off) (pv-leaf-for v i)))
                 (let* ((run (fxmin (fx- n i) (fx- (vector-length leaf) off)))
                        (end (fx+ off run)))
                   (let inner ((j off))
                     (if (fx=? j end)
                         (outer (fx+ i run))
                         (let ((x (vector-ref leaf j)))
                           ;; User hash/equality code can retire t during a write.
                           (jolt-site-kept (begin (jolt-trans-check t "conj!") insert!))
                           (inner (fx+ j 1))))))))))))))

(define-pvec-inserter (tmap-conj-pvec! t v x) (tmap-conj-entry! t x))
(define-pvec-inserter (tset-conj-pvec! t v x) (thash-put! t x x))

(define (transient-pvec-inserter t)
  (and (jolt-transient? t) (jolt-transient-active t)
       (case (jolt-transient-kind t)
         ((vec) tvec-append-pvec!)
         ((map) tmap-conj-pvec!)
         ((set) tset-conj-pvec!)
         (else #f))))

;; (conj!) -> fresh transient vector; (conj! coll) -> the 1-arity transducer-
;; completion identity (JVM: no transient check). (conj! t x ...) mutates t.
;;
;; The two-argument call is every transient build's hot call — (reduce conj!
;; (transient []) coll), into, and the core fns written that way — and a single
;; `. args` signature charged each of them a rest list before doing anything
;; (32 of the 85 bytes a 26k-element build allocated per element). It is a fixed
;; clause now, with the one-element body the variadic clause folds over.
(define (jolt-conj-one! t x)
  (cond
    ((jrec-trans-method t "conj") => (lambda (m) (jolt-invoke m t x)))
    (else
     (jolt-trans-check t "conj!")
     (case (jolt-transient-kind t)
       ((vec) (tvec-conj1! t x))
       ((set) (thash-put! t x x))
       ((map) (tmap-conj-entry! t x))
       (else (jolt-transient-buf-set! t (jolt-conj (jolt-transient-buf t) x))))
     t)))
(define jolt-conj!
  (case-lambda
    (() (jolt-transient-new (jolt-vector)))
    ((t) t)
    ((t x) (jolt-conj-one! t x))
    ((t . xs) (fold-left jolt-conj-one! t xs))))

;; assoc! is variadic. JVM: a complete first key/val pair present (>=3 kvs) with a
;; trailing lone key fills nil; a lone key alone (1 kv) is a wrong-arity throw.
(define (assoc-pad kvs) (if (and (>= (length kvs) 3) (odd? (length kvs))) (append kvs (list jolt-nil)) kvs))
(define (jolt-assoc! t . kvs0)
  (cond
    ((jrec-trans-method t "assoc")
     => (lambda (m) (let lp ((xs (assoc-pad kvs0)))
                      (if (null? xs) t (begin (jolt-invoke m t (car xs) (cadr xs)) (lp (cddr xs)))))))
    (else
  (jolt-trans-check t "assoc!")
  (let ((kvs (assoc-pad kvs0)))
    (when (odd? (length kvs)) (throw-jvm (quote IllegalArgumentException) "assoc!: no value supplied for key"))
    (case (jolt-transient-kind t)
      ((map) (let lp ((xs kvs)) (unless (null? xs) (tmap-put! t (car xs) (cadr xs)) (lp (cddr xs)))))
      ((vec) (let lp ((xs kvs)) (unless (null? xs) (tvec-assoc1! t (car xs) (cadr xs)) (lp (cddr xs)))))
      (else (jolt-transient-buf-set! t (apply jolt-assoc (jolt-transient-buf t) kvs)))))
  t)))
(define (jolt-dissoc! t . ks)
  (cond
    ((jrec-trans-method t "without")
     => (lambda (m) (fold-left (lambda (acc k) (jolt-invoke m acc k)) t ks)))
    (else
  (jolt-trans-check t "dissoc!")
  (case (jolt-transient-kind t)
    ((map) (for-each (lambda (k) (tmap-del! t k)) ks))
    (else (jolt-transient-buf-set! t (apply jolt-dissoc (jolt-transient-buf t) ks))))
  t)))
(define (jolt-disj! t . xs)
  (cond
    ((jrec-trans-method t "disjoin")
     => (lambda (m) (fold-left (lambda (acc x) (jolt-invoke m acc x)) t xs)))
    (else
  (jolt-trans-check t "disj!")
  (case (jolt-transient-kind t)
    ((set) (for-each (lambda (x) (thash-del! t x)) xs))
    (else (jolt-transient-buf-set! t (apply jolt-disj (jolt-transient-buf t) xs))))
  t)))
(define (jolt-pop! t)
  (cond
    ((jrec-trans-method t "pop") => (lambda (m) (jolt-invoke m t)))
    (else
  (jolt-trans-check t "pop!")
  (case (jolt-transient-kind t)
    ((vec) (tvec-pop1! t))
    (else (jolt-transient-buf-set! t (jolt-pop (jolt-transient-buf t)))))
  t)))

;; persistent disj over sets (pset-disj already exists in collections.ss).
(define (jolt-disj s . xs)
  ;; (disj nil ...) is nil on the JVM (disj is otherwise set-only).
  (if (jolt-nil? s)
      jolt-nil
      (cond
        ((pset? s)
         (meta-carry s
           (let loop ((s s) (xs xs)) (if (null? xs) s (loop (pset-disj s (car xs)) (cdr xs))))))
        ;; a deftype implementing clojure.lang.IPersistentSet.disjoin (flatland's
        ;; OrderedSet) disjoins through its own method. jrec?/jrec-cl are forward
        ;; refs to records.ss (loaded after this file, bound by call time).
        ((and (jrec? s) (jrec-cl s "disjoin"))
         => (lambda (m) (meta-carry s (fold-left (lambda (acc x) (jolt-invoke m acc x)) s xs))))
        (else
         (jolt-throw (jolt-host-throwable "java.lang.ClassCastException"
                       (string-append "class " (guard (e (#t "?")) (jolt-class-name s))
                                      " cannot be cast to class clojure.lang.IPersistentSet")))))))

;; --- see-through accessors ---------------------------------------------------
;; The copy-on-write ('cow) transient kind delegates reads to the plain collection
;; ops on its wrapped immutable coll (never a transient itself, so no recursion
;; through the transient get/count/contains? arms). collections.ss defines these
;; before this file loads.
(define %prev-jolt-get jolt-get)
(define %prev-jolt-count jolt-count)
(define %prev-jolt-contains? jolt-contains?)
(define t-absent (list 'transient-absent))   ; unique missing-key sentinel
(define (tvec-in-bounds? t i) (and (fixnum? i) (fx>=? i 0) (fx<? i (jolt-transient-n t))))
(define (t-get t k d)
  (jolt-trans-check t "get")
  (case (jolt-transient-kind t)
    ((vec) (let ((i (vec-key-index k))) (if (and i (tvec-in-bounds? t i)) (tvec-ref t i) d)))
    ((map) (if (tmap-array? t)
               (let* ((buf (jolt-transient-buf t)) (i (amap-index buf (fx* 2 (jolt-transient-n t)) k)))
                 (if (fx<? i 0) d (vector-ref buf (fx+ i 1))))
               (thash-get t k d)))
    ;; the stored VALUE is the element the set holds, so this hands that back
    ;; rather than the equal key it was probed with
    ((set) (thash-get t k d))
    (else (%prev-jolt-get (jolt-transient-buf t) k d))))
(define (t-count t)
  (jolt-trans-check t "count")
  (case (jolt-transient-kind t)
    ((vec) (jolt-transient-n t))
    ((map) (jolt-transient-n t))
    ((set) (jolt-transient-n t))
    (else (%prev-jolt-count (jolt-transient-buf t)))))
(define (t-contains? t k)
  (jolt-trans-check t "contains?")
  (case (jolt-transient-kind t)
    ((vec) (let ((i (vec-key-index k))) (and i (tvec-in-bounds? t i))))
    ((map) (if (tmap-array? t)
               (fx>=? (amap-index (jolt-transient-buf t) (fx* 2 (jolt-transient-n t)) k) 0)
               (not (eq? t-absent (thash-get t k t-absent)))))
    ((set) (not (eq? t-absent (thash-get t k t-absent))))
    (else (%prev-jolt-contains? (jolt-transient-buf t) k))))

;; Redefine the native get/count/contains?/nth (captured first) so the existing
;; emit lowerings unwrap a transient; non-transients are untouched.
;; count/contains?/nth wrappers are collapsed into records.ss (loaded later) —
;; only the get-arm registration lives here.
(register-get-arm! jolt-transient? (lambda (coll k d) (t-get coll k d)))

;; Is this transient key-addressable — the JVM's ITransientAssociative2, which is
;; what RT.find accepts alongside Associative? A transient map or vector is; a
;; transient SET is not (find on one throws). The `cow` fallback is jolt's own
;; superset, so it answers for whatever it wraps.
(def-var! "jolt.host" "transient-associative?"
  (lambda (t)
    (and (jolt-transient? t)
         (case (jolt-transient-kind t)
           ((vec map) #t)
           ((set) #f)
           (else (jrec? (jolt-transient-buf t)))))))

(def-var! "clojure.core" "transient" jolt-transient-new)
(def-var! "clojure.core" "transient?" jolt-transient?)
(def-var! "clojure.core" "persistent!" jolt-persistent!)
(def-var! "clojure.core" "conj!" jolt-conj!)
(def-var! "clojure.core" "assoc!" jolt-assoc!)
(def-var! "clojure.core" "dissoc!" jolt-dissoc!)
(def-var! "clojure.core" "disj!" jolt-disj!)
(def-var! "clojure.core" "pop!" jolt-pop!)
(def-var! "clojure.core" "disj" jolt-disj)
