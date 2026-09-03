;; transients — mutable backing per collection kind, snapshotted to the immutable
;; collection on persistent!. conj!/assoc!/dissoc!/disj!/pop! mutate in place
;; (amortized O(1)); persistent! converts back to a pvec / pmap / pset once.
;;
;;   vec : a growable Scheme vector (capacity) + a fill count `n`. conj!/pop! are
;;         O(1) amortized — the old copy-on-write rebuilt the whole vector per op,
;;         so building an N-vector was O(N^2).
;;   map : an EDITABLE HAMT (collections.ss enode) sharing the source map's root
;;         — a write claims each node on its path once and then mutates in
;;         place, so persistent! only has to freeze the claimed spine. An
;;         array-mode map rides a k/v slot buffer instead; see below.
;;   set : the same editable HAMT, each element stored as both key and value, so
;;         a lookup answers with the stored element (which carries the metadata)
;;         and not the equal key it was probed with — the JVM's
;;         ITransientMap-of-val->val.
;;   cow : fallback for anything else (e.g. a sorted coll) — copy-on-write over
;;         the persistent ops, preserving jolt's superset of Clojure's transients.
;;
;; get/count/contains?/nth see THROUGH a transient (frequencies/group-by read a
;; transient map; a transient is callable). vector? on a transient is false (it's
;; this record, not a pvec), which group-by relies on. Loaded after collections.ss
;; (persistent ops + key-hash) and converters.ss.

;; A transient MAP has two modes, and `buf` says which: an editable HAMT root
;; (an enode, or the source's hnode until the first write claims it) means
;; HASH mode; anything else is the k/v slot buffer of ARRAY mode — the source
;; map's slots copied with spare capacity, TransientArrayMap's Object[]. (The
;; test is on the node types, never vector?: a target may represent records
;; as vectors.) In both `n` is the entry count (array mode uses slots 0 .. 2n-1). A
;; transient SET is always hash mode, like PersistentHashSet. For a vector `n`
;; is the element count. `ord` is unused.
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
     (let* ((v (pvec-v coll)) (cnt (vector-length v)) (cap (fxmax tvec-min-cap cnt))
            (buf (make-vector cap jolt-nil)))
       (sa-vector-copy-range! buf 0 v 0 cnt)
       (make-jolt-transient 'vec buf cnt #t #f)))
    ((pmap? coll)
     ;; The source's mode rides along. An array-mode map's slots are copied
     ;; into a buffer of max(16, its own length) slots and it comes back an
     ;; array map; a hash-mode map hands its ROOT straight over — nothing is
     ;; copied here, the first write into a node claims it.
     (let ((root (pmap-root coll)))
       (if (hnode? root)
           (make-jolt-transient 'map root (pmap-cnt coll) #t #f)
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
     (make-jolt-transient 'set (pmap-root (pset-m coll)) (pset-count coll) #t #f))
    ;; a deftype implementing clojure.lang.IEditableCollection.asTransient
    ;; (flatland's OrderedMap/OrderedSet) returns its OWN transient type, which
    ;; drives its declared ITransient* methods — not the copy-on-write wrapper.
    ;; find-method-any-protocol is a forward ref to records.ss (bound by call time).
    ((and (jrec? coll) (find-method-any-protocol (jrec-tag coll) "asTransient"))
     => (lambda (m) (jolt-invoke m coll)))
    ;; RFC 0003: any COLLECTION transients (the sorted/list/seq superset rides
    ;; the copy-on-write fallback); a non-collection is the JVM's cast failure.
    ((or (cseq? coll) (empty-list-t? coll) (jolt-lazyseq? coll)
         (htable? coll) (jrec? coll))
     (make-jolt-transient 'cow coll 0 #t #f))
    (else
     (jolt-throw (jolt-host-throwable
                  "java.lang.ClassCastException"
                  (string-append "class " (guard (e (#t "?")) (jolt-class-name coll))
                                 " cannot be cast to class clojure.lang.IEditableCollection"))))))

;; --- hash mode: write straight into the editable HAMT ------------------------
;; Claim the root on the way in; every node below is claimed as the write
;; descends through it (enode-assoc!).
(define (thash-put! t k v)
  (let ((root (enode-claim (jolt-transient-buf t))) (added (box #f)))
    (jolt-transient-buf-set! t root)
    (enode-assoc! root 0 (key-hash k) k v added)
    (when (unbox added) (jolt-transient-n-set! t (fx+ (jolt-transient-n t) 1)))))
(define (thash-del! t k)
  (let ((root (enode-claim (jolt-transient-buf t))) (removed (box #f)))
    (jolt-transient-buf-set! t root)
    (enode-dissoc! root 0 (key-hash k) k removed)
    (when (unbox removed) (jolt-transient-n-set! t (fx- (jolt-transient-n t) 1)))))
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
        (root (enode-empty)) (added (box #f)))
    (let loop ((i 0))
      (when (fx<? i len)
        (let ((k (vector-ref buf i)))
          (enode-assoc! root 0 (key-hash k) k (vector-ref buf (fx+ i 1)) added))
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
     (let ((buf (jolt-transient-buf t)) (cnt (jolt-transient-n t)))
       ;; exact fit: hand off the buffer (no other reference exists, and the
       ;; transient is now inactive so it can't mutate it). Else trim to size —
       ;; a pvec's backing length must equal its count.
       (if (fx=? cnt (vector-length buf))
           (make-pvec buf)
           (make-pvec (vec-copy-range buf 0 cnt)))))
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
(define (tvec-ensure! t need)            ; grow capacity to >= need by doubling
  (let ((buf (jolt-transient-buf t)))
    (when (fx>? need (vector-length buf))
      (let* ((ncap (let grow ((c (fxmax tvec-min-cap (vector-length buf)))) (if (fx>=? c need) c (grow (fx* 2 c)))))
             (nbuf (make-vector ncap jolt-nil)) (cnt (jolt-transient-n t)))
        (sa-vector-copy-range! nbuf 0 buf 0 cnt)
        (jolt-transient-buf-set! t nbuf)))))
(define (tvec-conj1! t x)
  (let ((cnt (jolt-transient-n t)))
    (tvec-ensure! t (fx+ cnt 1))
    (vector-set! (jolt-transient-buf t) cnt x)
    (jolt-transient-n-set! t (fx+ cnt 1))))
(define (tvec-assoc1! t i x)
  (let ((i (->idx i)) (cnt (jolt-transient-n t)))
    (cond ((and (fixnum? i) (fx>=? i 0) (fx<? i cnt)) (vector-set! (jolt-transient-buf t) i x))
          ((and (fixnum? i) (fx=? i cnt)) (tvec-conj1! t x))
          (else (throw-jvm (quote IndexOutOfBoundsException) "assoc!: index out of bounds")))))
;; conj! onto a transient map: a [k v] pair (vector/map-entry) or a whole map.
(define (tmap-conj-entry! t x)
  (cond
    ((jolt-nil? x) #t)
    ;; a vector is one entry, so it must be a pair — the persistent conj's rule
    ((pvec? x) (if (fx=? 2 (pvec-count x))
                   (tmap-put! t (pvec-nth-d x 0 jolt-nil) (pvec-nth-d x 1 jolt-nil))
                   (throw-jvm 'IllegalArgumentException "Vector arg to map conj must be a pair")))
    ((pmap? x) (pmap-fold-fwd x (lambda (k v acc) (tmap-put! t k v) acc) 0))
    (else (throw-jvm (quote IllegalArgumentException) "conj!: a transient map takes a map entry or a map"))))

;; (conj!) -> fresh transient vector; (conj! coll) -> the 1-arity transducer-
;; completion identity (JVM: no transient check). (conj! t x ...) mutates t.
(define (jolt-conj! . args)
  (cond
    ((null? args) (jolt-transient-new (jolt-vector)))
    ((null? (cdr args)) (car args))
    (else
      (let ((t (car args)) (xs (cdr args)))
        (cond
          ((jrec-trans-method t "conj")
           => (lambda (m) (fold-left (lambda (acc x) (jolt-invoke m acc x)) t xs)))
          (else
        (jolt-trans-check t "conj!")
        (case (jolt-transient-kind t)
          ((vec) (for-each (lambda (x) (tvec-conj1! t x)) xs))
          ((set) (for-each (lambda (x) (thash-put! t x x)) xs))
          ((map) (for-each (lambda (x) (tmap-conj-entry! t x)) xs))
          (else (jolt-transient-buf-set! t (apply jolt-conj (jolt-transient-buf t) xs))))
        t))))))

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
    ((vec) (let ((cnt (jolt-transient-n t)))
             (if (fx=? cnt 0) (throw-jvm (quote IllegalStateException) "pop!: can't pop empty transient vector")
                 (jolt-transient-n-set! t (fx- cnt 1)))))
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
    ((vec) (let ((i (->idx k))) (if (tvec-in-bounds? t i) (vector-ref (jolt-transient-buf t) i) d)))
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
    ((vec) (tvec-in-bounds? t (->idx k)))
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
           (else (let ((c (jolt-transient-buf t))) (or (jrec? c) (htable? c))))))))

(def-var! "clojure.core" "transient" jolt-transient-new)
(def-var! "clojure.core" "transient?" jolt-transient?)
(def-var! "clojure.core" "persistent!" jolt-persistent!)
(def-var! "clojure.core" "conj!" jolt-conj!)
(def-var! "clojure.core" "assoc!" jolt-assoc!)
(def-var! "clojure.core" "dissoc!" jolt-dissoc!)
(def-var! "clojure.core" "disj!" jolt-disj!)
(def-var! "clojure.core" "pop!" jolt-pop!)
(def-var! "clojure.core" "disj" jolt-disj)
