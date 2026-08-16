;; transients — mutable backing per collection kind, snapshotted to the immutable
;; collection on persistent!. conj!/assoc!/dissoc!/disj!/pop! mutate in place
;; (amortized O(1)); persistent! converts back to a pvec / pmap / pset once.
;;
;;   vec : a growable Scheme vector (capacity) + a fill count `n`. conj!/pop! are
;;         O(1) amortized — the old copy-on-write rebuilt the whole vector per op,
;;         so building an N-vector was O(N^2).
;;   map : an EDITABLE HAMT (collections.ss enode) sharing the source map's root
;;         — a write claims each node on its path once and then mutates in
;;         place, so persistent! only has to freeze the claimed spine. Small
;;         insertion-ordered maps ride a Chez hashtable instead; see below.
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

;; A transient MAP has two modes, and `ord` says which: a list means ARRAY mode
;; (small, insertion-ordered — `buf` is a hashtable and `ord` the reverse
;; insertion-order pair list), #f means HASH mode (`buf` is an editable HAMT root
;; and `n` the entry count). A transient SET is always hash mode, like
;; PersistentHashSet. For a vector `n` is the element count.
;;
;; Array mode promotes to hash mode on the write that makes an array map
;; impossible — the same pmap-array-keep? rule the persistent assoc path uses
;; (under 8 always; a keyword-only map grows to 64) — and the promotion is
;; ONE-WAY, like the JVM's TransientArrayMap -> TransientHashMap. Deciding
;; lazily at persistent! from the final count instead let a transient that grew
;; past the limit and shrank back come down an array map, where the JVM gives a
;; hash map.
(define-record-type jolt-transient
  (fields kind (mutable buf) (mutable n) (mutable active) (mutable ord))
  (nongenerative jolt-transient-v3))

(define tvec-min-cap 8)

(define (jolt-transient-new coll)
  (cond
    ((pvec? coll)
     (let* ((v (pvec-v coll)) (cnt (vector-length v)) (cap (fxmax tvec-min-cap cnt))
            (buf (make-vector cap jolt-nil)))
       (let loop ((i 0)) (when (fx<? i cnt) (vector-set! buf i (vector-ref v i)) (loop (fx+ i 1))))
       (make-jolt-transient 'vec buf cnt #t #f)))
    ((pmap? coll)
     ;; The source's mode rides along. An array-mode map keeps `ord` and comes
     ;; back an array map; a hash-mode map hands its ROOT straight over — nothing
     ;; is copied here, the first write into a node claims it.
     (if (pmap-order coll)
         (let ((ht (make-hashtable key-hash jolt=2)) (ord '()))
           ;; visit in iteration order so `ord` ends up reverse-insertion (persistent! reverses it back)
           (pmap-fold-fwd coll (lambda (k v acc) (hashtable-set! ht k v) (set! ord (cons (cons k v) ord)) acc) 0)
           (make-jolt-transient 'map ht (pmap-cnt coll) #t ord))
         (make-jolt-transient 'map (pmap-root coll) (pmap-cnt coll) #t #f)))
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
;; Rebuild the editable root from `ord` (at most 64 entries), then continue in
;; hash mode. Values come from the HASHTABLE, not from ord's cdrs: tmap-put!
;; only appends to `ord` for a NEW key, so a replaced key's pair there still
;; carries the old value and only its position is meaningful.
(define (tmap-promote! t)
  (let ((ht (jolt-transient-buf t)) (ord (jolt-transient-ord t))
        (root (enode-empty)) (cnt 0) (added (box #f)))
    (for-each (lambda (p)
                (let ((k (car p)))
                  (set-box! added #f)
                  (enode-assoc! root 0 (key-hash k) k (hashtable-ref ht k jolt-nil) added)
                  (when (unbox added) (set! cnt (fx+ cnt 1)))))
              (reverse ord))
    (jolt-transient-buf-set! t root)
    (jolt-transient-ord-set! t #f)
    (jolt-transient-n-set! t cnt)))

;; map put/delete: hash mode goes straight to the HAMT; array mode maintains the
;; reverse insertion-order list, promoting when a new key can no longer keep the
;; map in array mode.
(define (tmap-put! t k v)
  (if (not (jolt-transient-ord t))
      (thash-put! t k v)
      (let ((ht (jolt-transient-buf t)) (ord (jolt-transient-ord t)))
        (cond
          ((hashtable-contains? ht k) (hashtable-set! ht k v))
          ((pmap-array-keep? (hashtable-size ht) ord k #f)
           (jolt-transient-ord-set! t (cons (cons k v) ord))
           (hashtable-set! ht k v))
          (else (tmap-promote! t) (thash-put! t k v))))))
(define (tmap-del! t k)
  (if (not (jolt-transient-ord t))
      (thash-del! t k)
      (let ((ht (jolt-transient-buf t)))
        (when (hashtable-contains? ht k)
          (jolt-transient-ord-set! t (remove-key (jolt-transient-ord t) k)))
        (hashtable-delete! ht k))))

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
           (let ((out (make-vector cnt)))
             (let loop ((i 0))
               (if (fx<? i cnt) (begin (vector-set! out i (vector-ref buf i)) (loop (fx+ i 1)))
                   (make-pvec out)))))))
      ((map)
       (if (jolt-transient-ord t)
           ;; still in array mode, so an array map is what it is — no threshold
           ;; check here any more: promotion already happened at write time.
           (let ((ht (jolt-transient-buf t)) (m empty-pmap))
             (for-each (lambda (p) (set! m (pmap-put-ordered m (car p) (hashtable-ref ht (car p) jolt-nil))))
                       (reverse (jolt-transient-ord t)))
             m)
           ;; hash mode: freeze the edited spine. No rehashing and no path
           ;; copying — untouched subtrees stay shared with the source map.
           (make-pmap (enode-freeze (jolt-transient-buf t)) (jolt-transient-n t) #f)))
    ;; The element/lookup-value split survives because the tree already holds it:
    ;; conj! of an element equal to one present keeps the stored key and
    ;; overwrites the value, so seq yields the first element and get the second.
    ((set)
     (make-pset (make-pmap (enode-freeze (jolt-transient-buf t)) (jolt-transient-n t) #f)))
    (else (jolt-transient-buf t))))))

;; --- in-place mutation -------------------------------------------------------
(define (tvec-ensure! t need)            ; grow capacity to >= need by doubling
  (let ((buf (jolt-transient-buf t)))
    (when (fx>? need (vector-length buf))
      (let* ((ncap (let grow ((c (fxmax tvec-min-cap (vector-length buf)))) (if (fx>=? c need) c (grow (fx* 2 c)))))
             (nbuf (make-vector ncap jolt-nil)) (cnt (jolt-transient-n t)))
        (let loop ((i 0)) (when (fx<? i cnt) (vector-set! nbuf i (vector-ref buf i)) (loop (fx+ i 1))))
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
    ((pvec? x) (tmap-put! t (pvec-nth-d x 0 jolt-nil) (pvec-nth-d x 1 jolt-nil)))
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
    ((map) (if (jolt-transient-ord t) (hashtable-ref (jolt-transient-buf t) k d) (thash-get t k d)))
    ;; the stored VALUE is the element the set holds, so this hands that back
    ;; rather than the equal key it was probed with
    ((set) (thash-get t k d))
    (else (%prev-jolt-get (jolt-transient-buf t) k d))))
(define (t-count t)
  (jolt-trans-check t "count")
  (case (jolt-transient-kind t)
    ((vec) (jolt-transient-n t))
    ((map) (if (jolt-transient-ord t) (hashtable-size (jolt-transient-buf t)) (jolt-transient-n t)))
    ((set) (jolt-transient-n t))
    (else (%prev-jolt-count (jolt-transient-buf t)))))
(define (t-contains? t k)
  (jolt-trans-check t "contains?")
  (case (jolt-transient-kind t)
    ((vec) (tvec-in-bounds? t (->idx k)))
    ((map) (if (jolt-transient-ord t)
               (hashtable-contains? (jolt-transient-buf t) k)
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
