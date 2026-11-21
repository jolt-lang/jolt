;;   Portions of this file are from ClojureScript's src/main/cljs/cljs/core.cljs,
;;   Copyright (c) Rich Hickey. All rights reserved.
;;   The use and distribution terms for that software are covered by the
;;   Eclipse Public License 1.0 (licenses/EPL-1.0.txt).

;; host tables + sorted collections — the jolt.host value primitives and the
;; 25-sorted tier's runtime.
;;
;; jolt.host/tagged-table + ref-put! + ref-get back the whole sorted tier
;; (sorted-map/sorted-set/subseq/rsubseq) AND every overlay fn that calls
;; (sorted? x) — empty, ifn?, reversible?, map?, set?, coll?. This provides:
;;   1. tagged-table / ref-put! / ref-get over a Chez mutable tagged-table type
;;      (a string-keyed hashtable wrapped in an `htable` record), def-var!'d into
;;      the jolt.host ns. The sorted tier (25-sorted.clj) mints its wrapper with
;;      these — a red-black tree + :ops table travel inside the htable.
;;   2. a sorted-coll arm on the collection dispatchers, set!-extended the same
;;      way records.ss extends them for jrec: each op routes through the value's
;;      own :ops table (the dispatch pattern). first/rest/
;;      next/last fall out free once jolt-seq has a sorted arm (they seq first).
;;
;; Loaded LAST (after records.ss / transients.ss / natives-meta.ss): it wraps the
;; jrec-extended dispatchers + value-host-tags, delegating to the captured prior.

;; --- jolt.host primitives ----------------------------------------------------
;; A tagged-table: a string-keyed hashtable (keyword field -> value). Keyword
;; keys collapse to their ns/name string so interning isn't relied on.
;; `hc` caches a sorted coll's hash (#f until asked): the wrapper is immutable
;; data once minted, so the hash is a pure function of it, like the hasheq slot a
;; pmap carries. A racing pair of writers store the same fixnum. An htable never
;; travels in a state image as itself (its string hashtable cannot be fasl'd; a
;; sorted coll is rebuilt from its entries), so the layout change needs no
;; legacy arm.
(define-record-type (htable make-htable* htable?)
  (fields (immutable h) (mutable hc)) (nongenerative chez-htable-v2))
(define (make-htable h) (make-htable* h #f))
(define (kw->key k)
  (let ((ns (keyword-t-ns k)))
    (if (and ns (not (jolt-nil? ns))) (string-append ns "/" (keyword-t-name k)) (keyword-t-name k))))
(define (jolt-tagged-table tag)
  (let ((h (make-hashtable string-hash string=?)))
    (hashtable-set! h "jolt/type" tag)
    (make-htable h)))
;; ref-put! threads the table back; a nil value REMOVES the key. Errors on a
;; non-htable so the atom-watch / volatile uses (which pass a different ref type
;; and have no table yet) stay a crash rather than silently diverging.
(define (jolt-ref-put! t k v)
  (unless (and (htable? t) (not (scoll? t))) (error #f "ref-put!: not a host table" t))
  (if (jolt-nil? v)
      (hashtable-delete! (htable-h t) (kw->key k))
      (hashtable-set! (htable-h t) (kw->key k) v))
  t)
(define (jolt-ref-get t k)
  (cond ((scoll? t) (scoll-ref t (kw->key k) jolt-nil))
        ((htable? t) (hashtable-ref (htable-h t) (kw->key k) jolt-nil))
        (else jolt-nil)))
;; A table's field by its string key, for the host code that reads one directly
;; (the class arm, tagged method dispatch). One door, so a sorted coll answers
;; the same keys a tagged table does.
(define (htable-str-ref t key dflt)
  (if (scoll? t) (scoll-ref t key dflt) (hashtable-ref (htable-h t) key dflt)))

;; --- the sorted collection's wrapper ---------------------------------------------
;; A sorted-map / sorted-set is an htable whose fields are FIXED, so it is a record
;; of them rather than a string-keyed hashtable: it was minted as a fresh
;; hashtable and five ref-put!s per conj, and every field read, and every
;; sorted-map?/sorted-set? test the dispatchers make, built a key string and
;; hashed it. In a sorted-set build that was half the time, more than the tree.
;; A subtype of htable, so everything that recognizes a tagged table recognizes
;; it, and ref-get answers its keys as before; the table slot is #f. KIND is
;; 'map or 'set, read off TAG once.
(define-record-type (scoll make-scoll* scoll?)
  (parent htable)
  (fields kind tag tree cnt cmp cmp-fn ops)
  (nongenerative chez-scoll-v1))
(define scoll-map-tag (keyword "jolt" "sorted-map"))
(define (make-scoll tag tree cnt cmp cmp-fn ops)
  (make-scoll* #f #f (if (jolt=2 tag scoll-map-tag) 'map 'set)
               tag tree cnt cmp cmp-fn ops))
(define (scoll-copy x)
  (make-scoll* #f (htable-hc x) (scoll-kind x) (scoll-tag x) (scoll-tree x) (scoll-cnt x)
               (scoll-cmp x) (scoll-cmp-fn x) (scoll-ops x)))
(define (scoll-ref t key dflt)
  (cond ((string=? key "tree") (scoll-tree t))
        ((string=? key "cnt") (scoll-cnt t))
        ((string=? key "jolt/type") (scoll-tag t))
        ((string=? key "ops") (scoll-ops t))
        ((string=? key "cmp") (scoll-cmp t))
        ((string=? key "cmp-fn") (scoll-cmp-fn t))
        (else dflt)))
(def-var! "jolt.host" "make-sorted" make-scoll)
;; the collection an op answers: X's kind, tag, comparators and ops over a new
;; tree and count — what nearly every op builds, in one call of three arguments
;; rather than make-sorted's six read back out of X
(def-var! "jolt.host" "sorted-with"
  (lambda (x tree cnt)
    (make-scoll* #f #f (scoll-kind x) (scoll-tag x) tree cnt (scoll-cmp x) (scoll-cmp-fn x) (scoll-ops x))))
(def-var! "jolt.host" "sorted-tree" (lambda (x) (scoll-tree x)))
(def-var! "jolt.host" "sorted-cnt" (lambda (x) (scoll-cnt x)))
(def-var! "jolt.host" "sorted-cmp" (lambda (x) (scoll-cmp x)))
(def-var! "jolt.host" "sorted-cmp-fn" (lambda (x) (scoll-cmp-fn x)))
(def-var! "jolt.host" "sorted-ops" (lambda (x) (scoll-ops x)))
;; :map, :set or nil — sorted-map? and sorted-set? in one test
(def-var! "jolt.host" "sorted-kind"
  (let ((km (keyword #f "map")) (ks (keyword #f "set")))
    (lambda (x) (if (scoll? x) (if (eq? (scoll-kind x) 'map) km ks) jolt-nil))))

(def-var! "jolt.host" "tagged-table" jolt-tagged-table)
(def-var! "jolt.host" "ref-put!" jolt-ref-put!)
(def-var! "jolt.host" "ref-get" jolt-ref-get)
;; map-entry constructor: a 2-elem entry-flagged pvec (map-entry? true, vector?
;; false), so sorted-map seq/first produce real map entries that key/val accept.
(def-var! "jolt.host" "map-entry" make-map-entry)
;; O(log n) RRB concat / slice over pvec (collections.ss). The stdlib
;; clojure.core.rrb-vector ns is a thin Clojure layer over these two.
(def-var! "jolt.host" "catvec" pvec-catvec)
;; subvec's slice: the O(log n) structural slice of a native vector; any other
;; IPersistentVector (a deftype such as clojure.core.Vec) copies the range by
;; index into a native vector, the value RT.subvec's SubVector view has.
(def-var! "jolt.host" "slice"
  (lambda (v start end)
    (if (pvec? v)
        (pvec-slice v start end)
        (let ((s (->idx start)) (e (->idx end)))
          (let loop ((i (fx- e 1)) (acc jolt-empty-list))
            (if (fx<? i s)
                (jolt-vec acc)
                (loop (fx- i 1) (jolt-cons (jolt-nth v i) acc))))))))
;; the class stamp clojure.core/subvec applies to a slice result (issue #629):
;; a non-empty subvec is clojure.lang.APersistentVector$SubVector, an empty one
;; is RT.subvec's PersistentVector.EMPTY. slice itself stays unstamped — pop
;; and the rrb-vector overlay slice without changing class.
(def-var! "jolt.host" "as-subvec" pvec-as-subvec)
;; the stack pop primitive (the "pop" op's jolt-pop), for clojure.core/pop's
;; var body — its vector arm used to slice via subvec, which would now stamp
;; a plain vector's pop as a SubVector. jolt-pop also carries meta, as
;; PersistentVector.pop does.
(def-var! "jolt.host" "pop" jolt-pop)
;; The stored entry for k, or nil — the native map's Associative.entryAt. The
;; entry's key is the key the MAP holds, which is jolt= to the one probed with but
;; is the only one carrying the element's metadata; clojure.core/find reads it.
;;
;; Anything that is not a native map answers `not-mine`, which find distinguishes
;; from a real nil (an absent key). That is what keeps find at one traversal for
;; the overwhelmingly common case: without the sentinel a miss on a native map
;; would have to re-probe through find's slower arms to tell the two apart.
(def-var! "jolt.host" "entry-at"
  (lambda (m k not-mine)
    (if (pmap? m)
        (let ((p (pmap-entry-at m k)))
          (if p (make-map-entry (car p) (cdr p)) jolt-nil))
        not-mine)))
;; clojure.core/find's 2-arity call sites lower here (op-registry "find"), so the
;; overwhelmingly common case — a native map — costs one traversal and an entry,
;; with no var deref in front of it. Everything else is the type taxonomy, which
;; belongs with the rest of it in the core overlay: hand off to find-other there.
;; :inline-only?, so a value-position `find` still resolves to the overlay var.
(define (jolt-find2 m k)
  (if (pmap? m)
      (let ((p (pmap-entry-at m k)))
        (if p (make-map-entry (car p) (cdr p)) jolt-nil))
      (let ((cell (var-cell-lookup "clojure.core" "find-other")))
        (if (and cell (var-cell-defined? cell))
            (jolt-invoke (var-cell-root cell) m k)
            jolt-nil))))

;; --- sorted-coll recognition + ops access ------------------------------------
(define kw-jtype (keyword "jolt" "type"))
(define kw-sorted-map (keyword "jolt" "sorted-map"))
(define kw-sorted-set (keyword "jolt" "sorted-set"))
(define kw-ops (keyword #f "ops"))
(define kw-cmp-fn (keyword #f "cmp-fn"))
(define kw-op-count (keyword #f "count"))
(define kw-op-seq (keyword #f "seq"))
(define kw-op-rseq (keyword #f "rseq"))
(define kw-op-first (keyword #f "first"))
(define kw-op-get (keyword #f "get"))
(define kw-op-contains (keyword #f "contains"))
(define kw-op-assoc (keyword #f "assoc"))
(define kw-op-dissoc (keyword #f "dissoc"))
(define kw-op-conj (keyword #f "conj"))
(define kw-op-disj (keyword #f "disj"))
;; the one-element ops: conj/assoc/dissoc/disj of a single item, which is every
;; call into/reduce makes, go straight to the per-item fn rather than through a
;; vector built to hold it and a reduce over that vector
(define kw-op-conj1 (keyword #f "conj1"))
(define kw-op-assoc1 (keyword #f "assoc1"))
(define kw-op-dissoc1 (keyword #f "dissoc1"))
(define kw-op-disj1 (keyword #f "disj1"))

(define (htable-sorted-map? x) (and (scoll? x) (eq? (scoll-kind x) 'map)))
(define (htable-sorted-set? x) (and (scoll? x) (eq? (scoll-kind x) 'set)))
(define (htable-sorted? x) (scoll? x))
;; the op fn for `op-kw` from the value's attached :ops map, then invoke it on sc.
(define (sc-op sc op-kw) (jolt-get (scoll-ops sc) op-kw jolt-nil))
(define sc-call
  (case-lambda
    ((sc op-kw) (jolt-invoke1 (sc-op sc op-kw) sc))
    ((sc op-kw a) (jolt-invoke2 (sc-op sc op-kw) sc a))
    ((sc op-kw a b) (jolt-invoke3 (sc-op sc op-kw) sc a b))))

;; --- the B+tree ------------------------------------------------------------------
;; A persistent B+tree, path-copied like the red-black tree it replaced (Rich
;; Hickey's PersistentTreeMap, via cljs). A node is one Scheme vector:
;;
;;   leaf   #(#t  k0 v0  k1 v1 ...)        entries in comparator order
;;   inner  #(#f  m0 c0  m1 c1 ...)        child Ci and Mi, the largest key below Ci
;;
;; so a node of N entries is 2N+1 slots, a field read is a vector-ref, and an
;; update copies one contiguous vector per level. With up to 16 entries a leaf
;; and 32 an inner node, a lookup touches ~log32(n) nodes where the red-black tree
;; touched ~log2(n)
;; separate 5-slot vectors, and a walk reads leaves front to back. Every
;; comparison is (cmp k node-key), as the reference makes it, but the NUMBER of
;; calls is a binary search's per node, not one per red-black level: a counting
;; comparator sees a different total than on the JVM (known-divergences).
;;
;; Occupancy: every node holds at most its kind's capacity (bt-cap) and, off the
;; two outer spines, at least half of it (bt-floor). A node on the leftmost or rightmost spine may hold
;; fewer, because an append past the last key (or before the first) splits a full
;; node into a full one and a one-entry one, rather than two half-full ones; an
;; ascending (into (sorted-set) (range n)) then fills every node to capacity. The
;; root is a non-empty leaf or an inner node of at least two children; the empty
;; tree is jolt-nil. bt-check asserts all of it.
;;
;; Not image surface: an image writes a sorted collection out as its entries and
;; rebuilds it (state-image.ss walk-sorted), so the node layout never travels.
;;
;; CMP is the collection's comparator as 25-sorted.clj holds it, or jolt-nil for
;; natural order, which is Util.compare (jolt-compare) and not the clojure.core
;; var: the JVM's PersistentTreeMap orders by RT.DEFAULT_COMPARATOR, which a
;; rebinding of #'compare does not reach. A comparator may answer any number.
(define bt-leaf-max 16)
(define bt-inner-max 32)
;; a node's capacity, and the floor it is held to off the outer spines
(define-syntax bt-cap (syntax-rules () ((_ n) (if (bt-leaf? n) bt-leaf-max bt-inner-max))))
(define-syntax bt-floor (syntax-rules () ((_ n) (fxsrl (bt-cap n) 1))))
(define-syntax bt-leaf? (syntax-rules () ((_ n) (vector-ref n 0))))
(define-syntax bt-size (syntax-rules () ((_ n) (fxsrl (vector-length n) 1))))
(define-syntax bt-key (syntax-rules () ((_ n i) (vector-ref n (fx+ (fx+ i i) 1)))))
(define-syntax bt-val (syntax-rules () ((_ n i) (vector-ref n (fx+ (fx+ i i) 2)))))
;; natural order on two fixnums answers in line; anything else goes through
;; Util.compare or the comparator
(define (bt-compare-slow cmp a b)
  (if (jolt-nil? cmp) (jolt-compare a b) (jolt-invoke cmp a b)))
(define-syntax bt-compare
  (syntax-rules ()
    ((_ cmp a b)
     (let ((c cmp) (x a) (y b))
       (if (and (jolt-nil? c) (fixnum? x) (fixnum? y))
           (cond ((fx<? x y) -1) ((fx=? x y) 0) (else 1))
           (bt-compare-slow c x y))))))

;; Node copies are the target's block moves into a fresh vector (sa-subvector,
;; sa-vector-append, sa-vector-copy; collections.ss says why): no zero fill and no
;; write barrier per slot, where a slot loop at the runtime's optimize level pays
;; a check and a barrier each. Never a bare vector-copy: the vendored irregex
;; rebinds that name at top level.
(define (bt-alloc leaf? n)
  (let ((v (make-vector (fx+ (fx+ n n) 1)))) (vector-set! v 0 leaf?) v))
(define (bt-max-key n) (bt-key n (fx- (bt-size n) 1)))
;; N with entry I replaced
(define (bt-with n i k v)
  (let ((r (sa-vector-copy n)))
    (vector-set! r (fx+ (fx+ i i) 1) k)
    (vector-set! r (fx+ (fx+ i i) 2) v)
    r))
(define (bt-insert-at n i k v)
  (let ((p (fx+ (fx+ i i) 1)))
    (sa-vector-append (sa-subvector n 0 p) (vector k v) (sa-subvector n p (vector-length n)))))
(define (bt-remove-at n i)
  (let ((p (fx+ (fx+ i i) 1)))
    (sa-vector-append (sa-subvector n 0 p) (sa-subvector n (fx+ p 2) (vector-length n)))))
;; entries [S, E) of N as a node of N's kind
(define (bt-slice n s e)
  (sa-vector-append (vector (bt-leaf? n)) (sa-subvector n (fx+ (fx+ s s) 1) (fx+ (fx+ e e) 1))))
(define (bt-concat a b)
  (sa-vector-append a (sa-subvector b 1 (vector-length b))))

;; Binary search of N for K: 2i+1 when entry I's key compares equal, else 2i for
;; the first entry whose key is greater (I = size when there is none). Natural
;; order on a fixnum K compares fixnum keys in line, as rb-compare did.
(define (bt-search-fx n k)
  (let loop ((lo 0) (hi (bt-size n)))
    (if (fx>=? lo hi)
        (fx+ lo lo)
        (let* ((m (fxsrl (fx+ lo hi) 1)) (x (bt-key n m)))
          (if (fixnum? x)
              (cond ((fx>? k x) (loop (fx+ m 1) hi))
                    ((fx<? k x) (loop lo m))
                    (else (fx+ (fx+ m m) 1)))
              (let ((c (jolt-compare k x)))
                (cond ((positive? c) (loop (fx+ m 1) hi))
                      ((negative? c) (loop lo m))
                      (else (fx+ (fx+ m m) 1)))))))))
(define-syntax define-bt-search
  (syntax-rules ()
    ((_ name (cmp n k) compare-expr)
     (define (name cmp n k)
       (let loop ((lo 0) (hi (bt-size n)))
         (if (fx>=? lo hi)
             (fx+ lo lo)
             (let* ((m (fxsrl (fx+ lo hi) 1)) (x (bt-key n m)) (c (compare-expr k x)))
               (cond ((positive? c) (loop (fx+ m 1) hi))
                     ((negative? c) (loop lo m))
                     (else (fx+ (fx+ m m) 1))))))))))
;; natural order on any other key, and a collection's own comparator
(define-bt-search bt-search-natural (cmp n k) jolt-compare)
(define-bt-search bt-search-cmp (cmp n k) (lambda (a b) (jolt-invoke2 cmp a b)))
(define-syntax bt-search
  (syntax-rules ()
    ((_ cmp n k)
     (let ((c* cmp) (k* k))
       (cond ((not (jolt-nil? c*)) (bt-search-cmp c* n k*))
             ((fixnum? k*) (bt-search-fx n k*))
             (else (bt-search-natural c* n k*)))))))
(define-syntax bt-hit? (syntax-rules () ((_ r) (fx=? (fxand r 1) 1))))

;; the last leaf below N
(define (bt-last-leaf n)
  (if (bt-leaf? n) n (bt-last-leaf (bt-val n (fx- (bt-size n) 1)))))
(define (bt-first-leaf n)
  (if (bt-leaf? n) n (bt-first-leaf (bt-val n 0))))

;; Find K in T: FOUND with L bound to its leaf and I to its index there, else
;; MISSING. Expanded in place at each lookup so nothing is returned through a
;; call. A key equal to an inner node's Mi IS Ci's last key, so the walk drops
;; down Ci's right edge without comparing.
(define-syntax bt-locate
  (syntax-rules ()
    ((_ cmp t k (l i) found missing)
     (let ((c* cmp) (k* k) (t* t))
       (if (jolt-nil? t*)
           missing
           (let loop ((n t*))
             (let* ((r (bt-search c* n k*)) (j (fxsrl r 1)))
               (cond ((bt-leaf? n) (if (bt-hit? r) (let ((l n) (i j)) found) missing))
                     ((bt-hit? r) (let* ((l (bt-last-leaf (bt-val n j))) (i (fx- (bt-size l) 1))) found))
                     ((fx=? j (bt-size n)) missing)
                     (else (loop (bt-val n j)))))))))))

;; --- insert --------------------------------------------------------------------
;; N with K/V added as entry I; past its capacity, the two nodes it splits into,
;; as a pair, each built straight from N's slots (no oversized copy first). The
;; split is at the middle, except at the outer edge of the tree (EDGE bit 1 on
;; the leftmost spine, bit 2 on the rightmost), where an entry added at the very
;; front (ADDED 0) or back (ADDED = the new last index) leaves the full node full.
(define (bt-add n i k v added edge)
  (let* ((sz (bt-size n)) (cap (bt-cap n)))
    (if (fx<? sz cap)
        (bt-insert-at n i k v)
        (let* ((at (cond ((and (fx=? (fxand edge 2) 2) (fx=? added sz)) cap)
                         ((and (fx=? (fxand edge 1) 1) (fx=? added 0)) 1)
                         (else (fxsrl (fx+ sz 1) 1))))
               (tag (vector (bt-leaf? n)))
               (p (fx+ (fx+ i i) 1))
               (len (vector-length n)))
          (if (fx<? i at)
              ;; the new entry falls left: left is N's [0, i) + K/V + [i, at-1)
              (let ((cut (fx- (fx+ at at) 1)))
                (cons (sa-vector-append tag (sa-subvector n 1 p) (vector k v) (sa-subvector n p cut))
                      (sa-vector-append tag (sa-subvector n cut len))))
              ;; it falls right: right is N's [at, i) + K/V + [i, sz)
              (let ((cut (fx+ (fx+ at at) 1)))
                (cons (sa-vector-append tag (sa-subvector n 1 cut))
                      (sa-vector-append tag (sa-subvector n cut p) (vector k v) (sa-subvector n p len)))))))))
(define (bt-child-edge edge i sz)
  (fxior (if (fx=? i 0) (fxand edge 1) 0) (if (fx=? i (fx- sz 1)) (fxand edge 2) 0)))
;; K already present as N's last key: (values N' #t), N' with the value
;; replaced (the stored key kept), or #f when V is that very value.
(define (bt-ins-last n v)
  (let ((i (fx- (bt-size n) 1)))
    (if (bt-leaf? n)
        (values (if (eq? v (bt-val n i)) #f (bt-with n i (bt-key n i) v)) #t)
        (let-values (((c present) (bt-ins-last (bt-val n i) v)))
          (values (and c (bt-with n i (bt-key n i) c)) present)))))
;; Insert K/V below N. Answers two values: the new node, a pair (left . right)
;; when N split, or #f when nothing changed; and whether K was already present
;; (its stored key is kept, its value replaced unless V is that very value).
(define (bt-ins cmp n k v edge)
  (let* ((r (bt-search cmp n k)) (i (fxsrl r 1)))
    (cond
      ((bt-leaf? n)
       (if (bt-hit? r)
           (values (if (eq? v (bt-val n i)) #f (bt-with n i (bt-key n i) v)) #t)
           (values (bt-add n i k v i edge) #f)))
      ((bt-hit? r)
       (let-values (((c present) (bt-ins-last (bt-val n i) v)))
         (values (and c (bt-with n i (bt-key n i) c)) present)))
      (else
       (let* ((sz (bt-size n))
              (i (if (fx=? i sz) (fx- sz 1) i)))
         (let-values (((c present) (bt-ins cmp (bt-val n i) k v (bt-child-edge edge i sz))))
           (values
             (cond ((not c) #f)
                   ((pair? c)
                    (let ((l (car c)) (rt (cdr c)))
                      ;; the short half of an edge split is the entry "added" at
                      ;; this level; a half split adds neither (-1)
                      (bt-add (bt-with n i (bt-max-key l) l) (fx+ i 1) (bt-max-key rt) rt
                              (cond ((fx<? (bt-size rt) (bt-floor rt)) (fx+ i 1))
                                    ((fx<? (bt-size l) (bt-floor l)) i)
                                    (else -1))
                              edge)))
                   (else (bt-with n i (bt-max-key c) c)))
             present)))))))

;; --- delete --------------------------------------------------------------------
;; Child I of N replaced by C, which a removal may have left under its floor: an
;; empty C is dropped; a short one is merged with a neighbour when the two fit in
;; one node, else the two share their entries evenly. N itself may come out short
;; or empty; its own parent mends it in turn, and the root is trimmed last.
(define (bt-mend n i c)
  (let ((cs (bt-size c)) (sz (bt-size n)))
    (cond
      ((fx=? cs 0) (bt-remove-at n i))
      ((or (fx>=? cs (bt-floor c)) (fx=? sz 1)) (bt-with n i (bt-max-key c) c))
      (else
       (let* ((lo (if (fx>? i 0) (fx- i 1) i))
              (a (if (fx=? lo i) c (bt-val n lo)))
              (b (if (fx=? lo i) (bt-val n (fx+ i 1)) c))
              (na (bt-size a)) (nb (bt-size b)) (tot (fx+ na nb)))
         (if (fx<=? tot (bt-cap c))
             (let ((m (bt-concat a b)))
               (bt-with (bt-remove-at n (fx+ lo 1)) lo (bt-max-key m) m))
             (let* ((h (fxsrl tot 1))
                    (all (bt-concat a b))
                    (a2 (bt-slice all 0 h))
                    (b2 (bt-slice all h tot))
                    (r (sa-vector-copy n)))
               (vector-set! r (fx+ (fx+ lo lo) 1) (bt-max-key a2))
               (vector-set! r (fx+ (fx+ lo lo) 2) a2)
               (vector-set! r (fx+ (fx+ lo lo) 3) (bt-max-key b2))
               (vector-set! r (fx+ (fx+ lo lo) 4) b2)
               r)))))))
;; N without its last entry
(define (bt-del-last n)
  (let ((i (fx- (bt-size n) 1)))
    (if (bt-leaf? n) (bt-remove-at n i) (bt-mend n i (bt-del-last (bt-val n i))))))
;; N without K, or #f when K is absent
(define (bt-del cmp n k)
  (let* ((r (bt-search cmp n k)) (i (fxsrl r 1)))
    (cond ((bt-leaf? n) (and (bt-hit? r) (bt-remove-at n i)))
          ((bt-hit? r) (bt-mend n i (bt-del-last (bt-val n i))))
          ((fx=? i (bt-size n)) #f)
          (else (let ((c (bt-del cmp (bt-val n i) k))) (and c (bt-mend n i c)))))))
;; an empty root is the empty tree; an inner root of one child is that child
(define (bt-trim t)
  (cond ((fx=? (bt-size t) 0) jolt-nil)
        ((and (not (bt-leaf? t)) (fx=? (bt-size t) 1)) (bt-trim (bt-val t 0)))
        (else t)))

;; --- shape check (gates) ---------------------------------------------------------
;; #t when T is a well-formed tree under CMP: keys strictly ascending, every leaf
;; at one depth, every Mi the identical object as Ci's last key, occupancy within
;; bounds (see above).
(define (bt-check t cmp)
  (call/cc
    (lambda (fail)
      (define (no) (fail #f))
      (define depth #f)
      (define prev (list 'none))
      (define (walk n d left? right? root?)
        (unless (and (vector? n) (odd? (vector-length n)) (boolean? (vector-ref n 0))) (no))
        (let ((sz (bt-size n)))
          (when (fx>? sz (bt-cap n)) (no))
          (cond (root? (when (if (bt-leaf? n) (fx<? sz 1) (fx<? sz 2)) (no)))
                ((or left? right?) (when (fx<? sz 1) (no)))
                (else (when (fx<? sz (bt-floor n)) (no))))
          (if (bt-leaf? n)
              (begin
                (if depth (unless (fx=? depth d) (no)) (set! depth d))
                (do ((i 0 (fx+ i 1))) ((fx=? i sz))
                  (unless (or (and (pair? prev) (eq? (car prev) 'none))
                              (negative? (bt-compare cmp (car prev) (bt-key n i))))
                    (no))
                  (set! prev (cons (bt-key n i) '()))))
              (do ((i 0 (fx+ i 1))) ((fx=? i sz))
                (let ((c (bt-val n i)))
                  (walk c (fx+ d 1) (and left? (fx=? i 0)) (and right? (fx=? i (fx- sz 1))) #f)
                  (unless (eq? (bt-key n i) (bt-max-key c)) (no)))))))
      (or (jolt-nil? t) (begin (walk t 0 #t #t #t) #t)))))

;; --- the ops 25-sorted.clj calls ------------------------------------------------
;; Each answers with a VALUE (the stored val, the stored key, an entry, a new
;; tree) and never with a node, so the layout above stays private to this file.
;;
;;   rbt-get / rbt-get-key   the stored val / stored key at K, or NF
;;   rbt-entry               the map-entry at K (its stored key), or nil
;;   rbt-contains?           K present
;;   rbt-assoc               a new tree when K was absent; when K was present, a
;;                           present marker whose rbt-present-tree is the tree with
;;                           K's value replaced (the stored key kept), or the SAME
;;                           tree when the value is identical (PersistentTreeMap
;;                           returns this when found.val() == val)
;;   rbt-without             the tree without K; the SAME tree when K is absent
;;   rbt-min-key / -entry    the first key / entry of a non-empty tree
(def-var! "jolt.host" "rbt-get"
  (lambda (tree cmp k nf) (bt-locate cmp tree k (l i) (bt-val l i) nf)))
(def-var! "jolt.host" "rbt-get-key"
  (lambda (tree cmp k nf) (bt-locate cmp tree k (l i) (bt-key l i) nf)))
(def-var! "jolt.host" "rbt-entry"
  (lambda (tree cmp k) (bt-locate cmp tree k (l i) (make-map-entry (bt-key l i) (bt-val l i)) jolt-nil)))
(def-var! "jolt.host" "rbt-contains?"
  (lambda (tree cmp k) (bt-locate cmp tree k (l i) #t #f)))
(def-var! "jolt.host" "rbt-assoc"
  (lambda (tree cmp k v)
    (if (jolt-nil? tree)
        (let ((l (bt-alloc #t 1))) (vector-set! l 1 k) (vector-set! l 2 v) l)
        (let-values (((r present) (bt-ins cmp tree k v 3)))
          (cond (present (cons 'rbt-present (or r tree)))
                ((pair? r)
                 (let ((t (bt-alloc #f 2)))
                   (vector-set! t 1 (bt-max-key (car r))) (vector-set! t 2 (car r))
                   (vector-set! t 3 (bt-max-key (cdr r))) (vector-set! t 4 (cdr r))
                   t))
                (else r))))))
(def-var! "jolt.host" "rbt-present?" (lambda (r) (pair? r)))
(def-var! "jolt.host" "rbt-present-tree" (lambda (r) (cdr r)))
(def-var! "jolt.host" "rbt-without"
  (lambda (tree cmp k)
    (if (jolt-nil? tree)
        tree
        (let ((r (bt-del cmp tree k))) (if r (bt-trim r) tree)))))
(def-var! "jolt.host" "rbt-min-key" (lambda (tree) (bt-key (bt-first-leaf tree) 0)))
(def-var! "jolt.host" "rbt-min-entry"
  (lambda (tree) (let ((l (bt-first-leaf tree))) (make-map-entry (bt-key l 0) (bt-val l 0)))))

;; --- the in-order walk over the B+tree -------------------------------------------
;; The walk is Clojure's PersistentTreeMap$Seq, over leaves: a stack of frames
;; (node . i), innermost first. In a leaf frame I is the next entry out; in an
;; inner frame, the next child to descend. The head is O(log n) away and every
;; later step amortized O(1). It is done here rather than in 25-sorted.clj
;; because a per-node lazy cell written in Clojure cost 1.5x the old eager walk
;; end to end (jolt-r8tz.7); filling a chunk of up to 32 projected entries per
;; step in Scheme beats the eager walk instead, and the result is still a lazy
;; seq — `first`/`take` touch one chunk, not the tree.
(define kw-tree (keyword #f "tree"))
(define kw-cnt (keyword #f "cnt"))
(define kw-cmp (keyword #f "cmp"))
(define (bt-near n asc?) (if asc? 0 (fx- (bt-size n) 1)))
(define-syntax bt-step (syntax-rules () ((_ i asc?) (if asc? (fx+ i 1) (fx- i 1)))))
;; push N and its near spine, down to a leaf frame
(define (bt-push-down n asc? stack)
  (let ((i (bt-near n asc?)))
    (if (bt-leaf? n)
        (cons (cons n i) stack)
        (bt-push-down (bt-val n i) asc? (cons (cons n (bt-step i asc?)) stack)))))
;; STACK with its top made a leaf frame that has an entry left, or '() at the end
(define (bt-settle stack asc?)
  (if (null? stack)
      stack
      (let* ((f (car stack)) (n (car f)) (i (cdr f)))
        (cond ((or (fx<? i 0) (fx>=? i (bt-size n))) (bt-settle (cdr stack) asc?))
              ((bt-leaf? n) stack)
              (else (bt-push-down (bt-val n i) asc? (cons (cons n (bt-step i asc?)) (cdr stack))))))))
;; what an entry walks as. mode 0: a map entry (PersistentTreeMap$Seq); 1: its key
;; (a sorted set's seq, or (keys m) — APersistentMap$KeySeq); 2: its val (ValSeq).
(define (sc-entry-proj n i mode)
  (cond ((fx=? mode 0) (make-map-entry (bt-key n i) (bt-val n i)))
        ((fx=? mode 1) (bt-key n i))
        (else (bt-val n i))))
(define (sc-mode-kind mode)
  (cond ((fx=? mode 0) sk-treemap-seq) ((fx=? mode 1) sk-key-seq) (else sk-val-seq)))
(define sc-walk-chunk 32)
;; The seq of what is left on STACK: a ChunkedCons-shaped cell over the next
;; <=32 entries, flavored as the tree seq it is (so chunked-seq? stays false, as
;; it is on the JVM, while reduce still runs the chunk in vec-reduce's loop),
;; whose after-chunk rest is a lazy seq over the stack that remains. The stack and
;; the packed walk (mode*2 + ascending?) are the lazy-src's data, so a seq caught
;; half-walked by a state image restores still lazy.
(define (sc-stack->seq stack code)
  (let* ((asc? (fx=? (fxand code 1) 1)) (st0 (bt-settle stack asc?)))
    (if (null? st0)
        jolt-nil
        (let ((mode (fxsrl code 1)) (buf (make-vector sc-walk-chunk)))
          (let fill ((j 0) (st st0))
            (if (or (null? st) (fx=? j sc-walk-chunk))
                (cseq-chunked/k (make-pvec (if (fx=? j sc-walk-chunk) buf (vec-copy-range buf 0 j)))
                                0
                                (if (null? st) jolt-nil (jolt-make-lazy-src lz-sorted-walk st code))
                                (sc-mode-kind mode))
                (let* ((f (car st)) (n (car f)) (sz (bt-size n)))
                  (let take ((i (cdr f)) (j j))
                    (cond ((or (fx<? i 0) (fx>=? i sz)) (fill j (bt-settle (cdr st) asc?)))
                          ((fx=? j sc-walk-chunk) (fill j (cons (cons n i) (cdr st))))
                          (else (vector-set! buf j (sc-entry-proj n i mode))
                                (take (bt-step i asc?) (fx+ j 1))))))))))))
(define lz-sorted-walk
  (register-lazy-src! 'sorted-walk (lambda (st code) (sc-stack->seq st code))))
(define (sc-walk-code asc? mode) (fx+ (fx* mode 2) (if asc? 1 0)))
;; the whole tree, ascending or descending
(define (sc-tree-seq tree asc? mode)
  (if (jolt-nil? tree)
      jolt-nil
      (sc-stack->seq (bt-push-down tree asc? '()) (sc-walk-code asc? mode))))
;; PersistentTreeMap.seqFrom: the walk from the first key at or past K in the
;; walk's direction, nil when there is none. The comparator is called the way
;; seqFrom calls it, (cmp k node-key).
(define (sc-tree-seq-from tree asc? mode cmp k)
  (let ((code (sc-walk-code asc? mode)))
    (if (jolt-nil? tree)
        jolt-nil
        (let loop ((n tree) (stack '()))
          (let* ((sz (bt-size n))
                 (r (let bs ((lo 0) (hi sz))
                      (if (fx>=? lo hi)
                          (fx+ lo lo)
                          (let* ((m (fxsrl (fx+ lo hi) 1)) (c (jolt-invoke cmp k (bt-key n m))))
                            (cond ((positive? c) (bs (fx+ m 1) hi))
                                  ((negative? c) (bs lo m))
                                  (else (fx+ (fx+ m m) 1)))))))
                 (i (fxsrl r 1)))
            (cond
              ((bt-leaf? n)
               (sc-stack->seq (cons (cons n (if (or asc? (bt-hit? r)) i (fx- i 1))) stack) code))
              (asc? (if (fx=? i sz)
                        (sc-stack->seq stack code)
                        (loop (bt-val n i) (cons (cons n (fx+ i 1)) stack))))
              (else (let ((i (if (fx=? i sz) (fx- sz 1) i)))
                      (loop (bt-val n i) (cons (cons n (fx- i 1)) stack))))))))))
(def-var! "jolt.host" "sorted-seq"
  (lambda (tree asc? mode) (sc-tree-seq tree (jolt-truthy? asc?) mode)))
(def-var! "jolt.host" "sorted-seq-from"
  (lambda (tree asc? mode cmp k) (sc-tree-seq-from tree (jolt-truthy? asc?) mode cmp k)))
;; (f k v acc) over every entry, in order
(define (sc-tree-fold-kv t f acc)
  (if (jolt-nil? t)
      acc
      (let walk ((n t) (acc acc))
        (let ((sz (bt-size n)))
          (if (bt-leaf? n)
              (do ((i 0 (fx+ i 1)) (acc acc (f (bt-key n i) (bt-val n i) acc))) ((fx=? i sz) acc))
              (do ((i 0 (fx+ i 1)) (acc acc (walk (bt-val n i) acc))) ((fx=? i sz) acc)))))))
;; every entry, in order, satisfies (pred k v); stops at the first that does not
(define (sc-tree-every-kv? t pred)
  (or (jolt-nil? t)
      (let walk ((n t))
        (let ((sz (bt-size n)))
          (let loop ((i 0))
            (or (fx=? i sz)
                (and (if (bt-leaf? n) (pred (bt-key n i) (bt-val n i)) (walk (bt-val n i)))
                     (loop (fx+ i 1)))))))))
;; Two trees' entries in lockstep, ascending: #t when SAME? holds for every pair
;; (same? ka va kb vb) and both run out together. The two trees need not share a
;; shape, so each side keeps its own leaf and index; a frame is consed only where
;; a side crosses into its next leaf, not per entry.
(define (sc-tree-lockstep? ta tb same?)
  (let loop ((sa (if (jolt-nil? ta) '() (bt-push-down ta #t '())))
             (sb (if (jolt-nil? tb) '() (bt-push-down tb #t '()))))
    (cond ((null? sa) (null? sb))
          ((null? sb) #f)
          (else
           (let ((la (caar sa)) (lb (caar sb)))
             (let run ((ia (cdar sa)) (ib (cdar sb)))
               (let ((enda (fx=? ia (bt-size la))) (endb (fx=? ib (bt-size lb))))
                 (cond ((or enda endb)
                        (loop (if enda (bt-settle (cdr sa) #t) (cons (cons la ia) (cdr sa)))
                              (if endb (bt-settle (cdr sb) #t) (cons (cons lb ib) (cdr sb)))))
                       ((same? (bt-key la ia) (bt-val la ia) (bt-key lb ib) (bt-val lb ib))
                        (run (fx+ ia 1) (fx+ ib 1)))
                       (else #f)))))))))

;; --- extend the collection dispatchers with a sorted arm ---------------------
;; A sorted coll's seq is the :seq op's: the lazy walk above, already flavored
;; (a map's is a PersistentTreeMap$Seq, a set's an APersistentMap$KeySeq —
;; PersistentTreeSet.seq is RT.keys(impl.seq())).
(register-seq-arm! htable-sorted? (lambda (x) (sc-call x kw-op-seq)))
;; first on a sorted collection answers from the tree's leftmost node — the :first
;; op is an O(log n) spine walk (25-sorted.clj). Without this arm it went through
;; the generic (seq-first (jolt-seq x)), and the :seq op materializes the WHOLE
;; tree into a vector before the head can be read: 190ms against the reference's
;; 0.4us over 200k entries. Clojure answers the same question through
;; PersistentTreeMap.min(). It still saves building the first chunk of the walk.
(register-first-arm! htable-sorted? (lambda (x) (sc-call x kw-op-first)))
(register-count-arm! htable-sorted?
  (lambda (coll) (sc-call coll kw-op-count)))
(register-get-arm! htable-sorted? (lambda (coll k d) (sc-call coll kw-op-get k d)))
(register-contains-arm! htable-sorted?
  (lambda (coll k) (if (jolt-truthy? (sc-call coll kw-op-contains k)) #t #f)))
(define %h-assoc1 jolt-assoc1)
(set! jolt-assoc1 (lambda (coll k v)
  (if (htable-sorted-map? coll) (meta-carry coll (sc-call coll kw-op-assoc1 k v)) (%h-assoc1 coll k v))))
(define %h-dissoc jolt-dissoc)
(set! jolt-dissoc (lambda (coll . ks)
  (if (htable-sorted-map? coll) (meta-carry coll (sc-call coll kw-op-dissoc (apply jolt-vector ks))) (apply %h-dissoc coll ks))))
(define %h-dissoc2 jolt-dissoc2)
(set! jolt-dissoc2 (lambda (coll k)
  (if (htable-sorted-map? coll) (meta-carry coll (sc-call coll kw-op-dissoc1 k)) (%h-dissoc2 coll k))))
(register-conj-arm! htable-sorted? (lambda (coll x) (meta-carry coll (sc-call coll kw-op-conj1 x))))
(define %h-disj jolt-disj)
(set! jolt-disj (lambda (s . xs)
  (cond ((not (htable-sorted-set? s)) (apply %h-disj s xs))
        ((and (pair? xs) (null? (cdr xs))) (meta-carry s (sc-call s kw-op-disj1 (car xs))))
        (else (meta-carry s (sc-call s kw-op-disj (apply jolt-vector xs)))))))
(def-var! "clojure.core" "disj" jolt-disj)
(register-empty-arm! htable-sorted? (lambda (coll) (zero? (sc-call coll kw-op-count))))
(define %h-keys jolt-keys)
(set! jolt-keys (lambda (m)
  (if (htable-sorted-map? m)
      (sc-tree-seq (jolt-ref-get m kw-tree) #t 1)
      (%h-keys m))))
(define %h-vals jolt-vals)
(set! jolt-vals (lambda (m)
  (if (htable-sorted-map? m)
      (sc-tree-seq (jolt-ref-get m kw-tree) #t 2)
      (%h-vals m))))
;; keys/vals walk the tree lazily as the KeySeq/ValSeq they are on the JVM.
;; sorted colls carry collection metadata like the natives-meta collections. htable?
;; is only in scope here (host-table loads after natives-meta), so with-meta/meta-copy
;; are extended by set!. A fresh-identity shallow copy of the inner table keys meta off
;; the original; the :tree/:cmp/:ops it holds are immutable persistent values.
(define %ht-meta-copy meta-copy)
(set! meta-copy
  (lambda (x)
    (cond
      ((scoll? x) (scoll-copy x))
      ((htable? x)
        (let ((h (make-hashtable string-hash string=?)))
          (vector-for-each (lambda (k) (hashtable-set! h k (hashtable-ref (htable-h x) k #f)))
                           (hashtable-keys (htable-h x)))
          (make-htable* h (htable-hc x))))
      (else (%ht-meta-copy x)))))
(define %ht-with-meta jolt-with-meta)
(set! jolt-with-meta
  (lambda (x m)
    (if (htable? x)
        (let ((c (meta-copy x)))
          (if (jolt-nil? m) (meta-table-del! c) (meta-table-set! c m))
          c)
        (%ht-with-meta x m))))
;; the clojure.core var was def-var!'d in natives-meta.ss to the prior closure; user
;; with-meta resolves the var, so re-bind it after the set! (cf. disj above).
(def-var! "clojure.core" "with-meta" jolt-with-meta)

;; sorted colls are collections (callable as fns via jolt-invoke, conj-able).
(define %h-coll? jolt-coll?)
(set! jolt-coll? (lambda (x) (or (htable-sorted? x) (%h-coll? x))))
;; sorted colls invoke like their unordered counterparts: a sorted-map is
;; IFn(get k [d]), a sorted-set is IFn(get k). Registered as invoke arms so
;; jolt-invoke dispatches them before the final ClassCastException fallback.
(register-invoke-arm! htable-sorted-map?
  (lambda (f args)
    (let ((n (length args)))
      (jolt-check-arity-1or2 "clojure.lang.PersistentTreeMap" n)
      (apply jolt-get f args))))
(register-invoke-arm! htable-sorted-set?
  (lambda (f args)
    (let ((n (length args)))
      (jolt-check-arity-1 "clojure.lang.PersistentTreeSet" n)
      (apply jolt-get f args))))

;; public predicates: a sorted-map is map?, a sorted-set is set?, both coll?.
;; predicates.ss/records.ss def-var!'d a snapshot, so re-def-var! after set!.
(register-map-pred-arm! htable-sorted-map?)
(def-var! "clojure.core" "map?" jolt-map?)
(define %h-set? jolt-set?)
(set! jolt-set? (lambda (x) (or (htable-sorted-set? x) (%h-set? x))))
(def-var! "clojure.core" "set?" jolt-set?)
(def-var! "clojure.core" "coll?" (lambda (x) (or (htable-sorted? x) (jrec-collection? x) (jolt-coll-pred? x))))

;; --- equality / hash ---------------------------------------------------------
;; A sorted coll canonicalizes like its unordered counterpart:
;; a sorted-map equals ANY map (hash or sorted) with the same entries, a
;; sorted-set ANY set with the same elements — the comparator is irrelevant to =.
;; The general answer converts to the plain persistent coll and delegates to the
;; prior jolt=2. (htable-sorted? short-circuits on a non-htable BEFORE any jolt=2,
;; so extending jolt=2 here doesn't recurse: the inner tag compare gets two
;; keywords.)
(define (sorted-map->pmap sc)
  (sc-tree-fold-kv (jolt-ref-get sc kw-tree) (lambda (k v m) (pmap-assoc m k v)) empty-pmap))
(define (sorted-set->pset sc)
  (sc-tree-fold-kv (jolt-ref-get sc kw-tree) (lambda (k v s) (pset-conj s k)) empty-pset))
(define (sorted->plain x) (if (htable-sorted-map? x) (sorted-map->pmap x) (sorted-set->pset x)))
;; The common pairings answer without building anything, the way APersistentMap /
;; APersistentSet.equiv do: counts first, then one walk of the sorted tree. Against
;; a hash coll each entry is probed there (hash lookup, the same test the converted
;; compare made); against a sorted coll with the SAME comparator the two trees walk
;; in lockstep, which is O(n) with no comparator calls at all. 'eq / 'ne, or #f to
;; fall back to the conversion (a different comparator, a record, a Java map, …).
(define (sc-count x) (jolt-ref-get x kw-cnt))
(define (sorted-fast= s o)
  (define (verdict b) (if b 'eq 'ne))
  (define (same-cmp? o) (eq? (jolt-ref-get s kw-cmp-fn) (jolt-ref-get o kw-cmp-fn)))
  (define (lockstep same?) (sc-tree-lockstep? (jolt-ref-get s kw-tree) (jolt-ref-get o kw-tree) same?))
  (if (htable-sorted-map? s)
      (cond
        ((pmap? o)
         (verdict (and (eqv? (sc-count s) (pmap-cnt o))
                       (sc-tree-every-kv? (jolt-ref-get s kw-tree)
                         (lambda (k v) (let ((p (pmap-entry-at o k)))
                                         (and p (jolt=2 v (cdr p)))))))))
        ((and (htable-sorted-map? o) (not (eqv? (sc-count s) (sc-count o)))) 'ne)
        ((and (htable-sorted-map? o) (same-cmp? o))
         (verdict (lockstep (lambda (ka va kb vb) (and (jolt=2 ka kb) (jolt=2 va vb))))))
        (else #f))
      (cond
        ((pset? o)
         (verdict (and (eqv? (sc-count s) (pset-count o))
                       (sc-tree-every-kv? (jolt-ref-get s kw-tree)
                         (lambda (k v) (pset-contains? o k))))))
        ((and (htable-sorted-set? o) (not (eqv? (sc-count s) (sc-count o)))) 'ne)
        ((and (htable-sorted-set? o) (same-cmp? o))
         (verdict (lockstep (lambda (ka va kb vb) (jolt=2 ka kb)))))
        (else #f))))
(register-eq-arm! (lambda (a b) (or (htable-sorted? a) (htable-sorted? b)))
                  (lambda (a b)
                    (let ((v (if (htable-sorted? a) (sorted-fast= a b) (sorted-fast= b a))))
                      (if v
                          (eq? v 'eq)
                          ;; a sorted coll compares as its plain equivalent: normalize
                          ;; and re-dispatch (the normalized values aren't sorted, so
                          ;; this arm won't re-match — the base compares).
                          (jolt=2 (if (htable-sorted? a) (sorted->plain a) a)
                                  (if (htable-sorted? b) (sorted->plain b) b))))))
;; A sorted coll hashes as its plain equivalent — Murmur3.hashUnordered over its
;; entries (each the ordered hash of [k v]) or its elements, exactly the folds
;; jolt-coll-hash runs over a pmap / pset — computed by folding the tree and cached
;; on the wrapper, as APersistentMap caches _hasheq.
(define (sorted-hash x)
  (or (htable-hc x)
      (let* ((t (jolt-ref-get x kw-tree))
             (sum (if (htable-sorted-map? x)
                      (sc-tree-fold-kv t (lambda (k v acc) (add32 acc (entry-hasheq k v))) 0)
                      (sc-tree-fold-kv t (lambda (k v acc) (+ acc (jolt-hasheq k))) 0)))
             (h (mix-coll-hash sum (sc-count x))))
        (htable-hc-set! x h)
        h)))
(register-hash-arm! htable-sorted? sorted-hash)

;; --- printing ----------------------------------------------------------------
;; sorted colls render in SORTED order (the value's :seq), not HAMT order; a
;; sorted-map prints "{k v, k v}" (", " between pairs) like the pmap arm.
;; *print-level* and *print-length* apply as to the hash collections (the JVM
;; prints both through print-sequential): a level past the limit is "#", and at
;; most *print-length* elements are rendered before "...".
(define (sorted-map-render sc render)
  (if (jolt-print-hash?) "#"
      (with-deeper-print
        (string-append "{"
          (jolt-str-join-comma
            (jolt-limited-seq-strs (jolt-seq (sc-call sc kw-op-seq))
              (lambda (e) (string-append (render (jolt-nth e 0)) " " (render (jolt-nth e 1))))))
          "}"))))
(define (sorted-set-render sc render)
  (if (jolt-print-hash?) "#"
      (with-deeper-print
        (string-append "#{"
          (jolt-str-join (jolt-limited-seq-strs (jolt-seq (sc-call sc kw-op-seq)) render))
          "}"))))
(define (sorted-render x render)
  (if (htable-sorted-map? x) (sorted-map-render x render) (sorted-set-render x render)))

;; sorted colls render in :seq order via the calling printer (str vs readable).
(register-pr-readable-arm! htable-sorted? (lambda (x) (sorted-render x jolt-pr-readable)))
(register-pr-str-arm! htable-sorted? (lambda (x) (sorted-render x jolt-pr-str)))
(register-str-render! htable-sorted? (lambda (x) (sorted-render x jolt-str-render-one)))

;; --- protocol dispatch over builtins (extend-protocol Map/Set on sorted) ------
;; value-host-tags (records.ss) drives extend-protocol on host values; a
;; sorted-map must answer to "Map", a sorted-set to "Set"/"Collection".
(define %h-value-host-tags value-host-tags)
(set! value-host-tags (lambda (obj)
  (cond
    ((htable-sorted-map? obj) (jch-tags "clojure.lang.PersistentTreeMap"))
    ((htable-sorted-set? obj) (jch-tags "clojure.lang.PersistentTreeSet"))
    (else (%h-value-host-tags obj)))))

;; (class e) on a throwable tagged-table (a library's ex-info envelope carrying a
;; JVM :class, e.g. jolt-lang/http-client's UnknownHostException) reads that
;; class name, so clojure.test's (thrown? Class …) / (= Class (class e)) match.
;; an htable carrying a string "class" entry reports it (a host-object class mirror).
(register-class-arm! (lambda (x) (and (htable? x) (string? (htable-str-ref x "class" #f))))
                     (lambda (x) (htable-str-ref x "class" #f)))
