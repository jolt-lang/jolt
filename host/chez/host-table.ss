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
;;      these — a B+tree + :ops table travel inside the htable.
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
(define kw-op-conj-kvs (keyword #f "conj-kvs"))
(define kw-op-assoc-all (keyword #f "assoc-all"))
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
;; Comparators that order two fixnums as their numeric order (-1 / 0 / 1), the
;; way Util.compare does: clojure.core/compare, java.util.TreeMap's natural
;; order. Under one of these a fixnum K is compared with a fixnum key in line,
;; and with anything else through the comparator, so its answers are unchanged.
(define bt-fixnum-ordered '())
(define (bt-fixnum-ordered! cmp)
  (unless (memq cmp bt-fixnum-ordered) (set! bt-fixnum-ordered (cons cmp bt-fixnum-ordered))))
(define-bt-search bt-search-fxc (cmp n k)
  (lambda (a b) (if (fixnum? b) (cond ((fx<? a b) -1) ((fx=? a b) 0) (else 1)) (jolt-invoke2 cmp a b))))
(define-syntax bt-search
  (syntax-rules ()
    ((_ cmp n k)
     (let ((c* cmp) (k* k))
       (cond ((not (jolt-nil? c*))
              (if (and (fixnum? k*) (memq c* bt-fixnum-ordered))
                  (bt-search-fxc c* n k*)
                  (bt-search-cmp c* n k*)))
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
;; what an insert or delete answers as the old value of a key that was not there
(define bt-absent (list 'bt-absent))
;; K already present as N's last key: (values N' OLD), N' with the value
;; replaced (the stored key kept), or #f when V is that very value, and OLD the
;; value it had.
(define (bt-ins-last n v)
  (let ((i (fx- (bt-size n) 1)))
    (if (bt-leaf? n)
        (values (if (eq? v (bt-val n i)) #f (bt-with n i (bt-key n i) v)) (bt-val n i))
        (let-values (((c present) (bt-ins-last (bt-val n i) v)))
          (values (and c (bt-with n i (bt-key n i) c)) present)))))
;; Insert K/V below N. Answers two values: the new node, a pair (left . right)
;; when N split, or #f when nothing changed; and K's old value, bt-absent when
;; it was not present (a present K keeps its stored key, and its value is
;; replaced unless V is that very value).
(define (bt-ins cmp n k v edge)
  (let* ((r (bt-search cmp n k)) (i (fxsrl r 1)))
    (cond
      ((bt-leaf? n)
       (if (bt-hit? r)
           (values (if (eq? v (bt-val n i)) #f (bt-with n i (bt-key n i) v)) (bt-val n i))
           (values (bt-add n i k v i edge) bt-absent)))
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
;; N without its last entry: (values N' OLD), OLD the value it held
(define (bt-del-last n)
  (let ((i (fx- (bt-size n) 1)))
    (if (bt-leaf? n)
        (values (bt-remove-at n i) (bt-val n i))
        (let-values (((c old) (bt-del-last (bt-val n i)))) (values (bt-mend n i c) old)))))
;; N without K: (values N' OLD), K's old value; (values #f bt-absent) when K is absent
(define (bt-del cmp n k)
  (let* ((r (bt-search cmp n k)) (i (fxsrl r 1)))
    (cond ((bt-leaf? n) (if (bt-hit? r) (values (bt-remove-at n i) (bt-val n i)) (values #f bt-absent)))
          ((bt-hit? r) (let-values (((c old) (bt-del-last (bt-val n i)))) (values (bt-mend n i c) old)))
          ((fx=? i (bt-size n)) (values #f bt-absent))
          (else (let-values (((c old) (bt-del cmp (bt-val n i) k)))
                  (values (and c (bt-mend n i c)) old))))))
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
(define (bt-assoc tree cmp k v)
  (if (jolt-nil? tree)
      (let ((l (bt-alloc #t 1))) (vector-set! l 1 k) (vector-set! l 2 v) l)
      (let-values (((r old) (bt-ins cmp tree k v 3)))
        (cond ((not (eq? old bt-absent)) (cons 'rbt-present (or r tree)))
              ((pair? r)
               (let ((t (bt-alloc #f 2)))
                 (vector-set! t 1 (bt-max-key (car r))) (vector-set! t 2 (car r))
                 (vector-set! t 3 (bt-max-key (cdr r))) (vector-set! t 4 (cdr r))
                 t))
              (else r)))))
(def-var! "jolt.host" "rbt-assoc" bt-assoc)
(def-var! "jolt.host" "rbt-present?" (lambda (r) (pair? r)))
(def-var! "jolt.host" "rbt-present-tree" (lambda (r) (cdr r)))
(define (bt-without tree cmp k)
  (if (jolt-nil? tree)
      tree
      (let-values (((r old) (bt-del cmp tree k))) (if r (bt-trim r) tree))))
(def-var! "jolt.host" "rbt-without" bt-without)
(def-var! "jolt.host" "rbt-min-key" (lambda (tree) (bt-key (bt-first-leaf tree) 0)))
(def-var! "jolt.host" "rbt-min-entry"
  (lambda (tree) (let ((l (bt-first-leaf tree))) (make-map-entry (bt-key l 0) (bt-val l 0)))))

;; --- many entries at once ---------------------------------------------------------
;; A batch (a constructor's arguments, a multi-arg assoc/conj, an into) against an
;; empty tree or one no bigger than the batch is sorted and bulk-loaded rather than
;; inserted entry by entry: every node is written once, full, and nothing is
;; copied along a path. A small batch against a big tree inserts one entry at a
;; time, which touches only the paths it changes.
;;
;; The batch's meaning is the entry-by-entry one: of comparator-equal keys the
;; first key stays (an existing key over a batch key) and the last value wins,
;; and a batch that changes nothing answers the SAME tree. Comparisons keep the
;; (cmp later earlier) shape of an insert's (cmp k node-key).

;; stable merge sort of the N entries of flat F (k v k v ...) by key, in place;
;; a half already in order behind the other is not merged
(define (bt-sort-flat! f n cmp)
  (let ((tmp (make-vector (fx+ n n))))
    (define (key i) (vector-ref f (fx+ i i)))
    (define (move! dst o src i)
      (vector-set! dst (fx+ o o) (vector-ref src (fx+ i i)))
      (vector-set! dst (fx+ (fx+ o o) 1) (vector-ref src (fx+ (fx+ i i) 1))))
    (let rec ((lo 0) (hi n))
      (when (fx>? (fx- hi lo) 1)
        (let ((mid (fxsrl (fx+ lo hi) 1)))
          (rec lo mid)
          (rec mid hi)
          (when (negative? (bt-compare cmp (key mid) (key (fx- mid 1))))
            (let merge ((i lo) (j mid) (o lo))
              (cond
                ((and (fx<? i mid) (fx<? j hi))
                 (if (negative? (bt-compare cmp (key j) (key i)))
                     (begin (move! tmp o f j) (merge i (fx+ j 1) (fx+ o 1)))
                     (begin (move! tmp o f i) (merge (fx+ i 1) j (fx+ o 1)))))
                ((fx<? i mid) (move! tmp o f i) (merge (fx+ i 1) j (fx+ o 1)))
                ((fx<? j hi) (move! tmp o f j) (merge i (fx+ j 1) (fx+ o 1)))
                (else (do ((x lo (fx+ x 1))) ((fx=? x hi)) (move! f x tmp x)))))))))))
;; the N sorted entries of flat F with each run of equal keys made one entry:
;; its first key, its last value. Compacts F in place; answers the new count.
(define (bt-dedup-flat! f n cmp)
  (if (fx=? n 0)
      0
      (let loop ((i 1) (o 0))
        (if (fx=? i n)
            (fx+ o 1)
            (if (zero? (bt-compare cmp (vector-ref f (fx+ i i)) (vector-ref f (fx+ o o))))
                (begin (vector-set! f (fx+ (fx+ o o) 1) (vector-ref f (fx+ (fx+ i i) 1)))
                       (loop (fx+ i 1) o))
                (let ((o (fx+ o 1)))
                  (vector-set! f (fx+ o o) (vector-ref f (fx+ i i)))
                  (vector-set! f (fx+ (fx+ o o) 1) (vector-ref f (fx+ (fx+ i i) 1)))
                  (loop (fx+ i 1) o)))))))
;; F's first N entries (flat k v k v ...) packed into nodes of LEAF?'s capacity,
;; spread evenly so each is at least half full: (values P m), P the flat
;; (max-key node ...) of the m nodes
(define (bt-pack f n leaf?)
  (let* ((cap (if leaf? bt-leaf-max bt-inner-max))
         (k (fxquotient (fx+ n (fx- cap 1)) cap))
         (base (fxquotient n k))
         (extra (fxremainder n k))
         (p (make-vector (fx+ k k)))
         (tag (vector leaf?)))
    (let loop ((j 0) (start 0))
      (if (fx=? j k)
          (values p k)
          (let* ((sz (if (fx<? j extra) (fx+ base 1) base))
                 (node (sa-vector-append tag (sa-subvector f (fx+ start start) (fx* 2 (fx+ start sz))))))
            (vector-set! p (fx+ j j) (bt-max-key node))
            (vector-set! p (fx+ (fx+ j j) 1) node)
            (loop (fx+ j 1) (fx+ start sz)))))))
;; a tree of the N entries of flat F, in order, bottom up
(define (bt-bulk f n)
  (if (fx=? n 0)
      jolt-nil
      (let loop ((f f) (n n) (leaf? #t))
        (let-values (((p m) (bt-pack f n leaf?)))
          (if (fx=? m 1) (vector-ref p 1) (loop p m #f))))))
;; TREE's entries merged with the D sorted, deduplicated entries of flat G and
;; bulk-loaded: (values tree count changed?)
(define (bt-merge-load tree cmp g d)
  (let* ((xs (list->vector (reverse (sc-tree-fold-kv tree (lambda (k v acc) (cons v (cons k acc))) '()))))
         (nx (fxsrl (vector-length xs) 1))
         (out (make-vector (fx* 2 (fx+ nx d)))))
    (define (emit! o k v) (vector-set! out (fx+ o o) k) (vector-set! out (fx+ (fx+ o o) 1) v))
    (let loop ((i 0) (j 0) (o 0) (changed #f))
      (cond
        ((and (fx<? i nx) (fx<? j d))
         (let* ((xk (vector-ref xs (fx+ i i))) (xv (vector-ref xs (fx+ (fx+ i i) 1)))
                (gk (vector-ref g (fx+ j j))) (gv (vector-ref g (fx+ (fx+ j j) 1)))
                (c (bt-compare cmp gk xk)))
           (cond ((negative? c) (emit! o gk gv) (loop i (fx+ j 1) (fx+ o 1) #t))
                 ((positive? c) (emit! o xk xv) (loop (fx+ i 1) j (fx+ o 1) changed))
                 (else (emit! o xk gv) (loop (fx+ i 1) (fx+ j 1) (fx+ o 1) (or changed (not (eq? gv xv))))))))
        ((fx<? i nx)
         (emit! o (vector-ref xs (fx+ i i)) (vector-ref xs (fx+ (fx+ i i) 1)))
         (loop (fx+ i 1) j (fx+ o 1) changed))
        ((fx<? j d)
         (emit! o (vector-ref g (fx+ j j)) (vector-ref g (fx+ (fx+ j j) 1)))
         (loop i (fx+ j 1) (fx+ o 1) #t))
        (else (values (if changed (bt-bulk out o) tree) o changed))))))
;; the batch as a fresh flat k v vector: a vector's slots copied by block, a set's
;; elements (KEYS-ONLY) given nil values, anything else folded
(define (bt-batch-flat kvs keys-only?)
  (cond
    ((and (pvec? kvs) (not keys-only?))
     (let ((v (make-vector (pvec-cnt kvs)))) (pvec-copy-into! v 0 kvs) v))
    (keys-only?
     (let* ((xs (into-fold (lambda (acc x) (cons x acc)) '() kvs))
            (n (length xs))
            (v (make-vector (fx+ n n) jolt-nil)))
       (let loop ((xs xs) (i (fx- n 1)))
         (if (null? xs) v (begin (vector-set! v (fx+ i i) (car xs)) (loop (cdr xs) (fx- i 1)))))))
    (else (list->vector (reverse (into-fold (lambda (acc x) (cons x acc)) '() kvs))))))
;; under this many entries a batch is inserted entry by entry
(define bt-batch-min 8)
;; (rbt-assoc-all tree cmp cnt kvs keys-only?): KVS assoc'd onto TREE of CNT
;; entries — k0 v0 k1 v1 ..., or with KEYS-ONLY? each element a key with a nil
;; value — answered as a map entry [tree count]
(def-var! "jolt.host" "rbt-assoc-all"
  (lambda (tree cmp cnt kvs keys-only?)
    (let* ((f (bt-batch-flat kvs (jolt-truthy? keys-only?)))
           (m (fxsrl (vector-length f) 1)))
      (if (or (fx<? m bt-batch-min) (and (not (jolt-nil? tree)) (fx<? m cnt)))
          (let loop ((i 0) (t tree) (c cnt))
            (if (fx=? i m)
                (make-map-entry t c)
                (let ((r (bt-assoc t cmp (vector-ref f (fx+ i i)) (vector-ref f (fx+ (fx+ i i) 1)))))
                  (if (pair? r) (loop (fx+ i 1) (cdr r) c) (loop (fx+ i 1) r (fx+ c 1))))))
          ;; in order already? each key against the one before it
          (let-values (((sorted dups)
                        (let check ((i 1) (dups #f))
                          (if (fx>=? i m)
                              (values #t dups)
                              (let ((c (bt-compare cmp (vector-ref f (fx+ i i)) (vector-ref f (fx- (fx+ i i) 2)))))
                                (if (negative? c) (values #f dups) (check (fx+ i 1) (or dups (zero? c)))))))))
            (unless sorted (bt-sort-flat! f m cmp))
            (let ((d (if (or dups (not sorted)) (bt-dedup-flat! f m cmp) m)))
              (if (jolt-nil? tree)
                  (make-map-entry (bt-bulk f d) d)
                  (let-values (((t n changed) (bt-merge-load tree cmp f d)))
                    (make-map-entry t (if changed n cnt))))))))))

;; a source no bigger than this goes into an empty sorted coll by plain conj
(define bt-into-conj-max 8)
;; FROM's element count when it is O(1) to know, else #f
(define (bt-known-count from)
  (cond ((pvec? from) (pvec-cnt from))
        ((pmap? from) (pmap-cnt from))
        ((pset? from) (pset-count from))
        ((scoll? from) (scoll-cnt from))
        (else #f)))
;; (into sorted-coll [xform] from): conj after conj's result, built as a batch.
;; Each element folds onto a transient buffer through the coll's own :conj-kvs
;; step (a map's entry rules, a set's element) and :assoc-all takes the buffer
;; whole: a bulk load, a merge, or single inserts, as host-table.ss's batch
;; decides. A source small enough to count goes into an EMPTY coll by plain
;; conj, which costs less than gathering a buffer. XFORM is #f for a plain into.
(define (sorted-into to xform from)
  (let ((n (bt-known-count from)))
    (if (and n (fx<=? n bt-into-conj-max) (fx=? (scoll-cnt to) 0))
        (if xform (into-xform-conj to xform from) (meta-carry-conj to (into-fold jolt-conj1 to from)))
        (let* ((step (sc-op to kw-op-conj-kvs))
               (buf (jolt-transient-new (make-pvec (vector))))
               (res (if xform
                        (let ((xrf (jolt-invoke xform (lambda a (if (fx=? (length a) 1) (car a) (jolt-invoke2 step (car a) (cadr a)))))))
                          (jolt-invoke xrf (into-fold xrf buf from)))
                        (into-fold (lambda (acc x) (jolt-invoke2 step acc x)) buf from))))
          (meta-carry-conj to (sc-call to kw-op-assoc-all (jolt-persistent! res)))))))

;; --- the in-order walk over the B+tree -------------------------------------------
;; The walk is Clojure's PersistentTreeMap$Seq, over leaves: a stack of frames
;; (node . i), innermost first. In a leaf frame I is the next entry out; in an
;; inner frame, the next child to descend. The head is O(log n) away and every
;; later step amortized O(1). It is done here rather than in 25-sorted.clj
;; because a per-node lazy cell written in Clojure cost 1.5x the old eager walk
;; end to end (jolt-r8tz.7); filling a chunk of up to 32 projected entries per
;; step in Scheme beats the eager walk instead, and the result is still a lazy
;; seq — `first`/`take` touch one chunk, not the tree.
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
;; The seq of what is left on STACK: a ChunkedCons-shaped cell over the next
;; entries of one leaf, flavored as the tree seq it is (so chunked-seq? stays false, as
;; it is on the JVM, while reduce still runs the chunk in vec-reduce's loop),
;; whose after-chunk rest is a lazy seq over the stack that remains. The stack and
;; the packed walk (mode*2 + ascending?) are the lazy-src's data, so a seq caught
;; half-walked by a state image restores still lazy.
(define (sc-stack->seq stack code)
  (let* ((asc? (fx=? (fxand code 1) 1)) (st (bt-settle stack asc?)))
    (if (null? st)
        jolt-nil
        ;; one chunk: the rest of the top leaf (at most bt-leaf-max entries).
        ;; Ending a chunk with its leaf keeps the head of a walk (first of an
        ;; rseq or a subseq) at one leaf's entries.
        (let* ((mode (fxsrl code 1)) (n (caar st)) (i0 (cdar st))
               (cnt (if asc? (fx- (bt-size n) i0) (fx+ i0 1)))
               (buf (make-vector cnt)))
          (do ((j 0 (fx+ j 1)) (i i0 (bt-step i asc?))) ((fx=? j cnt))
            (vector-set! buf j (sc-entry-proj n i mode)))
          (let ((rest (bt-settle (cdr st) asc?)))
            (cseq-chunked/k (make-pvec buf)
                            0
                            (if (null? rest) jolt-nil (jolt-make-lazy-src lz-sorted-walk rest code))
                            (sc-mode-kind mode)))))))
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
;; The walk position at the first key at or past K in the walk's direction: (values
;; STACK HIT?), STACK settled (its top a leaf frame with an entry left, or '() when
;; there is none), HIT? whether that entry's key compares equal to K. The
;; comparator is called the way seqFrom calls it, (cmp k node-key).
(define (bt-seek tree cmp k asc?)
  (if (jolt-nil? tree)
      (values '() #f)
      (let loop ((n tree) (stack '()))
        (let* ((sz (bt-size n))
               (r (bt-search cmp n k))
               (i (fxsrl r 1)))
          (cond
            ((bt-leaf? n)
             (let ((st (bt-settle (cons (cons n (if (or asc? (bt-hit? r)) i (fx- i 1))) stack) asc?)))
               ;; a hit stays on top only when the settle did not move off it
               (values st (and (bt-hit? r) (pair? st) (eq? (caar st) n)))))
            (asc? (if (fx=? i sz)
                      (values (bt-settle stack asc?) #f)
                      (loop (bt-val n i) (cons (cons n (fx+ i 1)) stack))))
            (else (let ((i (if (fx=? i sz) (fx- sz 1) i)))
                    (loop (bt-val n i) (cons (cons n (fx- i 1)) stack)))))))))
;; a settled STACK moved one entry on in its direction
(define (bt-advance stack asc?)
  (let ((f (car stack)))
    (bt-settle (cons (cons (car f) (bt-step (cdr f) asc?)) (cdr stack)) asc?)))
;; PersistentTreeMap.seqFrom: the walk from the first key at or past K in the
;; walk's direction, nil when there is none.
(define (sc-tree-seq-from tree asc? mode cmp k)
  (let-values (((st hit) (bt-seek tree cmp k asc?)))
    (sc-stack->seq st (sc-walk-code asc? mode))))
(def-var! "jolt.host" "sorted-seq"
  (lambda (tree asc? mode) (sc-tree-seq tree (jolt-truthy? asc?) mode)))
(def-var! "jolt.host" "sorted-seq-from"
  (lambda (tree asc? mode cmp k) (sc-tree-seq-from tree (jolt-truthy? asc?) mode cmp k)))
;; --- direct access for java.util.TreeMap / TreeSet (java/tree-map.ss) -------------
;; The TreeMap shim keeps a sorted map as its tree and reads and writes it here,
;; under the map's own comparator, without the Clojure ops' dispatch or a seq.
(define (sc-ref sc k nf) (bt-locate (scoll-cmp sc) (scoll-tree sc) k (l i) (bt-val l i) nf))
(define (sc-with sc tree cnt)
  (make-scoll* #f #f (scoll-kind sc) (scoll-tag sc) tree cnt (scoll-cmp sc) (scoll-cmp-fn sc) (scoll-ops sc)))
;; SC with K -> V, in one walk: (values SC' OLD), SC' the new coll (SC itself
;; when nothing changed), OLD K's previous value or NF
(define (sc-put sc k v nf)
  (let ((tree (scoll-tree sc)) (cmp (scoll-cmp sc)))
    (if (jolt-nil? tree)
        (values (sc-with sc (let ((l (bt-alloc #t 1))) (vector-set! l 1 k) (vector-set! l 2 v) l) 1) nf)
        (let-values (((r old) (bt-ins cmp tree k v 3)))
          (cond ((not (eq? old bt-absent)) (values (if r (sc-with sc r (scoll-cnt sc)) sc) old))
                ((pair? r)
                 (let ((t (bt-alloc #f 2)))
                   (vector-set! t 1 (bt-max-key (car r))) (vector-set! t 2 (car r))
                   (vector-set! t 3 (bt-max-key (cdr r))) (vector-set! t 4 (cdr r))
                   (values (sc-with sc t (fx+ (scoll-cnt sc) 1)) nf)))
                (else (values (sc-with sc r (fx+ (scoll-cnt sc) 1)) nf)))))))
;; SC without K, in one walk: (values SC' OLD), SC itself and NF when K is absent
(define (sc-remove sc k nf)
  (let ((tree (scoll-tree sc)))
    (if (jolt-nil? tree)
        (values sc nf)
        (let-values (((r old) (bt-del (scoll-cmp sc) tree k)))
          (if r (values (sc-with sc (bt-trim r) (fx- (scoll-cnt sc) 1)) old) (values sc nf))))))
;; how many entries lie between two bounds, each optional (LO? / HI?) and each
;; inclusive or not: the two ends are sought once and the leaves between them
;; summed, so no key in the middle is compared
(define (sc-count-range sc lo? lo lo-incl hi? hi hi-incl)
  (let* ((tree (scoll-tree sc)) (cmp (scoll-cmp sc))
         (end-at (lambda (on? k incl? asc?)
                   (cond ((jolt-nil? tree) '())
                         ((not on?) (bt-settle (bt-push-down tree asc? '()) asc?))
                         (else (let-values (((st hit) (bt-seek tree cmp k asc?)))
                                 (if (and hit (not incl?)) (bt-advance st asc?) st))))))
         (start (end-at lo? lo lo-incl #t))
         (end (end-at hi? hi hi-incl #f)))
    (if (or (null? start) (null? end))
        0
        (let ((el (caar end)) (ei (cdar end)))
          (if (negative? (bt-compare cmp (bt-key el ei) (bt-key (caar start) (cdar start))))
              0
              (let loop ((st start) (n 0))
                (let ((l (caar st)) (i (cdar st)))
                  (if (eq? l el)
                      (fx+ n (fx+ (fx- ei i) 1))
                      (loop (bt-settle (cdr st) #t) (fx+ n (fx- (bt-size l) i)))))))))))
(define (sc-top-entry st)
  (and (pair? st) (let ((n (caar st)) (i (cdar st))) (make-map-entry (bt-key n i) (bt-val n i)))))
;; the entry at the first key at or past K (strictly past with STRICT?) in the
;; direction ASC?: ceiling / higher ascending, floor / lower descending; #f if none
(define (sc-nav sc k asc? strict?)
  (let-values (((st hit) (bt-seek (scoll-tree sc) (scoll-cmp sc) k asc?)))
    (sc-top-entry (if (and strict? hit) (bt-advance st asc?) st))))
;; the first (ASC?) or last entry, #f when empty
(define (sc-end-entry sc asc?)
  (let ((tree (scoll-tree sc)))
    (and (not (jolt-nil? tree))
         (let* ((l (if asc? (bt-first-leaf tree) (bt-last-leaf tree))) (i (bt-near l asc?)))
           (make-map-entry (bt-key l i) (bt-val l i))))))
;; (f k v acc) over the entries from START (or the end the walk begins at, with
;; START? #f; past an equal key unless INCL?) in direction ASC?, while (keep? k)
(define (sc-walk sc start? start incl? asc? keep? f acc)
  (let* ((tree (scoll-tree sc))
         (st (cond ((jolt-nil? tree) '())
                   ((not start?) (bt-settle (bt-push-down tree asc? '()) asc?))
                   (else (let-values (((st hit) (bt-seek tree (scoll-cmp sc) start asc?)))
                           (if (and hit (not incl?)) (bt-advance st asc?) st))))))
    (let loop ((st st) (acc acc))
      (if (null? st)
          acc
          (let* ((n (caar st)) (sz (bt-size n)))
            (let run ((i (cdar st)) (acc acc))
              (cond ((or (fx<? i 0) (fx>=? i sz)) (loop (bt-settle (cdr st) asc?) acc))
                    ((keep? (bt-key n i)) (run (bt-step i asc?) (f (bt-key n i) (bt-val n i) acc)))
                    (else acc))))))))

;; (f k v acc) over every entry, in order
(define (sc-tree-fold-kv t f acc)
  (if (jolt-nil? t)
      acc
      (let walk ((n t) (acc acc))
        (let ((sz (bt-size n)))
          (if (bt-leaf? n)
              (do ((i 0 (fx+ i 1)) (acc acc (f (bt-key n i) (bt-val n i) acc))) ((fx=? i sz) acc))
              (do ((i 0 (fx+ i 1)) (acc acc (walk (bt-val n i) acc))) ((fx=? i sz) acc)))))))
;; (step acc k v) over every entry, in order, stopping at a reduced acc, which is
;; answered still wrapped
(define (sc-tree-reduce-kv t step init)
  (if (jolt-nil? t)
      init
      (let walk ((n t) (acc init))
        (let ((sz (bt-size n)))
          (let loop ((i 0) (acc acc))
            (cond ((or (fx=? i sz) (jolt-reduced? acc)) acc)
                  ((bt-leaf? n) (loop (fx+ i 1) (step acc (bt-key n i) (bt-val n i))))
                  (else (loop (fx+ i 1) (walk (bt-val n i) acc)))))))))
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
;; get and contains? read the tree directly rather than through the :get /
;; :contains ops: they are the hot lookups, and the ops answer exactly this (a
;; map's stored value, a set's stored element, presence)
(register-get-arm! htable-sorted?
  (lambda (coll k d)
    (if (htable-sorted-map? coll)
        (sc-ref coll k d)
        (bt-locate (scoll-cmp coll) (scoll-tree coll) k (l i) (bt-key l i) d))))
(register-contains-arm! htable-sorted?
  (lambda (coll k) (bt-locate (scoll-cmp coll) (scoll-tree coll) k (l i) #t #f)))
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
      (sc-tree-seq (scoll-tree m) #t 1)
      (%h-keys m))))
(define %h-vals jolt-vals)
(set! jolt-vals (lambda (m)
  (if (htable-sorted-map? m)
      (sc-tree-seq (scoll-tree m) #t 2)
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
  (sc-tree-fold-kv (scoll-tree sc) (lambda (k v m) (pmap-assoc m k v)) empty-pmap))
(define (sorted-set->pset sc)
  (sc-tree-fold-kv (scoll-tree sc) (lambda (k v s) (pset-conj s k)) empty-pset))
(define (sorted->plain x) (if (htable-sorted-map? x) (sorted-map->pmap x) (sorted-set->pset x)))
;; The common pairings answer without building anything, the way APersistentMap /
;; APersistentSet.equiv do: counts first, then one walk of the sorted tree. Against
;; a hash coll each entry is probed there (hash lookup, the same test the converted
;; compare made); against a sorted coll with the SAME comparator the two trees walk
;; in lockstep, which is O(n) with no comparator calls at all. 'eq / 'ne, or #f to
;; fall back to the conversion (a different comparator, a record, a Java map, …).
(define (sc-count x) (scoll-cnt x))
(define (sorted-fast= s o)
  (define (verdict b) (if b 'eq 'ne))
  (define (same-cmp? o) (eq? (scoll-cmp-fn s) (scoll-cmp-fn o)))
  (define (lockstep same?) (sc-tree-lockstep? (scoll-tree s) (scoll-tree o) same?))
  (if (htable-sorted-map? s)
      (cond
        ((pmap? o)
         (verdict (and (eqv? (sc-count s) (pmap-cnt o))
                       (sc-tree-every-kv? (scoll-tree s)
                         (lambda (k v) (let ((p (pmap-entry-at o k)))
                                         (and p (jolt=2 v (cdr p)))))))))
        ((and (htable-sorted-map? o) (not (eqv? (sc-count s) (sc-count o)))) 'ne)
        ((and (htable-sorted-map? o) (same-cmp? o))
         (verdict (lockstep (lambda (ka va kb vb) (and (jolt=2 ka kb) (jolt=2 va vb))))))
        (else #f))
      (cond
        ((pset? o)
         (verdict (and (eqv? (sc-count s) (pset-count o))
                       (sc-tree-every-kv? (scoll-tree s)
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
      (let* ((t (scoll-tree x))
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
