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

;; --- the red-black tree --------------------------------------------------------
;; Rich Hickey's PersistentTreeMap algorithm (via cljs.core's port: tree-map-add,
;; balance-left/right, tree-map-append, balance-*-del), which 25-sorted.clj
;; carries the semantics around. A node is a 5-slot Scheme vector
;; #(red? key val left right), red? a boolean and an absent child jolt-nil. It
;; lived in 25-sorted.clj as a jolt vector [:red k v l r], where every field read
;; was a generic nth and every node a persistent vector: ~200ns a tree level
;; against the JVM's ~25ns, and a sorted-set build 4.5x the JVM's. Here a field
;; read is a vector-ref.
;;
;; Not image surface: an image writes a sorted collection out as its entries and
;; rebuilds it (state-image.ss walk-sorted), so the node layout never travels.
;;
;; CMP is the collection's comparator as 25-sorted.clj holds it, or jolt-nil for
;; natural order, which is Util.compare (jolt-compare) and not the clojure.core
;; var: the JVM's PersistentTreeMap orders by RT.DEFAULT_COMPARATOR, which a
;; rebinding of #'compare does not reach. Every comparison is (cmp k node-key),
;; once per level, as the reference makes it; the corpus pins the counts.
;;
;; The node helpers are macros, not procedures: a top-level procedure is a call
;; Chez cannot inline across, and an insert makes a dozen of these per level.
;; Only this file uses them as macros; sc-nd-key and friends below are the
;; procedures the other files call.
(define-syntax rb-red?
  (syntax-rules () ((_ n) (let ((x n)) (and (not (jolt-nil? x)) (vector-ref x 0))))))
(define-syntax rb-black?
  (syntax-rules () ((_ n) (let ((x n)) (and (not (jolt-nil? x)) (not (vector-ref x 0)))))))
(define-syntax rb-red (syntax-rules () ((_ k v l r) (vector #t k v l r))))
(define-syntax rb-black (syntax-rules () ((_ k v l r) (vector #f k v l r))))
(define-syntax rb-key (syntax-rules () ((_ n) (vector-ref n 1))))
(define-syntax rb-val (syntax-rules () ((_ n) (vector-ref n 2))))
(define-syntax rb-left (syntax-rules () ((_ n) (vector-ref n 3))))
(define-syntax rb-right (syntax-rules () ((_ n) (vector-ref n 4))))
(define (sc-nd-key n) (rb-key n))
(define (sc-nd-val n) (rb-val n))
(define (sc-nd-left n) (rb-left n))
(define (sc-nd-right n) (rb-right n))
;; BlackNode.blacken is the node itself; RedNode.blacken a black copy. redden is
;; only ever applied to a known-black node.
(define (rb-blacken n)
  (if (rb-red? n) (rb-black (rb-key n) (rb-val n) (rb-left n) (rb-right n)) n))
(define (rb-redden n) (rb-red (rb-key n) (rb-val n) (rb-left n) (rb-right n)))
;; natural order on two fixnums answers in line; anything else goes through
;; Util.compare or the comparator
(define (rb-compare-slow cmp a b)
  (if (jolt-nil? cmp) (jolt-compare a b) (jolt-invoke cmp a b)))
(define-syntax rb-compare
  (syntax-rules ()
    ((_ cmp a b)
     (let ((c cmp) (x a) (y b))
       (if (and (jolt-nil? c) (fixnum? x) (fixnum? y))
           (cond ((fx<? x y) -1) ((fx=? x y) 0) (else 1))
           (rb-compare-slow c x y))))))

;; insert balancing: RedNode/BlackNode .balanceLeft/.balanceRight
(define (rb-ins-balance-left ins p)
  (if (rb-red? ins)
      (let ((l (rb-left ins)) (r (rb-right ins)))
        (cond
          ((rb-red? l) (rb-red (rb-key ins) (rb-val ins)
                               (rb-blacken l)
                               (rb-black (rb-key p) (rb-val p) r (rb-right p))))
          ((rb-red? r) (rb-red (rb-key r) (rb-val r)
                               (rb-black (rb-key ins) (rb-val ins) l (rb-left r))
                               (rb-black (rb-key p) (rb-val p) (rb-right r) (rb-right p))))
          (else (rb-black (rb-key p) (rb-val p) ins (rb-right p)))))
      (rb-black (rb-key p) (rb-val p) ins (rb-right p))))
(define (rb-ins-balance-right ins p)
  (if (rb-red? ins)
      (let ((l (rb-left ins)) (r (rb-right ins)))
        (cond
          ((rb-red? r) (rb-red (rb-key ins) (rb-val ins)
                               (rb-black (rb-key p) (rb-val p) (rb-left p) l)
                               (rb-blacken r)))
          ((rb-red? l) (rb-red (rb-key l) (rb-val l)
                               (rb-black (rb-key p) (rb-val p) (rb-left p) (rb-left l))
                               (rb-black (rb-key ins) (rb-val ins) (rb-right l) r)))
          (else (rb-black (rb-key p) (rb-val p) (rb-left p) ins))))
      (rb-black (rb-key p) (rb-val p) (rb-left p) ins)))
(define (rb-add-left p ins)
  (if (rb-red? p)
      (rb-red (rb-key p) (rb-val p) ins (rb-right p))
      (rb-ins-balance-left ins p)))
(define (rb-add-right p ins)
  (if (rb-red? p)
      (rb-red (rb-key p) (rb-val p) (rb-left p) ins)
      (rb-ins-balance-right ins p)))

;; Insert in ONE walk: the new subtree, or #f when k is present, with that node
;; left in FOUND (PersistentTreeMap.add answers null and fills its box the same
;; way).
(define (rb-ins cmp t k v found)
  (if (jolt-nil? t)
      (rb-red k v jolt-nil jolt-nil)
      (let ((c (rb-compare cmp k (rb-key t))))
        (cond
          ((zero? c) (set-box! found t) #f)
          ((negative? c)
           (let ((ins (rb-ins cmp (rb-left t) k v found)))
             (and ins (rb-add-left t ins))))
          (else
           (let ((ins (rb-ins cmp (rb-right t) k v found)))
             (and ins (rb-add-right t ins))))))))

;; Replace the value at a present key, keeping the tree's shape and the node's
;; own (first-inserted) key, as PersistentTreeMap.replace does.
(define (rb-replace cmp t k v)
  (let ((c (rb-compare cmp k (rb-key t))))
    (vector (vector-ref t 0) (rb-key t)
            (if (zero? c) v (rb-val t))
            (if (negative? c) (rb-replace cmp (rb-left t) k v) (rb-left t))
            (if (positive? c) (rb-replace cmp (rb-right t) k v) (rb-right t)))))

(define (rb-lookup cmp t k)
  (let loop ((t t))
    (if (jolt-nil? t)
        jolt-nil
        (let ((c (rb-compare cmp k (rb-key t))))
          (cond ((zero? c) t)
                ((negative? c) (loop (rb-left t)))
                (else (loop (rb-right t))))))))

;; delete balancing: cljs's standalone balance-left/-right and *-del
(define (rb-balance-left k v ins right)
  (if (rb-red? ins)
      (let ((il (rb-left ins)) (ir (rb-right ins)))
        (cond
          ((rb-red? il) (rb-red (rb-key ins) (rb-val ins) (rb-blacken il) (rb-black k v ir right)))
          ((rb-red? ir) (rb-red (rb-key ir) (rb-val ir)
                                (rb-black (rb-key ins) (rb-val ins) il (rb-left ir))
                                (rb-black k v (rb-right ir) right)))
          (else (rb-black k v ins right))))
      (rb-black k v ins right)))
(define (rb-balance-right k v left ins)
  (if (rb-red? ins)
      (let ((il (rb-left ins)) (ir (rb-right ins)))
        (cond
          ((rb-red? ir) (rb-red (rb-key ins) (rb-val ins) (rb-black k v left il) (rb-blacken ir)))
          ((rb-red? il) (rb-red (rb-key il) (rb-val il)
                                (rb-black k v left (rb-left il))
                                (rb-black (rb-key ins) (rb-val ins) (rb-right il) ir)))
          (else (rb-black k v left ins))))
      (rb-black k v left ins)))
(define (rb-invariant-violation)
  (jolt-throw (jolt-ex-info "red-black tree invariant violation" (jolt-hash-map))))
(define (rb-balance-left-del k v del right)
  (cond
    ((rb-red? del) (rb-red k v (rb-blacken del) right))
    ((rb-black? right) (rb-balance-right k v del (rb-redden right)))
    ((and (rb-red? right) (rb-black? (rb-left right)))
     (let ((rl (rb-left right)))
       (rb-red (rb-key rl) (rb-val rl)
               (rb-black k v del (rb-left rl))
               (rb-balance-right (rb-key right) (rb-val right) (rb-right rl)
                                 (rb-redden (rb-right right))))))
    (else (rb-invariant-violation))))
(define (rb-balance-right-del k v left del)
  (cond
    ((rb-red? del) (rb-red k v left (rb-blacken del)))
    ((rb-black? left) (rb-balance-left k v (rb-redden left) del))
    ((and (rb-red? left) (rb-black? (rb-right left)))
     (let ((lr (rb-right left)))
       (rb-red (rb-key lr) (rb-val lr)
               (rb-balance-left (rb-key left) (rb-val left) (rb-redden (rb-left left))
                                (rb-left lr))
               (rb-black k v (rb-right lr) del))))
    (else (rb-invariant-violation))))
;; merge the two subtrees of a removed node
(define (rb-append left right)
  (cond
    ((jolt-nil? left) right)
    ((jolt-nil? right) left)
    ((rb-red? left)
     (if (rb-red? right)
         (let ((app (rb-append (rb-right left) (rb-left right))))
           (if (rb-red? app)
               (rb-red (rb-key app) (rb-val app)
                       (rb-red (rb-key left) (rb-val left) (rb-left left) (rb-left app))
                       (rb-red (rb-key right) (rb-val right) (rb-right app) (rb-right right)))
               (rb-red (rb-key left) (rb-val left) (rb-left left)
                       (rb-red (rb-key right) (rb-val right) app (rb-right right)))))
         (rb-red (rb-key left) (rb-val left) (rb-left left) (rb-append (rb-right left) right))))
    ((rb-red? right)
     (rb-red (rb-key right) (rb-val right) (rb-append left (rb-left right)) (rb-right right)))
    (else
     (let ((app (rb-append (rb-right left) (rb-left right))))
       (if (rb-red? app)
           (rb-red (rb-key app) (rb-val app)
                   (rb-black (rb-key left) (rb-val left) (rb-left left) (rb-left app))
                   (rb-black (rb-key right) (rb-val right) (rb-right app) (rb-right right)))
           (rb-balance-left-del (rb-key left) (rb-val left) (rb-left left)
                                (rb-black (rb-key right) (rb-val right) app (rb-right right))))))))
;; remove k, which the caller has found present
(define (rb-del cmp t k)
  (let ((c (rb-compare cmp k (rb-key t))))
    (cond
      ((zero? c) (rb-append (rb-left t) (rb-right t)))
      ((negative? c)
       (let ((del (rb-del cmp (rb-left t) k)))
         (if (rb-black? (rb-left t))
             (rb-balance-left-del (rb-key t) (rb-val t) del (rb-right t))
             (rb-red (rb-key t) (rb-val t) del (rb-right t)))))
      (else
       (let ((del (rb-del cmp (rb-right t) k)))
         (if (rb-black? (rb-right t))
             (rb-balance-right-del (rb-key t) (rb-val t) (rb-left t) del)
             (rb-red (rb-key t) (rb-val t) (rb-left t) del)))))))

;; The ops 25-sorted.clj calls. A FOUND answer from rbt-insert is a pair holding
;; the present node, which rbt-found unwraps; a tree is always a node (a vector)
;; or nil, so the two cannot be confused.
(def-var! "jolt.host" "rbt-insert"
  (lambda (tree cmp k v)
    (let* ((found (box #f)) (t (rb-ins cmp tree k v found)))
      (if t (rb-blacken t) (cons 'rbt-found (unbox found))))))
(def-var! "jolt.host" "rbt-found"
  (lambda (r) (if (pair? r) (cdr r) jolt-nil)))
(def-var! "jolt.host" "rbt-replace" (lambda (tree cmp k v) (rb-replace cmp tree k v)))
(def-var! "jolt.host" "rbt-lookup" (lambda (tree cmp k) (rb-lookup cmp tree k)))
(def-var! "jolt.host" "rbt-delete"
  (lambda (tree cmp k) (let ((t (rb-del cmp tree k))) (if (jolt-nil? t) t (rb-blacken t)))))
(def-var! "jolt.host" "rbt-key" (lambda (n) (rb-key n)))
(def-var! "jolt.host" "rbt-val" (lambda (n) (rb-val n)))
;; the leftmost node, down one spine: PersistentTreeMap.min
(def-var! "jolt.host" "rbt-leftmost"
  (lambda (n) (let loop ((n n)) (let ((l (rb-left n))) (if (jolt-nil? l) n (loop l))))))

;; --- the in-order walk over the red-black tree -------------------------------
;; Over the nodes above; nil children are jolt-nil. The walk is Clojure's PersistentTreeMap$Seq: a stack
;; of the nodes still to visit, whose top is the next one out, so the head is
;; O(log n) away and every later step is amortized O(1). It is done here rather
;; than in 25-sorted.clj because a per-node lazy cell written in Clojure cost
;; 1.5x the old eager walk end to end (jolt-r8tz.7); filling a chunk of up to 32
;; projected nodes per step in Scheme beats the eager walk instead, and the result
;; is still a lazy seq — `first`/`take` touch one chunk, not the tree.
(define kw-tree (keyword #f "tree"))
(define kw-cnt (keyword #f "cnt"))
(define kw-cmp (keyword #f "cmp"))
;; push n and its whole near-side spine: the left one walking up, the right one down
(define (sc-push-spine n asc? stack)
  (if (jolt-nil? n)
      stack
      (sc-push-spine (if asc? (sc-nd-left n) (sc-nd-right n)) asc? (cons n stack))))
;; what a node walks as. mode 0: a map entry (PersistentTreeMap$Seq); 1: its key
;; (a sorted set's seq, or (keys m) — APersistentMap$KeySeq); 2: its val (ValSeq).
(define (sc-node-proj n mode)
  (cond ((fx=? mode 0) (make-map-entry (sc-nd-key n) (sc-nd-val n)))
        ((fx=? mode 1) (sc-nd-key n))
        (else (sc-nd-val n))))
(define (sc-mode-kind mode)
  (cond ((fx=? mode 0) sk-treemap-seq) ((fx=? mode 1) sk-key-seq) (else sk-val-seq)))
(define sc-walk-chunk 32)
;; The seq of what is left on STACK: a ChunkedCons-shaped cell over the next
;; <=32 nodes, flavored as the tree seq it is (so chunked-seq? stays false, as it
;; is on the JVM, while reduce still runs the chunk in vec-reduce's loop), whose
;; after-chunk rest is a lazy seq over the stack that remains. The stack and the
;; packed walk (mode*2 + ascending?) are the lazy-src's data, so a seq caught
;; half-walked by a state image restores still lazy.
(define (sc-stack->seq stack code)
  (if (null? stack)
      jolt-nil
      (let ((asc? (fx=? (fxand code 1) 1)) (mode (fxsrl code 1))
            (buf (make-vector sc-walk-chunk)))
        (let loop ((i 0) (st stack))
          (if (or (null? st) (fx=? i sc-walk-chunk))
              (cseq-chunked/k (make-pvec (if (fx=? i sc-walk-chunk) buf (vec-copy-range buf 0 i)))
                              0
                              (if (null? st) jolt-nil (jolt-make-lazy-src lz-sorted-walk st code))
                              (sc-mode-kind mode))
              (let ((n (car st)))
                (vector-set! buf i (sc-node-proj n mode))
                (loop (fx+ i 1)
                      (sc-push-spine (if asc? (sc-nd-right n) (sc-nd-left n)) asc? (cdr st)))))))))
(define lz-sorted-walk
  (register-lazy-src! 'sorted-walk (lambda (st code) (sc-stack->seq st code))))
(define (sc-walk-code asc? mode) (fx+ (fx* mode 2) (if asc? 1 0)))
;; the whole tree, ascending or descending
(define (sc-tree-seq tree asc? mode)
  (sc-stack->seq (sc-push-spine tree asc? '()) (sc-walk-code asc? mode)))
;; PersistentTreeMap.seqFrom: the walk from the first key at or past K in the
;; walk's direction, nil when there is none. The comparator is called the way
;; seqFrom calls it, (cmp k node-key).
(define (sc-tree-seq-from tree asc? mode cmp k)
  (let loop ((t tree) (stack '()))
    (if (jolt-nil? t)
        (sc-stack->seq stack (sc-walk-code asc? mode))
        (let ((c (jolt-invoke cmp k (sc-nd-key t))))
          (cond ((zero? c) (sc-stack->seq (cons t stack) (sc-walk-code asc? mode)))
                (asc? (if (negative? c) (loop (sc-nd-left t) (cons t stack)) (loop (sc-nd-right t) stack)))
                (else (if (positive? c) (loop (sc-nd-right t) (cons t stack)) (loop (sc-nd-left t) stack))))))))
(def-var! "jolt.host" "sorted-seq"
  (lambda (tree asc? mode) (sc-tree-seq tree (jolt-truthy? asc?) mode)))
(def-var! "jolt.host" "sorted-seq-from"
  (lambda (tree asc? mode cmp k) (sc-tree-seq-from tree (jolt-truthy? asc?) mode cmp k)))
;; every node, in order, satisfies pred (stops at the first that does not)
(define (sc-tree-every? t pred)
  (or (jolt-nil? t)
      (and (sc-tree-every? (sc-nd-left t) pred)
           (pred t)
           (sc-tree-every? (sc-nd-right t) pred))))
(define (sc-tree-fold t f acc)
  (if (jolt-nil? t)
      acc
      (sc-tree-fold (sc-nd-right t) f (f t (sc-tree-fold (sc-nd-left t) f acc)))))

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
  (sc-tree-fold (jolt-ref-get sc kw-tree)
                (lambda (n m) (pmap-assoc m (sc-nd-key n) (sc-nd-val n))) empty-pmap))
(define (sorted-set->pset sc)
  (sc-tree-fold (jolt-ref-get sc kw-tree) (lambda (n s) (pset-conj s (sc-nd-key n))) empty-pset))
(define (sorted->plain x) (if (htable-sorted-map? x) (sorted-map->pmap x) (sorted-set->pset x)))
;; The common pairings answer without building anything, the way APersistentMap /
;; APersistentSet.equiv do: counts first, then one walk of the sorted tree. Against
;; a hash coll each node is probed there (hash lookup, the same test the converted
;; compare made); against a sorted coll with the SAME comparator the two trees walk
;; in lockstep, which is O(n) with no comparator calls at all. 'eq / 'ne, or #f to
;; fall back to the conversion (a different comparator, a record, a Java map, …).
(define (sc-count x) (jolt-ref-get x kw-cnt))
(define (sorted-fast= s o)
  (define (verdict b) (if b 'eq 'ne))
  (define (same-cmp? o) (eq? (jolt-ref-get s kw-cmp-fn) (jolt-ref-get o kw-cmp-fn)))
  (define (lockstep node=?)
    (let loop ((sa (sc-push-spine (jolt-ref-get s kw-tree) #t '()))
               (sb (sc-push-spine (jolt-ref-get o kw-tree) #t '())))
      (cond ((null? sa) (null? sb))
            ((null? sb) #f)
            (else (let ((na (car sa)) (nb (car sb)))
                    (and (node=? na nb)
                         (loop (sc-push-spine (sc-nd-right na) #t (cdr sa))
                               (sc-push-spine (sc-nd-right nb) #t (cdr sb)))))))))
  (if (htable-sorted-map? s)
      (cond
        ((pmap? o)
         (verdict (and (eqv? (sc-count s) (pmap-cnt o))
                       (sc-tree-every? (jolt-ref-get s kw-tree)
                         (lambda (n) (let ((p (pmap-entry-at o (sc-nd-key n))))
                                       (and p (jolt=2 (sc-nd-val n) (cdr p)))))))))
        ((and (htable-sorted-map? o) (not (eqv? (sc-count s) (sc-count o)))) 'ne)
        ((and (htable-sorted-map? o) (same-cmp? o))
         (verdict (lockstep (lambda (a b) (and (jolt=2 (sc-nd-key a) (sc-nd-key b))
                                               (jolt=2 (sc-nd-val a) (sc-nd-val b)))))))
        (else #f))
      (cond
        ((pset? o)
         (verdict (and (eqv? (sc-count s) (pset-count o))
                       (sc-tree-every? (jolt-ref-get s kw-tree)
                         (lambda (n) (pset-contains? o (sc-nd-key n)))))))
        ((and (htable-sorted-set? o) (not (eqv? (sc-count s) (sc-count o)))) 'ne)
        ((and (htable-sorted-set? o) (same-cmp? o))
         (verdict (lockstep (lambda (a b) (jolt=2 (sc-nd-key a) (sc-nd-key b))))))
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
                      (sc-tree-fold t (lambda (n acc) (add32 acc (entry-hasheq (sc-nd-key n) (sc-nd-val n)))) 0)
                      (sc-tree-fold t (lambda (n acc) (+ acc (jolt-hasheq (sc-nd-key n)))) 0)))
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
