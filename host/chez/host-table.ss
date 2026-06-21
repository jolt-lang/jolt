;; host tables + sorted collections (jolt-cf1q.3, jolt-0zoy) — the jolt.host
;; value primitives and the 25-sorted tier's runtime.
;;
;; jolt.host/tagged-table + ref-put! + ref-get resolved to jolt-nil on the Chez
;; prelude, so the whole sorted tier (sorted-map/sorted-set/subseq/rsubseq) AND
;; every overlay fn that calls (sorted? x) — empty, ifn?, reversible?, map?, set?,
;; coll? — hit the apply-jolt-nil crash bucket. This provides:
;;   1. tagged-table / ref-put! / ref-get over a Chez mutable tagged-table type
;;      (a string-keyed hashtable wrapped in an `htable` record), def-var!'d into
;;      the jolt.host ns. The sorted tier (25-sorted.clj) mints its wrapper with
;;      these — a red-black tree + :ops table travel inside the htable.
;;   2. a sorted-coll arm on the collection dispatchers, set!-extended the same
;;      way records.ss extends them for jrec: each op routes through the value's
;;      own :ops table (the seed's dispatch pattern, core_coll.janet). first/rest/
;;      next/last fall out free once jolt-seq has a sorted arm (they seq first).
;;
;; Loaded LAST (after records.ss / transients.ss / natives-meta.ss): it wraps the
;; jrec-extended dispatchers + value-host-tags, delegating to the captured prior.

;; --- jolt.host primitives ----------------------------------------------------
;; A tagged-table: a string-keyed hashtable (keyword field -> value). Keyword
;; keys collapse to their ns/name string so interning isn't relied on.
(define-record-type htable (fields (immutable h)) (nongenerative chez-htable-v1))
(define (kw->key k)
  (let ((ns (keyword-t-ns k)))
    (if (and ns (not (jolt-nil? ns))) (string-append ns "/" (keyword-t-name k)) (keyword-t-name k))))
(define (jolt-tagged-table tag)
  (let ((h (make-hashtable string-hash string=?)))
    (hashtable-set! h "jolt/type" tag)
    (make-htable h)))
;; ref-put! threads the table back; a nil value REMOVES the key (Janet table
;; semantics — h-ref-put!). Errors on a non-htable so the atom-watch / volatile
;; uses (which pass a different ref type and have no Chez table yet) stay a crash
;; rather than silently diverging.
(define (jolt-ref-put! t k v)
  (unless (htable? t) (error #f "ref-put!: not a host table" t))
  (if (jolt-nil? v)
      (hashtable-delete! (htable-h t) (kw->key k))
      (hashtable-set! (htable-h t) (kw->key k) v))
  t)
(define (jolt-ref-get t k)
  (if (htable? t) (hashtable-ref (htable-h t) (kw->key k) jolt-nil) jolt-nil))

(def-var! "jolt.host" "tagged-table" jolt-tagged-table)
(def-var! "jolt.host" "ref-put!" jolt-ref-put!)
(def-var! "jolt.host" "ref-get" jolt-ref-get)

;; --- sorted-coll recognition + ops access ------------------------------------
(define kw-jtype (keyword "jolt" "type"))
(define kw-sorted-map (keyword "jolt" "sorted-map"))
(define kw-sorted-set (keyword "jolt" "sorted-set"))
(define kw-ops (keyword #f "ops"))
(define kw-op-count (keyword #f "count"))
(define kw-op-seq (keyword #f "seq"))
(define kw-op-get (keyword #f "get"))
(define kw-op-contains (keyword #f "contains"))
(define kw-op-assoc (keyword #f "assoc"))
(define kw-op-dissoc (keyword #f "dissoc"))
(define kw-op-conj (keyword #f "conj"))
(define kw-op-disj (keyword #f "disj"))

(define (htable-sorted-map? x) (and (htable? x) (jolt=2 (jolt-ref-get x kw-jtype) kw-sorted-map)))
(define (htable-sorted-set? x) (and (htable? x) (jolt=2 (jolt-ref-get x kw-jtype) kw-sorted-set)))
(define (htable-sorted? x) (or (htable-sorted-map? x) (htable-sorted-set? x)))
;; the op fn for `op-kw` from the value's attached :ops map, then invoke it on sc.
(define (sc-op sc op-kw) (jolt-get (jolt-ref-get sc kw-ops) op-kw jolt-nil))
(define (sc-call sc op-kw . args) (apply jolt-invoke (sc-op sc op-kw) sc args))

;; --- extend the collection dispatchers with a sorted arm ---------------------
(define %h-seq jolt-seq)
(set! jolt-seq (lambda (x) (if (htable-sorted? x) (sc-call x kw-op-seq) (%h-seq x))))
(define %h-count jolt-count)
(set! jolt-count (lambda (coll) (if (htable-sorted? coll) (sc-call coll kw-op-count) (%h-count coll))))
(define %h-get jolt-get)
(set! jolt-get (case-lambda
  ((coll k)   (if (htable-sorted? coll) (sc-call coll kw-op-get k jolt-nil) (%h-get coll k)))
  ((coll k d) (if (htable-sorted? coll) (sc-call coll kw-op-get k d) (%h-get coll k d)))))
(define %h-contains? jolt-contains?)
(set! jolt-contains? (lambda (coll k)
  (if (htable-sorted? coll) (if (jolt-truthy? (sc-call coll kw-op-contains k)) #t #f) (%h-contains? coll k))))
(define %h-assoc1 jolt-assoc1)
(set! jolt-assoc1 (lambda (coll k v)
  (if (htable-sorted-map? coll) (sc-call coll kw-op-assoc (jolt-vector k v)) (%h-assoc1 coll k v))))
(define %h-dissoc jolt-dissoc)
(set! jolt-dissoc (lambda (coll . ks)
  (if (htable-sorted-map? coll) (sc-call coll kw-op-dissoc (apply jolt-vector ks)) (apply %h-dissoc coll ks))))
(define %h-conj1 jolt-conj1)
(set! jolt-conj1 (lambda (coll x)
  (if (htable-sorted? coll) (sc-call coll kw-op-conj (jolt-vector x)) (%h-conj1 coll x))))
(define %h-disj jolt-disj)
(set! jolt-disj (lambda (s . xs)
  (if (htable-sorted-set? s) (sc-call s kw-op-disj (apply jolt-vector xs)) (apply %h-disj s xs))))
(def-var! "clojure.core" "disj" jolt-disj)
(define %h-empty? jolt-empty?)
(set! jolt-empty? (lambda (coll) (if (htable-sorted? coll) (zero? (sc-call coll kw-op-count)) (%h-empty? coll))))
(define %h-keys jolt-keys)
(set! jolt-keys (lambda (m)
  (if (htable-sorted-map? m)
      (list->cseq (map (lambda (e) (jolt-nth e 0)) (seq->list (sc-call m kw-op-seq))))
      (%h-keys m))))
(define %h-vals jolt-vals)
(set! jolt-vals (lambda (m)
  (if (htable-sorted-map? m)
      (list->cseq (map (lambda (e) (jolt-nth e 1)) (seq->list (sc-call m kw-op-seq))))
      (%h-vals m))))
;; sorted colls are collections (callable as fns via jolt-invoke, conj-able).
(define %h-coll? jolt-coll?)
(set! jolt-coll? (lambda (x) (or (htable-sorted? x) (%h-coll? x))))

;; public predicates: a sorted-map is map?, a sorted-set is set?, both coll?.
;; predicates.ss/records.ss def-var!'d a snapshot, so re-def-var! after set!.
(define %h-map? jolt-map?)
(set! jolt-map? (lambda (x) (or (htable-sorted-map? x) (%h-map? x))))
(def-var! "clojure.core" "map?" jolt-map?)
(define %h-set? jolt-set?)
(set! jolt-set? (lambda (x) (or (htable-sorted-set? x) (%h-set? x))))
(def-var! "clojure.core" "set?" jolt-set?)
(def-var! "clojure.core" "coll?" (lambda (x) (or (htable-sorted? x) (jrec? x) (jolt-coll-pred? x))))

;; --- equality / hash ---------------------------------------------------------
;; A sorted coll canonicalizes like its unordered counterpart (core_types.janet):
;; a sorted-map equals ANY map (hash or sorted) with the same entries, a
;; sorted-set ANY set with the same elements — the comparator is irrelevant to =.
;; Convert to the plain persistent coll and delegate to the prior jolt=2 / hash.
;; (htable-sorted? short-circuits on a non-htable BEFORE any jolt=2, so extending
;; jolt=2 here doesn't recurse: the inner tag compare gets two keywords.)
(define (sorted-map->pmap sc)
  (fold-left (lambda (m e) (pmap-assoc m (jolt-nth e 0) (jolt-nth e 1)))
             empty-pmap (seq->list (sc-call sc kw-op-seq))))
(define (sorted-set->pset sc)
  (fold-left (lambda (s x) (pset-conj s x)) empty-pset (seq->list (sc-call sc kw-op-seq))))
(define (sorted->plain x) (if (htable-sorted-map? x) (sorted-map->pmap x) (sorted-set->pset x)))
(define %h-jolt=2 jolt=2)
(set! jolt=2 (lambda (a b)
  (cond ((htable-sorted? a) (%h-jolt=2 (sorted->plain a) (if (htable-sorted? b) (sorted->plain b) b)))
        ((htable-sorted? b) (%h-jolt=2 a (sorted->plain b)))
        (else (%h-jolt=2 a b)))))
(define %h-jolt-hash jolt-hash)
(set! jolt-hash (lambda (x) (if (htable-sorted? x) (%h-jolt-hash (sorted->plain x)) (%h-jolt-hash x))))

;; --- printing ----------------------------------------------------------------
;; sorted colls render in SORTED order (the value's :seq), not HAMT order — and
;; a sorted-map prints "{k v, k v}" (", " between pairs) like the seed's
;; pr-render-pairs, NOT the space-only form the unordered pmap arm uses.
(define (sorted-map-render sc render)
  (string-append "{"
    (let loop ((es (seq->list (sc-call sc kw-op-seq))) (first #t) (acc ""))
      (if (null? es) acc
          (loop (cdr es) #f
                (string-append acc (if first "" ", ")
                               (render (jolt-nth (car es) 0)) " " (render (jolt-nth (car es) 1))))))
    "}"))
(define (sorted-set-render sc render)
  (string-append "#{" (jolt-str-join (map render (seq->list (sc-call sc kw-op-seq)))) "}"))
(define (sorted-render x render)
  (if (htable-sorted-map? x) (sorted-map-render x render) (sorted-set-render x render)))

(define %h-pr-readable jolt-pr-readable)
(set! jolt-pr-readable (lambda (x) (if (htable-sorted? x) (sorted-render x jolt-pr-readable) (%h-pr-readable x))))
(define %h-pr-str jolt-pr-str)
(set! jolt-pr-str (lambda (x) (if (htable-sorted? x) (sorted-render x jolt-pr-str) (%h-pr-str x))))
(define %h-str-render-one jolt-str-render-one)
(set! jolt-str-render-one (lambda (x) (if (htable-sorted? x) (sorted-render x jolt-str-render-one) (%h-str-render-one x))))

;; --- protocol dispatch over builtins (extend-protocol Map/Set on sorted) ------
;; value-host-tags (records.ss) drives extend-protocol on host values; a
;; sorted-map must answer to "Map", a sorted-set to "Set"/"Collection".
(define %h-value-host-tags value-host-tags)
(set! value-host-tags (lambda (obj)
  (cond
    ((htable-sorted-map? obj) '("PersistentTreeMap" "Sorted" "IPersistentMap" "Associative"
                                "Map" "java.util.Map" "IPersistentCollection" "Object"))
    ((htable-sorted-set? obj) '("PersistentTreeSet" "Sorted" "IPersistentSet"
                                "Set" "java.util.Set" "Collection" "IPersistentCollection" "Object"))
    (else (%h-value-host-tags obj)))))
