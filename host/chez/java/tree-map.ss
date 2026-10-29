;; tree-map.ss — java.util.TreeMap and java.util.TreeSet, the views they hand
;; out (headMap / tailMap / subMap / descendingMap, keySet / values / entrySet,
;; headSet / tailSet / subSet / descendingSet), and the Comparator objects
;; Comparator/reverseOrder, Comparator/naturalOrder and Collections/reverseOrder
;; return. Shared by every target.
;;
;; The tree is clojure.core's own sorted map (jolt-core 25-sorted.clj) held in a
;; mutable root: a put swaps in the assoc'd map, so the red-black tree, its key
;; equality (comparator zero, first key kept), subseq / rsubseq navigation and the
;; comparator seam (jolt-comparator-fn: a Clojure fn, a reified
;; java.util.Comparator, a host Comparator object) are the ones sorted-map-by
;; already has. A clone or a copy constructor shares the persistent map, so it is
;; O(1). What this file adds is the java.util contract over it:
;;
;;   - Natural ordering is Comparable.compareTo, not clojure.core/compare: a nil
;;     key is NullPointerException (even on an empty map's get / put, which the
;;     JDK checks before it compares), a map or a seq key is ClassCastException,
;;     and two numbers of different classes — a Long and a Double — are a
;;     ClassCastException too, where compare would order them.
;;   - Every view is LIVE: it is the same root plus bounds and a direction, so a
;;     write through a view is visible in the map and the other way round, and a
;;     put outside a view's range is IllegalArgumentException. The view logic is
;;     the JDK's NavigableSubMap (absLowest / absCeiling / … with the direction
;;     swapping floor<->ceiling and lower<->higher); the root map is the view
;;     with no bounds.
;;
;; Iteration hands out live TreeMap$Entry objects whose setValue writes through,
;; the navigation methods immutable snapshots (see "entries" below); an iterator
;; walks a snapshot of its view and removes through it, and is fail-fast: the
;; root keeps TreeMap's modCount (a new key, a removal, a clear of the whole map
;; bump it; a put over an existing key and setValue do not), which an iterator,
;; forEach, replaceAll and the compute family watch as the JDK's do.
;;
;; Needs jutil-colls.ss (the Map / Set seam), jolt-fi-call (records-dispatch.ss), host-table.ss's
;; sorted-coll op access (sc-call, kw-op-*), and clojure.core's sorted-map-by /
;; subseq / rsubseq and the comparison fns they take, read at call time — this
;; file loads before the prelude. Each is spelled as a literal var-deref so the
;; tree-shaker's runtime roots (dce.ss) can keep it.

;; ---- natural ordering ---------------------------------------------------------
(define (tm-class-name x)
  (let ((n (guard (e (#t #f)) (jolt-class-name x))))
    (if (string? n) n "java.lang.Object")))
;; Values that are not java.lang.Comparable on the JVM: the persistent maps,
;; sets and seqs, fns, the sorted colls and the java.util shims. Anything else
;; is left to jolt-compare, which raises ClassCastException for a pair it cannot
;; order.
(define (tm-comparable? x)
  (not (or (pmap? x) (pset? x) (cseq? x) (empty-list-t? x) (jolt-lazyseq? x)
           (procedure? x) (htable-sorted? x) (jutil-coll-kind x)
           ;; a bare (Object.), which is no Comparable
           (and (jhost? x) (string=? (jhost-tag x) "object")))))
(define (tm-number-class x)
  (cond ((flonum? x) "java.lang.Double")
        ((and (exact? x) (integer? x)) "java.lang.Long")
        ((exact? x) "clojure.lang.Ratio")
        (else (tm-class-name x))))
;; a.compareTo(b): a must be Comparable, b must be castable to a's class. A
;; Ratio compares against any Number (Ratio.compareTo goes through Numbers).
(define (tm-natural-compare a b)
  (cond ((or (jolt-nil? a) (jolt-nil? b)) (throw-jvm 'NullPointerException jolt-nil))
        ((not (tm-comparable? a))
         (throw-jvm 'ClassCastException
                    (string-append "class " (tm-class-name a)
                                   " cannot be cast to class java.lang.Comparable")))
        ((and (number? a) (number? b))
         (let ((ca (tm-number-class a)) (cb (tm-number-class b)))
           (if (or (string=? ca cb) (string=? ca "clojure.lang.Ratio"))
               (jolt-compare a b)
               (throw-jvm 'ClassCastException
                          (string-append "class " cb " cannot be cast to class " ca)))))
        (else (jolt-compare a b))))

;; A comparator object as a 3-way procedure, with AFunction.compare's reading of
;; a boolean result (a less-than predicate: true is -1, else ask the other way).
(define (tm-comparator->cmpf c)
  (let ((f (jolt-comparator-fn c)))
    (lambda (a b)
      (let ((r (f a b)))
        (cond ((number? r) r)
              ((jolt-truthy? r) -1)
              ((jolt-truthy? (f b a)) 1)
              (else 0))))))
(define (tm-comparator-like? x)
  (or (procedure? x) (jhost-compare-method? x) (and (iface-method x "compare" #f) #t)))

;; ---- Comparator objects ----------------------------------------------------------
;; Collections.reverseOrder() and Comparator.naturalOrder() are singletons, and
;; Collections.reverseOrder(cmp) unwraps the obvious pairs exactly as the JDK
;; does, so (.reversed (Comparator/reverseOrder)) is naturalOrder itself.
(define tm-reverse-order (make-jhost "reverse-comparator" (vector)))
(define tm-natural-order (make-jhost "natural-comparator" (vector)))
(define (tm-reverse-of c)
  (cond ((jolt-nil? c) tm-reverse-order)
        ((eq? c tm-reverse-order) tm-natural-order)
        ((eq? c tm-natural-order) tm-reverse-order)
        ((and (jhost? c) (string=? (jhost-tag c) "reverse-comparator2"))
         (vector-ref (jhost-state c) 0))
        (else (make-jhost "reverse-comparator2" (vector c)))))
(register-host-methods! "reverse-comparator"
  (list (cons "compare" (lambda (self a b) (tm-natural-compare b a)))
        (cons "reversed" (lambda (self) tm-natural-order))))
(register-host-methods! "natural-comparator"
  (list (cons "compare" (lambda (self a b) (tm-natural-compare a b)))
        (cons "reversed" (lambda (self) tm-reverse-order))))
(register-host-methods! "reverse-comparator2"
  (list (cons "compare" (lambda (self a b)
                          ((tm-comparator->cmpf (vector-ref (jhost-state self) 0)) b a)))
        (cons "reversed" (lambda (self) (vector-ref (jhost-state self) 0)))))
(register-class-statics! "java.util.Comparator"
  (list (cons "reverseOrder" (lambda () tm-reverse-order))
        (cons "naturalOrder" (lambda () tm-natural-order))))
(register-class-statics! "java.util.Collections"
  (list (cons "reverseOrder" (case-lambda (() tm-reverse-order)
                                          ((c) (tm-reverse-of c))))))

;; ---- the root: the sorted map a TreeMap and all its views share -----------------
;; #(sorted-map comparator-object cmpf modcount). comparator-object is nil for
;; natural ordering — what .comparator answers — and cmpf the 3-way procedure
;; every compare in this file goes through, bounds checks included.
(define (tm-make-root cmp-obj)
  (let ((cmpf (if (jolt-nil? cmp-obj) tm-natural-compare (tm-comparator->cmpf cmp-obj))))
    (vector (jolt-invoke (var-deref "clojure.core" "sorted-map-by") cmpf) cmp-obj cmpf 0)))
(define (tmr-sm r) (vector-ref r 0))
(define (tmr-sm! r sm)
  (when (tmr-concurrent? r) (tm-publish-fence))
  (vector-set! r 0 sm))
(define (tmr-cmp-obj r) (vector-ref r 1))
(define (tmr-cmpf r) (vector-ref r 2))
(define (tmr-natural? r) (jolt-nil? (vector-ref r 1)))
(define (tmr-bump! r) (vector-set! r 3 (fxand (fx+ (vector-ref r 3) 1) #x3fffffff)))
;; the modCount a walk over the root expects, or #f for one that is not
;; fail-fast (a ConcurrentSkipListMap's, whose iterators are weakly consistent)
(define (tmr-expect r) (and (not (tmr-concurrent? r)) (vector-ref r 3)))
(define (tmr-check! r e) (when (and e (not (fx=? e (vector-ref r 3)))) (jutil-cme!)))
;; A ConcurrentSkipListMap / Set (jutil-concurrent.ss, Chez only) is this same
;; root with a lock in a fifth slot. Every write to it runs under that lock
;; (tmr-write), so two writers never build from the same tree and lose one's
;; change, and publishes the new tree with one store behind a release fence;
;; a read takes the published tree with no lock, and since the tree is
;; persistent it is never seen half built. It refuses nil keys and values. A
;; TreeMap's root has four slots and takes none of this.
(define (tmr-concurrent? r) (fx>? (vector-length r) 4))
(define tm-publish-fence (lambda () #f))
(define tm-root-write (lambda (r thunk) (thunk)))
(define (tmr-write r thunk) (if (tmr-concurrent? r) (tm-root-write r thunk) (thunk)))
(define tm-absent (list 'tm-absent))
(define (tmr-count r) (sc-call (tmr-sm r) kw-op-count))
;; getEntry's Objects.requireNonNull: natural ordering refuses a nil key even
;; when there is nothing to compare it with.
(define (tmr-check-key r k)
  (when (and (jolt-nil? k) (or (tmr-natural? r) (tmr-concurrent? r))) (throw-jvm 'NullPointerException jolt-nil)))
(define (tmr-ref r k)
  (tmr-check-key r k)
  (sc-call (tmr-sm r) kw-op-get k tm-absent))
(define (tmr-put! r k v)
  (when (and (jolt-nil? v) (tmr-concurrent? r)) (throw-jvm 'NullPointerException jolt-nil))
  (tmr-write r
    (lambda ()
      ;; put on an empty map compares the key with itself: the JDK's type (and
      ;; null) check, so (.put (TreeMap.) {:a 1} 1) fails before anything is stored.
      (when (eqv? 0 (tmr-count r)) ((tmr-cmpf r) k k))
      (let ((old (tmr-ref r k)))
        (tmr-sm! r (sc-call (tmr-sm r) kw-op-assoc (jolt-vector k v)))
        (if (eq? old tm-absent) (begin (tmr-bump! r) jolt-nil) old)))))
(define (tmr-remove! r k)
  (tmr-write r
    (lambda ()
      (let ((old (tmr-ref r k)))
        (unless (eq? old tm-absent)
          (tmr-sm! r (sc-call (tmr-sm r) kw-op-dissoc (jolt-vector k)))
          (tmr-bump! r))
        old))))
(define (tm-seq-head s)
  (let ((s (jolt-seq s))) (if (jolt-nil? s) #f (seq-first s))))
(define (tm-always c z) #t)
(define (tmr-first r) (let ((e (jolt-first (tmr-sm r)))) (if (jolt-nil? e) #f e)))
;; the greatest entry, by an rsubseq whose test always holds: an O(log n) walk
;; down the right spine. The bound it is handed is any key of the map (the
;; first), so the comparisons it makes are ones the map can answer.
(define (tmr-desc-all r)
  (let ((f (tmr-first r)))
    (if f (jolt-invoke (var-deref "clojure.core" "rsubseq") (tmr-sm r) tm-always (jolt-nth f 0)) jolt-nil)))
(define (tmr-last r) (tm-seq-head (tmr-desc-all r)))
(define (tmr-asc-from r k incl?)
  (jolt-invoke (var-deref "clojure.core" "subseq") (tmr-sm r)
               (if incl? (var-deref "clojure.core" ">=") (var-deref "clojure.core" ">")) k))
(define (tmr-desc-from r k incl?)
  (jolt-invoke (var-deref "clojure.core" "rsubseq") (tmr-sm r)
               (if incl? (var-deref "clojure.core" "<=") (var-deref "clojure.core" "<")) k))
(define (tmr-ceiling r k incl?) (tm-seq-head (tmr-asc-from r k incl?)))
(define (tmr-floor r k incl?) (tm-seq-head (tmr-desc-from r k incl?)))

;; ---- views ------------------------------------------------------------------------
;; A view's state is #(root lo lo-incl hi hi-incl descending? sub?); lo / hi are
;; tm-unbounded for an open end. sub? is whether the view came from a
;; head/tail/sub/descending map or set, which on the JDK is a NavigableSubMap
;; even when it ends up unbounded ((.descendingMap (.descendingMap m))): the
;; classes of its entry set, values and iterators follow from it. The tag says what the view IS (a map, a key
;; set, values, entries, a TreeSet); the bounds and direction are the same
;; machinery for all of them.
(define tm-unbounded (list 'tm-unbounded))
(define (make-tm-view tag root lo lo-incl hi hi-incl desc? . sub)
  (make-jhost tag (vector root lo lo-incl hi hi-incl desc? (and (pair? sub) (car sub)))))
(define (v-root v) (vector-ref (jhost-state v) 0))
(define (v-lo v) (vector-ref (jhost-state v) 1))
(define (v-lo-incl v) (vector-ref (jhost-state v) 2))
(define (v-hi v) (vector-ref (jhost-state v) 3))
(define (v-hi-incl v) (vector-ref (jhost-state v) 4))
(define (v-desc? v) (vector-ref (jhost-state v) 5))
(define (v-sub? v) (vector-ref (jhost-state v) 6))
(define (v-cmp v a b) ((tmr-cmpf (v-root v)) a b))
(define (v-unbounded? v) (and (eq? (v-lo v) tm-unbounded) (eq? (v-hi v) tm-unbounded)))
(define (v-too-low? v k)
  (and (not (eq? (v-lo v) tm-unbounded))
       (let ((c (v-cmp v k (v-lo v)))) (or (< c 0) (and (= c 0) (not (v-lo-incl v)))))))
(define (v-too-high? v k)
  (and (not (eq? (v-hi v) tm-unbounded))
       (let ((c (v-cmp v k (v-hi v)))) (or (> c 0) (and (= c 0) (not (v-hi-incl v)))))))
(define (v-in-range? v k) (and (not (v-too-low? v k)) (not (v-too-high? v k))))
(define (v-in-closed-range? v k)
  (and (or (eq? (v-lo v) tm-unbounded) (>= (v-cmp v k (v-lo v)) 0))
       (or (eq? (v-hi v) tm-unbounded) (>= (v-cmp v (v-hi v) k) 0))))
(define (v-in-range/incl? v k incl?) (if incl? (v-in-range? v k) (v-in-closed-range? v k)))
(define (e-key e) (jolt-nth e 0))
(define (e-val e) (jolt-nth e 1))

;; the absolute (ascending) navigation, then the view-relative one
(define (v-abs-lowest v)
  (let ((e (if (eq? (v-lo v) tm-unbounded) (tmr-first (v-root v))
               (tmr-ceiling (v-root v) (v-lo v) (v-lo-incl v)))))
    (and e (not (v-too-high? v (e-key e))) e)))
(define (v-abs-highest v)
  (let ((e (if (eq? (v-hi v) tm-unbounded) (tmr-last (v-root v))
               (tmr-floor (v-root v) (v-hi v) (v-hi-incl v)))))
    (and e (not (v-too-low? v (e-key e))) e)))
(define (v-abs-ceiling v k incl?)
  (if (v-too-low? v k) (v-abs-lowest v)
      (let ((e (tmr-ceiling (v-root v) k incl?)))
        (and e (not (v-too-high? v (e-key e))) e))))
(define (v-abs-floor v k incl?)
  (if (v-too-high? v k) (v-abs-highest v)
      (let ((e (tmr-floor (v-root v) k incl?)))
        (and e (not (v-too-low? v (e-key e))) e))))
(define (v-lowest v) (if (v-desc? v) (v-abs-highest v) (v-abs-lowest v)))
(define (v-highest v) (if (v-desc? v) (v-abs-lowest v) (v-abs-highest v)))
(define (v-ceiling v k) (if (v-desc? v) (v-abs-floor v k #t) (v-abs-ceiling v k #t)))
(define (v-higher v k) (if (v-desc? v) (v-abs-floor v k #f) (v-abs-ceiling v k #f)))
(define (v-floor v k) (if (v-desc? v) (v-abs-ceiling v k #t) (v-abs-floor v k #t)))
(define (v-lower v k) (if (v-desc? v) (v-abs-ceiling v k #f) (v-abs-floor v k #f)))

;; the view's entries in its own order, as a Scheme list
(define (tm-take-while s keep?)
  (let loop ((s (jolt-seq s)) (acc '()))
    (if (or (jolt-nil? s) (not (keep? (seq-first s))))
        (reverse acc)
        (loop (jolt-seq (seq-more s)) (cons (seq-first s) acc)))))
(define (v-entries v)
  (let ((r (v-root v)))
    (cond
      ((and (v-unbounded? v) (not (v-desc? v))) (seq->list (jolt-seq (tmr-sm r))))
      ((v-desc? v)
       (tm-take-while (if (eq? (v-hi v) tm-unbounded) (tmr-desc-all r)
                          (tmr-desc-from r (v-hi v) (v-hi-incl v)))
                      (lambda (e) (not (v-too-low? v (e-key e))))))
      (else
       (tm-take-while (if (eq? (v-lo v) tm-unbounded) (jolt-seq (tmr-sm r))
                          (tmr-asc-from r (v-lo v) (v-lo-incl v)))
                      (lambda (e) (not (v-too-high? v (e-key e)))))))))
(define (v-keys v) (map e-key (v-entries v)))
(define (v-size v) (if (v-unbounded? v) (tmr-count (v-root v)) (length (v-entries v))))
(define (v-empty? v) (if (v-unbounded? v) (eqv? 0 (tmr-count (v-root v))) (not (v-lowest v))))

;; reads and writes through a view: out of range is absent to a read and
;; IllegalArgumentException to a put
(define (v-ref v k)
  (if (v-in-range? v k) (tmr-ref (v-root v) k) (begin (tmr-check-key (v-root v) k) tm-absent)))
(define (v-get v k) (let ((x (v-ref v k))) (if (eq? x tm-absent) jolt-nil x)))
(define (v-contains? v k) (not (eq? (v-ref v k) tm-absent)))
(define (v-put! v k val)
  (unless (v-in-range? v k) (throw-jvm 'IllegalArgumentException "key out of range"))
  (tmr-put! (v-root v) k val))
(define (v-remove! v k)
  (if (v-in-range? v k)
      (let ((old (tmr-remove! (v-root v) k))) (if (eq? old tm-absent) jolt-nil old))
      (begin (tmr-check-key (v-root v) k) jolt-nil)))
(define (v-remove-present! v k)
  (if (v-in-range? v k) (not (eq? (tmr-remove! (v-root v) k) tm-absent)) #f))
;; TreeMap.clear counts a change even on an empty map; a sub-map (a descending
;; one included) clears through its entry set's iterator, one removal at a time
(define (v-clear! v)
  (tmr-write (v-root v)
    (lambda ()
      (cond ((not (v-unbounded? v)) (for-each (lambda (k) (tmr-remove! (v-root v) k)) (v-keys v)))
            ((and (v-sub? v) (eqv? 0 (tmr-count (v-root v)))) #f)
            (else (tmr-sm! (v-root v) (sc-call (tmr-sm (v-root v)) (keyword #f "empty")))
                  (tmr-bump! (v-root v))))))
  jolt-nil)
(define (v-poll! v e)
  (if e (begin (tmr-remove! (v-root v) (e-key e)) e) #f))
(define (v-comparator v)
  (if (v-desc? v) (tm-reverse-of (tmr-cmp-obj (v-root v))) (tmr-cmp-obj (v-root v))))

;; ---- sub-views ----------------------------------------------------------------------
;; head / tail / sub in the VIEW's order, mapped onto absolute bounds: a
;; descending view's head is the absolute tail. The JDK checks the new bound is
;; inside this view's range, then (its NavigableSubMap constructor) that the
;; bounds are ordered — or, for a one-sided view, that its key compares with
;; itself, which is how a nil bound under natural ordering is refused.
(define (v-make-sub v tag lo lo-incl hi hi-incl desc?)
  (let ((cmpf (tmr-cmpf (v-root v))))
    (cond ((and (not (eq? lo tm-unbounded)) (not (eq? hi tm-unbounded)))
           (when (> (cmpf lo hi) 0) (throw-jvm 'IllegalArgumentException "fromKey > toKey")))
          (else (unless (eq? lo tm-unbounded) (cmpf lo lo))
                (unless (eq? hi tm-unbounded) (cmpf hi hi)))))
  (make-tm-view tag (v-root v) lo lo-incl hi hi-incl desc? #t))
(define (v-check-bound v k incl? which)
  (unless (v-in-range/incl? v k incl?)
    (throw-jvm 'IllegalArgumentException (string-append which " out of range"))))
(define (v-head v tag to incl?)
  (v-check-bound v to incl? "toKey")
  (if (v-desc? v)
      (v-make-sub v tag to incl? (v-hi v) (v-hi-incl v) #t)
      (v-make-sub v tag (v-lo v) (v-lo-incl v) to incl? #f)))
(define (v-tail v tag from incl?)
  (v-check-bound v from incl? "fromKey")
  (if (v-desc? v)
      (v-make-sub v tag (v-lo v) (v-lo-incl v) from incl? #t)
      (v-make-sub v tag from incl? (v-hi v) (v-hi-incl v) #f)))
(define (v-sub v tag from from-incl to to-incl)
  (v-check-bound v from from-incl "fromKey")
  (v-check-bound v to to-incl "toKey")
  (if (v-desc? v)
      (v-make-sub v tag to to-incl from from-incl #t)
      (v-make-sub v tag from from-incl to to-incl #f)))
(define (v-flip v tag)
  (make-tm-view tag (v-root v) (v-lo v) (v-lo-incl v) (v-hi v) (v-hi-incl v) (not (v-desc? v)) #t))
;; a map's descending view, made once per map as the JDK keeps it in a field
(define (tm-descending-map v)
  (jutil-view v 3 (lambda () (v-flip v (tm-map-tag (not (v-desc? v)))))))
(define (v-retag v tag)
  (make-tm-view tag (v-root v) (v-lo v) (v-lo-incl v) (v-hi v) (v-hi-incl v) (v-desc? v) (v-sub? v)))

;; list helpers spelled out: the R6RS memp / find / for-all are not on every target
(define (tm-find pred xs)
  (cond ((null? xs) #f) ((pred (car xs)) (car xs)) (else (tm-find pred (cdr xs)))))
(define (tm-every? pred xs)
  (or (null? xs) (and (pred (car xs)) (tm-every? pred (cdr xs)))))

;; ---- tags ---------------------------------------------------------------------------
(define (tm-map-tag desc?) (if desc? "treemap-desc-sub" "treemap-asc-sub"))
(define tm-map-tags '("treemap" "treemap-asc-sub" "treemap-desc-sub"))
(define tm-set-tags '("treeset" "treemap-keyset"))
;; values() and entrySet() are TreeMap$Values and $EntrySet on the map itself and
;; other classes on a sub-map, so each has a tag per JDK class (classes follow tags)
(define tm-values-tags '("treemap-values" "treemap-submap-values"))
(define tm-entryset-tags '("treemap-entryset" "treemap-asc-entryset" "treemap-desc-entryset"))
(define (tm-values-tag v) (if (v-sub? v) "treemap-submap-values" "treemap-values"))
(define (tm-entryset-tag v)
  (cond ((not (v-sub? v)) "treemap-entryset")
        ((v-desc? v) "treemap-desc-entryset")
        (else "treemap-asc-entryset")))
(define (tm-tag-in? x tags) (and (jhost? x) (member (jhost-tag x) tags) #t))
(define (tm-map? x) (tm-tag-in? x tm-map-tags))
(define (tm-set? x) (tm-tag-in? x tm-set-tags))
(define (tm-view? x)
  (tm-tag-in? x (append '("treemap" "treemap-asc-sub" "treemap-desc-sub" "treeset" "treemap-keyset")
                        tm-values-tags tm-entryset-tags)))
;; what a view's iteration yields, by what it is
;; ---- entries ---------------------------------------------------------------------------
;; What iteration hands out is a TreeMap$Entry: a live entry whose setValue
;; writes through to the map, and whose getValue reads the map (the JDK's entry
;; IS the tree node, so a later put shows through it). Once its key has left the
;; map it keeps the last value it saw. firstEntry / floorEntry / pollFirstEntry
;; and the rest export an AbstractMap$SimpleImmutableEntry snapshot instead,
;; whose setValue is UnsupportedOperationException. Both are java.util.Map$Entry
;; objects to the runtime through jutil-colls.ss — key / val / nth, map-entry?,
;; conj onto a map, k=v toString — and neither is a vector, as on the JVM.
;; State: #(root key last-value) and #(key value).
(define (tm-live-entry v e)
  (make-jhost "treemap-entry" (vector (v-root v) (e-key e) (e-val e))))
(define (tm-live-entry-value self)
  (let* ((st (jhost-state self))
         (x (sc-call (tmr-sm (vector-ref st 0)) kw-op-get (vector-ref st 1) tm-absent)))
    (if (eq? x tm-absent) (vector-ref st 2) (begin (vector-set! st 2 x) x))))
(register-jutil-entry! "treemap-entry"
  (lambda (self) (cons (vector-ref (jhost-state self) 1) (tm-live-entry-value self))))
(register-host-methods! "treemap-entry"
  (list (cons "setValue"
              (lambda (self x)
                (let* ((st (jhost-state self)) (r (vector-ref st 0)) (k (vector-ref st 1))
                       (old (tm-live-entry-value self)))
                  (tmr-write r
                    (lambda ()
                      (unless (eq? (sc-call (tmr-sm r) kw-op-get k tm-absent) tm-absent)
                        (tmr-sm! r (sc-call (tmr-sm r) kw-op-assoc (jolt-vector k x))))))
                  (vector-set! st 2 x)
                  old)))))
(define (tm-snapshot-entry e)
  (make-jhost "immutable-entry" (vector (e-key e) (e-val e))))
(register-jutil-entry! "immutable-entry"
  (lambda (self) (cons (vector-ref (jhost-state self) 0) (vector-ref (jhost-state self) 1))))
(register-host-methods! "immutable-entry"
  (list (cons "setValue" (lambda (self x) (throw-jvm 'UnsupportedOperationException jolt-nil)))))
(define (v-live-entries v) (map (lambda (e) (tm-live-entry v e)) (v-entries v)))

;; what a view's iteration yields, by what it is
(define (tm-elems v)
  (let ((t (jhost-tag v)))
    (cond ((member t tm-set-tags) (v-keys v))
          ((member t tm-values-tags) (map e-val (v-entries v)))
          (else (v-live-entries v)))))
;; (seq v) chunk by chunk, as RT.seq walks the fail-fast iterator (jutil-ff-seq)
(define (tm-elems-seq v) (jutil-ff-seq v (tm-elems v)))
(define (tm-no-such-element) (throw-jvm 'NoSuchElementException jolt-nil))
(define (tm-key-or-throw e) (if e (e-key e) (tm-no-such-element)))
(define (tm-key-or-nil e) (if e (e-key e) jolt-nil))
(define (tm-entry-or-nil e) (if e (tm-snapshot-entry e) jolt-nil))

;; ---- constructors ---------------------------------------------------------------------
(define (tm-fill-map! v m)
  (for-each (lambda (e) (v-put! v (jolt-nth e 0) (jolt-nth e 1)))
            (seq->list (jolt-seq m))))
(define (tm-new-map cmp-obj)
  (make-tm-view "treemap" (tm-make-root cmp-obj) tm-unbounded #f tm-unbounded #f #f))
;; a copy of a whole, ascending TreeMap shares the persistent tree
(define (tm-copy-root-view v tag)
  (let* ((r (v-root v))
         (nr (vector (tmr-sm r) (tmr-cmp-obj r) (tmr-cmpf r) 0)))
    (make-tm-view tag nr tm-unbounded #f tm-unbounded #f #f)))
(define (tm-ctor-arg-error class)
  (throw-jvm 'IllegalArgumentException (string-append "No matching ctor found for class " class)))
;; (TreeMap.) | (TreeMap. Comparator) | (TreeMap. SortedMap) | (TreeMap. Map).
;; A SortedMap keeps its comparator; any other Map — a Clojure sorted-map is
;; not a java.util.SortedMap — is copied under natural ordering.
;; A ConcurrentSkipListMap / Set is a SortedMap / SortedSet with a view's state
;; under a tag of its own; tm-foreign-view answers that view, so the copy
;; constructors keep its comparator and share its tree. Set by jutil-concurrent.ss.
(define tm-foreign-view (lambda (x) #f))
(define tm-ctor
  (case-lambda
    (() (tm-new-map jolt-nil))
    ((x0)
     (define x (or (tm-foreign-view x0) x0))
     (cond
       ((and (tm-map? x) (v-unbounded? x) (not (v-desc? x))) (tm-copy-root-view x "treemap"))
       ((tm-map? x) (let ((v (tm-new-map (v-comparator x)))) (tm-fill-map! v x) v))
       ((or (jolt-map? x) (eq? (jutil-coll-kind x) 'map))
        (let ((v (tm-new-map jolt-nil))) (tm-fill-map! v x) v))
       ((tm-comparator-like? x) (tm-new-map x))
       (else (tm-ctor-arg-error "java.util.TreeMap"))))))
(register-class-ctor! "TreeMap" tm-ctor)
(register-class-ctor! "java.util.TreeMap" tm-ctor)
;; (TreeSet.) | (TreeSet. Comparator) | (TreeSet. SortedSet) | (TreeSet. Collection)
(define (ts-add-all! v xs)
  (fold-left (lambda (changed x) (or (ts-add! v x) changed)) #f xs))
(define (ts-add! v x) (eq? (v-ref-or-put! v x) tm-absent))
(define (v-ref-or-put! v x)
  (let ((old (if (v-in-range? v x) (tmr-ref (v-root v) x) tm-absent)))
    (when (eq? old tm-absent) (v-put! v x #t))
    old))
(define ts-ctor
  (case-lambda
    (() (make-tm-view "treeset" (tm-make-root jolt-nil) tm-unbounded #f tm-unbounded #f #f))
    ((x0)
     (define x (or (tm-foreign-view x0) x0))
     (cond
       ((and (tm-set? x) (v-unbounded? x) (not (v-desc? x))) (tm-copy-root-view x "treeset"))
       ((tm-set? x)
        (let ((v (make-tm-view "treeset" (tm-make-root (v-comparator x))
                               tm-unbounded #f tm-unbounded #f #f)))
          (ts-add-all! v (v-keys x)) v))
       ((tm-comparator-like? x)
        (make-tm-view "treeset" (tm-make-root x) tm-unbounded #f tm-unbounded #f #f))
       ((jolt-map? x) (tm-ctor-arg-error "java.util.TreeSet"))
       (else
        (let ((v (make-tm-view "treeset" (tm-make-root jolt-nil) tm-unbounded #f tm-unbounded #f #f)))
          (ts-add-all! v (seq->list (jolt-seq x))) v))))))
(register-class-ctor! "TreeSet" ts-ctor)
(register-class-ctor! "java.util.TreeSet" ts-ctor)

;; ---- the NavigableMap surface ---------------------------------------------------------
;; SUB? overrides the view's own: descendingKeySet() is descendingMap()'s key
;; set on the JDK, a sub-map view even on the map itself
(define (tm-key-set v desc? . sub)
  (make-tm-view "treemap-keyset" (v-root v) (v-lo v) (v-lo-incl v) (v-hi v) (v-hi-incl v) desc?
                (if (pair? sub) (car sub) (v-sub? v))))
(define treemap-methods
  (list
    (cons "size" (lambda (self) (v-size self)))
    (cons "isEmpty" (lambda (self) (v-empty? self)))
    (cons "get" (lambda (self k) (v-get self k)))
    (cons "containsKey" (lambda (self k) (v-contains? self k)))
    (cons "containsValue" (lambda (self x)
                            (and (tm-find (lambda (e) (jolt=2 x (e-val e))) (v-entries self)) #t)))
    (cons "put" (lambda (self k val) (v-put! self k val)))
    (cons "remove" (case-lambda
                     ((self k) (v-remove! self k))
                     ;; Map.remove(key, value): only when mapped to exactly that
                     ((self k val) (if (and (v-contains? self k) (jolt=2 val (v-get self k)))
                                       (begin (v-remove! self k) #t)
                                       #f))))
    (cons "clear" (lambda (self) (v-clear! self)))
    (cons "putAll" (lambda (self m) (tm-fill-map! self m) jolt-nil))
    ;; keySet IS navigableKeySet on the JDK, one cached view
    (cons "keySet" (lambda (self) (jutil-view self 0 (lambda () (tm-key-set self (v-desc? self))))))
    (cons "navigableKeySet" (lambda (self) (jutil-view self 0 (lambda () (tm-key-set self (v-desc? self))))))
    ;; descendingMap().navigableKeySet(), as the JDK answers it: cached on the
    ;; cached descending map
    (cons "descendingKeySet" (lambda (self)
                               (let ((d (tm-descending-map self)))
                                 (jutil-view d 0 (lambda () (tm-key-set d (v-desc? d)))))))
    (cons "values" (lambda (self) (jutil-view self 1 (lambda () (v-retag self (tm-values-tag self))))))
    (cons "entrySet" (lambda (self) (jutil-view self 2 (lambda () (v-retag self (tm-entryset-tag self))))))
    ;; Map's default methods. A key mapped to nil counts as absent, like the
    ;; JDK's; each returns what the JDK's returns (the previous value for
    ;; putIfAbsent, the new one for the compute/merge family).
    (cons "getOrDefault" (lambda (self k d) (let ((x (v-ref self k))) (if (eq? x tm-absent) d x))))
    (cons "putIfAbsent" (lambda (self k val)
                          (let ((old (v-get self k)))
                            (if (jolt-nil? old) (begin (v-put! self k val) jolt-nil) old))))
    (cons "computeIfAbsent" (lambda (self k f)
                              (let ((old (v-get self k)))
                                (if (jolt-nil? old)
                                    (let ((nv (v-apply-checked self f "apply" k)))
                                      (unless (jolt-nil? nv) (v-put! self k nv))
                                      nv)
                                    old))))
    (cons "computeIfPresent" (lambda (self k f)
                               (let ((old (v-get self k)))
                                 (if (jolt-nil? old)
                                     jolt-nil
                                     (let ((nv (v-apply-checked self f "apply" k old)))
                                       (if (jolt-nil? nv) (v-remove! self k) (v-put! self k nv))
                                       nv)))))
    (cons "compute" (lambda (self k f)
                      (let* ((old (v-get self k))
                             (nv (v-apply-checked self f "apply" k old)))
                        (cond ((not (jolt-nil? nv)) (v-put! self k nv) nv)
                              ((v-contains? self k) (v-remove! self k) jolt-nil)
                              (else jolt-nil)))))
    (cons "merge" (lambda (self k val f)
                    (let* ((old (v-get self k))
                           (nv (if (jolt-nil? old) val (v-apply-checked self f "apply" old val))))
                      (if (jolt-nil? nv)
                          (begin (v-remove! self k) jolt-nil)
                          (begin (v-put! self k nv) nv)))))
    (cons "replace" (case-lambda
                      ((self k val) (if (v-contains? self k) (v-put! self k val) jolt-nil))
                      ((self k old val) (if (and (v-contains? self k) (jolt=2 old (v-get self k)))
                                            (begin (v-put! self k val) #t)
                                            #f))))
    ;; TreeMap's forEach and replaceAll check the modCount after every entry
    (cons "forEach" (lambda (self f)
                      (let* ((r (v-root self)) (mc (tmr-expect r)))
                        (for-each (lambda (e) (jolt-fi-call f "accept" (e-key e) (e-val e)) (tmr-check! r mc))
                                  (v-entries self)))
                      jolt-nil))
    (cons "replaceAll" (lambda (self f)
                         (let* ((r (v-root self)) (mc (tmr-expect r)))
                           (for-each (lambda (e)
                                       (let ((nv (jolt-fi-call f "apply" (e-key e) (e-val e))))
                                         (tmr-check! r mc)
                                         (tmr-put! r (e-key e) nv)))
                                     (v-entries self)))
                         jolt-nil))
    (cons "firstKey" (lambda (self) (tm-key-or-throw (v-lowest self))))
    (cons "lastKey" (lambda (self) (tm-key-or-throw (v-highest self))))
    (cons "firstEntry" (lambda (self) (tm-entry-or-nil (v-lowest self))))
    (cons "lastEntry" (lambda (self) (tm-entry-or-nil (v-highest self))))
    (cons "pollFirstEntry" (lambda (self) (tm-entry-or-nil (v-poll! self (v-lowest self)))))
    (cons "pollLastEntry" (lambda (self) (tm-entry-or-nil (v-poll! self (v-highest self)))))
    (cons "floorKey" (lambda (self k) (tm-key-or-nil (v-floor self k))))
    (cons "ceilingKey" (lambda (self k) (tm-key-or-nil (v-ceiling self k))))
    (cons "lowerKey" (lambda (self k) (tm-key-or-nil (v-lower self k))))
    (cons "higherKey" (lambda (self k) (tm-key-or-nil (v-higher self k))))
    (cons "floorEntry" (lambda (self k) (tm-entry-or-nil (v-floor self k))))
    (cons "ceilingEntry" (lambda (self k) (tm-entry-or-nil (v-ceiling self k))))
    (cons "lowerEntry" (lambda (self k) (tm-entry-or-nil (v-lower self k))))
    (cons "higherEntry" (lambda (self k) (tm-entry-or-nil (v-higher self k))))
    ;; SortedMap's half-open overloads, NavigableMap's flagged ones
    (cons "headMap" (case-lambda
                      ((self to) (v-head self (tm-map-tag (v-desc? self)) to #f))
                      ((self to incl) (v-head self (tm-map-tag (v-desc? self)) to (jolt-truthy? incl)))))
    (cons "tailMap" (case-lambda
                      ((self from) (v-tail self (tm-map-tag (v-desc? self)) from #t))
                      ((self from incl) (v-tail self (tm-map-tag (v-desc? self)) from (jolt-truthy? incl)))))
    (cons "subMap" (case-lambda
                     ((self from to) (v-sub self (tm-map-tag (v-desc? self)) from #t to #f))
                     ((self from fi to ti)
                      (v-sub self (tm-map-tag (v-desc? self)) from (jolt-truthy? fi) to (jolt-truthy? ti)))))
    (cons "descendingMap" (lambda (self) (tm-descending-map self)))
    ;; SequencedMap, as NavigableMap answers it: reversed is the descending map,
    ;; and a sorted map has no first or last place to put a key
    (cons "reversed" (lambda (self) (tm-descending-map self)))
    (cons "putFirst" (lambda (self k val) (throw-jvm 'UnsupportedOperationException jolt-nil)))
    (cons "putLast" (lambda (self k val) (throw-jvm 'UnsupportedOperationException jolt-nil)))
    (cons "comparator" (lambda (self) (v-comparator self)))
    (cons "clone" (lambda (self) (tm-ctor self)))))
;; the compute family's function: a change it made to the map's structure is a
;; ConcurrentModificationException once it returns (TreeMap's
;; callMappingFunctionWithCheck)
(define (v-apply-checked v f method . args)
  (let* ((r (v-root v)) (mc (tmr-expect r)) (x (apply jolt-fi-call f method args)))
    (tmr-check! r mc)
    x))
(for-each (lambda (t) (register-host-methods! t treemap-methods)) tm-map-tags)

;; ---- iterators ---------------------------------------------------------------------------
;; An iterator over a view walks a snapshot of it taken when the iterator is
;; made, and remove() takes the last element it returned out of the map through
;; the view it came from — the JDK idiom for filtering a TreeMap in place. It is
;; fail-fast as TreeMap$PrivateEntryIterator is: next checks the end first, then
;; the modCount; remove the IllegalStateException first, then the count, and
;; takes the new count after removing. With the count unchanged the snapshot is
;; the walk the JDK makes.
;; State: #(view remaining-entries last-entry-or-#f project expected-modcount root)
;; where project maps an entry to what next() hands back.
;; One tag per JDK iterator class (classes follow tags), all sharing these
;; methods: the map's own Entry/Key/ValueIterator, a sub-map's SubMap* and
;; DescendingSubMap* iterators, and a sub-map values() view's anonymous one.
(define tm-iterator-tags
  '("treemap-entry-iterator" "treemap-key-iterator" "treemap-value-iterator"
    "treemap-submap-entry-iterator" "treemap-submap-key-iterator"
    "treemap-desc-submap-entry-iterator" "treemap-desc-submap-key-iterator"
    "treemap-submap-value-iterator"))
(define (make-tm-iterator tag v entries project)
  (let ((r (v-root v))) (make-jhost tag (vector v entries #f project (tmr-expect r) r))))
(define (tmi-ref it i) (vector-ref (jhost-state it) i))
(define (tmi-set! it i x) (vector-set! (jhost-state it) i x))
;; inline: next pays it per element. Unchecked reads (sa-uvector-ref): ST is
;; the 6-slot vector make-tm-iterator builds and slot 5 a root, which has at
;; least four slots (tm-make-root, cs-new-root).
(define-syntax tmi-current-st?
  (syntax-rules ()
    ((_ st) (let ((e (sa-uvector-ref st 4))) (or (not e) (fx=? e (sa-uvector-ref (sa-uvector-ref st 5) 3)))))))
(define (tmi-current? it) (tmi-current-st? (jhost-state it)))
(define (tmi-next self)
  (let* ((st (jhost-state self)) (es (vector-ref st 1)))
    (when (null? es) (tm-no-such-element))
    (unless (tmi-current-st? st) (jutil-cme!))
    (vector-set! st 1 (cdr es))
    (vector-set! st 2 (car es))
    ((vector-ref st 3) (car es))))
;; iterator-seq's chunk fill (records-dispatch.ss): up to 32 nexts at once
(define (tmi-fill it buf)
  (let ((project (tmi-ref it 3)))
    (and (or (null? (tmi-ref it 1)) (tmi-current? it))
    (let loop ((j 0) (es (tmi-ref it 1)) (last (tmi-ref it 2)))
      (if (or (fx=? j 32) (null? es))
          (begin (tmi-set! it 1 es) (tmi-set! it 2 last) j)
          (begin (vector-set! buf j (project (car es)))
                 (loop (fx+ j 1) (cdr es) (car es))))))))
(define treemap-iterator-methods
  (list (cons "hasNext" (lambda (self) (pair? (vector-ref (jhost-state self) 1))))
        (cons "next" tmi-next)
        (cons "remove" (lambda (self)
                         (let ((e (tmi-ref self 2)))
                           (unless e (throw-jvm 'IllegalStateException jolt-nil))
                           (unless (tmi-current? self) (jutil-cme!))
                           (let ((r (v-root (tmi-ref self 0))))
                             (tmr-remove! r (e-key e))
                             (tmi-set! self 4 (tmr-expect r)))
                           (tmi-set! self 2 #f)
                           jolt-nil)))
        ;; Iterator's default: hasNext and next to the end
        (cons "forEachRemaining" (lambda (self f)
                                   (when (jolt-nil? f) (throw-jvm 'NullPointerException jolt-nil))
                                   (let loop ()
                                     (when (pair? (tmi-ref self 1))
                                       (jolt-fi-call f "accept" (tmi-next self))
                                       (loop)))
                                   jolt-nil))))
(for-each (lambda (t) (register-host-methods! t treemap-iterator-methods)) tm-iterator-tags)
(register-iterator-seq-fill! tm-iterator-tags tmi-fill tmi-next)
;; (iterator-seq it) / (seq it): what the iterator has not handed out yet
(register-seq-arm! (lambda (x) (tm-tag-in? x tm-iterator-tags))
                   (lambda (it) (list->cseq (map (tmi-ref it 3) (tmi-ref it 1)))))
;; DESC? is a descendingIterator() call, which on the JDK goes through the
;; map's descending key set — a sub-map — even on the map itself.
(define (tm-iterator v desc?)
  (let* ((t (jhost-tag v))
         (es (v-entries v))
         (es (if desc? (reverse es) es))
         (kind (cond ((member t tm-set-tags) 'key) ((member t tm-values-tags) 'value) (else 'entry)))
         (sub? (or desc? (v-sub? v)))
         (down? (if desc? (not (v-desc? v)) (v-desc? v)))
         (tag (cond ((not sub?) (case kind
                                  ((key) "treemap-key-iterator")
                                  ((value) "treemap-value-iterator")
                                  (else "treemap-entry-iterator")))
                    ((eq? kind 'value) "treemap-submap-value-iterator")
                    (down? (if (eq? kind 'key) "treemap-desc-submap-key-iterator"
                               "treemap-desc-submap-entry-iterator"))
                    (else (if (eq? kind 'key) "treemap-submap-key-iterator"
                              "treemap-submap-entry-iterator")))))
    (make-tm-iterator tag v es (case kind
                                 ((key) e-key)
                                 ((value) (tm-live-value (v-root v)))
                                 (else (lambda (e) (tm-live-entry v e)))))))
;; a value iterator hands out the value the map holds when next() is called, as
;; the JDK's (which reads the node) does: the snapshot's while the tree is the
;; one it was taken from, else the map's for that key
(define (tm-live-value r)
  (let ((sm0 (vector-ref r 0)))
    (lambda (e)
      (let ((sm (vector-ref r 0)))
        (if (eq? sm sm0)
            (e-val e)
            (let ((x (sc-call sm kw-op-get (e-key e) tm-absent))) (if (eq? x tm-absent) (e-val e) x)))))))

;; ---- the NavigableSet surface: TreeSet and a map's key set -------------------------------
;; The two differ in add (a key set has no value to put, so it refuses) and in
;; what a head/tail/sub/descending set is: a TreeSet's is a TreeSet, a key
;; set's a key set — the tag the receiver already has.
(define (ts-iterator v desc?) (tm-iterator v desc?))
(define navigable-set-methods
  (list
    (cons "size" (lambda (self) (v-size self)))
    (cons "isEmpty" (lambda (self) (v-empty? self)))
    (cons "contains" (lambda (self x) (v-contains? self x)))
    (cons "containsAll" (lambda (self c)
                          (tm-every? (lambda (x) (v-contains? self x)) (seq->list (jolt-seq c)))))
    (cons "remove" (lambda (self x) (v-remove-present! self x)))
    (cons "removeAll" (lambda (self c)
                        (fold-left (lambda (ch x) (or (v-remove-present! self x) ch)) #f
                                   (seq->list (jolt-seq c)))))
    (cons "clear" (lambda (self) (v-clear! self)))
    (cons "iterator" (lambda (self) (ts-iterator self #f)))
    (cons "descendingIterator" (lambda (self) (ts-iterator self #t)))
    ;; Iterable.forEach and Collection.removeIf, the defaults a TreeSet and a
    ;; key set keep: over the fail-fast iterator
    (cons "forEach" (lambda (self f) (jutil-walk-for-each (ts-iterator self #f) f)))
    (cons "removeIf" (lambda (self pred) (jutil-walk-remove-if (ts-iterator self #f) pred)))
    (cons "first" (lambda (self) (tm-key-or-throw (v-lowest self))))
    (cons "last" (lambda (self) (tm-key-or-throw (v-highest self))))
    (cons "floor" (lambda (self x) (tm-key-or-nil (v-floor self x))))
    (cons "ceiling" (lambda (self x) (tm-key-or-nil (v-ceiling self x))))
    (cons "lower" (lambda (self x) (tm-key-or-nil (v-lower self x))))
    (cons "higher" (lambda (self x) (tm-key-or-nil (v-higher self x))))
    (cons "pollFirst" (lambda (self) (tm-key-or-nil (v-poll! self (v-lowest self)))))
    (cons "pollLast" (lambda (self) (tm-key-or-nil (v-poll! self (v-highest self)))))
    (cons "headSet" (case-lambda
                      ((self to) (v-head self (jhost-tag self) to #f))
                      ((self to incl) (v-head self (jhost-tag self) to (jolt-truthy? incl)))))
    (cons "tailSet" (case-lambda
                      ((self from) (v-tail self (jhost-tag self) from #t))
                      ((self from incl) (v-tail self (jhost-tag self) from (jolt-truthy? incl)))))
    (cons "subSet" (case-lambda
                     ((self from to) (v-sub self (jhost-tag self) from #t to #f))
                     ((self from fi to ti)
                      (v-sub self (jhost-tag self) from (jolt-truthy? fi) to (jolt-truthy? ti)))))
    (cons "descendingSet" (lambda (self) (v-flip self (jhost-tag self))))
    ;; SequencedCollection, as NavigableSet answers it
    (cons "reversed" (lambda (self) (v-flip self (jhost-tag self))))
    (cons "getFirst" (lambda (self) (tm-key-or-throw (v-lowest self))))
    (cons "getLast" (lambda (self) (tm-key-or-throw (v-highest self))))
    (cons "removeFirst" (lambda (self) (tm-key-or-throw (v-poll! self (v-lowest self)))))
    (cons "removeLast" (lambda (self) (tm-key-or-throw (v-poll! self (v-highest self)))))
    (cons "addFirst" (lambda (self x) (throw-jvm 'UnsupportedOperationException jolt-nil)))
    (cons "addLast" (lambda (self x) (throw-jvm 'UnsupportedOperationException jolt-nil)))
    (cons "comparator" (lambda (self) (v-comparator self)))))
(register-host-methods! "treeset"
  (append navigable-set-methods
          (list (cons "add" (lambda (self x) (ts-add! self x)))
                (cons "addAll" (lambda (self c) (ts-add-all! self (seq->list (jolt-seq c)))))
                (cons "clone" (lambda (self) (ts-ctor self))))))
(let ((unsupported (lambda (self . _) (throw-jvm 'UnsupportedOperationException jolt-nil))))
  (register-host-methods! "treemap-keyset"
    (append navigable-set-methods
            (list (cons "add" (lambda (self x) (unsupported self)))
                  (cons "addAll" (lambda (self c) (unsupported self)))))))

;; ---- values() and entrySet() ------------------------------------------------------------
(define (tm-entry-in? v e)
  (and (or (jolt-map-entry? e) (jutil-entry? e))
       (let ((x (v-ref v (e-key e)))) (and (not (eq? x tm-absent)) (jolt=2 x (e-val e))))))
(define treemap-values-methods
  (list (cons "size" (lambda (self) (v-size self)))
        (cons "isEmpty" (lambda (self) (v-empty? self)))
        (cons "contains" (lambda (self x)
                           (and (tm-find (lambda (e) (jolt=2 x (e-val e))) (v-entries self)) #t)))
        ;; Collection.remove on the values: the first entry holding it, in key order
        (cons "remove" (lambda (self x)
                         (let ((e (tm-find (lambda (e) (jolt=2 x (e-val e))) (v-entries self))))
                           (if e (begin (tmr-remove! (v-root self) (e-key e)) #t) #f))))
        (cons "clear" (lambda (self) (v-clear! self)))
        (cons "iterator" (lambda (self) (tm-iterator self #f)))
        (cons "forEach" (lambda (self f) (jutil-walk-for-each (tm-iterator self #f) f)))
        (cons "removeIf" (lambda (self pred) (jutil-walk-remove-if (tm-iterator self #f) pred)))))
(define treemap-entryset-methods
  (list (cons "size" (lambda (self) (v-size self)))
        (cons "isEmpty" (lambda (self) (v-empty? self)))
        (cons "contains" (lambda (self e) (tm-entry-in? self e)))
        (cons "remove" (lambda (self e)
                         (if (tm-entry-in? self e)
                             (begin (tmr-remove! (v-root self) (e-key e)) #t)
                             #f)))
        (cons "clear" (lambda (self) (v-clear! self)))
        (cons "iterator" (lambda (self) (tm-iterator self #f)))
        (cons "forEach" (lambda (self f) (jutil-walk-for-each (tm-iterator self #f) f)))
        (cons "removeIf" (lambda (self pred) (jutil-walk-remove-if (tm-iterator self #f) pred)))))
(for-each (lambda (t) (register-host-methods! t treemap-values-methods)) tm-values-tags)
(for-each (lambda (t) (register-host-methods! t treemap-entryset-methods)) tm-entryset-tags)

;; ---- what the rest of the runtime sees ----------------------------------------------------
;; every view reads its root's modCount (a concurrent root's is never watched)
(for-each (lambda (t) (register-modcount! t (lambda (v) (tmr-expect (v-root v)))))
          (append tm-map-tags tm-set-tags tm-values-tags tm-entryset-tags))
(for-each (lambda (t) (register-jutil-coll! t 'map v-entries)) tm-map-tags)
(for-each (lambda (t) (register-jutil-coll! t 'set v-keys)) tm-set-tags)
(for-each (lambda (t) (register-jutil-coll! t 'entries v-live-entries)) tm-entryset-tags)
(for-each (lambda (t) (register-jutil-coll! t 'coll tm-elems)) tm-values-tags)
;; (seq m) walks the entries (RT.seq over an Iterable); count is size().
(register-seq-arm! tm-view? tm-elems-seq)
(register-count-arm! tm-view? v-size)
;; get / contains? on a java.util.Map are Map.get / containsKey — so a key the
;; ordering cannot compare raises, as it does on the JVM; on a Set, contains? is
;; Set.contains (RT.get has no Set arm, so get on a TreeSet stays nil).
(register-get-arm! tm-map?
  (lambda (m k d) (let ((x (v-ref m k))) (if (eq? x tm-absent) d x))))
(register-contains-arm! (lambda (x) (tm-tag-in? x '("treemap" "treemap-asc-sub" "treemap-desc-sub"
                                                    "treeset" "treemap-keyset")))
                        v-contains?)
(register-contains-arm! (lambda (x) (tm-tag-in? x tm-entryset-tags)) tm-entry-in?)
