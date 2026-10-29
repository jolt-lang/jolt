;; jutil-views.ss — the java.util collections that are views of another one, or
;; immutable values: Collections.unmodifiable* and synchronized*, and List.of /
;; Set.of / Map.of / copyOf / Map.entry / Map.ofEntries.
;;
;; Collections.unmodifiableList and the rest return a VIEW (jolt-2o4h): reads go
;; to the backing collection, so a later change to it shows through, and every
;; mutator throws UnsupportedOperationException — through the view's iterators,
;; list iterators and entries too, and through every view it hands out (keySet,
;; subList, headMap, …), each of which is unmodifiable in turn. The classes are
;; the JDK's (java.util.Collections$UnmodifiableRandomAccessList, …).
;; synchronized* returns a wrapper whose every method holds one lock, shared by
;; the views it hands out, as the JDK's do; its iterators are the backing
;; collection's own and are not locked, which is the JDK's documented contract.
;;
;; The wrappers answer a method by calling it on the backing collection, so
;; whatever that collection answers — its own shim methods, coll-defaults.ss's
;; inherited ones — is what the view answers.
;;
;; Every tag here starts "jx-", which is how the count / seq / contains? / get
;; arms below turn every other value away with a couple of character tests.
;; jutil-concurrent.ss registers its classes through the same table.
;;
;; Loaded after coll-defaults.ss and host-static-classes.ss.

;; ---- the jx registry ----------------------------------------------------------------
;; tag -> #(kind count elems contains get). kind is jutil-colls.ss's, or iter
;; for an iterator; count / contains / get are #f where Clojure's fn does not
;; apply (a List has no contains?, a Set no get).
(define jx-ops-tbl (make-hashtable string-hash string=?))
(define (jx-tag? t)
  (and (fx>? (string-length t) 3)
       (char=? (string-ref t 0) #\j) (char=? (string-ref t 1) #\x) (char=? (string-ref t 2) #\-)))
(define (jx-ops x)
  (and (jhost? x)
       (let ((t (jhost-tag x)))
         (and (jx-tag? t) (hashtable-ref jx-ops-tbl t #f)))))
(define (jx-kind x) (let ((o (jx-ops x))) (and o (vector-ref o 0))))
;; jx-ops only looks a tag up when it starts "jx-", so a tag without the prefix
;; would register and then never answer (ArrayDeque's iterators did, and
;; iterator-seq over one threw). Refuse it here, at load.
(define (jx-register! tag kind elems count contains get)
  (unless (jx-tag? tag) (error 'jx-register! "a jx tag must start \"jx-\"" tag))
  (hashtable-set! jx-ops-tbl tag (vector kind count elems contains get))
  (unless (eq? kind 'iter) (register-jutil-coll! tag kind elems)))
(define (jx-class! tag fqn . supers)
  (hashtable-set! jhost-tag->fqn tag fqn)
  (when (pair? supers) (jch-register-supers! fqn (car supers))))
(define (jx-op x i) (let ((o (jx-ops x))) (and o (vector-ref o i))))
;; last in each walk: these are rare, and HashMap / ArrayList lookups should not
;; pay their predicate
(register-seq-arm-last! (lambda (x) (and (jx-op x 2) #t))
  (lambda (x) (let ((xs ((jx-op x 2) x))) (if (null? xs) jolt-nil (list->cseq xs)))))
(register-count-arm-last! (lambda (x) (and (jx-op x 1) #t)) (lambda (x) ((jx-op x 1) x)))
;; reduce folds the element list itself (seq.ss jolt-reduce-elems-hook)
(set! jolt-reduce-elems-hook (lambda (x) (let ((el (jx-op x 2))) (and el (el x)))))
(register-contains-arm-last! (lambda (x) (and (jx-op x 3) #t)) (lambda (x k) (and ((jx-op x 3) x k) #t)))
(register-get-arm-last! (lambda (x) (and (jx-op x 4) #t)) (lambda (x k d) ((jx-op x 4) x k d)))
;; nth over a RandomAccess one, as RT.nth reads a java.util.List by get
(let ((prior shim-nth-target))
  (set! shim-nth-target
        (lambda (coll)
          (if (memq (jx-kind coll) '(ralist list))
              (list->cseq ((jx-op coll 2) coll))
              (prior coll)))))

(define (jx-uoe . _) (throw-jvm 'UnsupportedOperationException jolt-nil))
(define (jx-npe) (throw-jvm 'NullPointerException jolt-nil))
(define (jx-call x m args) (record-method-dispatch x m (if (null? args) jolt-nil (list->cseq args))))
(define (jx-seq-list x) (seq->list (jolt-seq x)))

;; The java.util interfaces' instance methods and their argument counts, as JDK
;; 21 declares them (Object's and the stream / spliterator ones left out: the
;; registry and streams.ss answer those).
(define jx-iface-methods
  '(("java.util.Collection" ("add" 1) ("addAll" 1) ("clear" 0) ("contains" 1) ("containsAll" 1)
     ("forEach" 1) ("isEmpty" 0) ("iterator" 0) ("remove" 1) ("removeAll" 1) ("removeIf" 1)
     ("retainAll" 1) ("size" 0) ("toArray" 0 1))
    ("java.util.SequencedCollection" ("addFirst" 1) ("addLast" 1) ("getFirst" 0) ("getLast" 0)
     ("removeFirst" 0) ("removeLast" 0) ("reversed" 0))
    ("java.util.SortedSet" ("comparator" 0) ("first" 0) ("last" 0) ("headSet" 1) ("tailSet" 1) ("subSet" 2))
    ("java.util.NavigableSet" ("ceiling" 1) ("floor" 1) ("higher" 1) ("lower" 1) ("pollFirst" 0) ("pollLast" 0)
     ("descendingIterator" 0) ("descendingSet" 0) ("headSet" 2) ("tailSet" 2) ("subSet" 4))
    ("java.util.List" ("add" 2) ("addAll" 2) ("get" 1) ("set" 2) ("remove" 1) ("indexOf" 1) ("lastIndexOf" 1)
     ("listIterator" 0 1) ("subList" 2) ("replaceAll" 1) ("sort" 1))
    ("java.util.Map" ("clear" 0) ("compute" 2) ("computeIfAbsent" 2) ("computeIfPresent" 2) ("containsKey" 1)
     ("containsValue" 1) ("entrySet" 0) ("forEach" 1) ("get" 1) ("getOrDefault" 2) ("isEmpty" 0) ("keySet" 0)
     ("merge" 3) ("put" 2) ("putAll" 1) ("putIfAbsent" 2) ("remove" 1 2) ("replace" 2 3) ("replaceAll" 1)
     ("size" 0) ("values" 0))
    ("java.util.SequencedMap" ("firstEntry" 0) ("lastEntry" 0) ("pollFirstEntry" 0) ("pollLastEntry" 0)
     ("putFirst" 2) ("putLast" 2) ("reversed" 0) ("sequencedEntrySet" 0) ("sequencedKeySet" 0) ("sequencedValues" 0))
    ("java.util.SortedMap" ("comparator" 0) ("firstKey" 0) ("lastKey" 0) ("headMap" 1) ("tailMap" 1) ("subMap" 2))
    ("java.util.NavigableMap" ("ceilingEntry" 1) ("ceilingKey" 1) ("floorEntry" 1) ("floorKey" 1)
     ("higherEntry" 1) ("higherKey" 1) ("lowerEntry" 1) ("lowerKey" 1) ("descendingKeySet" 0)
     ("descendingMap" 0) ("navigableKeySet" 0) ("headMap" 2) ("tailMap" 2) ("subMap" 4))))
(define (jx-iface-table ifaces)
  ;; name -> merged arities, over every interface in IFACES
  (let ((h (make-hashtable string-hash string=?)))
    (for-each (lambda (i)
                (for-each (lambda (m)
                            (hashtable-set! h (car m)
                              (let loop ((as (cdr m)) (acc (hashtable-ref h (car m) '())))
                                (cond ((null? as) acc)
                                      ((memv (car as) acc) (loop (cdr as) acc))
                                      (else (loop (cdr as) (cons (car as) acc)))))))
                          (cdr (assoc i jx-iface-methods))))
              ifaces)
    (let-values (((ks vs) (hashtable-entries h)))
      (map cons (vector->list ks) (vector->list vs)))))
(define jx-collection-ifaces '("java.util.Collection"))
(define jx-seqcoll-ifaces '("java.util.Collection" "java.util.SequencedCollection"))
(define jx-sortedset-ifaces '("java.util.Collection" "java.util.SequencedCollection" "java.util.SortedSet"))
(define jx-navset-ifaces '("java.util.Collection" "java.util.SequencedCollection" "java.util.SortedSet" "java.util.NavigableSet"))
(define jx-list-ifaces '("java.util.Collection" "java.util.SequencedCollection" "java.util.List"))
(define jx-map-ifaces '("java.util.Map"))
(define jx-seqmap-ifaces '("java.util.Map" "java.util.SequencedMap"))
(define jx-sortedmap-ifaces '("java.util.Map" "java.util.SequencedMap" "java.util.SortedMap"))
(define jx-navmap-ifaces '("java.util.Map" "java.util.SequencedMap" "java.util.SortedMap" "java.util.NavigableMap"))

;; The members that change a collection, refused by an unmodifiable view.
(define jx-mutators
  '("add" "addAll" "clear" "remove" "removeAll" "removeIf" "retainAll" "addFirst" "addLast" "removeFirst"
    "removeLast" "set" "sort" "replaceAll" "put" "putAll" "putIfAbsent" "compute" "computeIfAbsent"
    "computeIfPresent" "merge" "replace" "pollFirst" "pollLast" "pollFirstEntry" "pollLastEntry"
    "putFirst" "putLast"))

;; ---- the wrapper kinds ---------------------------------------------------------------
;; family -> (unmodifiable-tag synchronized-tag jutil-kind interfaces). One table
;; drives the tags, classes, methods and the registry for both wrapper sorts.
(define jx-families
  `((coll "jx-unmod-coll" "jx-sync-coll" coll ,jx-collection-ifaces
          "Collections$UnmodifiableCollection" "Collections$SynchronizedCollection")
    (seqcoll "jx-unmod-seqcoll" #f coll ,jx-seqcoll-ifaces "Collections$UnmodifiableSequencedCollection" #f)
    (set "jx-unmod-set" "jx-sync-set" set ,jx-collection-ifaces
         "Collections$UnmodifiableSet" "Collections$SynchronizedSet")
    (seqset "jx-unmod-seqset" #f set ,jx-seqcoll-ifaces "Collections$UnmodifiableSequencedSet" #f)
    (sortedset "jx-unmod-sortedset" "jx-sync-sortedset" set ,jx-sortedset-ifaces
               "Collections$UnmodifiableSortedSet" "Collections$SynchronizedSortedSet")
    (navset "jx-unmod-navset" "jx-sync-navset" set ,jx-navset-ifaces
            "Collections$UnmodifiableNavigableSet" "Collections$SynchronizedNavigableSet")
    (list "jx-unmod-list" "jx-sync-list" list ,jx-list-ifaces
          "Collections$UnmodifiableList" "Collections$SynchronizedList")
    (ralist "jx-unmod-ralist" "jx-sync-ralist" ralist ,jx-list-ifaces
            "Collections$UnmodifiableRandomAccessList" "Collections$SynchronizedRandomAccessList")
    (map "jx-unmod-map" "jx-sync-map" map ,jx-map-ifaces "Collections$UnmodifiableMap" "Collections$SynchronizedMap")
    (seqmap "jx-unmod-seqmap" #f map ,jx-seqmap-ifaces "Collections$UnmodifiableSequencedMap" #f)
    (sortedmap "jx-unmod-sortedmap" "jx-sync-sortedmap" map ,jx-sortedmap-ifaces
               "Collections$UnmodifiableSortedMap" "Collections$SynchronizedSortedMap")
    (navmap "jx-unmod-navmap" "jx-sync-navmap" map ,jx-navmap-ifaces
            "Collections$UnmodifiableNavigableMap" "Collections$SynchronizedNavigableMap")
    (entryset "jx-unmod-entryset" #f entries ,jx-collection-ifaces "Collections$UnmodifiableMap$UnmodifiableEntrySet" #f)))
(define (jx-family f) (or (assq f jx-families) (error 'jx-family "unknown family" f)))
(define (jx-unmod-tag f) (list-ref (jx-family f) 1))
(define (jx-sync-tag f) (list-ref (jx-family f) 2))

;; the wrapped collection, and the synchronized one's lock
(define (jxw-inner w) (vector-ref (jhost-state w) 0))
(define (jxw-lock w) (vector-ref (jhost-state w) 1))
(define (jx-wrap-unmod family x)
  (when (jolt-nil? x) (jx-npe))
  (make-jhost (jx-unmod-tag family) (vector x)))
;; LOCK is the monitor every method holds: the wrapper's own when LOCK is #f, as
;; Collections.synchronized*'s mutex is the wrapper itself, so (locking w …) around
;; an iteration excludes the wrapper's methods (the JDK's documented idiom); a
;; view's is its backing collection's. A monitor and not a mutex: the wrapped
;; collection's methods may run user code, which may park a fiber.
(define (jx-wrap-sync family x lock)
  (when (jolt-nil? x) (jx-npe))
  (let ((w (make-jhost (jx-sync-tag family) (vector x lock))))
    (unless lock (vector-set! (jhost-state w) 1 (object-monitor w)))
    w))
(define (jx-random-access? x)
  (let ((c (guard (e (#t #f)) (jolt-class-name x))))
    (and (string? c) (jch-isa? c "java.util.RandomAccess"))))
(define (jx-list-family x) (if (or (pvec? x) (jx-random-access? x)) 'ralist 'list))

;; What a view hands back from METHOD, for the family it is: views of views are
;; wrapped the same way, iterators unmodifiably (an unmodifiable view) or not
;; at all (a synchronized one, whose iterators are the caller's to lock).
(define (jx-result-family self-family method)
  (case (string->symbol method)
    ((keySet) 'set)
    ((navigableKeySet descendingKeySet) 'navset)
    ((sequencedKeySet) 'seqset)
    ((values) 'coll)
    ((sequencedValues) 'seqcoll)
    ((entrySet sequencedEntrySet) 'entryset)
    ((subList) 'list)
    ((headSet tailSet subSet) self-family)
    ((descendingSet) 'navset)
    ((headMap tailMap subMap) self-family)
    ((descendingMap) 'navmap)
    ((reversed) (case self-family ((list ralist) 'list) ((map seqmap sortedmap navmap) 'seqmap)
                  ((set seqset sortedset navset) 'seqset) (else 'seqcoll)))
    ((iterator descendingIterator) 'iter)
    ((listIterator) 'listiter)
    ((firstEntry lastEntry floorEntry ceilingEntry lowerEntry higherEntry) 'entry)
    (else #f)))
(define (jx-sorted-overload? method nargs)
  (if (member method '("subMap" "subSet"))
      (fx=? nargs 2)
      (and (member method '("headMap" "tailMap" "headSet" "tailSet")) (fx=? nargs 1))))
(define (jx-unmod-result family method r nargs)
  ;; UnmodifiableNavigableMap/Set inherit the SortedMap/Set overloads, which
  ;; wrap as sorted views; the navigable overloads wrap as navigable ones
  (let ((rf (let ((f (jx-result-family family method)))
              (cond ((not (jx-sorted-overload? method nargs)) f)
                    ((eq? f 'navmap) 'sortedmap)
                    ((eq? f 'navset) 'sortedset)
                    (else f)))))
    (cond ((or (not rf) (jolt-nil? r)) r)
          ((eq? rf 'iter) (make-jhost (if (eq? family 'entryset) "jx-unmod-entry-iter" "jx-unmod-iter") (vector r)))
          ((eq? rf 'listiter) (make-jhost "jx-unmod-listiter" (vector r)))
          ((eq? rf 'entry) (make-jhost "jx-unmod-entry" (vector r)))
          ((eq? rf 'list) (jx-wrap-unmod (jx-list-family r) r))
          (else (jx-wrap-unmod rf r)))))
(define (jx-sync-result family method r lock)
  (let ((rf (jx-result-family family method)))
    (cond ((or (not rf) (jolt-nil? r) (memq rf '(iter listiter entry))) r)
          ((eq? rf 'list) (jx-wrap-sync (jx-list-family r) r lock))
          ((eq? rf 'entryset) (jx-wrap-sync 'set r lock))
          ((not (jx-sync-tag rf)) (jx-wrap-sync (if (memq rf '(seqmap)) 'map (if (eq? rf 'seqcoll) 'coll 'set)) r lock))
          (else (jx-wrap-sync rf r lock)))))

(define (jx-unmod-method family name arities)
  (host-arity-of arities #t
    (if (member name jx-mutators)
        jx-uoe
        (lambda (self . args) (jx-unmod-result family name (jx-call (jxw-inner self) name args) (length args))))))
(define (jx-sync-method family name arities)
  (host-arity-of arities #t
    (lambda (self . args)
      (let ((r (jolt-call-with-monitor (jxw-lock self) (lambda () (jx-call (jxw-inner self) name args)))))
        (jx-sync-result family name r (jxw-lock self))))))

;; reads through the wrapped collection, for the jx arms and the registry
(define (jxw-elems w) (jx-seq-list (jxw-inner w)))
(define (jxw-count w) (jolt-count (jxw-inner w)))
;; an entry set's elements are the entries its iterator hands out: unmodifiable
(define (jxw-entry-elems w) (map jx-unmod-entry (jxw-elems w)))
(define (jxw-contains w k) (jolt-contains? (jxw-inner w) k))
(define (jxw-get w k d) (jolt-get (jxw-inner w) k d))
(define (jxw-sync-elems w) (jolt-call-with-monitor (jxw-lock w) (lambda () (jxw-elems w))))
(define (jxw-sync-count w) (jolt-call-with-monitor (jxw-lock w) (lambda () (jxw-count w))))
(define (jxw-sync-contains w k) (jolt-call-with-monitor (jxw-lock w) (lambda () (jxw-contains w k))))
(define (jxw-sync-get w k d) (jolt-call-with-monitor (jxw-lock w) (lambda () (jxw-get w k d))))

(for-each
  (lambda (fam)
    (let* ((family (car fam)) (ut (list-ref fam 1)) (st (list-ref fam 2)) (kind (list-ref fam 3))
           (ifaces (list-ref fam 4)) (ucls (string-append "java.util." (list-ref fam 5)))
           (methods (jx-iface-table ifaces))
           (map? (eq? kind 'map)) (set? (memq kind '(set entries))))
      (jx-class! ut ucls)
      (register-host-methods! ut (map (lambda (m) (cons (car m) (jx-unmod-method family (car m) (cdr m)))) methods))
      ;; a map's entries, and an entry set's, are UnmodifiableEntry wrappers
      ;; wherever they are handed out — seq, reduce, into — not only through
      ;; the iterator
      (jx-register! ut kind (if (or map? (eq? family 'entryset)) jxw-entry-elems jxw-elems) jxw-count (and (or map? set?) jxw-contains) (and map? jxw-get))
      (when st
        (jx-class! st (string-append "java.util." (list-ref fam 6)))
        (register-host-methods! st (map (lambda (m) (cons (car m) (jx-sync-method family (car m) (cdr m)))) methods))
        (jx-register! st kind jxw-sync-elems jxw-sync-count (and (or map? set?) jxw-sync-contains)
                      (and map? jxw-sync-get)))))
  jx-families)
;; the unmodifiable entry set's toArray and forEach hand out wrapped entries too
(register-host-methods! "jx-unmod-entryset"
  (list (cons "toArray" (case-lambda ((self) (cd-to-array (jxw-entry-elems self) '()))
                                     ((self a) (cd-to-array (jxw-entry-elems self) (list a)))))
        (cons "forEach" (lambda (self f) (for-each (lambda (e) (jolt-fi-call f "accept" e)) (jxw-entry-elems self)) jolt-nil))))
(for-each (lambda (r) (jch-register-supers! (string-append "java.util." (car r))
                                            (map (lambda (s) (if (string=? s "java.io.Serializable") s
                                                                 (if (char=? (string-ref s 0) #\C) (string-append "java.util." s) s)))
                                                 (cdr r))))
  '(("Collections$UnmodifiableCollection" "java.util.Collection" "java.io.Serializable")
    ("Collections$UnmodifiableSequencedCollection" "Collections$UnmodifiableCollection" "java.util.SequencedCollection")
    ("Collections$UnmodifiableSet" "Collections$UnmodifiableCollection" "java.util.Set")
    ("Collections$UnmodifiableSequencedSet" "Collections$UnmodifiableSequencedCollection" "java.util.SequencedSet")
    ("Collections$UnmodifiableSortedSet" "Collections$UnmodifiableSet" "java.util.SortedSet")
    ("Collections$UnmodifiableNavigableSet" "Collections$UnmodifiableSortedSet" "java.util.NavigableSet")
    ("Collections$UnmodifiableList" "Collections$UnmodifiableCollection" "java.util.List")
    ("Collections$UnmodifiableRandomAccessList" "Collections$UnmodifiableList" "java.util.RandomAccess")
    ("Collections$UnmodifiableMap" "java.util.Map" "java.io.Serializable")
    ("Collections$UnmodifiableSequencedMap" "Collections$UnmodifiableMap" "java.util.SequencedMap")
    ("Collections$UnmodifiableSortedMap" "Collections$UnmodifiableMap" "java.util.SortedMap")
    ("Collections$UnmodifiableNavigableMap" "Collections$UnmodifiableSortedMap" "java.util.NavigableMap")
    ("Collections$UnmodifiableMap$UnmodifiableEntrySet" "Collections$UnmodifiableSet")
    ("Collections$SynchronizedCollection" "java.util.Collection" "java.io.Serializable")
    ("Collections$SynchronizedSet" "Collections$SynchronizedCollection" "java.util.Set")
    ("Collections$SynchronizedSortedSet" "Collections$SynchronizedSet" "java.util.SortedSet")
    ("Collections$SynchronizedNavigableSet" "Collections$SynchronizedSortedSet" "java.util.NavigableSet")
    ("Collections$SynchronizedList" "Collections$SynchronizedCollection" "java.util.List")
    ("Collections$SynchronizedRandomAccessList" "Collections$SynchronizedList" "java.util.RandomAccess")
    ("Collections$SynchronizedMap" "java.util.Map" "java.io.Serializable")
    ("Collections$SynchronizedSortedMap" "Collections$SynchronizedMap" "java.util.SortedMap")
    ("Collections$SynchronizedNavigableMap" "Collections$SynchronizedSortedMap" "java.util.NavigableMap")))

;; The unmodifiable iterators: the backing iterator, with remove (and a list
;; iterator's set / add) refused. An entry set's hands out unmodifiable entries.
(define (jx-it w) (vector-ref (jhost-state w) 0))
(define (jx-unmod-entry e) (if (jolt-nil? e) e (make-jhost "jx-unmod-entry" (vector e))))
(define jx-unmod-iter-methods
  (list (cons "hasNext" (lambda (self) (jx-call (jx-it self) "hasNext" '())))
        (cons "next" (lambda (self) (jx-call (jx-it self) "next" '())))
        (cons "remove" (lambda (self) (jx-uoe)))
        (cons "forEachRemaining" (lambda (self f)
                                   (let loop ()
                                     (when (jolt-truthy? (jx-call (jx-it self) "hasNext" '()))
                                       (jolt-fi-call f "accept" (jx-call (jx-it self) "next" '()))
                                       (loop)))
                                   jolt-nil))))
(jx-class! "jx-unmod-iter" "java.util.Collections$UnmodifiableCollection$1" '("java.util.Iterator"))
(register-host-methods! "jx-unmod-iter" jx-unmod-iter-methods)
(jx-class! "jx-unmod-entry-iter" "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$1" '("java.util.Iterator"))
(register-host-methods! "jx-unmod-entry-iter"
  (append jx-unmod-iter-methods
          (list (cons "next" (lambda (self) (jx-unmod-entry (jx-call (jx-it self) "next" '()))))
                (cons "forEachRemaining" (lambda (self f)
                                           (let loop ()
                                             (when (jolt-truthy? (jx-call (jx-it self) "hasNext" '()))
                                               (jolt-fi-call f "accept" (jx-unmod-entry (jx-call (jx-it self) "next" '())))
                                               (loop)))
                                           jolt-nil)))))
(jx-class! "jx-unmod-listiter" "java.util.Collections$UnmodifiableList$1" '("java.util.ListIterator"))
(register-host-methods! "jx-unmod-listiter"
  (append jx-unmod-iter-methods
          (map (lambda (m) (cons m (lambda (self) (jx-call (jx-it self) m '()))))
               '("hasPrevious" "previous" "nextIndex" "previousIndex"))
          (list (cons "set" (lambda (self x) (jx-uoe))) (cons "add" (lambda (self x) (jx-uoe))))))
(define (jx-unmod-iter-rest it)
  (let loop ((acc '()))
    (if (jolt-truthy? (jx-call it "hasNext" '())) (loop (cons (jx-call it "next" '()) acc)) (reverse acc))))
(register-seq-arm-last! (lambda (x) (and (jhost? x) (member (jhost-tag x) '("jx-unmod-iter" "jx-unmod-entry-iter" "jx-unmod-listiter")) #t))
  (lambda (x) (let ((xs (jx-unmod-iter-rest (jx-it x))))
                (if (null? xs) jolt-nil
                    (list->cseq (if (string=? (jhost-tag x) "jx-unmod-entry-iter") (map jx-unmod-entry xs) xs))))))
;; an entry whose setValue is refused, reading through to the backing one
(jx-class! "jx-unmod-entry" "java.util.Collections$UnmodifiableMap$UnmodifiableEntrySet$UnmodifiableEntry"
           '("java.util.Map$Entry"))
(register-jutil-entry! "jx-unmod-entry"
  (lambda (self) (let ((e (jx-it self))) (cons (jolt-nth e 0) (jolt-nth e 1)))))
(register-host-methods! "jx-unmod-entry" (list (cons "setValue" (lambda (self v) (jx-uoe)))))

;; ---- the Collections statics -----------------------------------------------------------
;; Wrapping a collection that is already that view answers it, as the JDK's do.
(define (jx-unmod-static family)
  (lambda (x)
    (cond ((jolt-nil? x) (jx-npe))
          ((and (jhost? x) (string=? (jhost-tag x) (jx-unmod-tag family))) x)
          (else (jx-wrap-unmod family x)))))
(register-class-statics! "java.util.Collections"
  (list (cons "unmodifiableCollection" (jx-unmod-static 'coll))
        (cons "unmodifiableSequencedCollection" (jx-unmod-static 'seqcoll))
        (cons "unmodifiableSet" (jx-unmod-static 'set))
        (cons "unmodifiableSequencedSet" (jx-unmod-static 'seqset))
        (cons "unmodifiableSortedSet" (jx-unmod-static 'sortedset))
        (cons "unmodifiableNavigableSet" (jx-unmod-static 'navset))
        (cons "unmodifiableList"
              (lambda (x)
                (cond ((jolt-nil? x) (jx-npe))
                      ((and (jhost? x) (member (jhost-tag x) '("jx-unmod-list" "jx-unmod-ralist"))) x)
                      (else (jx-wrap-unmod (jx-list-family x) x)))))
        (cons "unmodifiableMap" (jx-unmod-static 'map))
        (cons "unmodifiableSequencedMap" (jx-unmod-static 'seqmap))
        (cons "unmodifiableSortedMap" (jx-unmod-static 'sortedmap))
        (cons "unmodifiableNavigableMap" (jx-unmod-static 'navmap))
        (cons "synchronizedCollection" (lambda (x) (jx-wrap-sync 'coll x #f)))
        (cons "synchronizedSet" (lambda (x) (jx-wrap-sync 'set x #f)))
        (cons "synchronizedSortedSet" (lambda (x) (jx-wrap-sync 'sortedset x #f)))
        (cons "synchronizedNavigableSet" (lambda (x) (jx-wrap-sync 'navset x #f)))
        (cons "synchronizedList" (lambda (x) (jx-wrap-sync (jx-list-family x) x #f)))
        (cons "synchronizedMap" (lambda (x) (jx-wrap-sync 'map x #f)))
        (cons "synchronizedSortedMap" (lambda (x) (jx-wrap-sync 'sortedmap x #f)))
        (cons "synchronizedNavigableMap" (lambda (x) (jx-wrap-sync 'navmap x #f)))))

;; Collections.emptyList / singletonList / nCopies: fixed lists, n copies of one
;; element (a null one included), the JDK's three classes. Each is an
;; AbstractList that answers size, get and contains itself and inherits the
;; rest (coll-defaults.ss); every change is refused, as AbstractList refuses it,
;; except the ones the JDK's classes leave as no-ops when there is nothing to
;; change (an empty list's clear, removeIf, replaceAll and sort, and a
;; one-element list's sort). State #(n x).
(define (jx-copies-n l) (vector-ref (jhost-state l) 0))
(define (jx-copies-x l) (vector-ref (jhost-state l) 1))
(define (jx-copies-elems l) (make-list (jx-copies-n l) (jx-copies-x l)))
(define (jx-copies-get l i)
  (let ((i (jnum->exact i)) (n (jx-copies-n l)))
    (unless (and (fx>=? i 0) (fx<? i n))
      (let ((is (number->string i)))
        (throw-jvm 'IndexOutOfBoundsException
                   (case (string->symbol (jhost-tag l))
                     ((jx-empty-list) (string-append "Index: " is))
                     ((jx-singleton-list) (string-append "Index: " is ", Size: 1"))
                     (else (string-append "Index " is " out of bounds for length " (number->string n)))))))
    (jx-copies-x l)))
(define (jx-copies-refuse . _) (jx-uoe))
(define (jx-copies-noop-when-empty f)
  (lambda (self . args) (if (fx=? 0 (jx-copies-n self)) (apply f self args) (jx-uoe))))
(define jx-copies-methods
  (list (cons "size" (lambda (self) (->num (jx-copies-n self))))
        (cons "isEmpty" (lambda (self) (fx=? 0 (jx-copies-n self))))
        (cons "get" jx-copies-get)
        (cons "contains" (lambda (self x) (and (fx>? (jx-copies-n self) 0) (objects-equals? x (jx-copies-x self)))))
        (cons "add" (host-arity-of '(1 2) #t jx-copies-refuse))
        (cons "addAll" (host-arity-of '(1 2) #t jx-copies-refuse))
        (cons "set" jx-copies-refuse)
        (cons "remove" jx-copies-refuse)
        (cons "removeAll" jx-copies-refuse)
        (cons "retainAll" jx-copies-refuse)
        (cons "clear" (jx-copies-noop-when-empty (lambda (self) jolt-nil)))
        (cons "removeIf" (jx-copies-noop-when-empty (lambda (self p) (when (jolt-nil? p) (jx-npe)) #f)))
        (cons "replaceAll" (jx-copies-noop-when-empty (lambda (self f) (when (jolt-nil? f) (jx-npe)) jolt-nil)))
        (cons "sort" (lambda (self c)
                       (if (fx<? (jx-copies-n self) (if (string=? (jhost-tag self) "jx-copies-list") 1 2))
                           jolt-nil
                           (jx-uoe))))))
(for-each (lambda (p)
            (jx-class! (car p) (cdr p) '("java.util.AbstractList" "java.util.RandomAccess" "java.io.Serializable"))
            (register-host-methods! (car p) jx-copies-methods)
            (jx-register! (car p) 'ralist jx-copies-elems jx-copies-n
                          (lambda (l x) (and (fx>? (jx-copies-n l) 0) (objects-equals? x (jx-copies-x l)))) #f))
          '(("jx-empty-list" . "java.util.Collections$EmptyList")
            ("jx-singleton-list" . "java.util.Collections$SingletonList")
            ("jx-copies-list" . "java.util.Collections$CopiesList")))
(define jx-the-empty-list (make-jhost "jx-empty-list" (vector 0 jolt-nil)))
;; Collections.emptySet / emptyMap / emptyIterator: one instance each, holding
;; nothing; a write that would add is refused, one with nothing to do is not.
(define (jx-empty-npe-check x) (when (jolt-nil? x) (jx-npe)))
(jx-class! "jx-empty-iter" "java.util.Collections$EmptyIterator" '("java.util.Iterator"))
(register-host-methods! "jx-empty-iter"
  (list (cons "hasNext" (lambda (self) #f))
        (cons "next" (lambda (self) (throw-jvm 'NoSuchElementException jolt-nil)))
        (cons "remove" (lambda (self) (throw-jvm 'IllegalStateException jolt-nil)))
        (cons "forEachRemaining" (lambda (self f) (jx-empty-npe-check f) jolt-nil))))
(define jx-the-empty-iter (make-jhost "jx-empty-iter" (vector)))
(jx-class! "jx-empty-set" "java.util.Collections$EmptySet" '("java.util.AbstractSet" "java.io.Serializable"))
(register-host-methods! "jx-empty-set"
  (list (cons "size" (lambda (self) 0))
        (cons "isEmpty" (lambda (self) #t))
        (cons "contains" (lambda (self x) #f))
        (cons "containsAll" (lambda (self c) (null? (jx-seq-list c))))
        (cons "iterator" (lambda (self) jx-the-empty-iter))
        (cons "add" (lambda (self x) (jx-uoe)))
        (cons "addAll" (lambda (self c) (if (null? (jx-seq-list c)) #f (jx-uoe))))
        (cons "remove" (lambda (self x) #f))
        (cons "removeAll" (lambda (self c) (jx-empty-npe-check c) #f))
        (cons "retainAll" (lambda (self c) (jx-empty-npe-check c) #f))
        (cons "removeIf" (lambda (self p) (jx-empty-npe-check p) #f))
        (cons "clear" (lambda (self) jolt-nil))
        (cons "forEach" (lambda (self f) (jx-empty-npe-check f) jolt-nil))))
(jx-register! "jx-empty-set" 'set (lambda (s) '()) (lambda (s) 0) (lambda (s x) #f) #f)
(define jx-the-empty-set (make-jhost "jx-empty-set" (vector)))
(jx-class! "jx-empty-map" "java.util.Collections$EmptyMap" '("java.util.AbstractMap" "java.io.Serializable"))
(register-host-methods! "jx-empty-map"
  (list (cons "size" (lambda (self) 0))
        (cons "isEmpty" (lambda (self) #t))
        (cons "get" (lambda (self k) jolt-nil))
        (cons "getOrDefault" (lambda (self k d) d))
        (cons "containsKey" (lambda (self k) #f))
        (cons "containsValue" (lambda (self v) #f))
        (cons "keySet" (lambda (self) jx-the-empty-set))
        (cons "values" (lambda (self) jx-the-empty-set))
        (cons "entrySet" (lambda (self) jx-the-empty-set))
        (cons "put" (lambda (self k v) (jx-uoe)))
        (cons "putAll" (lambda (self m) (if (null? (jx-seq-list m)) jolt-nil (jx-uoe))))
        (cons "remove" (lambda (self k) jolt-nil))
        (cons "clear" (lambda (self) jolt-nil))
        (cons "forEach" (lambda (self f) (jx-empty-npe-check f) jolt-nil))
        (cons "replaceAll" (lambda (self f) (jx-empty-npe-check f) jolt-nil))
        (cons "putIfAbsent" (lambda (self k v) (jx-uoe)))
        (cons "computeIfAbsent" (lambda (self k f) (jx-uoe)))
        (cons "computeIfPresent" (lambda (self k f) (jx-uoe)))
        (cons "compute" (lambda (self k f) (jx-uoe)))
        (cons "merge" (lambda (self k v f) (jx-uoe)))))
(jx-register! "jx-empty-map" 'map (lambda (m) '()) (lambda (m) 0) (lambda (m k) #f) (lambda (m k d) d))
(define jx-the-empty-map (make-jhost "jx-empty-map" (vector)))

;; Collections.sort and reverse change the list in place (sort through the
;; list's own sort, as the JDK's does); max and min walk the collection, by its
;; natural order or a Comparator (a Clojure fn is one, as on the JVM).
(define (jx-collections-pick coll cmp better?)
  (let ((xs (jx-seq-list coll))
        (f (if (jolt-nil? cmp) jolt-compare (tm-comparator->cmpf cmp))))
    (when (null? xs) (throw-jvm 'NoSuchElementException jolt-nil))
    (fold-left (lambda (best x) (if (better? (f x best)) x best)) (car xs) (cdr xs))))
(define (jx-collections-reverse! l)
  (let ((n (jnum->exact (jx-call l "size" '()))))
    (let loop ((i 0) (j (fx- n 1)))
      (when (fx<? i j)
        (let ((a (jx-call l "get" (list (->num i)))))
          (jx-call l "set" (list (->num i) (jx-call l "get" (list (->num j)))))
          (jx-call l "set" (list (->num j) a))
          (loop (fx+ i 1) (fx- j 1)))))
    jolt-nil))
(register-class-statics! "java.util.Collections"
  (list (cons "emptyList" (lambda () jx-the-empty-list))
        (cons "emptySet" (lambda () jx-the-empty-set))
        (cons "emptyMap" (lambda () jx-the-empty-map))
        (cons "emptyIterator" (lambda () jx-the-empty-iter))
        (cons "singletonList" (lambda (x) (make-jhost "jx-singleton-list" (vector 1 x))))
        (cons "nCopies" (lambda (n x)
                          (let ((n (jnum->exact n)))
                            (when (fx<? n 0) (throw-jvm 'IllegalArgumentException (string-append "List length = " (number->string n))))
                            (make-jhost "jx-copies-list" (vector n x)))))
        (cons "sort" (case-lambda ((l) (jx-call l "sort" (list jolt-nil)) jolt-nil)
                                  ((l c) (jx-call l "sort" (list c)) jolt-nil)))
        (cons "reverse" jx-collections-reverse!)
        (cons "max" (case-lambda ((c) (jx-collections-pick c jolt-nil positive?))
                                 ((c cmp) (jx-collections-pick c cmp positive?))))
        (cons "min" (case-lambda ((c) (jx-collections-pick c jolt-nil negative?))
                                 ((c cmp) (jx-collections-pick c cmp negative?))))))

;; ---- List.of / Set.of / Map.of ---------------------------------------------------------------
;; java.util.ImmutableCollections: ListN for none or three and more elements,
;; List12 for one or two, the same split for Set12 / SetN and Map1 / MapN. Every
;; element, key and value is checked non-null; Set.of and Map.of refuse a
;; duplicate. contains(null) and the like are NullPointerException, as they are
;; on these classes. Iteration is in the order given: the JDK's SetN and MapN
;; order is salted per run, so any fixed order is one of its orders.
(define (jx-no-nulls! xs) (for-each (lambda (x) (when (jolt-nil? x) (jx-npe))) xs) xs)
(define (jx-varargs args)
  ;; of(E...) handed one reference array is that array's elements
  (if (and (= 1 (length args)) (jolt-array? (car args)) (na-ref-kind? (jolt-array-kind (car args))))
      (ja->list (car args))
      args))
(define (jx-str x) (if (string? x) x (jolt-str-render-one x)))

;; lists: state #(vector)
(define (jxl-vec l) (vector-ref (jhost-state l) 0))
(define (jxl-list l) (vector->list (jxl-vec l)))
(define (make-jx-imm-list xs)
  (let ((v (list->vector xs)))
    (make-jhost (if (memv (vector-length v) '(1 2)) "jx-list12" "jx-listn") (vector v))))
(define (jxl-ref l i)
  (let ((i (jnum->exact i)) (v (jxl-vec l)))
    (unless (and (fx>=? i 0) (fx<? i (vector-length v)))
      ;; each class fails its own way: ListN indexes its array, List12 checks by
      ;; hand, the SubList through Objects.checkIndex
      (let ((n (number->string (vector-length v))) (is (number->string i)))
        (case (string->symbol (jhost-tag l))
          ((jx-listn) (throw-jvm 'ArrayIndexOutOfBoundsException (string-append "Index " is " out of bounds for length " n)))
          ((jx-list12) (throw-jvm 'IndexOutOfBoundsException (string-append "Index: " is " Size: " n)))
          (else (throw-jvm 'IndexOutOfBoundsException (string-append "Index " is " out of bounds for length " n))))))
    (vector-ref v i)))
(define (jx-index-of xs x)
  (when (jolt-nil? x) (jx-npe))
  (let loop ((xs xs) (i 0)) (cond ((null? xs) -1) ((objects-equals? x (car xs)) i) (else (loop (cdr xs) (fx+ i 1))))))
(define (make-jx-imm-listitr l i) (make-jhost "jx-imm-listitr" (vector (jxl-vec l) i)))
(define jx-imm-list-methods
  (append
    (map (lambda (m) (cons m (host-arity-of '(0 1 2) #t jx-uoe)))
         '("add" "addAll" "clear" "remove" "removeAll" "removeIf" "retainAll" "replaceAll" "set" "sort"
           "addFirst" "addLast" "removeFirst" "removeLast"))
    (list
      (cons "size" (lambda (self) (->num (vector-length (jxl-vec self)))))
      (cons "isEmpty" (lambda (self) (fx=? 0 (vector-length (jxl-vec self)))))
      (cons "get" jxl-ref)
      (cons "contains" (lambda (self x) (fx>=? (jx-index-of (jxl-list self) x) 0)))
      (cons "containsAll" (lambda (self c) (for-all (lambda (x) (fx>=? (jx-index-of (jxl-list self) x) 0)) (jx-seq-list c))))
      (cons "indexOf" (lambda (self x) (->num (jx-index-of (jxl-list self) x))))
      (cons "lastIndexOf" (lambda (self x)
                            (let ((i (jx-index-of (reverse (jxl-list self)) x)))
                              (->num (if (fx<? i 0) -1 (fx- (fx- (vector-length (jxl-vec self)) 1) i))))))
      (cons "iterator" (lambda (self) (make-jx-imm-listitr self 0)))
      (cons "listIterator" (case-lambda ((self) (make-jx-imm-listitr self 0))
                                        ((self i) (let ((i (jnum->exact i)))
                                                    (unless (and (fx>=? i 0) (fx<=? i (vector-length (jxl-vec self))))
                                                      (throw-jvm 'IndexOutOfBoundsException (string-append "Index: " (number->string i))))
                                                    (make-jx-imm-listitr self i)))))
      (cons "subList" (lambda (self from to)
                        (let ((from (jnum->exact from)) (to (jnum->exact to)) (n (vector-length (jxl-vec self))))
                          (cd-range-check! from to n)
                          (make-jhost "jx-imm-sublist" (vector (list->vector (list-head (list-tail (jxl-list self) from) (fx- to from))))))))
      (cons "toArray" (case-lambda ((self) (cd-to-array (jxl-list self) '())) ((self a) (cd-to-array (jxl-list self) (list a)))))
      (cons "forEach" (lambda (self f) (for-each (lambda (x) (jolt-fi-call f "accept" x)) (jxl-list self)) jolt-nil))
      (cons "getFirst" (lambda (self) (if (fx=? 0 (vector-length (jxl-vec self))) (throw-jvm 'NoSuchElementException jolt-nil) (vector-ref (jxl-vec self) 0))))
      (cons "getLast" (lambda (self) (let ((n (vector-length (jxl-vec self))))
                                       (if (fx=? 0 n) (throw-jvm 'NoSuchElementException jolt-nil) (vector-ref (jxl-vec self) (fx- n 1))))))
      ;; List.reversed: ReverseOrderListView over it, whose writes the list refuses
      (cons "reversed" (lambda (self) (make-jhost "jx-reverse-list" (vector self)))))))
(for-each (lambda (p)
            (jx-class! (car p) (cdr p))
            (register-host-methods! (car p) jx-imm-list-methods)
            (jx-register! (car p) 'ralist jxl-list (lambda (l) (vector-length (jxl-vec l))) #f #f))
          '(("jx-listn" . "java.util.ImmutableCollections$ListN")
            ("jx-list12" . "java.util.ImmutableCollections$List12")
            ("jx-imm-sublist" . "java.util.ImmutableCollections$SubList")))
(jch-register-supers! "java.util.ImmutableCollections$AbstractImmutableCollection" '("java.util.AbstractCollection"))
(jch-register-supers! "java.util.ImmutableCollections$AbstractImmutableList"
                      '("java.util.ImmutableCollections$AbstractImmutableCollection" "java.util.List" "java.util.RandomAccess"))
(jch-register-supers! "java.util.ImmutableCollections$List12" '("java.util.ImmutableCollections$AbstractImmutableList" "java.io.Serializable"))
(jch-register-supers! "java.util.ImmutableCollections$SubList" '("java.util.ImmutableCollections$AbstractImmutableList" "java.util.RandomAccess"))
(jch-register-supers! "java.util.AbstractCollection" '("java.util.Collection"))
;; the list iterator, read-only: #(vector cursor)
(jx-class! "jx-imm-listitr" "java.util.ImmutableCollections$ListItr" '("java.util.ListIterator"))
(define (jxi-v it) (vector-ref (jhost-state it) 0))
(define (jxi-i it) (vector-ref (jhost-state it) 1))
(define (jxi-i! it i) (vector-set! (jhost-state it) 1 i))
(register-host-methods! "jx-imm-listitr"
  (list (cons "hasNext" (lambda (self) (fx<? (jxi-i self) (vector-length (jxi-v self)))))
        (cons "next" (lambda (self)
                       (let ((i (jxi-i self)))
                         (unless (fx<? i (vector-length (jxi-v self))) (throw-jvm 'NoSuchElementException jolt-nil))
                         (jxi-i! self (fx+ i 1)) (vector-ref (jxi-v self) i))))
        (cons "hasPrevious" (lambda (self) (fx>? (jxi-i self) 0)))
        (cons "previous" (lambda (self)
                           (let ((i (fx- (jxi-i self) 1)))
                             (when (fx<? i 0) (throw-jvm 'NoSuchElementException jolt-nil))
                             (jxi-i! self i) (vector-ref (jxi-v self) i))))
        (cons "nextIndex" (lambda (self) (->num (jxi-i self))))
        (cons "previousIndex" (lambda (self) (->num (fx- (jxi-i self) 1))))
        (cons "remove" (lambda (self) (jx-uoe))) (cons "set" (lambda (self x) (jx-uoe))) (cons "add" (lambda (self x) (jx-uoe)))
        (cons "forEachRemaining" (lambda (self f)
                                   (let loop ()
                                     (when (fx<? (jxi-i self) (vector-length (jxi-v self)))
                                       (let ((x (vector-ref (jxi-v self) (jxi-i self))))
                                         (jxi-i! self (fx+ (jxi-i self) 1))
                                         (jolt-fi-call f "accept" x)
                                         (loop))))
                                   jolt-nil))))
(define (jx-iter-from-list tag xs) (make-jhost tag (vector (list->vector xs) 0)))
(register-seq-arm-last! (lambda (x) (and (jhost? x) (member (jhost-tag x) jx-imm-iter-tags) #t))
  (lambda (it) (let ((rest (list-tail (vector->list (jxi-v it)) (jxi-i it))))
                 (if (null? rest) jolt-nil (list->cseq rest)))))
;; sets and maps iterate the same way, read-only, each its JDK iterator class
(define jx-imm-iter-tags '("jx-imm-listitr" "jx-imm-setn-iter" "jx-imm-set12-iter" "jx-imm-mapn-iter"
                           "jx-absmap-keys-iter" "jx-absmap-values-iter"))
(for-each (lambda (p)
            (jx-class! (car p) (cdr p) '("java.util.Iterator"))
            (alias-host-methods! (car p) "jx-imm-listitr"))
          '(("jx-imm-setn-iter" . "java.util.ImmutableCollections$SetN$SetNIterator")
            ("jx-imm-set12-iter" . "java.util.ImmutableCollections$Set12$1")
            ("jx-imm-mapn-iter" . "java.util.ImmutableCollections$MapN$MapNIterator")
            ("jx-absmap-keys-iter" . "java.util.AbstractMap$1$1")
            ("jx-absmap-values-iter" . "java.util.AbstractMap$2$1")))

;; sets: state #(vector table)
(define (jxs-vec s) (vector-ref (jhost-state s) 0))
(define (jxs-list s) (vector->list (jxs-vec s)))
(define (jxs-has? s x) (when (jolt-nil? x) (jx-npe)) (hashtable-contains? (vector-ref (jhost-state s) 1) x))
(define (make-jx-imm-set xs dup-error?)
  (let ((ht (make-hashtable hm-hash jolt=2)))
    (let loop ((xs (jx-no-nulls! xs)) (acc '()))
      (cond ((null? xs)
             (let ((v (list->vector (reverse acc))))
               (make-jhost (if (memv (vector-length v) '(1 2)) "jx-set12" "jx-setn") (vector v ht))))
            ((hashtable-contains? ht (car xs))
             (if dup-error?
                 (throw-jvm 'IllegalArgumentException (string-append "duplicate element: " (jx-str (car xs))))
                 (loop (cdr xs) acc)))
            (else (hashtable-set! ht (car xs) #t) (loop (cdr xs) (cons (car xs) acc)))))))
(define jx-imm-set-methods
  (append
    (map (lambda (m) (cons m (host-arity-of '(0 1) #t jx-uoe)))
         '("add" "addAll" "clear" "remove" "removeAll" "removeIf" "retainAll"))
    (list
      (cons "size" (lambda (self) (->num (vector-length (jxs-vec self)))))
      (cons "isEmpty" (lambda (self) (fx=? 0 (vector-length (jxs-vec self)))))
      (cons "contains" jxs-has?)
      (cons "containsAll" (lambda (self c) (for-all (lambda (x) (jxs-has? self x)) (jx-seq-list c))))
      (cons "iterator" (lambda (self) (jx-iter-from-list (if (string=? (jhost-tag self) "jx-set12") "jx-imm-set12-iter" "jx-imm-setn-iter")
                                                         (jxs-list self))))
      (cons "toArray" (case-lambda ((self) (cd-to-array (jxs-list self) '())) ((self a) (cd-to-array (jxs-list self) (list a)))))
      (cons "forEach" (lambda (self f) (for-each (lambda (x) (jolt-fi-call f "accept" x)) (jxs-list self)) jolt-nil)))))
(for-each (lambda (p)
            (jx-class! (car p) (cdr p) '("java.util.ImmutableCollections$AbstractImmutableSet" "java.io.Serializable"))
            (register-host-methods! (car p) jx-imm-set-methods)
            (jx-register! (car p) 'set jxs-list (lambda (s) (vector-length (jxs-vec s)))
                          ;; contains? / get of nil is the JDK's NullPointerException
                          (lambda (s x) (jxs-has? s x)) #f))
          '(("jx-setn" . "java.util.ImmutableCollections$SetN") ("jx-set12" . "java.util.ImmutableCollections$Set12")))
(jch-register-supers! "java.util.ImmutableCollections$AbstractImmutableSet"
                      '("java.util.ImmutableCollections$AbstractImmutableCollection" "java.util.Set"))

;; maps: state #(keys-vector table); an entry is a KeyValueHolder
(define (jxm-keys m) (vector->list (vector-ref (jhost-state m) 0)))
(define (jxm-tbl m) (vector-ref (jhost-state m) 1))
(define (jxm-ref m k) (when (jolt-nil? k) (jx-npe)) (hashtable-ref (jxm-tbl m) k cd-absent))
(define (jxm-entries m) (map (lambda (k) (make-jx-kv k (hashtable-ref (jxm-tbl m) k jolt-nil))) (jxm-keys m)))
(define (jxm-vals m) (map (lambda (k) (hashtable-ref (jxm-tbl m) k jolt-nil)) (jxm-keys m)))
(define (make-jx-kv k v) (when (or (jolt-nil? k) (jolt-nil? v)) (jx-npe)) (make-jhost "jx-kvholder" (vector k v)))
(jx-class! "jx-kvholder" "java.util.KeyValueHolder" '("java.util.Map$Entry"))
(register-jutil-entry! "jx-kvholder" (lambda (self) (cons (vector-ref (jhost-state self) 0) (vector-ref (jhost-state self) 1))))
(register-host-methods! "jx-kvholder"
  (list (cons "setValue" (lambda (self v) (throw-jvm 'UnsupportedOperationException "not supported")))))
(define (make-jx-imm-map pairs)
  (let ((ht (make-hashtable hm-hash jolt=2)))
    (let loop ((ps pairs) (acc '()))
      (cond ((null? ps)
             (let ((v (list->vector (reverse acc))))
               (make-jhost (if (fx=? (vector-length v) 1) "jx-map1" "jx-mapn") (vector v ht))))
            (else
             (let ((k (caar ps)) (v (cdar ps)))
               (when (or (jolt-nil? k) (jolt-nil? v)) (jx-npe))
               (when (hashtable-contains? ht k)
                 (throw-jvm 'IllegalArgumentException (string-append "duplicate key: " (jx-str k))))
               (hashtable-set! ht k v)
               (loop (cdr ps) (cons k acc))))))))
(define jx-imm-map-methods
  (append
    (map (lambda (m) (cons m (host-arity-of '(0 1 2 3) #t jx-uoe)))
         '("clear" "compute" "computeIfAbsent" "computeIfPresent" "merge" "put" "putAll" "putIfAbsent"
           "remove" "replace" "replaceAll"))
    (list
      (cons "size" (lambda (self) (->num (vector-length (vector-ref (jhost-state self) 0)))))
      (cons "isEmpty" (lambda (self) (fx=? 0 (vector-length (vector-ref (jhost-state self) 0)))))
      (cons "get" (lambda (self k) (let ((v (jxm-ref self k))) (if (eq? v cd-absent) jolt-nil v))))
      (cons "getOrDefault" (lambda (self k d) (let ((v (jxm-ref self k))) (if (eq? v cd-absent) d v))))
      (cons "containsKey" (lambda (self k) (not (eq? (jxm-ref self k) cd-absent))))
      (cons "containsValue" (lambda (self x) (when (jolt-nil? x) (jx-npe))
                              (and (exists (lambda (v) (objects-equals? x v)) (jxm-vals self)) #t)))
      (cons "keySet" (lambda (self) (make-jhost "jx-absmap-keyset" (vector self))))
      (cons "values" (lambda (self) (make-jhost "jx-absmap-values" (vector self))))
      (cons "entrySet" (lambda (self)
                         (if (string=? (jhost-tag self) "jx-map1")
                             (make-jx-imm-set (jxm-entries self) #f)
                             (make-jhost "jx-imm-mapn-entryset" (vector self)))))
      (cons "forEach" (lambda (self f)
                        (for-each (lambda (k) (jolt-fi-call f "accept" k (hashtable-ref (jxm-tbl self) k jolt-nil))) (jxm-keys self))
                        jolt-nil)))))
(for-each (lambda (p)
            (jx-class! (car p) (cdr p) '("java.util.ImmutableCollections$AbstractImmutableMap" "java.io.Serializable"))
            (register-host-methods! (car p) jx-imm-map-methods)
            (jx-register! (car p) 'map jxm-entries (lambda (m) (vector-length (vector-ref (jhost-state m) 0)))
                          (lambda (m k) (not (eq? (jxm-ref m k) cd-absent)))
                          (lambda (m k d) (let ((v (jxm-ref m k))) (if (eq? v cd-absent) d v)))))
          '(("jx-mapn" . "java.util.ImmutableCollections$MapN") ("jx-map1" . "java.util.ImmutableCollections$Map1")))
(jch-register-supers! "java.util.ImmutableCollections$AbstractImmutableMap" '("java.util.AbstractMap"))
;; the three views over an immutable map: AbstractMap's key set and values, and
;; MapN's entry set. Read-only, like the map; state #(map).
(define (jxv-map v) (vector-ref (jhost-state v) 0))
(define (jx-view-methods elems iter-tag has?)
  (append
    (map (lambda (m) (cons m (host-arity-of '(0 1) #t jx-uoe)))
         '("add" "addAll" "clear" "remove" "removeAll" "removeIf" "retainAll"))
    (list (cons "size" (lambda (self) (->num (length (elems self)))))
          (cons "isEmpty" (lambda (self) (null? (elems self))))
          (cons "contains" (lambda (self x) (has? self x)))
          (cons "iterator" (lambda (self) (jx-iter-from-list iter-tag (elems self))))
          (cons "toArray" (case-lambda ((self) (cd-to-array (elems self) '())) ((self a) (cd-to-array (elems self) (list a))))))))
(define (jxv-keys v) (jxm-keys (jxv-map v)))
(define (jxv-vals v) (jxm-vals (jxv-map v)))
(define (jxv-entries v) (jxm-entries (jxv-map v)))
(define (jxv-has-key? v k) (and (not (jolt-nil? k)) (not (eq? (jxm-ref (jxv-map v) k) cd-absent))))
(define (jxv-has-val? v x) (and (exists (lambda (y) (objects-equals? x y)) (jxv-vals v)) #t))
(define (jxv-has-entry? v e)
  (let ((kv (or (jolt-host-entry e) (and (jolt-map-entry? e) (cons (jolt-nth e 0) (jolt-nth e 1))))))
    (and kv (not (jolt-nil? (car kv)))
         (let ((x (jxm-ref (jxv-map v) (car kv)))) (and (not (eq? x cd-absent)) (objects-equals? x (cdr kv)))))))
(jx-class! "jx-absmap-keyset" "java.util.AbstractMap$1" '("java.util.AbstractSet"))
(register-host-methods! "jx-absmap-keyset" (jx-view-methods jxv-keys "jx-absmap-keys-iter" jxv-has-key?))
(jx-register! "jx-absmap-keyset" 'set jxv-keys (lambda (v) (length (jxv-keys v))) jxv-has-key? #f)
(jx-class! "jx-absmap-values" "java.util.AbstractMap$2" '("java.util.AbstractCollection"))
(register-host-methods! "jx-absmap-values" (jx-view-methods jxv-vals "jx-absmap-values-iter" jxv-has-val?))
(jx-register! "jx-absmap-values" 'coll jxv-vals (lambda (v) (length (jxv-vals v))) #f #f)
(jx-class! "jx-imm-mapn-entryset" "java.util.ImmutableCollections$MapN$1" '("java.util.AbstractSet"))
(register-host-methods! "jx-imm-mapn-entryset" (jx-view-methods jxv-entries "jx-imm-mapn-iter" jxv-has-entry?))
(jx-register! "jx-imm-mapn-entryset" 'entries jxv-entries (lambda (v) (length (jxv-entries v))) jxv-has-entry? #f)
(jch-register-supers! "java.util.AbstractSet" '("java.util.AbstractCollection" "java.util.Set"))
(jch-register-supers! "java.util.AbstractMap" '("java.util.Map"))

(define (jx-imm-list? x) (and (jhost? x) (member (jhost-tag x) '("jx-listn" "jx-list12")) #t))
(define (jx-imm-set? x) (and (jhost? x) (member (jhost-tag x) '("jx-setn" "jx-set12")) #t))
(define (jx-imm-map? x) (and (jhost? x) (member (jhost-tag x) '("jx-mapn" "jx-map1")) #t))
(define (jx-entry-kv e)
  (or (jolt-host-entry e)
      (and (jolt-map-entry? e) (cons (jolt-nth e 0) (jolt-nth e 1)))
      (throw-jvm 'ClassCastException (string-append "class " (jolt-class-name e) " cannot be cast to class java.util.Map$Entry"))))
(define (jx-map-of args)
  (unless (even? (length args))
    (throw-jvm 'IllegalArgumentException "Map.of takes key, value pairs"))
  (make-jx-imm-map (let loop ((xs args)) (if (null? xs) '() (cons (cons (car xs) (cadr xs)) (loop (cddr xs)))))))
(register-class-statics! "java.util.List"
  (list (cons "of" (lambda args (make-jx-imm-list (jx-no-nulls! (jx-varargs args)))))
        (cons "copyOf" (lambda (c)
                         (cond ((jolt-nil? c) (jx-npe))
                               ((jx-imm-list? c) c)
                               (else (make-jx-imm-list (jx-no-nulls! (jx-seq-list c)))))))))
(register-class-statics! "java.util.Set"
  (list (cons "of" (lambda args (make-jx-imm-set (jx-varargs args) #t)))
        (cons "copyOf" (lambda (c)
                         (cond ((jolt-nil? c) (jx-npe))
                               ((jx-imm-set? c) c)
                               (else (make-jx-imm-set (jx-seq-list c) #f)))))))
(register-class-statics! "java.util.Map"
  (list (cons "of" (lambda args (jx-map-of args)))
        (cons "entry" (lambda (k v) (make-jx-kv k v)))
        (cons "ofEntries" (lambda args (make-jx-imm-map (map jx-entry-kv (jx-varargs args)))))
        (cons "copyOf" (lambda (m)
                         (cond ((jolt-nil? m) (jx-npe))
                               ((jx-imm-map? m) m)
                               (else (make-jx-imm-map (map jx-entry-kv (jx-seq-list m)))))))))
