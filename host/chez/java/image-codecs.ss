;; image-codecs.ss — how the host's own values travel in a state image.
;;
;; A value whose state is a Chez structure the image cannot write as it is (a
;; hashtable keyed through jolt's hash, a chain of nodes, a tree whose comparator
;; is an internal procedure) is written through a codec (host-static.ss
;; register-image-codec!): its contents in order, as plain data, and rebuilt from
;; them by the shim's own constructor. Contents keep their order, a sorted
;; collection its comparator, a map its access order. A value that cannot travel
;; is refused by name. test/chez/image-coverage-test.ss (make imagecoverage) holds
;; every java.util collection shim to one or the other.
;;
;; Loaded after every shim it describes, before state-image.ss.

;; items as a flat list k1 v1 k2 v2 …, and back
(define (codec-kvs pairs) (fold-right (lambda (p acc) (cons (car p) (cons (cdr p) acc))) '() pairs))
(define (codec-each-kv! items f)
  (let loop ((xs items))
    (unless (null? xs) (f (car xs) (cadr xs)) (loop (cddr xs)))))

;; ---- HashMap, its look-alikes, Properties and HashSet ---------------------------
;; header #(tag defaults); a Properties keeps its defaults, not System's writethrough
(register-image-codec! "java.util.HashMap" '("hashmap" "properties" "chm" "jx-weak-hashmap" "jx-identity-hashmap")
  (lambda (m)
    (vector (vector (jhost-tag m) (or (and (string=? (jhost-tag m) "properties") (props-defaults m)) jolt-nil))
            (codec-kvs (hm-pairs-ordered m))))
  (lambda (h)
    (if (string=? (vector-ref h 0) "properties")
        (props-own-monitor! (make-jhost "properties" (props-state (make-hashtable hm-hash jolt=2) '()
                                                                  (if (jolt-nil? (vector-ref h 1)) #f (vector-ref h 1)) #f)))
        ;; each map's own kind of table: weak keys, identity keys, or equality
        (let ((tag (vector-ref h 0)))
          (cond ((string=? tag "jx-weak-hashmap") (make-jhost tag (hm-state (make-weak-hashtable hm-hash jolt=2) (list 'v))))
                ((string=? tag "jx-identity-hashmap") (make-jhost tag (hm-state (make-eq-hashtable) '())))
                ((string=? tag "chm") (make-chm-jhost))
                (else (make-jhost tag (hm-state (make-hashtable hm-hash jolt=2) '())))))))
  (lambda (m items)
    (codec-each-kv! items (lambda (k v) (hm-note-key! m k) (hashtable-set! (hm-tbl m) k v)))))
(register-image-codec! "java.util.HashSet" '("hashset")
  (lambda (s) (vector "hashset" (hs->list s)))
  (lambda (h) (make-jhost h (hm-state (make-hashtable hm-hash jolt=2) '())))
  (lambda (s items) (for-each (lambda (x) (hm-note-key! s x) (hashtable-set! (hm-tbl s) x #t)) items)))

;; ---- LinkedHashMap / LinkedHashSet: encounter order and access order -------------
(register-image-codec! "java.util.LinkedHashMap" '("linkedhashmap")
  (lambda (m)
    (let ((c (jhost-state m)))
      (vector (lh-access? c) (codec-kvs (map (lambda (n) (cons (n-key n) (n-val n))) (lh-nodes c #f))))))
  (lambda (access?) (make-jhost "linkedhashmap" (lh-new access?)))
  (lambda (m items)
    (let ((c (jhost-state m)))
      (codec-each-kv! items (lambda (k v) (lh-insert! c #f k v #f))))))
(register-image-codec! "java.util.LinkedHashSet" '("linkedhashset")
  (lambda (s) (vector jolt-nil (map n-key (lh-nodes (jhost-state s) #f))))
  (lambda (h) (make-lhs))
  (lambda (s items) (for-each (lambda (x) (lh-insert! (jhost-state s) #f x #t #f)) items)))

;; java.util.jar.Attributes is a LinkedHashMap chain under its own class
;; (embedded-manifest.ss); its keys are Attributes.Name values.
(register-image-codec! "java.util.jar.Attributes" '("jar-attributes")
  (lambda (a) (vector jolt-nil (codec-kvs (jar-attributes-pairs a))))
  (lambda (h) (make-manifest-attributes))
  (lambda (a items) (codec-each-kv! items (lambda (k v) (jar-attributes-put! a k v)))))

;; ---- TreeMap / TreeSet: the comparator object (nil for natural order) ------------
;; The treemap and treeset tags are a whole map or set; a sub-map or a key set
;; has a tag of its own and is a view of one, refused below.
(register-image-codec! "java.util.TreeMap" '("treemap")
  (lambda (m)
    (vector (tmr-cmp-obj (v-root m))
            (codec-kvs (map (lambda (e) (cons (e-key e) (e-val e))) (v-entries m)))))
  (lambda (cmp) (tm-new-map cmp))
  (lambda (m items) (codec-each-kv! items (lambda (k v) (v-put! m k v)))))
(register-image-codec! "java.util.TreeSet" '("treeset")
  (lambda (s) (vector (tmr-cmp-obj (v-root s)) (v-keys s)))
  (lambda (cmp) (if (jolt-nil? cmp) (ts-ctor) (ts-ctor cmp)))
  (lambda (s items) (for-each (lambda (x) (ts-add! s x)) items)))

;; ---- EnumSet / EnumMap: the enum class by name, the members by ordinal ----------
(define (codec-enum-info fqn)
  (or (enum-class-info fqn)
      (jolt-throw (jolt-ex-info (string-append "image: no enum " fqn " in this build") empty-pmap))))
(register-image-codec! "java.util.EnumSet" '("enum-set-regular" "enum-set-jumbo")
  (lambda (s)
    (vector (enum-info-fqn (eset-info s))
            (let loop ((i (fx- (vector-length (eset-bits s)) 1)) (acc '()))
              (if (fx<? i 0) acc (loop (fx- i 1) (if (vector-ref (eset-bits s) i) (cons i acc) acc))))))
  (lambda (fqn) (eset-of (codec-enum-info fqn) #f))
  (lambda (s items) (for-each (lambda (i) (vector-set! (eset-bits s) i #t)) items)))
(register-image-codec! "java.util.EnumMap" '("enum-map")
  (lambda (m) (vector (enum-info-fqn (emap-info m)) (codec-kvs (emap-slots m))))
  (lambda (fqn) (emap-new (codec-enum-info fqn)))
  (lambda (m items) (codec-each-kv! items (lambda (i v) (vector-set! (emap-vals m) i v)))))

;; ---- StringBuilder / StringBuffer: the text --------------------------------------
(register-image-codec! "java.lang.StringBuilder" '("string-builder" "string-buffer")
  (lambda (sb) (vector (jhost-tag sb) (sb-str* sb)))
  (lambda (tag) (host-new (if (string=? tag "string-buffer") "java.lang.StringBuffer" "java.lang.StringBuilder")))
  (lambda (sb text) (record-method-dispatch sb "append" (jolt-list text))))

;; ---- a library's tagged table (jolt.host/tagged-table): its fields ---------------
;; A sorted collection is an htable too, and travels by its own arm (state-image.ss).
(register-image-codec-pred! "jolt.host.TaggedTable"
  (lambda (x) (and (htable? x) (not (htable-sorted? x))))
  (lambda (t)
    (vector jolt-nil
            (let-values (((ks vs) (hashtable-entries (htable-h t))))
              (codec-kvs (map cons (vector->list ks) (vector->list vs))))))
  (lambda (h) (make-htable (make-hashtable string-hash string=?)))
  (lambda (t items) (codec-each-kv! items (lambda (k v) (hashtable-set! (htable-h t) k v)))))

;; ---- Collections.unmodifiable* / synchronized* (jutil-views.ss) -------------------
;; A wrapper travels with the collection it wraps, as the JDK serializes one; a
;; synchronized one gets its own monitor afresh, which no image can carry.
(define codec-unmod-tags (filter values (map (lambda (f) (list-ref f 1)) jx-families)))
(define codec-sync-tags (filter values (map (lambda (f) (list-ref f 2)) jx-families)))
(register-image-codec! "java.util.Collections$UnmodifiableCollection"
  (filter (lambda (t) (not (string=? t "jx-unmod-entryset"))) codec-unmod-tags)
  (lambda (w) (vector (jhost-tag w) (list (jxw-inner w))))
  (lambda (tag) (make-jhost tag (vector #f)))
  (lambda (w items) (vector-set! (jhost-state w) 0 (car items))))
(register-image-codec! "java.util.Collections$SynchronizedCollection" codec-sync-tags
  (lambda (w) (vector (jhost-tag w) (list (jxw-inner w))))
  (lambda (tag) (let ((w (make-jhost tag (vector #f #f)))) (vector-set! (jhost-state w) 1 (object-monitor w)) w))
  (lambda (w items) (vector-set! (jhost-state w) 0 (car items))))
;; ---- List.of / Set.of / Map.of and Map.entry ------------------------------------------
(register-image-codec! "java.util.ImmutableCollections$ListN" '("jx-listn" "jx-list12" "jx-imm-sublist")
  (lambda (l) (vector (jhost-tag l) (jxl-list l)))
  (lambda (tag) (make-jhost tag (vector #f)))
  (lambda (l items) (vector-set! (jhost-state l) 0 (list->vector items))))
(register-image-codec! "java.util.ImmutableCollections$SetN" '("jx-setn" "jx-set12")
  (lambda (s) (vector (jhost-tag s) (jxs-list s)))
  (lambda (tag) (make-jhost tag (vector (vector) (make-hashtable hm-hash jolt=2))))
  (lambda (s items)
    (vector-set! (jhost-state s) 0 (list->vector items))
    (for-each (lambda (x) (hashtable-set! (vector-ref (jhost-state s) 1) x #t)) items)))
(register-image-codec! "java.util.ImmutableCollections$MapN" '("jx-mapn" "jx-map1")
  (lambda (m) (vector (jhost-tag m) (codec-kvs (map (lambda (k) (cons k (hashtable-ref (jxm-tbl m) k jolt-nil))) (jxm-keys m)))))
  (lambda (tag) (make-jhost tag (vector (vector) (make-hashtable hm-hash jolt=2))))
  (lambda (m items)
    (let loop ((xs items) (ks '()))
      (if (null? xs)
          (vector-set! (jhost-state m) 0 (list->vector (reverse ks)))
          (begin (hashtable-set! (jxm-tbl m) (car xs) (cadr xs)) (loop (cddr xs) (cons (car xs) ks)))))))
;; Collections.emptyList comes back as THE empty list, as the JDK's readResolve
;; answers EMPTY_LIST; a singleton or copies list as its count and element
(register-image-codec! "java.util.Collections$CopiesList" '("jx-empty-list" "jx-singleton-list" "jx-copies-list")
  (lambda (l) (vector (jhost-tag l) (list (jx-copies-n l) (jx-copies-x l))))
  (lambda (tag) (if (string=? tag "jx-empty-list") jx-the-empty-list (make-jhost tag (vector 0 jolt-nil))))
  (lambda (l items)
    (unless (eq? l jx-the-empty-list)
      (vector-set! (jhost-state l) 0 (car items))
      (vector-set! (jhost-state l) 1 (cadr items)))))
;; the empty set and map are one instance each, and come back as it
(register-image-codec! "java.util.Collections$EmptySet" '("jx-empty-set" "jx-empty-map")
  (lambda (x) (vector (jhost-tag x) '()))
  (lambda (tag) (if (string=? tag "jx-empty-set") jx-the-empty-set jx-the-empty-map))
  (lambda (x items) (void)))
(register-image-codec! "java.util.KeyValueHolder" '("jx-kvholder")
  (lambda (e) (vector jolt-nil (list (vector-ref (jhost-state e) 0) (vector-ref (jhost-state e) 1))))
  (lambda (h) (make-jhost "jx-kvholder" (vector #f #f)))
  (lambda (e items) (vector-set! (jhost-state e) 0 (car items)) (vector-set! (jhost-state e) 1 (cadr items))))
(register-image-refusal!
  '("jx-unmod-entryset" "jx-unmod-entry" "jx-absmap-keyset" "jx-absmap-values" "jx-imm-mapn-entryset"
    "cd-arraylist-sublist" "cd-ra-sublist" "cd-sublist")
  "it is a view of another collection, which the image would have to share; write the collection itself")
(register-image-refusal!
  '("jx-unmod-iter" "jx-unmod-entry-iter" "jx-unmod-listiter" "jx-imm-listitr" "jx-imm-setn-iter"
    "jx-imm-set12-iter" "jx-imm-mapn-iter" "jx-absmap-keys-iter" "jx-absmap-values-iter"
    "cd-arraylist-listitr" "cd-vector-listitr" "cd-abstractlist-listitr")
  "an iterator is a position in a walk this process is making; write the collection instead")

;; ---- the legacy and concurrent collections (jutil-concurrent.ss) ---------------------
;; A Vector keeps its capacity and increment, a Hashtable its bucket count and
;; load factor (refilled newest-last so each chain, and so the iteration order,
;; comes back as it was), a PriorityQueue its heap array as it stands, a skip
;; list its comparator. Each gets a fresh lock.
(register-image-codec! "java.util.Vector" '("jx-vector" "jx-stack")
  (lambda (v) (vector (vector (jhost-tag v) (vec-cap v) (vec-incr v)) (vec-list v)))
  (lambda (h) (make-jx-vector (vector-ref h 0) '() (vector-ref h 1) (vector-ref h 2)))
  (lambda (v items) (for-each (lambda (x) (al-push! (vec-al v) x)) items)))
(register-image-codec! "java.util.Hashtable" '("jx-hashtable")
  (lambda (h) (vector (vector (vector-length (ht-buckets h)) (ht-lf h))
                      (codec-kvs (map (lambda (e) (cons (he-key e) (he-val e))) (with-ht h (ht-entry-list h))))))
  (lambda (hd) (make-jx-hashtable (vector-ref hd 0) (vector-ref hd 1)))
  (lambda (h items)
    (let loop ((xs items) (acc '()))
      (if (null? xs)
          (for-each (lambda (p) (ht-put! h (car p) (cdr p))) acc)
          (loop (cddr xs) (cons (cons (car xs) (cadr xs)) acc))))))
(register-image-codec! "java.util.PriorityQueue" '("jx-priority-queue")
  (lambda (p) (vector (pq-cmp-obj p) (pq-list p)))
  (lambda (cmp) (make-jx-pq cmp 1))
  (lambda (p items) (pq-fill! p items)))
(register-image-codec! "java.util.concurrent.ConcurrentLinkedQueue" '("jx-clq" "jx-cld")
  (lambda (q) (vector (jhost-tag q) (cl-list q)))
  (lambda (tag) (make-jx-clq tag '()))
  (lambda (q items) (for-each (lambda (x) (cl-link-last! q x)) items)))
(register-image-codec! "java.util.concurrent.CopyOnWriteArrayList" '("jx-cow-list" "jx-cow-set")
  (lambda (l) (vector (jhost-tag l) (cow-list l)))
  (lambda (tag) (make-jx-cow tag '()))
  (lambda (l items) (vector-set! (jhost-state l) 0 (list->vector items))))
(register-image-codec! "java.util.concurrent.ConcurrentSkipListMap" '("jx-cslm")
  (lambda (m) (vector (tmr-cmp-obj (cs-root m)) (codec-kvs (map (lambda (e) (cons (e-key e) (e-val e))) (v-entries (cs-base m))))))
  (lambda (cmp) (make-cs "jx-cslm" (cs-new-root cmp)))
  (lambda (m items) (codec-each-kv! items (lambda (k v) (tmr-put! (cs-root m) k v)))))
(register-image-codec! "java.util.concurrent.ConcurrentSkipListSet" '("jx-csls")
  (lambda (s) (vector (tmr-cmp-obj (cs-root s)) (v-keys (cs-base s))))
  (lambda (cmp) (make-cs "jx-csls" (cs-new-root cmp)))
  (lambda (s items) (for-each (lambda (k) (tmr-put! (cs-root s) k #t)) items)))
(register-image-refusal!
  '("jx-reverse-list" "jx-hashtable-keyset" "jx-hashtable-values" "jx-hashtable-entryset" "jx-hashtable-entry"
    "jx-reverse-deque" "jx-cow-sublist" "jx-cslm-sub" "jx-cslm-dsub" "jx-cslm-keyset" "jx-cslm-values"
    "jx-cslm-subvalues" "jx-cslm-entryset" "jx-cslm-subentryset" "jx-cslm-dsubentryset"
    "jx-seq-keyset" "jx-seq-values" "jx-seq-entryset"
    "jx-reverse-linkedlist" "jx-cow-reversed" "jx-rev-sortedmap" "jx-rev-sortedset"
    "jx-hm-keyset" "jx-hm-values" "jx-hm-entryset" "jx-hm-node")
  "it is a view of another collection, which the image would have to share; write the collection itself")
(register-image-refusal!
  '("jx-vector-itr" "jx-vector-enum" "jx-enum-iterator" "jx-reverse-list-itr" "jx-hashtable-enumerator"
    "jx-pq-itr" "jx-clq-itr" "jx-cld-itr" "jx-cld-desc-itr" "jx-cow-itr" "jx-cow-sublist-itr"
    "jx-cslm-entry-itr" "jx-cslm-key-itr" "jx-cslm-value-itr" "jx-cslm-sub-entry-itr" "jx-cslm-sub-key-itr"
    "jx-cslm-dsub-entry-itr" "jx-cslm-dsub-key-itr" "jx-cslm-sub-value-itr"
    "jx-hm-key-itr" "jx-hm-value-itr" "jx-hm-entry-itr")
  "an iterator is a position in a walk this process is making; write the collection instead")

;; ---- what cannot travel -----------------------------------------------------------
(register-image-refusal!
  '("lhm-keyset" "lhm-values" "lhm-entryset" "lhm-reversed" "lhs-reversed" "lhm-entry"
    "enum-map-keyset" "enum-map-values" "enum-map-entryset" "enum-map-entry"
    "treemap-asc-sub" "treemap-desc-sub" "treemap-keyset" "treemap-values"
    "treemap-submap-values" "treemap-entryset" "treemap-asc-entryset" "treemap-desc-entryset"
    "treemap-entry")
  "it is a view of another collection, which the image would have to share; write the collection itself")
(register-image-refusal!
  '("arraylist-iterator" "arraylist-listitr" "linkedlist-iterator" "linkedlist-desc-iterator" "arrays-aslist-iterator"
    "immutable-list-iterator" "hashset-iterator" "lhs-iterator" "lhm-keyset-iterator" "lhm-values-iterator" "lhm-entryset-iterator"
    "enum-set-regular-iterator" "enum-set-jumbo-iterator" "enum-map-keyset-iterator"
    "enum-map-values-iterator" "enum-map-entryset-iterator"
    "treemap-entry-iterator" "treemap-key-iterator" "treemap-value-iterator"
    "treemap-submap-entry-iterator" "treemap-submap-key-iterator"
    "treemap-desc-submap-entry-iterator" "treemap-desc-submap-key-iterator"
    "treemap-submap-value-iterator")
  "an iterator is a position in a walk this process is making; write the collection instead")
(register-image-refusal!
  '("user-thread" "thread-group" "executor-service" "scheduled-executor" "cf-delayed-executor"
    "future-task" "j-future" "scheduled-future" "completable-future" "process" "process-handle")
  "it is execution, not state: a thread, a pool or a task belongs to this process")
(register-image-refusal!
  '("in-stream" "out-stream" "char-reader" "char-writer" "reader-adapter" "pushback-reader"
    "line-numbering-pushback-reader" "string-reader" "print-stream" "print-writer" "port-writer"
    "random-access-file" "file-channel")
  "it is an open stream over a port this process holds; write what it reads or wrote instead")
