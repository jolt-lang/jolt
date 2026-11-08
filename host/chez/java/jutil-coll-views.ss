;; jutil-coll-views.ss — the views and spliterators java.util's collections
;; share: SequencedMap's default views, List.reversed() views, HashMap's
;; keySet / values / entrySet views and their live nodes, a proxy over
;; java.util's abstract collection classes, and java.util.Spliterator with
;; Spliterators / StreamSupport.
;;
;; Split from jutil-concurrent.ss (#1517). Everything here acts on collections
;; the boot already has — HashMap, ConcurrentHashMap, Properties, ArrayList, the
;; persistent collections — so it loads with the boot. jutil-concurrent.ss keeps
;; the classes themselves (Vector, Stack, Hashtable, BitSet, PriorityQueue and
;; the java.util.concurrent collections), which load on first reference
;; (lazy-shims.ss). Registrations below that name one of those classes' tags
;; (a spliterator for jx-vector, say) only add members to its table ahead of it.
;;
;; Loaded after jutil-views.ss and tree-map.ss, before jutil-concurrent.ss.

;; the JDK arities of the Collection / Deque members the delegating views below
;; answer through one variadic procedure each

(define jxc-arities
  '(("iterator" 0) ("size" 0) ("isEmpty" 0) ("contains" 1) ("containsAll" 1) ("remove" 1) ("removeAll" 1)
    ("removeIf" 1) ("retainAll" 1) ("clear" 0) ("toArray" 0 1) ("forEach" 1) ("toString" 0) ("add" 1)
    ("addAll" 1) ("addFirst" 1) ("addLast" 1) ("offerFirst" 1) ("offerLast" 1) ("pollFirst" 0) ("pollLast" 0)
    ("peekFirst" 0) ("peekLast" 0) ("getFirst" 0) ("getLast" 0) ("removeFirst" 0) ("removeLast" 0)
    ("removeFirstOccurrence" 1) ("removeLastOccurrence" 1) ("push" 1) ("pop" 0) ("poll" 0) ("peek" 0)
    ("element" 0) ("offer" 1) ("descendingIterator" 0)))
(define (jxc-arity-of name f)
  (let ((a (assoc name jxc-arities)))
    (unless a (error 'jxc-arity-of "no arity for" name))
    (host-arity-of (cdr a) #t f)))
(define (jxc-npe-if-nil! . xs) (for-each (lambda (x) (when (jolt-nil? x) (jx-npe))) xs))
(define (jxc-elems c) (seq->list (jolt-seq c)))
(define (jxc-to-array elems)
  (case-lambda ((self) (cd-to-array (elems self) '())) ((self a) (cd-to-array (elems self) (list a)))))

;; a Vector / CopyOnWriteArrayList's reversed() view, state #(list)
(define (rev-list r) (vector-ref (jhost-state r) 0))
(define (rev-n r) (cd-size (rev-list r)))
(define (rev-elems r) (reverse (jxc-elems (rev-list r))))
(define (jx-isa? x cls) (let ((c (guard (e (#t #f)) (jolt-class-name x)))) (and (string? c) (jch-isa? c cls))))

;; ======================================================================================
;; SequencedMap's default views
;; ======================================================================================
;; sequencedKeySet / sequencedValues / sequencedEntrySet on a map that does not
;; override them (TreeMap, ConcurrentSkipListMap): SequencedMap's own views,
;; each the map's keySet / values / entrySet behind AbstractMap.ViewCollection,
;; whose reversed is the reversed map's. State #(map).
(define (sqv-map v) (vector-ref (jhost-state v) 0))
(define (sqv-view v) (jx-call (sqv-map v) (case (string->symbol (jhost-tag v))
                                            ((jx-seq-keyset) "keySet") ((jx-seq-values) "values") (else "entrySet"))
                              '()))
(define (sqv-methods reversed-method identity?)
  (append
    (map (lambda (m) (cons m (jxc-arity-of m (lambda (self . args) (jx-call (sqv-view self) m args)))))
         '("iterator" "size" "isEmpty" "contains" "containsAll" "remove" "removeAll" "removeIf" "retainAll"
           "clear" "toArray" "forEach" "toString"))
    (if identity? '()
        (list (cons "equals" (lambda (self o) (jx-call (sqv-view self) "equals" (list o))))
              (cons "hashCode" (lambda (self) (jx-call (sqv-view self) "hashCode" '())))))
    (list (cons "add" (lambda (self x) (jx-uoe))) (cons "addAll" (lambda (self c) (jx-uoe)))
          (cons "addFirst" (lambda (self x) (jx-uoe))) (cons "addLast" (lambda (self x) (jx-uoe)))
          (cons "reversed" (lambda (self) (jx-call (jx-call (sqv-map self) "reversed" '()) reversed-method '())))
          (cons "getFirst" (lambda (self) (let ((xs (jxc-elems (sqv-view self)))) (if (null? xs) (throw-jvm 'NoSuchElementException jolt-nil) (car xs)))))
          (cons "getLast" (lambda (self) (let ((xs (jxc-elems (sqv-view self)))) (if (null? xs) (throw-jvm 'NoSuchElementException jolt-nil) (list-ref xs (fx- (length xs) 1))))))
          (cons "removeFirst" (lambda (self) (let ((it (jx-call (sqv-view self) "iterator" '())))
                                               (unless (jolt-truthy? (jx-call it "hasNext" '())) (throw-jvm 'NoSuchElementException jolt-nil))
                                               (let ((x (jx-call it "next" '()))) (jx-call it "remove" '()) x))))
          (cons "removeLast" (lambda (self) (let ((it (jx-call (jx-call (sqv-map self) "reversed" '()) reversed-method '())))
                                              (jx-call it "removeFirst" '())))))))
(for-each (lambda (r)
            (jx-class! (car r) (cadr r) (caddr r))
            (register-host-methods! (car r) (sqv-methods (cadddr r) (eq? (list-ref r 4) 'identity))))
          '(("jx-seq-keyset" "java.util.SequencedMap$1SeqKeySet" ("java.util.AbstractMap$ViewCollection" "java.util.SequencedSet") "sequencedKeySet" eq)
            ("jx-seq-values" "java.util.SequencedMap$1SeqValues" ("java.util.AbstractMap$ViewCollection" "java.util.SequencedCollection") "sequencedValues" identity)
            ("jx-seq-entryset" "java.util.SequencedMap$1SeqEntrySet" ("java.util.AbstractMap$ViewCollection" "java.util.SequencedSet") "sequencedEntrySet" eq)))
(jch-register-supers! "java.util.AbstractMap$ViewCollection" '("java.util.Collection"))
(jch-register-supers! "java.util.SequencedSet" '("java.util.SequencedCollection" "java.util.Set"))
(define (sqv-elems v) (jxc-elems (sqv-view v)))
(define (sqv-count v) (jolt-count (sqv-view v)))
(jx-register! "jx-seq-keyset" 'set sqv-elems sqv-count (lambda (v x) (jolt-contains? (sqv-view v) x)) #f)
(jx-register! "jx-seq-values" 'coll sqv-elems sqv-count #f #f)
(jx-register! "jx-seq-entryset" 'entries sqv-elems sqv-count (lambda (v x) (jolt-truthy? (jx-call (sqv-view v) "contains" (list x)))) #f)
(define seq-view-methods
  (list (cons "sequencedKeySet" (lambda (self) (make-jhost "jx-seq-keyset" (vector self))))
        (cons "sequencedValues" (lambda (self) (make-jhost "jx-seq-values" (vector self))))
        (cons "sequencedEntrySet" (lambda (self) (make-jhost "jx-seq-entryset" (vector self))))))
;; ConcurrentSkipListMap's three map tags take the same views, registered at the
;; end of jutil-concurrent.ss: they override the class's own there, after it.
(for-each (lambda (t) (register-host-methods! t seq-view-methods)) tm-map-tags)

;; ======================================================================================
;; reversed() views
;; ======================================================================================
;; List.reversed(): ReverseOrderListView, a live view through the list's own get /
;; set / add / remove at mirrored indexes. State #(list). An ArrayList's, an
;; unmodifiable list's (jutil-views.ss) and a Vector's all answer through it.
(jx-class! "jx-reverse-list" "java.util.ReverseOrderListView$Rand" '("java.util.ReverseOrderListView" "java.util.RandomAccess"))
(jch-register-supers! "java.util.ReverseOrderListView" '("java.util.List"))
(register-host-methods! "jx-reverse-list"
  (list (cons "size" (lambda (self) (->num (rev-n self))))
        (cons "get" (lambda (self i) (let ((i (jnum->exact i))) (cd-index-check! i (rev-n self))
                                       (cd-get (rev-list self) (fx- (fx- (rev-n self) 1) i)))))
        (cons "set" (lambda (self i x) (let ((i (jnum->exact i))) (cd-index-check! i (rev-n self))
                                         (cd-call (rev-list self) "set" (->num (fx- (fx- (rev-n self) 1) i)) x))))
        (cons "add" (case-lambda
                      ((self x) (cd-call (rev-list self) "add" (->num 0) x) #t)
                      ((self i x) (cd-call (rev-list self) "add" (->num (fx- (rev-n self) (jnum->exact i))) x) jolt-nil)))
        (cons "remove" (lambda (self i)
                         (if (and (fixnum? i))
                             (begin (cd-index-check! i (rev-n self))
                                    (cd-call (rev-list self) "remove" (->num (fx- (fx- (rev-n self) 1) i))))
                             (let ((k (cd-last-index-of (rev-list self) i)))
                               (if (fx<? k 0) #f (begin (cd-call (rev-list self) "remove" (->num k)) #t))))))
        (cons "iterator" (lambda (self) (make-jutil-iterator "jx-reverse-list-itr" (rev-elems self) (lambda (x) (jx-uoe)))))
        (cons "reversed" (lambda (self) (rev-list self)))))
(jx-class! "jx-reverse-list-itr" "java.util.ReverseOrderListView$DescendingIterator" '("java.util.Iterator"))
(register-jutil-iterator! "jx-reverse-list-itr")
(jx-register! "jx-reverse-list" 'ralist rev-elems rev-n #f #f)
;; Deque.reversed(): ReverseOrderDequeView, each end swapped for the other.
(define (rvd-d r) (vector-ref (jhost-state r) 0))
(define rvd-swaps
  '(("addFirst" . "addLast") ("addLast" . "addFirst") ("offerFirst" . "offerLast") ("offerLast" . "offerFirst")
    ("pollFirst" . "pollLast") ("pollLast" . "pollFirst") ("peekFirst" . "peekLast") ("peekLast" . "peekFirst")
    ("getFirst" . "getLast") ("getLast" . "getFirst") ("removeFirst" . "removeLast") ("removeLast" . "removeFirst")
    ("removeFirstOccurrence" . "removeLastOccurrence") ("removeLastOccurrence" . "removeFirstOccurrence")
    ("push" . "addLast") ("pop" . "removeLast") ("poll" . "pollLast") ("peek" . "peekLast") ("element" . "getLast")
    ("offer" . "offerFirst") ("add" . "addFirst") ("iterator" . "descendingIterator") ("descendingIterator" . "iterator")))
(jx-class! "jx-reverse-deque" "java.util.ReverseOrderDequeView" '("java.util.Deque"))
(register-host-methods! "jx-reverse-deque"
  (append
    (map (lambda (s)
           (cons (car s) (jxc-arity-of (car s)
                           (lambda (self . args)
                             (let ((r (jx-call (rvd-d self) (cdr s) args)))
                               (if (string=? (car s) "add") #t r))))))
         rvd-swaps)
    (list (cons "size" (lambda (self) (jx-call (rvd-d self) "size" '())))
          (cons "isEmpty" (lambda (self) (jx-call (rvd-d self) "isEmpty" '())))
          (cons "contains" (lambda (self x) (jx-call (rvd-d self) "contains" (list x))))
          (cons "remove" (case-lambda ((self) (jx-call (rvd-d self) "removeLast" '()))
                                      ((self x) (jx-call (rvd-d self) "removeLastOccurrence" (list x)))))
          (cons "reversed" (lambda (self) (rvd-d self))))))
(jx-register! "jx-reverse-deque" 'coll (lambda (r) (reverse (jxc-elems (rvd-d r)))) (lambda (r) (jolt-count (rvd-d r))) #f #f)
;; List.reversed is ReverseOrderListView over the list (jx-reverse-list above),
;; except where a class overrides it: LinkedList's own view, CopyOnWriteArrayList's
;; Reversed. Deque.reversed is ReverseOrderDequeView. Each reads and writes the
;; list it reverses, so an unmodifiable or immutable list refuses the writes.
(for-each (lambda (r)
            (jx-class! (car r) (cadr r) (caddr r))
            (register-host-methods! (car r) (host-method-entries "jx-reverse-list"))
            (jx-register! (car r) (cadddr r) rev-elems rev-n #f #f))
          '(("jx-reverse-linkedlist" "java.util.LinkedList$ReverseOrderLinkedListView" ("java.util.LinkedList") list)
            ("jx-cow-reversed" "java.util.concurrent.CopyOnWriteArrayList$Reversed" ("java.util.List" "java.util.RandomAccess") ralist)))
(define (jx-reversed-by tag) (list (cons "reversed" (lambda (self) (make-jhost tag (vector self))))))
(for-each (lambda (t) (register-host-methods! t (jx-reversed-by "jx-reverse-list")))
          '("arraylist" "arrays-aslist" "immutable-list" "jx-unmod-list" "jx-unmod-ralist" "jx-sync-list" "jx-sync-ralist"))
(register-host-methods! "linkedlist" (jx-reversed-by "jx-reverse-linkedlist"))
(register-host-methods! "arraydeque" (jx-reversed-by "jx-reverse-deque"))
;; an unmodifiable navigable map or set reverses to its unmodifiable descending
;; view; a merely sorted one to ReverseOrderSortedMapView / SetView, here the
;; unmodifiable view of the backing descending map or set under that class
(register-host-methods! "jx-unmod-navmap"
  (list (cons "reversed" (lambda (self) (jx-wrap-unmod 'navmap (jx-call (jxw-inner self) "descendingMap" '()))))))
(register-host-methods! "jx-unmod-navset"
  (list (cons "reversed" (lambda (self) (jx-wrap-unmod 'navset (jx-call (jxw-inner self) "descendingSet" '()))))))
(for-each (lambda (r)
            (let ((tag (car r)) (from (cadr r)))
              (jx-class! tag (caddr r) (list (cadddr r)))
              (register-host-methods! tag (host-method-entries from))
              (register-host-methods! tag
                (list (cons "reversed" (lambda (self) (make-jhost from (vector (jx-call (jxw-inner self) (list-ref r 4) '())))))))
              (jx-register! tag (list-ref r 5) (if (eq? (list-ref r 5) 'map) jxw-entry-elems jxw-elems) jxw-count jxw-contains
                            (and (eq? (list-ref r 5) 'map) jxw-get))
              (register-host-methods! from
                (list (cons "reversed" (lambda (self) (make-jhost tag (vector (jx-call (jxw-inner self) (list-ref r 4) '())))))))))
          '(("jx-rev-sortedmap" "jx-unmod-sortedmap" "java.util.ReverseOrderSortedMapView" "java.util.SortedMap" "descendingMap" map)
            ("jx-rev-sortedset" "jx-unmod-sortedset" "java.util.ReverseOrderSortedSetView" "java.util.SortedSet" "descendingSet" set)))

;; ======================================================================================
;; HashMap's views
;; ======================================================================================
;; keySet / values / entrySet of a HashMap: live views of the map, as the JDK's
;; HashMap$KeySet / $Values / $EntrySet are. Removing through one removes from
;; the map, a later put shows through, and an entry is a HashMap$Node whose
;; getValue reads the map and whose setValue writes it. Their iterators are
;; fail-fast: the map replaces its key-order list on every structural change
;; (host-static-classes.ss hm-note-key! / hm-drop-key!), so an iterator holds the
;; list it started from and throws ConcurrentModificationException when the
;; map's is another. State #(map); an iterator's #(map remaining last kind expected).
(define (hmv-map v) (vector-ref (jhost-state v) 0))
(define (hmv-ref m k) (hashtable-ref (hm-tbl m) k jolt-nil))
;; A node is the map's entry until its key is removed (or the map cleared):
;; then it is detached, keeping the value it last saw even if the key comes
;; back. Each map that has handed out nodes keeps a generation per removed key
;; and one for clears, which a node compares with the ones it was made under.
;; State #(map key last generation map-generations).
(define (hmn-gen* g k)
  ;; most maps remove nothing between handing out nodes: skip the key probe then
  (let ((kt (vector-ref g 1)))
    (if (fx=? 0 (hashtable-size kt)) (vector-ref g 0) (+ (vector-ref g 0) (hashtable-ref kt k 0)))))
;; A map's generations #(clear-gen key-table) live in its own state, slot 3
;; (host-static-classes.ss hm-state; a ConcurrentHashMap's reservations and a
;; Properties' defaults sit there too, never a vector). They are written as the
;; map itself is, so a map one thread owns pays no lock to remove a key.
(define (hmn-gens-of m) (let ((g (vector-ref (jhost-state m) 3))) (and (vector? g) g)))
(define (hmn-track! m)
  ;; made on the map's first node, keyed as the map is: by identity in an
  ;; IdentityHashMap, weakly in a WeakHashMap (a removed key must not be kept here)
  (or (hmn-gens-of m)
      (let ((g (vector 0 (hm-like-table m)))) (vector-set! (jhost-state m) 3 g) g)))
(set! hm-detach-hook
  (lambda (m k)
    (let ((g (hmn-gens-of m)))
      (when g
        (if (eq? k hm-detach-hook)
            (vector-set! g 0 (+ 1 (vector-ref g 0)))
            (hashtable-update! (vector-ref g 1) k (lambda (n) (+ n 1)) 0))))))
;; HashMap, WeakHashMap and IdentityHashMap share these views, iterators and
;; entries; each family's carry its own JDK class names, by the map's tag.
(define hm-families
  '(("jx-hm" "java.util.HashMap" "java.util.HashMap$Node")
    ("jx-whm" "java.util.WeakHashMap" "java.util.WeakHashMap$Entry")
    ("jx-ihm" "java.util.IdentityHashMap" "java.util.IdentityHashMap$EntryIterator$Entry")))
(define (hm-node-tag m)
  (let ((t (jhost-tag m)))
    (cond ((string=? t "jx-weak-hashmap") "jx-whm-node") ((string=? t "jx-identity-hashmap") "jx-ihm-node") (else "jx-hm-node"))))
(define (hm-node* m k g) (make-jhost (hm-node-tag m) (vector m k (hmv-ref m k) (hmn-gen* g k) g)))
(define (hm-node m k) (hm-node* m k (hmn-track! m)))
;; every entry of m in its order, as its entrySet's iterator hands them out: the
;; seq of a HashMap (host-static-classes.ss hm-seq-entries), which skips making
;; the view and picks the node class once
(define (hm-node-list m)
  (let ((g (hmn-track! m)) (tag (hm-node-tag m)) (t (hm-tbl m)))
    (hm-keys-ordered-map m (lambda (k) (make-jhost tag (vector m k (hashtable-ref t k jolt-nil) (hmn-gen* g k) g))))))
(define hmn-absent (list 'absent))
(define (hmn-attached? st) (= (vector-ref st 3) (hmn-gen* (vector-ref st 4) (vector-ref st 1))))
(define (hmn-value n)
  ;; the map's value while the node is attached, else the last one seen
  (let ((st (jhost-state n)))
    (if (hmn-attached? st)
        (let ((v (hashtable-ref (hm-tbl (vector-ref st 0)) (vector-ref st 1) hmn-absent)))
          (if (eq? v hmn-absent) (vector-ref st 2) (begin (vector-set! st 2 v) v)))
        (vector-ref st 2))))
(define hm-node-methods
  (list (cons "setValue" (lambda (self v)
                           (let* ((st (jhost-state self)) (m (vector-ref st 0)) (k (vector-ref st 1)) (old (hmn-value self)))
                             (when (and (hmn-attached? st) (hashtable-contains? (hm-tbl m) k)) (hashtable-set! (hm-tbl m) k v))
                             (vector-set! st 2 v)
                             old)))))
(for-each (lambda (f)
            (let ((tag (string-append (car f) "-node")))
              (jx-class! tag (caddr f) '("java.util.Map$Entry"))
              (register-jutil-entry! tag (lambda (n) (cons (vector-ref (jhost-state n) 1) (hmn-value n))))
              (register-host-methods! tag hm-node-methods)))
          hm-families)
(define (hmv-project kind m k) (case kind ((key) k) ((value) (hmv-ref m k)) (else (hm-node m k))))
(define (hmv-elems kind)
  ;; one pass over the key order, which the map keeps newest first, the
  ;; projection chosen once
  (case kind
    ((key) (lambda (v) (hm-keys-ordered (hmv-map v))))
    ((value) (lambda (v) (let ((t (hm-tbl (hmv-map v))))
                           (hm-keys-ordered-map (hmv-map v) (lambda (k) (hashtable-ref t k jolt-nil))))))
    (else (lambda (v) (let* ((m (hmv-map v)) (g (hmn-track! m)))
                        (hm-keys-ordered-map m (lambda (k) (hm-node* m k g))))))))
(define (hmv-entry-kv e)
  (or (jolt-host-entry e) (and (jolt-map-entry? e) (cons (jolt-nth e 0) (jolt-nth e 1)))))
(define (hmv-has-entry? m e)
  (let ((kv (hmv-entry-kv e)))
    (and kv (hashtable-contains? (hm-tbl m) (car kv)) (hm-veq? m (cdr kv) (hmv-ref m (car kv))) #t)))
(define (hmv-remove-key! m k) (jx-call m "remove" (list k)))
;; HashMap$HashIterator: the key order is the walk, slot 4 the modCount it
;; expects; next checks it before the end, remove after the IllegalStateException
(define (hmv-iterator kind tag)
  (lambda (self) (let ((m (hmv-map self))) (make-jhost tag (vector m (hm-keys-ordered m) jutil-iterator-none kind (hm-mc m))))))
(define (hmi-comod! it)
  (let ((st (jhost-state it)))
    (unless (fx=? (vector-ref st 4) (hm-mc (vector-ref st 0))) (jutil-cme!))))
(define hm-iterator-methods
  (list (cons "hasNext" (lambda (self) (pair? (vector-ref (jhost-state self) 1))))
        (cons "next" (lambda (self)
                       (hmi-comod! self)
                       (let* ((st (jhost-state self)) (ks (vector-ref st 1)))
                         (when (null? ks) (throw-jvm 'NoSuchElementException jolt-nil))
                         (vector-set! st 1 (cdr ks))
                         (vector-set! st 2 (car ks))
                         (hmv-project (vector-ref st 3) (vector-ref st 0) (car ks)))))
        (cons "remove" (lambda (self)
                         (let ((st (jhost-state self)))
                           (when (eq? (vector-ref st 2) jutil-iterator-none) (throw-jvm 'IllegalStateException jolt-nil))
                           (hmi-comod! self)
                           (hmv-remove-key! (vector-ref st 0) (vector-ref st 2))
                           (vector-set! st 2 jutil-iterator-none)
                           (vector-set! st 4 (hm-mc (vector-ref st 0)))
                           jolt-nil)))
        (cons "forEachRemaining" (lambda (self f)
                                   (let loop () (when (pair? (vector-ref (jhost-state self) 1))
                                                  (jolt-fi-call f "accept" (jx-call self "next" '())) (loop)))
                                   jolt-nil))))
(for-each (lambda (r)
            (jx-class! (car r) (cadr r) '("java.util.Iterator"))
            (register-host-methods! (car r) hm-iterator-methods)
            (jx-register! (car r) 'iter
                          (lambda (it) (let ((st (jhost-state it)))
                                         (if (eq? (vector-ref st 3) 'entry)
                                             (let* ((m (vector-ref st 0)) (g (hmn-track! m)))
                                               (map (lambda (k) (hm-node* m k g)) (vector-ref st 1)))
                                             (map (lambda (k) (hmv-project (vector-ref st 3) (vector-ref st 0) k)) (vector-ref st 1)))))
                          #f #f #f))
          (apply append
                 (map (lambda (f)
                        (map (lambda (k) (list (string-append (car f) "-" (car k) "-itr") (string-append (cadr f) "$" (cadr k) "Iterator")))
                             '(("key" "Key") ("value" "Value") ("entry" "Entry"))))
                      hm-families)))
(define (hmv-common kind itr-tag has? remove!)
  (list (cons "size" (lambda (self) (hashtable-size (hm-tbl (hmv-map self)))))
        (cons "isEmpty" (lambda (self) (fx=? 0 (hashtable-size (hm-tbl (hmv-map self))))))
        (cons "contains" (lambda (self x) (and (has? (hmv-map self) x) #t)))
        (cons "remove" (lambda (self x) (remove! (hmv-map self) x)))
        (cons "clear" (lambda (self) (jx-call (hmv-map self) "clear" '())))
        (cons "iterator" (hmv-iterator kind itr-tag))
        (cons "add" (lambda (self x) (jx-uoe)))
        (cons "addAll" (lambda (self c) (jx-uoe)))
        (cons "toArray" (jxc-to-array (hmv-elems kind)))
        ;; HashMap's view forEach walks the table and checks for a structural
        ;; change once the walk ends; WeakHashMap's and IdentityHashMap's keep
        ;; Iterable's, over the fail-fast iterator
        (cons "forEach" (lambda (self f)
                          (let ((m (hmv-map self)))
                            (if (eq? (hm-ff-kind m) 'end)
                                (hm-walk m (lambda (k v) (jolt-fi-call f "accept" (hmv-project kind m k))))
                                (jutil-walk-for-each (jx-call self "iterator" '()) f)))))))
(define (hmv-has-value? m v) (exists (lambda (kv) (hm-veq? m v (cdr kv))) (hm-pairs-ordered m)))
(for-each
  (lambda (f)
    (let* ((p (car f)) (owner (cadr f)) (t (lambda (sfx) (string-append p sfx))))
      (jx-class! (t "-keyset") (string-append owner "$KeySet") '("java.util.AbstractSet"))
      (register-host-methods! (t "-keyset")
        (hmv-common 'key (t "-key-itr")
                    (lambda (m k) (hashtable-contains? (hm-tbl m) k))
                    (lambda (m k) (and (hashtable-contains? (hm-tbl m) k) (begin (hmv-remove-key! m k) #t)))))
      (jx-register! (t "-keyset") 'set (hmv-elems 'key) (lambda (v) (hashtable-size (hm-tbl (hmv-map v))))
                    (lambda (v k) (hashtable-contains? (hm-tbl (hmv-map v)) k)) #f)
      (jx-class! (t "-values") (string-append owner "$Values") '("java.util.AbstractCollection"))
      (register-host-methods! (t "-values")
        (hmv-common 'value (t "-value-itr") hmv-has-value?
                    (lambda (m v) (let ((k (find (lambda (k) (hm-veq? m v (hmv-ref m k))) (hm-keys-ordered m))))
                                    (if k (begin (hmv-remove-key! m k) #t) #f)))))
      (jx-register! (t "-values") 'coll (hmv-elems 'value) (lambda (v) (hashtable-size (hm-tbl (hmv-map v)))) #f #f)
      (jx-class! (t "-entryset") (string-append owner "$EntrySet") '("java.util.AbstractSet"))
      (register-host-methods! (t "-entryset")
        (hmv-common 'entry (t "-entry-itr") hmv-has-entry?
                    (lambda (m e) (if (hmv-has-entry? m e) (begin (hmv-remove-key! m (car (hmv-entry-kv e))) #t) #f))))
      (jx-register! (t "-entryset") 'entries (hmv-elems 'entry) (lambda (v) (hashtable-size (hm-tbl (hmv-map v))))
                    (lambda (v e) (hmv-has-entry? (hmv-map v) e)) #f)
      ;; a view's modCount is its map's
      (for-each (lambda (sfx) (register-modcount! (t sfx) (lambda (v) (hm-mc (hmv-map v)))))
                '("-keyset" "-values" "-entryset"))))
  hm-families)
;; (seq (.values m)) and the rest: claimed ahead of the general arms, which the
;; jx arm (jutil-views.ss) sits behind — a walk past all of them cost a small
;; map's view seq three times its own work. A miss costs a tag-prefix test
;; and, for a jx- tag only, one table read. Like the map's own seq, it walks the
;; view's fail-fast iterator 32 elements at a time.
;;
;; The views' tags are the string literals of the methods that make them, so
;; the tag OBJECTS seen are few: each is remembered by identity the first time
;; the table names it, and after that the arm answers with an assq rather than
;; a string hash. Bounded, since a tag read back from an image is a fresh string.
(define hmv-seq-elems (make-hashtable string-hash string=?))
(for-each (lambda (f)
            (for-each (lambda (k) (hashtable-set! hmv-seq-elems (string-append (car f) (car k)) (hmv-elems (cdr k))))
                      '(("-keyset" . key) ("-values" . value) ("-entryset" . entry))))
          hm-families)
(define hmv-seq-seen '())               ; ((tag-object . elems) ...), written under the mutex
(define hmv-seq-mu (make-mutex))
(define (hmv-view-elems x)
  (and (jhost? x)
       (let ((t (jhost-tag x)))
         (and (jx-tag? t)
              (let ((hit (assq t hmv-seq-seen)))
                (if hit
                    (cdr hit)
                    (let ((e (hashtable-ref hmv-seq-elems t #f)))
                      (when (and e (fx<? (length hmv-seq-seen) 16))
                        (jolt-with-mutex hmv-seq-mu (set! hmv-seq-seen (cons (cons t e) hmv-seq-seen))))
                      e)))))))
(define (hmv-view-mc v) (hm-mc (hmv-map v)))
(register-seq-arm! hmv-view-elems (lambda (v) (jutil-ff-seq/mc v ((hmv-view-elems v) v) hmv-view-mc)))
;; IdentityHashMap compares by reference wherever the JDK's uses ==: replace's
;; old value, and equals, which holds a map whose mappings are the same objects
;; (an IdentityHashMap's, or any map's entries matched by identity on both key
;; and value); hashCode sums identityHashCode(key) ^ identityHashCode(value).
;; Clojure's = asks the left operand, so a persistent or HashMap on the left
;; compares as they do, through the identity map's own get.
(define-jhost-tag-pred (ihm? t) (string=? t "jx-identity-hashmap"))
(set! jutil-identity-map? ihm?)
(define (ihm-contains-mapping? m k v)
  (let ((t (hm-tbl m))) (and (hashtable-contains? t k) (eq? v (hashtable-ref t k jolt-nil)))))
(define (map-entries-of x)
  ;; (k . v) pairs of any map value
  (if (hm-hashmap? x)
      (hm-pairs-ordered x)
      (map (lambda (e) (cons (jolt-nth e 0) (jolt-nth e 1))) (seq->list (jolt-seq x)))))
(define (map-value? x) (or (hm-hashmap? x) (jolt-map? x) (eq? (jutil-coll-kind x) 'map)))
(define (ihm-equals? m o)
  (or (eq? m o)
      (and (map-value? o)
           (let ((es (map-entries-of o)))
             (and (fx=? (length es) (hashtable-size (hm-tbl m)))
                  (for-all (lambda (e) (ihm-contains-mapping? m (car e) (cdr e))) es))))))
(define (ihm-hash m)
  (fold-left (lambda (h k)
               (+ h (bitwise-xor (if (jolt-nil? k) 0 (jolt-identity-hasheq k))
                                 (let ((v (hashtable-ref (hm-tbl m) k jolt-nil))) (if (jolt-nil? v) 0 (jolt-identity-hasheq v))))))
             0 (hm-keys-ordered m)))
(register-host-methods! "jx-identity-hashmap"
  (list (cons "replace" (case-lambda
                          ((self k v) ((cdr (assoc "replace" hashmap-methods)) self k v))
                          ((self k old v) (if (ihm-contains-mapping? self k old) (begin (hashtable-set! (hm-tbl self) k v) #t) #f))))
        (cons "equals" ihm-equals?)
        (cons "hashCode" (lambda (self) (->num (int32-low (ihm-hash self)))))))
(register-eq-arm! (lambda (a b) (or (ihm? a) (ihm? b)))
  (lambda (a b)
    (cond ((ihm? a) (ihm-equals? a b))
          ;; another map on the left: its entries, looked up in the identity map
          ((map-value? a)
           (let ((es (map-entries-of a)) (t (hm-tbl b)))
             (and (fx=? (length es) (hashtable-size t))
                  (for-all (lambda (e) (and (hashtable-contains? t (car e)) (jolt=2 (cdr e) (hashtable-ref t (car e) jolt-nil)))) es))))
          (else #f))))
(register-hash-arm! ihm? (lambda (m) (int32-low (ihm-hash m))))
;; the weak and identity maps' own views
(for-each (lambda (r)
            (register-host-methods! (car r)
              (let ((ks (string-append (cadr r) "-keyset")) (vs (string-append (cadr r) "-values"))
                    (es (string-append (cadr r) "-entryset")))
                (list (cons "keySet" (lambda (self) (jutil-view self 0 (lambda () (make-jhost ks (vector self))))))
                      (cons "values" (lambda (self) (jutil-view self 1 (lambda () (make-jhost vs (vector self))))))
                      (cons "entrySet" (lambda (self) (jutil-view self 2 (lambda () (make-jhost es (vector self))))))))))
          '(("jx-weak-hashmap" "jx-whm") ("jx-identity-hashmap" "jx-ihm")))
(register-image-refusal! (apply append (map (lambda (f) (map (lambda (sfx) (string-append (car f) sfx))
                                                             '("-keyset" "-values" "-entryset" "-node" "-key-itr" "-value-itr" "-entry-itr")))
                                            (cdr hm-families)))
                         "a view, an entry or an iterator is a window onto a map this process holds; write the map instead")
;; ConcurrentHashMap's views, and Properties' (a Properties is a ConcurrentHashMap
;; inside since JDK 9, its views that map's wrapped in Collections.synchronized*).
;; The same live views, but WEAKLY CONSISTENT, as the JDK's: an iterator never
;; throws ConcurrentModificationException; it walks the keys present when it
;; started, skipping any removed since. keySet(default) is a view whose add puts
;; the key mapped to that default. State #(map default); an iterator's
;; #(map remaining last kind pending).
;;
;; An iterator reads its next entry AHEAD, as the JDK's Traverser does: hasNext
;; finds the next key still in the map and keeps it with the value it read
;; (pending, a (key . value) pair), and next hands that out. So once hasNext has
;; answered true, next cannot find the map emptied under it by another thread
;; and throw NoSuchElementException; a key removed after it was read ahead is
;; still handed out, as the JDK's weak consistency allows.
(define wv-absent (list 'absent))
(define (wv-advance! st)
  ;; -> the pending (key . value), reading ahead past keys removed since the
  ;; walk began, or #f at the end
  (or (vector-ref st 4)
      (let ((t (hm-tbl (vector-ref st 0))))
        (let loop ()
          (let ((ks (vector-ref st 1)))
            (and (pair? ks)
                 (let ((v (hashtable-ref t (car ks) wv-absent)))
                   (vector-set! st 1 (cdr ks))
                   (if (eq? v wv-absent)
                       (loop)
                       (let ((kv (cons (car ks) v))) (vector-set! st 4 kv) kv)))))))))
;; Each walk takes the keys and values together (hm-pairs-ordered, under a
;; ConcurrentHashMap's monitor), so it never pairs a key with the nil a racing
;; remove left.
(define (wv-node m k v) (make-jhost "jx-chm-entry" (vector m k v)))
(define (wv-project kind m k v) (case kind ((key) k) ((value) v) (else (wv-node m k v))))
(define (wv-elems kind)
  (case kind
    ((entry) (lambda (v) (let ((m (hmv-map v))) (map (lambda (kv) (wv-node m (car kv) (cdr kv))) (hm-pairs-ordered m)))))
    (else (hmv-elems kind))))
(define wv-iterator-methods
  (list (cons "hasNext" (lambda (self) (if (wv-advance! (jhost-state self)) #t #f)))
        (cons "next" (lambda (self)
                       (let* ((st (jhost-state self)) (kv (wv-advance! st)))
                         (unless kv (throw-jvm 'NoSuchElementException jolt-nil))
                         (vector-set! st 4 #f)
                         (vector-set! st 2 (car kv))
                         (wv-project (vector-ref st 3) (vector-ref st 0) (car kv) (cdr kv)))))
        (cons "remove" (lambda (self)
                         (let ((st (jhost-state self)))
                           (when (eq? (vector-ref st 2) jutil-iterator-none) (throw-jvm 'IllegalStateException jolt-nil))
                           (hmv-remove-key! (vector-ref st 0) (vector-ref st 2))
                           (vector-set! st 2 jutil-iterator-none)
                           jolt-nil)))
        (cons "forEachRemaining" (lambda (self f)
                                   (let loop () (when (jx-call self "hasNext" '())
                                                  (jolt-fi-call f "accept" (jx-call self "next" '())) (loop)))
                                   jolt-nil))))
(for-each (lambda (r)
            (jx-class! (car r) (cadr r) '("java.util.Iterator"))
            (register-host-methods! (car r) wv-iterator-methods)
            (jx-register! (car r) 'iter
                          ;; what is left: the pending entry, then the rest as they stand now
                          (lambda (it) (let* ((st (jhost-state it)) (t (hm-tbl (vector-ref st 0)))
                                              (pending (vector-ref st 4)))
                                         (define (project kv) (wv-project (vector-ref st 3) (vector-ref st 0) (car kv) (cdr kv)))
                                         (let ((rest (fold-right (lambda (k acc)
                                                                   (let ((v (hashtable-ref t k wv-absent)))
                                                                     (if (eq? v wv-absent) acc (cons (cons k v) acc))))
                                                                 '() (vector-ref st 1))))
                                           (map project (if pending (cons pending rest) rest)))))
                          #f #f #f))
          '(("jx-chm-key-itr" "java.util.concurrent.ConcurrentHashMap$KeyIterator")
            ("jx-chm-value-itr" "java.util.concurrent.ConcurrentHashMap$ValueIterator")
            ("jx-chm-entry-itr" "java.util.concurrent.ConcurrentHashMap$EntryIterator")))
(jx-class! "jx-chm-entry" "java.util.concurrent.ConcurrentHashMap$MapEntry" '("java.util.Map$Entry"))
;; a ConcurrentHashMap$MapEntry holds the value it was read with (setValue
;; writes through to the map and to the entry)
(register-jutil-entry! "jx-chm-entry" (lambda (n) (cons (vector-ref (jhost-state n) 1) (vector-ref (jhost-state n) 2))))
(register-host-methods! "jx-chm-entry"
  (list (cons "setValue" (lambda (self v)
                           (when (jolt-nil? v) (throw-jvm 'NullPointerException jolt-nil))
                           (let* ((st (jhost-state self)) (m (vector-ref st 0)) (k (vector-ref st 1)) (old (vector-ref st 2)))
                             (jx-call m "put" (list k v))
                             (vector-set! st 2 v)
                             old)))))
(define (wv-iterator kind tag)
  (lambda (self) (let ((m (hmv-map self))) (make-jhost tag (vector m (hm-keys-ordered m) jutil-iterator-none kind #f)))))
(define (wv-common kind itr-tag has? remove!)
  ;; hmv-common's view methods over a weakly consistent iterator (later wins)
  (append (hmv-common kind itr-tag has? remove!)
          (list (cons "iterator" (wv-iterator kind itr-tag))
                (cons "toArray" (jxc-to-array (wv-elems kind)))
                (cons "forEach" (lambda (self f) (for-each (lambda (x) (jolt-fi-call f "accept" x)) ((wv-elems kind) self)) jolt-nil)))))
(define (wv-keyset-add self k)
  (let ((d (vector-ref (jhost-state self) 1)))
    (if (jolt-nil? d) (jx-uoe)
        (let ((m (hmv-map self)))
          (jolt-nil? (jx-call m "putIfAbsent" (list k d)))))))
(define wv-key-has (lambda (m k) (hashtable-contains? (hm-tbl m) k)))
;; each removal is one call of the map's, so a racing write cannot slip between
;; finding the mapping and removing it
(define wv-key-remove (lambda (m k) (not (jolt-nil? (hmv-remove-key! m k)))))
(define wv-value-remove (lambda (m v) (let loop ((kvs (hm-pairs-ordered m)))
                                        (cond ((null? kvs) #f)
                                              ((and (jolt=2 v (cdar kvs)) (jx-call m "remove" (list (caar kvs) (cdar kvs)))) #t)
                                              (else (loop (cdr kvs)))))))
(define wv-entry-remove (lambda (m e) (let ((kv (hmv-entry-kv e)))
                                        (and kv (not (jolt-nil? (car kv))) (jx-call m "remove" (list (car kv) (cdr kv))) #t))))
(for-each
  (lambda (r)
    (let ((tag (car r)) (cls (cadr r)) (kind (caddr r)) (supers (cadddr r)))
      (jx-class! tag cls supers)
      (register-host-methods! tag
        (append (wv-common kind (case kind ((key) "jx-chm-key-itr") ((value) "jx-chm-value-itr") (else "jx-chm-entry-itr"))
                           (case kind ((key) wv-key-has) ((value) hmv-has-value?) (else hmv-has-entry?))
                           (case kind ((key) wv-key-remove) ((value) wv-value-remove) (else wv-entry-remove)))
                (if (eq? kind 'key)
                    (list (cons "add" wv-keyset-add)
                          (cons "getMappedValue" (lambda (self) (vector-ref (jhost-state self) 1))))
                    '())))
      (jx-register! tag (case kind ((key) 'set) ((value) 'coll) (else 'entries)) (wv-elems kind)
                    (lambda (v) (hashtable-size (hm-tbl (hmv-map v))))
                    (case kind
                      ((key) (lambda (v k) (hashtable-contains? (hm-tbl (hmv-map v)) k)))
                      ((entry) (lambda (v e) (hmv-has-entry? (hmv-map v) e)))
                      (else #f))
                    #f)
      (register-image-refusal! (list tag) "a view is a window onto a map this process holds; write the map instead")))
  '(("jx-chm-keyset" "java.util.concurrent.ConcurrentHashMap$KeySetView" key ("java.util.Set" "java.io.Serializable"))
    ("jx-chm-values" "java.util.concurrent.ConcurrentHashMap$ValuesView" value ("java.util.Collection" "java.io.Serializable"))
    ("jx-chm-entryset" "java.util.concurrent.ConcurrentHashMap$EntrySetView" entry ("java.util.Set" "java.io.Serializable"))
    ("jx-props-keyset" "java.util.Collections$SynchronizedSet" key ("java.util.Set" "java.io.Serializable"))
    ("jx-props-values" "java.util.Collections$SynchronizedCollection" value ("java.util.Collection" "java.io.Serializable"))
    ("jx-props-entryset" "java.util.Collections$SynchronizedSet" entry ("java.util.Set" "java.io.Serializable"))))
(register-image-refusal! '("jx-chm-key-itr" "jx-chm-value-itr" "jx-chm-entry-itr" "jx-chm-entry")
                         "an iterator or entry is a position in a map this process holds; write the map instead")
(register-host-methods! "chm"
  (list (cons "keySet" (case-lambda
                         ((self) (make-jhost "jx-chm-keyset" (vector self jolt-nil)))
                         ((self d) (when (jolt-nil? d) (throw-jvm 'NullPointerException jolt-nil))
                                   (make-jhost "jx-chm-keyset" (vector self d)))))
        (cons "values" (lambda (self) (make-jhost "jx-chm-values" (vector self))))
        (cons "entrySet" (lambda (self) (make-jhost "jx-chm-entryset" (vector self))))))
;; a fresh wrapper per call, as Properties' own keySet/values/entrySet make one
(register-host-methods! "properties"
  (list (cons "keySet" (lambda (self) (make-jhost "jx-props-keyset" (vector self jolt-nil))))
        (cons "values" (lambda (self) (make-jhost "jx-props-values" (vector self))))
        (cons "entrySet" (lambda (self) (make-jhost "jx-props-entryset" (vector self))))))

;; AbstractMap$SimpleEntry: a mutable Map.Entry anyone can build. State #(key value)
(jx-class! "jx-simple-entry" "java.util.AbstractMap$SimpleEntry" '("java.util.Map$Entry" "java.io.Serializable"))
(register-jutil-entry! "jx-simple-entry" (lambda (e) (cons (vector-ref (jhost-state e) 0) (vector-ref (jhost-state e) 1))))
(register-host-methods! "jx-simple-entry"
  (list (cons "setValue" (lambda (self v) (let ((old (vector-ref (jhost-state self) 1))) (vector-set! (jhost-state self) 1 v) old)))))
;; …and AbstractMap$SimpleImmutableEntry, whose setValue refuses. Both by the
;; simple name an import gives too, as every java.util class is.
(jx-class! "jx-simple-imm-entry" "java.util.AbstractMap$SimpleImmutableEntry" '("java.util.Map$Entry" "java.io.Serializable"))
(register-jutil-entry! "jx-simple-imm-entry" (lambda (e) (cons (vector-ref (jhost-state e) 0) (vector-ref (jhost-state e) 1))))
(register-host-methods! "jx-simple-imm-entry" (list (cons "setValue" (lambda (self v) (jx-uoe)))))
(for-each
  (lambda (r)
    (let ((ctor (let ((tag (cadr r)))
                  (case-lambda ((k v) (make-jhost tag (vector k v)))
                               ((e) (let ((kv (hmv-entry-kv e)))
                                      (unless kv (throw-jvm 'ClassCastException "not a Map.Entry"))
                                      (make-jhost tag (vector (car kv) (cdr kv)))))))))
      (register-class-ctor! (string-append "java.util." (car r)) ctor)
      (register-class-ctor! (car r) ctor)))
  '(("AbstractMap$SimpleEntry" "jx-simple-entry") ("AbstractMap$SimpleImmutableEntry" "jx-simple-imm-entry")))
;; ======================================================================================
;; A proxy over java.util's abstract collection classes
;; ======================================================================================
;; (proxy [java.util.AbstractList] [] (get [i] …) (size [] …)) extends the class
;; on the JVM and inherits everything it does not write. jolt has no constructor
;; for an abstract class, so the proxy is a reify of the class's name, and these
;; tables (host-static.ss register-abstract-methods!) are the inheritance: each
;; method runs against the proxy, so its calls back into get / size / iterator /
;; entrySet reach the proxy's own. coll-defaults.ss holds most of them already;
;; added here are what the abstract classes write themselves: toString, equality
;; and hashing by kind, remove(Object), the stubs an abstract method raises, and
;; AbstractMap's lookups over entrySet.
(define (abs-by-arity rows)
  ;; (name argc proc) rows -> (name . proc), the proc choosing by argument count
  (let ((names (fold-left (lambda (acc r) (if (member (car r) acc) acc (append acc (list (car r))))) '() rows)))
    (map (lambda (n)
           (let ((by (filter (lambda (r) (string=? (car r) n)) rows)))
             (cons n (lambda (self . args)
                       (let ((r (find (lambda (r) (fx=? (cadr r) (length args))) by)))
                         (if r (apply (caddr r) self args)
                             (throw-jvm 'IllegalArgumentException
                                        (string-append "No matching method " n " found taking "
                                                       (number->string (length args)) " args"))))))))
         names)))
(define (abs-stub name argc)
  ;; an abstract method the proxy does not write: Clojure's proxy raises UOE
  (list name argc (lambda (self . _) (throw-jvm 'UnsupportedOperationException name))))
(define (abs-elems self) (seq->list (jolt-seq (record-method-dispatch self "iterator" jolt-nil))))
(define (abs-elem-str self x map?)
  (cond ((eq? x self) (if map? "(this Map)" "(this Collection)")) ((jolt-nil? x) "null") (else (jolt-str-render-one x))))
(define (abs-coll-string self)
  (let loop ((xs (abs-elems self)) (acc '()))
    (if (null? xs) (string-append "[" (apply string-append (reverse acc)) "]")
        (loop (cdr xs) (cons (string-append (if (null? acc) "" ", ") (abs-elem-str self (car xs) #f)) acc)))))
(define (abs-remove-first! self x)
  (let ((it (record-method-dispatch self "iterator" jolt-nil)))
    (let loop ()
      (if (jolt-truthy? (record-method-dispatch it "hasNext" jolt-nil))
          (if (objects-equals? x (record-method-dispatch it "next" jolt-nil))
              (begin (record-method-dispatch it "remove" jolt-nil) #t)
              (loop))
          #f))))
;; the persistent collection a proxy equals, by kind
(define (abs-as-list self) (apply jolt-vector (abs-elems self)))
(define (abs-as-set self) (apply jolt-hash-set (abs-elems self)))
(define (abs-map-entries self) (seq->list (jolt-seq (record-method-dispatch self "entrySet" jolt-nil))))
(define (abs-as-map self)
  (fold-left (lambda (m e) (jolt-assoc m (jolt-nth e 0) (jolt-nth e 1))) (jolt-hash-map) (abs-map-entries self)))
(define abs-collection-rows
  (append cd-collection-defaults
          (list (abs-stub "iterator" 0) (abs-stub "size" 0)
                ;; Collection's defaults over the iterator
                (list "stream" 0 (lambda (self) (make-stream 'ref (list->cseq (abs-elems self)))))
                (list "parallelStream" 0 (lambda (self) (make-stream 'ref (list->cseq (abs-elems self)))))
                (list "spliterator" 0 (lambda (self) (spl-default self)))
                (list "remove" 1 (lambda (self x) (abs-remove-first! self x)))
                (list "toString" 0 abs-coll-string))))
(define abs-list-rows
  (append (filter (lambda (r) (not (and (string=? (car r) "add") (fx=? (cadr r) 1)))) cd-list-defaults)
          (list (abs-stub "get" 1)
                (list "add" 1 (lambda (self x) (record-method-dispatch self "add" (list->cseq (list (->num (cd-size self)) x))) #t))
                (list "add" 2 (lambda (self i x) (throw-jvm 'UnsupportedOperationException jolt-nil)))
                (list "set" 2 (lambda (self i x) (throw-jvm 'UnsupportedOperationException jolt-nil)))
                (list "remove" 1 (lambda (self x)
                                   (if (and (fixnum? x) (not (jfloat? x)))
                                       (throw-jvm 'UnsupportedOperationException jolt-nil)
                                       (abs-remove-first! self x))))
                (list "equals" 1 (lambda (self o) (or (eq? self o) (jolt=2 (abs-as-list self) o))))
                (list "hashCode" 0 (lambda (self) (->num (jolt-java-hashcode (abs-as-list self))))))
          abs-collection-rows))
(define abs-set-rows
  (append (list (list "equals" 1 (lambda (self o) (or (eq? self o) (jolt=2 (abs-as-set self) o))))
                (list "hashCode" 0 (lambda (self) (->num (jolt-java-hashcode (abs-as-set self))))))
          abs-collection-rows))
(define abs-map-rows
  (append
    (list (abs-stub "entrySet" 0)
          (list "size" 0 (lambda (self) (->num (length (abs-map-entries self)))))
          (list "isEmpty" 0 (lambda (self) (null? (abs-map-entries self))))
          (list "containsKey" 1 (lambda (self k) (and (exists (lambda (e) (objects-equals? k (jolt-nth e 0))) (abs-map-entries self)) #t)))
          (list "get" 1 (lambda (self k) (let ((e (find (lambda (e) (objects-equals? k (jolt-nth e 0))) (abs-map-entries self))))
                                         (if e (jolt-nth e 1) jolt-nil))))
          (list "put" 2 (lambda (self k v) (throw-jvm 'UnsupportedOperationException jolt-nil)))
          (list "remove" 1 (lambda (self k)
                             (let ((it (record-method-dispatch (record-method-dispatch self "entrySet" jolt-nil) "iterator" jolt-nil)))
                               (let loop ()
                                 (if (jolt-truthy? (record-method-dispatch it "hasNext" jolt-nil))
                                     (let ((e (record-method-dispatch it "next" jolt-nil)))
                                       (if (objects-equals? k (jolt-nth e 0))
                                           (begin (record-method-dispatch it "remove" jolt-nil) (jolt-nth e 1))
                                           (loop)))
                                     jolt-nil)))))
          (list "clear" 0 (lambda (self) (record-method-dispatch (record-method-dispatch self "entrySet" jolt-nil) "clear" jolt-nil) jolt-nil))
          ;; snapshots of the keys and values (AbstractMap's are live views over entrySet)
          (list "keySet" 0 (lambda (self) (apply jolt-hash-set (map (lambda (e) (jolt-nth e 0)) (abs-map-entries self)))))
          (list "values" 0 (lambda (self) (list->cseq (map (lambda (e) (jolt-nth e 1)) (abs-map-entries self)))))
          (list "equals" 1 (lambda (self o) (or (eq? self o) (jolt=2 (abs-as-map self) o))))
          (list "hashCode" 0 (lambda (self) (->num (jolt-java-hashcode (abs-as-map self)))))
          (list "toString" 0 (lambda (self)
                               (let loop ((es (abs-map-entries self)) (acc '()))
                                 (if (null? es) (string-append "{" (apply string-append (reverse acc)) "}")
                                     (loop (cdr es) (cons (string-append (if (null? acc) "" ", ")
                                                                         (abs-elem-str self (jolt-nth (car es) 0) #t) "="
                                                                         (abs-elem-str self (jolt-nth (car es) 1) #t))
                                                          acc)))))))
    cd-map-defaults))
(register-abstract-methods! "java.util.AbstractCollection" (abs-by-arity abs-collection-rows))
(register-abstract-methods! "java.util.AbstractList" (abs-by-arity abs-list-rows))
(register-abstract-methods! "java.util.AbstractSet" (abs-by-arity abs-set-rows))
(register-abstract-methods! "java.util.AbstractQueue" (abs-by-arity (append cd-queue-defaults abs-collection-rows)))
(register-abstract-methods! "java.util.AbstractMap" (abs-by-arity abs-map-rows))
;; …and such a proxy is that collection to count, seq, get, contains?, =, hash,
;; str and pr, by the kind its declared class gives (a proxy with a delegate is
;; the delegate's, jutil-colls.ss). Last in each walk: they are rare.
(define abs-kind-memo (make-weak-eq-hashtable))
(define abs-kind-mu (make-mutex))
(define (abs-proxy-kind x)
  (and (jreify? x) (not (jreify-delegate x))
       (let ((ps (jreify-protos x)))
         (let ((k (hashtable-ref abs-kind-memo ps 'none)))
           (if (eq? k 'none)
               (let* ((tags (jreify-host-tags x))
                      (has? (lambda (c) (and (member c tags) #t)))
                      (k (and (exists (lambda (t) (hashtable-ref abstract-methods-tbl t #f)) tags)
                              (cond ((has? "java.util.Map") 'map)
                                    ((has? "java.util.Set") 'set)
                                    ((has? "java.util.List") (if (has? "java.util.RandomAccess") 'ralist 'list))
                                    ((has? "java.util.Collection") 'coll)
                                    (else #f)))))
                 (jolt-with-mutex abs-kind-mu (hashtable-set! abs-kind-memo ps k))
                 k)
               k)))))
(define (abs-plain x)
  (case (abs-proxy-kind x)
    ((map) (abs-as-map x)) ((set) (abs-as-set x)) ((list ralist) (abs-as-list x)) (else x)))
(register-count-arm-last! abs-proxy-kind (lambda (x) (jnum->exact (record-method-dispatch x "size" jolt-nil))))
(register-seq-arm-last! abs-proxy-kind
  (lambda (x) (if (eq? (abs-proxy-kind x) 'map)
                  (jolt-seq (record-method-dispatch x "entrySet" jolt-nil))
                  (jolt-seq (iterator-cursor->seq (record-method-dispatch x "iterator" jolt-nil))))))
(register-get-arm-last! (lambda (x) (eq? (abs-proxy-kind x) 'map))
  (lambda (x k d) (if (jolt-truthy? (record-method-dispatch x "containsKey" (list->cseq (list k))))
                      (record-method-dispatch x "get" (list->cseq (list k))) d)))
(register-contains-arm-last! (lambda (x) (memq (abs-proxy-kind x) '(map set)))
  (lambda (x k) (jolt-truthy? (record-method-dispatch x (if (eq? (abs-proxy-kind x) 'map) "containsKey" "contains")
                                                      (list->cseq (list k))))))
(define (abs-equiv-kind x) (let ((k (abs-proxy-kind x))) (and k (not (eq? k 'coll)) k)))
(register-eq-arm-last! (lambda (a b) (or (abs-equiv-kind a) (abs-equiv-kind b)))
  (lambda (a b) (jolt=2 (abs-plain a) (abs-plain b))))
(register-hash-arm-last! abs-equiv-kind (lambda (x) (jnum->exact (record-method-dispatch x "hashCode" jolt-nil))))
(register-str-render! abs-proxy-kind (lambda (x) (record-method-dispatch x "toString" jolt-nil)))
;; print-method's java.util arms: a List as (…), a RandomAccess one as […], a Set
;; as #{…}, a Map as {…}; any other Collection as #object
(register-pr-readable-arm! abs-equiv-kind
  (lambda (x) (if (jolt-pr-readable?)
                  (jolt-pr-readable (case (abs-proxy-kind x)
                                      ((list) (list->cseq (abs-elems x)))
                                      (else (abs-plain x))))
                  (jolt-object-repr x #f))))
;; ======================================================================================
;; java.util.Spliterator
;; ======================================================================================
;; One object for every JDK spliterator class: its class name, characteristics
;; and comparator are the JDK's for the source, and it is backed one of two ways.
;;   vec   a snapshot of the elements, split at the midpoint as an array-backed
;;         spliterator is (ArrayList's, an array's, a Vector's, …): the prefix goes
;;         to a new one of the same class
;;   iter  a live iterator, split by batches as IteratorSpliterator and the
;;         linked collections' are: the batch comes back as an ArraySpliterator.
;;         It stays lazy, so a spliteratorUnknownSize over an endless iterator
;;         works
;; A hash or tree spliterator splits where its table or tree does on the JDK,
;; which jolt's do not share; those split at the midpoint here, with the same
;; contents in the same order overall. State #(kind src lo hi chars cmp batch class)
(define spl-ORDERED 16) (define spl-DISTINCT 1) (define spl-SORTED 4) (define spl-SIZED 64)
(define spl-NONNULL 256) (define spl-IMMUTABLE 1024) (define spl-CONCURRENT 4096) (define spl-SUBSIZED 16384)
(define spl-long-max 9223372036854775807)
(define spl-classes
  '("java.util.ArrayList$ArrayListSpliterator" "java.util.LinkedList$LLSpliterator" "java.util.ArrayDeque$DeqSpliterator"
    "java.util.HashMap$KeySpliterator" "java.util.HashMap$ValueSpliterator" "java.util.HashMap$EntrySpliterator"
    "java.util.Spliterators$IteratorSpliterator" "java.util.Spliterators$ArraySpliterator"
    "java.util.TreeMap$KeySpliterator" "java.util.TreeMap$ValueSpliterator" "java.util.TreeMap$EntrySpliterator"
    "java.util.AbstractList$RandomAccessSpliterator" "java.util.concurrent.ConcurrentLinkedQueue$CLQSpliterator"
    "java.util.concurrent.ConcurrentLinkedDeque$CLDSpliterator" "java.util.concurrent.ConcurrentSkipListMap$KeySpliterator"
    "java.util.concurrent.ConcurrentSkipListMap$ValueSpliterator" "java.util.concurrent.ConcurrentSkipListMap$EntrySpliterator"
    "java.util.PriorityQueue$PriorityQueueSpliterator" "java.util.Vector$VectorSpliterator" "clojure.lang.PersistentVector$3"
    "java.util.ArrayList$SubList$2" "java.util.Spliterators$EmptySpliterator$OfRef"
    "java.util.concurrent.ConcurrentHashMap$KeySpliterator" "java.util.concurrent.ConcurrentHashMap$ValueSpliterator"
    "java.util.concurrent.ConcurrentHashMap$EntrySpliterator"
    "java.util.WeakHashMap$KeySpliterator" "java.util.WeakHashMap$ValueSpliterator" "java.util.WeakHashMap$EntrySpliterator"
    "java.util.stream.StreamSpliterators$WrappingSpliterator" "java.util.stream.StreamSpliterators$IntWrappingSpliterator"
    "java.util.stream.StreamSpliterators$LongWrappingSpliterator" "java.util.stream.StreamSpliterators$DoubleWrappingSpliterator"
    "java.util.stream.Streams$RangeIntSpliterator" "java.util.stream.Streams$RangeLongSpliterator"
    "java.util.Spliterators$EmptySpliterator$OfInt" "java.util.Spliterators$EmptySpliterator$OfLong"
    "java.util.Spliterators$EmptySpliterator$OfDouble"
    "java.util.Spliterators$IntArraySpliterator" "java.util.Spliterators$LongArraySpliterator" "java.util.Spliterators$DoubleArraySpliterator"
    "java.util.IdentityHashMap$KeySpliterator" "java.util.IdentityHashMap$ValueSpliterator" "java.util.IdentityHashMap$EntrySpliterator"))
(define spl-tag-of (make-hashtable string-hash string=?))
(jch-register-supers! "java.util.Spliterator" '())
(define (spl-ref x i) (vector-ref (jhost-state x) i))
(define (spl-set! x i v) (vector-set! (jhost-state x) i v))
(define (spl-sized? x) (not (fx=? 0 (fxand (spl-ref x 4) spl-SIZED))))
;; slots 8 and 9: the collection a late-binding snapshot watches and the
;; modCount it expects (#f for none): a HashMap's or TreeMap's spliterator takes
;; the count when it binds, checks it after each tryAdvance's action and once
;; after forEachRemaining, as theirs do
(define (make-spl cls kind src lo hi chars cmp)
  (make-jhost (hashtable-ref spl-tag-of cls #f) (vector kind src lo hi chars cmp 0 cls #f #f)))
(define (spl-watch-check! x)
  (let ((c (spl-ref x 8)))
    (when (and c (not (eqv? (spl-ref x 9) (jutil-modcount c)))) (jutil-cme!))))
(define (make-vec-spl cls chars elems . cmp)
  (let ((v (list->vector elems))) (make-spl cls 'vec v 0 (vector-length v) chars (if (pair? cmp) (car cmp) jolt-nil))))
(define (make-iter-spl cls chars it est . cmp)
  (make-spl cls 'iter it 0 est chars (if (pair? cmp) (car cmp) jolt-nil)))
;; A list that keeps a modCount (ArrayList, LinkedList, Vector) is walked live:
;; kind live, src the list, lo the next index, hi its fence (-1 until first
;; use) and slot 6 the count expected. Elements are read from the list as it
;; stands, and the count is checked after each tryAdvance's action and once
;; after a forEachRemaining, as ArrayListSpliterator / LLSpliterator /
;; VectorSpliterator do. A LinkedList's splits off a batch as an
;; ArraySpliterator; the others split at the midpoint.
(define (spl-live-size l) (if (al-family? l) (al-cnt l) (jnum->exact (jx-call l "size" '()))))
(define (spl-live-get l i) (if (al-family? l) (al-ref l i) (jx-call l "get" (list (->num i)))))
(define (spl-live-check! x)
  (let ((e (spl-ref x 6)))
    (when (and e (not (eqv? e (jutil-modcount (spl-ref x 1))))) (jutil-cme!))))
;; a live one's split batch size, in the comparator slot (a list has no comparator)
(define (spl-batch x) (let ((b (spl-ref x 5))) (if (fixnum? b) b 0)))
(define (spl-batch! x n) (spl-set! x 5 n))
(define (make-live-spl cls chars l)
  (let ((s (make-spl cls 'live l 0 -1 chars jolt-nil))) (spl-set! s 6 #f) s))
(define (spl-force! x)
  ;; a late-binding one takes its elements at its first use, as the JDK's
  ;; collection spliterators bind to the source then rather than at creation
  (when (and (eq? (spl-ref x 0) 'live) (fx<? (spl-ref x 3) 0))
    (spl-set! x 3 (spl-live-size (spl-ref x 1)))
    (spl-set! x 6 (jutil-modcount (spl-ref x 1))))
  (when (eq? (spl-ref x 0) 'lazy)
    (let ((c (spl-ref x 8)))
      (when c (spl-set! x 9 (jutil-modcount c))))
    (let ((v (list->vector ((spl-ref x 1)))))
      (spl-set! x 0 'vec) (spl-set! x 1 v) (spl-set! x 3 (vector-length v))))
  ;; an IteratorSpliterator over a collection makes its iterator at first use
  (when (eq? (spl-ref x 0) 'iterc)
    (let ((c (spl-ref x 1)))
      (spl-set! x 0 'iter) (spl-set! x 1 (spl-it c)) (spl-set! x 3 (spl-n c)))))
(define (spl-advance! x)
  ;; the next element, or the spl-none marker
  (spl-force! x)
  (case (spl-ref x 0)
    ((vec) (let ((lo (spl-ref x 2)))
             (if (fx<? lo (spl-ref x 3)) (begin (spl-set! x 2 (fx+ lo 1)) (vector-ref (spl-ref x 1) lo)) spl-none)))
    ((live) (let ((lo (spl-ref x 2)) (l (spl-ref x 1)))
              ;; a list shrunk under the walk ends it (the count check reports it)
              (if (and (fx<? lo (spl-ref x 3)) (fx<? lo (spl-live-size l)))
                  (begin (spl-set! x 2 (fx+ lo 1)) (spl-live-get l lo))
                  spl-none)))
    (else (let ((it (spl-ref x 1)))
            (if (jolt-truthy? (jx-call it "hasNext" '()))
                (let ((v (jx-call it "next" '())))
                  (when (and (spl-sized? x) (< (spl-ref x 3) spl-long-max) (> (spl-ref x 3) 0)) (spl-set! x 3 (- (spl-ref x 3) 1)))
                  v)
                spl-none)))))
(define spl-none (list 'spl-none))
(define (spl-split! x)
  (spl-force! x)
  (case (if (member (spl-ref x 7) '("java.util.stream.StreamSpliterators$WrappingSpliterator" "java.util.stream.StreamSpliterators$IntWrappingSpliterator"
                                     "java.util.stream.StreamSpliterators$LongWrappingSpliterator" "java.util.stream.StreamSpliterators$DoubleWrappingSpliterator"))
            'none (spl-ref x 0))
    ;; a sequential pipeline's does not split
    ((none) jolt-nil)
    ((live)
     (let* ((lo (spl-ref x 2)) (hi (spl-ref x 3)) (l (spl-ref x 1)))
       (if (string=? (spl-ref x 7) "java.util.LinkedList$LLSpliterator")
           ;; LLSpliterator: a batch, 1024 more each time, as an ArraySpliterator
           ;; it splits only with more than one element left
           (let* ((b (spl-batch x)) (take (fxmin (fx+ b 1024) (fx- hi lo))))
             (if (fx<=? (fx- hi lo) 1) jolt-nil
                 (let loop ((j 0) (acc '()))
                   (if (and (fx<? j take) (fx<? (fx+ lo j) (spl-live-size l)))
                       (loop (fx+ j 1) (cons (spl-live-get l (fx+ lo j)) acc))
                       (begin (spl-set! x 2 (fx+ lo j)) (spl-batch! x j)
                              (make-vec-spl "java.util.Spliterators$ArraySpliterator" (fxior spl-ORDERED spl-SIZED spl-SUBSIZED)
                                            (reverse acc)))))))
           (let ((mid (fxsrl (fx+ lo hi) 1)))
             (if (fx>=? lo mid) jolt-nil
                 (let ((p (make-spl (spl-ref x 7) 'live l lo mid (spl-ref x 4) (spl-ref x 5))))
                   (spl-set! p 6 (spl-ref x 6))
                   (spl-set! x 2 mid)
                   p))))))
    ((vec) (let* ((lo (spl-ref x 2)) (mid (fxsrl (fx+ lo (spl-ref x 3)) 1)))
             (if (fx>=? lo mid) jolt-nil
                 (begin (spl-set! x 2 mid)
                        (let ((p (make-spl (spl-ref x 7) 'vec (spl-ref x 1) lo mid (spl-ref x 4) (spl-ref x 5))))
                          ;; the halves watch the same count
                          (spl-set! p 8 (spl-ref x 8)) (spl-set! p 9 (spl-ref x 9))
                          p)))))
    (else
     ;; IteratorSpliterator / LLSpliterator take batches of 1024 more each time;
     ;; the concurrent linked queues start at one and grow by one
     (let* ((it (spl-ref x 1)) (est (spl-ref x 3))
            (clq? (not (fx=? 0 (fxand (spl-ref x 4) spl-CONCURRENT)))))
       (if (and (or clq? (> est 1)) (jolt-truthy? (jx-call it "hasNext" '())))
           (let* ((b (spl-ref x 6))
                  (n (if clq? (if (fx<=? b 0) 1 (fxmin (fx+ b 1) 33554432)) (min (fx+ b 1024) est 33554432))))
             (let loop ((j 0) (acc '()))
               (if (and (fx<? j n) (jolt-truthy? (jx-call it "hasNext" '())))
                   (loop (fx+ j 1) (cons (jx-call it "next" '()) acc))
                   (begin
                     (spl-set! x 6 j)
                     (when (< est spl-long-max) (spl-set! x 3 (- est j)))
                     (if (and (not clq?) (= est spl-long-max))
                         ;; an unsized one's batch keeps its characteristics and
                         ;; reports half the unknown size, as the JDK's does
                         (let ((b (make-vec-spl "java.util.Spliterators$ArraySpliterator" (spl-ref x 4) (reverse acc))))
                           (spl-set! b 6 (quotient spl-long-max 2))
                           b)
                         (make-vec-spl "java.util.Spliterators$ArraySpliterator"
                                       (fxior (if clq? (fxior spl-ORDERED spl-NONNULL spl-CONCURRENT) (spl-ref x 4)) spl-SIZED spl-SUBSIZED)
                                       (reverse acc)))))))
           jolt-nil)))))
(define spliterator-methods
  (list (cons "tryAdvance" (lambda (self f)
                             (when (jolt-nil? f) (jx-npe))
                             (let ((v (spl-advance! self)))
                               (if (eq? v spl-none) #f
                                   (begin (jolt-fi-call f "accept" v)
                                          (if (eq? (spl-ref self 0) 'live) (spl-live-check! self) (spl-watch-check! self))
                                          #t)))))
        (cons "forEachRemaining" (lambda (self f)
                                   (when (jolt-nil? f) (jx-npe))
                                   (if (and (eq? (spl-ref self 0) 'live) (al-family? (spl-ref self 1))
                                            (not (string=? (spl-ref self 7) "java.util.LinkedList$LLSpliterator")))
                                       ;; ArrayListSpliterator walks its backing array to the fence,
                                       ;; whatever the action does to the list: index i as the array
                                       ;; holds it then, and nothing (null) past the list's end
                                       (begin
                                         (spl-force! self)
                                         (let* ((l (spl-ref self 1)) (hi (spl-ref self 3)))
                                           (let loop ((i (spl-ref self 2)))
                                             (when (fx<? i hi)
                                               (spl-set! self 2 (fx+ i 1))
                                               (jolt-fi-call f "accept" (if (fx<? i (al-cnt l)) (al-ref l i) jolt-nil))
                                               (loop (fx+ i 1))))))
                                       (let loop () (let ((v (spl-advance! self))) (unless (eq? v spl-none) (jolt-fi-call f "accept" v) (loop)))))
                                   (if (eq? (spl-ref self 0) 'live) (spl-live-check! self) (spl-watch-check! self))
                                   jolt-nil))
        (cons "trySplit" (lambda (self) (spl-split! self)))
        (cons "estimateSize" (lambda (self)
                               (spl-force! self)
                               (->num (cond ((eq? (spl-ref self 0) 'live) (fx- (spl-ref self 3) (spl-ref self 2)))
                                            ((not (eq? (spl-ref self 0) 'vec)) (spl-ref self 3))
                                            ((and (not (spl-sized? self)) (> (spl-ref self 6) 0)) (spl-ref self 6))
                                            (else (fx- (spl-ref self 3) (spl-ref self 2)))))))
        (cons "getExactSizeIfKnown" (lambda (self) (if (spl-sized? self) (jx-call self "estimateSize" '()) (->num -1))))
        (cons "characteristics" (lambda (self) (->num (spl-ref self 4))))
        (cons "hasCharacteristics" (lambda (self c) (fx=? (fxand (spl-ref self 4) (jnum->exact c)) (jnum->exact c))))
        (cons "getComparator" (lambda (self) (if (fx=? 0 (fxand (spl-ref self 4) spl-SORTED)) (throw-jvm 'IllegalStateException jolt-nil) (spl-ref self 5))))))
(for-each (lambda (cls)
            (let ((tag (string-append "jx-spl:" cls)))
              (hashtable-set! spl-tag-of cls tag)
              (jx-class! tag cls '("java.util.Spliterator"))
              (register-host-methods! tag spliterator-methods)
              (register-image-refusal! (list tag) "a spliterator is a position in a walk this process is making; write the collection instead")))
          spl-classes)
(register-class-statics! "java.util.Spliterator"
  (list (cons "ORDERED" (->num spl-ORDERED)) (cons "DISTINCT" (->num spl-DISTINCT)) (cons "SORTED" (->num spl-SORTED))
        (cons "SIZED" (->num spl-SIZED)) (cons "NONNULL" (->num spl-NONNULL)) (cons "IMMUTABLE" (->num spl-IMMUTABLE))
        (cons "CONCURRENT" (->num spl-CONCURRENT)) (cons "SUBSIZED" (->num spl-SUBSIZED))))
;; ---- the sources ---------------------------------------------------------------------
(define (spl-it c) (jx-call c "iterator" '()))
(define (spl-n c) (jnum->exact (jx-call c "size" '())))
(define (spl-vec-of cls chars . cmp)
  ;; late-binding: the elements are read at the spliterator's first use, and
  ;; the modCount of a collection that keeps one taken then
  (lambda (self) (spl-lazy-of cls chars self (if (pair? cmp) (car cmp) jolt-nil))))
(define (spl-lazy-of cls chars self cmp)
  (let ((s (make-spl cls 'lazy (lambda () (jxc-elems self)) 0 0 chars cmp)))
    (when (jutil-modcount self) (spl-set! s 8 self))
    s))
(define (spl-snapshot-of cls chars) (lambda (self) (make-vec-spl cls chars (jxc-elems self))))
;; Spliterators.spliterator(collection, …): the iterator and the size are taken
;; at first use
(define (spl-iter-of cls chars) (lambda (self) (make-spl cls 'iterc self 0 0 chars jolt-nil)))
(define spl-OSS (fxior spl-ORDERED spl-SIZED spl-SUBSIZED))                    ; 16464
(define (spl-entry-cmp cmp)
  ;; TreeMap's EntrySpliterator orders entries by key, under the map's comparator
  (let ((kc (if (jolt-nil? cmp) tm-natural-compare (tm-comparator->cmpf cmp))))
    (lambda (a b) (kc (jolt-nth a 0) (jolt-nth b 0)))))
(for-each (lambda (r) (register-host-methods! (car r) (list (cons "spliterator" (cdr r)))))
  (list
    (cons "arraylist" (lambda (self) (make-live-spl "java.util.ArrayList$ArrayListSpliterator" spl-OSS self)))
    (cons "cd-arraylist-sublist" (spl-vec-of "java.util.ArrayList$SubList$2" spl-OSS))
    (cons "linkedlist" (lambda (self) (make-live-spl "java.util.LinkedList$LLSpliterator" spl-OSS self)))
    (cons "hashset" (spl-vec-of "java.util.HashMap$KeySpliterator" (fxior spl-SIZED spl-DISTINCT)))
    (cons "jx-hm-keyset" (spl-vec-of "java.util.HashMap$KeySpliterator" (fxior spl-SIZED spl-DISTINCT)))
    (cons "jx-chm-keyset" (spl-vec-of "java.util.concurrent.ConcurrentHashMap$KeySpliterator" (fxior spl-DISTINCT spl-NONNULL spl-CONCURRENT)))
    (cons "jx-chm-values" (spl-vec-of "java.util.concurrent.ConcurrentHashMap$ValueSpliterator" (fxior spl-NONNULL spl-CONCURRENT)))
    (cons "jx-chm-entryset" (spl-vec-of "java.util.concurrent.ConcurrentHashMap$EntrySpliterator" (fxior spl-DISTINCT spl-NONNULL spl-CONCURRENT)))
    (cons "jx-props-keyset" (spl-vec-of "java.util.concurrent.ConcurrentHashMap$KeySpliterator" (fxior spl-DISTINCT spl-NONNULL spl-CONCURRENT)))
    (cons "jx-props-values" (spl-vec-of "java.util.concurrent.ConcurrentHashMap$ValueSpliterator" (fxior spl-NONNULL spl-CONCURRENT)))
    (cons "jx-props-entryset" (spl-iter-of "java.util.Spliterators$IteratorSpliterator" (fxior spl-SIZED spl-SUBSIZED spl-DISTINCT)))
    (cons "jx-hm-values" (spl-vec-of "java.util.HashMap$ValueSpliterator" spl-SIZED))
    (cons "jx-hm-entryset" (spl-vec-of "java.util.HashMap$EntrySpliterator" (fxior spl-SIZED spl-DISTINCT)))
    (cons "jx-whm-keyset" (spl-vec-of "java.util.WeakHashMap$KeySpliterator" spl-DISTINCT))
    (cons "jx-whm-values" (spl-vec-of "java.util.WeakHashMap$ValueSpliterator" 0))
    (cons "jx-whm-entryset" (spl-vec-of "java.util.WeakHashMap$EntrySpliterator" spl-DISTINCT))
    (cons "jx-ihm-keyset" (spl-vec-of "java.util.IdentityHashMap$KeySpliterator" (fxior spl-SIZED spl-DISTINCT)))
    (cons "jx-ihm-values" (spl-vec-of "java.util.IdentityHashMap$ValueSpliterator" spl-SIZED))
    (cons "jx-ihm-entryset" (spl-vec-of "java.util.IdentityHashMap$EntrySpliterator" (fxior spl-SIZED spl-DISTINCT)))
    (cons "linkedhashset" (spl-iter-of "java.util.Spliterators$IteratorSpliterator" (fxior spl-OSS spl-DISTINCT)))
    (cons "lhm-keyset" (spl-iter-of "java.util.Spliterators$IteratorSpliterator" (fxior spl-OSS spl-DISTINCT)))
    (cons "lhm-values" (spl-iter-of "java.util.Spliterators$IteratorSpliterator" spl-OSS))
    (cons "lhm-entryset" (spl-iter-of "java.util.Spliterators$IteratorSpliterator" (fxior spl-OSS spl-DISTINCT)))
    ;; TreeMap's spliterators bind to the tree at first use
    (cons "treeset" (lambda (self) (spl-lazy-of "java.util.TreeMap$KeySpliterator" (fxior spl-DISTINCT spl-SORTED spl-ORDERED spl-SIZED)
                                                self (jx-call self "comparator" '()))))
    (cons "treemap-keyset" (lambda (self) (spl-lazy-of "java.util.TreeMap$KeySpliterator" (fxior spl-DISTINCT spl-SORTED spl-ORDERED spl-SIZED)
                                                       self (jx-call self "comparator" '()))))
    (cons "treemap-values" (spl-vec-of "java.util.TreeMap$ValueSpliterator" (fxior spl-ORDERED spl-SIZED)))
    (cons "treemap-entryset" (lambda (self) (spl-lazy-of "java.util.TreeMap$EntrySpliterator" (fxior spl-DISTINCT spl-SORTED spl-ORDERED spl-SIZED)
                                                         self (spl-entry-cmp (tmr-cmp-obj (v-root self))))))
    (cons "arrays-aslist" (spl-vec-of "java.util.Spliterators$ArraySpliterator" spl-OSS))
    (cons "jx-listn" (spl-vec-of "java.util.AbstractList$RandomAccessSpliterator" spl-OSS))
    (cons "jx-list12" (spl-vec-of "java.util.AbstractList$RandomAccessSpliterator" spl-OSS))
    (cons "jx-setn" (spl-iter-of "java.util.Spliterators$IteratorSpliterator" (fxior spl-SIZED spl-SUBSIZED spl-DISTINCT)))
    (cons "jx-set12" (spl-iter-of "java.util.Spliterators$IteratorSpliterator" (fxior spl-SIZED spl-SUBSIZED spl-DISTINCT)))))
;; the unmodifiable and synchronized wrappers hand back the collection's own
(define (spl-of-inner self) (jx-call (jxw-inner self) "spliterator" '()))
(for-each (lambda (f)
            (for-each (lambda (t) (when t (register-host-methods! t (list (cons "spliterator" spl-of-inner)))))
                      (list (list-ref f 1) (list-ref f 2))))
          jx-families)
;; a wrapper's modCount is the collection's it wraps: its iterator is that
;; collection's, so a reduce or a stream over it is fail-fast the same way
(for-each (lambda (f)
            (for-each (lambda (t) (when t (register-modcount! t (lambda (w) (jutil-modcount (jxw-inner w))))))
                      (list (list-ref f 1) (list-ref f 2))))
          jx-families)
;; any other Collection: the interface's default — a RandomAccess List's
;; RandomAccessSpliterator, otherwise an IteratorSpliterator, ORDERED for a List
;; and DISTINCT for a Set
(define (spl-default self)
  (cond ((and (jx-isa? self "java.util.List") (jx-isa? self "java.util.RandomAccess"))
         (make-vec-spl "java.util.AbstractList$RandomAccessSpliterator" spl-OSS (jxc-elems self)))
        ((jx-isa? self "java.util.List") ((spl-iter-of "java.util.Spliterators$IteratorSpliterator" spl-OSS) self))
        ((jx-isa? self "java.util.Set") ((spl-iter-of "java.util.Spliterators$IteratorSpliterator" (fxior spl-SIZED spl-SUBSIZED spl-DISTINCT)) self))
        (else ((spl-iter-of "java.util.Spliterators$IteratorSpliterator" (fxior spl-SIZED spl-SUBSIZED)) self))))
(hashtable-set! cd-default-tbl "java.util.Collection"
                (append (hashtable-ref cd-default-tbl "java.util.Collection" '()) (list (list "spliterator" 0 spl-default))))
;; a persistent collection: a vector's own (PersistentVector$3), a map's
;; Iterable default (no size, no characteristics), a set's and any seq's the
;; Collection one
(set! persistent-spliterator-hook
  (lambda (obj)
    (cond ((pvec? obj) (make-vec-spl "clojure.lang.PersistentVector$3" (fxior spl-OSS spl-IMMUTABLE) (seq->list (jolt-seq obj))))
          ((jolt-map? obj) (make-iter-spl "java.util.Spliterators$IteratorSpliterator" 0 (make-jiterator (jolt-seq obj)) spl-long-max))
          ((jolt-set? obj) (make-iter-spl "java.util.Spliterators$IteratorSpliterator" (fxior spl-SIZED spl-SUBSIZED spl-DISTINCT)
                                          (make-jiterator (jolt-seq obj)) (jolt-count obj)))
          (else (make-iter-spl "java.util.Spliterators$IteratorSpliterator" spl-OSS (make-jiterator (jolt-seq obj)) (jolt-count obj))))))
;; Stream.spliterator: the source collection's own when the stream came straight
;; from one (a head stream, as the JDK's pipeline head answers), else one over
;; the elements; either way it consumes the stream
;; A pipeline's (a stream after an intermediate op) is the JDK's
;; StreamSpliterators$WrappingSpliterator: what its source reports, ORDERED,
;; DISTINCT, SORTED, SIZED and SUBSIZED only, edited by each operation as the
;; JDK's StreamOpFlags edit it, and sized to the source (exactly, while SIZED
;; holds). A sequential one does not split. Every stream class answers it.
(define (spl-pipeline-base origin)
  ;; (characteristics . estimate) of what the pipeline started from
  (case (and (pair? origin) (car origin))
    ((coll) (let ((sp (jx-call (cdr origin) "spliterator" '())))
              (cons (jnum->exact (jx-call sp "characteristics" '())) (jnum->exact (jx-call sp "estimateSize" '())))))
    ((array) (cons (fxior spl-ORDERED spl-SIZED spl-SUBSIZED) (cdr origin)))
    ((range) (cons (fxior spl-ORDERED spl-DISTINCT spl-SORTED spl-SIZED spl-SUBSIZED) (cdr origin)))
    ((generate) (cons 0 spl-long-max))
    ((empty) (cons (fxior spl-SIZED spl-SUBSIZED) 0))
    (else (cons spl-ORDERED spl-long-max))))
(define (spl-pipeline-shape origin ops)
  (let ((keep (fxior spl-ORDERED spl-DISTINCT spl-SORTED spl-SIZED spl-SUBSIZED))
        (unsized (lambda (f) (fxand f (fxnot (fxior spl-SIZED spl-SUBSIZED)))))
        (sized? (lambda (f) (not (fx=? 0 (fxand f spl-SIZED))))))
    (let loop ((ops (reverse ops))
               (f (fxand (car (spl-pipeline-base origin)) keep))
               (n (cdr (spl-pipeline-base origin))))
      (if (null? ops)
          (cons f n)
          (let ((op (car ops)))
            (case (if (pair? op) (car op) op)
              ((map) (loop (cdr ops) (fxand f (fxnot (fxior spl-DISTINCT spl-SORTED))) n))
              ((filter) (loop (cdr ops) (unsized f) n))
              ((flat) (loop (cdr ops) (unsized (fxand f (fxnot (fxior spl-DISTINCT spl-SORTED)))) n))
              ((sorted) (loop (cdr ops) (fxior f spl-SORTED spl-ORDERED) n))
              ((sorted-by) (loop (cdr ops) (fxior (fxand f (fxnot spl-SORTED)) spl-ORDERED) n))
              ((distinct) (loop (cdr ops) (unsized (fxior f spl-DISTINCT)) n))
              ((unordered) (loop (cdr ops) (fxand f (fxnot spl-ORDERED)) n))
              ((limit) (loop (cdr ops) f (if (sized? f) (min n (cdr op)) n)))
              ((skip) (loop (cdr ops) f (if (sized? f) (max 0 (- n (cdr op))) n)))
              (else (loop (cdr ops) f n))))))))
(define (stream-spliterator self)
  (let* ((src (stream-source self)) (ops (stream-ops self)) (origin (stream-origin self))
         (kind (stream-kind self)) (xs (stream-take! self))
         ;; the JDK's class for this element kind: Int / Long / Double or plain
         (by-kind (lambda (int long double ref) (case kind ((int) int) ((long) long) ((double) double) (else ref))))
         (what (and (pair? origin) (car origin))))
    (cond ((pair? ops)
           (let ((shape (spl-pipeline-shape origin ops)))
             (make-spl (string-append "java.util.stream.StreamSpliterators$"
                                      (by-kind "Int" "Long" "Double" "") "WrappingSpliterator")
                       'iter (make-jiterator xs) 0 (cdr shape) (car shape) jolt-nil)))
          (src (jx-call src "spliterator" '()))
          ((or (eq? what 'empty) (and (memq what '(range)) (eqv? (cdr origin) 0)))
           (make-vec-spl (string-append "java.util.Spliterators$EmptySpliterator$Of" (by-kind "Int" "Long" "Double" "Ref"))
                         (fxior spl-SIZED spl-SUBSIZED) '()))
          ((eq? what 'range)
           (make-vec-spl (by-kind "java.util.stream.Streams$RangeIntSpliterator" "java.util.stream.Streams$RangeLongSpliterator"
                                  "java.util.stream.Streams$RangeLongSpliterator" "java.util.stream.Streams$RangeIntSpliterator")
                         (fxior spl-ORDERED spl-DISTINCT spl-SORTED spl-SIZED spl-SUBSIZED spl-NONNULL spl-IMMUTABLE)
                         (seq->list (jolt-seq xs))))
          ((eq? what 'array)
           (make-vec-spl (string-append "java.util.Spliterators$" (by-kind "Int" "Long" "Double" "") "ArraySpliterator")
                         (fxior spl-ORDERED spl-SIZED spl-SUBSIZED spl-IMMUTABLE)
                         (seq->list (jolt-seq xs))))
          (else (make-iter-spl "java.util.Spliterators$IteratorSpliterator" spl-ORDERED (make-jiterator xs) spl-long-max)))))
(for-each (lambda (t) (register-host-methods! t (list (cons "spliterator" stream-spliterator))))
          '("stream" "int-stream" "long-stream" "double-stream"))
;; Spliterators and StreamSupport
(define (spl-chars c) (jnum->exact c))
(register-class-statics! "java.util.Spliterators"
  (list (cons "spliterator"
              (case-lambda
                ((src c) (if (jolt-array? src)
                             (make-vec-spl "java.util.Spliterators$ArraySpliterator" (fxior (spl-chars c) spl-SIZED spl-SUBSIZED) (ja->list src))
                             (make-iter-spl "java.util.Spliterators$IteratorSpliterator" (fxior (spl-chars c) spl-SIZED spl-SUBSIZED)
                                            (spl-it src) (spl-n src))))
                ((it size c) (make-iter-spl "java.util.Spliterators$IteratorSpliterator" (fxior (spl-chars c) spl-SIZED spl-SUBSIZED) it (jnum->exact size)))
                ((arr from to c) (make-vec-spl "java.util.Spliterators$ArraySpliterator" (fxior (spl-chars c) spl-SIZED spl-SUBSIZED)
                                               (list-head (list-tail (ja->list arr) (jnum->exact from)) (- (jnum->exact to) (jnum->exact from)))))))
        (cons "spliteratorUnknownSize" (lambda (it c) (make-iter-spl "java.util.Spliterators$IteratorSpliterator" (spl-chars c) it spl-long-max)))
        (cons "emptySpliterator" (lambda () (make-vec-spl "java.util.Spliterators$EmptySpliterator$OfRef" (fxior spl-SIZED spl-SUBSIZED) '())))
        (cons "iterator" (lambda (spl) (make-jhost "jx-spl-adapter" (vector spl spl-none))))))
;; Spliterators.iterator(spliterator): Spliterators$1Adapter, a one-element lookahead
(define (spla-fill! a)
  (let ((st (jhost-state a)))
    (when (eq? (vector-ref st 1) spl-none) (vector-set! st 1 (spl-advance! (vector-ref st 0))))))
(jx-class! "jx-spl-adapter" "java.util.Spliterators$1Adapter" '("java.util.Iterator"))
(register-host-methods! "jx-spl-adapter"
  (list (cons "hasNext" (lambda (self) (spla-fill! self) (not (eq? (vector-ref (jhost-state self) 1) spl-none))))
        (cons "next" (lambda (self) (spla-fill! self)
                       (let ((v (vector-ref (jhost-state self) 1)))
                         (when (eq? v spl-none) (throw-jvm 'NoSuchElementException jolt-nil))
                         (vector-set! (jhost-state self) 1 spl-none) v)))))
(jx-register! "jx-spl-adapter" 'iter
  (lambda (a) (let loop ((acc '())) (if (jolt-truthy? (jx-call a "hasNext" '())) (loop (cons (jx-call a "next" '()) acc)) (reverse acc))))
  #f #f #f)
(register-image-refusal! '("jx-spl-adapter") "an iterator is a position in a walk this process is making; write the collection instead")
(register-class-statics! "java.util.stream.StreamSupport"
  (list (cons "stream" (lambda (spl parallel?)
                         (make-stream 'ref (let loop ()
                                             (jolt-make-lazy-seq
                                               (lambda () (let ((v (spl-advance! spl))) (if (eq? v spl-none) jolt-nil (jolt-cons v (loop))))))))))))
