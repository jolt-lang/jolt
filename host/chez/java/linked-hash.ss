;; linked-hash.ss — java.util.LinkedHashMap and java.util.LinkedHashSet.
;;
;; A hash table from key to node, the nodes chained in encounter order, so every
;; operation the JDK makes O(1) is O(1) here: a lookup, an insert at either end,
;; an unlink, a move to the tail. The map's encounter order is insertion order,
;; or access order for one built (LinkedHashMap. cap load-factor true): a get,
;; and every operation that reaches an existing mapping, moves it to the tail.
;; After a NEW mapping goes in, removeEldestEntry is asked about the head — of the
;; proxy over the map when there is one (proxy-attach!), which is how the LRU
;; idiom (proxy [LinkedHashMap] [16 0.75 true] (removeEldestEntry …)) evicts.
;;
;; The SequencedMap / SequencedSet surface is here too: putFirst / putLast,
;; firstEntry / lastEntry / pollFirstEntry / pollLastEntry, addFirst …
;; removeLast, and reversed(), a live view in the other direction.
;;
;;   chain   #(table head tail access? proxy modcount)   table: key -> node
;;   node    #(key value prev next)
;;
;; A map or set is a jhost over a chain; keySet, values, entrySet and their
;; sequenced spellings are live views #(chain reversed?); an entry is its node.
;;
;; modcount is HashMap's, which LinkedHashMap inherits: a new key, a removal,
;; every clear, and a move to either end (an access-ordered map's get, a
;; putFirst / putLast of a key that is elsewhere) bump it. The iterators, the
;; forEach / replaceAll walks and the compute family watch it as the JDK's do.
;; Loaded after jutil-colls.ss (proxy-attach-hook!, the java.util registry) and
;; before host-static-classes.ss, so its arms are walked after the HashMap family's.

;; ---- the chain -------------------------------------------------------------------
(define (lh-new access?) (vector (make-hashtable hm-hash jolt=2) #f #f access? #f 0))
(define (lh-mc c) (vector-ref c 5))
(define (lh-bump! c) (vector-set! c 5 (fxand (fx+ (vector-ref c 5) 1) #x3fffffff)))
(define (lh-tbl c) (vector-ref c 0))
(define (lh-head c) (vector-ref c 1))
(define (lh-tail c) (vector-ref c 2))
(define (lh-access? c) (vector-ref c 3))
(define (lh-proxy c) (vector-ref c 4))
(define (n-key n) (vector-ref n 0))
(define (n-val n) (vector-ref n 1))
(define (n-prev n) (vector-ref n 2))
(define (n-next n) (vector-ref n 3))
(define (lh-size c) (hashtable-size (lh-tbl c)))
(define (lh-node c k) (hashtable-ref (lh-tbl c) k #f))

(define (lh-unlink! c n)
  (let ((p (n-prev n)) (x (n-next n)))
    (if p (vector-set! p 3 x) (vector-set! c 1 x))
    (if x (vector-set! x 2 p) (vector-set! c 2 p))
    (vector-set! n 2 #f) (vector-set! n 3 #f)))
(define (lh-link-last! c n)
  (let ((t (lh-tail c)))
    (vector-set! n 2 t) (vector-set! n 3 #f)
    (if t (vector-set! t 3 n) (vector-set! c 1 n))
    (vector-set! c 2 n)))
(define (lh-link-first! c n)
  (let ((h (lh-head c)))
    (vector-set! n 3 h) (vector-set! n 2 #f)
    (if h (vector-set! h 2 n) (vector-set! c 2 n))
    (vector-set! c 1 n)))
(define (lh-move-last! c n) (unless (eq? n (lh-tail c)) (lh-unlink! c n) (lh-link-last! c n) (lh-bump! c)))
(define (lh-move-first! c n) (unless (eq? n (lh-head c)) (lh-unlink! c n) (lh-link-first! c n) (lh-bump! c)))
;; an access-ordered map moves what was just reached to the tail
(define (lh-touch! c n) (when (lh-access? c) (lh-move-last! c n)))

(define (lh-remove-node! c n)
  (hashtable-delete! (lh-tbl c) (n-key n))
  (lh-unlink! c n)
  (lh-bump! c))
(define (lh-clear! c)
  (hashtable-clear! (lh-tbl c))
  (vector-set! c 1 #f) (vector-set! c 2 #f)
  (lh-bump! c))
;; (f node) from one end to the other along the chain as it stands, then the
;; modCount check: LinkedHashMap's forEach and replaceAll, and its views'
;; forEach. A node unlinked under the walk ends it there, as the JDK's does.
(define (lh-walk c rev? f)
  (let ((mc (lh-mc c)))
    (let loop ((n (if rev? (lh-tail c) (lh-head c))))
      (when n (f n) (loop (if rev? (n-prev n) (n-next n)))))
    (unless (fx=? mc (lh-mc c)) (jutil-cme!))
    jolt-nil))
;; the compute family's function: a change it made is a
;; ConcurrentModificationException once it returns
(define (lh-apply-checked c f method . args)
  (let* ((mc (lh-mc c)) (v (apply jolt-fi-call f method args)))
    (unless (fx=? mc (lh-mc c)) (jutil-cme!))
    v))
;; nodes in encounter order, or the reverse
(define (lh-nodes c rev?)
  (let loop ((n (if rev? (lh-head c) (lh-tail c))) (acc '()))
    (if n (loop (if rev? (n-next n) (n-prev n)) (cons n acc)) acc)))

;; a NEW key at the tail (or the head), then the eviction question
(define (lh-insert! c owner k v first?)
  (let ((n (vector k v #f #f)))
    (lh-bump! c)
    (hashtable-set! (lh-tbl c) k n)
    (if first? (lh-link-first! c n) (lh-link-last! c n))
    (lh-after-insert! c owner)
    n))
(define (lh-after-insert! c owner)
  (let ((h (lh-head c)))
    (when (and h (lh-proxy c)
               (jolt-truthy? (record-method-dispatch (lh-proxy c) "removeEldestEntry"
                                                     (jolt-list (lh-entry h)))))
      (when (eq? (lh-node c (n-key h)) h) (lh-remove-node! c h)))))

;; put: an existing key keeps its place (or moves, access-ordered); a new one goes last
(define (lh-put! c owner k v)
  (let ((n (lh-node c k)))
    (if n
        (let ((old (n-val n))) (vector-set! n 1 v) (lh-touch! c n) old)
        (begin (lh-insert! c owner k v #f) jolt-nil))))
;; putFirst / putLast: the mapping ends up at that end, new or not
(define (lh-put-end! c owner k v first?)
  (let ((n (lh-node c k)))
    (if n
        (let ((old (n-val n)))
          (vector-set! n 1 v)
          (if first? (lh-move-first! c n) (lh-move-last! c n))
          old)
        (begin (lh-insert! c owner k v first?) jolt-nil))))
(define (lh-get c k d)
  (let ((n (lh-node c k)))
    (if n (begin (lh-touch! c n) (n-val n)) d)))

;; ---- entries ----------------------------------------------------------------------
;; A live entry is its node: getValue reads it, setValue writes it.
(define (lh-entry n) (make-jhost "lhm-entry" n))
(register-jutil-entry! "lhm-entry" (lambda (e) (let ((n (jhost-state e))) (cons (n-key n) (n-val n)))))
(register-host-methods! "lhm-entry"
  (list (cons "setValue" (lambda (self v) (let* ((n (jhost-state self)) (old (n-val n)))
                                            (vector-set! n 1 v) old)))))
;; firstEntry and the rest answer an unmodifiable snapshot, as the JDK's do
(define (lh-snapshot n) (if n (make-jhost "lhm-snapshot-entry" (cons (n-key n) (n-val n))) jolt-nil))
(register-jutil-entry! "lhm-snapshot-entry" jhost-state)
(register-host-methods! "lhm-snapshot-entry"
  (list (cons "setValue" (lambda (self v) (throw-jvm 'UnsupportedOperationException "not supported")))))

;; ---- the map ------------------------------------------------------------------------
(define (lhm-chain m) (jhost-state m))
(define (make-lhm access?) (make-jhost "linkedhashmap" (lh-new access?)))
(define (lhm-fill! m src)
  (for-each (lambda (e) (lh-put! (lhm-chain m) m (jolt-nth e 0) (jolt-nth e 1)))
            (seq->list (jolt-seq src))))
(define lhm-ctor
  (case-lambda
    (() (make-lhm #f))
    ((x) (let ((m (make-lhm #f))) (unless (number? x) (lhm-fill! m x)) m))
    ((cap lf) (make-lhm #f))
    ((cap lf access-order?) (make-lhm (jolt-truthy? access-order?)))))
(register-class-ctor! "LinkedHashMap" lhm-ctor)
(register-class-ctor! "java.util.LinkedHashMap" lhm-ctor)
(register-class-statics! "java.util.LinkedHashMap"
  (list (cons "newLinkedHashMap"
              (lambda (n)
                (when (< (jnum->exact n) 0)
                  (throw-jvm 'IllegalArgumentException
                             (string-append "Negative number of mappings: " (jolt-str-render-one n))))
                (make-lhm #f)))))

(define (lh-fi f method . args) (apply jolt-fi-call f method args))
;; the Map methods over CHAIN, OWNER the jhost the eviction question is asked for;
;; REV? reverses what the view's own ends mean (reversed())
(define (lhm-methods chain-of rev?)
  (define (c self) (chain-of self))
  (define (owner self) self)
  (list
    (cons "size" (lambda (self) (lh-size (c self))))
    (cons "isEmpty" (lambda (self) (fx=? 0 (lh-size (c self)))))
    (cons "get" (lambda (self k) (lh-get (c self) k jolt-nil)))
    (cons "getOrDefault" (lambda (self k d) (lh-get (c self) k d)))
    (cons "containsKey" (lambda (self k) (and (lh-node (c self) k) #t)))
    (cons "containsValue" (lambda (self v)
                            (and (exists (lambda (n) (jolt=2 v (n-val n))) (lh-nodes (c self) #f)) #t)))
    (cons "put" (lambda (self k v) (lh-put! (c self) (owner self) k v)))
    (cons "putFirst" (lambda (self k v) (lh-put-end! (c self) (owner self) k v (not rev?))))
    (cons "putLast" (lambda (self k v) (lh-put-end! (c self) (owner self) k v rev?)))
    (cons "putAll" (lambda (self src)
                     (for-each (lambda (e) (lh-put! (c self) (owner self) (jolt-nth e 0) (jolt-nth e 1)))
                               (seq->list (jolt-seq src)))
                     jolt-nil))
    (cons "remove" (case-lambda
                     ((self k) (let ((n (lh-node (c self) k)))
                                 (if n (begin (lh-remove-node! (c self) n) (n-val n)) jolt-nil)))
                     ((self k v) (let ((n (lh-node (c self) k)))
                                   (if (and n (jolt=2 v (n-val n)))
                                       (begin (lh-remove-node! (c self) n) #t)
                                       #f)))))
    (cons "clear" (lambda (self) (lh-clear! (c self)) jolt-nil))
    (cons "putIfAbsent" (lambda (self k v)
                          (let ((n (lh-node (c self) k)))
                            (cond ((not n) (lh-insert! (c self) (owner self) k v #f) jolt-nil)
                                  ((jolt-nil? (n-val n)) (vector-set! n 1 v) (lh-touch! (c self) n) jolt-nil)
                                  (else (lh-touch! (c self) n) (n-val n))))))
    (cons "computeIfAbsent" (lambda (self k f)
                              (let ((n (lh-node (c self) k)))
                                (if (and n (not (jolt-nil? (n-val n))))
                                    (begin (lh-touch! (c self) n) (n-val n))
                                    (let ((v (lh-apply-checked (c self) f "apply" k)))
                                      (cond ((jolt-nil? v) jolt-nil)
                                            (n (vector-set! n 1 v) (lh-touch! (c self) n) v)
                                            (else (lh-insert! (c self) (owner self) k v #f) v)))))))
    (cons "computeIfPresent" (lambda (self k f)
                               (let ((n (lh-node (c self) k)))
                                 (if (or (not n) (jolt-nil? (n-val n)))
                                     jolt-nil
                                     (let ((v (lh-apply-checked (c self) f "apply" k (n-val n))))
                                       (if (jolt-nil? v)
                                           (begin (lh-remove-node! (c self) n) jolt-nil)
                                           (begin (vector-set! n 1 v) (lh-touch! (c self) n) v)))))))
    (cons "compute" (lambda (self k f)
                      (let* ((n (lh-node (c self) k))
                             (v (lh-apply-checked (c self) f "apply" k (if n (n-val n) jolt-nil))))
                        (cond ((and n (jolt-nil? v)) (lh-remove-node! (c self) n) jolt-nil)
                              ((jolt-nil? v) jolt-nil)
                              (n (vector-set! n 1 v) (lh-touch! (c self) n) v)
                              (else (lh-insert! (c self) (owner self) k v #f) v)))))
    (cons "merge" (lambda (self k v f)
                    (let ((n (lh-node (c self) k)))
                      (if (or (not n) (jolt-nil? (n-val n)))
                          (if n
                              (begin (vector-set! n 1 v) (lh-touch! (c self) n) v)
                              (begin (lh-insert! (c self) (owner self) k v #f) v))
                          (let ((nv (lh-apply-checked (c self) f "apply" (n-val n) v)))
                            (if (jolt-nil? nv)
                                (begin (lh-remove-node! (c self) n) jolt-nil)
                                (begin (vector-set! n 1 nv) (lh-touch! (c self) n) nv)))))))
    (cons "replace" (case-lambda
                      ((self k v) (let ((n (lh-node (c self) k)))
                                    (if n
                                        (let ((old (n-val n))) (vector-set! n 1 v) (lh-touch! (c self) n) old)
                                        jolt-nil)))
                      ((self k old v) (let ((n (lh-node (c self) k)))
                                        (if (and n (jolt=2 old (n-val n)))
                                            (begin (vector-set! n 1 v) (lh-touch! (c self) n) #t)
                                            #f)))))
    (cons "replaceAll" (lambda (self f)
                         (lh-walk (c self) rev? (lambda (n) (vector-set! n 1 (lh-fi f "apply" (n-key n) (n-val n)))))))
    (cons "forEach" (lambda (self f)
                      (lh-walk (c self) rev? (lambda (n) (lh-fi f "accept" (n-key n) (n-val n))))))
    (cons "firstEntry" (lambda (self) (lh-snapshot (if rev? (lh-tail (c self)) (lh-head (c self))))))
    (cons "lastEntry" (lambda (self) (lh-snapshot (if rev? (lh-head (c self)) (lh-tail (c self))))))
    (cons "pollFirstEntry" (lambda (self) (let ((n (if rev? (lh-tail (c self)) (lh-head (c self)))))
                                            (when n (lh-remove-node! (c self) n))
                                            (lh-snapshot n))))
    (cons "pollLastEntry" (lambda (self) (let ((n (if rev? (lh-head (c self)) (lh-tail (c self)))))
                                           (when n (lh-remove-node! (c self) n))
                                           (lh-snapshot n))))
    (cons "keySet" (lambda (self) (jutil-view self 0 (lambda () (lh-view "lhm-keyset" (c self) rev?)))))
    (cons "values" (lambda (self) (jutil-view self 1 (lambda () (lh-view "lhm-values" (c self) rev?)))))
    (cons "entrySet" (lambda (self) (jutil-view self 2 (lambda () (lh-view "lhm-entryset" (c self) rev?)))))
    (cons "sequencedKeySet" (lambda (self) (lh-view "lhm-keyset" (c self) rev?)))
    (cons "sequencedValues" (lambda (self) (lh-view "lhm-values" (c self) rev?)))
    (cons "sequencedEntrySet" (lambda (self) (lh-view "lhm-entryset" (c self) rev?)))))

(register-host-methods! "linkedhashmap"
  (append (lhm-methods lhm-chain #f)
          (list (cons "reversed" (lambda (self) (make-jhost "lhm-reversed" self)))
                (cons "clone" (lambda (self)
                                (let* ((src (lhm-chain self)) (m (make-lhm (lh-access? src))))
                                  (for-each (lambda (n) (lh-insert! (lhm-chain m) m (n-key n) (n-val n) #f))
                                            (lh-nodes src #f))
                                  m)))
                ;; the JDK's default: keep everything
                (cons "removeEldestEntry" (lambda (self e) #f)))))
;; reversed(): the same mappings the other way round; a put lands where the
;; map's own put does, which is this view's front
(define (lhm-reversed-base v) (jhost-state v))
(register-host-methods! "lhm-reversed"
  (append (lhm-methods (lambda (v) (lhm-chain (lhm-reversed-base v))) #t)
          (list (cons "reversed" (lambda (self) (lhm-reversed-base self))))))

;; a proxy over a LinkedHashMap is the one asked removeEldestEntry
(proxy-attach-hook! "linkedhashmap" (lambda (base proxy) (vector-set! (lhm-chain base) 4 proxy)))

;; ---- the views --------------------------------------------------------------------
;; #(chain reversed?), tag: lhm-keyset / lhm-values / lhm-entryset
(define (lh-view tag c rev?) (make-jhost tag (vector c rev?)))
(define (lhv-chain v) (vector-ref (jhost-state v) 0))
(define (lhv-rev? v) (vector-ref (jhost-state v) 1))
(define (lhv-nodes v) (lh-nodes (lhv-chain v) (lhv-rev? v)))
(define (lh-project tag)
  (cond ((string=? tag "lhm-keyset") n-key)
        ((string=? tag "lhm-values") n-val)
        (else lh-entry)))
(define (lhv-elems v) (map (lh-project (jhost-tag v)) (lhv-nodes v)))
;; the node a view element names, or #f
(define (lhv-node-of v x)
  (let ((c (lhv-chain v)) (t (jhost-tag v)))
    (cond ((string=? t "lhm-keyset") (lh-node c x))
          ((string=? t "lhm-values") (find (lambda (n) (jolt=2 x (n-val n))) (lhv-nodes v)))
          (else (and (or (jolt-map-entry? x) (jutil-entry? x))
                     (let ((n (lh-node c (jolt-nth x 0))))
                       (and n (jolt=2 (n-val n) (jolt-nth x 1)) n)))))))
(define lh-iterator-classes
  '(("lhm-keyset" . "java.util.LinkedHashMap$LinkedKeyIterator")
    ("lhm-values" . "java.util.LinkedHashMap$LinkedValueIterator")
    ("lhm-entryset" . "java.util.LinkedHashMap$LinkedEntryIterator")))
(for-each
  (lambda (row)
    (let* ((tag (car row)) (it-tag (string-append tag "-iterator")))
      (hashtable-set! jhost-tag->fqn it-tag (cdr row))
      (register-jutil-iterator! it-tag)
      (register-host-methods! tag
        (list (cons "size" (lambda (self) (lh-size (lhv-chain self))))
              (cons "isEmpty" (lambda (self) (fx=? 0 (lh-size (lhv-chain self)))))
              (cons "contains" (lambda (self x) (and (lhv-node-of self x) #t)))
              (cons "remove" (lambda (self x)
                               (let ((n (lhv-node-of self x)))
                                 (and n (begin (lh-remove-node! (lhv-chain self) n) #t)))))
              (cons "clear" (lambda (self) (lh-clear! (lhv-chain self)) jolt-nil))
              (cons "add" (lambda (self x) (throw-jvm 'UnsupportedOperationException jolt-nil)))
              (cons "iterator"
                    (lambda (self)
                      (let ((c (lhv-chain self)))
                        (make-jutil-ff-iterator it-tag (lhv-nodes self)
                                                (lambda (n) (when (eq? (lh-node c (n-key n)) n) (lh-remove-node! c n)))
                                                (lh-project tag) c 5))))
              ;; LinkedKeySet / LinkedValues / LinkedEntrySet walk the chain
              (cons "forEach" (lambda (self f)
                                (let ((p (lh-project tag)))
                                  (lh-walk (lhv-chain self) (lhv-rev? self) (lambda (n) (jolt-fi-call f "accept" (p n)))))))
              (cons "toArray" (case-lambda
                                ((self) (jcoll-to-array (lhv-elems self) '()))
                                ((self a) (jcoll-to-array (lhv-elems self) (list a)))))
              (cons "reversed" (lambda (self) (lh-view tag (lhv-chain self) (not (lhv-rev? self)))))))))
  lh-iterator-classes)
(for-each (lambda (p) (hashtable-set! jhost-tag->fqn (car p) (cdr p)))
  '(("linkedhashmap" . "java.util.LinkedHashMap")
    ("lhm-reversed" . "java.util.LinkedHashMap$ReversedLinkedHashMapView")
    ("lhm-keyset" . "java.util.LinkedHashMap$LinkedKeySet")
    ("lhm-values" . "java.util.LinkedHashMap$LinkedValues")
    ("lhm-entryset" . "java.util.LinkedHashMap$LinkedEntrySet")
    ("lhm-entry" . "java.util.LinkedHashMap$Entry")
    ("lhm-snapshot-entry" . "jdk.internal.util.NullableKeyValueHolder")))
(define (lhm-entries m) (map lh-entry (lh-nodes (lhm-chain m) #f)))
(register-jutil-coll! "linkedhashmap" 'map lhm-entries)
(register-jutil-coll! "lhm-reversed" 'map (lambda (v) (map lh-entry (lh-nodes (lhm-chain (lhm-reversed-base v)) #t))))
(register-jutil-coll! "lhm-keyset" 'set lhv-elems)
(register-jutil-coll! "lhm-values" 'coll lhv-elems)
(register-jutil-coll! "lhm-entryset" 'entries lhv-elems)

;; ---- the set ------------------------------------------------------------------------
;; A LinkedHashSet is a chain whose values are unused; its reversed() a view
(define (lhs? x) (and (jhost? x) (string=? (jhost-tag x) "linkedhashset")))
(define (make-lhs) (make-jhost "linkedhashset" (lh-new #f)))
(define (lhs-add! c x) (if (lh-node c x) #f (begin (lh-insert! c #f x #t #f) #t)))
(define lhs-ctor
  (case-lambda
    (() (make-lhs))
    ((x) (let ((s (make-lhs)))
           (unless (number? x) (for-each (lambda (e) (lhs-add! (jhost-state s) e)) (seq->list (jolt-seq x))))
           s))
    ((cap lf) (make-lhs))))
(register-class-ctor! "LinkedHashSet" lhs-ctor)
(register-class-ctor! "java.util.LinkedHashSet" lhs-ctor)
(register-class-statics! "java.util.LinkedHashSet"
  (list (cons "newLinkedHashSet"
              (lambda (n)
                (when (< (jnum->exact n) 0)
                  (throw-jvm 'IllegalArgumentException
                             (string-append "Negative number of elements: " (jolt-str-render-one n))))
                (make-lhs)))))
(define (lhs-chain-of s) (if (lhs? s) (jhost-state s) (jhost-state (jhost-state s))))
(define (lhs-rev? s) (not (lhs? s)))
(define (lhs-elems s) (map n-key (lh-nodes (lhs-chain-of s) (lhs-rev? s))))
(define (lhs-end s first?)   ; the node at the set's own first (or last) end
  (let ((c (lhs-chain-of s)))
    (if (eq? first? (not (lhs-rev? s))) (lh-head c) (lh-tail c))))
(define (lhs-no-such) (throw-jvm 'NoSuchElementException jolt-nil))
(define lhs-methods
  (list (cons "add" (lambda (self x) (lhs-add! (lhs-chain-of self) x)))
        (cons "addFirst" (lambda (self x) (lh-put-end! (lhs-chain-of self) #f x #t (not (lhs-rev? self))) jolt-nil))
        (cons "addLast" (lambda (self x) (lh-put-end! (lhs-chain-of self) #f x #t (lhs-rev? self)) jolt-nil))
        (cons "getFirst" (lambda (self) (let ((n (lhs-end self #t))) (if n (n-key n) (lhs-no-such)))))
        (cons "getLast" (lambda (self) (let ((n (lhs-end self #f))) (if n (n-key n) (lhs-no-such)))))
        (cons "removeFirst" (lambda (self) (let ((n (lhs-end self #t)))
                                             (unless n (lhs-no-such))
                                             (lh-remove-node! (lhs-chain-of self) n) (n-key n))))
        (cons "removeLast" (lambda (self) (let ((n (lhs-end self #f)))
                                            (unless n (lhs-no-such))
                                            (lh-remove-node! (lhs-chain-of self) n) (n-key n))))
        (cons "remove" (lambda (self x) (let ((n (lh-node (lhs-chain-of self) x)))
                                          (and n (begin (lh-remove-node! (lhs-chain-of self) n) #t)))))
        (cons "contains" (lambda (self x) (and (lh-node (lhs-chain-of self) x) #t)))
        (cons "containsAll" (lambda (self c) (for-all (lambda (x) (and (lh-node (lhs-chain-of self) x) #t))
                                                      (seq->list (jolt-seq c)))))
        (cons "addAll" (lambda (self c) (fold-left (lambda (ch x) (or (lhs-add! (lhs-chain-of self) x) ch))
                                                   #f (seq->list (jolt-seq c)))))
        (cons "removeAll" (lambda (self c)
                            (fold-left (lambda (ch x)
                                         (let ((n (lh-node (lhs-chain-of self) x)))
                                           (if n (begin (lh-remove-node! (lhs-chain-of self) n) #t) ch)))
                                       #f (seq->list (jolt-seq c)))))
        (cons "retainAll" (lambda (self c)
                            (let ((keep (seq->list (jolt-seq c))))
                              (fold-left (lambda (ch n)
                                           (if (exists (lambda (k) (jolt=2 k (n-key n))) keep)
                                               ch
                                               (begin (lh-remove-node! (lhs-chain-of self) n) #t)))
                                         #f (lh-nodes (lhs-chain-of self) #f)))))
        ;; Collection.removeIf's default, through the iterator
        (cons "removeIf" (lambda (self pred) (jutil-walk-remove-if (lhs-iterator self) pred)))
        (cons "size" (lambda (self) (lh-size (lhs-chain-of self))))
        (cons "isEmpty" (lambda (self) (fx=? 0 (lh-size (lhs-chain-of self)))))
        (cons "clear" (lambda (self) (lh-clear! (lhs-chain-of self)) jolt-nil))
        (cons "iterator" (lambda (self) (lhs-iterator self)))
        ;; Iterable.forEach, as LinkedHashSet keeps it: over the iterator
        (cons "forEach" (lambda (self f) (jutil-walk-for-each (lhs-iterator self) f)))
        (cons "toArray" (case-lambda
                          ((self) (jcoll-to-array (lhs-elems self) '()))
                          ((self a) (jcoll-to-array (lhs-elems self) (list a)))))))
(define (lhs-iterator self)
  (let ((c (lhs-chain-of self)))
    (make-jutil-ff-iterator "lhs-iterator" (lh-nodes c (lhs-rev? self))
                            (lambda (n) (when (eq? (lh-node c (n-key n)) n) (lh-remove-node! c n)))
                            n-key c 5)))
(register-host-methods! "linkedhashset"
  (append lhs-methods
          (list (cons "reversed" (lambda (self) (make-jhost "lhs-reversed" self)))
                (cons "clone" (lambda (self) (lhs-ctor (list->cseq (lhs-elems self))))))))
(register-host-methods! "lhs-reversed"
  (append lhs-methods (list (cons "reversed" (lambda (self) (jhost-state self))))))
(for-each (lambda (p) (hashtable-set! jhost-tag->fqn (car p) (cdr p)))
  '(("linkedhashset" . "java.util.LinkedHashSet")
    ("lhs-reversed" . "java.util.LinkedHashSet$1ReverseLinkedHashSetView")
    ("lhs-iterator" . "java.util.LinkedHashMap$LinkedKeyIterator")))
(register-jutil-iterator! "lhs-iterator")
(register-jutil-coll! "linkedhashset" 'set lhs-elems)
;; every member of the family reads its chain's modCount
(register-modcount! "linkedhashmap" (lambda (m) (lh-mc (lhm-chain m))))
(register-modcount! "lhm-reversed" (lambda (v) (lh-mc (lhm-chain (lhm-reversed-base v)))))
(for-each (lambda (t) (register-modcount! t (lambda (v) (lh-mc (lhv-chain v)))))
          '("lhm-keyset" "lhm-values" "lhm-entryset"))
(for-each (lambda (t) (register-modcount! t (lambda (s) (lh-mc (lhs-chain-of s)))))
          '("linkedhashset" "lhs-reversed"))
(register-jutil-coll! "lhs-reversed" 'set lhs-elems)

;; ---- what the rest of the runtime sees ----------------------------------------------
;; seq / count / contains? / get, one arm each for the family: every tag starts
;; with l, so another value is turned away by its first character. A
;; java.util.jar.Attributes (embedded-manifest.ss) is a LinkedHashMap chain under
;; its own class and answers through these arms too, rather than through arms of
;; its own registered later -- which would be walked ahead of the HashMap
;; family's and put a predicate in front of every HashMap get
;; (test/fastpath_ratio_test.clj hashmap-get).
(define (lh-kind x)
  (and (jhost? x)
       (let ((t (jhost-tag x)))
         (and (fx>? (string-length t) 3)
              (case (string-ref t 0)
                ((#\l) #t)
                ((#\j) (string=? t "jar-attributes"))
                (else #f))
              (cond ((string=? t "linkedhashmap") 'map)
                    ((string=? t "jar-attributes") 'map)
                    ((string=? t "lhm-reversed") 'rmap)
                    ((string=? t "linkedhashset") 'set)
                    ((string=? t "lhs-reversed") 'set)
                    ((member t '("lhm-keyset" "lhm-values" "lhm-entryset")) 'view)
                    (else #f))))))
(define (lh-seq-elems x)
  (case (lh-kind x)
    ((map) (lhm-entries x))
    ((rmap) (map lh-entry (lh-nodes (lhm-chain (lhm-reversed-base x)) #t)))
    ((set) (lhs-elems x))
    (else (lhv-elems x))))
(define (lh-chain-of x)
  (case (lh-kind x)
    ((map) (lhm-chain x))
    ((rmap) (lhm-chain (lhm-reversed-base x)))
    ((set) (lhs-chain-of x))
    (else (lhv-chain x))))
;; chunk by chunk, as RT.seq walks the fail-fast iterator (jutil-ff-seq)
(register-seq-arm! lh-kind (lambda (x) (jutil-ff-seq x (lh-seq-elems x))))
(register-count-arm! lh-kind (lambda (x) (lh-size (lh-chain-of x))))
(register-contains-arm! lh-kind
  (lambda (x k) (if (eq? (lh-kind x) 'view)
                    (and (lhv-node-of x k) #t)
                    (and (lh-node (lh-chain-of x) k) #t))))
;; get on a map is Map.get: it counts as an access
(register-get-arm! (lambda (x) (memq (lh-kind x) '(map rmap)))
  (lambda (m k d) (lh-get (lh-chain-of m) k d)))
