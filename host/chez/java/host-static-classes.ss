;; host-static-classes.ss — instantiable host object classes: ArrayList, HashMap,
;; the String/Reader/Writer/Tokenizer shims, BigInteger/MapEntry ctors, and URL
;; codecs. Holds the tagged-table method dispatch (the (.method ...) arm on a jhost)
;; and the pluggable instance? hook. Loaded after host-static-methods.ss; the
;; `Class/member` static methods live there, the registry core in host-static.ss.

;; ---- java.util.ArrayList ----------------------------------------------------
;; A mutable list backed by a growable Scheme vector. State is #(backing count);
;; .add amortizes O(1) and .get is O(1) (a list backing made both O(n)). medley's
;; stateful transducers (window / partition-between) build one with .add / .size /
;; .toArray / .clear / .remove. (ArrayList.) | (ArrayList. n) | (ArrayList. coll).
;; State: #(backing-vector count head). Logical index i lives at physical
;; head+i. The head offset makes FRONT operations O(1) — removeFirst/poll/pop
;; advance head, addFirst/push retreat into head slack — where shifting the
;; whole vector made the standard deque worklist idiom (.poll + .add, or
;; tools.reader's .remove(0) on its pending-splice LinkedList) O(n^2). When an
;; append finds the physical end full it compacts into the head slack if that
;; is at least half the buffer (each compacted element was paid for by a prior
;; front-removal, so appends stay amortized O(1)), and grows otherwise.
;;
;; Slot 3 is the modCount, as AbstractList's: every structural change (an add,
;; an insert, a remove, a clear) bumps it, and an iterator, a listIterator, a
;; subList and a spliterator each remember the count they started from and
;; raise ConcurrentModificationException when it has moved (tools/failfast-model.pl
;; checks the protocol). A set is no structural change and does not bump. A
;; list restored from an image written before the slot has three slots: it reads
;; as count 0 and gains the slot on its first bump.
(define al-min-cap 8)
(define (al-mc self)
  (let ((st (jhost-state self))) (if (fx>? (vector-length st) 3) (vector-ref st 3) 0)))
(define-syntax al-bump!
  ;; inline: every add and remove pays it
  (syntax-rules ()
    ((_ self) (let ((st (jhost-state self)))
                (if (fx>? (vector-length st) 3)
                    (vector-set! st 3 (fxand (fx+ (vector-ref st 3) 1) #x3fffffff))
                    (al-upgrade-mc! self))))))
(define (al-upgrade-mc! self)
  (let ((st (jhost-state self)))
    (jhost-state-set! self (vector (vector-ref st 0) (vector-ref st 1) (vector-ref st 2) 1))))
(define (al-vec self) (vector-ref (jhost-state self) 0))
(define (al-cnt self) (vector-ref (jhost-state self) 1))
(define (al-cnt! self n) (vector-set! (jhost-state self) 1 n))
(define (al-head self) (vector-ref (jhost-state self) 2))
(define (al-head! self h) (vector-set! (jhost-state self) 2 h))
(define (al-ref self i) (vector-ref (al-vec self) (fx+ (al-head self) i)))
;; get/set past the end is an IndexOutOfBoundsException, never the buffer's
;; spare slot.
(define (al-check-index! self i)
  (unless (and (fixnum? i) (fx>=? i 0) (fx<? i (al-cnt self)))
    (throw-jvm 'IndexOutOfBoundsException
               (string-append "Index " (number->string i) " out of bounds for length "
                              (number->string (al-cnt self))))))
(define (al-set! self i x) (vector-set! (al-vec self) (fx+ (al-head self) i) x))
(define (make-arraylist xs)               ; xs: a Scheme list of initial elements
  (let* ((n (length xs)) (cap (fxmax al-min-cap n)) (v (make-vector cap jolt-nil)))
    (let loop ((i 0) (xs xs)) (when (pair? xs) (vector-set! v i (car xs)) (loop (fx+ i 1) (cdr xs))))
    (make-jhost "arraylist" (vector v n 0 0))))
;; room for one more element at the physical tail: compact into head slack or
;; grow (doubling), either way head returns to 0.
(define (al-ensure-tail! self)
  (let* ((v (al-vec self)) (n (al-cnt self)) (h (al-head self)))
    (when (fx=? (fx+ h n) (vector-length v))
      (if (fx>=? h n)
          (begin
            (do ((i 0 (fx+ i 1))) ((fx=? i n)) (vector-set! v i (vector-ref v (fx+ h i))))
            (do ((i n (fx+ i 1))) ((fx=? i (fx+ h n))) (vector-set! v i jolt-nil))
            (al-head! self 0))
          (let ((nv (make-vector (fxmax al-min-cap (fx* 2 (vector-length v))) jolt-nil)))
            (do ((i 0 (fx+ i 1))) ((fx=? i n)) (vector-set! nv i (vector-ref v (fx+ h i))))
            (vector-set! (jhost-state self) 0 nv)
            (al-head! self 0))))))
(define (al-push! self x)
  (al-bump! self)                       ; first: it may replace an old image's state
  ;; the state read once: an append with room is a store and a count
  (let* ((st (jhost-state self)) (v (vector-ref st 0)) (n (vector-ref st 1))
         (i (fx+ (vector-ref st 2) n)))
    (if (fx<? i (vector-length v))
        (begin (vector-set! v i x) (vector-set! st 1 (fx+ n 1)))
        (begin
          (al-ensure-tail! self)
          (vector-set! (al-vec self) (fx+ (al-head self) n) x)
          (al-cnt! self (fx+ n 1))))))
(define (al-insert-at! self i x)
  (al-bump! self)
  (let ((n (al-cnt self)) (h (al-head self)))
    (if (and (fx=? i 0) (fx>? h 0))
        (begin (al-head! self (fx- h 1))
               (vector-set! (al-vec self) (fx- h 1) x)
               (al-cnt! self (fx+ n 1)))
        (begin
          (al-ensure-tail! self)
          (let ((v (al-vec self)) (h (al-head self)))
            (let shift ((j (fx+ h n))) (when (fx>? j (fx+ h i)) (vector-set! v j (vector-ref v (fx- j 1))) (shift (fx- j 1))))
            (vector-set! v (fx+ h i) x) (al-cnt! self (fx+ n 1)))))))
(define (al-remove-at! self i)
  (al-bump! self)
  (let* ((st (jhost-state self)) (v (vector-ref st 0)) (n (vector-ref st 1)) (h (vector-ref st 2)))
    (if (fx=? i 0)
        (begin (vector-set! v h jolt-nil)
               (vector-set! st 1 (fx- n 1))
               ;; empty resets head so a drained deque reuses the whole buffer
               (vector-set! st 2 (if (fx=? n 1) 0 (fx+ h 1))))
        (begin
          (let shift ((j (fx+ h i))) (when (fx<? j (fx+ h (fx- n 1))) (vector-set! v j (vector-ref v (fx+ j 1))) (shift (fx+ j 1))))
          (vector-set! v (fx+ h (fx- n 1)) jolt-nil) (vector-set! st 1 (fx- n 1))))))
(define (al->list self)                   ; the `count` live elements as a Scheme list
  (let ((v (al-vec self)) (h (al-head self)))
    (let loop ((i (fx- (al-cnt self) 1)) (acc '())) (if (fx<? i 0) acc (loop (fx- i 1) (cons (vector-ref v (fx+ h i)) acc))))))
;; Collection.toArray, both overloads, for any modeled collection that can hand
;; over its live elements in iteration order. Written against a LIST rather than
;; against ArrayList's backing vector because toArray is a Collection method, not
;; a List one: HashSet inherits exactly this contract and shares the code below,
;; and whatever is modeled next needs only its own element list.
(define (jcoll-to-array elems args)
  (let ((n (length elems)))
    (cond
      ;; Collection.toArray() returns a new Object[], never a Clojure vector.
      ((null? args)
       (make-jolt-array (list->vector elems) 'object))
      ((null? (cdr args))
       (let ((dst (car args)))
         (unless (and (jolt-array? dst)
                      (na-ref-kind? (jolt-array-kind dst)))
           (jolt-cast-throw dst "[Ljava.lang.Object;"))
         (let ((cap (ja-len dst)))
           (if (fx<? cap n)
               ;; toArray(T[]) answers an array of the argument's own runtime
               ;; type, so a too-small String[] is replaced by a String[].
               (make-jolt-array (list->vector elems) (jolt-array-kind dst))
               (begin
                 (let fill ((i 0) (es elems))
                   (unless (null? es)
                     (ja-set! dst i (car es))
                     (fill (fx+ i 1) (cdr es))))
                 ;; The Java overload marks the logical end when the caller's
                 ;; destination has spare capacity; later cells stay untouched.
                 (when (fx>? cap n) (ja-set! dst n jolt-nil))
                 dst)))))
      (else
       (throw-jvm 'IllegalArgumentException
                  "Collection.toArray expects zero or one argument")))))
(register-class-ctor! "ArrayList"
  (lambda args
    (cond ((null? args) (make-arraylist '()))
          ((number? (car args)) (make-arraylist '()))   ; initial capacity, ignored
          (else (make-arraylist (seq->list (jolt-seq (car args))))))))
(register-class-ctor! "java.util.ArrayList"
  (lambda args
    (cond ((null? args) (make-arraylist '()))
          ((number? (car args)) (make-arraylist '()))
          (else (make-arraylist (seq->list (jolt-seq (car args))))))))
;; toString / equals / hashCode of the collection shims come from the shared
;; java.util registry (java/jutil-colls.ss); each shim registers below.

;; The default methods java.lang.Iterable, java.util.Collection and java.util.List
;; give every collection, over a shim's element list. The function argument is a
;; java.util.function interface, so it goes through jolt-fi-call: a Clojure fn is
;; invoked (1.12 coerces it) and a reify implementing the interface has its
;; method called. A Predicate answer counts by truthiness — the JVM would cast it
;; to Boolean, and a fn returning a non-boolean raises there (a superset here).
(define (jcoll-for-each elems f)
  (for-each (lambda (x) (jolt-fi-call f "accept" x)) elems)
  jolt-nil)
(define (jcoll-keep elems pred)
  (filter (lambda (x) (not (jolt-truthy? (jolt-fi-call pred "test" x)))) elems))
;; List.sort(Comparator): a nil comparator is the natural order; the sort is
;; stable, as the JDK's is.
(define (jcoll-sorted elems cmp)
  (list-sort (cmp->less (if (jolt-nil? cmp) jolt-compare cmp)) elems))
;; Replace the contents: a structural change (sort, removeIf); al-fill! writes
;; the same number of elements back in place, which is not one (a LinkedList's
;; sort and replaceAll go element by element through set on the JDK).
(define (al-reset! self xs)
  (al-bump! self)
  (vector-set! (jhost-state self) 0 (make-vector (fxmax al-min-cap (length xs)) jolt-nil))
  (al-cnt! self 0) (al-head! self 0)
  (for-each (lambda (x) (al-ensure-tail! self) (al-set! self (al-cnt self) x) (al-cnt! self (fx+ (al-cnt self) 1))) xs))
(define (al-fill! self xs)
  (let loop ((i 0) (xs xs)) (unless (null? xs) (al-set! self i (car xs)) (loop (fx+ i 1) (cdr xs)))))
(define (al-clear! self)
  (al-bump! self)
  (vector-set! (jhost-state self) 0 (make-vector al-min-cap jolt-nil))
  (al-cnt! self 0) (al-head! self 0))
(define (al-cme!) (throw-jvm 'ConcurrentModificationException jolt-nil))
(define (al-check! self expected) (unless (fx=? expected (al-mc self)) (al-cme!)))
(define (al-linked? self) (string=? (jhost-tag self) "linkedlist"))

;; ---- the live iterators ----------------------------------------------------
;; ArrayList$Itr / $ListItr, LinkedList$ListItr, Arrays$ArrayItr,
;; ImmutableCollections$ListItr: a cursor into the list as it is now, the index
;; last handed out, and the modCount expected (#f for a list with no structural
;; changes to watch). hasNext never checks the count — ArrayList's is
;; cursor != size and LinkedList's cursor < size, which is why removing the
;; second-to-last element inside a for-each ends the loop quietly on both and
;; why a cleared ArrayList's iterator still answers hasNext — and next checks it
;; before anything else. State #(list cursor lastRet expected kind) where kind
;; is ne (ArrayList), lt (LinkedList) or fixed (no structural changes: remove
;; and add refuse).
(define (make-al-iterator self tag)
  (let ((t (jhost-tag self)))
    ;; slot 5: where a forEachRemaining began
    (make-jhost tag (vector self 0 -1
                            (and (member t '("arraylist" "linkedlist")) (al-mc self))
                            (cond ((string=? t "linkedlist") 'lt)
                                  ((string=? t "arraylist") 'ne)
                                  (else 'fixed))
                            0))))
(define (ali-l it) (vector-ref (jhost-state it) 0))
(define (ali-c it) (vector-ref (jhost-state it) 1))
(define (ali-c! it v) (vector-set! (jhost-state it) 1 v))
(define (ali-lr it) (vector-ref (jhost-state it) 2))
(define (ali-lr! it v) (vector-set! (jhost-state it) 2 v))
(define (ali-exp it) (vector-ref (jhost-state it) 3))
(define (ali-exp! it v) (vector-set! (jhost-state it) 3 v))
(define (ali-kind it) (vector-ref (jhost-state it) 4))
(define (ali-check! it) (let ((e (ali-exp it))) (when e (al-check! (ali-l it) e))))
;; hasNext and next are the hot pair: one read of the iterator's state and one
;; of the list's each, no accessor calls
(define (ali-has-next? it)
  (let* ((st (jhost-state it)) (c (vector-ref st 1)) (n (vector-ref (jhost-state (vector-ref st 0)) 1)))
    (if (eq? (vector-ref st 4) 'ne) (not (fx=? c n)) (fx<? c n))))
(define (ali-next it)
  (let* ((st (jhost-state it)) (lst (jhost-state (vector-ref st 0))) (e (vector-ref st 3)))
    (when (and e (not (fx=? e (if (fx>? (vector-length lst) 3) (vector-ref lst 3) 0)))) (al-cme!))
    (let ((i (vector-ref st 1)))
      (unless (and (fx>=? i 0) (fx<? i (vector-ref lst 1))) (throw-jvm 'NoSuchElementException jolt-nil))
      (vector-set! st 1 (fx+ i 1)) (vector-set! st 2 i)
      (vector-ref (vector-ref lst 0) (fx+ (vector-ref lst 2) i)))))
(define (ali-refuse-fixed it) (when (eq? (ali-kind it) 'fixed) (throw-jvm 'UnsupportedOperationException "remove")))
(define (ali-remove it)
  (ali-refuse-fixed it)
  ;; ArrayList refuses a remove with nothing handed out before it checks the
  ;; count; LinkedList checks the count first
  (if (eq? (ali-kind it) 'lt)
      (begin (ali-check! it) (when (fx<? (ali-lr it) 0) (throw-jvm 'IllegalStateException jolt-nil)))
      (begin (when (fx<? (ali-lr it) 0) (throw-jvm 'IllegalStateException jolt-nil)) (ali-check! it)))
  (let ((l (ali-l it)) (lr (ali-lr it)))
    (al-remove-at! l lr)
    (ali-c! it lr) (ali-lr! it -1) (ali-exp! it (al-mc l))
    jolt-nil))
(define (ali-c-start it) (vector-ref (jhost-state it) 5))
(define (ali-for-each-remaining it f)
  (when (jolt-nil? f) (throw-jvm 'NullPointerException jolt-nil))
  (vector-set! (jhost-state it) 5 (ali-c it))
  (let* ((l (ali-l it)) (e (ali-exp it)) (n0 (al-cnt l)))
    ;; run while nothing changed, then check once (ArrayList reads up to the size
    ;; it started with, LinkedList up to the size as it goes)
    (let loop ()
      (let ((i (ali-c it)))
        (when (and (fx<? i (if (eq? (ali-kind it) 'lt) (al-cnt l) n0)) (or (not e) (fx=? e (al-mc l))))
          (ali-c! it (fx+ i 1)) (ali-lr! it i)
          (jolt-fi-call f "accept" (al-ref l i))
          (loop))))
    ;; ArrayList's checks only when it had elements left to walk; LinkedList's always
    (when (or (eq? (ali-kind it) 'lt) (fx<? (ali-c-start it) n0)) (ali-check! it))
    jolt-nil))
(define (ali-previous it)
  (ali-check! it)
  (let ((i (fx- (ali-c it) 1)))
    (when (fx<? i 0) (throw-jvm 'NoSuchElementException jolt-nil))
    (ali-c! it i) (ali-lr! it i)
    (al-ref (ali-l it) i)))
(define (ali-set it x)
  (when (eq? (ali-kind it) 'fixed)
    (unless (string=? (jhost-tag (ali-l it)) "arrays-aslist") (throw-jvm 'UnsupportedOperationException "set")))
  (when (fx<? (ali-lr it) 0) (throw-jvm 'IllegalStateException jolt-nil))
  (ali-check! it)
  (al-set! (ali-l it) (ali-lr it) x)
  jolt-nil)
(define (ali-add it x)
  (when (eq? (ali-kind it) 'fixed) (throw-jvm 'UnsupportedOperationException "add"))
  (ali-check! it)
  (let ((l (ali-l it)) (i (ali-c it)))
    (al-insert-at! l i x)
    (ali-c! it (fx+ i 1)) (ali-lr! it -1) (ali-exp! it (al-mc l))
    jolt-nil))
(define al-itr-methods
  (list (cons "hasNext" ali-has-next?) (cons "next" ali-next) (cons "remove" ali-remove)
        (cons "forEachRemaining" ali-for-each-remaining)))
(define al-listitr-methods
  (append al-itr-methods
          (list (cons "hasPrevious" (lambda (self) (fx>? (ali-c self) 0)))
                (cons "previous" ali-previous)
                (cons "nextIndex" (lambda (self) (->num (ali-c self))))
                (cons "previousIndex" (lambda (self) (->num (fx- (ali-c self) 1))))
                (cons "set" ali-set) (cons "add" ali-add))))
(define al-iterator-classes
  '(("arraylist-iterator" "java.util.ArrayList$Itr" #f)
    ("arraylist-listitr" "java.util.ArrayList$ListItr" #t)
    ("linkedlist-iterator" "java.util.LinkedList$ListItr" #t)
    ("arrays-aslist-iterator" "java.util.Arrays$ArrayItr" #f)
    ("immutable-list-iterator" "java.util.ImmutableCollections$ListItr" #t)))
(for-each (lambda (r)
            (hashtable-set! jhost-tag->fqn (car r) (cadr r))
            (jch-register-supers! (cadr r) (if (caddr r) '("java.util.ListIterator") '("java.util.Iterator")))
            (register-host-methods! (car r) (if (caddr r) al-listitr-methods al-itr-methods)))
          al-iterator-classes)
;; iterator-seq's chunk fill (records-dispatch.ss): up to 32 rounds of hasNext
;; and next with nothing between them, so the count is checked once and the
;; elements copied; any state where next would throw goes back to the loop.
(define (ali-fill it buf)
    (let* ((st (jhost-state it)) (lst (jhost-state (vector-ref st 0)))
           (e (vector-ref st 3)) (c (vector-ref st 1)) (size (vector-ref lst 1)))
      (and (or (not e) (fx=? e (if (fx>? (vector-length lst) 3) (vector-ref lst 3) 0)))
           (fx>=? c 0) (fx<=? c size)
           (let ((n (fxmin 32 (fx- size c))) (v (vector-ref lst 0)) (base (fx+ (vector-ref lst 2) c)))
             (do ((j 0 (fx+ j 1))) ((fx=? j n)) (vector-set! buf j (vector-ref v (fx+ base j))))
             (when (fx>? n 0)
               (vector-set! st 1 (fx+ c n))
               (vector-set! st 2 (fx+ c (fx- n 1))))
             n))))
(register-iterator-seq-fill! (map car al-iterator-classes) ali-fill ali-next)
;; (seq it): what the iterator would hand out from here, as the list is now
(register-seq-arm-last! (lambda (x) (and (jhost? x) (assoc (jhost-tag x) al-iterator-classes) #t))
  (lambda (it)
    (let ((l (ali-l it)))
      (let loop ((i (fx- (al-cnt l) 1)) (acc '()))
        (if (fx<? i (ali-c it)) (if (null? acc) jolt-nil (list->cseq acc)) (loop (fx- i 1) (cons (al-ref l i) acc)))))))
(define (al-iterator self)
  (make-al-iterator self (cond ((string=? (jhost-tag self) "linkedlist") "linkedlist-iterator")
                               ((string=? (jhost-tag self) "arrays-aslist") "arrays-aslist-iterator")
                               ((string=? (jhost-tag self) "immutable-list") "immutable-list-iterator")
                               (else "arraylist-iterator"))))
(define (al-list-iterator self i)
  (let ((n (al-cnt self)))
    (unless (and (fixnum? i) (fx>=? i 0) (fx<=? i n))
      (throw-jvm 'IndexOutOfBoundsException
                 (string-append "Index: " (if (number? i) (number->string i) "?") ", Size: " (number->string n)))))
  (let ((it (make-al-iterator self (cond ((string=? (jhost-tag self) "linkedlist") "linkedlist-iterator")
                                         ((string=? (jhost-tag self) "immutable-list") "immutable-list-iterator")
                                         (else "arraylist-listitr")))))
    (ali-c! it i)
    it))

;; ---- the bulk operations -----------------------------------------------------
;; Each as the JDK's own, so an action that changes the list part way through
;; is caught the same way: ArrayList's walk its array while the count holds and
;; check it once at the end; LinkedList's are the Iterable / Collection / List
;; defaults over its ListItr, which see the change at the next step.
(define (al-for-each self f)
  (when (jolt-nil? f) (throw-jvm 'NullPointerException jolt-nil))
  (if (al-linked? self)
      (let ((it (al-iterator self)))
        (let loop () (when (ali-has-next? it) (jolt-fi-call f "accept" (ali-next it)) (loop))))
      (let ((e (al-mc self)) (n (al-cnt self)))
        (let loop ((i 0))
          (when (and (fx<? i n) (fx=? e (al-mc self))) (jolt-fi-call f "accept" (al-ref self i)) (loop (fx+ i 1))))
        (al-check! self e)))
  jolt-nil)
(define (al-remove-if self pred)
  (when (jolt-nil? pred) (throw-jvm 'NullPointerException jolt-nil))
  (if (al-linked? self)
      (let ((it (al-iterator self)))
        (let loop ((any #f))
          (if (ali-has-next? it)
              (if (jolt-truthy? (jolt-fi-call pred "test" (ali-next it))) (begin (ali-remove it) (loop #t)) (loop any))
              any)))
      (let* ((e (al-mc self)) (n (al-cnt self))
             (dead (let loop ((i 0) (acc '()))
                     (if (fx=? i n) acc
                         (loop (fx+ i 1) (if (jolt-truthy? (jolt-fi-call pred "test" (al-ref self i))) (cons i acc) acc))))))
        (al-check! self e)
        (if (null? dead) #f
            (let ((keep (let loop ((i (fx- n 1)) (dead dead) (acc '()))
                          (cond ((fx<? i 0) acc)
                                ((and (pair? dead) (fx=? (car dead) i)) (loop (fx- i 1) (cdr dead) acc))
                                (else (loop (fx- i 1) dead (cons (al-ref self i) acc)))))))
              ;; the elements past the size it started with stay
              (al-reset! self (append keep (list-tail (al->list self) n)))
              #t)))))
(define (al-replace-all self f)
  (when (jolt-nil? f) (throw-jvm 'NullPointerException jolt-nil))
  (if (al-linked? self)
      (let ((it (al-list-iterator self 0)))
        (let loop () (when (ali-has-next? it) (ali-set it (jolt-fi-call f "apply" (ali-next it))) (loop))))
      (let ((e (al-mc self)) (n (al-cnt self)))
        (let loop ((i 0))
          (when (and (fx<? i n) (fx=? e (al-mc self)))
            (al-set! self i (jolt-fi-call f "apply" (al-ref self i))) (loop (fx+ i 1))))
        (al-check! self e)
        (al-bump! self)))
  jolt-nil)
(define (al-sort self cmp)
  (let* ((e (al-mc self)) (sorted (jcoll-sorted (al->list self) cmp)))
    (if (al-linked? self)
        (al-fill! self sorted)
        (begin (al-check! self e) (al-fill! self sorted) (al-bump! self))))
  jolt-nil)
(define (al-add-all self . a)
  ;; (.addAll coll) appends; (.addAll i coll) inserts at i. ArrayList counts a
  ;; change even when the collection is empty; LinkedList does not.
  (let* ((at-i (= 2 (length a)))
         (i (if at-i (jnum->exact (car a)) (al-cnt self)))
         (xs (seq->list (jolt-seq (if at-i (cadr a) (car a))))))
    (unless (and (fx>=? i 0) (fx<=? i (al-cnt self)))
      (throw-jvm 'IndexOutOfBoundsException (string-append "Index: " (number->string i) ", Size: " (number->string (al-cnt self)))))
    (when (and (null? xs) (not (al-linked? self))) (al-bump! self))
    (let loop ((xs xs) (k i))
      (unless (null? xs) (al-insert-at! self k (car xs)) (loop (cdr xs) (fx+ k 1))))
    (pair? xs)))
(define arraylist-methods
  (list
    (cons "forEach" al-for-each)
    (cons "removeIf" al-remove-if)
    (cons "replaceAll" al-replace-all)
    (cons "sort" al-sort)
    (cons "add" (lambda (self . a)
                  ;; (.add x) -> append+true; (.add i x) -> insert at i, returns nil.
                  (if (= 1 (length a))
                      (begin (al-push! self (car a)) #t)
                      (let ((i (jnum->exact (car a))))
                        (unless (and (fixnum? i) (fx>=? i 0) (fx<=? i (al-cnt self)))
                          (throw-jvm 'IndexOutOfBoundsException
                                     (string-append "Index: " (number->string i) ", Size: " (number->string (al-cnt self)))))
                        (al-insert-at! self i (cadr a)) jolt-nil))))
    (cons "add!" (lambda (self x) (al-push! self x) #t))
    (cons "addAll" al-add-all)
    (cons "get" (lambda (self i) (let ((idx (jnum->exact i))) (al-check-index! self idx) (al-ref self idx))))
    (cons "set" (lambda (self i x)
                  (let* ((idx (jnum->exact i)) (_ (al-check-index! self idx)) (old (al-ref self idx)))
                    (al-set! self idx x) old)))
    (cons "size" (lambda (self) (->num (al-cnt self))))
    (cons "isEmpty" (lambda (self) (fx=? 0 (al-cnt self))))
    ;; remove(int) for an integer, remove(Object) — the first equal element,
    ;; answering whether there was one — for anything else, the overload
    ;; Clojure's reflector picks for a long and for any other argument
    (cons "remove" (lambda (self i)
                     (if (or (fixnum? i) (and (number? i) (exact? i) (integer? i)))
                         (let* ((idx (if (fixnum? i) i (jnum->exact i)))
                                (old (begin (al-check-index! self idx) (al-ref self idx))))
                           (al-remove-at! self idx) old)
                         (let loop ((k 0))
                           (cond ((fx=? k (al-cnt self)) #f)
                                 ((objects-equals? i (al-ref self k)) (al-remove-at! self k) #t)
                                 (else (loop (fx+ k 1))))))))
    (cons "clear" (lambda (self) (al-clear! self) jolt-nil))
    (cons "contains" (lambda (self x) (and (memp (lambda (e) (jolt=2 e x)) (al->list self)) #t)))
    (cons "toArray" (lambda (self . args) (jcoll-to-array (al->list self) args)))
    (cons "iterator" (lambda (self) (al-iterator self)))
    (cons "listIterator" (case-lambda ((self) (al-list-iterator self 0)) ((self i) (al-list-iterator self (jnum->exact i)))))))
;; ArrayList's capacity methods: trimToSize is always a structural change on the
;; JDK (it counts one before looking); ensureCapacity is one when the backing
;; array grows, which here is when the request exceeds the buffer jolt holds
(define arraylist-capacity-methods
  (list (cons "trimToSize" (lambda (self) (al-bump! self) jolt-nil))
        (cons "ensureCapacity" (lambda (self n)
                                 (when (> (jnum->exact n) (vector-length (al-vec self))) (al-bump! self))
                                 jolt-nil))))

;; java.util.SequencedCollection (JDK 21): List and Deque both have it, so the
;; first/last accessors and mutators sit on the shared ArrayList table and
;; LinkedList inherits them. On an empty list the accessors raise
;; NoSuchElementException, as on the JVM.
;; the JDK raises these with no message
(define (al-first self)
  (if (fx=? 0 (al-cnt self)) (throw-jvm 'NoSuchElementException jolt-nil) (al-ref self 0)))
(define (al-last self)
  (if (fx=? 0 (al-cnt self)) (throw-jvm 'NoSuchElementException jolt-nil) (al-ref self (fx- (al-cnt self) 1))))
(define sequenced-list-methods
  (list
    (cons "addFirst" (lambda (self x) (al-insert-at! self 0 x) jolt-nil))
    (cons "addLast" (lambda (self x) (al-push! self x) jolt-nil))
    (cons "removeFirst" (lambda (self) (let ((o (al-first self))) (al-remove-at! self 0) o)))
    (cons "removeLast" (lambda (self) (let ((o (al-last self))) (al-remove-at! self (fx- (al-cnt self) 1)) o)))
    (cons "getFirst" al-first) (cons "getLast" al-last)))
(register-host-methods! "arraylist" (append arraylist-methods arraylist-capacity-methods sequenced-list-methods))
(register-modcount! "arraylist" al-mc)
(register-modcount! "linkedlist" al-mc)

;; java.util.LinkedList: the ArrayList backing plus the Deque surface. Its
;; iterator is a ListItr (cursor < size), and its bulk operations are the
;; List / Collection defaults (al-for-each and friends dispatch on the tag).
;; tools.reader holds pending splice forms in one and (seq)s / .remove(0)s it.
(define (ll-poll-first self) (if (fx=? 0 (al-cnt self)) jolt-nil (let ((o (al-first self))) (al-remove-at! self 0) o)))
(define (ll-poll-last self) (if (fx=? 0 (al-cnt self)) jolt-nil (let ((o (al-last self))) (al-remove-at! self (fx- (al-cnt self) 1)) o)))
(define (ll-remove-occurrence self x last?)
  (let loop ((k (if last? (fx- (al-cnt self) 1) 0)))
    (cond ((or (fx<? k 0) (fx>=? k (al-cnt self))) #f)
          ((objects-equals? x (al-ref self k)) (al-remove-at! self k) #t)
          (else (loop (if last? (fx- k 1) (fx+ k 1)))))))
;; LinkedList$DescendingIterator: a ListItr walked backward from the end
(define (make-ll-desc-iterator self)
  (make-jhost "linkedlist-desc-iterator" (al-list-iterator self (al-cnt self))))
(hashtable-set! jhost-tag->fqn "linkedlist-desc-iterator" "java.util.LinkedList$DescendingIterator")
(jch-register-supers! "java.util.LinkedList$DescendingIterator" '("java.util.Iterator"))
(register-host-methods! "linkedlist-desc-iterator"
  (list (cons "hasNext" (lambda (self) (fx>? (ali-c (jhost-state self)) 0)))
        (cons "next" (lambda (self) (ali-previous (jhost-state self))))
        (cons "remove" (lambda (self) (ali-remove (jhost-state self))))))
(define linkedlist-methods
  (append arraylist-methods sequenced-list-methods
    (list
      (cons "offer" (lambda (self x) (al-push! self x) #t))
      (cons "offerFirst" (lambda (self x) (al-insert-at! self 0 x) #t))
      (cons "offerLast" (lambda (self x) (al-push! self x) #t))
      (cons "peek" (lambda (self) (if (fx=? 0 (al-cnt self)) jolt-nil (al-first self))))
      (cons "peekFirst" (lambda (self) (if (fx=? 0 (al-cnt self)) jolt-nil (al-first self))))
      (cons "peekLast" (lambda (self) (if (fx=? 0 (al-cnt self)) jolt-nil (al-last self))))
      (cons "element" al-first)
      (cons "poll" ll-poll-first)
      (cons "pollFirst" ll-poll-first)
      (cons "pollLast" ll-poll-last)
      (cons "push" (lambda (self x) (al-insert-at! self 0 x) jolt-nil))
      (cons "pop" (lambda (self) (let ((o (al-first self))) (al-remove-at! self 0) o)))
      (cons "removeFirstOccurrence" (lambda (self x) (ll-remove-occurrence self x #f)))
      (cons "removeLastOccurrence" (lambda (self x) (ll-remove-occurrence self x #t)))
      (cons "descendingIterator" make-ll-desc-iterator)
      ;; remove() is removeFirst; remove(int) / remove(Object) as the list's
      (cons "remove" (let ((rm (cdr (assoc "remove" arraylist-methods))))
                       (case-lambda ((self) (let ((o (al-first self))) (al-remove-at! self 0) o))
                                    ((self x) (rm self x))))))))
(define (make-linkedlist xs)
  (let ((al (make-arraylist xs))) (make-jhost "linkedlist" (jhost-state al))))
(register-host-methods! "linkedlist" linkedlist-methods)
(let ((ctor (lambda args
              (cond ((null? args) (make-linkedlist '()))
                    (else (make-linkedlist (seq->list (jolt-seq (car args)))))))))
  (register-class-ctor! "LinkedList" ctor)
  (register-class-ctor! "java.util.LinkedList" ctor))

;; ArrayDeque is a ring buffer of its own (java/array-deque.ss).

;; ArrayList / LinkedList are Iterable: (seq al) walks the elements (nil if empty),
;; so (seq pending-forms) and reduce/into over one work like the JVM.
(define-jhost-tag-pred (al-family? t)
  (or (string=? t "arraylist") (string=? t "linkedlist")
      (string=? t "arrays-aslist") (string=? t "immutable-list")))
;; …through its iterator, 32 elements to a chunk, as clojure.lang.RT seqs an
;; Iterable (chunkIteratorSeq): the chunks after the first are read when the walk
;; reaches them, so a change made meanwhile is a ConcurrentModificationException,
;; as it is on the JVM, where doseq or reduce over a list they change raises one.
(define (al-chunked-seq it)
  ;; a chunk is up to 32 of the iterator's next elements, copied from the
  ;; backing in one step: its next would check the count before each, and
  ;; nothing runs between them, so one check answers for the chunk
  (if (not (ali-has-next? it))
      jolt-nil
      (let* ((st (jhost-state it)) (l (vector-ref st 0)) (c (vector-ref st 1))
             (k (fxmin 32 (fx- (al-cnt l) c))))
        (when (fx<=? k 0) (ali-next it))               ; past the end: next says what
        (ali-check! it)
        (let ((bv (al-vec l)) (h (al-head l)))
          (vector-set! st 1 (fx+ c k)) (vector-set! st 2 (fx+ c k -1))
          ;; whether more follow is asked as the chunk is made, as the JDK's
          ;; chunkIteratorSeq asks it: a list of 32 or fewer is settled then.
          ;; The chunk goes out as plain cells ending in that tail, the shape
          ;; first/next (doseq) walks fastest
          (let ((tail (if (ali-has-next? it) (jolt-make-lazy-seq (lambda () (al-chunked-seq it))) jolt-nil)))
            (let loop ((i (fx- k 1)) (rest tail))
              (if (fx<? i 0) rest
                  (loop (fx- i 1) (make-cseq (vector-ref bv (fx+ h c i)) rest sk-cons jolt-nil)))))))))
(register-seq-arm! al-family?
  (lambda (x)
    (if (member (jhost-tag x) '("arraylist" "linkedlist"))
        (al-chunked-seq (al-iterator x))
        (list->cseq (al->list x)))))

;; ---- StringWriter -----------------------------------------------------------
;; Writer.write(int) writes the CHAR for that code; append(char) appends the char.
;; What one value puts into a stream. A number is the CHARACTER with that code
;; (write(int) writes one unit, not the digits). A char[] is its characters —
;; every write and print overload that takes one is defined that way, and it used
;; to render as "#object[[C …]" straight into the stream. A byte[] is NOT handled
;; here: print/println have no byte[] overload on the JVM and fall to
;; print(Object), so it renders as an object, and only write puts its bytes out.
;; char-array-arg? is string-builder.ss's, char-array->string natives-array.ss's.
(define (byte-array-arg? x) (and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)))
(define (writer-piece x)
  (cond ((number? x) (string (integer->char (jnum->exact x))))
        ((char-array-arg? x) (char-array->string x))
        (else (render-piece x))))
;; The bytes a byte[] .write argument carries, with the JVM's 3-arg (off len)
;; region applied. Out-of-range ends clamp rather than raise: a stream write is
;; not the place to turn a caller's off-by-one into a crash mid-output.
(define (byte-array-range x rest)
  (let ((bv (na-bytearray->bv x)))
    (if (and (pair? rest) (pair? (cdr rest)))
        (let* ((off (max 0 (jnum->exact (car rest))))
               (len (max 0 (jnum->exact (cadr rest))))
               (start (min off (bytevector-length bv)))
               (end (min (bytevector-length bv) (+ start len)))
               (sub (make-bytevector (- end start))))
          (bytevector-copy! bv start sub 0 (- end start))
          sub)
        bv)))
;; What a .write argument puts into a TEXT sink: writer-piece, plus the byte[]
;; whose CONTENT write is defined to emit — (.write w (.getBytes s)) is how
;; ported code writes raw output, and it used to put "#object[[B …]" in the
;; stream. Bytes decode as UTF-8, the encoding jolt renders in; they are sliced
;; before decoding, since the offsets are byte offsets and cutting the decoded
;; string would land in the middle of a multi-byte character. A sink with a real
;; descriptor under it takes the bytes themselves — see pw-write-bytes! below.
(define (writer-piece-range x rest)
  (cond
    ((byte-array-arg? x) (utf8-bytes->string (byte-array-range x rest)))
    ((and (pair? rest) (pair? (cdr rest)))
     (let* ((s (writer-piece x))
            (off (max 0 (jnum->exact (car rest))))
            (len (max 0 (jnum->exact (cadr rest))))
            (start (min off (string-length s)))
            (end (min (string-length s) (+ start len))))
       (substring s start end)))
    (else (writer-piece x))))
;; Same accumulator as StringBuilder, and for the same reason: writing to a
;; StringWriter a piece at a time — which is what printStackTrace and every
;; print-to-a-writer path does — used to copy the whole buffer per write.
;; As on the JDK, a StringWriter writes into a StringBuffer and synchronizes on
;; it: the writer and its getBuffer share one store, whose lock is that buffer.
(register-class-ctor! "StringWriter"
  (lambda args
    (let ((size (if (null? args) 16 (jnum->exact (car args)))))
      (when (< size 0) (throw-jvm 'IllegalArgumentException "Negative buffer size"))
      (let ((st (make-sb-state "" size)))
        (make-locked-sb "string-buffer" st)
        (make-jhost "writer" st)))))
(register-host-methods! "writer"
  (list (cons "write" (lambda (self x . rest) (sb-append*! self (writer-piece-range x rest)) jolt-nil))
        (cons "append" (lambda (self x . rest) (sb-append*! self (append-text x rest)) self))
        (cons "flush" (lambda (self) jolt-nil))
        (cons "close" (lambda (self) jolt-nil))
        (cons "getBuffer" (lambda (self) (vector-ref (jhost-state self) 2)))
        (cons "toString" (lambda (self) (sb-str* self)))))
;; (str sw) / print a StringWriter -> its accumulated content, like the JVM
;; (str calls toString) — data.csv writes CSV to a StringWriter and reads it back.
(register-str-render! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "writer"))) sb-str*)

;; a writer over a real Chez port — the values *out* / *err* hold. write/append
;; push to the port (so (.write *out* s) and (binding [*out* *err*] …) work);
;; it isn't a buffer, so toString is empty. Lets libraries that touch *out*/*err*
;; (tools.logging, selmer) compile and run.
;; *out*/*err* resolve their port LIVE — 'out -> (current-output-port), 'err ->
;; (current-error-port) — so a (.write *out* …) / (.flush *out*) follows a
;; with-out-str redirect (with-output-to-string rebinds current-output-port) the
;; same way print/__write do. Storing the startup port instead pinned *out* to the
;; real stdout, so rewrite-clj's (z/print) — which writes via *out* — escaped the
;; capture. A stored port object (should any other code make a port-writer) is used
;; as-is.
;;
;; 'stdout / 'stderr are the other half of that: the PROCESS streams, resolved
;; once at startup and never again. They back System/out and System/err, which
;; the JVM does NOT route through *out* — a with-out-str captures (print …) and
;; leaves (.println System/out …) going to the terminal. Resolving them live
;; through (current-output-port) like 'out made jolt capture both, so a library
;; that deliberately writes past the capture (a progress bar, a prompt, a logger
;; writing to the real stderr) had its output swallowed into the string instead.
(define port-writer-stdout (current-output-port))
(define port-writer-stderr (current-error-port))
(define (port-writer-port self)
  (let ((p (vector-ref (jhost-state self) 0)))
    (cond ((eq? p 'out) (current-output-port))
          ((eq? p 'err) (current-error-port))
          ((eq? p 'stdout) port-writer-stdout)
          ((eq? p 'stderr) port-writer-stderr)
          (else p))))

;; The process streams also have a BYTE path. System/out and System/err are
;; java.io.PrintStreams — OutputStreams — so bytes written to one have to reach
;; the descriptor as they are, and (io/copy System/in System/out) is the cat.
;; Decoding them as UTF-8 first replaced every byte that was not text with U+FFFD.
;; Chez's process port is textual and re-encodes whatever it is given, so the
;; bytes go out through a second port on the same descriptor. That port is
;; UNBUFFERED and the textual one is flushed ahead of it, so the two stay in the
;; order they were written even when stdout is a pipe.
;;
;; Only the real process stream takes this path: with-out-str resolves 'out to a
;; string port, which can only take characters, so a byte[] written there decodes
;; as before.
(define pw-byte-port-mu (make-mutex))
(define pw-stdout-bytes (box #f))
(define pw-stderr-bytes (box #f))
(define (pw-byte-port-memo cell open)
  (unless (unbox cell)
    (jolt-with-mutex pw-byte-port-mu
      (unless (unbox cell) (set-box! cell (open (buffer-mode none))))))
  (unbox cell))
(define (pw-byte-port port)
  (cond ((eq? port port-writer-stdout) (pw-byte-port-memo pw-stdout-bytes standard-output-port))
        ((eq? port port-writer-stderr) (pw-byte-port-memo pw-stderr-bytes standard-error-port))
        (else #f)))
;; Writes bv to the descriptor behind a process stream; #f when there is none, so
;; the caller falls back to writing it as text.
(define (pw-write-bytes! self bv)
  (let* ((port (port-writer-port self))
         (bp (pw-byte-port port)))
    (and bp
         (begin (flush-output-port port)
                ;; past the textual port, so under its failure rule by hand
                (jolt-process-stream-write port (lambda () (put-bytevector bp bv)))
                #t))))
;; print / println / printf are PrintStream's, and System/out is a port-writer, so
;; ported code writing (.println System/out …) lands here. println with no argument
;; is the bare newline, as on the JVM.
(register-host-methods! "port-writer"
  (list (cons "write" (lambda (self x . rest)
                        (unless (and (byte-array-arg? x)
                                     (pw-write-bytes! self (byte-array-range x rest)))
                          (display (writer-piece-range x rest) (port-writer-port self)))
                        jolt-nil))
        (cons "append" (lambda (self x . rest) (display (append-text x rest) (port-writer-port self)) self))
        (cons "print" (lambda (self x) (display (writer-piece x) (port-writer-port self)) jolt-nil))
        (cons "println" (lambda (self . xs)
                          (let ((p (port-writer-port self)))
                            (unless (null? xs) (display (writer-piece (car xs)) p))
                            (display "\n" p))
                          jolt-nil))
        (cons "flush" (lambda (self) (flush-output-port (port-writer-port self)) jolt-nil))
        (cons "close" (lambda (self) jolt-nil))
        ;; PrintStream/PrintWriter.checkError: flush, then whether the stream has
        ;; failed — only a process stream can, the others raise
        (cons "checkError" (lambda (self)
                             (let ((p (port-writer-port self)))
                               (flush-output-port p)
                               (jolt-process-stream-failed? p))))
        (cons "toString" host-object-tostring)))
(def-dynvar! "clojure.core" "*out*" (make-jhost "port-writer" (vector 'out)))
(def-dynvar! "clojure.core" "*err*" (make-jhost "port-writer" (vector 'err)))

;; System/out and System/err — the process's own streams, so unlike *out*/*err*
;; they are NOT affected by a (binding [*out* …]) or a with-out-str, matching the
;; JVM. Ported code writes to them directly ((.println System/out …)). They are
;; port-writers over the 'stdout / 'stderr ports resolved at startup; io-streams.ss
;; then wraps each in the PrintStream shim, because on the JVM System.out is a
;; java.io.PrintStream and not the java.io.PrintWriter *out* is, and libraries
;; branch on that class.
;;
;; They live in the MUTABLE static cells rather than the plain statics table,
;; because setOut/setErr replace them wholesale — clojure.tools.logging's
;; log-capture! points them at a PrintStream over a proxy so every write becomes a
;; log record. Both spellings get the cell, since a read resolves the class name as
;; written.
(define system-out-writer (make-jhost "port-writer" (vector 'stdout)))
(define system-err-writer (make-jhost "port-writer" (vector 'stderr)))
(register-class-statics! "System"
  (list (cons "out" system-out-writer)
        (cons "err" system-err-writer)))
(define (sys-set-stream! member v)
  (for-each (lambda (c) (vector-set! (mutable-static-cell c member #t) 0 v))
            '("System" "java.lang.System"))
  jolt-nil)
(sys-set-stream! "out" system-out-writer)
(sys-set-stream! "err" system-err-writer)
(register-class-statics! "System"
  (list (cons "setOut" (lambda (v) (sys-set-stream! "out" v)))
        (cons "setErr" (lambda (v) (sys-set-stream! "err" v)))))

;; PrintWriter — a thin wrapper over a target writer. clojure.data.json's pretty
;; printer builds (PrintWriter. *out*) where *out* is bound to clojure.pprint's
;; pretty-writer (a jolt record), so text for one of those routes column-aware
;; through clojure.pprint/-write.
;;
;; A target that is a Writer in its own right — a host writer (file-backed,
;; OutputStreamWriter, BufferedWriter, a nested PrintWriter) or a proxy/reify
;; java.io.Writer — is called the way the JDK's PrintWriter calls its `out`:
;; text (write(String), print, append, a char[]) as write(s, 0, len), a single
;; char (write(int), append(char)) as write(c), the line separator
;; as write("\n"), and flush/close as its own flush/close. Which overload a
;; proxy receives is observable: a java.io.Writer proxy need only define the
;; abstract write(cbuf, off, len), and one that does must work.
(define (pw-target self) (vector-ref (jhost-state self) 0))
;; Whether the target is a Writer of its own, decided once: it never changes.
;; State is #(target trouble? writer?); an image from before has #(target).
(define (pw-writer? self)
  (let ((st (jhost-state self)))
    (if (fx> (vector-length st) 2) (vector-ref st 2) (pw-writer-target? (vector-ref st 0)))))
(define (pw-writer-target? t)
  (or (and (jhost? t)
           (not (string=? (jhost-tag t) "port-writer"))
           (not (string=? (jhost-tag t) "writer"))
           (not (sb-jhost? t)))
      (and (not (jhost? t)) (iface-method t "write" #f) #t)))
;; Text into a target that is not a Writer of its own: stdout/stderr, a
;; StringWriter/StringBuilder, or clojure.pprint's pretty-writer.
(define (pw-emit-text t s)
  (cond
    ;; through port-writer-port, not the raw slot: a port-writer holds the SYMBOL
    ;; 'out / 'err and resolves it per call, so a (PrintWriter. *out*) built inside
    ;; a with-out-str writes to the capture rather than past it.
    ((and (jhost? t) (string=? (jhost-tag t) "port-writer"))
     (display s (port-writer-port t)))
    ((jhost? t) (sb-append*! t s))
    (else (jolt-invoke (var-deref "clojure.pprint" "-write") t s))))
(define (pw-text! self s)
  (let ((t (pw-target self)))
    (if (pw-writer? self)
        (record-method-dispatch t "write" (jolt-list s (->num 0) (->num (string-length s))))
        (pw-emit-text t s))))
(define (pw-char! self c)                 ; c: the char's code
  (let ((t (pw-target self)))
    (if (pw-writer? self)
        (record-method-dispatch t "write" (jolt-list (->num c)))
        (pw-emit-text t (string (integer->char c))))))
(define (pw-newline! self)
  (let ((t (pw-target self)))
    (if (pw-writer? self)
        (record-method-dispatch t "write" (jolt-list "\n"))
        (pw-emit-text t "\n"))))
(define (pw-print! self x)              ; print(char) too is text: write(s, 0, 1)
  (if (char-array-arg? x)
      (pw-text! self (char-array->string x))
      (pw-text! self (render-piece x))))
(define (pw-pass! self name)              ; flush / close, when the target has one
  (let ((t (pw-target self)))
    (when (or (and (jhost? t) (host-method-ref (jhost-tag t) name))
              (and (not (jhost? t)) (iface-method t name #f)))
      (record-method-dispatch t name jolt-nil))
    jolt-nil))
;; A PrintWriter never throws an IOException from its target: it records it,
;; checkError reports it, and the call returns as if it had written. An
;; InterruptedIOException re-asserts the thread's interrupt instead, and
;; anything that is not an IOException propagates, as in the JDK's PrintWriter.
(define (pw-trouble? self)
  (let ((st (jhost-state self))) (and (fx> (vector-length st) 1) (vector-ref st 1))))
(define (pw-trouble! self)
  (let ((st (jhost-state self)))
    (if (fx> (vector-length st) 1)
        (vector-set! st 1 #t)
        (jhost-state-set! self (vector (vector-ref st 0) #t (pw-writer-target? (vector-ref st 0)))))))
(define pw-ioexception (jolt-symbol #f "java.io.IOException"))
(define pw-interrupted-io (jolt-symbol #f "java.io.InterruptedIOException"))
;; Only a Writer target can raise one: a StringWriter, StringBuilder or process
;; stream cannot, and those run the body straight, with no guard and no closure
;; (both cost ~25% of a print into a StringWriter).
(define-syntax pw-guarded
  (syntax-rules ()
    ;; the body arrives as (lambda () body ...), written so to read as a thunk
    ((_ self (_lambda () body ...))
     (let ((s self))
       (if (pw-writer? s)
           (pw-guarded* s (lambda () body ...))
           (begin body ...))))))
(define (pw-guarded* self thunk)
  (guard (e ((let ((v (jolt-unwrap-throw e)))
               (and (jolt-truthy? (instance-check pw-ioexception v)) v))
             => (lambda (v)
                  (if (jolt-truthy? (instance-check pw-interrupted-io v))
                      (record-method-dispatch (host-static-call "Thread" "currentThread") "interrupt" jolt-nil)
                      (pw-trouble! self))
                  jolt-nil)))
    (thunk)))
(define (make-print-writer args)
  (let ((t (if (pair? args) (car args) jolt-nil)))
    (make-jhost "print-writer" (vector t #f (pw-writer-target? t)))))
(register-class-ctor! "PrintWriter" (lambda args (make-print-writer args)))
(register-class-ctor! "java.io.PrintWriter" (lambda args (make-print-writer args)))
(register-host-methods! "print-writer"
  ;; write takes (buf off len) and renders an int as its character; append takes
  ;; (csq start end) and renders everything as text. Sharing append-text between
  ;; them read the 3-arg write's LENGTH as an end index and printed (.write w 65)
  ;; as "65" rather than "A".
  (list (cons "write" (lambda (self x . rest)
                        (pw-guarded self
                          (lambda ()
                            (if (and (null? rest) (number? x))
                                (pw-char! self (jnum->exact x))
                                (pw-text! self (writer-piece-range x rest)))))
                        jolt-nil))
        (cons "print" (lambda (self x) (pw-guarded self (lambda () (pw-print! self x))) jolt-nil))
        (cons "println" (lambda (self . xs)
                          (pw-guarded self
                            (lambda ()
                              (unless (null? xs) (pw-print! self (car xs)))
                              (pw-newline! self)))
                          jolt-nil))
        (cons "append" (lambda (self x . rest)
                         (pw-guarded self
                           (lambda ()
                             (if (and (null? rest) (char? x))
                                 (pw-char! self (char->integer x))
                                 (pw-text! self (append-text x rest)))))
                         self))
        (cons "printf" (lambda (self a . rest)
                         (jvm-format-pieces a rest
                           (lambda (piece) (pw-guarded self (lambda () (pw-text! self piece)))))
                         self))
        (cons "format" (lambda (self a . rest)
                         (jvm-format-pieces a rest
                           (lambda (piece) (pw-guarded self (lambda () (pw-text! self piece)))))
                         self))
        (cons "flush" (lambda (self) (pw-guarded self (lambda () (pw-pass! self "flush")))))
        (cons "close" (lambda (self) (pw-guarded self (lambda () (pw-pass! self "close")))))
        ;; flush, then its own flag, then the target's answer when it has one: a
        ;; PrintWriter over System/out reports the process stream failing, as
        ;; the JDK's delegates to the PrintStream it wraps
        (cons "checkError" (lambda (self)
                             (pw-guarded self (lambda () (pw-pass! self "flush")))
                             (or (pw-trouble? self)
                                 (let ((t (pw-target self)))
                                   (and (jhost? t) (host-method-ref (jhost-tag t) "checkError")
                                        (jolt-truthy? (record-method-dispatch t "checkError" jolt-nil)))))))
        (cons "toString" host-object-tostring)))

;; PrintWriter-on — a writer that accumulates writes and, on flush, hands the
;; accumulated string to flush-fn and clears it; close calls close-fn if given.
;; (binding [*out* (PrintWriter-on f nil)] …) routes print/pr through it because
;; they write via the jhost's "write" method.
(define (pwo-buf self) (vector-ref (jhost-state self) 0))
(define (pwo-flush-fn self) (vector-ref (jhost-state self) 1))
(define (pwo-close-fn self) (vector-ref (jhost-state self) 2))
(define (jolt-print-writer-on flush-fn close-fn)
  (make-jhost "print-writer-on" (vector (box "") flush-fn close-fn)))
(register-host-methods! "print-writer-on"
  (list (cons "write" (lambda (self x . rest) (set-box! (pwo-buf self)
                                  (string-append (unbox (pwo-buf self)) (writer-piece-range x rest))) jolt-nil))
        (cons "append" (lambda (self x . rest) (set-box! (pwo-buf self)
                                  (string-append (unbox (pwo-buf self)) (append-text x rest))) self))
        (cons "flush" (lambda (self)
                        (let ((b (pwo-buf self)) (ff (pwo-flush-fn self)))
                          (unless (jolt-nil? ff) (jolt-invoke ff (unbox b)))
                          (set-box! b "") jolt-nil)))
        (cons "close" (lambda (self)
                        (let ((cf (pwo-close-fn self)))
                          (unless (jolt-nil? cf) (jolt-invoke cf)) jolt-nil)))
        (cons "toString" (lambda (self) (unbox (pwo-buf self))))))
(def-var! "clojure.core" "PrintWriter-on" jolt-print-writer-on)

;; ---- java.util.HashMap ------------------------------------------------------
;; A mutable map keyed by jolt values (jolt-hash / jolt=2). State
;; #(chez-hashtable key-order modcount) — a Properties adds its defaults and
;; writethrough after them. Slot 2 is HashMap's modCount: a new key, a removed
;; key and every clear (an empty map's too) bump it; a put over an existing key
;; does not. The fail-fast views' iterators, forEach, replaceAll, the compute
;; family and the HashSet over the same storage watch it (jolt-i6iwq).
;; Constructors: () | (capacity) | (capacity load-factor) [sizing args ignored] |
;; (Map m) [copy]. Enough of the Map surface for libraries that build a fast lookup
;; (malli's fast-registry: (doto (HashMap. 1024 0.25) (.putAll m)) then .get).
(define (hm-hash k) (let ((h (jolt-hash k)))
                      (bitwise-and (if (and (integer? h) (exact? h)) (abs h) 0) #x3FFFFFFF)))
(define (hm-tbl self) (vector-ref (jhost-state self) 0))
;; insertion order for iteration (seq/keySet/values/entrySet render
;; deterministically; the JVM's hash order is arbitrary, insertion order is the
;; deterministic superset jolt's small maps already use).
;; The slot holds the keys newest first, as a plain list, until the map's first
;; removal of a key that is not its newest (jolt-lfm8p: filtering the list then
;; made every such remove linear in the map). Such a removal turns the slot into
;; an hm-idx: the keys as cells (key . live?), newest first, and a table from key
;; to cell. remove clears its key's cell and drops it from the table, a new key
;; pushes a cell, and a walk keeps the live cells, so a returning key sits where
;; it was last inserted, as before. Dead cells are swept once they outnumber live
;; keys, keeping remove amortized O(1). A map that never removes, or only pops its
;; newest key, keeps the plain list and pays nothing for this.
(define-record-type hm-idx (fields (mutable cells) index (mutable dead)))
(define (hm-ord self) (vector-ref (jhost-state self) 1))
(define (hm-ord! self v) (vector-set! (jhost-state self) 1 v))
(define (hm-idx-live-keys o)            ; oldest first
  (let loop ((cs (hm-idx-cells o)) (acc '()))
    (cond ((null? cs) acc)
          ((cdar cs) (loop (cdr cs) (cons (caar cs) acc)))
          (else (loop (cdr cs) acc)))))
(define (hm-idx-sweep! o)
  (hm-idx-cells-set! o (filter cdr (hm-idx-cells o)))
  (hm-idx-dead-set! o 0))
(define (hm-list->idx self ks)          ; ks newest first, all live
  (let ((ix (hm-like-table self)))
    (make-hm-idx (map (lambda (k) (let ((c (cons k #t))) (hashtable-set! ix k c) c)) ks) ix 0)))
;; slot 3 is a ConcurrentHashMap's claims (chm-claim!); '() elsewhere
(define (hm-state tbl ord) (vector tbl ord 0 '()))
;; A ConcurrentHashMap's state adds slot 4, its private monitor, which every
;; write and every walk of the key order holds, and slot 5, how many threads
;; wait in chm-await-key! for another's claim on a key to end.
(define (chm-state tbl) (vector tbl '() 0 '() (make-monitor) 0))
(define (hm-chm? self) (string=? (jhost-tag self) "chm"))
;; Before a second thread exists (jolt-mt?, values.ss) there is no writer to
;; exclude, so the body runs as it is: the monitor is most of a single-threaded
;; put's cost. Unless another owner holds a claim (chm-claim!): fibers drained on
;; this one thread can, and waiting that claim out needs the monitor.
(define-syntax with-chm
  (syntax-rules ()
    ((_ self e ...)
     (let ((st (jhost-state self)))
       (if (and (not jolt-mt?) (chm-claims-mine? st))
           (begin e ...)
           (jolt-call-with-monitor (vector-ref st 4) (lambda () e ...)))))))
;; A Properties is synchronized on itself, as the JDK's writes are: slot 5 holds
;; its own monitor, so (locking props ...) excludes its methods. The same
;; single-threaded shortcut as with-chm, unless this thread holds the monitor
;; already (a locking block around the call) and so must enter it again.
(define (props-state tbl ord defaults writethrough)
  (vector tbl ord 0 defaults writethrough #f))
(define (props-own-monitor! self) (vector-set! (jhost-state self) 5 (object-monitor self)) self)
(define (hm-props? self) (string=? (jhost-tag self) "properties"))
(define-syntax with-props
  (syntax-rules ()
    ((_ self e ...)
     (let ((m (vector-ref (jhost-state self) 5)))
       (if (and (not jolt-mt?) (not (vector-ref m monitor-i-owner)))
           (begin e ...)
           (jolt-call-with-monitor m (lambda () e ...)))))))
(define (chm-claims-mine? st)
  (let ((cs (vector-ref st 3)))
    (or (null? cs)
        (let ((me (chm-owner))) (for-all (lambda (c) (eqv? (chm-claim-owner c) me)) cs)))))
(define (hm-mc self) (vector-ref (jhost-state self) 2))
(define-syntax hm-bump!
  ;; inline: every new key and every removal pays it
  (syntax-rules ()
    ((_ self) (let ((st (jhost-state self)))
                (vector-set! st 2 (fxand (fx+ (vector-ref st 2) 1) #x3fffffff))))))
;; A WeakHashMap's table is weak, so it cannot keep its keys in a list: its slot
;; holds a version token instead, a fresh one per structural change (what a
;; view's iterator checks), and its keys come from the table, in the table's
;; order, as the JDK's come in hash order.
(define (hm-weak? self) (hashtable-weak? (hm-tbl self)))
(define (hm-note-key! self k)
  (when (not (hashtable-contains? (hm-tbl self) k))
    (hm-bump! self)
    (if (hm-weak? self)
        (hm-ord! self (list 'v))
        (let ((o (hm-ord self)))
          (if (hm-idx? o)
              (let ((c (cons k #t)))
                (hashtable-set! (hm-idx-index o) k c)
                (hm-idx-cells-set! o (cons c (hm-idx-cells o))))
              (hm-ord! self (cons k o)))))))
;; values compare as the map's keys do: by identity in an IdentityHashMap
(define (hm-veq? self a b)
  (if (eq? (hashtable-equivalence-function (hm-tbl self)) eq?) (eq? a b) (jolt=2 a b)))
;; an empty table of this map's kind (weak, identity, or equality-keyed)
(define (hm-like-table self)
  (let* ((t (hm-tbl self)) (id? (eq? (hashtable-equivalence-function t) eq?)))
    (cond ((hashtable-weak? t) (if id? (make-weak-eq-hashtable) (make-weak-hashtable hm-hash jolt=2)))
          (id? (make-eq-hashtable))
          (else (make-hashtable hm-hash jolt=2)))))
;; A removal detaches what was handed out for the key (an entrySet node keeps
;; the value it last saw, as the JDK's removed Node does); jutil-concurrent.ss
;; installs the hook once a map has handed one out.
(define hm-detach-hook #f)
(define (hm-drop-key! self k)
  (when hm-detach-hook (hm-detach-hook self k))
  (hm-bump! self)
  ;; k is already out of the table. Popping the newest key (a stack-like use)
  ;; leaves a plain list plain; the front check goes by the table's own key
  ;; equivalence (two equal keys are two keys in an IdentityHashMap).
  (if (hm-weak? self)
      (hm-ord! self (list 'v))
      (let ((o (hm-ord self)))
        (if (hm-idx? o)
            (let ((c (hashtable-ref (hm-idx-index o) k #f)))
              (when c
                (hashtable-delete! (hm-idx-index o) k)
                (if (eq? c (car (hm-idx-cells o)))
                    (hm-idx-cells-set! o (cdr (hm-idx-cells o)))
                    (let ((n (fx+ (hm-idx-dead o) 1)))
                      (set-cdr! c #f)
                      (hm-idx-dead-set! o n)
                      (when (fx>? n (fx+ (hashtable-size (hm-tbl self)) 32)) (hm-idx-sweep! o))))))
            (cond ((null? o))
                  (((hashtable-equivalence-function (hm-tbl self)) (car o) k) (hm-ord! self (cdr o)))
                  (else
                   ;; k's entry is still in the list: index the rest, then k's
                   ;; cell goes dead like any later removal's
                   (let* ((ix (hm-list->idx self o)) (c (hashtable-ref (hm-idx-index ix) k #f)))
                     (hashtable-delete! (hm-idx-index ix) k)
                     (set-cdr! c #f)
                     (hm-idx-dead-set! ix 1)
                     (hm-ord! self ix))))))))
(define (hm-clear! self)
  (when hm-detach-hook (hm-detach-hook self hm-detach-hook))
  (hm-bump! self)
  (hashtable-clear! (hm-tbl self))
  (hm-ord! self (if (hm-weak? self) (list 'v) '())))
;; A ConcurrentHashMap's and a Properties' walks take the order and the values
;; under the map's lock, so a walk racing a write sees each key with the value it
;; had, never a key whose value a remove already took.
(define (hm-keys-ordered self)
  (cond ((hm-chm? self) (with-chm self (hm-keys-ordered* self)))
        ((hm-props? self) (with-props self (hm-keys-ordered* self)))
        (else (hm-keys-ordered* self))))
(define (hm-pairs-ordered self)          ; ((k . v) ...) in the map's order
  (define (pairs) (let ((t (hm-tbl self))) (map (lambda (k) (cons k (hashtable-ref t k jolt-nil))) (hm-keys-ordered* self))))
  (cond ((hm-chm? self) (with-chm self (pairs)))
        ((hm-props? self) (with-props self (pairs)))
        (else (pairs))))
(define (hm-keys-ordered* self)
  (if (hm-weak? self)
      ;; by hash: the same keys come out in the same order whatever the
      ;; history of the table (an image restore included). Not the JDK's bucket
      ;; order (known-divergences); the Map contract leaves it unspecified
      (map cdr (list-sort (lambda (a b) (< (car a) (car b)))
                          (map (lambda (k) (cons (hm-hash k) k)) (vector->list (hashtable-keys (hm-tbl self))))))
      (let ((o (hm-ord self)))
        (if (hm-idx? o) (hm-idx-live-keys o) (reverse o)))))
;; A hashmap-shaped jhost: the HashMap family and java.util.Properties, which is
;; a Hashtable with two string-typed accessors on top and so carries the same
;; #(table key-order) state. One predicate for every question about that shape —
;; seq, count, contains?, get, copy-into — so a new member of the family cannot
;; be map-like through one of them and opaque through another.
(define-jhost-tag-pred (hm-hashmap? t)
  (or (string=? t "hashmap") (string=? t "properties") (string=? t "chm")
      (string=? t "jx-weak-hashmap") (string=? t "jx-identity-hashmap")))
(define (hm-copy-into-ordered! self src)  ; copy a jolt map or hashmap, keeping insertion order
  (if (hm-hashmap? src)
      (for-each (lambda (kv)
                  (hm-note-key! self (car kv))
                  (hashtable-set! (hm-tbl self) (car kv) (cdr kv)))
                (hm-pairs-ordered src))
      (for-each (lambda (e)
                  (hm-note-key! self (jolt-nth e 0))
                  (hashtable-set! (hm-tbl self) (jolt-nth e 0) (jolt-nth e 1)))
                (seq->list (jolt-seq src)))))
(register-class-ctor! "HashMap"
  (lambda args
    (let* ((ht (make-hashtable hm-hash jolt=2))
           (self (make-jhost "hashmap" (hm-state ht '()))))
      (when (and (pair? args) (not (number? (car args))))
        (hm-copy-into-ordered! self (car args)))
      self)))
;; How a HashMap-shaped map's own walks and compute methods answer a change
;; made under them: a HashMap checks its modCount once a forEach / replaceAll
;; walk ends and around each compute-family function; an IdentityHashMap and a
;; WeakHashMap after every element (their compute methods are Map's defaults,
;; which check nothing); a ConcurrentHashMap and a Properties never.
(define (hm-ff-kind self)
  (let ((t (jhost-tag self)))
    (cond ((string=? t "hashmap") 'end)
          ((or (string=? t "jx-weak-hashmap") (string=? t "jx-identity-hashmap")) 'step)
          (else #f))))
;; (f k v) over the map in its order. Under a HashMap's walk a key removed by
;; an earlier call is not reached, as the JDK's table walk does not reach an
;; unlinked node.
(define (hm-walk self f)
  (let ((kind (hm-ff-kind self)) (t (hm-tbl self)) (mc (hm-mc self)))
    (if kind
        (for-each (lambda (k)
                    (case kind
                      ((end) (when (or (fx=? mc (hm-mc self)) (hashtable-contains? t k))
                               (f k (hashtable-ref t k jolt-nil))))
                      (else (f k (hashtable-ref t k jolt-nil))
                            (unless (fx=? mc (hm-mc self)) (jutil-cme!)))))
                  (hm-keys-ordered self))
        ;; weakly consistent: the entries as they stood when the walk began
        (for-each (lambda (kv) (f (car kv) (cdr kv))) (hm-pairs-ordered self)))
    (when (and (eq? kind 'end) (not (fx=? mc (hm-mc self)))) (jutil-cme!))
    jolt-nil))
;; a HashMap's compute family: a function that changed the map's structure is
;; a ConcurrentModificationException once it returns
(define (hm-apply-checked self f method . args)
  (if (eq? (hm-ff-kind self) 'end)
      (let* ((mc (hm-mc self)) (v (apply jolt-fi-call f method args)))
        (unless (fx=? mc (hm-mc self)) (jutil-cme!))
        v)
      (apply jolt-fi-call f method args)))
;; jolt-fi-call (records-dispatch.ss) calls the java.util.function argument.
(define hashmap-methods
  (list (cons "put" (lambda (self k v) (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
                                          (hm-note-key! self k)
                                          (hashtable-set! (hm-tbl self) k v) old)))
        (cons "get" (lambda (self k) (hashtable-ref (hm-tbl self) k jolt-nil)))
        (cons "getOrDefault" (lambda (self k d) (hashtable-ref (hm-tbl self) k d)))
        ;; The java.util.Map default methods. A mapping to null counts as absent,
        ;; like the JVM's, and each returns what the JVM's returns (putIfAbsent
        ;; the PREVIOUS value — nil when it did insert; the compute/merge family
        ;; the NEW value). The function argument is called through jolt-invoke,
        ;; so a Clojure fn, a reify'd Function/BiFunction and a jolt keyword all
        ;; work as the mapper.
        (cons "putIfAbsent" (lambda (self k v)
          (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
            (if (jolt-nil? old)
                (begin (hm-note-key! self k) (hashtable-set! (hm-tbl self) k v) jolt-nil)
                old))))
        (cons "computeIfAbsent" (lambda (self k f)
          (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
            (if (jolt-nil? old)
                (let ((v (hm-apply-checked self f "apply" k)))
                  (unless (jolt-nil? v)
                    (hm-note-key! self k)
                    (hashtable-set! (hm-tbl self) k v))
                  v)
                old))))
        (cons "computeIfPresent" (lambda (self k f)
          (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
            (if (jolt-nil? old)
                jolt-nil
                (let ((v (hm-apply-checked self f "apply" k old)))
                  (if (jolt-nil? v)
                      (begin (hashtable-delete! (hm-tbl self) k) (hm-drop-key! self k) jolt-nil)
                      (begin (hashtable-set! (hm-tbl self) k v) v)))))))
        (cons "compute" (lambda (self k f)
          (let* ((old (hashtable-ref (hm-tbl self) k jolt-nil))
                 (v (hm-apply-checked self f "apply" k old)))
            (cond ((not (jolt-nil? v))
                   (hm-note-key! self k) (hashtable-set! (hm-tbl self) k v) v)
                  ;; a nil result removes a key that is present, mapped to nil included
                  ((not (hashtable-contains? (hm-tbl self) k)) jolt-nil)
                  (else (hashtable-delete! (hm-tbl self) k) (hm-drop-key! self k) jolt-nil)))))
        (cons "merge" (lambda (self k v f)
          (let* ((old (hashtable-ref (hm-tbl self) k jolt-nil))
                 (v (if (jolt-nil? old) v (hm-apply-checked self f "apply" old v))))
            (cond ((not (jolt-nil? v)) (hm-note-key! self k) (hashtable-set! (hm-tbl self) k v) v)
                  ;; an absent key stays absent, the key order untouched
                  ((not (hashtable-contains? (hm-tbl self) k)) jolt-nil)
                  (else (hashtable-delete! (hm-tbl self) k) (hm-drop-key! self k) jolt-nil)))))
        ;; replace only touches a key that is already mapped — including one
        ;; mapped to nil, which is why this asks containsKey rather than reading
        ;; the value like putIfAbsent does.
        (cons "replace" (lambda (self k v)
          (if (hashtable-contains? (hm-tbl self) k)
              (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
                (hashtable-set! (hm-tbl self) k v) old)
              jolt-nil)))
        (cons "forEach" (lambda (self f)
          (hm-walk self (lambda (k v) (jolt-fi-call f "accept" k v)))))
        ;; Map.replaceAll(BiFunction): every value becomes f(key, value); a key
        ;; the function removed stays removed
        (cons "replaceAll" (lambda (self f)
          (let ((t (hm-tbl self)) (mc (hm-mc self)))
            (hm-walk self (lambda (k v)
                            (let ((nv (jolt-fi-call f "apply" k v)))
                              (when (or (fx=? mc (hm-mc self)) (hashtable-contains? t k))
                                (hashtable-set! t k nv))))))))
        (cons "containsKey" (lambda (self k) (if (hashtable-contains? (hm-tbl self) k) #t #f)))
        (cons "containsValue" (lambda (self v)
          (let ((found #f))
            (vector-for-each (lambda (k) (when (hm-veq? self v (hashtable-ref (hm-tbl self) k jolt-nil)) (set! found #t)))
                             (hashtable-keys (hm-tbl self))) found)))
        (cons "size" (lambda (self) (hashtable-size (hm-tbl self))))
        (cons "isEmpty" (lambda (self) (= 0 (hashtable-size (hm-tbl self)))))
        (cons "remove" (case-lambda
                         ;; a key that is not there changes nothing, so the
                         ;; key order (what a view's iterator checks) is untouched
                         ((self k) (if (hashtable-contains? (hm-tbl self) k)
                                       (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
                                         (hashtable-delete! (hm-tbl self) k)
                                         (hm-drop-key! self k) old)
                                       jolt-nil))
                         ;; remove(key, value): only when mapped to exactly that
                         ((self k v) (if (and (hashtable-contains? (hm-tbl self) k)
                                              (hm-veq? self v (hashtable-ref (hm-tbl self) k jolt-nil)))
                                         (begin (hashtable-delete! (hm-tbl self) k) (hm-drop-key! self k) #t)
                                         #f))))
        (cons "clone" (lambda (self)
                        (let ((m (make-jhost (jhost-tag self) (hm-state (hm-like-table self) (if (hm-weak? self) (list 'v) '())))))
                          (hm-copy-into-ordered! m self)
                          m)))
        (cons "clear" (lambda (self) (hm-clear! self) jolt-nil))
        (cons "putAll" (lambda (self m) (hm-copy-into-ordered! self m) jolt-nil))
        ;; keySet / values / entrySet are live views (jutil-concurrent.ss), as
        ;; the JDK's HashMap$KeySet / $Values / $EntrySet are
        (cons "keySet" (lambda (self) (make-jhost "jx-hm-keyset" (vector self))))
        (cons "values" (lambda (self) (make-jhost "jx-hm-values" (vector self))))
        (cons "entrySet" (lambda (self) (make-jhost "jx-hm-entryset" (vector self))))))
(register-host-methods! "hashmap" hashmap-methods)
(register-host-methods! "jx-weak-hashmap" hashmap-methods)
(register-host-methods! "jx-identity-hashmap" hashmap-methods)

;; java.util.Properties — a Hashtable of strings with getProperty/setProperty and
;; a DEFAULTS chain. The JVM is precise about which operations see that chain:
;; getProperty, propertyNames and stringPropertyNames do; the inherited Hashtable
;; surface (get, containsKey, keySet, values, size) does NOT. Checked against the
;; reference — a Properties holding only defaults answers isEmpty true and an
;; empty keySet, while its propertyNames lists them.
;;
;; The WRITE-THROUGH target is jolt's own. System/getProperties hands back an
;; object whose entries are a snapshot, because jolt computes most system
;; properties on read (user.dir tracks the source roots, java.class.path the
;; resolved paths) and they cannot be a live table; but a setProperty through it
;; must still be what System/getProperty reports, so writes go to both.
;;
;; System/getProperties used to return a plain map, which answered .getProperty
;; as a key lookup and so read nil for every system property — how a library that
;; reads them through the Properties API saw nothing.
(define (make-properties-jhost . defaults)
  (props-own-monitor!
    (make-jhost "properties"
                (props-state (make-hashtable hm-hash jolt=2) (quote ())
                             (if (pair? defaults) (car defaults) #f) #f))))
(define (props-slot self i)
  (let ((st (jhost-state self)))
    (and (fx>? (vector-length st) i) (vector-ref st i))))
(define (props-defaults self) (props-slot self 3))
(define (props-writethrough self) (props-slot self 4))
;; getProperty answers only STRING values, as on the JVM: a non-string mapping is
;; invisible to it (get still sees it) and the default is returned instead. The
;; defaults are themselves searched by getProperty, so a Properties whose
;; defaults is another Properties walks the WHOLE chain, as the JVM's recursive
;; defaults.getProperty does.
(define (props-string-ref self k dflt)
  (let ((v (hashtable-ref (hm-tbl self) k jolt-nil)))
    (cond ((string? v) v)
          ((not (jolt-nil? v)) dflt)
          (else (let ((d (props-defaults self)))
                  (cond ((not d) dflt)
                        ((and (jhost? d) (string=? (jhost-tag d) "properties"))
                         (props-string-ref d k dflt))
                        (else (let ((dv (jolt-get d k jolt-nil)))
                                (if (string? dv) dv dflt)))))))))
;; every name the object can answer for: its own keys plus its defaults' — down
;; the whole chain, since propertyNames enumerates it all on the JVM too.
(define (props-names self)
  (let* ((own (hm-keys-ordered self))
         (d (props-defaults self))
         (inherited
          (cond ((not d) (quote ()))
                ((and (jhost? d) (string=? (jhost-tag d) "properties")) (props-names d))
                (else (seq->list (jolt-keys d))))))
    (append own
            (filter (lambda (k) (not (hashtable-contains? (hm-tbl self) k))) inherited))))
(define (props-put! self k v)
  (hm-note-key! self k)
  (hashtable-set! (hm-tbl self) k v)
  (let ((wt (props-writethrough self)))
    (when wt (jolt-with-mutex sys-prop-mu (hashtable-set! wt k (if (string? v) v (jolt-str-render-one v)))))))
;; the System properties' writethrough table (host-static-methods.ss) is shared
;; with System/setProperty, so its writes take that table's mutex
(define props-methods
  (append
    hashmap-methods
    (list
      (cons "getProperty" (lambda (self k . dflt)
                            (props-string-ref self k (if (pair? dflt) (car dflt) jolt-nil))))
      ;; setProperty delegates to put on the JVM, so it returns THIS object's
      ;; previous value and never the one it was shadowing in the defaults.
      (cons "setProperty" (lambda (self k v)
                            (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
                              (props-put! self k (if (string? v) v (jolt-str-render-one v)))
                              old)))
      (cons "put" (lambda (self k v)
                    (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
                      (props-put! self k v) old)))
      (cons "remove" (letrec ((rm (case-lambda
                                    ((self k)
                                     (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
                                       ;; an absent key changes nothing, the key order included
                                       (when (hashtable-contains? (hm-tbl self) k)
                                         (hashtable-delete! (hm-tbl self) k)
                                         (hm-drop-key! self k))
                                       (let ((wt (props-writethrough self)))
                                         (when wt (jolt-with-mutex sys-prop-mu (hashtable-delete! wt k))))
                                       old))
                                    ;; remove(key, value): only when mapped to exactly that
                                    ((self k v)
                                     (and (hashtable-contains? (hm-tbl self) k)
                                          (jolt=2 v (hashtable-ref (hm-tbl self) k jolt-nil))
                                          (begin (rm self k) #t))))))
                       rm))
      (cons "clear" (lambda (self)
                      (hm-clear! self)
                      (let ((wt (props-writethrough self)))
                        (when wt (jolt-with-mutex sys-prop-mu (hashtable-clear! wt))))
                      jolt-nil))
      ;; the two enumerations that DO span the defaults chain.
      (cons "stringPropertyNames"
            (lambda (self) (apply jolt-hash-set
                             (filter (lambda (k) (string? (props-string-ref self k #f)))
                                     (props-names self)))))
      (cons "propertyNames" (lambda (self) (list->cseq (props-names self))))
      (cons "clone" (lambda (self)
                      (let ((c (if (props-defaults self) (make-properties-jhost (props-defaults self)) (make-properties-jhost))))
                        (hm-copy-into-ordered! c self)
                        c))))))
;; The JDK's Properties synchronizes every write and forEach on the object (a
;; compute function runs under it there too); reads go to its table unlocked.
(define props-synchronized
  '("put" "putIfAbsent" "setProperty" "remove" "clear" "putAll" "replace" "replaceAll"
    "merge" "compute" "computeIfAbsent" "computeIfPresent" "forEach"))
(define (props-locked proc)
  (make-arity-wrapper-procedure
    (case-lambda
      ((self) (with-props self (proc self)))
      ((self a) (with-props self (proc self a)))
      ((self a b) (with-props self (proc self a b)))
      ((self a b c) (with-props self (proc self a b c)))
      ((self . r) (with-props self (apply proc self r))))
    (procedure-arity-mask proc)
    #f))
(register-host-methods! "properties"
  (map (lambda (m) (if (member (car m) props-synchronized) (cons (car m) (props-locked (cdr m))) m))
       props-methods))
(register-class-ctor! "Properties"
  (lambda args (apply make-properties-jhost args)))
(register-class-ctor! "java.util.Properties"
  (lambda args (apply make-properties-jhost args)))
;; System/getProperties: real entries, so get/count/seq see them as on the JVM,
;; recomputed per call so user.dir and java.class.path stay current, with writes
;; going through to the override store.
(define (make-system-properties m override-tbl)
  (let ((self (props-own-monitor!
                (make-jhost "properties" (props-state (make-hashtable hm-hash jolt=2) (quote ()) #f override-tbl)))))
    (for-each (lambda (e)
                (hm-note-key! self (jolt-nth e 0))
                (hashtable-set! (hm-tbl self) (jolt-nth e 0) (jolt-nth e 1)))
              (seq->list (jolt-seq m)))
    self))

;; java.util.concurrent.ConcurrentHashMap — the HashMap shim's storage under a
;; tag of its own, so it is its own class: a ConcurrentMap, refusing null keys
;; and values, whose views are weakly consistent (jutil-concurrent.ss).
;; (get a-chm k) reads the map (clojure.core/get).
;;
;; Every write, and every walk of the key order, holds the map's private monitor
;; (chm-state): two writers corrupt a Chez hashtable, and the key order is a
;; second structure each write keeps in step with the table. A read of one key
;; (get, containsKey, size) takes no lock; against a locked writer it can only
;; see the value the write is replacing.
;;
;; The compute family runs its function OUTSIDE the monitor, as the JDK runs it
;; holding only the key's own bin: the key is CLAIMED instead (slot 3), and
;; another thread's write to that key waits until the claim ends
;; (chm-await-key!) while writes to other keys go through. Holding the monitor
;; across the function would deadlock a function that waits on another thread
;; writing any key of the map, which the JDK does not.
;; While computeIfAbsent, or compute over an absent key, runs its function, the
;; JDK holds a ReservationNode in the key's bin, and a write that reaches that
;; bin from inside the function is IllegalStateException "Recursive update"
;; (jolt-2gc66): such a claim is RESERVED. Over a present key the JDK holds the
;; bin's lock, which its own thread re-enters, so the function's own writes to
;; the key go through. Which OTHER keys share the bin is the JDK's table layout,
;; so only the claimed key itself is refused here (known-divergences).
(define (make-chm-jhost . args)
  (let ((self (make-jhost "chm" (chm-state (make-hashtable hm-hash jolt=2)))))
    (when (and (pair? args) (not (number? (car args))))
      (chm-put-all! self (car args)))
    self))
(register-class-ctor! "ConcurrentHashMap" make-chm-jhost)
(register-class-ctor! "java.util.concurrent.ConcurrentHashMap" make-chm-jhost)
(define (chm-npe!) (throw-jvm 'NullPointerException jolt-nil))
(define (chm-method name) (cdr (assoc name hashmap-methods)))
(define (chm-owner)
  (let ((r (virtual-register jolt-vreg-current-fiber))) (if (eq? r 0) (get-thread-id) r)))
(define (chm-monitor self) (vector-ref (jhost-state self) 4))
(define-record-type chm-claim (fields owner key reserved?))
;; Under the monitor: wait out another owner's claim on k (the wait lets the
;; monitor go). Every claim on one key is one owner's, since a second owner
;; waits here; a reserved one of this owner's own is the Recursive update.
(define (chm-await-key! self k)
  (let ((st (jhost-state self)))
    (let loop ()
      (unless (null? (vector-ref st 3))
        (let ((on-k (filter (lambda (c) (jolt=2 (chm-claim-key c) k)) (vector-ref st 3))))
          (cond ((null? on-k))
                ((not (eqv? (chm-claim-owner (car on-k)) (chm-owner)))
                 (vector-set! st 5 (fx+ (vector-ref st 5) 1))
                 (guard (e (#t (vector-set! st 5 (fx- (vector-ref st 5) 1)) (raise e)))
                   (monitor-object-wait! (chm-monitor self) #f))
                 (vector-set! st 5 (fx- (vector-ref st 5) 1))
                 (loop))
                ((exists chm-claim-reserved? on-k)
                 (throw-jvm 'IllegalStateException "Recursive update"))))))))
(define (chm-claim! self k reserved?)   ; under the monitor, after chm-await-key!
  (let ((c (make-chm-claim (chm-owner) k reserved?)) (st (jhost-state self)))
    (vector-set! st 3 (cons c (vector-ref st 3)))
    c))
(define (chm-release! self c)           ; under the monitor
  (let ((st (jhost-state self)))
    (vector-set! st 3 (remq c (vector-ref st 3)))
    (unless (eqv? 0 (vector-ref st 5)) (monitor-notify! (chm-monitor self) #t))))
;; under the monitor: k mapped to v, or to nothing when v is nil
(define (chm-store! self k v)
  (let ((t (hm-tbl self)))
    (cond ((not (jolt-nil? v)) (hm-note-key! self k) (hashtable-set! t k v))
          ((hashtable-contains? t k) (hashtable-delete! t k) (hm-drop-key! self k)))
    v))
(define chm-reserve (list 'reserve))
(define chm-lock (list 'lock))
;; One compute-family call. Under the monitor, once no other owner claims k,
;; (start old) answers the call's result, or chm-reserve / chm-lock to claim k;
;; then (call old) runs with the monitor let go and its value is stored. A call
;; that throws leaves the map as it was; the claim ends either way, except that
;; a fiber parking inside the function keeps it.
(define (chm-compute! self k start call)
  (let* ((old jolt-nil)
         (s (with-chm self
              (chm-await-key! self k)
              (set! old (hashtable-ref (hm-tbl self) k jolt-nil))
              (let ((d (start old)))
                (cond ((eq? d chm-reserve) (chm-claim! self k #t))
                      ((eq? d chm-lock) (chm-claim! self k #f))
                      (else d))))))
    (if (chm-claim? s)
        (let ((done #f))
          (dynamic-wind
            (lambda () #f)
            (lambda ()
              (let ((v (call old)))
                (set! done #t)
                (with-chm self (chm-release! self s) (chm-store! self k v))))
            (lambda ()
              (unless (or done (jolt-park-unwinding?))
                (set! done #t)
                (with-chm self (chm-release! self s))))))
        s)))
(define (chm-put-all! self m)
  (for-each (lambda (e)
              (let ((k (jolt-nth e 0)) (v (jolt-nth e 1)))
                (when (or (jolt-nil? k) (jolt-nil? v)) (chm-npe!))
                (with-chm self (chm-await-key! self k) (chm-store! self k v))))
            (if (hm-hashmap? m)
                (map (lambda (kv) (make-map-entry (car kv) (cdr kv))) (hm-pairs-ordered m))
                (seq->list (jolt-seq m)))))
;; hashmap's methods, refusing a null key or value as ConcurrentHashMap does;
;; fixed arities, so a call allocates nothing for the check
(register-host-methods! "chm"
  (append hashmap-methods
          (let ((put (chm-method "put")) (pia (chm-method "putIfAbsent"))
                (rm (chm-method "remove")) (rep (chm-method "replace")))
            (list (cons "put" (lambda (self k v)
                                (when (or (jolt-nil? k) (jolt-nil? v)) (chm-npe!))
                                (with-chm self (chm-await-key! self k) (put self k v))))
                  (cons "putIfAbsent" (lambda (self k v)
                                        (when (or (jolt-nil? k) (jolt-nil? v)) (chm-npe!))
                                        (with-chm self (chm-await-key! self k) (pia self k v))))
                  (cons "get" (lambda (self k) (when (jolt-nil? k) (chm-npe!)) (hashtable-ref (hm-tbl self) k jolt-nil)))
                  (cons "getOrDefault" (lambda (self k d) (when (jolt-nil? k) (chm-npe!)) (hashtable-ref (hm-tbl self) k d)))
                  (cons "containsKey" (lambda (self k) (when (jolt-nil? k) (chm-npe!)) (if (hashtable-contains? (hm-tbl self) k) #t #f)))
                  (cons "containsValue" (lambda (self v)
                                          (when (jolt-nil? v) (chm-npe!))
                                          (if (exists (lambda (kv) (jolt=2 v (cdr kv))) (hm-pairs-ordered self)) #t #f)))
                  (cons "mappingCount" (lambda (self) (hashtable-size (hm-tbl self))))
                  (cons "remove" (case-lambda
                                   ((self k) (when (jolt-nil? k) (chm-npe!))
                                    (with-chm self (chm-await-key! self k) (rm self k)))
                                   ;; remove(key, null) removes nothing, as the JDK's
                                   ((self k v) (when (jolt-nil? k) (chm-npe!))
                                    (and (not (jolt-nil? v))
                                         (with-chm self (chm-await-key! self k) (rm self k v))))))
                  (cons "replace" (case-lambda
                                    ((self k v) (when (or (jolt-nil? k) (jolt-nil? v)) (chm-npe!))
                                     (with-chm self (chm-await-key! self k) (rep self k v)))
                                    ((self k old v) (when (or (jolt-nil? k) (jolt-nil? old) (jolt-nil? v)) (chm-npe!))
                                     (with-chm self
                                       (chm-await-key! self k)
                                       (if (and (hashtable-contains? (hm-tbl self) k)
                                                (jolt=2 old (hashtable-ref (hm-tbl self) k jolt-nil)))
                                           (begin (hashtable-set! (hm-tbl self) k v) #t)
                                           #f)))))
                  (cons "merge" (lambda (self k v f)
                                  (when (or (jolt-nil? k) (jolt-nil? v) (jolt-nil? f)) (chm-npe!))
                                  (chm-compute! self k
                                    (lambda (old) (if (jolt-nil? old) (chm-store! self k v) chm-lock))
                                    (lambda (old) (jolt-fi-call f "apply" old v)))))
                  (cons "computeIfPresent" (lambda (self k f)
                                             (when (or (jolt-nil? k) (jolt-nil? f)) (chm-npe!))
                                             (chm-compute! self k
                                               (lambda (old) (if (jolt-nil? old) jolt-nil chm-lock))
                                               (lambda (old) (jolt-fi-call f "apply" k old)))))
                  ;; a present key answers its value from one lock-free lookup, as
                  ;; the JDK's does from the bin's first node: the map holds no
                  ;; nil, so nil is absent
                  (cons "computeIfAbsent"
                        (lambda (self k f)
                          (when (or (jolt-nil? k) (jolt-nil? f)) (chm-npe!))
                          (let ((old (hashtable-ref (hm-tbl self) k jolt-nil)))
                            (if (jolt-nil? old)
                                (chm-compute! self k
                                  (lambda (old) (if (jolt-nil? old) chm-reserve old))
                                  (lambda (old) (jolt-fi-call f "apply" k)))
                                old))))
                  (cons "compute" (lambda (self k f)
                                    (when (or (jolt-nil? k) (jolt-nil? f)) (chm-npe!))
                                    (chm-compute! self k
                                      (lambda (old) (if (jolt-nil? old) chm-reserve chm-lock))
                                      (lambda (old) (jolt-fi-call f "apply" k old)))))
                  (cons "putAll" (lambda (self m) (chm-put-all! self m) jolt-nil))
                  (cons "clear" (lambda (self) (with-chm self (hm-clear! self)) jolt-nil))
                  ;; each value becomes f(key, value) unless a write changed it
                  ;; meanwhile, when f sees the new one, as the JDK's replaceNode
                  ;; loop does; f runs with the monitor let go
                  (cons "replaceAll" (lambda (self f)
                                       (for-each (lambda (kv)
                                                   (let loop ((v (cdr kv)))
                                                     (let ((nv (jolt-fi-call f "apply" (car kv) v)))
                                                       (when (jolt-nil? nv) (chm-npe!))
                                                       (let ((cur (with-chm self
                                                                    (chm-await-key! self (car kv))
                                                                    (let ((cur (hashtable-ref (hm-tbl self) (car kv) jolt-nil)))
                                                                      (if (or (eq? cur v) (jolt=2 cur v))
                                                                          (begin (hashtable-set! (hm-tbl self) (car kv) nv) jolt-nil)
                                                                          cur)))))
                                                         (unless (jolt-nil? cur) (loop cur))))))
                                                 (hm-pairs-ordered self))
                                       jolt-nil))
                  (cons "clone" (lambda (self) (make-chm-jhost self)))))))
;; WeakHashMap: the HashMap shim over a weak table (Chez's generic weak
;; hashtables hold their keys weakly under jolt's own hash and equality), so an
;; entry whose key nothing else holds goes at the next collection, as on the JDK.
(define (make-weak-hashmap-jhost . args)
  (let ((self (make-jhost "jx-weak-hashmap" (hm-state (make-weak-hashtable hm-hash jolt=2) (list 'v)))))
    (when (and (pair? args) (not (number? (car args)))) (hm-copy-into-ordered! self (car args)))
    self))
(register-class-ctor! "WeakHashMap" make-weak-hashmap-jhost)
(register-class-ctor! "java.util.WeakHashMap" make-weak-hashmap-jhost)
;; IdentityHashMap: the HashMap shim over an eq? table, so a key — and a value
;; where the JDK compares one (containsValue, remove(k, v)) — is matched by
;; reference, as == matches it.
(define (make-identity-hashmap-jhost . args)
  (let ((self (make-jhost "jx-identity-hashmap" (hm-state (make-eq-hashtable) '()))))
    (when (and (pair? args) (not (number? (car args)))) (hm-copy-into-ordered! self (car args)))
    self))
(register-class-ctor! "IdentityHashMap" make-identity-hashmap-jhost)
(register-class-ctor! "java.util.IdentityHashMap" make-identity-hashmap-jhost)
;; java.util.concurrent.atomic.Atomic{Reference,Integer,Long,Boolean}: a
;; thread-safe mutable cell (mutex-guarded, shared heap). One "atomic" jhost
;; serves all four; the numeric ops are meaningful only on Integer/Long.
;; AtomicReference CAS compares object identity, while the primitive wrappers
;; compare their unboxed values. Keep that closed distinction in the cell
;; instead of making the shared CAS path guess from the values it contains (or
;; calling a stored, potentially generic comparator while the mutex is held).
;; One jhost SHAPE serves all four, but each reports its own JVM class, so the
;; tag has to carry the kind: (class (AtomicLong. 1)) is AtomicLong, not a
;; placeholder, and only the two numeric ones answer true to (instance? Number x).
;; Same reason the two ThreadLocal storage shims carry separate tags — a caller
;; asks which one it holds with instance?. The kind stays in the state as well:
;; the CAS and conversion paths switch on it, and reading a symbol beats parsing
;; the tag string back.
(define (atomic-tag-for kind)
  (case kind
    ((integer) "atomic-integer")
    ((long) "atomic-long")
    ((boolean) "atomic-boolean")
    (else "atomic-reference")))
(define atomic-tags '("atomic-integer" "atomic-long" "atomic-boolean" "atomic-reference"))
;; A jhost is a nongenerative record, so an atomic written into a jolt image
;; travels with its tag STRING, and this build still reads image formats 2 to 7.
;; Every atomic dumped before the tag split carries the old shared "atomic", and
;; a tag with no method table answers no method at all — the restored cell would
;; be inert. Registering the same table under it keeps those images working
;; exactly as they did: the methods read the kind out of the state, and an old
;; atomic reports :object to (class x) there, as it always has (there is one FQN
;; per tag, and the old tag names four classes).
(define atomic-legacy-tag "atomic")
(define (make-atomic init kind)
  (make-jhost (atomic-tag-for kind) (vector (box init) (make-mutex) kind)))
(define (atomic-box self) (vector-ref (jhost-state self) 0))
(define (atomic-lock self) (vector-ref (jhost-state self) 1))
(define (atomic-kind self) (vector-ref (jhost-state self) 2))
;; Convert a modeled Java method argument at the signature boundary. Numeric
;; atomic cells never contain an unbounded jolt integer: the constructor and
;; every value-taking method accept exactly the primitive width named by their
;; Java signature. Keep this outside the mutex because conversion may throw.
(define (atomic-convert self value)
  (case (atomic-kind self)
    ((integer) (jolt-int-cast value))
    ((long) (jolt-long-cast value))
    (else value)))
;; Java's primitive atomic arithmetic is two's-complement arithmetic. Its
;; result wraps even though a checked cast at a method boundary rejects an
;; out-of-range argument. VALUE is an exact sum or difference of already
;; converted cell data, so this helper is leaf-only inside the counted mutex.
(define (atomic-wrap self value)
  (case (atomic-kind self)
    ((integer) (jolt-unchecked-int value))
    ((long) (jolt-wrap64 value))
    (else value)))
(define (atomic-numeric-transition! self delta return-old?)
  (jolt-with-mutex (atomic-lock self)
    (let* ((old (unbox (atomic-box self)))
           (next (atomic-wrap self (+ old delta))))
      (set-box! (atomic-box self) next)
      (if return-old? old next))))
;; CAS under the atomic's mutex. updateAndGet/getAndUpdate run the user fn
;; LOCK-FREE in a CAS retry loop (JVM parity), so the fn may re-enter the same
;; atomic; a mutex-held fn deadlocked on the non-recursive Chez mutex there
;; (PSL R4 cluster 4).
(define (atomic-cas! self o n)
  (let* ((kind (atomic-kind self))
         (expected (atomic-convert self o))
         (next (atomic-convert self n)))
    (jolt-with-mutex (atomic-lock self)
      (let ((current (unbox (atomic-box self))))
        (if (case kind
              ((reference boolean) (eq? current expected))
              ((integer long) (= current expected))
              (else #f))
            (begin (set-box! (atomic-box self) next) #t)
            #f)))))
(let ((ref-ctor (lambda args
                  (make-atomic (if (pair? args) (car args) jolt-nil) 'reference)))
      (int-ctor (lambda args
                  (make-atomic (jolt-int-cast (if (pair? args) (car args) 0)) 'integer)))
      (long-ctor (lambda args
                   (make-atomic (jolt-long-cast (if (pair? args) (car args) 0)) 'long)))
      (bool-ctor (lambda args
                   (make-atomic (if (pair? args) (car args) #f) 'boolean))))
  (for-each (lambda (n) (register-class-ctor! n ref-ctor))
            '("AtomicReference" "java.util.concurrent.atomic.AtomicReference"))
  (for-each (lambda (n) (register-class-ctor! n int-ctor))
            '("AtomicInteger" "java.util.concurrent.atomic.AtomicInteger"))
  (for-each (lambda (n) (register-class-ctor! n long-ctor))
            '("AtomicLong" "java.util.concurrent.atomic.AtomicLong"))
  (for-each (lambda (n) (register-class-ctor! n bool-ctor))
            '("AtomicBoolean" "java.util.concurrent.atomic.AtomicBoolean")))
;; The four tags share ONE method table — the methods read the kind out of the
;; state, so nothing here is per-class.
(let ((atomic-methods
  (list (cons "get" (lambda (self) (unbox (atomic-box self))))
        ;; Serialization of the plain set method against read-modify-write
        ;; methods is a separate concern. Preserve its current write boundary
        ;; here while ensuring conversion completes before state changes.
        (cons "set" (lambda (self v)
          (let ((next (atomic-convert self v)))
            ;; set is a volatile write: what was built before it is visible to
            ;; a get on another thread
            (memory-order-release)
            (set-box! (atomic-box self) next)
            jolt-nil)))
        (cons "getAndSet" (lambda (self v)
          (let ((next (atomic-convert self v)))
            (jolt-with-mutex (atomic-lock self)
              (let ((o (unbox (atomic-box self))))
                (set-box! (atomic-box self) next)
                o)))))
        (cons "compareAndSet" (lambda (self o n) (atomic-cas! self o n)))
        (cons "updateAndGet" (lambda (self f)
          (let loop ((v (unbox (atomic-box self))))
            ;; atomic-cas! deliberately re-normalizes V and N. This keeps every
            ;; CAS caller behind one typed boundary; conversion is idempotent.
            (let ((n (atomic-convert self (jolt-fi-call f "apply" v))))
              (if (atomic-cas! self v n) n (loop (unbox (atomic-box self))))))))
        (cons "getAndUpdate" (lambda (self f)
          (let loop ((v (unbox (atomic-box self))))
            (let ((n (atomic-convert self (jolt-fi-call f "apply" v))))
              (if (atomic-cas! self v n) v (loop (unbox (atomic-box self))))))))
        (cons "incrementAndGet"
          (lambda (self) (atomic-numeric-transition! self 1 #f)))
        (cons "decrementAndGet"
          (lambda (self) (atomic-numeric-transition! self -1 #f)))
        (cons "getAndIncrement"
          (lambda (self) (atomic-numeric-transition! self 1 #t)))
        (cons "getAndDecrement"
          (lambda (self) (atomic-numeric-transition! self -1 #t)))
        (cons "addAndGet" (lambda (self d)
          (let ((delta (atomic-convert self d)))
            (atomic-numeric-transition! self delta #f))))
        (cons "getAndAdd" (lambda (self d)
          (let ((delta (atomic-convert self d)))
            (atomic-numeric-transition! self delta #t))))
        (cons "intValue" (lambda (self)
          (if (eq? (atomic-kind self) 'long)
              (jolt-unchecked-int (unbox (atomic-box self)))
              (jnum->exact (unbox (atomic-box self))))))
        (cons "longValue" (lambda (self) (jnum->exact (unbox (atomic-box self)))))
        (cons "toString" (lambda (self) (jolt-str-render-one (unbox (atomic-box self))))))))
  (for-each (lambda (t) (register-host-methods! t atomic-methods))
            (cons atomic-legacy-tag atomic-tags)))
;; java.util.Collections/unmodifiable* and synchronized* are views over their
;; argument (jutil-views.ss); they used to answer the argument itself.
(register-class-statics! "java.util.Collections"
  (list (cons "emptyList" (lambda _ (jolt-vector))) (cons "emptyMap" (lambda _ (jolt-hash-map)))))

;; Collections/enumeration: an Enumeration over a collection's elements, which
;; enumeration-seq and SequenceInputStream drive. Collections/list drains any
;; Enumeration (a StringTokenizer, a reify) into a fresh ArrayList. Running
;; past the end is NoSuchElementException with no message, as the JDK's.
(define (seq-enum-rest self) (vector-ref (jhost-state self) 0))
(register-host-methods! "seq-enumeration"
  (list (cons "hasMoreElements" (lambda (self) (not (jolt-nil? (seq-enum-rest self)))))
        (cons "nextElement"
              (lambda (self)
                (let ((s (seq-enum-rest self)))
                  (when (jolt-nil? s) (throw-jvm 'NoSuchElementException jolt-nil))
                  (vector-set! (jhost-state self) 0 (jolt-next s))
                  (jolt-first s))))))
(register-class-statics! "java.util.Collections"
  (list (cons "enumeration"
              (lambda (c) (make-jhost "seq-enumeration" (vector (jolt-seq c)))))
        (cons "list"
              (lambda (e)
                (let loop ((acc '()))
                  (if (jolt-truthy? (record-method-dispatch e "hasMoreElements" jolt-nil))
                      (loop (cons (record-method-dispatch e "nextElement" jolt-nil) acc))
                      (make-arraylist (reverse acc))))))))

;; A mutable set over the same value-keyed hashtable (element -> #t).
;; Constructors: () | (capacity) [ignored] | (coll) [copy].
(define-jhost-tag-pred (hs-hashset? t) (string=? t "hashset"))
(define (hs->list self) (hm-keys-ordered self))
(define (hs-ctor tag)
  (lambda args
    (let* ((ht (make-hashtable hm-hash jolt=2))
           (self (make-jhost tag (hm-state ht '()))))
      (when (and (pair? args) (not (number? (car args))))
        (for-each (lambda (e)
                    (hm-note-key! self e)
                    (hashtable-set! ht e #t))
                  (seq->list (jolt-seq (car args)))))
      self)))
(register-class-ctor! "HashSet" (hs-ctor "hashset"))
(register-class-ctor! "java.util.HashSet" (hs-ctor "hashset"))
;; HashMap.newHashMap / HashSet.newHashSet (JDK 19): sized for N mappings, which
;; this table needs no hint for
(define (hm-sized-new what make)
  (lambda (n)
    (when (< (jnum->exact n) 0)
      (throw-jvm 'IllegalArgumentException
                 (string-append "Negative number of " what ": " (jolt-str-render-one n))))
    (make)))
(register-class-statics! "java.util.HashMap"
  (list (cons "newHashMap" (hm-sized-new "mappings" (lambda () (make-jhost "hashmap" (hm-state (make-hashtable hm-hash jolt=2) '())))))))
(register-class-statics! "java.util.HashSet"
  (list (cons "newHashSet" (hm-sized-new "elements" (lambda () ((hs-ctor "hashset")))))))
;; the linked subclasses inherit them
(register-class-statics! "java.util.LinkedHashMap"
  (list (cons "newHashMap" (host-static-ref "java.util.HashMap" "newHashMap"))))
(register-class-statics! "java.util.LinkedHashSet"
  (list (cons "newHashSet" (host-static-ref "java.util.HashSet" "newHashSet"))))

(register-host-methods! "hashset"
  (list (cons "add" (lambda (self x) (let ((had (hashtable-contains? (hm-tbl self) x)))
                                        (hm-note-key! self x)
                                        (hashtable-set! (hm-tbl self) x #t) (not had))))
        ;; an absent element changes nothing, the modCount included
        (cons "remove" (lambda (self x) (and (hashtable-contains? (hm-tbl self) x)
                                             (begin (hashtable-delete! (hm-tbl self) x)
                                                    (hm-drop-key! self x) #t))))
        (cons "contains" (lambda (self x) (if (hashtable-contains? (hm-tbl self) x) #t #f)))
        (cons "addAll" (lambda (self c)
                         (fold-left (lambda (ch x)
                                      (if (hashtable-contains? (hm-tbl self) x)
                                          ch
                                          (begin (hm-note-key! self x) (hashtable-set! (hm-tbl self) x #t) #t)))
                                    #f (seq->list (jolt-seq c)))))
        (cons "containsAll" (lambda (self c)
                              (for-all (lambda (x) (hashtable-contains? (hm-tbl self) x)) (seq->list (jolt-seq c)))))
        (cons "removeAll" (lambda (self c)
                            (fold-left (lambda (ch x)
                                         (if (hashtable-contains? (hm-tbl self) x)
                                             (begin (hashtable-delete! (hm-tbl self) x) (hm-drop-key! self x) #t)
                                             ch))
                                       #f (seq->list (jolt-seq c)))))
        (cons "retainAll" (lambda (self c)
                            (let ((keep (seq->list (jolt-seq c))))
                              (fold-left (lambda (ch x)
                                           (if (exists (lambda (k) (jolt=2 k x)) keep)
                                               ch
                                               (begin (hashtable-delete! (hm-tbl self) x) (hm-drop-key! self x) #t)))
                                         #f (hs->list self)))))
        (cons "clone" (lambda (self) ((hs-ctor (jhost-tag self)) (list->cseq (hs->list self)))))
        ;; HashMap$KeyIterator: fail-fast over the set's modCount
        (cons "iterator" (lambda (self) (hs-iterator self)))
        (cons "size" (lambda (self) (hashtable-size (hm-tbl self))))
        (cons "isEmpty" (lambda (self) (= 0 (hashtable-size (hm-tbl self)))))
        (cons "clear" (lambda (self) (hm-clear! self) jolt-nil))
        ;; The Collection method, not a List one — a HashSet answers it on the
        ;; JVM and had none here at all, so (.toArray h) was "No matching field
        ;; found: toArray for class java.util.HashSet". Same jcoll-to-array the
        ;; ArrayList family uses, over the set's own iteration order.
        (cons "toArray" (lambda (self . args) (jcoll-to-array (hs->list self) args)))
        ;; Iterable.forEach and Collection.removeIf, the defaults HashSet keeps:
        ;; over its iterator, so a change the action makes is caught at the
        ;; next element
        (cons "forEach" (lambda (self f) (jutil-walk-for-each (hs-iterator self) f)))
        (cons "removeIf" (lambda (self pred) (jutil-walk-remove-if (hs-iterator self) pred)))))
(define (hs-iterator self)
  (make-jutil-ff-iterator "hashset-iterator" (hs->list self)
                          (lambda (x) (hashtable-delete! (hm-tbl self) x) (hm-drop-key! self x))
                          jutil-identity (jhost-state self) 2))
(register-jutil-iterator! "hashset-iterator")
;; reduce over a fail-fast collection walks its iterator (seq.ss
;; jolt-reduce-iter-hook); a Map is no Iterable and reduces over its seq
(set! jolt-reduce-iter-hook
  (lambda (x)
    (and (jutil-modcount x)
         (not (eq? (jutil-coll-kind x) 'map))
         (host-method-ref (jhost-tag x) "iterator")
         (let ((it (record-method-dispatch x "iterator" jolt-nil)))
           (if (al-family? x)
               ;; ArrayList$Itr / LinkedList$ListItr, stepped directly
               (lambda () (if (ali-has-next? it) (ali-next it) reduce-iter-done))
               (let ((has? (iterator-seq-method it "hasNext")) (next (iterator-seq-method it "next")))
                 (lambda () (if (jolt-truthy? (has? it)) (next it) reduce-iter-done))))))))
(register-modcount! "hashset" hm-mc)
(register-modcount! "hashmap" hm-mc)
(register-modcount! "jx-weak-hashmap" hm-mc)
(register-modcount! "jx-identity-hashmap" hm-mc)
(register-seq-arm! hs-hashset? (lambda (x) (jutil-ff-seq x (hs->list x))))

;; the HashMap family and HashSet are a java.util.Map / Set to =, hash, pr, str
;; and reduce-kv (java/jutil-colls.ss), in their insertion order. An ArrayList
;; (and Arrays$ArrayList, ImmutableCollections$ListN) is a RandomAccess List,
;; printed [..]; a LinkedList a List, printed (..); an ArrayDeque only a
;; Collection, which keeps the identity equals/hashCode registered above and
;; prints as #object over its toString.
;; the entries =, hash and print walk, which see only keys and values
(define (hm-entries self)
  (map (lambda (kv) (make-map-entry (car kv) (cdr kv))) (hm-pairs-ordered self)))
;; A Map seqs through its entrySet's iterator on the JVM (RT.seq), so a seq
;; yields the map's own live entries (HashMap$Node, ConcurrentHashMap$MapEntry
;; ...), as a LinkedHashMap's already does, not detached MapEntry pairs
;; (jolt-12zv0). The entrySet views (jutil-concurrent.ss) build them from the
;; key order, never back through here.
(define (hm-seq-entries self)
  (let ((t (jhost-tag self)))
    (if (or (string=? t "hashmap") (string=? t "jx-identity-hashmap") (string=? t "jx-weak-hashmap"))
        (hm-node-list self)
        (jutil-coll-elems (record-method-dispatch self "entrySet" jolt-nil)))))
(register-jutil-coll! "hashmap" 'map hm-entries)
(register-jutil-coll! "chm" 'map hm-entries)
(register-jutil-coll! "jx-weak-hashmap" 'map hm-entries)
(register-jutil-coll! "jx-identity-hashmap" 'map hm-entries)
(register-jutil-coll! "properties" 'map hm-entries)
(register-jutil-coll! "hashset" 'set hs->list)
(register-jutil-coll! "arraylist" 'ralist al->list)
(register-jutil-coll! "linkedlist" 'list al->list)
;; Collection.toArray on the TreeSet / TreeMap views (java/tree-map.ss), which
;; are shared with targets that have no Java arrays.
(for-each (lambda (tag)
            (register-host-methods! tag
              (list (cons "toArray" (case-lambda
                                      ((self) (jcoll-to-array (tm-elems self) '()))
                                      ((self a) (jcoll-to-array (tm-elems self) (list a))))))))
          (append '("treeset" "treemap-keyset") tm-values-tags tm-entryset-tags))
(register-get-arm! hm-hashmap?
                   (lambda (coll k d) (hashtable-ref (hm-tbl coll) k d)))
;; count / contains? over the mutable map shim (clojure.core/count + contains?,
;; which core.cache's SoftCache uses on its backing ConcurrentHashMap).
(define jhost-hashmap? hm-hashmap?)
(register-count-arm! jhost-hashmap? (lambda (c) (hashtable-size (hm-tbl c))))
(register-contains-arm! jhost-hashmap?
  (lambda (c k) (if (hashtable-contains? (hm-tbl c) k) #t #f)))

;; ---- java.lang.ref.Soft/WeakReference + ReferenceQueue ----------------------
;; Real GC reclamation via Chez's generational collector: the referent is held
;; through a weak pair (collected once otherwise unreachable, leaving the bwp
;; object), and a guardian registered on the referent makes the reference itself
;; available the moment its referent is reclaimed — which the ReferenceQueue
;; surfaces as enqueued, exactly like the JVM. (Chez has no softer-than-weak
;; reference, so a SoftReference clears on unreachability rather than under memory
;; pressure — its SoftCache evicts more eagerly than the JVM's, but it is genuine
;; GC eviction, not an unbounded strong cache. Immediates like fixnums/keywords
;; are never collected.)
;; ref-queue state: #(guardian pending-front pending-rear) — the pending refs as
;; a front/rear two-list queue (rq-add! is one cons; the old single list was
;; re-copied per append, O(n^2) across a mass eviction's pump).
;; reference state: #(weak-pair queue enqueued?).
(define (rq-guardian-of q) (vector-ref (jhost-state q) 0))
(define (rq-add! q ref)
  (let ((st (jhost-state q))) (vector-set! st 2 (cons ref (vector-ref st 2)))))
(define (rq-pump! q)                                  ; drain GC-reclaimed refs onto the queue
  (let loop ()
    (let ((rep ((rq-guardian-of q)))) (when rep (rq-add! q rep) (loop)))))
(define (rq-poll q)
  (rq-pump! q)
  (let ((st (jhost-state q)))
    (when (and (null? (vector-ref st 1)) (pair? (vector-ref st 2)))
      (vector-set! st 1 (reverse (vector-ref st 2)))
      (vector-set! st 2 '()))
    (let ((l (vector-ref st 1)))
      (if (null? l) jolt-nil (begin (vector-set! st 1 (cdr l)) (car l))))))
(define (a-ref-queue? x) (and (jhost? x) (string=? (jhost-tag x) "ref-queue")))
;; Each reference class has its own tag (class-hierarchy.ss maps the tag to the
;; class), sharing one method table: the two differ only in the class they
;; report, and a WeakReference used to report as a SoftReference.
(define (make-reference tag v rest)
  (let* ((rq (if (pair? rest) (car rest) jolt-nil))
         (ref (make-jhost tag (vector (weak-cons v #f) rq #f))))
    (when (a-ref-queue? rq) ((rq-guardian-of rq) v ref))   ; fire on the referent's collection
    ref))
(for-each (lambda (nm) (register-class-ctor! nm (lambda (v . rest) (make-reference "soft-ref" v rest))))
          '("SoftReference" "java.lang.ref.SoftReference"))
(for-each (lambda (nm) (register-class-ctor! nm (lambda (v . rest) (make-reference "weak-ref" v rest))))
          '("WeakReference" "java.lang.ref.WeakReference"))
;; the referent, or nil once cleared — by the program or by the collector
(define (reference-referent self)
  (let ((r (car (vector-ref (jhost-state self) 0))))
    (if (bwp-object? r) jolt-nil r)))
(register-host-methods! "weak-ref"
  (list (cons "get" reference-referent)
        (cons "clear" (lambda (self) (set-car! (vector-ref (jhost-state self) 0) jolt-nil) jolt-nil))
        ;; Reference.refersTo(obj): identity against the referent, so a cleared
        ;; reference refers to null — the JDK 16 spelling of "is it still x"
        ;; that avoids strengthening the referent the way get() does.
        (cons "refersTo" (lambda (self x) (eq? (reference-referent self) x)))
        (cons "isEnqueued" (lambda (self) (vector-ref (jhost-state self) 2)))
        (cons "enqueue" (lambda (self)
          (let* ((st (jhost-state self)) (rq (vector-ref st 1)))
            (if (vector-ref st 2) #f
                (begin (vector-set! st 2 #t) (when (a-ref-queue? rq) (rq-add! rq self)) #t)))))))
(alias-host-methods! "soft-ref" "weak-ref")
(for-each (lambda (nm) (register-class-ctor! nm (lambda _ (make-jhost "ref-queue" (vector (make-guardian) '() '())))))
          '("ReferenceQueue" "java.lang.ref.ReferenceQueue"))
(register-host-methods! "ref-queue"
  (list (cons "poll" (lambda (self . _) (rq-poll self)))
        (cons "remove" (lambda (self . _) (rq-poll self)))))

;; ---- StringReader -----------------------------------------------------------
;; state: a vector #(string pos marked).
(register-class-ctor! "StringReader"
  ;; src is a String or a char[] ((StringReader. (char-array s)) — selmer's parser
  ;; reads templates this way); a char-array becomes the string of its chars.
  (lambda (src . _)
    (make-jhost "string-reader"
      (vector (cond ((string? src) src)
                    ((jolt-array? src) (apply string-append (map jolt-str-render-one (seq->list (jolt-seq src)))))
                    (else (jolt-str-render-one src)))
              0 0 #f))))
;; Slot 3 is set on the string-reader io/reader hands back over an in-memory
;; source (a jar entry, a baked resource): that one is a BufferedReader on the
;; JVM, to class and instance? alike, where a constructed StringReader is not.
(define (string-reader-buffered? x)
  (let ((st (jhost-state x))) (and (fx>? (vector-length st) 3) (vector-ref st 3))))
(register-class-arm! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "string-reader")
                                      (string-reader-buffered? x)))
                     (lambda (x) "java.io.BufferedReader"))
;; Reader.read(char[]) and Reader.read(char[], int, int) are BOTH overloads on
;; java.io.Reader, and the one-argument form means "fill the whole array". Only
;; the three-argument form was destructured here, so `(.read r cbuf)` — the
;; shorter spelling, and the one a streaming loop reaches for first — died in
;; `caddr: incorrect list structure` on every reader in this file rather than
;; reading anything. char-reader (io-streams.ss) already defaulted correctly;
;; this is the same defaulting, shared by the readers that did not.
(define (reader-read-buf-args rest)
  (let ((cbuf (car rest)))
    (if (>= (length rest) 3)
        (values cbuf (jnum->exact (cadr rest)) (jnum->exact (caddr rest)))
        (values cbuf 0 (ja-len cbuf)))))

;; read(cbuf, off, len) for a reader that hands out one character at a time:
;; GET1 answers a char or the eof object, MORE? whether another GET1 would
;; answer without blocking. The JVM's contract is "block until SOME input is
;; available" and then hand back what is there, not "fill the buffer": a read
;; that waits for LEN characters over a pipe, a socket or a terminal stalls
;; until the sender has said more, which an SSE stream or an interactive
;; protocol never does. So only the first character may block. The byte side
;; is the same rule (in-stream's read, io-streams.ss).
(define (reader-fill-available! buf off len get1 more?)
  (let loop ((i 0))
    (if (or (>= i len) (and (> i 0) (not (more?))))
        (->num i)
        (let ((c (get1)))
          (if (eof-object? c)
              (if (= i 0) -1 (->num i))
              (begin (ja-set! buf (+ off i) c) (loop (+ i 1))))))))

;; Does reader R say its next read would not block? A reader that cannot
;; answer — a reify with no ready, one whose ready throws — says no, which is
;; java.io.Reader's own default.
(define (reader-ready? r)
  (guard (_ (#t #f))
    (jolt-truthy? (record-method-dispatch r "ready" jolt-nil))))

;; Reader.read(CharBuffer), which every java.io.Reader inherits: the buffer's
;; remaining room is read through the reader's own read(char[],int,int) and put
;; at its position, which advances by what was read. The JDK's own body for a
;; buffer with no array, and the same answer for one with an array; a read-only
;; buffer raises the JDK's ReadOnlyBufferException from the put.
(define (reader-read-charbuffer self cb)
  (let* ((rem (nb-remaining cb))
         (tmp (na-char-array rem))
         (n (jnum->exact (record-method-dispatch self "read" (jolt-list tmp 0 rem)))))
    (when (> n 0) (nb-put cb tmp 0 n))
    (->num n)))
;; A reader's "read" method with the CharBuffer overload in front of it.
(define (with-charbuffer-read read)
  (lambda (self . rest)
    (if (and (pair? rest) (null? (cdr rest)) (nb? (car rest)))
        (reader-read-charbuffer self (car rest))
        (apply read self rest))))

;; close drops the string; every read reaches it through sr-s, which then
;; raises what a closed java.io.StringReader does.
(define (sr-s self)
  (or (vector-ref (jhost-state self) 0)
      (throw-jvm 'java.io.IOException "Stream closed")))
(define (sr-pos self) (vector-ref (jhost-state self) 1))
(define (sr-pos! self p) (vector-set! (jhost-state self) 1 p))
(register-host-methods! "string-reader"
  (list (cons "read" (with-charbuffer-read (lambda (self . rest)
                       (let ((s (sr-s self)) (p (sr-pos self)))
                         (cond
                           ;; .read() -> one char code, -1 at EOF
                           ((null? rest)
                            (if (>= p (string-length s)) -1
                                (begin (sr-pos! self (+ p 1)) (->num (char->integer (string-ref s p))))))
                           ;; .read(cbuf, off, len) -> fill cbuf, return count or -1 at EOF
                           (else
                            (let ((slen (string-length s)))
                              (if (>= p slen) -1
                                  (let-values (((cbuf off len) (reader-read-buf-args rest)))
                                    (let ((n (min len (- slen p)))
                                          (v (and (jolt-array? cbuf) (jolt-array-vec cbuf))))
                                      ;; a char[] is backed by a string: one block copy
                                      (if (and (string? v) (fixnum? off) (fx>=? off 0) (fx>=? n 0)
                                               (fx<=? (fx+ off n) (string-length v)))
                                          (sa-string-copy-range! v off s p (+ p n))
                                          (let loop ((i 0)) (when (< i n) (ja-set! cbuf (+ off i) (string-ref s (+ p i))) (loop (+ i 1)))))
                                      (sr-pos! self (+ p n)) (->num n)))))))))))
        (cons "mark" (lambda (self . _) (sr-s self) (vector-set! (jhost-state self) 2 (sr-pos self)) jolt-nil))
        (cons "reset" (lambda (self) (sr-s self) (sr-pos! self (vector-ref (jhost-state self) 2)) jolt-nil))
        (cons "skip" (lambda (self n) (let ((n (jnum->exact n)))
                                        (sr-pos! self (min (string-length (sr-s self)) (+ (sr-pos self) n))) (->num n))))
        ;; readLine: the next line without its terminator, nil at EOF — what
        ;; line-seq drives over a BufferedReader. \n, \r and \r\n all end a line,
        ;; which is java.io.BufferedReader's rule; a LONE \r used to be carried
        ;; into the line, so text written by a classic-Mac-era tool read as one
        ;; enormous line.
        (cons "readLine"
          (lambda (self)
            (let ((s (sr-s self)) (p (sr-pos self)) (len (string-length (sr-s self))))
              (if (>= p len) jolt-nil
                  (let scan ((i p))
                    (cond
                      ((>= i len) (sr-pos! self len) (substring s p len))
                      ((char=? (string-ref s i) #\newline)
                       (sr-pos! self (+ i 1))
                       (substring s p i))
                      ((char=? (string-ref s i) #\return)
                       (sr-pos! self (if (and (< (+ i 1) len) (char=? (string-ref s (+ i 1)) #\newline))
                                         (+ i 2)
                                         (+ i 1)))
                       (substring s p i))
                      (else (scan (+ i 1)))))))))
        ;; lines: the rest of the input one readLine at a time — what
        ;; (BufferedReader. (StringReader. s)) hands back on the JVM, and
        ;; BufferedReader over jolt's own readers IS the wrapped reader.
        (cons "lines" (lambda (self)
                        (let loop ((acc '()))
                          (let ((l (record-method-dispatch self "readLine" jolt-nil)))
                            (if (jolt-nil? l) (list->cseq (reverse acc)) (loop (cons l acc)))))))
        (cons "ready" (lambda (self) (sr-s self) #t))
        (cons "close" (lambda (self) (vector-set! (jhost-state self) 0 #f) jolt-nil))))

;; ---- PushbackReader ---------------------------------------------------------
;; state: a vector #(wrapped-reader pushed-list line-numbering? line column skip-lf?
;;                   at-line-start? prev-at-line-start? owned-reader line-pending?)
;; The counters split the way clojure.lang.LineNumberingPushbackReader's do. LINE
;; (0-based, as java.io.LineNumberReader keeps it) is counted BELOW the pushback,
;; so a character read twice through an unread counts once; skip-lf? marks the
;; \n of a \r\n, already counted; line-pending? is JDK 21's end-of-input rule —
;; the stream ending after anything but a line terminator ends one more line,
;; once. COLUMN (1-based) and at-line-start? are the pushback reader's own, moved
;; by every read including one out of the pushback: a newline or the end of input
;; puts the column back to 1, anything else moves it on, and an unread steps it
;; back. prev-at-line-start? is what an unread restores at-line-start? to.
;;
;; wrapped-reader is what reads come from, and a form read replaces it: the
;; drain-parse-refill path of host-reader-read-form (io.ss) drains it and puts a
;; StringReader over the unconsumed tail in its place. owned-reader is the
;; reader the constructor was handed, which that swap never touches, and it is
;; what close closes (jolt-lang/jolt#1117) -- closing slot 0 after a read closed
;; the in-memory tail and left the file open until a GC.
(define (make-pbr-state rdr line-numbering?)
  (vector rdr '() line-numbering? 0 1 #f #t #t rdr #f))
(register-class-ctor! "PushbackReader"
  (lambda (rdr . _) (make-jhost "pushback-reader" (make-pbr-state rdr #f))))
;; Fully-qualified aliases so (java.io.PushbackReader. …) / (java.io.StringReader. …)
;; resolve to these built-ins even when a library defines a deftype of the same
;; simple name (tools.reader), which would otherwise take the bare-name slot.
(register-class-ctor! "java.io.PushbackReader" (lookup-class class-ctors-tbl "PushbackReader"))
(register-class-ctor! "java.io.StringReader" (lookup-class class-ctors-tbl "StringReader"))
;; clojure.lang.LineNumberingPushbackReader is pushback over java.io.LineNumberReader,
;; and that reader normalizes line terminators: \r, \n and \r\n each read as a single
;; \n and bump the line number. So a source read through it looks the same whether it
;; was written on Unix, Windows or a classic Mac — tools.reader's source logging
;; depends on that, and without it a \r leaks through as its own character.
;; Its own jhost tag, not "pushback-reader": the value has to report
;; clojure.lang.LineNumberingPushbackReader for tools.reader's
;; (extend LineNumberingPushbackReader IndexingReader …) to dispatch. The methods
;; are shared with the plain reader below, so the two cannot drift.
(define (make-lnpbr rdr . _)
  (make-jhost "line-numbering-pushback-reader" (make-pbr-state rdr #t)))
(register-class-ctor! "LineNumberingPushbackReader" make-lnpbr)
(register-class-ctor! "clojure.lang.LineNumberingPushbackReader" make-lnpbr)
(define (read-unit r)        ; read one code unit (flonum) from any reader, -1 at EOF
  (record-method-dispatch r "read" jolt-nil))
;; One character from the wrapped reader, terminators folded to \n: the
;; LineNumberReader half, which owns the line count. Pushback sits ABOVE this (as
;; it does on the JVM), so an unread \n is handed straight back and does not
;; count a second line.
(define (pbr-read-translated self)
  (let* ((st (jhost-state self))
         (c (read-unit (vector-ref st 0)))
         (n (and (number? c) (jnum->exact c))))
    (cond
      ((and (vector-ref st 5) (eqv? n 10))     ; the \n of a \r\n pair, already counted
       (vector-set! st 5 #f)
       (pbr-read-translated self))
      (else
       (vector-set! st 5 (eqv? n 13))
       (cond
         ((or (eqv? n 13) (eqv? n 10))
          (vector-set! st 3 (+ 1 (vector-ref st 3)))
          (vector-set! st 9 #f)
          (->num 10))
         ((or (jolt-nil? c) (and n (< n 0)))
          (when (vector-ref st 9)
            (vector-set! st 3 (+ 1 (vector-ref st 3)))
            (vector-set! st 9 #f))
          c)
         (else (vector-set! st 9 #t) c))))))
;; Every java.io.Reader has close(), so the JVM's PushbackReader.close can call
;; in.close() unconditionally. jolt can be handed something a JVM PushbackReader
;; could not: clojure.core's *in* is a reify over IReader (jolt-core/clojure/
;; core/50-io.clj) with no close method at all, and dispatching one at it would
;; throw where the JVM has nothing to throw about. So a record or reify is asked
;; whether it HAS the method; a host reader (io/reader, StringReader, the port
;; readers) always does and is dispatched straight.
(define (pbr-closeable? rdr)
  (if (or (jrec? rdr) (jreify? rdr))
      (and (iface-method rdr "close" #f) #t)
      #t))
(define (pbr-ready? self)
  (let ((st (jhost-state self)))
    (or (pair? (vector-ref st 1)) (reader-ready? (vector-ref st 0)))))
(register-host-methods! "pushback-reader"
  (list (cons "read"
          (with-charbuffer-read
          (lambda (self . rest)
            (define (read1)
              (let* ((st (jhost-state self)) (pushed (vector-ref st 1))
                     (c (cond
                          ((pair? pushed) (vector-set! st 1 (cdr pushed)) (car pushed))
                          ((vector-ref st 2) (pbr-read-translated self))
                          (else (read-unit (vector-ref st 0)))))
                     (n (and (number? c) (jnum->exact c))))
                (vector-set! st 7 (vector-ref st 6))
                (cond ((or (eqv? n 10) (eqv? n -1) (jolt-nil? c))
                       (vector-set! st 6 #t)
                       (vector-set! st 4 1))
                      (else
                       (vector-set! st 6 #f)
                       (vector-set! st 4 (+ 1 (vector-ref st 4)))))
                c))
            (if (null? rest)
                (read1)
                ;; .read(cbuf, off, len) -> one code unit at a time into cbuf, the
                ;; first blocking and the rest only while the pushback or the
                ;; wrapped reader has them; count, or -1 at immediate EOF.
                (let-values (((cbuf off len) (reader-read-buf-args rest)))
                  (reader-fill-available! cbuf off len
                    (lambda ()
                      (let ((u (read1)))
                        (if (or (jolt-nil? u) (< (jnum->exact u) 0))
                            (eof-object)
                            (integer->char (jnum->exact u)))))
                    (lambda () (pbr-ready? self))))))))
        ;; PushbackReader.ready: something pushed back, or the wrapped reader ready
        (cons "ready" pbr-ready?)
        (cons "unread"
          (lambda (self ch . rest)
            (vector-set! (jhost-state self) 6 (vector-ref (jhost-state self) 7))
            (if (null? rest)
                ;; unread(int|char) — push one code unit back, and step the column
                ;; back over it (only this arity is LineNumberingPushbackReader's
                ;; own; the char[] one below is PushbackReader's and leaves it)
                (begin
                  (vector-set! (jhost-state self) 4 (- (vector-ref (jhost-state self) 4) 1))
                  (vector-set! (jhost-state self) 1
                    (cons (if (char? ch) (->num (char->integer ch)) ch) (vector-ref (jhost-state self) 1))))
                ;; unread(char[] cbuf, off, len) — push cbuf[off,off+len) so cbuf[off]
                ;; reads back first (the list head).
                (let ((off (jnum->exact (car rest))) (len (jnum->exact (cadr rest))))
                  (let loop ((i (- (+ off len) 1)) (acc (vector-ref (jhost-state self) 1)))
                    (if (< i off)
                        (vector-set! (jhost-state self) 1 acc)
                        (loop (- i 1) (cons (->num (char->integer (ja-ref ch i))) acc))))))
            jolt-nil))
        ;; java.io.PushbackReader.close() is `in.close()` — it closes the reader it
        ;; WRAPS, and a no-op here leaked every one of them (jolt-lang/jolt#1109).
        ;; with-open over a PushbackReader is the normal spelling, so nothing else
        ;; ever closed the wrapped reader: jolt.loader brackets each source file
        ;; that way (stdlib/jolt/loader.clj), so every file it loaded stayed open
        ;; for the life of the process. Invisible on POSIX, where an open
        ;; descriptor does not stop an unlink; on Windows it makes the directory
        ;; undeletable, which is how it was found.
        ;;
        ;; Only the PUSHBACK readers delegate. The other no-op closes in this file
        ;; ("writer", "string-reader", ...) are over in-memory buffers, where a
        ;; no-op is exactly what the JDK does.
        (cons "close"
          (lambda (self)
            (let ((rdr (vector-ref (jhost-state self) 8)))
              (when (and (not (jolt-nil? rdr)) (pbr-closeable? rdr))
                (record-method-dispatch rdr "close" jolt-nil)))
            jolt-nil))
        ;; 1-based, like clojure.lang.LineNumberingPushbackReader's own +1 over the
        ;; underlying LineNumberReader. A plain PushbackReader counts nothing.
        (cons "getLineNumber" (lambda (self) (->num (+ 1 (vector-ref (jhost-state self) 3)))))
        ;; setLineNumber(n): the NEXT getLineNumber answers n — the JVM stores
        ;; n-1 on the underlying LineNumberReader and adds its 1 back on read.
        ;; clojure.main/renumbering-read re-reads a form under the line it
        ;; came from this way.
        (cons "setLineNumber"
          (lambda (self n) (vector-set! (jhost-state self) 3 (- (jnum->exact n) 1)) jolt-nil))
        (cons "atLineStart" (lambda (self) (vector-ref (jhost-state self) 6)))
        (cons "getColumnNumber" (lambda (self) (->num (vector-ref (jhost-state self) 4))))
        ;; readLine: the next line without its terminator, nil at EOF. On the JVM
        ;; only the line-numbering subclass has it (from its BufferedReader half);
        ;; the shared method table gives it to the plain PushbackReader as well,
        ;; which is a superset with one reasonable meaning. Goes through this
        ;; table's own `read`, so the pushback buffer and the \r\n folding above
        ;; are both honored rather than re-implemented.
        (cons "readLine"
          (lambda (self)
            (let loop ((acc '()))
              (let* ((u (read-unit self))
                     (n (and (number? u) (jnum->exact u))))
                (cond
                  ((or (jolt-nil? u) (and n (< n 0)))
                   (if (null? acc) jolt-nil (list->string (reverse acc))))
                  ((eqv? n 10)
                   (list->string (reverse (if (and (pair? acc) (char=? (car acc) #\return))
                                              (cdr acc) acc))))
                  (else (loop (cons (integer->char n) acc))))))))))
;; The line-numbering subclass IS a PushbackReader — same methods, same table.
;; Only the class it reports differs (see make-lnpbr).
(alias-host-methods! "line-numbering-pushback-reader" "pushback-reader")

;; ---- StringTokenizer --------------------------------------------------------
;; state: a vector #(tokens-list pos)
(define (tokenize s delims)
  (let ((dset (string->list delims)))
    (let loop ((chars (string->list s)) (cur '()) (toks '()))
      (cond ((null? chars) (reverse (if (null? cur) toks (cons (list->string (reverse cur)) toks))))
            ((memv (car chars) dset)
             (loop (cdr chars) '() (if (null? cur) toks (cons (list->string (reverse cur)) toks))))
            (else (loop (cdr chars) (cons (car chars) cur) toks))))))
;; state: #(token-VECTOR pos) — length/list-ref per token made a full drain
;; O(tokens^2); a vector makes every method O(1).
(define (st-toks self) (vector-ref (jhost-state self) 0))
(define (st-pos self) (vector-ref (jhost-state self) 1))
(define (st-next! self)
  (let ((toks (st-toks self)) (p (st-pos self)))
    (if (fx<? p (vector-length toks))
        (begin (vector-set! (jhost-state self) 1 (fx+ p 1)) (vector-ref toks p))
        (jolt-throw (jolt-host-throwable "java.util.NoSuchElementException" "no more tokens")))))
(define (st-more? self) (fx<? (st-pos self) (vector-length (st-toks self))))
(register-class-ctor! "StringTokenizer"
  (lambda (s . delims) (make-jhost "string-tokenizer"
    (vector (list->vector
             (tokenize (if (string? s) s (jolt-str-render-one s))
                       (if (null? delims) " \t\n\r\f" (car delims)))) 0))))
(register-host-methods! "string-tokenizer"
  (list (cons "hasMoreTokens" st-more?)
        (cons "countTokens" (lambda (self) (->num (fx- (vector-length (st-toks self)) (st-pos self)))))
        (cons "nextToken" st-next!)
        ;; StringTokenizer implements java.util.Enumeration — enumeration-seq drives
        ;; it through these, so alias them onto the token methods.
        (cons "hasMoreElements" st-more?)
        (cons "nextElement" st-next!)))

;; ---- String / BigInteger / MapEntry constructors ----------------------------
;; (String. bytes [charset]) decodes bytes (a bytevector OR a jolt byte-array)
;; with the named charset (UTF-8 default; ISO-8859-1/latin1/ascii = one byte per
;; char); else stringify. clj-http-lite's body coercion is (String. ^[B body cs).
(define (string-charset-name rest)
  (if (pair? rest)
      (let ((c (car rest)))
        ;; via charset-arg-name: a "charset" jhost's state is the
        ;; #(canonical-name encode-max) vector charset-for-name builds, not an
        ;; alist, and assq on it raised "improperly formed alist" — so
        ;; (String. bytes (Charset/forName …)) threw where the JVM decodes.
        (cond ((or (string? c) (and (jhost? c) (string=? (jhost-tag c) "charset")))
               (charset-arg-name c))
              (else "UTF-8")))
      "UTF-8"))
(define (decode-bytevector bv rest)
  (let* ((name (string-charset-name rest))
         (cs (charset-canonical-down name)))
    (cond
      ;; utf8-bytes->string, not utf8->string: Java's decoder replaces malformed
      ;; input per RUN and Chez's per SEQUENCE, so the two differ on overlongs
      ;; and on a leading BOM (natives-str.ss has the table). Every byte->text
      ;; seam below this -- (String. bytes), slurp of a byte array or a byte
      ;; stream, .readAllBytes, the CharsetDecoder, a zip entry name -- inherits
      ;; whichever one is named here, which is how (slurp (io/input-stream f))
      ;; came to disagree with (slurp f) on the same bytes (jolt-dta.13).
      ((string=? cs "utf-8") (utf8-bytes->string bv))
      ((string=? cs "iso-8859-1")
       (list->string (map integer->char (bytevector->u8-list bv))))
      ;; a byte past 127 is not ASCII, and the JVM's decoder replaces it with
      ;; U+FFFD rather than reading it as Latin-1
      ((string=? cs "us-ascii")
       (list->string (map (lambda (b) (if (fx<? b 128) (integer->char b) #\xFFFD))
                          (bytevector->u8-list bv))))
      ;; UTF-16, UTF-32 and everything iconv knows: the same incremental
      ;; decoders an InputStreamReader streams through (natives-str.ss), run
      ;; over the whole buffer at once. A charset the host does not have is the
      ;; JVM's UnsupportedEncodingException rather than a silent
      ;; reinterpretation of the bytes as UTF-8.
      (else (java-decode-all bv cs name)))))
;; (String. bytes offset length [charset]) — decode a SLICE. Returns (bv . rest')
;; where rest' is the charset args; a plain (String. bytes [charset]) is unsliced.
;; N is the source's length, COPY! copies a span of it into a fresh bytevector,
;; WHOLE gives the unsliced bytes. Only the slice is copied: a jolt byte array
;; used to become a bytevector in full before the slice was cut, so taking 30
;; bytes out of a 1.5 MB array cost the 1.5 MB. The range is String's own check
;; (checkBoundsOffCount), with its exception and message.
(define (bytes-slice-for-string n copy! whole rest)
  (if (and (pair? rest) (number? (car rest)) (pair? (cdr rest)) (number? (cadr rest)))
      (let ((off (jnum->exact (car rest))) (len (jnum->exact (cadr rest))))
        (unless (and (>= off 0) (>= len 0) (<= off (- n len)))
          (throw-jvm 'StringIndexOutOfBoundsException
                     (string-append "Range [" (number->string off) ", " (number->string off)
                                    " + " (number->string len) ") out of bounds for length "
                                    (number->string n))))
        (let ((out (make-bytevector len)))
          (copy! off out len)
          (cons out (cddr rest))))
      (cons (whole) rest)))
(define (jolt-new-string x . rest)
    (cond ((bytevector? x)
           (let ((p (bytes-slice-for-string (bytevector-length x)
                                            (lambda (off out len) (bytevector-copy! x off out 0 len))
                                            (lambda () x) rest)))
             (decode-bytevector (car p) (cdr p))))
          ((and (jolt-array? x) (eq? (jolt-array-kind x) 'byte))
           (let ((p (bytes-slice-for-string (ja-len x)
                                            (lambda (off out len) (ja-bytes->bv! x off out 0 len))
                                            (lambda () (na-bytearray->bv x)) rest)))
             (decode-bytevector (car p) (cdr p))))
          ;; (String. char[] [offset count]) — the whole array or a slice. Buffered
          ;; readers (data.json) build a string from a fill buffer this way.
          ;; The whole array went through (list->string (ja->list x)) — a cons per
          ;; character, then a second walk to build the string — and the slice
          ;; went through ja-ref, which re-reads the checked record accessor and
          ;; re-tests the backing type on every index. Both showed up as ~21x
          ;; babashka on the chunked-read path that feeds this (an 8.7 MB
          ;; char[] cost 457 ms against bb's 22 ms), where a decode loop builds
          ;; a string from its fill buffer once per chunk.
          ;;
          ;; A char array's backing IS a Chez string (natives-array.ss), so this
          ;; whole constructor is one substring — the characters are already
          ;; laid out the way the result needs them, and nothing is walked. The
          ;; vector arm below it is the same copy against the older boxed
          ;; backing (a promoted array, or one restored from an image written
          ;; before the string backing); anything else — a slice the caller got
          ;; wrong — keeps the checked ja-ref path, which raises the JVM's own
          ;; exception at the offending index.
          ((and (jolt-array? x) (eq? (jolt-array-kind x) 'char))
           (let* ((v (jolt-array-vec x))
                  (n (ja-len x))
                  (off (if (pair? rest) (jnum->exact (car rest)) 0))
                  (cnt (if (pair? rest) (jnum->exact (cadr rest)) n))
                  (ok? (and (fixnum? off) (fixnum? cnt)
                            (fx>=? off 0) (fx>=? cnt 0) (fx<=? (fx+ off cnt) n))))
             (cond
               ;; NOT substring: Chez's substring is ~2.2x slower than an
               ;; explicit copy of the same bytes (measured on 8.1M characters,
               ;; x20: whole 1301ms vs 589ms for string-copy, half 831ms vs
               ;; 374ms for make-string + string-copy!). Since the backing is
               ;; already a string, the whole-array case — which is what
               ;; (String. ca) means — is exactly string-copy.
               ((and ok? (string? v))
                (if (and (fx=? off 0) (fx=? cnt (string-length v)))
                    (string-copy v)
                    (let ((out (make-string cnt)))
                      (sa-string-copy-range! out 0 v off (fx+ off cnt))
                      out)))
               ((and ok? (vector? v))
                (let ((out (make-string cnt)))
                  (let loop ((i 0))
                    (if (fx=? i cnt) out
                        (begin (string-set! out i (vector-ref v (fx+ off i)))
                               (loop (fx+ i 1)))))))
               (else
                (let ((out (make-string cnt)))
                  (let loop ((i 0))
                    (if (fx=? i cnt) out
                        (begin (string-set! out i (ja-ref x (fx+ off i)))
                               (loop (fx+ i 1))))))))))
          ;; new String(s) is a new object, never s itself (identical? tells them
          ;; apart, and a WeakHashMap keyed by it must not be kept by a literal)
          ((string? x) (string-copy x))
          (else (jolt-str-render-one x))))
;; new String() is the empty string
(register-class-ctor! "String" (case-lambda (() (make-string 0)) ((x . rest) (apply jolt-new-string x rest))))
;; (BigInteger. s) | (BigInteger. s radix) — parse a string in the given radix
;; (default 10). tools.reader's integer parser builds (BigInteger. digits radix).
;; (BigInteger. signum magnitude-bytes) is the other JVM constructor: an unsigned
;; big-endian magnitude with an explicit sign, which is how a digest is turned into
;; a number before hex-formatting it (Selmer's {{ x|hash:"md5" }} filter does
;; (format "%032x" (BigInteger. 1 bs))). Each byte contributes its UNSIGNED value,
;; so the sign of jolt's signed byte array does not leak into the result.
(define (bigint-from-magnitude signum bytes)
  (let ((n (ja-len bytes)))
    (let loop ((i 0) (acc 0))
      (if (fx>=? i n)
          (* (jnum->exact signum) acc)
          (loop (fx+ i 1)
                (+ (* acc 256) (bitwise-and (jnum->exact (ja-ref bytes i)) #xFF)))))))
(define (bigint-ctor v . r)
  (if (and (pair? r) (jolt-array? (car r)))
      (bigint-from-magnitude v (car r))
      (parse-int-or-throw v (if (null? r) 10 (jnum->exact (car r))) "big")))
(register-class-ctor! "BigInteger" bigint-ctor)
(register-class-ctor! "java.math.BigInteger" bigint-ctor)
(register-class-ctor! "MapEntry" (lambda (k v) (make-map-entry k v)))
;; clojure.lang.APersistentVector$RSeq: the reverse seq of an Indexed value from
;; index i down to 0, as an IPersistentVector deftype's rseq builds it
;; (clojure.core.Vec). The same lazy cells a native vector's rseq uses, so the
;; class answer and the walk match.
(define lz-indexed-rseq
  (register-lazy-src! 'indexed-rseq (lambda (v i) (indexed->rseq v i))))
(define (indexed->rseq v i)
  (if (fx<? i 0)
      jolt-nil
      (make-cseqn (jolt-nth v i) (make-lazy-src lz-indexed-rseq v (fx- i 1)) sk-rseq jolt-nil (fx+ i 1))))
(let ((rseq-ctor (lambda (v i) (indexed->rseq v (jnum->exact i)))))
  (register-class-ctor! "APersistentVector$RSeq" rseq-ctor)
  (register-class-ctor! "clojure.lang.APersistentVector$RSeq" rseq-ctor))
;; clojure.lang.MapEntry/create — the static factory clojure.walk and kin use
;; when rebuilding map entries.
(register-class-statics! "MapEntry" (list (cons "create" (lambda (k v) (make-map-entry k v)))))
;; JVM exception ctors -> a typed host throwable carrying the canonical :jolt/class
;; (so class / instance? / getMessage / ex-message reflect the real type) and the
;; message. Supports (E. msg), (E. msg cause), (E. cause), and (E.).
;; Derived from the ONE exception hierarchy in class-hierarchy.ss: every
;; jch-isa? -> Throwable gets a ctor with no second list to maintain.
(define (make-exc-ctor canonical)
  (lambda args
    (let* ((a0 (if (pair? args) (car args) jolt-nil))
           (rest (if (pair? args) (cdr args) '()))
           (cause (if (pair? rest) (car rest) jolt-nil)))
      (cond
        ((string? a0) (jolt-host-throwable canonical a0 cause))
        ((jolt-nil? a0) (jolt-host-throwable canonical jolt-nil))
        ;; (E. cause): a lone throwable arg is the cause, and the message is the
        ;; cause's toString, as Throwable(Throwable) sets it on the JVM —
        ;; (ExecutionException. (IllegalStateException. "bad")) has the message
        ;; "java.lang.IllegalStateException: bad", not nil.
        ((and (null? rest) (ex-info-map? a0))
         (jolt-host-throwable canonical (jolt-str-render-one a0) a0))
        (else (jolt-host-throwable canonical (jolt-str-render-one a0) cause))))))
(let-values (((keys vals) (hashtable-entries jvm-class-parents)))
  (vector-for-each
    (lambda (canonical supers)
      (when (jch-isa? canonical "Throwable")
        (let ((short (jch-last-segment canonical)))
          (register-class-ctor! short (make-exc-ctor canonical))
          (unless (string=? short canonical)
            (register-class-ctor! canonical (make-exc-ctor canonical))))))
    keys vals))

;; clojure.lang.ArityException(int actual, String name) builds the JVM message.
(register-class-ctor! "ArityException"
  (lambda (actual name . _)
    (jolt-host-throwable "clojure.lang.ArityException"
      (string-append "Wrong number of args (" (jvm-arity-count actual)
                     ") passed to: " (if (string? name) name (jolt-str-render-one name))))))
(register-class-ctor! "clojure.lang.ArityException"
  (lambda (actual name . _)
    (jolt-host-throwable "clojure.lang.ArityException"
      (string-append "Wrong number of args (" (jvm-arity-count actual)
                     ") passed to: " (if (string? name) name (jolt-str-render-one name))))))

;; java.text.ParseException(String s, int errorOffset): unlike the exceptions
;; above, its second ctor arg is an int offset (getErrorOffset), not a cause.
;; Store the offset in the record's error-offset field (invisible to ex-data).
(let ((parse-exc-ctor
       (lambda args
         (let* ((a0 (if (pair? args) (car args) jolt-nil))
                (off (if (and (pair? args) (pair? (cdr args))) (cadr args) 0))
                (msg (if (string? a0) a0 (jolt-str-render-one a0)))
                (rec (jolt-host-throwable "java.text.ParseException" msg)))
           (jolt-ex-info-record-error-offset-set! rec off)
           rec))))
  (register-class-ctor! "ParseException" parse-exc-ctor)
  (register-class-ctor! "java.text.ParseException" parse-exc-ctor))

;; ---- URLEncoder / URLDecoder (www-form-urlencoded) --------------------------
;; Both honour the charset argument, which they used to ignore and always encode
;; UTF-8 — so (URLEncoder/encode "\u3044" "Shift_JIS") is %82%A2, not %E3%81%84.
;;
;; The conversion is per RUN, not per string or per character, which is what the
;; JVM does: unreserved ASCII passes through as itself and only the characters
;; between them are converted, together. It matters for a stateful charset —
;; UTF-16 writes a byte-order mark at the head of whatever it is handed, so
;; (URLEncoder/encode "foo/bar" "UTF-16") is foo%FE%FF%00%2Fbar, one mark before
;; the escaped "/" rather than one at the head of the whole string.
(define (url-unreserved-char? c)
  (let ((b (char->integer c)))
    (or (and (>= b 48) (<= b 57)) (and (>= b 65) (<= b 90)) (and (>= b 97) (<= b 122))
        (= b 46) (= b 42) (= b 95) (= b 45))))
(define hex-digits "0123456789ABCDEF")
;; The charset is validated up front, as on the JVM: a name the host cannot honour
;; is an error even when the input happens to be all-unreserved and no conversion
;; would have run.
(define (url-charset cs)
  (let ((name (charset-arg-name cs)))
    (if (or (charset-lookup name) (iconv-known? name)) cs (unsupported-encoding-throw name))))
(define (url-encode s . rest)
  (let* ((str (if (string? s) s (jolt-str-render-one s)))
         (cs (url-charset (if (null? rest) "UTF-8" (car rest))))
         (n (string-length str))
         (out '()))
    (define (emit-escaped! bv)
      (do ((i 0 (+ i 1))) ((= i (bytevector-length bv)))
        (let ((b (bytevector-u8-ref bv i)))
          (set! out (cons (string-ref hex-digits (bitwise-and b 15))
                     (cons (string-ref hex-digits (bitwise-arithmetic-shift-right b 4))
                       (cons #\% out)))))))
    (let loop ((i 0))
      (if (>= i n)
          (list->string (reverse out))
          (let ((c (string-ref str i)))
            (cond
              ((url-unreserved-char? c) (set! out (cons c out)) (loop (+ i 1)))
              ((char=? c #\space) (set! out (cons #\+ out)) (loop (+ i 1)))
              (else
               (let run ((j i))
                 (if (and (< j n)
                          (not (url-unreserved-char? (string-ref str j)))
                          (not (char=? (string-ref str j) #\space)))
                     (run (+ j 1))
                     (begin (emit-escaped! (charset-encode-bv (substring str i j) cs))
                            (loop j)))))))))))
(define (hexv c)
  (cond ((and (char<=? #\0 c) (char<=? c #\9)) (- (char->integer c) 48))
        ((and (char<=? #\A c) (char<=? c #\F)) (- (char->integer c) 55))
        ((and (char<=? #\a c) (char<=? c #\f)) (- (char->integer c) 87))
         (else (throw-jvm 'IllegalArgumentException "URLDecoder: illegal hex escape"))))
(define (url-decode s . rest)
  (let* ((str (if (string? s) s (jolt-str-render-one s)))
         (cs (list (url-charset (if (null? rest) "UTF-8" (car rest)))))
         (n (string-length str))
         (out '()))
    (let loop ((i 0))
      (if (>= i n)
          (list->string (reverse out))
          (let ((c (string-ref str i)))
            (cond
              ((char=? c #\+) (set! out (cons #\space out)) (loop (+ i 1)))
              ((char=? c #\%)
               (let run ((j i) (bytes '()))
                 (if (and (< j n) (char=? (string-ref str j) #\%))
                     (run (+ j 3) (cons (+ (* 16 (hexv (string-ref str (+ j 1))))
                                           (hexv (string-ref str (+ j 2))))
                                        bytes))
                     (let ((dec (decode-bytevector (u8-list->bytevector (reverse bytes)) cs)))
                       (do ((k 0 (+ k 1))) ((= k (string-length dec)))
                         (set! out (cons (string-ref dec k) out)))
                       (loop j)))))
              (else (set! out (cons c out)) (loop (+ i 1)))))))))
(define (u8-list->bytevector lst)
  (let ((bv (make-bytevector (length lst))))
    (let loop ((l lst) (i 0)) (if (null? l) bv (begin (bytevector-u8-set! bv i (car l)) (loop (cdr l) (+ i 1)))))))
;; Registered under the QUALIFIED name, which mirrors to the short one (see
;; class-statics-merge!): the short spelling alone left both classes with working
;; statics and no class TOKEN, because forname-known? consults the statics table
;; by FQN only. So (Class/forName "java.net.URLDecoder") was a
;; ClassNotFoundException while (URLDecoder/decode …) worked — and an interpreter
;; that resolves the qualifier as a classname before the static (SCI, which is how
;; kmet evaluates extensions) could not reach the static at all. Their rows in the
;; class graph are in class-hierarchy.ss. #999.
(register-class-statics! "java.net.URLEncoder" (list (cons "encode" url-encode)))
(register-class-statics! "java.net.URLDecoder" (list (cons "decode" url-decode)))
;; Charset is registered further down, under the qualified name — which mirrors
;; to the short one — and answers with a charset OBJECT (charset-for-name, which
;; validates the name and carries the encoding). A second short-name registration
;; stood here answering with the canonical name string, and was overwritten member
;; for member by that one: dead, and JOLT_DEBUG reported the overwrite as drift
;; between two host files.

;; ---- Base64 (RFC 4648) ------------------------------------------------------
;; One codec, two alphabets: basic (+/) and URL-safe (-_), section 5 of the RFC.
;; An encoder jhost carries (vector alphabet pad? line-width line-sep) and a
;; decoder (vector alphabet pad? mime?), so getUrlEncoder/getUrlDecoder,
;; getMimeEncoder/getMimeDecoder and .withoutPadding are the SAME tags with
;; different state — .withoutPadding returns a fresh encoder, like the JDK's
;; (whose encoders are immutable), and each decoder rejects the other
;; alphabet's chars because b64-char-val searches only its own alphabet.
;;
;; The MIME pair is the other half of RFC 2045: the encoder breaks its output
;; into lines and the decoder IGNORES every character outside the alphabet, which
;; is what makes a PEM/PKCS#8 body — wrapped at 64 columns, newlines and all —
;; decodable at all. The basic decoder refuses those line breaks, so it is no
;; substitute, and every PEM-reading path (JWT, service-account login) failed on
;; the first decode without them (#955).
(define b64-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
(define b64url-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")
(define (b64-state-ref self i dflt)
  (let ((st (jhost-state self)))
    (if (and (vector? st) (> (vector-length st) i)) (vector-ref st i) dflt)))
(define (b64-self-alphabet self) (b64-state-ref self 0 b64-alphabet))
(define (b64-self-pad? self) (b64-state-ref self 1 #t))
(define (b64-self-width self) (b64-state-ref self 2 0))
(define (b64-self-sep self) (b64-state-ref self 3 ""))
(define (b64-self-mime? self) (b64-state-ref self 2 #f))
(define (->bytevector x)
  (cond ((bytevector? x) x)
        ((and (jolt-array? x) (eq? (jolt-array-kind x) 'byte)) (na-bytearray->bv x))
        ((string? x) (string->utf8 x))
        (else (string->utf8 (jolt-str-render-one x)))))
(define (b64-encode x alphabet pad?)
  (let* ((bs (->bytevector x)) (n (bytevector-length bs)) (out '()))
    (let loop ((i 0))
      (if (>= i n) (list->string (filter char? (reverse out)))
          (let* ((b0 (bytevector-u8-ref bs i))
                 (b1 (if (< (+ i 1) n) (bytevector-u8-ref bs (+ i 1)) #f))
                 (b2 (if (< (+ i 2) n) (bytevector-u8-ref bs (+ i 2)) #f)))
            (set! out (cons (string-ref alphabet (bitwise-arithmetic-shift-right b0 2)) out))
            (set! out (cons (string-ref alphabet (bitwise-ior (bitwise-arithmetic-shift-left (bitwise-and b0 3) 4)
                                                                  (bitwise-arithmetic-shift-right (or b1 0) 4))) out))
            (set! out (cons (if b1 (string-ref alphabet (bitwise-ior (bitwise-arithmetic-shift-left (bitwise-and b1 15) 2)
                                                                         (bitwise-arithmetic-shift-right (or b2 0) 6))) (and pad? #\=)) out))
            (set! out (cons (if b2 (string-ref alphabet (bitwise-and b2 63)) (and pad? #\=)) out))
            (loop (+ i 3)))))))
(define (b64-char-val c alphabet)
  (let loop ((i 0)) (cond ((= i 64) (throw-jvm 'IllegalArgumentException "Base64: illegal character")) ((char=? (string-ref alphabet i) c) i) (else (loop (+ i 1))))))
(define (b64-in-alphabet? c alphabet)
  (let loop ((i 0)) (cond ((= i 64) #f) ((char=? (string-ref alphabet i) c) #t) (else (loop (+ i 1))))))
;; Break encoded output into lines of `width` characters joined by `sep`, with no
;; separator after the last one — the JDK's MIME shape. A width of 0 (every
;; non-MIME encoder, and getMimeEncoder with a non-positive length) wraps nothing.
(define (b64-wrap-lines s width sep)
  (let ((len (string-length s)))
    (if (or (<= width 0) (= 0 (string-length sep)) (<= len width))
        s
        (let loop ((i 0) (out '()))
          (if (>= i len)
              (apply string-append (reverse out))
              (let ((j (min len (+ i width))))
                (loop j (cons (substring s i j) (if (null? out) out (cons sep out))))))))))
(define (b64-decode x alphabet mime?)
  (let* ((str (let ((s (if (string? x) x (utf8->string (->bytevector x)))))
                (list->string (filter (lambda (c)
                                        (and (not (char=? c #\=))
                                             ;; a MIME decoder discards everything
                                             ;; outside the alphabet; the basic one
                                             ;; hands it to b64-char-val, which throws.
                                             (or (not mime?) (b64-in-alphabet? c alphabet))))
                                      (string->list s)))))
         (out '()) (acc 0) (bits 0))
    (for-each (lambda (c)
                (set! acc (bitwise-ior (bitwise-arithmetic-shift-left acc 6) (b64-char-val c alphabet)))
                (set! bits (+ bits 6))
                (when (>= bits 8)
                  (set! bits (- bits 8))
                  (set! out (cons (bitwise-and (bitwise-arithmetic-shift-right acc bits) 255) out))))
              (string->list str))
    (u8-list->bytevector (reverse out))))
;; .encode / .decode return byte[] on the JVM, so they return a jolt byte-array here
;; (signed elements, seqable, bytes?) — they used to hand back a raw Chez bytevector,
;; which no collection dispatcher knows, so (vec (.decode dec s)) threw "Don't know
;; how to create ISeq from" and every caller had to route it through String. first.
(define (b64-encoded self bs)
  (b64-wrap-lines (b64-encode bs (b64-self-alphabet self) (b64-self-pad? self))
                  (b64-self-width self) (b64-self-sep self)))
(register-host-methods! "b64-encoder"
  (list (cons "encode" (lambda (self bs) (na-bv->bytearray (string->utf8 (b64-encoded self bs)))))
        (cons "encodeToString" b64-encoded)
        (cons "withoutPadding"
              (lambda (self) (make-jhost "b64-encoder"
                               (vector (b64-self-alphabet self) #f
                                       (b64-self-width self) (b64-self-sep self)))))))
(register-host-methods! "b64-decoder"
  (list (cons "decode" (lambda (self s)
                         (na-bv->bytearray (b64-decode s (b64-self-alphabet self) (b64-self-mime? self)))))))
;; getMimeEncoder(lineLength, lineSeparator): the JDK rounds the length DOWN to a
;; multiple of 4 (so a line never splits a 4-character group), treats a
;; non-positive length as "no line separator at all", and refuses a separator
;; containing a character of the base64 alphabet — which would make the output
;; undecodable by its own decoder.
(define b64-mime-width 76)
(define b64-mime-sep "\r\n")
(define (b64-mime-encoder width sep)
  (let ((w (* 4 (quotient (max 0 width) 4)))
        (sep (if (string? sep) sep (utf8->string (->bytevector sep)))))
    (let loop ((i 0))
      (cond ((= i (string-length sep))
             (make-jhost "b64-encoder" (vector b64-alphabet #t w sep)))
            ((or (char=? (string-ref sep i) #\=) (b64-in-alphabet? (string-ref sep i) b64-alphabet))
             (throw-jvm 'IllegalArgumentException
                        (string-append "Illegal base64 line separator character 0x"
                                       (number->string (char->integer (string-ref sep i)) 16))))
            (else (loop (+ i 1)))))))
(register-class-statics! "Base64"
  (list (cons "getEncoder" (lambda () (make-jhost "b64-encoder" (vector b64-alphabet #t 0 ""))))
        (cons "getDecoder" (lambda () (make-jhost "b64-decoder" (vector b64-alphabet #t #f))))
        (cons "getUrlEncoder" (lambda () (make-jhost "b64-encoder" (vector b64url-alphabet #t 0 ""))))
        (cons "getUrlDecoder" (lambda () (make-jhost "b64-decoder" (vector b64url-alphabet #t #f))))
        (cons "getMimeEncoder"
              (lambda args
                (if (null? args)
                    (make-jhost "b64-encoder" (vector b64-alphabet #t b64-mime-width b64-mime-sep))
                    (b64-mime-encoder (jnum->exact (car args)) (cadr args)))))
        (cons "getMimeDecoder" (lambda () (make-jhost "b64-decoder" (vector b64-alphabet #t #t))))))

;; ---- java.util.regex.Pattern ------------------------------------------------
;; Pattern/compile returns a jolt-regex value (regex-t), so str/replace, re-find,
;; .split etc. accept it transparently.
(define (pattern-quote s)
  (let ((meta "\\.[]{}()*+-?^$|&") (s (if (string? s) s (jolt-str-render-one s))) (out '()))
    (let loop ((i 0))
      (if (= i (string-length s)) (list->string (reverse out))
          (let ((c (string-ref s i)))
            (when (memv c (string->list meta)) (set! out (cons #\\ out)))
            (set! out (cons c out))
            (loop (+ i 1)))))))
;; The flag constants, as the JVM ints (java.util.regex.Pattern). They are exact
;; integers: a library builds a flags word with bit-or, which a flonum refused.
(define pattern-flag-bits
  '(("UNIX_LINES" . 1) ("CASE_INSENSITIVE" . 2) ("COMMENTS" . 4) ("MULTILINE" . 8)
    ("LITERAL" . 16) ("DOTALL" . 32) ("UNICODE_CASE" . 64) ("CANON_EQ" . 128)
    ("UNICODE_CHARACTER_CLASS" . 256)))
;; A regex-t carries its source and nothing else (the layout travels raw in
;; images, so it is frozen), so a flags word compiles as the equivalent inline
;; (?…) group, which the translator reads and .flags reads back. LITERAL has no
;; inline spelling: the source is \Q-quoted instead, the way Pattern.quote
;; spells a literal (a \E inside it is closed and reopened around). CANON_EQ
;; has no effect here.
(define pattern-flag-letters
  '((1 . #\d) (2 . #\i) (4 . #\x) (8 . #\m) (32 . #\s) (64 . #\u) (256 . #\U)))
(define (pattern-flags-source s flags)
  (let* ((flags (jnum->exact flags))
         (on? (lambda (bit) (= (bitwise-and flags bit) bit)))
         (letters (apply string-append
                         (map (lambda (e) (if (on? (car e)) (string (cdr e)) ""))
                              pattern-flag-letters)))
         (body (if (on? 16) (pattern-literal-quote s) s)))
    (if (string=? letters "")
        body
        (string-append "(?" letters ")" body))))
(define (pattern-literal-quote s)
  ;; \Q…\E around s. A \E inside s would end the quote early, so it is spelled
  ;; \E (end) \\E (a literal backslash-E) \Q (resume), as Pattern.quote does.
  (let loop ((i 0) (start 0) (parts (list "\\Q")))
    (cond ((>= (+ i 1) (string-length s))
           (apply string-append
                  (reverse (cons "\\E" (cons (substring s start (string-length s)) parts)))))
          ((and (char=? (string-ref s i) #\\) (char=? (string-ref s (+ i 1)) #\E))
           (loop (+ i 2) (+ i 2) (cons "\\E\\\\E\\Q" (cons (substring s start i) parts))))
          (else (loop (+ i 1) start parts)))))
;; the one Pattern statics block (compile / quote / the flag constants). nio-file
;; and host-static-methods used to register competing compile/quote members that
;; last-wins clobbered; this is now the single source.
(let ((pattern-statics
       (append
        (list (cons "compile" (lambda (s . flags)
                                (if (pair? flags)
                                    (jolt-regex (pattern-flags-source s (car flags)))
                                    (jolt-regex s))))
              (cons "quote" (lambda (s) (pattern-quote s))))
        pattern-flag-bits)))
  (register-class-statics! "Pattern" pattern-statics)
  (register-class-statics! "java.util.regex.Pattern" pattern-statics))
;; record-method-dispatch already routes string? -> jolt-string-method. Add a
;; regex-t arm (Pattern .split / .matcher-less surface used by corpus) by wrapping
;; once more — a regex-t isn't a jhost.
(register-method-arm! arm-priority-regex
  (lambda (obj method-name rest-args)
   ;; receiver first, then the cheap conversion — see dot-forms.ss's arm
   (if (not (or (regex-t? obj) (jolt-matcher? obj)))
    'pass
    (let ((rest (method-rest-args->list rest-args)))
      (cond
        ((regex-t? obj)
         (cond ((string=? method-name "split")
                ;; .split returns a String[] — a real array, the same shape
                ;; String.split answers with. The optional second argument is the
                ;; JVM's limit; it used to be dropped, so
                ;; (.split COLON "user:pass:word" 2) split three ways.
                (jvm-split-array (regex-t-irx obj) (car rest) (split-limit-arg rest 1)))
               ((string=? method-name "pattern") (regex-t-source obj))
               ((or (string=? method-name "toString")) (regex-t-source obj))
               ;; (.matcher pattern s) -> a Matcher (matcher-t) for stepping matches.
               ((string=? method-name "matcher") (jolt-re-matcher obj (car rest)))
               ;; .flags — the int Pattern was compiled with. jolt has no compile-time
               ;; flag argument, so the only flags a pattern can carry are the inline
               ;; ones it opens with; read those. Callers use it to decide whether a
               ;; pattern is one they can handle (test.chuck's string-from-regex).
               ((string=? method-name "flags") (rx-inline-flags (regex-t-source obj)))
               (else (dispatch-miss obj method-name rest))))
        ;; java.util.regex.Matcher: .matches (anchored whole-region), .find
        ;; (next match, or the next at-or-after an index), .group [n],
        ;; .groupCount, and the region controls .region / .regionStart /
        ;; .regionEnd / .reset.
        ((jolt-matcher? obj)
         (cond ((string=? method-name "matches") (jolt-matcher-matches obj))
               ((string=? method-name "lookingAt") (jolt-matcher-looking-at obj))
               ;; .find() resumes at the scan cursor; .find(from) resets and
               ;; scans from `from`.
               ((string=? method-name "find")
                (if (pair? rest)
                    (jolt-matcher-find-from obj (jnum->exact (car rest)))
                    (not (jolt-nil? (jolt-re-find obj)))))
               ((string=? method-name "group") (apply jolt-matcher-group obj rest))
               ((string=? method-name "groupCount") (jolt-matcher-group-count obj))
               ;; .replaceAll/.replaceFirst reset the matcher, substitute every
               ;; match in the region (or the first), expand $N in the replacement,
               ;; and leave the matcher with no match.
               ((string=? method-name "replaceAll")
                (jolt-matcher-replace obj (car rest) #t))
               ((string=? method-name "replaceFirst")
                (jolt-matcher-replace obj (car rest) #f))
               ((string=? method-name "region")
                (jolt-matcher-region obj (jnum->exact (car rest)) (jnum->exact (cadr rest))))
               ((string=? method-name "regionStart") (matcher-t-rstart obj))
               ((string=? method-name "regionEnd") (matcher-t-rend obj))
               ;; .reset(cs) — a new input on the same pattern — is not modelled;
               ;; only the no-argument reset, which is the one region and
               ;; find(int) are defined in terms of.
               ((and (string=? method-name "reset") (null? rest)) (matcher-reset! obj))
               ;; start/end of the last successful find (whole match, or group n)
               ((string=? method-name "start")
                (let ((mm (matcher-t-last obj)))
                  (if mm (irregex-match-start-index mm (if (pair? rest) (jnum->exact (car rest)) 0))
                      (jolt-matcher-no-match))))
               ((string=? method-name "end")
                (let ((mm (matcher-t-last obj)))
                  (if mm (irregex-match-end-index mm (if (pair? rest) (jnum->exact (car rest)) 0))
                      (jolt-matcher-no-match))))
               (else (dispatch-miss obj method-name rest))))
        (else 'pass))))))

;; ---- def-var! the registry entry points so emit can also reach them ---------
(def-var! "clojure.core" "host-static-ref" host-static-ref)
(def-var! "clojure.core" "host-static-call" (lambda (c m . a) (apply host-static-call c m a)))
(def-var! "clojure.core" "host-new" (lambda (c . a) (apply host-new c a)))

;; Clojure-visible class-registration hooks. A host shim (e.g. reitit.trie-jolt,
;; which mirrors the reitit.Trie Java class) registers a constructor proc or a
;; map of static members against a class token so (Class. args) / (Class/member
;; args) resolve to it. The statics argument is a jolt map {member-name -> val}.
(define (jmap->static-alist m)
  (let loop ((s (jolt-seq m)) (acc '()))
    (if (jolt-nil? s) acc
        (let ((e (jolt-first s)))
          (loop (jolt-seq (jolt-rest s)) (cons (cons (jolt-nth e 0) (jolt-nth e 1)) acc))))))
(def-var! "clojure.core" "__register-class-ctor!"
  (lambda (name proc) (register-class-ctor-user! name proc) jolt-nil))
(def-var! "clojure.core" "__register-class-statics!"
  (lambda (name members) (register-class-statics-user! name (jmap->static-alist members)) jolt-nil))

;; ---- tagged-table method dispatch + pluggable instance? --------------------
;; A jolt library can build stateful host objects with (jolt.host/tagged-table
;; tag) and dispatch (.method obj ...) to handlers registered here, keyed by the
;; table's "jolt/type" tag — the htable analogue of the jhost method registry
;; above. jolt-lang/http-client uses this to emulate java.net URL /
;; HttpURLConnection / java.io byte streams so clj-http-lite runs unchanged.
(define tagged-methods-tbl (make-hashtable string-hash string=?))   ; tag-key -> (method-ht)
(define (tag->method-key tag)
  (if (keyword-t? tag)
      (let ((ns (keyword-t-ns tag)))
        (if (and ns (not (jolt-nil? ns))) (string-append ns "/" (keyword-t-name tag)) (keyword-t-name tag)))
      (jolt-str-render-one tag)))
;; hsc-mu (java/class-model.ss) covers this registry too: it is reachable from
;; Clojure as clojure.core/__register-class-methods!, so from a namespace load,
;; which is now parallel. Single-key reads stay unlocked — strong hashtables,
;; per var-table.

;; The probe and the create are ONE step. Split, two threads registering methods
;; for one tag each built their own inner table and published it over the other's,
;; so every method in the loser's batch vanished — the same shape as the protocol
;; registry's type-registry. The member writes go under the same lock, since they
;; mutate the table this just published.
(define (register-tagged-methods! tag members)
  (jolt-with-mutex hsc-mu
    (let* ((key (tag->method-key tag))
           (h (or (hashtable-ref tagged-methods-tbl key #f)
                  (let ((nh (make-hashtable string-hash string=?)))
                    (hashtable-set! tagged-methods-tbl key nh) nh))))
      (for-each (lambda (p) (hashtable-set! h (car p) (cdr p))) members))))

;; The method a tagged table would dispatch, or #f. The dispatch arm above is the
;; caller; this is for runtime code that must ASK whether a library object
;; answers a method before deciding how to resolve (java/io.ss's io/resource
;; 2-arity: a loader object with getResource resolves in its own context).
(define (tagged-method-lookup obj name)
  (let* ((tag (and (htable? obj) (htable-str-ref obj "jolt/type" #f)))
         (mh (and tag (hashtable-ref tagged-methods-tbl (tag->method-key tag) #f))))
    (and mh (hashtable-ref mh name #f))))

;; htable arm: dispatch (.method obj a*) through the table's tag method registry;
;; an unregistered method falls through (sorted colls are htables too).
(register-method-arm! arm-priority-htable
  (lambda (obj method-name rest-args)
    (let ((tag (and (htable? obj) (htable-str-ref obj "jolt/type" #f))))
      (let* ((mh (and tag (hashtable-ref tagged-methods-tbl (tag->method-key tag) #f)))
             (f  (and mh (hashtable-ref mh method-name #f))))
        (cond
          (f (apply f obj (if (jolt-nil? rest-args) '() (seq->list rest-args))))
          ;; a stream marked as answering the java.io contract inherits the
          ;; methods InputStream / OutputStream write over read / write, as a
          ;; proxy of either does (io-streams.ss stream-contract-method)
          ((and tag (stream-contract-method obj method-name))
           => (lambda (g) (apply g obj (if (jolt-nil? rest-args) '() (seq->list rest-args)))))
          (else 'pass))))))

(def-var! "clojure.core" "__register-class-methods!"
  (lambda (tag members) (register-tagged-methods! tag (jmap->static-alist members)) jolt-nil))

;; java.lang.ThreadLocal / java.lang.InheritableThreadLocal.
;;
;; The two classes differ in exactly one thing — what a FORKED thread sees — and
;; that one thing is the contract, so each gets the storage that expresses it:
;;
;;   ThreadLocal             a per-thread table in a virtual register. A freshly
;;                           forked thread starts every vreg at fixnum 0, so it
;;                           builds its own table and re-runs initialValue: the
;;                           JVM's "each thread has its own, independently
;;                           initialized copy".
;;   InheritableThreadLocal  one thread parameter per instance. Chez hands a
;;                           forked thread the creating thread's parameter
;;                           values, which IS childValue(parentValue) — including
;;                           the case where the parent never read it, since the
;;                           parameter then still holds tl-unset and the child
;;                           runs its own initialValue like the JVM.
;;
;; A thread parameter for BOTH is what this was, which made the plain class the
;; inheritable one and left jolt unable to tell them apart at all (the proxy macro
;; lowered both to one call). It is not a cosmetic divergence: three workers
;; sharing a ThreadLocal<Process> each got the parent's already-drained subprocess
;; instead of spawning one, and test.check's per-thread RNG handed a forked thread
;; the parent's generator (jolt-uecg). CONTRACT.txt recorded it as accepted; it
;; isn't — "each thread has its own" is the entire content of the class.
;;
;; The table holds its entries for the life of the THREAD, where the JVM's
;; ThreadLocalMap holds weak keys and drops an unreachable ThreadLocal early. The
;; usual shape (a top-level defonce) is live for the process either way, and
;; .remove — the JVM's own answer for releasing a value — deletes the entry here
;; too. A weak table would cost every collection a scan of entries whose keys are
;; long-lived by construction (the per-thread-cache reasoning in hasheq.ss).
(define tl-unset (list 'tl-unset))

;; slot 10: see the vreg registry in rt.ss. Per-thread, and NOT a thread
;; parameter, for the same reason as slot 9's interrupt box: a parameter is
;; inherited by a forked thread and this table must not be — a shared table would
;; also be two threads mutating one hashtable, which faults inside the collector.
(define jolt-vreg-threadlocals 10)
(define (tl-table)
  (let ((t (virtual-register jolt-vreg-threadlocals)))
    (if (eq? t 0)
        (let ((nt (make-eq-hashtable)))
          (set-virtual-register! jolt-vreg-threadlocals nt)
          nt)
        t)))

;; state: #(param init) — param is the instance's thread parameter for an
;; InheritableThreadLocal and #f for a plain one, which is also the discriminator
;; the three accessors branch on. init is the initialValue source: a thunk (the
;; proxy lowering), a java.util.function.Supplier (withInitial), or nil (the bare
;; constructor, whose initialValue returns null).
(define (tl-param self) (vector-ref (jhost-state self) 0))
(define (tl-initial self)
  (let ((f (vector-ref (jhost-state self) 1)))
    (cond ((jolt-nil? f) jolt-nil)
          ;; a reified Supplier, the argument withInitial takes on the JVM. A
          ;; plain fn is accepted too: jolt has no functional-interface coercion,
          ;; so requiring the reify would only make the shim harder to call than
          ;; the class it models.
          ((iface-method f "get" 1) (record-method-dispatch f "get" jolt-nil))
          (else (jolt-invoke f)))))
(define (tl-ref self)
  (let ((p (tl-param self)))
    (if p (p) (hashtable-ref (tl-table) self tl-unset))))
(define (tl-set! self v)
  (let ((p (tl-param self)))
    (if p (p v) (hashtable-set! (tl-table) self v))))
(define (tl-clear! self)
  (let ((p (tl-param self)))
    (if p (p tl-unset) (hashtable-delete! (tl-table) self))))

;; init: a thunk / Supplier / nil. inheritable?: build the InheritableThreadLocal.
(define jolt-make-thread-local
  (case-lambda
    ;; the one-argument shape the pre-jolt-uecg seed's proxy lowering calls, kept
    ;; so a bootstrap pass driven by an older seed still mints (it means the plain
    ;; class, which is what that lowering produced for `proxy [ThreadLocal]`).
    ((init) (jolt-make-thread-local init jolt-nil))
    ((init inheritable?)
     (if (jolt-truthy? inheritable?)
         (make-jhost "inheritable-threadlocal" (vector (make-thread-parameter tl-unset) init))
         (make-jhost "threadlocal" (vector #f init))))))

;; .get computes initialValue on the first read and REMEMBERS it, so a fn with a
;; side effect (test.check's split, a spawned process) runs once per thread and
;; not once per read.
(register-host-methods! "threadlocal"
  (list (cons "get" (lambda (self)
                      (let ((v (tl-ref self)))
                        (if (eq? v tl-unset)
                            (let ((nv (tl-initial self))) (tl-set! self nv) nv)
                            v))))
        (cons "set" (lambda (self v) (tl-set! self v) jolt-nil))
        (cons "remove" (lambda (self) (tl-clear! self) jolt-nil))))
;; ALIAS, not a second registration: the two tags differ only in the class they
;; report and in the storage the state vector selects, so they must never grow
;; separate method tables that can drift.
(alias-host-methods! "inheritable-threadlocal" "threadlocal")

(for-each (lambda (nm) (register-class-ctor! nm (lambda args (jolt-make-thread-local jolt-nil jolt-nil))))
          '("ThreadLocal" "java.lang.ThreadLocal"))
(for-each (lambda (nm) (register-class-ctor! nm (lambda args (jolt-make-thread-local jolt-nil #t))))
          '("InheritableThreadLocal" "java.lang.InheritableThreadLocal"))
(register-class-statics! "java.lang.ThreadLocal"
  (list (cons "withInitial" (lambda (supplier) (jolt-make-thread-local supplier jolt-nil)))))
(def-var! "jolt.host" "make-thread-local" jolt-make-thread-local)

;; Pluggable instance? — a library registers (fn [class-name-string val] -> true
;; | false | nil); nil means "not my class, fall through". First non-nil wins.
;; user-instance-checks lives in records-interop.ss.
(register-instance-check-user-arm!
  (lambda (type-sym val)
    (let ((tname (symbol-t-name type-sym)))
      (let loop ((fs user-instance-checks))
        (if (null? fs)
            'pass
            (let ((r (seam-invoke2 (car fs) tname val)))
              (if (jolt-nil? r) (loop (cdr fs)) (if (jolt-truthy? r) #t #f))))))))
;; the bump retires every instance? answer cached while no library arm existed
;; (records-interop.ss jolt-instance-site) — the arm may claim any of them now
(def-var! "clojure.core" "__register-instance-check!"
  (lambda (f)
    (set! user-instance-checks (append user-instance-checks (list f)))
    (set! instance-arms-epoch (fx+ instance-arms-epoch 1))
    jolt-nil))

;; ---- value-semantics seams -------------------------------------------------
;; A seam fn is jolt code the HOST calls, from inside =/hash/str/compare/class of
;; any value -- the compiler and the runtime included, between any two user
;; calls. Its tail calls store their site pair (rt.ss jolt-site!) and nothing
;; clears it on return, so the pair outlived the call: every later throw from
;; host code reported the predicate as its innermost frame (jolt-lang/time's
;; jolt.time.impl/jt? showed up in every nREPL error once it loaded). The host
;; called it, so no live jolt frame can be its caller; put the slot back when it
;; returns. A throw out of the seam fn leaves its own pair, which is genuine.
(define (seam-invoke1 f x)
  (let* ((s (virtual-register jolt-vreg-site)) (r (jolt-invoke f x)))
    (set-virtual-register! jolt-vreg-site s)
    r))
(define (seam-invoke2 f a b)
  (let* ((s (virtual-register jolt-vreg-site)) (r (jolt-invoke f a b)))
    (set-virtual-register! jolt-vreg-site s)
    r))
;; A library that models its own host values (java.time via jolt-lang/time) needs
;; those values to compare, hash, print, and order like the real thing. These
;; expose the internal arm registries to Clojure: pred/handler are Clojure fns,
;; and results are coerced to the Scheme forms each arm expects (a boolean for
;; eq, an integer for hash/compare, a string for str/pr). pred should be cheap
;; and return false for values it doesn't own — it runs on the slow path of every
;; =/hash/compare/print.
(def-var! "clojure.core" "__register-eq!"
  (lambda (pred handler)
    (register-eq-arm! (lambda (a b) (jolt-truthy? (seam-invoke2 pred a b)))
                      (lambda (a b) (jolt-truthy? (seam-invoke2 handler a b))))
    jolt-nil))
(def-var! "clojure.core" "__register-hash!"
  (lambda (pred handler)
    (register-hash-arm! (lambda (x) (jolt-truthy? (seam-invoke1 pred x)))
                        (lambda (x) (seam-invoke1 handler x)))
    jolt-nil))
(def-var! "clojure.core" "__register-str!"
  (lambda (pred render)
    (register-str-render! (lambda (x) (jolt-truthy? (seam-invoke1 pred x)))
                          (lambda (x) (seam-invoke1 render x)))
    jolt-nil))
(def-var! "clojure.core" "__register-pr!"
  (lambda (pred render)
    (register-pr-arm! (lambda (x) (jolt-truthy? (seam-invoke1 pred x)))
                      (lambda (x) (seam-invoke1 render x)))
    jolt-nil))
(def-var! "clojure.core" "__register-compare!"
  (lambda (pred handler)
    (register-compare-arm! (lambda (a b) (jolt-truthy? (seam-invoke2 pred a b)))
                           (lambda (a b) (seam-invoke2 handler a b)))
    jolt-nil))

;; __register-class! makes a library's own host values answer (class x)/(type x)
;; AND dispatch protocols extended to their class. class-fn returns the class name;
;; tags-fn returns the list of class/interface names the value satisfies (its own
;; plus supertypes), which value-host-tags (records.ss) feeds to protocol dispatch.
;; Without this, (class x) is :object and (extend-protocol P TheClass …) never fires.
(define jt-user-value-tags-arms '())
(let ((prev value-host-tags))
  (set! value-host-tags
    (lambda (obj)
      (let loop ((as jt-user-value-tags-arms))
        (cond ((null? as) (prev obj))
              (((caar as) obj) ((cdar as) obj))
              (else (loop (cdr as))))))))
(define (jt-jolt-strs->list v)
  (let loop ((s (jolt-seq v)) (acc '()))
    (if (jolt-nil? s) (reverse acc) (loop (jolt-seq (jolt-rest s)) (cons (jolt-first s) acc)))))
(def-var! "clojure.core" "__register-class!"
  (lambda (pred class-fn tags-fn)
    (let ((p (lambda (x) (jolt-truthy? (seam-invoke1 pred x)))))
      (register-class-arm-checked! p (lambda (x) (seam-invoke1 class-fn x)))
      (set! jt-user-value-tags-arms
            (append jt-user-value-tags-arms
                    (list (cons p (lambda (x) (jt-jolt-strs->list (seam-invoke1 tags-fn x))))))))
    jolt-nil))

;; values that carry metadata (mirrors jolt-with-meta's set in natives-meta.ss).
(define (hsc-imeta? x)
  (or (pvec? x) (pmap? x) (pset? x) (cseq? x) (empty-list-t? x)
      (jolt-lazyseq? x) (jrec? x) (jreify? x) (procedure? x) (symbol-t? x)))
(register-instance-check-arm!
  (lambda (type-sym val)
    (let ((iface (hsc-last-segment (symbol-t-name type-sym))))
      ;; the value's own class-graph tags (value-host-tags) are authoritative — the
      ;; SAME source protocol dispatch reads, so instance? and extend-protocol can't
      ;; disagree about the interfaces a builtin implements.
      (if (let ((tags (value-host-tags val)))
            (or (member (symbol-t-name type-sym) tags) (member iface tags)))
          #t
      (let ((hit (cond
                   ;; IObj/IMeta — metadata-bearing values not tagged via jch-tags
                   ;; (cseq, empty-list, procedure, sorted-map/set)
                   ((or (string=? iface "IObj") (string=? iface "IMeta")) (hsc-imeta? val))
                   ((or (string=? iface "IMapEntry") (string=? iface "MapEntry")) (jolt-map-entry? val))
                   ;; IRecord applies only to defrecord, not bare deftype (JVM contract).
                   ((string=? iface "IRecord") (jrec-record? val))
                   ;; IFn — maps/sets/vectors are callable in jolt beyond the JVM
                   ;; class hierarchy, so jch-tags doesn't cover them for these types.
                   ;; A no on these falls THROUGH rather than deciding: a deftype or
                   ;; reify that declares clojure.lang.IFn is callable and answers
                   ;; instance? through its own declared interfaces.
                   ((string=? iface "IFn")
                    (or (procedure? val) (keyword? val) (symbol-t? val)
                        (pmap? val) (pset? val) (pvec? val)
                        'none))
                   ;; reader jhosts — data.json re-wraps a reader in a new
                   ;; PushbackReader unless (instance? PushbackReader r), so this
                   ;; must hold for repeated reads from one reader to work.
                   ((string=? iface "PushbackReader")
                    (and (jhost? val) (pushback-reader-tag? (jhost-tag val))))
                   ((string=? iface "StringReader")
                    (and (jhost? val) (string=? (jhost-tag val) "string-reader")
                         (not (string-reader-buffered? val))))
                   ((string=? iface "Reader") (reader-jhost? val))
                   ;; ...but a PushbackReader is NOT a BufferedReader: on the JVM
                   ;; it extends FilterReader, and library code branches on the
                   ;; difference — tools.reader's read-line routes a BufferedReader
                   ;; through *in* and reads anything else char by char. What
                   ;; io/reader hands back is an in-memory StringReader here and a
                   ;; BufferedReader on the JVM, so the string-reader tag keeps
                   ;; answering true; a bare (java.io.StringReader. s) shares that
                   ;; tag and answers with it (known-divergences, :host-model).
                   ;; what io/reader made is one (a string-reader it marked, a
                   ;; char-reader classed BufferedReader); a StringReader, a
                   ;; FileReader or an InputStreamReader is not
                   ((string=? iface "BufferedReader")
                    (cond ((and (jhost? val) (string=? (jhost-tag val) "char-reader") (stream-class val))
                           => (lambda (c) (jch-isa? c "java.io.BufferedReader")))
                          ((not (reader-jhost? val)) #f)
                          ((pushback-reader-tag? (jhost-tag val)) #f)
                          ((string=? (jhost-tag val) "string-reader") (string-reader-buffered? val))
                          (else #t)))
                   (else 'none))))
        (if (eq? hit 'none) 'pass (if hit #t #f)))))))

;; The Class methods that answer with an ARRAY — reflection over the constructor
;; table, and the annotation surface (jolt models no annotations, so every class
;; reports none, which is the JVM's own answer for an unannotated class). The rest
;; of java.lang.Class is java/class-model.ss, shared with the Gambit target, which
;; has no arrays; these join the same "class" table.
(register-host-methods! "class"
  (list (cons "getConstructors" (lambda (self) (class-constructors self)))
        (cons "getDeclaredConstructors" (lambda (self) (class-constructors self)))
        (cons "getAnnotations" (lambda (self) (make-jolt-array (vector) "java.lang.annotation.Annotation")))
        (cons "getDeclaredAnnotations" (lambda (self) (make-jolt-array (vector) "java.lang.annotation.Annotation")))))

;; ---- java.lang.reflect.Modifier ---------------------------------------------
;; The bit constants and their predicates, the JVM's values, over whatever int a
;; caller has — Class.getModifiers here, and a member's modifiers read the same.
(define (mod-bit? m bit) (if (fx=? 0 (fxand (jnum->exact m) bit)) #f #t))
;; Modifier.toString's rendering, as a procedure rather than only as a registered
;; static: the reflective member objects (java/natives-array.ss) spell their own
;; toString the way the JVM does — "public static final java.lang.Object Foo.BAR" —
;; and must name the modifiers with the very words Modifier/toString would.
(define (jmod-string m)
  (let loop ((ps (list (cons 1 "public") (cons 4 "protected") (cons 2 "private")
                       (cons 1024 "abstract") (cons 8 "static") (cons 16 "final")
                       (cons 32 "synchronized") (cons 256 "native") (cons 2048 "strictfp")
                       (cons 128 "transient") (cons 64 "volatile") (cons 512 "interface")))
             (acc '()))
    (if (null? ps)
        (let ((names (reverse acc)))
          (if (null? names) ""
              (fold-left (lambda (a b) (string-append a " " b)) (car names) (cdr names))))
        (loop (cdr ps) (if (mod-bit? m (caar ps)) (cons (cdar ps) acc) acc)))))
(register-class-statics! "java.lang.reflect.Modifier"
  (list (cons "PUBLIC" (->num 1)) (cons "PRIVATE" (->num 2)) (cons "PROTECTED" (->num 4))
        (cons "STATIC" (->num 8)) (cons "FINAL" (->num 16)) (cons "SYNCHRONIZED" (->num 32))
        (cons "VOLATILE" (->num 64)) (cons "TRANSIENT" (->num 128)) (cons "NATIVE" (->num 256))
        (cons "INTERFACE" (->num 512)) (cons "ABSTRACT" (->num 1024)) (cons "STRICT" (->num 2048))
        (cons "isPublic" (lambda (m) (mod-bit? m 1)))
        (cons "isPrivate" (lambda (m) (mod-bit? m 2)))
        (cons "isProtected" (lambda (m) (mod-bit? m 4)))
        (cons "isStatic" (lambda (m) (mod-bit? m 8)))
        (cons "isFinal" (lambda (m) (mod-bit? m 16)))
        (cons "isSynchronized" (lambda (m) (mod-bit? m 32)))
        (cons "isVolatile" (lambda (m) (mod-bit? m 64)))
        (cons "isTransient" (lambda (m) (mod-bit? m 128)))
        (cons "isNative" (lambda (m) (mod-bit? m 256)))
        (cons "isInterface" (lambda (m) (mod-bit? m 512)))
        (cons "isAbstract" (lambda (m) (mod-bit? m 1024)))
        (cons "isStrict" (lambda (m) (mod-bit? m 2048)))
        ;; Modifier.toString lists the set bits in the JLS's canonical order.
        (cons "toString" jmod-string)))

;; ---- constructors as values -------------------------------------------------
;; Enough of java.lang.reflect.Constructor to pick a constructor by arity and call
;; it, which is how a Clojure-level reader builds a #ns.Rec[…] literal: find the
;; constructor whose parameter count matches, then invoke it. jolt knows the arity
;; of a deftype or defrecord constructor from its declared fields; for any other
;; class it has no signature to report, so getConstructors is empty there and a
;; caller sees the same "no matching constructor" it would for a real mismatch.
;; A record also carries the JVM's (fields + meta + ext-map) constructor, so an
;; arity-counting caller finds the same two it would on the JVM.
(define ctor-arity-probe '(0 1 2 3 4 5))
(define (ctor-obj cls arity) (make-jhost "class-ctor" (vector cls arity)))
;; A Constructor[], as Class.getConstructors answers.
(define (class-constructors cls)
  (make-jolt-array (list->vector (seq->list (jolt-seq (class-constructor-list cls))))
                   "java.lang.reflect.Constructor"))
(define (class-constructor-list cls)
  (let* ((nm (jclass-name cls))
         (dbl (hashtable-ref chez-record-dbl-tbl nm #f)))
    (cond
      ;; a deftype or defrecord: its fields ARE its signature
      (dbl (let ((n (vector-length dbl)))
             (if (hashtable-ref chez-record-type-tbl nm #f)
                 (jolt-vector (ctor-obj cls n) (ctor-obj cls (+ n 2)))
                 (jolt-vector (ctor-obj cls n)))))
      ;; a host class jolt backs: the arities its registered constructor accepts.
      ;; Many are variadic and coerce, so the probe stops at 5 rather than claiming
      ;; every arity; past that a caller gets the constructor's own error.
      ((lookup-class class-ctors-tbl nm)
       => (lambda (c)
            (let ((mask (procedure-arity-mask c)))
              (apply jolt-vector
                     (map (lambda (k) (ctor-obj cls k))
                          (filter (lambda (k) (bitwise-bit-set? mask k)) ctor-arity-probe))))))
      (else (jolt-vector)))))
;; "java.lang.Object, java.lang.Object" — the parameter list a member's toString
;; prints between its parens. jolt carries no signatures, so every parameter is an
;; Object; the COUNT is the part that is real, and it is the part a caller reading
;; the string is looking for.
(define (jreflect-param-str n)
  (if (fx=? n 0) ""
      (let loop ((k (fx- n 1)) (acc "java.lang.Object"))
        (if (fx=? k 0) acc (loop (fx- k 1) (string-append acc ", java.lang.Object"))))))
(register-host-methods! "class-ctor"
  (list (cons "getParameterCount" (lambda (self) (->num (vector-ref (jhost-state self) 1))))
        (cons "getParameterTypes"
              (lambda (self)
                (make-jolt-array (make-vector (vector-ref (jhost-state self) 1)
                                              (jolt-class-for "java.lang.Object"))
                                 "java.lang.Class")))
        (cons "getDeclaringClass" (lambda (self) (vector-ref (jhost-state self) 0)))
        ;; Constructor.getName is the DECLARING CLASS's fully qualified name on
        ;; the JVM, not a member name of its own.
        (cons "getName" (lambda (self) (jclass-name (vector-ref (jhost-state self) 0))))
        (cons "getModifiers" (lambda (self) (->num 1)))
        (cons "getExceptionTypes" (lambda (self) (make-jolt-array (vector) "java.lang.Class")))
        (cons "setAccessible" (lambda (self v) jolt-nil))
        ;; Constructor.newInstance(Object... initargs): from Clojure the varargs
        ;; arrive as ONE array, which is the argument list, and a nil is the empty
        ;; one (sci.impl.reflector's box-args answers nil for no parameters) — the
        ;; same contract Method.invoke reads (java/natives-array.ss). Arguments
        ;; spelled out loose pass straight through.
        (cons "newInstance"
              (lambda (self . args)
                (apply reflect-construct (vector-ref (jhost-state self) 0)
                       (reflect-varargs args))))
        ;; the JVM's shape — "public user.Foo(java.lang.Object, java.lang.Object)".
        ;; It used to print the bare class name, which is also what getName answers,
        ;; so two constructors of different arity were indistinguishable in output.
        (cons "toString"
              (lambda (self)
                (string-append "public " (jclass-name (vector-ref (jhost-state self) 0))
                               "(" (jreflect-param-str (vector-ref (jhost-state self) 1)) ")")))))
(register-str-render! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "class-ctor")))
                      (lambda (x) (record-method-dispatch x "toString" jolt-nil)))

;; ---- clojure.lang.Reflector -------------------------------------------------
;; The reflective entry points Clojure code reaches for by name. Each is the
;; dynamic form of an interop call jolt already performs, so they route to the
;; same registries rather than to a second mechanism.
(define (reflect-args a) (if (jolt-nil? a) '() (seq->list (jolt-seq a))))
;; The trailing Object... of a reflective call (Constructor.newInstance,
;; Method.invoke) as the argument list: a lone array IS the list, a lone nil is
;; the empty one — as on the JVM, where a null varargs array means none — and
;; loose arguments are themselves.
(define (reflect-varargs args)
  (cond ((and (pair? args) (null? (cdr args)) (jolt-array? (car args))) (ja->list (car args)))
        ((and (pair? args) (null? (cdr args)) (jolt-nil? (car args))) '())
        (else args)))
(define (reflect-construct cls . args)
  (apply host-new (if (jclass? cls) (jclass-name cls) (jolt-str-render-one cls)) args))
;; clojure.lang.Var / Symbol / Keyword statics — all four used to raise "No
;; dependency provides", and typedclojure gensyms every fresh type variable
;; through Symbol/intern. Var.find is find-var plus the JVM's missing-namespace
;; error (find-var answers nil for any missing cell); Var.intern is spelled out
;; below, where it parts ways with clojure.core/intern. Symbol.intern / create are `symbol` (the first slash
;; splits the namespace); Keyword.intern is `keyword`; Keyword.find answers only
;; a keyword the intern table already holds, nil otherwise, as on the JVM.
(register-class-statics! "clojure.lang.Var"
  (list (cons "find" (lambda (sym)
          (let ((sns (symbol-t-ns sym)))
            (if (and (string? sns) (not (hashtable-ref ns-registry sns #f)))
                (throw-jvm (quote IllegalArgumentException) (string-append "No such namespace: " sns))
                (jolt-find-var sym)))))
        ;; Var.intern is NOT clojure.core/intern: the two-argument overload takes a
        ;; symbol and findOrCreates the namespace, where clojure.core/intern raises
        ;; on a missing one; and only the Namespace overload takes a root, so a
        ;; symbol with one is the JVM's ClassCastException.
        (cons "intern" (lambda (ns sym . root)
          (when (and (pair? root) (not (jns? ns)))
            (throw-jvm (quote ClassCastException)
                       "class clojure.lang.Symbol cannot be cast to class clojure.lang.Namespace"))
          (unless (jns? ns) (jolt-create-ns ns))
          (apply jolt-intern ns sym root)))))
(register-class-statics! "clojure.lang.Symbol"
  (list (cons "intern" (lambda args (apply jolt-symbol-new args)))
        (cons "create" (lambda args (apply jolt-symbol-new args)))))
(define (keyword-find . args)
  (let-values (((ns name)
                (if (= (length args) 2)
                    (values (let ((n (car args))) (if (jolt-nil? n) #f n)) (jolt-need-string (cadr args)))
                    (let ((s (jolt-symbol-new (car args))))   ; a symbol, or a string split at its first slash
                      (values (symbol-t-ns s) (symbol-t-name s))))))
    (or (if ns
            (hashtable-ref keyword-table (keyword-intern-key ns name) #f)
            (hashtable-ref keyword-table-bare name #f))
        jolt-nil)))
(register-class-statics! "clojure.lang.Keyword"
  (list (cons "intern" (lambda args (apply jolt-keyword args)))
        (cons "find" keyword-find)))
(register-class-statics! "clojure.lang.Reflector"
  (list (cons "invokeConstructor"
              (lambda (cls args) (apply reflect-construct cls (reflect-args args))))
        (cons "invokeStaticMethod"
              (lambda (cls method args)
                (apply host-static-call (if (jclass? cls) (jclass-name cls) (jolt-str-render-one cls))
                       (jolt-str-render-one method) (reflect-args args))))
        (cons "invokeInstanceMethod"
              (lambda (target method args)
                (record-method-dispatch target (jolt-str-render-one method)
                                        (list->cseq (reflect-args args)))))
        ;; The two companions a reflective caller reaching a member through
        ;; getMethods (java/natives-array.ss) needs. prepRet unboxes a primitive
        ;; return on the JVM; jolt reports Object for every return type, so the
        ;; value passes through. getAsMethodOfAccessibleBase opens a member
        ;; declared by a non-public class; jolt's members carry no accessibility
        ;; state to open, and its classes are public, so the member stands.
        (cons "prepRet" (lambda (c ret) ret))
        (cons "getAsMethodOfAccessibleBase" (lambda (c m target) m))))
;; A deftype/defrecord type token answers every java.lang.Class method, through
;; the SAME table (class inst) uses — records-dispatch.ss owns the token arm and
;; loads before this file, so it calls back through here. Returns a one-element
;; list holding the result (so a nil/#f answer is still a hit) or #f for "no such
;; method", which is what lets that arm fall through to its own error.
(set-rd-class-method-hook!
  (lambda (tag method-name args)
    (let ((f (host-method-ref "class" method-name)))
      (and f (list (apply f (jolt-class-for tag) args))))))

;; (class x) on a jclass value returns java.lang.Class, so (instance? Class
;; (class y)) and class-based dispatch see the correct JVM class.
(register-class-arm! jclass? (lambda (x) "java.lang.Class"))

;; (jolt.host/table? x) — is x a host tagged-table?
(def-var! "jolt.host" "table?" (lambda (x) (if (htable? x) #t #f)))

;; --- Arrays.asList ----------------------------------------------------------
;; A fixed-size List VIEW of the array: .set writes the array and aset shows
;; through the list, and add/remove raise UnsupportedOperationException, as
;; java.util.Arrays$ArrayList does. The view shares the array's backing vector
;; in ArrayList's state layout — #(backing count head) — so the ArrayList
;; accessors read it unchanged. A reference array's backing is a plain vector
;; and is never replaced (only fxvector and string backings promote), so the
;; view cannot detach from its array.
;;
;; asList is varargs: an Object[] is the list's array, and loose elements get a
;; fresh one, the looseness String/format already has.
(define (arrays-as-list . args)
  (let ((arr (if (and (= 1 (length args)) (jolt-array? (car args))
                      (na-ref-kind? (jolt-array-kind (car args))))
                 (car args)
                 (make-jolt-array (list->vector args) 'object))))
    (make-jhost "arrays-aslist" (vector (jolt-array-vec arr) (ja-len arr) 0))))
(define (aslist-unsupported . _) (throw-jvm 'UnsupportedOperationException jolt-nil))
(define arrays-aslist-methods
  (let ((read-only (lambda (name) (cdr (assoc name arraylist-methods)))))
    (append
      (map (lambda (n) (cons n (read-only n)))
           '("get" "set" "size" "isEmpty" "contains" "toArray" "iterator"))
      (list (cons "getFirst" al-first) (cons "getLast" al-last))
      (map (lambda (e) (cons (car e) (host-arity-of (cdr e) #t aslist-unsupported)))
           '(("add" 1 2) ("addAll" 1 2) ("remove" 1) ("clear" 0)
             ("addFirst" 1) ("addLast" 1) ("removeFirst" 0) ("removeLast" 0))))))
(register-host-methods! "arrays-aslist" arrays-aslist-methods)
(register-jutil-coll! "arrays-aslist" 'ralist al->list)

;; An unmodifiable List (java.util.ImmutableCollections$ListN) — what
;; Stream.toList and List.copyOf answer: every reader of the ArrayList table, and
;; every mutator, set included, raises UnsupportedOperationException. Same state
;; layout as the ArrayList, #(backing count head).
(define (make-immutable-list xs)
  (make-jhost "immutable-list" (vector (list->vector xs) (length xs) 0)))
(register-host-methods! "immutable-list"
  (append
    (map (lambda (n) (cons n (cdr (assoc n arraylist-methods))))
         '("get" "size" "isEmpty" "contains" "toArray" "iterator" "forEach"))
    (list (cons "getFirst" al-first) (cons "getLast" al-last))
    (map (lambda (e) (cons (car e) (host-arity-of (cdr e) #t aslist-unsupported)))
         '(("set" 2) ("add" 1 2) ("addAll" 1 2) ("remove" 1) ("clear" 0) ("removeIf" 1)
           ("replaceAll" 1) ("sort" 1) ("addFirst" 1) ("addLast" 1)
           ("removeFirst" 0) ("removeLast" 0)))))
(register-jutil-coll! "immutable-list" 'ralist al->list)

;; --- java.util.Arrays -------------------------------------------------------
;; Arrays/sort sorts IN PLACE and returns void, so it writes back through the
;; array's own backing (whichever of the four natives-array.ss picks for the
;; element kind) rather than building a new array — orchard.profile relies on
;; (doto (Arrays/copyOfRange …) Arrays/sort). list-sort is a stable merge sort,
;; matching Arrays.sort over objects. The comparator goes through cmp->less, the
;; shared comparator seam, so a reify/deftype Comparator works here exactly as it
;; does in sort / sort-by.
;; JVM overloads: sort(a), sort(a, cmp), sort(a, from, to), sort(a, from, to, cmp).
;; Two args means a comparator — sort(a, from) is not an overload.
(define (arrays-sort! a from to cmp)
  (let* ((f (jnum->exact from))
         (t (if to (jnum->exact to) (ja-len a)))
         (less? (cmp->less cmp))
         (items (let loop ((i f) (acc '()))
                  (if (fx>=? i t) (reverse acc) (loop (fx+ i 1) (cons (ja-ref a i) acc))))))
    (let loop ((i f) (xs (list-sort less? items)))
      (if (null? xs) jolt-nil
          (begin (ja-set! a i (car xs)) (loop (fx+ i 1) (cdr xs)))))))
(define arrays-sort
  (case-lambda
    ((a) (arrays-sort! a 0 #f jolt-compare))
    ((a cmp) (arrays-sort! a 0 #f cmp))
    ((a from to) (arrays-sort! a from to jolt-compare))
    ((a from to cmp) (arrays-sort! a from to cmp))))
(let ((arrays-statics
       (list
         (cons "equals" (lambda (a b)
                          (cond ((and (jolt-nil? a) (jolt-nil? b)) #t)
                                ((or (jolt-nil? a) (jolt-nil? b)) #f)
                                (else (ja-equal? a b)))))
         (cons "fill" (lambda (a v)
                        (let ((n (ja-len a)) (v (na-elem-of (jolt-array-kind a) v)))
                          (do ((i 0 (fx+ i 1))) ((fx=? i n) jolt-nil) (ja-set! a i v)))))
         ;; The tail past the source is the element kind's ZERO — nil in a
         ;; reference array, #\nul in a char[], false in a boolean[] — not the 0
         ;; every kind used to get regardless.
         (cons "copyOf" (lambda (a n)
                          (let* ((len (jnum->exact n)) (kind (jolt-array-kind a))
                                 (out (make-jolt-array (na-make-backing len kind (na-zero-of kind)) kind)))
                            (do ((i 0 (fx+ i 1))) ((fx=? i (min len (ja-len a))) out)
                              (ja-set! out i (ja-ref a i))))))
         (cons "copyOfRange" (lambda (a from to)
                               (let* ((f (jnum->exact from)) (tt (jnum->exact to))
                                      (len (- tt f)) (kind (jolt-array-kind a))
                                      (out (make-jolt-array (na-make-backing len kind (na-zero-of kind)) kind)))
                                 (do ((i 0 (fx+ i 1))) ((fx=? i len) out)
                                   (ja-set! out i (ja-ref a (+ f i)))))))
         (cons "sort" arrays-sort)
         (cons "deepEquals" (lambda (a b) (objects-deep-equals? a b)))
         ;; Arrays.toString is "[a, b]" — comma-separated element toString, "null"
         ;; for a nil array. It used to print the elements as a jolt VECTOR, which
         ;; renders "[a b]" (no commas) and pr-quotes a string element.
         (cons "toString" (lambda (a)
                            (if (jolt-nil? a) "null"
                                (let ((parts (map (lambda (x) (if (jolt-nil? x) "null" (jolt-str-render-one x)))
                                                  (ja->list a))))
                                  (string-append
                                    "[" (if (null? parts) ""
                                            (fold-left (lambda (acc s) (string-append acc ", " s))
                                                       (car parts) (cdr parts)))
                                    "]")))))
         (cons "asList" arrays-as-list))))
  (register-class-statics! "Arrays" arrays-statics)
  (register-class-statics! "java.util.Arrays" arrays-statics))

;; --- java.util.Objects ------------------------------------------------------
;; The null-safe forms of equals/hashCode/toString, over the same seams a value's
;; own .equals and .hashCode take (record-method-dispatch, jolt-java-hashcode), so
;; Objects/equals on two jolt values answers what (.equals a b) does: an array is
;; equal only to itself, a vector equals a list with the same elements.
;; objects-equals? itself lives in natives-misc.ss, which both hosts load.
(define (objects-deep-equals? a b)
  (cond ((eq? a b) #t)
        ((or (jolt-nil? a) (jolt-nil? b)) #f)
        ((and (jolt-array? a) (jolt-array? b))
         ;; Arrays.deepEquals0: two reference arrays (any component class — a
         ;; String[] against an Object[]) compare element-wise, two primitive
         ;; arrays only when their element type is the same
         (and (let ((ka (jolt-array-kind a)) (kb (jolt-array-kind b)))
                (or (eq? ka kb) (and (na-ref-kind? ka) (na-ref-kind? kb))))
              (= (ja-len a) (ja-len b))
              (let loop ((i 0))
                (or (fx=? i (ja-len a))
                    (and (objects-deep-equals? (ja-ref a i) (ja-ref b i)) (loop (fx+ i 1)))))))
        (else (objects-equals? a b))))
;; Arrays.hashCode(Object[]), which Objects.hash is: 31*h + hash(e) from 1, in
;; 32-bit int arithmetic, nil hashing to 0.
(define (objects-hash-of xs)
  (fold-left (lambda (h x) (i32 (+ (* 31 h) (jolt-java-hashcode x)))) 1 xs))
;; A Supplier argument: a reified Supplier's get, or a plain fn (as withInitial
;; takes above).
(define (objects-supply f)
  (if (iface-method f "get" 1) (record-method-dispatch f "get" jolt-nil) (jolt-invoke f)))
(define (objects-npe msg) (throw-jvm 'NullPointerException msg))
(let ((objects-statics
       (list
         (cons "equals" (lambda (a b) (objects-equals? a b)))
         (cons "deepEquals" (lambda (a b) (objects-deep-equals? a b)))
         (cons "hashCode" (lambda (o) (->num (jolt-java-hashcode o))))
         ;; varargs: an Object[] is the values, loose values are too
         (cons "hash" (lambda xs
                        (->num (objects-hash-of
                                 (if (and (= 1 (length xs)) (jolt-array? (car xs))
                                          (na-ref-kind? (jolt-array-kind (car xs))))
                                     (ja->list (car xs))
                                     xs)))))
         (cons "toString"
               (case-lambda
                 ((o) (if (jolt-nil? o) "null" (jolt-str-render-one o)))
                 ((o d) (if (jolt-nil? o) d (jolt-str-render-one o)))))
         (cons "isNull" (lambda (o) (jolt-nil? o)))
         (cons "nonNull" (lambda (o) (not (jolt-nil? o))))
         ;; the message is a String, or a Supplier of one asked only on failure
         (cons "requireNonNull"
               (case-lambda
                 ((o) (if (jolt-nil? o) (objects-npe jolt-nil) o))
                 ((o m) (if (jolt-nil? o)
                            (objects-npe (cond ((string? m) m)
                                               ((jolt-nil? m) jolt-nil)
                                               (else (objects-supply m))))
                            o))))
         (cons "requireNonNullElse"
               (lambda (o d)
                 (cond ((not (jolt-nil? o)) o)
                       ((not (jolt-nil? d)) d)
                       (else (objects-npe "defaultObj")))))
         (cons "requireNonNullElseGet"
               (lambda (o s)
                 (cond ((not (jolt-nil? o)) o)
                       ((jolt-nil? s) (objects-npe "supplier"))
                       (else (let ((v (objects-supply s)))
                               (if (jolt-nil? v) (objects-npe "supplier.get()") v))))))
         (cons "compare"
               (lambda (a b c) (if (eq? a b) 0 ((jolt-comparator-fn c) a b))))
         (cons "checkIndex"
               (lambda (i n)
                 (let ((i (jnum->exact i)) (n (jnum->exact n)))
                   (if (and (>= i 0) (< i n))
                       (->num i)
                       (throw-jvm 'IndexOutOfBoundsException
                         (string-append "Index " (number->string i)
                                        " out of bounds for length " (number->string n))))))))))
  (register-class-statics! "Objects" objects-statics)
  (register-class-statics! "java.util.Objects" objects-statics))

;; --- java.util.Random -------------------------------------------------------
;; Java-compatible LCG: java.util.Random's exact algorithm.
;; State is #(seed) where seed is a 48-bit exact integer.
;; Reference: JDK java.util.Random source.

(define random-multiplier #x5DEECE66D)
(define random-addend #xB)
(define random-mask #xFFFFFFFFFFFF)  ;; (1<<48)-1

(define (random-init-seed given-seed)
  (bitwise-and (bitwise-xor (exact (truncate given-seed)) random-multiplier) random-mask))

(define (random-next bits seed-vec)
  (let* ((old-seed (vector-ref seed-vec 0))
         (new-seed (bitwise-and (+ (* old-seed random-multiplier) random-addend) random-mask)))
    (vector-set! seed-vec 0 new-seed)
    (bitwise-arithmetic-shift-right new-seed (- 48 bits))))

;; Convert an unsigned 32-bit value to signed 32-bit (Java's (int) cast).
(define (random-u32->s32 v)
  (if (>= v #x80000000) (- v #x100000000) v))

;; Simulate Java's 32-bit signed addition/subtraction overflow — used for the
;; rejection-sampling check in nextInt(bound): (u - r + bound - 1) < 0,
;; where the overflow of the intermediate 32-bit signed expression is the
;; rejection criterion.
(define (random-overflow-lt0 u r bound)
  (let ((raw (+ (- u r) bound -1)))
    (< (random-u32->s32 (bitwise-and raw #xFFFFFFFF)) 0)))

(for-each
  (lambda (nm)
    (register-class-ctor! nm
      (lambda args
        ;; No-seed (Random.) used (truncate (current-time)) — current-time returns a
        ;; time OBJECT, not a number, so truncate threw and the no-arg ctor never
        ;; worked at all. The JVM seeds this instance from nanoTime mixed with a
        ;; uniquifier; jolt-random is already seeded per process and per thread, so
        ;; drawing the 48-bit seed from it gives distinct instances the same way.
        (let ((given (if (pair? args) (car args) (jolt-random 281474976710656))))
          (make-jhost "random" (vector (random-init-seed given)))))))
  '("Random" "java.util.Random"))
(register-host-methods! "random"
  (list
    ;; nextBytes draws one nextInt() per FOUR bytes, taking them low-to-high — it
    ;; does not draw 8 bits per byte, which consumed the LCG four times as fast and
    ;; produced a different stream from the JVM's for the same seed. Elements are
    ;; signed bytes (the JVM's (byte)rnd cast).
    (cons "nextBytes" (lambda (self ba)
                        (let* ((n (ja-len ba)) (st (jhost-state self)))
                          (let loop ((i 0))
                            (when (fx<? i n)
                              (let inner ((rnd (random-u32->s32 (random-next 32 st)))
                                          (k (min (fx- n i) 4)) (i i))
                                (if (fx=? k 0) (loop i)
                                    (begin (ja-set! ba i (na-byte-of (bitwise-and rnd #xff)))
                                           (inner (bitwise-arithmetic-shift-right rnd 8)
                                                  (fx- k 1) (fx+ i 1)))))))
                          jolt-nil)))
    (cons "nextInt" (lambda (self . a)
                      (let ((st (jhost-state self)))
                        (if (pair? a)
                            (let ((bound (exact (truncate (car a)))))
                              (if (<= bound 0)
                                  (throw-jvm (quote IllegalArgumentException) "bound must be positive")
                                  (let ((m (- bound 1)))
                                    (if (fx=? (bitwise-and bound m) 0)
                                        ;; power of two
                                        (->num (random-u32->s32
                                                 (bitwise-arithmetic-shift-right
                                                   (* bound (random-next 31 st)) 31)))
                                        ;; rejection sample with 32-bit overflow semantics
                                        (let loop ((u (random-u32->s32 (random-next 31 st))))
                                          (let ((r (modulo u bound)))
                                            (if (random-overflow-lt0 u r bound)
                                                (loop (random-u32->s32 (random-next 31 st)))
                                                (->num r))))))))
                            (->num (random-u32->s32 (random-next 32 st)))))))
    (cons "nextLong" (lambda (self)
                       (let ((st (jhost-state self)))
                         (let* ((hi (random-u32->s32 (random-next 32 st)))
                                (lo (random-u32->s32 (random-next 32 st))))
                           (->num (+ (* hi (expt 2 32)) lo))))))
    (cons "nextDouble" (lambda (self)
                         (let* ((st (jhost-state self))
                                (hi (random-next 26 st))
                                (lo (random-next 27 st)))
                           (* (+ (* hi (expt 2 27)) lo)
                              (/ 1.0 (expt 2 53))))))
    (cons "nextFloat" (lambda (self)
                        (let ((st (jhost-state self)))
                          (/ (random-next 24 st) (exact->inexact (expt 2 24))))))
    (cons "nextBoolean" (lambda (self) (fx=? 1 (random-next 1 (jhost-state self)))))))

;; --- java.util.SplittableRandom ------------------------------------------------
;; SplitMix64, the JDK's algorithm step for step: a 64-bit seed advanced by an
;; odd gamma, each output a mix of the new seed. State is #(seed gamma) held as
;; unsigned 64-bit values; results come back as the JVM's signed long/int.
;; test.check's JavaUtilSplittableRandom is a port of this class, and its suite
;; checks the port against the real one draw for draw.
(define sr-mask64 #xFFFFFFFFFFFFFFFF)
(define sr-golden #x9e3779b97f4a7c15)
(define (sr-u64 x) (bitwise-and x sr-mask64))
(define (sr-s64 x) (let ((u (sr-u64 x))) (if (>= u #x8000000000000000) (- u #x10000000000000000) u)))
(define (sr-s32 x) (let ((u (bitwise-and x #xFFFFFFFF))) (if (>= u #x80000000) (- u #x100000000) u)))
(define (sr-xorshift z n) (bitwise-xor z (bitwise-arithmetic-shift-right z n)))
(define (sr-mix64 z)
  (let* ((z (sr-u64 (* (sr-xorshift z 30) #xbf58476d1ce4e5b9)))
         (z (sr-u64 (* (sr-xorshift z 27) #x94d049bb133111eb))))
    (sr-xorshift z 31)))
(define (sr-mix32 z)
  (let ((z (sr-u64 (* (sr-xorshift z 33) #x62a9d9ed799705f5))))
    (sr-s32 (bitwise-arithmetic-shift-right (sr-u64 (* (sr-xorshift z 28) #xcb24d0a5c88c35b3)) 32))))
(define (sr-mix-gamma z)
  (let* ((z (sr-u64 (* (sr-xorshift z 33) #xff51afd7ed558ccd)))
         (z (sr-u64 (* (sr-xorshift z 33) #xc4ceb9fe1a85ec53)))
         (z (bitwise-ior (sr-xorshift z 33) 1)))
    (if (< (bitwise-bit-count (sr-xorshift z 1)) 24)
        (bitwise-xor z #xaaaaaaaaaaaaaaaa)
        z)))
(define (make-splittable-random seed gamma)
  (make-jhost "splittable-random" (vector (sr-u64 seed) gamma)))
(define (sr-next-seed! st)
  (let ((s (sr-u64 (+ (vector-ref st 0) (vector-ref st 1)))))
    (vector-set! st 0 s)
    s))
(define (sr-next-long st) (sr-s64 (sr-mix64 (sr-next-seed! st))))
(define (sr-next-int st) (sr-mix32 (sr-next-seed! st)))
(define (sr-next-double st)
  (* (bitwise-arithmetic-shift-right (sr-mix64 (sr-next-seed! st)) 11) (expt 2.0 -53)))
;; An argument to a long or int parameter narrows the way reflective dispatch
;; narrows it on the JVM, through Number.longValue/intValue: an integer wraps to
;; the parameter's width and a double saturates (NaN is 0). So (.nextInt r
;; 3000000000) reaches the JDK as a negative bound and throws its "bound must be
;; positive", instead of drawing from a range no int can reach.
(define (sr-narrow x lo hi wrap)
  (if (flonum? x)
      (cond ((not (= x x)) 0)
            ((<= x (inexact lo)) lo)
            ((>= x (inexact hi)) hi)
            (else (exact (truncate x))))
      (wrap (exact (truncate x)))))
(define (sr-long-arg x)
  (sr-narrow x #x-8000000000000000 #x7FFFFFFFFFFFFFFF sr-s64))
(define (sr-int-arg x)
  (sr-narrow x #x-80000000 #x7FFFFFFF sr-s32))
;; Math.nextDown: the largest double below d, for a bounded nextDouble that
;; rounded up to its bound. Both zeros step to -Double/MIN_VALUE.
(define (sr-next-down d)
  (if (= d 0.0)
      -4.9406564584124654e-324
      (let ((bv (make-bytevector 8)))
        (bytevector-ieee-double-set! bv 0 d (endianness little))
        (let ((bits (bytevector-s64-ref bv 0 (endianness little))))
          (bytevector-s64-set! bv 0 (if (> d 0.0) (- bits 1) (+ bits 1)) (endianness little))
          (bytevector-ieee-double-ref bv 0 (endianness little))))))
;; RandomSupport.boundedNextLong / boundedNextInt: rejection sampling written with
;; the JVM's wrapping arithmetic, so the draws consumed match the JDK's.
(define (sr-bounded-long st origin bound)
  (let ((r (sr-next-long st)))
    (let* ((n (sr-s64 (- bound origin))) (m (sr-s64 (- n 1))))
      (cond
        ((= 0 (bitwise-and n m)) (sr-s64 (+ (bitwise-and r m) origin)))
        ((> n 0)
         (let loop ((u (bitwise-arithmetic-shift-right (sr-u64 r) 1)))
           (let ((r (remainder u n)))
             (if (< (sr-s64 (- (+ u m) r)) 0)
                 (loop (bitwise-arithmetic-shift-right (sr-u64 (sr-next-long st)) 1))
                 (sr-s64 (+ r origin))))))
        (else
         (let loop ((r r))
           (if (or (< r origin) (>= r bound)) (loop (sr-next-long st)) r)))))))
(define (sr-bounded-int st origin bound)
  (let ((r (sr-next-int st)))
    (let* ((n (sr-s32 (- bound origin))) (m (sr-s32 (- n 1))))
      (cond
        ((= 0 (bitwise-and n m)) (sr-s32 (+ (bitwise-and r m) origin)))
        ((> n 0)
         (let loop ((u (bitwise-arithmetic-shift-right (bitwise-and r #xFFFFFFFF) 1)))
           (let ((r (remainder u n)))
             (if (< (sr-s32 (- (+ u m) r)) 0)
                 (loop (bitwise-arithmetic-shift-right (bitwise-and (sr-next-int st) #xFFFFFFFF) 1))
                 (sr-s32 (+ r origin))))))
        (else
         (let loop ((r r))
           (if (or (< r origin) (>= r bound)) (loop (sr-next-int st)) r)))))))
(define (sr-check-bound bound)
  (unless (> bound 0) (throw-jvm 'IllegalArgumentException "bound must be positive")))
(define (sr-check-range origin bound)
  (unless (< origin bound) (throw-jvm 'IllegalArgumentException "bound must be greater than origin")))

(for-each
  (lambda (nm)
    (register-class-ctor! nm
      (lambda args
        (if (pair? args)
            (make-splittable-random (sr-long-arg (car args)) sr-golden)
            ;; the JDK seeds a no-arg instance from a shared generator; jolt-random
            ;; is seeded per process and per thread, which serves the same purpose
            (let ((s (jolt-random #x10000000000000000)))
              (make-splittable-random (sr-mix64 s) (sr-mix-gamma (sr-u64 (+ s sr-golden)))))))))
  '("SplittableRandom" "java.util.SplittableRandom"))
(register-host-methods! "splittable-random"
  (list
    (cons "split" (lambda (self)
                    (let* ((st (jhost-state self))
                           (seed (sr-next-long st)))
                      (make-splittable-random seed (sr-mix-gamma (sr-next-seed! st))))))
    (cons "nextLong" (lambda (self . a)
                       (let ((st (jhost-state self)))
                         (cond
                           ((null? a) (sr-next-long st))
                           ((null? (cdr a))
                            (let ((b (sr-long-arg (car a))))
                              (sr-check-bound b)
                              (sr-bounded-long st 0 b)))
                           (else
                            (let ((o (sr-long-arg (car a))) (b (sr-long-arg (cadr a))))
                              (sr-check-range o b)
                              (sr-bounded-long st o b)))))))
    (cons "nextInt" (lambda (self . a)
                      (let ((st (jhost-state self)))
                        (cond
                          ((null? a) (sr-next-int st))
                          ((null? (cdr a))
                           (let ((b (sr-int-arg (car a))))
                             (sr-check-bound b)
                             (sr-bounded-int st 0 b)))
                          (else
                           (let ((o (sr-int-arg (car a))) (b (sr-int-arg (cadr a))))
                             (sr-check-range o b)
                             (sr-bounded-int st o b)))))))
    (cons "nextDouble" (lambda (self . a)
                         (let ((st (jhost-state self)))
                           (cond
                             ((null? a) (sr-next-double st))
                             ((null? (cdr a))
                              (let ((b (inexact (car a))))
                                (unless (and (> b 0.0) (< b +inf.0))
                                  (throw-jvm 'IllegalArgumentException "bound must be finite and positive"))
                                (let ((r (* (sr-next-double st) b)))
                                  (if (>= r b) (sr-next-down b) r))))
                             (else
                              ;; RandomSupport.checkRange + boundedNextDouble: both
                              ;; ends finite, and a span that overflows to Infinity
                              ;; is drawn at half scale and doubled back
                              (let ((o (inexact (car a))) (b (inexact (cadr a))))
                                (unless (and (finite? o) (finite? b) (< o b))
                                  (throw-jvm 'IllegalArgumentException "bound must be greater than origin"))
                                (let* ((d (sr-next-double st))
                                       (r (if (< (- b o) +inf.0)
                                              (+ (* d (- b o)) o)
                                              (let ((ho (* 0.5 o)))
                                                (* (+ (* d (- (* 0.5 b) ho)) ho) 2.0)))))
                                  (if (>= r b) (sr-next-down b) r))))))))
    (cons "nextBoolean" (lambda (self) (< (sr-next-int (jhost-state self)) 0)))))

;; --- java.security.SecureRandom ----------------------------------------------
;; Every draw comes straight from the OS CSPRNG (jolt-random-bytes), so there is
;; no seed and no internal state to carry: unlike java.util.Random above, two
;; instances are not distinguishable and none of this is reproducible. That is
;; the contract — SecureRandom promises unpredictability, and the JVM explicitly
;; does not promise a repeatable stream for a given seed.
(define (sr-uint n)
  (let ((bv (jolt-random-bytes n)))
    (let loop ((i 0) (acc 0))
      (if (fx=? i n) acc (loop (fx+ i 1) (+ (* acc 256) (bytevector-u8-ref bv i)))))))

(define (sr-next-int-bound bound)
  (if (<= bound 0)
      (throw-jvm (quote IllegalArgumentException) "bound must be positive")
      ;; Rejection sampling, not a bare modulo: taking (mod u bound) over a range
      ;; that is not a multiple of bound makes the low residues more likely, which
      ;; is a real bias in something callers reach for to pick tokens and salts.
      (let loop ()
        (let* ((u (bitwise-and (sr-uint 4) #x7fffffff))
               (r (modulo u bound)))
          (if (<= (- u r) (- 2147483648 bound)) (->num r) (loop))))))

;; (SecureRandom. seed) is accepted and the seed ignored: the JVM's seeded ctor
;; SUPPLEMENTS entropy rather than replacing it, so ignoring it cannot make the
;; output weaker than the caller asked for. There is no state either way, so one
;; procedure serves the constructor and both getInstance forms — and both
;; spellings, which share one member table: a fresh closure per spelling
;; re-registers the member with a different value, and JOLT_DEBUG then reports the
;; runtime's own boot as registry drift.
(define (sr-new . args) (make-jhost "securerandom" #f))
(for-each
  (lambda (nm)
    (register-class-ctor! nm sr-new)
    (register-class-statics! nm
      (list (cons "getInstance" sr-new)
            (cons "getInstanceStrong" sr-new))))
  '("SecureRandom" "java.security.SecureRandom"))

(register-host-methods! "securerandom"
  (list
    (cons "nextBytes" (lambda (self ba)
                        (let ((n (ja-len ba)))
                          (ja-bv->bytes! (jolt-random-bytes n) 0 ba 0 n)
                          jolt-nil)))
    (cons "nextInt" (lambda (self . a)
                      (if (pair? a)
                          (sr-next-int-bound (exact (truncate (car a))))
                          (->num (random-u32->s32 (sr-uint 4))))))
    (cons "nextLong" (lambda (self)
                       (let ((u (sr-uint 8)))
                         (->num (if (>= u #x8000000000000000) (- u #x10000000000000000) u)))))
    (cons "nextDouble" (lambda (self)
                         (* (bitwise-and (sr-uint 7) (- (expt 2 53) 1)) (/ 1.0 (expt 2 53)))))
    (cons "nextFloat" (lambda (self)
                        (/ (bitwise-and (sr-uint 3) (- (expt 2 24) 1))
                           (exact->inexact (expt 2 24)))))
    (cons "nextBoolean" (lambda (self) (fx=? 1 (bitwise-and (sr-uint 1) 1))))
    (cons "generateSeed" (lambda (self n)
                           (na-bv->bytearray (jolt-random-bytes (exact (truncate n))))))
    ;; setSeed supplements on the JVM and never replaces; with an OS source there
    ;; is nothing to supplement, so it is a no-op rather than a weakening.
    (cons "setSeed" (lambda (self . _) jolt-nil))))

;; --- java.util.Optional -----------------------------------------------------
;; Returned by getters across java.time / java.net.http (e.g. HttpRequest.timeout,
;; HttpClient.connectTimeout). Value-equal so (= (Optional/of x) (Optional/of x)).
(define (jt-optional present? value) (make-jhost "optional" (vector present? value)))
(define jt-optional-empty (jt-optional #f jolt-nil))
(define-jhost-tag-pred (opt? t) (string=? t "optional"))
(define (opt-present? o) (vector-ref (jhost-state o) 0))
(define (opt-value o) (vector-ref (jhost-state o) 1))
(let ((statics (list (cons "of" (lambda (v) (if (jolt-nil? v) (throw-jvm 'NullPointerException "Optional.of(null)") (jt-optional #t v))))
                     (cons "ofNullable" (lambda (v) (if (jolt-nil? v) jt-optional-empty (jt-optional #t v))))
                     (cons "empty" (lambda _ jt-optional-empty)))))
  (register-class-statics! "Optional" statics)
  (register-class-statics! "java.util.Optional" statics))
(register-host-methods! "optional"
  (list (cons "isPresent" (lambda (o) (opt-present? o)))
        (cons "isEmpty" (lambda (o) (not (opt-present? o))))
        (cons "get" (lambda (o) (if (opt-present? o) (opt-value o) (throw-jvm 'NoSuchElementException "No value present"))))
        (cons "orElse" (lambda (o d) (if (opt-present? o) (opt-value o) d)))
        (cons "orElseGet" (lambda (o f) (if (opt-present? o) (opt-value o) (jolt-fi-call f "get"))))
        (cons "ifPresent" (lambda (o f) (when (opt-present? o) (jolt-fi-call f "accept" (opt-value o))) jolt-nil))
        (cons "equals" (lambda (o x) (jolt=2 o x)))
        (cons "hashCode" (lambda (o) (opt-hashcode o)))
        (cons "toString" (lambda (o) (if (opt-present? o)
                                         (string-append "Optional[" (jolt-str-render-one (opt-value o)) "]")
                                         "Optional.empty")))))
(register-eq-arm! (lambda (a b) (or (opt? a) (opt? b)))
                  (lambda (a b) (and (opt? a) (opt? b) (eq? (opt-present? a) (opt-present? b))
                                     (or (not (opt-present? a)) (objects-equals? (opt-value a) (opt-value b))))))
;; Optional.equals is the values' .equals (1.0 and (float 1.0) differ, where =
;; would call them equal), and Optional.hashCode is its value's hashCode, 0 when
;; empty — the pair agrees, as = and hash have to.
(define (opt-hashcode o) (if (opt-present? o) (i32 (jolt-java-hashcode (opt-value o))) 0))
(register-hash-arm! opt? opt-hashcode)

;; class hierarchy lives in class-hierarchy.ss (jvm-class-parents).
;; fn classes (ns$name) inherit AFunction (handled by jch-direct-supers).

;; (instance? Class e) on a throwable tagged-table carrying a JVM :class matches the
;; carried class or any of its ancestors (full name or last segment), so a library's
;; (catch UnknownHostException e …) / (catch IOException e …) matches the ex-info
;; envelope it threw. Mirrors the (class e) arm (host-table.ss) for catch dispatch,
;; which lowers to (instance? C e). Non-match returns 'pass so other arms still run.
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (and (htable? val) (string? (htable-str-ref val "class" #f)))
        (let* ((cls (htable-str-ref val "class" #f))
               (want (symbol-t-name type-sym))
               (want-seg (hsc-last-segment want)))
          (let loop ((names (cons cls (jch-closure cls))))
            (cond ((null? names) 'pass)
                  ((or (string=? want (car names))
                       (string=? want-seg (hsc-last-segment (car names)))) #t)
                  (else (loop (cdr names))))))
        'pass)))

;; ---- (class x) for host-shim values ------------------------------------------
;; jhost-backed shims report their JVM class instead of falling through to the
;; opaque :object rendering, so class-driven dispatch (and type-classification
;; libraries reading (type x)) see the real name.
;; jhost value class names derive from the single jhost-tag->fqn registry
;; (class-hierarchy.ss) — one row per shim tag, shared with value-host-tags and
;; the instance? arm below so all three stay in agreement.
(register-class-arm!
  (lambda (x) (and (jhost? x) (jhost-fqn (jhost-tag x)) #t))
  (lambda (x) (jhost-fqn (jhost-tag x))))
;; An iterator is a seq walked by hasNext/next, which is what
;; clojure.lang.SeqIterator is; the JVM's per-collection inner classes
;; (PersistentVector$2) have no counterpart in jolt's single representation.
;; Without a row here (class it) leaked the :object taxonomy keyword and
;; (instance? java.util.Iterator it) was false — value-host-tags reads the
;; class arms, so this one row answers class, instance? and protocol dispatch.
(register-class-arm! jiterator? (lambda (x) "clojure.lang.SeqIterator"))
;; sorted collections and transients report their JVM classes. jolt's one
;; transient-map representation reports TransientHashMap (the JVM also has
;; PersistentArrayMap$TransientArrayMap for small maps).
(register-class-arm! htable-sorted-map? (lambda (x) "clojure.lang.PersistentTreeMap"))
(register-class-arm! htable-sorted-set? (lambda (x) "clojure.lang.PersistentTreeSet"))
(register-class-arm!
  (lambda (x) (and (jolt-transient? x) #t))
  (lambda (x)
    (case (jolt-transient-kind x)
      ((vec) "clojure.lang.PersistentVector$TransientVector")
      ;; a transient over an array-mode map carries its insertion order
      ((map) (if (jolt-transient-array-map? x)
                 "clojure.lang.PersistentArrayMap$TransientArrayMap"
                 "clojure.lang.PersistentHashMap$TransientHashMap"))
      ((set) "clojure.lang.PersistentHashSet$TransientHashSet")
      (else "clojure.lang.ATransientCollection"))))
;; instance? for these shims derives from the class graph: the value's class name
;; (jhost-fqn) walked through jch-isa? answers interface questions —
;; (instance? java.util.List an-ArrayList), Deque/Queue/Collection/Iterable chains.
;; Widening only: an unknown pairing passes to the other arms, never denies.
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (and (jhost? val) (symbol-t? type-sym))
        (let ((fqn (jhost-fqn (jhost-tag val))))
          (if fqn
              (let* ((tname (symbol-t-name type-sym))
                     (q (or (resolve-class-hint tname) tname)))
                (if (jch-isa? fqn q) #t 'pass))
              'pass))
        'pass)))
;; count over the mutable collection shims, like RT.count over a java.util
;; Collection/Map on the JVM. Each shim registers a count arm (the hashmap arm
;; is registered with its get/contains arms above); jolt-count walks the arms
;; newest-first for jhost values, so these replace an earlier set!-wrap of
;; jolt-count that duplicated the hashmap arm.
(register-count-arm! al-family? (lambda (c) (al-cnt c)))
(register-count-arm! hs-hashset? (lambda (c) (hashtable-size (hm-tbl c))))
;; contains? on a java.util.Set is Set.contains (RT.contains)
(register-contains-arm! hs-hashset? (lambda (c k) (if (hashtable-contains? (hm-tbl c) k) #t #f)))
;; IEditableCollection / ITransient* answer from the representation: the
;; transient-able persistent collections are editable; a transient reports its
;; kind's interfaces. Widening only — anything else passes to the other arms.
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (symbol-t? type-sym)
        (let* ((tn (symbol-t-name type-sym))
               (short (let loop ((i (- (string-length tn) 1)))
                        (cond ((< i 0) tn)
                              ((char=? (string-ref tn i) #\.) (substring tn (+ i 1) (string-length tn)))
                              (else (loop (- i 1)))))))
          (cond
             ((string=? short "IEditableCollection")
              ;; a MapEntry is pvec-backed but not editable on the JVM
              (if (or (and (pvec? val) (not (jolt-map-entry? val))) (pset? val)
                      (pmap? val))
                 #t 'pass))
            ((string=? short "ITransientCollection")
             (if (jolt-transient? val) #t 'pass))
            ((string=? short "ITransientVector")
             (if (and (jolt-transient? val) (eq? 'vec (jolt-transient-kind val))) #t 'pass))
            ((or (string=? short "ITransientMap") (string=? short "ITransientAssociative"))
             (if (and (jolt-transient? val) (eq? 'map (jolt-transient-kind val))) #t 'pass))
            ((string=? short "ITransientSet")
             (if (and (jolt-transient? val) (eq? 'set (jolt-transient-kind val))) #t 'pass))
            (else 'pass)))
        'pass)))
;; (seq a-HashMap) walks its entrySet, like RT.seqFrom over a java.util.Map: the
;; map's own entries in the map's own order (hm-seq-entries)
(register-seq-arm! hm-hashmap? (lambda (x) (jutil-ff-seq x (hm-seq-entries x))))
;; Which shims are Iterable (seqable?) is the java.util registry's answer
;; (jutil-colls.ss jhost-seqable-shim?): every shim above registers there.
;; a MapEntry does not carry meta on the JVM (AMapEntry); deny IObj/IMeta so the
;; pvec backing doesn't claim it.
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (and (symbol-t? type-sym) (jolt-map-entry? val))
        (let ((tn (symbol-t-name type-sym)))
          (if (or (string=? tn "IObj") (string=? tn "clojure.lang.IObj")
                  (string=? tn "IMeta") (string=? tn "clojure.lang.IMeta"))
              #f 'pass))
        'pass)))
;; a reader-conditional value reports its JVM class, not its tagged-map backing.
(define (reader-conditional-value? x)
  (jolt-reader-conditional-record? x))
(register-class-arm! reader-conditional-value? (lambda (x) "clojure.lang.ReaderConditional"))
;; a multimethod reports its JVM class.
(register-class-arm! (lambda (x) (jolt-multifn? x)) (lambda (x) "clojure.lang.MultiFn"))
;; …and answers its public constructor, MultiFn(String, IFn, Object,
;; IPersistentMap). `defmulti` and `defprotocol` written in jolt never reach it —
;; they expand to defmulti-setup / make-protocol, which build the value directly —
;; but code that constructs a multimethod REFLECTIVELY spells it this way, and
;; that is not a corner case: sci.impl.multimethods/multi-fn-impl is
;; (new clojure.lang.MultiFn name dispatch-fn default hierarchy), and SCI's
;; defprotocol expands to defmulti, so without this constructor no SCI context
;; could define a protocol at all ("No matching ctor found for class
;; clojure.lang.MultiFn"). The hierarchy argument is an IRef on the JVM, deref'd
;; per dispatch (SCI passes #'clojure.core/global-hierarchy); nil means the
;; global hierarchy, which is how the multifn record spells "none of my own".
(let ((multifn-ctor
       (lambda (name dispatch default hierarchy)
         (make-multifn (jolt-str-render-one name) dispatch default
                       (if (jolt-nil? hierarchy) #f hierarchy)))))
  (register-class-ctor! "MultiFn" multifn-ctor)
  (register-class-ctor! "clojure.lang.MultiFn" multifn-ctor))
;; exact-own-class fallback: (instance? C x) is true when C names x's own class —
;; covers checks against a captured (class y) value (transient classes, MultiFn)
;; that no interface arm models. Widening only. The class's ANCESTRY needs no
;; arm of its own: the interface arm above reads value-host-tags, which derives
;; from the class graph for every value whose class arm names a modeled class,
;; so a multimethod is an AFn and a transient is Counted the moment the graph
;; says so. (An ancestry walk here, ahead of the base, was measured at 1.26x on
;; an (instance? IPersistentMap a-vector) miss — the value's class name is the
;; expensive part, and this arm already pays it once.)
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (symbol-t? type-sym)
        (let* ((tn (symbol-t-name type-sym))
               (q (or (resolve-class-hint tn) tn))
               (cn (jolt-class-name val)))
          (if (and (string? cn) (string=? cn q)) #t 'pass))
        'pass)))
;; (instance? Class x) / (instance? java.lang.Class x): a jclass value IS a Class.
(register-instance-check-arm!
  (lambda (type-sym val)
    (let ((tn (symbol-t-name type-sym)))
      (if (member tn '("Class" "java.lang.Class"))
          (if (jclass? val) #t #f)
          'pass))))
;; nth over the java.util List shims, like RT.nth on a java.util.List.
;; This stays a set!-wrap of jolt-nth rather than a registered arm: unlike
;; jolt-count (which has a register-count-arm! registry), jolt-nth is an inline
;; case-lambda with no nth-arm registry, so there is no arm mechanism to move
;; this into. Converting an al-family List to a cseq here and delegating to the
;; base jolt-nth is the only way nth reaches these shims. See the note by
;; register-count-arm! in collections.ss — jolt-nth's migration to an arm
;; registry is deferred until it gets its own bench guard.
;; A StringBuilder rides the same wrap: RT.nth reads a CharSequence by charAt, and
;; delegating its content string reuses the base nth's own bounds reporting.
(define %shim-nth jolt-nth)
(define (shim-nth-target coll)
  (cond ((al-family? coll) (list->cseq (al->list coll)))
        ((sb-jhost? coll) (sb-str* coll))
        (else coll)))
(set! jolt-nth
  (case-lambda
    ((coll i) (%shim-nth (shim-nth-target coll) i))
    ((coll i d) (%shim-nth (shim-nth-target coll) i d))))
(def-var! "clojure.core" "nth" jolt-nth)

;; --- java.nio.charset ---------------------------------------------------------
;; Charset as a jhost "charset" over #(canonical-name encode-max): encode-max is
;; the largest char code the charset encodes (#x10FFFF for UTF-*, i.e. every
;; char — Chez strings are full Unicode). forName is case-insensitive and
;; accepts the JVM's common aliases; unknown names throw
;; UnsupportedCharsetException, malformed ones IllegalCharsetNameException (the
;; JVM's distinction: IllegalCharsetNameException for names that can't be a
;; charset name, UnsupportedCharsetException for well-formed but unknown).
(define (charset-legal-name? s)
  (and (> (string-length s) 0)
       (let loop ((i 0))
         (cond ((= i (string-length s)) #t)
               ((let ((c (string-ref s i)))
                  (or (char-alphabetic? c) (char-numeric? c)
                      (memv c '(#\- #\_ #\. #\:))))
                (loop (+ i 1)))
               (else #f)))))
;; (canonical-name encode-max alias …). The alias lists are the JVM's own —
;; java.nio.charset.Charset/aliases, read off OpenJDK 21 — because that is what
;; libraries pass: clj-http-lite hands (Charset/forName body-encoding) whatever
;; the caller wrote, and "ASCII" is how everyone spells US-ASCII. Matching only
;; the canonical names threw UnsupportedCharsetException on names the JVM
;; resolves. The UTF-32 family is here too; the encode/decode paths below
;; already handled it, but forName rejected it.
(define charset-table
  '(("US-ASCII" 127
     "ASCII" "ascii7" "646" "ANSI_X3.4-1968" "ANSI_X3.4-1986" "IBM367"
     "ISO646-US" "ISO_646.irv:1991" "iso_646.irv:1983" "cp367" "csASCII"
     "iso-ir-6" "us")
    ("ISO-8859-1" 255
     "latin1" "l1" "819" "8859_1" "IBM-819" "IBM819" "ISO8859-1" "ISO8859_1"
     "ISO_8859-1" "ISO_8859-1:1987" "ISO_8859_1" "cp819" "csISOLatin1"
     "iso-ir-100")
    ("UTF-8"    #x10FFFF "UTF8" "unicode-1-1-utf-8")
    ("UTF-16"   #x10FFFF "UTF_16" "UnicodeBig" "unicode" "utf16")
    ("UTF-16BE" #x10FFFF "UTF_16BE" "ISO-10646-UCS-2" "UnicodeBigUnmarked" "X-UTF-16BE")
    ("UTF-16LE" #x10FFFF "UTF_16LE" "UnicodeLittleUnmarked" "X-UTF-16LE")
    ("UTF-32"   #x10FFFF "UTF32" "UTF_32")
    ("UTF-32BE" #x10FFFF "UTF_32BE" "X-UTF-32BE")
    ("UTF-32LE" #x10FFFF "UTF_32LE" "X-UTF-32LE")))

(define (charset-lookup s)
  (let loop ((t charset-table))
    (cond ((null? t) #f)
          ;; the canonical name and every alias, case-insensitively (the JVM's
          ;; forName is case-insensitive over both).
          ((let names ((ns (cons (caar t) (cddar t))))
             (cond ((null? ns) #f)
                   ((string-ci=? s (car ns)) #t)
                   (else (names (cdr ns)))))
           (cons (caar t) (cadar t)))
          (else (loop (cdr t))))))

;; A charset name (canonical or alias) lowercased to its CANONICAL form, so the
;; encode/decode dispatches below match one spelling each instead of carrying
;; their own partial alias lists — which is how (.getBytes s "l1") used to
;; silently produce UTF-8 bytes. An unknown name passes through unchanged and
;; still hits each dispatch's UTF-8 fallback.
(define (charset-canonical-down s)
  (let ((hit (charset-lookup s)))
    (ascii-string-down (if hit (car hit) s))))
(define (charset-for-name s)
  (let ((s (jolt-str-render-one s)))
    (if (not (charset-legal-name? s))
        (throw-jvm 'java.nio.charset.IllegalCharsetNameException s)
        (let ((hit (charset-lookup s)))
          (cond
            (hit (make-jhost "charset" (vector (car hit) (cdr hit))))
            ;; the table lists the charsets Chez encodes natively; the rest come
            ;; from the system iconv, so forName must accept whatever encoding
            ;; through it will accept or the two disagree about the same name.
            ((iconv-known? s) (make-jhost "charset" (vector s #x10FFFF)))
            (else (throw-jvm 'java.nio.charset.UnsupportedCharsetException s)))))))
(define (charset-name c) (vector-ref (jhost-state c) 0))
(define (charset-encode-max c) (vector-ref (jhost-state c) 1))
;; One charset ARGUMENT — a name string or a Charset object — as its name string.
;; A Charset jhost does not render to its name (jolt-str-render-one gives
;; "#object[java.nio.charset.Charset]"), so encoding through an object used to
;; match no arm and fall through to the UTF-8 default: (.getBytes "õ" (Charset/
;; forName "ISO-8859-1")) returned the UTF-8 bytes [195 181] instead of [245].
;; ASCII-only text and an explicit UTF-8 request both hid it.
(define (charset-arg-name c)
  (cond ((string? c) c)
        ((and (jhost? c) (string=? (jhost-tag c) "charset")) (charset-name c))
        (else (jolt-str-render-one c))))
(register-class-statics! "java.nio.charset.Charset"
  (list (cons "forName" charset-for-name)
        (cons "defaultCharset" (lambda () (make-jhost "charset" (vector "UTF-8" #x10FFFF))))
        (cons "isSupported" (lambda (s) (if (charset-lookup (jolt-str-render-one s)) #t #f)))))
(register-host-methods! "charset"
  (list (cons "name" (lambda (c) (charset-name c)))
        (cons "displayName" (lambda (c) (charset-name c)))
        (cons "toString" (lambda (c) (charset-name c)))
        (cons "newEncoder" (lambda (c) (make-jhost "charset-encoder" (vector c))))
        ;; #(charset malformed-action unmappable-action replacement) — the
        ;; JVM's defaults are REPORT for both actions and U+FFFD for the
        ;; replacement. The methods that read these live in charset-coding.ss,
        ;; which loads after this file (it needs ByteBuffer).
        (cons "newDecoder" (lambda (c) (make-jhost "charset-decoder" (vector c #f #f "\xFFFD;"))))
        (cons "canEncode" (lambda (c) #t))
        (cons "equals" (lambda (c o) (and (jhost? o) (string=? (jhost-tag o) "charset")
                                          (string=? (charset-name c) (charset-name o)))))
        (cons "hashCode" (lambda (c) (java-string-hashcode (charset-name c))))))
;; CharsetEncoder.canEncode: over a string, every char must fit; over a char the
;; char itself. A char is a Scheme char in jolt; a string a string.
(register-host-methods! "charset-encoder"
  (list (cons "canEncode"
              (lambda (e x)
                (let ((lim (charset-encode-max (vector-ref (jhost-state e) 0))))
                  (cond ((char? x) (if (<= (char->integer x) lim) #t #f))
                        ((string? x) (let loop ((i 0))
                                       (cond ((= i (string-length x)) #t)
                                             ((> (char->integer (string-ref x i)) lim) #f)
                                             (else (loop (+ i 1))))))
                        (else #f)))))
        (cons "charset" (lambda (e) (vector-ref (jhost-state e) 0)))))
(register-host-methods! "charset-decoder"
  (list (cons "charset" (lambda (d) (vector-ref (jhost-state d) 0)))))
(register-class-arm! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "charset"))) (lambda (x) "java.nio.charset.Charset"))
;; Charset equality, hash and order go by canonical name, as Charset.equals,
;; hashCode and compareTo (case-insensitive) do.
(define-jhost-tag-pred (charset-jhost? t) (string=? t "charset"))
(register-eq-arm! (lambda (a b) (or (charset-jhost? a) (charset-jhost? b)))
                  (lambda (a b) (and (charset-jhost? a) (charset-jhost? b)
                                     (string=? (charset-name a) (charset-name b)))))
(register-hash-arm! charset-jhost? (lambda (c) (java-string-hashcode (charset-name c))))
(register-compare-arm! (lambda (a b) (and (charset-jhost? a) (charset-jhost? b)))
                       (lambda (a b) (jvm-string-ci-compare (charset-name a) (charset-name b))))
(register-class-arm! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "charset-encoder"))) (lambda (x) "java.nio.charset.CharsetEncoder"))
(register-class-arm! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "charset-decoder"))) (lambda (x) "java.nio.charset.CharsetDecoder"))

;; --- resolve/ns-resolve return the Class for a class mapping ------------------
;; A JVM ns mapping is a var or a Class, and resolve hands back whichever the
;; symbol names. jolt's base resolve (ns.ss) is var-only, so it re-registers here
;; with two class-aware layers on top:
;;   - a resolved var that IS the class model's registration for its own name —
;;     a class-token var (String, java.lang.Long) or a deftype/defrecord name var
;;     holding its ctor — resolves through to the class value. A user var that
;;     merely holds a class ((def MyCls String)) keeps resolving to the var,
;;     which is what the JVM's def would produce. The name must match the class
;;     (FQN or simple), so ->Rec ctor vars stay vars.
;;   - an unresolved symbol classifies as a class the same way, through the same
;;     jolt-class-for interner, so (= (resolve 'X) X) holds for every class form
;;     the asking namespace maps: dotted names (java.util.Map) and the java.lang
;;     auto-imports.
;;
;; Both layers answer the JVM's question, which is per-NAMESPACE: "does this ns
;; map this name to a class", not "does this class exist". The distinction is not
;; academic — jolt keeps a class-token var in clojure.core for EVERY class it
;; models (above) so a bare `Pattern` self-evaluates, and reading those as
;; mappings made (resolve 'Path) answer in a namespace that never imported
;; java.nio.file.Path. typedclojure's (u/def-object Path ...) guards on exactly
;; that resolve before emitting its deftype, so the guard fired, the deftype was
;; skipped, and the Path-maker it also emits never existed (jolt-17w2).
(define (rsv-registration-class-name c)
  (let ((root (var-cell-root c)))
    (cond ((jclass? root) (jclass-name root))
          ;; a deftype/defrecord NAME var holds its ctor, which carries the tag
          ((procedure? root) (deftype-ctor-tag root))
          (else #f))))
(define (rsv-registration-class c)
  (let ((cn (rsv-registration-class-name c)))
    (and cn
         (let ((nm (var-cell-name c)))
           (and (or (string=? nm cn) (string=? nm (hsc-last-segment cn)))
                (var-cell-root c))))))
;; Is that registration a mapping the asking namespace HAS? jolt holds a ns's own
;; class mappings as cells in that ns — (:import ...) binds the short name there
;; (natives-str.ss), deftype/defrecord binds its own name there (protocols.ss) —
;; so a cell found in the asking ns is one by construction. Beyond that only the
;; two kinds the JVM makes visible everywhere answer: a fully-qualified name, and
;; the java.lang auto-imports. The clojure.core class tokens for everything else
;; are the class MODEL and stay invisible to resolve.
(define (rsv-mapping-visible? c ns)
  (let ((nm (var-cell-name c)))
    (or (string=? (var-cell-ns c) ns)
        (hc-fq-class-name? nm)
        (and (jolt-default-import-fqn nm) #t))))
(define (rsv-class-for-name nm)
  ;; Only a class the host actually MODELS answers, and the syntactic dotted-name
  ;; shape is deliberately not enough: resolve is how tooling feature-detects a
  ;; class (compliment gates its JDK9 module scanner on resolving
  ;; java.lang.module.ModuleFinder), and a token for a class jolt cannot back
  ;; sends such code down paths that then die on the missing pieces — the
  ;; instance? macro reads a class answer here as "the symbol evaluates to that
  ;; class" and emits it unquoted, so answering for an unbacked java.lang name
  ;; (ExceptionInInitializerError, which nothing registers) turns fipp's
  ;; (catch ExceptionInInitializerError _) into an unresolved symbol.
  ;; A symbol in CODE keeps the wider syntactic model (hc-resolve-global).
  ;; A bare name needs no arm here: every one a namespace maps and jolt models is
  ;; a cell — an :import, a deftype, or the clojure.core token the class model
  ;; registers for each auto-import it knows — so jolt-resolve already found it.
  ;; jch-known-exact?, not jch-known?: the latter falls back to the last dotted
  ;; segment, so fake.pkg.String answered java.lang.String's registration and
  ;; resolve handed back a token for a class that exists nowhere — the opposite
  ;; of the feature-detection answer this is here to give.
  (and (hc-fq-class-name? nm)
       (let ((c (if (or (jch-known-exact? nm) (host-class-registered? nm))
                    nm
                    ;; the JVM spelling of a deftype in a dashed namespace
                    ;; (rf.def_two.R3), what a library builds from
                    ;; (namespace-munge *ns*) — the same class, one token
                    (jch-registered-name nm))))
         (and c (jolt-class-for c)))))
(define (rsv-through v sym ns)
  (cond ((jolt-nil? v)
         (or (and (symbol-t? sym) (not (string? (symbol-t-ns sym)))
                  (rsv-class-for-name (symbol-t-name sym)))
             jolt-nil))
        ;; a class registration the ns does not map is NOT the var either — the
        ;; JVM has no such var to hand back, so the answer is nil.
        ((rsv-registration-class v)
         => (lambda (root) (if (rsv-mapping-visible? v ns) root jolt-nil)))
        (else v)))
;; An array class symbol — Component/N, N one digit 1-9 (Clojure 1.12) — resolves
;; to that array's Class when the component does, and raises the reference's
;; ClassNotFoundException "Unable to resolve component classname" when it does not. #f for any other
;; symbol. The component is a primitive name or whatever class the bare symbol
;; resolves to in the namespace.
(define rsv-array-prim-letters
  '(("boolean" . "Z") ("byte" . "B") ("char" . "C") ("short" . "S") ("int" . "I")
    ("long" . "J") ("float" . "F") ("double" . "D")))
(define (rsv-array-class sym resolve-component)
  (and (symbol-t? sym) (string? (symbol-t-ns sym))
       (let ((nm (symbol-t-name sym)))
         (and (fx=? (string-length nm) 1) (char<=? #\1 (string-ref nm 0) #\9)))
       (let* ((cn (symbol-t-ns sym))
              (dims (fx- (char->integer (string-ref (symbol-t-name sym) 0)) (char->integer #\0)))
              (prefix (make-string dims #\[))
              (letter (assoc cn rsv-array-prim-letters)))
         (if letter
             (jolt-class-for (string-append prefix (cdr letter)))
             (let ((c (resolve-component (jolt-symbol #f cn))))
               (if (jclass? c)
                   (jolt-class-for (string-append prefix "L" (jclass-name c) ";"))
                   (throw-jvm 'ClassNotFoundException
                              (string-append "Unable to resolve component classname: " cn))))))))
(def-var! "clojure.core" "resolve"
  (case-lambda
    ((sym) (or (rsv-array-class sym (lambda (s) (rsv-through (jolt-resolve s) s (chez-current-ns))))
               (rsv-through (jolt-resolve sym) sym (chez-current-ns))))
    ;; the &env arity: a local named sym answers nil, never a class
    ((env sym) (if (and (pmap? env) (pmap-contains? env sym))
                   jolt-nil
                   (rsv-through (jolt-resolve env sym) sym (chez-current-ns))))))
(def-var! "clojure.core" "ns-resolve"
  (case-lambda
    ((ns-desig sym)
     (rsv-through (jolt-ns-resolve ns-desig sym) sym
                  (jns-name (jolt-the-ns ns-desig))))
    ;; the &env arity, as on resolve: a local named sym answers nil
    ((ns-desig env sym)
     (if (and (pmap? env) (pmap-contains? env sym))
         jolt-nil
         (rsv-through (jolt-ns-resolve ns-desig sym) sym
                      (jns-name (jolt-the-ns ns-desig)))))))

;; --- ns-imports reports the namespace's own class mappings --------------------
;; A JVM namespace maps class names as well as vars: the java.lang auto-imports
;; everywhere, an (:import ...) into the ns that asked for it, a deftype/defrecord
;; into the ns that defines it. ns.ss returns the auto-imports alone because the
;; class model does not exist yet where it is defined; this reads the other two
;; back off the namespace itself rather than keeping a second table that could
;; drift out of step with the cells resolve answers from.
(define hsc-default-imports
  (pmap-fold jolt-default-imports
             (lambda (k v acc) (jolt-assoc acc k (jolt-class-for v)))
             (jolt-hash-map)))
(define (hsc-ns-import-class c)
  ;; the cell is the ns's mapping FOR a class only when it is bound under that
  ;; class's own simple name, so Foo counts and ->Foo / map->Foo do not.
  (and (var-cell-defined? c)
       (let ((cn (rsv-registration-class-name c)))
         (and cn (string=? (var-cell-name c) (hsc-last-segment cn))
              (jolt-class-for cn)))))
(define (hsc-ns-imports . desig)
  (let ((cns (if (pair? desig) (ns-desig->name (car desig)) (chez-current-ns))))
    (fold-left (lambda (m c)
                 (let ((cls (hsc-ns-import-class c)))
                   (if cls (jolt-assoc m (jolt-symbol #f (var-cell-name c)) cls) m)))
               hsc-default-imports
               (ns-cells-list cns))))
(set! jolt-ns-imports hsc-ns-imports)   ; so ns-map (ns.ss) sees them too
(def-var! "clojure.core" "ns-imports" hsc-ns-imports)
;; ...and the other half of that: a cell that IS a class mapping is an import, so
;; ns-interns / ns-publics / the refers built from clojure.core's publics must not
;; report it as a var. Without this the class tokens above are public vars of
;; clojure.core, every namespace refers them, and the refer shadows the very
;; ns-imports entry this section just built ((ns-map 'user) answered the token var
;; for String where the JVM answers the class).
(set! ns-cell-class-mapping? (lambda (c) (and (hsc-ns-import-class c) #t)))
