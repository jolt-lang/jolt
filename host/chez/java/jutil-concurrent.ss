;; jutil-concurrent.ss — java.util's legacy synchronized collections and the
;; java.util.concurrent ones (jolt-urj36): Vector, Stack, Hashtable, BitSet,
;; PriorityQueue, ConcurrentLinkedQueue, ConcurrentLinkedDeque,
;; CopyOnWriteArrayList, CopyOnWriteArraySet, ConcurrentSkipListMap and
;; ConcurrentSkipListSet. Each has its JDK 21 public API and class, the
;; iterator its JDK class answers (removing where the JDK's does), printing,
;; equality and hashing through the java.util registry, Clojure's seq / count /
;; get / contains? / nth where the JVM gives them, and a jolt.image codec
;; (image-codecs.ss).
;;
;; Thread safety, by class. Every lock here is a MONITOR, not a mutex: their
;; methods run user code (equals, hashCode, a comparator, a compute function),
;; which may park a fiber, and a fiber may hold a monitor across a park where a
;; held mutex would wedge its carrier (host/chez/locks.ss).
;;   - Vector, Stack and Hashtable hold their OWN monitor for every method, as
;;     their methods are synchronized on themselves, so (locking v …) around a
;;     walk excludes them as on the JVM; their iterators take it per step.
;;   - ConcurrentLinkedQueue / Deque hold a private monitor for every change and
;;     every step of an iterator; an iterator is weakly consistent: it walks the
;;     live node chain, skipping what was removed, and a node it stands on keeps
;;     its link onward after removal.
;;   - CopyOnWriteArrayList / Set and ConcurrentSkipListMap / Set are copy on
;;     write: a reader takes the published (immutable) array or tree with no
;;     lock; a writer, under a private monitor, builds the new one and publishes
;;     it with one store after a release fence. tools/cow-publish-model.pl checks
;;     that protocol (make cowpublishmodel).
;;   - PriorityQueue and BitSet are not thread-safe on the JVM either.
;;
;; Tags start "jx-", so they register through jutil-views.ss's jx table.
;; Loaded after jutil-views.ss and tree-map.ss.

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

;; ======================================================================================
;; Vector and Stack
;; ======================================================================================
;; State #(arraylist monitor capacity increment). The elements live in an ArrayList
;; of their own (host-static-classes.ss), which every method reaches under the
;; monitor, so Vector is ArrayList's representation and behavior with Vector's
;; lock, legacy methods and capacity accounting on top. capacity() follows the
;; JDK's growth rule (double, or grow by capacityIncrement), since programs
;; print it.
(define (vec-al v) (vector-ref (jhost-state v) 0))
(define (vec-mu v) (vector-ref (jhost-state v) 1))
(define (vec-cap v) (vector-ref (jhost-state v) 2))
(define (vec-cap! v c) (vector-set! (jhost-state v) 2 c))
(define (vec-incr v) (vector-ref (jhost-state v) 3))
(define (vec-size v) (al-cnt (vec-al v)))
;; the vector's own monitor, as Vector's methods are synchronized on it
(define (jx-own-monitor! h i) (vector-set! (jhost-state h) i (object-monitor h)) h)
(define (vec-list v) (jolt-call-with-monitor (vec-mu v) (lambda () (al->list (vec-al v)))))
;; slot 4 is modCount: every structural change bumps it, and an iterator that
;; sees it move throws ConcurrentModificationException, as Vector$Itr does
(define (make-jx-vector tag xs cap incr) (jx-own-monitor! (make-jhost tag (vector (make-arraylist xs) #f cap incr 0)) 1))
(define (vec-mod v) (vector-ref (jhost-state v) 4))
(define (vec-mod! v) (vector-set! (jhost-state v) 4 (fxand (fx+ 1 (vec-mod v)) #x3fffffff)))
(define vec-structural '("add" "addAll" "remove" "clear" "replaceAll" "sort" "removeFirst" "removeLast"))
;; a change only when they remove something, as Vector's bulkRemove counts one
(define vec-structural-if-changed '("removeAll" "retainAll" "removeIf"))
(define (vec-grow! v min)
  (let ((old (vec-cap v)))
    (when (> min old)
      (vec-cap! v (+ old (max (- min old) (if (> (vec-incr v) 0) (vec-incr v) old)))))))
(define (vec-call v m . args)
  (when (member m vec-structural) (vec-mod! v))
  (let ((r (jx-call (vec-al v) m args)))
    (when (and (member m vec-structural-if-changed) (jolt-truthy? r)) (vec-mod! v))
    r))
(define-syntax with-vec
  (syntax-rules () ((_ v e ...) (jolt-call-with-monitor (vec-mu v) (lambda () e ...)))))
;; the ArrayList members a Vector answers as they are, under its lock
(define vec-plain
  '(("size" 0) ("isEmpty" 0) ("contains" 1) ("indexOf" 1) ("lastIndexOf" 1) ("toArray" 0 1)
    ("clear" 0) ("containsAll" 1) ("removeAll" 1) ("retainAll" 1) ("removeIf" 1)
    ("forEach" 1) ("replaceAll" 1) ("sort" 1) ("getFirst" 0) ("getLast" 0) ("removeFirst" 0) ("removeLast" 0)))
(define (vec-index-from xs x from step)
  ;; indexOf(o, index) / lastIndexOf(o, index): from INDEX in direction STEP
  (let ((v (list->vector xs)))
    (let loop ((i from))
      (cond ((or (fx<? i 0) (fx>=? i (vector-length v))) -1)
            ((objects-equals? x (vector-ref v i)) i)
            (else (loop (fx+ i step)))))))
(define (vec-index! v i)
  (cond ((fx>=? i (vec-size v)) (throw-jvm 'ArrayIndexOutOfBoundsException (string-append "Array index out of range: " (number->string i))))
        ((fx<? i 0) (throw-jvm 'ArrayIndexOutOfBoundsException
                               (string-append "Index " (number->string i) " out of bounds for length " (number->string (vec-cap v)))))))
(define (vec-enumeration v) (make-jhost "jx-vector-enum" (vector v 0)))
(define vector-methods
  (append
    (map (lambda (m)
           (cons (car m)
                 (host-arity-of (cdr m) #t
                   (lambda (self . args) (with-vec self (apply vec-call self (car m) args))))))
         vec-plain)
    (list
      (cons "add" (case-lambda
                    ((self x) (with-vec self (vec-grow! self (fx+ 1 (vec-size self))) (vec-call self "add" x)))
                    ((self i x) (with-vec self
                                  (let ((i (jnum->exact i)))
                                    (unless (and (fx>=? i 0) (fx<=? i (vec-size self)))
                                      (throw-jvm 'ArrayIndexOutOfBoundsException
                                                 (string-append (number->string i) " > " (number->string (vec-size self)))))
                                    (vec-grow! self (fx+ 1 (vec-size self)))
                                    (vec-call self "add" (->num i) x))))))
      (cons "addAll" (case-lambda
                       ((self c) (let ((xs (jxc-elems c)))
                                   (with-vec self (vec-grow! self (+ (vec-size self) (length xs)))
                                     (vec-call self "addAll" (list->cseq xs)))))
                       ((self i c) (let ((xs (jxc-elems c)))
                                     (with-vec self (vec-grow! self (+ (vec-size self) (length xs)))
                                       (vec-call self "addAll" i (list->cseq xs)))))))
      (cons "addFirst" (lambda (self x) (with-vec self (vec-grow! self (fx+ 1 (vec-size self))) (vec-call self "add" (->num 0) x)) jolt-nil))
      (cons "addLast" (lambda (self x) (with-vec self (vec-grow! self (fx+ 1 (vec-size self))) (vec-call self "add" x)) jolt-nil))
      (cons "addElement" (lambda (self x) (with-vec self (vec-grow! self (fx+ 1 (vec-size self))) (vec-call self "add" x)) jolt-nil))
      (cons "insertElementAt" (lambda (self x i)
                                (with-vec self
                                  (let ((i (jnum->exact i)))
                                    (unless (and (fx>=? i 0) (fx<=? i (vec-size self)))
                                      (throw-jvm 'ArrayIndexOutOfBoundsException
                                                 (string-append (number->string i) " > " (number->string (vec-size self)))))
                                    (vec-grow! self (fx+ 1 (vec-size self)))
                                    (vec-call self "add" (->num i) x)
                                    jolt-nil))))
      ;; get / set / remove(int) check the index themselves: past the end is
      ;; Vector's "Array index out of range", a negative one the array's own
      (cons "get" (lambda (self i) (with-vec self (let ((i (jnum->exact i))) (vec-index! self i) (al-ref (vec-al self) i)))))
      (cons "set" (lambda (self i x) (with-vec self (let ((i (jnum->exact i))) (vec-index! self i)
                                                      (let ((old (al-ref (vec-al self) i))) (al-set! (vec-al self) i x) old)))))
      (cons "remove" (lambda (self x)
                       (with-vec self
                         (if (and (fixnum? x) (not (jfloat? x)))
                             (begin (vec-index! self x) (vec-mod! self)
                                    (let ((old (al-ref (vec-al self) x))) (al-remove-at! (vec-al self) x) old))
                             ;; remove(Object) is removeElement, which counts a change found or not
                             (let ((i (vec-index-from (al->list (vec-al self)) x 0 1)))
                               (vec-mod! self)
                               (if (fx<? i 0) #f (begin (al-remove-at! (vec-al self) i) #t)))))))
      (cons "elementAt" (lambda (self i)
                          (with-vec self
                            (let ((i (jnum->exact i)))
                              (unless (fx<? i (vec-size self))
                                (throw-jvm 'ArrayIndexOutOfBoundsException
                                           (string-append (number->string i) " >= " (number->string (vec-size self)))))
                              (al-ref (vec-al self) i)))))
      (cons "firstElement" (lambda (self) (with-vec self (if (fx=? 0 (vec-size self)) (throw-jvm 'NoSuchElementException jolt-nil)
                                                             (al-ref (vec-al self) 0)))))
      (cons "lastElement" (lambda (self) (with-vec self (if (fx=? 0 (vec-size self)) (throw-jvm 'NoSuchElementException jolt-nil)
                                                            (al-ref (vec-al self) (fx- (vec-size self) 1))))))
      (cons "setElementAt" (lambda (self x i)
                             (with-vec self
                               (let ((i (jnum->exact i)))
                                 (unless (fx<? i (vec-size self))
                                   (throw-jvm 'ArrayIndexOutOfBoundsException
                                              (string-append (number->string i) " >= " (number->string (vec-size self)))))
                                 (al-set! (vec-al self) i x) jolt-nil))))
      (cons "removeElementAt" (lambda (self i)
                                (with-vec self
                                  (let ((i (jnum->exact i)))
                                    (cond ((fx>=? i (vec-size self))
                                           (throw-jvm 'ArrayIndexOutOfBoundsException
                                                      (string-append (number->string i) " >= " (number->string (vec-size self)))))
                                          ((fx<? i 0) (throw-jvm 'ArrayIndexOutOfBoundsException (number->string i))))
                                    (vec-mod! self) (al-remove-at! (vec-al self) i) jolt-nil))))
      (cons "removeElement" (lambda (self x)
                              (with-vec self
                                (let ((i (vec-index-from (al->list (vec-al self)) x 0 1)))
                                  (vec-mod! self)
                                  (if (fx<? i 0) #f (begin (al-remove-at! (vec-al self) i) #t))))))
      (cons "removeAllElements" (lambda (self) (with-vec self (vec-call self "clear")) jolt-nil))
      (cons "indexOf" (case-lambda
                        ((self x) (with-vec self (->num (vec-index-from (al->list (vec-al self)) x 0 1))))
                        ((self x i) (with-vec self
                                      (let ((i (jnum->exact i)))
                                        (when (fx<? i 0) (throw-jvm 'IndexOutOfBoundsException
                                                                    (string-append "Index " (number->string i) " out of bounds for length " (number->string (vec-size self)))))
                                        (->num (vec-index-from (al->list (vec-al self)) x i 1)))))))
      (cons "lastIndexOf" (case-lambda
                            ((self x) (with-vec self (->num (vec-index-from (al->list (vec-al self)) x (fx- (vec-size self) 1) -1))))
                            ((self x i) (with-vec self
                                          (let ((i (jnum->exact i)))
                                            (when (fx>=? i (vec-size self))
                                              (throw-jvm 'IndexOutOfBoundsException
                                                         (string-append (number->string i) " >= " (number->string (vec-size self)))))
                                            (->num (vec-index-from (al->list (vec-al self)) x i -1)))))))
      (cons "setSize" (lambda (self n)
                        (with-vec self
                          (let ((n (jnum->exact n)))
                            (when (fx<? n 0) (throw-jvm 'ArrayIndexOutOfBoundsException (number->string n)))
                            (vec-grow! self n)
                            (vec-mod! self)
                            (let loop () (when (fx>? (vec-size self) n) (al-remove-at! (vec-al self) (fx- (vec-size self) 1)) (loop)))
                            (let loop () (when (fx<? (vec-size self) n) (al-push! (vec-al self) jolt-nil) (loop)))
                            jolt-nil))))
      (cons "capacity" (lambda (self) (with-vec self (->num (vec-cap self)))))
      ;; both count a structural change as the JDK's do: trimToSize always,
      ;; ensureCapacity for any positive request, whether or not it grows
      (cons "ensureCapacity" (lambda (self n)
                               (with-vec self
                                 (when (> (jnum->exact n) 0) (vec-mod! self) (vec-grow! self (jnum->exact n))))
                               jolt-nil))
      (cons "trimToSize" (lambda (self) (with-vec self (vec-mod! self) (vec-cap! self (vec-size self))) jolt-nil))
      (cons "copyInto" (lambda (self arr)
                         (with-vec self
                           (let loop ((xs (al->list (vec-al self))) (i 0))
                             (unless (null? xs)
                               (when (fx>=? i (ja-len arr)) (throw-jvm 'ArrayIndexOutOfBoundsException
                                                                       (string-append "Index " (number->string i) " out of bounds for length " (number->string (ja-len arr)))))
                               (ja-set! arr i (car xs)) (loop (cdr xs) (fx+ i 1))))
                           jolt-nil)))
      (cons "elements" (lambda (self) (vec-enumeration self)))
      (cons "iterator" (lambda (self) (with-vec self (make-jhost "jx-vector-itr" (vector self 0 -1 (vec-mod self))))))
      (cons "listIterator" (case-lambda
                             ((self) (make-cd-list-iterator self 0))
                             ((self i) (let ((i (jnum->exact i)))
                                         (unless (and (fx>=? i 0) (fx<=? i (vec-size self)))
                                           (throw-jvm 'IndexOutOfBoundsException (string-append "Index: " (number->string i))))
                                         (make-cd-list-iterator self i)))))
      (cons "subList" (lambda (self from to)
                        (with-vec self (jx-wrap-sync 'ralist (make-cd-sublist self (jnum->exact from) (jnum->exact to)) (vec-mu self)))))
      (cons "reversed" (lambda (self) (make-jhost "jx-reverse-list" (vector self))))
      (cons "clone" (lambda (self)
                      (with-vec self (make-jx-vector (jhost-tag self) (al->list (vec-al self)) (vec-cap self) (vec-incr self))))))))
(define stack-methods
  (list
    (cons "push" (lambda (self x) (with-vec self (vec-grow! self (fx+ 1 (vec-size self))) (vec-call self "add" x)) x))
    (cons "pop" (lambda (self)
                  (with-vec self
                    (when (fx=? 0 (vec-size self)) (throw-jvm 'EmptyStackException jolt-nil))
                    (let ((x (al-ref (vec-al self) (fx- (vec-size self) 1))))
                      (vec-mod! self) (al-remove-at! (vec-al self) (fx- (vec-size self) 1)) x))))
    (cons "peek" (lambda (self)
                   (with-vec self
                     (when (fx=? 0 (vec-size self)) (throw-jvm 'EmptyStackException jolt-nil))
                     (al-ref (vec-al self) (fx- (vec-size self) 1)))))
    (cons "empty" (lambda (self) (with-vec self (fx=? 0 (vec-size self)))))
    (cons "search" (lambda (self x)
                     (with-vec self
                       (let ((i (vec-index-from (al->list (vec-al self)) x (fx- (vec-size self) 1) -1)))
                         (->num (if (fx<? i 0) -1 (fx- (vec-size self) i)))))))))
(jx-class! "jx-vector" "java.util.Vector" '("java.util.AbstractList" "java.util.List" "java.util.RandomAccess" "java.lang.Cloneable" "java.io.Serializable"))
(jx-class! "jx-stack" "java.util.Stack" '("java.util.Vector"))
(register-host-methods! "jx-vector" vector-methods)
(register-modcount! "jx-vector" vec-mod)
(register-modcount! "jx-stack" vec-mod)
(register-host-methods! "jx-stack" (append vector-methods stack-methods))
(for-each (lambda (t) (jx-register! t 'ralist vec-list (lambda (v) (with-vec v (vec-size v))) #f #f))
          '("jx-vector" "jx-stack"))
(jch-register-supers! "java.util.EmptyStackException" '("java.lang.RuntimeException"))
(define (vec-ctor tag)
  (case-lambda
    (() (make-jx-vector tag '() 10 0))
    ((x) (cond ((and (number? x) (< (jnum->exact x) 0))
                (throw-jvm 'IllegalArgumentException (string-append "Illegal Capacity: " (number->string (jnum->exact x)))))
               ((number? x) (make-jx-vector tag '() (jnum->exact x) 0))
               (else (let ((xs (jxc-elems x))) (make-jx-vector tag xs (length xs) 0)))))
    ((c i) (when (< (jnum->exact c) 0)
             (throw-jvm 'IllegalArgumentException (string-append "Illegal Capacity: " (number->string (jnum->exact c)))))
           (make-jx-vector tag '() (jnum->exact c) (jnum->exact i)))))
(register-class-ctor! "java.util.Vector" (vec-ctor "jx-vector"))
(register-class-ctor! "Vector" (lookup-class class-ctors-tbl "java.util.Vector"))
(register-class-ctor! "java.util.Stack" (lambda () (make-jx-vector "jx-stack" '() 10 0)))
(register-class-ctor! "Stack" (lookup-class class-ctors-tbl "java.util.Stack"))
;; Vector$Itr, as the JDK writes it: a cursor, the last index handed out, and the
;; modCount it expects, each step under the vector's monitor.
;; State #(vector cursor last-returned expected-modcount)
(define (vit-vec it) (vector-ref (jhost-state it) 0))
(define (vit-ref it i) (vector-ref (jhost-state it) i))
(define (vit-set! it i x) (vector-set! (jhost-state it) i x))
(define (vit-comod! it) (unless (fx=? (vit-ref it 3) (vec-mod (vit-vec it))) (throw-jvm 'ConcurrentModificationException jolt-nil)))
(define (vit-next! it)
  (let ((v (vit-vec it)))
    (with-vec v
      (vit-comod! it)
      (let ((i (vit-ref it 1)))
        (unless (fx<? i (vec-size v)) (throw-jvm 'NoSuchElementException jolt-nil))
        (vit-set! it 1 (fx+ i 1))
        (vit-set! it 2 i)
        (al-ref (vec-al v) i)))))
(jx-class! "jx-vector-itr" "java.util.Vector$Itr" '("java.util.Iterator"))
(register-host-methods! "jx-vector-itr"
  (list (cons "hasNext" (lambda (self) (not (fx=? (vit-ref self 1) (vec-size (vit-vec self))))))
        (cons "next" (lambda (self) (vit-next! self)))
        (cons "remove" (lambda (self)
                         (when (fx=? -1 (vit-ref self 2)) (throw-jvm 'IllegalStateException jolt-nil))
                         (let ((v (vit-vec self)))
                           (with-vec v
                             (vit-comod! self)
                             (vec-mod! v)
                             (al-remove-at! (vec-al v) (vit-ref self 2))
                             (vit-set! self 3 (vec-mod v))
                             (vit-set! self 1 (vit-ref self 2))
                             (vit-set! self 2 -1)))
                         jolt-nil))
        (cons "forEachRemaining" (lambda (self f)
                                   (let loop () (when (fx<? (vit-ref self 1) (vec-size (vit-vec self)))
                                                  (jolt-fi-call f "accept" (vit-next! self)) (loop)))
                                   jolt-nil))))
(register-seq-arm-last! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "jx-vector-itr")))
  (lambda (it) (let ((xs (list-tail (vec-list (vit-vec it)) (fxmin (vit-ref it 1) (vec-size (vit-vec it))))))
                 (if (null? xs) jolt-nil (list->cseq xs)))))
;; elements(): Vector$1, an Enumeration over the live vector
(jx-class! "jx-vector-enum" "java.util.Vector$1" '("java.util.Enumeration"))
(register-host-methods! "jx-vector-enum"
  (list (cons "hasMoreElements" (lambda (self) (let ((v (vit-vec self))) (with-vec v (fx<? (vector-ref (jhost-state self) 1) (vec-size v))))))
        (cons "nextElement" (lambda (self)
                              (let ((v (vit-vec self)) (i (vector-ref (jhost-state self) 1)))
                                (with-vec v
                                  (unless (fx<? i (vec-size v)) (throw-jvm 'NoSuchElementException "Vector Enumeration"))
                                  (vector-set! (jhost-state self) 1 (fx+ i 1))
                                  (al-ref (vec-al v) i)))))
        (cons "asIterator" (lambda (self) (make-jhost "jx-enum-iterator" (vector self))))))
(jch-register-supers! "java.util.Enumeration" '())
;; Enumeration.asIterator(): an Iterator that asks the enumeration
(jx-class! "jx-enum-iterator" "java.util.Enumeration$1" '("java.util.Iterator"))
(register-host-methods! "jx-enum-iterator"
  (list (cons "hasNext" (lambda (self) (jx-call (vit-vec self) "hasMoreElements" '())))
        (cons "next" (lambda (self) (jx-call (vit-vec self) "nextElement" '())))))
;; List.reversed() for a Vector or a CopyOnWriteArrayList: ReverseOrderListView,
;; a live view through the list's own get / set / add / remove at mirrored
;; indexes. State #(list).
(define (rev-list r) (vector-ref (jhost-state r) 0))
(define (rev-n r) (cd-size (rev-list r)))
(define (rev-elems r) (reverse (jxc-elems (rev-list r))))
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

;; ======================================================================================
;; Hashtable
;; ======================================================================================
;; The JDK's own table, ported: an array of buckets, each a chain with the
;; newest entry first, index (hash & 0x7FFFFFFF) mod capacity, a rehash to
;; 2n+1 buckets at the load factor's threshold, iteration from the last bucket
;; to the first. That is what makes (str ht) print in the JVM's order, which
;; programs show. Keys and values are never null. Every method holds the
;; table's monitor; keySet / values / entrySet are Collections.synchronized*
;; views over it on the same lock, as the JDK's are.
;; State #(buckets count threshold load-factor monitor modcount); an entry is a
;; mutable #(hash key value next).
(define (ht-buckets h) (vector-ref (jhost-state h) 0))
(define (ht-count h) (vector-ref (jhost-state h) 1))
(define (ht-count! h n) (vector-set! (jhost-state h) 1 n))
(define (ht-threshold h) (vector-ref (jhost-state h) 2))
(define (ht-lf h) (vector-ref (jhost-state h) 3))
(define (ht-mu h) (vector-ref (jhost-state h) 4))
(define (ht-mod! h) (vector-set! (jhost-state h) 5 (fx+ 1 (vector-ref (jhost-state h) 5))))
(define-syntax with-ht
  (syntax-rules () ((_ h e ...) (jolt-call-with-monitor (ht-mu h) (lambda () e ...)))))
(define ht-max-array 2147483639)   ; Integer.MAX_VALUE - 8
(define (ht-threshold-for cap lf) (exact (floor (min (* cap lf) (+ ht-max-array 1)))))
(define (make-jx-hashtable cap lf)
  (let ((cap (if (eqv? cap 0) 1 cap)))
    (jx-own-monitor! (make-jhost "jx-hashtable" (vector (make-vector cap #f) 0 (ht-threshold-for cap lf) lf #f 0)) 4)))
(define (ht-hash k) (jolt-java-hashcode k))
(define (ht-index hash n) (modulo (bitwise-and hash #x7FFFFFFF) n))
(define (he-hash e) (vector-ref e 0))
(define (he-key e) (vector-ref e 1))
(define (he-val e) (vector-ref e 2))
(define (he-next e) (vector-ref e 3))
(define (ht-find h k)
  (let* ((hash (ht-hash k)) (b (ht-buckets h)))
    (let loop ((e (vector-ref b (ht-index hash (vector-length b)))))
      (cond ((not e) #f)
            ((and (= (he-hash e) hash) (objects-equals? (he-key e) k)) e)
            (else (loop (he-next e)))))))
(define (ht-rehash! h)
  (let* ((old (ht-buckets h)) (oldn (vector-length old))
         (newn (+ (* 2 oldn) 1)) (new (make-vector newn #f)))
    (vector-set! (jhost-state h) 2 (ht-threshold-for newn (ht-lf h)))
    (vector-set! (jhost-state h) 0 new)
    (ht-mod! h)
    (do ((i (fx- oldn 1) (fx- i 1))) ((fx<? i 0))
      (let loop ((e (vector-ref old i)))
        (when e
          (let ((next (he-next e)) (idx (ht-index (he-hash e) newn)))
            (vector-set! e 3 (vector-ref new idx))
            (vector-set! new idx e)
            (loop next)))))))
(define (ht-put! h k v)
  (jxc-npe-if-nil! k v)
  (let ((e (ht-find h k)))
    (if e
        (let ((old (he-val e))) (vector-set! e 2 v) old)
        (begin
          (when (>= (ht-count h) (ht-threshold h)) (ht-rehash! h))
          (let* ((hash (ht-hash k)) (b (ht-buckets h)) (idx (ht-index hash (vector-length b))))
            (vector-set! b idx (vector hash k v (vector-ref b idx)))
            (ht-count! h (fx+ 1 (ht-count h)))
            (ht-mod! h)
            jolt-nil)))))
(define (ht-remove! h k)
  (jxc-npe-if-nil! k)
  (let* ((hash (ht-hash k)) (b (ht-buckets h)) (idx (ht-index hash (vector-length b))))
    (let loop ((e (vector-ref b idx)) (prev #f))
      (cond ((not e) cd-absent)
            ((and (= (he-hash e) hash) (objects-equals? (he-key e) k))
             (if prev (vector-set! prev 3 (he-next e)) (vector-set! b idx (he-next e)))
             (ht-count! h (fx- (ht-count h) 1))
             (ht-mod! h)
             (he-val e))
            (else (loop (he-next e) e))))))
;; every entry, in the Enumerator's order: the last bucket first
(define (ht-entry-list h)
  (let ((b (ht-buckets h)))
    (let outer ((i (fx- (vector-length b) 1)) (acc '()))
      (if (fx<? i 0)
          (reverse acc)
          (outer (fx- i 1) (let chain ((e (vector-ref b i)) (acc acc)) (if e (chain (he-next e) (cons e acc)) acc)))))))
(define (ht-ref h k) (jxc-npe-if-nil! k) (let ((e (ht-find h k))) (if e (he-val e) cd-absent)))
(define (ht-live-entry h e) (make-jhost "jx-hashtable-entry" (vector h e)))
(define (ht-pairs h) (with-ht h (map (lambda (e) (ht-live-entry h e)) (ht-entry-list h))))
(define (ht-clear! h)
  (vector-fill! (ht-buckets h) #f) (ht-count! h 0) (ht-mod! h))
(define hashtable-methods
  (list
    (cons "size" (lambda (self) (with-ht self (->num (ht-count self)))))
    (cons "isEmpty" (lambda (self) (with-ht self (fx=? 0 (ht-count self)))))
    (cons "get" (lambda (self k) (with-ht self (let ((v (ht-ref self k))) (if (eq? v cd-absent) jolt-nil v)))))
    (cons "containsKey" (lambda (self k) (with-ht self (not (eq? (ht-ref self k) cd-absent)))))
    (cons "contains" (lambda (self v) (jxc-npe-if-nil! v)
                       (with-ht self (and (exists (lambda (e) (objects-equals? v (he-val e))) (ht-entry-list self)) #t))))
    (cons "containsValue" (lambda (self v) (jxc-npe-if-nil! v)
                            (with-ht self (and (exists (lambda (e) (objects-equals? v (he-val e))) (ht-entry-list self)) #t))))
    (cons "put" (lambda (self k v) (with-ht self (ht-put! self k v))))
    (cons "remove" (case-lambda
                     ((self k) (with-ht self (let ((v (ht-remove! self k))) (if (eq? v cd-absent) jolt-nil v))))
                     ((self k v) (jxc-npe-if-nil! v)
                                 (with-ht self
                                   (let ((cur (ht-ref self k)))
                                     (if (and (not (eq? cur cd-absent)) (objects-equals? cur v))
                                         (begin (ht-remove! self k) #t) #f))))))
    (cons "putAll" (lambda (self m)
                     (let ((es (jxc-elems m)))
                       (with-ht self (for-each (lambda (e) (ht-put! self (jolt-nth e 0) (jolt-nth e 1))) es)))
                     jolt-nil))
    (cons "clear" (lambda (self) (with-ht self (ht-clear! self)) jolt-nil))
    (cons "clone" (lambda (self)
                    (with-ht self
                      (let ((c (make-jx-hashtable (vector-length (ht-buckets self)) (ht-lf self))))
                        ;; the same chains, copied bucket by bucket
                        (let ((b (ht-buckets self)) (nb (ht-buckets c)))
                          (do ((i 0 (fx+ i 1))) ((fx=? i (vector-length b)))
                            (vector-set! nb i (let copy ((e (vector-ref b i)))
                                                (and e (vector (he-hash e) (he-key e) (he-val e) (copy (he-next e))))))))
                        (ht-count! c (ht-count self))
                        (vector-set! (jhost-state c) 2 (ht-threshold self))
                        c))))
    (cons "keys" (lambda (self) (make-ht-enumerator self 'key)))
    (cons "elements" (lambda (self) (make-ht-enumerator self 'value)))
    (cons "keySet" (lambda (self) (jx-wrap-sync 'set (make-jhost "jx-hashtable-keyset" (vector self)) (ht-mu self))))
    (cons "values" (lambda (self) (jx-wrap-sync 'coll (make-jhost "jx-hashtable-values" (vector self)) (ht-mu self))))
    (cons "entrySet" (lambda (self) (jx-wrap-sync 'set (make-jhost "jx-hashtable-entryset" (vector self)) (ht-mu self))))
    (cons "getOrDefault" (lambda (self k d) (with-ht self (let ((v (ht-ref self k))) (if (eq? v cd-absent) d v)))))
    (cons "putIfAbsent" (lambda (self k v)
                          (jxc-npe-if-nil! v)
                          (with-ht self (let ((cur (ht-ref self k))) (if (eq? cur cd-absent) (ht-put! self k v) cur)))))
    (cons "replace" (case-lambda
                      ((self k v) (jxc-npe-if-nil! k v)
                                  (with-ht self (let ((e (ht-find self k)))
                                                  (if e (let ((old (he-val e))) (vector-set! e 2 v) old) jolt-nil))))
                      ((self k old v) (jxc-npe-if-nil! k old v)
                                      (with-ht self (let ((e (ht-find self k)))
                                                      (if (and e (objects-equals? (he-val e) old))
                                                          (begin (vector-set! e 2 v) #t) #f))))))
    (cons "computeIfAbsent" (lambda (self k f)
                              (with-ht self
                                (let ((cur (ht-ref self k)))
                                  (if (eq? cur cd-absent)
                                      (let ((nv (jolt-fi-call f "apply" k)))
                                        (unless (jolt-nil? nv) (ht-put! self k nv))
                                        nv)
                                      cur)))))
    (cons "computeIfPresent" (lambda (self k f)
                               (with-ht self
                                 (let ((cur (ht-ref self k)))
                                   (if (eq? cur cd-absent)
                                       jolt-nil
                                       (let ((nv (jolt-fi-call f "apply" k cur)))
                                         (if (jolt-nil? nv) (ht-remove! self k) (ht-put! self k nv))
                                         nv))))))
    (cons "compute" (lambda (self k f)
                      (with-ht self
                        (let* ((cur (ht-ref self k))
                               (nv (jolt-fi-call f "apply" k (if (eq? cur cd-absent) jolt-nil cur))))
                          (cond ((not (jolt-nil? nv)) (ht-put! self k nv) nv)
                                ((not (eq? cur cd-absent)) (ht-remove! self k) jolt-nil)
                                (else jolt-nil))))))
    (cons "merge" (lambda (self k v f)
                    (jxc-npe-if-nil! v)
                    (with-ht self
                      (let* ((cur (ht-ref self k))
                             (nv (if (eq? cur cd-absent) v (jolt-fi-call f "apply" cur v))))
                        (if (jolt-nil? nv)
                            (begin (ht-remove! self k) jolt-nil)
                            (begin (ht-put! self k nv) nv))))))
    (cons "forEach" (lambda (self f)
                      (with-ht self
                        (let ((m (ht-mod self)))
                          (for-each (lambda (e) (jolt-fi-call f "accept" (he-key e) (he-val e))) (ht-entry-list self))
                          (unless (fx=? m (ht-mod self)) (throw-jvm 'ConcurrentModificationException jolt-nil))))
                      jolt-nil))
    (cons "replaceAll" (lambda (self f)
                         (with-ht self
                           (for-each (lambda (e)
                                       (let ((nv (jolt-fi-call f "apply" (he-key e) (he-val e))))
                                         (jxc-npe-if-nil! nv)
                                         (vector-set! e 2 nv)))
                                     (ht-entry-list self)))
                         jolt-nil))))
(jx-class! "jx-hashtable" "java.util.Hashtable" '("java.util.Dictionary" "java.util.Map" "java.lang.Cloneable" "java.io.Serializable"))
(jch-register-supers! "java.util.Dictionary" '())
(register-host-methods! "jx-hashtable" hashtable-methods)
(jx-register! "jx-hashtable" 'map ht-pairs (lambda (h) (with-ht h (ht-count h)))
              ;; ht-ref refuses a nil key: (get ht nil) is NullPointerException, as Map.get is
              (lambda (h k) (with-ht h (not (eq? (ht-ref h k) cd-absent))))
              (lambda (h k d) (with-ht h (let ((v (ht-ref h k))) (if (eq? v cd-absent) d v)))))
(define (ht-ctor . args)
  (cond ((null? args) (make-jx-hashtable 11 0.75))
        ((and (number? (car args)) (null? (cdr args)))
         (let ((c (jnum->exact (car args))))
           (when (< c 0) (throw-jvm 'IllegalArgumentException (string-append "Illegal Capacity: " (number->string c))))
           (make-jx-hashtable c 0.75)))
        ((number? (car args))
         (let ((c (jnum->exact (car args))) (lf (inexact (cadr args))))
           (when (< c 0) (throw-jvm 'IllegalArgumentException (string-append "Illegal Capacity: " (number->string c))))
           (when (or (<= lf 0) (nan? lf)) (throw-jvm 'IllegalArgumentException (string-append "Illegal Load: " (jolt-str-render-one (inexact (cadr args))))))
           (make-jx-hashtable c lf)))
        (else
         (let* ((es (jxc-elems (car args)))
                (h (make-jx-hashtable (max (* 2 (length es)) 11) 0.75)))
           (for-each (lambda (e) (ht-put! h (jolt-nth e 0) (jolt-nth e 1))) es)
           h))))
(register-class-ctor! "java.util.Hashtable" (case-lambda (() (ht-ctor)) ((a) (ht-ctor a)) ((a b) (ht-ctor a b))))
(register-class-ctor! "Hashtable" (lookup-class class-ctors-tbl "java.util.Hashtable"))
;; Hashtable$Entry: the chain node itself, so setValue writes into the table
(jx-class! "jx-hashtable-entry" "java.util.Hashtable$Entry" '("java.util.Map$Entry"))
(define (hte-e x) (vector-ref (jhost-state x) 1))
(define (hte-h x) (vector-ref (jhost-state x) 0))
(register-jutil-entry! "jx-hashtable-entry" (lambda (x) (cons (he-key (hte-e x)) (with-ht (hte-h x) (he-val (hte-e x))))))
(register-host-methods! "jx-hashtable-entry"
  (list (cons "setValue" (lambda (self v)
                           (jxc-npe-if-nil! v)
                           (with-ht (hte-h self) (let ((old (he-val (hte-e self)))) (vector-set! (hte-e self) 2 v) old))))))
;; Hashtable$Enumerator: both the Enumeration keys() / elements() answer and the
;; Iterator of the three views. It walks a snapshot of the chain nodes; remove
;; takes the last one it returned out of the table, by key. State
;; #(table remaining last kind iterator?) with kind key / value / entry.
;; slot 5: the modCount an Iterator (not an Enumeration) expects, as the JDK's
;; Enumerator checks it only when it is one
(define (ht-mod h) (vector-ref (jhost-state h) 5))
(define (make-ht-enumerator h kind . iter?)
  (with-ht h
    (make-jhost "jx-hashtable-enumerator"
                (vector h (ht-entry-list h) #f kind (and (pair? iter?) (car iter?)) (ht-mod h)))))
(define (hten-comod! it)
  (let ((st (jhost-state it)))
    (when (and (vector-ref st 4) (not (fx=? (vector-ref st 5) (ht-mod (vector-ref st 0)))))
      (throw-jvm 'ConcurrentModificationException jolt-nil))))
(define (hten-project it e)
  (case (vector-ref (jhost-state it) 3)
    ((key) (he-key e)) ((value) (he-val e)) (else (ht-live-entry (vector-ref (jhost-state it) 0) e))))
(define (hten-next! it)
  (hten-comod! it)
  (let* ((st (jhost-state it)) (es (vector-ref st 1)))
    (when (null? es) (throw-jvm 'NoSuchElementException "Hashtable Enumerator"))
    (vector-set! st 1 (cdr es))
    (vector-set! st 2 (car es))
    (hten-project it (car es))))
(jx-class! "jx-hashtable-enumerator" "java.util.Hashtable$Enumerator" '("java.util.Enumeration" "java.util.Iterator"))
(register-host-methods! "jx-hashtable-enumerator"
  (list (cons "hasMoreElements" (lambda (self) (pair? (vector-ref (jhost-state self) 1))))
        (cons "nextElement" (lambda (self) (hten-next! self)))
        (cons "hasNext" (lambda (self) (pair? (vector-ref (jhost-state self) 1))))
        (cons "next" (lambda (self) (hten-next! self)))
        (cons "asIterator" (lambda (self) self))
        (cons "remove" (lambda (self)
                         (let* ((st (jhost-state self)) (e (vector-ref st 2)))
                           (unless (vector-ref st 4) (throw-jvm 'UnsupportedOperationException jolt-nil))
                           (unless e (throw-jvm 'IllegalStateException "Hashtable Enumerator"))
                           (hten-comod! self)
                           (with-ht (vector-ref st 0) (ht-remove! (vector-ref st 0) (he-key e))
                             (vector-set! st 5 (ht-mod (vector-ref st 0))))
                           (vector-set! st 2 #f)
                           jolt-nil)))
        (cons "forEachRemaining" (lambda (self f)
                                   (let loop ()
                                     (when (pair? (vector-ref (jhost-state self) 1))
                                       (jolt-fi-call f "accept" (hten-next! self))
                                       (loop)))
                                   jolt-nil))))
(register-seq-arm-last! (lambda (x) (and (jhost? x) (string=? (jhost-tag x) "jx-hashtable-enumerator")))
  (lambda (it) (let ((es (vector-ref (jhost-state it) 1)))
                 (if (null? es) jolt-nil (list->cseq (map (lambda (e) (hten-project it e)) es))))))
;; the three views, which the synchronized wrappers above hold the table's lock
;; around. State #(table).
(define (htv-table v) (vector-ref (jhost-state v) 0))
(define (htv-methods kind has? remove!)
  (list (cons "size" (lambda (self) (->num (ht-count (htv-table self)))))
        (cons "isEmpty" (lambda (self) (fx=? 0 (ht-count (htv-table self)))))
        (cons "contains" (lambda (self x) (has? (htv-table self) x)))
        (cons "remove" (lambda (self x) (remove! (htv-table self) x)))
        (cons "clear" (lambda (self) (ht-clear! (htv-table self)) jolt-nil))
        (cons "iterator" (lambda (self) (make-ht-enumerator (htv-table self) kind #t)))
        (cons "toArray" (case-lambda ((self) (cd-to-array (htv-elems kind self) '()))
                                     ((self a) (cd-to-array (htv-elems kind self) (list a)))))))
(define (htv-elems kind v)
  (let ((h (htv-table v)))
    (map (lambda (e) (case kind ((key) (he-key e)) ((value) (he-val e)) (else (ht-live-entry h e))))
         (with-ht h (ht-entry-list h)))))
(define (ht-has-entry? h x)
  (let ((kv (or (jolt-host-entry x) (and (jolt-map-entry? x) (cons (jolt-nth x 0) (jolt-nth x 1))))))
    (and kv (not (jolt-nil? (car kv)))
         (let ((e (ht-find h (car kv)))) (and e (objects-equals? (he-val e) (cdr kv)))))))
(jx-class! "jx-hashtable-keyset" "java.util.Hashtable$KeySet" '("java.util.AbstractSet"))
(register-host-methods! "jx-hashtable-keyset"
  (htv-methods 'key (lambda (h k) (and (not (jolt-nil? k)) (ht-find h k) #t))
               (lambda (h k) (if (jolt-nil? k) #f (not (eq? (ht-remove! h k) cd-absent))))))
(jx-register! "jx-hashtable-keyset" 'set (lambda (v) (htv-elems 'key v)) (lambda (v) (ht-count (htv-table v)))
              (lambda (v k) (and (not (jolt-nil? k)) (ht-find (htv-table v) k) #t)) #f)
(jx-class! "jx-hashtable-values" "java.util.Hashtable$ValueCollection" '("java.util.AbstractCollection"))
(register-host-methods! "jx-hashtable-values"
  (htv-methods 'value (lambda (h x) (and (exists (lambda (e) (objects-equals? x (he-val e))) (ht-entry-list h)) #t))
               (lambda (h x) (let ((e (find (lambda (e) (objects-equals? x (he-val e))) (ht-entry-list h))))
                               (if e (begin (ht-remove! h (he-key e)) #t) #f)))))
(jx-register! "jx-hashtable-values" 'coll (lambda (v) (htv-elems 'value v)) (lambda (v) (ht-count (htv-table v))) #f #f)
(jx-class! "jx-hashtable-entryset" "java.util.Hashtable$EntrySet" '("java.util.AbstractSet"))
(register-host-methods! "jx-hashtable-entryset"
  (htv-methods 'entry ht-has-entry?
               (lambda (h x) (if (ht-has-entry? h x) (begin (ht-remove! h (car (or (jolt-host-entry x) (cons (jolt-nth x 0) #f)))) #t) #f))))
(jx-register! "jx-hashtable-entryset" 'entries (lambda (v) (htv-elems 'entry v)) (lambda (v) (ht-count (htv-table v)))
              (lambda (v x) (ht-has-entry? (htv-table v) x)) #f)

;; ======================================================================================
;; BitSet
;; ======================================================================================
;; The bits are one exact nonnegative integer, bit i of it being bit i of the
;; set; what the JDK's long[] adds on top is its LENGTH, which size() reports
;; and programs print. State #(bits words sticky?): words is words.length, and
;; sticky? the JDK's sizeIsSticky (a size the caller asked for, kept by clone).
;; Growth follows ensureCapacity: double the words, or what is required.
(define (bs-bits b) (vector-ref (jhost-state b) 0))
(define (bs-bits! b n) (vector-set! (jhost-state b) 0 n))
(define (bs-words b) (vector-ref (jhost-state b) 1))
(define (bs-sticky? b) (vector-ref (jhost-state b) 2))
(define (bs-word-index i) (bitwise-arithmetic-shift-right i 6))
(define (bs-words-for nbits) (fx+ 1 (bs-word-index (fx- nbits 1))))
(define (make-jx-bitset bits words sticky?) (make-jhost "jx-bitset" (vector bits words sticky?)))
(define (bs-words-in-use n) (quotient (+ (bitwise-length n) 63) 64))
(define (bs-ensure! b required)
  (when (fx<? (bs-words b) required)
    (vector-set! (jhost-state b) 1 (fxmax (fx* 2 (bs-words b)) required))
    (vector-set! (jhost-state b) 2 #f)))
(define (bs-expand-to! b word-index) (bs-ensure! b (fx+ word-index 1)))
(define (bs-ioobe msg . xs)
  (throw-jvm 'IndexOutOfBoundsException (apply string-append msg (map number->string xs))))
(define (bs-check-index! i) (when (< i 0) (bs-ioobe "bitIndex < 0: " i)))
(define (bs-check-range! from to)
  (when (< from 0) (bs-ioobe "fromIndex < 0: " from))
  (when (< to 0) (bs-ioobe "toIndex < 0: " to))
  (when (> from to) (throw-jvm 'IndexOutOfBoundsException
                               (string-append "fromIndex: " (number->string from) " > toIndex: " (number->string to)))))
(define (bs-mask from to) (bitwise-arithmetic-shift-left (- (bitwise-arithmetic-shift-left 1 (- to from)) 1) from))
(define (bs-of x) (unless (jx-bitset? x) (jx-npe)) x)
(define (bs-set-bits b) ; the set bits, ascending
  (let loop ((n (bs-bits b)) (acc '()))
    ;; highest first, so the list comes out ascending
    (if (zero? n)
        acc
        (let ((i (fx- (bitwise-length n) 1))) (loop (bitwise-and n (- (bitwise-arithmetic-shift-left 1 i) 1)) (cons i acc))))))
(define (bs-string b)
  (let loop ((xs (bs-set-bits b)) (acc '()))
    (if (null? xs)
        (string-append "{" (apply string-append (reverse acc)) "}")
        (loop (cdr xs) (cons (string-append (if (null? acc) "" ", ") (number->string (car xs))) acc)))))
(define (bs-signed64 x)
  (let ((m (bitwise-and x #xFFFFFFFFFFFFFFFF))) (if (>= m #x8000000000000000) (- m #x10000000000000000) m)))
(define (bs-word n i) (bs-signed64 (bitwise-bit-field n (fx* 64 i) (fx* 64 (fx+ i 1)))))
(define (bs-hash b)
  ;; h = 1234; h ^= words[i] * (i + 1), from the top word down; (int)((h >> 32) ^ h)
  (let* ((n (bs-bits b))
         (h (let loop ((i (fx- (bs-words-in-use n) 1)) (h 1234))
              (if (fx<? i 0) h
                  (loop (fx- i 1) (bs-signed64 (bitwise-xor h (bs-signed64 (* (bs-word n i) (fx+ i 1))))))))))
    (i32 (bitwise-xor (bitwise-arithmetic-shift-right h 32) h))))
(define (bs-trim-copy b)
  ;; clone(): trimToSize unless the size is sticky
  (let ((n (bs-bits b)))
    (make-jx-bitset n (if (bs-sticky? b) (bs-words b) (bs-words-in-use n)) (bs-sticky? b))))
(define (bs-set-range! b from to on?)
  (bs-check-range! from to)
  (unless (= from to)
    (when on? (bs-expand-to! b (bs-word-index (- to 1))))
    (bs-bits! b (if on? (bitwise-ior (bs-bits b) (bs-mask from to))
                    (bitwise-and (bs-bits b) (bitwise-not (bs-mask from to)))))))
(define bitset-methods
  (list
    (cons "set" (case-lambda
                  ((self i) (let ((i (jnum->exact i))) (bs-check-index! i) (bs-expand-to! self (bs-word-index i))
                              (bs-bits! self (bitwise-ior (bs-bits self) (bitwise-arithmetic-shift-left 1 i)))) jolt-nil)
                  ((self i x) (if (boolean? x)
                                  (let ((i (jnum->exact i)))
                                    (if x (bs-set-range! self i (+ i 1) #t)
                                        (begin (bs-check-index! i) (bs-bits! self (bitwise-and (bs-bits self) (bitwise-not (bitwise-arithmetic-shift-left 1 i)))))))
                                  (bs-set-range! self (jnum->exact i) (jnum->exact x) #t))
                              jolt-nil)
                  ((self from to on?) (bs-set-range! self (jnum->exact from) (jnum->exact to) (jolt-truthy? on?)) jolt-nil)))
    (cons "clear" (case-lambda
                    ((self) (bs-bits! self 0) jolt-nil)
                    ((self i) (let ((i (jnum->exact i))) (bs-check-index! i)
                                (bs-bits! self (bitwise-and (bs-bits self) (bitwise-not (bitwise-arithmetic-shift-left 1 i))))) jolt-nil)
                    ((self from to) (bs-set-range! self (jnum->exact from) (jnum->exact to) #f) jolt-nil)))
    (cons "flip" (case-lambda
                   ((self i) (let ((i (jnum->exact i))) (bs-check-index! i) (bs-expand-to! self (bs-word-index i))
                               (bs-bits! self (bitwise-xor (bs-bits self) (bitwise-arithmetic-shift-left 1 i)))) jolt-nil)
                   ((self from to) (let ((from (jnum->exact from)) (to (jnum->exact to)))
                                     (bs-check-range! from to)
                                     (unless (= from to)
                                       (bs-expand-to! self (bs-word-index (- to 1)))
                                       (bs-bits! self (bitwise-xor (bs-bits self) (bs-mask from to)))))
                                   jolt-nil)))
    (cons "get" (case-lambda
                  ((self i) (let ((i (jnum->exact i))) (bs-check-index! i) (bitwise-bit-set? (bs-bits self) i)))
                  ((self from to)
                   (let ((from (jnum->exact from)) (to (jnum->exact to)))
                     (bs-check-range! from to)
                     (let ((len (bitwise-length (bs-bits self))))
                       (if (or (<= len from) (= from to))
                           (make-jx-bitset 0 0 #t)
                           (let ((to (min to len)))
                             (make-jx-bitset (bitwise-bit-field (bs-bits self) from to) (bs-words-for (- to from)) #t))))))))
    (cons "cardinality" (lambda (self) (->num (bitwise-bit-count (bs-bits self)))))
    (cons "length" (lambda (self) (->num (bitwise-length (bs-bits self)))))
    (cons "size" (lambda (self) (->num (fx* 64 (bs-words self)))))
    (cons "isEmpty" (lambda (self) (zero? (bs-bits self))))
    (cons "nextSetBit" (lambda (self from)
                         (let ((from (jnum->exact from)))
                           (when (< from 0) (bs-ioobe "fromIndex < 0: " from))
                           (let ((s (bitwise-arithmetic-shift-right (bs-bits self) from)))
                             (->num (if (zero? s) -1 (+ from (bitwise-first-bit-set s))))))))
    (cons "nextClearBit" (lambda (self from)
                           (let ((from (jnum->exact from)))
                             (when (< from 0) (bs-ioobe "fromIndex < 0: " from))
                             (->num (+ from (bitwise-first-bit-set (bitwise-not (bitwise-arithmetic-shift-right (bs-bits self) from))))))))
    (cons "previousSetBit" (lambda (self from)
                             (let ((from (jnum->exact from)))
                               (cond ((= from -1) (->num -1))
                                     ((< from -1) (bs-ioobe "fromIndex < -1: " from))
                                     (else (->num (fx- (bitwise-length (bitwise-and (bs-bits self) (- (bitwise-arithmetic-shift-left 1 (+ from 1)) 1))) 1)))))))
    (cons "previousClearBit" (lambda (self from)
                               (let ((from (jnum->exact from)))
                                 (cond ((= from -1) (->num -1))
                                       ((< from -1) (bs-ioobe "fromIndex < -1: " from))
                                       (else (let loop ((i from))
                                               (cond ((< i 0) (->num -1))
                                                     ((bitwise-bit-set? (bs-bits self) i) (loop (- i 1)))
                                                     (else (->num i)))))))))
    (cons "and" (lambda (self o) (bs-bits! self (bitwise-and (bs-bits self) (bs-bits (bs-of o)))) jolt-nil))
    (cons "or" (lambda (self o) (bs-ensure! self (bs-words-in-use (bs-bits (bs-of o))))
                 (bs-bits! self (bitwise-ior (bs-bits self) (bs-bits o))) jolt-nil))
    (cons "xor" (lambda (self o) (bs-ensure! self (bs-words-in-use (bs-bits (bs-of o))))
                  (bs-bits! self (bitwise-xor (bs-bits self) (bs-bits o))) jolt-nil))
    (cons "andNot" (lambda (self o) (bs-bits! self (bitwise-and (bs-bits self) (bitwise-not (bs-bits (bs-of o))))) jolt-nil))
    (cons "intersects" (lambda (self o) (not (zero? (bitwise-and (bs-bits self) (bs-bits (bs-of o)))))))
    (cons "equals" (lambda (self o) (and (jx-bitset? o) (= (bs-bits self) (bs-bits o)))))
    (cons "hashCode" (lambda (self) (->num (bs-hash self))))
    (cons "toString" (lambda (self) (bs-string self)))
    (cons "clone" (lambda (self) (bs-trim-copy self)))
    (cons "toLongArray" (lambda (self)
                          (let ((n (bs-bits self)))
                            (make-jolt-array (na-list->backing (let loop ((i (fx- (bs-words-in-use n) 1)) (acc '()))
                                                                 (if (fx<? i 0) acc (loop (fx- i 1) (cons (bs-word n i) acc))))
                                                               'long)
                                             'long))))
    (cons "toByteArray" (lambda (self)
                          (let* ((n (bs-bits self)) (nb (quotient (+ (bitwise-length n) 7) 8)))
                            (make-jolt-array (na-list->backing
                                               (let loop ((i (fx- nb 1)) (acc '()))
                                                 (if (fx<? i 0) acc
                                                     (loop (fx- i 1) (cons (let ((x (bitwise-bit-field n (fx* 8 i) (fx* 8 (fx+ i 1)))))
                                                                             (if (>= x 128) (- x 256) x))
                                                                           acc))))
                                               'byte)
                                             'byte))))
    (cons "stream" (lambda (self) (make-stream 'int (list->cseq (map ->num (bs-set-bits self))))))))
(jx-class! "jx-bitset" "java.util.BitSet" '("java.lang.Cloneable" "java.io.Serializable"))
(register-host-methods! "jx-bitset" bitset-methods)
(register-class-ctor! "java.util.BitSet"
  (case-lambda
    (() (make-jx-bitset 0 1 #f))
    ((nbits) (let ((n (jnum->exact nbits)))
               (when (< n 0) (throw-jvm 'NegativeArraySizeException (string-append "nbits < 0: " (number->string n))))
               (make-jx-bitset 0 (if (= n 0) 0 (bs-words-for n)) #t)))))
(register-class-ctor! "BitSet" (lookup-class class-ctors-tbl "java.util.BitSet"))
;; = and hash are BitSet.equals / hashCode: a BitSet is a value on the JVM
(define-jhost-tag-pred (jx-bitset? t) (string=? t "jx-bitset"))
(register-eq-arm-last! (lambda (a b) (or (jx-bitset? a) (jx-bitset? b)))
  (lambda (a b) (and (jx-bitset? a) (jx-bitset? b) (= (bs-bits a) (bs-bits b)))))
(register-hash-arm-last! jx-bitset? bs-hash)
(define (bs-from-words ws) ; valueOf: the words as given, trailing zero words dropped
  (let* ((ws (let loop ((ws (reverse ws))) (if (and (pair? ws) (zero? (car ws))) (loop (cdr ws)) (reverse ws))))
         (n (let loop ((ws ws) (i 0) (n 0))
              (if (null? ws) n (loop (cdr ws) (fx+ i 1) (bitwise-ior n (bitwise-arithmetic-shift-left (bitwise-and (car ws) #xFFFFFFFFFFFFFFFF) (fx* 64 i))))))))
    (make-jx-bitset n (length ws) #f)))
(register-class-statics! "java.util.BitSet"
  (list (cons "valueOf"
              (lambda (a)
                (case (jolt-array-kind a)
                  ((long) (bs-from-words (map jnum->exact (ja->list a))))
                  ((byte) (let* ((bytes (map (lambda (x) (bitwise-and (jnum->exact x) 255)) (ja->list a)))
                                 (bytes (let loop ((bs (reverse bytes))) (if (and (pair? bs) (zero? (car bs))) (loop (cdr bs)) (reverse bs))))
                                 (n (let loop ((bs bytes) (i 0) (n 0))
                                      (if (null? bs) n (loop (cdr bs) (fx+ i 1) (bitwise-ior n (bitwise-arithmetic-shift-left (car bs) (fx* 8 i))))))))
                            (make-jx-bitset n (quotient (+ (length bytes) 7) 8) #f)))
                  (else (throw-jvm 'IllegalArgumentException "BitSet.valueOf takes a long[] or a byte[]")))))))

;; ======================================================================================
;; PriorityQueue
;; ======================================================================================
;; The JDK's binary heap, ported line for line (siftUp / siftDown / removeAt /
;; heapify / bulkRemove), so iteration — which walks the heap array — and
;; toString come out in the JVM's order, and an iterator's remove of an element
;; the heap moves behind it hands that element back later (forgetMeNot).
;; State #(array size comparator-object cmpf modcount); not thread-safe, as on
;; the JDK.
(define (pq-q p) (vector-ref (jhost-state p) 0))
(define (pq-n p) (vector-ref (jhost-state p) 1))
(define (pq-n! p n) (vector-set! (jhost-state p) 1 n))
(define (pq-cmp-obj p) (vector-ref (jhost-state p) 2))
(define (pq-cmpf p) (vector-ref (jhost-state p) 3))
(define (pq-mod p) (vector-ref (jhost-state p) 4))
(define (pq-mod! p) (vector-set! (jhost-state p) 4 (fx+ 1 (pq-mod p))))
(define (pq-natural? p) (jolt-nil? (pq-cmp-obj p)))
(define (make-jx-pq cmp-obj cap)
  (make-jhost "jx-priority-queue"
              (vector (make-vector (fxmax cap 1) jolt-nil) 0 cmp-obj
                      (if (jolt-nil? cmp-obj) tm-natural-compare (tm-comparator->cmpf cmp-obj)) 0)))
(define (pq-grow! p)
  (let* ((old (pq-q p)) (n (vector-length old))
         (new (make-vector (if (fx<? n 64) (fx+ (fx* 2 n) 2) (fx+ n (fxsra n 1))) jolt-nil)))
    (do ((i 0 (fx+ i 1))) ((fx=? i n)) (vector-set! new i (vector-ref old i)))
    (vector-set! (jhost-state p) 0 new)))
(define (pq-check-elem! p x)
  ;; siftUpComparable casts the element to Comparable before it compares
  (when (jolt-nil? x) (jx-npe))
  (when (pq-natural? p) (tm-natural-compare x x)))
(define (pq-sift-up! p k x)
  (let ((q (pq-q p)) (cmp (pq-cmpf p)))
    (let loop ((k k))
      (if (fx>? k 0)
          (let* ((parent (fxsrl (fx- k 1) 1)) (e (vector-ref q parent)))
            (if (>= (cmp x e) 0)
                (vector-set! q k x)
                (begin (vector-set! q k e) (loop parent))))
          (vector-set! q k x)))))
(define (pq-sift-down! p k x n)
  (let ((q (pq-q p)) (cmp (pq-cmpf p)) (half (fxsrl n 1)))
    (let loop ((k k))
      (if (fx<? k half)
          (let* ((child (fx+ (fx* 2 k) 1)) (right (fx+ child 1))
                 (child (if (and (fx<? right n) (> (cmp (vector-ref q child) (vector-ref q right)) 0)) right child))
                 (c (vector-ref q child)))
            (if (<= (cmp x c) 0)
                (vector-set! q k x)
                (begin (vector-set! q k c) (loop child))))
          (vector-set! q k x)))))
(define (pq-heapify! p)
  (let ((n (pq-n p)) (q (pq-q p)))
    (do ((i (fx- (fxsrl n 1) 1) (fx- i 1))) ((fx<? i 0))
      (pq-sift-down! p i (vector-ref q i) n))))
(define (pq-offer! p x)
  (pq-check-elem! p x)
  (pq-mod! p)
  (let ((i (pq-n p)))
    (when (fx>=? i (vector-length (pq-q p))) (pq-grow! p))
    (pq-sift-up! p i x)
    (pq-n! p (fx+ i 1))
    #t))
(define (pq-poll! p)
  (let ((n (pq-n p)))
    (if (fx=? n 0)
        jolt-nil
        (let* ((q (pq-q p)) (result (vector-ref q 0)) (n (fx- n 1)) (x (vector-ref q n)))
          (pq-mod! p)
          (vector-set! q n jolt-nil)
          (pq-n! p n)
          (when (fx>? n 0) (pq-sift-down! p 0 x n))
          result))))
;; removeAt(i): the element moved into a slot before i, or #f
(define (pq-remove-at! p i)
  (let* ((q (pq-q p)) (s (fx- (pq-n p) 1)))
    (pq-mod! p)
    (pq-n! p s)
    (if (fx=? s i)
        (begin (vector-set! q i jolt-nil) #f)
        (let ((moved (vector-ref q s)))
          (vector-set! q s jolt-nil)
          (pq-sift-down! p i moved s)
          (if (eq? (vector-ref q i) moved)
              (begin (pq-sift-up! p i moved)
                     (if (eq? (vector-ref q i) moved) #f moved))
              #f)))))
(define (pq-index-of p x eq)
  (let ((q (pq-q p)) (n (pq-n p)))
    (let loop ((i 0)) (cond ((fx=? i n) -1) ((eq x (vector-ref q i)) i) (else (loop (fx+ i 1)))))))
(define (pq-list p) (let ((q (pq-q p))) (let loop ((i (fx- (pq-n p) 1)) (acc '())) (if (fx<? i 0) acc (loop (fx- i 1) (cons (vector-ref q i) acc))))))
(define (pq-bulk-remove! p doomed?)
  (pq-mod! p)
  (let* ((q (pq-q p)) (end (pq-n p)) (expected (pq-mod p))
         (dead (let loop ((i 0) (acc '())) (if (fx=? i end) (reverse acc) (loop (fx+ i 1) (if (jolt-truthy? (doomed? (vector-ref q i))) (cons i acc) acc))))))
    (unless (fx=? expected (pq-mod p)) (throw-jvm 'ConcurrentModificationException jolt-nil))
    (if (null? dead)
        #f
        (let loop ((i 0) (w 0) (dead dead))
          (cond ((fx<? i end)
                 (if (and (pair? dead) (fx=? (car dead) i))
                     (loop (fx+ i 1) w (cdr dead))
                     (begin (vector-set! q w (vector-ref q i)) (loop (fx+ i 1) (fx+ w 1) dead))))
                (else (do ((k w (fx+ k 1))) ((fx=? k end)) (vector-set! q k jolt-nil))
                      (pq-n! p w)
                      (pq-heapify! p)
                      #t))))))
(define (pq-fill! p xs) ; initElementsFromCollection: in order, no heapify
  (for-each (lambda (x) (when (jolt-nil? x) (jx-npe))) xs)
  (let ((v (list->vector xs)))
    (vector-set! (jhost-state p) 0 (if (fx=? 0 (vector-length v)) (make-vector 1 jolt-nil) v))
    (pq-n! p (vector-length v))))
(define (jx-isa? x cls) (let ((c (guard (e (#t #f)) (jolt-class-name x)))) (and (string? c) (jch-isa? c cls))))
(define priority-queue-methods
  (list
    (cons "add" (lambda (self x) (pq-offer! self x)))
    (cons "offer" (lambda (self x) (pq-offer! self x)))
    (cons "poll" (lambda (self) (pq-poll! self)))
    (cons "peek" (lambda (self) (if (fx=? 0 (pq-n self)) jolt-nil (vector-ref (pq-q self) 0))))
    (cons "element" (lambda (self) (if (fx=? 0 (pq-n self)) (throw-jvm 'NoSuchElementException jolt-nil) (vector-ref (pq-q self) 0))))
    (cons "remove" (case-lambda
                     ((self) (if (fx=? 0 (pq-n self)) (throw-jvm 'NoSuchElementException jolt-nil) (pq-poll! self)))
                     ((self x) (let ((i (pq-index-of self x objects-equals?)))
                                 (if (fx<? i 0) #f (begin (pq-remove-at! self i) #t))))))
    (cons "size" (lambda (self) (->num (pq-n self))))
    (cons "isEmpty" (lambda (self) (fx=? 0 (pq-n self))))
    (cons "clear" (lambda (self) (pq-mod! self) (vector-fill! (pq-q self) jolt-nil) (pq-n! self 0) jolt-nil))
    (cons "contains" (lambda (self x) (fx>=? (pq-index-of self x objects-equals?) 0)))
    (cons "comparator" (lambda (self) (pq-cmp-obj self)))
    (cons "toArray" (jxc-to-array pq-list))
    (cons "iterator" (lambda (self) (make-jhost "jx-pq-itr" (vector self 0 -1 '() #f (pq-mod self)))))
    (cons "addAll" (lambda (self c)
                     (when (eq? c self) (throw-jvm 'IllegalArgumentException jolt-nil))
                     (fold-left (lambda (ch x) (pq-offer! self x) #t) #f (jxc-elems c))))
    (cons "forEach" (lambda (self f)
                      (let ((m (pq-mod self)))
                        (for-each (lambda (x) (jolt-fi-call f "accept" x)) (pq-list self))
                        (unless (fx=? m (pq-mod self)) (throw-jvm 'ConcurrentModificationException jolt-nil)))
                      jolt-nil))
    (cons "removeIf" (lambda (self f) (when (jolt-nil? f) (jx-npe)) (pq-bulk-remove! self (lambda (x) (jolt-fi-call f "test" x)))))
    (cons "removeAll" (lambda (self c) (when (jolt-nil? c) (jx-npe)) (pq-bulk-remove! self (lambda (x) (cd-truthy? (cd-call c "contains" x))))))
    (cons "retainAll" (lambda (self c) (when (jolt-nil? c) (jx-npe)) (pq-bulk-remove! self (lambda (x) (not (cd-truthy? (cd-call c "contains" x)))))))))
(jx-class! "jx-priority-queue" "java.util.PriorityQueue" '("java.util.AbstractQueue" "java.io.Serializable"))
(jch-register-supers! "java.util.AbstractQueue" '("java.util.AbstractCollection" "java.util.Queue"))
(register-host-methods! "jx-priority-queue" priority-queue-methods)
(jx-register! "jx-priority-queue" 'coll pq-list (lambda (p) (pq-n p)) #f #f)
(define (pq-ctor . args)
  (define (cap! c) (let ((c (jnum->exact c))) (when (< c 1) (throw-jvm 'IllegalArgumentException jolt-nil)) c))
  (cond ((null? args) (make-jx-pq jolt-nil 11))
        ((and (null? (cdr args)) (number? (car args))) (make-jx-pq jolt-nil (cap! (car args))))
        ((null? (cdr args))
         (let ((x (car args)))
           (cond ((tm-comparator-like? x) (make-jx-pq x 11))
                 ((and (jhost? x) (string=? (jhost-tag x) "jx-priority-queue"))
                  (let ((p (make-jx-pq (pq-cmp-obj x) 1))) (pq-fill! p (pq-list x)) p))
                 ((jx-isa? x "java.util.SortedSet")
                  (let ((p (make-jx-pq (jx-call x "comparator" '()) 1))) (pq-fill! p (jxc-elems x)) p))
                 (else (let ((p (make-jx-pq jolt-nil 1)))
                         (pq-fill! p (jxc-elems x))
                         (when (fx=? 1 (pq-n p)) (pq-check-elem! p (vector-ref (pq-q p) 0)))
                         (pq-heapify! p)
                         p)))))
        (else (make-jx-pq (cadr args) (cap! (car args))))))
(register-class-ctor! "java.util.PriorityQueue" (case-lambda (() (pq-ctor)) ((a) (pq-ctor a)) ((a b) (pq-ctor a b))))
(register-class-ctor! "PriorityQueue" (lookup-class class-ctors-tbl "java.util.PriorityQueue"))
;; PriorityQueue$Itr. State #(queue cursor lastRet forgetMeNot lastRetElt expectedModCount)
(define (pqi-ref it i) (vector-ref (jhost-state it) i))
(define (pqi-set! it i x) (vector-set! (jhost-state it) i x))
(define (pqi-comod! it) (unless (fx=? (pqi-ref it 5) (pq-mod (pqi-ref it 0))) (throw-jvm 'ConcurrentModificationException jolt-nil)))
(define (pqi-has-next? it) (or (fx<? (pqi-ref it 1) (pq-n (pqi-ref it 0))) (pair? (pqi-ref it 3))))
(define (pqi-next! it)
  (pqi-comod! it)
  (let ((p (pqi-ref it 0)) (cursor (pqi-ref it 1)))
    (cond ((fx<? cursor (pq-n p)) (pqi-set! it 2 cursor) (pqi-set! it 1 (fx+ cursor 1)) (vector-ref (pq-q p) cursor))
          ((pair? (pqi-ref it 3))
           (let ((x (car (pqi-ref it 3)))) (pqi-set! it 2 -1) (pqi-set! it 3 (cdr (pqi-ref it 3))) (pqi-set! it 4 x) x))
          (else (throw-jvm 'NoSuchElementException jolt-nil)))))
;; iterator-seq's chunk fill (records-dispatch.ss): up to 32 nexts, the count
;; checked once, since nothing runs between them
(define (pqi-fill it buf)
  (and (fx=? (pqi-ref it 5) (pq-mod (pqi-ref it 0)))
       (let* ((p (pqi-ref it 0)) (q (pq-q p)) (n (pq-n p)) (c (pqi-ref it 1))
              (k (if (fx<? c n) (fxmin 32 (fx- n c)) 0)))
         ;; the heap array first, in one run
         (do ((j 0 (fx+ j 1))) ((fx=? j k)) (vector-set! buf j (vector-ref q (fx+ c j))))
         (when (fx>? k 0) (pqi-set! it 2 (fx+ c (fx- k 1))) (pqi-set! it 1 (fx+ c k)))
         ;; then the elements a remove moved behind the cursor
         (let loop ((j k))
           (if (and (fx<? j 32) (pair? (pqi-ref it 3)))
               (let ((x (car (pqi-ref it 3))))
                 (pqi-set! it 2 -1) (pqi-set! it 3 (cdr (pqi-ref it 3))) (pqi-set! it 4 x)
                 (vector-set! buf j x)
                 (loop (fx+ j 1)))
               j)))))
(register-iterator-seq-fill! '("jx-pq-itr") pqi-fill pqi-next!)
(jx-class! "jx-pq-itr" "java.util.PriorityQueue$Itr" '("java.util.Iterator"))
(register-host-methods! "jx-pq-itr"
  (list (cons "hasNext" (lambda (self) (pqi-has-next? self)))
        (cons "next" pqi-next!)
        (cons "remove" (lambda (self)
                         (pqi-comod! self)
                         (let ((p (pqi-ref self 0)) (last (pqi-ref self 2)))
                           (cond ((not (fx=? last -1))
                                  (let ((moved (pq-remove-at! p last)))
                                    (pqi-set! self 2 -1)
                                    (if moved
                                        (pqi-set! self 3 (append (pqi-ref self 3) (list moved)))
                                        (pqi-set! self 1 (fx- (pqi-ref self 1) 1)))))
                                 ((pqi-ref self 4)
                                  (let ((i (pq-index-of p (pqi-ref self 4) eq?)))
                                    (when (fx>=? i 0) (pq-remove-at! p i)))
                                  (pqi-set! self 4 #f))
                                 (else (throw-jvm 'IllegalStateException jolt-nil)))
                           (pqi-set! self 5 (pq-mod p))
                           jolt-nil)))
        (cons "forEachRemaining" (lambda (self f)
                                   (let loop () (when (pqi-has-next? self) (jolt-fi-call f "accept" (pqi-next! self)) (loop)))
                                   jolt-nil))))
(jx-register! "jx-pq-itr" 'iter
  (lambda (it) (let ((p (pqi-ref it 0)))
                 (append (list-tail (pq-list p) (fxmin (pqi-ref it 1) (pq-n p))) (pqi-ref it 3))))
  #f #f #f)

;; ======================================================================================
;; ConcurrentLinkedQueue and ConcurrentLinkedDeque
;; ======================================================================================
;; A doubly linked chain between two sentinels, every change and every read
;; under the queue's monitor. A node is #(item next prev); removing one marks its
;; item dead and unlinks it from its neighbours but leaves its own links, so an
;; iterator standing on it walks on to what follows. That, and an iterator that
;; reads the next item ahead as the JDK's Itr does (nextNode / nextItem), is the
;; JDK's weak consistency: an element removed after the iterator read it ahead is
;; still handed out. Nothing is nil: an element may be false, so death is a
;; marker. State #(head-sentinel tail-sentinel monitor).
(define cl-dead (list 'cl-dead))
(define (cl-node item next prev) (vector item next prev))
(define (cln-item n) (vector-ref n 0))
(define (cln-next n) (vector-ref n 1))
(define (cln-prev n) (vector-ref n 2))
(define (cl-head q) (vector-ref (jhost-state q) 0))
(define (cl-tail q) (vector-ref (jhost-state q) 1))
(define (cl-mu q) (vector-ref (jhost-state q) 2))
(define-syntax with-cl
  (syntax-rules () ((_ q e ...) (jolt-call-with-monitor (cl-mu q) (lambda () e ...)))))
(define (make-jx-clq tag xs)
  (let* ((h (cl-node cl-dead #f #f)) (t (cl-node cl-dead #f h)))
    (vector-set! h 1 t)
    (let ((q (make-jhost tag (vector h t (make-monitor)))))
      (for-each (lambda (x) (cl-link-last! q x)) xs)
      q)))
(define (cl-sentinel? q n) (or (eq? n (cl-head q)) (eq? n (cl-tail q))))
(define (cl-link-before! next x)
  (when (jolt-nil? x) (jx-npe))
  (let* ((prev (cln-prev next)) (n (cl-node x next prev)))
    (vector-set! prev 1 n) (vector-set! next 2 n)))
(define (cl-link-last! q x) (cl-link-before! (cl-tail q) x))
(define (cl-link-first! q x) (cl-link-before! (cln-next (cl-head q)) x))
(define (cl-unlink! n)
  (unless (eq? (cln-item n) cl-dead)
    (vector-set! n 0 cl-dead)
    (let ((p (cln-prev n)) (x (cln-next n)))
      (vector-set! p 1 x) (vector-set! x 2 p))))
(define (cl-first-node q) (let ((n (cln-next (cl-head q)))) (if (eq? n (cl-tail q)) #f n)))
(define (cl-last-node q) (let ((n (cln-prev (cl-tail q)))) (if (eq? n (cl-head q)) #f n)))
(define (cl-list q)
  (with-cl q
    (let loop ((n (cln-prev (cl-tail q))) (acc '()))
      (if (eq? n (cl-head q)) acc (loop (cln-prev n) (cons (cln-item n) acc))))))
(define (cl-count q) (with-cl q (let loop ((n (cln-next (cl-head q))) (k 0)) (if (eq? n (cl-tail q)) k (loop (cln-next n) (fx+ k 1))))))
(define (cl-find q x forward?)
  (let ((end (if forward? (cl-tail q) (cl-head q))))
    (let loop ((n (if forward? (cln-next (cl-head q)) (cln-prev (cl-tail q)))))
      (cond ((eq? n end) #f)
            ((objects-equals? x (cln-item n)) n)
            (else (loop (if forward? (cln-next n) (cln-prev n))))))))
(define (cl-take! q first?)
  (with-cl q
    (let ((n (if first? (cl-first-node q) (cl-last-node q))))
      (if n (let ((x (cln-item n))) (cl-unlink! n) x) cl-dead))))
(define (cl-peek q first?)
  (with-cl q (let ((n (if first? (cl-first-node q) (cl-last-node q)))) (if n (cln-item n) cl-dead))))
(define (cl-or-nil x) (if (eq? x cl-dead) jolt-nil x))
(define (cl-or-throw x) (if (eq? x cl-dead) (throw-jvm 'NoSuchElementException jolt-nil) x))
(define (cl-remove-occurrence! q x forward?)
  (with-cl q (let ((n (cl-find q x forward?))) (if n (begin (cl-unlink! n) #t) #f))))
(define (cl-bulk-remove! q doomed?)
  ;; the live (node . item) pairs, read under the lock; each item is tested
  ;; outside it, so the test only ever sees an element, and its node is unlinked
  ;; if it is still there
  (fold-left (lambda (changed p)
               (if (jolt-truthy? (doomed? (cdr p)))
                   (or (with-cl q (and (not (eq? (cln-item (car p)) cl-dead)) (begin (cl-unlink! (car p)) #t))) changed)
                   changed))
             #f
             (with-cl q (let loop ((n (cln-prev (cl-tail q))) (acc '()))
                          (cond ((eq? n (cl-head q)) acc)
                                ((eq? (cln-item n) cl-dead) (loop (cln-prev n) acc))
                                (else (loop (cln-prev n) (cons (cons n (cln-item n)) acc))))))))
;; Itr / DescendingItr: #(queue next-node next-item last-node forward?)
(define (make-cl-itr tag q forward?)
  (let ((it (make-jhost tag (vector q #f #f #f forward?))))
    (with-cl q (cli-advance! it (if forward? (cl-head q) (cl-tail q))))
    it))
(define (cli-ref it i) (vector-ref (jhost-state it) i))
(define (cli-advance! it from)
  ;; the next live node after FROM in the walk's direction, with its item read now
  (let* ((q (cli-ref it 0)) (fwd? (cli-ref it 4)) (end (if fwd? (cl-tail q) (cl-head q))))
    (let loop ((n (if fwd? (cln-next from) (cln-prev from))))
      (cond ((or (not n) (eq? n end)) (vector-set! (jhost-state it) 1 #f) (vector-set! (jhost-state it) 2 #f))
            ((eq? (cln-item n) cl-dead) (loop (if fwd? (cln-next n) (cln-prev n))))
            (else (vector-set! (jhost-state it) 1 n) (vector-set! (jhost-state it) 2 (cln-item n)))))))
(define (cli-next! it)
  (let ((n (cli-ref it 1)))
    (unless n (throw-jvm 'NoSuchElementException jolt-nil))
    (let ((x (cli-ref it 2)))
      (vector-set! (jhost-state it) 3 n)
      (with-cl (cli-ref it 0) (cli-advance! it n))
      x)))
(define cl-itr-methods
  (list (cons "hasNext" (lambda (self) (and (cli-ref self 1) #t)))
        (cons "next" (lambda (self) (cli-next! self)))
        (cons "remove" (lambda (self)
                         (let ((n (cli-ref self 3)))
                           (unless n (throw-jvm 'IllegalStateException jolt-nil))
                           (with-cl (cli-ref self 0) (cl-unlink! n))
                           (vector-set! (jhost-state self) 3 #f)
                           jolt-nil)))
        (cons "forEachRemaining" (lambda (self f)
                                   (let loop () (when (cli-ref self 1) (jolt-fi-call f "accept" (cli-next! self)) (loop)))
                                   jolt-nil))))
(define (cli-rest it) ; what the iterator has not handed out yet, for (seq it)
  (let ((n (cli-ref it 1)))
    (if (not n) '()
        (let ((q (cli-ref it 0)) (fwd? (cli-ref it 4)))
          (with-cl q
            (let loop ((m (if fwd? (cln-next n) (cln-prev n))) (acc (list (cli-ref it 2))))
              (cond ((or (not m) (cl-sentinel? q m)) (reverse acc))
                    ((eq? (cln-item m) cl-dead) (loop (if fwd? (cln-next m) (cln-prev m)) acc))
                    (else (loop (if fwd? (cln-next m) (cln-prev m)) (cons (cln-item m) acc))))))))))
(for-each (lambda (p)
            (jx-class! (car p) (cdr p) '("java.util.Iterator"))
            (register-host-methods! (car p) cl-itr-methods)
            (jx-register! (car p) 'iter cli-rest #f #f #f))
          '(("jx-clq-itr" . "java.util.concurrent.ConcurrentLinkedQueue$Itr")
            ("jx-cld-itr" . "java.util.concurrent.ConcurrentLinkedDeque$Itr")
            ("jx-cld-desc-itr" . "java.util.concurrent.ConcurrentLinkedDeque$DescendingItr")))
(define (cl-add-all! q c)
  (when (eq? c q) (throw-jvm 'IllegalArgumentException jolt-nil))
  (let ((xs (jxc-elems c)))
    (for-each (lambda (x) (when (jolt-nil? x) (jx-npe))) xs)
    (with-cl q (for-each (lambda (x) (cl-link-last! q x)) xs))
    (pair? xs)))
(define (cl-common-methods itr-tag)
  (list
    (cons "size" (lambda (self) (->num (cl-count self))))
    (cons "isEmpty" (lambda (self) (not (with-cl self (cl-first-node self)))))
    (cons "contains" (lambda (self x) (and (not (jolt-nil? x)) (with-cl self (cl-find self x #t)) #t)))
    (cons "iterator" (lambda (self) (make-cl-itr itr-tag self #t)))
    (cons "toArray" (jxc-to-array cl-list))
    (cons "addAll" (lambda (self c) (cl-add-all! self c)))
    (cons "clear" (lambda (self) (with-cl self (let loop () (let ((n (cl-first-node self))) (when n (cl-unlink! n) (loop))))) jolt-nil))
    (cons "forEach" (lambda (self f) (for-each (lambda (x) (jolt-fi-call f "accept" x)) (cl-list self)) jolt-nil))
    (cons "removeIf" (lambda (self f) (when (jolt-nil? f) (jx-npe)) (cl-bulk-remove! self (lambda (x) (jolt-fi-call f "test" x)))))
    (cons "removeAll" (lambda (self c) (when (jolt-nil? c) (jx-npe)) (cl-bulk-remove! self (lambda (x) (cd-truthy? (cd-call c "contains" x))))))
    (cons "retainAll" (lambda (self c) (when (jolt-nil? c) (jx-npe)) (cl-bulk-remove! self (lambda (x) (not (cd-truthy? (cd-call c "contains" x)))))))
    (cons "offer" (lambda (self x) (with-cl self (cl-link-last! self x)) #t))
    (cons "add" (lambda (self x) (with-cl self (cl-link-last! self x)) #t))
    (cons "poll" (lambda (self) (cl-or-nil (cl-take! self #t))))
    (cons "peek" (lambda (self) (cl-or-nil (cl-peek self #t))))
    (cons "element" (lambda (self) (cl-or-throw (cl-peek self #t))))))
(define clq-methods
  (append (cl-common-methods "jx-clq-itr")
          (list (cons "remove" (case-lambda
                                 ((self) (cl-or-throw (cl-take! self #t)))
                                 ((self x) (and (not (jolt-nil? x)) (cl-remove-occurrence! self x #t))))))))
(define cld-methods
  (append (cl-common-methods "jx-cld-itr")
          (list
            (cons "remove" (case-lambda
                             ((self) (cl-or-throw (cl-take! self #t)))
                             ((self x) (when (jolt-nil? x) (jx-npe)) (cl-remove-occurrence! self x #t))))
            (cons "contains" (lambda (self x) (and (not (jolt-nil? x)) (with-cl self (cl-find self x #t)) #t)))
            (cons "addFirst" (lambda (self x) (with-cl self (cl-link-first! self x)) jolt-nil))
            (cons "addLast" (lambda (self x) (with-cl self (cl-link-last! self x)) jolt-nil))
            (cons "offerFirst" (lambda (self x) (with-cl self (cl-link-first! self x)) #t))
            (cons "offerLast" (lambda (self x) (with-cl self (cl-link-last! self x)) #t))
            (cons "push" (lambda (self x) (with-cl self (cl-link-first! self x)) jolt-nil))
            (cons "pop" (lambda (self) (cl-or-throw (cl-take! self #t))))
            (cons "pollFirst" (lambda (self) (cl-or-nil (cl-take! self #t))))
            (cons "pollLast" (lambda (self) (cl-or-nil (cl-take! self #f))))
            (cons "peekFirst" (lambda (self) (cl-or-nil (cl-peek self #t))))
            (cons "peekLast" (lambda (self) (cl-or-nil (cl-peek self #f))))
            (cons "getFirst" (lambda (self) (cl-or-throw (cl-peek self #t))))
            (cons "getLast" (lambda (self) (cl-or-throw (cl-peek self #f))))
            (cons "removeFirst" (lambda (self) (cl-or-throw (cl-take! self #t))))
            (cons "removeLast" (lambda (self) (cl-or-throw (cl-take! self #f))))
            (cons "removeFirstOccurrence" (lambda (self x) (when (jolt-nil? x) (jx-npe)) (cl-remove-occurrence! self x #t)))
            (cons "removeLastOccurrence" (lambda (self x) (when (jolt-nil? x) (jx-npe)) (cl-remove-occurrence! self x #f)))
            (cons "descendingIterator" (lambda (self) (make-cl-itr "jx-cld-desc-itr" self #f)))
            (cons "reversed" (lambda (self) (make-jhost "jx-reverse-deque" (vector self)))))))
(jx-class! "jx-clq" "java.util.concurrent.ConcurrentLinkedQueue" '("java.util.AbstractQueue" "java.util.Queue" "java.io.Serializable"))
(jx-class! "jx-cld" "java.util.concurrent.ConcurrentLinkedDeque" '("java.util.AbstractCollection" "java.util.Deque" "java.io.Serializable"))
(register-host-methods! "jx-clq" clq-methods)
(register-host-methods! "jx-cld" cld-methods)
(jx-register! "jx-clq" 'coll cl-list cl-count #f #f)
(jx-register! "jx-cld" 'coll cl-list cl-count #f #f)
(for-each (lambda (p)
            (register-class-ctor! (car p) (case-lambda (() (make-jx-clq (cdr p) '())) ((c) (make-jx-clq (cdr p) (jxc-elems c)))))
            (register-class-ctor! (substring (car p) 21 (string-length (car p))) (lookup-class class-ctors-tbl (car p))))
          '(("java.util.concurrent.ConcurrentLinkedQueue" . "jx-clq") ("java.util.concurrent.ConcurrentLinkedDeque" . "jx-cld")))
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

;; ======================================================================================
;; CopyOnWriteArrayList and CopyOnWriteArraySet
;; ======================================================================================
;; The elements are a Scheme vector that is never changed once published. A
;; reader takes the published vector with no lock; a writer, under the list's
;; monitor, builds a new vector from the published one and publishes it with one
;; store behind a release fence (tools/cow-publish-model.pl, make
;; cowpublishmodel). An iterator is the vector it started from (COWIterator), so
;; it never sees a later write and refuses remove. State #(vector monitor).
(define (cow-vec l) (vector-ref (jhost-state l) 0))
(define (cow-list l) (vector->list (cow-vec l)))
(define (cow-n l) (vector-length (cow-vec l)))
(define (cow-publish! l v) (memory-order-release) (vector-set! (jhost-state l) 0 v))
(define-syntax with-cow
  (syntax-rules () ((_ l e ...) (jolt-call-with-monitor (vector-ref (jhost-state l) 1) (lambda () e ...)))))
(define (make-jx-cow tag xs) (make-jhost tag (vector (list->vector xs) (make-monitor))))
(define (cow-index-of v x from to)
  (let loop ((i from)) (cond ((fx>=? i to) -1) ((objects-equals? x (vector-ref v i)) i) (else (loop (fx+ i 1))))))
(define (cow-last-index-of v x from)
  (let loop ((i from)) (cond ((fx<? i 0) -1) ((objects-equals? x (vector-ref v i)) i) (else (loop (fx- i 1))))))
(define (cow-insert v i xs)
  (let* ((n (vector-length v)) (k (length xs)) (new (make-vector (fx+ n k))))
    (do ((j 0 (fx+ j 1))) ((fx=? j i)) (vector-set! new j (vector-ref v j)))
    (let loop ((xs xs) (j i)) (unless (null? xs) (vector-set! new j (car xs)) (loop (cdr xs) (fx+ j 1))))
    (do ((j i (fx+ j 1))) ((fx=? j n)) (vector-set! new (fx+ j k) (vector-ref v j)))
    new))
(define (cow-delete v i)
  (let* ((n (vector-length v)) (new (make-vector (fx- n 1))))
    (do ((j 0 (fx+ j 1))) ((fx=? j i)) (vector-set! new j (vector-ref v j)))
    (do ((j (fx+ i 1) (fx+ j 1))) ((fx=? j n)) (vector-set! new (fx- j 1) (vector-ref v j)))
    new))
(define (cow-index! i n) (unless (and (fx>=? i 0) (fx<? i n)) (throw-jvm 'IndexOutOfBoundsException (string-append "Index " (number->string i) " out of bounds for length " (number->string n)))))
;; the list itself indexes its array, so a bad index is the array's exception
(define (cow-aindex! i n) (unless (and (fx>=? i 0) (fx<? i n)) (throw-jvm 'ArrayIndexOutOfBoundsException (string-append "Index " (number->string i) " out of bounds for length " (number->string n)))))
(define (cow-add-index! i n) (unless (and (fx>=? i 0) (fx<=? i n)) (throw-jvm 'IndexOutOfBoundsException (string-append "Index: " (number->string i) ", Size: " (number->string n)))))
(define (cow-remove-if! l doomed?)
  ;; the test runs on a snapshot outside the lock; the write publishes only if
  ;; nothing changed meanwhile, else it tests again — removeIf is atomic on the JDK
  (let retry ()
    (let* ((v (cow-vec l)) (keep (filter (lambda (x) (not (jolt-truthy? (doomed? x)))) (vector->list v))))
      (if (fx=? (length keep) (vector-length v))
          #f
          (or (with-cow l (and (eq? v (cow-vec l)) (begin (cow-publish! l (list->vector keep)) #t)))
              (retry))))))
(define (cow-add-if-absent! l x)
  (with-cow l (let ((v (cow-vec l)))
                (if (fx>=? (cow-index-of v x 0 (vector-length v)) 0) #f
                    (begin (cow-publish! l (cow-insert v (vector-length v) (list x))) #t)))))
(define cow-list-methods
  (list
    (cons "size" (lambda (self) (->num (cow-n self))))
    (cons "isEmpty" (lambda (self) (fx=? 0 (cow-n self))))
    (cons "get" (lambda (self i) (let ((v (cow-vec self)) (i (jnum->exact i))) (cow-aindex! i (vector-length v)) (vector-ref v i))))
    (cons "set" (lambda (self i x)
                  (with-cow self
                    (let* ((v (cow-vec self)) (i (jnum->exact i)))
                      (cow-aindex! i (vector-length v))
                      ;; setting an element to itself keeps the array, so a subList stays valid
                      (let ((old (vector-ref v i)))
                        (unless (eq? old x)
                          (let ((new (vector-copy v))) (vector-set! new i x) (cow-publish! self new)))
                        old)))))
    (cons "add" (case-lambda
                  ((self x) (with-cow self (cow-publish! self (cow-insert (cow-vec self) (cow-n self) (list x)))) #t)
                  ((self i x) (with-cow self (let ((i (jnum->exact i))) (cow-add-index! i (cow-n self))
                                               (cow-publish! self (cow-insert (cow-vec self) i (list x))))) jolt-nil)))
    (cons "addAll" (case-lambda
                     ((self c) (let ((xs (jxc-elems c)))
                                 (with-cow self (cow-publish! self (cow-insert (cow-vec self) (cow-n self) xs)))
                                 (pair? xs)))
                     ((self i c) (let ((xs (jxc-elems c)))
                                   (with-cow self (let ((i (jnum->exact i))) (cow-add-index! i (cow-n self))
                                                    (cow-publish! self (cow-insert (cow-vec self) i xs))))
                                   (pair? xs)))))
    (cons "addIfAbsent" (lambda (self x) (cow-add-if-absent! self x)))
    (cons "addAllAbsent" (lambda (self c)
                           (let ((xs (jxc-elems c)))
                             (with-cow self
                               (let* ((v (cow-vec self))
                                      (new (fold-left (lambda (acc x) (if (or (fx>=? (cow-index-of v x 0 (vector-length v)) 0) (exists (lambda (y) (objects-equals? x y)) acc)) acc (append acc (list x)))) '() xs)))
                                 (unless (null? new) (cow-publish! self (cow-insert v (vector-length v) new)))
                                 (->num (length new)))))))
    (cons "remove" (lambda (self x)
                     (if (and (fixnum? x) (not (jfloat? x)))
                         (with-cow self (let ((v (cow-vec self))) (cow-aindex! x (vector-length v))
                                          (let ((old (vector-ref v x))) (cow-publish! self (cow-delete v x)) old)))
                         (with-cow self (let* ((v (cow-vec self)) (i (cow-index-of v x 0 (vector-length v))))
                                          (if (fx<? i 0) #f (begin (cow-publish! self (cow-delete v i)) #t)))))))
    (cons "clear" (lambda (self) (with-cow self (cow-publish! self (vector))) jolt-nil))
    (cons "contains" (lambda (self x) (let ((v (cow-vec self))) (fx>=? (cow-index-of v x 0 (vector-length v)) 0))))
    (cons "indexOf" (case-lambda
                      ((self x) (let ((v (cow-vec self))) (->num (cow-index-of v x 0 (vector-length v)))))
                      ((self x i) (let ((v (cow-vec self)) (i (jnum->exact i)))
                                    (when (and (fx<? i 0) (fx>? (vector-length v) 0)) (cow-aindex! i (vector-length v)))
                                    (->num (cow-index-of v x (fxmax i 0) (vector-length v)))))))
    (cons "lastIndexOf" (case-lambda
                          ((self x) (let ((v (cow-vec self))) (->num (cow-last-index-of v x (fx- (vector-length v) 1)))))
                          ((self x i) (let ((v (cow-vec self)) (i (jnum->exact i)))
                                        (when (fx>=? i (vector-length v)) (cow-aindex! i (vector-length v)))
                                        (->num (cow-last-index-of v x i))))))
    (cons "removeIf" (lambda (self f) (when (jolt-nil? f) (jx-npe)) (cow-remove-if! self (lambda (x) (jolt-fi-call f "test" x)))))
    (cons "removeAll" (lambda (self c) (when (jolt-nil? c) (jx-npe)) (cow-remove-if! self (lambda (x) (cd-truthy? (cd-call c "contains" x))))))
    (cons "retainAll" (lambda (self c) (when (jolt-nil? c) (jx-npe)) (cow-remove-if! self (lambda (x) (not (cd-truthy? (cd-call c "contains" x)))))))
    (cons "replaceAll" (lambda (self f)
                         (with-cow self (cow-publish! self (vector-map (lambda (x) (jolt-fi-call f "apply" x)) (cow-vec self))))
                         jolt-nil))
    (cons "sort" (lambda (self c)
                   (with-cow self
                     (let ((cmp (if (jolt-nil? c) tm-natural-compare (tm-comparator->cmpf c))))
                       (cow-publish! self (list->vector (list-sort (lambda (a b) (< (cmp a b) 0)) (cow-list self))))))
                   jolt-nil))
    (cons "forEach" (lambda (self f) (vector-for-each (lambda (x) (jolt-fi-call f "accept" x)) (cow-vec self)) jolt-nil))
    (cons "toArray" (jxc-to-array cow-list))
    (cons "iterator" (lambda (self) (make-jhost "jx-cow-itr" (vector (cow-vec self) 0))))
    (cons "listIterator" (case-lambda
                           ((self) (make-jhost "jx-cow-itr" (vector (cow-vec self) 0)))
                           ((self i) (let ((v (cow-vec self)) (i (jnum->exact i)))
                                       (unless (and (fx>=? i 0) (fx<=? i (vector-length v)))
                                         (throw-jvm 'IndexOutOfBoundsException (string-append "Index: " (number->string i) ", Size: " (number->string (vector-length v)))))
                                       (make-jhost "jx-cow-itr" (vector v i))))))
    (cons "subList" (lambda (self from to)
                      (let ((from (jnum->exact from)) (to (jnum->exact to)) (v (cow-vec self)))
                        (unless (and (fx>=? from 0) (fx<=? to (vector-length v)) (fx<=? from to))
                          (throw-jvm 'IndexOutOfBoundsException jolt-nil))
                        (make-jhost "jx-cow-sublist" (vector self from (fx- to from) v)))))
    (cons "getFirst" (lambda (self) (let ((v (cow-vec self))) (if (fx=? 0 (vector-length v)) (throw-jvm 'NoSuchElementException jolt-nil) (vector-ref v 0)))))
    (cons "getLast" (lambda (self) (let ((v (cow-vec self))) (if (fx=? 0 (vector-length v)) (throw-jvm 'NoSuchElementException jolt-nil) (vector-ref v (fx- (vector-length v) 1))))))
    (cons "removeFirst" (lambda (self) (with-cow self (let ((v (cow-vec self)))
                                                       (when (fx=? 0 (vector-length v)) (throw-jvm 'NoSuchElementException jolt-nil))
                                                       (cow-publish! self (cow-delete v 0)) (vector-ref v 0)))))
    (cons "removeLast" (lambda (self) (with-cow self (let* ((v (cow-vec self)) (n (vector-length v)))
                                                      (when (fx=? 0 n) (throw-jvm 'NoSuchElementException jolt-nil))
                                                      (cow-publish! self (cow-delete v (fx- n 1))) (vector-ref v (fx- n 1))))))
    (cons "addFirst" (lambda (self x) (with-cow self (cow-publish! self (cow-insert (cow-vec self) 0 (list x)))) jolt-nil))
    (cons "addLast" (lambda (self x) (with-cow self (cow-publish! self (cow-insert (cow-vec self) (cow-n self) (list x)))) jolt-nil))
    (cons "reversed" (lambda (self) (make-jhost "jx-reverse-list" (vector self))))
    (cons "clone" (lambda (self) (make-jhost "jx-cow-list" (vector (cow-vec self) (make-monitor)))))))
(jx-class! "jx-cow-list" "java.util.concurrent.CopyOnWriteArrayList" '("java.util.List" "java.util.RandomAccess" "java.lang.Cloneable" "java.io.Serializable"))
(register-host-methods! "jx-cow-list" cow-list-methods)
(jx-register! "jx-cow-list" 'ralist cow-list cow-n #f #f)
(register-class-ctor! "java.util.concurrent.CopyOnWriteArrayList"
  (case-lambda (() (make-jx-cow "jx-cow-list" '()))
               ((c) (make-jx-cow "jx-cow-list" (if (jolt-array? c) (ja->list c) (jxc-elems c))))))
(register-class-ctor! "CopyOnWriteArrayList" (lookup-class class-ctors-tbl "java.util.concurrent.CopyOnWriteArrayList"))
;; COWIterator: a cursor over one published vector. #(vector cursor)
(define (cowi-v it) (vector-ref (jhost-state it) 0))
(define (cowi-i it) (vector-ref (jhost-state it) 1))
(define (cowi-i! it i) (vector-set! (jhost-state it) 1 i))
(jx-class! "jx-cow-itr" "java.util.concurrent.CopyOnWriteArrayList$COWIterator" '("java.util.ListIterator"))
(register-host-methods! "jx-cow-itr"
  (list (cons "hasNext" (lambda (self) (fx<? (cowi-i self) (vector-length (cowi-v self)))))
        (cons "hasPrevious" (lambda (self) (fx>? (cowi-i self) 0)))
        (cons "next" (lambda (self) (let ((i (cowi-i self)))
                                      (unless (fx<? i (vector-length (cowi-v self))) (throw-jvm 'NoSuchElementException jolt-nil))
                                      (cowi-i! self (fx+ i 1)) (vector-ref (cowi-v self) i))))
        (cons "previous" (lambda (self) (let ((i (fx- (cowi-i self) 1)))
                                          (when (fx<? i 0) (throw-jvm 'NoSuchElementException jolt-nil))
                                          (cowi-i! self i) (vector-ref (cowi-v self) i))))
        (cons "nextIndex" (lambda (self) (->num (cowi-i self))))
        (cons "previousIndex" (lambda (self) (->num (fx- (cowi-i self) 1))))
        (cons "remove" (lambda (self) (jx-uoe))) (cons "set" (lambda (self x) (jx-uoe))) (cons "add" (lambda (self x) (jx-uoe)))
        (cons "forEachRemaining" (lambda (self f)
                                   (let loop () (when (fx<? (cowi-i self) (vector-length (cowi-v self)))
                                                  (let ((x (vector-ref (cowi-v self) (cowi-i self))))
                                                    (cowi-i! self (fx+ 1 (cowi-i self))) (jolt-fi-call f "accept" x) (loop))))
                                   jolt-nil))))
(jx-register! "jx-cow-itr" 'iter (lambda (it) (list-tail (vector->list (cowi-v it)) (cowi-i it))) #f #f #f)
;; COWSubList: a window onto the list, which a write to the list from outside it
;; invalidates (ConcurrentModificationException), as the JDK's. #(list offset size expected-vector)
(define (cows-l s) (vector-ref (jhost-state s) 0))
(define (cows-off s) (vector-ref (jhost-state s) 1))
(define (cows-n s) (vector-ref (jhost-state s) 2))
(define (cows-check! s) (unless (eq? (cow-vec (cows-l s)) (vector-ref (jhost-state s) 3)) (throw-jvm 'ConcurrentModificationException jolt-nil)))
(define (cows-resync! s dn) (vector-set! (jhost-state s) 2 (fx+ (cows-n s) dn)) (vector-set! (jhost-state s) 3 (cow-vec (cows-l s))))
(define (cows-elems s) (cows-check! s) (let ((v (cow-vec (cows-l s)))) (let loop ((i (fx- (cows-n s) 1)) (acc '())) (if (fx<? i 0) acc (loop (fx- i 1) (cons (vector-ref v (fx+ (cows-off s) i)) acc))))))
(define (cows-locked s thunk) (with-cow (cows-l s) (cows-check! s) (thunk)))
(jx-class! "jx-cow-sublist" "java.util.concurrent.CopyOnWriteArrayList$COWSubList" '("java.util.List" "java.util.RandomAccess"))
(register-host-methods! "jx-cow-sublist"
  (list (cons "size" (lambda (self) (cows-check! self) (->num (cows-n self))))
        (cons "get" (lambda (self i) (cows-check! self) (let ((i (jnum->exact i))) (cow-index! i (cows-n self)) (vector-ref (cow-vec (cows-l self)) (fx+ (cows-off self) i)))))
        (cons "set" (lambda (self i x) (cows-locked self (lambda () (let ((i (jnum->exact i))) (cow-index! i (cows-n self))
                                                                       (let ((old (jx-call (cows-l self) "set" (list (->num (fx+ (cows-off self) i)) x)))) (cows-resync! self 0) old))))))
        (cons "add" (case-lambda
                      ((self x) (cows-locked self (lambda () (jx-call (cows-l self) "add" (list (->num (fx+ (cows-off self) (cows-n self))) x)) (cows-resync! self 1) #t)))
                      ((self i x) (cows-locked self (lambda () (let ((i (jnum->exact i))) (cow-add-index! i (cows-n self))
                                                                 (jx-call (cows-l self) "add" (list (->num (fx+ (cows-off self) i)) x)) (cows-resync! self 1) jolt-nil))))))
        (cons "remove" (lambda (self x)
                         (cows-locked self
                           (lambda ()
                             (if (and (fixnum? x) (not (jfloat? x)))
                                 (begin (cow-index! x (cows-n self))
                                        (let ((old (jx-call (cows-l self) "remove" (list (->num (fx+ (cows-off self) x)))))) (cows-resync! self -1) old))
                                 (let ((i (cow-index-of (cow-vec (cows-l self)) x (cows-off self) (fx+ (cows-off self) (cows-n self)))))
                                   (if (fx<? i 0) #f (begin (jx-call (cows-l self) "remove" (list (->num i))) (cows-resync! self -1) #t))))))))
        (cons "clear" (lambda (self) (cows-locked self (lambda () (let loop () (when (fx>? (cows-n self) 0) (jx-call (cows-l self) "remove" (list (->num (cows-off self)))) (cows-resync! self -1) (loop))))) jolt-nil))
        (cons "iterator" (lambda (self) (make-jhost "jx-cow-sublist-itr" (vector (list->vector (cows-elems self)) 0))))))
(jx-register! "jx-cow-sublist" 'ralist cows-elems (lambda (s) (cows-check! s) (cows-n s)) #f #f)
(jx-class! "jx-cow-sublist-itr" "java.util.concurrent.CopyOnWriteArrayList$COWSubListIterator" '("java.util.ListIterator"))
(register-host-methods! "jx-cow-sublist-itr" (host-method-entries "jx-cow-itr"))
(jx-register! "jx-cow-sublist-itr" 'iter (lambda (it) (list-tail (vector->list (cowi-v it)) (cowi-i it))) #f #f #f)
;; CopyOnWriteArraySet: a CopyOnWriteArrayList in which add is addIfAbsent.
(define cow-set-methods
  (list
    (cons "size" (lambda (self) (->num (cow-n self))))
    (cons "isEmpty" (lambda (self) (fx=? 0 (cow-n self))))
    (cons "contains" (lambda (self x) (let ((v (cow-vec self))) (fx>=? (cow-index-of v x 0 (vector-length v)) 0))))
    (cons "add" (lambda (self x) (cow-add-if-absent! self x)))
    (cons "addAll" (lambda (self c)
                     (let ((xs (jxc-elems c)))
                       (with-cow self
                         (let* ((v (cow-vec self))
                                (new (fold-left (lambda (acc x) (if (or (fx>=? (cow-index-of v x 0 (vector-length v)) 0) (exists (lambda (y) (objects-equals? x y)) acc)) acc (append acc (list x)))) '() xs)))
                           (unless (null? new) (cow-publish! self (cow-insert v (vector-length v) new)))
                           (pair? new))))))
    (cons "remove" (lambda (self x) (with-cow self (let* ((v (cow-vec self)) (i (cow-index-of v x 0 (vector-length v))))
                                                    (if (fx<? i 0) #f (begin (cow-publish! self (cow-delete v i)) #t))))))
    (cons "clear" (lambda (self) (with-cow self (cow-publish! self (vector))) jolt-nil))
    (cons "removeIf" (lambda (self f) (when (jolt-nil? f) (jx-npe)) (cow-remove-if! self (lambda (x) (jolt-fi-call f "test" x)))))
    (cons "removeAll" (lambda (self c) (when (jolt-nil? c) (jx-npe)) (cow-remove-if! self (lambda (x) (cd-truthy? (cd-call c "contains" x))))))
    (cons "retainAll" (lambda (self c) (when (jolt-nil? c) (jx-npe)) (cow-remove-if! self (lambda (x) (not (cd-truthy? (cd-call c "contains" x)))))))
    (cons "containsAll" (lambda (self c) (let ((v (cow-vec self))) (for-all (lambda (x) (fx>=? (cow-index-of v x 0 (vector-length v)) 0)) (jxc-elems c)))))
    (cons "forEach" (lambda (self f) (vector-for-each (lambda (x) (jolt-fi-call f "accept" x)) (cow-vec self)) jolt-nil))
    (cons "toArray" (jxc-to-array cow-list))
    (cons "iterator" (lambda (self) (make-jhost "jx-cow-itr" (vector (cow-vec self) 0))))))
(jx-class! "jx-cow-set" "java.util.concurrent.CopyOnWriteArraySet" '("java.util.AbstractSet" "java.io.Serializable"))
(register-host-methods! "jx-cow-set" cow-set-methods)
(jx-register! "jx-cow-set" 'set cow-list cow-n
              (lambda (s x) (let ((v (cow-vec s))) (fx>=? (cow-index-of v x 0 (vector-length v)) 0))) #f)
(define (cow-dedupe xs) (reverse (fold-left (lambda (acc x) (if (exists (lambda (y) (objects-equals? x y)) acc) acc (cons x acc))) '() xs)))
(register-class-ctor! "java.util.concurrent.CopyOnWriteArraySet"
  (case-lambda (() (make-jx-cow "jx-cow-set" '())) ((c) (make-jx-cow "jx-cow-set" (cow-dedupe (jxc-elems c))))))
(register-class-ctor! "CopyOnWriteArraySet" (lookup-class class-ctors-tbl "java.util.concurrent.CopyOnWriteArraySet"))

;; ======================================================================================
;; ConcurrentSkipListMap and ConcurrentSkipListSet
;; ======================================================================================
;; tree-map.ss's TreeMap with a monitor in its root (tmr-concurrent?): the same
;; persistent tree, views and navigation, under the JDK's classes. A write — a
;; put, a remove, an iterator's remove, and every compound method whole
;; (putIfAbsent, compute, merge, pollFirstEntry, …) — holds the root's monitor and
;; publishes the new tree with one store behind a release fence; a read takes the
;; published tree with no lock (tools/cow-publish-model.pl). Iteration walks a
;; snapshot, which the JDK's weak consistency allows, and hands out
;; AbstractMap$SimpleImmutableEntry snapshots, as the JDK's iterators do. Nil
;; keys, values and arguments are NullPointerException.
;;
;; Each tree-map.ss tag has a tag here; a call strips it back to the tree-map.ss
;; tag (same state vector), runs that method, and gives what comes back — a view,
;; an iterator, an entry — this file's tag in turn.
(define cs-tag-table
  '(("treemap" "jx-cslm" "ConcurrentSkipListMap" root)
    ("treemap-asc-sub" "jx-cslm-sub" "ConcurrentSkipListMap$SubMap" view)
    ("treemap-desc-sub" "jx-cslm-dsub" "ConcurrentSkipListMap$SubMap" view)
    ("treeset" "jx-csls" "ConcurrentSkipListSet" root)
    ("treemap-keyset" "jx-cslm-keyset" "ConcurrentSkipListMap$KeySet" view)
    ("treemap-values" "jx-cslm-values" "ConcurrentSkipListMap$Values" view)
    ("treemap-submap-values" "jx-cslm-subvalues" "ConcurrentSkipListMap$Values" view)
    ("treemap-entryset" "jx-cslm-entryset" "ConcurrentSkipListMap$EntrySet" view)
    ("treemap-asc-entryset" "jx-cslm-subentryset" "ConcurrentSkipListMap$EntrySet" view)
    ("treemap-desc-entryset" "jx-cslm-dsubentryset" "ConcurrentSkipListMap$EntrySet" view)
    ("treemap-entry-iterator" "jx-cslm-entry-itr" "ConcurrentSkipListMap$EntryIterator" iter)
    ("treemap-key-iterator" "jx-cslm-key-itr" "ConcurrentSkipListMap$KeyIterator" iter)
    ("treemap-value-iterator" "jx-cslm-value-itr" "ConcurrentSkipListMap$ValueIterator" iter)
    ("treemap-submap-entry-iterator" "jx-cslm-sub-entry-itr" "ConcurrentSkipListMap$SubMap$SubMapEntryIterator" iter)
    ("treemap-submap-key-iterator" "jx-cslm-sub-key-itr" "ConcurrentSkipListMap$SubMap$SubMapKeyIterator" iter)
    ("treemap-desc-submap-entry-iterator" "jx-cslm-dsub-entry-itr" "ConcurrentSkipListMap$SubMap$SubMapEntryIterator" iter)
    ("treemap-desc-submap-key-iterator" "jx-cslm-dsub-key-itr" "ConcurrentSkipListMap$SubMap$SubMapKeyIterator" iter)
    ("treemap-submap-value-iterator" "jx-cslm-sub-value-itr" "ConcurrentSkipListMap$SubMap$SubMapValueIterator" iter)))
(define cs-by-base (make-hashtable string-hash string=?))
(define cs-to-base (make-hashtable string-hash string=?))
(for-each (lambda (r) (hashtable-set! cs-by-base (car r) (cadr r)) (hashtable-set! cs-to-base (cadr r) (car r))) cs-tag-table)
(define (cs-base x) (make-jhost (hashtable-ref cs-to-base (jhost-tag x) #f) (jhost-state x)))
(define (cs-snapshot-entry e)
  (make-jhost "immutable-entry" (vector (vector-ref (jhost-state e) 1) (tm-live-entry-value e))))
(define (cs-out r)
  (if (jhost? r)
      (let ((t (jhost-tag r)))
        (cond ((hashtable-ref cs-by-base t #f) => (lambda (cs) (make-jhost cs (jhost-state r))))
              ((string=? t "treemap-entry") (cs-snapshot-entry r))
              (else r)))
      r))
(define (cs-root x) (vector-ref (jhost-state x) 0))
(define cs-writers
  '("put" "putAll" "putIfAbsent" "remove" "clear" "compute" "computeIfAbsent" "computeIfPresent" "merge"
    "replace" "replaceAll" "pollFirstEntry" "pollLastEntry" "pollFirst" "pollLast" "removeFirst" "removeLast" "add" "addAll"
    "removeAll" "retainAll" "removeIf"))
(define (cs-check-args! name args)
  (cond ((string=? name "equals") #f)
        ;; remove(key, value): a nil value matches nothing, and is not an error
        ((and (string=? name "remove") (pair? (cdr args)))
         (when (jolt-nil? (car args)) (jx-npe)))
        ((string=? name "getOrDefault") (when (jolt-nil? (car args)) (jx-npe)))
        (else (for-each (lambda (a) (when (jolt-nil? a) (jx-npe))) args))))
(define (cs-wrap kind name proc)
  (host-arity-like proc
    (if (eq? kind 'iter)
        (lambda (self . args) (cs-out (apply proc (cs-base self) args)))
        (lambda (self . args)
          (cs-check-args! name args)
          (cs-out (if (member name cs-writers)
                      (jolt-call-with-monitor (vector-ref (cs-root self) 4) (lambda () (apply proc (cs-base self) args)))
                      (apply proc (cs-base self) args)))))))
(define (cs-new-root cmp)
  ;; a TreeMap root with the lock in a fifth slot (tmr-concurrent?)
  (let ((r (tm-make-root cmp))) (vector (vector-ref r 0) (vector-ref r 1) (vector-ref r 2) 0 (make-monitor))))
(define (make-cs tag root) (make-jhost tag (vector root tm-unbounded #f tm-unbounded #f #f #f)))
(define (cs-clone x) ; the same persistent tree under a root of its own
  (let ((r (cs-root x))) (make-cs (jhost-tag x) (vector (tmr-sm r) (tmr-cmp-obj r) (tmr-cmpf r) 0 (make-monitor)))))
(for-each
  (lambda (r)
    (let ((base (car r)) (tag (cadr r)) (kind (cadddr r)))
      (jx-class! tag (string-append "java.util.concurrent." (caddr r)))
      (register-host-methods! tag
        (append
          (map (lambda (m) (cons (car m) (cs-wrap kind (car m) (cdr m))))
               (filter (lambda (m) (not (string=? (car m) "clone"))) (host-method-entries base)))
          (if (eq? kind 'root) (list (cons "clone" (lambda (self) (cs-clone self)))) '())))))
  cs-tag-table)
;; what the rest of the runtime sees: = / hash / str / seq / count / get / contains?
(define (cs-base-call f) (lambda (x . args) (apply f (cs-base x) args)))
(for-each
  (lambda (r)
    (let* ((base (car r)) (tag (cadr r)) (kind (cadddr r)))
      (cond
        ((eq? kind 'iter)
         (jx-register! tag 'iter (lambda (it) (map cs-out (map (tmi-ref it 3) (tmi-ref it 1)))) #f #f #f))
        ((member base tm-map-tags)
         ;; seq / first / reduce hand out the JDK's SimpleImmutableEntry snapshots
         (jx-register! tag 'map (lambda (m) (map tm-snapshot-entry (v-entries (cs-base m)))) (cs-base-call v-size) (cs-base-call v-contains?)
                       (lambda (m k d) (let ((x (v-ref (cs-base m) k))) (if (eq? x tm-absent) d x)))))
        ((member base tm-set-tags)
         (jx-register! tag 'set (cs-base-call v-keys) (cs-base-call v-size) (cs-base-call v-contains?) #f))
        ((member base tm-entryset-tags)
         (jx-register! tag 'entries (lambda (v) (map tm-snapshot-entry (v-entries (cs-base v))))
                       (cs-base-call v-size) (cs-base-call tm-entry-in?) #f))
        (else
         (jx-register! tag 'coll (lambda (v) (map e-val (v-entries (cs-base v)))) (cs-base-call v-size) #f #f)))))
  cs-tag-table)
(jch-register-supers! "java.util.concurrent.ConcurrentMap" '("java.util.Map"))
(jch-register-supers! "java.util.concurrent.ConcurrentNavigableMap" '("java.util.concurrent.ConcurrentMap" "java.util.NavigableMap"))
(jch-register-supers! "java.util.concurrent.ConcurrentSkipListMap"
  '("java.util.AbstractMap" "java.util.concurrent.ConcurrentNavigableMap" "java.lang.Cloneable" "java.io.Serializable"))
(jch-register-supers! "java.util.concurrent.ConcurrentSkipListMap$SubMap"
  '("java.util.AbstractMap" "java.util.concurrent.ConcurrentNavigableMap" "java.io.Serializable"))
(jch-register-supers! "java.util.concurrent.ConcurrentSkipListMap$KeySet" '("java.util.AbstractSet" "java.util.NavigableSet"))
(jch-register-supers! "java.util.concurrent.ConcurrentSkipListMap$Values" '("java.util.AbstractCollection"))
(jch-register-supers! "java.util.concurrent.ConcurrentSkipListMap$EntrySet" '("java.util.AbstractSet"))
(jch-register-supers! "java.util.concurrent.ConcurrentSkipListSet"
  '("java.util.AbstractSet" "java.util.NavigableSet" "java.lang.Cloneable" "java.io.Serializable"))
(for-each (lambda (c) (jch-register-supers! (string-append "java.util.concurrent." c) '("java.util.Iterator")))
          '("ConcurrentSkipListMap$EntryIterator" "ConcurrentSkipListMap$KeyIterator" "ConcurrentSkipListMap$ValueIterator"
            "ConcurrentSkipListMap$SubMap$SubMapEntryIterator" "ConcurrentSkipListMap$SubMap$SubMapKeyIterator"
            "ConcurrentSkipListMap$SubMap$SubMapValueIterator"))
(set! tm-publish-fence (lambda () (memory-order-release)))
(set! tm-root-write (lambda (r thunk) (jolt-call-with-monitor (vector-ref r 4) thunk)))
(set! tm-foreign-view (lambda (x) (and (jhost? x) (hashtable-ref cs-to-base (jhost-tag x) #f) (cs-base x))))
(define (cs-sorted-comparator x) (if (or (jx-isa? x "java.util.SortedMap") (jx-isa? x "java.util.SortedSet")) (jx-call x "comparator" '()) #f))
(register-class-ctor! "java.util.concurrent.ConcurrentSkipListMap"
  (case-lambda
    (() (make-cs "jx-cslm" (cs-new-root jolt-nil)))
    ((x) (if (and (not (jolt-map? x)) (not (eq? (jutil-coll-kind x) 'map)) (not (jx-kind x)) (tm-comparator-like? x))
             (make-cs "jx-cslm" (cs-new-root x))
             (let* ((cmp (or (cs-sorted-comparator x) jolt-nil)) (m (make-cs "jx-cslm" (cs-new-root cmp))))
               (for-each (lambda (e) (tmr-put! (cs-root m) (jolt-nth e 0) (jolt-nth e 1))) (jxc-elems x))
               m)))))
(register-class-ctor! "ConcurrentSkipListMap" (lookup-class class-ctors-tbl "java.util.concurrent.ConcurrentSkipListMap"))
(register-class-ctor! "java.util.concurrent.ConcurrentSkipListSet"
  (case-lambda
    (() (make-cs "jx-csls" (cs-new-root jolt-nil)))
    ((x) (if (and (not (jolt-coll? x)) (not (jutil-coll-kind x)) (not (jx-kind x)) (tm-comparator-like? x))
             (make-cs "jx-csls" (cs-new-root x))
             (let* ((cmp (or (cs-sorted-comparator x) jolt-nil)) (s (make-cs "jx-csls" (cs-new-root cmp))))
               (for-each (lambda (k) (when (jolt-nil? k) (jx-npe)) (unless (v-contains? (cs-base s) k) (tmr-put! (cs-root s) k #t))) (jxc-elems x))
               s)))))
(register-class-ctor! "ConcurrentSkipListSet" (lookup-class class-ctors-tbl "java.util.concurrent.ConcurrentSkipListSet"))

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
(let ((seq-view-methods
        (list (cons "sequencedKeySet" (lambda (self) (make-jhost "jx-seq-keyset" (vector self))))
              (cons "sequencedValues" (lambda (self) (make-jhost "jx-seq-values" (vector self))))
              (cons "sequencedEntrySet" (lambda (self) (make-jhost "jx-seq-entryset" (vector self)))))))
  (for-each (lambda (t) (register-host-methods! t seq-view-methods))
            (append tm-map-tags '("jx-cslm" "jx-cslm-sub" "jx-cslm-dsub"))))

;; ======================================================================================
;; reversed() views
;; ======================================================================================
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
(register-host-methods! "jx-cow-list" (jx-reversed-by "jx-cow-reversed"))
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
    (map (lambda (k) (make-jhost tag (vector m k (hashtable-ref t k jolt-nil) (hmn-gen* g k) g)))
         (hm-keys-ordered m))))
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
    ((value) (lambda (v) (map cdr (hm-pairs-ordered (hmv-map v)))))
    (else (lambda (v) (let* ((m (hmv-map v)) (g (hmn-track! m)))
                        (map (lambda (k) (hm-node* m k g)) (hm-keys-ordered m)))))))
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
              (list (cons "keySet" (lambda (self) (make-jhost (string-append (cadr r) "-keyset") (vector self))))
                    (cons "values" (lambda (self) (make-jhost (string-append (cadr r) "-values") (vector self))))
                    (cons "entrySet" (lambda (self) (make-jhost (string-append (cadr r) "-entryset") (vector self)))))))
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
    (cons "jx-set12" (spl-iter-of "java.util.Spliterators$IteratorSpliterator" (fxior spl-SIZED spl-SUBSIZED spl-DISTINCT)))
    ;; a copy-on-write collection's is a snapshot of the array at creation
    (cons "jx-cow-list" (spl-snapshot-of "java.util.Spliterators$ArraySpliterator" (fxior spl-OSS spl-IMMUTABLE)))
    (cons "jx-cow-set" (spl-snapshot-of "java.util.Spliterators$ArraySpliterator" (fxior spl-SIZED spl-SUBSIZED spl-IMMUTABLE spl-DISTINCT)))
    (cons "jx-clq" (lambda (self) (make-iter-spl "java.util.concurrent.ConcurrentLinkedQueue$CLQSpliterator"
                                                 (fxior spl-ORDERED spl-NONNULL spl-CONCURRENT) (spl-it self) spl-long-max)))
    (cons "jx-cld" (lambda (self) (make-iter-spl "java.util.concurrent.ConcurrentLinkedDeque$CLDSpliterator"
                                                 (fxior spl-ORDERED spl-NONNULL spl-CONCURRENT) (spl-it self) spl-long-max)))
    (cons "jx-csls" (lambda (self) (make-vec-spl "java.util.concurrent.ConcurrentSkipListMap$KeySpliterator"
                                                 (fxior spl-DISTINCT spl-SORTED spl-ORDERED spl-NONNULL spl-CONCURRENT)
                                                 (jxc-elems self) (jx-call self "comparator" '()))))
    (cons "jx-cslm-keyset" (lambda (self) (make-vec-spl "java.util.concurrent.ConcurrentSkipListMap$KeySpliterator"
                                                        (fxior spl-DISTINCT spl-SORTED spl-ORDERED spl-NONNULL spl-CONCURRENT)
                                                        (jxc-elems self) (jx-call self "comparator" '()))))
    (cons "jx-cslm-values" (spl-vec-of "java.util.concurrent.ConcurrentSkipListMap$ValueSpliterator"
                                       (fxior spl-ORDERED spl-NONNULL spl-CONCURRENT)))
    (cons "jx-cslm-entryset" (lambda (self) (make-vec-spl "java.util.concurrent.ConcurrentSkipListMap$EntrySpliterator"
                                                          (fxior spl-DISTINCT spl-SORTED spl-ORDERED spl-NONNULL spl-CONCURRENT)
                                                          (jxc-elems self) (spl-entry-cmp (tmr-cmp-obj (cs-root self))))))
    (cons "jx-priority-queue" (spl-vec-of "java.util.PriorityQueue$PriorityQueueSpliterator" (fxior spl-SIZED spl-SUBSIZED spl-NONNULL)))
    (cons "jx-vector" (lambda (self) (make-live-spl "java.util.Vector$VectorSpliterator" spl-OSS self)))
    (cons "jx-stack" (lambda (self) (make-live-spl "java.util.Vector$VectorSpliterator" spl-OSS self)))))
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
