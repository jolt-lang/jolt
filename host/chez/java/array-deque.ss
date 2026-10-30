;; java.util.ArrayDeque: a ring buffer, as the JDK's is.
;;
;; State #(elements head tail #f): elements is a vector whose empty slots hold
;; nil (an ArrayDeque refuses null elements, so nil is never an element), the
;; live elements run from head up to tail, wrapping, and the buffer is never
;; full: an add that makes head meet tail grows it at once. Both ends are O(1).
;;
;; The layout is the JDK's to the slot — the same initial capacities, the same
;; growth (old + old + 2 below 64, then half again), a wrapped first leg slid to
;; the end on growth, and delete(i) shifting whichever side of i is shorter —
;; because an ArrayDeque has no modCount: what its iterators, forEach and
;; spliterator do when the deque changes under them is read off that layout (a
;; slot found empty, a tail that moved), and only the same layout gives the same
;; answer. tools/failfast-model.pl checks that the ring refines a list and that
;; an iterator's own removes walk it correctly.
;;
;; A deque restored from an image written before the ring has the old 3-slot
;; list state #(backing count head); it converts in place on first use.

(define (dq-inc i n) (let ((j (fx+ i 1))) (if (fx>=? j n) 0 j)))
(define (dq-dec i n) (let ((j (fx- i 1))) (if (fx<? j 0) (fx- n 1) j)))
(define (dq-sub i j n) (let ((k (fx- i j))) (if (fx<? k 0) (fx+ k n) k)))

(define (dq-st self)
  (let ((st (jhost-state self)))
    (if (fx=? (vector-length st) 4)
        st
        ;; the old list state: its count elements from head, in a buffer sized
        ;; as ArrayDeque(Collection) sizes one
        (let* ((v (vector-ref st 0)) (n (vector-ref st 1)) (h (vector-ref st 2))
               (es (make-vector (fxmax 2 (fx+ n 1)) jolt-nil)))
          (do ((i 0 (fx+ i 1))) ((fx=? i n)) (vector-set! es i (vector-ref v (fx+ h i))))
          (let ((nst (vector es 0 n #f))) (jhost-state-set! self nst) nst)))))
(define (dq-es self) (vector-ref (dq-st self) 0))
(define (dq-head self) (vector-ref (dq-st self) 1))
(define (dq-tail self) (vector-ref (dq-st self) 2))
(define (dq-es! self v) (vector-set! (dq-st self) 0 v))
(define (dq-head! self h) (vector-set! (dq-st self) 1 h))
(define (dq-tail! self t) (vector-set! (dq-st self) 2 t))
(define (dq-size self) (dq-sub (dq-tail self) (dq-head self) (vector-length (dq-es self))))

(define (make-array-deque cap) (make-jhost "arraydeque" (vector (make-vector cap jolt-nil) 0 0 #f)))

;; grow by at least NEEDED: double (+2) while small, else half again; a wrapped
;; first leg slides to the end of the new buffer
(define (dq-grow! self needed)
  (let* ((es (dq-es self)) (old (vector-length es))
         (jump (if (fx<? old 64) (fx+ old 2) (fxsrl old 1)))
         (cap (if (fx<? jump needed) (fx+ old needed) (fx+ old jump)))
         (nes (make-vector cap jolt-nil))
         (h (dq-head self)) (t (dq-tail self)))
    (do ((i 0 (fx+ i 1))) ((fx=? i old)) (vector-set! nes i (vector-ref es i)))
    (dq-es! self nes)
    (when (or (fx<? t h) (and (fx=? t h) (not (jolt-nil? (vector-ref nes h)))))
      (let ((space (fx- cap old)))
        (do ((i (fx- old 1) (fx- i 1))) ((fx<? i h)) (vector-set! nes (fx+ i space) (vector-ref nes i)))
        (do ((i h (fx+ i 1))) ((fx=? i (fx+ h space))) (vector-set! nes i jolt-nil))
        (dq-head! self (fx+ h space))))))

(define (dq-npe-if-nil x) (when (jolt-nil? x) (throw-jvm 'NullPointerException jolt-nil)))
(define (dq-add-last! self x)
  (dq-npe-if-nil x)
  (let* ((st (dq-st self)) (es (vector-ref st 0)) (t (vector-ref st 2)))
    (vector-set! es t x)
    (let ((t1 (dq-inc t (vector-length es))))
      (vector-set! st 2 t1)
      (when (fx=? (vector-ref st 1) t1) (dq-grow! self 1)))))
(define (dq-add-first! self x)
  (dq-npe-if-nil x)
  (let* ((es (dq-es self)) (h (dq-dec (dq-head self) (vector-length es))))
    (vector-set! es h x)
    (dq-head! self h)
    (when (fx=? h (dq-tail self)) (dq-grow! self 1))))
(define (dq-poll-first! self)
  (let* ((es (dq-es self)) (h (dq-head self)) (x (vector-ref es h)))
    (unless (jolt-nil? x)
      (vector-set! es h jolt-nil)
      (dq-head! self (dq-inc h (vector-length es))))
    x))
(define (dq-poll-last! self)
  (let* ((es (dq-es self)) (t (dq-dec (dq-tail self) (vector-length es))) (x (vector-ref es t)))
    (unless (jolt-nil? x)
      (vector-set! es t jolt-nil)
      (dq-tail! self t))
    x))
(define (dq-peek-first self) (vector-ref (dq-es self) (dq-head self)))
(define (dq-peek-last self) (let ((es (dq-es self))) (vector-ref es (dq-dec (dq-tail self) (vector-length es)))))
(define (dq-need x) (when (jolt-nil? x) (throw-jvm 'NoSuchElementException jolt-nil)) x)

;; remove the element at physical index I, shifting the shorter side; #t when
;; the back half moved left (an iterator then steps its cursor back)
(define (dq-delete! self i)
  (let* ((es (dq-es self)) (n (vector-length es)) (h (dq-head self)) (t (dq-tail self))
         (front (dq-sub i h n)) (back (fx- (dq-sub t i n) 1)))
    (if (fx<? front back)
        (begin
          (let loop ((j i)) (unless (fx=? j h) (let ((p (dq-dec j n))) (vector-set! es j (vector-ref es p)) (loop p))))
          (vector-set! es h jolt-nil)
          (dq-head! self (dq-inc h n))
          #f)
        (let ((t1 (dq-dec t n)))
          (let loop ((j i)) (unless (fx=? j t1) (let ((q (dq-inc j n))) (vector-set! es j (vector-ref es q)) (loop q))))
          (vector-set! es t1 jolt-nil)
          (dq-tail! self t1)
          #t))))

;; the live elements, head to tail
(define (dq-elems self)
  ;; backward from tail, consing, so the list comes out head first; the wrap is
  ;; two straight runs rather than a modulus per element
  (let* ((st (dq-st self)) (es (vector-ref st 0)) (h (vector-ref st 1)) (t (vector-ref st 2)))
    (define (run from to acc) ; elements [from, to) consed in front of acc
      (let loop ((i (fx- to 1)) (acc acc)) (if (fx<? i from) acc (loop (fx- i 1) (cons (vector-ref es i) acc)))))
    (if (fx<=? h t) (run h t '()) (run h (vector-length es) (run 0 t '())))))

;; Walk physical indices FROM up to END over ES, wrapping, as the JDK's loops do
(define (dq-each-index es from end f)
  (let ((n (vector-length es)))
    (let loop ((i from)) (unless (fx=? i end) (f i) (loop (dq-inc i n))))))

(define (dq-remove-first-occurrence! self o)
  (and (not (jolt-nil? o))
       (let ((es (dq-es self)) (end (dq-tail self)))
         (call/cc (lambda (k)
                    (dq-each-index es (dq-head self) end
                      (lambda (i) (when (objects-equals? o (vector-ref es i)) (dq-delete! self i) (k #t))))
                    #f)))))
(define (dq-remove-last-occurrence! self o)
  (and (not (jolt-nil? o))
       (let* ((es (dq-es self)) (n (vector-length es)) (h (dq-head self)))
         (let loop ((i (dq-tail self)))
           (if (fx=? i h) #f
               (let ((j (dq-dec i n)))
                 (if (objects-equals? o (vector-ref es j)) (begin (dq-delete! self j) #t) (loop j))))))))
(define (dq-contains? self o)
  (and (not (jolt-nil? o)) (exists (lambda (x) (objects-equals? o x)) (dq-elems self)) #t))

;; forEach: the elements as the buffer holds them while the walk runs (a slot
;; emptied under it reads nil), then a tail that moved is a CME
(define (dq-for-each self f)
  (when (jolt-nil? f) (throw-jvm 'NullPointerException jolt-nil))
  (let ((es (dq-es self)) (end (dq-tail self)))
    (dq-each-index es (dq-head self) end (lambda (i) (jolt-fi-call f "accept" (vector-ref es i))))
    (unless (fx=? end (dq-tail self)) (throw-jvm 'ConcurrentModificationException jolt-nil))
    jolt-nil))

;; removeIf / removeAll / retainAll: the JDK's bulkRemove. A first pass tests
;; up to the tail read at the start; nothing matching and a tail that moved is
;; a CME. At the first match bulkRemoveModified takes over: it reads the buffer
;; and the tail AGAIN, so an element the predicate added before then is tested
;; too, marks the rest, compacts the survivors over the buffer, and only then
;; checks the tail (a CME leaves the buffer half compacted, as the JDK's does).
;; Its death row is sized by the distance from the first match to that tail; an
;; empty one (the add grew the buffer and the tail wrapped onto the match) is the
;; JDK's ArrayIndexOutOfBoundsException.
(define (dq-bulk-remove! self test)
  (let* ((es (dq-es self)) (end (dq-tail self)) (h (dq-head self)))
    (let loop ((i h) (to (if (fx<=? h end) end (vector-length es))))
      (cond ((fx<? i to)
             (if (jolt-truthy? (test (vector-ref es i)))
                 (dq-bulk-remove-modified! self test i)
                 (loop (fx+ i 1) to)))
            ((fx=? to end)
             (unless (fx=? end (dq-tail self)) (throw-jvm 'ConcurrentModificationException jolt-nil))
             #f)
            (else (loop 0 end))))))
(define (dq-bulk-remove-modified! self test beg)
  (let* ((es (dq-es self)) (cap (vector-length es)) (end (dq-tail self))
         (len (dq-sub end beg cap))
         (dead (make-vector len #f)))
    (when (fx=? len 0)
      (throw-jvm 'ArrayIndexOutOfBoundsException "Index 0 out of bounds for length 0"))
    (vector-set! dead 0 #t)
    ;; mark: i walks the legs, k makes i - k the distance from beg
    (let mark ((i (fx+ beg 1)) (to (if (fx<=? (fx+ beg 1) end) end cap)) (k beg))
      (let run ((i i))
        (if (fx<? i to)
            (begin (when (jolt-truthy? (test (vector-ref es i))) (vector-set! dead (fx- i k) #t))
                   (run (fx+ i 1)))
            (unless (fx=? to end) (mark 0 end (fx- k cap))))))
    ;; compact: hare i reads, tortoise w writes
    (let ((w beg) (i (fx+ beg 1)) (to (if (fx<=? (fx+ beg 1) end) end cap)) (k beg))
      (define (clear? i) (not (vector-ref dead (fx- i k))))
      (let outer ()
        (let same-leg () ; i and w on one leg
          (when (fx<? i to)
            (when (clear? i) (vector-set! es w (vector-ref es i)) (set! w (fx+ w 1)))
            (set! i (fx+ i 1))
            (same-leg)))
        (unless (fx=? to end)
          ;; w on the first leg, i on the second
          (set! i 0) (set! to end) (set! k (fx- k cap))
          (let cross ()
            (when (and (fx<? i to) (fx<? w cap))
              (when (clear? i) (vector-set! es w (vector-ref es i)) (set! w (fx+ w 1)))
              (set! i (fx+ i 1))
              (cross)))
          (if (fx>=? i to)
              (when (fx=? w cap) (set! w 0))
              (begin (set! w 0) (outer)))))
      (unless (fx=? end (dq-tail self)) (throw-jvm 'ConcurrentModificationException jolt-nil))
      (dq-tail! self w)
      (dq-each-index es w end (lambda (i) (vector-set! es i jolt-nil)))
      #t)))
(define (dq-clear! self)
  (let ((es (dq-es self))) (dq-each-index es (dq-head self) (dq-tail self) (lambda (i) (vector-set! es i jolt-nil))))
  (dq-head! self 0) (dq-tail! self 0)
  jolt-nil)
;; The JDK adds through c.forEach. A deque added to itself walks to the tail it
;; started with, adding each element at the end, and then finds its tail moved:
;; every element is in twice and the call is a ConcurrentModificationException.
(define (dq-add-all! self c)
  (when (jolt-nil? c) (throw-jvm 'NullPointerException jolt-nil))
  (let* ((xs (seq->list (jolt-seq c))) (s (dq-size self))
         (needed (fx- (fx+ s (length xs) 1) (vector-length (dq-es self)))))
    (when (fx>? needed 0) (dq-grow! self needed))
    (for-each (lambda (x) (dq-add-last! self x)) xs)
    (when (and (eq? c self) (pair? xs)) (throw-jvm 'ConcurrentModificationException jolt-nil))
    (fx>? (dq-size self) s)))

;; The list layout took a nil ((.add d nil) was accepted before the ring), but
;; the ring marks an empty slot with nil, so a deque holding one cannot become a
;; ring: reading it from an image is refused rather than losing the element.
(register-image-read-check! '("arraydeque")
  (lambda (d)
    (let ((st (jhost-state d)))
      (and (fx=? (vector-length st) 3)
           (let ((v (vector-ref st 0)) (n (vector-ref st 1)) (h (vector-ref st 2)))
             (let loop ((i 0))
               (cond ((fx=? i n) #f)
                     ((jolt-nil? (vector-ref v (fx+ h i)))
                      "it holds nil, which an ArrayDeque cannot (an image written before format 14 let one in)")
                     (else (loop (fx+ i 1))))))))))

;; ---- iterators ---------------------------------------------------------------
;; State #(deque cursor remaining lastRet descending?). remaining is fixed when
;; the iterator is made: an element added later is not handed out, and a slot
;; found empty is a CME.
(define (make-dq-iterator self desc?)
  (let ((es (dq-es self)))
    (make-jhost (if desc? "jx-arraydeque-desc-iterator" "jx-arraydeque-iterator")
                (vector self (if desc? (dq-dec (dq-tail self) (vector-length es)) (dq-head self))
                        (dq-size self) -1 desc?))))
(define (dqi-ref it i) (vector-ref (jhost-state it) i))
(define (dqi-set! it i v) (vector-set! (jhost-state it) i v))
(define (dqi-next it)
  ;; the hot step: one read of the iterator's state and of the deque's
  (let ((st (jhost-state it)))
    (when (fx<=? (vector-ref st 2) 0) (throw-jvm 'NoSuchElementException jolt-nil))
    (let* ((es (vector-ref (dq-st (vector-ref st 0)) 0)) (n (vector-length es)) (c (vector-ref st 1)) (x (vector-ref es c)))
      (when (jolt-nil? x) (throw-jvm 'ConcurrentModificationException jolt-nil))
      (vector-set! st 3 c)
      (vector-set! st 1 (if (vector-ref st 4)
                            (if (fx=? c 0) (fx- n 1) (fx- c 1))
                            (let ((j (fx+ c 1))) (if (fx=? j n) 0 j))))
      (vector-set! st 2 (fx- (vector-ref st 2) 1))
      x)))
;; iterator-seq's chunk fill: the slots it would read checked first, so a
;; hole (a CME at that next) leaves the loop to throw where it would
(define (dqi-fill it buf)
  (let* ((st (jhost-state it)) (es (vector-ref (dq-st (vector-ref st 0)) 0)) (cap (vector-length es))
         (desc? (vector-ref st 4)) (c0 (vector-ref st 1)) (n (fxmin 32 (vector-ref st 2))))
    (define (step c) (if desc? (if (fx=? c 0) (fx- cap 1) (fx- c 1)) (let ((j (fx+ c 1))) (if (fx=? j cap) 0 j))))
    (and (fx>=? n 0)
         (let check ((j 0) (c c0))
           (cond ((fx=? j n) #t)
                 ((jolt-nil? (vector-ref es c)) #f)
                 (else (check (fx+ j 1) (step c)))))
         (let copy ((j 0) (c c0) (last (vector-ref st 3)))
           (if (fx=? j n)
               (begin (vector-set! st 1 c) (vector-set! st 3 last) (vector-set! st 2 (fx- (vector-ref st 2) n)) n)
               (begin (vector-set! buf j (vector-ref es c)) (copy (fx+ j 1) (step c) c)))))))
(define (dqi-remove it)
  (when (fx<? (dqi-ref it 3) 0) (throw-jvm 'IllegalStateException jolt-nil))
  (let* ((d (dqi-ref it 0)) (left? (dq-delete! d (dqi-ref it 3))) (n (vector-length (dq-es d))))
    ;; the cursor follows the half that moved toward it
    (if (dqi-ref it 4)
        (unless left? (dqi-set! it 1 (dq-inc (dqi-ref it 1) n)))
        (when left? (dqi-set! it 1 (dq-dec (dqi-ref it 1) n))))
    (dqi-set! it 3 -1)
    jolt-nil))
(define (dqi-for-each-remaining it f)
  (when (jolt-nil? f) (throw-jvm 'NullPointerException jolt-nil))
  (let ((r (dqi-ref it 2)))
    (when (fx>? r 0)
      (dqi-set! it 2 0)
      (let* ((d (dqi-ref it 0)) (es (dq-es d)) (n (vector-length es)) (c (dqi-ref it 1)))
        (if (dqi-ref it 4)
            (let ((end (dq-head d)))
              (when (or (jolt-nil? (vector-ref es c)) (not (fx=? (fx+ (dq-sub c end n) 1) r)))
                (throw-jvm 'ConcurrentModificationException jolt-nil))
              (let loop ((i c))
                (jolt-fi-call f "accept" (vector-ref es i))
                (unless (fx=? i end) (loop (dq-dec i n))))
              (unless (fx=? end (dq-head d)) (throw-jvm 'ConcurrentModificationException jolt-nil))
              (dqi-set! it 3 end))
            (let ((end (dq-tail d)))
              (when (or (jolt-nil? (vector-ref es c)) (not (fx=? (dq-sub end c n) r)))
                (throw-jvm 'ConcurrentModificationException jolt-nil))
              (dq-each-index es c end (lambda (i) (jolt-fi-call f "accept" (vector-ref es i))))
              (unless (fx=? end (dq-tail d)) (throw-jvm 'ConcurrentModificationException jolt-nil))
              (dqi-set! it 3 (dq-dec end n))))))
    jolt-nil))
(define dq-iterator-methods
  (list (cons "hasNext" (lambda (self) (fx>? (vector-ref (jhost-state self) 2) 0)))
        (cons "next" dqi-next)
        (cons "remove" dqi-remove)
        (cons "forEachRemaining" dqi-for-each-remaining)))
(for-each (lambda (r)
            (jx-class! (car r) (cadr r) '("java.util.Iterator"))
            (register-host-methods! (car r) dq-iterator-methods)
            (jx-register! (car r) 'iter
                          (lambda (it)
                            ;; what it has not handed out yet, as the buffer holds it
                            (let* ((d (dqi-ref it 0)) (es (dq-es d)) (n (vector-length es)))
                              (let loop ((c (dqi-ref it 1)) (r (dqi-ref it 2)) (acc '()))
                                (if (fx<=? r 0) (reverse acc)
                                    (loop (if (dqi-ref it 4) (dq-dec c n) (dq-inc c n)) (fx- r 1) (cons (vector-ref es c) acc))))))
                          #f #f #f))
          '(("jx-arraydeque-iterator" "java.util.ArrayDeque$DeqIterator")
            ("jx-arraydeque-desc-iterator" "java.util.ArrayDeque$DescendingIterator")))
(register-iterator-seq-fill! '("jx-arraydeque-iterator" "jx-arraydeque-desc-iterator") dqi-fill dqi-next)
(register-image-refusal! '("jx-arraydeque-iterator" "jx-arraydeque-desc-iterator")
                         "an iterator is a position in a walk this process is making; write the collection instead")

;; ---- the spliterator -----------------------------------------------------------
;; ArrayDeque$DeqSpliterator: late-binding (fence -1 until first use), a split
;; hands off the first half, and a slot found empty is a CME.
;; State #(deque cursor fence)
(define (make-dq-spl d cursor fence) (make-jhost "jx-deq-spl" (vector d cursor fence)))
(define (dqs-bind! s)
  (let ((st (jhost-state s)))
    (when (fx<? (vector-ref st 2) 0)
      (vector-set! st 2 (dq-tail (vector-ref st 0)))
      (vector-set! st 1 (dq-head (vector-ref st 0))))
    (vector-ref st 2)))
(jx-class! "jx-deq-spl" "java.util.ArrayDeque$DeqSpliterator" '("java.util.Spliterator"))
(register-image-refusal! '("jx-deq-spl") "a spliterator is a position in a walk this process is making; write the collection instead")
(register-host-methods! "jx-deq-spl"
  (list (cons "tryAdvance"
              (lambda (self f)
                (when (jolt-nil? f) (throw-jvm 'NullPointerException jolt-nil))
                (let* ((fence (dqs-bind! self)) (st (jhost-state self)) (d (vector-ref st 0))
                       (es (dq-es d)) (i (vector-ref st 1)))
                  (if (fx=? i fence) #f
                      (let ((x (vector-ref es i)))
                        (when (jolt-nil? x) (throw-jvm 'ConcurrentModificationException jolt-nil))
                        (vector-set! st 1 (dq-inc i (vector-length es)))
                        (jolt-fi-call f "accept" x)
                        #t)))))
        (cons "forEachRemaining"
              (lambda (self f)
                (when (jolt-nil? f) (throw-jvm 'NullPointerException jolt-nil))
                (let* ((end (dqs-bind! self)) (st (jhost-state self)) (c (vector-ref st 1))
                       (es (dq-es (vector-ref st 0))))
                  (unless (fx=? c end)
                    (vector-set! st 1 end)
                    (when (or (jolt-nil? (vector-ref es c)) (jolt-nil? (vector-ref es (dq-dec end (vector-length es)))))
                      (throw-jvm 'ConcurrentModificationException jolt-nil))
                    (dq-each-index es c end (lambda (i) (jolt-fi-call f "accept" (vector-ref es i))))))
                jolt-nil))
        (cons "trySplit"
              (lambda (self)
                (let* ((fence (dqs-bind! self)) (st (jhost-state self)) (d (vector-ref st 0))
                       (n (vector-length (dq-es d))) (i (vector-ref st 1)) (half (fxsrl (dq-sub fence i n) 1)))
                  (if (fx<=? half 0) jolt-nil
                      (let ((mid (fxmod (fx+ i half) n)))
                        (vector-set! st 1 mid)
                        (make-dq-spl d i mid))))))
        (cons "estimateSize" (lambda (self)
                               (let* ((fence (dqs-bind! self)) (st (jhost-state self)))
                                 (->num (dq-sub fence (vector-ref st 1) (vector-length (dq-es (vector-ref st 0))))))))
        (cons "getExactSizeIfKnown" (lambda (self) (jx-call self "estimateSize" '())))
        (cons "characteristics" (lambda (self) (->num 16720)))
        (cons "hasCharacteristics" (lambda (self c) (let ((c (jnum->exact c))) (fx=? (fxand 16720 c) c))))
        (cons "getComparator" (lambda (self) (throw-jvm 'IllegalStateException jolt-nil)))))

;; ---- the deque -------------------------------------------------------------------
(define (dq-clone self)
  (let ((st (dq-st self)))
    (make-jhost "arraydeque" (vector (vector-copy (vector-ref st 0)) (vector-ref st 1) (vector-ref st 2) #f))))
(register-host-methods! "arraydeque"
  (list
    (cons "addFirst" (lambda (self x) (dq-add-first! self x) jolt-nil))
    (cons "addLast" (lambda (self x) (dq-add-last! self x) jolt-nil))
    (cons "offerFirst" (lambda (self x) (dq-add-first! self x) #t))
    (cons "offerLast" (lambda (self x) (dq-add-last! self x) #t))
    (cons "add" (lambda (self x) (dq-add-last! self x) #t))
    (cons "offer" (lambda (self x) (dq-add-last! self x) #t))
    (cons "push" (lambda (self x) (dq-add-first! self x) jolt-nil))
    (cons "pollFirst" dq-poll-first!)
    (cons "pollLast" dq-poll-last!)
    (cons "poll" dq-poll-first!)
    (cons "removeFirst" (lambda (self) (dq-need (dq-poll-first! self))))
    (cons "removeLast" (lambda (self) (dq-need (dq-poll-last! self))))
    (cons "pop" (lambda (self) (dq-need (dq-poll-first! self))))
    (cons "remove" (case-lambda
                     ((self) (dq-need (dq-poll-first! self)))
                     ((self o) (dq-remove-first-occurrence! self o))))
    (cons "peekFirst" dq-peek-first)
    (cons "peekLast" dq-peek-last)
    (cons "peek" dq-peek-first)
    (cons "getFirst" (lambda (self) (dq-need (dq-peek-first self))))
    (cons "getLast" (lambda (self) (dq-need (dq-peek-last self))))
    (cons "element" (lambda (self) (dq-need (dq-peek-first self))))
    (cons "removeFirstOccurrence" dq-remove-first-occurrence!)
    (cons "removeLastOccurrence" dq-remove-last-occurrence!)
    (cons "contains" dq-contains?)
    (cons "size" (lambda (self) (->num (dq-size self))))
    (cons "isEmpty" (lambda (self) (fx=? (dq-head self) (dq-tail self))))
    (cons "clear" dq-clear!)
    (cons "addAll" dq-add-all!)
    (cons "removeIf" (lambda (self p) (dq-bulk-remove! self (lambda (x) (jolt-fi-call p "test" x)))))
    (cons "removeAll" (lambda (self c) (dq-npe-if-nil c) (dq-bulk-remove! self (lambda (x) (cd-has? c x)))))
    (cons "retainAll" (lambda (self c) (dq-npe-if-nil c) (dq-bulk-remove! self (lambda (x) (not (cd-has? c x))))))
    (cons "forEach" dq-for-each)
    (cons "iterator" (lambda (self) (make-dq-iterator self #f)))
    (cons "descendingIterator" (lambda (self) (make-dq-iterator self #t)))
    (cons "spliterator" (lambda (self) (make-dq-spl self 0 -1)))
    (cons "toArray" (lambda (self . args) (jcoll-to-array (dq-elems self) args)))
    (cons "clone" dq-clone)
    ;; an ArrayDeque keeps Object's identity equals and hashCode
    (cons "equals" (lambda (self o) (eq? self o)))
    (cons "hashCode" (lambda (self) (->num (jolt-identity-hasheq self))))))
(register-jutil-coll! "arraydeque" 'coll dq-elems)
(jch-register-supers! "java.util.ArrayDeque" '("java.util.AbstractCollection" "java.util.Deque" "java.lang.Cloneable" "java.io.Serializable"))
(define-jhost-tag-pred (array-deque? t) (string=? t "arraydeque"))
(register-seq-arm! array-deque? (lambda (x) (let ((xs (dq-elems x))) (if (null? xs) jolt-nil (list->cseq xs)))))
(register-count-arm! array-deque? dq-size)
(let ((ctor (lambda args
              (cond ((null? args) (make-array-deque 17))
                    ((number? (car args)) (let ((n (jnum->exact (car args)))) (make-array-deque (if (fx<? n 1) 1 (fx+ n 1)))))
                    ;; ArrayDeque(Collection): a buffer of size+1, filled in order
                    (else (let* ((xs (seq->list (jolt-seq (car args)))) (n (length xs))
                                 (es (make-vector (fx+ n 1) jolt-nil)))
                            (let loop ((i 0) (xs xs))
                              (unless (null? xs) (dq-npe-if-nil (car xs)) (vector-set! es i (car xs)) (loop (fx+ i 1) (cdr xs))))
                            (make-jhost "arraydeque" (vector es 0 n #f))))))))
  (register-class-ctor! "ArrayDeque" ctor)
  (register-class-ctor! "java.util.ArrayDeque" ctor))
