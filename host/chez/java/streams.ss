;; streams.ss — java.util.stream for the Chez host.
;;
;; Clojure 1.12 reads a Stream through stream-seq! / stream-reduce! /
;; stream-transduce! / stream-into! (all over its .iterator), and Java APIs hand
;; streams back, so a Stream here is a real value: a one-shot pipeline over a
;; lazy jolt seq. An intermediate op (map, filter, limit, …) consumes its stream
;; and answers a new one over the transformed seq; a terminal op (forEach,
;; collect, reduce, iterator, …) consumes it and answers a value. A second use of
;; a consumed stream raises the JVM's IllegalStateException.
;;
;; The function argument of every op is a java.util.function interface, so it
;; goes through jolt-fi-call (records-dispatch.ss): a Clojure fn is invoked, and a
;; reify of the interface has its method called by name. The method name depends
;; on the stream's element kind — an IntStream's map takes an IntUnaryOperator,
;; whose method is applyAsInt — which is why the kind rides on the tag.
;;
;; Loaded after host-static-classes.ss (Optional, the ArrayList/HashSet shims,
;; jcoll-*), natives-array.ss (arrays) and lazy-bridge.ss (jolt-make-lazy-seq).

;; tag -> element kind. The tag is also the class model's handle (class-hierarchy.ss
;; jhost-tag->fqn): a Stream is a ReferencePipeline, an IntStream an IntPipeline.
(define stream-tags '(("stream" . ref) ("int-stream" . int) ("long-stream" . long)
                      ("double-stream" . double)))
(define-jhost-tag-pred (stream? t) (and (assoc t stream-tags) #t))
(define (stream-kind s) (cdr (assoc (jhost-tag s) stream-tags)))
(define (kind-tag k) (car (find (lambda (p) (eq? (cdr p) k)) stream-tags)))
;; state: #(seq consumed? close-handlers source origin ops): source is the
;; collection a Collection.stream() came from, whose own spliterator a head
;; stream hands back. origin and ops describe the pipeline for Stream.spliterator
;; (jutil-concurrent.ss): origin is what the head was made from — (coll . c),
;; (array . n), (range . n), (iterate), (generate) or #f — and ops the
;; intermediate operations since, newest first, each a symbol or (op . arg).
(define (make-stream kind seq) (make-jhost (kind-tag kind) (vector seq #f '() #f #f '())))
(define (make-stream-from kind seq origin)
  (let ((s (make-stream kind seq))) (vector-set! (jhost-state s) 4 origin) s))
(define (stream-source s) (vector-ref (jhost-state s) 3))
(define (stream-origin s) (vector-ref (jhost-state s) 4))
(define (stream-ops s) (vector-ref (jhost-state s) 5))

;; Take a stream's elements, marking it used: every op, intermediate or
;; terminal, links or consumes the stream it is called on exactly once.
(define (stream-take! s)
  (let ((st (jhost-state s)))
    (when (vector-ref st 1)
      (throw-jvm 'IllegalStateException "stream has already been operated upon or closed"))
    (vector-set! st 1 #t)
    (vector-ref st 0)))
;; An intermediate op: a new stream of KIND over (f seq), carrying the close
;; handlers along so closing the end of a pipeline runs them all.
(define (stream-derive s kind f . op)
  ;; OP names the operation for the pipeline description (none: it changes
  ;; nothing a spliterator reports)
  (let* ((st (jhost-state s)) (handlers (vector-ref st 2))
         (n (make-stream kind (f (stream-take! s)))) (nst (jhost-state n)))
    (vector-set! nst 2 handlers)
    (vector-set! nst 4 (or (vector-ref st 4) (and (vector-ref st 3) (cons 'coll (vector-ref st 3)))))
    (vector-set! nst 5 (if (pair? op) (cons (car op) (vector-ref st 5)) (cons 'same (vector-ref st 5))))
    n))

;; ---- the pipeline: one element at a time ----------------------------------------
;; A JDK stream pulls each element through every stage before the next one, and
;; a short-circuiting terminal (anyMatch, findFirst, limit) stops pulling. The
;; stages here are lazy seqs that realize ONE element per cell, whatever the
;; source's chunking: map/filter over a vector would otherwise run a whole
;; 32-element chunk through one stage before the next saw any of it.
(define (st-lazy thunk) (jolt-make-lazy-seq thunk))
(define (st-cell x rest) (make-cseq x rest sk-cons jolt-nil))
;; The rest of a cell WITHOUT realizing it: a stage's cell holds its rest as a
;; lazy seq, and pulling that is the next element's work, not this one's.
(define (st-rest q)
  (if (cseq? q)
      (let ((t (cseq-tail q))) (if (jolt-lazyseq? t) t (jolt-rest q)))
      (jolt-rest q)))
(define (st-map f xs)
  (st-lazy (lambda ()
             (let ((q (jolt-seq xs)))
               (if (jolt-nil? q) jolt-nil
                   (st-cell (f (jolt-first q)) (st-map f (st-rest q))))))))
(define (st-filter keep? xs)
  (st-lazy (lambda ()
             (let loop ((q (jolt-seq xs)))
               (cond ((jolt-nil? q) jolt-nil)
                     ((keep? (jolt-first q)) (st-cell (jolt-first q) (st-filter keep? (st-rest q))))
                     (else (loop (jolt-seq (st-rest q)))))))))
;; limit(n): after the n-th element nothing more is pulled
(define (st-take n xs)
  (if (<= n 0)
      jolt-nil
      (st-lazy (lambda ()
                 (let ((q (jolt-seq xs)))
                   (if (jolt-nil? q) jolt-nil
                       (st-cell (jolt-first q) (st-take (- n 1) (st-rest q)))))))))
(define (st-drop n xs)
  (st-lazy (lambda ()
             (let loop ((n n) (q (jolt-seq xs)))
               (if (or (<= n 0) (jolt-nil? q)) q (loop (- n 1) (jolt-seq (st-rest q))))))))
(define (st-take-while keep? xs)
  (st-lazy (lambda ()
             (let ((q (jolt-seq xs)))
               (if (or (jolt-nil? q) (not (keep? (jolt-first q)))) jolt-nil
                   (st-cell (jolt-first q) (st-take-while keep? (st-rest q))))))))
(define (st-drop-while drop? xs)
  (st-lazy (lambda ()
             (let loop ((q (jolt-seq xs)))
               (if (and (not (jolt-nil? q)) (drop? (jolt-first q))) (loop (jolt-seq (st-rest q))) q)))))
;; flatMap: each element's stream (or seqable) in turn
(define (st-append inner rest)
  (st-lazy (lambda ()
             (let ((q (jolt-seq inner)))
               (if (jolt-nil? q) (jolt-seq rest)
                   (st-cell (jolt-first q) (st-append (st-rest q) rest)))))))
(define (st-flat-map f xs)
  (st-lazy (lambda ()
             (let loop ((q (jolt-seq xs)))
               (if (jolt-nil? q) jolt-nil
                   (let ((inner (jolt-seq (f (jolt-first q)))))
                     (if (jolt-nil? inner)
                         (loop (jolt-seq (st-rest q)))
                         (jolt-seq (st-append inner (st-flat-map f (st-rest q)))))))))))
(define (st-distinct xs)
  (let ((seen (make-hashtable hm-hash jolt=2)))
    (let walk ((xs xs))
      (st-lazy (lambda ()
                 (let loop ((q (jolt-seq xs)))
                   (cond ((jolt-nil? q) jolt-nil)
                         ((hashtable-contains? seen (jolt-first q)) (loop (jolt-seq (st-rest q))))
                         (else (hashtable-set! seen (jolt-first q) #t)
                               (st-cell (jolt-first q) (walk (st-rest q)))))))))))

;; Terminal walks. stream-fold pulls every element in order, so the stages'
;; side effects interleave per element as the JDK's do; stream-find stops at the
;; first element PRED accepts and answers it boxed in a list, or #f.
(define (stream-fold s acc f)
  (let loop ((q (jolt-seq (stream-take! s))) (acc acc))
    (if (jolt-nil? q) acc (loop (jolt-seq (jolt-rest q)) (f acc (jolt-first q))))))
(define (stream-list! s) (reverse (stream-fold s '() (lambda (acc x) (cons x acc)))))
(define (stream-find s pred)
  (let loop ((q (jolt-seq (stream-take! s))))
    (cond ((jolt-nil? q) #f)
          ((pred (jolt-first q)) (list (jolt-first q)))
          (else (loop (jolt-seq (jolt-rest q)))))))

;; The functional-interface method names per kind, for the ops whose argument
;; type follows the stream's element type.
(define (kind-unary-method k)
  (case k ((int) "applyAsInt") ((long) "applyAsLong") ((double) "applyAsDouble") (else "apply")))
(define (kind-binary-method k) (kind-unary-method k))
;; A primitive stream's element as the JVM holds it: an int/long stream's are
;; integers, a double stream's doubles.
(define (kind-coerce k v)
  (case k
    ((int long) (if (and (number? v) (not (exact? v))) (exact (truncate v)) v))
    ((double) (if (number? v) (inexact v) v))
    (else v)))
(define (clj f) (var-deref "clojure.core" f))

(define (stream-map s method kind)
  (lambda (f)
    (stream-derive s kind
      (lambda (xs) (st-map (lambda (x) (kind-coerce kind (jolt-fi-call f method x))) xs))
      'map)))

(define (stream-sum k s)
  (let ((t (stream-fold s 0 +)))
    (if (eq? k 'double) (inexact t) t)))

(define (stream-reduce s args)
  (let ((k (stream-kind s)))
    (cond
      ;; reduce(BinaryOperator) -> Optional
      ((null? (cdr args))
       (let ((r (stream-fold s #f (lambda (acc x)
                                    (if acc
                                        (list (jolt-fi-call (car args) (kind-binary-method k) (car acc) x))
                                        (list x))))))
         (if r (jt-optional #t (car r)) jt-optional-empty)))
      ;; reduce(identity, accumulator [, combiner]) — sequential, so the
      ;; combiner never runs
      (else
       (stream-fold s (car args) (lambda (a x) (jolt-fi-call (cadr args) (kind-binary-method k) a x)))))))

;; ---- Collectors ---------------------------------------------------------------
;; A Collector is a finisher over the element list; collect(Collector) applies it.
;; groupingBy / partitioningBy hand each group's list to their downstream
;; collector's finisher, which is the JDK's composition.
(define (make-collector f) (make-jhost "stream-collector" f))
(define (collector? x) (and (jhost? x) (string=? (jhost-tag x) "stream-collector")))
(define (collector-finish c xs) ((jhost-state c) xs))
(define (joining . args)
  (let ((sep (if (pair? args) (jolt-str-render-one (car args)) ""))
        (pre (if (and (pair? args) (pair? (cdr args))) (jolt-str-render-one (cadr args)) ""))
        (suf (if (and (pair? args) (pair? (cdr args))) (jolt-str-render-one (caddr args)) "")))
    (make-collector
     (lambda (xs)
       (string-append
        pre
        (if (null? xs) ""
            (fold-left (lambda (a x) (string-append a sep (jolt-str-render-one x)))
                       (jolt-str-render-one (car xs)) (cdr xs)))
        suf)))))
(define (collector-to-list) (make-collector (lambda (xs) (make-arraylist xs))))
(define (st-put! m k v) (record-method-dispatch m "put" (list->cseq (list k v))))
(define (st-get m k) (record-method-dispatch m "get" (list->cseq (list k))))
(define (st-contains-key? m k) (jolt-truthy? (record-method-dispatch m "containsKey" (list->cseq (list k)))))
;; Group XS by (classify x) in encounter order: an alist of (key . reversed-members).
(define (st-group classify xs)
  (let ((tbl (make-hashtable hm-hash jolt=2)) (order '()))
    (for-each (lambda (x)
                (let ((k (classify x)))
                  (unless (hashtable-contains? tbl k) (set! order (cons k order)))
                  (hashtable-update! tbl k (lambda (l) (cons x l)) '())))
              xs)
    (map (lambda (k) (cons k (reverse (hashtable-ref tbl k '())))) (reverse order))))
;; groupingBy(classifier [, mapFactory] [, downstream]): a HashMap of key -> the
;; downstream's result over that key's elements (toList's ArrayList by default).
(define collectors-grouping-by
  (case-lambda
    ((f) (collectors-grouping-by f (collector-to-list)))
    ((f down) (collectors-grouping-by f (lambda () (host-new "java.util.HashMap")) down))
    ((f factory down)
     (make-collector
      (lambda (xs)
        (let ((m (jolt-fi-call factory "get")))
          (for-each (lambda (g) (st-put! m (car g) (collector-finish down (cdr g))))
                    (st-group (lambda (x) (jolt-fi-call f "apply" x)) xs))
          m))))))
;; partitioningBy(predicate [, downstream]): {false …, true …}, both keys always
(define collectors-partitioning-by
  (case-lambda
    ((p) (collectors-partitioning-by p (collector-to-list)))
    ((p down)
     (make-collector
      (lambda (xs)
        (let ((yes (filter (lambda (x) (jolt-truthy? (jolt-fi-call p "test" x))) xs))
              (no (filter (lambda (x) (not (jolt-truthy? (jolt-fi-call p "test" x)))) xs))
              (m (host-new "java.util.HashMap")))
          (st-put! m #f (collector-finish down no))
          (st-put! m #t (collector-finish down yes))
          m))))))
;; toMap(keyMapper, valueMapper [, merge [, mapFactory]]): a duplicate key with
;; no merge function is the JDK's IllegalStateException.
(define collectors-to-map
  (case-lambda
    ((kf vf) (collectors-to-map kf vf #f (lambda () (host-new "java.util.HashMap"))))
    ((kf vf merge) (collectors-to-map kf vf merge (lambda () (host-new "java.util.HashMap"))))
    ((kf vf merge factory)
     (make-collector
      (lambda (xs)
        (let ((m (jolt-fi-call factory "get")))
          (for-each
           (lambda (x)
             (let ((k (jolt-fi-call kf "apply" x)) (v (jolt-fi-call vf "apply" x)))
               (if (st-contains-key? m k)
                   (let ((old (st-get m k)))
                     (if merge
                         (st-put! m k (jolt-fi-call merge "apply" old v))
                         (throw-jvm 'IllegalStateException
                                    (string-append "Duplicate key " (jolt-str-render-one k)
                                                   " (attempted merging values "
                                                   (jolt-str-render-one old) " and "
                                                   (jolt-str-render-one v) ")"))))
                   (st-put! m k v))))
           xs)
          m))))))
(let ((statics
       (list (cons "toList" collector-to-list)
             (cons "toUnmodifiableList" (lambda () (make-collector (lambda (xs) (make-immutable-list xs)))))
             (cons "toSet" (lambda () (make-collector (lambda (xs) (host-new "java.util.HashSet" (list->cseq xs))))))
             (cons "toUnmodifiableSet" (lambda () (make-collector (lambda (xs) (apply jolt-hash-set xs)))))
             (cons "counting" (lambda () (make-collector (lambda (xs) (length xs)))))
             (cons "joining" joining)
             (cons "groupingBy" collectors-grouping-by)
             (cons "partitioningBy" collectors-partitioning-by)
             (cons "toMap" collectors-to-map))))
  (register-class-statics! "Collectors" statics)
  (register-class-statics! "java.util.stream.Collectors" statics))

;; collect(Collector) or collect(supplier, accumulator, combiner)
(define (stream-collect s args)
  (cond
    ((and (pair? args) (null? (cdr args)) (collector? (car args)))
     (collector-finish (car args) (stream-list! s)))
    ((fx=? (length args) 3)
     (let ((acc (jolt-fi-call (car args) "get")))
       (stream-fold s #f (lambda (_ x) (jolt-fi-call (cadr args) "accept" acc x)))
       acc))
    (else (throw-jvm 'IllegalArgumentException "collect expects a Collector or (supplier accumulator combiner)"))))

(define (stream-to-array s args)
  (let ((xs (stream-list! s)) (k (stream-kind s)))
    (if (pair? args)
        ;; toArray(IntFunction<A[]> generator): the generator sizes the array
        (let ((arr (jolt-fi-call (car args) "apply" (length xs))))
          (let loop ((i 0) (xs xs))
            (unless (null? xs) (ja-set! arr i (car xs)) (loop (fx+ i 1) (cdr xs))))
          arr)
        (make-jolt-array (list->vector xs) (case k ((int) 'int) ((long) 'long) ((double) 'double) (else 'object))))))

(define (stream-min-max s args pick)
  (let ((less? (cmp->less (if (pair? args) (car args) jolt-compare))))
    (let ((r (stream-fold s #f (lambda (best x) (if best (list (pick less? (car best) x)) (list x))))))
      (if r (jt-optional #t (car r)) jt-optional-empty))))

;; ---- IntSummaryStatistics & co ---------------------------------------------------
;; state #(count sum min max); an empty one's min/max are the type's extremes.
(define (summary-tag k)
  (case k ((int) "int-summary-stats") ((long) "long-summary-stats") (else "double-summary-stats")))
(define (summary-of k xs)
  (let ((st (case k
              ((int) (vector 0 0 2147483647 -2147483648))
              ((long) (vector 0 0 9223372036854775807 -9223372036854775808))
              (else (vector 0 0.0 +inf.0 -inf.0)))))
    (for-each (lambda (x)
                (vector-set! st 0 (+ 1 (vector-ref st 0)))
                (vector-set! st 1 (+ x (vector-ref st 1)))
                (vector-set! st 2 (if (< x (vector-ref st 2)) x (vector-ref st 2)))
                (vector-set! st 3 (if (> x (vector-ref st 3)) x (vector-ref st 3))))
              xs)
    (make-jhost (summary-tag k) st)))
(define (summary-average self)
  (let ((st (jhost-state self)))
    (if (fx=? 0 (vector-ref st 0)) 0.0 (inexact (/ (vector-ref st 1) (vector-ref st 0))))))
(define (summary-methods name int?)
  (let ((fmt (lambda (x) (jolt-invoke (clj "format") "%f" (inexact x)))))
    (list (cons "getCount" (lambda (self) (vector-ref (jhost-state self) 0)))
          (cons "getSum" (lambda (self) (vector-ref (jhost-state self) 1)))
          (cons "getMin" (lambda (self) (vector-ref (jhost-state self) 2)))
          (cons "getMax" (lambda (self) (vector-ref (jhost-state self) 3)))
          (cons "getAverage" summary-average)
          (cons "toString"
                (lambda (self)
                  (let* ((st (jhost-state self))
                         (num (lambda (x) (if int? (number->string x) (fmt x)))))
                    (string-append name "{count=" (number->string (vector-ref st 0))
                                   ", sum=" (num (vector-ref st 1))
                                   ", min=" (num (vector-ref st 2))
                                   ", average=" (fmt (summary-average self))
                                   ", max=" (num (vector-ref st 3)) "}")))))))
(register-host-methods! "int-summary-stats" (summary-methods "IntSummaryStatistics" #t))
(register-host-methods! "long-summary-stats" (summary-methods "LongSummaryStatistics" #t))
(register-host-methods! "double-summary-stats" (summary-methods "DoubleSummaryStatistics" #f))
(register-str-render! (lambda (x) (and (jhost? x) (member (jhost-tag x) '("int-summary-stats" "long-summary-stats" "double-summary-stats")) #t))
                      (lambda (x) (record-method-dispatch x "toString" jolt-nil)))

;; mapMulti(BiConsumer<T, Consumer<R>>): the mapper pushes any number of results
;; into the sink it is handed; they follow in order.
(define (make-stream-sink) (make-jhost "stream-sink" (vector '())))
(register-host-methods! "stream-sink"
  (list (cons "accept" (lambda (self x)
                         (vector-set! (jhost-state self) 0 (cons x (vector-ref (jhost-state self) 0)))
                         jolt-nil))))

(define (stream-close! s)
  (let ((hs (vector-ref (jhost-state s) 2)))
    (vector-set! (jhost-state s) 2 '())
    (vector-set! (jhost-state s) 1 #t)
    (for-each (lambda (h) (jolt-fi-call h "run")) (reverse hs))
    jolt-nil))

(define (st-test p) (lambda (x) (jolt-truthy? (jolt-fi-call p "test" x))))
(define (stream-methods)
  (list
   ;; --- intermediate ------------------------------------------------------
   (cons "map" (lambda (s f) ((stream-map s (kind-unary-method (stream-kind s)) (stream-kind s)) f)))
   (cons "mapToObj" (lambda (s f) ((stream-map s "apply" 'ref) f)))
   (cons "mapToInt" (lambda (s f) ((stream-map s "applyAsInt" 'int) f)))
   (cons "mapToLong" (lambda (s f) ((stream-map s "applyAsLong" 'long) f)))
   (cons "mapToDouble" (lambda (s f) ((stream-map s "applyAsDouble" 'double) f)))
   (cons "boxed" (lambda (s) (stream-derive s 'ref (lambda (xs) xs))))
   (cons "asLongStream" (lambda (s) (stream-derive s 'long (lambda (xs) xs))))
   ;; a widening keeps every flag (the JDK's asDoubleStream / asLongStream)
   (cons "asDoubleStream" (lambda (s) (stream-derive s 'double (lambda (xs) (st-map inexact xs)) 'same)))
   (cons "filter" (lambda (s p) (stream-derive s (stream-kind s) (lambda (xs) (st-filter (st-test p) xs)) 'filter)))
   (cons "flatMap" (lambda (s f)
                     (stream-derive s (stream-kind s)
                       (lambda (xs)
                         (st-flat-map (lambda (x)
                                        (let ((r (jolt-fi-call f "apply" x)))
                                          (if (stream? r) (stream-take! r) r)))
                                      xs))
                       'flat)))
   (cons "mapMulti" (lambda (s f)
                      (stream-derive s (stream-kind s)
                        (lambda (xs)
                          (st-flat-map (lambda (x)
                                         (let ((sink (make-stream-sink)))
                                           (jolt-fi-call f "accept" x sink)
                                           (list->cseq (reverse (vector-ref (jhost-state sink) 0)))))
                                       xs))
                        'flat)))
   (cons "peek" (lambda (s f)
                  (stream-derive s (stream-kind s)
                    (lambda (xs) (st-map (lambda (x) (jolt-fi-call f "accept" x) x) xs))
                    'same)))
   (cons "limit" (lambda (s n) (stream-derive s (stream-kind s) (lambda (xs) (st-take (jnum->exact n) xs)) (cons 'limit (jnum->exact n)))))
   (cons "skip" (lambda (s n) (stream-derive s (stream-kind s) (lambda (xs) (st-drop (jnum->exact n) xs)) (cons 'skip (jnum->exact n)))))
   (cons "distinct" (lambda (s) (stream-derive s (stream-kind s) st-distinct 'distinct)))
   (cons "sorted" (lambda (s . cmp)
                    (stream-derive s (stream-kind s)
                      (lambda (xs) (list->cseq (jcoll-sorted (seq->list (jolt-seq xs))
                                                             (if (pair? cmp) (car cmp) jolt-nil))))
                      (if (and (pair? cmp) (not (jolt-nil? (car cmp)))) 'sorted-by 'sorted))))
   (cons "takeWhile" (lambda (s p) (stream-derive s (stream-kind s) (lambda (xs) (st-take-while (st-test p) xs)) 'filter)))
   (cons "dropWhile" (lambda (s p) (stream-derive s (stream-kind s) (lambda (xs) (st-drop-while (st-test p) xs)) 'filter)))
   ;; sequential execution is all there is, so these are the stream itself
   (cons "sequential" (lambda (s) s)) (cons "parallel" (lambda (s) s))
   (cons "unordered" (lambda (s) (stream-derive s (stream-kind s) (lambda (xs) xs) 'unordered)))
   (cons "isParallel" (lambda (s) #f))
   (cons "onClose" (lambda (s h)
                     (vector-set! (jhost-state s) 2 (cons h (vector-ref (jhost-state s) 2)))
                     s))
   (cons "close" stream-close!)
   ;; --- terminal ------------------------------------------------------------
   (cons "iterator" (lambda (s) (make-jiterator (jolt-seq (stream-take! s)))))
   (cons "forEach" (lambda (s f) (stream-fold s jolt-nil (lambda (_ x) (jolt-fi-call f "accept" x) jolt-nil))))
   (cons "forEachOrdered" (lambda (s f) (stream-fold s jolt-nil (lambda (_ x) (jolt-fi-call f "accept" x) jolt-nil))))
   ;; Stream.toList is an unmodifiable List (ImmutableCollections$ListN)
   (cons "toList" (lambda (s) (make-immutable-list (stream-list! s))))
   (cons "toArray" (lambda (s . gen) (stream-to-array s gen)))
   (cons "collect" (lambda (s . args) (stream-collect s args)))
   (cons "reduce" (lambda (s . args) (stream-reduce s args)))
   (cons "count" (lambda (s) (stream-fold s 0 (lambda (n _) (+ n 1)))))
   (cons "sum" (lambda (s) (stream-sum (stream-kind s) s)))
   (cons "average" (lambda (s)
                     (let ((r (stream-fold s (cons 0 0) (lambda (a x) (cons (+ (car a) x) (+ (cdr a) 1))))))
                       (if (eqv? 0 (cdr r)) jt-optional-empty
                           (jt-optional #t (inexact (/ (car r) (cdr r))))))))
   (cons "summaryStatistics" (lambda (s) (summary-of (stream-kind s) (stream-list! s))))
   (cons "min" (lambda (s . c) (stream-min-max s c (lambda (less? b x) (if (less? x b) x b)))))
   (cons "max" (lambda (s . c) (stream-min-max s c (lambda (less? b x) (if (less? b x) x b)))))
   ;; the short-circuiting terminals stop at the first deciding element
   (cons "anyMatch" (lambda (s p) (and (stream-find s (st-test p)) #t)))
   (cons "allMatch" (lambda (s p) (not (stream-find s (lambda (x) (not ((st-test p) x)))))))
   (cons "noneMatch" (lambda (s p) (not (stream-find s (st-test p)))))
   (cons "findFirst" (lambda (s)
                       (let ((r (stream-find s (lambda (x) #t))))
                         (if r (jt-optional #t (car r)) jt-optional-empty))))
   (cons "findAny" (lambda (s)
                     (let ((r (stream-find s (lambda (x) #t))))
                       (if r (jt-optional #t (car r)) jt-optional-empty))))))
(for-each (lambda (p) (register-host-methods! (car p) (stream-methods))) stream-tags)

;; CharSequence.chars / codePoints: an IntStream of the characters. jolt's
;; strings hold code points, so the two answer the same. String's arms
;; (natives-str.ss) and the builders' both come here.
(define (string->int-stream s)
  (make-stream 'int (list->cseq (map char->integer (string->list s)))))
(for-each (lambda (tag)
            (register-host-methods! tag
              (list (cons "chars" (lambda (self) (string->int-stream (sb-str* self))))
                    (cons "codePoints" (lambda (self) (string->int-stream (sb-str* self)))))))
          '("string-builder" "string-buffer"))
;; a stream seqs and reduces like the iterator it is, so (seq s) / (into [] s)
;; and a reify over it see its elements (consuming it, as any iteration does)
(register-seq-arm! stream? (lambda (s) (jolt-seq (stream-take! s))))

;; The primitive streams' optionals read through getAsInt / getAsLong /
;; getAsDouble (OptionalInt and its siblings), which is what an IntStream's
;; min / max / findFirst / average hand back here.
(let ((get (lambda (o) (if (opt-present? o) (opt-value o) (throw-jvm 'NoSuchElementException "No value present")))))
  (register-host-methods! "optional"
    (list (cons "getAsInt" get) (cons "getAsLong" get) (cons "getAsDouble" get))))

;; ---- sources -----------------------------------------------------------------
;; Stream.of(T... values): one array argument IS the values, as the varargs
;; arrive from Clojure; anything else is the elements spelled out.
(define (stream-of kind)
  (lambda args
    (let ((xs (if (and (pair? args) (null? (cdr args)) (jolt-array? (car args))) (ja->list (car args)) args)))
      (make-stream-from kind (list->cseq xs) (cons 'array (length xs))))))
(define (stream-iterate kind)
  (case-lambda
    ((seed f) (make-stream-from kind (jolt-iterate (lambda (x) (jolt-fi-call f (kind-unary-method kind) x)) seed) '(iterate)))
    ;; iterate(seed, hasNext, next) — the for-loop form (JDK 9)
    ((seed has-next f)
     (make-stream-from kind
       (st-take-while (lambda (x) (jolt-truthy? (jolt-fi-call has-next "test" x)))
                      (jolt-iterate (lambda (x) (jolt-fi-call f (kind-unary-method kind) x)) seed))
       '(iterate)))))
(define (stream-concat kind)
  (lambda (a b) (make-stream kind (jolt-concat (stream-take! a) (stream-take! b)))))
(define (stream-range closed?)
  (lambda (kind)
    (lambda (from to)
      (let ((from (jnum->exact from)) (to (jnum->exact to)))
        (let ((to (if closed? (+ to 1) to)))
          (make-stream-from kind (jolt-invoke2 (clj "range") from to) (cons 'range (max 0 (- to from)))))))))
(define (stream-statics kind)
  (append
   (list (cons "of" (stream-of kind))
         (cons "empty" (lambda () (make-stream-from kind jolt-nil '(empty))))
         (cons "iterate" (stream-iterate kind))
         (cons "generate" (lambda (f)
                            (make-stream-from kind (jolt-invoke1 (clj "repeatedly")
                                                                 (lambda () (jolt-fi-call f "get")))
                                              '(generate))))
         (cons "concat" (stream-concat kind)))
   (if (eq? kind 'ref)
       (list (cons "ofNullable" (lambda (x) (make-stream 'ref (if (jolt-nil? x) jolt-nil (list->cseq (list x)))))))
       (list (cons "range" ((stream-range #f) kind))
             (cons "rangeClosed" ((stream-range #t) kind))))))
;; One member list per class, registered under both spellings: a fresh closure
;; per spelling re-registers each member with a different value, which the
;; boot's registry-drift check reports.
(for-each
 (lambda (p)
   (let ((members (stream-statics (cdr p))))
     (register-class-statics! (car p) members)
     (register-class-statics! (string-append "java.util.stream." (car p)) members)))
 '(("Stream" . ref) ("IntStream" . int) ("LongStream" . long) ("DoubleStream" . double)))

;; Arrays.stream(array [from to]): an int[]/long[]/double[] is a primitive
;; stream, any other array a Stream.
(register-class-statics! "java.util.Arrays"
  (list (cons "stream"
              (lambda (arr . range)
                (let* ((xs (ja->list arr))
                       (xs (if (pair? range)
                               (let ((from (jnum->exact (car range))) (to (jnum->exact (cadr range))))
                                 (list-head (list-tail xs from) (- to from)))
                               xs)))
                  (make-stream-from (case (jolt-array-kind arr)
                                      ((int) 'int) ((long) 'long) ((double) 'double) (else 'ref))
                                    (list->cseq xs) (cons 'array (length xs))))))))

;; Collection.stream() on every collection that is a java.util.Collection — the
;; persistent vector, set and list and every seq, and the ArrayList / HashSet
;; family. A map is not a Collection (it has no stream method on the JVM), and a
;; string is not either. Lazy: a seq source is streamed without realizing it.
(define (stream-source? x)
  (or (pvec? x) (pset? x) (jolt-seq? x) (jolt-lazyseq? x) (al-family? x) (hs-hashset? x)
      (jutil-collection? x)))
;; A list that keeps a modCount (ArrayList, LinkedList, Vector) streams live, as
;; its late-binding spliterator does on the JDK: the size and count are taken
;; when the stream first pulls an element, the elements are read as the list
;; stands, and a count that moved by the end of the walk is a
;; ConcurrentModificationException.
(define (stream-live-list l)
  ;; an ArrayList / LinkedList reads its backing directly; a Vector through its
  ;; own size and get. The elements bound at the first pull go out as one chunk
  ;; (the walk reads the list as it stands then), and the count is checked once
  ;; the chunk is used up, before the stream ends.
  (let* ((al? (al-family? l))
         (size (lambda () (if al? (al-cnt l) (jnum->exact (record-method-dispatch l "size" jolt-nil)))))
         (get (lambda (i) (if al? (al-ref l i) (record-method-dispatch l "get" (list->cseq (list (->num i))))))))
    (jolt-make-lazy-seq
      (lambda ()
        (let* ((n (size)) (e (jutil-modcount l))
               (xs (let loop ((i (fx- n 1)) (acc '())) (if (fx<? i 0) acc (loop (fx- i 1) (cons (get i) acc))))))
          ;; realized cells, then one lazy tail that makes the check when the
          ;; walk reaches it
          (let ((end (jolt-make-lazy-seq
                       (lambda () (unless (eqv? e (jutil-modcount l)) (jutil-cme!)) jolt-nil))))
            (fold-right (lambda (x rest) (make-cseq x rest sk-cons jolt-nil)) end xs)))))))
;; Any other collection that keeps a modCount — the hash and tree sets and the
;; map views — streams as its spliterator walks on the JDK: a HashMap's or a
;; TreeMap's binds its elements at the first pull and checks the count once the
;; walk ends; a LinkedHashMap's or LinkedHashSet's (an IteratorSpliterator)
;; walks the collection's fail-fast iterator one element at a time.
(define (stream-list-source? x)
  (or (al-family? x) (cd-sublist? x) (and (jhost? x) (member (jhost-tag x) '("jx-vector" "jx-stack")) #t)))
(define (stream-iterator-seq it)
  (let ((has? (iterator-seq-method it "hasNext")) (next (iterator-seq-method it "next")))
    (let loop ()
      (jolt-make-lazy-seq
        (lambda ()
          (if (jolt-truthy? (has? it))
              (let ((x (next it))) (make-cseq x (loop) sk-cons jolt-nil))
              jolt-nil))))))
(define (stream-live-coll c)
  (jolt-make-lazy-seq
    (lambda ()
      (if (lh-kind c)
          (stream-iterator-seq (record-method-dispatch c "iterator" jolt-nil))
          (let* ((e (jutil-modcount c))
                 (xs (seq->list (jolt-seq c)))
                 (end (jolt-make-lazy-seq
                        (lambda () (unless (eqv? e (jutil-modcount c)) (jutil-cme!)) jolt-nil))))
            (fold-right (lambda (x rest) (make-cseq x rest sk-cons jolt-nil)) end xs))))))
;; any other java.util shim that is a Collection (not a Map)
(define (jutil-collection? x) (and (memq (jutil-coll-kind x) '(set ralist list coll entries)) #t))
(define arm-priority-stream 29)
(register-method-arm! arm-priority-stream
  (lambda (obj name rest)
    (if (and (or (string=? name "stream") (string=? name "parallelStream"))
             (stream-source? obj))
        (if (null? (method-rest-args->list rest))
            (let ((st (make-stream 'ref (if (jutil-modcount obj)
                                            (if (stream-list-source? obj) (stream-live-list obj) (stream-live-coll obj))
                                            (if (or (al-family? obj) (hs-hashset? obj) (jutil-collection? obj))
                                            ;; a mutable source is snapshotted, as the JDK's
                                            ;; late-binding spliterator is by the time it runs
                                            (list->cseq (seq->list (jolt-seq obj)))
                                            obj)))))
              (vector-set! (jhost-state st) 3 obj)
              st)
            (dispatch-miss obj name (method-rest-args->list rest)))
        'pass)))
