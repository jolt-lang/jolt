;; natives-queue.ss — clojure.lang.PersistentQueue for the Chez host.
;;
;; A functional queue: a `front` Scheme list (the dequeue end, head = front of the
;; queue) + a reversed `rear` Scheme list (the enqueue end, head = most recent).
;; conj adds to rear; peek/first read front; pop drops the front, rebalancing
;; rear->front when front empties — amortized O(1). A queue is jolt-sequential?, so
;; seq=?/seq-hash give cross-type equality (= [1 2 3] (queue 1 2 3)) for free, like
;; the JVM. Loaded after seq/collections/lazy-bridge/records/host-table so every
;; dispatcher it chains is at its latest binding.

(define-record-type jolt-queue (fields front rear cnt) (nongenerative jolt-queue-v1))
(define jolt-queue-empty (make-jolt-queue '() '() 0))

(define (queue-conj q x)
  (if (null? (jolt-queue-front q))
      (make-jolt-queue (list x) '() (fx+ (jolt-queue-cnt q) 1))
      (make-jolt-queue (jolt-queue-front q) (cons x (jolt-queue-rear q)) (fx+ (jolt-queue-cnt q) 1))))
(define (queue->list q) (append (jolt-queue-front q) (reverse (jolt-queue-rear q))))
(define (queue-peek q) (if (null? (jolt-queue-front q)) jolt-nil (car (jolt-queue-front q))))
(define (queue-pop q)
  (let ((f (jolt-queue-front q)))
    (cond ((null? f) (error 'pop "can't pop empty queue"))
          ((null? (cdr f)) (make-jolt-queue (reverse (jolt-queue-rear q)) '() (fx- (jolt-queue-cnt q) 1)))
          (else (make-jolt-queue (cdr f) (jolt-queue-rear q) (fx- (jolt-queue-cnt q) 1))))))

;; --- extend the collection dispatchers to see a jolt-queue ------------------
(define %q-seq jolt-seq)
(set! jolt-seq (lambda (x) (if (jolt-queue? x)
                               (let ((l (queue->list x))) (if (null? l) jolt-nil (list->cseq l)))
                               (%q-seq x))))
(define %q-count jolt-count)
(set! jolt-count (lambda (x) (if (jolt-queue? x) (jolt-queue-cnt x) (%q-count x))))
(define %q-empty? jolt-empty?)
(set! jolt-empty? (lambda (x) (if (jolt-queue? x) (fx=? 0 (jolt-queue-cnt x)) (%q-empty? x))))
(define %q-peek jolt-peek)
(set! jolt-peek (lambda (x) (if (jolt-queue? x) (queue-peek x) (%q-peek x))))
(define %q-pop jolt-pop)
(set! jolt-pop (lambda (x) (if (jolt-queue? x) (queue-pop x) (%q-pop x))))
(define %q-conj1 jolt-conj1)
(set! jolt-conj1 (lambda (coll x) (if (jolt-queue? coll) (queue-conj coll x) (%q-conj1 coll x))))
;; sequential => seq=?/seq-hash handle queue equality + hashing.
(define %q-sequential? jolt-sequential?)
(set! jolt-sequential? (lambda (x) (or (jolt-queue? x) (%q-sequential? x))))

;; printing: render the elements as a parenthesized list (delegate to the seq path).
(define (jolt-seq-or-empty x) (let ((s (jolt-seq x))) (if (jolt-nil? s) jolt-empty-list s)))
(register-pr-readable-arm! jolt-queue? (lambda (x) (jolt-pr-readable (jolt-seq-or-empty x))))
(register-str-render! jolt-queue? (lambda (x) (jolt-str-render-one (jolt-seq-or-empty x))))

;; class / type / instance? recognize a queue.
(register-class-arm! jolt-queue? (lambda (x) "clojure.lang.PersistentQueue"))
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (jolt-queue? val)
        (let ((tn (cond ((string? type-sym) type-sym)
                        ((symbol-t? type-sym) (symbol-t-name type-sym)) (else ""))))
          (and (member (last-dot tn)
                       '("PersistentQueue" "IPersistentCollection" "Sequential" "Collection" "Object"))
               #t))
        'pass)))

;; clojure.lang.PersistentQueue/EMPTY + a queue? predicate.
(register-class-statics! "PersistentQueue" (list (cons "EMPTY" jolt-queue-empty)))
(register-class-statics! "clojure.lang.PersistentQueue" (list (cons "EMPTY" jolt-queue-empty)))
(def-var! "clojure.core" "queue?" (lambda (x) (jolt-queue? x)))
;; the FQ class token self-evaluates (for (instance? clojure.lang.PersistentQueue …)).
(def-var! "clojure.core" "clojure.lang.PersistentQueue" "clojure.lang.PersistentQueue")
