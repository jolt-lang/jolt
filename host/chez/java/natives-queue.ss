;; natives-queue.ss — clojure.lang.PersistentQueue for the Chez host.
;;
;; Clojure's layout: a `front` pvec read from index `fi` (the dequeue end) and
;; a `rear` pvec (the enqueue end, in order). conj appends to rear; peek/first
;; read front[fi]; pop advances fi, and when front runs out the rear BECOMES
;; the front in O(1) — the JVM's f = RT.seq(r). The old layout kept the rear as
;; a reversed list and reversed it on that boundary, and since a queue is
;; persistent, every pop of the same boundary queue paid the O(n) reverse again.
;; A queue is jolt-sequential?, so seq=?/seq-hash give cross-type equality
;; (= [1 2 3] (queue 1 2 3)) for free, like the JVM; its hash is cached like a
;; seq head's (hasheq.ss). Loaded after seq/collections/lazy-bridge/records/
;; host-table so every dispatcher it chains is at its latest binding.

(define-record-type jolt-queue (fields front fi rear cnt (mutable h))
  (nongenerative jolt-queue-v2)
  (protocol (lambda (new) (lambda (f fi r cnt) (new f fi r cnt #f)))))
(define jolt-queue-empty (make-jolt-queue empty-pvec 0 empty-pvec 0))

(define (queue-conj q x)
  (if (fx=? 0 (jolt-queue-cnt q))
      (make-jolt-queue (jolt-vector x) 0 empty-pvec 1)
      (make-jolt-queue (jolt-queue-front q) (jolt-queue-fi q)
                       (pvec-conj (jolt-queue-rear q) x) (fx+ (jolt-queue-cnt q) 1))))
(define (queue-peek q)
  (if (fx=? 0 (jolt-queue-cnt q)) jolt-nil (pvec-nth-in-range (jolt-queue-front q) (jolt-queue-fi q))))
(define (queue-pop q)
  (let ((n (jolt-queue-cnt q)) (fi (fx+ (jolt-queue-fi q) 1)))
    ;; popping an empty PersistentQueue returns it (Clojure's pop: if f==null
    ;; return this) — unlike a vector, which throws.
    (cond ((fx=? n 0) q)
          ((fx<? fi (pvec-cnt (jolt-queue-front q)))
           (make-jolt-queue (jolt-queue-front q) fi (jolt-queue-rear q) (fx- n 1)))
          (else (make-jolt-queue (jolt-queue-rear q) 0 empty-pvec (fx- n 1))))))

;; --- extend the collection dispatchers to see a jolt-queue ------------------
;; The seq realizes the front in blocks of queue-seq-block cells, each block
;; ending in a lazy tail, and moves on to the rear once the front runs out:
;; (seq q) and (first q) are O(1) as on the JVM, and a full walk forces one
;; tail per block rather than per element. The tail is a lazy-src (its first
;; argument the (front . index) pair) so a seq over a queue still travels in a
;; state image.
(define queue-seq-block 32)
(define lz-queue-walk
  (register-lazy-src! 'queue-walk (lambda (fi r) (queue-walk (car fi) (cdr fi) r))))
(define (queue-walk f i r)
  (cond ((fx<? i (pvec-cnt f)) (queue-block f i r queue-seq-block))
        ((fx=? 0 (pvec-cnt r)) jolt-nil)
        (else (queue-walk r 0 empty-pvec))))
(define (queue-block f i r k)
  (let ((x (pvec-nth-in-range f i)) (j (fx+ i 1)))
    (cond ((fx<? j (pvec-cnt f))
           (if (fx=? k 1)
               (cseq-lazy x (make-lazy-src lz-queue-walk (cons f j) r))
               (cseq-realized x (queue-block f j r (fx- k 1)))))
          ((fx=? 0 (pvec-cnt r)) (cseq-realized x jolt-nil))
          (else (cseq-lazy x (make-lazy-src lz-queue-walk (cons r 0) empty-pvec))))))
(define (queue->seq x)
  (if (fx=? 0 (jolt-queue-cnt x)) jolt-nil
      (queue-walk (jolt-queue-front x) (jolt-queue-fi x) (jolt-queue-rear x))))
(register-seq-arm! jolt-queue? queue->seq)
(register-count-arm! jolt-queue? (lambda (x) (jolt-queue-cnt x)))
(register-empty-arm! jolt-queue? (lambda (x) (fx=? 0 (jolt-queue-cnt x))))
(define %q-peek jolt-peek)
(set! jolt-peek (lambda (x) (if (jolt-queue? x) (queue-peek x) (%q-peek x))))
;; pop threads the receiver's meta, as the JVM's PersistentQueue.pop does
;; (popping an empty queue answers the queue itself, meta and all)
(define %q-pop jolt-pop)
(set! jolt-pop (lambda (x) (if (jolt-queue? x) (meta-carry x (queue-pop x)) (%q-pop x))))
(register-conj-arm! jolt-queue? queue-conj)
;; sequential => seq=?/seq-hash handle queue equality + hashing. The hash is
;; cached in the queue (PersistentQueue._hasheq): a queue is immutable, and
;; without the cache every (hash q) re-walked it through a fresh seq.
(define %q-seq-hasheq-cached seq-hasheq-cached)
(set! seq-hasheq-cached
  (lambda (x)
    (if (jolt-queue? x)
        (or (jolt-queue-h x)
            (let ((h (hash-ordered (jolt-seq x)))) (jolt-queue-h-set! x h) h))
        (%q-seq-hasheq-cached x))))
(define %q-sequential? jolt-sequential?)
(set! jolt-sequential? (lambda (x) (or (jolt-queue? x) (%q-sequential? x))))

;; printing: render the elements as a parenthesized list (delegate to the seq path).
(define (jolt-seq-or-empty x) (let ((s (jolt-seq x))) (if (jolt-nil? s) jolt-empty-list s)))
(register-pr-readable-arm! jolt-queue? (lambda (x) (jolt-pr-readable (jolt-seq-or-empty x))))
(register-str-render! jolt-queue? (lambda (x) (jolt-str-render-one (jolt-seq-or-empty x))))

;; class / type / instance? recognize a queue.
(register-class-arm! jolt-queue? (lambda (x) "clojure.lang.PersistentQueue"))
;; its own type tag, as a sorted set and a sorted map have, so tag-keyed dispatch
;; (print-method on __type-tag) tells it apart from an arbitrary host object
(define ty-queue (keyword "jolt" "queue"))
(register-type-arm! jolt-queue? (lambda (x) ty-queue))
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (jolt-queue? val)
        (let ((tn (cond ((string? type-sym) type-sym)
                        ((symbol-t? type-sym) (symbol-t-name type-sym)) (else ""))))
          (and (member (last-dot tn)
                       '("PersistentQueue" "IPersistentCollection" "Sequential" "Collection" "Object"))
               #t))
        'pass)))

;; A queue is an IObj: its meta lives in the identity side table (natives-meta.ss),
;; like a sorted collection's, so with-meta answers a fresh-identity copy (the
;; fields are immutable pvecs; the hash cache restarts empty). conj carries it
;; through meta-carry-conj's side-table arm, pop above, empty below.
(define %q-with-meta jolt-with-meta)
(set! jolt-with-meta
  (lambda (x m)
    (if (jolt-queue? x)
        (if (eq? (jolt-meta x) m)
            x
            (let ((c (make-jolt-queue (jolt-queue-front x) (jolt-queue-fi x)
                                      (jolt-queue-rear x) (jolt-queue-cnt x))))
              (unless (jolt-nil? m) (meta-table-set! c m))
              c))
        (%q-with-meta x m))))
(def-var! "clojure.core" "with-meta" jolt-with-meta)
(register-dot-coll-kind! jolt-queue? 'queue)
;; with-meta, not meta-carry: carry would key the meta onto the shared EMPTY
(register-empty-coll-arm! jolt-queue? (lambda (q) (jolt-with-meta jolt-queue-empty (jolt-meta q))))

;; clojure.lang.PersistentQueue/EMPTY + a queue? predicate.
(register-class-statics! "PersistentQueue" (list (cons "EMPTY" jolt-queue-empty)))
(register-class-statics! "clojure.lang.PersistentQueue" (list (cons "EMPTY" jolt-queue-empty)))
(def-var! "clojure.core" "queue?" (lambda (x) (jolt-queue? x)))
;; the FQ class token self-evaluates to the interned Class object (for
;; (instance? clojure.lang.PersistentQueue …) and (= clojure.lang.PersistentQueue (type q))).
(def-var! "clojure.core" "clojure.lang.PersistentQueue" (jolt-class-for "clojure.lang.PersistentQueue"))

;; PersistentQueue's JVM taxonomy: an IPersistentStack (peek/pop), an ordinary
;; persistent collection, and a meta carrier.
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (and (jolt-queue? val) (symbol-t? type-sym))
        (let* ((tn (symbol-t-name type-sym))
               (short (let loop ((i (- (string-length tn) 1)))
                        (cond ((< i 0) tn)
                              ((char=? (string-ref tn i) #\.) (substring tn (+ i 1) (string-length tn)))
                              (else (loop (- i 1)))))))
          (if (member short '("IPersistentStack" "IPersistentCollection" "IPersistentList"
                              "Collection" "Seqable" "Sequential" "Counted" "IObj" "IMeta"
                              "Iterable"))
              #t 'pass))
        'pass)))
