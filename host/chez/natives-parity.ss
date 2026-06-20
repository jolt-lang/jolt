;; natives-parity.ss (jolt-cf1q.7) — native Chez shims for clojure.core fns that
;; live in the Janet seed (src/jolt/core*.janet) but had no Chez shim, so on the
;; zero-Janet spine they resolved to nil ("not a fn"). Pure-Chez, JVM-matching.
;;
;; Loaded after host-table.ss (htable-sorted?), transients.ss (jolt-transient?),
;; values.ss (jolt-hash), seq.ss (jolt-seq/seq->list/list->cseq/jolt-invoke).

;; --- hash family (mirrors core_extra.janet: 24-bit masked so int? holds) ------
(define (np-h24 x) (bitwise-and (jolt-hash x) #xffffff))
(define (np-hash x) (np-h24 x))
(define (np-hash-combine a b)
  (bitwise-and (bitwise-xor (np-h24 a) (+ (np-h24 b) #x9e3779)) #xffffff))
(define (np-hash-ordered-coll coll)
  (let loop ((xs (seq->list (jolt-seq coll))) (h 1))
    (if (null? xs) h (loop (cdr xs) (bitwise-and (+ (* 31 h) (np-h24 (car xs))) #xffffff)))))
(define (np-hash-unordered-coll coll)
  (let loop ((xs (seq->list (jolt-seq coll))) (h 0))
    (if (null? xs) h (loop (cdr xs) (bitwise-and (+ h (np-h24 (car xs))) #xffffff)))))

;; --- transient? ---------------------------------------------------------------
(define (np-transient? x) (jolt-transient? x))

;; --- rseq: vectors + sorted colls only (Clojure), reverse of the ascending seq.
(define (np-rseq coll)
  (if (or (pvec? coll) (htable-sorted? coll))
      (list->cseq (reverse (seq->list (jolt-seq coll))))
      (jolt-throw (jolt-ex-info "rseq requires a vector or sorted collection" (jolt-hash-map)))))

;; --- cat transducer (mirrors core_refs.janet core-cat): each item of the input
;; is itself a collection, concatenated into the downstream reducing fn.
(define (np-cat rf)
  (lambda a
    (cond
      ((null? a) (jolt-invoke rf))
      ((null? (cdr a)) (jolt-invoke rf (car a)))
      (else
       (let loop ((xs (seq->list (jolt-seq (cadr a)))) (acc (car a)))
         (if (null? xs) acc (loop (cdr xs) (jolt-invoke rf acc (car xs)))))))))

(def-var! "clojure.core" "hash" np-hash)
(def-var! "clojure.core" "hash-combine" np-hash-combine)
(def-var! "clojure.core" "hash-ordered-coll" np-hash-ordered-coll)
(def-var! "clojure.core" "hash-unordered-coll" np-hash-unordered-coll)
(def-var! "clojure.core" "transient?" np-transient?)
(def-var! "clojure.core" "rseq" np-rseq)
(def-var! "clojure.core" "cat" np-cat)
