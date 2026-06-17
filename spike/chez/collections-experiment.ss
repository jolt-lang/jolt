;; Phase 0c — persistent-collection perf experiment.
;;
;; Decides shim-vs-self-hosted for collections: is a persistent HAMT fast enough
;; on the Chez substrate that we can afford to SELF-HOST it (in Clojure compiled
;; by Chez) rather than keep it in the Scheme shim? This measures the substrate
;; ceiling with a hand-written Scheme HAMT (what the backend would emit) against
;; Chez's native mutable hashtable (the non-persistent lower bound), on the
;; collections-bench map workload (freq-map + sum-vals, n keys mod 4096).
;;   chez --script collections-experiment.ss [n=30000] [optlevel=2]
(import (chezscheme))
(optimize-level
  (let ((a (command-line-arguments)))
    (if (and (pair? a) (pair? (cdr a))) (string->number (cadr a)) 2)))

;; ---- persistent bitmap HAMT (assoc/get), 5 bits/level, integer-key hash ------
(define-record-type hnode (fields bitmap arr) (nongenerative hnode-v1))
(define empty-map (make-hnode 0 (vector)))

(define (popcount n)
  (let loop ((n n) (c 0)) (if (fx=? n 0) c (loop (fxand n (fx- n 1)) (fx+ c 1)))))
(define (mask h shift) (fxand (fxsra h shift) 31))
(define (idxof bitmap bit) (popcount (fxand bitmap (fx- bit 1))))

(define (vec-insert v i x)
  (let* ((n (vector-length v)) (out (make-vector (fx+ n 1))))
    (let loop ((j 0))
      (when (fx<? j i) (vector-set! out j (vector-ref v j)) (loop (fx+ j 1))))
    (vector-set! out i x)
    (let loop ((j i)) (when (fx<? j n) (vector-set! out (fx+ j 1) (vector-ref v j)) (loop (fx+ j 1))))
    out))
(define (vec-set v i x)
  (let ((out (vector-copy v))) (vector-set! out i x) out))

;; leaf = (cons key val); subtree = hnode
(define (merge-leaves shift k1h e1 k2h k2 v2)
  (if (fx>? shift 30)
      ;; hash exhausted (won't happen for distinct small ints) — chain via assoc-list leaf
      (cons 'collision (list e1 (cons k2 v2)))
      (let ((i1 (mask k1h shift)) (i2 (mask k2h shift)))
        (if (fx=? i1 i2)
            (let ((sub (merge-leaves (fx+ shift 5) k1h e1 k2h k2 v2)))
              (make-hnode (fxsll 1 i1) (vector sub)))
            (let* ((b1 (fxsll 1 i1)) (b2 (fxsll 1 i2)))
              (if (fx<? i1 i2)
                  (make-hnode (fxior b1 b2) (vector e1 (cons k2 v2)))
                  (make-hnode (fxior b1 b2) (vector (cons k2 v2) e1))))))))

(define (assoc-h node shift h key val)
  (let* ((bit (fxsll 1 (mask h shift)))
         (bm (hnode-bitmap node))
         (arr (hnode-arr node)))
    (if (fx=? 0 (fxand bm bit))
        (make-hnode (fxior bm bit) (vec-insert arr (idxof bm bit) (cons key val)))
        (let* ((i (idxof bm bit)) (child (vector-ref arr i)))
          (cond
            ((hnode? child)
             (make-hnode bm (vec-set arr i (assoc-h child (fx+ shift 5) h key val))))
            ((eqv? (car child) key)              ; leaf, same key -> replace
             (make-hnode bm (vec-set arr i (cons key val))))
            (else                                ; leaf, diff key -> split
             (make-hnode bm (vec-set arr i (merge-leaves (fx+ shift 5)
                                              (car child) child h key val)))))))))
(define (assoc-map m key val) (assoc-h m 0 key key val))   ; hash = key (distinct small ints)

(define (get-h node shift h key default)
  (let* ((bit (fxsll 1 (mask h shift))) (bm (hnode-bitmap node)))
    (if (fx=? 0 (fxand bm bit)) default
        (let ((child (vector-ref (hnode-arr node) (idxof bm bit))))
          (cond ((hnode? child) (get-h child (fx+ shift 5) h key default))
                ((eqv? (car child) key) (cdr child))
                (else default))))))
(define (get-map m key default) (get-h m 0 key key default))

;; ---- workloads (mirror bench/collections.clj freq-map + sum-vals) ------------
(define buckets 4096)
(define (freq-hamt n)
  (let loop ((i 0) (m empty-map))
    (if (fx<? i n)
        (let ((k (fxmod (fx* i 2654435761) buckets)))
          (loop (fx+ i 1) (assoc-map m k (fx+ 1 (get-map m k 0)))))
        m)))
(define (freq-native n)
  (let ((m (make-eqv-hashtable)))
    (let loop ((i 0))
      (if (fx<? i n)
          (let ((k (fxmod (fx* i 2654435761) buckets)))
            (hashtable-set! m k (fx+ 1 (hashtable-ref m k 0)))
            (loop (fx+ i 1)))
          m))))
;; sum back: HAMT walk vs native walk
(define (sum-hamt m)
  (let walk ((node m) (acc 0))
    (let ((arr (hnode-arr node)))
      (let loop ((j 0) (acc acc))
        (if (fx<? j (vector-length arr))
            (let ((c (vector-ref arr j)))
              (loop (fx+ j 1) (if (hnode? c) (walk c acc) (fx+ acc (cdr c)))))
            acc)))))
(define (sum-native m) (call-with-values (lambda () (hashtable-entries m))
                         (lambda (ks vs) (let ((acc 0)) (vector-for-each (lambda (v) (set! acc (fx+ acc v))) vs) acc))))

;; ---- bench harness -----------------------------------------------------------
(define (now-ns) (let ((t (current-time 'time-monotonic)))
                   (+ (* (time-second t) 1000000000) (time-nanosecond t))))
(define (bench name build sum n)
  (sum (build (quotient n 4))) (sum (build (quotient n 4)))         ; warmup
  (let loop ((k 0) (acc '()) (r 0))
    (if (fx<? k 3)
        (let* ((t0 (now-ns)) (m (build n)) (s (sum m))
               (ms (/ (- (now-ns) t0) 1000000.0)))
          (loop (fx+ k 1) (cons ms acc) s))
        (printf "~a  result ~a  mean ~a ms\n" name r
                (exact->inexact (/ (apply + acc) 3.0))))))

(let* ((a (command-line-arguments))
       (n (if (pair? a) (string->number (car a)) 30000)))
  (printf "collections map-churn (n=~a, ~a buckets)\n" n buckets)
  (bench "persistent HAMT (self-hostable) " freq-hamt   sum-hamt   n)
  (bench "native hashtable (mutable, ceil)" freq-native sum-native n))
