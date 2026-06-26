;; run-reduce-sroa.ss — reduce-accumulator scalar replacement gate.
;;
;; A (reduce (fn [acc x] body) (->Rec inits) coll) whose accumulator is a
;; non-escaping record (read only via its fields, rebuilt each step as a same-shape
;; ctor or carried forward unchanged) lowers to a seq loop that carries the acc's
;; fields as scalar loop vars and reconstructs the record once at exit — killing the
;; per-step allocation. This is the ray tracer's hit-all pattern (a HitAcc per
;; sphere test). Pinned here: the reduce call is gone (lowered to a loop), the
;; lowered result matches the generic reduce, and non-lowerable shapes (non-record
;; init, escaping acc) keep the ordinary reduce.
;;
;;   chez --script host/chez/run-reduce-sroa.ss
(import (chezscheme))
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

(define analyze              (var-deref "jolt.analyzer" "analyze"))
(define set-record-shapes!   (var-deref "jolt.passes.types" "set-record-shapes!"))
(define set-protocol-methods! (var-deref "jolt.passes.types" "set-protocol-methods!"))
(define run-passes           (var-deref "jolt.passes" "run-passes"))
(define emit                 (var-deref "jolt.backend-scheme" "emit"))

(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))
(define (evals src) (jolt-compile-eval (string-append "(do " src ")") "user"))
(define (built scm-src) (eval (read (open-input-string scm-src)) (interaction-environment)))
(define (contains-sub? s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) sub) #t)
            (else (loop (+ i 1)))))))

(define fails 0) (define total 0)
(define (check label actual expected)
  (set! total (+ total 1))
  (unless (equal? actual expected)
    (set! fails (+ fails 1))
    (printf "  FAIL ~a: got ~s expected ~s\n" label actual expected)))

(evals "(defrecord Acc [sum cnt])")
(set-record-shapes! (chez-record-shapes-map))
(set-protocol-methods! (jolt-hash-map))
(set-optimize! #t)
(define (emit-opt src) (emit (run-passes (anode src) (make-analyze-ctx "user"))))

;; canonical accumulator: a same-shape ctor in one branch, the acc carried forward
;; in the other. Only the fields of acc are read; acc never escapes.
(define run-src
  "(def run (fn [xs] (:sum (reduce (fn [acc x] (if (> x 0) (->Acc (+ (:sum acc) x) (+ (:cnt acc) 1)) acc)) (->Acc 0 0) xs))))")
(define run-scm (emit-opt run-src))
(check "reduce accumulator lowered (no reduce call)" (contains-sub? run-scm "reduce") #f)
(built run-scm)
(check "lowered result matches generic"
       (jolt-invoke (var-deref "user" "run") (jolt-vector 1 -2 3 4))
       (evals "(:sum (reduce (fn [acc x] (if (> x 0) (->Acc (+ (:sum acc) x) (+ (:cnt acc) 1)) acc)) (->Acc 0 0) [1 -2 3 4]))"))

;; a reduce over a record acc that reads BOTH fields at the end still matches.
(define cnt-src
  "(def cntr (fn [xs] (:cnt (reduce (fn [acc x] (->Acc (+ (:sum acc) x) (+ (:cnt acc) 1))) (->Acc 0 0) xs))))")
(define cnt-scm (emit-opt cnt-src))
(check "second accumulator lowered" (contains-sub? cnt-scm "reduce") #f)
(built cnt-scm)
(check "count accumulator correct" (jolt-invoke (var-deref "user" "cntr") (jolt-vector 5 5 5 5)) 4)

;; empty coll returns the init (reduce semantics): (:sum (->Acc 0 0)) = 0
(check "empty coll returns init" (jolt-invoke (var-deref "user" "run") (jolt-vector)) 0)

;; --- negatives: shapes that must NOT be lowered keep the ordinary reduce --------
;; non-record init
(check "non-record reduce untouched"
       (contains-sub? (emit-opt "(def sm (fn [xs] (reduce (fn [acc x] (+ acc x)) 0 xs)))") "reduce") #t)
;; acc escapes (passed whole to a fn)
(check "escaping-acc reduce untouched"
       (contains-sub? (emit-opt "(def esc (fn [xs] (reduce (fn [acc x] (do (identity acc) (->Acc (+ (:sum acc) x) (:cnt acc)))) (->Acc 0 0) xs)))") "reduce") #t)

(if (= fails 0)
    (begin (printf "reduce-sroa gate: ~a/~a passed\n" total total) (exit 0))
    (begin (printf "reduce-sroa gate: ~a/~a passed (~a failed)\n" (- total fails) total fails) (exit 1)))
