;; run-fieldnum.ss — ^double record field reads unbox to fl-ops (jolt-evr9 R2).
;;
;; A record field tagged ^double reads back as a flonum (:double in the lattice),
;; so hintless arithmetic over those fields — (* (:x a) (:x b)) — lowers to fl-ops,
;; the same machinery as a ^double param. Two halves pinned here: (1) the ctor
;; coerces a ^double field to a flonum at construction (JVM parity, and what makes
;; the fl-op sound), and (2) field-field arithmetic over a record param (typed by
;; the whole-program fixpoint) emits fl*.
;;
;;   chez --script host/chez/run-fieldnum.ss
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
(define wp-infer!            (var-deref "jolt.passes.types" "wp-infer!"))
(define run-passes           (var-deref "jolt.passes" "run-passes"))
(define emit                 (var-deref "jolt.backend-scheme" "emit"))

(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))
(define (evals src) (jolt-compile-eval (string-append "(do " src ")") "user"))
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

;; a record with ^double fields; the ctor must coerce an integer arg to a flonum.
(evals "(defrecord V [^double x ^double y])")
(check "ctor coerces ^double field to flonum" (flonum? (evals "(:x (->V 1 2))")) #t)
(check "coerced field value matches" (evals "(:x (->V 1 2))") 1.0)
(check "a flonum arg passes through" (evals "(:y (->V 1.5 2.5))") 2.5)

;; dot is hintless; its caller passes V instances, so the fixpoint types a/b as V
;; records, the ^double fields read :double, and the field-field arithmetic unboxes.
(define dot  (anode "(def dot (fn [a b] (+ (* (:x a) (:x b)) (* (:y a) (:y b)))))"))
(define used (anode "(def used (fn [] (dot (->V 1.0 2.0) (->V 3.0 4.0))))"))
(set-record-shapes! (chez-record-shapes-map))
(set-protocol-methods! (jolt-hash-map))
(wp-infer! (jolt-vector dot used))
(set-optimize! #t)
(define dot-emit (emit (run-passes dot (make-analyze-ctx "user"))))
(check "field-field arithmetic unboxes to fl*" (contains-sub? dot-emit "fl*") #t)
(check "field-field arithmetic unboxes to fl+" (contains-sub? dot-emit "fl+") #t)

;; a ^V param hint types the param with no inferable caller (open-world / cross-fn:
;; the receiver isn't a ctor return). This is the record-ctor-key path — without it
;; the hint is dead and the reads fall back to generic jolt-get + boxed arithmetic.
(define hinted (anode "(def hyp (fn [^V v] (+ (* (:x v) (:x v)) (* (:y v) (:y v)))))"))
(define hint-emit (emit (run-passes hinted (make-analyze-ctx "user"))))
(check "^V param hint direct-accesses field reads" (contains-sub? hint-emit "jrec2-f0") #t)
(check "^V param hint unboxes arithmetic" (contains-sub? hint-emit "fl*") #t)
(check "^V param hint leaves no generic jolt-get" (contains-sub? hint-emit "jolt-get") #f)

;; an UNTAGGED field stays generic — no fl-op (the read is :any, not :double).
(evals "(defrecord W [p q])")
(define dotw (anode "(def dotw (fn [a b] (* (:p a) (:p b))))"))
(define usew (anode "(def usew (fn [] (dotw (->W 1.0 2.0) (->W 3.0 4.0))))"))
(set-record-shapes! (chez-record-shapes-map))
(wp-infer! (jolt-vector dotw usew))
(check "untagged field stays generic (no fl*)"
       (contains-sub? (emit (run-passes dotw (make-analyze-ctx "user"))) "fl*") #f)

(if (= fails 0)
    (begin (printf "fieldnum gate: ~a/~a passed\n" total total) (exit 0))
    (begin (printf "fieldnum gate: ~a/~a passed (~a failed)\n" (- total fails) total fails) (exit 1)))
