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
(load "host/chez/run-gate-harness.ss")

(define analyze              (var-deref "jolt.analyzer" "analyze"))
(define set-record-shapes!   (var-deref "jolt.passes.types" "set-record-shapes!"))
(define set-protocol-methods! (var-deref "jolt.passes.types" "set-protocol-methods!"))
(define wp-infer!            (var-deref "jolt.passes.types" "wp-infer!"))
(define run-passes           (var-deref "jolt.passes" "run-passes"))
(define emit                 (var-deref "jolt.backend-scheme" "emit"))

(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))
(define (evals src) (jolt-compile-eval (string-append "(do " src ")") "user"))

;; a record with ^double fields; the ctor must coerce an integer arg to a flonum.
(evals "(defrecord V [^double x ^double y])")
(gate-check "ctor coerces ^double field to flonum" (flonum? (evals "(:x (->V 1 2))")) #t)
(gate-check "coerced field value matches" (evals "(:x (->V 1 2))") 1.0)
(gate-check "a flonum arg passes through" (evals "(:y (->V 1.5 2.5))") 2.5)

;; dot is hintless; its caller passes V instances, so the fixpoint types a/b as V
;; records, the ^double fields read :double, and the field-field arithmetic unboxes.
(define dot  (anode "(def dot (fn [a b] (+ (* (:x a) (:x b)) (* (:y a) (:y b)))))"))
(define used (anode "(def used (fn [] (dot (->V 1.0 2.0) (->V 3.0 4.0))))"))
(set-record-shapes! (chez-record-shapes-map))
(set-protocol-methods! (jolt-hash-map))
(wp-infer! (jolt-vector dot used))
(set-optimize! #t)
(define dot-emit (emit (run-passes dot (make-analyze-ctx "user"))))
(gate-check "field-field arithmetic unboxes to fl*" (gate-sub? dot-emit "fl*") #t)
(gate-check "field-field arithmetic unboxes to fl+" (gate-sub? dot-emit "fl+") #t)

;; a ^V param hint types the param with no inferable caller (open-world / cross-fn:
;; the receiver isn't a ctor return). This is the record-ctor-key path — without it
;; the hint is dead and the reads fall back to generic jolt-get + boxed arithmetic.
(define hinted (anode "(def hyp (fn [^V v] (+ (* (:x v) (:x v)) (* (:y v) (:y v)))))"))
(define hint-emit (emit (run-passes hinted (make-analyze-ctx "user"))))
(gate-check "^V param hint direct-accesses field reads" (gate-sub? hint-emit "jrec2-f0") #t)
(gate-check "^V param hint unboxes arithmetic" (gate-sub? hint-emit "fl*") #t)
(gate-check "^V param hint leaves no generic jolt-get" (gate-sub? hint-emit "jolt-get") #f)

;; an UNTAGGED field whose every ctor site passes a flonum now unboxes too —
;; whole-program field-type inference (run-fieldjoin) derives :double from the ctor
;; joins, so portable hint-free code reaches the same fl* as the ^double case above.
(evals "(defrecord W [p q])")
(define dotw (anode "(def dotw (fn [a b] (* (:p a) (:p b))))"))
(define usew (anode "(def usew (fn [] (dotw (->W 1.0 2.0) (->W 3.0 4.0))))"))
(set-record-shapes! (chez-record-shapes-map))
(wp-infer! (jolt-vector dotw usew))
(gate-check "untagged all-flonum field unboxes to fl* (field-typed)"
       (gate-sub? (emit (run-passes dotw (make-analyze-ctx "user"))) "fl*") #t)

(gate-summary "fieldnum")
