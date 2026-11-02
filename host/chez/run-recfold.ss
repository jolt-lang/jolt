;; run-recfold.ss — what an optimizing compile emits for record construction and
;; for the type predicates over a proven record.
;;
;;   - (R. f …) at the positional arity is the record's ctor, the one (->R f …)
;;     already inlines: make-jrecN on the descriptor, no host-new class lookup.
;;   - record? on a proven struct asks which kind it is: a deftype has a shape
;;     too and is no record, so it folds to false (it used to fold to true in a
;;     built binary while the unoptimized path answered false).
;;   - seq? on a proven defrecord folds to false, and the if it gates types as
;;     its taken branch, so map destructuring of a record — which opens with
;;     (if (seq? m) … m) — keeps the record type and reads the slots directly.
;;     A deftype may implement ISeq, so seq? on one stays a runtime test.
;;
;;   chez --script host/chez/run-recfold.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")

(define set-record-shapes! (var-deref "jolt.passes.types" "set-record-shapes!"))
(define set-protocol-methods! (var-deref "jolt.passes.types" "set-protocol-methods!"))
(define run-passes (var-deref "jolt.passes" "run-passes"))
(define emit    (var-deref "jolt.backend-scheme" "emit"))
(define analyze (var-deref "jolt.analyzer" "analyze"))
(define U ((var-deref "jolt.passes.types" "new-unit")))
((var-deref "jolt.backend-scheme" "set-emit-unit!") U)
((var-deref "jolt.backend-scheme" "set-prelude-mode!") #t)
(define (evals src) (jolt-compile-eval (string-append "(do " src ")") "user"))

(evals "(defrecord RP [x y z])")
(evals "(deftype DT [x])")

(define (run-emit scm) (eval (read (open-input-string scm)) (interaction-environment)))
(define (opt-emit src)
  (set-optimize! #t)
  (set-record-shapes! U (chez-record-shapes-map))
  (set-protocol-methods! U (chez-protocol-methods-map))
  (let ((r (emit (run-passes (analyze (make-analyze-ctx "user") (jolt-ce-read src))
                             (make-analyze-ctx "user") U))))
    (set-optimize! #f)
    r))

;; --- the shape registry says which shapes are records ---------------------------
(let ((m (chez-record-shapes-map)) (k (lambda (n) (keyword #f n))))
  (gate-check "a defrecord's shape carries :record"
              (jolt-get (jolt-get m "user/->RP") (k "record")) #t)
  (gate-check "a deftype's shape does not"
              (jolt-nil? (jolt-get (jolt-get m "user/->DT") (k "record"))) #t))

;; --- (R. …) is the inlined ctor ---------------------------------------------------
(let ((e (opt-emit "(defn mk-new [a] (RP. a 2 3))")))
  (gate-check "(RP. a 2 3) emits make-jrec3" (gate-sub? e "make-jrec3") #t)
  (gate-check "...with no host-new lookup" (gate-sub? e "host-new") #f))
(let ((e (opt-emit "(defn mk-new-ext [a] (RP. a 2 3 nil {:w 1}))")))
  (gate-check "the (R. f… meta ext) arity keeps host-new" (gate-sub? e "host-new") #t))
(evals "(defn mk-new [a] (RP. a 2 3))")
(gate-check "(RP. …) value" (jolt-final-str (evals "(pr-str (mk-new 1))")) "#user.RP{:x 1, :y 2, :z 3}")

;; --- record? / seq? folds ------------------------------------------------------------
(let ((e (opt-emit "(defn rq-rec [] (let [r (->RP 1 2 3)] (record? r)))")))
  (gate-check "record? on a proven defrecord folds" (gate-sub? e "record?") #f))
(let ((e (opt-emit "(defn rq-dt [] (let [t (->DT 1)] (record? t)))")))
  ;; folds too — to false, which is the answer, where it used to fold to true
  (gate-check "record? on a proven deftype folds" (gate-sub? e "record?") #f)
  (run-emit e)
  (gate-check "...to false" ((var-deref "user" "rq-dt")) #f))
(let ((e (opt-emit "(defn sq-dt [] (let [t (->DT 1)] (seq? t)))")))
  (gate-check "seq? on a proven deftype stays a call" (gate-sub? e "seq?") #t))
(let ((e (opt-emit "(defn ds [] (let [r (->RP 1 2 3) {:keys [x y z]} r] (+ x y z)))")))
  (gate-check "map destructuring of a proven record drops the seq? test" (gate-sub? e "seq?") #f)
  (gate-check "...and reads the slots directly" (gate-sub? e "jrec3-f1") #t)
  (gate-check "...with no generic get left" (gate-sub? e "(jolt-get ") #f))

(gate-summary "recfold")
