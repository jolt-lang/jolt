;; run-protoret.ss — protocol-method return-type inference gate.
;;
;; A protocol method whose impls all return the same record type has a monomorphic
;; return: collect-pm-rets! joins the impl return types, and call-ret-type then types
;; a (method recv ..) call as that record — so a field read off the result bare-
;; indexes. This is the ray tracer's (:ray (scatter material ..)): scatter's impls
;; all return a ScatterResult, so the bounced ray types without a hint.
;;
;;   chez --script host/chez/run-protoret.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")

(define analyze (var-deref "jolt.analyzer" "analyze"))
(define set-record-shapes! (var-deref "jolt.passes.types" "set-record-shapes!"))
(define set-protocol-methods! (var-deref "jolt.passes.types" "set-protocol-methods!"))
(define wp-infer! (var-deref "jolt.passes.types" "wp-infer!"))
(define run-passes (var-deref "jolt.passes" "run-passes"))
(define emit (var-deref "jolt.backend-scheme" "emit"))
(define U ((var-deref "jolt.passes.types" "new-unit")))
((var-deref "jolt.backend-scheme" "set-emit-unit!") U)
((var-deref "jolt.backend-scheme" "set-prelude-mode!") #t)
(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))
(define (evals src) (jolt-compile-eval (string-append "(do " src ")") "user"))
(define (gate-sub? s t)(let((n(string-length s))(m(string-length t)))(let loop((i 0))(cond((>(+ i m)n)#f)((string=?(substring s i(+ i m))t)#t)(else(loop(+ i 1)))))))
(evals "(defrecord R [^double k])")
(evals "(defprotocol P (m [x]))")
(evals "(defrecord A [v] P (m [x] (->R 1.0)))")
(evals "(defrecord B [v] P (m [x] (->R 2.0)))")
(evals "(defprotocol Q (q [x]))")
(evals "(defrecord C [v] Q (q [x] (->R 3.0)))")
(evals "(defrecord D [v] Q (q [x] 7)))")     ; one impl returns a number, not R
(set-record-shapes! U (chez-record-shapes-map))
(set-protocol-methods! U (chez-protocol-methods-map))
(set-optimize! #t)

;; analyze the impl-registering forms + a consumer; the fixpoint collects the
;; impl return types. (the analyzed defrecord nodes carry register-inline-method.)
(define na (anode "(defrecord A [v] P (m [x] (->R 1.0)))"))
(define nb (anode "(defrecord B [v] P (m [x] (->R 2.0)))"))
(define nc (anode "(defrecord C [v] Q (q [x] (->R 3.0)))"))
(define nd (anode "(defrecord D [v] Q (q [x] 7))"))
(define f  (anode "(def f (fn [a] (* (:k (m a)) 2.0)))"))
(define g  (anode "(def g (fn [a] (:k (q a))))"))
(wp-infer! U (jolt-vector na nb nc nd f g))

;; m's impls all return R -> (:k (m a)) reads off an R -> bare-index + unbox.
(define fe (emit (run-passes f (make-analyze-ctx "user") U)))
(gate-check "monomorphic protocol return direct-accesses the field read" (gate-sub? fe "jrec1-f0") #t)
(gate-check "monomorphic protocol return unboxes the ^double field" (gate-sub? fe "fl") #t)

;; q's impls return R and a number -> joined to non-record -> stays generic (sound).
(define ge (emit (run-passes g (make-analyze-ctx "user") U)))
(gate-check "mixed-return protocol keeps generic jolt-get" (gate-sub? ge "jolt-get") #t)
(gate-check "mixed-return protocol does not bare-index" (gate-sub? ge "jrec-field-at") #f)

(gate-summary "protoret")
