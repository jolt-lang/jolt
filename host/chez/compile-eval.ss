;; compile-eval.ss (jolt-hs9n, Phase 3 inc6) — the zero-Janet compile spine.
;;
;; Ties together the cross-compiled compiler image (jolt.ir + jolt.analyzer +
;; jolt.backend-scheme, loaded as def-var! forms) and the host contract
;; (host-contract.ss) into a runtime entry: a Clojure source string is read by the
;; Chez data reader, analyzed by the ON-CHEZ analyzer to IR, emitted to Scheme by
;; the ON-CHEZ emitter, and eval'd — no Janet in the loop. This is the spine the
;; stage2==stage3 bootstrap fixpoint (later increments) closes over.
;;
;; Loaded after host-contract.ss + the compiler image.

(define jolt-ce-analyze (var-deref "jolt.analyzer" "analyze"))
(define jolt-ce-emit (var-deref "jolt.backend-scheme" "emit"))
(define jolt-ce-read (var-deref "clojure.core" "read-string"))

;; Source string -> Scheme source string (read -> analyze -> emit, all on Chez).
;; `ns` is the compile namespace unqualified symbols resolve against.
(define (jolt-analyze-emit src ns)
  (let* ((form (jolt-ce-read src))
         (ctx (make-analyze-ctx ns))
         (ir (jolt-ce-analyze ctx form)))
    (jolt-ce-emit ir)))

;; Source string -> value (compile on Chez, then eval the emitted Scheme in the
;; top-level environment where rt.ss's runtime procedures live).
(define (jolt-compile-eval src ns)
  (eval (read (open-input-string (jolt-analyze-emit src ns)))
        (interaction-environment)))
