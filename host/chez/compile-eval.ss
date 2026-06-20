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

;; The zero-Janet spine ALWAYS runs with the full clojure.core prelude loaded, so a
;; clojure.* ref must lower to var-deref (resolved from the prelude), not trip the
;; emitter's "unsupported stdlib fn (no core on Chez yet)" out-of-subset guard —
;; that guard is only for the bare -e subset with no prelude. Turn prelude mode on
;; once, here, so every analyze->emit on this spine sees the full core (jolt-qjr0).
((var-deref "jolt.backend-scheme" "set-prelude-mode!") #t)

;; (quote X) -> X, else x — unwraps a quoted require spec.
(define (ce-unquote x)
  (if (and (cseq? x) (cseq-list? x))
      (let ((items (seq->list x)))
        (if (and (pair? items) (symbol-t? (car items))
                 (string=? (symbol-t-name (car items)) "quote") (pair? (cdr items)))
            (cadr items) x))
      x))

;; Pre-register any (require ...)/(use ...) :as aliases under `ns` BEFORE analysis,
;; so a qualified s/foo resolves while compiling (analysis precedes the runtime
;; require). Walks the whole form (a require may be nested in a do/let). jolt-qjr0.
(define (ce-clause-require? cl)          ; (:require ...) / (:use ...) ns clause
  (and (pair? cl) (keyword? (car cl))
       (let ((kn (keyword-t-name (car cl)))) (or (string=? kn "require") (string=? kn "use")))))
(define (ce-scan-requires! form ns)
  (when (and (cseq? form) (cseq-list? form))
    (let ((items (seq->list form)))
      (when (pair? items)
        (let* ((h (car items)) (hn (and (symbol-t? h) (symbol-t-name h))))
          (cond
            ;; (require spec...) / (use spec...) — specs are quoted
            ((and hn (or (string=? hn "require") (string=? hn "use")))
             (for-each (lambda (a) (chez-register-spec! ns (ce-unquote a))) (cdr items)))
            ;; (ns name (:require [a :as x]) ...) — clause specs are literal
            ((and hn (string=? hn "ns"))
             (for-each (lambda (clause)
                         (when (and (cseq? clause) (cseq-list? clause))
                           (let ((cl (seq->list clause)))
                             (when (ce-clause-require? cl)
                               (for-each (lambda (spec) (chez-register-spec! ns spec)) (cdr cl))))))
                       (if (pair? (cdr items)) (cddr items) '())))
            (else (for-each (lambda (x) (ce-scan-requires! x ns)) items))))))))

;; Already-read FORM -> Scheme source string (analyze -> emit on Chez).
;; `ns` is the compile namespace unqualified symbols resolve against.
(define (jolt-analyze-emit-form form ns)
  (ce-scan-requires! form ns)
  (let* ((ctx (make-analyze-ctx ns))
         (ir (jolt-ce-analyze ctx form)))
    (jolt-ce-emit ir)))

;; Source string -> Scheme source string (read then analyze -> emit, all on Chez).
(define (jolt-analyze-emit src ns)
  (jolt-analyze-emit-form (jolt-ce-read src) ns))

;; --- runtime defmacro (jolt-r8ku) -------------------------------------------
;; Shared with emit-image.ss (loaded after this). A defmacro lowers to a def of
;; its expander fn + a macro flag, exactly as the prelude emits build-time macros.

;; Is `f` a (defmacro ...) / (definline ...) form?
(define (ce-macro-form? f)
  (and (cseq? f) (cseq-list? f)
       (let ((items (seq->list f)))
         (and (pair? items) (symbol-t? (car items))
              (let ((h (symbol-t-name (car items))))
                (or (string=? h "defmacro") (string=? h "definline")))))))

;; (defmacro NAME [docstring] [attr-map] params body...) -> (values "NAME" (fn ...)).
;; Strips a leading docstring (native string) + attr-map (a non-symbol pmap), then
;; re-heads the rest with `fn` so a destructured macro arglist desugars. Emits the
;; BARE fn (the caller wraps it in def-var! + mark-macro!), never a (def NAME ...) —
;; interning NAME would make require skip the real macro (jolt-r9lm).
(define (ce-defmacro->fn f)
  (let* ((items (seq->list f))
         (name-sym (cadr items))
         (after-name (cddr items))
         (a1 (if (and (pair? after-name) (string? (car after-name)))
                 (cdr after-name) after-name))
         (after-meta (if (and (pair? a1) (pmap? (car a1)))
                         (cdr a1) a1))
         (fn-sym (jolt-symbol #f "fn")))
    (values (symbol-t-name name-sym)
            (apply jolt-list (cons fn-sym after-meta)))))

;; A bare top-level (do ...) form — head is the unqualified `do` symbol.
(define (ce-top-do? form)
  (and (cseq? form) (cseq-list? form)
       (let ((h (seq-first form)))
         (and (symbol-t? h) (jolt-nil? (hc-sym-ns h))
              (string=? (symbol-t-name h) "do")))))

;; Compile + eval ONE already-read form in compile ns `ns`; returns the value.
;; A top-level (do ...) is UNROLLED — each subform compiled+eval'd in turn, like
;; Clojure's top-level do — so a runtime defmacro/def in an earlier subform is
;; visible (macro flag set, var interned) before a later subform is analyzed.
(define (jolt-compile-eval-form form ns)
  (cond
    ((ce-top-do? form)
     (let loop ((fs (cdr (seq->list form))) (result jolt-nil))
       (if (null? fs)
           result
           (loop (cdr fs) (jolt-compile-eval-form (car fs) ns)))))
    ;; runtime defmacro: def the expander fn + mark the var a macro so subsequent
    ;; forms expand it (hc-macro? reads var-macro-table). Mirrors emit-image.ss
    ;; ei-emit-ns and the Janet seed eval-defmacro.
    ((ce-macro-form? form)
     (let-values (((nm fn-form) (ce-defmacro->fn form)))
       (def-var! ns nm (jolt-compile-eval-form fn-form ns))
       (mark-macro! ns nm)
       jolt-nil))
    (else
     (eval (read (open-input-string (jolt-analyze-emit-form form ns)))
           (interaction-environment)))))

;; Source string -> value (read one form, compile + eval on Chez, in the
;; top-level environment where rt.ss's runtime procedures live).
(define (jolt-compile-eval src ns)
  (jolt-compile-eval-form (jolt-ce-read src) ns))

;; clojure.core/load-string: read every form from the source string and compile+
;; eval each in the current ns, returning the last value (nil for blank input).
;; Mirrors src/jolt/api.janet load-string (the parse-next loop). jolt-r8ku.
(define (jolt-load-string s)
  (let loop ((src s) (result jolt-nil))
    (let ((pn (jolt-parse-next src)))
      (if (jolt-nil? pn)
          result
          (loop (jolt-nth pn 1)
                (jolt-compile-eval-form (jolt-nth pn 0) (chez-current-ns)))))))

;; eval / load-string are FUNCTIONS on the spine (the compiler image is resident
;; at runtime). eval takes an already-read FORM (e.g. from quote / list); it and
;; load-string compile+eval in the current ns. eval is removed from the analyzer's
;; special-symbol lists (host-contract.ss) so it resolves as an ordinary core var.
(def-var! "clojure.core" "eval"
  (lambda (form) (jolt-compile-eval-form form (chez-current-ns))))
(def-var! "clojure.core" "load-string" jolt-load-string)
