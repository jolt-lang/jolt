;; compile-eval.ss — the compile spine.
;;
;; Ties together the cross-compiled compiler image (jolt.ir + jolt.analyzer +
;; jolt.backend-scheme, loaded as def-var! forms) and the host contract
;; (host-contract.ss) into a runtime entry: a Clojure source string is read by the
;; Chez data reader, analyzed by the analyzer to IR, emitted to Scheme by the
;; emitter, and eval'd. This is the spine the stage2==stage3 bootstrap fixpoint
;; closes over.
;;
;; Loaded after host-contract.ss + the compiler image.

(define jolt-ce-analyze (var-deref "jolt.analyzer" "analyze"))
(define jolt-ce-emit (var-deref "jolt.backend-scheme" "emit"))
;; jolt.passes/run-passes: const-fold every analyzed form, plus inline + type
;; inference when the unit opted into direct-linking (jolt build --opt). Off that
;; path it is a pure const-fold. Loaded from the compiler image (jolt.passes).
(define jolt-ce-run-passes (var-deref "jolt.passes" "run-passes"))
;; The compiler reads source as FORMS (set literals stay {:jolt/type :jolt/set},
;; which the analyzer lowers) — the raw reader, not clojure.core/read-string,
;; whose data conversion would turn those into real sets.
(define jolt-ce-read jolt-read-form-raw)

;; --- current source location ------------------------------------------------
;; The position of the top-level form currently compiling/evaluating, so an
;; uncaught error can report where it came from (cli.ss jolt-report-uncaught).
;; Thread-local: a future/agent worker tracks its own form. Holds #f or a
;; {:line :column :file?} position map (jolt.host/form-position's shape).
;; Top-level granularity — one set per top-level form, nothing per call.
(define jolt-current-source (make-thread-parameter #f))

;; clojure.lang.Compiler/LINE and /COLUMN — derefable cells (Vars on the JVM)
;; holding the line/column of the form being compiled. Macros read @Compiler/LINE
;; as a fallback when &form carries no position (jolt's reader stamps :line on list
;; forms, so this is rarely hit). Updated per top-level form, like *current-source*.
(define compiler-line-cell (jolt-atom-new 0))
(define compiler-column-cell (jolt-atom-new 0))
(let ((members (list (cons "LINE" compiler-line-cell) (cons "COLUMN" compiler-column-cell))))
  (register-class-statics! "Compiler" members)
  (register-class-statics! "clojure.lang.Compiler" members))

(define (jolt-enter-form! form)
  (let ((p (hc-form-position form)))
    (when (pmap? p)
      (jolt-current-source p)
      (let ((line (jolt-get p hc-kw-line jolt-nil)) (col (jolt-get p hc-kw-column jolt-nil)))
        (jolt-atom-val-set! compiler-line-cell (if (jolt-nil? line) 0 line))
        (jolt-atom-val-set! compiler-column-cell (if (jolt-nil? col) 0 col))))))

;; "file:line:col" / "line:col" for the current form, or #f when none is set.
(define (jolt-current-source-string)
  (let ((p (jolt-current-source)))
    (and (pmap? p)
         (let ((line (jolt-get p hc-kw-line jolt-nil))
               (col  (jolt-get p hc-kw-column jolt-nil))
               (file (jolt-get p hc-kw-file jolt-nil)))
           (string-append
             (if (jolt-nil? file) "" (string-append file ":"))
             (if (jolt-nil? line) "?" (number->string line)) ":"
             (if (jolt-nil? col) "?" (number->string col)))))))

;; The spine ALWAYS runs with the full clojure.core prelude loaded, so a clojure.*
;; ref must lower to var-deref (resolved from the prelude), not trip the emitter's
;; "unsupported stdlib fn (no core on Chez yet)" out-of-subset guard — that guard
;; is only for the bare -e subset with no prelude. Turn prelude mode on once, here,
;; so every analyze->emit on this spine sees the full core.
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
;; require). Walks the whole form (a require may be nested in a do/let).
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
         (ir (jolt-ce-run-passes (jolt-ce-analyze ctx form) ctx)))
    (jolt-ce-emit ir)))

;; --- runtime defmacro -------------------------------------------------------
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
;; interning NAME would make require skip the real macro.
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
    ;; thread the current ns: an earlier subform may switch it (ns/in-ns call
    ;; set-chez-ns!), and the next subform must be ANALYZED in that ns so its defs
    ;; land there and its refs resolve (cross-ns def/require in one program).
    ((ce-top-do? form)
     (let loop ((fs (cdr (seq->list form))) (result jolt-nil) (cur ns))
       (if (null? fs)
           result
           (let ((r (jolt-compile-eval-form (car fs) cur)))
             (loop (cdr fs) r (chez-current-ns))))))
    ;; defmacro is compiled like any other form — the analyzer lowers it to a def
    ;; of the expander fn + (mark-macro! …) so subsequent forms expand it. One
    ;; macro-expansion path (no separate spine interception).
    (else
     ;; record this form's source location first, so a compile- or run-time error
     ;; in it reports the right place.
     (jolt-enter-form! form)
     (eval (read (open-input-string (jolt-analyze-emit-form form ns)))
           (interaction-environment)))))

;; Source string -> value (read one form, compile + eval on Chez, in the
;; top-level environment where rt.ss's runtime procedures live).
(define (jolt-compile-eval src ns)
  (jolt-compile-eval-form (jolt-ce-read src) ns))

;; clojure.core/load-string: read every form from the source string and compile+
;; eval each in the current ns, returning the last value (nil for blank input).
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
