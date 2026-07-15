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
;; clojure.lang.Compiler/specials — the JVM's special-form table (sym -> parser).
;; tools.macro reads (keys Compiler/specials) to know which heads NOT to expand.
;; Only the keys matter here; values are #t. The set matches Clojure 1.2/1.3.
(define compiler-specials
  (let ((unq '("def" "loop*" "recur" "if" "case*" "let*" "letfn*" "do" "fn*"
               "quote" "var" "." "set!" "try" "monitor-enter" "monitor-exit"
               "throw" "new" "&" "catch" "finally" "reify*" "deftype*")))
    (fold-left (lambda (m s) (jolt-assoc1 m (jolt-symbol #f s) #t))
               (jolt-assoc1 (jolt-hash-map) (jolt-symbol "clojure.core" "import*") #t)
               unq)))
;; clojure.lang.Compiler/demunge — reverse the name munging Clojure applies to
;; build JVM class/method names, so "clojure.core$odd_QMARK_" -> clojure.core/odd?.
;; clojure.spec.alpha's fn-sym uses it to recover a symbol from a fn's class name.
;; Longest tokens first; a standalone _ is a hyphen; $ separates ns from name.
(define demunge-token-map
  '(("_DOUBLEQUOTE_" . "\"") ("_SINGLEQUOTE_" . "'") ("_AMPERSAND_" . "&") ("_PERCENT_" . "%")
    ("_LBRACE_" . "{") ("_RBRACE_" . "}") ("_LBRACK_" . "[") ("_RBRACK_" . "]")
    ("_BSLASH_" . "\\") ("_TILDE_" . "~") ("_CIRCA_" . "@") ("_SHARP_" . "#") ("_BANG_" . "!")
    ("_CARET_" . "^") ("_COLON_" . ":") ("_QMARK_" . "?") ("_SLASH_" . "/") ("_PLUS_" . "+")
    ("_STAR_" . "*") ("_BAR_" . "|") ("_GT_" . ">") ("_LT_" . "<") ("_EQ_" . "=") ("_DOT_" . ".")))
(define (compiler-demunge s)
  (let* ((s (if (string? s) s (jolt-str-render-one s)))
         (n (string-length s))
         (out (open-output-string)))
    (let loop ((i 0))
      (if (>= i n) (get-output-string out)
          (let ((tok (let scan ((ts demunge-token-map))
                       (cond ((null? ts) #f)
                             ((let ((t (caar ts)))
                                (and (<= (+ i (string-length t)) n)
                                     (string=? (substring s i (+ i (string-length t))) t)))
                              (car ts))
                             (else (scan (cdr ts)))))))
            (cond
              (tok (display (cdr tok) out) (loop (+ i (string-length (car tok)))))
              ((char=? (string-ref s i) #\_) (write-char #\- out) (loop (+ i 1)))
              ((char=? (string-ref s i) #\$) (write-char #\/ out) (loop (+ i 1)))
              (else (write-char (string-ref s i) out) (loop (+ i 1)))))))))
;; clojure.lang.Compiler/CHAR_MAP — the forward munge map (special char -> escape
;; token), the inverse of demunge-token-map. Derived from that single source so
;; the two can't drift: drop _DOT_ (a '.' is never munged in CHAR_MAP) and add the
;; hyphen -> "_" entry (demunge treats a lone _ as '-' via a separate rule).
(define compiler-char-map
  (fold-left (lambda (m pair)
               (if (string=? (car pair) "_DOT_")
                   m
                   (jolt-assoc1 m (string-ref (cdr pair) 0) (car pair))))
             (jolt-assoc1 (jolt-hash-map) #\- "_")
             demunge-token-map))
(let ((members (list (cons "LINE" compiler-line-cell) (cons "COLUMN" compiler-column-cell)
                     (cons "specials" compiler-specials)
                     (cons "CHAR_MAP" compiler-char-map)
                     (cons "demunge" compiler-demunge))))
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
;; Cache resolved var cells per reference site in runtime-compiled code (the big
;; win for libraries / REPL code). emit-image.ss turns this back off so the seed
;; mint and AOT build stay byte-deterministic. Guarded: the flag is absent in an
;; older seed during the first re-mint pass.
(let ((scv (var-deref "jolt.backend-scheme" "set-var-cache!")))
  (when (procedure? scv) (scv #t)))
;; JOLT_TRACE is a falsey value (case-insensitive) — the single predicate both the
;; dev-mode enable and the whole-run enable consult, so "off" never accidentally
;; means "on". An empty / unset value is NOT falsey here — it carries no signal, so
;; dev mode still traces and a whole run still doesn't.
(define (jolt-trace-env-off? e)
  (and (string? e)
       (let ((s (string-downcase e)))
         (or (string=? s "0") (string=? s "false") (string=? s "no")
             (string=? s "off") (string=? s "n")))))
;; Tail-frame history. Turning it on makes the emitter add a per-fn history push to
;; every fn compiled AFTERWARD, and allocates this thread's ring. Suppressed when
;; JOLT_TRACE is a falsey value, so JOLT_TRACE=0 / off / no disables it in dev mode.
(define (jolt-enable-trace!)
  (unless (jolt-trace-env-off? (getenv "JOLT_TRACE"))
    (let ((stf (var-deref "jolt.backend-scheme" "set-trace-frames!")))
      (when (procedure? stf) (stf #t)))
    (jolt-trace-enable!)))
;; Exposed so the REPL / nREPL entrypoints (jolt.main, jolt.nrepl) can turn tracing
;; on for REPL-driven development without the user setting JOLT_TRACE. Because the
;; push is baked in at compile time, only code compiled after this call is traced —
;; which is exactly the code you eval / reload in a live session.
(def-var! "jolt.host" "enable-trace!" jolt-enable-trace!)
;; Explicit opt-in for a whole run (JOLT_TRACE=1): turn tracing on BEFORE any app
;; namespace is compiled, so a plain `-M:run` traces the app's own code too. Called
;; from the runtime entrypoints (cli.ss, and the built joltc launcher) — NOT at load
;; time: a built joltc runs top-level forms at heap-build time, where JOLT_TRACE is
;; always unset, so a load-time check would never see the user's runtime env. Only an
;; affirmative value (set, non-empty, not falsey) forces it on.
(define (jolt-trace-init-from-env!)
  (let ((e (getenv "JOLT_TRACE")))
    (when (and e (fx>? (string-length e) 0) (not (jolt-trace-env-off? e)))
      (jolt-enable-trace!))))

;; (with-meta sym m) -> sym, else x — an (ns ^:no-doc name …) yields the name with
;; reader metadata as a with-meta form; strip it to read the bare ns symbol.
(define (ce-unwrap-meta x)
  (if (and (cseq? x) (cseq-list? x))
      (let ((items (seq->list x)))
        (if (and (pair? items) (symbol-t? (car items))
                 (string=? (symbol-t-name (car items)) "with-meta") (pair? (cdr items)))
            (cadr items) x))
      x))

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
            ;; (ns name (:require [a :as x]) ...) — clause specs are literal. Register
            ;; the aliases under NAME (the ns being defined), not the passed `ns`:
            ;; when a file is loaded its ns form compiles while (chez-current-ns) is
            ;; still the requiring ns, so using `ns` would leak the loaded ns's
            ;; aliases into its requirer and clobber a same-named alias there
            ;; (rewrite-clj.zip.base's [node.protocols :as node] over the caller's node).
            ((and hn (string=? hn "ns"))
             (let ((ns-name (if (and (pair? (cdr items)) (symbol-t? (ce-unwrap-meta (cadr items))))
                                (symbol-t-name (ce-unwrap-meta (cadr items)))
                                ns)))
               (for-each (lambda (clause)
                           (when (and (cseq? clause) (cseq-list? clause))
                             (let ((cl (seq->list clause)))
                               (when (ce-clause-require? cl)
                                 (for-each (lambda (spec) (chez-register-spec! ns-name spec)) (cdr cl))))))
                         (if (pair? (cdr items)) (cddr items) '()))))
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
;; interning NAME would make require skip the real macro. The head is the QUALIFIED
;; clojure.core/fn, not a bare `fn`, so it resolves to the real fn macro even when
;; the macro being defined IS `fn` (schema's s/fn) or the ns excluded it.
(define (ce-defmacro->fn f)
  (let* ((items (seq->list f))
         (name-sym (cadr items))
         (after-name (cddr items))
         (a1 (if (and (pair? after-name) (string? (car after-name)))
                 (cdr after-name) after-name))
         (after-meta (if (and (pair? a1) (pmap? (car a1)))
                         (cdr a1) a1))
         (fn-sym (jolt-symbol "clojure.core" "fn")))
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
;; Only lists, symbols, and the persistent collections may carry code the
;; analyzer must compile; every other value — numbers, strings, keywords, and
;; opaque host objects (a #inst Date, a #uuid, a regex, a record, a function) —
;; evaluates to itself, as eval does on the JVM. (read-string builds those host
;; values eagerly, so eval must accept them without trying to analyze them.)
(define (jolt-compile-eval-form form ns)
  (if (or (cseq? form) (jolt-lazyseq? form) (empty-list-t? form) (symbol-t? form)
          (pvec? form) (pmap? form) (pset? form))
      (jolt-compile-eval-form* form ns)
      form))
(define (jolt-compile-eval-form* form ns)
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
     ;; drop tail-frame history from earlier top-level forms, so an error's trace
     ;; shows only this form's own call history (a no-op unless JOLT_TRACE is on).
     (jolt-trace-reset!)
     (eval (read (open-input-string (jolt-analyze-emit-form form ns)))
           (interaction-environment)))))

;; Source string -> value (read one form, compile + eval on Chez, in the
;; top-level environment where rt.ss's runtime procedures live).
(define (jolt-compile-eval src ns)
  (jolt-compile-eval-form (jolt-ce-read src) ns))

;; clojure.core/load-string: read every form from the source string and compile+
;; eval each in the current ns, returning the last value (nil for blank input).
;; Reads RAW forms (like loading a file) so reader literals — #inst/#uuid/#"regex"
;; and user #tag readers — stay as forms the analyzer compiles, rather than being
;; built into opaque values the way read-string does. `data-readers-active` and
;; `ldr-apply-readers` come from the loader, present in the CLI runtime; guard the
;; read so load-string still works in a bootstrap/build context without it.
(define (jolt-load-string s)
  (let ((end (string-length s))
        (drl (guard (_ (#t #f)) data-readers-active)))
    (let loop ((i 0) (result jolt-nil))
      (if (>= i end)
          result
          (let-values (((form j) (rdr-read-form s i end)))
            (if (> j i)
                (loop j (if (rdr-eof? form)
                            result
                            (jolt-compile-eval-form
                             (if drl (ldr-apply-readers form) form)
                             (chez-current-ns))))
                result))))))

;; eval / load-string are FUNCTIONS on the spine (the compiler image is resident
;; at runtime). eval takes an already-read FORM (e.g. from quote / list); it and
;; load-string compile+eval in the current ns. eval is removed from the analyzer's
;; special-symbol lists (host-contract.ss) so it resolves as an ordinary core var.
(def-var! "clojure.core" "eval"
  (lambda (form) (jolt-compile-eval-form form (chez-current-ns))))
(def-var! "clojure.core" "load-string" jolt-load-string)
