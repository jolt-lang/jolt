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
(define jolt-ce-emit (var-deref "jolt.backend-scheme" "emit-top-form"))
;; jolt.passes/run-passes: const-fold every analyzed form, plus inline + type
;; inference when the unit opted into direct-linking (jolt build --opt). Off that
;; path it is a pure const-fold. Loaded from the compiler image (jolt.passes).
;; 2-arg run-passes makes its own fresh per-form inference context (this spine never
;; optimizes, so it's unused anyway); build/emit-image use the 3-arg form to share the
;; whole-program-seeded context across forms.
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
;; AOT compile-cache capture port (#f or an open output string port), set by
;; loader.ss aot-capture-load. When set, jolt-compile-eval-form* tees each form's
;; emitted Scheme here so a cache miss records exactly what was eval'd (preserving
;; the interleaved analyze→eval semantics the cache later replays via `load`).
(define jolt-aot-capture (make-thread-parameter #f))
;; The file that capture port was opened FOR. load-jolt-file* tees only while
;; loading that file: a nested load must not append its forms to the requiring
;; namespace's artifact, because the artifact already re-runs the require that
;; pulls it in. See load-jolt-file* for what leaked before this existed.
(define jolt-aot-capture-file (make-thread-parameter #f))

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

;; Record that we are working on `path`, for a phase that walks a file WITHOUT
;; evaluating its forms — `jolt build`'s require scan, its whole-program inference
;; walk, its emit walk. Those never reach jolt-enter-form!, so an error in one used
;; to be reported with no location at all: just "Unhandled exception" over a trace
;; of runtime procedure names, with nothing saying which file was being read.
;;
;; A bare set, not a parameterize, because the reporter runs from the CLI's guard —
;; OUTSIDE every dynamic binding the failing phase had established. A parameterized
;; value is long gone by then; only a sticky one survives the unwind, which is the
;; same reason jolt-enter-form! sets rather than binds and load-jolt-file* restores
;; on normal return only. Clears any leftover line/column: they belong to whatever
;; was last evaluated, in some other file entirely.
(define (jolt-enter-file! path)
  (when (string? path)
    (jolt-current-source (jolt-hash-map hc-kw-file path))))

;; "file:line:col" for a form position, bare "file" for a file-only one (above), or
;; #f when nothing is set.
(define (jolt-current-source-string)
  (let ((p (jolt-current-source)))
    (and (pmap? p)
         (let ((line (jolt-get p hc-kw-line jolt-nil))
               (col  (jolt-get p hc-kw-column jolt-nil))
               (file (jolt-get p hc-kw-file jolt-nil)))
           (if (jolt-nil? line)
               (and (string? file) file)
               (string-append
                 (if (jolt-nil? file) "" (string-append file ":"))
                 (number->string line) ":"
                 (if (jolt-nil? col) "?" (number->string col))))))))

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
;; Register each fn def's source, so an uncaught error's backtrace names frames
;; "ns/name (file:line)" the way an AOT build's does instead of falling back to
;; bare Chez procedure names. One hashtable insert per def at definition time, no
;; per-call cost — unlike the tail-frame history (JOLT_TRACE), which pays a push
;; per call and stays opt-in. emit-image.ss turns this back off for the seed mint,
;; whose emitted prelude must not carry this machine's absolute paths.
(let ((ssr (var-deref "jolt.backend-scheme" "set-source-reg!")))
  (when (procedure? ssr) (ssr #t)))
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
;; Turn tracing on for a whole run, BEFORE any app namespace is compiled, so the
;; app's own code carries the entry prologue. Called from the runtime entrypoints
;; (cli.ss, and the built jolt launcher) — NOT at load time: a built jolt runs
;; top-level forms at heap-build time, where JOLT_TRACE is always unset, so a
;; load-time check would never see the user's runtime env.
;;
;; ON BY DEFAULT on this path — running from source is the develop-and-debug path,
;; and without the history a tail call chain reports no location at all: TCO erases
;; the frames, so the common shape (-main tail-calls a fn that throws) left the
;; reporter with an empty continuation and nothing to name. Set JOLT_TRACE=0 to opt
;; out. The cost is a per-entry ring push in code compiled at runtime; core is not
;; affected (the seed prelude is already compiled), which is why a seq/string/map
;; workload measures the same either way. It is only visible in code that is almost
;; entirely user-level calls — a fib microbenchmark pays ~7x, which is the case to
;; set JOLT_TRACE=0 for. A `jolt build` binary is unaffected: its prologues are
;; baked at build time, so this runtime switch cannot add them.
(define (jolt-trace-init-from-env!)
  (unless (jolt-trace-env-off? (getenv "JOLT_TRACE"))
    (jolt-enable-trace!)))
;; Host-side mirror of the emitter's trace-frames? flag: whether the emitted
;; text carries #|L<line>|# markers (backend_scheme.clj with-site). When it is
;; off, no markers exist, so the annotated read below would attach source
;; objects with nothing to resolve them against — the eval path then takes
;; exactly today's plain-read route. The SAME flag the emitter consults, so the
;; two can never disagree about whether a form's text has markers. Resolved at
;; load time (the image is resident), read at eval time (the flag flips at
;; runtime via jolt-enable-trace! / emit-image).
(define jolt-ce-trace-frames?-fn (var-deref "jolt.backend-scheme" "trace-frames?"))
(define (jolt-ce-trace-frames?)
  (and (procedure? jolt-ce-trace-frames?-fn) (jolt-ce-trace-frames?-fn)))

;; Eval one emitted Scheme form WITH Chez source annotations, so a frame from
;; this code carries a source object whose name is a registry key and whose
;; offset is a byte position in `scm` — the pair a backtrace resolves to a clj
;; line via jolt-eval-source-line. get-datum/annotations is what attaches the
;; source object; make-source-file-descriptor needs a BINARY port, and
;; get-datum/annotations a TEXTUAL one, so the text is opened twice over the
;; same bytes. The registered marker table is all that survives this call — the
;; text itself is transient. Tracing must be on (markers present) when called.
(define (jolt-eval-with-source scm)
  (let* ((bv (string->utf8 scm))
         (name (jolt-register-eval-marker-table! scm))
         (sfd (make-source-file-descriptor name (open-bytevector-input-port bv)))
         (ann (call-with-values
               (lambda ()
                 (get-datum/annotations
                  (transcoded-port (open-bytevector-input-port bv) (native-transcoder))
                  sfd 0))
               (lambda (a p) a))))
    (eval ann (interaction-environment))))

;; (with-meta sym m) -> sym, else x — an (ns ^:no-doc name …) yields the name with
;; reader metadata as a with-meta form; strip it to read the bare ns symbol.
(define (ce-unwrap-meta x)
  (if (cseq? x)
      (let ((items (seq->list x)))
        (if (and (pair? items) (symbol-t? (car items))
                 (string=? (symbol-t-name (car items)) "with-meta") (pair? (cdr items)))
            (cadr items) x))
      x))

;; (quote X) -> X, else x — unwraps a quoted require spec.
(define (ce-unquote x)
  (if (cseq? x)
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
  (when (cseq? form)
    (let ((items (seq->list form)))
      (when (pair? items)
        (let* ((h (car items)) (hn (and (symbol-t? h) (symbol-t-name h))))
          ;; skip quoted data: (quote ...) must not be scanned for require specs
          (cond
            ((and hn (string=? hn "quote")) #t)
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
                           (when (cseq? clause)
                             (let ((cl (seq->list clause)))
                               (when (ce-clause-require? cl)
                                 (for-each (lambda (spec) (chez-register-spec! ns-name spec)) (cdr cl))))))
                         (if (pair? (cdr items)) (cddr items) '()))))
            (else (for-each (lambda (x) (ce-scan-requires! x ns)) items))))))))

;; --- success-type lint (RFC 0006), opt-in via JOLT_CHECK --------------------
;; A Carp-inspired surfacing of the existing success-type checker: with JOLT_CHECK
;; set to a truthy value, every runtime-compiled top-level form is run through
;; jolt.passes.types/check-form and any diagnostics are printed to stderr as
;; located warnings (file:line:col: warning: …). Off by default: zero cost, no
;; behavior change. A checker error is swallowed — linting must never break a
;; compile. Only runtime-compiled code is linted; clojure.core and the prelude are
;; baked into the seed at mint time, so they are never re-checked here.
(define jolt-check-lint?
  (let ((e (getenv "JOLT_CHECK")))
    (and e (fx>? (string-length e) 0) (not (jolt-trace-env-off? e)))))
(define jolt-check-form-fn #f)
(define jolt-check-unit #f)
(define (jolt-check-form node)
  (unless jolt-check-form-fn
    (set! jolt-check-form-fn (var-deref "jolt.passes.types" "check-form"))
    (set! jolt-check-unit ((var-deref "jolt.passes.types" "new-unit"))))
  ;; non-strict: only the always-safe core error domains, no user-fn signature
  ;; probing (fewer false positives for a warning-level lint).
  (jolt-check-form-fn jolt-check-unit node #f))

;; "file:line:col" / "line:col" for a diagnostic's :pos map, the top-level form's
;; position as a fallback, or #f when neither is known.
(define (jolt-diag-loc-string pos)
  (let ((p (if (pmap? pos) pos (jolt-current-source))))
    (and (pmap? p)
         (let ((line (jolt-get p hc-kw-line jolt-nil))
               (col  (jolt-get p hc-kw-column jolt-nil))
               (file (jolt-get p hc-kw-file jolt-nil)))
           (string-append
             (if (jolt-nil? file) "" (string-append (jolt-str-render-one file) ":"))
             (if (jolt-nil? line) "?" (number->string line)) ":"
             (if (jolt-nil? col) "?" (number->string col)))))))

(define diag-kw-pos (keyword #f "pos"))
(define diag-kw-msg (keyword #f "msg"))
(define (jolt-lint-node! node)
  (when jolt-check-lint?
    (guard (_ (#t #f))
      (let ((diags (jolt-check-form node))
            (port (current-error-port)))
        (let loop ((s (jolt-seq diags)))
          (unless (jolt-nil? s)
            (let* ((d (seq-first s))
                   (loc (jolt-diag-loc-string (jolt-get d diag-kw-pos jolt-nil)))
                   (msg (jolt-get d diag-kw-msg jolt-nil)))
              (when loc (display loc port) (display ": " port))
              (display "warning: " port)
              (unless (jolt-nil? msg) (display (jolt-str-render-one msg) port))
              (newline port))
            (loop (jolt-seq (seq-more s)))))))))

;; Already-read FORM -> Scheme source string (analyze -> emit on Chez).
;; `ns` is the compile namespace unqualified symbols resolve against.
(define (jolt-analyze-emit-form form ns)
  (ce-scan-requires! form ns)
  (let* ((ctx (make-analyze-ctx ns))
         (node (jolt-ce-analyze ctx form)))
    (jolt-lint-node! node)
    (jolt-ce-emit (jolt-ce-run-passes node ctx))))

;; --- runtime defmacro -------------------------------------------------------
;; Shared with emit-image.ss (loaded after this). A defmacro lowers to a def of
;; its expander fn + a macro flag, exactly as the prelude emits build-time macros.

;; Is `f` a (defmacro ...) / (definline ...) form?
(define (ce-macro-form? f)
  (and (cseq? f)
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
  (and (cseq? form)
       (let ((h (seq-first form)))
         (and (symbol-t? h) (jolt-nil? (hc-sym-ns h))
              (string=? (symbol-t-name h) "do")))))

;; Clojure's compilation-unit rule applies to a top-level MACRO CALL too: the
;; form is macroexpanded first, and when the expansion lands on (do ...) its
;; children are compiled+evaluated as their own top-level forms — that is how a
;; macro can emit (do (require …) (deftest …)) with the require in force before
;; the deftest compiles. The expansion here is a PROBE: a non-do expansion is
;; discarded and the ORIGINAL form goes to the analyzer unchanged (its macro
;; arm re-expands, keeping def position stamping and the rest of that path), so
;; only forms that really unroll take this route. Special-form heads never
;; expand, mirroring analyze-list's precedence; an expander that throws is the
;; analyzer's to report, with its source position.
(define (ce-expand-to-top-do form ns)
  (let ((ctx (make-analyze-ctx ns)))
    (let loop ((f form) (n 0))
      (cond
        ((> n 100) #f)                    ; runaway expansion; leave it to the analyzer
        ((ce-top-do? f) f)
        ((and (cseq? f)
              (let ((h (seq-first f)))
                (and (symbol-t? h)
                     (not (hc-special? (symbol-t-name h)))
                     (hc-macro? ctx h))))
         (loop (guard (e (#t #f)) (hc-expand-1 ctx f)) (+ n 1)))
        (else #f)))))

;; Compile + eval ONE already-read form in ns `ns`; returns the value.
;;
;; `ns` is the form's namespace for BOTH halves of the job: unqualified symbols
;; are analyzed against it, and the runtime current ns is pointed at it too, so
;; *ns*, resolve, macroexpand, and anything a macro reads through them agree with
;; the ns the form compiled in. They used to be able to disagree — the analyzer
;; took `ns` and the runtime kept whatever the last load left behind — which made
;; a runtime (defmacro …) intern into `ns` and then be invisible to the very
;; resolve/macroexpand that would have to find it.
;;
;; The switch is NOT restored on the way out. That is deliberate and it is what
;; makes an (ns …) work: a form is allowed to move the current ns and have the
;; move outlive it, which is how the loader threads one file's ns through the
;; rest of that file, and how (eval '(in-ns 'foo)) lands you in foo. A caller
;; that wants the ambient ns back saves and restores around the call, the way
;; Clojure's load binds *ns* to itself — ldr-require-ns does, per file. The build
;; walks (build.ss, emit-image.ss ei-for-each-form) set the ns themselves before
;; walking a namespace's forms and want it to stay set, so for them this is a
;; write of the value that is already there.
;;
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
  ;; Written with set-chez-ns! and guarded by a difference test, so this is
  ;; exactly what an (in-ns …) in the form itself would do — including the one
  ;; case where it does nothing: under a (binding [*ns* …]) the current ns
  ;; DERIVES from that binding (multimethods.ss chez-current-ns), the caller
  ;; passed it as `ns` in the first place, and the test is already false.
  (unless (string=? ns (chez-current-ns)) (set-chez-ns! ns))
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
    ;; a macro call whose expansion is a (do ...): unroll the expansion the same
    ;; way (each child re-enters, so nested macro-to-do chains unroll too). The
    ;; children are macro-built and carry no reader position, so each list child
    ;; without one inherits the call form's — an error in a child (an ns form's
    ;; failing :require, say) must still point at the form in the source file.
    ((ce-expand-to-top-do form ns)
     => (lambda (expansion)
          (jolt-compile-eval-form
            (apply jolt-list
                   (cons (seq-first expansion)
                         (map (lambda (c) (hc-propagate-pos form c))
                              (cdr (seq->list expansion)))))
            ns)))
    ;; defmacro is compiled like any other form — the analyzer lowers it to a def
    ;; of the expander fn + (mark-macro! …) so subsequent forms expand it. One
    ;; macro-expansion path (no separate spine interception).
    (else
     ;; record this form's source location first, so a compile- or run-time error
     ;; in it reports the right place.
     (jolt-enter-form! form)
     (let* ((scm (jolt-analyze-emit-form form ns))
            (cap (jolt-aot-capture)))            ; tee for the AOT cache (loader.ss)
       (when cap (put-string cap scm) (newline cap))
       (if (jolt-ce-trace-frames?)
           (jolt-eval-with-source scm)
           (eval (read (open-input-string scm)) (interaction-environment)))))))

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
;; Establishes default thread bindings for JVM-compiler vars so vendored code
;; that (set! *warn-on-reflection* …) at the file level finds a thread-local slot.
(define (jolt-load-string s)
  (let ((end (string-length s))
        (drl (guard (_ (#t #f)) data-readers-active)))
    ;; dynamic-wind: a throw mid-load must still pop, or the frame leaks into
    ;; the caller's binding stack for the rest of the thread (observed poisoning
    ;; every later corpus case in a shared-process run).
    (dynamic-wind
      (lambda ()
        (jolt-push-thread-bindings
          (jolt-hash-map
            (jolt-var "clojure.core" "*warn-on-reflection*")
              (var-cell-root (jolt-var "clojure.core" "*warn-on-reflection*"))
            (jolt-var "clojure.core" "*assert*")
              (var-cell-root (jolt-var "clojure.core" "*assert*")))))
      (lambda ()
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
                    result)))))
      (lambda () (jolt-pop-thread-bindings)))))

;; eval / load-string are FUNCTIONS on the spine (the compiler image is resident
;; at runtime). eval takes an already-read FORM (e.g. from quote / list); it and
;; load-string compile+eval in the current ns. eval is removed from the analyzer's
;; special-symbol lists (host-contract.ss) so it resolves as an ordinary core var.
(def-var! "clojure.core" "eval"
  (lambda (form) (jolt-compile-eval-form form (chez-current-ns))))
(def-var! "clojure.core" "load-string" jolt-load-string)

;; --- jolt.scheme: the Scheme escape hatch (epic jolt-of08.6) ------------------
;; stdlib/jolt/scheme.clj is a thin veneer over these two. The contract is RAW:
;; nothing is marshaled in either direction — numbers, strings, booleans and
;; chars are shared representations anyway; everything else crosses as whatever
;; it is on the other side, and the docs say so. Host-specific by design.
;;
;; scheme-proc resolves a top-level Scheme binding at CALL time, through the
;; adapter's global-reflection seam (sa-baked-global — portcheck pins the raw
;; top-level-value/bound? primitives to scheme-adapter-runtime.ss). Its #f
;; sentinel conflates "unbound" with "bound to #f", which is fine here: this
;; fetches PROCEDURES, and a procedure is never #f. An unbound name answers a
;; catchable ex-info rather than Chez's raw error, because "you typo'd the
;; primitive" is the hatch's everyday failure — and in a tree-shaken binary it
;; is also how a shaken-out primitive reports itself.
(def-var! "jolt.host" "scheme-proc"
  (lambda (name)
    (let ((sym (string->symbol (jolt-need-string name))))
      (let ((v (sa-baked-global sym)))
        (or v
            (jolt-throw (jolt-ex-info
                         (string-append "no top-level Scheme binding: "
                                        (symbol->string sym))
                         (jolt-hash-map (jolt-keyword "name")
                                        (symbol->string sym)))))))))
;; scheme-eval-string reads SCHEME text with the Scheme reader (not jolt's) and
;; evaluates every form, returning the last value; definitions persist in the
;; interaction environment, where the runtime itself lives.
(def-var! "jolt.host" "scheme-eval-string"
  (lambda (s)
    (let ((p (open-input-string (jolt-need-string s))))
      (let loop ((last jolt-nil))
        (let ((form (read p)))
          (if (eof-object? form)
              last
              (loop (eval form))))))))
