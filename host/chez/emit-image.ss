;; emit-image.ss — the on-Chez compiler-image emitter.
;;
;; This is the stage2/stage3 half of the self-hosting fixpoint. The
;; analyze->emit runs ON CHEZ (jolt-ce-analyze / jolt-ce-emit, loaded from a
;; previously-built image): feed it stage1's image and it produces stage2; feed it
;; stage2 and it produces stage3. stage2 == stage3 byte-for-byte proves the
;; on-Chez compiler reproduces itself (self-hosting-bootstrap-research §4).
;;
;; Loaded after compile-eval.ss (needs jolt-ce-analyze/jolt-ce-emit/ce-scan-requires!,
;; make-analyze-ctx) and rt.ss (read-file-string, the reader's rdr-read-form).

;; --- build/emit phase profile -----------------------------------------------------
;; JOLT_BUILD_PROFILE=1 reports each phase's wall-clock milliseconds on stderr.
;; A build's time divides between jolt's own passes (source parses, the
;; whole-program fixpoint, per-form emit) and Chez's compile-file of the flat
;; runtime+app source, and those two call for unrelated fixes — so which one
;; dominates has to be observable rather than guessed at.
(define ei-profile? #f)
(define ei-profile-last 0)
(define ei-profile-start 0)
(define (ei-profile-init!)
  (set! ei-profile? (and (getenv "JOLT_BUILD_PROFILE") #t))
  (when ei-profile?
    (set! ei-profile-start (sa-real-time-ms))
    (set! ei-profile-last ei-profile-start)))
(define (ei-profile-pad s n)
  (if (>= (string-length s) n) s (string-append s (make-string (- n (string-length s)) #\space))))
(define (ei-profile-padl s n)
  (if (>= (string-length s) n) s (string-append (make-string (- n (string-length s)) #\space) s)))
;; Sub-phase accumulator. The two big phases are per-namespace loops, so their
;; parts can't be marked once — each part adds into a named bucket, reported
;; under its phase at the end. This is what says whether a phase is spending its
;; time parsing the same sources repeatedly, analyzing, or in the fixpoint.
(define ei-acc-tbl (make-hashtable string-hash string=?))
(define (ei-timed label thunk)
  (if ei-profile?
      (let ((t0 (sa-real-time-ms)))
        (let ((v (thunk)))
          (hashtable-update! ei-acc-tbl label (lambda (x) (+ x (- (sa-real-time-ms) t0))) 0)
          v))
      (thunk)))
(define (ei-acc-report!)
  (when ei-profile?
    (let-values (((ks vs) (hashtable-entries ei-acc-tbl)))
      ;; slowest first — the point of the breakdown is which part to attack
      (for-each
        (lambda (p)
          (display (string-append "jolt build: [profile]    - " (ei-profile-pad (car p) 21)
                                  (ei-profile-padl (number->string (cdr p)) 7) " ms\n")
                   (current-error-port)))
        (sort (lambda (a b) (> (cdr a) (cdr b)))
              (let loop ((i 0) (acc '()))
                (if (fx=? i (vector-length ks))
                    acc
                    (loop (fx+ i 1) (cons (cons (vector-ref ks i) (vector-ref vs i)) acc)))))))
    (hashtable-clear! ei-acc-tbl)
    (flush-output-port (current-error-port))))

;; Elapsed since the previous mark. Returns #f so it can sit in a let* binding
;; between two phases without disturbing them.
(define (ei-mark! label)
  (when ei-profile?
    (let ((now (sa-real-time-ms)))
      (display (string-append "jolt build: [profile] " (ei-profile-pad label 24)
                              (ei-profile-padl (number->string (- now ei-profile-last)) 7) " ms"
                              "   (cumulative " (number->string (- now ei-profile-start)) ")\n")
               (current-error-port))
      (flush-output-port (current-error-port))
      (set! ei-profile-last now)))
  #f)

;; Read every top-level form from a source string (a Chez read-all).
;; Uses the same reader the spine reads single forms with.
;; Loop by READING POSITION, not by the first eof-form: a non-matching reader
;; conditional (e.g. `#?(:cljs …)` with no :clj branch) reads as "no form" — an
;; eof marker mid-source — and must be skipped, not treated as end of file, or a
;; namespace's forms after it are silently dropped. Mirrors load-jolt-file.
;; rdr-read-top for the same reason load-jolt-file* uses it: a stray close
;; delimiter parks the position, which the no-progress arm below reads as end of
;; input, so a build would emit an image of everything BEFORE the bad paren and
;; report success. It has to be the same read error the loader raises, or `jolt
;; build` and `jolt run` disagree about what a file contains (jolt-3amm).
(define (ei-read-all src)
  (let ((end (string-length src)))
    (let loop ((i 0) (acc '()))
      (if (>= i end)
          (reverse acc)
          (let-values (((form j) (rdr-read-top src i end)))
            (if (> j i)
                (loop j (if (rdr-eof? form) acc (cons form acc)))
                (reverse acc)))))))

;; Is `f` an (ns ...) form? (Its only role in the image is alias registration; we
;; never emit it — the def-var!s carry explicit ns names.)
(define (ei-ns-form? f)
  (and (cseq? f)
       (let ((items (seq->list f)))
         (and (pair? items) (symbol-t? (car items))
              (string=? (symbol-t-name (car items)) "ns")))))

;; --- parsed top-level forms, shared by a build's two walks -------------------
;; `jolt build` reads and parses every app source TWICE: once in build.ss
;; bld-wp-infer! (to analyze + type it) and once in the emit walk (to consume
;; that analyzed IR positionally — one pop per form). Both read the same file
;; with the same reader and must see the same form sequence, so parsing is
;; exactly the kind of work the second walk can reuse instead of repeating.
;; Measured on kmet's 311 namespaces, same binary, alternating runs: emit's
;; per-namespace walk 40.1/41.2s with the cache against 46.3/47.2s without
;; (the wp walk's 5.0-5.6s parse, the first one, is unchanged either way).
;; The emit hook builds new forms rather than mutating them, so sharing is safe.
;;
;; Keyed by namespace with the SOURCE STRING it was parsed from, compared by
;; value: an in-process rebuild (nREPL, a second `build` in one session) after an
;; edit parses afresh instead of serving the previous source's forms.
;; JOLT_NO_FORM_CACHE=1 opts out — the same two walks then parse as before, at
;; the cost of the second parse — for a build on a memory-tight box, since the
;; cache holds every namespace's forms from the wp walk until its emit walk
;; reads them.
(define ei-form-cache-off? (and (getenv "JOLT_NO_FORM_CACHE") #t))
(define ei-form-cache (make-hashtable string-hash string=?))
(define (ei-read-all-for ns-name src)
  (if ei-form-cache-off?
      (ei-read-all src)
      (let ((e (hashtable-ref ei-form-cache ns-name #f)))
        (if (and e (string=? (car e) src))
            (cdr e)
            (let ((forms (ei-read-all src)))
              (hashtable-set! ei-form-cache ns-name (cons src forms))
              forms)))))
(define (ei-form-cache-clear!) (hashtable-clear! ei-form-cache))
;; Hand a parse to the next walk that asks ei-read-all-for for it. The graph's
;; scan (build.ss bld-scan-forms) fills this for sources it could read in normal
;; mode, so the wp walk skips its own parse of them; the wp walk fills it for the
;; rest before the emit walk consumes it.
(define (ei-form-cache-put! ns-name src forms)
  (unless ei-form-cache-off?
    (hashtable-set! ei-form-cache ns-name (cons src forms))))

;; ei-macro-form? / ei-defmacro->fn moved to compile-eval.ss (ce-macro-form? /
;; ce-defmacro->fn, loaded before this) — shared with the runtime defmacro spine.

;; Cross-compile one namespace's source to a list of guard-wrapped Scheme strings.
;; Each form is analyzed with a fresh ctx — resolution is via the runtime var-table
;; + alias tables, not ctx-accumulated state, so this matches the spine's per-form
;; analyze. A defmacro emits its expander fn as (def-var! ns name <fn>) +
;; (mark-macro! ns name) so the on-Chez analyzer can expand it.
;; Analyze -> (optionally run passes) -> emit one form. optimize? runs
;; jolt.passes/run-passes (build optimizes; the seed minter stays un-optimized so
;; the self-host fixpoint is independent of the passes). emit-top-form is the
;; top-level entry: in direct-link mode it binds jv$<fqn> for a top-level def; off
;; that mode (the minter, runtime eval) it is exactly emit, so output is unchanged.
(define jolt-ce-emit-top (var-deref "jolt.backend-scheme" "emit-top-form"))
;; emit-top-form grew a second arity naming the enclosing def for a form that
;; does not carry it (a macro's bare expander fn). The SEED that mints the next
;; one is the previous release's, whose emit-top-form is still single-arity, so
;; ask before passing it: pass 1 of a remint falls back and pass 2 — now running
;; the reminted seed — uses it. Without this the mint dies with "incorrect number
;; of arguments 2" before it can produce the seed that would accept them.
(define (ei-emit-top-2? )
  (and (procedure? jolt-ce-emit-top)
       (bitwise-bit-set? (procedure-arity-mask jolt-ce-emit-top) 2)))
(define (ei-emit-top ir fnsrc-def)
  (if (and fnsrc-def (ei-emit-top-2?))
      (jolt-ce-emit-top ir fnsrc-def)
      (jolt-ce-emit-top ir)))
;; Hoist the var cell behind every late-bound reference in the minted seed, so a
;; clojure.core fn that calls another core fn resolves the callee's cell once per
;; def instead of once per call. This was OFF, on the theory that the gensym-
;; numbered cell names would not survive the mint's byte-fixpoint; they do —
;; remint converges in 3 passes — and the shape is the same eager const-pool hoist
;; the keyword literals have used here all along (backend_scheme.clj
;; hoist-var-cell). It is worth the pass: the name lookup it removes is a
;; string-append plus a string-hash probe, ~102ns, against ~1ns for the hoisted
;; read, and 258 of core's 357 emitted vars carry at least one such reference —
;; frequencies resolves assoc! per ELEMENT. Measured on 1M elements, that alone
;; put clojure.core/frequencies at 439ms against 337ms for the identical body
;; written in user code (which has had this since the per-site cache landed);
;; both are 337ms now.
;;
;; Costs, measured against the same hello-world binary built either way: +297KB
;; of binary (+1.3%) and +4.6ms of startup (+2.0%).
;;
;; Guarded for the first re-mint pass off an older seed.
(let ((scv (var-deref "jolt.backend-scheme" "set-var-cache!")))
  (when (procedure? scv) (scv #t)))
;; Tail-frame tracing off for the mint + `jolt build`: the seed must stay a
;; byte-fixpoint, and a built app should carry no per-call trace overhead.
(let ((stf (var-deref "jolt.backend-scheme" "set-trace-frames!")))
  (when (procedure? stf) (stf #f)))
;; Source-map registration off here too (compile-eval.ss turned it on for runtime
;; eval). A registration carries the def's absolute path, so minting core with it
;; on would bake THIS machine's paths into prelude.ss and the seed would stop
;; being a byte-fixpoint anywhere else. `jolt build` registers app namespaces
;; through emit-def-cached, which is unaffected by this flag.
(let ((ssr (var-deref "jolt.backend-scheme" "set-source-reg!")))
  (when (procedure? ssr) (ssr #f)))
;; The per-compilation inference/pass context run-passes threads (jolt.passes.types/
;; new-unit) instead of module-global state. Created LAZILY: the first re-mint off an
;; older seed has no new-unit var yet, but the mint never optimizes so it never
;; reaches ei-unit. bld-wp-infer! (build.ss) calls ei-fresh-unit! per build and shares
;; this same context with the run-passes calls below, so whole-program seeds flow.
(define jolt-ei-new-unit (var-deref "jolt.passes.types" "new-unit"))
(define jolt-set-emit-unit! (var-deref "jolt.backend-scheme" "set-emit-unit!"))
(define ei-unit-box #f)
(define (ei-unit) (or ei-unit-box (begin (set! ei-unit-box (jolt-ei-new-unit)) ei-unit-box)))
;; a build creates one unit per build and publishes it immediately, so the mode flags
;; a build then sets (set-direct-link!/set-var-cache!) land on THIS unit — the same one
;; the per-form emit reads its emit-session state from.
(define (ei-fresh-unit!) (set! ei-unit-box (jolt-ei-new-unit)) (ei-publish-unit!) ei-unit-box)
;; publish the current unit to the backend so the emit reads its emit-session state
;; (flags, gensym, cache-cells, ctor-shapes) and the contagion clone reaches it.
;; Guarded for the first re-mint off an older seed (no set-emit-unit! yet).
(define (ei-publish-unit!) (when (procedure? jolt-set-emit-unit!) (jolt-set-emit-unit! (ei-unit))))
;; A fresh unit starts with every mode flag off, so a driver that publishes one
;; for a single emission must put the caller's unit back when it is done.
;; Leaving the fresh unit published left the rest of the process compiling with
;; var-cache off: every stdlib fasl build-jolt captured after the CLI AOT
;; section resolved each var reference by name on every call (~7x a cached
;; site), and an in-process build (nREPL) did the same to the session's later
;; compiles. ei-save-unit / ei-restore-unit! bracket ei-fresh-unit!.
(define jolt-current-emit-unit (var-deref "jolt.backend-scheme" "current-unit"))
(define (ei-save-unit)
  (cons ei-unit-box (and (procedure? jolt-current-emit-unit) (jolt-current-emit-unit))))
(define (ei-restore-unit! saved)
  (set! ei-unit-box (car saved))
  (when (and (cdr saved) (procedure? jolt-set-emit-unit!))
    (jolt-set-emit-unit! (cdr saved))))
;; --- whole-program analysis cache for --opt builds ----------------------------
;; bld-wp-infer! populates this; ei-compile-form checks it to skip re-analysis.
(define ei-cached-ir (make-hashtable string-hash string=?))
(define ei-cached-ir-idx (make-hashtable string-hash string=?))

;; Held as a VECTOR rather than the list bld-wp-infer! builds: the emit walk pops
;; one form per call, and indexing a list cost a (length …) plus a (list-ref …)
;; per pop — quadratic in a namespace's form count, paid on the largest ported
;; namespaces precisely because they have the most forms.
(define (ei-set-cached! ns forms)
  (hashtable-set! ei-cached-ir ns (if (vector? forms) forms (list->vector forms)))
  (hashtable-set! ei-cached-ir-idx ns 0))

(define (ei-next-cached ns)
  (let ((idx (hashtable-ref ei-cached-ir-idx ns #f))
        (forms (hashtable-ref ei-cached-ir ns #f)))
    (cond
      ((not (and idx forms)) #f)
      ((fx<? idx (vector-length forms))
       (hashtable-set! ei-cached-ir-idx ns (fx+ idx 1))
       (vector-ref forms idx))
      (else
       ;; Exhausted: this namespace's emit walk is done with its whole-program IR,
       ;; so drop it instead of holding every namespace's IR live until the build
       ;; ends. Peak heap otherwise scales with the whole program. Returning #f is
       ;; what an absent entry already returns, so a later call is unaffected.
       (hashtable-delete! ei-cached-ir ns)
       (hashtable-delete! ei-cached-ir-idx ns)
       #f))))

(define (ei-clear-cached!)
  (hashtable-clear! ei-cached-ir)
  (hashtable-clear! ei-cached-ir-idx))

;; FNSRC-DEF (optional) names the enclosing def for a form that does not carry
;; the name — a macro's expander arrives here as a bare fn. See emit-top-form.
(define ei-compile-form
  (case-lambda
    ((ctx f optimize?) (ei-compile-form ctx f optimize? #f))
    ((ctx f optimize? fnsrc-def)
     (let* ((ns (chez-actx-cns ctx))
            (cached (and optimize? (ei-next-cached ns)))
            (ir (or cached
                    (ei-timed "emit: analyze" (lambda () (jolt-ce-analyze ctx f))))))
       (when optimize? (ei-publish-unit!))
       (let ((ir* (if optimize?
                      (ei-timed "emit: run-passes" (lambda () (jolt-ce-run-passes ir ctx (ei-unit))))
                      ir)))
         (ei-timed "emit: emit-top"
                   (lambda () (ei-emit-top ir* fnsrc-def))))))))

;; The emitted `(def-var! …)(mark-macro! …)` pair for a defmacro, guard-wrapped
;; (tolerant) or bare (strict) to match guard?. With meta-scm (the derived
;; :doc/:arglists cross-compiled as quoted data) the def becomes
;; def-var-with-meta!, so an image-baked macro answers (meta #'when) like the
;; runtime spine does instead of the bare {:ns :name}.
(define (ei-macro-string ns-name nm scm meta-scm guard?)
  (let ((def-str (if meta-scm
                     (string-append "(def-var-with-meta! " (ei-str-lit ns-name) " " (ei-str-lit nm)
                                    "\n    " scm "\n    " meta-scm ")")
                     (string-append "(def-var! " (ei-str-lit ns-name) " " (ei-str-lit nm)
                                    "\n    " scm ")"))))
    (if guard?
        (string-append "(guard (e (#t #f))\n  " def-str
                       "\n  (mark-macro! " (ei-str-lit ns-name) " " (ei-str-lit nm) "))")
        (string-append def-str
                       "\n(mark-macro! " (ei-str-lit ns-name) " " (ei-str-lit nm) ")"))))

;; A defmacro's derived meta pmap, cross-compiled as quoted data (or #f).
;; Emitted through analyze+emit-top directly — NOT ei-compile-form, whose
;; optimize? arm pops the positionally-aligned whole-program IR cache (one pop
;; per SOURCE form; the meta is not a source form).
(define (ei-emit-meta ns mmap guard?)
  (and mmap
       (let ((emit1 (lambda ()
                      (jolt-ce-emit-top
                        (jolt-ce-analyze (make-analyze-ctx ns)
                          (jolt-list (jolt-symbol #f "quote") mmap))))))
         (if guard? (guard (e (#t #f)) (emit1)) (emit1)))))

;; Cross-compile one namespace's source to a list of Scheme strings — shared by
;; the seed minter (ei-emit-ns: optimize? #f, guard? #t — tolerant, skips a form
;; that fails to emit) and `jolt build` (bld-emit-ns: optimize? #t, guard? #f —
;; strict, a failing form errors the build).
;; A per-form transform applied to each read form before emit — the build sets it
;; to the data-reader rewrite (loader.ss ldr-apply-readers) so a registered #tag
;; literal compiles in a `jolt build` the same as it does in an interpreted load.
;; #f (the default, and during the seed mint where loader.ss isn't loaded) is no
;; transform, so emit-image.ss carries no loader dependency.
(define ei-emit-form-hook (make-parameter #f))

;; Read every form of a namespace's source once — ns set for ::kw resolution, the
;; per-form hook (the data-reader rewrite under `jolt build`) applied, requires
;; scanned — and dispatch each classified form to proc. Shared by the direct-emit
;; path (ei-emit-ns*) and the record path (ei-emit-ns-records) so a --tree-shake
;; build applies the SAME form transforms as a plain build. The two forked once and
;; the record path dropped the hook, emitting a semantically different binary off the
;; same source (a #tag literal the plain path rewrites crashed uncompilable).
;; A compiler-flag set! — (set! *unchecked-math* …) / (set! *warn-on-reflection* …)
;; at a file's top level. These have to be EVALUATED as emission walks past them,
;; not just emitted: the analyzer reads *unchecked-math* while lowering each later
;; form (host-contract.ss hc-unchecked-math? decides whether +/-/* become their
;; wrapping variants), so a form emitted before the set! runs would compile with
;; the flag off. The reference compiler gets this for free because Compiler.compile
;; evaluates top-level forms as it compiles them.
(define (ei-flag-set-form? f)
  (and (cseq? f)
       (let ((items (seq->list f)))
         (and (= (length items) 3) (symbol-t? (car items))
              (string=? (symbol-t-name (car items)) "set!")
              (symbol-t? (cadr items))
              (let ((n (symbol-t-name (cadr items))))
                (or (string=? n "*unchecked-math*")
                    (string=? n "*warn-on-reflection*")))))))

(define (ei-for-each-form ns-name src proc)
  (set-chez-ns! ns-name)               ; ::kw resolves against this ns
  (let ((hook (ei-emit-form-hook)))
    ;; The same thread binding the loader and a built binary use, so a flag set!
    ;; is legal here and its effect ends with this namespace instead of bleeding
    ;; into whichever one is emitted next.
    (dynamic-wind
      jolt-ns-load-vars-push!
      (lambda ()
        (let loop ((forms (ei-read-all-for ns-name src)) (ord 0))
          (unless (null? forms)
            (let ((f (if hook (hook (car forms)) (car forms))))
              ;; ord mirrors the loader's per-file top-level form counter
              ;; (load-jolt-file*): every form read, the ns form included, so the
              ;; def-ordinal visibility replay (rt.ss var-def-ordinals) compares
              ;; a form's analysis against the same stamps the in-order pass-1
              ;; load wrote. Set around the whole dispatch — analysis AND the
              ;; flag-form evals — and, like the loader's, left BEFORE the tail
              ;; call, so a namespace's forms do not nest one parameterize per
              ;; form for the length of the file.
              (parameterize ((jolt-form-ordinal ord))
                (ce-scan-requires! f ns-name)
                (cond
                  ((ei-ns-form? f) #f)
                  ((ce-macro-form? f)
                   ;; macro kind hands proc (fn-form . meta-pmap-or-#f) — the
                   ;; derived :doc/:arglists ride along to the def emission.
                   (let-values (((nm fn-form mmap) (ce-defmacro->fn f)))
                     (hc-mint-walked! ns-name nm)
                     (hc-mint-ungated
                       (lambda () (proc ns-name 'macro nm (cons fn-form mmap))))))
                  (else
                   (when (ei-flag-set-form? f)
                     (jolt-compile-eval-form f ns-name))
                   (proc ns-name 'form #f f))))
              (loop (cdr forms) (fx+ ord 1))))))
      jolt-ns-load-vars-pop!)))

;; Count of forms silently dropped during a guarded emit (a source form that
;; fails to compile is skipped so a partial overlay still boots). remint.sh reads
;; this after the fixpoint pass and fails on a nonzero count — a regression that
;; makes an overlay defn uncompilable would otherwise just delete the var while
;; the byte-fixpoint still converges. Reset at the start of each full emit.
(define ei-skipped-count 0)
(define (ei-reset-skipped!) (set! ei-skipped-count 0))

;; Does SCM bind a top-level jv$ variable — a direct-linked def from
;; emit-def-cached? Such a form cannot be load-guarded: `define` inside a
;; guard's body is an internal definition, invisible at the top level, so the
;; mint emits it bare. A def whose init raises at load then aborts the boot
;; instead of vanishing quietly — the failure run-seed-defs.ss exists to catch.
(define (ei-top-level-define? scm)
  (let* ((pat "(define jv$") (m (string-length pat)) (n (string-length scm)))
    (let loop ((i 0))
      (cond ((fx> (fx+ i m) n) #f)
            ((let cmp ((k 0))
               (or (fx= k m)
                   (and (char=? (string-ref scm (fx+ i k)) (string-ref pat k))
                        (cmp (fx+ k 1)))))
             #t)
            (else (loop (fx+ i 1)))))))

;; What a mint report calls a form: the macro's name, else the name a
;; (def… name …) form defines, else <top-level-form>.
(define (ei-form-label nm form)
  (or nm
      (and (cseq? form)
           (let ((items (seq->list form)))
             (and (pair? items) (pair? (cdr items))
                  (symbol-t? (car items)) (symbol-t? (cadr items))
                  (let ((h (symbol-t-name (car items))))
                    (and (fx>= (string-length h) 3) (string=? (substring h 0 3) "def")))
                  (symbol-t-name (cadr items)))))
      "<top-level-form>"))

(define (ei-emit-ns* ns-name src optimize? guard?)
  (let ((acc '()))
    (ei-for-each-form ns-name src
      (lambda (ns kind nm f)
        (let* ((form (if (eq? kind 'macro) (car f) f))
               ;; a macro's expander is a bare fn form here, so hand the emitter
               ;; the macro's name for fn-form registration (see emit-top-form)
               (fnsrc-def (and (eq? kind 'macro) nm))
               ;; A guarded form that fails is reported by name only; the
               ;; reason is deliberately quiet during the fixpoint (an early
               ;; pass compiles against an older seed and fails forms a later
               ;; pass emits fine). JOLT_MINT_DEBUG=1 prints it, for the pass
               ;; that stays failing at convergence.
               (scm (if guard?
                        (guard (e (#t (when (getenv "JOLT_MINT_DEBUG")
                                        (fprintf (current-error-port) "mint: ~a/~a raised: ~a\n" ns (ei-form-label nm form)
                                                 (if (and (condition? e) (message-condition? e))
                                                     (apply format (condition-message e) (if (irritants-condition? e) (condition-irritants e) '()))
                                                     (guard (_ (#t "?")) (jolt-repl-str (jolt-unwrap-throw e))))))
                                      #f))
                          (ei-compile-form (make-analyze-ctx ns) form optimize? fnsrc-def))
                        (ei-compile-form (make-analyze-ctx ns) form optimize? fnsrc-def))))
          (if (and guard? (not scm))
              ;; a form the guard swallowed — report it so the drop isn't silent
              (begin
                (set! ei-skipped-count (+ ei-skipped-count 1))
                (fprintf (current-error-port) "mint: skipped ~a/~a (failed to compile)\n"
                         ns (ei-form-label nm form)))
              (set! acc
                    (cons (if (eq? kind 'macro)
                              (ei-macro-string ns nm scm (ei-emit-meta ns (cdr f) guard?) guard?)
                              (if (and guard? (not (ei-top-level-define? scm)))
                                  (string-append "(guard (e (#t #f))\n  " scm ")")
                                  scm))
                          acc))))))
    (reverse acc)))

(define (ei-emit-ns ns-name src) (ei-emit-ns* ns-name src #f #t))

;; --- DCE record producer ----------------------------------------------------
;; Cross-compile a namespace's source to tree-shaking records — the app/library
;; counterpart to dce-blob-records (the prelude). The shake itself and all dce-*
;; helpers live in dce.ss; this stays here because it drives the ei-* compiler. A
;; top-level def becomes a prunable record; any other form a kept (side-effecting)
;; record whose refs are roots. A macro is prunable — its expander isn't called at
;; runtime in an AOT build. Refs come from dce-app-refs (IR walk UNIONED with a text
;; scan of the emitted Scheme) so a var-deref the back end emits outside a :var node
;; still roots its target.
(define (ei-emit-ns-records ns-name src)
  (ei-publish-unit!)
  (let ((acc '()))
    (ei-for-each-form ns-name src
      (lambda (ns kind nm f)
        (let* ((form (if (eq? kind 'macro) (car f) f))
               (ctx (make-analyze-ctx ns))
               (cached (ei-next-cached ns))
               (analyzed (or cached
                             (ei-timed "emit: analyze (uncached)"
                               (lambda () (jolt-ce-analyze ctx form)))))
               (ir (ei-timed "emit: run-passes"
                     (lambda () (jolt-ce-run-passes analyzed ctx (ei-unit)))))
               (str (if (eq? kind 'macro)
                        (ei-macro-string ns nm
                          (ei-timed "emit: emit-macro" (lambda () (ei-emit-top ir nm)))
                          (ei-emit-meta ns (cdr f) #f) #f)
                        (ei-timed "emit: emit-top" (lambda () (jolt-ce-emit-top ir)))))
               (fqn (if (eq? kind 'macro) (string-append ns "/" nm) (dce-def-fqn ir)))
               (refs (ei-timed "emit: dce-refs" (lambda () (dce-app-refs ir str)))))
          (set! acc (cons (if fqn (dce-rec #f fqn refs str (dce-def-init-runs? ir)) (dce-rec #t #f refs str)) acc)))))
    (reverse acc)))

;; Scheme string literal for a ns/name — uses the runtime's own writer
;; (printable ASCII identifiers only here).
(define (ei-str-lit s) (with-output-to-string (lambda () (write s))))

;; Emit (string->utf8 "...") so the embedded value is a bytevector (1B/char)
;; instead of a UCS-4 string (4B/char) — saves ~9MB heap per 3MB of source.
(define (ei-bytes-lit s)
  (string-append "(string->utf8 " (ei-str-lit s) ")"))

;; The compiler namespaces, in load order: each after every namespace it
;; requires, which the mint's in-order visibility gate holds it to. The passes
;; (fold/inline/types + the jolt.passes façade) load after ir so run-passes is
;; available to the back end; fold/inline/types come before the façade that
;; :refers them, and the back end, which requires jolt.passes.types and
;; jolt.passes.numeric, comes last.
(define ei-compiler-ns-files
  (list (cons "jolt.op-registry" "jolt-core/jolt/op_registry.clj")
        (cons "jolt.ir" "jolt-core/jolt/ir.clj")
        (cons "jolt.analyzer" "jolt-core/jolt/analyzer.clj")
        (cons "jolt.passes.fold" "jolt-core/jolt/passes/fold.clj")
        (cons "jolt.passes.numeric" "jolt-core/jolt/passes/numeric.clj")
        (cons "jolt.passes.inline" "jolt-core/jolt/passes/inline.clj")
        (cons "jolt.passes.types.lattice" "jolt-core/jolt/passes/types/lattice.clj")
        (cons "jolt.passes.types.check" "jolt-core/jolt/passes/types/check.clj")
        (cons "jolt.passes.types" "jolt-core/jolt/passes/types.clj")
        (cons "jolt.passes" "jolt-core/jolt/passes.clj")
        (cons "jolt.backend-scheme" "jolt-core/jolt/backend_scheme.clj")))

;; The clojure.core tiers + stdlib namespaces, in load order.
;; Re-emitting these on Chez is the
;; prelude half of the fixpoint (the whole emitted system reproducing itself).
;; This list IS the load order — the numeric filename prefixes are only a hint and
;; don't disambiguate a tie: 00-syntax loads BEFORE 00-kernel (it provides the
;; when/and/or/cond macros the kernel-adjacent tiers lean on).
(define ei-prelude-ns-files
  (append
    (map (lambda (tf) (cons "clojure.core" (string-append "jolt-core/clojure/core/" tf ".clj")))
         '("00-syntax" "00-kernel" "10-seq" "20-coll" "21-coll" "22-coll" "25-sorted" "30-macros" "40-lazy" "50-io" "60-gvec"))
    (list (cons "clojure.string" "stdlib/clojure/string.clj")
          (cons "clojure.walk" "stdlib/clojure/walk.clj")
          (cons "clojure.template" "stdlib/clojure/template.clj")
          (cons "clojure.edn" "stdlib/clojure/edn.clj")
          (cons "clojure.set" "stdlib/clojure/set.clj")
          (cons "clojure.pprint" "stdlib/clojure/pprint.clj")
          (cons "clojure.repl" "stdlib/clojure/repl.clj")
          (cons "clojure.main" "stdlib/clojure/main.clj")
          ;; LAST: the generated :doc/:arglists shard fills what the sources
          ;; above did not declare, for every image ns (tools/gen-core-docs.sh).
          (cons "clojure.core" "jolt-core/clojure/core/90-docs.clj"))))

;; Join a list of form strings with "\n", no trailing newline.
(define (ei-join forms)
  (let join ((fs forms) (out ""))
    (cond
      ((null? fs) out)
      ((string=? out "") (join (cdr fs) (car fs)))
      (else (join (cdr fs) (string-append out "\n" (car fs)))))))

;; Re-emit the whole list of (ns . file) pairs ON CHEZ as one Scheme string.
;; Walked in list order, which is load order: Chez's `map` applies its procedure
;; in no fixed order (right to left in practice), and the mint's in-order
;; visibility gate (host-contract.ss hc-mint-unwalked) needs each namespace
;; compiled after the ones it loads after.
;;
;; Each file is read with its CLASSPATH name as the reader's file, so a def in it
;; carries :file the way the reference's does ("clojure/string.clj", resolvable
;; with io/resource) instead of :line alone. The checkout prefix is what makes a
;; path depend on where the tree sits, so it is the part dropped: the name is the
;; same on every machine, and the seed stays a byte-fixpoint.
(define (ei-classpath-name file)
  (let loop ((ps '("jolt-core/" "stdlib/")))
    (cond ((null? ps) file)
          ((and (>= (string-length file) (string-length (car ps)))
                (string=? (substring file 0 (string-length (car ps))) (car ps)))
           (substring file (string-length (car ps)) (string-length file)))
          (else (loop (cdr ps))))))
(define (ei-emit-ns-files nfs)
  (let loop ((nfs nfs) (acc '()))
    (if (null? nfs)
        (ei-join (apply append (reverse acc)))
        (loop (cdr nfs)
              (cons (parameterize ((rdr-source-file (ei-classpath-name (cdr (car nfs)))))
                      (ei-emit-ns (car (car nfs)) (read-file-string (cdr (car nfs)))))
                    acc)))))

;; Emit the compiler image (jolt.ir + jolt.analyzer + jolt.backend-scheme) on Chez.
(define (jolt-emit-image) (ei-emit-ns-files ei-compiler-ns-files))

;; Emit the clojure.core prelude (all tiers + stdlib) on Chez — the prelude half of
;; the self-hosting fixpoint.
(define (jolt-emit-prelude) (ei-emit-ns-files ei-prelude-ns-files))
