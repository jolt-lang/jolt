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

;; Read every top-level form from a source string (a Chez read-all).
;; Uses the same reader the spine reads single forms with.
;; Loop by READING POSITION, not by the first eof-form: a non-matching reader
;; conditional (e.g. `#?(:cljs …)` with no :clj branch) reads as "no form" — an
;; eof marker mid-source — and must be skipped, not treated as end of file, or a
;; namespace's forms after it are silently dropped. Mirrors load-jolt-file.
(define (ei-read-all src)
  (let ((end (string-length src)))
    (let loop ((i 0) (acc '()))
      (if (>= i end)
          (reverse acc)
          (let-values (((form j) (rdr-read-form src i end)))
            (if (> j i)
                (loop j (if (rdr-eof? form) acc (cons form acc)))
                (reverse acc)))))))

;; Is `f` an (ns ...) form? (Its only role in the image is alias registration; we
;; never emit it — the def-var!s carry explicit ns names.)
(define (ei-ns-form? f)
  (and (cseq? f) (cseq-list? f)
       (let ((items (seq->list f)))
         (and (pair? items) (symbol-t? (car items))
              (string=? (symbol-t-name (car items)) "ns")))))

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
;; Seed mint and AOT build must stay byte-deterministic, so emit the image with var
;; cell-caching OFF (compile-eval.ss turned it on for runtime eval; this file loads
;; after it). Guarded for the first re-mint pass off an older seed.
(let ((scv (var-deref "jolt.backend-scheme" "set-var-cache!")))
  (when (procedure? scv) (scv #f)))
;; Tail-frame tracing off for the mint + `jolt build`: the seed must stay a
;; byte-fixpoint, and a built app should carry no per-call trace overhead.
(let ((stf (var-deref "jolt.backend-scheme" "set-trace-frames!")))
  (when (procedure? stf) (stf #f)))
;; --- whole-program analysis cache for --opt builds ----------------------------
;; bld-wp-infer! populates this; ei-compile-form checks it to skip re-analysis.
(define ei-cached-ir (make-hashtable string-hash string=?))
(define ei-cached-ir-idx (make-hashtable string-hash string=?))

(define (ei-set-cached! ns forms)
  (hashtable-set! ei-cached-ir ns forms)
  (hashtable-set! ei-cached-ir-idx ns 0))

(define (ei-next-cached ns)
  (let ((idx (hashtable-ref ei-cached-ir-idx ns #f))
        (forms (hashtable-ref ei-cached-ir ns #f)))
    (if (and idx forms (< idx (length forms)))
        (begin (hashtable-set! ei-cached-ir-idx ns (+ idx 1))
               (list-ref forms idx))
        #f)))

(define (ei-clear-cached!)
  (hashtable-clear! ei-cached-ir)
  (hashtable-clear! ei-cached-ir-idx))

(define (ei-compile-form ctx f optimize?)
  (let* ((ns (chez-actx-cns ctx))
         (cached (and optimize? (ei-next-cached ns)))
         (ir (or cached (jolt-ce-analyze ctx f))))
    (jolt-ce-emit-top (if optimize? (jolt-ce-run-passes ir ctx) ir))))

;; The emitted `(def-var! …)(mark-macro! …)` pair for a defmacro, guard-wrapped
;; (tolerant) or bare (strict) to match guard?.
(define (ei-macro-string ns-name nm scm guard?)
  (if guard?
      (string-append "(guard (e (#t #f))\n  (def-var! " (ei-str-lit ns-name) " " (ei-str-lit nm)
                     "\n    " scm ")\n  (mark-macro! " (ei-str-lit ns-name) " " (ei-str-lit nm) "))")
      (string-append "(def-var! " (ei-str-lit ns-name) " " (ei-str-lit nm) "\n  " scm
                     ")\n(mark-macro! " (ei-str-lit ns-name) " " (ei-str-lit nm) ")")))

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
(define (ei-for-each-form ns-name src proc)
  (set-chez-ns! ns-name)               ; ::kw resolves against this ns
  (let ((hook (ei-emit-form-hook)))
    (let loop ((forms (ei-read-all src)))
      (unless (null? forms)
        (let ((f (if hook (hook (car forms)) (car forms))))
          (ce-scan-requires! f ns-name)
          (cond
            ((ei-ns-form? f) (loop (cdr forms)))
            ((ce-macro-form? f)
             (let-values (((nm fn-form) (ce-defmacro->fn f)))
               (proc ns-name 'macro nm fn-form))
             (loop (cdr forms)))
            (else
             (proc ns-name 'form #f f)
             (loop (cdr forms)))))))))

(define (ei-emit-ns* ns-name src optimize? guard?)
  (let ((acc '()))
    (ei-for-each-form ns-name src
      (lambda (ns kind nm f)
        (let ((scm (if guard?
                       (guard (e (#t #f)) (ei-compile-form (make-analyze-ctx ns) f optimize?))
                       (ei-compile-form (make-analyze-ctx ns) f optimize?))))
          (unless (and guard? (not scm))
            (set! acc
                  (cons (if (eq? kind 'macro)
                            (ei-macro-string ns nm scm guard?)
                            (if guard? (string-append "(guard (e (#t #f))\n  " scm ")") scm))
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
  (let ((acc '()))
    (ei-for-each-form ns-name src
      (lambda (ns kind nm f)
        (let* ((ctx (make-analyze-ctx ns))
               (cached (ei-next-cached ns))
               (ir (jolt-ce-run-passes (or cached (jolt-ce-analyze ctx f)) ctx))
               (str (if (eq? kind 'macro)
                        (ei-macro-string ns nm (jolt-ce-emit-top ir) #f)
                        (jolt-ce-emit-top ir)))
               (fqn (if (eq? kind 'macro) (string-append ns "/" nm) (dce-def-fqn ir)))
               (refs (dce-app-refs ir str)))
          (set! acc (cons (if fqn (dce-rec #f fqn refs str) (dce-rec #t #f refs str)) acc)))))
    (reverse acc)))

;; Scheme string literal for a ns/name — uses the runtime's own writer
;; (printable ASCII identifiers only here).
(define (ei-str-lit s) (with-output-to-string (lambda () (write s))))

;; Emit (string->utf8 "...") so the embedded value is a bytevector (1B/char)
;; instead of a UCS-4 string (4B/char) — saves ~9MB heap per 3MB of source.
(define (ei-bytes-lit s)
  (string-append "(string->utf8 " (ei-str-lit s) ")"))

;; The compiler namespaces, in load order. The passes (fold/inline/types + the
;; jolt.passes façade) load after ir so run-passes is available to the back end;
;; fold/inline/types come before the façade that :refers them.
(define ei-compiler-ns-files
  (list (cons "jolt.ir" "jolt-core/jolt/ir.clj")
        (cons "jolt.analyzer" "jolt-core/jolt/analyzer.clj")
        (cons "jolt.backend-scheme" "jolt-core/jolt/backend_scheme.clj")
        (cons "jolt.passes.fold" "jolt-core/jolt/passes/fold.clj")
        (cons "jolt.passes.numeric" "jolt-core/jolt/passes/numeric.clj")
        (cons "jolt.passes.inline" "jolt-core/jolt/passes/inline.clj")
        (cons "jolt.passes.types.lattice" "jolt-core/jolt/passes/types/lattice.clj")
        (cons "jolt.passes.types.check" "jolt-core/jolt/passes/types/check.clj")
        (cons "jolt.passes.types" "jolt-core/jolt/passes/types.clj")
        (cons "jolt.passes" "jolt-core/jolt/passes.clj")))

;; The clojure.core tiers + stdlib namespaces, in load order.
;; Re-emitting these on Chez is the
;; prelude half of the fixpoint (the whole emitted system reproducing itself).
(define ei-prelude-ns-files
  (append
    (map (lambda (tf) (cons "clojure.core" (string-append "jolt-core/clojure/core/" tf ".clj")))
         '("00-syntax" "00-kernel" "10-seq" "20-coll" "21-coll" "22-coll" "25-sorted" "30-macros" "40-lazy" "50-io"))
    (list (cons "clojure.string" "stdlib/clojure/string.clj")
          (cons "clojure.walk" "stdlib/clojure/walk.clj")
          (cons "clojure.template" "stdlib/clojure/template.clj")
          (cons "clojure.edn" "stdlib/clojure/edn.clj")
          (cons "clojure.set" "stdlib/clojure/set.clj")
          (cons "clojure.pprint" "stdlib/clojure/pprint.clj"))))

;; Join a list of form strings with "\n", no trailing newline.
(define (ei-join forms)
  (let join ((fs forms) (out ""))
    (cond
      ((null? fs) out)
      ((string=? out "") (join (cdr fs) (car fs)))
      (else (join (cdr fs) (string-append out "\n" (car fs)))))))

;; Re-emit the whole list of (ns . file) pairs ON CHEZ as one Scheme string.
(define (ei-emit-ns-files nfs)
  (ei-join (apply append
             (map (lambda (nf) (ei-emit-ns (car nf) (read-file-string (cdr nf)))) nfs))))

;; Emit the compiler image (jolt.ir + jolt.analyzer + jolt.backend-scheme) on Chez.
(define (jolt-emit-image) (ei-emit-ns-files ei-compiler-ns-files))

;; Emit the clojure.core prelude (all tiers + stdlib) on Chez — the prelude half of
;; the self-hosting fixpoint.
(define (jolt-emit-prelude) (ei-emit-ns-files ei-prelude-ns-files))
