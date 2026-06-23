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
(define (ei-read-all src)
  (let ((end (string-length src)))
    (let loop ((i 0) (acc '()))
      (let-values (((form j) (rdr-read-form src i end)))
        (if (rdr-eof? form)
            (reverse acc)
            (loop j (cons form acc)))))))

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
(define (ei-compile-form ctx f optimize?)
  (let ((ir (jolt-ce-analyze ctx f)))
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
(define (ei-emit-ns* ns-name src optimize? guard?)
  (let loop ((forms (ei-read-all src)) (acc '()))
    (if (null? forms)
        (reverse acc)
        (let ((f (car forms)))
          (ce-scan-requires! f ns-name)
          (cond
            ((ei-ns-form? f) (loop (cdr forms) acc))
            ((ce-macro-form? f)
             (let-values (((nm fn-form) (ce-defmacro->fn f)))
               (let ((scm (if guard?
                              (guard (e (#t #f)) (ei-compile-form (make-analyze-ctx ns-name) fn-form optimize?))
                              (ei-compile-form (make-analyze-ctx ns-name) fn-form optimize?))))
                 (loop (cdr forms)
                       (if (and guard? (not scm)) acc
                           (cons (ei-macro-string ns-name nm scm guard?) acc))))))
            (else
             (let ((scm (if guard?
                            (guard (e (#t #f)) (ei-compile-form (make-analyze-ctx ns-name) f optimize?))
                            (ei-compile-form (make-analyze-ctx ns-name) f optimize?))))
               (loop (cdr forms)
                     (if (and guard? (not scm)) acc
                         (cons (if guard? (string-append "(guard (e (#t #f))\n  " scm ")") scm) acc))))))))))

(define (ei-emit-ns ns-name src) (ei-emit-ns* ns-name src #f #t))

;; --- tree-shaking (jolt build --tree-shake) ---------------------------------
;; Reachability DCE over the re-emitted app + library forms: keep -main, every
;; side-effecting (non-def) top-level form, and every def reachable from those;
;; drop the rest (unused library code). Bails (keeps everything) if the app resolves
;; vars by name at runtime (eval/resolve/...), which static reachability can't
;; follow. clojure.core / the compiler stay baked (the prelude + image blobs), so
;; only the re-emitted namespaces are shaken.
(define dce-kw-op   (keyword #f "op"))
(define dce-kw-var  (keyword #f "var"))
(define dce-kw-def  (keyword #f "def"))
(define dce-kw-ns   (keyword #f "ns"))
(define dce-kw-name (keyword #f "name"))
(define dce-reduce-children (var-deref "jolt.ir" "reduce-ir-children"))

;; "ns/name" of every :var reference anywhere in node, prepended to acc. Arg order
;; (acc node) matches reduce-ir-children's fold fn, so it nests directly.
(define (dce-collect-refs acc node)
  (if (eq? (jolt-get node dce-kw-op) dce-kw-var)
      (cons (string-append (jolt-get node dce-kw-ns) "/" (jolt-get node dce-kw-name)) acc)
      (dce-reduce-children dce-collect-refs acc node)))

;; The fqn of a bare top-level def (the only prunable form), else #f.
(define (dce-def-fqn node)
  (and (eq? (jolt-get node dce-kw-op) dce-kw-def)
       (string-append (jolt-get node dce-kw-ns) "/" (jolt-get node dce-kw-name))))

;; A reference whose presence forces keep-everything (runtime name resolution).
(define dce-bail-refs
  '("clojure.core/eval" "clojure.core/resolve" "clojure.core/ns-resolve"
    "clojure.core/requiring-resolve" "clojure.core/find-var" "clojure.core/intern"
    "clojure.core/load-string" "clojure.core/load-file" "clojure.core/load-reader"
    "clojure.core/load"))

;; One record per form: (vector keep? fqn refs str). keep? #t = a non-def form,
;; always emitted, its refs are reachability roots; #f = a prunable def emitted only
;; if fqn is reached. A macro is a prunable def (its expander isn't called at runtime
;; in an AOT build). Strict (no guard) like the build's ei-emit-ns* path.
(define (ei-emit-ns-records ns-name src)
  (let loop ((forms (ei-read-all src)) (acc '()))
    (if (null? forms)
        (reverse acc)
        (let ((f (car forms)))
          (ce-scan-requires! f ns-name)
          (cond
            ((ei-ns-form? f) (loop (cdr forms) acc))
            ((ce-macro-form? f)
             (let-values (((nm fn-form) (ce-defmacro->fn f)))
               (let* ((ctx (make-analyze-ctx ns-name))
                      (ir (jolt-ce-run-passes (jolt-ce-analyze ctx fn-form) ctx))
                      (str (ei-macro-string ns-name nm (jolt-ce-emit-top ir) #f))
                      (refs (dce-collect-refs '() ir)))
                 (loop (cdr forms) (cons (vector #f (string-append ns-name "/" nm) refs str) acc)))))
            (else
             (let* ((ctx (make-analyze-ctx ns-name))
                    (ir (jolt-ce-run-passes (jolt-ce-analyze ctx f) ctx))
                    (str (jolt-ce-emit-top ir))
                    (fqn (dce-def-fqn ir))
                    (refs (dce-collect-refs '() ir)))
               (loop (cdr forms)
                     (cons (if fqn (vector #f fqn refs str) (vector #t #f refs str)) acc)))))))))

;; Scheme string literal for a ns/name — uses the runtime's own writer
;; (printable ASCII identifiers only here).
(define (ei-str-lit s) (with-output-to-string (lambda () (write s))))

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
