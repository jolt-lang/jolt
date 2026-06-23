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
;; the self-host fixpoint is independent of the passes).
(define (ei-compile-form ctx f optimize?)
  (let ((ir (jolt-ce-analyze ctx f)))
    (jolt-ce-emit (if optimize? (jolt-ce-run-passes ir ctx) ir))))

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
