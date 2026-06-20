;; emit-image.ss (jolt-cf1q.4 inc8) — the on-Chez compiler-image emitter.
;;
;; This is the stage2/stage3 half of the self-hosting fixpoint. driver.janet's
;; emit-compiler-image cross-compiles the compiler sources (jolt.ir +
;; jolt.analyzer + jolt.backend-scheme) to a Scheme def-var! image USING THE JANET
;; analyzer/emitter — that is stage1. This file does the SAME job but the
;; analyze->emit runs ON CHEZ (jolt-ce-analyze / jolt-ce-emit, loaded from a
;; previously-built image): feed it stage1's image and it produces stage2; feed it
;; stage2 and it produces stage3. stage2 == stage3 byte-for-byte proves the
;; on-Chez compiler reproduces itself (self-hosting-bootstrap-research §4).
;;
;; Loaded after compile-eval.ss (needs jolt-ce-analyze/jolt-ce-emit/ce-scan-requires!,
;; make-analyze-ctx) and rt.ss (read-file-string, the reader's rdr-read-form).

;; Read every top-level form from a source string (a Chez read-all). Mirrors the
;; Janet driver's parse-all; uses the same reader the spine reads single forms with.
(define (ei-read-all src)
  (let ((end (string-length src)))
    (let loop ((i 0) (acc '()))
      (let-values (((form j) (rdr-read-form src i end)))
        (if (rdr-eof? form)
            (reverse acc)
            (loop j (cons form acc)))))))

;; Is `f` an (ns ...) form? (Its only role in the image is alias registration; we
;; never emit it — the def-var!s carry explicit ns names. Matches driver.janet.)
(define (ei-ns-form? f)
  (and (cseq? f) (cseq-list? f)
       (let ((items (seq->list f)))
         (and (pair? items) (symbol-t? (car items))
              (string=? (symbol-t-name (car items)) "ns")))))

;; Is `f` a (defmacro ...) / (definline ...) form?
(define (ei-macro-form? f)
  (and (cseq? f) (cseq-list? f)
       (let ((items (seq->list f)))
         (and (pair? items) (symbol-t? (car items))
              (let ((h (symbol-t-name (car items))))
                (or (string=? h "defmacro") (string=? h "definline")))))))

;; (defmacro NAME [docstring] [attr-map] params body...) -> (values "NAME" (fn ...)).
;; Mirrors driver.janet defmacro->fn: strip a leading docstring (native string) and
;; an attr-map (a pmap that isn't a symbol), then re-head the rest with `fn` so a
;; destructured macro arglist desugars before lowering. We emit the BARE fn (the
;; caller wraps it in def-var! + mark-macro!), never a (def NAME ...) — interning
;; NAME would make require skip the real macro (jolt-r9lm).
(define (ei-defmacro->fn f)
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

;; Cross-compile one namespace's source to a list of guard-wrapped Scheme strings.
;; Mirrors driver.janet emit-ns-forms-list/emit-core-prelude + emit-form-scheme.
;; Each form is analyzed with a fresh ctx — resolution is via the runtime var-table
;; + alias tables, not ctx-accumulated state, so this matches the spine's per-form
;; analyze. A defmacro emits its expander fn as (def-var! ns name <fn>) +
;; (mark-macro! ns name) so the on-Chez analyzer can expand it (jolt-r9lm).
(define (ei-emit-ns ns-name src)
  (let loop ((forms (ei-read-all src)) (acc '()))
    (if (null? forms)
        (reverse acc)
        (let ((f (car forms)))
          (ce-scan-requires! f ns-name)
          (cond
            ((ei-ns-form? f) (loop (cdr forms) acc))
            ((ei-macro-form? f)
             (let-values (((nm fn-form) (ei-defmacro->fn f)))
               (let ((scm (guard (e (#t #f))
                            (let ((ctx (make-analyze-ctx ns-name)))
                              (jolt-ce-emit (jolt-ce-analyze ctx fn-form))))))
                 (loop (cdr forms)
                       (if scm
                           (cons (string-append
                                   "(guard (e (#t #f))\n  (def-var! "
                                   (ei-str-lit ns-name) " " (ei-str-lit nm) "\n    "
                                   scm ")\n  (mark-macro! "
                                   (ei-str-lit ns-name) " " (ei-str-lit nm) "))")
                                 acc)
                           acc)))))
            (else
             (let* ((ctx (make-analyze-ctx ns-name))
                    (scm (guard (e (#t #f)) (jolt-ce-emit (jolt-ce-analyze ctx f)))))
               (loop (cdr forms)
                     (if scm
                         (cons (string-append "(guard (e (#t #f))\n  " scm ")") acc)
                         acc)))))))))

;; Scheme string literal for a ns/name — uses the runtime's own writer so it
;; matches the Janet driver's %j (printable ASCII identifiers only here).
(define (ei-str-lit s) (with-output-to-string (lambda () (write s))))

;; The compiler namespaces, in load order — same list as driver.janet
;; compiler-ns-files.
(define ei-compiler-ns-files
  (list (cons "jolt.ir" "jolt-core/jolt/ir.clj")
        (cons "jolt.analyzer" "jolt-core/jolt/analyzer.clj")
        (cons "jolt.backend-scheme" "jolt-core/jolt/backend_scheme.clj")))

;; The clojure.core tiers + stdlib namespaces, in load order — same lists as
;; driver.janet core-tier-files / stdlib-ns-files. Re-emitting these on Chez is the
;; prelude half of the fixpoint (the whole emitted system reproducing itself).
(define ei-prelude-ns-files
  (append
    (map (lambda (tf) (cons "clojure.core" (string-append "jolt-core/clojure/core/" tf ".clj")))
         '("00-syntax" "00-kernel" "10-seq" "20-coll" "25-sorted" "30-macros" "40-lazy" "50-io"))
    (list (cons "clojure.string" "src/jolt/clojure/string.clj")
          (cons "clojure.walk" "src/jolt/clojure/walk.clj")
          (cons "clojure.template" "src/jolt/clojure/template.clj")
          (cons "clojure.edn" "src/jolt/clojure/edn.clj")
          (cons "clojure.set" "src/jolt/clojure/set.clj")
          (cons "clojure.pprint" "src/jolt/clojure/pprint.clj"))))

;; Join a list of form strings with "\n", no trailing newline — byte-identical
;; layout to the Janet driver's (string/join out "\n").
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
