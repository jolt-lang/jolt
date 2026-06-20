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

;; Cross-compile one namespace's source to a list of guard-wrapped Scheme strings.
;; Mirrors driver.janet emit-ns-forms-list + emit-form-scheme (the compiler
;; namespaces define no macros, so there is no defmacro branch). Each form is
;; analyzed with a fresh ctx — resolution is via the runtime var-table + alias
;; tables, not ctx-accumulated state, so this matches the spine's per-form analyze.
(define (ei-emit-ns ns-name src)
  (let loop ((forms (ei-read-all src)) (acc '()))
    (if (null? forms)
        (reverse acc)
        (let ((f (car forms)))
          (ce-scan-requires! f ns-name)
          (if (ei-ns-form? f)
              (loop (cdr forms) acc)
              (let* ((ctx (make-analyze-ctx ns-name))
                     (scm (guard (e (#t #f)) (jolt-ce-emit (jolt-ce-analyze ctx f)))))
                (loop (cdr forms)
                      (if scm
                          (cons (string-append "(guard (e (#t #f))\n  " scm ")") acc)
                          acc))))))))

;; The compiler namespaces, in load order — same list as driver.janet
;; compiler-ns-files.
(define ei-compiler-ns-files
  (list (cons "jolt.ir" "jolt-core/jolt/ir.clj")
        (cons "jolt.analyzer" "jolt-core/jolt/analyzer.clj")
        (cons "jolt.backend-scheme" "jolt-core/jolt/backend_scheme.clj")))

;; Emit the whole compiler image as one Scheme string: every namespace's forms,
;; joined by newlines. Byte-identical layout to driver.janet emit-compiler-image
;; (forms joined with "\n", no trailing newline) so an image emitted here can be
;; diffed directly against the Janet-built one.
(define (jolt-emit-image)
  (let ((forms (apply append
                 (map (lambda (nf) (ei-emit-ns (car nf) (read-file-string (cdr nf))))
                      ei-compiler-ns-files))))
    (let join ((fs forms) (out ""))
      (cond
        ((null? fs) out)
        ((string=? out "") (join (cdr fs) (car fs)))
        (else (join (cdr fs) (string-append out "\n" (car fs))))))))
