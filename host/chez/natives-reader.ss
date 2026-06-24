;; natives-reader.ss — reader/macro runtime-support natives: the #?() reader feature
;; set, the reader-conditional + re-matcher tagged-map constructors, and macroexpand.
;;
;; Loaded late (after ns.ss): macroexpand forward-refs the runtime macro table
;; (host-contract hc-macro?/hc-expand-1) + the analyzer ctx, resolved at call time
;; after the spine loads. The hash / transient? / rseq / cat natives that used to
;; live here moved to natives-misc, transients, natives-seq, and natives-transduce.

;; --- reader feature set (for #?() conditionals) — mutable list of name strings,
;; default jolt + default. __reader-features returns the strings; -set! replaces.
(define nr-reader-features (list "jolt" "default"))
(define (nr-reader-features-get) (list->cseq nr-reader-features))
(define (nr-reader-features-set! names)
  (set! nr-reader-features
        (map (lambda (n) (cond ((keyword-t? n) (keyword-t-name n)) ((string? n) n) (else (jolt-pr-str n))))
             (seq->list (jolt-seq names))))
  jolt-nil)

;; --- reader-conditional / re-matcher: tagged maps (reader-conditional? + the
;; matcher consumers are overlay tagged-value predicates that read :jolt/type).
(define nr-kw-type (keyword "jolt" "type"))
(define nr-kw-rc   (keyword "jolt" "reader-conditional"))
(define nr-kw-form (keyword #f "form"))
(define nr-kw-spl  (keyword #f "splicing?"))
(define nr-kw-mat  (keyword "jolt" "matcher"))
(define nr-kw-re   (keyword #f "re"))
(define nr-kw-s    (keyword #f "s"))
(define nr-kw-pos  (keyword #f "pos"))
(define (nr-reader-conditional form splicing?)
  (jolt-hash-map nr-kw-type nr-kw-rc nr-kw-form form nr-kw-spl splicing?))
(define (nr-re-matcher re s)
  (jolt-hash-map nr-kw-type nr-kw-mat nr-kw-re re nr-kw-s s nr-kw-pos 0.0))

;; --- macroexpand-1 / macroexpand: expand a (quoted) call form via the runtime
;; macro table (host-contract hc-macro?/hc-expand-1; forward-referenced, resolved
;; at call time after the spine loads). macroexpand loops until the head is no
;; longer a macro (subforms are not expanded, matching Clojure).
(define (nr-macroexpand-1 form)
  (if (and (cseq? form) (cseq-list? form) (symbol-t? (seq-first form)))
      (let ((ctx (make-analyze-ctx (chez-current-ns))))
        (if (hc-macro? ctx (seq-first form)) (hc-expand-1 ctx form) form))
      form))
(define (nr-macroexpand form)
  (let loop ((cur form))
    (let ((nxt (nr-macroexpand-1 cur))) (if (eq? cur nxt) cur (loop nxt)))))

(def-var! "clojure.core" "__reader-features" nr-reader-features-get)
(def-var! "clojure.core" "__reader-features-set!" nr-reader-features-set!)
(def-var! "clojure.core" "reader-conditional" nr-reader-conditional)
(def-var! "clojure.core" "re-matcher" nr-re-matcher)
(def-var! "clojure.core" "macroexpand-1" nr-macroexpand-1)
(def-var! "clojure.core" "macroexpand" nr-macroexpand)
