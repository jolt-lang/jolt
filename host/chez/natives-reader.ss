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

;; --- reader-conditional: a tagged map (reader-conditional? is an overlay
;; tagged-value predicate that reads :jolt/type). STAYS NATIVE: building a
;; :jolt/type-tagged map is part of the native value model — an overlay defn
;; returning {:jolt/type ...} silently fails to bind during the seed mint (the
;; guard around each prelude form swallows the load-time error), the same reason
;; every other tagged-value constructor (atom/volatile!/tagged-literal) is native.
;; re-matcher / re-find / re-groups are the stateful matcher API in regex.ss.
(define nr-kw-type (keyword "jolt" "type"))
(define nr-kw-rc   (keyword "jolt" "reader-conditional"))
(define nr-kw-form (keyword #f "form"))
(define nr-kw-spl  (keyword #f "splicing?"))
(define (nr-reader-conditional form splicing?)
  (jolt-hash-map nr-kw-type nr-kw-rc nr-kw-form form nr-kw-spl splicing?))

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
(def-var! "clojure.core" "macroexpand-1" nr-macroexpand-1)

;; letfn is a special form (the analyzer lowers it to letrec*, checked before any
;; macro), but on the JVM it is also a clojure.core macro that (resolve 'letfn)
;; finds — like let / loop / fn here. Intern a var so resolution matches; the value
;; is never invoked (the analyzer handles every (letfn …) form), and it is NOT
;; marked a macro, so macroexpand leaves a letfn form alone (it is special).
(def-var! "clojure.core" "letfn"
  (lambda args (jolt-throw (jolt-ex-info "letfn is a special form" (jolt-hash-map)))))
(def-var! "clojure.core" "macroexpand" nr-macroexpand)
