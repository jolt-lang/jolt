;; natives-parity.ss (jolt-cf1q.7) — native Chez shims for clojure.core fns that
;; live in the Janet seed (src/jolt/core*.janet) but had no Chez shim, so on the
;; zero-Janet spine they resolved to nil ("not a fn"). Pure-Chez, JVM-matching.
;;
;; Loaded after host-table.ss (htable-sorted?), transients.ss (jolt-transient?),
;; values.ss (jolt-hash), seq.ss (jolt-seq/seq->list/list->cseq/jolt-invoke).

;; --- hash family (mirrors core_extra.janet: 24-bit masked so int? holds) ------
(define (np-h24 x) (bitwise-and (jolt-hash x) #xffffff))
(define (np-hash x) (np-h24 x))
(define (np-hash-combine a b)
  (bitwise-and (bitwise-xor (np-h24 a) (+ (np-h24 b) #x9e3779)) #xffffff))
(define (np-hash-ordered-coll coll)
  (let loop ((xs (seq->list (jolt-seq coll))) (h 1))
    (if (null? xs) h (loop (cdr xs) (bitwise-and (+ (* 31 h) (np-h24 (car xs))) #xffffff)))))
(define (np-hash-unordered-coll coll)
  (let loop ((xs (seq->list (jolt-seq coll))) (h 0))
    (if (null? xs) h (loop (cdr xs) (bitwise-and (+ h (np-h24 (car xs))) #xffffff)))))

;; --- transient? ---------------------------------------------------------------
(define (np-transient? x) (jolt-transient? x))

;; --- rseq: vectors + sorted colls only (Clojure), reverse of the ascending seq.
(define (np-rseq coll)
  (if (or (pvec? coll) (htable-sorted? coll))
      (list->cseq (reverse (seq->list (jolt-seq coll))))
      (jolt-throw (jolt-ex-info "rseq requires a vector or sorted collection" (jolt-hash-map)))))

;; --- cat transducer (mirrors core_refs.janet core-cat): each item of the input
;; is itself a collection, concatenated into the downstream reducing fn.
(define (np-cat rf)
  (lambda a
    (cond
      ((null? a) (jolt-invoke rf))
      ((null? (cdr a)) (jolt-invoke rf (car a)))
      (else
       (let loop ((xs (seq->list (jolt-seq (cadr a)))) (acc (car a)))
         (if (null? xs) acc (loop (cdr xs) (jolt-invoke rf acc (car xs)))))))))

;; --- reader feature set (for #?() conditionals) — mutable list of name strings,
;; default jolt + default. __reader-features returns the strings; -set! replaces.
(define np-reader-features (list "jolt" "default"))
(define (np-reader-features-get) (list->cseq np-reader-features))
(define (np-reader-features-set! names)
  (set! np-reader-features
        (map (lambda (n) (cond ((keyword-t? n) (keyword-t-name n)) ((string? n) n) (else (jolt-pr-str n))))
             (seq->list (jolt-seq names))))
  jolt-nil)

;; --- reader-conditional / re-matcher: tagged maps (reader-conditional? + the
;; matcher consumers are overlay tagged-value predicates that read :jolt/type).
(define np-kw-type (keyword "jolt" "type"))
(define np-kw-rc   (keyword "jolt" "reader-conditional"))
(define np-kw-form (keyword #f "form"))
(define np-kw-spl  (keyword #f "splicing?"))
(define np-kw-mat  (keyword "jolt" "matcher"))
(define np-kw-re   (keyword #f "re"))
(define np-kw-s    (keyword #f "s"))
(define np-kw-pos  (keyword #f "pos"))
(define (np-reader-conditional form splicing?)
  (jolt-hash-map np-kw-type np-kw-rc np-kw-form form np-kw-spl splicing?))
(define (np-re-matcher re s)
  (jolt-hash-map np-kw-type np-kw-mat np-kw-re re np-kw-s s np-kw-pos 0.0))

;; (delay? / make-delay / force live in concurrency.ss with the real delay type.)

;; --- macroexpand-1 / macroexpand: expand a (quoted) call form via the runtime
;; macro table (host-contract hc-macro?/hc-expand-1; forward-referenced, resolved
;; at call time after the spine loads). macroexpand loops until the head is no
;; longer a macro (subforms are not expanded, matching Clojure).
(define (np-macroexpand-1 form)
  (if (and (cseq? form) (cseq-list? form) (symbol-t? (seq-first form)))
      (let ((ctx (make-analyze-ctx (chez-current-ns))))
        (if (hc-macro? ctx (seq-first form)) (hc-expand-1 ctx form) form))
      form))
(define (np-macroexpand form)
  (let loop ((cur form))
    (let ((nxt (np-macroexpand-1 cur))) (if (eq? cur nxt) cur (loop nxt)))))

(def-var! "clojure.core" "hash" np-hash)
(def-var! "clojure.core" "hash-combine" np-hash-combine)
(def-var! "clojure.core" "hash-ordered-coll" np-hash-ordered-coll)
(def-var! "clojure.core" "hash-unordered-coll" np-hash-unordered-coll)
(def-var! "clojure.core" "transient?" np-transient?)
(def-var! "clojure.core" "rseq" np-rseq)
(def-var! "clojure.core" "cat" np-cat)
(def-var! "clojure.core" "__reader-features" np-reader-features-get)
(def-var! "clojure.core" "__reader-features-set!" np-reader-features-set!)
(def-var! "clojure.core" "reader-conditional" np-reader-conditional)
(def-var! "clojure.core" "re-matcher" np-re-matcher)
(def-var! "clojure.core" "macroexpand-1" np-macroexpand-1)
(def-var! "clojure.core" "macroexpand" np-macroexpand)
