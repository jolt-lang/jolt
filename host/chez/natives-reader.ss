;; natives-reader.ss — reader/macro runtime-support natives: the #?() reader feature
;; set, the reader-conditional + re-matcher tagged-map constructors, and macroexpand.
;;
;; Loaded late (after ns.ss): macroexpand forward-refs the runtime macro table
;; (host-contract hc-macro?/hc-expand-1) + the analyzer ctx, resolved at call time
;; after the spine loads. The hash / transient? / rseq / cat natives that used to
;; live here moved to natives-misc, transients, natives-seq, and natives-transduce.

;; --- reader feature set (for #?() conditionals) — delegates to rdr-features
;; from reader.ss so that __reader-features and __reader-features-set! directly
;; affect #? reads.
(define (nr-reader-features-get) (list->cseq rdr-features))
(define (nr-reader-features-set! names)
  (set! rdr-features
        (map (lambda (n) (cond ((keyword-t? n) (keyword-t-name n)) ((string? n) n) (else (jolt-pr-str n))))
             (seq->list (jolt-seq names))))
  jolt-nil)

;; --- reader-conditional record type -----------------------------------------
;; A reader-conditional is a distinct record type — NOT a pmap — so pmap?/
;; coll?/map?/seqable?/ifn?/associative? are naturally false. Value equality:
;; (= rc1 rc2) true when form and splicing? match (JVM parity). ILookup for
;; :form and :splicing? only (NOT a general map — (:other rc) is nil).
(define-record-type jolt-reader-conditional-record
  (fields form splicing?)
  (nongenerative jolt-reader-conditional-record-v1))

;; re-matcher / re-find / re-groups are the stateful matcher API in regex.ss.
(define (nr-reader-conditional form splicing?)
  (make-jolt-reader-conditional-record form splicing?))

;; Register ILookup arm for :form and :splicing? — ReaderConditional IS ILookup
;; on the JVM for these two keys only (not a general map).
(let ((kw-form (keyword #f "form")) (kw-spl (keyword #f "splicing?")))
  (register-get-arm! jolt-reader-conditional-record?
    (lambda (coll k d)
      (cond ((jolt= k kw-form) (jolt-reader-conditional-record-form coll))
            ((jolt= k kw-spl) (if (jolt-reader-conditional-record-splicing? coll) #t #f))
            (else d)))))

;; Register value-equality arm: two reader-conditionals are = when their form
;; and splicing? fields match.
(register-eq-arm!
  (lambda (a b) (or (jolt-reader-conditional-record? a) (jolt-reader-conditional-record? b)))
  (lambda (a b)
    (and (jolt-reader-conditional-record? a) (jolt-reader-conditional-record? b)
         (jolt= (jolt-reader-conditional-record-form a)
                (jolt-reader-conditional-record-form b))
         (eq? (jolt-reader-conditional-record-splicing? a)
              (jolt-reader-conditional-record-splicing? b)))))

;; Register hash arm — matches JVM hasheq which hashes form + splicing?.
(register-hash-arm! jolt-reader-conditional-record?
  (lambda (x)
    (hash-combine (jolt-hash (jolt-reader-conditional-record-form x))
                  (if (jolt-reader-conditional-record-splicing? x) 1231 1237))))

;; pr form: #?(form ...) or #?@(form ...). Matches JVM output exactly — the form
;; is a list whose elements are rendered inline (not as a nested list).
(register-pr-arm! jolt-reader-conditional-record?
  (lambda (x)
    (let* ((form (jolt-reader-conditional-record-form x))
           (prefix (if (jolt-reader-conditional-record-splicing? x) "#?@(" "#?("))
           (s (jolt-pr-str form)))
      (string-append prefix
                     (if (and (> (string-length s) 1)
                              (char=? (string-ref s 0) #\())
                         (substring s 1 (- (string-length s) 1))
                         s)
                     ")"))))

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
