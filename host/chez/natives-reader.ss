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
(define (nr-feature-name n)
  (cond ((keyword-t? n) (keyword-t-name n)) ((string? n) n) (else (jolt-pr-str n))))
(define (nr-reader-features-set! names)
  (set! rdr-features (map nr-feature-name (seq->list (jolt-seq names))))
  jolt-nil)
;; ...and the additive half, which is what a project's deps.edn reaches:
;;
;;   :jolt/features [:bb]
;;
;; Widening only — a project can teach jolt to read a key its host set does not
;; carry, but cannot take :jolt/:clj/:default away, so it can never make jolt
;; stop reading the branches its own stdlib is written against. The use this
;; exists for is a script ported from babashka whose :bb branches are the ones
;; the author wants; jolt does not match :bb on its own (see reader.ss).
(define (nr-reader-features-add! names)
  (for-each (lambda (n)
              (let ((f (nr-feature-name n)))
                (unless (member f rdr-features)
                  (set! rdr-features (append rdr-features (list f))))))
            (seq->list (jolt-seq names)))
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
;; Any SEQ whose head is a macro symbol expands, which is Clojure's rule
;; (macroexpand1 tests ISeq). Requiring a cseq-list? left a seq that came out of
;; another expansion unexpanded — the head symbol and the value were identical,
;; only the provenance differed, so (macroexpand-all (macroexpand-1 form)) stopped
;; one level early on a macro that builds its own expansion.
;;
;; The ENV argument is what separates these two from the pair Clojure exposes.
;; hc-expand-1 binds &env around the expander call, and the analyzer passes the
;; in-scope locals (analyzer.clj amp-env-map: local symbol -> nil). The public
;; macroexpand-1 has no env to pass and so binds {} — fine for a caller that is
;; only INSPECTING an expansion, and wrong for one that then emits it, because a
;; macro reading &env expands differently under {} and the emitted program is not
;; the one the analyzer would have compiled.
;;
;; clojure.core.async's go pass is that caller: it macroexpands to find the park
;; sites and rebuilds the body out of the expansion. __macroexpand-env is the
;; seam it uses. Deliberately NOT a second arity on macroexpand-1 — the JVM's is
;; one-argument and code that feature-tests the arity should keep getting that
;; answer — and __-prefixed like the other internal entry points here.
(define (nr-macroexpand-1* form env)
  (if (and (hc-list? form) (not (jolt-nil? (jolt-seq form))) (symbol-t? (seq-first (jolt-seq form))))
      (let ((ctx (make-analyze-ctx (chez-current-ns))))
        (if (hc-macro? ctx (seq-first (jolt-seq form))) (hc-expand-1 ctx form env) form))
      form))
(define (nr-macroexpand* form env)
  (let loop ((cur form))
    (let ((nxt (nr-macroexpand-1* cur env))) (if (eq? cur nxt) cur (loop nxt)))))

;; One empty map, not one per call: this is on the public macroexpand path and
;; the value is immutable.
(define nr-no-env (jolt-hash-map))
(define (nr-macroexpand-1 form) (nr-macroexpand-1* form nr-no-env))
(define (nr-macroexpand form) (nr-macroexpand* form nr-no-env))
;; nil env is the empty one, so a caller with nothing to say need not build a map.
(define (nr-macroexpand-env form env)
  (nr-macroexpand* form (if (jolt-nil? env) nr-no-env env)))

(def-var! "clojure.core" "__reader-features" nr-reader-features-get)
(def-var! "clojure.core" "__reader-features-set!" nr-reader-features-set!)
;; The Clojure-facing seam for :jolt/features (see nr-reader-features-add!).
;; jolt.deps collects the key and jolt.main calls this once after it resolves the
;; project and before any of the project compiles — the same ordering
;; :jolt/provides needs, because the first form READ is what consults it.
(def-var! "jolt.host" "add-reader-features!" nr-reader-features-add!)
(def-var! "clojure.core" "reader-conditional" nr-reader-conditional)
;; read and read+string's opts arity: their :features and :read-cond in force for
;; the read (reader.ss rdr-call-with-read-opts)
(def-var! "jolt.host" "call-with-read-opts"
  (lambda (opts f) (rdr-call-with-read-opts opts (lambda () (jolt-invoke f)))))
(def-var! "clojure.core" "macroexpand-1" nr-macroexpand-1)
(def-var! "clojure.core" "__macroexpand-env" nr-macroexpand-env)

;; letfn is a special form (the analyzer lowers it to letrec*, checked before any
;; macro), but on the JVM it is also a clojure.core macro that (resolve 'letfn)
;; finds — like let / loop / fn here. Intern a var so resolution matches; the value
;; is never invoked (the analyzer handles every (letfn …) form), and it is NOT
;; marked a macro, so macroexpand leaves a letfn form alone (it is special).
(def-var! "clojure.core" "letfn"
  (lambda args (jolt-throw (jolt-ex-info "letfn is a special form" (jolt-hash-map)))))
(def-var! "clojure.core" "macroexpand" nr-macroexpand)
