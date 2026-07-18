;; multimethods — the multimethod dispatch runtime on the Chez host.
;;
;; defmulti/defmethod are macros that expand to ctx-capturing setup CALLS
;; (defmulti-setup / defmethod-setup, + the table ops get-method/methods/
;; remove-method/prefer-method/prefers), implemented here against
;; the runtime's ns/var machinery.
;;
;; A multimethod VALUE is a jolt-multifn record carrying its dispatch fn and a
;; mutable method table (dispatch-val -> method fn, keyed with jolt= so keyword/
;; vector/number dispatch values match by value). jolt-invoke dispatches it:
;; an exact method, else an isa?/hierarchy match (resolved through prefer-method
;; and the overlay's isa?/derive/hierarchy), else the :default method.
;;
;; NS resolution: defmulti expands to (defmulti-setup (quote name) ...) with a
;; BARE symbol — the Chez RT has no compile-time current ns at the call site, so a
;; runtime `chez-current-ns` box names where to def-var! the multifn. It defaults
;; to "user" (matching the analyzer's ns for -e user code); the assembled prelude
;; sets it to "clojure.core" around its own load (program-with-prelude), so the
;; print-method/print-dup defmultis land in clojure.core. defmethod-setup and the
;; symbol-taking table ops resolve the multifn via (var-deref (chez-current-ns) …),
;; so they agree with defmulti. Loaded from rt.ss after seq.ss (jolt-invoke),
;; collections.ss (jolt=/key-hash/jolt-hash-map) and the var-cell machinery.

;; THREAD-LOCAL: a Chez thread-parameter, so each OS thread (an nREPL
;; session worker / future) has its own current ns — vars stay global, only the
;; "current ns" pointer is per-thread, matching Clojure's thread-local *ns*. A new
;; thread inherits the forking thread's value. `star-ns-cell` (the *ns* var cell,
;; captured by dyn-binding.ss once *ns* exists) lets chez-current-ns DERIVE from a
;; thread-local (binding [*ns* ..]) so a bound *ns* drives load-string/analyzer
;; resolution; bootstrap-safe (it's #f until then, so we just read the parameter).
(define chez-current-ns-param (make-thread-parameter "user"))
(define star-ns-cell #f)
(define (chez-current-ns)
  (if star-ns-cell
      (let ((bv (dyn-binding-value star-ns-cell)))
        (if (and (not (eq? bv dyn-no-binding)) (jns? bv))
            (jns-name bv)
            (chez-current-ns-param)))
      (chez-current-ns-param)))
(define (set-chez-ns! ns) (chez-current-ns-param ns))

(define-record-type jolt-multifn
  (fields name dispatch-fn methods default hierarchy prefers
          cache (mutable cache-epoch) (mutable cache-hier))
  (nongenerative jolt-multifn-v2))

(define kw-default (keyword #f "default"))
(define (new-mm-table) (make-hashtable key-hash jolt=))

;; (defmulti-setup 'name dispatch & opts) — opts is a flat :default/:hierarchy plist.
(define (parse-mm-opts opts)
  (let loop ((o opts) (dk kw-default) (h #f))
    (if (or (null? o) (null? (cdr o)))
        (values dk h)
        (let ((k (car o)) (v (cadr o)))
          (cond
            ((and (keyword? k) (not (keyword-t-ns k)) (string=? (keyword-t-name k) "default"))
             (loop (cddr o) v h))
            ((and (keyword? k) (not (keyword-t-ns k)) (string=? (keyword-t-name k) "hierarchy"))
             (loop (cddr o) dk v))
            (else (loop (cddr o) dk h)))))))

(define (jolt-defmulti-setup name-sym dispatch . opts)
  (let-values (((dk h) (parse-mm-opts opts)))
    (let* ((sns (symbol-t-ns name-sym))
           ;; the macro qualifies the name with its EXPANSION ns, so a defmulti
           ;; deferred inside a fn (a deftest body) still defines in the ns it
           ;; was written in, not whatever ns is current when it finally runs.
           (ns (if (string? sns) sns (chez-current-ns)))
           (mf (make-jolt-multifn (symbol-t-name name-sym) dispatch
                                  (new-mm-table) dk h (new-mm-table) (new-mm-table) -1 #f)))
      (def-var! ns (symbol-t-name name-sym) mf)
      mf)))

;; (defmethod-setup 'mm dispatch-val impl) — add a method. Auto-creates the multifn
;; if absent (defmethod before defmulti — rare; identity dispatch as a fallback).
(define (jolt-defmethod-setup mm-sym dval impl . rest)
  (let* ((nm (symbol-t-name mm-sym))
         (sns (symbol-t-ns mm-sym))
         (qns (and sns (not (jolt-nil? sns)) (not (null? sns)) sns))
         ;; the macro passes its EXPANSION ns so a defmethod deferred inside a
         ;; fn resolves like the JVM (against the ns it was written in, not the
         ;; ns current when it runs); absent (old emitted code) fall back to the
         ;; runtime ns.
         (here (if (and (pair? rest) (string? (car rest))) (car rest) (chez-current-ns)))
         ;; qualified (cf.mm/ext) resolves in its own ns (cross-ns defmethod);
         ;; unqualified resolves in the writing ns, else a :refer's home ns (so a
         ;; defmethod on a referred multifn lands on the real one), else stays in
         ;; the writing ns (a shadow, as before).
         (mns (cond
                (qns (or (chez-resolve-alias here qns) qns))
                ((var-cell-lookup here nm) here)
                ((chez-resolve-refer here nm) => values)
                (else here)))
         (cur (var-deref mns nm))
         (mf (if (jolt-multifn? cur) cur
                 ;; auto-create: copy the dispatch fn + default from a same-named
                 ;; clojure.core multifn (e.g. print-method's 2-arg dispatch) so a
                 ;; (defmethod print-method ...) before naming clojure.core's still
                 ;; dispatches right — the old 1-arg identity fallback crashed.
                 (let* ((core (var-deref "clojure.core" nm))
                        (disp (if (jolt-multifn? core)
                                  (jolt-multifn-dispatch-fn core)
                                  (var-deref "clojure.core" "identity")))
                        (deft (if (jolt-multifn? core) (jolt-multifn-default core) kw-default))
                        (m (make-jolt-multifn nm disp (new-mm-table) deft #f (new-mm-table) (new-mm-table) -1 #f)))
                   (def-var! mns nm m) m))))
    (hashtable-set! (jolt-multifn-methods mf) dval impl)
    (set! jolt-mm-epoch (fx+ jolt-mm-epoch 1))
    mf))

;; --- dispatch ----------------------------------------------------------------
(define (mm-isa? mf)
  ;; the overlay's isa? (the hierarchy system is pure Clojure); a per-mm :hierarchy
  ;; is an atom (deref each dispatch, like a Clojure var) or a plain map.
  (let* ((isa (var-deref "clojure.core" "isa?"))
         (h (jolt-multifn-hierarchy mf))
         (hval (and h (if (jolt-atom? h) (jolt-atom-val h) h))))
    (lambda (x y) (jolt-truthy? (if hval (jolt-invoke isa hval x y) (jolt-invoke isa x y))))))

;; the parent dispatch values of x in mf's hierarchy, as a Scheme list.
(define (mm-parents mf)
  (let* ((par (var-deref "clojure.core" "parents"))
         (h (jolt-multifn-hierarchy mf))
         (hval (and h (if (jolt-atom? h) (jolt-atom-val h) h))))
    (lambda (x)
      (let ((r (if hval (jolt-invoke par hval x) (jolt-invoke par x))))
        (if (or (jolt-nil? r) (jolt-nil? (jolt-seq r))) '()
            (let loop ((s (jolt-seq r)) (acc '()))
              (if (jolt-nil? s) (reverse acc)
                  (loop (jolt-seq (seq-more s)) (cons (seq-first s) acc)))))))))

;; Is x preferred over y? Like Clojure MultiFn.prefers: a direct preference, OR x
;; preferred over a PARENT of y, OR a PARENT of x preferred over y — walking the
;; hierarchy transitively (not just the direct prefers table).
(define (mm-prefers? mf x y)
  (let ((prefers (jolt-multifn-prefers mf))
        (parents-of (mm-parents mf)))
    (let pref? ((x x) (y y))
      (or (let ((px (hashtable-ref prefers x #f))) (and px (hashtable-ref px y #f) #t))
          (let scan ((ps (parents-of y))) (and (pair? ps) (or (pref? x (car ps)) (scan (cdr ps)))))
          (let scan ((ps (parents-of x))) (and (pair? ps) (or (pref? (car ps) y) (scan (cdr ps)))))))))

(define (mm-find-isa mf dv)
  (let* ((methods (jolt-multifn-methods mf))
         (isa? (mm-isa? mf))
         (default (jolt-multifn-default mf))
         (keys (filter (lambda (k) (not (jolt= k default)))
                       (vector->list (hashtable-keys methods))))
         (matches (filter (lambda (k) (isa? dv k)) keys)))
    (cond
      ((null? matches) #f)
      ((null? (cdr matches)) (hashtable-ref methods (car matches) #f))
      (else
       ;; >1 isa-match: pick the dominant key (x dominates y when x is
       ;; prefer-method'd over y, or (isa? x y)); ambiguity with no dominant is an
       ;; error, as in Clojure.
       (let* ((dom? (lambda (x y) (or (mm-prefers? mf x y) (isa? x y))))
              (best (fold-left (lambda (b k) (if (dom? k b) k b)) (car matches) (cdr matches))))
         (for-each
          (lambda (k)
            (when (and (not (jolt= k best)) (not (dom? best k)))
              (throw-jvm (quote IllegalArgumentException)
                         (string-append "Multiple methods in multimethod '" (jolt-multifn-name mf)
                                        "' match dispatch value: " (jolt-pr-str dv) " -> "
                                        (jolt-pr-str best) " and " (jolt-pr-str k)
                                        ", and neither is preferred"))))
          matches)
         (hashtable-ref methods best #f))))))

;; --- dispatch cache ----------------------------------------------------------
;; Clojure's MultiFn memoizes dispatch-value -> resolved method until the method
;; table, the prefers, or the hierarchy it dispatches against changes. Each
;; multifn holds its own cache (dv -> method fn); a global epoch (jolt-mm-epoch,
;; mirroring jolt-proto-epoch in records.ss) is bumped on every defmethod /
;; remove-method / remove-all-methods / prefer-method. derive/underive live in
;; the baked prelude and swap! a fresh hierarchy map, so rather than patch them we
;; invalidate when the hierarchy VALUE the multifn resolves against is no longer
;; eq? to the one the cache was stamped with - correct without touching the
;; prelude. A global epoch may over-invalidate across multifns (a miss, never a
;; wrong answer); correctness first, per jolt-mw44.30.
(define jolt-mm-epoch 0)

;; the hierarchy object isa? resolves against for mf: its own :hierarchy atom/map
;; when set, else the global-hierarchy atom's current value. deref'd lazily (the
;; prelude loads after this file), exactly as mm-isa? does.
(define (mm-current-hierarchy mf)
  (let ((h (jolt-multifn-hierarchy mf)))
    (cond
      ((not h)
       (let ((gh (var-deref "clojure.core" "global-hierarchy")))
         (if (jolt-atom? gh) (jolt-atom-val gh) gh)))
      ((jolt-atom? h) (jolt-atom-val h))
      (else h))))

;; drop the whole cache if the epoch advanced or the hierarchy value changed.
(define (mm-cache-validate! mf)
  (let ((hier (mm-current-hierarchy mf)))
    (unless (and (fx= (jolt-multifn-cache-epoch mf) jolt-mm-epoch)
                 (eq? (jolt-multifn-cache-hier mf) hier))
      (hashtable-clear! (jolt-multifn-cache mf))
      (jolt-multifn-cache-epoch-set! mf jolt-mm-epoch)
      (jolt-multifn-cache-hier-set! mf hier))))

;; resolve dv to its method fn. An exact table hit is always current (defmethod /
;; remove mutate that table and bump the epoch), so it bypasses the cache. On a
;; miss the cache is validated then consulted; a miss there runs the isa? scan
;; (mm-find-isa) and falls back to the :default method, memoizing the result. #f
;; only when nothing matches - the caller raises.
(define (mm-resolve mf dv)
  (let* ((methods (jolt-multifn-methods mf))
         (direct (hashtable-ref methods dv #f)))
    (or direct
        (begin
          (mm-cache-validate! mf)
          (let ((cache (jolt-multifn-cache mf)))
            (or (hashtable-ref cache dv #f)
                (let ((m (or (mm-find-isa mf dv)
                             (hashtable-ref methods (jolt-multifn-default mf) #f))))
                  (when m (hashtable-set! cache dv m))
                  m)))))))

(define (mm-no-method mf dv)
  (throw-jvm (quote IllegalArgumentException)
             (string-append "No method in multimethod '" (jolt-multifn-name mf)
                            "' for dispatch value: " (jolt-pr-str dv))))

;; fixed-arity entry points: the common arities call the dispatch fn and the
;; resolved method through jolt-invoke1/2/3, skipping the rest-list + the two
;; applys the fully-variadic multifn-dispatch pays (one apply for the dispatch
;; fn, one for the method).
(define (multifn-dispatch1 mf a)
  (let* ((dv (jolt-invoke1 (jolt-multifn-dispatch-fn mf) a))
         (m (mm-resolve mf dv)))
    (if m (jolt-invoke1 m a) (mm-no-method mf dv))))

(define (multifn-dispatch2 mf a b)
  (let* ((dv (jolt-invoke2 (jolt-multifn-dispatch-fn mf) a b))
         (m (mm-resolve mf dv)))
    (if m (jolt-invoke2 m a b) (mm-no-method mf dv))))

(define (multifn-dispatch3 mf a b c)
  (let* ((dv (jolt-invoke3 (jolt-multifn-dispatch-fn mf) a b c))
         (m (mm-resolve mf dv)))
    (if m (jolt-invoke3 m a b c) (mm-no-method mf dv))))

;; the generic path (arity > 3) keeps working and also uses the cache.
(define (multifn-dispatch mf . args)
  (let* ((dv (apply jolt-invoke (jolt-multifn-dispatch-fn mf) args))
         (m (mm-resolve mf dv)))
    (if m (apply jolt-invoke m args) (mm-no-method mf dv))))

;; jolt-invoke dispatches a multifn via a prefix arm (seq.ss). The arm gets the
;; args as a list; arity-dispatch the common cases to dispatch1/2/3 so they skip
;; the rest-list + applys; arity > 3 falls through to the generic path.
(register-invoke-prefix-arm! jolt-multifn?
  (lambda (f args)
    (cond ((null? (cdr args)) (multifn-dispatch1 f (car args)))
          ((null? (cddr args)) (multifn-dispatch2 f (car args) (cadr args)))
          ((null? (cdddr args)) (multifn-dispatch3 f (car args) (cadr args) (caddr args)))
          (else (apply multifn-dispatch f args)))))

;; --- table ops ---------------------------------------------------------------
;; All table ops take the multifn VALUE (Clojure semantics): the multifn record
;; carries its method/prefer tables, so the overlay fns pass the value directly.

(define (jolt-prefer-method-setup mf dval-a dval-b)
  (when (jolt-multifn? mf)
    ;; a preference b-over-a (direct or transitive) makes a-over-b a conflict, as
    ;; in Clojure MultiFn.preferMethod.
    (when (mm-prefers? mf dval-b dval-a)
      (throw-jvm (quote IllegalStateException)
                 (string-append "Preference conflict in multimethod '" (jolt-multifn-name mf)
                                "': " (jolt-pr-str dval-b) " is already preferred to " (jolt-pr-str dval-a))))
    (let ((sub (or (hashtable-ref (jolt-multifn-prefers mf) dval-a #f)
                   (let ((h (new-mm-table)))
                     (hashtable-set! (jolt-multifn-prefers mf) dval-a h) h))))
      (hashtable-set! sub dval-b #t))
    (set! jolt-mm-epoch (fx+ jolt-mm-epoch 1)))
  mf)

(define (jolt-remove-method-setup mf dval)
  (when (jolt-multifn? mf)
    (hashtable-delete! (jolt-multifn-methods mf) dval)
    (set! jolt-mm-epoch (fx+ jolt-mm-epoch 1)))
  mf)

(define (jolt-remove-all-methods-setup mf)
  (when (jolt-multifn? mf)
    (hashtable-clear! (jolt-multifn-methods mf))
    (set! jolt-mm-epoch (fx+ jolt-mm-epoch 1)))
  mf)

(define (jolt-get-method-setup mf dval)
  (if (jolt-multifn? mf)
      (or (hashtable-ref (jolt-multifn-methods mf) dval #f)
          (hashtable-ref (jolt-multifn-methods mf) (jolt-multifn-default mf) #f)
          jolt-nil)
      jolt-nil))

(define (jolt-methods-setup mf)
  (if (jolt-multifn? mf)
      (let-values (((ks vs) (hashtable-entries (jolt-multifn-methods mf))))
        (let loop ((i 0) (m (jolt-hash-map)))
          (if (fx>=? i (vector-length ks)) m
              (loop (fx+ i 1) (jolt-assoc m (vector-ref ks i) (vector-ref vs i))))))
      jolt-nil))

(define (jolt-prefers-setup mf)
  (if (not (jolt-multifn? mf)) (jolt-hash-map)
      (let-values (((ks vs) (hashtable-entries (jolt-multifn-prefers mf))))
        (let loop ((i 0) (m (jolt-hash-map)))
          (if (fx>=? i (vector-length ks)) m
              ;; each value is an inner set of preferred-over keys -> a jolt set
              (loop (fx+ i 1)
                    (jolt-assoc m (vector-ref ks i)
                                (apply jolt-hash-set
                                       (vector->list (hashtable-keys (vector-ref vs i)))))))))))

;; Print a multifn like the JVM's #object[clojure.lang.MultiFn 0x… "name"] rather
;; than dumping the record's fields (methods table, hierarchy, cache, …).
(define (multifn-pr mf)
  (string-append "#object[clojure.lang.MultiFn 0x0 \"" (jolt-multifn-name mf) "\"]"))
(register-pr-arm! jolt-multifn? multifn-pr)
(register-str-render! jolt-multifn? multifn-pr)

(def-var! "clojure.core" "defmulti-setup" jolt-defmulti-setup)
(def-var! "clojure.core" "defmethod-setup" jolt-defmethod-setup)
(def-var! "clojure.core" "prefer-method-setup" jolt-prefer-method-setup)
(def-var! "clojure.core" "remove-method-setup" jolt-remove-method-setup)
(def-var! "clojure.core" "remove-all-methods-setup" jolt-remove-all-methods-setup)
(def-var! "clojure.core" "get-method-setup" jolt-get-method-setup)
(def-var! "clojure.core" "methods-setup" jolt-methods-setup)
(def-var! "clojure.core" "prefers-setup" jolt-prefers-setup)
