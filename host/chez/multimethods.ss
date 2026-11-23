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
;; The WRITE half of the same seam, and it has to target whatever the read half
;; would consult or the two disagree: chez-current-ns prefers a live (binding
;; [*ns* ..]) over the parameter, so writing only the parameter under such a
;; binding is a write that no read can see. JVM parity too — (set! *ns* ..) is
;; Var.set, which writes the innermost thread binding and leaves the root alone,
;; so the binding frame popping still restores the ns that was current before it.
;;
;; What the asymmetry cost: ldr-load-body saves the current ns and restores it on
;; the way out so a loaded file's ns form cannot leak past its own load. Under a
;; *ns* binding that restore silently did nothing, so a nested require left *ns*
;; pointing at the LAST file it finished — every def after the require in the
;; requiring file interned into the wrong namespace.
;;
;; Bootstrap-safe by the same star-ns-cell test chez-current-ns uses: it is #f
;; until dyn-binding.ss captures the cell, and the boot writes just set the
;; parameter.
(define (set-chez-ns! ns)
  (let ((p (and star-ns-cell (dyn-find-binding star-ns-cell))))
    (if p
        (set-cdr! p (intern-ns! ns))
        (chez-current-ns-param ns))))

;; The jolt-multifn record type itself is defined in values.ss (see there).

;; a multimethod is CODE: a var holding one travels in a state image as the var's
;; NAME, the way a named fn does, instead of having its dispatch tables walked.
(register-code-value! jolt-multifn?)

(define kw-default (keyword #f "default"))
;; a key no dispatch value is eq? to (mm-resolve's inline cache, below)
(define mm-icache-empty-key (list 'mm-icache-empty))
(define (new-mm-table) (make-hashtable key-hash jolt=))

;; mm-tbl-mu serializes every MUTATION of every multifn's methods / prefers /
;; cache tables, every WHOLE-TABLE scan of them, and the jolt-mm-epoch bump.
;; Single-key reads stay unlocked, on the split rt.ss spells out for var-table:
;; these are strong general hashtables, whose insert publishes a fully-formed
;; list with one vector-set! and whose resize builds a complete new bucket vector
;; before publishing it, so an unlocked reader walks consistent structure and the
;; worst it can see is a stale miss. That matters here because the read is the
;; dispatch path — mm-resolve's first act is a probe of the methods table — and a
;; mutex there would sit on every multimethod call in the program.
;;
;; One mutex for all multifns rather than one per multifn: the writers are
;; defmethod / prefer-method / remove-method, which run at definition time, and a
;; per-multifn lock would mean a record field and a jolt-multifn-v3 UID for no
;; measurable gain.
;;
;; Two things go wrong without this. Racing defmethods lose entries outright (the
;; PR that added var-table-mu measured 8.6k of 240k lost on a strong table with
;; unlocked writers), and a lost defmethod is a method that silently is not
;; there. And the dispatch CACHE is invalidated by hashtable-clear!, so a resolve
;; that began before a defmethod could publish its now-stale answer AFTER the
;; clear and have every later dispatch read it — a wrong method rather than a
;; stale miss. mm-resolve's publish re-checks the generation under the lock for
;; exactly that.
;;
;; User code never runs under this mutex: mm-find-isa calls isa? / parents, and
;; every caller resolves before it locks.
(define mm-tbl-mu (make-mutex))

;; A fresh multimethod value: the dispatch fn, the default dispatch value, the
;; hierarchy to resolve isa? against (#f = the global one), and the three empty
;; tables every multifn starts with. Factored out of jolt-defmulti-setup because
;; `defmulti` is not the only way one gets built — the clojure.lang.MultiFn
;; constructor (java/host-static-classes.ss) is what code that spells a
;; multimethod reflectively reaches for, and it must produce the same value.
(define (make-multifn name dispatch default hierarchy)
  (make-jolt-multifn name dispatch (new-mm-table) default hierarchy
                     (new-mm-table) (new-mm-table) -1 #f mm-icache-empty))

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

;; A host layer that owns a multimethod's built-in methods (the java layer's
;; print-method printers) registers them here, keyed by the multimethod's
;; qualified name: they go into the multifn when defmulti makes it, or at once if
;; it already exists. The defmulti runs from the prelude, after the host files
;; load, so this is what lets a host file seed a table it cannot yet see.
(define mm-seed-hooks '())
(define (register-multifn-seed! ns name proc)
  (set! mm-seed-hooks (cons (cons (string-append ns "/" name) proc) mm-seed-hooks))
  (let* ((c (var-cell-lookup ns name))
         (cur (and c (var-cell-defined? c) (var-cell-root c))))
    (when (jolt-multifn? cur) (proc cur))))
(define (mm-run-seed-hooks! ns name mf)
  (let ((key (string-append ns "/" name)))
    (for-each (lambda (h) (when (string=? (car h) key) ((cdr h) mf))) mm-seed-hooks)))

(define (jolt-defmulti-setup name-sym dispatch . opts)
  (let-values (((dk h) (parse-mm-opts opts)))
    (let* ((sns (symbol-t-ns name-sym))
           ;; the macro qualifies the name with its EXPANSION ns, so a defmulti
           ;; deferred inside a fn (a deftest body) still defines in the ns it
           ;; was written in, not whatever ns is current when it finally runs.
           (ns (if (string? sns) sns (chez-current-ns)))
           (cell (var-cell-lookup ns (symbol-t-name name-sym)))
           (have (and cell (var-cell-defined? cell) (var-cell-root cell))))
      ;; defmulti expands through defonce on the JVM: re-evaluating it -- a file
      ;; reloaded in a REPL, a namespace required twice -- keeps the multifn and
      ;; every method registered on it, instead of starting an empty one
      (if (jolt-multifn? have)
          have
          (let ((mf (make-multifn (symbol-t-name name-sym) dispatch dk h)))
            (def-var! ns (symbol-t-name name-sym) mf)
            (mm-run-seed-hooks! ns (symbol-t-name name-sym) mf)
            mf)))))

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
         ;; ONE decision, yielding (ns . name) together: which namespace holds
         ;; the multifn AND what it is called there. Deriving the two separately
         ;; let them disagree -- a :rename'd refer whose local name is also
         ;; defined here took the local branch for the ns and the refer's source
         ;; name for the name, and looked up a var that was in neither place.
         (mns+name
          (cond
            (qns (cons (or (chez-resolve-alias here qns) qns) nm))
            ;; a locally DEFINED var (a multifn interned in `here`) wins; an
            ;; unbound forward-ref cell the analyzer interned for the multifn
            ;; name while compiling the defmethod body does NOT — otherwise a
            ;; library's unqualified (defmethod print-method …), loaded via
            ;; require, would shadow the referred clojure.core multifn instead
            ;; of extending it. An undefined local falls through to the refer.
            ((let ((c (var-cell-lookup here nm))) (and c (var-cell-defined? c)))
             (cons here nm))
            ;; the refer table's value IS (target . source-name), so a :rename'd
            ;; local reaches the var it was renamed FROM.
            ((chez-resolve-refer here nm) => values)
            ;; implicit refer-clojure: an unqualified name that isn't defined
            ;; or explicitly referred locally but names a clojure.core MULTIFN
            ;; (print-method, print-dup, …) extends that multifn. refer-clojure
            ;; is resolved at compile time, so chez-resolve-refer doesn't record
            ;; it; without this a library's (defmethod print-method …) built a
            ;; dead per-ns shadow the printer never consults.
            ((jolt-multifn? (var-deref "clojure.core" nm)) (cons "clojure.core" nm))
            (else (cons here nm))))
         (mns (car mns+name))
         ;; the multifn's name IN mns — the same as nm unless a :rename brought
         ;; it in under a different local name.
         (source-name (cdr mns+name))
         (cur (var-deref mns source-name))
         (mf (if (jolt-multifn? cur) cur
                 ;; auto-create: copy the dispatch fn + default from a same-named
                 ;; clojure.core multifn (e.g. print-method's 2-arg dispatch) so a
                 ;; (defmethod print-method ...) before naming clojure.core's still
                 ;; dispatches right — the old 1-arg identity fallback crashed.
                 ;; source-name throughout, not nm: under a :rename the var lives
                 ;; in mns under its ORIGINAL name, and it is core's same-named
                 ;; multifn -- not one named after the local alias -- that has the
                 ;; dispatch fn worth copying.
                 (let* ((core (var-deref "clojure.core" source-name))
                        (disp (if (jolt-multifn? core)
                                  (jolt-multifn-dispatch-fn core)
                                  (var-deref "clojure.core" "identity")))
                        (deft (if (jolt-multifn? core) (jolt-multifn-default core) kw-default))
                        (m (make-jolt-multifn source-name disp (new-mm-table) deft #f (new-mm-table) (new-mm-table) -1 #f mm-icache-empty)))
                   (def-var! mns source-name m) m))))
    (jolt-with-mutex mm-tbl-mu
      (hashtable-set! (jolt-multifn-methods mf) (mm-dispatch-val-canon dval) impl)
      (mm-epoch-bump!))
    mf))

;; --- dispatch ----------------------------------------------------------------
;; The hierarchy VALUE a multifn resolves against. On the JVM the field is an
;; IRef that MultiFn.getMethod derefs per dispatch, so an atom and a var both
;; read their current contents; a plain map is itself. The var arm is what
;; (new clojure.lang.MultiFn name dispatch default #'clojure.core/global-hierarchy)
;; hands over — the spelling SCI's defmulti emits.
(define (mm-hierarchy-value h)
  (let ((v (if (var-cell? h) (var-cell-deref h) h)))
    (if (jolt-atom? v) (jolt-atom-val v) v)))

(define (mm-isa? mf)
  ;; the overlay's isa? (the hierarchy system is pure Clojure); a per-mm :hierarchy
  ;; is an atom or a var (deref each dispatch, like a Clojure IRef) or a plain map.
  (let* ((isa (var-deref "clojure.core" "isa?"))
         (h (jolt-multifn-hierarchy mf))
         (hval (and h (mm-hierarchy-value h))))
    (lambda (x y) (jolt-truthy? (if hval (jolt-invoke isa hval x y) (jolt-invoke isa x y))))))

;; the parent dispatch values of x in mf's hierarchy, as a Scheme list.
(define (mm-parents mf)
  (let* ((par (var-deref "clojure.core" "parents"))
         (h (jolt-multifn-hierarchy mf))
         (hval (and h (mm-hierarchy-value h))))
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
         ;; a whole-table scan, so it takes the lock like every other one: keys
         ;; reads ht-size, allocates a vector of that length and walks the buckets
         ;; bounded by both, so a concurrent defmethod truncates the result and a
         ;; concurrent remove-method leaves trailing FILL slots — which then go to
         ;; jolt= as dispatch values. The isa? scan below runs OUTSIDE the lock.
         (keys (filter (lambda (k) (not (jolt= k default)))
                       (vector->list (jolt-with-mutex mm-tbl-mu (hashtable-keys methods)))))
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
                                        "' match dispatch value: " (mm-dv-string dv) " -> "
                                        (mm-dv-string best) " and " (mm-dv-string k)
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
;; Every change to a methods / prefers table bumps the epoch through here, with
;; the change already made and mm-tbl-mu held. The release fence orders the
;; table's stores ahead of the epoch's, and mm-resolve-table's acquire fence
;; orders its epoch load ahead of its table loads, so a reader that sees the new
;; epoch also sees the new table. Without the pair, arm64 lets a reader pair the
;; new epoch with the old method and cache that, stamped current, for good: a
;; writer swapping one method while three threads dispatched read its own
;; superseded method back 2 times in 200k rounds.
(define (mm-epoch-bump!)
  (memory-order-release)
  (set! jolt-mm-epoch (fx+ jolt-mm-epoch 1)))

;; the hierarchy object isa? resolves against for mf: its own :hierarchy atom/map
;; when set, else the global-hierarchy atom's current value. deref'd lazily (the
;; prelude loads after this file), exactly as mm-isa? does.
(define (mm-current-hierarchy mf)
  (let ((h (jolt-multifn-hierarchy mf)))
    (mm-hierarchy-value
      (or h (var-deref "clojure.core" "global-hierarchy")))))

;; drop the whole cache if the epoch advanced or the hierarchy value changed.
;; The check is re-run under the lock: the clear and the two stamp writes have to
;; be one step, or two threads noticing the same invalidation can interleave a
;; clear between the other's clear and its stamp, leaving the cache stamped
;; current while an entry from the previous generation survives.
(define (mm-cache-validate! mf)
  (let ((hier (mm-current-hierarchy mf)))
    (unless (and (fx= (jolt-multifn-cache-epoch mf) jolt-mm-epoch)
                 (eq? (jolt-multifn-cache-hier mf) hier))
      (jolt-with-mutex mm-tbl-mu
        (unless (and (fx= (jolt-multifn-cache-epoch mf) jolt-mm-epoch)
                     (eq? (jolt-multifn-cache-hier mf) hier))
          (hashtable-clear! (jolt-multifn-cache mf))
          (jolt-multifn-cache-epoch-set! mf jolt-mm-epoch)
          (jolt-multifn-cache-hier-set! mf hier))))))

;; resolve dv to its method fn. An exact table hit is always current (defmethod /
;; remove mutate that table and bump the epoch), so it bypasses the cache. On a
;; miss the cache is validated then consulted; a miss there runs the isa? scan
;; (mm-find-isa) and falls back to the :default method, memoizing the result. #f
;; only when nothing matches - the caller raises.
(define (mm-resolve mf dv)
  (let ((v (jolt-multifn-icache mf)))
    (if (fx= (vector-ref v 0) jolt-mm-epoch)
        (let scan ((i 1))
          (cond ((fx= i mm-icache-len) (mm-resolve-table mf dv))
                ((eq? (vector-ref v i) dv) (vector-ref v (fx+ i 1)))
                (else (scan (fx+ i 2)))))
        (mm-resolve-table mf dv))))

;; The inline cache in front of the table: the last eight dispatch values that
;; hit the methods table DIRECTLY, compared with eq?, stamped with the epoch they
;; were read under. The table lookup hashes the dispatch value with key-hash and
;; compares with jolt= on every call; a multimethod dispatching on keywords (most
;; of them -- rewrite-clj's parse-next* takes one per node, #1713) answers here in
;; a few loads instead. Eight, not four: parse-next* cycles through five to seven
;; values on ordinary source (token, whitespace, list, vector, delimiter, ...),
;; and a cache smaller than the cycle missed on every call. Only direct hits are
;; cached, so the entry depends on the methods table alone -- not the hierarchy
;; or the prefers -- and every change to that table bumps jolt-mm-epoch, which
;; retires the whole vector. Only values eq? can stand for are kept: an interned
;; keyword, a fixnum, a char, a boolean, nil. The vector is replaced, never
;; mutated, so a reader on another thread sees an old vector or a new one, never
;; a torn one; a lost race only loses an entry. Slot 0 is the epoch; the empty
;; vector's -1 never matches. A new entry goes in front and the last one drops.
(define mm-icache-slots 8)
(define mm-icache-len (fx+ 1 (fx* 2 mm-icache-slots)))
(define mm-icache-empty
  (let ((v (make-vector mm-icache-len #f)))
    (vector-set! v 0 -1)
    (do ((i 1 (fx+ i 2))) ((fx= i mm-icache-len) v)
      (vector-set! v i mm-icache-empty-key))))
(define (mm-icache-key? dv)
  (or (keyword? dv) (fixnum? dv) (char? dv) (boolean? dv) (jolt-nil? dv)))
(define (mm-icache-add! mf epoch dv m)
  (let* ((old (jolt-multifn-icache mf))
         (src (if (fx= (vector-ref old 0) epoch) old mm-icache-empty))
         (v (make-vector mm-icache-len #f)))
    (vector-set! v 0 epoch)
    (vector-set! v 1 dv)
    (vector-set! v 2 m)
    (do ((i 3 (fx+ i 1))) ((fx= i mm-icache-len))
      (vector-set! v i (vector-ref src (fx- i 2))))
    ;; the new vector's slots are written before another thread can reach it
    (memory-order-release)
    (jolt-multifn-icache-set! mf v)))

(define (mm-resolve-table mf dv)
  (let* ((methods (jolt-multifn-methods mf))
         (epoch (let ((e jolt-mm-epoch)) (memory-order-acquire) e))
         (direct (hashtable-ref methods dv #f)))
    (when (and direct (mm-icache-key? dv))
      (mm-icache-add! mf epoch dv direct))
    (or direct
        (begin
          (mm-cache-validate! mf)
          (let ((cache (jolt-multifn-cache mf)))
            (or (hashtable-ref cache dv #f)
                ;; Read the generation we are about to resolve against BEFORE
                ;; resolving, then publish only if it is still the one the cache
                ;; is stamped with. Without that, a resolve which started before a
                ;; defmethod can land its stale answer after mm-cache-validate!
                ;; cleared the table, and every later dispatch reads it — a wrong
                ;; method, not a stale miss, and it never expires because the
                ;; stamp already says current. mm-find-isa runs OUTSIDE the lock:
                ;; it calls isa? / parents, which is user code.
                (let* ((epoch (let ((e jolt-mm-epoch)) (memory-order-acquire) e))
                       (hier (mm-current-hierarchy mf))
                       (m (or (mm-find-isa mf dv)
                              (hashtable-ref methods (jolt-multifn-default mf) #f))))
                  (when m
                    (jolt-with-mutex mm-tbl-mu
                      (when (and (fx= epoch jolt-mm-epoch)
                                 (fx= (jolt-multifn-cache-epoch mf) epoch)
                                 (eq? (jolt-multifn-cache-hier mf) hier))
                        (hashtable-set! cache dv m))))
                  m)))))))

;; MultiFn's messages concatenate a dispatch value, so it reads as its toString:
;; a string bare, a class as "class x.Y", a collection readably, nil as null
(define (mm-dv-string dv)
  (if (jolt-nil? dv) "null" (jolt-str-one dv)))
(define (mm-no-method mf dv)
  (throw-jvm (quote IllegalArgumentException)
             (string-append "No method in multimethod '" (jolt-multifn-name mf)
                            "' for dispatch value: " (mm-dv-string dv))))

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

(define (jolt-prefer-method-setup mf dval-a* dval-b*)
  ;; every table op keys a dispatch value as defmethod stores it: a deftype or
  ;; defrecord name evaluates to its constructor, filed under its class tag
  (define dval-a (mm-dispatch-val-canon dval-a*))
  (define dval-b (mm-dispatch-val-canon dval-b*))
  (when (jolt-multifn? mf)
    ;; a preference b-over-a (direct or transitive) makes a-over-b a conflict, as
    ;; in Clojure MultiFn.preferMethod.
    ;; the conflict check calls parents (user code), so it stays outside the lock
    (when (mm-prefers? mf dval-b dval-a)
      (throw-jvm (quote IllegalStateException)
                 (string-append "Preference conflict in multimethod '" (jolt-multifn-name mf)
                                "': " (jolt-pr-str dval-b) " is already preferred to " (jolt-pr-str dval-a))))
    (jolt-with-mutex mm-tbl-mu
      ;; the sub-table lookup and its insert are one step: two threads preferring
      ;; different pairs under the same dval-a would otherwise each build a fresh
      ;; sub-table and one of the preferences would be dropped with it.
      (let ((sub (or (hashtable-ref (jolt-multifn-prefers mf) dval-a #f)
                     (let ((h (new-mm-table)))
                       (hashtable-set! (jolt-multifn-prefers mf) dval-a h) h))))
        (hashtable-set! sub dval-b #t))
      (mm-epoch-bump!)))
  mf)

(define (jolt-remove-method-setup mf dval)
  (when (jolt-multifn? mf)
    (jolt-with-mutex mm-tbl-mu
      (hashtable-delete! (jolt-multifn-methods mf) (mm-dispatch-val-canon dval))
      (mm-epoch-bump!)))
  mf)

(define (jolt-remove-all-methods-setup mf)
  (when (jolt-multifn? mf)
    (jolt-with-mutex mm-tbl-mu
      (hashtable-clear! (jolt-multifn-methods mf))
      (mm-epoch-bump!)))
  mf)

(define (jolt-get-method-setup mf dval*)
  (define dval (mm-dispatch-val-canon dval*))
  (if (jolt-multifn? mf)
      (or (hashtable-ref (jolt-multifn-methods mf) dval #f)
          ;; isa?-based lookup, exactly like the runtime mm-resolve path —
          ;; so get-method and actual dispatch return the same method.
          (mm-find-isa mf dval)
          (hashtable-ref (jolt-multifn-methods mf) (jolt-multifn-default mf) #f)
          jolt-nil)
      jolt-nil))

;; Whole-table scans, so they snapshot under the lock and build the jolt map
;; outside it. hashtable-entries reads ht-size, allocates two vectors of that
;; length and walks the buckets bounded by both, so a concurrent defmethod
;; truncates the result and a concurrent remove-method leaves trailing FILL slots
;; — which would reach jolt-assoc as a dispatch value of 0.
(define (jolt-methods-setup mf)
  (if (jolt-multifn? mf)
      (let* ((kv (jolt-with-mutex mm-tbl-mu
                   (let-values (((ks vs) (hashtable-entries (jolt-multifn-methods mf))))
                     (cons ks vs))))
             (ks (car kv)) (vs (cdr kv)))
        (let loop ((i 0) (m (jolt-hash-map)))
          (if (fx>=? i (vector-length ks)) m
              (loop (fx+ i 1) (jolt-assoc m (vector-ref ks i) (vector-ref vs i))))))
      jolt-nil))

(define (jolt-prefers-setup mf)
  (if (not (jolt-multifn? mf)) (jolt-hash-map)
      ;; the inner sub-tables are snapshotted in the SAME critical section: a
      ;; prefer-method landing between the outer scan and an inner one would
      ;; otherwise be read mid-insert.
      (let* ((kv (jolt-with-mutex mm-tbl-mu
                   (let-values (((ks vs) (hashtable-entries (jolt-multifn-prefers mf))))
                     (cons ks (vector-map (lambda (sub) (hashtable-keys sub)) vs)))))
             (ks (car kv)) (subs (cdr kv)))
        (let loop ((i 0) (m (jolt-hash-map)))
          (if (fx>=? i (vector-length ks)) m
              ;; each value is an inner set of preferred-over keys -> a jolt set
              (loop (fx+ i 1)
                    (jolt-assoc m (vector-ref ks i)
                                (apply jolt-hash-set
                                       (vector->list (vector-ref subs i))))))))))

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
