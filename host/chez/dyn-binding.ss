;; dynamic var binding — binding / with-bindings* / var-set / thread-bound? /
;; with-local-vars / with-redefs / bound-fn* / get-thread-bindings.
;;
;; A per-thread dynamic-binding stack: a list of frames, innermost (most recently
;; pushed) at the HEAD. Each frame is an alist of (var-cell . value) MUTABLE pairs
;; — so var-set can update the innermost binding in place (set-cdr!), matching
;; Clojure where var-set targets the current binding, not the root.
;;
;; The binding macro builds a frame as a jolt map (array-map of (var x) -> value);
;; push-thread-bindings folds it into the alist. Lookups walk frames by cell
;; IDENTITY (eq?) — vars are interned, so (var x) always yields the same cell, and
;; this sidesteps a persistent-hash-map-can't-find-a-var-key quirk.
;;
;; var reads (var-deref in compiled code, jolt-var-get / deref on a cell) consult
;; the stack before falling back to the cell root. Loaded LAST (after vars.ss and
;; ns.ss) so it chains the fully-extended jolt-var-get and overrides rt.ss var-deref.

;; THREAD-LOCAL: a Chez thread parameter, so each OS thread (a future / go block)
;; has its own binding stack. Chez initializes a new thread's parameter
;; to the spawning thread's value at fork time, giving Clojure binding conveyance
;; for free (the future shim also installs an explicit snapshot, belt-and-suspenders).
(define dyn-binding-stack (make-thread-parameter '()))

;; find the innermost (cell . value) pair binding CELL, or #f.
(define (dyn-find-binding cell)
  (let loop ((frames (dyn-binding-stack)))
    (and (pair? frames)
         (or (assq cell (car frames))
             (loop (cdr frames))))))

;; a unique sentinel: distinguishes "no thread binding" from a binding whose
;; value happens to be jolt-nil.
(define dyn-no-binding (list 'no-binding))
(define (dyn-binding-value cell)
  (if (pair? (dyn-binding-stack))
      (let ((p (dyn-find-binding cell)))
        (if p
            (let ((val (cdr p)))
              (if (var-cell? val) (jolt-var-get val) val))  ; nested var deref (Clojure)
            dyn-no-binding))
      dyn-no-binding))

;; push-thread-bindings: frame is a jolt map of var-cell -> value. Fold it into an
;; identity-keyed alist of mutable pairs and push.
(define (jolt-push-thread-bindings frame)
  (dyn-binding-stack
   (cons (pmap-fold frame (lambda (k v acc) (cons (cons k v) acc)) '())
         (dyn-binding-stack)))
  jolt-nil)

(define (jolt-pop-thread-bindings)
  (when (pair? (dyn-binding-stack))
    (dyn-binding-stack (cdr (dyn-binding-stack))))
  jolt-nil)

;; get-thread-bindings: a jolt map of every currently-bound cell -> value,
;; innermost wins. Merge oldest-frame-first (the stack head is innermost). The
;; result can be re-pushed by with-bindings* / bound-fn*.
(define (jolt-get-thread-bindings)
  (let loop ((frames (reverse (dyn-binding-stack))) (m (jolt-hash-map)))
    (if (null? frames)
        m
        (loop (cdr frames)
              (let frame-loop ((alist (car frames)) (m m))
                (if (null? alist)
                    m
                    (frame-loop (cdr alist)
                                (pmap-assoc m (caar alist) (cdar alist)))))))))

;; __thread-bound? — single var; true iff it has a thread binding.
(define (jolt-thread-bound? v)
  (and (var-cell? v) (dyn-find-binding v) #t))

;; var-set: update the innermost frame that binds v (in place); else set the root.
(define (jolt-var-set v val)
  (if (var-cell? v)
      (let ((p (dyn-find-binding v)))
        (if p
            (begin (set-cdr! p val) val)
            ;; a ROOT change is Var.bindRoot: validate, set, notify watches
            ;; (a thread-binding set does not notify, like the JVM).
            (let ((old (var-cell-root v)))
              (iref-validate v val)
              (var-cell-root-set! v val) (var-cell-defined?-set! v #t)
              (iref-notify v old val)
              val)))
      (error #f "var-set: not a var" v)))

;; alter-var-root: atomically apply f to the current root plus args.
(define (jolt-alter-var-root v f . args)
  (let* ((old (var-cell-root v))
         (new (apply jolt-invoke f old args)))
    (iref-validate v new)
    (var-cell-root-set! v new)
    (var-cell-defined?-set! v #t)
    (iref-notify v old new)
    new))

;; __local-var: a fresh free-standing var cell (not interned). with-local-vars
;; binds these as lexical locals; var-get/var-set read/write the root. Each gets a
;; unique name so two locals never compare/hash equal as map keys.
(define local-var-counter 0)
(define (jolt-local-var . args)
  (set! local-var-counter (fx+ local-var-counter 1))
  (make-var-cell "" (string-append "local-" (number->string local-var-counter))
                 (if (pair? args) (car args) jolt-nil)
                 #t))

;; --- chain the var-read paths onto the binding stack -------------------------

;; var-deref (rt.ss): the compiled-code read path for every clojure.core var
;; reference. Consult the stack first; fall straight back to the root (NOT through
;; jolt-var-get's unbound-error path) so undefined-var reads keep prior behaviour.
;; The *ns* var cell — its reads are thread-local: with no thread-binding they
;; derive from chez-current-ns (a thread-parameter), so *ns* tracks in-ns per
;; thread and a (binding [*ns* ..]) drives resolution. Captured now that *ns* is
;; defined (ns.ss loaded earlier); chez-current-ns consults it too.
(set! star-ns-cell (jolt-var "clojure.core" "*ns*"))

(define %dyn-rt-var-deref var-deref)
(set! var-deref
  (lambda (ns name)
    (let ((cell (jolt-var ns name)))
      (let ((bv (dyn-binding-value cell)))
        (cond ((not (eq? bv dyn-no-binding)) bv)
              ((eq? cell star-ns-cell) (intern-ns! (chez-current-ns)))
              (else (var-cell-root cell)))))))

;; var-deref's read on an ALREADY-RESOLVED cell — what compiled code emits when it
;; caches the cell at a reference site. Binding stack first, then *ns* thread-local,
;; else the raw root. Lenient on an unbound root (returns the sentinel), matching
;; var-deref — NOT the strict jolt-var-get, which throws "Unbound var".
(define (var-cell-deref cell)
  (let ((bv (dyn-binding-value cell)))
    (cond ((not (eq? bv dyn-no-binding)) bv)
          ((eq? cell star-ns-cell) (intern-ns! (chez-current-ns)))
          (else (var-cell-root cell)))))

;; jolt-var-get (vars.ss): the var-get fn + deref/@ on a cell. Stack first, then
;; the original (which errors on an unbound root, matching Clojure).
(define %dyn-var-get jolt-var-get)
(set! jolt-var-get
  (lambda (v)
    (if (var-cell? v)
        (let ((bv (dyn-binding-value v)))
          (cond ((not (eq? bv dyn-no-binding)) bv)
                ((eq? v star-ns-cell) (intern-ns! (chez-current-ns)))
                (else (%dyn-var-get v))))
        (%dyn-var-get v))))

;; var-cell keys hash/compare by ns/name (jolt=2 in vars.ss already compares
;; ns/name) — stable under root mutation, so a var works as a map key (with-redefs
;; builds (hash-map (var f) v); get-thread-bindings returns a var-keyed map).
(register-hash-arm! var-cell? (lambda (x) (equal-hash (cons (var-cell-ns x) (var-cell-name x)))))

;; --- bind the host seams the overlay references -----------------------------
(def-var! "clojure.core" "push-thread-bindings" jolt-push-thread-bindings)
(def-var! "clojure.core" "pop-thread-bindings" jolt-pop-thread-bindings)
(def-var! "clojure.core" "get-thread-bindings" jolt-get-thread-bindings)
(def-var! "clojure.core" "__thread-bound?" jolt-thread-bound?)
(def-var! "clojure.core" "var-set" jolt-var-set)
(def-var! "clojure.core" "alter-var-root" jolt-alter-var-root)
(def-var! "clojure.core" "__local-var" jolt-local-var)
;; re-assert var-get / deref to the new (stack-aware) closures (vars.ss captured
;; the pre-chain values).
(def-var! "clojure.core" "var-get" jolt-var-get)
(def-var! "clojure.core" "deref" jolt-deref)
