;; atoms — host-coupled mutable reference cells for the Chez host.
;;
;; atom/deref/swap!/reset! are host primitives (not the clojure.core overlay),
;; so the runtime provides native shims, def-var!'d into clojure.core. They
;; lower to var-deref in prelude mode. The hierarchy machinery
;; (global-hierarchy = (atom (make-hierarchy))) calls `atom` at the prelude's
;; LOAD time, so without this shim the whole prelude fails to load.
;;
;; compare-and-set!/swap-vals!/reset-vals! are overlay fns over the native kernel
;; in the live system; provided here natively too so the host is self-sufficient
;; for atoms without the full prelude (the overlay versions, when the full prelude
;; loads, override these but compose the same native kernel).

;; watches is an alist of (key . watch-fn); validator is a jolt fn or jolt-nil.
;; The peripheral ops + the notify/validate behaviour live natively here, and
;; post-prelude.ss re-asserts them over the overlay's def-var!.
;; `lock` is a per-atom mutex guarding the read-modify-write critical sections,
;; so swap!/reset!/compare-and-set! are atomic under real OS threads
;; (futures/go blocks share the heap). The user fn in swap! runs OUTSIDE the lock
;; (a CAS retry loop, like the JVM) so it never deadlocks on re-entrant access and
;; a watch/validator can deref the same atom.
(define-record-type jolt-atom
  (fields (mutable val) (mutable watches) (mutable validator) lock)
  (nongenerative jolt-atom-v3))

;; a rejected reference value is IllegalStateException, like ARef.validate.
(define (jolt-iref-state-throw)
  (jolt-throw (jolt-host-throwable "java.lang.IllegalStateException" "Invalid reference state")))

;; (atom init :meta m :validator f) — the ARef ctor contract: the validator runs
;; against the initial value (an invalid init never constructs), :meta must be a
;; map (anything else is the JVM's IPersistentMap cast failure).
(define (jolt-atom-new v . opts)
  (let loop ((o opts) (validator jolt-nil) (m #f))
    (cond
      ((or (null? o) (null? (cdr o)))
       (let ((a (make-jolt-atom v '() validator (make-mutex))))
         (jolt-atom-validate a v)
         (when (and m (not (jolt-nil? m)))
           (unless (jolt-map? m)
             (jolt-throw (jolt-host-throwable
                          "java.lang.ClassCastException"
                          (string-append "class " (jolt-class-name m)
                                         " cannot be cast to class clojure.lang.IPersistentMap"))))
           (hashtable-set! meta-table a m))
         a))
      ((and (keyword-t? (car o)) (string=? (keyword-t-name (car o)) "validator"))
       (loop (cddr o) (cadr o) m))
      ((and (keyword-t? (car o)) (string=? (keyword-t-name (car o)) "meta"))
       (loop (cddr o) validator (cadr o)))
      (else (loop (cddr o) validator m)))))

;; validate a candidate value: a non-nil validator that returns falsey rejects.
(define (jolt-atom-validate a v)
  (let ((vf (jolt-atom-validator a)))
    (when (and (not (jolt-nil? vf)) (jolt-not (jolt-invoke vf v)))
      (jolt-iref-state-throw))))

;; notify each watch (k ref old new), in insertion order (alist is reverse-built,
;; so walk it reversed to match add order).
(define (jolt-atom-notify a old new)
  (for-each (lambda (kv) (jolt-invoke (cdr kv) (car kv) a old new))
            (reverse (jolt-atom-watches a))))

;; deref reads an atom; it also unwraps a `reduced` (Clojure @(reduced x) => x,
;; which the overlay's `unreduced` relies on). The reduced record is in seq.ss.
(define (jolt-deref x)
  (cond
    ((jolt-atom? x) (jolt-atom-val x))
    ((jolt-reduced? x) (jolt-reduced-val x))
    (else (throw-jvm (quote ClassCastException) (string-append "deref: unsupported reference type " (jolt-final-str x))))))

;; CAS the val from `old` to `nv` by identity (eq?), atomically. Returns #t on
;; success. The compute step (f) runs outside this, so we re-check under the lock.
(define (jolt-atom-cas! a old nv)
  (with-mutex (jolt-atom-lock a)
    (if (eq? (jolt-atom-val a) old)
        (begin (jolt-atom-val-set! a nv) #t)
        #f)))

;; (swap! a f arg*): JVM-style CAS loop — read, compute f OUTSIDE the lock, then
;; atomically compare-and-set; retry if another thread changed it. Validate the
;; new value before storing, notify watches after.
(define (jolt-swap! a f . args)
  (let retry ()
    (let* ((old (jolt-atom-val a))
           (nv (apply jolt-invoke f old args)))
      (jolt-atom-validate a nv)
      (if (jolt-atom-cas! a old nv)
          (begin (jolt-atom-notify a old nv) nv)
          (retry)))))

(define (jolt-reset! a v)
  (jolt-atom-validate a v)
  (let ((old (with-mutex (jolt-atom-lock a)
               (let ((o (jolt-atom-val a))) (jolt-atom-val-set! a v) o))))
    (jolt-atom-notify a old v)
    v))

;; compare-and-set! keeps jolt= (value) semantics, done atomically under the lock.
(define (jolt-compare-and-set! a oldv newv)
  (jolt-atom-validate a newv)
  (let ((swapped (with-mutex (jolt-atom-lock a)
                   (if (jolt= (jolt-atom-val a) oldv)
                       (begin (jolt-atom-val-set! a newv) #t)
                       #f))))
    (when swapped (jolt-atom-notify a oldv newv))
    swapped))

(define (jolt-swap-vals! a f . args)
  (let retry ()
    (let* ((old (jolt-atom-val a))
           (nv (apply jolt-invoke f old args)))
      (jolt-atom-validate a nv)
      (if (jolt-atom-cas! a old nv)
          (begin (jolt-atom-notify a old nv) (jolt-vector old nv))
          (retry)))))

(define (jolt-reset-vals! a v)
  (jolt-atom-validate a v)
  (let ((old (with-mutex (jolt-atom-lock a)
               (let ((o (jolt-atom-val a))) (jolt-atom-val-set! a v) o))))
    (jolt-atom-notify a old v)
    (jolt-vector old v)))

;; --- watches / validators: the IRef seam --------------------------------------
;; On the JVM these are the ARef contract shared by atom/var/agent/ref. The atom
;; keeps its record slots (the hot swap!/reset! path); every OTHER watchable
;; reference type registers a predicate here and stores its watches/validator in
;; identity-keyed side tables. A ref type makes itself notify by calling
;; iref-notify at its mutation points (vars do at root set).
(define iref-arms '())
(define (register-iref-arm! pred) (set! iref-arms (cons pred iref-arms)))
(define (iref? r)
  (let loop ((as iref-arms))
    (cond ((null? as) #f) (((car as) r) #t) (else (loop (cdr as))))))
(define iref-watch-tbl (make-weak-eq-hashtable))
(define iref-validator-tbl (make-weak-eq-hashtable))
(define (iref-notify r old new)
  (for-each (lambda (kv) (jolt-invoke (cdr kv) (car kv) r old new))
            (reverse (hashtable-ref iref-watch-tbl r '()))))
(define (iref-validate r v)
  (let ((vf (hashtable-ref iref-validator-tbl r jolt-nil)))
    (when (and (not (jolt-nil? vf)) (jolt-not (jolt-invoke vf v)))
      (jolt-iref-state-throw))))

;; add-watch interns (key . fn) (replacing any existing key, keeping order);
;; remove-watch drops it; both return the reference. set-validator! installs a
;; validator and validates the CURRENT value immediately (Clojure throws if it's
;; already invalid); get-validator reads the slot.
(define (jolt-watch-add alist key f)
  (cons (cons key f) (remp (lambda (kv) (jolt=2 (car kv) key)) alist)))
(define (jolt-add-watch a key f)
  (cond
    ((jolt-atom? a)
     (jolt-atom-watches-set! a (jolt-watch-add (jolt-atom-watches a) key f))
     a)
    ((iref? a)
     (hashtable-set! iref-watch-tbl a (jolt-watch-add (hashtable-ref iref-watch-tbl a '()) key f))
     a)
    (else (throw-jvm (quote ClassCastException) "add-watch: not a watchable reference"))))
(define (jolt-remove-watch a key)
  (cond
    ((jolt-atom? a)
     (jolt-atom-watches-set! a
       (remp (lambda (kv) (jolt=2 (car kv) key)) (jolt-atom-watches a)))
     a)
    ((iref? a)
     (hashtable-set! iref-watch-tbl a
       (remp (lambda (kv) (jolt=2 (car kv) key)) (hashtable-ref iref-watch-tbl a '())))
     a)
    (else (throw-jvm (quote ClassCastException) "remove-watch: not a watchable reference"))))
(define (jolt-set-validator! a f)
  (let ((vf (if (jolt-nil? f) jolt-nil f)))
    (cond
      ((jolt-atom? a)
       (when (and (not (jolt-nil? vf)) (jolt-not (jolt-invoke vf (jolt-atom-val a))))
         (jolt-iref-state-throw))
       (jolt-atom-validator-set! a vf))
      ((iref? a)
       (when (and (not (jolt-nil? vf)) (jolt-not (jolt-invoke vf (jolt-deref a))))
         (jolt-iref-state-throw))
       (hashtable-set! iref-validator-tbl a vf))
      (else (throw-jvm (quote ClassCastException) "set-validator!: not a reference")))
    jolt-nil))
(define (jolt-get-validator a)
  (cond ((jolt-atom? a) (jolt-atom-validator a))
        ((iref? a) (hashtable-ref iref-validator-tbl a jolt-nil))
        (else jolt-nil)))

;; vars are watchable IRefs: a root change (def / var-set on the root /
;; alter-var-root) validates and notifies like Var.bindRoot. The def-var! wrap
;; pays two weak-table probes per def and only does IRef work on a watched var.
(register-iref-arm! var-cell?)
(define def-var!-pre-iref def-var!)
(set! def-var!
  (lambda (ns name v)
    (let ((c (jolt-var ns name)))
      (if (or (pair? (hashtable-ref iref-watch-tbl c '()))
              (not (jolt-nil? (hashtable-ref iref-validator-tbl c jolt-nil))))
          (let ((old (var-cell-root c)))
            (iref-validate c v)
            (let ((r (def-var!-pre-iref ns name v)))
              (iref-notify c old v)
              r))
          (def-var!-pre-iref ns name v)))))

(def-var! "clojure.core" "atom" jolt-atom-new)
(def-var! "clojure.core" "deref" jolt-deref)
(def-var! "clojure.core" "swap!" jolt-swap!)
(def-var! "clojure.core" "reset!" jolt-reset!)
(def-var! "clojure.core" "compare-and-set!" jolt-compare-and-set!)
(def-var! "clojure.core" "swap-vals!" jolt-swap-vals!)
(def-var! "clojure.core" "reset-vals!" jolt-reset-vals!)
(def-var! "clojure.core" "atom?" jolt-atom?)
;; peripheral ops: the overlay (20-coll) re-defs these over jolt.host/ref-put!,
;; which fails on an atom record — post-prelude.ss re-asserts the natives.
(def-var! "clojure.core" "add-watch" jolt-add-watch)
(def-var! "clojure.core" "remove-watch" jolt-remove-watch)
(def-var! "clojure.core" "set-validator!" jolt-set-validator!)
(def-var! "clojure.core" "get-validator" jolt-get-validator)
