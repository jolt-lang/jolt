;; atoms (jolt-9ziu) — host-coupled mutable reference cells for the Chez host.
;;
;; atom/deref/swap!/reset! stay in the Janet seed (not the clojure.core overlay),
;; so the Chez runtime needs native shims, def-var!'d into clojure.core. They
;; lower to var-deref in prelude mode. The hierarchy machinery
;; (global-hierarchy = (atom (make-hierarchy))) calls `atom` at the prelude's
;; LOAD time, so without this shim the whole prelude fails to load.
;;
;; compare-and-set!/swap-vals!/reset-vals! are overlay fns over the native kernel
;; in the live system; provided here natively too so the Chez host is
;; self-sufficient for atoms without the full prelude (the overlay versions, when
;; the full prelude loads, override these but compose the same native kernel).

;; watches is an alist of (key . watch-fn); validator is a jolt fn or jolt-nil.
;; The overlay's add-watch/set-validator! drive these via jolt.host/ref-put! on a
;; Janet table, which a Chez atom record is not — so the peripheral ops + the
;; notify/validate behaviour live natively here, and post-prelude.ss re-asserts
;; them over the overlay's def-var! (jolt-mn9o).
;; `lock` (jolt-byjr) is a per-atom mutex guarding the read-modify-write critical
;; sections, so swap!/reset!/compare-and-set! are atomic under real OS threads
;; (futures/go blocks share the heap on Chez). The user fn in swap! runs OUTSIDE
;; the lock (a CAS retry loop, like the JVM) so it never deadlocks on re-entrant
;; access and a watch/validator can deref the same atom.
(define-record-type jolt-atom
  (fields (mutable val) (mutable watches) (mutable validator) lock)
  (nongenerative jolt-atom-v3))

;; (atom init) / (atom init :validator f :meta m): scan the trailing keyword opts
;; for :validator (the only one with runtime behaviour; :meta is accepted/ignored).
(define (jolt-atom-new v . opts)
  (let loop ((o opts) (validator jolt-nil))
    (cond
      ((or (null? o) (null? (cdr o))) (make-jolt-atom v '() validator (make-mutex)))
      ((and (keyword-t? (car o)) (string=? (keyword-t-name (car o)) "validator"))
       (loop (cddr o) (cadr o)))
      (else (loop (cddr o) validator)))))

;; validate a candidate value: a non-nil validator that returns falsey rejects.
(define (jolt-atom-validate a v)
  (let ((vf (jolt-atom-validator a)))
    (when (and (not (jolt-nil? vf)) (jolt-not (jolt-invoke vf v)))
      (error #f "Invalid reference state"))))

;; notify each watch (k ref old new), in insertion order (alist is reverse-built,
;; so walk it reversed to match add order — matches the seed's :pairs iteration).
(define (jolt-atom-notify a old new)
  (for-each (lambda (kv) (jolt-invoke (cdr kv) (car kv) a old new))
            (reverse (jolt-atom-watches a))))

;; deref reads an atom; it also unwraps a `reduced` (Clojure @(reduced x) => x,
;; which the overlay's `unreduced` relies on). The reduced record is in seq.ss.
(define (jolt-deref x)
  (cond
    ((jolt-atom? x) (jolt-atom-val x))
    ((jolt-reduced? x) (jolt-reduced-val x))
    (else (error #f "deref: unsupported reference type" x))))

;; CAS the val from `old` to `nv` by identity (eq?), atomically. Returns #t on
;; success. The compute step (f) runs outside this, so we re-check under the lock.
(define (jolt-atom-cas! a old nv)
  (with-mutex (jolt-atom-lock a)
    (if (eq? (jolt-atom-val a) old)
        (begin (jolt-atom-val-set! a nv) #t)
        #f)))

;; (swap! a f arg*): JVM-style CAS loop — read, compute f OUTSIDE the lock, then
;; atomically compare-and-set; retry if another thread changed it. Validate the
;; new value before storing, notify watches after (the seed order).
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

;; --- watches / validators (jolt-mn9o) ---------------------------------------
;; add-watch interns (key . fn) (replacing any existing key, keeping order);
;; remove-watch drops it; both return the atom. set-validator! installs a
;; validator and validates the CURRENT value immediately (Clojure throws if it's
;; already invalid); get-validator reads the slot.
(define (jolt-add-watch a key f)
  (jolt-atom-watches-set! a
    (cons (cons key f)
          (remp (lambda (kv) (jolt=2 (car kv) key)) (jolt-atom-watches a))))
  a)
(define (jolt-remove-watch a key)
  (jolt-atom-watches-set! a
    (remp (lambda (kv) (jolt=2 (car kv) key)) (jolt-atom-watches a)))
  a)
(define (jolt-set-validator! a f)
  (let ((vf (if (jolt-nil? f) jolt-nil f)))
    (when (and (not (jolt-nil? vf)) (jolt-not (jolt-invoke vf (jolt-atom-val a))))
      (error #f "Invalid reference state"))
    (jolt-atom-validator-set! a vf)
    jolt-nil))
(define (jolt-get-validator a) (jolt-atom-validator a))

(def-var! "clojure.core" "atom" jolt-atom-new)
(def-var! "clojure.core" "deref" jolt-deref)
(def-var! "clojure.core" "swap!" jolt-swap!)
(def-var! "clojure.core" "reset!" jolt-reset!)
(def-var! "clojure.core" "compare-and-set!" jolt-compare-and-set!)
(def-var! "clojure.core" "swap-vals!" jolt-swap-vals!)
(def-var! "clojure.core" "reset-vals!" jolt-reset-vals!)
(def-var! "clojure.core" "atom?" jolt-atom?)
;; peripheral ops: the overlay (20-coll) re-defs these over jolt.host/ref-put!,
;; which fails on a Chez atom record — post-prelude.ss re-asserts the natives.
(def-var! "clojure.core" "add-watch" jolt-add-watch)
(def-var! "clojure.core" "remove-watch" jolt-remove-watch)
(def-var! "clojure.core" "set-validator!" jolt-set-validator!)
(def-var! "clojure.core" "get-validator" jolt-get-validator)
