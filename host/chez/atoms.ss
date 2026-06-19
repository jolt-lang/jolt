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
(define-record-type jolt-atom
  (fields (mutable val) (mutable watches) (mutable validator))
  (nongenerative jolt-atom-v2))

;; (atom init) / (atom init :validator f :meta m): scan the trailing keyword opts
;; for :validator (the only one with runtime behaviour; :meta is accepted/ignored).
(define (jolt-atom-new v . opts)
  (let loop ((o opts) (validator jolt-nil))
    (cond
      ((or (null? o) (null? (cdr o))) (make-jolt-atom v '() validator))
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

;; (swap! a f arg*) -> (reset! a (f @a arg*)); f is invoked through jolt-invoke.
;; Validate the new value BEFORE storing, notify watches AFTER (the seed order).
(define (jolt-swap! a f . args)
  (let* ((old (jolt-atom-val a))
         (nv (apply jolt-invoke f old args)))
    (jolt-atom-validate a nv)
    (jolt-atom-val-set! a nv)
    (jolt-atom-notify a old nv)
    nv))

(define (jolt-reset! a v)
  (let ((old (jolt-atom-val a)))
    (jolt-atom-validate a v)
    (jolt-atom-val-set! a v)
    (jolt-atom-notify a old v)
    v))

(define (jolt-compare-and-set! a oldv newv)
  (if (jolt= (jolt-atom-val a) oldv)
      (begin (jolt-reset! a newv) #t)
      #f))

(define (jolt-swap-vals! a f . args)
  (let* ((old (jolt-atom-val a))
         (nv (apply jolt-invoke f old args)))
    (jolt-reset! a nv)
    (jolt-vector old nv)))

(define (jolt-reset-vals! a v)
  (let ((old (jolt-atom-val a)))
    (jolt-reset! a v)
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
