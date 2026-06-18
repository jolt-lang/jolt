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

(define-record-type jolt-atom (fields (mutable val)) (nongenerative jolt-atom-v1))

;; (atom init) — extra :meta/:validator opts are accepted and ignored for now
;; (watches/validators are overlay features layered via jolt.host/ref-put!).
(define (jolt-atom-new v . _opts) (make-jolt-atom v))

;; deref reads an atom; it also unwraps a `reduced` (Clojure @(reduced x) => x,
;; which the overlay's `unreduced` relies on). The reduced record is in seq.ss.
(define (jolt-deref x)
  (cond
    ((jolt-atom? x) (jolt-atom-val x))
    ((jolt-reduced? x) (jolt-reduced-val x))
    (else (error #f "deref: unsupported reference type" x))))

;; (swap! a f arg*) -> (reset! a (f @a arg*)); f is invoked through jolt-invoke
;; (a jolt fn value, keyword, or invokable collection).
(define (jolt-swap! a f . args)
  (let ((nv (apply jolt-invoke f (jolt-atom-val a) args)))
    (jolt-atom-val-set! a nv)
    nv))

(define (jolt-reset! a v) (jolt-atom-val-set! a v) v)

(define (jolt-compare-and-set! a oldv newv)
  (if (jolt= (jolt-atom-val a) oldv)
      (begin (jolt-atom-val-set! a newv) #t)
      #f))

(define (jolt-swap-vals! a f . args)
  (let* ((old (jolt-atom-val a))
         (nv (apply jolt-invoke f old args)))
    (jolt-atom-val-set! a nv)
    (jolt-vector old nv)))

(define (jolt-reset-vals! a v)
  (let ((old (jolt-atom-val a)))
    (jolt-atom-val-set! a v)
    (jolt-vector old v)))

(def-var! "clojure.core" "atom" jolt-atom-new)
(def-var! "clojure.core" "deref" jolt-deref)
(def-var! "clojure.core" "swap!" jolt-swap!)
(def-var! "clojure.core" "reset!" jolt-reset!)
(def-var! "clojure.core" "compare-and-set!" jolt-compare-and-set!)
(def-var! "clojure.core" "swap-vals!" jolt-swap-vals!)
(def-var! "clojure.core" "reset-vals!" jolt-reset-vals!)
(def-var! "clojure.core" "atom?" jolt-atom?)
