;; Phase 1 (jolt-cf1q.2) — the minimal Chez RT the emitted Scheme rests on.
;;
;; Sits above the value model (values.ss) and below an emitted program. Adds the
;; two things the back end's output references that aren't in the value layer:
;;   1. the var-cell late-binding registry (Clojure vars — a global root that a
;;      reference reads at call time, so redefinition / mutual recursion work);
;;   2. the rt primitive shims the emitter names (jolt-inc/dec/not) and jolt's
;;      number printing (all jolt numbers model Clojure doubles; integer-valued
;;      print without a trailing ".0", matching the Janet host).
;;
;; Emitted programs do `(load "host/chez/rt.ss")`; this loads values.ss in turn.

(load "host/chez/values.ss")

;; --- rt arithmetic / logic shims (named in emit.janet's native-ops) ----------
(define (jolt-inc x) (+ x 1))
(define (jolt-dec x) (- x 1))
;; jolt `not`: only nil and false are falsey.
(define (jolt-not x) (if (jolt-truthy? x) #f #t))

;; --- var cells: late-bound global roots (Clojure vars) -----------------------
;; A var is a mutable cell keyed by "ns/name". A `:def` sets the root; a `:var`
;; reference reads it at use time (late binding), so a forward/mutually-recursive
;; reference resolves to whatever the cell holds when the call actually runs.
(define-record-type var-cell (fields ns name (mutable root)) (nongenerative var-cell-v1))
(define var-table (make-hashtable string-hash string=?))
(define (jolt-var ns name)
  (let ((k (string-append ns "/" name)))
    (or (hashtable-ref var-table k #f)
        (let ((c (make-var-cell ns name jolt-nil)))
          (hashtable-set! var-table k c)
          c))))
(define (var-deref ns name) (var-cell-root (jolt-var ns name)))
(define (def-var! ns name v) (var-cell-root-set! (jolt-var ns name) v) v)

;; --- jolt number printing ----------------------------------------------------
;; jolt models every number as a Clojure double: integer-valued values print
;; without a ".0" (the Janet host prints (* 1.0 5) as "5", (/ 1 2) as "0.5").
(define (jolt-num->string x)
  (if (and (rational? x) (integer? x))
      (number->string (exact x))
      (number->string x)))

;; Minimal pr-str for the program's final value (full printer is Phase 2).
(define (jolt-pr-str x)
  (cond
    ((jolt-nil? x) "nil")
    ((eq? x #t) "true")
    ((eq? x #f) "false")
    ((number? x) (jolt-num->string x))
    ((string? x) x)
    (else (format "~a" x))))
