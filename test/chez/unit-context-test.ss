;; unit-context-test.ss — the compilation-unit context (.52 Phase B).
;;
;; The emit-session state (mode flags, direct-link registries, ctor shapes, gensym
;; counter, cache cells) lives on the jolt.passes.types unit, read at emit through the
;; back-end emit-unit pointer. This gate pins the property the fold must preserve:
;; the state is PER-UNIT, so two units are isolated (reentrant) and a flag set under
;; one unit does not leak into another. It drives the real emit path (emit-form) and
;; compares the emitted Scheme, so a regression that dropped a per-unit read (reading
;; a stale/other unit's flag) fails here.
;;
;;   chez --script test/chez/unit-context-test.ss
(import (chezscheme))
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")
(load "host/chez/emit-image.ss")

(define total 0) (define fails 0)
(define (ok name pred) (set! total (+ total 1)) (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
(define (contains? s sub)
  (let ((ns (string-length s)) (nsub (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i nsub) ns) #f)
            ((string=? (substring s i (+ i nsub)) sub) #t)
            (else (loop (+ i 1)))))))

(define new-unit        (var-deref "jolt.passes.types" "new-unit"))
(define set-emit-unit!  (var-deref "jolt.backend-scheme" "set-emit-unit!"))
(define set-direct-link! (var-deref "jolt.backend-scheme" "set-direct-link!"))
(define direct-link-reset! (var-deref "jolt.backend-scheme" "direct-link-reset!"))

;; analyze+emit one form (string) in a namespace through the real build entry.
(define (emit-form ns-name str)
  (let-values (((f j) (rdr-read-form str 0 (string-length str))))
    (ei-compile-form (make-analyze-ctx ns-name) f #f)))

;; register app/a as a var so a reference classifies as :var.
(set-direct-link! #f)
(jolt-compile-eval "(def a (fn* ([] 1)))" "app")

;; two independent units.
(define U1 (new-unit))
(define U2 (new-unit))

;; --- U1: direct-link ON -----------------------------------------------------
(set-emit-unit! U1)
(direct-link-reset!)
(set-direct-link! #t)
(let ((e (emit-form "app" "(def a (fn* ([] 1)))")))
  (ok "U1 direct-link on: def emits a jv$ binding" (contains? e "(define jv$app$a ")))

;; --- U2: fresh unit, direct-link defaults OFF -> no jv$ (isolation) ----------
(set-emit-unit! U2)
(let ((e (emit-form "app" "(def a (fn* ([] 1)))")))
  (ok "U2 fresh unit: direct-link defaults off (no jv$ binding)"
      (not (contains? e "(define jv$app$a "))))

;; --- back to U1: its direct-link state is intact (reentrancy) ----------------
(set-emit-unit! U1)
(let ((e (emit-form "app" "(def a (fn* ([] 1)))")))
  (ok "U1 again: per-unit direct-link state preserved (jv$ binding back)"
      (contains? e "(define jv$app$a ")))

;; --- turning U1 off does not affect a value we already captured on U2 --------
(set-direct-link! #f)
(let ((e1 (emit-form "app" "(def a (fn* ([] 1)))")))
  (ok "U1 direct-link off again: no jv$ binding" (not (contains? e1 "(define jv$app$a "))))

(set-emit-unit! #f)
(printf "~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
