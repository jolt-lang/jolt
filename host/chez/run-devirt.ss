;; run-devirt.ss — protocol-call devirtualization gate (backend_scheme emit).
;;
;; The inference annotates a monomorphic protocol call with :devirt-type/-proto/
;; -method (jolt.passes.types); the back end then resolves the impl by that static
;; tag. This gate pins both halves: the emitted form uses find-protocol-method, and
;; evaluating it returns the same value the ordinary dispatch would — for a record's
;; inline impl, an extend-type impl, and across distinct receiver types.
;;
;;   chez --script host/chez/run-devirt.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")

(define analyze (var-deref "jolt.analyzer" "analyze"))
(define emit    (var-deref "jolt.backend-scheme" "emit"))
(define kw      (lambda (n) (keyword #f n)))

(define (evals src) (jolt-compile-eval (string-append "(do " src ")") "user"))
;; define two record types implementing one protocol — Circle via an inline impl,
;; Square via extend-type — plus instances to dispatch on.
(evals "(defprotocol Shape (area [s]))")
(evals "(defrecord Circle [r] Shape (area [s] (:r s)))")
(evals "(defrecord Square [w])")
(evals "(extend-type Square Shape (area [s] (* (:w s) (:w s))))")
(evals "(def c (->Circle 7))")
(evals "(def sq (->Square 5))")

;; analyze (area RECV), annotate it as a devirt call on `type`, and emit. RECV is a
;; var name (c/sq) the emitted code resolves at eval time.
(define (devirt-emit type recv)
  (let* ((ir (analyze (make-analyze-ctx "user") (jolt-ce-read (string-append "(area " recv ")"))))
         (dv (jolt-assoc ir (kw "devirt-type") type (kw "devirt-proto") "Shape"
                         (kw "devirt-method") "area")))
    (emit dv)))

(define (run-emit scm) (eval (read (open-input-string scm)) (interaction-environment)))

(let ((e (devirt-emit "user.Circle" "c")))
  (gate-check "emit uses devirt-resolve" (gate-sub? e "devirt-resolve") #t)
  (gate-check "devirt inline impl == dispatch" (run-emit e) (evals "(area c)")))   ; 7

(let ((e (devirt-emit "user.Square" "sq")))
  (gate-check "devirt extend-type impl == dispatch" (run-emit e) (evals "(area sq)")))  ; 25

;; a normal (no devirt) call still goes through dispatch and agrees — the path the
;; megamorphic / unknown-receiver site keeps.
(let ((e (emit (analyze (make-analyze-ctx "user") (jolt-ce-read "(area c)")))))
  (gate-check "non-devirt path no devirt-resolve" (gate-sub? e "devirt-resolve") #f)
  (gate-check "non-devirt still dispatches" (run-emit e) 7))

;; a record that relies on the protocol's Object default (no direct impl): the
;; inference still types it as a concrete record and annotates devirt, so the
;; emitted call must resolve the same value dispatch would. find-protocol-method
;; on the record's own tag misses here, so the devirt path has to fall back to
;; ordinary dispatch (else it applies #f and crashes).
(evals "(extend-protocol Shape Object (area [s] :obj-default))")
(evals "(defrecord Plain [n])")
(evals "(def pl (->Plain 9))")
(let ((e (devirt-emit "user.Plain" "pl")))
  (gate-check "devirt Object-default == dispatch" (run-emit e) (evals "(area pl)")))  ; :obj-default

;; in a direct-link build a devirt site caches the resolved impl in a per-site cell
;; (resolved once, reused) instead of resolving per call. Annotate the (area x) in a
;; def body and emit the top form; the result must carry the cell and still be right.
(let* ((set-direct-link! (var-deref "jolt.backend-scheme" "set-direct-link!"))
       (emit-top-form    (var-deref "jolt.backend-scheme" "emit-top-form"))
       (dn (analyze (make-analyze-ctx "user") (jolt-ce-read "(def usearea (fn [x] (area x)))")))
       (ar0 (jolt-nth (jolt-get (jolt-get dn (kw "init")) (kw "arities")) 0))
       (inv (jolt-get ar0 (kw "body")))
       (inv2 (jolt-assoc inv (kw "devirt-type") "user.Circle" (kw "devirt-proto") "Shape" (kw "devirt-method") "area"))
       (dn2 (jolt-assoc dn (kw "init")
                        (jolt-assoc (jolt-get dn (kw "init")) (kw "arities")
                                    (jolt-vector (jolt-assoc ar0 (kw "body") inv2))))))
  (set-direct-link! #t)
  (let ((e (emit-top-form dn2)))
    (set-direct-link! #f)
    (gate-check "devirt in a def caches in a per-site cell" (gate-sub? e "_dvc$") #t)
    (gate-check "cached cell still resolves the impl" (gate-sub? e "devirt-resolve") #t)
    ;; eval the def, then call it: caches on first call, reuses after — still 7.
    (run-emit e)
    (gate-check "cached devirt == dispatch (1st call)" (jolt-invoke (var-deref "user" "usearea") (var-deref "user" "c")) 7)
    (gate-check "cached devirt == dispatch (2nd call, from cell)" (jolt-invoke (var-deref "user" "usearea") (var-deref "user" "c")) 7)))

;; a warmed mono devirt cell must re-resolve after a runtime extend-type bumps
;; jolt-proto-epoch, mirroring the PIC: register-protocol-method bumps the epoch
;; on every extension, so a cached site that ignores it keeps serving the stale
;; impl while every other dispatch path serves the new one. The cell carries
;; (epoch . fn); a mismatch re-resolves.
(let* ((set-direct-link! (var-deref "jolt.backend-scheme" "set-direct-link!"))
       (emit-top-form    (var-deref "jolt.backend-scheme" "emit-top-form"))
       (dn (analyze (make-analyze-ctx "user") (jolt-ce-read "(def usearea2 (fn [x] (area x)))")))
       (ar0 (jolt-nth (jolt-get (jolt-get dn (kw "init")) (kw "arities")) 0))
       (inv (jolt-get ar0 (kw "body")))
       (inv2 (jolt-assoc inv (kw "devirt-type") "user.Circle" (kw "devirt-proto") "Shape" (kw "devirt-method") "area"))
       (dn2 (jolt-assoc dn (kw "init")
                        (jolt-assoc (jolt-get dn (kw "init")) (kw "arities")
                                    (jolt-vector (jolt-assoc ar0 (kw "body") inv2))))))
  (set-direct-link! #t)
  (let ((e (emit-top-form dn2)))
    (set-direct-link! #f)
    (run-emit e)
    ;; warm: first call caches Circle's original impl (:r -> 7)
    (gate-check "re-ext: warmed to original impl" (jolt-invoke (var-deref "user" "usearea2") (var-deref "user" "c")) 7)
    ;; re-extend Circle with a NEW impl; bumps jolt-proto-epoch
    (evals "(extend-type Circle Shape (area [s] (* (:r s) 100)))")
    ;; the warmed cell must re-resolve -> 700, not the stale 7
    (gate-check "re-ext: re-resolves to new impl after extend-type" (jolt-invoke (var-deref "user" "usearea2") (var-deref "user" "c")) 700)))

(gate-summary "devirt")
