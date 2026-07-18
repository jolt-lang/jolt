;; run-inline-body.ss — inline method body field-read gate.
;;
;; When `run-passes` re-infers inline method bodies with the receiver typed as the
;; record, (get _p :field) must emit jrec-field-at (bare index) instead of jolt-get.
;;
;;   chez --script host/chez/run-inline-body.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")

(define set-record-shapes! (var-deref "jolt.passes.types" "set-record-shapes!"))
(define set-protocol-methods! (var-deref "jolt.passes.types" "set-protocol-methods!"))
(define run-passes (var-deref "jolt.passes" "run-passes"))
(define emit    (var-deref "jolt.backend-scheme" "emit"))
(define analyze (var-deref "jolt.analyzer" "analyze"))
(define (evals src) (jolt-compile-eval (string-append "(do " src ")") "user"))

;; Populate runtime tables with a protocol and a defrecord with inline method impl.
(evals "(defprotocol Shape (area [s]))")
(evals "(defrecord Circle [^double r] Shape (area [_] (* r r 3.14159)))")

;; Get shapes from the populated runtime tables.
(define shapes (chez-record-shapes-map))
(define pmethods (chez-protocol-methods-map))

;; Analyze the defrecord form.  The macro expansion populates the runtime tables
;; (register-record-type!, register-inline-method!), so shapes are available.
(let* ((ir (analyze (make-analyze-ctx "user")
                    (jolt-ce-read "(defrecord Circle [^double r] Shape (area [_] (* r r 3.14159)))")))
       (_ (set-optimize! #t))
       (_ (set-record-shapes! shapes))
       (_ (set-protocol-methods! pmethods))
       (passed (run-passes ir (make-analyze-ctx "user")))
       (emitted (emit passed)))
  ;; The register-inline-method's fn body is inside the :do statements; the
  ;; reinfer pass should have seeded the receiver param so field reads emit
  ;; jrec-field-at.  Not checking jolt-get absence — the :do also contains
  ;; defs that use jolt-get for other purposes.
  (gate-check "inline method body field read uses direct accessor"
         (gate-sub? emitted "jrec1-f0") #t))

;; Also check that a deftype (non-record protocol impl) does NOT break anything.
;; deftype bodies use register-method, not register-inline-method.
(evals "(defrecord Square [s] Shape (area [_] (* s s)))")
(define shapes2 (chez-record-shapes-map))
(let* ((ir2 (analyze (make-analyze-ctx "user")
                     (jolt-ce-read "(defrecord Square [s] Shape (area [_] (* s s)))")))
       (_ (set-record-shapes! shapes2))
       (passed2 (run-passes ir2 (make-analyze-ctx "user")))
       (emitted2 (emit passed2)))
  (gate-check "deftype field read uses direct accessor"
         (gate-sub? emitted2 "jrec1-f0") #t))

;; jolt-ox7c.46: scalar-replace must not DISCARD a throwing sibling. A numeric op
;; throws on a non-numeric arg, so a map value like (+ x "throwme") whose key is
;; never read must NOT be dropped when the map binding is eliminated (that would
;; swallow the exception under --opt). The emitted body still contains the op.
(set-direct-link-flag! #t)
(let* ((ir (analyze (make-analyze-ctx "user")
                    (jolt-ce-read "(fn [x] (let [m {:a (+ x \"throwme\")}] (:b m)))")))
       (_ (set-optimize! #t))
       (passed (run-passes ir (make-analyze-ctx "user")))
       (emitted (emit passed)))
  (gate-check "throwing discarded map value survives scalar-replace"
              (gate-sub? emitted "throwme") #t))
(set-direct-link-flag! #f)

(gate-summary "inline-body")
