;; run-staticsite.ss — a `Class/member` site is emitted with a per-site cache
;; (host-static.ss host-static-ref-site / host-static-proc-site) when the unit has
;; a const pool and the var cache is on, and as the plain string-keyed lookup
;; otherwise (the seed mint). test/chez/static-site-test.clj covers what a cached
;; site must still see.
;;
;;   chez --script host/chez/run-staticsite.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")

(define analyze (var-deref "jolt.analyzer" "analyze"))
(define emit (var-deref "jolt.backend-scheme" "emit"))
(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))

((var-deref "jolt.backend-scheme" "set-var-cache!") #t)
(let ((e (emit (anode "(def f (fn [x] (<= Long/MIN_VALUE x)))"))))
  (gate-check "a static field read is a cached site" (gate-sub? e "(host-static-ref-site ") #t)
  (gate-check "...not the per-call lookup" (gate-sub? e "(host-static-ref \"") #f))
(let ((e (emit (anode "(def g (fn [x] (Long/numberOfLeadingZeros x)))"))))
  (gate-check "a static call is a cached site" (gate-sub? e "((host-static-proc-site ") #t)
  (gate-check "...passing its argument count" (gate-sub? e "\"numberOfLeadingZeros\" 1)") #t)
  (gate-check "...not the per-call lookup" (gate-sub? e "(host-static-call \"") #f))

((var-deref "jolt.backend-scheme" "set-var-cache!") #f)
(let ((e (emit (anode "(def h (fn [x] (Long/numberOfLeadingZeros x)))"))))
  (gate-check "off var-cache a static call keeps the plain lookup" (gate-sub? e "(host-static-call \"") #t))

(gate-summary "staticsite")
