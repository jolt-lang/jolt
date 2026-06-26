;; run-numwp.ss — hintless whole-program :double inference gate (jolt-evr9 R3).
;;
;; run-wp.ss drives the structural (record) fixpoint; this drives its numeric
;; refinement: a hintless fn whose every call site passes a flonum has its param
;; typed :double, which the back end then unboxes to fl-ops — no ^double hint. The
;; bridge is a synthetic [param :double] nhint (jolt.passes/inject-wp-nhints) that
;; the existing hint-directed pass + entry coercion consume unchanged.
;;
;; Soundness pinned here: :double only (never :long — an untyped integer can be a
;; bignum), so a caller passing an integer leaves the param generic; an escaped fn
;; keeps :any.
;;
;;   chez --script host/chez/run-numwp.ss
(import (chezscheme))
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

(define analyze              (var-deref "jolt.analyzer" "analyze"))
(define set-record-shapes!   (var-deref "jolt.passes.types" "set-record-shapes!"))
(define set-protocol-methods! (var-deref "jolt.passes.types" "set-protocol-methods!"))
(define wp-infer!            (var-deref "jolt.passes.types" "wp-infer!"))
(define param-num-seeds-for  (var-deref "jolt.passes.types" "param-num-seeds-for"))
(define inject-wp-nhints     (var-deref "jolt.passes" "inject-wp-nhints"))
(define annotate             (var-deref "jolt.passes.numeric" "annotate"))
(define run-passes           (var-deref "jolt.passes" "run-passes"))
(define emit                 (var-deref "jolt.backend-scheme" "emit"))
(define pr-str               (var-deref "clojure.core" "pr-str"))

(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))
(define (contains-sub? s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) sub) #t)
            (else (loop (+ i 1)))))))

(define fails 0) (define total 0)
(define (check label actual expected)
  (set! total (+ total 1))
  (unless (equal? actual expected)
    (set! fails (+ fails 1))
    (printf "  FAIL ~a: got ~s expected ~s\n" label actual expected)))

(set-record-shapes! (jolt-hash-map))
(set-protocol-methods! (jolt-hash-map))

;; sq is hintless; its only caller passes a flonum literal, so the fixpoint must
;; type x :double across the fn boundary.
(define sq   (anode "(def sq (fn [x] (* x x)))"))
(define usef (anode "(def usef (fn [] (sq 2.0)))"))
(wp-infer! (jolt-vector sq usef))

(define nseed (param-num-seeds-for "user/sq"))
(check "sq has a numeric param seed" (jolt-truthy? nseed) #t)
(when (jolt-truthy? nseed)
  (check "x seeded :double" (contains-sub? (pr-str nseed) ":double") #t))

;; the bridge: inject the derived nhint, run the numeric pass, emit -> fl*.
(define sq-opt (annotate (inject-wp-nhints sq)))
(check "sq body unboxes to fl*" (contains-sub? (emit sq-opt) "fl*") #t)
;; and the param is coerced at entry like a ^double param (no-op on a real flonum).
(check "sq coerces param at entry" (contains-sub? (emit sq-opt) "exact->inexact") #t)

;; a caller passing an INTEGER must NOT make the param :double — an untyped integer
;; can be a bignum, so fl-ops would diverge. The param stays generic.
(define sqi  (anode "(def sqi (fn [x] (* x x)))"))
(define usei (anode "(def usei (fn [] (sqi 2)))"))
(wp-infer! (jolt-vector sqi usei))
(check "integer caller leaves param generic"
       (jolt-truthy? (param-num-seeds-for "user/sqi")) #f)

;; a fn used in value position (escapes) has unknown callers -> no double seed.
(define esc  (anode "(def esc (fn [x] (* x x)))"))
(define hof  (anode "(def hof (fn [g] (g 2.0)))"))
(define ecl  (anode "(def ecaller (fn [] (hof esc)))"))   ; esc escapes
(wp-infer! (jolt-vector esc hof ecl))
(check "escaped fn keeps no double seed"
       (jolt-truthy? (param-num-seeds-for "user/esc")) #f)

;; :double flows through a returning helper: mag returns a flonum, so a param fed
;; only (mag _) results types :double too (cross-fn return propagation).
(define mag  (anode "(def mag (fn [a] (* a 2.0)))"))
(define dist (anode "(def dist (fn [b] (+ b b)))"))
(define dcl  (anode "(def dcaller (fn [] (dist (mag 3.0))))"))
(wp-infer! (jolt-vector mag dist dcl))
(check "param fed a flonum-returning call types :double"
       (jolt-truthy? (param-num-seeds-for "user/dist")) #t)

;; end to end through the real build pipeline: with optimize on, run-passes wires
;; the WP fixpoint's :double seeds into the numeric pass (inject-wp-nhints) so the
;; emitted def unboxes — proves the production path fires, not just the bridge in
;; isolation.
(set-optimize! #t)
(define sq2 (anode "(def sq2 (fn [x] (* x x)))"))
(define use2 (anode "(def use2 (fn [] (sq2 4.0)))"))
(wp-infer! (jolt-vector sq2 use2))
(check "run-passes unboxes a hintless double fn"
       (contains-sub? (emit (run-passes sq2 (make-analyze-ctx "user"))) "fl*") #t)

(if (= fails 0)
    (begin (printf "numwp gate: ~a/~a passed\n" total total) (exit 0))
    (begin (printf "numwp gate: ~a/~a passed (~a failed)\n" (- total fails) total fails) (exit 1)))
