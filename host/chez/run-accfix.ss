;; run-accfix.ss — a binding rebound through a feedback loop is not typed from its
;; initial value alone (jolt.passes.types infer-reduce-hof / reduce-acc-type).
;;
;; A reduce closure's accumulator is the init on the FIRST call only; after that
;; it is whatever the closure returned. Seeding it from the init alone proved a
;; nil init :nil, so nil?/some? folded to constants and the reduce answered with
;; the wrong element; a string init proved it :str, so string? folded true and
;; count lowered to the string primitive, which crashed on the first number; a 0.0 init pinned a ^double
;; hint on it, so a long the closure returned came back coerced. The seed is now
;; the fixpoint of the init joined with the closure's return. Pinned here on the
;; per-form inference path (predicate must survive in the emission, value must
;; match the JVM), on the whole-program path (the accumulator handed to a defn
;; must not narrow that defn's param either), on a nested reduce whose inner init
;; is the outer accumulator, and on the loop/recur analogue. The 0.0-init shape
;; whose closure always returns a flonum keeps its fl-ops.
;;
;;   chez --script host/chez/run-accfix.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")

(define analyze               (var-deref "jolt.analyzer" "analyze"))
(define set-record-shapes!    (var-deref "jolt.passes.types" "set-record-shapes!"))
(define set-protocol-methods! (var-deref "jolt.passes.types" "set-protocol-methods!"))
(define wp-infer!             (var-deref "jolt.passes.types" "wp-infer!"))
(define run-passes            (var-deref "jolt.passes" "run-passes"))
(define emit                  (var-deref "jolt.backend-scheme" "emit"))

(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))
(define (ev src) (jolt-compile-eval src "user"))
;; an evaluation that raises reports as a row failure, not as the gate dying —
;; the string-init shape crashed inside Chez's string-length before the fix
(define (ev-safe src)
  (guard (e (#t (list 'raised (condition-message-or-object e))))
    (ev src)))
(define (exact-safe src)
  (let ((r (ev-safe src))) (if (and (pair? r) (eq? (car r) 'raised)) r (jnum->exact r))))
(define (condition-message-or-object e)
  (if (message-condition? e) (condition-message e) e))
;; analyze + run-passes (optimize on, per-form inference) + emit
(define (emitf src)
  (let ((ctx (make-analyze-ctx "user")))
    (jolt-ce-emit (jolt-ce-run-passes (jolt-ce-analyze ctx (jolt-ce-read src)) ctx))))

(set-optimize! #t)

;; --- per-form: the predicate stays a runtime test, the value is the JVM's -----
(define min-src "(reduce (fn [b x] (if (or (nil? b) (< x b)) x b)) nil [5 3 9 1 7])")
(gate-check "nil init: nil? on the accumulator is not folded"
            (gate-sub? (emitf min-src) "jolt-nil?") #t)
(gate-check "nil init: the minimum, not the last rival" (exact-safe min-src) 1)

(define some-src "(reduce (fn [acc x] (if (some? acc) (conj acc x) [x])) nil [1 2 3])")
(gate-check "nil init: some? on the accumulator is not folded"
            (gate-sub? (emitf some-src) "jolt-some?") #t)
(gate-check "nil init: every element conj'd" (let ((r (ev-safe some-src))) (if (pair? r) r (jolt-count r))) 3)

(define str-src "(reduce (fn [acc x] (if (string? acc) (count acc) (+ acc x))) \"abc\" [1 2 3])")
;; (the emission's registration preamble replays the SOURCE, so a bare
;; `string?` is a vacuous substring; the emitted callee reference is not.) The
;; accumulator is not proven a string, so the test stays a runtime call. Inside
;; its then-branch the test DOES prove it (type-test narrowing), so count there
;; lowering to jolt-str-count is right, not a sign the test folded.
(gate-check "string init: string? on the accumulator stays a runtime test"
            (gate-sub? (emitf str-src) "(jolt-var \"clojure.core\" \"string?\")") #t)
(gate-check "string init: count in the string? branch lowers to the string primitive"
            (gate-sub? (emitf str-src) "jolt-str-count") #t)
(gate-check "string init: 3+2+3 = 8" (exact-safe str-src) 8)

;; a 0.0 init whose closure sometimes returns a long: no ^double hint on acc, so
;; the long is handed on as a long — (+ 0 2) is 2, not 2.0
(define mixed-src "(reduce (fn [acc x] (if (pos? x) (+ acc x) 0)) 0.0 [1.0 -1.0 2])")
(let ((r (ev-safe mixed-src)))
  (gate-check "0.0 init with a long return: the JVM's 2" (if (pair? r) r (jnum->exact r)) 2)
  (gate-check "0.0 init with a long return: a fixnum, not a coerced flonum" (fixnum? r) #t))

;; a 0.0 init whose closure always returns a flonum converges on :double at the
;; first probe and keeps its unboxed arithmetic
(define dbl-src "(reduce (fn [acc x] (+ acc (* x x))) 0.0 [1.0 2.0 3.0])")
(gate-check "0.0 init, flonum closure: fl+ kept" (gate-sub? (emitf dbl-src) "fl+") #t)
(gate-check "0.0 init, flonum closure: 14.0" (ev-safe dbl-src) 14.0)

;; nested: the inner reduce's init is the outer accumulator
(define nested-src
  "(reduce (fn [b xs] (reduce (fn [b x] (if (or (nil? b) (< x b)) x b)) b xs)) nil [[5 3] [9 1 7]])")
(gate-check "nested reduce: inner nil? not folded" (gate-sub? (emitf nested-src) "jolt-nil?") #t)
(gate-check "nested reduce: the minimum" (exact-safe nested-src) 1)

;; the loop analogue: a nil-initialized loop var rebound by recur (loop vars are
;; typed :any by design — this pins that the fold never reaches them)
(define loop-src
  "(loop [b nil xs [5 3 9 1 7]] (if (empty? xs) b (recur (if (or (nil? b) (< (first xs) b)) (first xs) b) (rest xs))))")
(gate-check "loop/recur: nil? on the loop var is not folded" (gate-sub? (emitf loop-src) "jolt-nil?") #t)
(gate-check "loop/recur: the minimum" (exact-safe loop-src) 1)

;; --- whole-program: the accumulator handed to a defn --------------------------
;; The closure calls pick with the accumulator, so the fixpoint types pick's b
;; from that call site. It must see the converged accumulator, not the nil init
;; — else (nil? b) folds inside pick. The probes that converge the seed run under
;; their own calls cell, so the narrow types never reach the param joins.
(define U ((var-deref "jolt.passes.types" "new-unit")))
((var-deref "jolt.backend-scheme" "set-emit-unit!") U)
(define pick (anode "(def pick (fn [b x] (if (or (nil? b) (< x b)) x b)))"))
(define usepick (anode "(def usepick (fn [] (reduce (fn [b x] (pick b x)) nil [5 3 9 1 7])))"))
(set-record-shapes! U (jolt-hash-map))
(set-protocol-methods! U (jolt-hash-map))
(wp-infer! U (jolt-vector pick usepick))
(define pick-emit (emit (run-passes pick (make-analyze-ctx "user") U)))
(gate-check "whole-program: nil? in the callee is not folded" (gate-sub? pick-emit "jolt-nil?") #t)
(ev "(def pick (fn [b x] (if (or (nil? b) (< x b)) x b)))")
(gate-check "whole-program: the minimum through the callee"
            (exact-safe "(reduce (fn [b x] (pick b x)) nil [5 3 9 1 7])") 1)

(set-optimize! #f)
(gate-summary "accfix")
