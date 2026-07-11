;; run-narrow.ss — nilable record types + flow-sensitive some?/nil? narrowing.
;;
;; A protocol method (or `if`) returning a record-or-nil types as a NILABLE record:
;; some?/nil? do NOT fold on it (it might be nil), so a runtime guard stays. Inside
;; (if (some? x) ..) / (if x ..) the then-branch narrows x to the non-nil record, so
;; its field reads bare-index and unbox. This is the ray tracer's
;; (let [scattered (scatter ..)] (if (some? scattered) (.. (:ray scattered) ..))).
;;
;; The load-bearing soundness check: the nil case must still take the else branch —
;; narrowing must NOT fold the guard away (else a real nil reaches the bare read).
;;
;;   chez --script host/chez/run-narrow.ss
(import (chezscheme))
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

(define analyze (var-deref "jolt.analyzer" "analyze"))
(define set-record-shapes! (var-deref "jolt.passes.types" "set-record-shapes!"))
(define set-protocol-methods! (var-deref "jolt.passes.types" "set-protocol-methods!"))
(define wp-infer! (var-deref "jolt.passes.types" "wp-infer!"))
(define run-passes (var-deref "jolt.passes" "run-passes"))
(define emit (var-deref "jolt.backend-scheme" "emit"))
(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))
(define (evals src) (jolt-compile-eval (string-append "(do " src ")") "user"))
(define (built scm) (eval (read (open-input-string scm)) (interaction-environment)))
(define (sub? s t)(let((n(string-length s))(m(string-length t)))(let loop((i 0))(cond((>(+ i m)n)#f)((string=?(substring s i(+ i m))t)#t)(else(loop(+ i 1)))))))
(define fails 0) (define total 0)
(define (check label actual expected)
  (set! total (+ total 1))
  (unless (equal? actual expected)
    (set! fails (+ fails 1))
    (printf "  FAIL ~a: got ~s expected ~s\n" label actual expected)))

(evals "(defrecord R [^double k])")
(evals "(defprotocol P (m [x]))")
(evals "(defrecord A [v] P (m [x] (->R 1.0)))")
(evals "(defrecord B [v] P (m [x] (if (< (:v x) 0) (->R 2.0) nil)))")  ; B.m returns R-or-nil
(set-record-shapes! (chez-record-shapes-map))
(set-protocol-methods! (chez-protocol-methods-map))
(set-optimize! #t)
(define na (anode "(defrecord A [v] P (m [x] (->R 1.0)))"))
(define nb (anode "(defrecord B [v] P (m [x] (if (< (:v x) 0) (->R 2.0) nil)))"))
;; guarded read: inside (some? s), s narrows to non-nil R -> (:k s) bare-indexes + unboxes
(define f (anode "(def f (fn [a] (let [s (m a)] (if (some? s) (* (:k s) 2.0) 0.0))))"))
(wp-infer! (jolt-vector na nb f))
(define fe (emit (run-passes f (make-analyze-ctx "user"))))
(check "guarded nullable read direct-accesses" (sub? fe "jrec1-f0") #t)
(check "guarded nullable read unboxes to fl*" (sub? fe "fl*") #t)

;; CORRECTNESS + the load-bearing soundness check: the nil case must take the else
;; branch (the guard is preserved), not run the bare read on nil.
(built fe)
(define ff (var-deref "user" "f"))
(check "non-nil (A.m -> R 1.0)"      (jolt-invoke ff (evals "(->A 5)"))  2.0)
(check "non-nil (B.m v<0 -> R 2.0)"  (jolt-invoke ff (evals "(->B -5)")) 4.0)
(check "nil case takes else (guard preserved, no crash)"
       (jolt-invoke ff (evals "(->B 5)")) 0.0)

;; an UNGUARDED nullable read must stay safe: jrec-field-at falls back to jolt-get on
;; nil. (Its result type is conservative — no unbox — so this just checks no crash.)
(define g (anode "(def g (fn [a] (let [s (m a)] (:k s))))"))
(define ge (emit (run-passes g (make-analyze-ctx "user"))))
;; an UNGUARDED nullable read must NOT take the direct (nil-unsafe) accessor — it
;; keeps the nil-safe jrec-field-at path, so a nil receiver returns nil instead of
;; crashing a bare slot load.
(check "unguarded nilable read stays nil-safe (no direct accessor)" (sub? ge "jrec1-f0") #f)
(built ge)
(define gg (var-deref "user" "g"))
(check "unguarded nullable read on nil returns nil" (jolt-nil? (jolt-invoke gg (evals "(->B 5)"))) #t)
(check "unguarded nullable read on non-nil returns the field" (jolt-invoke gg (evals "(->A 5)")) 1.0)

(if (= fails 0)
    (begin (printf "narrow gate: ~a/~a passed\n" total total) (exit 0))
    (begin (printf "narrow gate: ~a/~a passed (~a failed)\n" (- total fails) total fails) (exit 1)))
