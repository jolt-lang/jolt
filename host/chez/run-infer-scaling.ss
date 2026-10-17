;; run-infer-scaling.ss — the optimizing passes cost polynomial time in nesting depth.
;;
;; Two shapes went exponential in 0.8.23 and every behavioural gate stayed green,
;; because each answer was right, it just took 3^depth or 2^depth work to get it:
;;
;; LOOPS. A loop var's type is the join of its init and every recur (infer-loop),
;; found by re-inferring the body until it stops widening. Each trial pass of the
;; outer loop re-ran every nested loop's own fixpoint, so a nest of loops whose
;; vars widen once cost ~3^depth: depth 8 103 ms, 9 311 ms, 10 911 ms in
;; run-inference. numeric.clj's loop-kinds has the same fixpoint, and its trial
;; passes analyzed a loop nested in a let init the same way.
;;
;; HINTED FNS. guard-declared-records wraps a ^Record-hinted arity's body as
;; (if (rec-type? p tag) body body). A hinted fn nested in a hinted fn was copied
;; into both branches of the outer test, so the emitted code doubled per level.
;;
;; Every row is a ratio taken inside this one process, depth n against depth n+3,
;; so machine speed cancels. Code size is deterministic and its rows are exact
;; counts; the time rows are judged on the fastest of interleaved samples.
;;
;;   chez --script host/chez/run-infer-scaling.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")
(jolt-push-thread-bindings
  (jolt-hash-map (jolt-var "clojure.core" "*allow-unresolved-vars*") #t))

(define analyze            (var-deref "jolt.analyzer" "analyze"))
(define run-inference      (var-deref "jolt.passes.types" "run-inference"))
(define annotate           (var-deref "jolt.passes.numeric" "annotate"))
(define set-record-shapes! (var-deref "jolt.passes.types" "set-record-shapes!"))
(define set-protocol-methods! (var-deref "jolt.passes.types" "set-protocol-methods!"))
(define run-passes         (var-deref "jolt.passes" "run-passes"))
(define emit               (var-deref "jolt.backend-scheme" "emit"))
(define U ((var-deref "jolt.passes.types" "new-unit")))
((var-deref "jolt.backend-scheme" "set-emit-unit!") U)
((var-deref "jolt.backend-scheme" "set-prelude-mode!") #t)

(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))
(define (evals src) (jolt-compile-eval (string-append "(do " src ")") "user"))

(define (now-ns)
  (let ((t (current-time 'time-monotonic)))
    (+ (* (time-second t) 1000000000) (time-nanosecond t))))
;; fastest of `rounds` samples of each thunk, sampled alternately, as a ratio
(define (ratio small big rounds)
  (small) (big)
  (let loop ((i 0) (ts +inf.0) (tb +inf.0))
    (if (= i rounds)
        (/ tb (max 1.0 ts))
        (let* ((one (lambda (f) (let ((t (now-ns))) (f) (- (now-ns) t))))
               (a (if (even? i) (one small) #f))
               (b (one big))
               (a (or a (one small))))
          (loop (+ i 1) (min ts a) (min tb b))))))
(define (report label r) (printf "  ~a: ~,2f\n" label r) r)

;; --- loops ------------------------------------------------------------------
;; Each level's var starts a long and widens once (recur adds 0.5), and the next
;; level sits in a let init, the shape of an inner accumulation inside an outer
;; iteration.
(define (loop-src d)
  (let build ((k 0))
    (if (= k d)
        "0"
        (let ((a (format "a~a" k)) (s (format "s~a" k)))
          (format "(loop [~a 0] (let [~a ~a] (if (< ~a 3) (recur (+ ~a 0.5)) ~a)))"
                  a s (build (+ k 1)) a a s)))))
(define loop-n 6)
(define loop-small (anode (string-append "(fn [] " (loop-src loop-n) ")")))
(define loop-big (anode (string-append "(fn [] " (loop-src (+ loop-n 3)) ")")))
;; linear in the node count reads ~1.5 here; the old fixpoint read 8-27
;; (2-3 passes per level, cubed)
(define loop-ceiling 3.0)
(gate-check "nested loop inference is polynomial in depth"
  (< (report "run-inference depth n+3 / n"
             (ratio (lambda () (run-inference U loop-small))
                    (lambda () (run-inference U loop-big)) 5))
     loop-ceiling)
  #t)
(gate-check "nested loop kinds are polynomial in depth"
  (< (report "numeric annotate depth n+3 / n"
             (ratio (lambda () (annotate loop-small))
                    (lambda () (annotate loop-big)) 5))
     loop-ceiling)
  #t)

;; --- nested reduce closures -------------------------------------------------
;; A fn-literal reduce's accumulator type is a fixpoint too (reduce-acc-type):
;; the closure is probed under each seed until its return stops widening, then
;; inferred once more for real. Every probe of an outer closure re-ran the
;; nested reduce's whole fixpoint, so each level doubled the work. Two shapes:
;; levels independent of each other, and each level's accumulator starting from
;; (and adding) the enclosing one's, so the inner fixpoint really does see a
;; different context on every outer probe.
(define (reduce-src d capture?)
  (let build ((k 0))
    (if (= k d)
        (if (> k 0) (format "x~a" (- k 1)) "0")
        (let ((outer (if (and capture? (> k 0)) (format "acc~a" (- k 1)) #f)))
          (format "(reduce (fn [acc~a x~a] (+ acc~a ~a~a)) ~a [1 2 3])"
                  k k k (if outer (string-append outer " ") "") (build (+ k 1))
                  (or outer "0"))))))
(define reduce-n 5)
(define (reduce-row label capture?)
  (let ((small (anode (string-append "(fn [] " (reduce-src reduce-n capture?) ")")))
        (big (anode (string-append "(fn [] " (reduce-src (+ reduce-n 3) capture?) ")"))))
    ;; linear reads ~1.6; doubling per level read 8 or more
    (gate-check label
      (< (report (string-append label ": run-inference depth n+3 / n")
                 (ratio (lambda () (run-inference U small))
                        (lambda () (run-inference U big)) 5))
         loop-ceiling)
      #t)))
(reduce-row "nested reduce closures" #f)
(reduce-row "nested reduce closures over the outer accumulator" #t)

;; --- nested ^Record-hinted fns ----------------------------------------------
(evals "(defrecord P [x y])")
(set-record-shapes! U (chez-record-shapes-map))
(set-protocol-methods! U (jolt-hash-map))
(set-optimize! #t)
;; each level reads its own param and the one enclosing it, and calls the next
;; level on its param
(define (hinted-src d)
  (let build ((k 0))
    (let ((p (format "p~a" k)))
      (if (= k (- d 1))
          (format "(fn [^P ~a] (:y ~a))" p p)
          (format "(fn [^P ~a] (+ (:x ~a)~a (~a ~a)))"
                  p p (if (> k 0) (format " (:x p~a)" (- k 1)) "") (build (+ k 1)) p)))))
(define hint-counter 0)
(define (hinted-def d)
  (set! hint-counter (+ hint-counter 1))
  (anode (format "(def hnest~a ~a)" hint-counter (hinted-src d))))
(define (hinted-emit d) (emit (run-passes (hinted-def d) (make-analyze-ctx "user") U)))
(define hint-n 6)
(define size-small (string-length (hinted-emit hint-n)))
(define size-big (string-length (hinted-emit (+ hint-n 3))))
;; Doubling per level reads 8 here. Each level now adds a guarded copy of what
;; is below it plus one plain copy, quadratic in depth: (9/6)^2 = 2.25, measured
;; ~2.2. Code size is deterministic, so this row is exact, not sampled.
(gate-check "nested hinted fns: emitted size polynomial in depth"
  (< (report "emitted size depth n+3 / n" (/ size-big (max 1.0 size-small))) 3.5)
  #t)
(gate-check "nested hinted fns: compile time polynomial in depth"
  (< (report "run-passes depth n+3 / n"
             (ratio (lambda () (hinted-emit hint-n))
                    (lambda () (hinted-emit (+ hint-n 3))) 5))
     4.5)
  #t)

(gate-summary "inferscaling")
