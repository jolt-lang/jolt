;; run-infer.ss — inference / success-type-checking gate (jolt.passes.types).
;;
;; The corpus and unit gates compile through run-passes' const-fold-only branch,
;; so the inference walk (jolt.passes.types) runs only under `jolt build --opt` —
;; buildsmoke exercises it on one trivial app and asserts stdout only. This gate
;; drives the pass DIRECTLY: analyze a source string to IR, then call the public
;; checker/driver entry points (check-form, infer-body, the set-*! registries) and
;; assert their observable output (diagnostic counts, collected calls/escapes). It
;; pins the behavior the inference walk's internal state produces, so a refactor of
;; that state is gate-validatable.
;;
;;   chez --script host/chez/run-infer.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")
;; Inference fixtures analyze fragments with intentionally-free symbols (foo,
;; bar) — this gate tests TYPE inference, not resolution. Late-bind them like
;; the analyzer's nREPL escape hatch; the corpus/unit gates stay strict.
(jolt-push-thread-bindings
  (jolt-hash-map (jolt-var "jolt.analyzer" "*allow-unresolved-vars*") #t))

(define analyze            (var-deref "jolt.analyzer" "analyze"))
(define check-form         (var-deref "jolt.passes.types" "check-form"))
(define infer-body         (var-deref "jolt.passes.types" "infer-body"))
(define reset-escapes!     (var-deref "jolt.passes.types" "reset-escapes!"))
(define collected-escapes  (var-deref "jolt.passes.types" "collected-escapes"))
(define set-record-shapes! (var-deref "jolt.passes.types" "set-record-shapes!"))
(define set-vtypes!        (var-deref "jolt.passes.types" "set-vtypes!"))
(define set-check-mode!    (var-deref "jolt.passes.types" "set-check-mode!"))
(define run-inference      (var-deref "jolt.passes.types" "run-inference"))
(define take-diags!        (var-deref "jolt.passes.types" "take-diags!"))
(define reinfer-def        (var-deref "jolt.passes.types" "reinfer-def"))

;; analyze a source string to its IR node (fresh ctx, ns "user", no passes).
(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))
;; number of success-type diagnostics check-form produces for src.
(define (diags src strict?) (jolt-count (check-form (anode src) strict?)))

;; --- core error-domain checking (strict not required) -----------------------
(gate-check "num-op on keyword"        (diags "(+ 1 :k)" #f) 1)
(gate-check "num-op all numbers"       (diags "(+ 1 2)" #f) 0)
(gate-check "count on number"          (diags "(count 5)" #f) 1)
(gate-check "count on vector"          (diags "(count [1 2])" #f) 0)
(gate-check "lenient (:k 5)"           (diags "(:k 5)" #f) 0)
(gate-check "call a number"            (diags "(5 1)" #f) 1)
(gate-check "nested count return type" (diags "(+ 1 (count :k))" #f) 1)

;; --- walk arms thread the type env ------------------------------------------
(gate-check "let binds kw"             (diags "(let [x :k] (+ x 1))" #f) 1)
(gate-check "let binds ok"             (diags "(let [x 1] (+ x 1))" #f) 0)
(gate-check "if then branch error"     (diags "(if true (+ 1 :k) 2)" #f) 1)
(gate-check "do statement error"       (diags "(do (+ 1 :k) 2)" #f) 1)
(gate-check "mapv seeds element type"  (diags "(mapv (fn [x] (+ x 1)) [:a :b])" #f) 1)
(gate-check "mapv ok element type"     (diags "(mapv (fn [x] (+ x 1)) [1 2])" #f) 0)
(gate-check "reduce seeds element"     (diags "(reduce (fn [acc x] (+ acc x)) 0 [:a])" #f) 1)

;; --- strict user-function domains (checking-box / diag-memo / user-sig) ------
(gate-check "user wrong arg type"      (diags "(do (defn f [x] (+ x 1)) (f :k))" #t) 1)
(gate-check "user wrong arity"         (diags "(do (defn g [x] x) (g 1 2))" #t) 1)
(gate-check "user call ok"             (diags "(do (defn h [x] (+ x 1)) (h 3))" #t) 0)
(gate-check "user domains off w/o strict" (diags "(do (defn f [x] (+ x 1)) (f :k))" #f) 0)
;; recursive user fn terminates (cycle guard) and still flags the bad arg
(gate-check "user recursive terminates"
       (diags "(do (defn rf [x] (+ x (rf x))) (rf :k))" #t) 1)

;; --- infer-body collects calls + escapes ------------------------------------
(reset-escapes!)
(let ((r (infer-body (anode "(do (foo 1) (bar 2) (map inc [1]))") (jolt-hash-map))))
  (gate-check "infer-body calls" (jolt-count (jolt-nth r 2)) 3)        ; foo, bar, map
  (gate-check "infer-body escapes" (jolt-count (collected-escapes)) 1)) ; inc (value position)

;; --- the record-shapes registry feeds call-result types --------------------
;; without shapes a (->P …) call result is :any (accepted); with the registry it
;; types as a struct, so an arithmetic op over it is provably not-a-number.
(gate-check "ctor result :any w/o shapes" (diags "(+ (->P 1) 1)" #f) 0)
(set-record-shapes!
  (jolt-hash-map "user/->P"
                 (jolt-hash-map (keyword #f "fields") (jolt-vector (keyword #f "x"))
                                (keyword #f "tags")   (jolt-vector jolt-nil)
                                (keyword #f "type")   "user.P")))
(gate-check "ctor result struct w/ shapes" (diags "(+ (->P 1) 1)" #f) 1)
(set-record-shapes! (jolt-hash-map))

;; --- reinfer-def honors check-mode -----------------------------------------
;; When check-mode is on and a def is reinferred with WP seeds, the diagnostics
;; must be reported (they were silently lost before the fix).
(set-check-mode! #t #f)
(let* ((node (anode "(defn f [x] (+ x :k))"))
       (ptmap (jolt-hash-map "x" (keyword #f "any"))))
  (reinfer-def node ptmap)
  (gate-check "reinfer-def check-mode reports diags" (jolt-count (take-diags!)) 1)
  (gate-check "reinfer-def diags drained after take" (jolt-count (take-diags!)) 0))
(set-check-mode! #f #f)
(let* ((node (anode "(defn f [x] (+ x :k))"))
       (ptmap (jolt-hash-map "x" (keyword #f "any"))))
  (reinfer-def node ptmap)
  (gate-check "reinfer-def no diags when check-mode off" (jolt-count (take-diags!)) 0))

;; --- the opt-path checker: run-inference emits, take-diags! drains -----------
;; (set-check-mode! on strict?) arms checking during the next run-inference; the
;; diagnostics are stashed for take-diags! to drain once.
(set-check-mode! #t #f)
(run-inference (anode "(+ 1 :k)"))
(gate-check "take-diags drains run-inference" (jolt-count (take-diags!)) 1)
(gate-check "take-diags re-drained empty"     (jolt-count (take-diags!)) 0)
(set-check-mode! #f #f)
(run-inference (anode "(+ 1 :k)"))
(gate-check "no diags when check-mode off"    (jolt-count (take-diags!)) 0)

(gate-summary "infer")
