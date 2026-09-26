;; run-defmetacells.ss — a var referenced from a def's evaluated metadata goes
;; through a cache cell, as one referenced from the def's init does. Every deftest
;; body is the :test fn in its def's metadata, and without the cell scope each var
;; it named was resolved by name on every call (66 ns/iter against 8 in a defn).
;;
;;   chez --script host/chez/run-defmetacells.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")

(define analyze (var-deref "jolt.analyzer" "analyze"))
(define emit (var-deref "jolt.backend-scheme" "emit"))
((var-deref "jolt.backend-scheme" "set-var-cache!") #t)
(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))

(let ((e (emit (anode "(def ^{:t (fn [n] (clojure.core/str n))} x (fn [n] (clojure.core/str n)))"))))
  (gate-check "the init's var ref is cell-cached" (gate-sub? e "var-cell-deref") #t)
  (gate-check "the metadata fn's var ref is not resolved by name per call"
              (gate-sub? e "(var-deref \"clojure.core\" \"str\")") #f))

;; the same through deftest, which is where the cost showed
(let ((e (emit (anode "(def ^{:test (fn [] (loop [i 0] (when (< i 3) (clojure.core/str i) (recur (inc i)))))} t (fn [] nil))"))))
  (gate-check "a deftest-shaped :test body's var ref is not resolved by name per call"
              (gate-sub? e "(var-deref \"clojure.core\" \"str\")") #f))

;; off the var cache (the seed mint) the emission is unchanged
((var-deref "jolt.backend-scheme" "set-var-cache!") #f)
(let ((e (emit (anode "(def ^{:t (fn [n] (clojure.core/str n))} x 1)"))))
  (gate-check "off var-cache the metadata fn keeps the by-name deref"
              (gate-sub? e "(var-deref \"clojure.core\" \"str\")") #t))

(gate-summary "defmetacells")
