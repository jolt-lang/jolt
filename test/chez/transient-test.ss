;; Transient regression: mutable backing + snapshot-on-persist (jolt-kl2l). Run:
;;   chez --script test/chez/transient-test.ss
;; Semantics are covered broadly by the corpus; this pins the invariants the
;; mutable port must keep AND that large builds stay linear (a copy-on-write
;; regression would make the 200k builds quadratic and time the gate out).

(import (chezscheme))
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

(define total 0) (define fails 0)
(define (ok name pred) (set! total (+ total 1)) (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
(define (ev s) (jolt-final-str (jolt-compile-eval (string-append "(do " s ")") "user")))
(define (is name s expect) (ok (string-append name " => " expect) (string=? (ev s) expect)))

;; --- mutation is in place; persistent! snapshots back -----------------------
(is "vector build" "(persistent! (reduce conj! (transient []) (range 5)))" "[0 1 2 3 4]")
(is "map build"    "(= {0 0 1 1 2 2} (persistent! (reduce (fn [t i] (assoc! t i i)) (transient {}) (range 3))))" "true")
(is "set build"    "(count (persistent! (reduce conj! (transient #{}) [1 2 2 3])))" "3")
(is "pop!"         "(persistent! (pop! (conj! (transient [1 2]) 3)))" "[1 2]")
(is "dissoc!"      "(persistent! (dissoc! (assoc! (transient {}) :a 1 :b 2) :a))" "{:b 2}")
(is "disj!"        "(persistent! (disj! (conj! (transient #{}) :x :y) :x))" "#{:y}")

;; --- a transient never mutates its source -----------------------------------
(is "source map unchanged"    "(let [m {:a 1} _ (persistent! (assoc! (transient m) :b 2))] (= m {:a 1}))" "true")
(is "source vector unchanged" "(let [v [1 2] _ (persistent! (conj! (transient v) 3))] (= v [1 2]))" "true")

;; --- edges the port must keep -----------------------------------------------
(is "nil key"            "(get (persistent! (assoc! (transient {}) nil :v)) nil)" ":v")
(is "collection key"     "(get (persistent! (assoc! (transient {}) [1 2] :v)) [1 2])" ":v")
(is "dangling key pads"  "(= {:a 1 :b nil} (persistent! (assoc! (transient {}) :a 1 :b)))" "true")
(is "vector? is false"   "(vector? (transient []))" "false")
(is "transient sorted (cow)" "(persistent! (assoc! (transient (sorted-map :b 2)) :a 1))" "{:a 1, :b 2}")
(ok "lone key throws"        (guard (e (#t #t)) (ev "(persistent! (assoc! (transient {}) :a))") #f))
(ok "use after persistent!"  (guard (e (#t #t)) (ev "(let [t (transient [])] (persistent! t) (conj! t 1))") #f))

;; --- linear, not quadratic: 200k builds finish near-instantly ---------------
(is "big vector build"  "(count (persistent! (reduce conj! (transient []) (range 200000))))" "200000")
(is "big map build"     "(count (persistent! (reduce (fn [t i] (assoc! t i i)) (transient {}) (range 200000))))" "200000")

(printf "~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
