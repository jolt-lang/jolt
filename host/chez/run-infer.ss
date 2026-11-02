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
  (jolt-hash-map (jolt-var "clojure.core" "*allow-unresolved-vars*") #t))

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
;; one compilation-unit context threaded through the pass API (was module-global state).
(define U ((var-deref "jolt.passes.types" "new-unit")))

;; analyze a source string to its IR node (fresh ctx, ns "user", no passes).
(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))
(define kw (lambda (n) (keyword #f n)))
;; number of success-type diagnostics check-form produces for src.
(define (diags src strict?) (jolt-count (check-form U (anode src) strict?)))
;; argument count of the :invoke node run-inference hands back — a pattern helper
;; that rebuilds :args must reproduce every argument it was given.
(define (inferred-argc src)
  (jolt-count (jolt-get (run-inference U (anode src)) (keyword #f "args"))))

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

;; --- the HOF/lookup pattern helpers preserve every argument ------------------
;; Each helper rebuilds :args from the operands it inferred. map/mapv/mapcat take
;; one collection PER fn parameter, so a helper that rebuilds a fixed [f coll]
;; pair silently drops the extra collections: the emitted (jolt-map f c1) then
;; hands the 2-ary closure one element and Chez raises an arity error. These rows
;; run the same walk `jolt build` does in release/--opt mode (the corpus and unit
;; gates compile through the const-fold-only branch and never reach it).
(gate-check "map keeps one coll"       (inferred-argc "(map (fn [x] x) [1 2])") 2)
(gate-check "map keeps two colls"      (inferred-argc "(map (fn [x y] x) [1 2] [3 4])") 3)
(gate-check "map keeps three colls"    (inferred-argc "(map (fn [x y z] x) [1] [2] [3])") 4)
(gate-check "mapv keeps two colls"     (inferred-argc "(mapv (fn [x y] x) [1 2] [3 4])") 3)
(gate-check "mapcat keeps two colls"   (inferred-argc "(mapcat (fn [x y] [x]) [1 2] [3 4])") 3)
;; over-arity calls to the fixed-shape helpers fall through to the generic call
;; path rather than being silently truncated to the shape the helper knows.
(gate-check "reduce keeps init + coll" (inferred-argc "(reduce (fn [a x] a) 0 [1 2])") 3)
(gate-check "over-arity reduce intact" (inferred-argc "(reduce (fn [a x] a) 0 [1 2] [3 4])") 4)
(gate-check "get keeps default"        (inferred-argc "(get {:k 1} :k 0)") 3)
(gate-check "over-arity get intact"    (inferred-argc "(get {:k 1} :k 0 :extra)") 4)

;; --- strict user-function domains (checking-box / diag-memo / user-sig) ------
(gate-check "user wrong arg type"      (diags "(do (defn f [x] (+ x 1)) (f :k))" #t) 1)
(gate-check "user wrong arity"         (diags "(do (defn g [x] x) (g 1 2))" #t) 1)
(gate-check "user call ok"             (diags "(do (defn h [x] (+ x 1)) (h 3))" #t) 0)
(gate-check "user domains off w/o strict" (diags "(do (defn f [x] (+ x 1)) (f :k))" #f) 0)
;; recursive user fn terminates (cycle guard) and still flags the bad arg
(gate-check "user recursive terminates"
       (diags "(do (defn rf [x] (+ x (rf x))) (rf :k))" #t) 1)

;; --- infer-body collects calls + escapes ------------------------------------
(reset-escapes! U)
(let ((r (infer-body U (anode "(do (foo 1) (bar 2) (map inc [1]))") (jolt-hash-map))))
  (gate-check "infer-body calls" (jolt-count (jolt-nth r 2)) 3)        ; foo, bar, map
  (gate-check "infer-body escapes" (jolt-count (collected-escapes U)) 1)) ; inc (value position)

;; --- the walk descends through EVERY node, including ops it has no arm for --
;; `infer`'s per-op cond ends in a fallback. Answering :any there is right — the
;; pass has no opinion about such a node's type — but the fallback must still walk
;; the node's children, or nothing inside is annotated and every record read and
;; numeric op in the subtree quietly falls back to the generic path.
;;
;; That is not hypothetical. The inline pass wraps a spliced body in :coerce to
;; carry a callee's ^double/^long return coercion, :coerce had no arm, and so every
;; inlined copy of a return-hinted fn came out untyped: its field reads emitted
;; jolt-get instead of a slot read and its arithmetic the generic ops, which made
;; declaring a return type roughly halve the speed of the fn's inlined copies.
;;
;; Collected calls are the probe: a call inside the node is seen only if the walk
;; reached it. One case for :coerce (which has an arm now) and one for an op that
;; has none and carries a child, so the fallback itself is pinned.
;; :coerce has an arm of its own, so what it must be pinned on is the TYPE it
;; answers, not the walk — the fallback below would walk it either way. The arm is
;; what lets a spliced ^double body's RESULT type as a double for the code around
;; it rather than degrading to :any.
(let ((coerce-node (var-deref "jolt.ir" "coerce-node")))
  (reset-escapes! U)
  (let ((r (infer-body U (coerce-node (kw "double") (anode "(foo 1)")) (jolt-hash-map))))
    (gate-check "infer walks through :coerce" (jolt-count (jolt-nth r 2)) 1)
    (gate-check "a :coerce :double types as :double" (jolt-nth r 0) (kw "double"))))
(reset-escapes! U)
(let* ((unhandled (jolt-hash-map (kw "op") (kw "set-var")
                                 (kw "the-var") (jolt-hash-map (kw "op") (kw "the-var")
                                                               (kw "ns") "user" (kw "name") "sink")
                                 (kw "val") (anode "(foo 1)")))
       (r (infer-body U unhandled (jolt-hash-map))))
  (gate-check "infer walks through an op with no arm" (jolt-count (jolt-nth r 2)) 1))

;; --- the record-shapes registry feeds call-result types --------------------
;; without shapes a (->P …) call result is :any (accepted); with the registry it
;; types as a struct, so an arithmetic op over it is provably not-a-number.
(gate-check "ctor result :any w/o shapes" (diags "(+ (->P 1) 1)" #f) 0)
(set-record-shapes! U
  (jolt-hash-map "user/->P"
                 (jolt-hash-map (keyword #f "fields") (jolt-vector (keyword #f "x"))
                                (keyword #f "tags")   (jolt-vector jolt-nil)
                                (keyword #f "type")   "user.P")))
(gate-check "ctor result struct w/ shapes" (diags "(+ (->P 1) 1)" #f) 1)
(set-record-shapes! U (jolt-hash-map))

;; --- reinfer-def honors check-mode -----------------------------------------
;; When check-mode is on and a def is reinferred with WP seeds, the diagnostics
;; must be reported (they were silently lost before the fix).
(set-check-mode! U #t #f)
(let* ((node (anode "(defn f [x] (+ x :k))"))
       (ptmap (jolt-hash-map "x" (keyword #f "any"))))
  (reinfer-def U node ptmap)
  (gate-check "reinfer-def check-mode reports diags" (jolt-count (take-diags! U)) 1)
  (gate-check "reinfer-def diags drained after take" (jolt-count (take-diags! U)) 0))
(set-check-mode! U #f #f)
(let* ((node (anode "(defn f [x] (+ x :k))"))
       (ptmap (jolt-hash-map "x" (keyword #f "any"))))
  (reinfer-def U node ptmap)
  (gate-check "reinfer-def no diags when check-mode off" (jolt-count (take-diags! U)) 0))

;; --- the opt-path checker: run-inference emits, take-diags! drains -----------
;; (set-check-mode! on strict?) arms checking during the next run-inference; the
;; diagnostics are stashed for take-diags! to drain once.
(set-check-mode! U #t #f)
(run-inference U (anode "(+ 1 :k)"))
(gate-check "take-diags drains run-inference" (jolt-count (take-diags! U)) 1)
(gate-check "take-diags re-drained empty"     (jolt-count (take-diags! U)) 0)
(set-check-mode! U #f #f)
(run-inference U (anode "(+ 1 :k)"))
(gate-check "no diags when check-mode off"    (jolt-count (take-diags! U)) 0)

;; --- pred-on folds nil?/some? on a provably-nil operand ----------------------
;; A nil const initializer types as :nil, and :nil reaches pred-on's case table
;; (it slips past :any/:truthy/nilable?/union?). nil? must fold true, some? and
;; every type predicate false — the case table used to fold them the other way
;; because it assumed every concrete type is non-nil. These rows fail pre-remint
;; (the baked seed still has the old case table) and pass once the seed is reminted.
;; The third argument answers whether a struct's type tag is a defrecord; no
;; struct reaches it here.
(let* ((pred-on0 (var-deref "jolt.passes.types" "pred-on"))
       (pred-on (lambda (p t) (pred-on0 p t (lambda (tag) #f))))
       (t-nil (keyword #f "nil")))
  (gate-check "nil? on :nil folds true"      (pred-on "nil?"    t-nil) #t)
  (gate-check "some? on :nil folds false"    (pred-on "some?"   t-nil) #f)
  (gate-check "number? on :nil folds false"  (pred-on "number?" t-nil) #f)
  (gate-check "record? on :nil folds false"  (pred-on "record?" t-nil) #f)
  (gate-check "seq? on :nil folds false"     (pred-on "seq?"    t-nil) #f))

;; --- loop/fn-self bindings shadow in the inference tenv ----------------------
;; A loop var (or a (fn f [] …) self-name) that rebinds a record-typed outer
;; local must NOT keep the outer type — without shadowing (+ p 1) over the loop
;; body reports a num-op-on-record diag, and under --opt a bare slot read
;; miscompiles and crashes. With shadowing the rebound name is :any -> no diag.
;; The positive row confirms record typing outside the shadow is unchanged.
(set-record-shapes! U
  (jolt-hash-map "user/->P"
                 (jolt-hash-map (keyword #f "fields") (jolt-vector (keyword #f "x"))
                                (keyword #f "tags")   (jolt-vector jolt-nil)
                                (keyword #f "type")   "user.P")))
(gate-check "record local still num-op diag (unshadowed)"
       (diags "(let [p (->P 1.0 2.0)] (+ p 1))" #f) 1)
;; (the loop's p is a number: a leaked outer record type would be the diag. A
;; vector init is no longer a clean control -- the loop var is typed now, and
;; (+ [3 4] 1) is a genuine diag.)
(gate-check "loop var shadows record local (no diag)"
       (diags "(let [p (->P 1.0 2.0)] (loop [p 3] (+ p 1)))" #f) 0)
(gate-check "fn self-name shadows record local (no diag)"
       (diags "(let [f (->P 1.0 2.0)] ((fn f [] (+ f 1))))" #f) 0)
;; A loop var is the JOIN of its init and every recur that targets the loop: a
;; record every recur passes stays that record (so (+ p 1) is the record diag),
;; and one recur passing something else widens it (no diag).
(gate-check "loop var that stays a record is typed (diag)"
       (diags "(loop [p (->P 1.0 2.0) i 0] (if (< i 3) (recur (->P 2.0 3.0) (inc i)) (+ p 1)))" #f) 1)
(gate-check "loop var a recur widens is not (no diag)"
       (diags "(loop [p (->P 1.0 2.0) i 0] (if (< i 3) (recur [i] (inc i)) (+ p 1)))" #f) 0)
(gate-check "a recur in a nested loop does not feed the outer one"
       (diags "(loop [p (->P 1.0 2.0) i 0] (if (< i 3) (do (loop [p [1] j 0] (if (< j 1) (recur [2] (inc j)) p)) (recur (->P 2.0 3.0) (inc i))) (+ p 1)))" #f) 1)
(gate-check "a recur in a nested fn does not feed the outer loop"
       (diags "(loop [p (->P 1.0 2.0) i 0] (if (< i 3) (do ((fn [p] (if p (recur nil) p)) 1) (recur (->P 2.0 3.0) (inc i))) (+ p 1)))" #f) 1)
;; Loop inits bind in order, as let's do: q's init reads the loop's p, not an
;; outer p. Inferring every init in the outer env typed it from the wrong local.
(gate-check "a loop init sees the earlier loop binding"
       (diags "(let [p 1] (loop [p (->P 1.0 2.0) q (+ p 1)] q))" #f) 1)
(set-record-shapes! U (jolt-hash-map))

(gate-summary "infer")
