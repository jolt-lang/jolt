# Native codegen pipeline integration (jolt-ihdp): under :cgen?, a defn of a
# numeric-leaf fn is compiled to C and the cfunction installed as the var root,
# so direct-linked callers run native code. Pins (1) the root becomes a
# cfunction, (2) non-leaf / redefable defns stay bytecode, (3) results match.
# Skips the native legs where the C toolchain (cc + janet.h) is absent.
(import ../../src/jolt/api :as api)
(import ../../src/jolt/cgen :as cgen)

(print "Native codegen pipeline (jolt-ihdp)...")

(var failures 0)
(defn check [label ok] (unless ok (++ failures) (eprintf "  FAIL: %s" label)))
(defn callable? [x] (or (function? x) (cfunction? x)))

(def ctx (api/init-cached {:compile? true}))
(put (ctx :env) :direct-linking? true)
(put (ctx :env) :inline? true)
(put (ctx :env) :cgen? true)
(api/eval-string ctx "(ns cgp)")

(if (cgen/toolchain-available?)
  (do
    # numeric-leaf fn -> native root
    (api/eval-string ctx "(defn sq [x] (* x x))")
    (check "numeric-leaf root is a cfunction" (cfunction? (api/eval-string ctx "sq")))
    (check "native sq computes correctly" (= 49 (api/eval-string ctx "(sq 7)")))

    # non-leaf fn -> stays bytecode (NOT a cfunction)
    (api/eval-string ctx "(defn g [x] (str x))")
    (check "non-leaf root stays bytecode" (not (cfunction? (api/eval-string ctx "g"))))
    (check "g still works" (= "5" (api/eval-string ctx "(g 5)")))

    # ^:redef stays bytecode even though numeric-leaf
    (api/eval-string ctx "(defn ^:redef rsq [x] (* x x))")
    (check "redefable numeric-leaf stays bytecode" (not (cfunction? (api/eval-string ctx "rsq"))))

    # end-to-end: count-point native, run bytecode calling it; grid total matches
    (api/eval-string ctx "(defn count-point [cr ci cap] (loop [i 0 zr 0.0 zi 0.0] (if (or (>= i cap) (> (+ (* zr zr) (* zi zi)) 4.0)) i (recur (inc i) (+ (- (* zr zr) (* zi zi)) cr) (+ (* 2.0 (* zr zi)) ci)))))")
    (check "count-point root is native" (cfunction? (api/eval-string ctx "count-point")))
    (api/eval-string ctx "(defn run [n] (let [cap 200 nd (* 1.0 n)] (loop [y 0 acc 0] (if (< y n) (let [ci (- (/ (* 2.0 y) nd) 1.0) row (loop [x 0 a 0] (if (< x n) (let [cr (- (/ (* 2.0 x) nd) 1.5)] (recur (inc x) (+ a (count-point cr ci cap)))) a))] (recur (inc y) (+ acc row))) acc))))")
    (check "run stays bytecode (calls a user fn)" (not (cfunction? (api/eval-string ctx "run"))))
    (check "native count-point gives the right grid total" (= 3288753 (api/eval-string ctx "(run 200)"))))
  (print "  (toolchain absent — skipping native pipeline legs)"))

(if (= 0 failures)
  (print "All tests passed.")
  (do (eprintf "%d cgen-pipeline check(s) failed" failures) (os/exit 1)))
