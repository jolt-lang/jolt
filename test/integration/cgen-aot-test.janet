# Native codegen AOT build/deploy (jolt-a7ds): compile an app's numeric-leaf fns
# into ONE native module at build time, then deploy with NO cc — load the
# prebuilt module and install the cfunctions as var roots. Proves the build-time
# path that removes the runtime-toolchain dependency. Skips where cc/janet.h are
# absent.
(import ../../src/jolt/api :as api)
(import ../../src/jolt/cgen :as cgen)

(print "Native codegen AOT build/deploy (jolt-a7ds)...")

(var failures 0)
(defn check [label ok] (unless ok (++ failures) (eprintf "  FAIL: %s" label)))

(def cp-src "(defn count-point [cr ci cap] (loop [i 0 zr 0.0 zi 0.0] (if (or (>= i cap) (> (+ (* zr zr) (* zi zi)) 4.0)) i (recur (inc i) (+ (- (* zr zr) (* zi zi)) cr) (+ (* 2.0 (* zr zi)) ci)))))")
(def run-src "(defn run [n] (let [cap 200 nd (* 1.0 n)] (loop [y 0 acc 0] (if (< y n) (let [ci (- (/ (* 2.0 y) nd) 1.0) row (loop [x 0 a 0] (if (< x n) (let [cr (- (/ (* 2.0 x) nd) 1.5)] (recur (inc x) (+ a (count-point cr ci cap)))) a))] (recur (inc y) (+ acc row))) acc))))")

(if (cgen/toolchain-available?)
  (do
    # --- build phase: collect numeric-leaf fns, compile one module, write manifest
    (def bctx (api/init-cached {:compile? true}))
    (put (bctx :env) :direct-linking? true)
    (put (bctx :env) :cgen-collect? true)
    (api/eval-string bctx "(ns aot)")
    (api/eval-string bctx cp-src)
    (api/eval-string bctx run-src)
    (def collected (get (bctx :env) :cgen-collected))
    (check "collected exactly the numeric-leaf fn (count-point, not run)"
           (and collected (= 1 (length collected))
                (= "count-point" ((first collected) :name))))
    (def manifest-path (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-aot-test.jdn"))
    (def build (cgen/aot-build collected {:dir "build/cgen-aot-test"}))
    (check "aot-build produced a module" (and build (build :sopath)))
    (cgen/write-manifest manifest-path build)

    # --- deploy phase: fresh ctx, load prebuilt (NO cc), install roots, run
    (def dctx (api/init-cached {:compile? true}))
    (put (dctx :env) :direct-linking? true)
    (def prebuilt (cgen/load-aot manifest-path))
    (check "load-aot maps the qname to a cfunction"
           (cfunction? (get prebuilt "aot/count-point")))
    (put (dctx :env) :cgen-prebuilt prebuilt)
    (api/eval-string dctx "(ns aot)")
    (api/eval-string dctx cp-src)
    (api/eval-string dctx run-src)
    (check "deployed count-point root is the prebuilt cfunction"
           (cfunction? (api/eval-string dctx "count-point")))
    (check "deployed run stays bytecode" (not (cfunction? (api/eval-string dctx "run"))))
    (check "AOT-deployed mandelbrot computes the right total"
           (= 3288753 (api/eval-string dctx "(run 200)")))

    # a fn NOT in the manifest must stay bytecode in deploy
    (api/eval-string dctx "(defn sq [x] (* x x))")
    (check "unlisted numeric-leaf stays bytecode in deploy (no cc)"
           (not (cfunction? (api/eval-string dctx "sq")))))
  (print "  (toolchain absent — skipping AOT legs)"))

(if (= 0 failures)
  (print "All tests passed.")
  (do (eprintf "%d cgen-aot check(s) failed" failures) (os/exit 1)))
