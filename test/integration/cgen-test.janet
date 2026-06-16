# Native codegen (jolt-ihdp): IR -> C for numeric-leaf fns. Pins that the C
# translator (1) classifies candidates correctly and (2) produces a native fn
# whose results match the bytecode/interpreted fn over the mandelbrot grid.
# Skips cleanly where the C toolchain (cc + janet.h) is absent — the rest of the
# gate still runs.
(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/reader :as reader)
(import ../../src/jolt/cgen :as cgen)

(print "Native codegen IR->C (jolt-ihdp)...")

(def ctx (api/init-cached {:compile? true}))
(put (ctx :env) :direct-linking? true)
(put (ctx :env) :inline? true)
(api/eval-string ctx "(ns cgentest)")

(defn ir-of [src] (backend/analyze-form ctx (reader/parse-string src)))

(def count-point-src
  "(defn count-point [cr ci cap] (loop [i 0 zr 0.0 zi 0.0] (if (or (>= i cap) (> (+ (* zr zr) (* zi zi)) 4.0)) i (recur (inc i) (+ (- (* zr zr) (* zi zi)) cr) (+ (* 2.0 (* zr zi)) ci)))))")

(var failures 0)
(defn check [label ok] (unless ok (++ failures) (eprintf "  FAIL: %s" label)))

# --- classification ---
(check "count-point is a numeric leaf" (cgen/numeric-leaf? (ir-of count-point-src)))
(check "fn calling a non-native fn is NOT a leaf"
       (not (cgen/numeric-leaf? (ir-of "(defn f [x] (str x))"))))
(check "fn building a collection is NOT a leaf"
       (not (cgen/numeric-leaf? (ir-of "(defn f [x] [x x])"))))
(check "plain numeric expr fn IS a leaf"
       (cgen/numeric-leaf? (ir-of "(defn sq [x] (* x x))")))

# --- C generation is well-formed (smoke: contains the wrapper + a loop) ---
(def c-src (cgen/gen-c-fn (ir-of count-point-src) "count_point"))
(check "emits a cfunction wrapper" (string/find "cfun_count_point" c-src))
(check "lowers the loop to a C while" (string/find "while (" c-src))
(check "unboxes params with janet_getnumber" (string/find "janet_getnumber" c-src))

# --- behavioral equivalence (only where the toolchain is present) ---
(if (cgen/toolchain-available?)
  (do
    # start from a clean cache dir so the content-addressed-file count is exact
    (when (os/stat "build/cgen-test")
      (each f (os/dir "build/cgen-test") (os/rm (string "build/cgen-test/" f))))
    (api/eval-string ctx count-point-src)
    (def bc-cp (api/eval-string ctx "count-point"))   # the compiled/interpreted fn
    (def c-cp (cgen/compile-fn (ir-of count-point-src)
                               {:dir "build/cgen-test" :name "count_point_test"}))
    (defn callable? [x] (or (function? x) (cfunction? x)))
    (check "compile-fn returns a callable" (callable? c-cp))
    (when (callable? c-cp)
      # spot points spanning escape-fast .. in-set
      (var ptmatch true)
      (each [cr ci] [[2.0 2.0] [0.5 0.5] [-0.5 0.6] [0.28 0.0] [-0.74 0.1] [-0.5 0.0] [0.0 0.0]]
        (unless (= (c-cp cr ci 200) (bc-cp cr ci 200)) (set ptmatch false)))
      (check "C count-point matches bytecode on sample points" ptmatch)
      # full mandelbrot grid total
      (defn grid-total [cp n]
        (def cap 200) (def nd (* 1.0 n)) (var acc 0) (var y 0)
        (while (< y n)
          (def ci (- (/ (* 2.0 y) nd) 1.0)) (var x 0) (var a 0)
          (while (< x n)
            (def cr (- (/ (* 2.0 x) nd) 1.5)) (set a (+ a (cp cr ci cap))) (++ x))
          (set acc (+ acc a)) (++ y))
        acc)
      (check "C and bytecode agree on the full n=80 grid total"
             (= (grid-total c-cp 80) (grid-total bc-cp 80)))

      # caching: the .so is content-addressed, so a second compile of the same
      # fn reuses it. Drop the generated .c (keep the .so) then recompile: a
      # cache hit loads the existing .so without needing cc/source again.
      (def cache-files (filter |(string/has-suffix? ".so" $) (os/dir "build/cgen-test")))
      (check "produced a content-addressed .so" (= 1 (length cache-files)))
      (each f (os/dir "build/cgen-test")
        (when (string/has-suffix? ".c" f) (os/rm (string "build/cgen-test/" f))))
      (def c-cp2 (cgen/compile-fn (ir-of count-point-src)
                                  {:dir "build/cgen-test" :name "count_point_test"}))
      (check "cache hit recompiles without the source .c"
             (and (callable? c-cp2) (= (c-cp2 -0.5 0.0 200) 200)))))
  (print "  (toolchain absent — skipping behavioral equivalence)"))

(if (= 0 failures)
  (print "All tests passed.")
  (do (eprintf "%d cgen check(s) failed" failures) (os/exit 1)))
