# Single-binary native build (jolt-a7ds): `jolt cgen-build` fuses an app's hot
# numeric-leaf fns (compiled to C) + the app source into ONE static executable —
# no sidecar .so, no toolchain on the target. This test builds the fixture app to
# a binary, runs it from a CLEAN dir (no src/, no cg.so) to prove it's
# self-contained, and checks the result + native fn count. Skips with no cc.
(import ../../src/jolt/cgen :as cgen)
(import ../../src/jolt/cgen_build :as cb)

(print "Single-binary native build (jolt-a7ds)...")

(var failures 0)
(defn check [label ok] (if ok (print "  ok: " label) (do (++ failures) (eprintf "  FAIL: %s" label))))

(def home (os/cwd))                                       # the repo (gate runs here)
(def fixture-root (string home "/test/fixtures/cgen-build"))

(if (cgen/toolchain-available?)
  (do
    (def out (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-cgen-build-test/app"))
    (def r (cb/build-app {:main "cgapp" :out out :jolt-home home
                          :source-roots [fixture-root]}))
    (check "build reported the single native leaf (count-point)" (= 1 (r :native-count)))
    (check "binary exists at :out" (os/stat out))

    # Run from a CLEAN dir with NO src/ and NO cg.so beside it — proves the binary
    # is self-contained (runtime needs neither the jolt tree nor a sidecar .so).
    (def rundir (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-cgen-build-run"))
    (when (os/stat rundir) (each e (os/dir rundir) (os/rm (string rundir "/" e))) (os/rmdir rundir))
    (os/mkdir rundir)
    (def bin (string rundir "/app"))
    (spit bin (slurp out))
    (os/chmod bin 8r755)

    (defn run-app [&opt n]
      (def tmp (string rundir "/out.txt"))
      (def f (file/open tmp :w))
      (def code (os/execute (if n [bin (string n)] [bin]) :px {:out f} ))
      (file/close f)
      {:code code :out (string/trim (slurp tmp))})

    (def res (run-app))
    (check "exits 0" (= 0 (res :code)))
    (check "self-contained binary computes the right mandelbrot total (n=200)"
           (= "3288753" (res :out)))
    (check "passes through command-line args (n=400)"
           (= "13162060" ((run-app 400) :out))))
  (print "  (toolchain absent — skipping single-binary build)"))

(if (= 0 failures)
  (print "All tests passed.")
  (do (eprintf "%d cgen-build check(s) failed" failures) (os/exit 1)))
