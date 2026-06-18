# Phase 1 (jolt-cf1q.2) close-out bench — the compute benches run END TO END
# through the REAL pipeline (Clojure source -> Janet-hosted analyzer -> IR ->
# Scheme emitter -> Chez compile -> run), timed in-process (Chez startup
# excluded), and reported against the spike ceiling (spike/chez/RESULTS.md).
#
# This is the Phase 1 gate evidence: (1) compile-only is TOTAL for the compute
# subset — every form emits, no interpreter fallback (Chez has none); (2) the
# emitted code runs at ~the substrate ceiling, with the residual gap being
# exactly the typed fl*/fx* emission that Phase 4 owns.
#
#   JOLT_CHEZ_BENCH=1 janet test/chez/bench-pipeline.janet
# Opt-in (like core-bench); skipped in the normal gate.
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/reader :as r)
(import ../../host/chez/driver :as d)
(import ../../host/chez/emit :as emit)

(unless (os/getenv "JOLT_CHEZ_BENCH")
  (print "skip: set JOLT_CHEZ_BENCH=1 to run the Chez pipeline bench")
  (os/exit 0))
(unless (d/chez-available?)
  (print "skip: chez not on PATH")
  (os/exit 0))

(def ctx (d/make-ctx))

# Emit a top-level program (one or more defns) through the real pipeline. Returns
# the concatenated Scheme for every form — each form must emit (compile-only is
# total) or this throws, which is itself the totality check.
(defn emit-program [src]
  (def forms (map first (r/parse-all-positioned src)))
  (string/join (map (fn [f] (emit/emit (backend/analyze-form ctx f))) forms) "\n"))

(def fib-src "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))")

# mandelbrot kernel: loop/recur + let + or-expansion + cross-var call, all flonum.
(def mandel-src ``
(defn count-point [cr ci cap]
  (loop [i 0 zr 0.0 zi 0.0]
    (if (or (>= i cap) (> (+ (* zr zr) (* zi zi)) 4.0))
      i
      (recur (inc i)
             (+ (- (* zr zr) (* zi zi)) cr)
             (+ (* 2.0 (* zr zi)) ci)))))
(defn run [n]
  (let [cap 200 nd (* 1.0 n)]
    (loop [y 0 acc 0]
      (if (>= y n) acc
        (let [ci (- (* 2.0 (/ (* 1.0 y) nd)) 1.0)
              row (loop [x 0 a 0]
                    (if (>= x n) a
                      (let [cr (- (* 3.5 (/ (* 1.0 x) nd)) 2.5)]
                        (recur (inc x) (+ a (count-point cr ci cap))))))]
          (recur (inc y) (+ acc row)))))))
``)

(def fib-scm (emit-program fib-src))
(def mandel-scm (emit-program mandel-src))
(print "compile-only total: fib + mandelbrot emitted with no fallback")

# One Chez program: load the RT, the emitted defns, a hand-written FLONUM fib
# reference (jolt's realistic ceiling given the all-double model), then time each
# end-to-end value (warm up first; exclude Chez startup via a monotonic clock).
(def prog
  (string
    "(import (chezscheme))\n(load \"host/chez/rt.ss\")\n"
    fib-scm "\n" mandel-scm "\n"
    "(define fib (var-deref \"user\" \"fib\"))\n"
    "(define mrun (var-deref \"user\" \"run\"))\n"
    # hand flonum fib — the substrate ceiling for jolt's number model
    "(define (ffib n) (if (fl< n 2.0) n (fl+ (ffib (fl- n 1.0)) (ffib (fl- n 2.0)))))\n"
    "(define (now-ns) (let ((t (current-time 'time-monotonic))) (+ (* (time-second t) 1000000000) (time-nanosecond t))))\n"
    "(define (timed thunk) (let* ((t0 (now-ns)) (r (thunk)) (ms (/ (- (now-ns) t0) 1000000.0))) (cons r ms)))\n"
    "(fib 24)(ffib 24.0)(mrun 30)\n"   # warm up
    "(let ((a (timed (lambda () (fib 30))))\n"
    "      (b (timed (lambda () (ffib 30.0))))\n"
    "      (c (timed (lambda () (mrun 200)))))\n"
    "  (printf \"~a ~a ~a ~a ~a ~a\\n\"\n"
    "    (jolt-pr-str (car a)) (exact->inexact (cdr a))\n"
    "    (jolt-pr-str (car b)) (exact->inexact (cdr b))\n"
    "    (jolt-pr-str (car c)) (exact->inexact (cdr c))))"))

(def path (string "/tmp/chez-bench-pipeline-" (os/getpid) ".ss"))
(spit path prog)
(def proc (os/spawn ["chez" "--script" path] :p {:out :pipe :err :pipe}))
(def out (string/trim (string (ev/read (proc :out) 0x100000))))
(def err (string/trim (string (or (ev/read (proc :err) 0x100000) ""))))
(def code (os/proc-wait proc))
(unless (= code 0) (printf "BENCH FAILED (code %d): %s" code err) (os/exit 1))

(def p (string/split " " out))
(defn num [i] (scan-number (get p i "0")))
(printf "\nReal-pipeline compute benches (Chez startup excluded):\n")
(printf "  fib 30 (jolt, flonum)        = %s in %6.2f ms" (get p 0) (num 1))
(printf "  fib 30 (hand flonum ceiling) =        %6.2f ms   <- jolt's number-model ceiling" (num 3))
(printf "  fib 30 (spike fixnum ceiling)=          5.20 ms   <- Phase 4 fl/fx typed-emit target")
(printf "  mandelbrot 200 (jolt)        = %s in %6.2f ms" (get p 4) (num 5))
(printf "  mandelbrot 200 (spike generic)=        98.10 ms   <- generic-flonum ceiling")
(printf "  mandelbrot 200 (spike typed) =         13.40 ms   <- Phase 4 fl/fx typed-emit target")
(def fib-overhead (if (> (num 3) 0) (/ (num 1) (num 3)) 0))
(printf "\n  jolt fib is %.2fx the hand-flonum ceiling (residual = truthy/dispatch; typed-emit closes the fixnum gap)." fib-overhead)
(os/exit 0)
