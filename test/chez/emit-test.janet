# Phase 1 (jolt-cf1q.2) — REAL pipeline end to end: actual Clojure source ->
# Janet-hosted analyzer -> host-neutral IR -> Scheme emitter -> run on Chez.
# Correctness is checked by parity against the SAME program evaluated by the
# Janet host (jolt's own oracle), so a divergence is the back end's, not the
# program's.
#   janet test/chez/emit-test.janet      (from repo root)
(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/reader :as r)
(import ../../host/chez/driver :as d)
(import ../../host/chez/emit :as emit)

(unless (d/chez-available?)
  (print "skip: chez not on PATH")
  (os/exit 0))

(var total 0) (var fails 0)
(defn ok [name pred &opt extra]
  (++ total)
  (if pred (printf "ok: %s" name)
    (do (++ fails) (printf "FAIL: %s   %s" name (or extra "")))))

# Janet-host oracle: evaluate the same program, stringify its value the way jolt
# prints it at the CLI (so "832040" not "832040.0", "0.5" not 1/2, etc.).
(def oracle-ctx (api/init {:compile? true}))
(defn oracle [src] (string (api/load-string oracle-ctx src)))

(def ctx (d/make-ctx))

# 1) constant-folded arithmetic: (+ 1 2) -> the analyzer folds to const 3.
(let [[code out err] (d/run-on-chez ctx "(+ 1 2)")]
  (ok "(+ 1 2) = 3" (and (= code 0) (= out "3") (= out (oracle "(+ 1 2)"))) (string out " | " err)))

# 2) fib: var-cell def + named-fn self-recursion + native arith, via real IR.
(let [src "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 30)"
      [code out err] (d/run-on-chez ctx src)]
  (ok "(fib 30) = 832040" (and (= code 0) (= out "832040") (= out (oracle src))) (string out " | " err)))

# 3) mandelbrot kernel: loop/recur, let, or-expansion, cross-var call
#    (run -> count-point), flonum compute. Parity vs the Janet host on run(40).
(def mandel-defs ``
(defn count-point [cr ci cap]
  (loop [i 0 zr 0.0 zi 0.0]
    (if (or (>= i cap) (> (+ (* zr zr) (* zi zi)) 4.0))
      i
      (recur (inc i)
             (+ (- (* zr zr) (* zi zi)) cr)
             (+ (* 2.0 (* zr zi)) ci)))))
(defn run [n]
  (let [cap 200
        nd (* 1.0 n)]
    (loop [y 0 acc 0]
      (if (< y n)
        (let [ci (- (/ (* 2.0 y) nd) 1.0)
              row (loop [x 0 a 0]
                    (if (< x n)
                      (let [cr (- (/ (* 2.0 x) nd) 1.5)]
                        (recur (inc x) (+ a (count-point cr ci cap))))
                      a))]
          (recur (inc y) (+ acc row)))
        acc))))
``)
(let [src (string mandel-defs "\n(run 40)")
      [code out err] (d/run-on-chez ctx src)]
  (ok "mandelbrot run(40) parity" (and (= code 0) (= out (oracle src)))
      (string "chez=" out " janet=" (oracle src) " | " err)))

# 3b) regressions found via the corpus probe:
#   - loop binds SEQUENTIALLY (Scheme named-let is parallel); b must see a.
#   - #(...) shorthand gensyms params with a trailing `#` (invalid in Scheme).
(each [label src] [["loop sequential init" "(loop [a 1 b (+ a 10)] (+ a b))"]
                   ["#() shorthand" "(#(+ %1 %2) 1 2)"]]
  (let [[code out err] (d/run-on-chez ctx src)]
    (ok label (and (= code 0) (= out (oracle src))) (string "chez=" out " janet=" (oracle src) " | " err))))

# 4) perf signal: emitted fib(30) in-Scheme timing (excludes Chez startup), to
#    track against the spike ceiling (hand-Scheme fib ~5ms). Informational — the
#    jolt-truthy? wrapper (~3x) and flonum modeling are known Phase-4 levers.
(let [fib-ir (backend/analyze-form ctx (in (r/parse-next "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))") 0))
      fib-scm (emit/emit fib-ir)
      timed (string "(import (chezscheme))\n(load \"host/chez/rt.ss\")\n"
                    fib-scm "\n"
                    "(define fib (var-deref \"user\" \"fib\"))\n"
                    "(define (now-ns) (let ((t (current-time 'time-monotonic))) (+ (* (time-second t) 1000000000) (time-nanosecond t))))\n"
                    "(fib 24)(fib 24)\n"
                    "(let* ((t0 (now-ns)) (r (fib 30)) (ms (/ (- (now-ns) t0) 1000000.0)))\n"
                    "  (printf \"~a ~a\\n\" (jolt-pr-str r) (exact->inexact ms)))")]
  (spit "/tmp/chez-jolt-fib-timed.ss" timed)
  (def proc (os/spawn ["chez" "--script" "/tmp/chez-jolt-fib-timed.ss"] :p {:out :pipe :err :pipe}))
  (def out (string/trim (string (ev/read (proc :out) 0x100000))))
  (def err (string/trim (string (or (ev/read (proc :err) 0x100000) ""))))
  (def code (os/proc-wait proc))
  (def parts (string/split " " out))
  (def result (get parts 0))
  (def ms (scan-number (or (get parts 1) "999")))
  (ok "timed fib(30) correct" (and (= code 0) (= result "832040")) (string out " | " err))
  (printf "  emitted fib(30): %s in %.2f ms (hand-Scheme spike ~5ms)" result ms))

(printf "\nemit-test: %d/%d passed" (- total fails) total)
(os/exit (if (> fails 0) 1 0))
