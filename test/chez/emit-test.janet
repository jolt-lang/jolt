# Phase 1 — IR -> Scheme emitter tests. Hand-built IR in the real ir.clj shapes,
# emitted to Scheme, compiled+run on Chez, results + fib speed checked.
#   janet test/chez/emit-test.janet      (from repo root)
(import ../../host/chez/emit :as e)

(defn run-chez [src]
  (spit "/tmp/emit-prog.ss" src)
  (def proc (os/spawn ["chez" "--script" "/tmp/emit-prog.ss"] :p {:out :pipe :err :pipe}))
  (def out (ev/read (proc :out) 0x100000))
  (def err (ev/read (proc :err) 0x100000))
  (def code (os/proc-wait proc))
  [code (string/trim (if out (string out) "")) (string/trim (if err (string err) ""))])

(var total 0) (var fails 0)
(defn ok [name pred] (++ total) (unless pred (++ fails) (printf "FAIL: %s" name)))

# --- IR builders (ir.clj shapes) ---
(defn rt [name & args] {:op :invoke :fn {:op :rt :name name} :args args})
(defn lcl [n] {:op :local :name n})
(defn k [v] {:op :const :val v})

# 1) (+ 1 2)
(def add-ir (rt "+" (k 1) (k 2)))
(let [[code out err] (run-chez (e/program [] (e/emit add-ir)))]
  (ok "(+ 1 2) = 3" (and (= code 0) (= out "3")))
  (when (not= code 0) (printf "  err: %s" err)))

# 2) fib def + (fib 30)
(defn fib-call [arg] {:op :invoke :fn {:op :var :ns "user" :name "fib"} :args [arg]})
(def fib-def
  {:op :def :ns "user" :name "fib"
   :init {:op :fn :name "fib"
          :arities [{:params ["n"]
                     :body {:op :if
                            :test (rt "<" (lcl "n") (k 2))
                            :then (lcl "n")
                            :else (rt "+" (fib-call (rt "-" (lcl "n") (k 1)))
                                          (fib-call (rt "-" (lcl "n") (k 2))))}}]}})
(let [prog (e/program [(e/emit fib-def)] (e/emit (fib-call (k 30))))
      [code out err] (run-chez prog)]
  (ok "(fib 30) = 832040" (and (= code 0) (= out "832040")))
  (when (not= code 0) (printf "  err: %s" err)))

# 3) loop/recur sum 1..5 = 15
(def loop-ir
  {:op :loop
   :bindings [["i" (k 1)] ["acc" (k 0)]]
   :body {:op :if
          :test (rt ">" (lcl "i") (k 5))
          :then (lcl "acc")
          :else {:op :recur :args [(rt "inc" (lcl "i")) (rt "+" (lcl "acc") (lcl "i"))]}}})
(let [[code out err] (run-chez (e/program [] (e/emit loop-ir)))]
  (ok "loop/recur sum = 15" (and (= code 0) (= out "15")))
  (when (not= code 0) (printf "  err: %s" err)))

# 4) speed: emitted fib(30) should hit ~the spike ceiling (hand-Scheme ~5ms),
#    proving the IR->Scheme path adds no overhead vs hand-written Scheme.
(def timed-fib
  (string (e/emit fib-def) "\n"
          "(define (now-ns) (let ((t (current-time 'time-monotonic))) (+ (* (time-second t) 1000000000) (time-nanosecond t))))\n"
          "(fib 24)(fib 24)\n"
          "(let* ((t0 (now-ns)) (r (fib 30)) (ms (/ (- (now-ns) t0) 1000000.0)))\n"
          "  (printf \"~a ~a\\n\" r (exact->inexact ms)))"))
(let [[code out err] (run-chez (string "(import (chezscheme))\n(load \"host/chez/values.ss\")\n(define (jolt-inc x) (+ x 1))\n" timed-fib))]
  (def parts (string/split " " out))
  (def result (get parts 0))
  (def ms (scan-number (or (get parts 1) "999")))
  (ok "emitted fib(30) correct + fast" (and (= code 0) (= result "832040") (< ms 40)))
  (printf "  emitted fib(30): %s in %.2f ms (hand-Scheme spike ~5ms)" result ms)
  (when (not= code 0) (printf "  err: %s" err)))

(printf "\nemit-test: %d/%d passed" (- total fails) total)
(os/exit (if (> fails 0) 1 0))
