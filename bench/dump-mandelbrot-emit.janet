# Dump the Janet that jolt's backend emits for the mandelbrot hot fns, so we can
# A/B it against bench/mandelbrot-hand.janet and localize the ~1.43x jolt-over-
# hand-Janet gap measured in the foundational-runtime spike (jolt-5vsp).
(import ../src/jolt/api :as api)
(import ../src/jolt/backend :as backend)
(import ../src/jolt/reader :as reader)

(def ctx (api/init-cached {:compile? true}))
(put (ctx :env) :direct-linking? true)
(put (ctx :env) :inline? true)
(api/eval-string ctx "(ns mandelbrot)")

(def count-point-src
  "(defn count-point [cr ci cap] (loop [i 0 zr 0.0 zi 0.0] (if (or (>= i cap) (> (+ (* zr zr) (* zi zi)) 4.0)) i (recur (inc i) (+ (- (* zr zr) (* zi zi)) cr) (+ (* 2.0 (* zr zi)) ci)))))")

(def run-src
  "(defn run [n] (let [cap 200 nd (* 1.0 n)] (loop [y 0 acc 0] (if (< y n) (let [ci (- (/ (* 2.0 y) nd) 1.0) row (loop [x 0 a 0] (if (< x n) (let [cr (- (/ (* 2.0 x) nd) 1.5)] (recur (inc x) (+ a (count-point cr ci cap)))) a))] (recur (inc y) (+ acc row))) acc))))")

(api/eval-string ctx count-point-src)
(api/eval-string ctx run-src)

(defn emit [src]
  (string/format "%p" (backend/emit-ir ctx (backend/analyze-form ctx (reader/parse-string src)))))

(print "===== count-point emitted Janet =====")
(print (emit count-point-src))
(print "\n===== run emitted Janet =====")
(print (emit run-src))
