# Dump the analyzed IR (not emitted Janet) for count-point, so the C emitter can
# be written against the real node shapes. jolt-ihdp.
(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/reader :as reader)

(def ctx (api/init-cached {:compile? true}))
(put (ctx :env) :direct-linking? true)
(put (ctx :env) :inline? true)
(api/eval-string ctx "(ns mandelbrot)")

(def src "(defn count-point [cr ci cap] (loop [i 0 zr 0.0 zi 0.0] (if (or (>= i cap) (> (+ (* zr zr) (* zi zi)) 4.0)) i (recur (inc i) (+ (- (* zr zr) (* zi zi)) cr) (+ (* 2.0 (* zr zi)) ci)))))")

(def ir (backend/analyze-form ctx (reader/parse-string src)))
(printf "%P" ir)
