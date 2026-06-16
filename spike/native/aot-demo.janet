# AOT build/deploy demo (jolt-a7ds). Two phases, run as separate processes:
#   janet spike/native/aot-demo.janet build    # needs cc; compiles + writes manifest
#   janet spike/native/aot-demo.janet deploy    # run with cc REMOVED from PATH
# Proves the deploy target needs no C toolchain: it only loads the prebuilt .so.
(import ../../src/jolt/api :as api)
(import ../../src/jolt/cgen :as cgen)

(def cp "(defn count-point [cr ci cap] (loop [i 0 zr 0.0 zi 0.0] (if (or (>= i cap) (> (+ (* zr zr) (* zi zi)) 4.0)) i (recur (inc i) (+ (- (* zr zr) (* zi zi)) cr) (+ (* 2.0 (* zr zi)) ci)))))")
(def run "(defn run [n] (let [cap 200 nd (* 1.0 n)] (loop [y 0 acc 0] (if (< y n) (let [ci (- (/ (* 2.0 y) nd) 1.0) row (loop [x 0 a 0] (if (< x n) (let [cr (- (/ (* 2.0 x) nd) 1.5)] (recur (inc x) (+ a (count-point cr ci cap)))) a))] (recur (inc y) (+ acc row))) acc))))")
(def manifest (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-aot-demo.jdn"))
(def dir "spike/native/build/aot")

(defn setup [ctx]
  (put (ctx :env) :direct-linking? true)
  (api/eval-string ctx "(ns demo)"))

(def phase (get (dyn :args) 1))
(cond
  (= phase "build")
  (do
    (def ctx (api/init-cached {:compile? true}))
    (put (ctx :env) :cgen-collect? true)
    (setup ctx)
    (api/eval-string ctx cp)
    (api/eval-string ctx run)
    (def build (cgen/aot-build (get (ctx :env) :cgen-collected) {:dir dir}))
    (cgen/write-manifest manifest build)
    (printf "BUILD: cc-available? %p -> %s (%d fn)" (cgen/toolchain-available?)
            (build :sopath) (length (build :entries))))

  (= phase "deploy")
  (do
    (printf "DEPLOY: cc-available? %p (should be false with cc off PATH)"
            (cgen/toolchain-available?))
    (def ctx (api/init-cached {:compile? true}))
    (setup ctx)
    (put (ctx :env) :cgen-prebuilt (cgen/load-aot manifest))
    (api/eval-string ctx cp)
    (api/eval-string ctx run)
    (def native? (cfunction? (api/eval-string ctx "count-point")))
    (def t0 (os/clock))
    (def total (api/eval-string ctx "(run 200)"))
    (def ms (* 1000 (- (os/clock) t0)))
    (printf "DEPLOY: count-point native? %p  total %d (%s)  %.1f ms"
            native? total (if (= total 3288753) "OK" "MISMATCH") ms))

  (eprint "usage: aot-demo.janet build|deploy"))
