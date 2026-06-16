# Benchmark the native-C mandelbrot vs the spike's other legs (jolt-5vsp lever 1).
#   janet spike/native/bench-native.janet 200
(import ./build/mandel :as mandel)

(defn bench [label f n]
  (repeat 2 (f (div n 2)))
  (def times @[])
  (var last-r 0)
  (repeat 3
    (def t0 (os/clock))
    (set last-r (f n))
    (array/push times (* 1000.0 (- (os/clock) t0))))
  (printf "%-28s n %d  result %d  mean %.2f ms"
          label n last-r (/ (sum times) 3)))

# Leg A: whole run in native C (pure native-codegen ceiling).
(defn run-pure-c [n] (mandel/run-c n))

# Leg B: Janet `while` loop, but count-point is a native C cfunction called n^2
# times — measures the Janet->C boundary-crossing cost (the incremental hybrid).
(defn run-boundary [n]
  (def cap 200)
  (def nd (* 1.0 n))
  (var acc 0)
  (var y 0)
  (while (< y n)
    (def ci (- (/ (* 2.0 y) nd) 1.0))
    (var x 0)
    (var a 0)
    (while (< x n)
      (def cr (- (/ (* 2.0 x) nd) 1.5))
      (set a (+ a (mandel/count-point-c cr ci cap)))
      (++ x))
    (set acc (+ acc a))
    (++ y))
  acc)

# Leg C: C run loop calling a Janet bytecode count-point back via janet_call n^2
# times — the reverse crossing (hot C fn -> cold bytecode helper).
(defn count-point-janet [cr ci cap]
  (var i 0) (var zr 0.0) (var zi 0.0)
  (while (and (< i cap) (<= (+ (* zr zr) (* zi zi)) 4.0))
    (def nzr (+ (- (* zr zr) (* zi zi)) cr))
    (def nzi (+ (* 2.0 (* zr zi)) ci))
    (set zr nzr) (set zi nzi) (++ i))
  i)
(defn run-callback [n] (mandel/run-callback n count-point-janet))

(defn main [& args]
  (def n (if (> (length args) 1) (scan-number (get args 1)) 200))
  (bench "native-C whole run" run-pure-c n)
  (bench "Janet loop -> C count-point" run-boundary n)
  (bench "C loop -> janet_call back" run-callback n))
