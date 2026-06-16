# mandelbrot — hand-written idiomatic Janet, same nested loop as bench/mandelbrot.clj.
# This is the "optimal Janet" leg of the foundational-runtime spike (jolt-5vsp):
# comparing jolt-emitted Janet vs this vs JVM localizes the 15x compute floor —
# jolt-backend overhead vs the Janet VM's own floor.
#
#   janet bench/mandelbrot-hand.janet 200

(defn count-point [cr ci cap]
  (var i 0)
  (var zr 0.0)
  (var zi 0.0)
  (while (and (< i cap) (<= (+ (* zr zr) (* zi zi)) 4.0))
    (def nzr (+ (- (* zr zr) (* zi zi)) cr))
    (def nzi (+ (* 2.0 (* zr zi)) ci))
    (set zr nzr)
    (set zi nzi)
    (++ i))
  i)

(defn run [n]
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
      (set a (+ a (count-point cr ci cap)))
      (++ x))
    (set acc (+ acc a))
    (++ y))
  acc)

(defn main [& args]
  (def n (if (> (length args) 1) (scan-number (get args 1)) 1000))
  # warmup
  (repeat 2 (run (div n 2)))
  (def runs 3)
  (def times @[])
  (var last-r 0)
  (repeat runs
    (def t0 (os/clock))
    (def r (run n))
    (def ms (* 1000.0 (- (os/clock) t0)))
    (set last-r r)
    (array/push times ms))
  (def mean (/ (sum times) runs))
  (printf "mandelbrot n %d result %d" n last-r)
  (print "runs: " (string/join (map |(string (/ (math/round (* $ 10.0)) 10.0)) times) " "))
  (printf "mean: %.1f ms" mean))
