# mandelbrot — hand-written Janet that MIRRORS jolt's loop-lowering: every loop/
# recur becomes a self-recursive local closure stored in a var and called once
# per iteration (see bench/dump-mandelbrot-emit.janet). If this lands at jolt's
# ~219ms (vs the while-loop mandelbrot-hand.janet's ~153ms), the ~1.43x jolt-
# over-hand-Janet gap is the recursive-closure loop lowering, not anything else.
#   janet bench/mandelbrot-hand-rec.janet 200

(defn count-point [cr ci cap]
  (var loopfn nil)
  (set loopfn (fn [i zr zi]
    (if (let [t (>= i cap)] (if t t (> (+ (* zr zr) (* zi zi)) 4)))
      i
      (loopfn (+ i 1)
              (+ (- (* zr zr) (* zi zi)) cr)
              (+ (* 2 (* zr zi)) ci)))))
  (loopfn 0 0 0))

(defn run [n]
  (def cap 200)
  (def nd (* 1 n))
  (var yloop nil)
  (set yloop (fn [y acc]
    (if (< y n)
      (let [ci (- (/ (* 2 y) nd) 1)
            row (do
                  (var xloop nil)
                  (set xloop (fn [x a]
                    (if (< x n)
                      (let [cr (- (/ (* 2 x) nd) 1.5)]
                        (xloop (+ x 1) (+ a (count-point cr ci cap))))
                      a)))
                  (xloop 0 0))]
        (yloop (+ y 1) (+ acc row)))
      acc)))
  (yloop 0 0))

(defn main [& args]
  (def n (if (> (length args) 1) (scan-number (get args 1)) 1000))
  (repeat 2 (run (div n 2)))
  (def times @[])
  (var last-r 0)
  (repeat 3
    (def t0 (os/clock))
    (def r (run n))
    (array/push times (* 1000.0 (- (os/clock) t0)))
    (set last-r r))
  (printf "mandelbrot n %d result %d" n last-r)
  (print "runs: " (string/join (map |(string (/ (math/round (* $ 10.0)) 10.0)) times) " "))
  (printf "mean: %.1f ms" (/ (sum times) 3)))
