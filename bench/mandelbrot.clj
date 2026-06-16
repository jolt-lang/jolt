;; mandelbrot — pure floating-point compute: for each point of an NxN grid,
;; iterate z = z^2 + c up to a cap and count iterations. No allocation, no
;; dispatch, no collections in the hot loop — just double arithmetic and tight
;; recur loops. This isolates the irreducible-math axis the ray tracer is bound
;; on (where devirt/alloc passes measured flat), so it tracks native-arith codegen
;; (jolt-3pl) and loop quality directly.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   jolt -m mandelbrot 1000     (JOLT_DIRECT_LINK=1 JOLT_WHOLE_PROGRAM=1)
(ns mandelbrot
  (:require [jolt.png :as png]))

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

;; --- PNG demo (jolt.png) --------------------------------------------------
;; Render a real picture of the set, reusing count-point as the kernel. `render`
;; is a separate -main subcommand so the numeric-arg bench path is untouched.

(defn- color
  "Escape-iteration count -> RGB. In-set points (n>=cap) are black; faster
  escapes run through a warm gradient."
  [n cap]
  (if (>= n cap)
    [0 0 0]
    (let [t (/ (double n) cap)]
      [(int (* 255 (min 1.0 (* 3.0 t))))
       (int (* 255 (min 1.0 (max 0.0 (* 3.0 (- t 0.33))))))
       (int (* 255 (min 1.0 (max 0.0 (* 3.0 (- t 0.66))))))])))

(defn render!
  "Render a size×size view of the Mandelbrot set to a PNG at path."
  [path size]
  (let [w size h size cap 1000
        img (png/image w h)]
    (doseq [py (range h)]
      (doseq [px (range w)]
        (let [cr (- (* 3.5 (/ (double px) w)) 2.5)        ; real ∈ [-2.5, 1.0]
              ci (- (* 2.8 (/ (double py) h)) 1.4)        ; imag ∈ [-1.4, 1.4]
              [r g b] (color (count-point cr ci cap) cap)]
          (png/put! img r g b))))
    (png/write img w h path)
    (println "wrote" path (str w "×" h ", cap " cap))))

(defn- run-bench [args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 1000)]
    (dotimes [_ 2] (run (quot n 2)))                     ; warmup
    (let [runs 3
          times (mapv (fn [_]
                        (let [t0 (System/nanoTime)
                              r (run n)
                              ms (/ (- (System/nanoTime) t0) 1000000.0)]
                          [ms r]))
                      (range runs))
          mss (mapv first times)
          mean (/ (reduce + mss) runs)]
      (println "mandelbrot n" n "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))

(defn -main [& args]
  (if (= (first args) "render")
    (render! (or (second args) "mandelbrot.png")
             (if (nth args 2 nil) (Integer/parseInt (nth args 2)) 600))
    (run-bench args)))
