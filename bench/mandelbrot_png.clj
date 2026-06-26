;; mandelbrot picture demo — renders a real image of the set to a PNG via
;; jolt.png (FFI), reusing mandelbrot/count-point as the kernel. jolt-only (the
;; benchmark in mandelbrot.clj stays portable for the JVM reference).
;;   joltc run -m mandelbrot-png [path] [size]
(ns mandelbrot-png
  (:require [mandelbrot :as m]
            [jolt.png :as png]))

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
              [r g b] (color (m/count-point cr ci cap) cap)]
          (png/put! img r g b))))
    (png/write img w h path)
    (println "wrote" path (str w "×" h ", cap " cap))))

(defn -main [& args]
  (render! (or (first args) "mandelbrot.png")
           (if (second args) (Integer/parseInt (second args)) 600)))
