(ns app.core)

;; A ^Record hint on a parameter is a declaration, not a proof: Clojure's (:x p)
;; and record?/nil? look at the value whatever the hint says. Each value goes
;; through an atom so inference can't prove its type and only the hint is left.

(defn- opaque [x] @(atom x))

(defrecord Point [x y])
(defrecord Circle [r c])
(defrecord Vec2 [^double x ^double y])

(defn px [^Point p] (:x p))
(defn py [^Point p] (get p :y))
(defn rec? [^Point p] (record? p))
(defn absent? [^Point p] (nil? p))
(defn len2 [^Vec2 v] (+ (* (:x v) (:x v)) (* (:y v) (:y v))))
(defn both [^Point a ^Vec2 b] [(:x a) (:y b)])
;; a closure's hinted param and a spliced call site take the same rule
(def via-fn (fn [^Point p] (:y p)))
(defn spliced [m] (px m))
;; hinted fns nested in a hinted fn: each guard holds for its own param whatever
;; the enclosing one found
(defn nest [^Point a] (fn [^Vec2 b] (fn [^Point c] [(:x a) (:y b) (:y c)])))

(defn -main [& _]
  (println (px (opaque {:x 1 :y 2})))
  (println (px (opaque nil)))
  (println (px (opaque (->Circle 5 6))))
  (println (py (opaque {:y 9})))
  (println (rec? (opaque {:x 1})))
  (println (absent? (opaque nil)))
  (println (len2 (opaque {:x 3 :y 4})))
  (println (both (opaque {:x :a}) (opaque (->Vec2 1.0 2.0))))
  (println (via-fn (opaque {:y :fn})))
  (println (spliced (opaque {:x :sp})))
  (println (((nest (opaque {:x :m})) (opaque (->Vec2 1.0 2.0))) (opaque (->Point 3 4))))
  (println (((nest (opaque (->Point 7 8))) (opaque {:y :w})) (opaque {:y :z})))
  (println (((nest (opaque (->Point 7 8))) (opaque (->Vec2 3.0 4.0))) (opaque nil)))
  (println (((nest (opaque (->Point 7 8))) (opaque (->Vec2 3.0 4.0))) (opaque (->Point 5 6))))
  ;; and a value the hint holds for still reads its fields
  (println (px (opaque (->Point 7 8))) (len2 (opaque (->Vec2 3.0 4.0)))))
