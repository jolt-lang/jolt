(ns app.iterablereentry
  (:require [jolt.scheme :as scheme])
  (:import [java.util ArrayList Iterator]))

(def capture (scheme/proc "call/cc"))

(defn ^:redef decode-rows [mapper option limit source]
  (into [] (comp (map (mapper)) (if (option) (take (limit)) identity)) (source)))

(defn decode [mapper limit rows]
  (decode-rows (constantly mapper) (constantly limit) (constantly limit) (constantly rows)))

(defn mapper-reentry [limit at expected]
  (let [saved (atom nil)
        outputs (atom [])
        visited (atom [])
        out (decode (fn [x]
                      (swap! visited conj x)
                      (if (= at x)
                        (capture (fn [k] (reset! saved k) :first))
                        x))
                    limit (ArrayList. [1 2]))
        completed (swap! outputs conj out)]
    (case (count completed)
      1 (@saved :second)
      2 (@saved :third)
      (do (assert (= expected completed))
          (assert (= (range 1 (inc at)) @visited))
          (println :mapper limit completed)))))

(defn iterator-reentry [limit]
  (let [saved (atom nil)
        outputs (atom [])
        events (atom [])
        note! #(swap! events conj %)
        rows (reify Iterable
               (iterator [_]
                 (note! :iterator)
                 (let [i (volatile! 0)]
                   (reify Iterator
                     (hasNext [_] (note! [:has @i]) (< @i 4))
                     (next [_]
                       (let [x @i]
                         (note! [:next x])
                         (vswap! i inc)
                         (if (= x 2)
                           (capture (fn [k] (reset! saved k) :first))
                           x)))))))
        out (decode (fn [x] (note! [:map x]) x) limit rows)
        completed (swap! outputs conj out)]
    (note! [:complete out])
    (case (count completed)
      1 (@saved :second)
      2 (@saved :third)
      (do (assert (= (if limit
                       [[0 1 :first] [0] [0]]
                       [[0 1 :first 3] [0 1 :second] [0 1 :third]])
                     completed))
          (println :iterator limit @events)))))

(defn option-reentry []
  (let [choose (atom nil)
        fetch (atom nil)
        outputs (atom [])
        events (atom [])
        rows (ArrayList. [1 2])
        out (decode-rows #(do (swap! events conj :mapper) identity)
                         #(let [bounded? (capture (fn [k] (reset! choose k) true))]
                            (swap! events conj [:option bounded?])
                            bounded?)
                         #(do (swap! events conj :limit) 1)
                         #(let [source (capture (fn [k] (compare-and-set! fetch nil k) rows))]
                            (swap! events conj :source)
                            source))
        completed (swap! outputs conj out)]
    (case (count completed)
      1 (@choose false)
      2 (@fetch rows)
      (do (assert (= [[1] [1 2] [1]] completed))
          (println :option completed @events)))))

(defn -main [callback & _]
  (case callback
    "mapper" (do (mapper-reentry nil 2 [[1 :first] [1 :second] [1 :third]])
                 (mapper-reentry 2 2 [[1 :first] [1] [1]])
                 (mapper-reentry 1 1 [[:first] [] []])
                 (mapper-reentry 0 1 [[] [] []]))
    "iterator" (do (iterator-reentry nil)
                   (iterator-reentry 3))
    "option" (option-reentry)))
