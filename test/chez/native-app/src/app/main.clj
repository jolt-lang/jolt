(ns app.main
  (:require [jolt.native :refer [native defnative linked-regions]]))

(defnative sum-of-squares [xs]
  (loop [i 0 s 0.0] (if (< i (alength xs)) (recur (inc i) (+ s (* (aget xs i) (aget xs i)))) s)))

(defn norm [^doubles xs]
  (native [^doubles xs] (Math/sqrt (sum-of-squares xs))))

(defn scaled [^doubles xs ^double k]
  (native [^doubles xs ^double k]
    (let [a (aclone xs)]
      (dotimes [i (alength a)] (aset a i (* k (aget a i))))
      a)))

(defn add [a b]
  (native [^long a ^long b] (+ a b)))

(defn lsum [^longs ls]
  (native [^longs ls]
    (loop [i 0 s 0] (if (< i (alength ls)) (recur (inc i) (+ s (aget ls i))) s))))

(defn squares [n]
  (native [^long n]
    (let [a (long-array n)] (dotimes [i n] (aset a i (* i i))) a)))

(defn sq-unsafe [x]
  (native ^:unsafe [^double x] (* x x)))

(defn spin [n]
  (native [^long n]
    (loop [i 0 s 0.0] (if (< i n) (recur (inc i) (+ s (Math/sqrt (double i)))) s))))

(defn gc-during-region
  "A safe region lets other threads collect while it runs: a collection forced
  while one spins finishes before it does. An ^:unsafe one would hold it."
  []
  (let [done (promise)
        f (future (let [r (spin 300000000)] (deliver done (System/nanoTime)) r))]
    (Thread/sleep 100)
    (System/gc)
    (let [gc-at (System/nanoTime)]
      @f
      (< gc-at @done))))

(defn -main [& _]
  (println (norm (double-array [3 4])))
  (println (vec (scaled (double-array [1 2 3]) 2.0)))
  (println (add 2 3.9))
  (println (try (add Long/MAX_VALUE 1) (catch ArithmeticException e (.getMessage e))))
  (println (try (norm [1 2]) (catch ClassCastException _ "ClassCastException")))
  (println (try (aget (scaled (double-array [1]) 1.0) 1) (catch ArrayIndexOutOfBoundsException e (.getMessage e))))
  (println (lsum (long-array [1 2 3])))
  (println (vec (squares 4)))
  (println (sq-unsafe 3.0))
  (println "gc during region" (gc-during-region))
  (println "linked" (sort (distinct (vals (linked-regions))))))
