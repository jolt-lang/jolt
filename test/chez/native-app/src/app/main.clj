(ns app.main
  (:require [jolt.native :refer [native linked-regions]]))

(defn norm [^doubles xs]
  (native [^doubles xs]
    (let [n (alength xs)]
      (loop [i 0 s 0.0]
        (if (< i n) (recur (inc i) (+ s (* (aget xs i) (aget xs i)))) (Math/sqrt s))))))

(defn scaled [^doubles xs ^double k]
  (native [^doubles xs ^double k]
    (let [a (aclone xs)]
      (dotimes [i (alength a)] (aset a i (* k (aget a i))))
      a)))

(defn add [a b]
  (native [^long a ^long b] (+ a b)))

(defn -main [& _]
  (println (norm (double-array [3 4])))
  (println (vec (scaled (double-array [1 2 3]) 2.0)))
  (println (add 2 3.9))
  (println (try (add Long/MAX_VALUE 1) (catch ArithmeticException e (.getMessage e))))
  (println (try (norm [1 2]) (catch ClassCastException _ "ClassCastException")))
  (println (try (aget (scaled (double-array [1]) 1.0) 1) (catch ArrayIndexOutOfBoundsException e (.getMessage e))))
  (println "linked" (sort (distinct (vals (linked-regions))))))
