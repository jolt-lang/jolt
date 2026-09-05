(ns input)

(defn sum-to [n]
  (loop [i 0 acc 0]
    (if (< i n)
      (+ 1 (recur (inc i) (+ acc i)))
      acc)))

(println :compiled)
