(ns input)

(def lock (Object.))

(defn f []
  (loop [i 0]
    (locking lock
      (if (< i 3)
        (recur (inc i))
        i))))

(println :compiled)
