(ns input)

;; A handler that does its own work and then rethrows: the trace is the throw's.
(def seen (atom 0))
(defn inner [x] (let [r (/ 1 x)] (inc r)))
(defn mid [x] (let [v (try (inner x) (catch ArithmeticException e (swap! seen inc) (throw e)))] (str v)))
(defn outer [] (let [z (mid 0)] (str z)))

(outer)
