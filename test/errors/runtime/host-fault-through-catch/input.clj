(ns input)

;; A fault the host raised, passing through a catch that does not match it.
(defn inner [x] (let [r (double-array 2)] (+ 1 (aget r x))))
(defn mid [x] (let [v (try (inner x) (catch IllegalStateException e :no))] (str v)))
(defn outer [] (let [z (mid 5)] (str z)))

(outer)
