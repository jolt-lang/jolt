(ns input)

;; The catch does not match, so it rethrows: the trace is still the throw's.
(defn inner [x] (let [r (/ 1 x)] (inc r)))
(defn mid [x] (let [v (try (inner x) (catch IllegalStateException e :no))] (str v)))
(defn outer [] (let [z (mid 0)] (str z)))

(outer)
