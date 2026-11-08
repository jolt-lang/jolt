(ns input)

;; A caught throw is over: the fault after it is fault's, and the thrower has left.
(defn nontail-thrower []
  (let [e (ex-info "x" {})]
    (+ 1 (throw e))))

(defn fault []
  (try (nontail-thrower) (catch Exception _ nil))
  (let [z (count (str "a"))]
    (+ 1 (/ 1 (dec z)))))

(fault)
