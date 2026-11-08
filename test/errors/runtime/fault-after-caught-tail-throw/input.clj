(ns input)

;; As fault-after-caught-throw, with the caught throw in tail position.
(defn tail-thrower []
  (let [e (ex-info "x" {}) y 1]
    (throw e)))

(defn fault []
  (try (tail-thrower) (catch Exception _ nil))
  (let [z (count (str "a"))]
    (+ 1 (/ 1 (dec z)))))

(fault)
