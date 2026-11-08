(ns input)

;; A tail call into host code that calls back, then a throw not in tail position.
;; via-host's site is still in the slot at the throw; it is not the innermost frame,
;; and only-tail-caller never ran.
(defn boom []
  (let [e (ex-info "from boom" {})]
    (+ 1 (throw e))))

(defn via-host [f]
  (with-bindings* {} f))

(defn only-tail-caller [f]
  (via-host f))

(via-host boom)
