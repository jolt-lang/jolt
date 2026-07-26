(ns app.core)

;; A protocol call whose receiver is a record-or-NIL field. devirt resolves the impl
;; by the static type tag and a direct-link site caches the first resolution, so
;; devirtualizing a nilable receiver made the cached Circle impl serve a later nil
;; receiver — printing 3 where Clojure raises IllegalArgumentException ("No
;; implementation of method: :area of protocol: #'app.core/Shape found for class:
;; nil"). Circle has NO fields on purpose: with a field, the impl's destructuring
;; happens to trip the non-nil slot accessor and the call throws by accident, hiding
;; the wrong answer.
(defprotocol Shape (area [s]))
(defrecord Circle [] Shape (area [_] 3))
(defrecord Holder [shape])

(defn mk [n] (->Holder (if (zero? n) nil (->Circle))))
(defn use-it [h] (area (:shape h)))

(defn -main [& _]
  ;; iteration 0 passes a Circle (warms the site), iteration 1 passes nil.
  (dotimes [i 2]
    (println (try (use-it (mk (- 1 i)))
                  (catch IllegalArgumentException _ :no-impl)))))
