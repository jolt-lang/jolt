(ns app.core)

(defn checked-literals [x]
  [(try {(let [m {:v :k :x x} a m] (:v a)) 1 :k 2}
        (catch IllegalArgumentException _ :caught-map))
   (try [{:nested #{(let [m {:v :k :x x} a m] (:v a)) :k}}]
        (catch IllegalArgumentException _ :caught-set))])

(defn ^:redef complex-keys [] [{[:a] 1 [:b] 2}])

(defn -main [& _]
  ;; A checked literal must throw inside its handler on every call. A valid
  ;; constant at the same source site still returns one shared object.
  (when-not (= [:caught-map :caught-set] (checked-literals 1) (checked-literals 2))
    (throw (ex-info "Expected both literal constructors to throw at their use sites" {})))
  (when-not (identical? (complex-keys) (complex-keys))
    (throw (ex-info "Expected the successful constant to keep its identity" {})))
  ;; scalar-replace folds (:a {:a 1 :b (/ 1 0)}) -> 1 under --opt --direct-link,
  ;; discarding the throwing sibling. The divisor must still evaluate: / is not a
  ;; pure fn, so the map is kept, the ArithmeticException fires, and the catch
  ;; prints THROW OK instead of the folded 1.
  (println
    (try
      (:a {:a 1 :b (/ 1 0)})
      (catch ArithmeticException _ "THROW OK")))
  ;; Same for a literal :throw IR node as an unread map value: safe-op? admitted
  ;; :throw, so pure?/total? treated it as discardable and elim-let-structs
  ;; dropped the binding — the release binary printed 1 instead of throwing.
  ;; A throw is relocatable (safe-op?) but never pure/total.
  (println
    (try
      (let [m {:a 1 :b (throw "boom")}] (:a m))
      (catch :default _ "THROW2 OK"))))
