(ns app.core)

(defn ^:redef counter [n]
  (loop [i 0 m {:count 0}]
    (if (= i n) m
      (recur (inc i) (update m :count inc)))))

(defn ^:redef sibling-edits [m k a b]
  (-> m (assoc-in [:root k :a] a) (assoc-in [:root k :b] b)))

(defn ^:redef row-widths [xs]
  (into [] (comp (filter sequential?) (map count)) xs))

(defn ^:redef decode-rows [parse-row max-rows rows]
  (into [] (comp (map parse-row) (if max-rows (take max-rows) identity)) rows))

(defn -main [& _]
  (let [rows (java.util.ArrayList. [1 2 3])
        widths [[1 2] :skip [3]]]
    (assert (= {:count 2} (counter 2)))
    (assert (= {:root {:shapes {:a 1 :b 2}}} (sibling-edits {} :shapes 1 2)))
    (assert (= [2 1] (row-widths widths)))
    (assert (= [2 3] (decode-rows inc 2 rows)))
    (with-redefs [clojure.core/update (fn [m k _] (assoc m k :rebound))
                  clojure.core/assoc-in (fn [m path v] (assoc m (peek path) v))
                  clojure.core/comp (fn [_ _] (map (constantly :rebound)))]
      (assert (= {:count :rebound} (counter 2)))
      (assert (= {:a 1 :b 2} (sibling-edits {} :shapes 1 2)))
      (assert (= [:rebound :rebound :rebound] (row-widths widths)))
      (assert (= [:rebound :rebound :rebound] (decode-rows inc 2 rows)))))
  (println "GUARDS OK"))
