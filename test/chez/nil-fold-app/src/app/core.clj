(ns app.core)

(defn -main [& _]
  ;; inference proves x is :nil, so under --opt nil? must fold true and some?
  ;; false. Pre-fix the fold was inverted: this printed :b / :y.
  (println (let [x nil] (if (nil? x) :a :b)))
  (println (let [x nil] (if (some? x) :y :n))))
