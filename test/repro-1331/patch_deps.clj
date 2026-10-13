;; Add a :repro alias to kmet's deps.edn: otel's sources (checked out beside
;; kmet) plus the tools.logging it needs.
(require '[clojure.edn :as edn])
(let [d (edn/read-string (slurp "deps.edn"))]
  (spit "deps.edn"
        (pr-str (assoc-in d [:aliases :repro]
                          {:extra-paths ["../otel/src"]
                           :extra-deps {'org.clojure/tools.logging
                                        {:git/url "https://github.com/jolt-lang/logging"
                                         :git/sha "ec6747e55583118d881faf4ab41e68d7e587ce4c"}}})))
  (println "deps.edn: added :repro"))
