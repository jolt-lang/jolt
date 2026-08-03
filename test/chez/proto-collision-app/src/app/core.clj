(ns app.core
  (:require [pa.core :as a]
            [pb.core :as b]))

;; A protocol's identity is its defining namespace plus its name, so pa.core/Greet
;; and pb.core/Greet are distinct interfaces and each namespace's extend stands on
;; its own. Reference Clojure prints exactly this line.
(defn -main [& _]
  (println (str "A=" (a/greet 1) " B=" (b/greet 1)
                " reified=" (a/greet (a/reified))
                " cross-instance=" (instance? pb.core.Greeter (a/reified))
                " own-instance=" (instance? pa.core/Greet (a/reified))
                " satisfies=" (satisfies? pa.core/Greet (a/reified)))))
