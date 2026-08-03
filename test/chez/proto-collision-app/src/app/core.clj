(ns app.core
  (:require [pa.core :as a]
            [pb.core :as b]))

;; Reference Clojure prints "A=:from-A B=:from-B": the two protocols are distinct
;; interfaces, so each namespace's extend stands on its own. jolt keys a
;; protocol's method table by the protocol's SIMPLE name, so pb.core's extend
;; replaces pa.core's and both read :from-B — bead jolt-ewmt.
(defn -main [& _]
  (println (str "A=" (a/greet 1) " B=" (b/greet 1))))
