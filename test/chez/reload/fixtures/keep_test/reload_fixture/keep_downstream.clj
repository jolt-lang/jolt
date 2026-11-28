(ns reload-fixture.keep-downstream
  (:require
    reload-fixture.keep-upstream))

(defonce downstream-var
  (rand-int Integer/MAX_VALUE))