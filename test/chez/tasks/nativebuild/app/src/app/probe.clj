(ns app.probe
  (:require [jolt.ffi :as ffi]))

;; Probes for a symbol nothing defines and requires nothing of the library: a
;; probe answers nil without opening (or building) the dependency's native.
(defn -main [& _]
  (println "probe" (pr-str (ffi/find-symbol "no_such_optional_symbol_xyz"))
           (count (jolt.host/pending-native-roots))))
