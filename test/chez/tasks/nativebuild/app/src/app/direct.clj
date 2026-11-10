(ns app.direct
  (:require [jolt.ffi :as ffi]))

;; Binds the library's C symbol itself and requires nothing of the library: the
;; first call to a symbol nothing loaded defines loads the dependencies' pending
;; natives and looks again.
(ffi/defcfn answer "jbt_answer" [] :int)

(defn -main [& _] (println "direct" (answer)))
