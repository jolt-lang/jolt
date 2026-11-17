;; gx.c from the cache, before gx.a is loaded, then gx.a, then gx.b.
(ns gx.main2 (:require [gx.c :as c]))
(defn -main [& _]
  (println "main2 before"
           (try (c/call-g 1) (catch Throwable t [(.getName (class t)) (ex-message t)]))
           (.getName (class (c/ref-g))))
  (require 'gx.a)
  (println "main2 after a" (c/call-g 2) ((c/ref-g) 3))
  (require 'gx.b)
  (println "main2 after b" ((resolve 'gx.b/call-f) 4)))
