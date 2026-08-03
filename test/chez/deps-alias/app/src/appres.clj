(ns appres
  "io/resource must answer an ABSOLUTE file: URL for a file on a source root, as
  the JVM classloader's always is. Source roots are usually relative (\"./src\"),
  and \"file:./src/x\" is not a valid absolute URL — resolving another name
  against it throws MalformedURLException \"no protocol\"."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn -main [& _]
  (let [u (str (io/resource "appres.clj"))]
    (println "absolute:" (str/starts-with? u "file:/")
             "clean:" (not (str/includes? u "/./")))))
