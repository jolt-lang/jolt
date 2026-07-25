(ns tooltool
  "Tool fixture for the -T gate: reports whether the PROJECT's own src root is
  on the source roots. Under -T it must not be (the tool alias replaces the
  project basis); under -X it must be."
  (:require [clojure.string :as str]))
(defn report [_m]
  (println "tool: project-src-on-roots?"
           (boolean (some #(str/ends-with? % "deps-alias/app/src") (jolt.host/source-roots)))))
