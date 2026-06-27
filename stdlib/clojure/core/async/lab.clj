;; clojure.core.async.lab — experimental features over the channel primitives.
;;
;; multiplex/broadcast are ported as go-loops over jolt's primitives (the JVM
;; versions reify the impl handler protocol, which jolt does not expose).

(ns clojure.core.async.lab
  (:require [clojure.core.async :as async]))

(defn multiplex
  "Returns a read port that yields values from whichever of ports is ready. A
  closed port is dropped; the multiplex port closes once all ports have closed."
  [& ports]
  (let [out (async/chan)]
    (async/go-loop [cs (vec ports)]
      (if (pos? (count cs))
        (let [[v c] (async/alts! cs)]
          (if (nil? v)
            (recur (filterv #(not= c %) cs))
            (do (async/>! out v)
                (recur cs))))
        (async/close! out)))
    out))

(defn broadcast
  "Returns a write port that writes each value to all of ports. A write parks until
  the value has been written to every port."
  [& ports]
  (let [in (async/chan)]
    (async/go-loop []
      (let [v (async/<! in)]
        (when (some? v)
          (doseq [p ports] (async/>! p v))
          (recur))))
    in))
