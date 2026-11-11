#!/usr/bin/env bb
(ns generated-project.collection-oracle
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]))

;; Model each returned value arithmetically. No benchmarked collection or
;; generated kernel is used to compute the expected checksum.
(defn contribution [op n k]
  (case op
    ("collections/control/probe" "collections/vector/conj") k
    ("collections/map/get-hit" "collections/vector/nth"
     "collections/map/build-transient" "collections/vector/build-transient") (inc k)
    ("collections/map/assoc-new" "collections/map/assoc-existing" "collections/vector/assoc") (+ k 2)
    ("collections/map/get-miss" "collections/map/dissoc"
     "collections/set/contains-miss" "collections/set/disj") 0
    ("collections/set/contains-hit" "collections/set/conj" "collections/set/build-transient") 1
    "collections/vector/pop" (if (= k (dec n)) 0 (inc k))
    ("collections/map/reduce-kv" "collections/vector/reduce") (+ k (quot (* n (inc n)) 2))
    "collections/set/reduce" (+ k (quot (* n (dec n)) 2))))

(defn expected [{:keys [op size offset stride bulk]} input budget]
  (let [calls (if bulk (max 1 (quot budget size)) budget)]
    (loop [i 0 sum 0]
      (if (< i calls)
        (let [k (mod (+ input offset (* i stride)) size)]
          (recur (inc i) (+ sum (contribution op size k))))
        {:calls calls :checksum sum}))))

(when (= (fs/absolutize *file*) (some-> (System/getProperty "babashka.file") fs/absolutize))
  (let [[input budget] (mapv parse-long *command-line-args*)
        profiles (:profiles (edn/read-string (slurp (fs/file (fs/parent (fs/absolutize *file*)) "manifest.edn"))))]
    (when-not (and input budget (<= 0 input 1000002) (<= 1 budget 1000000))
      (throw (ex-info "INPUT must be 0..1000002 and BUDGET must be 1..1000000" {})))
    (prn (mapv #(expected % input budget) profiles))))
