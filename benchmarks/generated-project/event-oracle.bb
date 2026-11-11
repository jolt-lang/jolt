#!/usr/bin/env bb
(require '[babashka.fs :as fs]
         '[clojure.edn :as edn])

;; Reconstruct each account's history independently of the incremental index.
(let [{:keys [ingest updates queries]} (edn/read-string (slurp (fs/file (fs/parent (fs/absolutize *file*)) "events.edn")))
      histories (group-by :account (concat ingest updates))
      index (into {} (for [[account events] histories]
                       [account {:count (count events)
                                 :total (reduce + (map :amount events))
                                 :last-id (:id (peek events))}]))]
  (prn {:index index :answers (mapv index queries)}))
