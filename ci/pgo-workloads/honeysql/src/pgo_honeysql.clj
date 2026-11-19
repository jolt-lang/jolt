;; pgo-honeysql — honey.sql/format throughput, the loops honeysql's own
;; cache_test times, uncached: a small query with a random id and a large one
;; with joins, a nested where, grouping and ordering, plus VALUES with
;; matching widths and a ragged ROW-marked input. ci/pgo-bench.sh builds it
;; as a library workload (honeysql v2.7.1437, pinned in deps.edn).
;;   jolt run -m pgo-honeysql 20000
(ns pgo-honeysql
  (:refer-clojure :exclude [group-by])
  (:require [honey.sql :as sql]
            [honey.sql.helpers
             :refer [select-distinct from join left-join right-join where
                     group-by having order-by limit offset]]))

(def big-complicated-map
  (-> (select-distinct :f.* :b.baz :c.quux [:b.bla "bla-bla"]
                       [[:now]] [[:raw "@x := 10"]])
      (from [:foo :f] [:baz :b])
      (join :draq [:= :f.b :draq.x]
            :eldr [:= :f.e :eldr.t])
      (left-join [:clod :c] [:= :f.a :c.d])
      (right-join :bock [:= :bock.z :c.e])
      (where [:or
              [:and [:= :f.a "bort"] [:not= :b.baz [:param :param1]]]
              [:and [:< 1 2] [:< 2 3]]
              [:in :f.e [1 [:param :param2] 3]]
              [:between :f.e 10 20]])
      (group-by :f.a :c.e)
      (having [:< 0 :f.e])
      (order-by [:b.baz :desc] :c.quux [:f.a :nulls-first])
      (limit 50)
      (offset 10)))

(def regular-values
  {:insert-into :table :columns [:id :amount] :values [[1 10] (list 2 20)]})

(def ragged-values
  {:insert-into :table :columns [:id :amount] :values [:row (list 1 10) [2]]})

(defn run [n]
  (dotimes [_ n]
    (sql/format {:select [:*] :from [:table] :where [:= :id (rand-int 10)]})
    (sql/format big-complicated-map {:params {:param1 "gabba" :param2 (rand-int 10)}})
    (sql/format regular-values)
    (sql/format ragged-values)))

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 20000)]
    (run (quot n 10))                                     ; warmup
    (let [runs 3
          mss (mapv (fn [_]
                      (let [t0 (System/nanoTime)]
                        (run n)
                        (/ (- (System/nanoTime) t0) 1000000.0)))
                    (range runs))
          mean (/ (reduce + mss) runs)]
      (println "format" (sql/format big-complicated-map {:params {:param1 "gabba" :param2 2}}))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
