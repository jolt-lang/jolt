;; Object.wait's release must not clobber the count of the next owner (jolt-are).
;;
;; monitor-wait-enter! (host/chez/java/concurrency.ss) freed the owner word and
;; only then zeroed the hold count. A fast enter claims a free word without the
;; monitor's bk mutex and writes its own count of 1, so the waiter's late 0 could
;; land on top of it; that owner's exit then took the count to -1, never freed
;; the word, and every later contender blocked for good.
;;
;; ConcurrentHashMap.merge on one key from four threads drives exactly that mix:
;; each merge claims the key, and the threads that find it claimed wait on the
;; map's monitor while the others enter and leave it on the fast path. Before the
;; fix this hung within a few rounds; smoke.sh's timeout turns a hang into a fail.
(def plus (reify java.util.function.BiFunction (apply [_ a b] (+ a b))))

(defn round []
  (let [m (java.util.concurrent.ConcurrentHashMap.)
        ts (doall (for [_ (range 4)]
                    (doto (Thread. (fn [] (dotimes [_ 500] (.merge m :k 1 plus))))
                      (.start))))]
    (run! #(.join %) ts)
    (.get m :k)))

(let [bad (remove #(= 2000 %) (repeatedly 40 round))]
  (if (empty? bad)
    (println "MONITOR-WAIT-COUNT OK")
    (println "MONITOR-WAIT-COUNT FAIL: merged totals" (vec bad))))
