;; STM threaded tests: isolation, txn-leak prevention, io! in txn with threads.
;; Prints STM OK when all pass.
;; Rounds 2-3 add txn-leak (future inside dosync) and agent-send-hold tests.
(def failures (atom []))
(defn chk [label ok] (when-not ok (swap! failures conj label)))

;; --- Isolation (#4): thread A writes + sleep + throw, thread B reads mid-txn
(let [r (ref 0)
      done (promise)]
  (future
    (try
      (dosync
        (ref-set r 5)
        (Thread/sleep 400)
        (throw (ex-info "rollback" {})))
      (catch Exception e)))
  (Thread/sleep 100)  ;; let A get inside dosync and set r to 5
  (deliver done @r)   ;; B reads — must see 0 (isolation), not 5
  (Thread/sleep 500)  ;; wait for A's txn to unwind
  (chk "isolation: B sees committed value during A's uncommitted txn"
       (= @done 0))
  (chk "isolation: final value is rolled back"
       (= @r 0)))

(if (empty? @failures)
  (println "STM OK")
  (doseq [f @failures] (println "FAIL:" f)))
