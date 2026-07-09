;; STM threaded tests: isolation, txn-leak prevention, io! in txn with threads.
;; Prints STM OK when all pass.
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

;; --- Txn-leak (#5): future spawned inside dosync must not inherit *txn*
(let [r (ref 0)]
  (try
    (dosync
      (deref (future (ref-set r 1))))
    (catch Exception e
      (chk "txn-leak: ref-set in future throws IllegalStateException"
           (instance? IllegalStateException e))))
  (chk "txn-leak: ref unchanged after failed future ref-set"
       (= @r 0)))

;; --- io! in txn with thread (#6): future inside dosync must not throw io!
(let [r (ref 0)]
  (dosync
    (deref (future (io! :ok))))
  ;; if we reach here, io! inside future didn't throw inside the dosync's txn
  (chk "io!-in-future: io! inside future inside dosync does not throw" true))

(if (empty? @failures)
  (println "STM OK")
  (doseq [f @failures] (println "FAIL:" f)))
