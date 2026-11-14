;; A monitor release signals one waiting thread, and the owner it hands to must
;; wake the next (jolt#1654). A broken hand-off strands a waiter with the monitor
;; free, so every round runs under a deadline and fails instead of hanging.
;; Counters are plain array elements, so a lost increment is a lost exclusion.
;; Sections spin or sleep briefly so the waiters pile up.
(import '(java.util.concurrent.locks ReentrantLock)
        '(java.util.concurrent TimeUnit))

(def failures (atom []))
(defn fail! [& msg] (swap! failures conj (apply str msg)))

(defn await-all
  "Deref every future with one shared deadline; answers the count that missed it."
  [fs ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (count (remove #(not= ::timeout (deref % (max 1 (- deadline (System/currentTimeMillis))) ::timeout)) fs))))

(defn crowd-round [label enter! exit! n per]
  (let [cell (long-array 1)
        fs (doall (for [_ (range n)]
                    (future (dotimes [_ per]
                              (enter!)
                              (try (let [v (aget cell 0)]
                                     (Thread/sleep 0 50000)
                                     (aset cell 0 (inc v)))
                                   (finally (exit!)))))))
        missed (await-all fs 20000)]
    (cond (pos? missed) (fail! label ": " missed " of " n " futures stranded")
          (not= (* n per) (aget cell 0)) (fail! label ": count " (aget cell 0) " want " (* n per)))))

(defn lock-round []
  (let [lk (ReentrantLock.)]
    (crowd-round "ReentrantLock" #(.lock lk) #(.unlock lk) 64 4)
    (when (.isLocked lk) (fail! "ReentrantLock: still locked after the round"))))

(defn locking-round []
  (let [o (Object.) cell (long-array 1)
        fs (doall (for [_ (range 64)]
                    (future (dotimes [_ 4]
                              (locking o
                                (let [v (aget cell 0)] (Thread/sleep 0 50000) (aset cell 0 (inc v))))))))
        missed (await-all fs 20000)]
    (cond (pos? missed) (fail! "locking: " missed " futures stranded")
          (not= 256 (aget cell 0)) (fail! "locking: count " (aget cell 0)))))

(defn reentrant-round []
  (let [lk (ReentrantLock.) cell (long-array 1)
        fs (doall (for [_ (range 32)]
                    (future (.lock lk)
                            (try (.lock lk)
                                 (try (let [v (aget cell 0) h (.getHoldCount lk)]
                                        (Thread/sleep 0 50000)
                                        (when (not= 2 h) (fail! "reentrant: hold count " h))
                                        (aset cell 0 (inc v)))
                                      (finally (.unlock lk)))
                                 (finally (.unlock lk))))))
        missed (await-all fs 20000)]
    (cond (pos? missed) (fail! "reentrant: " missed " futures stranded")
          (not= 32 (aget cell 0)) (fail! "reentrant: count " (aget cell 0))
          (.isLocked lk) (fail! "reentrant: still locked"))))

(defn mixed-acquire-round []
  ;; Waiters queue behind a holder: a short tryLock gives up, the interrupted
  ;; acquire throws, everyone else gets the lock.
  (let [lk (ReentrantLock.) held (promise) release (promise) cell (long-array 1)
        holder (future (.lock lk) (deliver held true) @release (.unlock lk))
        _ @held
        plain (doall (for [_ (range 16)] (future (.lock lk) (try (aset cell 0 (inc (aget cell 0))) (finally (.unlock lk))))))
        short-try (future (.tryLock lk 5 TimeUnit/MILLISECONDS))
        long-try (doall (for [_ (range 4)]
                          (future (if (.tryLock lk 10 TimeUnit/SECONDS)
                                    (try (aset cell 0 (inc (aget cell 0))) true (finally (.unlock lk)))
                                    false))))
        intr-thread (promise)
        intr (future (deliver intr-thread (Thread/currentThread))
                     (try (.lockInterruptibly lk) (.unlock lk) :acquired
                          (catch InterruptedException _ :interrupted)))
        _ (Thread/sleep 20)
        _ (.interrupt ^Thread @intr-thread)
        intr-result (deref intr 5000 ::timeout)
        short-result (deref short-try 5000 ::timeout)]
    (deliver release true)
    (let [missed (await-all (concat [holder] plain long-try) 20000)]
      (when (not= false short-result) (fail! "tryLock 5ms against a held lock answered " short-result))
      (when (not= :interrupted intr-result) (fail! "lockInterruptibly interrupted answered " intr-result))
      (cond (pos? missed) (fail! "mixed: " missed " acquirers stranded")
            (not= [true true true true] (mapv deref long-try)) (fail! "mixed: a 10s tryLock gave up")
            (not= 20 (aget cell 0)) (fail! "mixed: count " (aget cell 0))
            (.isLocked lk) (fail! "mixed: still locked")))))

(defn wait-notify-round []
  ;; Object.wait releases the monitor while other threads keep entering it.
  (let [o (Object.) ready (atom 0) go (atom false) cell (long-array 1)
        waiters (doall (for [_ (range 8)]
                         (future (locking o
                                   (swap! ready inc)
                                   (while (not @go) (.wait o 1000))
                                   (aset cell 0 (inc (aget cell 0)))))))
        entrants (doall (for [_ (range 32)]
                          (future (dotimes [_ 4]
                                    (locking o (let [v (aget cell 0)] (Thread/sleep 0 20000) (aset cell 0 (inc v))))))))]
    (loop [i 0] (when (and (< @ready 8) (< i 2000)) (Thread/sleep 1) (recur (inc i))))
    (locking o (reset! go true) (.notifyAll o))
    (let [missed (await-all (concat waiters entrants) 20000)]
      (cond (pos? missed) (fail! "wait/notify: " missed " stranded")
            (not= (+ 8 128) (aget cell 0)) (fail! "wait/notify: count " (aget cell 0))))))

(def rounds (or (some-> (System/getenv "MONITOR_HANDOFF_ROUNDS") parse-long) 10))
(dotimes [_ rounds]
  (when (empty? @failures)
    (lock-round) (locking-round) (reentrant-round) (mixed-acquire-round) (wait-notify-round)))

(if (empty? @failures)
  (println "MONITOR-HANDOFF OK")
  (do (println "MONITOR-HANDOFF FAIL:" (first @failures)) (shutdown-agents) (System/exit 1)))
(shutdown-agents)
