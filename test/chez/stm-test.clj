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
      ;; deref of a failed future wraps in ExecutionException (JVM parity,
      ;; jolt-mw44.9); the txn-leak ISE is its cause.
      (chk "txn-leak: ref-set in future throws IllegalStateException"
           (and (instance? java.util.concurrent.ExecutionException e)
                (instance? IllegalStateException (ex-cause e))))))
  (chk "txn-leak: ref unchanged after failed future ref-set"
       (= @r 0)))

;; --- io! in txn with thread (#6): future inside dosync must not throw io!
(let [r (ref 0)]
  (dosync
    (deref (future (io! :ok))))
  ;; if we reach here, io! inside future didn't throw inside the dosync's txn
  (chk "io!-in-future: io! inside future inside dosync does not throw" true))

;; --- PSL R4 cluster-1 confirmation: re-entrance is the *txn* join, never locking
;; A nested dosync on the SAME thread joins the outer transaction through the
;; *txn* thread parameter (jolt-sync) rather than starting a second one.
(let [r (ref 0)]
  (dosync
    (ref-set r 1)
    (dosync (alter r inc)))
  (chk "nested-dosync: inner dosync joins the outer txn without deadlock" (= @r 2)))

;; swap!'s user fn runs OUTSIDE the per-atom lock (CAS retry loop, atoms.ss).
;; Probes that converge: f may deref the atom it swaps (lock-free read), and
;; may reset! a DIFFERENT atom (would deadlock if any atom lock were held).
;; NOTE: mutating the SAME atom inside its own swap! fn livelocks the CAS
;; retry loop — identical to the JVM's Atom.swap; a documented anti-pattern.
(let [a (atom 10)]
  (swap! a (fn [x] (+ x @a)))
  (chk "atom-swap-read: swap! fn may deref the atom it swaps" (= @a 20)))
(let [a (atom 0) b (atom 0)]
  (swap! a (fn [x] (reset! b 1) (inc x)))
  (chk "atom-swap-cross: swap! fn may reset! another atom" (and (= @a 1) (= @b 1))))

;; --- PSL R4 cluster-2 confirmation: executor workers don't inherit the creator's txn
;; A pool created INSIDE a dosync forks its workers while the creating thread's
;; *txn* is live; Chez fork-inheritance hands the workers that record. Without an
;; explicit clear, a job calling ref-set outside a transaction would silently write
;; into the dead txn's log (never committed) instead of throwing ISE like the JVM.
(let [ex (dosync (Executors/newFixedThreadPool 1))
      r (ref 0)
      f (.submit ex (fn [] (ref-set r 1)))]
  (let [threw (try (.get f) false (catch Exception e true))]
    (chk "executor-txn: ref-set in executor job outside a txn throws IllegalStateException" threw)
    (chk "executor-txn: ref unchanged after job" (= @r 0)))
  (.shutdown ex))

;; --- PSL R4 cluster-2 confirmation: monitors are reentrant per thread (JVM parity)
;; A nested (locking x ...) on the same object from the same thread must re-enter
;; the per-object monitor, not deadlock on the non-recursive Chez mutex.
(let [a (atom 0)]
  (locking a
    (locking a
      (chk "locking-reentrant: nested locking on the same object does not deadlock" true))))

;; --- PSL R4 cluster-2 confirmation: the bare monitor-enter/monitor-exit halves
;; route through the same reentrant helpers as locking (the dynaload path under
;; malli emits these directly). Same-thread re-entry must not deadlock; a
;; non-owner exit must throw IllegalMonitorStateException (JVM parity).
(let [a (atom 0)]
  (monitor-enter a)
  (monitor-enter a)
  (monitor-exit a)
  (monitor-exit a)
  (chk "monitor-halves: bare monitor-enter/exit are reentrant per thread" true))

(let [a (atom 0)
      ok (promise)]
  (monitor-enter a)
  (future
    (try (monitor-exit a)
         (deliver ok false)
         (catch Exception e
           (deliver ok (= "java.lang.IllegalMonitorStateException"
                          (.getName (class e)))))))
  (chk "monitor-ims: non-owner monitor-exit throws IllegalMonitorStateException"
       (= true (deref ok 5000 :timeout)))
  (monitor-exit a))

;; --- PSL R4 cluster-2 confirmation: monitor state survives cross-thread
;; contention with re-entry (single-writer owner discipline; contention smoke —
;; a deadlock here trips the harness timeout).
(let [a (atom 0)
      t1 (future (dotimes [_ 200] (locking a (locking a 1))))
      t2 (future (dotimes [_ 200] (locking a (locking a 1))))]
  ;; bounded: a lost-wakeup regression must FAIL, never hang the gate
  (chk "monitor-contention: concurrent reentrant locking completes"
       (and (not= :hang (deref t1 20000 :hang))
            (not= :hang (deref t2 20000 :hang)))))

;; --- PSL R4 cluster-2 confirmation: agent actions inherit the sender's dynamic
;; bindings via fork-inheritance ALONE (contract pin; concurrency.ss:305/467).
;; On fresh-default-per-thread semantics the action would read :unbound.
(def ^:dynamic *psl-sentinel* :unbound)
(let [a (agent nil)
      seen (promise)]
  (binding [*psl-sentinel* :bound]
    (send a (fn [s] (deliver seen *psl-sentinel*) s)))
  (await a)
  ;; bounded deref: a conveyance regression must FAIL the check, never hang
  ;; the gate (an agent error here starves the promise forever).
  (chk "agent-bindings: agent action sees the sender's dynamic binding"
       (= :bound (deref seen 5000 :timeout))))

;; --- PSL R4 cluster-3: await/await-for throw when the agent fails DURING the
;; wait. Entry on an already-failed agent throwing IS JVM parity (tap-agents
;; pins it). The mid-wait case is a DELIBERATE divergence: the JVM's latch
;; action never runs on a failed agent, so its await blocks forever — jolt
;; throws needs-restart instead (recorded in known-divergences.edn). jolt
;; previously returned normally, which read as success. Every deref below is
;; bounded so a regression FAILS the check instead of hanging the gate.
;; ORDERING: the send happens first and `entered` proves the action is running
;; before the awaiting future starts — otherwise the future can win the race,
;; await an idle healthy agent, and return normally (seen on Linux CI). With
;; the agent provably busy, await either enters the wait (the mid-fail path
;; under test) or arrives after the failure (the entry path) — both throw.
(let [a (agent 0)
      entered (promise)
      go (promise)]
  (send a (fn [s]
            (deliver entered :in)
            (when (= :go (deref go 5000 :timeout)) (throw (ex-info "boom" {})))
            s))
  (deref entered 5000 :timeout)
  (let [t (future (try (await a) false
                       (catch RuntimeException e
                         (= "Agent is failed, needs restart" (ex-message e)))))]
    (Thread/sleep 50)             ;; let await enter the wait loop on a healthy agent
    (deliver go :go)
    (chk "agent-await-midfail: agent failing during await throws needs-restart"
         (= true (deref t 12000 :hang)))))

(let [a (agent 0)
      entered (promise)
      go (promise)]
  (send a (fn [s]
            (deliver entered :in)
            (when (= :go (deref go 5000 :timeout)) (throw (ex-info "boom" {})))
            s))
  (deref entered 5000 :timeout)
  (let [t (future (try (await-for 5000 a) false
                       (catch RuntimeException e
                         (= "Agent is failed, needs restart" (ex-message e)))))]
    (Thread/sleep 50)
    (deliver go :go)
    (chk "agent-awaitfor-midfail: await-for on an agent failing mid-wait throws needs-restart"
         (= true (deref t 12000 :hang)))))

;; --- PSL R4 cluster-4 confirmation: AtomicReference.updateAndGet is a CAS
;; retry loop, the fn runs LOCK-FREE (JVM parity) — so the fn may re-enter the
;; same atomic. The old mutex-held implementation deadlocked right there; the
;; bounded deref turns a regression into a FAIL, not a hang.
(let [a (java.util.concurrent.atomic.AtomicReference. 0)
      t (future (.updateAndGet a (fn [v] (if (= v 0)
                                            (do (.updateAndGet a (fn [_] 10)) (inc v))
                                            (inc v)))))]
  (chk "atomic-reentrant-update: updateAndGet fn may re-enter the same atomic"
       (= 11 (deref t 5000 :hang))))

(if (empty? @failures)
  (println "STM OK")
  (doseq [f @failures] (println "FAIL:" f)))

;; Done with the agent system's pools (futures, agents): end them, as a JVM
;; program does, or their idle workers hold the process up for their keep-alive.
(shutdown-agents)
