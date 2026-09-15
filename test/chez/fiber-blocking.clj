;; test/chez/fiber-blocking.clj — a fiber must never block its carrier (jolt-x1no).
;; Run through jolt; wired into host/chez/smoke.sh, which caps every case.
;;
;; WHAT WAS WRONG. Every wait in the runtime except the loader's and the object
;; monitor's was a bare condition-wait, which blocks the THREAD. On a carrier that
;; is every fiber placed on it, and a fiber cannot migrate away, so the wait also
;; stopped whatever the carrier would have run next — including, often enough, the
;; very thing that would have ended the wait. @a-promise from a go block was enough.
;;
;; THE SHAPE OF EVERY CASE, and why it is a deadlock rather than a slow test. The
;; pool is pinned to ONE carrier, and each case runs two fibers on it: a WAITER,
;; spawned first, and a RELEASER behind it in the queue. If the waiter parks, the
;; carrier runs the releaser, the waiter is resumed and the case answers. If the
;; waiter blocks, the releaser never runs at all and nothing can ever end the wait.
;; So one carrier is not a way of making the bug likelier — it is what turns a
;; stall into a hang, which is the only version of this a test can catch reliably.
;; (With the default pool the releaser lands on another carrier and every one of
;; these passes while still stalling every OTHER fiber on the waiter's carrier,
;; which is the part no assertion can see.)
;;
;; The four TIMED cases have no releaser at all. They cover the other half of a
;; parked wait, which is that something must wake a fiber AT its deadline: a thread
;; hands the deadline to condition-wait, and a fiber has nothing to do that, so
;; jolt-cv-wait registers with the shared timer. A fiber that parks with a deadline
;; and is never woken is a hang too, and it would pass a test that only checked the
;; untimed paths.
(require '[clojure.core.async :as a])
(import '[java.util.concurrent CountDownLatch Executors TimeUnit])

(alter-var-root #'clojure.core.async/*fiber-carrier-count* (constantly 1))

(def results (atom []))
(def case-timeout-ms 5000)

;; The waiter's answer, or :HUNG if the carrier stopped. Both fibers land on the
;; one carrier, waiter first, so the releaser only ever runs if the waiter parked.
(defn- probe [k waiter releaser]
  (let [w (binding [a/*go-backend* :fiber] (a/go (waiter)))]
    (when releaser
      (binding [a/*go-backend* :fiber] (a/go (releaser))))
    (let [[v port] (a/alts!! [w (a/timeout case-timeout-ms)])]
      (swap! results conj [k (if (= port w) v :HUNG)]))))

;; 1-3. promise deref: the shortest path to the bug, and the one people hit.
(let [p (promise)]
  (probe :promise #(deref p) #(deliver p :delivered)))
(let [p (promise)]
  (probe :promise-timed #(deref p 3000 :timed-out) #(deliver p :delivered)))
(let [p (promise)]
  (probe :promise-deadline #(deref p 150 :timed-out) nil))

;; 4-5. future deref. The future's body runs on its own thread, so a blocked
;; carrier would not stop it by itself — the body waits on a promise only the
;; RELEASER fiber can deliver, which puts the carrier back in the cycle.
(let [p (promise)
      f (future (deref p))]
  (probe :future #(deref f) #(deliver p :from-future)))
(let [f (future (deref (promise)))]        ; never settles
  (probe :future-deadline #(deref f 150 :timed-out) nil))

;; 6. Thread.join, over a thread that cannot exit until the releaser runs.
(let [p (promise)
      t (Thread. (fn [] (deref p)))]
  (.start t)
  (probe :thread-join (fn [] (.join t) :joined) #(deliver p :go)))

;; 7-8. CountDownLatch, the await/countDown pair and its bounded form.
(let [latch (CountDownLatch. 1)]
  (probe :latch (fn [] (.await latch) :counted) #(.countDown latch)))
(let [latch (CountDownLatch. 1)]
  (probe :latch-deadline
         (fn [] (.await latch 150 TimeUnit/MILLISECONDS))
         nil))

;; 9-10. An executor task's Future, and awaitTermination — which was a sleep poll,
;; so a fiber calling it used to sleep its carrier in 100ms slices.
(let [p (promise)
      ex (Executors/newSingleThreadExecutor)
      fut (.submit ex (fn [] (deref p)))]
  (probe :executor-get (fn [] (.get fut)) #(deliver p :from-task))
  (.shutdown ex))
(let [p (promise)
      ex (Executors/newSingleThreadExecutor)]
  (.submit ex (fn [] (deref p)))
  (.shutdown ex)
  (probe :executor-await
         (fn [] (.awaitTermination ex 3 TimeUnit/SECONDS))
         #(deliver p :from-task)))

;; 11. A piped stream: the reader waits for bytes the releaser writes.
(let [in (PipedInputStream.)
      out (PipedOutputStream. in)]
  (probe :piped-read
         (fn [] (.read in))
         (fn [] (.write out 65) (.flush out) :wrote)))

;; 12. agent await, over an action that cannot finish until the releaser runs. The
;; action is on the agent's worker thread; the await is what used to block.
(let [p (promise)
      ag (agent 0)]
  (send ag (fn [s] (deref p) (inc s)))
  (probe :agent-await (fn [] (await ag) :drained) #(deliver p :go)))

;; 13. .waitFor on a subprocess. Not a condition-variable wait at all — there is
;; nothing to attach one to, so it polls waitpid — but the same hazard and the worst
;; version of it: the poll slept the carrier, unboundedly, for the whole life of the
;; child, AND it held the per-process mutex the whole time, which makes the carrier
;; unpreemptible too (the scheduler refuses to preempt a fiber whose carrier holds a
;; counted lock). So this one was out of reach of even the preemption backstop.
;;
;; Asserted as ORDER rather than through `probe`, because there is no releaser: the
;; sibling fiber queued behind the waiter must reach the channel FIRST, while the
;; child is still running.
(let [order (a/chan 4)
      p (.start (ProcessBuilder. ["sh" "-c" "sleep 0.4"]))
      fs (binding [a/*go-backend* :fiber]
           [(a/go (let [c (.waitFor p)] (a/>!! order [:waited c])))
            (a/go (a/>!! order :sibling-ran))])
      [v port] (a/alts!! [order (a/timeout case-timeout-ms)])]
  (swap! results conj [:waitfor-yields (if (= port order) v :HUNG)])
  (mapv a/<!! fs))

;; 14. read-line, whose stdin readiness poll had the same shape. stdin is not
;; readable under the harness, so this checks the part that is testable here: a
;; fiber blocked in it does not stop the sibling behind it. The reader is left
;; parked; the case is over once the sibling has run.
(let [order (a/chan 4)]
  (binding [a/*go-backend* :fiber]
    (a/go (read-line) :read)
    (a/go (a/>!! order :sibling-ran)))
  (let [[v port] (a/alts!! [order (a/timeout case-timeout-ms)])]
    (swap! results conj [:read-line-yields (if (= port order) v :HUNG)])))

;; 15-16. Object.wait on an object monitor (jolt#1011), which is the one wait in
;; the runtime that hands a LOCK back before it parks. The releaser can only take
;; the monitor because the waiter gave it up, so this case checks the release as
;; well as the park; and the waiter re-acquires on the way out, which on one
;; carrier it can only do after the releaser has left the monitor.
(let [o (Object.)]
  (probe :object-wait
         (fn [] (locking o (.wait o 10000)) :notified)
         (fn [] (locking o (.notify o)))))
(let [o (Object.)]
  (probe :object-wait-deadline
         (fn [] (locking o (.wait o 150)) :timed-out)
         nil))

(def expected
  {:promise :delivered
   :promise-timed :delivered
   :promise-deadline :timed-out
   :future :from-future
   :future-deadline :timed-out
   :thread-join :joined
   :latch :counted
   :latch-deadline false
   :executor-get :from-task
   :executor-await true
   :piped-read 65
   :agent-await :drained
   :waitfor-yields :sibling-ran
   :read-line-yields :sibling-ran
   :object-wait :notified
   :object-wait-deadline :timed-out})

(let [got (into {} @results)
      bad (remove (fn [[k v]] (= v (get expected k))) (seq got))
      missing (remove (fn [k] (contains? got k)) (keys expected))]
  (cond
    (seq missing)
    (do (println "FIBER-BLOCKING FAILED cases did not run:" (pr-str (vec missing)))
        (System/exit 1))

    (seq bad)
    (do (doseq [[k v] bad]
          (println "FAIL:" k "->" (pr-str v) "expected" (pr-str (get expected k))
                   (if (= :HUNG v) "(the waiter blocked its carrier)" "")))
        (println "FIBER-BLOCKING FAILED" (count bad) "of" (count expected))
        (System/exit 1))

    :else
    (do (println "FIBER-BLOCKING OK" (count expected) "waits give up the carrier, on 1 carrier")
        (System/exit 0))))
