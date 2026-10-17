;; test/chez/poller-registration.clj — a readiness registration must never be
;; lost. Run through jolt (wired into host/chez/smoke.sh).
;;
;; WHY THIS EXISTS. jolt.io-poller's poller thread drained its pending
;; registrations in TWO critical sections — read :pending, then clear it — so a
;; registration landing in between was erased before it ever reached the
;; kqueue/epoll set. That fd's readiness was then never reported and the fiber
;; waiting on it never resumed. The window is microseconds wide, so it presented
;; as one fiber out of eight failing to finish, once, in a `make fibers` sequence,
;; and never again in six isolated runs of that gate.
;;
;; The workload keeps registrations landing while the poller is mid-round: rounds
;; of fibers that all park on their own socket read, over and over, so some
;; registrations are bound to arrive during the drain. Unfixed this loses ~11% of
;; them (283 of 320); fixed it loses none. The run STOPS at the first round that
;; loses one, because otherwise every subsequent loss pays the alts!! timeout
;; again: the unfixed version took over eleven minutes to finish what the fixed
;; one does in under two seconds, and a gate that slow is its own problem.
;;
;; This header used to claim that a lost wakeup is a FAILURE and never a hang,
;; on the grounds that every read is bounded by an alts!! timeout. It is not:
;; that timeout is itself a core.async channel, so the machinery this case exists
;; to stress is also the machinery the bound depends on, and .accept and the
;; socket setup are not bounded at all. A run did wedge — over ninety minutes,
;; main thread parked in a condition wait, poller thread in kevent — and reported
;; nothing (jolt-8tma). Hence the watchdog: the MAIN thread does nothing but
;; watch a deadline, and the workload runs on a spawned thread, so a wedge fails
;; the case and names the round and phase it stopped in. That way round because
;; the watcher must be the one thread that cannot be what wedges — every socket,
;; fiber and channel in this case belongs to the workload. (It was also the only
;; way round that worked when this was written: System/exit on a spawned thread
;; unwound that thread and left the process running, which is jolt-7xls, fixed
;; since.) What made that one run wedge is still unknown; this is what will say
;; where it was the next time.
(require '[jolt.socket])
(require '[jolt.io-poller])
(require '[clojure.core.async :as a])

(def rounds 40)
(def per 8)

;; Generous against the real workload (under two seconds) and against the worst
;; legitimate slow path (one losing round pays a 2s alts!! timeout and then the
;; run stops), so anything that reaches it is wedged, not slow.
(def hang-ms 60000)

;; Where the workload is, for the watchdog to quote when it gives up.
(def progress (atom {:round 0 :phase :starting}))
(defn- at! [phase] (swap! progress assoc :phase phase))

;; Both ends of every connection are closed at the end of the round. Holding the
;; accepted Socket rather than just its stream is the point: 40 rounds of 8 leaks
;; 320 descriptors otherwise, and under a low ulimit -n the gate starts failing on
;; accept for a reason that has nothing to do with the bug it exists to catch.
(defn- one-round [ss port]
  (let [clients (doall (for [_ (range per)] (do (at! :connect) (java.net.Socket. "127.0.0.1" port))))
        srvs (doall (for [_ (range per)] (do (at! :accept) (.accept ss))))
        outs (doall (for [c clients]
                      (binding [a/*go-backend* :fiber]
                        (a/go (let [b (byte-array 8)
                                    n (.read (.getInputStream c) b 0 8)]
                                (String. b 0 n "UTF-8"))))))]
    ;; let them all park before the data arrives, so every read registers
    (at! :settling)
    (Thread/sleep 15)
    (at! :writing)
    (doseq [s srvs] (.write (.getOutputStream s) (.getBytes "x" "UTF-8") 0 1))
    (let [got (doall (for [[i o] (map-indexed vector outs)]
                       (do (at! (str "collecting " i " of " per))
                           (first (a/alts!! [o (a/timeout 2000)])))))]
      ;; Classify a loss BEFORE the closes tear the evidence down: the poller
      ;; table snapshot (jolt.io-poller/debug-state) says which stage dropped
      ;; the wakeup — still :pending (never drained), waiters parked with no
      ;; event (kernel set), or ready-with-no-waiters (resume lost).
      (when (< (count (filter #(= "x" %) got)) per)
        (println "POLLER-DEBUG" (pr-str (jolt.io-poller/debug-state))))
      (at! :closing)
      (doseq [c clients] (.close c))
      (doseq [s srvs] (.close s))
      (count (filter #(= "x" %) got)))))

(defn- run-rounds []
  (let [ss (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress))
        port (.getLocalPort ss)]
    (loop [r 0 acc 0]
      (swap! progress assoc :round r)
      (if (= r rounds)
        (do (.close ss) {:status :ok :total acc})
        (let [n (one-round ss port)]
          (if (< n per)
            (do (.close ss) {:status :lost :missing (- per n) :round r})
            (recur (inc r) (+ acc n))))))))

(def outcome (atom nil))

(.start (Thread. (fn []
                   (reset! outcome
                           (try (run-rounds)
                                (catch Throwable t {:status :threw :ex (str t)}))))))

;; The main thread from here on is only the deadline. Every exit goes through it.
(let [deadline (+ (System/currentTimeMillis) hang-ms)]
  (loop []
    (Thread/sleep 25)
    (let [{:keys [status total missing round ex]} @outcome]
      (cond
        (= status :ok)    (do (println "POLLER-REGISTRATION OK" total) (System/exit 0))
        (= status :lost)  (do (println (str "POLLER-REGISTRATION LOST " missing " of " per
                                            " readiness registrations in round " round))
                              (System/exit 1))
        (= status :threw) (do (println (str "POLLER-REGISTRATION THREW " ex)) (System/exit 1))
        (< (System/currentTimeMillis) deadline) (recur)
        :else (let [{:keys [round phase]} @progress]
                (println (str "POLLER-REGISTRATION HUNG after " hang-ms
                              "ms in round " round " at " phase))
                (System/exit 1))))))
