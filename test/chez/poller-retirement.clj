;; test/chez/poller-retirement.clj — retiring a registration must not cost the
;; poller its ability to report readiness. Run through jolt (wired into
;; host/chez/smoke.sh).
;;
;; WHY THIS EXISTS. Two regressions, one per platform, both from making the
;; poller's registrations per (fd, filter). Neither was caught by any gate:
;; poller-registration.clj is read-only on eight distinct fds, so no fd there ever
;; carries two filters and none of them closes while a delete for it is in flight.
;;
;; ROUND A — a refused delete must not wedge the loop (kqueue).
;; poller-round returns an empty event list for TWO different reasons: the wait
;; itself failed, or every entry the kernel reported was an EV_ERROR that got
;; filtered out. The loop carried its pending deletes forward on both, but an
;; EV_ERROR *is* the kernel having processed the changelist and refused an entry —
;; re-issuing that delete gets it refused again, forever. kevent returns as soon as
;; a changelist entry errors and reports only the error, so a single refused delete
;; stopped every readiness report in the process and spun a core doing it. A fiber
;; closing its own socket the moment its read returns is enough to produce one: the
;; kernel drops a closed fd from the kqueue set, so the delete the poller had
;; already scheduled for it comes back ENOENT. Unfixed this wedges within about
;; three rounds, and the ev-errors counter runs to seven figures.
;;
;; ROUND B — retiring one direction must not retire the other (epoll).
;; epoll keys a registration by fd and carries both directions in ONE mask; there is
;; no per-direction delete (EPOLL_CTL_DEL takes no mask and ignores one). So a bare
;; EPOLL_CTL_DEL issued to retire a fired :read also removes a :write registration
;; the same fd still has a fiber parked on — and that direction is not in :pending
;; any more, so nothing re-adds it and the writer never wakes. Before the split,
;; waiters were flat per fd and every event woke all of them, so a waiter that was
;; not ready re-registered and the loss healed itself; per-filter waiters removed
;; that. This round parks both directions of one socket at once and then lets the
;; read fire, which is the shape the whole (fd, filter) split exists to serve.
;;
;; Same watchdog structure as poller-registration.clj, and for the same reason: a
;; lost wakeup here presents as a hang, not a failure, so the MAIN thread does
;; nothing but watch a deadline while the workload runs on a spawned thread.
(require '[jolt.socket])
(require '[jolt.io-poller])
(require '[clojure.core.async :as a])

(def rounds 12)
(def per 4)
;; Round B is deterministic once its shape is built — the registration either
;; survives the other direction's retirement or it does not — so it runs twice, not
;; once per A round. It moves 8 MB through a socket to fill the send buffer, which
;; at jolt's stream throughput is most of a second; running it 12 times put the case
;; at 25s locally and over the watchdog on a slower CI runner. Round A is the one
;; that needs repeating: it is racing a close against a delete.
(def b-rounds 2)
(def hang-ms 60000)

(def progress (atom {:round 0 :phase :starting}))
(defn- at! [phase] (swap! progress assoc :phase phase))

;; ---- round A: the fiber closes its own socket the instant its read returns -----
;; That close races the EV_DELETE the poller scheduled when the read fired. When the
;; close wins, the kernel has already dropped the registration and the delete is
;; refused — the entry this round exists to survive.
(defn- round-a [ss port]
  (let [clients (doall (for [_ (range per)] (do (at! :a-connect) (java.net.Socket. "127.0.0.1" port))))
        srvs (doall (for [_ (range per)] (do (at! :a-accept) (.accept ss))))
        outs (doall (for [c clients]
                      (binding [a/*go-backend* :fiber]
                        (a/go (let [b (byte-array 8)
                                    n (.read (.getInputStream c) b 0 8)
                                    v (String. b 0 n "UTF-8")]
                                (.close c)
                                v)))))]
    (at! :a-settling)
    (Thread/sleep 15)
    (at! :a-writing)
    (doseq [s srvs] (.write (.getOutputStream s) (.getBytes "x" "UTF-8") 0 1))
    (let [got (doall (for [[i o] (map-indexed vector outs)]
                       (do (at! (str "a-collecting " i " of " per))
                           (first (a/alts!! [o (a/timeout 2000)])))))]
      (when (< (count (filter #(= "x" %) got)) per)
        (println "POLLER-DEBUG" (pr-str (jolt.io-poller/debug-state))))
      (at! :a-closing)
      (doseq [s srvs] (.close s))
      (count (filter #(= "x" %) got)))))

;; ---- round B: both directions of ONE fd parked at once -------------------------
;; epoll keys a registration by fd and carries both directions in one mask, so
;; retiring a fired :read with a bare EPOLL_CTL_DEL also drops a :write the same fd
;; still has parked — and that direction is no longer in :pending, so nothing re-adds
;; it. This round builds that shape: a writer that fills the socket until it parks on
;; :write, a reader parked on :read of the SAME fd, and then a byte that fires the
;; read.
;;
;; The shape is the hard part — the send buffer has to be genuinely full, and jolt
;; exposes no setsockopt to shrink it, so this writes until the poller table SAYS a
;; :write waiter is parked rather than assuming some byte count is enough. If it
;; never parks, the round reports :no-shape and the case FAILS: a round that quietly
;; passed without ever building its shape would gate nothing, which is how this hole
;; survived the case that was supposed to cover it.
(def payload (byte-array 262144))

(defn- write-waiter? [fd]
  (pos? (or (get-in (jolt.io-poller/debug-state) [:fds fd :write :waiters]) 0)))

;; The fd the poller knows this socket by — the one entry with a parked :read, which
;; is the reader this round just started. Read off the table rather than guessed.
(defn- read-parked-fd []
  (first (for [[fd per-filt] (:fds (jolt.io-poller/debug-state))
               :when (pos? (or (get-in per-filt [:read :waiters]) 0))]
           fd)))

(defn- round-b [ss port]
  (at! :b-connect)
  (let [c (java.net.Socket. "127.0.0.1" port)
        s (.accept ss)
        red (binding [a/*go-backend* :fiber]
              (a/go (try (let [b (byte-array 1)
                               n (.read (.getInputStream c) b 0 1)]
                           (if (pos? n) (String. b 0 n "UTF-8") :eof))
                         (catch Throwable t (str "read threw: " t)))))]
    (at! :b-read-parking)
    (Thread/sleep 100)
    (let [fd (read-parked-fd)
          wrote (binding [a/*go-backend* :fiber]
                  (a/go (try (let [os (.getOutputStream c)]
                               ;; 8 MB against a peer nobody reads. Comfortably past
                               ;; an autotuned loopback buffer, so send answers EAGAIN
                               ;; and the fiber parks — and small enough that the
                               ;; drain below finishes well inside the timeout.
                               (dotimes [_ 32] (.write os payload 0 (alength payload)))
                               :wrote)
                             (catch Throwable t (str "write threw: " t)))))]
      ;; wait for the writer to actually park on :write — up to 5s, then give up
      (at! :b-filling)
      (let [parked? (loop [n 0]
                      (cond (and fd (write-waiter? fd)) true
                            (> n 500) false
                            :else (do (Thread/sleep 10) (recur (inc n)))))]
        (if-not parked?
          (do (println "POLLER-DEBUG" (pr-str (jolt.io-poller/debug-state)))
              (.close c) (.close s)
              {:shape :no-shape :fd fd})
          (do
            ;; both directions of fd are parked now. Fire the read; its retirement is
            ;; what must not take the write registration with it.
            (at! :b-poking)
            (.write (.getOutputStream s) (.getBytes "x" "UTF-8") 0 1)
            (let [got-read (first (a/alts!! [red (a/timeout 3000)]))]
              (at! :b-draining)
              (let [drainer (Thread. (fn [] (try (let [is (.getInputStream s)
                                                       b (byte-array 262144)]
                                                   (loop [] (when (pos? (.read is b 0 262144)) (recur))))
                                                 (catch Throwable _ nil))))]
                (.start drainer)
                (let [got-write (first (a/alts!! [wrote (a/timeout 20000)]))]
                  (at! :b-closing)
                  (when (or (not= got-read "x") (not= got-write :wrote))
                    (println "POLLER-DEBUG" (pr-str (jolt.io-poller/debug-state))))
                  (.close c) (.close s)
                  (.join drainer 2000)
                  {:shape :ok :read got-read :write got-write})))))))))

(defn- run-rounds []
  (let [ss (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress))
        port (.getLocalPort ss)]
    (loop [r 0 acc 0]
      (swap! progress assoc :round r)
      (if (= r rounds)
        ;; A is done; now the both-directions case, twice.
        (loop [b 0]
          (swap! progress assoc :round (+ rounds b))
          (if (= b b-rounds)
            (do (.close ss) {:status :ok :total acc})
            (let [{:keys [shape read write fd]} (round-b ss port)]
              (cond
                (= shape :no-shape) (do (.close ss) {:status :no-shape :round b :got fd})
                (not= read "x")     (do (.close ss) {:status :lost-b-read :round b :got read})
                (not= write :wrote) (do (.close ss) {:status :lost-b-write :round b :got write})
                :else               (recur (inc b))))))
        (let [n (round-a ss port)]
          (if (< n per)
            (do (.close ss) {:status :lost-a :missing (- per n) :round r})
            (recur (inc r) (+ acc n))))))))

(def outcome (atom nil))

(.start (Thread. (fn []
                   (reset! outcome
                           (try (run-rounds)
                                (catch Throwable t {:status :threw :ex (str t)}))))))

(let [deadline (+ (System/currentTimeMillis) hang-ms)]
  (loop []
    (Thread/sleep 25)
    (let [{:keys [status total missing round ex got]} @outcome]
      (cond
        (= status :ok) (do (println "POLLER-RETIREMENT OK" total) (System/exit 0))
        (= status :lost-a)
        (do (println (str "POLLER-RETIREMENT LOST " missing " of " per
                          " readiness reports after a refused delete, round " round))
            (System/exit 1))
        (= status :lost-b-read)
        (do (println (str "POLLER-RETIREMENT LOST the read of a both-directions fd in round "
                          round " (got " (pr-str got) ")"))
            (System/exit 1))
        (= status :lost-b-write)
        (do (println (str "POLLER-RETIREMENT LOST the parked write when the read's"
                          " registration was retired, round " round " (got " (pr-str got) ")"))
            (System/exit 1))
        (= status :no-shape)
        (do (println (str "POLLER-RETIREMENT NO-SHAPE in round " round
                          ": the writer never parked on :write, so the both-directions"
                          " case tested nothing (fd " (pr-str got) ")"))
            (System/exit 1))
        (= status :threw) (do (println (str "POLLER-RETIREMENT THREW " ex)) (System/exit 1))
        (< (System/currentTimeMillis) deadline) (recur)
        :else (let [{:keys [round phase]} @progress]
                (println (str "POLLER-RETIREMENT HUNG after " hang-ms
                              "ms in round " round " at " phase))
                (System/exit 1))))))
