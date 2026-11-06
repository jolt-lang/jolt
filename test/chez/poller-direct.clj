;; test/chez/poller-direct.clj — what one park costs, and that a registration made
;; directly with the kernel still wakes whoever it should. Run through jolt (wired
;; into host/chez/smoke.sh and make fiberspoll).
;;
;; The wake used to set a :ready flag that the woken fiber's NEXT park consumed, so
;; every park did a second recv and a second trip through the lock to reach the same
;; EAGAIN. And every registration woke the poller through its control pipe, which
;; added a poller round per park. Neither loses anything, so nothing failed; it
;; showed up as a socket round trip three times the cost of a blocking one. Case
;; COST counts both rather than timing them.
;;
;; A registration is one-shot now and the fiber makes it itself, so the other cases
;; pin what that depends on. epoll's one-shot disables the whole fd, not the
;; direction that fired, so a direction still parked has to be re-armed; a
;; registration the kernel refuses has to end the wait, since no event can; and a
;; timed wait leaves a registration behind that must not get in the way of the next.
;;
;; Same watchdog shape as poller-registration.clj: a lost wakeup is a hang, so the
;; main thread only watches a deadline and the workload runs on a spawned thread.
(require '[jolt.io-poller :as p])
(require '[jolt.socket.native :as n])
(require '[jolt.ffi :as ffi])
(require '[clojure.core.async :as a])

(def hang-ms 60000)
(def wake-ms 5000)
(def progress (atom :starting))
(def kernel-backend? (not @#'jolt.io-poller/poll-backend?))

(defn- pair []
  (let [[x y] (n/loopback-pair)]
    (p/nonblock! x) (p/nonblock! y)
    [x y]))

(defn- close-pair [x y]
  (p/forget! x) (p/forget! y)
  (n/c-close x) (n/c-close y))

(defn- fail! [what & detail]
  (throw (ex-info (str what) {:detail detail :table (p/debug-state)})))

(defn- waiters [fd filt]
  (or (get-in (p/debug-state) [:fds fd filt :waiters]) 0))

(defn- until-parked [fd filt]
  (loop [n 0]
    (cond (pos? (waiters fd filt)) true
          (> n 500) (fail! "never parked" fd filt)
          :else (do (Thread/sleep 10) (recur (inc n))))))

(defn- woke? [ch] (= :woke (first (a/alts!! [ch (a/timeout wake-ms)]))))

(defn- park-fiber [fd filt]
  (a/io-thread (p/wait-ready fd filt) :woke))

;; Stuff fd until send answers EAGAIN, so a :write wait really does park.
(defn- fill! [fd]
  (let [b (ffi/alloc 65536)]
    (try (loop []
           (let [[r e] (n/c-send fd b 65536 0)]
             (cond (pos? r) (recur)
                   (and (neg? r) (n/eagain? e)) nil
                   :else (fail! "send failed filling" r e))))
         (finally (ffi/free b)))))

(defn- drain! [fd]
  (let [b (ffi/alloc 65536)]
    (try (loop []
           (let [[r _] (n/c-recv fd b 65536 0)]
             (when (pos? r) (recur))))
         (finally (ffi/free b)))))

;; A fiber parked on :write of a full socket. A full socket can still go writable
;; (loopback moving bytes to the peer's receive buffer, an ACK riding on a byte the
;; peer sends), so a wake before arm-writer! is the kernel telling the truth, not a
;; fault: the fiber fills again and parks again. arm-writer! waits for it to stay
;; parked; only a wake after that ends it.
(defn- park-writer [fd]
  (let [armed (atom false)]
    {:armed armed
     :ch (a/io-thread
          (loop []
            (fill! fd)
            (p/wait-ready fd :write)
            (if @armed :woke (recur))))}))

(defn- arm-writer! [{:keys [armed]} fd]
  (loop [n 0]
    (until-parked fd :write)
    (Thread/sleep 100)
    (cond (pos? (waiters fd :write)) nil
          (> n 50) (fail! "the writer never stayed parked" fd)
          :else (recur (inc n))))
  (reset! armed true))

;; ---- COST: one wait per message, one poller round per wait ---------------------
(defn- case-cost []
  (reset! progress :cost)
  (let [k 2000 slack 64
        [x y] (pair)
        bx (ffi/alloc 16) by (ffi/alloc 16)
        parks (atom 0)
        rd (fn [fd b]
             (loop []
               (let [[r e] (n/c-recv fd b 16 0)]
                 (if (and (neg? r) (n/eagain? e))
                   (do (swap! parks inc) (p/wait-ready fd :read) (recur))
                   r))))
        rounds0 (do (a/<!! (a/io-thread (dotimes [_ 16] (n/c-send x bx 1 0) (rd y by)) :warm))
                    @p/waits)
        _ (reset! parks 0)
        echo (a/io-thread (dotimes [_ k] (rd y by) (n/c-send y by 1 0)) :echo)
        ping (a/io-thread (dotimes [_ k] (n/c-send x bx 1 0) (rd x bx)) :ping)]
    (when-not (= :ping (first (a/alts!! [ping (a/timeout (* 6 wake-ms))]))) (fail! "ping stalled"))
    (when-not (= :echo (first (a/alts!! [echo (a/timeout wake-ms)]))) (fail! "echo stalled"))
    (let [msgs (* 2 k) rounds (- @p/waits rounds0)]
      (close-pair x y) (ffi/free bx) (ffi/free by)
      (when (> @parks (+ msgs slack))
        (fail! "more than one wait per message: a wake is spending the next park"
               {:msgs msgs :waits @parks}))
      (when (and kernel-backend? (> rounds (+ @parks slack)))
        (fail! "more than one poller round per wait: a registration is waking the poller"
               {:waits @parks :rounds rounds})))))

;; ---- REFUSED: a wait the kernel will not register returns, it does not hang -----
(defn- case-refused []
  (reset! progress :refused)
  (let [[x y] (pair)]
    (n/c-close x)
    (when-not (woke? (park-fiber x :read))
      (fail! "a wait on a closed fd never returned"))
    (when (pos? (waiters x :read)) (fail! "a refused wait stayed in the table"))
    (p/forget! x) (p/forget! y)
    (n/c-close y)))

;; ---- BOTH: each direction wakes without taking the other with it ---------------
(defn- case-both [fires-first]
  (reset! progress [:both fires-first])
  (let [[x y] (pair)
        w (park-writer x)
        r (park-fiber x :read)
        fire-read #(let [b (doto (ffi/alloc 1) (ffi/write :uint8 1))]
                     (n/c-send y b 1 0)
                     (ffi/free b))
        fire-write #(loop [n 0]
                      (drain! y)
                      (cond (zero? (waiters x :write)) true
                            (> n 500) (fail! "the drain never woke the writer")
                            :else (do (Thread/sleep 10) (recur (inc n)))))]
    (until-parked x :read)
    (if (= fires-first :read)
      (do (arm-writer! w x)
          (reset! (:armed w) false)
          (fire-read)
          (when-not (woke? r) (fail! "the read did not wake" fires-first))
          ;; Parked through the read's event, or back after an honest wake: either
          ;; way the wake below can only come from a registration that survived.
          (arm-writer! w x)
          (fire-write)
          (when-not (woke? (:ch w)) (fail! "the parked write was lost after the read fired")))
      (do (arm-writer! w x)
          (fire-write)
          (when-not (woke? (:ch w)) (fail! "the write did not wake" fires-first))
          (when-not (pos? (waiters x :read)) (fail! "the write's wake took the reader with it"))
          (fire-read)
          (when-not (woke? r) (fail! "the parked read was lost after the write fired"))))
    (close-pair x y)))

;; ---- STALE: a timed-out wait's registration does not spoil the next wait --------
(defn- case-stale []
  (reset! progress :stale)
  (let [[x y] (pair)
        b (doto (ffi/alloc 1) (ffi/write :uint8 1))
        timed #(a/<!! (a/io-thread (p/wait-ready x :read (p/deadline-ms 30))))]
    (when-not (= :timeout (timed)) (fail! "a timed wait did not time out"))
    ;; the same direction again, on the registration it left behind
    (let [r (park-fiber x :read)]
      (until-parked x :read)
      (n/c-send y b 1 0)
      (when-not (woke? r) (fail! "the wait after a timeout was not woken")))
    (drain! x)
    ;; the other direction, while the stale read registration is still around
    (when-not (= :timeout (timed)) (fail! "a second timed wait did not time out"))
    (let [w (park-writer x)]
      (arm-writer! w x)
      (loop [n 0]
        (drain! y)
        (when (and (pos? (waiters x :write)) (< n 500)) (Thread/sleep 10) (recur (inc n))))
      (when-not (woke? (:ch w))
        (fail! "the write after a stale read registration was not woken")))
    (ffi/free b)
    (close-pair x y)))

(def outcome (atom nil))
(.start (Thread. (fn []
                   (reset! outcome
                           (try
                             (case-both :read)
                             (case-both :write)
                             (case-stale)
                             ;; After the others: the poller starts on the first wait
                             ;; and opens fds of its own, which would take the number
                             ;; this case has just closed.
                             (case-refused)
                             (case-cost)
                             :ok
                             (catch Throwable t
                               (str (ex-message t) " " (pr-str (ex-data t)))))))))

(let [deadline (+ (System/currentTimeMillis) hang-ms)]
  (loop []
    (Thread/sleep 25)
    (let [o @outcome]
      (cond
        (= o :ok) (do (println "POLLER-DIRECT OK") (System/exit 0))
        o (do (println "POLLER-DIRECT FAILED in" (pr-str @progress) ":" o) (System/exit 1))
        (< (System/currentTimeMillis) deadline) (recur)
        :else (do (println "POLLER-DIRECT HUNG at" (pr-str @progress)
                           (pr-str (p/debug-state)))
                  (System/exit 1))))))
