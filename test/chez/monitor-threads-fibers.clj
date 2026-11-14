;; test/chez/monitor-threads-fibers.clj: one monitor contended by SEVERAL threads and
;; by fibers at the same time (jolt#1654). Run through jolt from host/chez/smoke.sh.
;;
;; A monitor release resumes every parked fiber and signals ONE waiting thread
;; (host/chez/java/concurrency.ss), and a slow claim made while other threads are
;; still counted takes the word marked, so the next release signals the next thread.
;; monitor-contention.clj has one thread among the fibers, and with a single
;; thread waiter a signal and a broadcast behave alike. This case has four threads
;; queued together with the fibers, which is where the thread count and the fiber
;; list are read together to decide whether a claim marks the word.
;;
;; The counter is a plain array element, so a lost increment is a lost exclusion,
;; and every result is read against a deadline, so a stranded waiter is a named
;; failure instead of a hang (smoke.sh's per-case cap still backs that up).
(require '[clojure.core.async :as a])

(alter-var-root #'clojure.core.async/*fiber-carrier-count* (constantly 8))

(def threads 4)
(def fibers 8)
(def per-thread 2000)
(def per-fiber 500)
(def expected (+ (* threads per-thread) (* fibers per-fiber)))

(def o (Object.))
(def counter (int-array 1))

;; The section spins while it holds the monitor, so the other contenders actually
;; queue on it. A bare increment is over before anyone waits, and with nobody
;; queued a signal and a broadcast are the same thing, so the case would pass a
;; broken hand-off. Spinning rather than sleeping, because a sleeping fiber parks.
(defn- spin [n] (loop [i 0] (when (< i n) (recur (inc i)))))

(defn- bump! []
  (locking o
    (let [v (aget counter 0)]
      (spin 2000)
      (aset counter 0 (inc v)))))

(def ts (doall (for [_ (range threads)]
                 (a/thread (dotimes [_ per-thread] (bump!)) :thread-done))))

(def gs (binding [a/*go-backend* :fiber]
          (doall (for [_ (range fibers)]
                   (a/go (dotimes [_ per-fiber] (bump!)) :fiber-done)))))

(def deadline (+ (System/currentTimeMillis) 60000))
(defn- take-by-deadline [ch]
  (let [[v _] (a/alts!! [ch (a/timeout (max 1 (- deadline (System/currentTimeMillis))))])]
    (or v :timeout)))

(let [tvs (mapv take-by-deadline ts)
      fvs (mapv take-by-deadline gs)
      got (aget counter 0)]
  (cond
    (not= (repeat threads :thread-done) (seq tvs))
    (do (println "MONITOR-THREADS-FIBERS THREADS DID NOT FINISH" (pr-str tvs)) (System/exit 1))

    (not= (repeat fibers :fiber-done) (seq fvs))
    (do (println "MONITOR-THREADS-FIBERS FIBERS DID NOT FINISH" (pr-str fvs)) (System/exit 1))

    (not= expected got)
    (do (println (str "MONITOR-THREADS-FIBERS LOST " (- expected got) " of " expected
                      " increments (exclusion broken)"))
        (System/exit 1))

    :else (do (println "MONITOR-THREADS-FIBERS OK" got) (System/exit 0))))
