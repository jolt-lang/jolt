;; Four threads and eight fibers contend for one monitor (jolt#1654). With a single
;; waiter a signal and a broadcast look alike, so this is the case that proves the
;; release hands off to the next thread while fibers are also queued. The counter
;; is a plain array element, so a lost increment is a lost exclusion, and results
;; are read against a deadline so a stranded waiter fails instead of hanging.
(require '[clojure.core.async :as a])

(alter-var-root #'clojure.core.async/*fiber-carrier-count* (constantly 8))

(def threads 4)
(def fibers 8)
(def per-thread 2000)
(def per-fiber 500)
(def expected (+ (* threads per-thread) (* fibers per-fiber)))

(def o (Object.))
(def counter (int-array 1))

;; Spin while holding the monitor so the others queue on it. Not a sleep, because a
;; sleeping fiber parks.
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
