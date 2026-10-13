;; Shared by the #1331 trace scenarios: timestamped log lines, otel spans, a main
;; thread that keeps allocating so a frozen collection shows up as missing
;; heartbeats.
(require '[otel.sdk :as sdk] '[otel.trace :as trace] '[clojure.string :as str])

(def t0 (System/nanoTime))
(defn ms [] (quot (- (System/nanoTime) t0) 1000000))
(def log-lock (Object.))
(defn log [& xs]
  (locking log-lock
    (println (str "[" (ms) "ms t" (.getId (Thread/currentThread)) "] " (str/join " " xs)))
    (flush)))

(def otel (sdk/init! {:service-name "repro-1331" :exporter :console :processor :simple
                      :metric-interval-ms 5000}))
(def tracer (sdk/tracer "repro-1331"))

(defmacro step [label & body]
  `(do (log "BEGIN" ~label)
       (let [r# (trace/with-span [sp# tracer ~label {}] ~@body)]
         (log "END" ~label)
         r#)))

(defn run-with-heartbeat
  "Run F on a thread; the main thread allocates and logs a heartbeat each second
   until F returns or SECS pass."
  [secs f]
  (let [done (promise)
        t (Thread. (fn [] (try (f) (catch Throwable e (log "scenario threw" e)))
                     (deliver done true)))]
    (.start t)
    (loop [n 0 last (ms)]
      (when (and (not (realized? done)) (< (ms) (* 1000 secs)))
        (dotimes [_ 2000] (vec (range 20)))
        (let [now (ms)]
          (if (>= (- now last) 1000)
            (do (log "heartbeat" n "gc" (jolt.host/gc-count)) (recur (inc n) now))
            (recur n last)))))
    (log "scenario" (if (realized? done) "finished" "still running at deadline"))
    (sdk/shutdown! otel)
    (System/exit 0)))
