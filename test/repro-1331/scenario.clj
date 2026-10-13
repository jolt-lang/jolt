;; #1331: kmet's bash tool starts a long compile in the background, then checks
;; on it from time to time. Arg: redirect | inherit | timeout.
(load-file "repro/harness.clj")
(require '[kmet.app.bash-executor :as b])
(def variant (or (first *command-line-args*) "redirect"))
(def shell (System/getenv "REPRO_BASH"))
(def jolt-exe (System/getenv "REPRO_JOLT"))
(def out-dir (or (System/getenv "REPRO_OUT") "repro-out"))
(defn bash [label cmd & {:as opts}]
  (step label
    (let [r (b/execute-bash (merge {:command cmd :shell-path shell} opts))]
      (log label "exit" (:exit-code r) "cancelled" (:cancelled r)
           "out" (pr-str (let [o (str (:output r))] (subs o (max 0 (- (count o) 300))))))
      r)))
(def build-cmd (str "\"" jolt-exe "\" build -m kmet.core --opt --dynamic -o \"" out-dir "/kmetj.exe\""))
(run-with-heartbeat 420
  (fn []
    (case variant
      "redirect"
      (do (bash "start-build" (str build-cmd " > build.log 2>&1 &"))
          (loop [i 0]
            (when (< i 80)
              (Thread/sleep 3000)
              (bash (str "poll-log-" i) "tail -3 build.log")
              (let [r (bash (str "poll-ps-" i) "ps | grep -i jolt | head -5")]
                (if (and (> i 3) (not (str/includes? (str (:output r)) "jolt")))
                  (log "build no longer running")
                  (recur (inc i)))))))
      "inherit"
      (do (bash "start-build-inherit" (str build-cmd " &"))
          (dotimes [i 80]
            (Thread/sleep 3000)
            (bash (str "poll-ps-" i) "ps | grep -i jolt | head -5")))
      "timeout"
      (let [sig (atom false)]
        (bash "ping-timeout" "ping -n 60 127.0.0.1" :timeout 5)
        (future (Thread/sleep 2000) (reset! sig true))
        (bash "ping-signal" "ping -n 60 127.0.0.1" :signal sig)))))
