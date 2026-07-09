;; Threaded tests for the tap system and the agent API. Prints TAP-AGENTS OK
;; when every check passes. (Timing-dependent behavior lives here rather than in
;; the corpus, which is pinned to deterministic JVM answers.)
(def failures (atom []))
(defn chk [label ok] (when-not ok (swap! failures conj label)))

;; Poll pred up to budget ticks of 5 ms; returns true once pred holds.
(defn wait-for [pred budget]
  (loop [i 0]
    (cond (pred) true
          (>= i budget) false
          :else (do (Thread/sleep 5) (recur (inc i))))))

;; --- taps ------------------------------------------------------------------
;; Values sent via tap> are delivered asynchronously to every registered fn;
;; a removed tap receives nothing; a nil round-trips as nil; a duplicate add-tap
;; registers the fn once (the tap set dedupes).

(let [seen (atom [])
      f (fn [x] (swap! seen conj x))]
  (add-tap f)
  (tap> :a)
  (tap> :b)
  (chk "taps: values delivered in order"
       (wait-for #(= @seen [:a :b]) 400))
  (remove-tap f)
  (tap> :c)
  (Thread/sleep 100)
  (chk "taps: removed tap receives nothing" (= @seen [:a :b])))

(let [seen (atom ::none)
      f (fn [x] (reset! seen x))]
  (add-tap f)
  (tap> nil)
  (chk "taps: nil round-trips as nil"
       (wait-for #(nil? @seen) 400))
  (remove-tap f))

(let [seen (atom 0)
      f (fn [_] (swap! seen inc))]
  (add-tap f)
  (add-tap f)
  (tap> :x)
  (wait-for #(pos? @seen) 400)
  (Thread/sleep 100)
  (chk "taps: duplicate add-tap registers once" (= @seen 1))
  (remove-tap f))

;; A throwing tap must not kill the delivery loop — a later tap still delivers.
(let [seen (atom [])
      good (fn [x] (swap! seen conj x))
      bad (fn [_] (throw (ex-info "boom" {})))]
  (add-tap bad)
  (add-tap good)
  (tap> :after-throw)
  (chk "taps: a throwing tap does not kill the loop"
       (wait-for #(some #{:after-throw} @seen) 400))
  (remove-tap bad)
  (remove-tap good))

;; --- agents: error modes --------------------------------------------------
;; :fail (default): a throwing action fails the agent; agent-error is set, a
;; later send rethrows the stored error, deref holds the last good state, and
;; restart-agent clears it. The error-handler fires in BOTH modes.
(let [a (agent 0)
      handled (atom [])]
  (set-error-handler! a (fn [_ e] (swap! handled conj e)))
  (send a (fn [_] (throw (ex-info "boom" {}))))
  (await a)
  (chk "agents:fail: agent-error set after a throwing action"
       (instance? Throwable (agent-error a)))
  (chk "agents:fail: deref holds last good state" (= @a 0))
  (chk "agents:fail: error-handler was invoked"
       (wait-for #(pos? (count @handled)) 100))
  (chk "agents:fail: send on a failed agent rethrows"
       (try (send a inc) false (catch Throwable _ true)))
  (restart-agent a 42 :clear-actions true)
  (chk "agents:fail: restart clears the error and sets state" (and (nil? (agent-error a)) (= @a 42))))

;; :continue: a throwing action leaves the state untouched and the agent keeps
;; running; a later send still applies.
(let [a (agent 0 :error-mode :continue)]
  (send a (fn [_] (throw (ex-info "x" {}))))
  (await a)
  (chk "agents:continue: state unchanged after failure" (= @a 0))
  (chk "agents:continue: not failed" (nil? (agent-error a)))
  (send a inc)
  (await a)
  (chk "agents:continue: processes a later action" (= @a 1)))

;; restart-agent on a healthy agent throws (Agent does not need a restart).
(let [a (agent 0)]
  (chk "agents: restart on healthy throws"
       (try (restart-agent a 0) false (catch Throwable _ true))))

;; --- agents: nested sends held until the action completes -----------------
;; A send dispatched from inside an action is held until the action returns,
;; then applied (so the nested mutation lands AFTER the action's own state).
(let [log (atom [])
      a (agent 0)]
  (send a (fn [s]
            (swap! log conj [:action s])
            (send a (fn [s2] (swap! log conj [:nested s2])))
            (inc s)))
  (chk "agents: nested send held then applied"
       (wait-for #(= [[:action 0] [:nested 1]] @log) 400)))

;; release-pending-sends inside an action flushes the held sends early and
;; returns the count; outside an action it returns 0.
(let [a (agent 0)
      flushed (atom nil)]
  (send a (fn [_]
            (send a (fn [_] :held-1))
            (send a (fn [_] :held-2))
            (reset! flushed (release-pending-sends))))
  (chk "agents: release-pending-sends returns held count"
       (wait-for #(= @flushed 2) 400))
  (chk "agents: release-pending-sends outside an action is 0"
       (zero? (release-pending-sends))))

;; --- agents: await-for timeout --------------------------------------------
;; await-for on an agent whose action never completes returns false on timeout.
(let [a (agent 0)
      slow (agent 0)]
  ;; an action that parks: it awaits ANOTHER agent that never receives work.
  ;; (await inside an action is illegal, so block on a deref of an undelivered
  ;;  promise with a bound instead.)
  (send slow (fn [_] (deref (promise) 800 :timed-out)))
  (chk "agents: await-for returns false on timeout"
       (not (await-for 50 slow))))

;; --- agents: shutdown-agents ----------------------------------------------
;; After shutdown-agents, send throws RejectedExecutionException; in-flight
;; workers may finish their queues.
(let [a (agent 0)]
  (shutdown-agents)
  (chk "agents: send after shutdown throws RejectedExecutionException"
       (instance? java.util.concurrent.RejectedExecutionException
                  (try (send a inc) nil (catch Throwable e e)))))

(if (empty? @failures)
  (println "TAP-AGENTS OK")
  (doseq [f @failures] (println "FAIL:" f)))
