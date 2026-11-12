;; jolt.fibers gate — the public lower-level fiber API (epic jolt-of08.1),
;; side by side with core.async: spawn a body on a fiber, join its value,
;; watch its completion. Run: bin/jolt run test/chez/jolt-fibers-test.clj
;; (smoke.sh greps for "JOLT-FIBERS-TEST OK").
(ns jolt-fibers-test)

(require '[jolt.fibers :as fib])
(require '[clojure.core.async :as a])

(def failures (atom []))

;; announce BEFORE evaluating, and flush: a check that parks and is never
;; resumed must name itself in the log rather than hang silently.
(defmacro check-eq [label got want]
  `(do
     (print (str "  .. " ~label "\n"))
     (flush)
     (let [g# ~got w# ~want]
       (when-not (= g# w#)
         (swap! failures conj (str ~label ": want " (pr-str w#) " got " (pr-str g#)))))))

;; spawn returns a fiber handle; join returns the body's value
(let [f (fib/spawn (fn [] (+ 1 2)))]
  (check-eq "fiber? on a handle" (fib/fiber? f) true)
  (check-eq "join returns the value" (fib/join f) 3))

(check-eq "fiber? on not-a-fiber" [(fib/fiber? 42) (fib/fiber? nil)] [false false])

;; a body's nil and false come back as themselves
(check-eq "join returns nil" (fib/join (fib/spawn (fn [] nil))) nil)
(check-eq "join returns false" (fib/join (fib/spawn (fn [] false))) false)

;; off-fiber context
(check-eq "current-fiber off a fiber" (fib/current-fiber) nil)
(check-eq "in-fiber? off a fiber" (fib/in-fiber?) false)

;; inside the body: in-fiber?, and current-fiber is the spawned handle
(let [f (fib/spawn (fn [] [(fib/in-fiber?) (fib/current-fiber)]))
      [in? cur] (fib/join f)]
  (check-eq "in-fiber? inside" in? true)
  (check-eq "current-fiber inside is the handle" (identical? cur f) true))

;; joining a finished fiber again answers the same value (terminal state is stable)
(let [f (fib/spawn (fn [] :once))]
  (check-eq "join twice" [(fib/join f) (fib/join f)] [:once :once]))

;; dynamic bindings convey at spawn; *txn* never does (runtime contract)
(def ^:dynamic *conveyed* :root)
(check-eq "binding conveys into spawn"
          (fib/join (binding [*conveyed* :bound] (fib/spawn (fn [] *conveyed*))))
          :bound)

;; yield: a body that yields still completes
(check-eq "yield completes"
          (fib/join (fib/spawn (fn [] (fib/yield) (fib/yield) :done-after-yields)))
          :done-after-yields)

;; yield off a fiber is a caller bug and says so
(check-eq "yield off a fiber throws"
          (try (fib/yield) :no-throw (catch Exception e :threw))
          :threw)

;; a throwing body: join rethrows the original exception, data intact
(let [f (fib/spawn (fn [] (throw (ex-info "boom" {:kind :test}))))]
  (check-eq "join rethrows"
            (try (fib/join f)
                 (catch clojure.lang.ExceptionInfo e [(ex-message e) (:kind (ex-data e))]))
            ["boom" :test]))

;; a host-raised error (typed JVM throwable) rethrows the same way
(let [f (fib/spawn (fn [] (/ 1 0)))]
  (check-eq "join rethrows a host error"
            (try (fib/join f) (catch ArithmeticException e :arithmetic))
            :arithmetic))

;; join from inside another fiber parks it (no carrier pinned, value propagates)
(check-eq "join from a fiber"
          (fib/join (fib/spawn (fn [] (inc (fib/join (fib/spawn (fn [] 41)))))))
          42)

;; timed join: a parked fiber times out; the same fiber joins after delivery
(let [p (promise)
      f (fib/spawn (fn [] @p))]
  (check-eq "timed join times out" (fib/join f 50 :timed-out) :timed-out)
  (deliver p :finally)
  (check-eq "timed join after deliver" (fib/join f 10000 :timed-out) :finally))

;; The waits below are hang caps, not timings: a callback that never runs, or a
;; fiber that never parks, fails at the cap either way, while a correct one
;; answers in microseconds. They were 1s, which one loaded scheduler delay could
;; cross; 10s costs nothing when the answer comes.
;; monitor!: success hands the callback nil
(let [got (promise)
      f (fib/spawn (fn [] :ok))]
  (fib/monitor! f (fn [err] (deliver got [:cb err])))
  (check-eq "monitor on success" (deref got 10000 :cb-never-ran) [:cb nil]))

;; monitor!: a death hands it the error
(let [got (promise)
      f (fib/spawn (fn [] (throw (ex-info "dead-fiber" {}))))]
  (fib/monitor! f (fn [err] (deliver got (ex-message err))))
  (check-eq "monitor on death" (deref got 10000 :cb-never-ran) "dead-fiber"))

;; monitor! on an already-finished fiber fires inline (no register-vs-die race)
(let [f (fib/spawn (fn [] :early))]
  (fib/join f)
  (let [got (atom :not-yet)]
    (fib/monitor! f (fn [err] (reset! got err)))
    (check-eq "late monitor fires inline" @got nil)))

;; state: terminal states
(let [f (fib/spawn (fn [] :fin))]
  (fib/join f)
  (check-eq "state after success" (fib/state f) :done))
(let [f (fib/spawn (fn [] (throw (ex-info "x" {}))))]
  (try (fib/join f) (catch Exception _ nil))
  (check-eq "state after death" (fib/state f) :dead))

;; a fiber waiting on a promise parks (deref is fiber-aware), then completes
(let [p (promise)
      f (fib/spawn (fn [] @p))]
  (loop [tries 0]
    (when (and (not= :parked (fib/state f)) (< tries 2000))
      (Thread/sleep 5)
      (recur (inc tries))))
  (check-eq "deref parks the fiber" (fib/state f) :parked)
  (deliver p :released)
  (check-eq "released fiber completes" (fib/join f) :released))

;; self-join throws rather than deadlocking
(check-eq "self-join throws"
          (fib/join (fib/spawn (fn [] (try (fib/join (fib/current-fiber)) :no-throw
                                           (catch Exception e :threw)))))
          :threw)

;; channel ops park a spawned fiber exactly as they do a :fiber go body
(let [ch (a/chan)
      f (fib/spawn (fn [] (a/<!! ch)))]
  (a/>!! ch :through-the-channel)
  (check-eq "channel take inside spawn" (fib/join f) :through-the-channel))

;; fibers lists every fiber that hasn't finished, whoever spawned it (jolt#1606):
;; one from spawn, one from io-thread (which never goes through spawn), one in a
;; named pool, and the caller's own; and none of them once they've finished.
(let [gate (a/chan)
      p (fib/pool {:name :listing :size 1})
      spawned (fib/spawn (fn [] (a/<!! gate)))
      pooled (fib/spawn (fn [] (a/<!! gate)) {:pool p})
      io-self (promise)
      io-ch (a/io-thread (deliver io-self (fib/current-fiber)) (a/<!! gate))
      io-fib (a/<!! (a/io-thread (fib/current-fiber)))   ; a finished one
      inner (fib/join (fib/spawn (fn [] (let [me (fib/current-fiber)]
                                          (boolean (some #(identical? me %) (fib/fibers)))))))
      listed? (fn [f] (boolean (some #(identical? f %) (fib/fibers))))]
  (check-eq "fibers returns a vector" (vector? (fib/fibers)) true)
  (check-eq "fibers lists a spawned fiber" (listed? spawned) true)
  (check-eq "fibers lists a pool's fiber" (listed? pooled) true)
  (check-eq "fibers lists the caller's own" inner true)
  (check-eq "fibers lists an io-thread body" (listed? @io-self) true)
  ;; its channel has the value before the fiber finishes, so join first
  (fib/join io-fib)
  (check-eq "a finished io-thread fiber is not listed" (listed? io-fib) false)
  (a/close! gate)
  (fib/join spawned) (fib/join pooled) (a/<!! io-ch)
  (check-eq "finished fibers drop out"
            [(listed? spawned) (listed? pooled)] [false false])
  (fib/shutdown! p))

;; enough finished fibers to set off the registry's lazy prune (it runs once the
;; finishes since the last one pass 4096): a fiber still parked survives it, and
;; none of the finished ones are listed after it.
(let [gate (a/chan)
      parked (fib/spawn (fn [] (a/<!! gate)))
      done (mapv (fn [_] (fib/spawn (fn [] :ok))) (range 5000))]
  (run! fib/join done)
  (fib/join (fib/spawn (fn [] :after)))   ; a spawn past the threshold prunes
  (let [listed (set (fib/fibers))]
    (check-eq "a parked fiber survives the prune" (contains? listed parked) true)
    (check-eq "no finished fiber is listed after the prune"
              (some #(contains? listed %) done) nil))
  (a/close! gate)
  (fib/join parked))

;; the knobs
(check-eq "carrier-count is positive" (pos? (fib/carrier-count)) true)
(check-eq "preempt-ticks is positive" (pos? (fib/preempt-ticks)) true)
(let [orig (fib/preempt-ticks)]
  (fib/set-preempt-ticks! (* 2 orig))
  (check-eq "set-preempt-ticks! takes" (fib/preempt-ticks) (* 2 orig))
  (fib/set-preempt-ticks! nil)                     ; nil restores the default
  (check-eq "nil restores the default" (fib/preempt-ticks) orig))
(check-eq "preempt floor refused"
          (try (fib/set-preempt-ticks! 1) :no-throw (catch Exception e :threw))
          :threw)
(check-eq "set-carrier-count! roundtrip"
          (do (fib/set-carrier-count! (fib/carrier-count)) (fib/carrier-count))
          (fib/carrier-count))

(if (empty? @failures)
  (println "JOLT-FIBERS-TEST OK")
  (do (doseq [f @failures] (println "FAIL:" f))
      (println "JOLT-FIBERS-TEST FAILED:" (count @failures))))
