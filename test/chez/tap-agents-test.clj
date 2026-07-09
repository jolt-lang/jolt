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

(if (empty? @failures)
  (println "TAP-AGENTS OK")
  (doseq [f @failures] (println "FAIL:" f)))
