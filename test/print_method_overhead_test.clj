;; A user print-method must not tax printing the values it does not reach.
;;
;; print-method's lookup runs for every printed value (io-streams.ss
;; user-print-method). With no user method it returns at once; with one, each
;; value used to compute its jolt-type tag (an arm walk and a long cond) and hash
;; it into the user-entry table, so one class-keyed method for an unrelated type
;; made printing a 10k-entry map of keywords and vectors ~30% slower, and one
;; tag-keyed method ~25% (reported against 0.8.28, reproduced at 7.0 -> 8.6-9.3ms).
;; A value of a runtime kind with no metadata is now answered per kind, once per
;; generation of the method table, and the same map measures ~1.08x.
;;
;; Judged as a ratio in ONE process: the same map printed with no user method,
;; then with each kind of method installed, the arms interleaved and the order
;; swapped every round, each arm its fastest sample on this thread's CPU clock.

(ns print-method-overhead-test)

(def ^:private m
  (into {} (for [i (range 10000)] [(keyword (str "k" i)) [i (str "s" i) {:a i}]])))

(deftype Unrelated [])

(defn- install! [arm]
  (case arm
    :none nil
    :class (defmethod print-method Unrelated [x w] (.write w "#unrelated"))
    :tag (defmethod print-method ::unrelated [x w] (.write w "#unrelated"))))

(defn- uninstall! []
  (remove-method print-method Unrelated)
  (remove-method print-method ::unrelated))

(def ^:private thread-mx (java.lang.management.ManagementFactory/getThreadMXBean))
(defn- cpu-ns [] (.getCurrentThreadCpuTime thread-mx))

(defn- sample [arm]
  (install! arm)
  (pr-str m)                            ; the first print after a change rebuilds the lookup state
  (System/gc)
  (let [t (cpu-ns)]
    (dotimes [_ 3] (pr-str m))
    (let [ns (- (cpu-ns) t)]
      (uninstall!)
      (/ ns 3.0))))

(def ^:private arms [:none :class :tag])
(def ^:private rounds 15)
(def ^:private max-ratio 1.15)

(defn- measure []
  (loop [i 0 best {}]
    (if (= i rounds)
      best
      (let [order (if (even? i) arms (reverse arms))]
        (recur (inc i)
               (reduce (fn [b arm] (update b arm (fnil min ##Inf) (sample arm))) best order))))))

(defn- ratios [best]
  {:class (/ (:class best) (:none best)) :tag (/ (:tag best) (:none best))})

(defn -main [& _]
  ;; the methods must still reach their own values
  (install! :class)
  (install! :tag)
  (let [s (pr-str {:a [1 (Unrelated.)] :b (with-meta [1] {:type ::unrelated})})]
    (uninstall!)
    (when-not (= s "{:a [1 #unrelated], :b #unrelated}")
      (println "FAIL print-method-overhead: user methods not honored:" s)
      (System/exit 1)))
  (let [over? (fn [r] (some #(> % max-ratio) (vals r)))
        r (ratios (measure))
        ;; a miss is measured once more before it counts: a real regression is
        ;; over in every round of both measurements
        r (if (over? r)
            (do (println (format "print-method-overhead: class %.2fx, tag %.2fx — over %.2f, measuring again"
                                 (:class r) (:tag r) max-ratio))
                (ratios (measure)))
            r)]
    (println (format "print-method-overhead: class-keyed %.2fx, tag-keyed %.2fx of no user method (ceiling %.2f)"
                     (:class r) (:tag r) max-ratio))
    (when (over? r)
      (println "FAIL print-method-overhead: an unrelated user print-method slows printing other values")
      (System/exit 1))))

(-main)
