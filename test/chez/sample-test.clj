;; jolt.profile sampling gate — the sampler over the per-thread tick scheduler.
;; hot spins three times as long as cold, both under driver, so their shares of
;; the samples are compared with each other rather than with a wall clock. They
;; alternate (sample_fixture.clj), so noise in the clock lands on both.
;; Run: bin/jolt run test/chez/sample-test.clj (smoke.sh greps "SAMPLE-TEST OK").
(ns sample-test
  (:require [clojure.string :as str]
            [jolt.profile :as prof]))

(def failures (atom []))
(defn check [label got want]
  (when-not (= got want)
    (swap! failures conj (str label ": want " (pr-str want) " got " (pr-str got)))))

(load-file "test/chez/fixtures/sample_fixture.clj")
(def driver (resolve 'sample-fixture/driver))
(def hot (resolve 'sample-fixture/hot))
(def n 20000000)

(defn row [rows sym] (first (filter #(= sym (:fn %)) rows)))

(check "not sampling at start" (prof/sampling?) false)
(check "sample! starts" (prof/sample! {:interval-ms 1}) true)
(check "sampling after sample!" (prof/sampling?) true)
(driver (* 2 n))
(prof/stop-sampling!)
(check "stopped" (prof/sampling?) false)

(def counts (prof/sample-counts))
(def hot-row (row counts 'sample-fixture/hot))
(def cold-row (row counts 'sample-fixture/cold))
(def spin-row (row counts 'sample-fixture/spin))
(def driver-row (row counts 'sample-fixture/driver))

(check "every fn on the path has a row"
       (every? some? [hot-row cold-row spin-row driver-row]) true)
(check "the loop is where the time is spent"
       (> (:self-ms spin-row) (* 0.8 (reduce + (map :self-ms counts)))) true)
(let [h (:total-ms hot-row) c (:total-ms cold-row)]
  (check (str "hot takes about three times cold (" h " / " c ")")
         (< 2.0 (/ h (max c 0.001)) 4.5) true))
(check "driver's total covers both"
       (>= (+ (:total-ms driver-row) 0.001)
           (* 0.95 (+ (:total-ms hot-row) (:total-ms cold-row))))
       true)
(check "rows carry the fixture file"
       (str/ends-with? (str (:file spin-row)) "sample_fixture.clj") true)
(check "rows carry the def line" (:line spin-row) 3)
(check "rows count samples" (pos? (:samples spin-row)) true)
(check "rows sorted by self time"
       (= (map :self-ms counts) (sort > (map :self-ms counts))) true)

;; The loop makes no call a line marker could sit before, so the frame stopped
;; in it reads at spin's own line, inside the fn either way.
(def lines (prof/sample-lines))
(check "the hottest line is in the loop's fn"
       (let [{:keys [file line fn]} (first lines)]
         (and (str/ends-with? (str file) "sample_fixture.clj") (<= 3 line 7)
              (= fn 'sample-fixture/spin)))
       true)

;; the stacks, root first, as collapsed lines
(def folded (with-out-str (prof/write-folded *out*)))
(check "folded stacks run root to leaf"
       (boolean (some #(str/includes? % "sample-fixture/driver;sample-fixture/hot;sample-fixture/spin ")
                      (str/split-lines folded)))
       true)
(check "folded lines end in a weight"
       (every? #(re-matches #".* \d+" %) (str/split-lines folded)) true)

(def report (with-out-str (prof/sample-report)))
(check "report names the loop" (str/includes? report "sample-fixture/spin") true)

;; another thread is sampled too, as its own thread
(prof/sample! {:interval-ms 1})
@(future (hot n))
(prof/stop-sampling!)
(def main-id (.getId (Thread/currentThread)))
(check "a future's thread is sampled"
       (boolean (some (fn [s] (and (not= main-id (:thread s))
                                   (some #(= 'sample-fixture/hot (:fn %)) (:stack s))))
                      (prof/samples)))
       true)

;; sample! starts over
(prof/sample! {:interval-ms 1})
(prof/stop-sampling!)
(check "a new run drops the old samples" (row (prof/sample-counts) 'sample-fixture/cold) nil)

(let [{:keys [result counts]} (prof/with-sampling {:interval-ms 1} (hot n))]
  (check "with-sampling returns the result" result (inc (* 3 (* 7/2 n))))
  (check "with-sampling returns the counts"
         (boolean (some #(= 'sample-fixture/spin (:fn %)) counts)) true))
(check "with-sampling stops" (prof/sampling?) false)

;; Threads that exist before sample! join it, whatever they are doing by then.
;; Each run gives its thread a fn nothing else runs in that run, and sample! drops
;; the run before it.
(def cold (resolve 'sample-fixture/cold))
(defn sampled-fn? [sym] (boolean (row (prof/sample-counts) sym)))

(def stop-spin (atom false))
(def spinning
  (doto (Thread. (fn [] (while (not @stop-spin) (cold 200000))))
    (.setDaemon true) (.start)))
(Thread/sleep 50)
(prof/sample! {:interval-ms 1})
(Thread/sleep 300)
(prof/stop-sampling!)
(reset! stop-spin true)
(.join spinning)
(check "a thread running flat out when sampling starts joins"
       (sampled-fn? 'sample-fixture/cold) true)

(def parked-gate (promise))
(def parked
  (doto (Thread. (fn [] @parked-gate (hot n)))
    (.setDaemon true) (.start)))
(Thread/sleep 50)
(prof/sample! {:interval-ms 1})
(deliver parked-gate true)
(.join parked)
(prof/stop-sampling!)
(check "a thread parked when sampling starts joins when it wakes"
       (sampled-fn? 'sample-fixture/hot) true)

@(future 1)
(Thread/sleep 50)
(prof/sample! {:interval-ms 1})
@(future (driver n))
(prof/stop-sampling!)
(check "a pool thread idle when sampling starts joins"
       (sampled-fn? 'sample-fixture/driver) true)

(if (empty? @failures)
  (println "SAMPLE-TEST OK")
  (do (doseq [f @failures] (println "FAIL:" f))
      (println "SAMPLE-TEST FAILED:" (count @failures))))
;; the future above leaves a pool thread behind, which the exit waits on for its
;; keep-alive, as the JVM's does
(shutdown-agents)
