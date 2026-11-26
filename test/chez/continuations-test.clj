;; continuations-test.clj — the jolt.continuations gate (issue #736).
;;
;; Pins the ONE-SHOT ESCAPE contract call-cc/letcc expose over the host's
;; call/1cc:
;;   * an escape returns its value from the call-cc form, from any depth;
;;   * falling through returns the body's value;
;;   * a `finally` between the capture and the escape RUNS (an escape is a real
;;     exit, unlike a fiber park, which drops those winders);
;;   * a binding/parameterize between them is restored;
;;   * a park between the capture and the escape is fine WITHIN one fiber — the
;;     fiber's stack segment is captured and restored whole.
;;
;; And the four ways to misuse it, each of which must raise a catchable jolt
;; error naming what happened. Two of them are why this file exists: before the
;; guard, invoking a continuation captured on another fiber HUNG the process
;; (no error, no timeout), and re-invoking one raised a Chez condition that
;; reached jolt as class :object: with an empty message.
;;
;; Every row that invokes a continuation across an ownership boundary runs on a
;; watchdog thread with a join deadline, so a regression that reinstates the
;; hang FAILS this gate instead of wedging it.

(ns continuations-test
  (:require [jolt.continuations :as k]
            [jolt.fibers :as fib]
            [jolt.scheme]))

(def failures (atom []))
(defn check [label pred]
  (when-not pred (swap! failures conj label)))

;; Run (f) on its own thread and answer [:ok v] / [:threw class message], or
;; :timeout when it does not finish inside ms. A guard that fails open shows up
;; here as :timeout rather than hanging the gate.
(defn within [ms f]
  (let [box (atom :timeout)
        t (Thread. (fn []
                     (reset! box
                             (try [:ok (f)]
                                  (catch Throwable e
                                    [:threw (.getName (class e)) (ex-message e)])))))]
    (.start t)
    (.join t ms)
    @box))

(defn illegal-state?
  "The result of `within` is a raise of IllegalStateException whose message
  mentions what the caller did wrong."
  [result needle]
  (and (vector? result)
       (= :threw (nth result 0))
       (= "java.lang.IllegalStateException" (nth result 1))
       (string? (nth result 2))
       (clojure.string/includes? (nth result 2) needle)))

;; --- escaping ----------------------------------------------------------------

(check "escape returns its value from call-cc"
       (= :escaped (k/call-cc (fn [escape] (escape :escaped) :not-reached))))

(check "falling through returns the body value"
       (= :fell-through (k/call-cc (fn [_] :fell-through))))

(check "escape from inside a loop"
       (= 3 (k/call-cc (fn [escape]
                         (doseq [x [1 2 3 4 5]]
                           (when (= x 3) (escape x)))
                         :not-reached))))

(check "escape from inside a reduce"
       (= :negative (k/call-cc (fn [escape]
                                 (reduce (fn [acc x]
                                           (if (neg? x) (escape :negative) (+ acc x)))
                                         0 [1 2 -3 4])))))

(check "escape from a deeply nested non-tail recursion"
       (= :deep (k/call-cc (fn [escape]
                             (letfn [(down [n] (if (zero? n) (escape :deep) (inc (down (dec n)))))]
                               (down 200))))))

(check "escape carries nil"
       (nil? (k/call-cc (fn [escape] (escape nil) :not-reached))))

(check "escape carries false"
       (false? (k/call-cc (fn [escape] (escape false) :not-reached))))

(check "zero-arity escape answers nil"
       (nil? (k/call-cc (fn [escape] (escape) :not-reached))))

;; --- letcc is call-cc ---------------------------------------------------------

(check "letcc escapes"
       (= 3 (k/letcc [escape]
              (doseq [x [1 2 3 4 5]] (when (= x 3) (escape x)))
              :not-reached)))

(check "letcc falls through to its last form"
       (= :fell-through (k/letcc [escape] :ignored :fell-through)))

(check "letcc body sees the enclosing scope"
       (= 42 (let [n 42] (k/letcc [escape] (escape n)))))

;; --- nesting ------------------------------------------------------------------

(check "an inner escape does not disturb the outer capture"
       (= [:inner :outer-done]
          (k/call-cc (fn [outer]
                       [(k/call-cc (fn [inner] (inner :inner) :not-reached))
                        :outer-done]))))

(check "an inner body can escape the OUTER capture"
       (= :straight-out
          (k/call-cc (fn [outer]
                       (k/call-cc (fn [_inner] (outer :straight-out)))
                       :not-reached))))

;; --- escape-fn? ---------------------------------------------------------------

(check "escape-fn? is true for the captured escape"
       (true? (k/call-cc (fn [escape] (k/escape-fn? escape)))))

(check "escape-fn? is false for an ordinary fn"
       (false? (k/escape-fn? (fn [_] nil))))

(check "escape-fn? is false for a non-fn"
       (and (false? (k/escape-fn? 1))
            (false? (k/escape-fn? nil))
            (false? (k/escape-fn? :escape))))

;; --- unwinding: an escape is a real exit ---------------------------------------

(check "finally runs when the escape leaves a try"
       (= [:finally]
          (let [log (atom [])]
            (k/call-cc (fn [escape]
                         (try (escape :out) (finally (swap! log conj :finally)))))
            @log)))

(check "nested finallys run innermost first"
       (= [:inner :outer]
          (let [log (atom [])]
            (k/call-cc (fn [escape]
                         (try (try (escape :out)
                                   (finally (swap! log conj :inner)))
                              (finally (swap! log conj :outer)))))
            @log)))

(check "the escape's value survives a finally that runs after it"
       (= :out (let [log (atom [])]
                 (k/call-cc (fn [escape]
                              (try (escape :out)
                                   (finally (swap! log conj :finally))))))))

(check "a dynamic binding is restored by the escape"
       (= [:inner :root]
          (let [seen (atom nil)]
            [(binding [*print-readably* :inner]
               (k/call-cc (fn [escape] (escape *print-readably*))))
             (do (reset! seen :root) @seen)])))

(check "an escape does not run a finally it never entered"
       (= [] (let [log (atom [])]
               (k/call-cc (fn [escape] (escape :out)))
               (try :untouched (finally nil))
               @log)))

;; --- fibers -------------------------------------------------------------------

(check "capture and escape inside one fiber"
       (= :fiber-escape
          (fib/join (fib/spawn (fn [] (k/call-cc (fn [escape] (escape :fiber-escape))))))))

(check "a park between the capture and the escape is fine within one fiber"
       (= :after-park
          (fib/join (fib/spawn (fn []
                                 (k/call-cc (fn [escape]
                                              (fib/yield)
                                              (escape :after-park))))))))

(check "a parked deref between the capture and the escape is fine within one fiber"
       (= :after-deref
          (fib/join (fib/spawn (fn []
                                 (k/call-cc (fn [escape]
                                              (let [p (promise)]
                                                (fib/spawn (fn [] (deliver p :v)))
                                                @p
                                                (escape :after-deref)))))))))

;; --- the four misuses ----------------------------------------------------------
;; Each must RAISE, and each raise must say which rule was broken.

(check "re-invoking an escape that already fired raises"
       (illegal-state?
        (within 5000
                (fn []
                  (let [saved (atom nil)]
                    (k/call-cc (fn [escape] (reset! saved escape) (escape :first)))
                    (@saved :again))))
        "already"))

(check "invoking an escape after its call-cc returned normally raises"
       (illegal-state?
        (within 5000
                (fn []
                  (let [saved (atom nil)]
                    (k/call-cc (fn [escape] (reset! saved escape) :fell-through))
                    (@saved :too-late))))
        "no longer"))

(check "invoking an escape from another thread raises rather than hanging"
       (illegal-state?
        (let [saved (atom nil)
              ready (promise)]
          (k/call-cc (fn [escape] (reset! saved escape) (deliver ready true) :captured))
          @ready
          (within 5000 (fn [] (@saved :cross-thread))))
        "another"))

(check "invoking an escape captured on a dead fiber raises rather than hanging"
       (illegal-state?
        (let [saved (atom nil)]
          (fib/join (fib/spawn (fn [] (k/call-cc (fn [escape] (reset! saved escape) :done)))))
          (within 5000 (fn [] (@saved :cross-fiber))))
        "another"))

(check "invoking an escape captured on a LIVE other fiber raises rather than hanging"
       (illegal-state?
        (let [saved (promise)
              hold (promise)
              f (fib/spawn (fn []
                             (k/call-cc (fn [escape]
                                          (deliver saved escape)
                                          @hold
                                          :done))))
              escape @saved
              result (within 5000 (fn [] (escape :cross-fiber)))]
          (deliver hold true)
          (fib/join f)
          result)
        "another"))

;; --- the guard does not cost correctness on the happy path ---------------------

(check "a fresh capture after a spent one still escapes"
       (= [:first :second]
          (let [saved (atom nil)]
            [(k/call-cc (fn [escape] (reset! saved escape) (escape :first)))
             (k/call-cc (fn [escape] (escape :second)))])))

(check "many captures in a loop each escape independently"
       (= (range 50)
          (map (fn [n] (k/call-cc (fn [escape] (escape n)))) (range 50))))

;; --- the escape registry under concurrency -------------------------------------
;; escape-fn? is backed by a process-wide weak table, so captures on several
;; threads WRITE it while escape-fn? READS it. Unguarded that is the
;; writer-vs-writer collector fault, which shows up as a crash rather than a
;; wrong answer — so this row is here to run the shape, not to assert a value.

(check "concurrent captures and escape-fn? across threads"
       (= (range 6)
          (let [results (atom {})
                ts (doall (for [t (range 6)]
                            (Thread. (fn []
                                       (dotimes [i 2000]
                                         (k/call-cc (fn [escape]
                                                      (k/escape-fn? escape)
                                                      (escape i))))
                                       (swap! results assoc t t)))))]
            (doseq [t ts] (.start t))
            (doseq [t ts] (.join t 60000))
            (sort (vals @results)))))

(check "concurrent captures across fibers sharing carriers"
       (= [:ok]
          (let [fs (doall (for [_ (range 16)]
                            (fib/spawn (fn []
                                         (dotimes [i 500]
                                           (k/call-cc (fn [escape]
                                                        (k/escape-fn? escape)
                                                        (escape i))))
                                         :ok))))]
            (distinct (map fib/join fs)))))

;; --- the escape path is one-shot all the way down ----------------------------
;; A capture and an escape are a call/1cc, a record and a closure: a few dozen
;; bytes. The three checks (spent / owner / live) are field reads. They once
;; wrapped get-thread-id and the fiber vreg in a `guard` — a full call/cc each,
;; which promoted the one-shot continuation to multi-shot so every return
;; through it COPIED the stack segment: 4.6 KB and ~190 ns per escape, 43% of
;; standard-clojure-style's parse time (a letcc per Choice attempt). Bytes are
;; deterministic where nanoseconds are not, so the invariant is allocation:
;; measured in one process over many iterations against the host's own
;; call/1cc, an escape may cost a small constant more, never a stack copy.

(defn allocated-total []
  (jolt.scheme/eval-string "(sstats-bytes (statistics))"))
(defn bytes-per [n f]
  (f)
  (let [before (allocated-total)]
    (dotimes [_ n] (f))
    (quot (- (allocated-total) before) n)))

(check "an escape allocates a few objects, not a stack segment"
       (let [per (bytes-per 20000 (fn [] (k/letcc [escape] (escape 1))))]
         (or (< per 400) (do (println "  escape allocated" per "bytes") false))))

(check "a normal return through call-cc allocates a few objects, not a stack segment"
       (let [per (bytes-per 20000 (fn [] (k/letcc [escape] 1)))]
         (or (< per 400) (do (println "  normal return allocated" per "bytes") false))))

(if (empty? @failures)
  (do (println "CONTINUATIONS-TEST OK") (flush) (System/exit 0))
  (do (doseq [failure @failures] (println "FAIL:" failure))
      (flush)
      (System/exit 1)))
