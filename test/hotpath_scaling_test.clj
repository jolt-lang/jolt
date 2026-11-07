;; Hot-path shape gates: split-with-limit, timeout arming, deque draining,
;; StringTokenizer, ns-publics/refer, and set/intersection must not scale
;; worse than linearly (or must be independent of a dimension they used to
;; scale with). One file, one boot: each section is the same in-process
;; judgment as read_scaling_test.clj — best-of-5 over alternating arms, only
;; ratios, sized so a REGRESSED implementation still finishes and fails rather
;; than hanging.
;;
;; What each section pins (all were real, found in the 2026-08 structural
;; sweep):
;;   split      (.split s LIMIT) recomputed (length out) per part — O(parts^2).
;;   timeout    core.async timeout arming was a linear sorted-list insert —
;;              O(k^2) for a burst; now a binary min-heap.
;;   deque      ArrayDeque/LinkedList front ops shifted the whole backing
;;              vector — the standard .poll worklist idiom was O(n^2).
;;   tokenizer  StringTokenizer called length/list-ref per token — O(n^2).
;;   ns-shape   ns-publics/refer scanned EVERY interned var in the image, so a
;;              3-var namespace cost O(total vars) — judged as shape
;;              independence: interning 30k unrelated vars must not change
;;              what ns-publics of a tiny ns costs.
;;   set-shape  intersection walked its FIRST argument — big ∩ small must cost
;;              what small ∩ big costs (the reference's smaller-side swap).

(ns hotpath-scaling-test
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.core.async :as async]))

;; Milliseconds as a double, from the nanosecond clock: a row whose small arm
;; takes a millisecond or two is otherwise quantized to 1 or 2, and its ratio
;; to 5.0 or 10.0 — which is where the timeout row's noise came from.
;; This thread's CPU time, not the wall clock: under make -j beside other gates, a
;; preempted sample reads as a regression. The work timed here all runs on this
;; thread, collections included.
(def ^:private thread-mx (java.lang.management.ManagementFactory/getThreadMXBean))
(defn- cpu-ns [] (.getCurrentThreadCpuTime thread-mx))

(defn- timed [f]
  (let [t (cpu-ns)
        v (f)]
    [(/ (- (cpu-ns) t) 1e6) v]))

(def ^:private failures (atom 0))

(defn- judge [label t1 t4 ceiling detail]
  (let [t1 (max 0.05 t1)
        ratio (double (/ t4 t1))]
    (println (format "hotpath %-9s %7.1fms vs %7.1fms, ratio %6.2f (ceiling %.1f)"
                     label t1 t4 ratio (double ceiling)))
    (when (> ratio ceiling)
      (println (str "FAIL hotpath " label ": " detail))
      (swap! failures inc))))

;; Two arms measured together: best-of-5 each, the runs ALTERNATING arms so a
;; contended stretch of a shared CI runner lands on both. Measured separately,
;; a short arm nearly always finds one clean run and a long arm often does not,
;; which reads as the long arm being superlinear — the deque row read 8.33 on a
;; 4-vs-16 expectation that way, then the tokenizer row 8.50. A miss is measured
;; once more before it counts; a regression misses twice.
;;
;; Each sample starts from a fresh collection, as fastpath_ratio_test's do. The
;; timeout arms allocate ~0.6MB and ~2.4MB, and the pending timers they arm stay
;; live, so a collection that trips inside the big arm copies them all and the
;; small arm rarely pays one: with the nursery pinned at 4MB the row read 6.3-6.8
;; against its linear 4, and a CI run read 8.37 and then 8.38 against the 8.0
;; ceiling. After a collect neither arm reaches the nursery floor, and the row
;; reads 3.8-4.2 at every nursery size from 4MB up.
;;
;; judge-staged takes each arm as a STAGE: a fn run untimed before every sample
;; that puts the image in that arm's state and returns the thunk to time. It is
;; what lets an arm be a state of the whole image (ns-shape) and still alternate.
(defn- judge-staged [label sa sb ceiling detail]
  (let [sample (fn [stage] (let [f (stage)] (System/gc) (first (timed f))))
        measure (fn []
                  (let [ts (doall (repeatedly 5 #(vector (sample sa) (sample sb))))]
                    [(reduce min (map first ts)) (reduce min (map second ts))]))
        [ta tb] (measure)
        ratio (double (/ tb (max 0.05 ta)))]
    (if (<= ratio ceiling)
      (judge label ta tb ceiling detail)
      (do (println (format "hotpath %-9s %7.1fms vs %7.1fms, ratio %6.2f — over %.1f, measuring again"
                           label ta tb ratio (double ceiling)))
          (let [[ua ub] (measure)] (judge label ua ub ceiling detail))))))

(defn- judge-thunks [label fa fb ceiling detail]
  (judge-staged label (constantly fa) (constantly fb) ceiling detail))

;; A scaling row: f1 drains the base size, f4 four times it. The arms are
;; balanced to the SAME wall time for a linear implementation — f1 runs 4×reps
;; times, f4 reps times — so the ratio reads ~1 for linear and ~4 for quadratic
;; (the ceiling sits at 2), and contention inflates both arms alike rather than
;; only the longer one. The big arm's total work is what "still finishes when
;; regressed" is sized around, and it is unchanged: reps drains of 4n.
(defn- judge-scaling [label f1 f4 reps ceiling detail]
  (judge-thunks label #(dotimes [_ (* 4 reps)] (f1)) #(dotimes [_ reps] (f4)) ceiling detail))

;; --- split with a positive limit ---------------------------------------------
(defn- split-drain [n]
  (let [s (str/join "," (range n))]
    (count (str/split s #"," 10000000))))

;; --- timeout arming: k pending timers, far-future distinct deadlines ---------
(defn- arm-timeouts [k base-ms]
  (dotimes [i k] (async/timeout (+ base-ms i)))
  k)

;; --- deque drain -------------------------------------------------------------
(defn- deque-drain [n]
  (let [d (java.util.ArrayDeque.)]
    (dotimes [i n] (.addLast d i))
    (loop [c 0] (if (nil? (.poll d)) c (recur (inc c))))))

;; --- StringTokenizer drain ---------------------------------------------------
(defn- tok-drain [n]
  (let [s (str/join " " (repeat n "tok"))
        t (java.util.StringTokenizer. s)]
    (loop [c 0] (if (.hasMoreTokens t) (do (.nextToken t) (recur (inc c))) c))))

(defn -main [& _]
  ;; correctness spot-checks before any cost is judged
  (when-not (and (= (str/split "a,b,c" #"," 5) ["a" "b" "c"])
                 (= (str/split "a,b,c" #"," 2) ["a" "b,c"])
                 (= 5 (deque-drain 5))
                 (= 5 (tok-drain 5))
                 (= #{2} (set/intersection #{1 2} #{2 3}))
                 (= #{2} (set/intersection (set (range 1000)) #{2})))
    (println "FAIL hotpath: wrong results from a fixed path")
    (System/exit 1))

  ;; 4 drains of 4n against 16 drains of n: a linear drain reads ~1, a
  ;; quadratic one ~4 (a regressed shifting deque sat at 16 on a 4-vs-16
  ;; expectation, which is 4 here). The base arm alone measured ~3ms, under the
  ;; CI noise floor; the repetition grows the measurement without growing n.
  (let [n1 4000]
    (judge-scaling "split" #(split-drain n1) #(split-drain (* 4 n1)) 4 2.0
           "re-split is recomputing (length out) per part again (natives-str.ss)")
    (judge-scaling "deque" #(deque-drain n1) #(deque-drain (* 4 n1)) 4 2.0
           "ArrayDeque front ops are shifting the backing vector again (host-static-classes.ss)")
    (judge-scaling "tokenizer" #(tok-drain n1) #(tok-drain (* 4 n1)) 4 2.0
           "StringTokenizer is scanning its token list per token again (host-static-classes.ss)"))

  ;; timeout arming: a plain 1-vs-4 row, not a balanced one. Arming is not
  ;; idempotent — every run arms into the heap the earlier runs filled — and
  ;; repeating the small arm 4x would make it the SAME 4k inserts as the big
  ;; arm, so a regressed linear insert would read 1. Best-of over the growing
  ;; heap is sound the other way round: a heap costs log n more per run, a
  ;; regressed list costs n more, so a miss re-measured only reads worse. Each
  ;; run gets its own far-future deadline band so nothing fires mid-measure.
  (let [k 2000
        band (atom 0)
        arm (fn [n] #(arm-timeouts n (+ 3600000 (* 200000 (swap! band inc)))))]
    (judge-thunks "timeout" (arm k) (arm (* 4 k)) 8.0
           "timeout-insert! is walking the pending list per arm again (async.ss)"))

  ;; ns-publics shape independence: a tiny namespace's ns-publics must not get
  ;; slower because unrelated vars exist. The two arms are the same probe in two
  ;; states of the image, without and with 30k unrelated vars, and remove-ns
  ;; takes the image back, so the states alternate like every other row's arms.
  ;; This was one reading of each, taken before and after the interning, with
  ;; 300 reps that took 0.1ms: a loaded machine read 3.02 against the 3.0
  ;; ceiling. A regressed scan costs O(every var in the image) per call, which
  ;; is 10x and more here, so the ceiling has room either way.
  (let [_ (eval '(do (ns tiny-probe-ns) (def a 1) (def b 2) (def c 3) (ns hotpath-scaling-test)))
        reps 3000
        bulk (mapv #(symbol (str "bulk-ns-" %)) (range 30))
        without (fn [] (doseq [s bulk] (remove-ns s)))
        with (fn [] (doseq [s bulk]
                      (let [n (create-ns s)]
                        (dotimes [j 1000] (intern n (symbol (str "v" j)) 1)))))
        probe (fn [enter] (fn [] (enter) #(dotimes [_ reps] (ns-publics 'tiny-probe-ns))))]
    (judge-staged "ns-shape" (probe without) (probe with) 3.0
                  "ns-publics is scanning the whole var table again (ns.ss) — 30k unrelated vars changed a 3-var namespace's cost")
    (without))

  ;; intersection shape independence: big ∩ small vs small ∩ big.
  (let [big (set (range 100000))
        small #{1 2 3}
        reps 200]
    (judge-thunks "set-shape" #(dotimes [_ reps] (set/intersection small big)) #(dotimes [_ reps] (set/intersection big small)) 5.0
           "intersection is walking its larger argument (set.clj)"))

  ;; protocol-count shape independence: a record collection op looks for a
  ;; declared impl before falling back to the record behaviour, and that lookup
  ;; used to snapshot the type's protocol keys under a mutex and probe each one
  ;; — so count/contains?/seq on a record got slower the more protocols its type
  ;; implemented, while answering "none declared" every time. Two records with
  ;; identical fields, one implementing 1 protocol and one implementing 8: the
  ;; per-op cost must not track the protocol count.
  (let [reps 40000
        one (eval '(do (ns proto-shape-ns)
                       (defrecord POne [x y z])
                       (defprotocol PA (pa [_]))
                       (extend-protocol PA POne (pa [_] 1))
                       (->POne 1 2 3)))
        many (eval '(do (ns proto-shape-ns)
                        (defrecord PMany [x y z])
                        (defprotocol PB (pb [_])) (defprotocol PC (pc [_]))
                        (defprotocol PD (pd [_])) (defprotocol PE (pe [_]))
                        (defprotocol PF (pf [_])) (defprotocol PG (pg [_]))
                        (defprotocol PH (ph [_])) (defprotocol PI (pi* [_]))
                        (extend-protocol PB PMany (pb [_] 1))
                        (extend-protocol PC PMany (pc [_] 1))
                        (extend-protocol PD PMany (pd [_] 1))
                        (extend-protocol PE PMany (pe [_] 1))
                        (extend-protocol PF PMany (pf [_] 1))
                        (extend-protocol PG PMany (pg [_] 1))
                        (extend-protocol PH PMany (ph [_] 1))
                        (extend-protocol PI PMany (pi* [_] 1))
                        (->PMany 1 2 3)))
        _ (when-not (and (= 3 (count one)) (= 3 (count many))
                         (contains? one :x) (contains? many :x))
            (println "FAIL hotpath proto-shape: record ops answered wrong")
            (System/exit 1))
        probe (fn [r] #(dotimes [_ reps] (do (count r) (contains? r :x) (seq r))))]
    (judge-thunks "proto-shape" (probe one) (probe many) 3.0
           "record collection ops are re-walking the type's protocol table (find-method-any-protocol, protocols.ss) — 8 protocols cost more than 1"))

  (if (pos? @failures)
    (do (println (str "hotpath-scaling: " @failures " section(s) failed"))
        (System/exit 1))
    (println "hotpath-scaling: passed")))

(-main)
