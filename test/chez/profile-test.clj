;; jolt.profile gate — the execution-count profiler over Chez's compile-profile
;; counters. The fixture's line numbers are part of the expectations: fib(15)
;; makes 1973 calls, 986 of them recursive; sum-to 100 tests its loop 101 times.
;; Run: bin/jolt run test/chez/profile-test.clj (smoke.sh greps "PROFILE-TEST OK").
(ns profile-test
  (:require [clojure.string :as str]
            [jolt.profile :as prof]))

(def failures (atom []))
(defn check [label got want]
  (when-not (= got want)
    (swap! failures conj (str label ": want " (pr-str want) " got " (pr-str got)))))

(def fixture "test/chez/fixtures/profile_fixture.clj")
(defn fixture? [m] (str/ends-with? (str (:file m)) "profile_fixture.clj"))
(defn hits-at [rows line]
  (some #(when (and (fixture? %) (= line (:line %))) (:hits %)) rows))
(defn fn-row [rows sym] (first (filter #(= sym (:fn %)) rows)))

(check "off at start" (prof/instrumenting?) false)
(check "instrument! on" (prof/instrument!) true)
(check "on after instrument!" (prof/instrumenting?) true)
(load-file fixture)
(prof/instrument! false)
(check "off after instrument! false" (prof/instrumenting?) false)
;; a mode it does not know names itself, rather than turning the counters on
(check "instrument! refuses an unknown mode"
       (try (prof/instrument! :nope) :no-throw (catch Exception e (ex-data e)))
       {:mode :nope})
(check "a refused mode leaves instrumentation off" (prof/instrumenting?) false)
(check "instrument! :counts names the counters"
       (do (prof/instrument! :counts) (prof/instrument-mode)) :counts)
(prof/instrument! false)
(check "instrument! nil is off" (do (prof/instrument! nil) (prof/instrumenting?)) false)
;; loading ran each defn form once, but that is top-level code: no fn has run yet
(check "a loaded fn that never ran has no fn row"
       (fn-row (prof/fn-counts) 'profile-fixture/sum-to) nil)
(prof/clear!)

((resolve 'profile-fixture/fib) 15)
((resolve 'profile-fixture/sum-to) 100)

(def lines (prof/line-counts))
(check "if line runs once per call" (hits-at lines 4) 1973)
(check "recursive line" (hits-at lines 6) 986)
(check "loop test" (hits-at lines 10) 101)
(check "recur line" (hits-at lines 11) 100)
(check "lines sorted by hits" (= (map :hits lines) (sort > (map :hits lines))) true)
(check "line rows name their fn" (:fn (first (filter #(and (fixture? %) (= 6 (:line %))) lines)))
       'profile-fixture/fib)
;; this file was compiled before instrument!, so none of it carries a counter
(check "uninstrumented code has no rows"
       (some #(str/ends-with? (str (:file %)) "profile-test.clj") lines) nil)

(def fns (prof/fn-counts))
(check "fib hits" (:hits (fn-row fns 'profile-fixture/fib)) 1973)
(check "sum-to hits" (:hits (fn-row fns 'profile-fixture/sum-to)) 101)
(check "fib line" (:line (fn-row fns 'profile-fixture/fib)) 3)
(check "fib evals exceed hits" (> (:evals (fn-row fns 'profile-fixture/fib)) 1973) true)

(def report (with-out-str (prof/report)))
(check "report names fib" (str/includes? report "profile-fixture/fib") true)
(check "report shows the count" (str/includes? report "1973") true)

(def annotated (with-out-str (prof/annotate fixture)))
(check "annotate prints the hot line with its count"
       (boolean (some #(and (str/includes? % "1973") (str/includes? % "(if (< n 2)"))
                      (str/split-lines annotated)))
       true)

(prof/clear!)
(check "clear! drops every count" (hits-at (prof/line-counts) 4) nil)
((resolve 'profile-fixture/fib) 5)
(check "counting resumes after clear!" (hits-at (prof/line-counts) 4) 15)

;; code compiled with instrumentation off counts nothing
(load-file fixture)
(prof/clear!)
((resolve 'profile-fixture/fib) 15)
(check "recompiled uninstrumented" (hits-at (prof/line-counts) 4) nil)

;; Records' and multimethods' methods register no def of their own; they are
;; named after the form that defines them.
(prof/instrument!)
(load-file fixture)
(prof/clear!)
;; an eval'd form with no position of its own counts under no file, not under
;; whatever top-level form ran last (this file's, which is uninstrumented)
((resolve 'profile-fixture/eval-loop))
(prof/instrument! false)
(dotimes [_ 50] ((resolve 'profile-fixture/area) ((resolve 'profile-fixture/->Square) 3)))
(dotimes [_ 7] ((resolve 'profile-fixture/describe) {:kind :dot :n 1}))
(def fns2 (prof/fn-counts))
(check "record method row" (:hits (fn-row fns2 'profile-fixture/Square.area)) 50)
(check "record method line" (:line (fn-row fns2 'profile-fixture/Square.area)) 18)
(check "defmethod row" (:hits (fn-row fns2 (symbol "profile-fixture/describe[:dot]"))) 7)
(def lines2 (prof/line-counts))
(check "eval'd code is not charged to the caller's form"
       (some #(and (str/ends-with? (str (:file %)) "profile-test.clj") (> (:hits %) 1)) lines2)
       nil)
(check "eval'd code counts under no file"
       (boolean (some #(and (nil? (:file %)) (= 1001 (:hits %))) lines2)) true)

;; Code that is compiled and dropped gives its counters back, and what they
;; counted is kept: the defn forms' load-time code is gone after a collection.
(prof/instrument!)
(load-file fixture)
(def before-gc (hits-at (prof/line-counts) 3))
(dotimes [_ 300] (eval (list 'fn [] (list 'inc 1))))
(System/gc)
(def n1 (count (jolt.host/profile-sites)))
(dotimes [_ 3000] (eval (list 'fn [] (list 'inc 1))))
(System/gc)
(def n2 (count (jolt.host/profile-sites)))
(prof/instrument! false)
(check "load-time counts survive the collection" (hits-at (prof/line-counts) 3) before-gc)
(check "dropped code's counters are released" (< n2 (+ n1 100)) true)
(prof/clear!)

(let [m (prof/measure (reduce + (range 100000)))]
  (check "measure result" (:result m) 4999950000)
  (check "measure fields"
         (every? #(and (number? (m %)) (>= (m %) 0))
                 [:real-ms :cpu-ms :allocated-bytes :gc-count :gc-ms])
         true))
(check "measure counts allocation"
       (> (:allocated-bytes (prof/measure (vec (range 100000)))) 100000) true)

(if (empty? @failures)
  (println "PROFILE-TEST OK")
  (do (doseq [f @failures] (println "FAIL:" f))
      (println "PROFILE-TEST FAILED:" (count @failures))))
