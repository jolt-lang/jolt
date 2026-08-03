;; Self-checking regression for clojure.test: the assert-expr / do-report / report
;; extension points plus the built-in is/are/testing/thrown?/use-fixtures surface.
;; Run via bin/jolt; prints a single sentinel line the smoke gate greps for.
(ns clojure-test-selfcheck
  (:require [clojure.string :as str]
            [clojure.test :as t :refer [deftest is are testing use-fixtures run-tests]]))

;; a library-style custom assertion registered through the assert-expr seam
(defmethod t/assert-expr 'near? [msg form]
  (let [[_ a b] form]
    `(if (< (let [d# (- ~a ~b)] (if (neg? d#) (- d#) d#)) 0.01)
       (clojure.test/do-report {:type :pass})
       (clojure.test/do-report {:type :fail :message ~msg :form '~form}))))

;; an ALIAS-QUALIFIED registered assertion whose simple name collides with the
;; built-in thrown? — the registered method must win over the by-name inline
;; path (clojure-test-suite's portability/thrown? registers exactly this shape).
(defmethod t/assert-expr 'p/thrown? [msg form]
  `(try
     (do ~@(rest form))
     (clojure.test/do-report {:type :fail :message ~msg :form '~form})
     (catch Throwable e#
       (clojure.test/do-report {:type :pass})
       e#)))

;; a custom report type (how test.check surfaces trial/shrink progress)
(def trials (atom 0))
(defmethod t/report ::trial [_m] (swap! trials inc))

(def setups (atom 0))
(use-fixtures :each (fn [f] (swap! setups inc) (f)))

(deftest builtins
  (testing "equality + predicate"
    (is (= 1 1))
    (is (vector? [1])))
  (are [x y] (= x y)
    2 (+ 1 1)
    6 (* 2 3))
  ;; template vars substitute inside quote (are is clojure.template, not let)
  (are [x] (special-symbol? 'x)
    if
    def)
  (is (thrown? clojure.lang.ExceptionInfo (throw (ex-info "x" {}))))
  (is (thrown-with-msg? Exception #"bad" (throw (ex-info "bad" {}))))
  (is (near? 1.0 1.005))
  (is (p/thrown? (throw (ex-info "boom" {})))))

(deftest expected-fail
  (is (= 1 2))
  (is (near? 1.0 5.0)))

;; --- reference report shape --------------------------------------------------
;; A failing assertion under with-test-out must print the reference clojure.test
;; header "FAIL in (test-var-name) (file:line)" (and the ERROR variant). These
;; two deftests also run in the tallies above. This file runs unchanged on
;; reference Clojure 1.12.5, where the same test-var calls print the same
;; headers (verified on the reference).
(deftest header-fail
  (testing "ctx" (is (= 1 2) "a message")))

(deftest header-error
  (is (do (throw (ex-info "boom" {})) true)))

;; run-tests returns THIS call's summary; with explicit nses it runs only their
;; tests (an unknown ns runs nothing).
(def r1 (run-tests))
(def r2 (run-tests 'no.such.test-ns))

;; Cumulative counters, snapshotted BEFORE the deselection run below adds to them:
;; 10 pass (= + vector? + 4 are rows + thrown? + thrown-with-msg? + near? + p/thrown?),
;; 3 fail (expected-fail's 2 + header-fail), 1 error (header-error), 4 fixture runs.
(def cum-ok (and (= (t/n-pass) 10) (= (t/n-fail) 3) (= (t/n-error) 1) (= @setups 4)))

;; Deselection is by :test METADATA, not by what deftest registered: clojure.test
;; finds tests by scanning vars for that key, so a runner filters by removing it
;; (the Cognitect runner's -v/-i/-e do exactly this, then restore it). Running
;; from the registry alone ignored the removal and ran everything.
(def deselected
  (do (alter-meta! #'expected-fail (fn [m] (-> m (assoc ::held (:test m)) (dissoc :test))))
      (let [r (run-tests)]
        (alter-meta! #'expected-fail (fn [m] (-> m (assoc :test (::held m)) (dissoc ::held))))
        r)))
;; only the passing test remains: 3 tests, its 10 assertions, the header-fail and
;; header-error tests still fail/error
(def deselect-ok (and (= 3 (:test deselected)) (= 10 (:pass deselected))
                      (= 1 (:fail deselected)) (= 1 (:error deselected))))
;; restoring the metadata puts it back, failures and all
(def restored (run-tests))
(def restore-ok (and (= 4 (:test restored)) (= 3 (:fail restored)) (= 1 (:error restored))))

;; The header checks below run AFTER the tallies above, so they can call
;; test-var directly (their extra counter bumps land after cum-ok's snapshot).
(def header-shape-ok
  (let [sw (java.io.StringWriter.)]
    (binding [t/*test-out* sw] (t/test-var #'header-fail))
    (let [lines (str/split-lines (str sw))
          hdr (some #(re-find #"^FAIL in \(header-fail\) \(.*:\d+\)$" %) lines)
          ctx (some #(= "ctx" %) lines)
          msg (some #(= "a message" %) lines)
          exp (some #(= "expected: (= 1 2)" %) lines)
          act (some #(= "  actual: (not (= 1 2))" %) lines)]
      (and hdr ctx msg exp act))))

(def error-header-ok
  (let [sw (java.io.StringWriter.)]
    (binding [t/*test-out* sw] (t/test-var #'header-error))
    (boolean (some #(re-find #"^ERROR in \(header-error\) \(.*:\d+\)$" %)
                   (str/split-lines (str sw))))))

(t/do-report {:type ::trial})
(t/do-report {:type ::trial})

(let [ok (and (= 4 (:test r1)) (= 10 (:pass r1)) (= 3 (:fail r1)) (= 1 (:error r1))
              (= 0 (:test r2)) (= 0 (:pass r2))
              cum-ok deselect-ok restore-ok
              header-shape-ok error-header-ok
              (= @trials 2))]
  (println (if ok
             "CLOJURE-TEST OK"
             (str "CLOJURE-TEST FAIL r1=" (pr-str r1) " r2=" (pr-str r2)
                  " cum-ok=" cum-ok " (pass=" (t/n-pass) " fail=" (t/n-fail)
                  " error=" (t/n-error) " setups=" @setups ")"
                  " deselected=" (pr-str deselected) " restored=" (pr-str restored)
                  " header-shape-ok=" header-shape-ok " error-header-ok=" error-header-ok
                  " trials=" @trials))))
