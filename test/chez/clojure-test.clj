;; Self-checking regression for clojure.test: the assert-expr / do-report / report
;; extension points plus the built-in is/are/testing/thrown?/use-fixtures surface.
;; Run via bin/joltc; prints a single sentinel line the smoke gate greps for.
(ns clojure-test-selfcheck
  (:require [clojure.test :as t :refer [deftest is are testing use-fixtures run-tests]]))

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
  (is (thrown? clojure.lang.ExceptionInfo (throw (ex-info "x" {}))))
  (is (thrown-with-msg? Exception #"bad" (throw (ex-info "bad" {}))))
  (is (near? 1.0 1.005))
  (is (p/thrown? (throw (ex-info "boom" {})))))

(deftest expected-fail
  (is (= 1 2))
  (is (near? 1.0 5.0)))

;; run-tests returns THIS call's summary; with explicit nses it runs only their
;; tests (an unknown ns runs nothing).
(def r1 (run-tests))
(def r2 (run-tests 'no.such.test-ns))
(t/do-report {:type ::trial})
(t/do-report {:type ::trial})

;; 8 pass (= + vector? + 2 are rows + thrown? + thrown-with-msg? + near? + p/thrown?),
;; 2 fail (= 1 2, near? 1.0 5.0), 0 error, 2 fixture runs, 2 custom reports
(let [ok (and (= (t/n-pass) 8) (= (t/n-fail) 2) (= (t/n-error) 0)
              (= 2 (:test r1)) (= 8 (:pass r1)) (= 2 (:fail r1))
              (= 0 (:test r2)) (= 0 (:pass r2))
              (= @setups 2) (= @trials 2))]
  (println (if ok
             "CLOJURE-TEST OK"
             (str "CLOJURE-TEST FAIL pass=" (t/n-pass) " fail=" (t/n-fail)
                  " error=" (t/n-error) " r1=" (pr-str r1) " r2=" (pr-str r2)
                  " setups=" @setups " trials=" @trials))))
