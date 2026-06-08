(ns clojure.data-test.diff
  (:require [clojure.test :refer [deftest is testing]]
;; NOTE (jolt): sequential-diff expectations corrected to match real Clojure —
;; clojure.data pads only to the max differing index (e.g. (diff [1 2 3] [1 9 3])
;; -> a=[nil 2], not [nil 2 nil]). The upstream clojurust fixtures had this wrong.
            [clojure.data :refer [diff]]))

;; ── Atoms ────────────────────────────────────────────────────────────────────

(deftest test-diff-equal-atoms
  (testing "equal atoms"
    (is (= [nil nil :a] (diff :a :a)))
    (is (= [nil nil 1] (diff 1 1)))
    (is (= [nil nil "hello"] (diff "hello" "hello")))
    (is (= [nil nil nil] (diff nil nil)))
    (is (= [nil nil true] (diff true true)))))

(deftest test-diff-unequal-atoms
  (testing "unequal atoms"
    (is (= [:a :b nil] (diff :a :b)))
    (is (= [1 2 nil] (diff 1 2)))
    (is (= ["a" "b" nil] (diff "a" "b")))
    (is (= [nil 1 nil] (diff nil 1)))
    (is (= [true false nil] (diff true false)))))

;; ── Maps ─────────────────────────────────────────────────────────────────────

(deftest test-diff-equal-maps
  (testing "equal maps"
    (is (= [nil nil {:a 1 :b 2}] (diff {:a 1 :b 2} {:a 1 :b 2})))
    (is (= [nil nil {}] (diff {} {})))))

(deftest test-diff-maps-only-in-a
  (testing "keys only in a"
    (let [[a b both] (diff {:a 1 :b 2} {:a 1})]
      (is (= {:b 2} a))
      (is (nil? b))
      (is (= {:a 1} both)))))

(deftest test-diff-maps-only-in-b
  (testing "keys only in b"
    (let [[a b both] (diff {:a 1} {:a 1 :b 2})]
      (is (nil? a))
      (is (= {:b 2} b))
      (is (= {:a 1} both)))))

(deftest test-diff-maps-different-values
  (testing "same keys, different values"
    (let [[a b both] (diff {:a 1 :b 2} {:a 1 :b 9})]
      (is (= {:b 2} a))
      (is (= {:b 9} b))
      (is (= {:a 1} both)))))

(deftest test-diff-maps-nested
  (testing "nested maps"
    (let [[a b both] (diff {:a {:x 1 :y 2}} {:a {:x 1 :z 3}})]
      (is (= {:a {:y 2}} a))
      (is (= {:a {:z 3}} b))
      (is (= {:a {:x 1}} both)))))

(deftest test-diff-maps-disjoint
  (testing "completely disjoint maps"
    (let [[a b both] (diff {:a 1} {:b 2})]
      (is (= {:a 1} a))
      (is (= {:b 2} b))
      (is (nil? both)))))

;; ── Sets ─────────────────────────────────────────────────────────────────────

(deftest test-diff-equal-sets
  (testing "equal sets"
    (is (= [nil nil #{1 2 3}] (diff #{1 2 3} #{1 2 3})))
    (is (= [nil nil #{}] (diff #{} #{})))))

(deftest test-diff-sets
  (testing "overlapping sets"
    (let [[a b both] (diff #{1 2 3} #{2 3 4})]
      (is (= #{1} a))
      (is (= #{4} b))
      (is (= #{2 3} both)))))

(deftest test-diff-disjoint-sets
  (testing "disjoint sets"
    (let [[a b both] (diff #{1 2} #{3 4})]
      (is (= #{1 2} a))
      (is (= #{3 4} b))
      (is (nil? both)))))

;; ── Vectors / Sequential ────────────────────────────────────────────────────

(deftest test-diff-equal-vectors
  (testing "equal vectors"
    (is (= [nil nil [1 2 3]] (diff [1 2 3] [1 2 3])))
    (is (= [nil nil []] (diff [] [])))))

(deftest test-diff-vectors-same-length
  (testing "same length, different elements"
    (let [[a b both] (diff [1 2 3] [1 9 3])]
      (is (= [nil 2] a))
      (is (= [nil 9] b))
      (is (= [1 nil 3] both)))))

(deftest test-diff-vectors-different-length
  (testing "different lengths"
    (let [[a b both] (diff [1 2 3] [1 2])]
      (is (= [nil nil 3] a))
      (is (nil? b))
      (is (= [1 2] both)))
    (let [[a b both] (diff [1] [1 2 3])]
      (is (nil? a))
      (is (= [nil 2 3] b))
      (is (= [1] both)))))

(deftest test-diff-lists
  (testing "lists treated as sequential"
    (let [[a b both] (diff '(1 2 3) '(1 9 3))]
      (is (= [nil 2] a))
      (is (= [nil 9] b))
      (is (= [1 nil 3] both)))))

;; ── Mixed types ─────────────────────────────────────────────────────────────

(deftest test-diff-mixed-types
  (testing "different partition types treated as atoms"
    (is (= [{:a 1} [1 2] nil] (diff {:a 1} [1 2])))
    (is (= [#{1} [1] nil] (diff #{1} [1])))
    (is (= [1 :a nil] (diff 1 :a)))))

;; ── Nil handling ────────────────────────────────────────────────────────────

(deftest test-diff-nil
  (testing "nil vs non-nil"
    (is (= [nil 1 nil] (diff nil 1)))
    (is (= [1 nil nil] (diff 1 nil)))
    (is (= [nil {:a 1} nil] (diff nil {:a 1})))))

;; ── Deeply nested ───────────────────────────────────────────────────────────

(deftest test-diff-deeply-nested
  (testing "deeply nested structures"
    (let [[a b both] (diff {:a {:b {:c 1}}} {:a {:b {:c 2}}})]
      (is (= {:a {:b {:c 1}}} a))
      (is (= {:a {:b {:c 2}}} b))
      (is (nil? both))))
  (testing "deeply nested with shared keys"
    (let [[a b both] (diff {:a {:b 1 :c 2}} {:a {:b 1 :c 9}})]
      (is (= {:a {:c 2}} a))
      (is (= {:a {:c 9}} b))
      (is (= {:a {:b 1}} both)))))
