(ns clojure.walk-test.walk
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.walk :as w]))

(deftest test-walk
  (testing "walk with identity"
    (is (= [1 2 3] (w/walk identity identity [1 2 3])))
    (is (= '(1 2 3) (w/walk identity identity '(1 2 3))))
    (is (= #{1 2 3} (w/walk identity identity #{1 2 3}))))

  (testing "walk with inner transform"
    (is (= [2 3 4] (w/walk inc identity [1 2 3])))
    (is (= [2 3 4] (w/walk inc vec [1 2 3]))))

  (testing "walk with outer transform"
    (is (= [1 2 3] (w/walk identity vec '(1 2 3))))))

(deftest test-postwalk
  (testing "postwalk with numbers"
    (is (= [2 3 4] (w/postwalk #(if (number? %) (inc %) %) [1 2 3]))))

  (testing "postwalk with nested structures"
    (is (= [2 [3 4] 5]
           (w/postwalk #(if (number? %) (inc %) %) [1 [2 3] 4]))))

  (testing "postwalk preserves types"
    (is (vector? (w/postwalk identity [1 2 3])))
    (is (list? (w/postwalk identity '(1 2 3))))
    (is (set? (w/postwalk identity #{1 2 3})))
    (is (map? (w/postwalk identity {:a 1 :b 2}))))

  (testing "postwalk on maps"
    (is (= {:a 2 :b 3}
           (w/postwalk #(if (number? %) (inc %) %) {:a 1 :b 2}))))

  (testing "postwalk on empty collections"
    (is (= [] (w/postwalk identity [])))
    (is (= {} (w/postwalk identity {})))
    (is (= #{} (w/postwalk identity #{})))
    (is (= '() (w/postwalk identity '())))))

(deftest test-prewalk
  (testing "prewalk with numbers"
    (is (= [2 3 4] (w/prewalk #(if (number? %) (inc %) %) [1 2 3]))))

  (testing "prewalk with nested structures"
    (is (= [2 [3 4] 5]
           (w/prewalk #(if (number? %) (inc %) %) [1 [2 3] 4]))))

  (testing "prewalk transforms before descending"
    ;; prewalk applies f to the outer form first, so we can replace
    ;; entire subtrees before they are walked
    (is (= [:replaced]
           (w/prewalk #(if (= % [1 2 3]) [:replaced] %) [1 2 3])))))

(deftest test-keywordize-keys
  (testing "basic keywordize"
    (is (= {:a 1 :b 2} (w/keywordize-keys {"a" 1 "b" 2}))))

  (testing "nested keywordize"
    (is (= {:a {:b 2}} (w/keywordize-keys {"a" {"b" 2}}))))

  (testing "non-string keys unchanged"
    (is (= {:a 1 42 2} (w/keywordize-keys {"a" 1 42 2}))))

  (testing "already keyword keys unchanged"
    (is (= {:a 1} (w/keywordize-keys {:a 1})))))

(deftest test-stringify-keys
  (testing "basic stringify"
    (is (= {"a" 1 "b" 2} (w/stringify-keys {:a 1 :b 2}))))

  (testing "nested stringify"
    (is (= {"a" {"b" 2}} (w/stringify-keys {:a {:b 2}}))))

  (testing "non-keyword keys unchanged"
    (is (= {"a" 1 42 2} (w/stringify-keys {:a 1 42 2})))))

(deftest test-postwalk-replace
  (testing "basic replacement"
    (is (= [:x :y :c] (w/postwalk-replace {:a :x :b :y} [:a :b :c]))))

  (testing "nested replacement"
    (is (= [:x [:y :c]] (w/postwalk-replace {:a :x :b :y} [:a [:b :c]]))))

  (testing "no matches"
    (is (= [1 2 3] (w/postwalk-replace {:a :x} [1 2 3]))))

  (testing "empty smap"
    (is (= [1 2 3] (w/postwalk-replace {} [1 2 3])))))

(deftest test-prewalk-replace
  (testing "basic replacement"
    (is (= [:x :y :c] (w/prewalk-replace {:a :x :b :y} [:a :b :c]))))

  (testing "nested replacement"
    (is (= [:x [:y :c]] (w/prewalk-replace {:a :x :b :y} [:a [:b :c]]))))

  (testing "replaces before descending"
    ;; prewalk-replace replaces the whole form first, then walks children
    (is (= :replaced (w/prewalk-replace {[:a :b] :replaced} [:a :b])))))
