(ns clojure.zip-test.zip
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.zip :as zip]))

(deftest test-vector-zip-navigation
  (let [data [[1 2] [3 [4 5]]]
        z    (zip/vector-zip data)]
    (testing "root node"
      (is (= (zip/node z) [[1 2] [3 [4 5]]]))
      (is (zip/branch? z)))
    (testing "down"
      (is (= (zip/node (zip/down z)) [1 2])))
    (testing "right"
      (is (= (zip/node (zip/right (zip/down z))) [3 [4 5]])))
    (testing "down into nested"
      (is (= (zip/node (zip/down (zip/right (zip/down z)))) 3)))
    (testing "up returns parent"
      (is (= (zip/node (zip/up (zip/down z))) [[1 2] [3 [4 5]]])))
    (testing "rights"
      (is (= (zip/rights (zip/down z)) '([3 [4 5]]))))
    (testing "lefts"
      (is (= (zip/lefts (zip/right (zip/down z))) [[1 2]])))))

(deftest test-vector-zip-rightmost-leftmost
  (let [z (zip/vector-zip [1 2 3])]
    (testing "rightmost"
      (is (= (zip/node (zip/rightmost (zip/down z))) 3)))
    (testing "leftmost"
      (is (= (zip/node (zip/leftmost (zip/rightmost (zip/down z)))) 1)))))

(deftest test-seq-zip-navigation
  (let [z (zip/seq-zip '(1 (2 3) 4))]
    (testing "root"
      (is (= (zip/node z) '(1 (2 3) 4))))
    (testing "down"
      (is (= (zip/node (zip/down z)) 1)))
    (testing "right"
      (is (= (zip/node (zip/right (zip/down z))) '(2 3))))
    (testing "down into nested list"
      (is (= (zip/node (zip/down (zip/right (zip/down z)))) 2)))))

(deftest test-path
  (let [z (zip/vector-zip [[1 2] [3 4]])]
    (testing "path at root is nil"
      (is (nil? (zip/path z))))
    (testing "path one level down"
      (is (= (zip/path (zip/down z)) [[[1 2] [3 4]]])))
    (testing "path two levels down"
      (is (= (zip/path (zip/down (zip/down z)))
             [[[1 2] [3 4]] [1 2]])))))

(deftest test-edit
  (let [z (zip/vector-zip [1 [2 3] [4 5]])]
    (testing "edit a leaf"
      (let [loc (-> z zip/down zip/right zip/down)
            edited (zip/edit loc inc)]
        (is (= (zip/root edited) [1 [3 3] [4 5]]))))
    (testing "edit a branch"
      (let [loc (-> z zip/down zip/right)
            edited (zip/edit loc (fn [x] (vec (map inc x))))]
        (is (= (zip/root edited) [1 [3 4] [4 5]]))))))

(deftest test-replace
  (let [z (zip/vector-zip '[a b c])]
    (is (= (zip/root (zip/replace (zip/down z) 'x))
           '[x b c]))))

(deftest test-insert-left-right
  (let [z (zip/vector-zip [1 2 3])
        loc (-> z zip/down zip/right)]
    (testing "insert-left"
      (is (= (zip/root (zip/insert-left loc 'x)) [1 'x 2 3])))
    (testing "insert-right"
      (is (= (zip/root (zip/insert-right loc 'y)) [1 2 'y 3])))))

(deftest test-insert-child-append-child
  (let [z (zip/vector-zip [1 2 3])]
    (testing "insert-child"
      (is (= (zip/root (zip/insert-child z 0)) [0 1 2 3])))
    (testing "append-child"
      (is (= (zip/root (zip/append-child z 4)) [1 2 3 4])))))

(deftest test-remove
  (let [z (zip/vector-zip [1 2 3])
        loc (-> z zip/down zip/right)]
    (is (= (zip/root (zip/remove loc)) [1 3]))))

(deftest test-next-traversal
  (let [z (zip/vector-zip [1 [2 3]])]
    (testing "next enumerates depth-first"
      (is (= (loop [loc z, acc []]
               (if (zip/end? loc)
                 acc
                 (recur (zip/next loc) (conj acc (zip/node loc)))))
             [[1 [2 3]] 1 [2 3] 2 3])))))

(deftest test-end?
  (let [z (zip/vector-zip [1 2])]
    (testing "not end at start"
      (is (not (zip/end? z))))
    (testing "end after full traversal"
      (is (zip/end? (-> z zip/next zip/next zip/next))))))

(deftest test-prev
  (let [z (zip/vector-zip [1 [2 3]])]
    (testing "prev from second child"
      (let [loc (-> z zip/next zip/next)]
        (is (= (zip/node loc) [2 3]))
        (is (= (zip/node (zip/prev loc)) 1))))
    (testing "prev from leaf inside nested"
      (let [loc (-> z zip/next zip/next zip/next)]
        (is (= (zip/node loc) 2))
        (is (= (zip/node (zip/prev loc)) [2 3]))))))

(deftest test-root-after-edits
  (testing "root unwinds all the way after deep edits"
    (let [z (zip/vector-zip [[1 2] [3 [4 5]]])
          loc (-> z zip/down zip/right zip/down zip/right zip/down)
          edited (zip/edit loc inc)]
      (is (= (zip/root edited) [[1 2] [3 [5 5]]])))))

(run-tests)
