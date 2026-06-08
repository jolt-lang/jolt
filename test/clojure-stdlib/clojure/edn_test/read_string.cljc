(ns clojure.edn-test.read-string
  (:require [clojure.edn :as edn]
            [clojure.test :refer [are deftest is testing]]))

(deftest test-read-string-scalars
  (testing "nil, booleans"
    (is (nil? (edn/read-string "nil")))
    (is (true? (edn/read-string "true")))
    (is (false? (edn/read-string "false"))))

  (testing "integers"
    (is (= 0 (edn/read-string "0")))
    (is (= 42 (edn/read-string "42")))
    (is (= -1 (edn/read-string "-1")))
    (is (= 1000000000000 (edn/read-string "1000000000000"))))

  (testing "floats"
    (is (= 3.14 (edn/read-string "3.14")))
    (is (= -0.5 (edn/read-string "-0.5")))
    (is (= 1.0 (edn/read-string "1.0"))))

  (testing "bigints"
    (is (= 42N (edn/read-string "42N"))))

  (testing "bigdecimals"
    (is (= 3.14M (edn/read-string "3.14M"))))

  (testing "strings"
    (is (= "" (edn/read-string "\"\"")))
    (is (= "hello" (edn/read-string "\"hello\"")))
    (is (= "line1\nline2" (edn/read-string "\"line1\\nline2\"")))
    (is (= "tab\there" (edn/read-string "\"tab\\there\""))))

  (testing "characters"
    (is (= \a (edn/read-string "\\a")))
    (is (= \newline (edn/read-string "\\newline")))
    (is (= \space (edn/read-string "\\space")))
    (is (= \tab (edn/read-string "\\tab"))))

  (testing "keywords"
    (is (= :foo (edn/read-string ":foo")))
    (is (= :bar/baz (edn/read-string ":bar/baz"))))

  (testing "symbols"
    (is (= 'foo (edn/read-string "foo")))
    (is (= 'bar/baz (edn/read-string "bar/baz")))))

(deftest test-read-string-collections
  (testing "vectors"
    (is (= [] (edn/read-string "[]")))
    (is (= [1 2 3] (edn/read-string "[1 2 3]")))
    (is (= [1 [2 3] 4] (edn/read-string "[1 [2 3] 4]"))))

  (testing "lists"
    (is (= '() (edn/read-string "()")))
    (is (= '(1 2 3) (edn/read-string "(1 2 3)")))
    (is (= '(+ 1 2) (edn/read-string "(+ 1 2)"))))

  (testing "maps"
    (is (= {} (edn/read-string "{}")))
    (is (= {:a 1} (edn/read-string "{:a 1}")))
    (is (= {:a 1 :b 2} (edn/read-string "{:a 1 :b 2}")))
    (is (= {:nested {:deep true}} (edn/read-string "{:nested {:deep true}}"))))

  (testing "sets"
    (is (= #{} (edn/read-string "#{}")))
    (is (= #{1 2 3} (edn/read-string "#{1 2 3}"))))

  (testing "mixed nested"
    (is (= {:users [{:name "Alice" :age 30}
                     {:name "Bob" :age 25}]}
           (edn/read-string "{:users [{:name \"Alice\" :age 30} {:name \"Bob\" :age 25}]}")))))

(deftest test-read-string-tagged-literals
  (testing "#uuid"
    (let [u (edn/read-string "#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"")]
      (is (uuid? u))
      (is (= u (edn/read-string "#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\""))))))

(deftest test-read-string-eof
  (testing "empty string with :eof option"
    (is (= :eof (edn/read-string {:eof :eof} "")))
    (is (= nil (edn/read-string {:eof nil} "")))
    (is (= 42 (edn/read-string {:eof 42} ""))))

  (testing "whitespace-only with :eof option"
    (is (= :done (edn/read-string {:eof :done} "   "))))

  (testing "nil input returns nil"
    (is (nil? (edn/read-string nil)))))

(deftest test-read-string-comments
  (testing "comments are skipped"
    (is (= 42 (edn/read-string "; this is a comment\n42"))))

  (testing "discard reader macro"
    (is (= 2 (edn/read-string "#_ 1 2")))))

(deftest test-read-string-only-first-form
  (testing "reads only the first form"
    (is (= 1 (edn/read-string "1 2 3")))
    (is (= :a (edn/read-string ":a :b :c")))))

(deftest test-read-string-ratios
  (testing "ratios"
    (is (= 1/2 (edn/read-string "1/2")))
    (is (= 3/4 (edn/read-string "3/4")))))
