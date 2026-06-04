# Ported from clojure/test_clojure/logic.clj
(use ../src/jolt/api)
(defn ct-eval [ctx s] (eval-string ctx s))

(print "Ported Logic Tests (from clojure/test-clojure/logic.clj)")

# --- test-if ---
(print "test-if: true/false/nil...")
(let [ctx (init)]
  (assert (= :t (ct-eval ctx "(if true :t)")) "if true")
  (assert (= :t (ct-eval ctx "(if true :t :f)")) "if true with else")  
  (assert (= nil (ct-eval ctx "(if false :t)")) "if false no else")
  (assert (= :f (ct-eval ctx "(if false :t :f)")) "if false with else")
  (assert (= nil (ct-eval ctx "(if nil :t)")) "if nil no else")
  (assert (= :f (ct-eval ctx "(if nil :t :f)")) "if nil with else"))
(print "  ok")

(print "test-if: zero/empty is true...")
(let [ctx (init)]
  (assert (= :t (ct-eval ctx "(if 0 :t :f)")) "0 is true")
  (assert (= :t (ct-eval ctx "(if 0.0 :t :f)")) "0.0 is true")
  (assert (= :t (ct-eval ctx "(if \"\" :t :f)")) "empty string is true")
  (assert (= :t (ct-eval ctx "(if () :t :f)")) "empty list is true")
  (assert (= :t (ct-eval ctx "(if [] :t :f)")) "empty vector is true")
  (assert (= :t (ct-eval ctx "(if {} :t :f)")) "empty map is true")
  (assert (= :t (ct-eval ctx "(if #{} :t :f)")) "empty set is true"))
(print "  ok")

(print "test-if: anything except nil/false is true...")
(let [ctx (init)]
  (assert (= :t (ct-eval ctx "(if 42 :t :f)")) "42 is true")
  (assert (= :t (ct-eval ctx "(if 1.2 :t :f)")) "1.2 is true")
  (assert (= :t (ct-eval ctx "(if \"abc\" :t :f)")) "string is true")
  (assert (= :t (ct-eval ctx "(if 'abc :t :f)")) "symbol is true")
  (assert (= :t (ct-eval ctx "(if :kw :t :f)")) "keyword is true")
  (assert (= :t (ct-eval ctx "(if '(1 2) :t :f)")) "list is true")
  (assert (= :t (ct-eval ctx "(if [1 2] :t :f)")) "vector is true")
  (assert (= :t (ct-eval ctx "(if {:a 1 :b 2} :t :f)")) "map is true")
  (assert (= :t (ct-eval ctx "(if #{1 2} :t :f)")) "set is true"))
(print "  ok")

# --- test-nil-punning ---
(print "test-nil-punning...")
(let [ctx (init)]
  (assert (= :yes (ct-eval ctx "(if (first []) :no :yes)")) "first [] nil")
  (assert (= :yes (ct-eval ctx "(if (next [1]) :no :yes)")) "next [1] nil")
  (assert (= :no (ct-eval ctx "(if (rest [1]) :no :yes)")) "rest [1] non-nil")
  (assert (= :yes (ct-eval ctx "(if (seq nil) :no :yes)")) "seq nil")
  (assert (= :yes (ct-eval ctx "(if (seq []) :no :yes)")) "seq [] nil")
  (assert (= :no (ct-eval ctx "(if (lazy-seq nil) :no :yes)")) "lazy-seq nil non-nil")
  (assert (= :no (ct-eval ctx "(if (lazy-seq []) :no :yes)")) "lazy-seq [] non-nil")
  (assert (= :no (ct-eval ctx "(if (filter (fn [x] (> x 10)) [1 2 3]) :no :yes)")) "filter non-match non-nil")
  (assert (= :no (ct-eval ctx "(if (map identity []) :no :yes)")) "map empty non-nil")
  (assert (= :no (ct-eval ctx "(if (apply concat []) :no :yes)")) "apply concat [] non-nil")
  (assert (= :no (ct-eval ctx "(if (concat) :no :yes)")) "concat empty non-nil")
  (assert (= :no (ct-eval ctx "(if (concat []) :no :yes)")) "concat [] non-nil")
  (assert (= :no (ct-eval ctx "(if (reverse nil) :no :yes)")) "reverse nil non-nil")
  (assert (= :no (ct-eval ctx "(if (reverse []) :no :yes)")) "reverse [] non-nil")
  (assert (= :no (ct-eval ctx "(if (sort nil) :no :yes)")) "sort nil non-nil")
  (assert (= :no (ct-eval ctx "(if (sort []) :no :yes)")) "sort [] non-nil"))
(print "  ok")

# --- test-and ---
(print "test-and...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(and)")) "and empty")
  (assert (= true (ct-eval ctx "(and true)")) "and true")
  (assert (= nil (ct-eval ctx "(and nil)")) "and nil")
  (assert (= false (ct-eval ctx "(and false)")) "and false")
  (assert (= nil (ct-eval ctx "(and true nil)")) "and true nil")
  (assert (= false (ct-eval ctx "(and true false)")) "and true false")
  (assert (= "abc" (ct-eval ctx "(and 1 true :kw 'abc \"abc\")")) "and chain last")
  (assert (= nil (ct-eval ctx "(and 1 true :kw nil 'abc \"abc\")")) "and chain nil")
  (assert (= false (ct-eval ctx "(and 1 true :kw 'abc \"abc\" false)")) "and chain false"))
(print "  ok")

# --- test-or ---
(print "test-or...")
(let [ctx (init)]
  (assert (= nil (ct-eval ctx "(or)")) "or empty")
  (assert (= true (ct-eval ctx "(or true)")) "or true")
  (assert (= nil (ct-eval ctx "(or nil)")) "or nil")
  (assert (= false (ct-eval ctx "(or false)")) "or false")
  (assert (= true (ct-eval ctx "(or nil false true)")) "or nil false true")
  (assert (= 1 (ct-eval ctx "(or nil false 1 2)")) "or picks first truthy")
  (assert (= "abc" (ct-eval ctx "(or nil false \"abc\" :kw)")) "or picks string")
  (assert (= nil (ct-eval ctx "(or false nil)")) "or false nil -> nil")
  (assert (= false (ct-eval ctx "(or nil false)")) "or nil false -> false")
  (assert (= false (ct-eval ctx "(or nil nil nil false)")) "or chain to false")
  (assert (= true (ct-eval ctx "(or nil true false)")) "or nil true false"))
(print "  ok")

# --- test-not ---
(print "test-not...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(not nil)")) "not nil")
  (assert (= true (ct-eval ctx "(not false)")) "not false")
  (assert (= false (ct-eval ctx "(not true)")) "not true")
  (assert (= false (ct-eval ctx "(not 0)")) "not 0")
  (assert (= false (ct-eval ctx "(not 0.0)")) "not 0.0")
  (assert (= false (ct-eval ctx "(not 42)")) "not 42")
  (assert (= false (ct-eval ctx "(not 1.2)")) "not 1.2")
  (assert (= false (ct-eval ctx "(not \"\")")) "not empty string")
  (assert (= false (ct-eval ctx "(not \"abc\")")) "not string")
  (assert (= false (ct-eval ctx "(not 'abc)")) "not symbol")
  (assert (= false (ct-eval ctx "(not :kw)")) "not keyword")
  (assert (= false (ct-eval ctx "(not ())")) "not empty list")
  (assert (= false (ct-eval ctx "(not '(1 2))")) "not list")
  (assert (= false (ct-eval ctx "(not []))")) "not empty vector")
  (assert (= false (ct-eval ctx "(not [1 2])")) "not vector")
  (assert (= false (ct-eval ctx "(not {})")) "not empty map")
  (assert (= false (ct-eval ctx "(not {:a 1 :b 2})")) "not map")
  (assert (= false (ct-eval ctx "(not #{})")) "not empty set")
  (assert (= false (ct-eval ctx "(not #{1 2})")) "not set"))
(print "  ok")

# --- test-some? ---
(print "test-some?...")
(let [ctx (init)]
  (assert (= false (ct-eval ctx "(some? nil)")) "some? nil")
  (assert (= true (ct-eval ctx "(some? false)")) "some? false")
  (assert (= true (ct-eval ctx "(some? 0)")) "some? 0")
  (assert (= true (ct-eval ctx "(some? \"abc\")")) "some? string")
  (assert (= true (ct-eval ctx "(some? [])")) "some? empty vec"))
(print "  ok")

(print "\nAll Ported Logic tests passed!")
