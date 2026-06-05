# Ported from clojure/test_clojure/control.clj
(use ../../src/jolt/api)
(defn ct-eval [ctx s] (normalize-pvecs (eval-string ctx s)))

(print "Ported Control Tests")

# --- test-do ---
(print "test-do...")
(let [ctx (init)]
  (assert (= nil (ct-eval ctx "(do)")) "do empty -> nil")
  (assert (= 1 (ct-eval ctx "(do 1)")) "do returns last")
  (assert (= 2 (ct-eval ctx "(do 1 2)")) "do returns last")
  (assert (= 5 (ct-eval ctx "(do 1 2 3 4 5)")) "do returns last"))
(print "  ok")

# --- test-loop ---
(print "test-loop...")
(let [ctx (init)]
  (assert (= 1 (ct-eval ctx "(loop [] 1)")) "loop body")
  (assert (= 3 (ct-eval ctx "(loop [a 1] (if (< a 3) (recur (inc a)) a))")) "loop recur")
  (assert (= true (ct-eval ctx
    "(= [6 4 2] (loop [a () b [1 2 3]]
       (if (seq b)
         (recur (conj a (* 2 (first b))) (next b))
         a)))")) "loop accum list")
  (assert (= true (ct-eval ctx
    "(= [2 4 6] (loop [a [] b [1 2 3]]
       (if (seq b)
         (recur (conj a (* 2 (first b))) (next b))
         a)))")) "loop accum vector"))
(print "  ok")

# --- test-when ---
(print "test-when...")
(let [ctx (init)]
  (assert (= 1 (ct-eval ctx "(when true 1)")) "when true")
  (assert (= nil (ct-eval ctx "(when true)")) "when true no body")
  (assert (= nil (ct-eval ctx "(when false)")) "when false")
  (assert (= nil (ct-eval ctx "(when false 1)")) "when false with body"))
(print "  ok")

# --- test-when-not ---
(print "test-when-not...")
(let [ctx (init)]
  (assert (= 1 (ct-eval ctx "(when-not false 1)")) "when-not false")
  (assert (= nil (ct-eval ctx "(when-not true)")) "when-not true no body")
  (assert (= nil (ct-eval ctx "(when-not false)")) "when-not false no body")
  (assert (= nil (ct-eval ctx "(when-not true 1)")) "when-not true with body"))
(print "  ok")

# --- test-if-not ---
(print "test-if-not...")
(let [ctx (init)]
  (assert (= 1 (ct-eval ctx "(if-not false 1)")) "if-not false")
  (assert (= 1 (ct-eval ctx "(if-not false 1 2)")) "if-not false with else")
  (assert (= nil (ct-eval ctx "(if-not true 1)")) "if-not true")
  (assert (= 2 (ct-eval ctx "(if-not true 1 2)")) "if-not true with else"))
(print "  ok")

# --- test-when-let ---
(print "test-when-let...")
(let [ctx (init)]
  (assert (= 1 (ct-eval ctx "(when-let [a 1] a)")) "when-let simple")
  (assert (= 2 (ct-eval ctx "(when-let [[a b] '(1 2)] b)")) "when-let destructure")
  (assert (= nil (ct-eval ctx "(when-let [a false] 1)")) "when-let false"))
(print "  ok")

# --- test-if-let ---
(print "test-if-let...")
(let [ctx (init)]
  (assert (= 1 (ct-eval ctx "(if-let [a 1] a)")) "if-let simple")
  (assert (= 2 (ct-eval ctx "(if-let [[a b] '(1 2)] b)")) "if-let destructure")
  (assert (= nil (ct-eval ctx "(if-let [a false] 1)")) "if-let false")
  (assert (= 1 (ct-eval ctx "(if-let [a false] a 1)")) "if-let false with else"))
(print "  ok")

# --- test-if-some ---
(print "test-if-some...")
(let [ctx (init)]
  (assert (= 1 (ct-eval ctx "(if-some [a 1] a)")) "if-some simple")
  (assert (= false (ct-eval ctx "(if-some [a false] a)")) "if-some false is some")
  (assert (= nil (ct-eval ctx "(if-some [a nil] 1)")) "if-some nil")
  (assert (= 3 (ct-eval ctx "(if-some [[a b] [1 2]] (+ a b))")) "if-some destructure"))
(print "  ok")

# --- test-when-some ---
(print "test-when-some...")
(let [ctx (init)]
  (assert (= 1 (ct-eval ctx "(when-some [a 1] a)")) "when-some simple")
  (assert (= 2 (ct-eval ctx "(when-some [[a b] [1 2]] b)")) "when-some destructure")
  (assert (= false (ct-eval ctx "(when-some [a false] a)")) "when-some false is some")
  (assert (= nil (ct-eval ctx "(when-some [a nil] 1)")) "when-some nil"))
(print "  ok")

# --- test-cond ---
(print "test-cond...")
(let [ctx (init)]
  (assert (= nil (ct-eval ctx "(cond)")) "cond empty -> nil")
  (assert (= nil (ct-eval ctx "(cond nil true)")) "cond nil -> nil")
  (assert (= nil (ct-eval ctx "(cond false true)")) "cond false -> nil")
  (assert (= 1 (ct-eval ctx "(cond true 1)")) "cond true -> 1")
  (assert (= 3 (ct-eval ctx "(cond nil 1 false 2 true 3 true 4)")) "cond third branch")
  (assert (= :b (ct-eval ctx "(cond false :a true :b)")) "cond skips false")
  (assert (= :a (ct-eval ctx "(cond true :a true :b)")) "cond takes first true"))
(print "  ok")

# --- test-condp ---
(print "test-condp...")
(let [ctx (init)]
  (assert (= :pass (ct-eval ctx "(condp = 1 1 :pass 2 :fail)")) "condp match first")
  (assert (= :pass (ct-eval ctx "(condp = 1 2 :fail 1 :pass)")) "condp match second")
  (assert (= :pass (ct-eval ctx "(condp = 1 2 :fail :pass)")) "condp default"))
(print "  ok")

# --- test-dotimes ---
(print "test-dotimes...")
(let [ctx (init)]
  (assert (= nil (ct-eval ctx "(dotimes [n 1] n)")) "dotimes returns nil")
  (assert (= 3 (ct-eval ctx
    "(let [a (atom 0)]
       (dotimes [n 3] (swap! a inc))
       @a)")) "dotimes 3 iterations")
  (assert (= [0 1 2] (ct-eval ctx
    "(let [a (atom [])]
       (dotimes [n 3] (swap! a conj n))
       @a)")) "dotimes with index"))
(print "  ok")

# --- test-while ---
(print "test-while...")
(let [ctx (init)]
  (assert (= nil (ct-eval ctx "(while nil 1)")) "while nil returns nil")
  (assert (= true (ct-eval ctx
    "(= [0 nil]
        (let [a (atom 3)
              w (while (pos? @a) (swap! a dec))]
          [@a w]))")) "while dec to 0"))
(print "  ok")

# --- test-case ---
(print "test-case...")
(let [ctx (init)]
  (assert (= :number (ct-eval ctx "(case 1 1 :number :default)")) "case match 1")
  (assert (= :string (ct-eval ctx "(case \"foo\" \"foo\" :string :default)")) "case match string")
  (assert (= :kw (ct-eval ctx "(case :zap :zap :kw :default)")) "case match keyword")
  (assert (= :symbol (ct-eval ctx "(case 'pow pow :symbol :default)")) "case match symbol")
  (assert (= :default (ct-eval ctx "(case 99 1 :number :default)")) "case default")
  (assert (= :matched (ct-eval ctx "(case 2 (2 3 4) :matched :default)")) "case one-of-many"))
(print "  ok")

(print "\nAll Ported Control tests passed!")
