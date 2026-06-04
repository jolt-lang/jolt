# Ported from clojure/test_clojure/macros.clj
(use ../src/jolt/api)
(defn ct-eval [ctx s] (normalize-pvecs (eval-string ctx s)))

(print "Ported Macros Tests")

# --- -> and ->> threading ---
(print "test -> and ->>...")
(let [ctx (init)]
  (ct-eval ctx "(defmacro c [arg] (if (= 'b (first arg)) :foo :bar))")
  (ct-eval ctx "(def a 2)")
  (ct-eval ctx "(def b identity)")
  (assert (= :foo (ct-eval ctx "(-> a b c)")) "-> threading")
  (assert (= :foo (ct-eval ctx "(->> a b c)")) "->> threading"))
(print "  ok")

# --- some-> ---
(print "test some->...")
(let [ctx (init)]
  (assert (= nil (ct-eval ctx "(some-> nil)")) "some-> nil")
  (assert (= 0 (ct-eval ctx "(some-> 0)")) "some-> 0")
  (assert (= -1 (ct-eval ctx "(some-> 1 (- 2))")) "some-> with form")
  (ct-eval ctx "(defn const-nil [_] nil)")
  (assert (= nil (ct-eval ctx "(some-> 1 const-nil (- 2))")) "some-> stop at nil"))
(print "  ok")

# --- some->> ---
(print "test some->>...")
(let [ctx (init)]
  (assert (= nil (ct-eval ctx "(some->> nil)")) "some->> nil")
  (assert (= 0 (ct-eval ctx "(some->> 0)")) "some->> 0")
  (assert (= 1 (ct-eval ctx "(some->> 1 (- 2))")) "some->> with form")
  (ct-eval ctx "(defn const-nil2 [_] nil)")
  (assert (= nil (ct-eval ctx "(some->> 1 const-nil2 (- 2))")) "some->> stop at nil"))
(print "  ok")

# --- cond-> ---
(print "test cond->...")
(let [ctx (init)]
  (assert (= 0 (ct-eval ctx "(cond-> 0)")) "cond-> single")
  (assert (= -1 (ct-eval ctx "(cond-> 0 true inc true (- 2))")) "cond-> with tests")
  (assert (= 0 (ct-eval ctx "(cond-> 0 false inc)")) "cond-> false test")
  (assert (= -1 (ct-eval ctx "(cond-> 1 true (- 2) false inc)")) "cond-> mix"))
(print "  ok")

# --- cond->> ---
(print "test cond->>...")
(let [ctx (init)]
  (assert (= 0 (ct-eval ctx "(cond->> 0)")) "cond->> single")
  (assert (= 1 (ct-eval ctx "(cond->> 0 true inc true (- 2))")) "cond->> with tests")
  (assert (= 0 (ct-eval ctx "(cond->> 0 false inc)")) "cond->> false test")
  (assert (= 1 (ct-eval ctx "(cond->> 1 true (- 2) false inc)")) "cond->> mix"))
(print "  ok")

# --- as-> ---
(print "test as->...")
(let [ctx (init)]
  (assert (= 0 (ct-eval ctx "(as-> 0 x)")) "as-> single")
  (assert (= 1 (ct-eval ctx "(as-> 0 x (inc x))")) "as-> one form")
  (assert (= 2 (ct-eval ctx "(as-> [0 1] x (map inc x) (reverse x) (first x))")) "as-> chain"))
(print "  ok")

(print "\nAll Ported Macros tests passed!")
