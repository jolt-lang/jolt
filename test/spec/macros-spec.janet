# Specification: macros, quoting and syntax-quote.
(use ../support/harness)

(defspec "macros / quoting"
  ["quote symbol"       "(quote a)"         "(quote a)"]
  ["quote list"         "[1 2 3]"     "(quote (1 2 3))"]
  ["quote nested"       "[1 [2 3]]"   "(quote (1 (2 3)))"]
  ["quote sugar"        "'a"          "'a"]
  ["syntax-quote literal" "[1 2]"     "`[1 2]"]
  ["unquote"            "[1 2 3]"     "(let [x 2] `[1 ~x 3])"]
  ["unquote-splicing"   "[1 2 3 4]"   "(let [xs [2 3]] `[1 ~@xs 4])"]
  ["unquote in list"    "[3]"         "(let [x 3] `(~x))"]
  ["syntax-quote symbol qualifies" "true" "(symbol? `foo)"])

(defspec "macros / defmacro"
  ["simple macro"       "3"
   "(do (defmacro m [a b] `(+ ~a ~b)) (m 1 2))"]
  ["macro unless"       "1"
   "(do (defmacro unless [c body] `(if ~c nil ~body)) (unless false 1))"]
  ["macro with body splice" "6"
   "(do (defmacro msum [& xs] `(+ ~@xs)) (msum 1 2 3))"]
  ["macroexpand-1"      "true"
   "(do (defmacro m [x] `(inc ~x)) (list? (macroexpand-1 '(m 5))))"]
  ["gensym unique"      "false"
   "(= (gensym) (gensym))"]
  ["gensym# in template" "true"
   "(do (defmacro m [] `(let [x# 1] x#)) (= 1 (m)))"])

# Core macros ported from Janet to the Clojure overlay (jolt-1j0 phase 3,
# jolt-core/clojure/core/30-macros.clj).
(defspec "macros / core-overlay"
  ["if-not true branch"  ":then"  "(if-not false :then :else)"]
  ["if-not else branch"  ":else"  "(if-not true :then :else)"]
  ["if-not no else"      "nil"    "(if-not true :then)"]
  ["if-not no else hit"  ":then"  "(if-not false :then)"]
  ["comment -> nil"      "nil"    "(comment a b c)"]
  ["comment in do"       "42"     "(do (comment ignored) 42)"]
  ["if-let then"         "6"      "(if-let [x 5] (inc x) :none)"]
  ["if-let else"         ":none"  "(if-let [x nil] (inc x) :none)"]
  ["if-let else scope"   "9"      "(let [x 9] (if-let [x nil] :t x))"]
  ["if-some zero"        "1"      "(if-some [x 0] (inc x) :none)"]
  ["if-some nil"         ":none"  "(if-some [x nil] x :none)"]
  ["when-some multi"     "14"     "(when-some [x 7] (inc x) (* x 2))"]
  ["when-some nil"       "nil"    "(when-some [x nil] x)"]
  ["while loop"          "3"      "(let [a (atom 0)] (while (< @a 3) (swap! a inc)) @a)"]
  ["dotimes sum"         "10"     "(let [a (atom 0)] (dotimes [i 5] (swap! a + i)) @a)"]
  ["as-> threads"        "12"     "(as-> 5 x (+ x 1) (* x 2))"]
  ["as-> no forms"       "5"      "(as-> 5 x)"]
  ["some-> through"      "6"      "(some-> {:a {:b 5}} :a :b inc)"]
  ["some-> short-circuit" "nil"   "(some-> {:a nil} :a :b)"]
  ["some->> through"     "9"      "(some->> [1 2 3] (map inc) (reduce +))"]
  ["some->> nil"         "nil"    "(some->> nil (map inc))"]
  ["doto returns obj"    "[1 2]"  "(deref (doto (atom []) (swap! conj 1) (swap! conj 2)))"]
  ["when-first"          "20"     "(when-first [x [10 20 30]] (* x 2))"]
  ["when-first empty"    "nil"    "(when-first [x []] :body)"]
  ["when-first nil coll" "nil"    "(when-first [x nil] :body)"]
  ["when-first range"    "0"      "(when-first [x (range 5)] x)"]
  ["cond->> threads"     "12"     "(cond->> 5 true (+ 1) false (* 100) true (* 2))"]
  ["cond->> skip"        "10"     "(cond->> 10 false (+ 1))"]
  ["assert pass"         ":ok"    "(do (assert (= 1 1)) :ok)"]
  ["assert throws"       ":threw" "(try (assert (= 1 2)) (catch :default e :threw))"]
  ["assert message"      "\"nope\"" "(try (assert false \"nope\") (catch :default e (ex-message e)))"]
  ["delay value"         "42"     "(deref (delay 42))"]
  ["delay forces once"   "1"      "(let [c (atom 0) d (delay (swap! c inc))] @d @d @c)"]
  ["future deref"        "9"      "(deref (future (* 3 3)))"]
  ["letfn simple"        "25"     "(letfn [(sq [x] (* x x))] (sq 5))"]
  ["letfn mutual"        "true"   "(letfn [(ev? [n] (if (zero? n) true (od? (dec n)))) (od? [n] (if (zero? n) false (ev? (dec n))))] (ev? 8))"]
  ["condp match"         ":two"   "(condp = 2 1 :one 2 :two 3 :three)"]
  ["condp default"       ":else"  "(condp = 9 1 :one 2 :two :else)"]
  ["condp :>> form"      "\"got 2\"" "(condp some [1 2 3] #{0 9} :>> (fn [x] (str \"got \" x)) #{2 6} :>> (fn [x] (str \"got \" x)))"]
  ["condp no match"      ":threw" "(try (condp = 9 1 :one) (catch :default e :threw))"]
  ["binding rebinds"     "99"     "(do (def ^:dynamic *bx* 10) (binding [*bx* 99] *bx*))"]
  ["binding restores"    "10"     "(do (def ^:dynamic *by* 10) (binding [*by* 99] *by*) *by*)"]
  ["binding seen by fn"  "7"      "(do (def ^:dynamic *bz* 0) (defn rdz [] *bz*) (binding [*bz* 7] (rdz)))"])

# time returns the body value (the timing line goes to *out*); with-redefs
# temporarily rebinds var roots and restores on exit (even on throw);
# macroexpand expands repeatedly until the head is no longer a macro.
(defspec "macros / time, with-redefs, macroexpand"
  ["time returns value"  "3"      "(time (+ 1 2))"]
  ["with-redefs rebinds" "42"     "(do (defn wr-f [] 1) (with-redefs [wr-f (fn [] 42)] (wr-f)))"]
  ["with-redefs restores" "1"     "(do (defn wr-g [] 1) (with-redefs [wr-g (fn [] 42)]) (wr-g))"]
  ["with-redefs restores on throw" "1"
   "(do (defn wr-h [] 1) (try (with-redefs [wr-h (fn [] 42)] (throw (ex-info \"x\" {}))) (catch :default e nil)) (wr-h))"]
  ["with-redefs-fn"      "42"     "(do (defn wr-i [] 1) (with-redefs-fn {(var wr-i) (fn [] 42)} (fn [] (wr-i))))"]
  ["macroexpand full"    "true"   "(let [e (macroexpand (quote (when-not false 1)))] (= (quote if) (first e)))"]
  ["macroexpand non-macro" "[1 2]" "(macroexpand (quote [1 2]))"])

# defmacro accepts the arity-clause form (defmacro name ([params] body...)), a
# leading docstring, and ^{:map} metadata on the name (jolt-whp, jolt-8w2).
(defspec "macros / defmacro arity-clause & name metadata"
  ["arity-clause form"    "10"  "(do (defmacro tw ([x] (list (quote *) x 2))) (tw 5))"]
  ["docstring + arity"    "15"  "(do (defmacro th \"triple\" ([x] (list (quote *) x 3))) (th 5))"]
  ["^{:map} name meta"    "7"   "(do (defmacro ^{:private true} pm [] 7) (pm))"]
  ["multi-form body"      "6"   "(do (defmacro mb ([a b] (list (quote +) a b))) (mb 2 4))"])

# Multi-arity defmacro (dispatch on arg count) and the docstring + attr-map +
# params form — both needed to run real Clojure macros, e.g.
# clojure.tools.logging/log (4 arities) and its level macros (jolt-q8l, jolt-qnr).
(defspec "macros / defmacro multi-arity & attr-map"
  ["multi-arity 1"   "6"  "(do (defmacro ma ([a] (list (quote +) a 1)) ([a b] (list (quote +) a b))) (ma 5))"]
  ["multi-arity 2"   "5"  "(do (defmacro ma ([a] (list (quote +) a 1)) ([a b] (list (quote +) a b))) (ma 2 3))"]
  ["arity delegates" "[:d nil 9]"
   "(do (defmacro lg ([m] `(lg :d nil ~m)) ([l t m] (list (quote vector) l t m))) (lg 9))"]
  ["doc + attr-map + params" "10"
   "(do (defmacro am \"doc\" {:arglists (quote ([x]))} [x] (list (quote inc) x)) (am 9))"]
  ["doc + attr-map + variadic" "6"
   "(do (defmacro vg \"d\" {:arglists (quote ([& a]))} [& xs] `(+ ~@xs)) (vg 1 2 3))"])
