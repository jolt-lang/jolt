# Regression tests mirroring clojure-features.clj, plus expanded coverage of
# related features. Each case asserts (= expected actual) evaluated inside Jolt
# (so comparisons use Jolt's own Clojure-semantics =). Run via `jpm test`.
(use ../src/jolt/api)

(var pass 0)
(def fails @[])

(defn check [label expected actual]
  # evaluate (= expected actual) in a fresh ctx; expects boolean true
  (def ctx (init))
  (def res (protect (eval-string ctx (string "(= " expected " " actual ")"))))
  (cond
    (not= (res 0) true)
      (array/push fails [label "ERROR" (string (res 1))])
    (= (res 1) true)
      (++ pass)
    (let [got (protect (eval-string (init) actual))]
      (array/push fails [label "NEQ"
                         (string "want=" expected " got="
                                 (if (= (got 0) true) (string/format "%q" (got 1)) (string "ERR:" (got 1))))]))))

(def cases
  [
   ### 1. Destructuring
   ["destr seq"          "[10 20 30]"  "(let [[a b c] [10 20 30]] [a b c])"]
   ["destr map :or"      "[\"Alice\" 30 \"Unknown\"]"
    "(let [{:keys [name age city] :or {city \"Unknown\"}} {:name \"Alice\" :age 30}] [name age city])"]
   ["destr nested map"   "[1.0 2.5]"   "(let [{[x y] :coords} {:coords [1.0 2.5]}] [x y])"]
   ["destr :as"          "[1 [1 2 3]]" "(let [[a :as all] [1 2 3]] [a all])"]
   ["destr & rest"       "[1 (quote (2 3))]" "(let [[a & r] [1 2 3]] [a r])"]
   ["destr :strs"        "[1 2]"       "(let [{:strs [a b]} {\"a\" 1 \"b\" 2}] [a b])"]
   ["destr fn-param"     "7"           "((fn [{:keys [a b]}] (+ a b)) {:a 3 :b 4})"]

   ### 2. Atoms
   ["atom swap! inc"     "1"           "(do (def a (atom 0)) (swap! a inc) @a)"]
   ["atom reset!"        "100"         "(do (def a (atom 0)) (reset! a 100) @a)"]
   ["atom CAS ok"        "true"        "(do (def a (atom 5)) (compare-and-set! a 5 10))"]
   ["atom CAS no"        "false"       "(do (def a (atom 5)) (compare-and-set! a 9 10))"]
   ["atom thread-first swap!" "213"    "(do (def a (atom 100)) (swap! a #(-> % (* 2) (+ 3))) (swap! a #(-> % (* 1) (+ 10))) @a)"]
   ["atom swap! args"    "10"          "(do (def a (atom 1)) (swap! a + 2 3 4) @a)"]
   ["atom swap-vals!"    "[1 2]"       "(do (def a (atom 1)) (swap-vals! a inc))"]
   ["atom watch"         "[1 2]"       "(do (def lg (atom nil)) (def a (atom 1)) (add-watch a :k (fn [k r o n] (reset! lg [o n]))) (swap! a inc) @lg)"]
   ["atom validator"     "5"           "(do (def a (atom 1 :validator pos?)) (reset! a 5) @a)"]

   ### 3. Lazy sequences
   ["lazy filter inf"    "(quote (0 2 4 6 8 10 12 14 16 18))" "(take 10 (filter even? (iterate inc 0)))"]
   ["lazy take-while sq" "(quote (0 1 4 9 16 25 36 49))" "(take-while #(< % 50) (map #(* % %) (range)))"]
   ["lazy cycle"         "(quote (:a :b :c :a :b :c :a :b :c :a))" "(take 10 (cycle [:a :b :c]))"]
   ["lazy-seq cons self" "(quote (1 2 4 8 16 32 64 128))"
    "(do (defn my-it [f x] (lazy-seq (cons x (my-it f (f x))))) (take 8 (my-it #(* 2 %) 1)))"]
   ["lazy self-ref fib"  "(quote (0 1 1 2 3 5 8 13 21 34))"
    "(do (def fib (lazy-cat [0 1] (map + (rest fib) fib))) (take 10 fib))"]
   ["repeatedly"         "(quote (1 1 1))" "(repeatedly 3 (fn [] 1))"]
   ["range step"         "(quote (0 2 4 6 8))" "(range 0 10 2)"]

   ### 4. Transducers
   ["xf comp into"       "[1 3 5 7 9]" "(into [] (comp (map inc) (filter odd?)) (range 10))"]
   ["xf sequence"        "(quote (1 3 5 7 9))" "(sequence (comp (map inc) (filter odd?)) (range 10))"]
   ["xf transduce"       "25"          "(transduce (comp (map inc) (filter odd?)) + 0 (range 10))"]
   ["xf take"            "[0 1 2]"     "(into [] (take 3) (range 100))"]
   ["xf remove"          "[1 3 5]"     "(into [] (remove even?) [1 2 3 4 5])"]

   ### 5. Protocols & Records
   ["record area circle" "78"          "(do (defprotocol Sh (ar [t])) (defrecord Ci [r] Sh (ar [_] (int (* 3.14159 r r)))) (int (ar (->Ci 5))))"]
   ["record field"       "5"           "(do (defrecord Ci [r]) (:r (->Ci 5)))"]
   ["record map->"       "3"           "(do (defrecord P [x y]) (:x (map->P {:x 3 :y 4})))"]
   ["protocol 2 methods" "[16 \"sq\"]" "(do (defprotocol Sh (ar [t]) (nm [t])) (defrecord Sq [s] Sh (ar [_] (* s s)) (nm [_] \"sq\")) (let [x (->Sq 4)] [(ar x) (nm x)]))"]
   ["extend-protocol"    "6"           "(do (defprotocol G (g [x])) (extend-protocol G java.lang.Long (g [x] (inc x))) (g 5))"]
   ["reify"              "42"          "(do (defprotocol P (m [_])) (m (reify P (m [_] 42))))"]
   ["record equality"    "true"        "(do (defrecord R [a]) (= (->R 1) (->R 1)))"]

   ### 6. Multimethods
   ["mm dispatch circle" "\"round\""   "(do (defmulti st :kind) (defmethod st :circle [_] \"round\") (defmethod st :default [_] \"unknown\") (st {:kind :circle}))"]
   ["mm default"         "\"unknown\"" "(do (defmulti st :kind) (defmethod st :circle [_] \"round\") (defmethod st :default [_] \"unknown\") (st {:kind :triangle}))"]
   ["mm multi-arity"     "[1 3]"       "(do (defmulti f (fn [& a] (first a))) (defmethod f :x ([_ y] y) ([_ y z] (+ y z))) [(f :x 1) (f :x 1 2)])"]

   ### 7. Macros
   ["macro log-call"     "6"           "(do (defmacro lc [e] `(let [r# ~e] r#)) (lc (* 2 3)))"]
   ["macro quote arg"    "(quote (* 2 3))" "(do (defmacro qa [e] `(quote ~e)) (qa (* 2 3)))"]
   ["macroexpand-1"      "true"        "(do (defmacro mm [x] (list 'inc x)) (= '(inc 5) (macroexpand-1 '(mm 5))))"]
   ["gensym distinct"    "false"       "(= (gensym) (gensym))"]
   ["syntax-quote splice" "[1 2 3]" "(let [xs [1 2 3]] `[~@xs])"]
   ["syntax-quote unquote" "(quote (+ 1 5))" "(let [x 5] `(+ 1 ~x))"]

   ### 8. Recursion
   ["recursion fact"     "120"         "(do (defn fact [n] (if (<= n 1) 1 (* n (fact (dec n))))) (fact 5))"]
   ["recursion loop"     "120"         "(loop [i 5 acc 1] (if (zero? i) acc (recur (dec i) (* acc i))))"]
   ["mutual recursion"   "true"        "(letfn [(ev? [n] (if (zero? n) true (od? (dec n)))) (od? [n] (if (zero? n) false (ev? (dec n))))] (ev? 6))"]
   ["trampoline"         ":done"       "(do (defn a [n] (if (zero? n) :done (fn [] (a (dec n))))) (trampoline a 8))"]

   ### 9. Higher-order functions
   ["partial"            "15"          "((partial + 5) 10)"]
   ["comp"               "8"           "((comp #(* 2 %) inc) 3)"]
   ["juxt"               "[5 6 4]"     "((juxt identity inc dec) 5)"]
   ["every-pred"         "true"        "((every-pred pos? even?) 2 4 6)"]
   ["some-fn"            "true"        "((some-fn even? neg?) 3 4)"]
   ["fnil"               "1"           "((fnil inc 0) nil)"]
   ["complement"         "true"        "((complement nil?) 1)"]

   ### 10. Threading macros
   ["->> pipeline"       "75"          "(->> (range 20) (filter odd?) (map #(* % 3)) (take 5) (reduce +))"]
   ["-> sqrt long"       "15"          "(-> 25 Math/sqrt long (+ 10))"]
   ["some->"             "2"           "(some-> {:a {:b 1}} :a :b inc)"]
   ["some-> nil"         "nil"         "(some-> {:a nil} :a :b)"]
   ["cond->"             "4"           "(cond-> 1 true inc false (* 100) true (* 2))"]
   ["as->"               "20"          "(as-> 1 x (inc x) (* x 10))"]

   ### 11. Exception handling
   ["ex catch"           "\"caught\""  "(try (throw (ex-info \"x\" {})) (catch :default e \"caught\"))"]
   ["ex-message"         "\"broke\""   "(try (throw (ex-info \"broke\" {:code 42})) (catch :default e (ex-message e)))"]
   ["ex-data"            "{:code 42}"  "(try (throw (ex-info \"broke\" {:code 42})) (catch :default e (ex-data e)))"]
   ["try finally"        "[:body :fin]" "(do (def lg (atom [])) (try (swap! lg conj :body) (finally (swap! lg conj :fin))) @lg)"]

   ### 12. For comprehension
   ["for nested :when"   "(quote ([0 1] [0 2] [1 0] [1 2] [2 0] [2 1]))"
    "(for [x (range 3) y (range 3) :when (not= x y)] [x y])"]
   ["for :let"           "(quote (1 4 9))" "(for [x [1 2 3] :let [sq (* x x)]] sq)"]
   ["for :while"         "(quote (0 1 2))" "(for [x (range 10) :while (< x 3)] x)"]

   ### 14. Janet interop
   ["interop method"     "\"v=41\""    "(. {:value 41 :describe (fn [self] (str \"v=\" (:value self)))} describe)"]
   ["interop field"      "41"          "(.-value {:value 41})"]
   # vectors are persistent vectors (Janet tables); lists are Janet arrays
   ["interop janet-type" ":array"      "(do (require '[jolt.interop :as j]) (j/janet-type (list 1 2 3)))"]
  ])

(each [label expected actual] cases (check label expected actual))

(printf "\n=== features-test: %d/%d passed ===" pass (length cases))
(unless (empty? fails)
  (print "--- Failures ---")
  (each [label kind detail] fails (printf "[%s] %s: %s" kind label detail)))
(when (pos? (length fails))
  (error (string (length fails) " feature regression(s)")))
(print "All feature tests passed!")

# Smoke test: the demo file itself loads and runs end-to-end without error.
(when (os/stat "clojure-features.clj")
  (print "\n--- running clojure-features.clj (smoke test) ---")
  (def res (protect (load-string (init) (slurp "clojure-features.clj"))))
  (unless (= (res 0) true)
    (error (string "clojure-features.clj failed to run: " (res 1))))
  (print "--- clojure-features.clj ran cleanly ---"))
