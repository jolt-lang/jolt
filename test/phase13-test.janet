(use ../src/jolt/api)
(use ../src/jolt/evaluator)
(use ../src/jolt/reader)
(defn ct-eval [ctx s] (normalize-pvecs (eval-string ctx s)))

(defn load-clj [ctx filepath]
  (var s (slurp filepath))
  (while (> (length (string/trim s)) 0)
    (def [form rest] (parse-next s))
    (set s rest)
    (when (not (nil? form))
      (eval-form ctx @{} form))))

(print "=== Phase 13: Protocol Completion ===")

(print "28: reify dispatch...")
(let [ctx (init)]
  (ct-eval ctx "(defprotocol Greeter (say-hello [this]))")
  (ct-eval ctx "(def r (reify Greeter (say-hello [this] \"hello reify\")))")
  (assert (= "hello reify" (ct-eval ctx "(say-hello r)")) "reify dispatch"))
(print "  ok")

(print "29: #() anon-fn reader...")
(let [ctx (init)]
  (assert (= 2 (ct-eval ctx "(#(inc %) 1)")) "anon fn %")
  (assert (= 3 (ct-eval ctx "(#(+ %1 %2) 1 2)")) "anon fn %1 %2")
  (assert (= [1 2 3] (ct-eval ctx "(map #(inc %) [0 1 2])")) "anon fn map"))
(print "  ok")

(print "30: extend-type full dispatch...")
(let [ctx (init)]
  (ct-eval ctx "(defprotocol Greet (g [this]))")
  (ct-eval ctx "(deftype Dog [name])")
  (ct-eval ctx "(extend-type Dog Greet (g [this] (str \"woof \" (.-name this))))")
  (assert (= "woof Rex" (ct-eval ctx "(g (Dog. \"Rex\"))")) "extend-type dog"))
(print "  ok")

(print "31: clojure.walk loading...")
(let [ctx (init)]
  (load-clj ctx "src/jolt/clojure/walk.clj")
  (assert (function? (ct-eval ctx "keywordize-keys")) "keywordize-keys is fn"))
(print "  ok")

(print "\nAll Phase 13 tests passed!")
