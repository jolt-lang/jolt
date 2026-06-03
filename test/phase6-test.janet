# Phase 6: Reader Extensions Tests
(use ../src/jolt/api)
(defn ct-eval [ctx s] (eval-string ctx s))

(print "28: #inst tagged literal...")
(let [ctx (init)]
  (let [val (ct-eval ctx "#inst \"2024-01-15\"")]
    (assert (= "2024-01-15" val) "#inst string"))
  (let [val (ct-eval ctx "#inst \"2024-01-15T10:30:00Z\"")]
    (assert (= "2024-01-15T10:30:00Z" val) "#inst timestamp")))
(print "  passed")

(print "29: #uuid tagged literal...")
(let [ctx (init)]
  (let [val (ct-eval ctx "#uuid \"550e8400-e29b-41d4-a716-446655440000\"")]
    (assert (= "550e8400-e29b-41d4-a716-446655440000" val) "#uuid string")))
(print "  passed")

(print "30: #? reader conditionals...")
(let [ctx (init)]
  (assert (= :yes (ct-eval ctx "#?(:clj :yes :cljs :no)")) "#? selects :clj")
  (assert (= nil (ct-eval ctx "#?(:cljs :no)")) "#? nil on no match"))
(print "  passed")

(print "31: #?@ splicing...")
(let [ctx (init)]
  (assert (= [1 2 3] (ct-eval ctx "[#?@(:clj [1 2 3] :cljs [4 5 6])]"))
          "#?@ splices :clj")
  (assert (= [] (ct-eval ctx "[#?@(:cljs [1 2])]"))
          "#?@ nothing on no match"))
(print "  passed")

(print "\nAll Phase 6 tests passed!")
