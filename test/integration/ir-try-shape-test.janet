# IR shape hygiene for :try nodes (phase 3b, jolt-26dm). analyze-try used to
# assoc :catch-sym/:catch-body/:finally nil-when-absent, which made the node a
# phm (jolt's nil-valued-key map representation) and forced backend densification
# before every :op read. It now adds those keys only when the clause is present
# — same discipline as the arity :rest key — so a try node stays a fast struct.
# The change is behavior-invisible (the back end reads each key nil-safely and
# gates on it), so we also pin that tries still evaluate correctly.
(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/reader :as reader)

(print "IR :try shape...")
(def ctx (api/init-cached {:compile? true}))
(defn try-node [src] (backend/analyze-form ctx (reader/parse-string src)))

(defn check-struct [src]
  (def n (try-node src))
  (assert (= :try (n :op)) (string src " is a :try node"))
  # a struct (nil-free) — NOT a phm; phm nodes carry a :jolt/deftype tag
  (assert (struct? n) (string src " analyzes to a struct, not a phm"))
  (assert (nil? (get n :jolt/deftype)) (string src " is not a phm")))

# no catch, no finally — neither optional key should appear
(check-struct "(try 1)")
# finally only — :catch-sym/:catch-body must be ABSENT, not nil
(def fin (try-node "(try 1 (finally 2))"))
(assert (struct? fin) "(try .. finally) is a struct")
(assert (nil? (get fin :catch-body)) "no catch-body when there is no catch")
(assert (get fin :finally) "finally present")
# catch only
(def cat (try-node "(try 1 (catch Throwable e 2))"))
(assert (struct? cat) "(try .. catch) is a struct")
(assert (get cat :catch-sym) "catch-sym present")
(assert (nil? (get cat :finally)) "no finally when there is none")
# catch + finally
(check-struct "(try 1 (catch Throwable e 2) (finally 3))")

# behavior unchanged across all shapes
(assert (= 1 (api/eval-string ctx "(try 1)")) "try value")
(assert (= 2 (api/eval-string ctx "(try (throw (ex-info \"x\" {})) (catch Throwable e 2))")) "catch value")
(assert (= 7 (api/eval-string ctx "(let [a (atom 0)] (try (reset! a 7) (finally nil)) @a)")) "finally runs")
(assert (= 2 (api/eval-string ctx "(try (throw (ex-info \"x\" {})) (catch Throwable e 2) (finally 9))")) "catch+finally")
(print "IR :try shape passed!")
