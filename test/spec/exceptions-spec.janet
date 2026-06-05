# Specification: exceptions — try/catch/finally, throw, ex-info.
(use ../support/harness)

(defspec "exceptions / try-catch"
  ["catch :default"      ":caught"
   "(try (throw (ex-info \"boom\" {})) (catch :default e :caught))"]
  ["catch by class"      ":caught"
   "(try (throw (ex-info \"boom\" {})) (catch Exception e :caught))"]
  ["catch binds error"   "\"boom\""
   "(try (throw (ex-info \"boom\" {})) (catch :default e (ex-message e)))"]
  ["no throw -> body"    "1"
   "(try 1 (catch :default e :caught))"]
  ["finally runs on ok"  "2"
   "(let [a (atom 0)] (try 2 (finally (reset! a 9))) )"]
  ["finally runs on throw" "9"
   "(let [a (atom 0)] (try (throw (ex-info \"x\" {})) (catch :default e nil) (finally (reset! a 9))) @a)"]
  ["catch value of body" "5"
   "(try (+ 2 3) (catch :default e 0))"])

(defspec "exceptions / assert"
  ["assert true -> ok"   ":ok"    "(do (assert true) :ok)"]
  ["assert expr -> ok"   ":ok"    "(do (assert (= 1 1)) :ok)"]
  ["assert false throws" :throws  "(assert false)"]
  ["assert nil throws"   :throws  "(assert nil)"])

(defspec "exceptions / ex-info"
  ["ex-message"          "\"oops\""  "(ex-message (ex-info \"oops\" {}))"]
  ["ex-data"             "{:k 1}"    "(ex-data (ex-info \"oops\" {:k 1}))"]
  ["ex-data via catch"   "{:code 42}"
   "(try (throw (ex-info \"e\" {:code 42})) (catch :default e (ex-data e)))"]
  ["ex-cause"            "true"
   "(let [c (ex-info \"root\" {})] (= c (ex-cause (ex-info \"outer\" {} c))))"]
  ["propagates to outer"  "\"inner\""
   "(try (try (throw (ex-info \"inner\" {})) (finally nil)) (catch :default e (ex-message e)))"]
  ["catch binds thrown value" "42"
   "(try (throw 42) (catch :default e e))"]
  ["rethrow preserves ex"  "\"inner\""
   "(try (try (throw (ex-info \"inner\" {})) (catch :default e (throw e))) (catch :default e (ex-message e)))"])
