# Specification: protocols, types and records.
(use ../support/harness)

(defspec "protocols / defprotocol & dispatch"
  ["protocol on record"  "16"
   "(do (defprotocol Shape (area [s])) (defrecord Sq [side] Shape (area [_] (* side side))) (area (->Sq 4)))"]
  ["protocol on deftype"  "16"
   "(do (defprotocol Shape (area [s])) (deftype Sq [side] Shape (area [_] (* side side))) (area (->Sq 4)))"]
  ["multiple methods"     "[1 2]"
   "(do (defprotocol P (m [s]) (n [s])) (defrecord R [a b] P (m [_] a) (n [_] b)) [(m (->R 1 2)) (n (->R 1 2))])"]
  ["multiple protocols"   "[:a :b]"
   "(do (defprotocol P1 (p1 [s])) (defprotocol P2 (p2 [s])) (deftype T [] P1 (p1 [_] :a) P2 (p2 [_] :b)) [(p1 (->T)) (p2 (->T))])"]
  ["method args"          "7"
   "(do (defprotocol P (add [s x])) (defrecord R [n] P (add [_ x] (+ n x))) (add (->R 5) 2))"]
  ["extend-type"          "10"
   "(do (defprotocol P (twice [s])) (extend-type Number P (twice [n] (* n 2))) (twice 5))"]
  ["extend-protocol"      "[2 4]"
   "(do (defprotocol P (dbl [s])) (extend-protocol P Number (dbl [n] (* n 2))) [(dbl 1) (dbl 2)])"])

(defspec "protocols / records"
  ["record field access"  "1"
   "(do (defrecord R [a b]) (:a (->R 1 2)))"]
  ["record map access"    "2"
   "(do (defrecord R [a b]) (get (->R 1 2) :b))"]
  ["record equality"      "true"
   "(do (defrecord R [a b]) (= (->R 1 2) (->R 1 2)))"]
  ["record inequality"    "false"
   "(do (defrecord R [a b]) (= (->R 1 2) (->R 3 4)))"]
  ["map-> factory"        "1"
   "(do (defrecord R [a b]) (:a (map->R {:a 1 :b 2})))"]
  ["record? true"         "true"
   "(do (defrecord R [a]) (record? (->R 1)))"]
  ["assoc on record"      "9"
   "(do (defrecord R [a]) (:a (assoc (->R 1) :a 9)))"])

(defspec "protocols / reify & satisfies"
  ["reify dispatch"       "42"
   "(do (defprotocol P (m [_])) (m (reify P (m [_] 42))))"]
  ["reify multi-method"   "[1 2]"
   "(do (defprotocol P (a [_]) (b [_])) (let [r (reify P (a [_] 1) (b [_] 2))] [(a r) (b r)]))"]
  ["satisfies? true"      "true"
   "(do (defprotocol P (m [_])) (defrecord R [] P (m [_] 1)) (satisfies? P (->R)))"]
  ["satisfies? false"     "false"
   "(do (defprotocol P (m [_])) (defrecord R []) (satisfies? P (->R)))"]
  ["instance? record"     "true"
   "(do (defrecord R [a]) (instance? R (->R 1)))"])
