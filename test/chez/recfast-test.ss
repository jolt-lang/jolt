;; Plain-record fast paths (records-coll.ss jrec-plain?).
;;   chez --script test/chez/recfast-test.ss
;; A defrecord that declares none of the collection methods answers count,
;; contains?, seq, empty?, conj, assoc, dissoc, keys/vals, reduce-kv, reduce,
;; into, = and hash from its slots, ahead of the arm walk. This pins the three
;; things that make that sound:
;;   - which types are plain (and that one stops being plain the moment an
;;     extend-type gives it one of those methods),
;;   - that NO registered arm claims a plain record, so answering first cannot
;;     preempt one,
;;   - that each op answers exactly what the arm walk did, extension keys,
;;     reduced, ^double widening and the >8-field spill included.
;; The values are certified against the JVM by the record rows of corpus.edn and
;; unit.edn; the speed by the record arms of test/fastpath_ratio_test.clj.

(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred) (set! total (+ total 1)) (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
(define (evv s) (jolt-compile-eval (string-append "(do " s ")") "user"))
(define (ev s) (jolt-final-str (evv s)))
(define (is name expect s)
  (let ((got (guard (e (#t (string-append "THREW: " (if (message-condition? e) (condition-message e) "?")))) (ev s))))
    (ok (string-append name " => " expect " (got " got ")") (string=? got expect))))

(evv "(defrecord P [x y z])
      (defrecord E [])
      (defrecord D [^double a b])
      (defrecord Big [a b c d e f g h i j])
      (defprotocol Area (area [t]))
      (defrecord WithProto [x] Area (area [_] x))
      (defrecord Counts [x] clojure.lang.Counted (count [_] 42))
      (deftype T [x])
      (defrecord Later [x])")

;; --- which types are plain ---------------------------------------------------
(ok "a bare defrecord is plain" (jrec-plain? (evv "(->P 1 2 3)")))
(ok "a zero-field defrecord is plain" (jrec-plain? (evv "(->E)")))
(ok "a spilled (>8 field) defrecord is plain" (jrec-plain? (evv "(->Big 1 2 3 4 5 6 7 8 9 10)")))
(ok "a protocol impl leaves a record plain" (jrec-plain? (evv "(->WithProto 1)")))
(ok "a declared count makes it not plain" (not (jrec-plain? (evv "(->Counts 1)"))))
(ok "a deftype is never plain" (not (jrec-plain? (evv "(->T 1)"))))
(ok "a map is not plain" (not (jrec-plain? (evv "{:x 1}"))))
(ok "Later starts plain" (jrec-plain? (evv "(->Later 1)")))
(evv "(extend-type Later clojure.lang.Counted (count [_] 7))")
(ok "an extend-type adding count ends plain" (not (jrec-plain? (evv "(->Later 1)"))))
(is "...and count then asks the new method" "7" "(count (->Later 1))")
(is "a declared count still wins" "42" "(count (->Counts 1))")

;; --- no arm claims a plain record ---------------------------------------------
(define plain-probes
  (list (evv "(->P 1 2 3)") (evv "(->E)") (evv "(assoc (->P 1 2 3) :w 4)")
        (evv "(->Big 1 2 3 4 5 6 7 8 9 10)") (evv "(->WithProto 1)")))
(define (no-arm-claims name arms)
  (for-each
   (lambda (r)
     (for-each
      (lambda (a)
        (ok (format "no ~a arm claims a plain record (~a)" name (car a))
            (not (guard (e (#t #f)) ((car a) r)))))
      arms))
   plain-probes))
(no-arm-claims "count" jolt-count-arms)
(no-arm-claims "contains" jolt-contains-arms)
(no-arm-claims "seq" jolt-seq-arms)
(no-arm-claims "empty" jolt-empty-arms)
(no-arm-claims "conj" jolt-conj1-arms)
(no-arm-claims "get" jolt-get-arms)

;; --- each op answers what the walk did ----------------------------------------
(is "count" "3" "(count (->P 1 2 3))")
(is "count with ext" "4" "(count (assoc (->P 1 2 3) :w 4))")
(is "count zero-field" "0" "(count (->E))")
(is "count spill" "10" "(count (->Big 1 2 3 4 5 6 7 8 9 10))")
(is "contains? field" "true" "(contains? (->P 1 nil 3) :y)")
(is "contains? ext" "[true false]" "(let [r (assoc (->P 1 2 3) :w nil)] [(contains? r :w) (contains? r :v)])")
(is "contains? non-key" "false" "(contains? (->P 1 2 3) \"x\")")
(is "seq" "([:x 1] [:y 2] [:z 3])" "(seq (->P 1 2 3))")
(is "seq with ext in order" "([:x 1] [:y 2] [:z 3] [:a 4] [:b 5])" "(seq (assoc (->P 1 2 3) :a 4 :b 5))")
(is "seq zero-field" "nil" "(pr-str (seq (->E)))")
(is "seq zero-field with ext" "([:q 1])" "(seq (assoc (->E) :q 1))")
(is "seq entries are map entries" "[true :x 1]" "(let [e (first (->P 1 2 3))] [(map-entry? e) (key e) (val e)])")
(is "empty?" "[false true false]" "[(empty? (->P 1 2 3)) (empty? (->E)) (empty? (assoc (->E) :a 1))]")
(is "keys" "(:x :y :z :w)" "(keys (assoc (->P 1 2 3) :w 4))")
(is "vals" "(1 2 3 4)" "(vals (assoc (->P 1 2 3) :w 4))")
(is "keys zero-field" "nil" "(pr-str (keys (->E)))")
(is "reduce-kv" "[[:x 1] [:y 2] [:z 3] [:w 4]]" "(reduce-kv (fn [a k v] (conj a [k v])) [] (assoc (->P 1 2 3) :w 4))")
(is "reduce-kv reduced in fields" ":y" "(reduce-kv (fn [a k v] (if (= v 2) (reduced k) a)) nil (->P 1 2 3))")
(is "reduce-kv reduced in ext" ":w" "(reduce-kv (fn [a k v] (if (= v 4) (reduced k) a)) nil (assoc (->P 1 2 3) :w 4 :v 5))")
(is "reduce-kv spill" "55" "(reduce-kv (fn [a k v] (+ a v)) 0 (->Big 1 2 3 4 5 6 7 8 9 10))")
(is "reduce with init" "6" "(reduce (fn [a [k v]] (+ a v)) 0 (->P 1 2 3))")
(is "reduce without init" "[:x 1]" "(reduce (fn [a e] a) (->P 1 2 3))")
(is "reduce without init, empty" "0" "(reduce + (->E))")
(is "reduce reduced" "[:y 2]" "(reduce (fn [a e] (if (= 2 (val e)) (reduced e) a)) nil (->P 1 2 3))")
(is "into {}" "{:x 1, :y 2, :z 3, :w 4}" "(into {} (assoc (->P 1 2 3) :w 4))")
(is "into []" "[[:x 1] [:y 2] [:z 3]]" "(into [] (->P 1 2 3))")
(is "conj pair" "#user.P{:x 9, :y 2, :z 3}" "(conj (->P 1 2 3) [:x 9])")
(is "conj map" "#user.P{:x 1, :y 2, :z 3, :w 4}" "(conj (->P 1 2 3) {:w 4})")
(is "merge" "#user.P{:x 1, :y 8, :z 3, :w 4}" "(merge (->P 1 2 3) {:y 8 :w 4})")
(is "assoc field" "#user.P{:x 1, :y 9, :z 3}" "(assoc (->P 1 2 3) :y 9)")
(is "assoc ext" "#user.P{:x 1, :y 2, :z 3, :w 9}" "(assoc (->P 1 2 3) :w 9)")
(is "assoc keeps the type" "true" "(instance? P (assoc (->P 1 2 3) :y 9))")
(is "assoc ^double widens" "[1.0 2]" "(let [r (assoc (->D 0.5 1) :a 1 :b 2)] [(:a r) (:b r)])")
(is "update" "#user.P{:x 1, :y 3, :z 3}" "(update (->P 1 2 3) :y inc)")
(is "dissoc field downgrades" "[{:x 1, :z 3} false]" "(let [m (dissoc (->P 1 2 3) :y)] [m (record? m)])")
(is "dissoc ext keeps record" "[#user.P{:x 1, :y 2, :z 3} true]" "(let [m (dissoc (assoc (->P 1 2 3) :w 4) :w)] [m (record? m)])")
(is "dissoc absent" "true" "(let [r (->P 1 2 3)] (= r (dissoc r :q)))")
(is "= same" "true" "(= (->P 1 2 3) (->P 1 2 3))")
(is "= differs" "false" "(= (->P 1 2 3) (->P 1 2 4))")
(is "= ext" "[true false]" "[(= (assoc (->P 1 2 3) :w 1) (assoc (->P 1 2 3) :w 1)) (= (assoc (->P 1 2 3) :w 1) (->P 1 2 3))]")
(is "= other record type" "false" "(do (defrecord P2 [x y z]) (= (->P 1 2 3) (->P2 1 2 3)))")
(is "= deftype" "false" "(= (->P 1 2 3) (->T 1))")
(is "= map" "false" "(= (->P 1 2 3) {:x 1 :y 2 :z 3})")
(is "hash stable" "true" "(= (hash (->P 1 2 3)) (hash (->P 1 2 3)))")
(is "hash is the JVM's" "true" "(= (hash (->P 1 2 3)) (bit-xor (hash 'user.P) (hash {:x 1 :y 2 :z 3})))")
(is "set of records" "2" "(count (set [(->P 1 2 3) (->P 1 2 3) (->P 1 2 4)]))")

;; --- construction -------------------------------------------------------------
(is "->P" "#user.P{:x 1, :y 2, :z 3}" "(->P 1 2 3)")
(is "P." "#user.P{:x 1, :y 2, :z 3}" "(P. 1 2 3)")
(is "P. with meta and ext" "[#user.P{:x 1, :y 2, :z 3, :w 4} {:m 1}]" "(let [r (P. 1 2 3 {:m 1} {:w 4})] [r (meta r)])")
(is "P. wrong arity" "Wrong number of args (2) passed to: user/->P" "(try (P. 1 2) (catch Exception e (ex-message e)))")
(is "->E" "#user.E{}" "(->E)")
(is "E. wrong arity" "true" "(try (E. 1) false (catch Exception e true))")
(is "->D widens" "[1.0 2]" "(let [r (->D 1 2)] [(:a r) (:b r)])")
(is "D. widens" "1.0" "(:a (D. 1 2))")
(is "->Big" "10" "(:j (->Big 1 2 3 4 5 6 7 8 9 10))")
(is "map->P" "#user.P{:x 1, :y 2, :z nil}" "(map->P {:x 1 :y 2})")
(is "map->P ext" "#user.P{:x 1, :y 2, :z 3, :w 4}" "(map->P {:x 1 :y 2 :z 3 :w 4})")
(is "map->P nil" "#user.P{:x nil, :y nil, :z nil}" "(map->P nil)")
(is "map->P from record" "#user.P{:x 1, :y 2, :z 3, :w 4}" "(map->P (assoc (->P 1 2 3) :w 4))")

;; --- the keyword site's record cache ----------------------------------------
;; one compiled site, fed two record types, a map, a deftype and nil in turn. A
;; deftype's field reads by keyword here (the JVM answers nil); the site keeps
;; whatever jolt-get answers, and does not cache it — only a record's slot is.
(evv "(defrecord Q [y x]) (defn kx [r] (:x r))")
(is "kw site across receivers" "[1 2 3 4 nil 1 2]"
    "[(kx (->P 1 0 0)) (kx (->Q 0 2)) (kx {:x 3}) (kx (->T 4)) (kx nil) (kx (->P 1 0 0)) (kx (->Q 0 2))]")
(is "kw site ext key" "[5 nil]" "(do (defn kw [r] (:w r)) [(kw (assoc (->P 1 2 3) :w 5)) (kw (->P 1 2 3))])")
(is "kw site after redefinition" "[1 2]"
    "(let [a (kx (->P 1 0 0))] (defrecord P [z x]) [a (kx (->P 1 2))])")

(printf "recfast: ~a/~a passed\n" (- total fails) total)
(exit (if (= fails 0) 0 1))
