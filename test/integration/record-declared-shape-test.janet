# Records use declared-shape layout with fast field access (jolt-t34), by default
# in a direct-linking unit — no JOLT_SHAPE needed. The key property: a record is
# laid out in DECLARED field order, and field reads bare-index by that order, so
# fields that are NOT alphabetically sorted must still read correctly. This is
# what `sidx` reads off the :shape vector (declared order, not str-sorted).
(use ../../src/jolt/api)

(var failures 0)
(defn- check [label got want]
  (unless (= got want)
    (++ failures)
    (printf "FAIL [%s] got %q want %q" label got want)))

# A direct-linking ctx: records are shape-recs, reads proven/bare-indexed.
(def dl (init {:compile? true :direct-linking? true}))

# --- representation: a record is a shape-rec (tuple), not a table -------------
(check "record is a shape-rec"
       (tuple? (eval-string dl "(do (defrecord Sp [x y]) (->Sp 1 2))")) true)

# --- DECLARED-ORDER field access: fields are NOT alphabetically sorted; each
# must read its own value, locally and through a fn boundary. Each case uses a
# DISTINCT record name (redefining a record with new fields is jolt-wf4). -------
(def cases
  [["decl-order local"   "(do (defrecord Ra [b a c]) (let [r (->Ra 10 20 30)] (= [10 20 30] [(:b r) (:a r) (:c r)])))"]
   ["decl-order via fn"   "(do (defrecord Rb [b a c]) (defn rdb [r] [(:b r) (:a r) (:c r)]) (= [10 20 30] (rdb (->Rb 10 20 30))))"]
   ["single field z-first" "(do (defrecord Rc [z m a]) (= 7 (:z (->Rc 7 8 9))))"]
   ["protocol method body" "(do (defprotocol Sh (area [s])) (defrecord Box [w h] Sh (area [b] (* (:w b) (:h b)))) (= 12 (area (->Box 3 4))))"]
   ["record? true"        "(do (defrecord Rd [x y]) (record? (->Rd 1 2)))"]
   ["record vs map not="  "(do (defrecord Re [x y]) (not (= (->Re 1 2) {:x 1 :y 2})))"]
   ["assoc keeps type"    "(do (defrecord Rf [x y]) (record? (assoc (->Rf 1 2) :x 9)))"]
   ["pr declared order"   "(do (defrecord Rg [b a c]) (= \"#user.Rg{:b 10, :a 20, :c 30}\" (pr-str (->Rg 10 20 30))))"]
   # a record shape-rec is a Janet tuple, but a record is NOT a vector/sequential
   # in Clojure — else map-destructuring it takes the kwargs coerce path and
   # corrupts (reitit router crash, jolt-14k).
   ["vector? record false"     "(do (defrecord Rh [x y]) (not (vector? (->Rh 1 2))))"]
   ["sequential? record false" "(do (defrecord Ri [x y]) (not (sequential? (->Ri 1 2))))"]
   ["destructure record :or"   "(do (defrecord Rj [a b c d e]) (let [{:keys [a e] :or {a 0}} (->Rj 1 2 3 4 5)] (= 6 (+ a e))))"]])

(each [label prog] cases
  (check label (eval-string dl prog) true))

(if (pos? failures)
  (do (printf "record-declared-shape: %d failure(s)" failures) (os/exit 1))
  (print "record-declared-shape: all cases passed"))
