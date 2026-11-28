(ns jolt.backend-scheme.slots
  "A deftype method's own field reads and writes as slot accesses: unchecked
  where the arity proved its receiver, as lambda.clj self-receiver does, or a
  specialized site's guard proved its type, as proven-slot answers; else behind
  a per-site descriptor test."
  (:require [jolt.backend-scheme.names :refer [munge-name]]
            [jolt.backend-scheme.session :as session :refer [fresh-label]]
            [jolt.backend-scheme.hoist :refer [hoist-const-per-site]]))

;; [i n] when the slot access's receiver is a local a specialized site proved
;; (session *proven-recs*) and its type's layout puts the field at PLACE, the
;; [i n] the access names; for a write, also not ^double (a set! coerces one).
;; Else nil.
(defn proven-slot [arg-nodes place write?]
  (let [o0 (first arg-nodes)
        pr (when (and (= :local (:op o0)) (not= :gambit (session/target)))
             (get session/*proven-recs* (munge-name (:name o0))))]
    (when pr
      (let [[i n] place
            k (:val (second arg-nodes))
            fields (:fields pr)
            dbl (:dbl pr)]
        (when (and (= n (count fields)) (< -1 i n) (= k (nth fields i))
                   (or (not write?) (and (some? dbl) (= n (count dbl)) (not (nth dbl i)))))
          [i n])))))

(defn proven-slot-read [[i n] o]
  (str (if (<= n 8) "(jrec-slot/u " "(jrec*-slot/u ") o " " i ")"))

(defn proven-slot-write [[i n] o val]
  (let [v (fresh-label "_v$")]
    (str "(let ((" v " " val ")) "
         (if (<= n 8) "(jrec-slot-set!/u " "(jrec*-slot-set!/u ") o " " i " " v ") " v ")")))

;; jrecN-fI, the per-arity slot accessor (records.ss); its -set! twin is the
;; write. names.clj lists every one, so a local of that name is renamed away.
(defn- slot-accessor [n i] (str "jrec" n "-f" i))

(defn- deftype-slot-place [arg-nodes]
  [(:val (nth arg-nodes 2)) (:val (nth arg-nodes 3))])

(declare emit-deftype-slot-read*)
(defn emit-deftype-slot-read [arg-nodes order-args]
  (let [o0 (first arg-nodes)
        proven (proven-slot arg-nodes (deftype-slot-place arg-nodes) false)
        flag (when (= :local (:op o0)) (get session/*self-recs* (munge-name (:name o0))))]
    (cond
      proven
      (order-args (fn [as] (proven-slot-read proven (first as))))
      flag
      ;; the arity checked its receiver once (lambda.clj self-receiver)
      (let [[i n] (deftype-slot-place arg-nodes)]
        (order-args
         (fn [as]
           (str "(if " flag
                (if (<= n 8) (str " (jrec-slot/u " (first as) " " i ")")
                    (str " (jrec*-slot/u " (first as) " " i ")"))
                " (jrec-field " (first as) " " (second as) "))"))))
      :else (emit-deftype-slot-read* arg-nodes order-args))))

(defn- emit-deftype-slot-read* [arg-nodes order-args]
  (let [site (hoist-const-per-site "(jrec-field-site-make)")
        [i n] (deftype-slot-place arg-nodes)
        o (fresh-label "_o$")]
    (order-args
     (fn [as]
       (str "(let ((" o " " (first as) "))"
            " (if (and (jrec? " o ") (eq? (jrec-desc " o ") (unbox " site ")))"
            (cond
              ;; on Chez the matched descriptor is the proof: an unchecked load
              (and (not= :gambit (session/target)) (<= n 8)) (str " (jrec-slot/u " o " " i ")")
              (not= :gambit (session/target)) (str " (jrec*-slot/u " o " " i ")")
              (<= n 8) (str " (" (slot-accessor n i) " " o ")")
              :else (str " (vector-ref (jrec*-vals " o ") " i ")"))
            " (jrec-own-slot-miss " site " " o " " (second as) " " i " " n ")))")))))

(declare emit-deftype-slot-write*)
(defn emit-deftype-slot-write [arg-nodes order-args]
  (if-let [proven (proven-slot arg-nodes (deftype-slot-place arg-nodes) true)]
    (order-args (fn [as] (proven-slot-write proven (first as) (nth as 4))))
    (emit-deftype-slot-write* arg-nodes order-args)))

(defn- emit-deftype-slot-write* [arg-nodes order-args]
  (let [site (hoist-const-per-site "(jrec-field-site-make)")
        [i n] (deftype-slot-place arg-nodes)
        o (fresh-label "_o$")
        v (fresh-label "_v$")]
    (order-args
     (fn [as]
       (str "(let ((" o " " (first as) ") (" v " " (nth as 4) "))"
            " (if (and (jrec? " o ") (eq? (jrec-desc " o ") (unbox " site ")))"
            (cond
              (and (not= :gambit (session/target)) (<= n 8)) (str " (begin (jrec-slot-set!/u " o " " i " " v ") " v ")")
              (not= :gambit (session/target)) (str " (begin (jrec*-slot-set!/u " o " " i " " v ") " v ")")
              (<= n 8) (str " (begin (" (slot-accessor n i) "-set! " o " " v ") " v ")")
              :else (str " (begin (vector-set! (jrec*-vals " o ") " i " " v ") " v ")"))
            " (jrec-own-slot-set-miss! " site " " o " " (second as) " " i " " n " " v ")))")))))
