(ns jolt.passes.types.narrow
  "Predicate folding and flow narrowing for jolt.passes.types: which type
  predicates a proven operand type settles at compile time, the nil narrowing an
  `if` test buys its branches, and the declared-^Record guards the optimizing
  passes wrap around a hinted arity. Pure over types and IR — the inference walk
  (jolt.passes.types) consults these; they never recurse into it."
  (:require [jolt.ir :refer [map-ir-children]]
            [jolt.passes.types.lattice :refer
             [union-type? umembers nilable? strip-nilable decl-of type-depth]]
            [jolt.passes.types.records :refer [record-type-from-entry record-t?]]))

;; Predicate folding: a type predicate whose argument's type is
;; PROVEN folds to a compile-time boolean. Only the precise tags are folded —
;; :num/:str/:kw mean exactly that scalar, and a record carries its defrecord
;; :type tag. NOT folded: vector?/set?/map?, because the :vec tag conflates a
;; real vector with a range/seq (so vector? could be wrong) — left for when the
;; lattice distinguishes them. :any and :truthy carry no structural info (a
;; value known only non-nil could be any concrete type), so every predicate is
;; unknown on them. nil?/some? fold because every concrete type here is
;; provably non-nil.
;;
;; record? and seq? on a struct ask WHICH kind of struct: a deftype has a shape
;; too, and is no record; it may also implement ISeq. Both fold only for a type
;; DEFREC? (the shape registry's :record flag) says is a defrecord — which is
;; never a seq, so the (if (seq? m) … m) map destructuring opens with folds away
;; and the field reads after it stay slot reads.
(def fold-preds
  #{"number?" "string?" "keyword?" "record?" "nil?" "some?" "seq?"})
(declare pred-on)
;; a type no predicate can be proven on: no structural info (:any/:truthy), a
;; DECLARED ^String/^Keyword (the value may be anything), or a nilable struct (it
;; might be nil — nil?/some?/record? can't be proven, so the runtime guard must
;; stay; this is what makes the narrowing sound).
(defn- unprovable-type? [t]
  (or (= t :any) (= t :truthy) (some? (decl-of t)) (nilable? t)))
;; a bounded scalar union folds only when every member agrees
(defn- pred-on-union [pname t defrec?]
  (let [vs (map (fn [m] (pred-on pname m defrec?)) (umembers t))]
    (if (and (seq vs) (not (nil? (first vs))) (apply = vs)) (first vs) nil)))
(defn- defrec-t? [t defrec?] (and (record-t? t) (defrec? (get t :type))))
;; the case table for a concrete, provably non-nil type
(defn- pred-on-concrete [pname t defrec?]
  (case pname
    "number?"  (or (= t :num) (= t :double))
    "string?"  (= t :str)
    "keyword?" (= t :kw)
    "record?"  (defrec-t? t defrec?)
    ;; a defrecord or a scalar is never a seq; anything else may be one
    "seq?"     (when (or (= t :num) (= t :double) (= t :str) (= t :kw)
                         (defrec-t? t defrec?))
                 false)
    "nil?"     false
    "some?"    true
    nil))
(defn pred-on [pname t defrec?]
  (cond
    (unprovable-type? t) nil
    ;; a provably-nil operand (a nil const initializer reaches the case table
    ;; because :any/:truthy/nilable?/union? all miss it): nil? is true, some? and
    ;; every type predicate are false. Without this arm the case table below folds
    ;; nil?->false / some?->true — the wrong constants, since it assumes every
    ;; concrete type is non-nil.
    (= t :nil) (= pname "nil?")
    (union-type? t) (pred-on-union pname t defrec?)
    :else (pred-on-concrete pname t defrec?)))
;; Is TAG (a struct's :type) a defrecord? Read off the shape registry, so a tag
;; with no shape — or one whose shape predates the flag — answers false, which
;; only declines to fold.
(defn defrec-tag-fn [env]
  (let [shapes (get env :record-shapes)]
    (fn [tag] (boolean (some (fn [e] (let [s (val e)] (and (= tag (get s :type)) (get s :record))))
                             shapes)))))
;; Side-effect-free node whose evaluation can be dropped when its predicate
;; folds away (a wider purity analysis can broaden this later).
(defn pure-node? [n] (let [op (get n :op)] (or (= op :const) (= op :local))))

;; Flow-sensitive nil narrowing: in (if (some? x) ..) / (if x ..) / (if (nil? x) ..)
;; a nilable-struct local x is proven non-nil in one branch, so its field reads
;; bare-index and unbox there. Only a nilable local narrows — nothing else changes.
(defn- test-local [test pred-name]
  (when (= :invoke (get test :op))
    (let [f (get test :fn) args (get test :args)]
      (when (and (= :var (get f :op)) (= "clojure.core" (get f :ns))
                 (= pred-name (get f :name))
                 (= 1 (count args)) (= :local (get (nth args 0) :op)))
        (get (nth args 0) :name)))))
(defn- narrow-nonnil [tenv nm]
  (let [t (get tenv nm)] (if (nilable? t) (assoc tenv nm (strip-nilable t)) tenv)))
;; --- declared ^Record params ------------------------------------------------
;; A ^Record hint the program wrote (:uphints) is a declaration, not a proof:
;; Clojure's (:x p), record? and nil? look at the value whatever p is hinted, and
;; a caller may pass a map, nil or another record. Seeding the param with the
;; record type made (:x p) a fixed-slot read with no check, which crashed the
;; binary on anything else. So the optimizing passes wrap a hinted arity's body
;; in a test of the param's record type:
;;
;;   (if (jolt.host/rec-type? p "app.core.Point") body plain-body)
;;
;; The then-branch PROVES p is that record (if-narrow), so it keeps the bare slot
;; reads, devirtualized protocol calls and ^double field arithmetic the hint was
;; there for; the else-branch is the plain body (below). When inference
;; already proves the argument (a whole-program seed, a spliced call with a
;; constructed record), the test folds to true and const-fold drops the copy.
;; The guard args are [local tag ...]; :rec-guard holds the ctor key of each pair
;; by position. Names are read from the args, not stored, because the inline
;; splicer substitutes them.
(def ^:private rec-guard-var {:op :var :ns "jolt.host" :name "rec-type?"})
(defn- rec-guard-pairs [test]
  (when (and (= :invoke (get test :op)) (some? (get test :rec-guard)))
    (let [args (get test :args) cks (get test :rec-guard)]
      (mapv (fn [i] [(nth args (* 2 i)) (get (nth args (inc (* 2 i))) :val) (nth cks i)])
            (range (count cks))))))
(defn- strip-uphints
  "node with every arity's :uphints dropped: the body jolt run compiles, where a
  declared hint types nothing."
  [node]
  (let [node (map-ir-children strip-uphints node)]
    (if (= :fn (get node :op))
      (assoc node :arities (mapv (fn [a] (dissoc a :uphints)) (get node :arities)))
      node)))
;; The else-branch is that plain body, not a second guarded copy. It is the
;; fallback for a value the hint was wrong about, and it answers what jolt run
;; does. Guarding the hinted fns nested in it as well put every nested guard in
;; both branches of every enclosing one, so a nest of hinted fns emitted
;; 2^depth copies of the innermost body. With the else-branch plain, each level
;; adds one guarded copy and one plain copy of what is below it.
(defn guard-declared-records
  "Replace each arity's :uphints with a record-type test around its body (see
  above). `shapes` is the ctor-key -> record shape registry; a hint whose record
  the build doesn't know is dropped, leaving the param untyped."
  [node shapes]
  (let [orig node
        node (map-ir-children (fn [c] (guard-declared-records c shapes)) node)]
    (if (= :fn (get node :op))
      (assoc node :arities
             (mapv (fn [a oa]
                     (let [uh (get a :uphints)]
                       (if (nil? uh)
                         a
                         (let [pairs (reduce (fn [acc pr] (if (some? (get shapes (nth pr 1))) (conj acc pr) acc))
                                             [] uh)
                               a (dissoc a :uphints)]
                           (if (empty? pairs)
                             a
                             (assoc a :body
                                    {:op :if
                                     :test {:op :invoke :fn rec-guard-var
                                            :args (reduce (fn [acc pr]
                                                            (conj acc {:op :local :name (nth pr 0)}
                                                                  {:op :const :val (get (get shapes (nth pr 1)) :type)}))
                                                          [] pairs)
                                            :rec-guard (mapv (fn [pr] (nth pr 1)) pairs)}
                                     :then (get a :body)
                                     :else (strip-uphints (get oa :body))}))))))
                   (get node :arities) (get orig :arities)))
      node)))

;; [then-tenv else-tenv] for an `if` whose test narrows a nilable local, or is a
;; declared-record guard (each tested local is that record in the then-branch).
(defn if-narrow [test tenv env]
  (let [somev (test-local test "some?")
        nilv (test-local test "nil?")
        rg (rec-guard-pairs test)]
    (cond
      (= :local (get test :op)) [(narrow-nonnil tenv (get test :name)) tenv]
      somev [(narrow-nonnil tenv somev) tenv]
      nilv [tenv (narrow-nonnil tenv nilv)]
      rg (let [shapes (get env :record-shapes)]
           [(reduce (fn [te g]
                      (let [x (nth g 0) ck (nth g 2) ent (get shapes ck)]
                        (if (and ent (= :local (get x :op)))
                          (assoc te (get x :name) (record-type-from-entry ent ck type-depth shapes))
                          te)))
                    tenv rg)
            tenv])
      :else [tenv tenv])))
