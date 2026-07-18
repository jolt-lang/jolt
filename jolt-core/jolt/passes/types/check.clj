(ns jolt.passes.types.check
  "Success-type error domains (RFC 0006): the curated tables of which concrete
  types each core op provably throws on, the diagnostic emitter, and the user-fn
  signature registry. Pure over inferred types plus the run's `env` cells — no
  inference — so the inferencer (jolt.passes.types) and these rules can't perturb
  each other. The inferencer calls these during its walk; the infer-coupled user
  call probes (re-inference) stay in the inferencer."
  (:require [jolt.op-registry :as op-registry]
            [jolt.passes.types.lattice :refer
             [union-type? umembers struct-type? vec-type? set-type?]]))

;; concrete non-numbers: arithmetic provably throws on these. A union is in the
;; error domain only when EVERY member is — if any member is an
;; accepted type the call is accepted (no false positive).
(defn- not-number? [t]
  (if (union-type? t)
    (every? not-number? (umembers t))
    (or (= t :str) (= t :kw) (= t :phm)
        (struct-type? t) (vec-type? t) (set-type? t))))

;; concrete non-seqable scalars: seq/count/first/nth provably throw on these.
;; (Strings and collections ARE seqable/countable; :truthy is ambiguous; :nil
;; and :any are accepted.) A union throws only when every member does.
(defn- not-seqable? [t]
  (if (union-type? t)
    (every? not-seqable? (umembers t))
    (or (= t :num) (= t :kw))))

;; concrete non-callable values: calling them throws "Cannot call X
;; as a function". Only :num and :str — keywords/maps/vectors/sets are IFn,
;; :truthy/:any/:nil are ambiguous (accepted). A union is non-callable only when
;; every member is.
(defn not-callable? [t]
  (if (union-type? t)
    (every? not-callable? (umembers t))
    (or (= t :num) (= t :str))))

;; arithmetic / numeric ops: EVERY argument must be a number — the registry's
;; :num-args? set.
(def ^:private num-ops op-registry/num-arg-ops)
;; seq/count/index ops: argument 0 must be seqable/countable.
(def ^:private seq-ops #{"count" "first" "rest" "next" "seq" "nth"})

(defn type-name
  "Render an inferred type for an error message."
  [t]
  (cond (union-type? t)
          (reduce (fn [s m] (if (= s "") (type-name m) (str s " or " (type-name m))))
                  "" (umembers t))
        (struct-type? t) "a map"
        (vec-type? t) "a vector"
        (set-type? t) "a set"
        (= t :str) "a string"
        (= t :kw) "a keyword"
        (= t :num) "a number"
        (= t :phm) "a map"
        :else (str t)))

(defn check-invoke
  "If node is a core-op call whose argument type is provably in the error domain,
  conj a diagnostic into env's diags cell. arg-types is the vector of inferred
  argument types; pos is the call form's source offset, carried into each
  diagnostic."
  [cn args arg-types pos env]
  (cond
    (contains? num-ops cn)
    (reduce (fn [_ i]
              (let [t (nth arg-types i)]
                (when (not-number? t)
                  (swap! (get env :diags) conj
                         {:op cn :argpos i :type (type-name t) :pos pos
                          :msg (str "`" cn "` requires a number, but argument "
                                    (inc i) " is " (type-name t))})))
              nil)
            nil (range (count args)))
    (and (contains? seq-ops cn) (> (count args) 0))
    (let [t (nth arg-types 0)]
      (when (not-seqable? t)
        (swap! (get env :diags) conj
               {:op cn :argpos 0 :type (type-name t) :pos pos
                :msg (str "`" cn "` requires "
                          (if (= cn "count") "a countable collection" "a seqable")
                          ", but argument 1 is " (type-name t))})))
    :else nil))

(defn register-user-fn!
  "Record a (def name (fn [params] body)) — single fixed arity, not redefinable —
  for later user-fn call checking. Redefinable/dynamic and multi/variadic fns are
  skipped (their body is not a stable requirement)."
  [node env]
  (let [init (get node :init)
        m (get node :meta)
        redefable (and m (or (get m :redef) (get m :dynamic)))]
    (when (and (not redefable) (= :fn (get init :op)))
      (let [arities (get init :arities)]
        (when (= 1 (count arities))
          (let [ar (first arities)]
            (when (not (get ar :rest))
              (swap! (get env :user-sigs) assoc
                     (str (get node :ns) "/" (get node :name))
                     {:name (get node :name)
                      :params (get ar :params) :body (get ar :body)}))))))))
