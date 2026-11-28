(ns jolt.passes.types.check
  "Success-type error domains (RFC 0006): the curated tables of which concrete
  types each core op provably throws on, the diagnostic emitter, and the user-fn
  signature registry. Pure over inferred types plus the run's `env` cells — no
  inference — so the inferencer (jolt.passes.types) and these rules can't perturb
  each other. The inferencer calls these during its walk; the infer-coupled user
  call probes (re-inference) stay in the inferencer."
  (:require [jolt.op-registry :as op-registry]
            [jolt.passes.types.lattice :refer
             [union-type? umembers struct-type? vec-type? set-type? decl-of]]))

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
(def seq-ops #{"count" "first" "rest" "next" "seq" "nth"})

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

;; --- levels -------------------------------------------------------------------
;; Every diagnostic carries a :level, which is what the compiler does with it:
;;
;;   :error  the call can never succeed. Its argument is PROVEN (a literal, a
;;           value a proven op returned, a local a type test narrowed) to be a
;;           type the op throws on, so wherever it is reached it throws, and the
;;           compile fails. No hint, var root or caller assumption feeds an
;;           :error: the check runs on a fresh unit, where every var and user
;;           fn is :any.
;;   :warn   a likely mistake that can still have a valid runtime reason: the
;;           wrong type is only DECLARED (a ^String hint the value may not
;;           honour), a NaN operand would let min/max answer without throwing,
;;           or the call sits inside a try that catches what it throws (a
;;           thrown? assertion, a probe for the exception).
;;   :lint   not a mistake at all, only advice (a dead type hint), printed only
;;           under JOLT_CHECK.
;;
;; Each rule's error domain is checked against the runtime by `make rettype`:
;; every call it calls an :error has to throw on representative values.

(defn- arg-desc
  "The argument as written, for a message: a local's name or a literal, else nil."
  [node]
  (case (get node :op)
    :local (str "`" (get node :name) "`")
    :const (let [v (get node :val)]
             (when (or (string? v) (keyword? v) (number? v) (nil? v) (boolean? v) (char? v))
               (pr-str v)))
    nil))

(defn- arg-phrase [i node]
  (let [d (arg-desc node)]
    (str "argument " (inc i) (if d (str " (" d ")") ""))))

;; Could this argument be ##NaN? Only a number can, and a literal says which it is.
(defn- maybe-nan? [node t]
  (and (not (not-number? t))
       (not (and (= :const (get node :op)) (number? (get node :val))
                 (== (get node :val) (get node :val))))))

(defn- emit-diag! [env level m]
  (swap! (get env :diags) conj (assoc m :level level)))

;; A proven violation inside a catching try is someone testing for the throw.
(defn- proven-level [env] (if (get env :caught?) :warn :error))

(defn- num-arg-diag! [cn args arg-types i pos env]
  (let [t (nth arg-types i) node (nth args i)
        dt (decl-of t)]
    (cond
      (not-number? t)
      (let [nan-out? (and (contains? #{"min" "max"} cn)
                          (some (fn [j] (and (not= j i) (maybe-nan? (nth args j) (nth arg-types j))))
                                (range (count args))))]
        (emit-diag! env (if nan-out? :warn (proven-level env))
                    {:op cn :argpos i :type (type-name t) :pos pos
                     :msg (str "`" cn "` requires a number, but " (arg-phrase i node)
                               " is " (type-name t)
                               (if nan-out?
                                 ", so this throws unless another argument is NaN"
                                 ", so this call always throws"))}))
      (and dt (not-number? dt))
      (emit-diag! env :warn
                  {:op cn :argpos i :type (type-name dt) :pos pos
                   :msg (str "`" cn "` requires a number, but " (arg-phrase i node)
                             " is hinted as " (type-name dt)
                             "; if the hint is right this call always throws")})
      :else nil)))

(defn check-invoke
  "If node is a core-op call whose argument type is in the op's error domain,
  conj a leveled diagnostic (see levels above) into env's diags cell. arg-types
  is the vector of inferred argument types; pos is the call form's source
  offset, carried into each diagnostic."
  [cn args arg-types pos env]
  (cond
    (and (contains? num-ops cn)
         (not (and (= 1 (count args)) (contains? op-registry/arg1-identity-ops cn))))
    (run! (fn [i] (num-arg-diag! cn args arg-types i pos env)) (range (count args)))
    (and (contains? seq-ops cn) (> (count args) 0))
    (let [t (nth arg-types 0) dt (decl-of t)
          need (if (= cn "count") "a countable collection" "a seqable")]
      (cond
        (not-seqable? t)
        (emit-diag! env (proven-level env)
                    {:op cn :argpos 0 :type (type-name t) :pos pos
                     :msg (str "`" cn "` requires " need ", but " (arg-phrase 0 (nth args 0))
                               " is " (type-name t) ", so this call always throws")})
        (and dt (not-seqable? dt))
        (emit-diag! env :warn
                    {:op cn :argpos 0 :type (type-name dt) :pos pos
                     :msg (str "`" cn "` requires " need ", but " (arg-phrase 0 (nth args 0))
                               " is hinted as " (type-name dt)
                               "; if the hint is right this call always throws")})
        :else nil))
    :else nil))

(defn check-callee
  "Calling a value proven not to be a function: :error (or :warn when caught)."
  [callee-node callee-t pos env]
  (when (not-callable? callee-t)
    (emit-diag! env (proven-level env)
                {:op :call :type (type-name callee-t) :pos pos
                 :msg (str "cannot call " (type-name callee-t)
                           (let [d (arg-desc callee-node)] (if d (str " " d) ""))
                           " as a function, so this call always throws")})))

(defn register-user-fn!
  "Record a (def name (fn [params] body)) — single fixed arity, not redefinable —
  for later user-fn call checking. Redefinable/dynamic and multi/variadic fns are
  skipped (their body is not a stable requirement)."
  [node env]
  (let [init (get node :init)
        m (get node :meta)
        redefable (and m (or (get m :redef) (get m :dynamic)))]
    (when (and (not redefable) (jolt.ir/single-fixed-arity-fn? init))
      (let [ar (first (get init :arities))]
        (swap! (get env :user-sigs) assoc
               (str (get node :ns) "/" (get node :name))
               {:name (get node :name)
                :params (get ar :params) :body (get ar :body)})))))
