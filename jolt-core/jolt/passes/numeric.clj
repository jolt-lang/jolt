(ns jolt.passes.numeric
  "Hint-directed numeric specialization. A local forward type-flow that seeds
  local kinds from `^double`/`^long` fn-param hints and float literals, propagates
  them through let inits, arithmetic results, and if/do, and tags an arithmetic
  `:invoke` node with `:num-kind :double` or `:long` when every operand is that
  kind (an integer literal is a wildcard, valid in either). The back end then emits
  Chez `fl*`/`fx*` ops instead of generic arithmetic.

  Soundness: `:long` is seeded ONLY from an explicit `^long` hint — never a bare
  integer literal — so un-hinted integer code keeps jolt's arbitrary-precision
  numbers (no fixnum overflow surprise). `:double` is seeded from `^double` hints
  and float literals; flonum arithmetic is always flonum, so this matches the
  generic result. A `^long` hint is a promise the value is a fixnum: `fx+` raises
  on overflow rather than promoting, exactly as a JVM primitive long is fixed-width.

  Runs in every build and at `-e`/repl, but not the seed mint (which compiles with
  the passes off), so it stays out of the self-host fixpoint and benefits open and
  closed builds alike."
  (:require [jolt.ir :refer [map-ir-children]]))

;; --- operand classification -------------------------------------------------
(defn- int-lit? [n]
  (and (= :const (get n :op))
       (let [v (get n :val)] (and (number? v) (integer? v)))))
(defn- float-lit? [n]
  (and (= :const (get n :op))
       (let [v (get n :val)] (and (number? v) (float? v)))))

;; result kind of a double-specialized op at this name/arity, or nil if N/A.
;; arithmetic -> :double; comparison -> :bool (operands specialized, result not numeric).
;; Every op name dbl-spec / lng-spec returns non-nil for must have a Chez op in
;; jolt.backend-scheme/dbl-ops resp. lng-ops, or emit-numeric splices a nil op.
(defn- dbl-spec [nm n]
  (cond
    (and (>= n 1) (contains? #{"+" "-" "*" "/" "min" "max"} nm)) :double
    (and (= n 1) (contains? #{"inc" "dec"} nm)) :double
    (and (>= n 2) (contains? #{"<" ">" "<=" ">=" "=" "=="} nm)) :bool
    :else nil))

;; result kind of a long-specialized op, or nil. `/` is absent on purpose:
;; (/ long long) is a Ratio in Clojure, not a long. unchecked-* join the fast path
;; (they aren't native ops otherwise).
(defn- lng-spec [nm n]
  (cond
    (and (>= n 1) (contains? #{"+" "-" "*" "min" "max"
                               "unchecked-add" "unchecked-subtract" "unchecked-multiply"} nm)) :long
    (and (= n 1) (contains? #{"inc" "dec" "unchecked-inc" "unchecked-dec"} nm)) :long
    (and (= n 2) (contains? #{"quot" "rem" "mod"} nm)) :long
    (and (>= n 2) (contains? #{"<" ">" "<=" ">=" "=" "=="} nm)) :bool
    :else nil))

;; result kind of a bigdec-specialized op, or nil. Arithmetic / quot / rem yield a
;; bigdec; the comparisons and zero?/pos?/neg? yield a bool. `=` is left to the
;; generic jolt= (already bigdec-aware), and `/` can throw (non-terminating) but is
;; still a bigdec op. Each non-nil name must have an entry in backend bd-ops.
(defn- bd-spec [nm n]
  (cond
    (and (>= n 1) (contains? #{"+" "-" "*" "/"} nm)) :bigdec
    (and (= n 2) (contains? #{"quot" "rem"} nm)) :bigdec
    (and (= n 1) (contains? #{"zero?" "pos?" "neg?"} nm)) :bool
    (and (>= n 2) (contains? #{"<" ">" "<=" ">="} nm)) :bool
    :else nil))

;; A non-numeric result (a comparison) doesn't propagate a numeric kind.
(defn- propagate [spec] (if (= spec :bool) nil spec))

(declare an)

;; The recur-arg kinds for the recurs targeting THIS loop level. recur only appears
;; in tail position (an if branch, a do's ret, a let body), so descend only those;
;; a nested loop/fn (and any non-tail child) owns its own recur and is skipped.
(defn- recur-kinds [node tenv]
  (let [op (get node :op)]
    (cond
      (= op :recur) [(mapv (fn [a] (nth (an a tenv) 0)) (get node :args))]
      (= op :let) (recur-kinds (get node :body)
                               (reduce (fn [te b] (assoc te (nth b 0) (nth (an (nth b 1) te) 0)))
                                       tenv (get node :bindings)))
      (= op :if) (concat (recur-kinds (get node :then) tenv) (recur-kinds (get node :else) tenv))
      (= op :do) (recur-kinds (get node :ret) tenv)
      :else [])))

;; The recur-arg NODE lists for the recurs at THIS loop level (structural, no env),
;; parallel to recur-kinds. Used to recognise a counter.
(defn- recur-arg-lists [node]
  (let [op (get node :op)]
    (cond
      (= op :recur) [(get node :args)]
      (= op :let) (recur-arg-lists (get node :body))
      (= op :if) (concat (recur-arg-lists (get node :then)) (recur-arg-lists (get node :else)))
      (= op :do) (recur-arg-lists (get node :ret))
      :else [])))

;; Is `arg` an increment-style step of loop var `vname`: the var unchanged, or
;; inc/dec/unchecked-inc/dec, or (+/- var <int-literal>)? Bounded growth that a
;; fixnum-range counter can sustain for any realistic loop — unlike (* acc x), which
;; overflows fast, so a multiplicative accumulator never qualifies and stays
;; arbitrary-precision.
(defn- counter-step? [arg vname]
  (cond
    (and (= :local (get arg :op)) (= vname (get arg :name))) true
    (= :invoke (get arg :op))
    (let [f (get arg :fn) as (get arg :args)]
      (and (= :var (get f :op)) (= "clojure.core" (get f :ns))
           (let [nm (get f :name)
                 v? (fn [n] (and (= :local (get n :op)) (= vname (get n :name))))]
             (cond
               (and (contains? #{"inc" "dec" "unchecked-inc" "unchecked-dec"} nm) (= 1 (count as)))
               (v? (nth as 0))
               (and (contains? #{"+" "unchecked-add"} nm) (= 2 (count as)))
               (or (and (v? (nth as 0)) (int-lit? (nth as 1)))
                   (and (v? (nth as 1)) (int-lit? (nth as 0))))
               (and (contains? #{"-" "unchecked-subtract"} nm) (= 2 (count as)))
               (and (v? (nth as 0)) (int-lit? (nth as 1)))
               :else false))))
    :else false))

;; Loop-var kinds by bounded fixpoint. A var keeps its init kind (:double or :long)
;; only if every recur arg in that slot is the same kind (under the current
;; assumption) — a monotone demotion that stops at a fixpoint, bounded by the var
;; count. An integer-literal init has kind nil and stays generic, so a bignum loop
;; keeps arbitrary precision (no :long from a bare literal). A typed loop var's init
;; and recur args are all flonums/fixnums (a :long init flows from a coerced ^long
;; value or an fx op), so no entry coercion is needed here, unlike a fn param.
(defn- loop-kinds [names seed body tenv]
  (loop [cur seed iter 0]
    (if (> iter (count names))
      cur
      (let [te (reduce (fn [t i] (assoc t (nth names i) (nth cur i))) tenv (range (count names)))
            rks (recur-kinds body te)
            nxt (mapv (fn [j]
                        (let [k (nth cur j)]
                          (if (and k (every? (fn [rk] (= k (nth rk j))) rks)) k nil)))
                      (range (count names)))]
        (if (= nxt cur) cur (recur nxt (inc iter)))))))

;; Seed a fn arity's local env from its numeric param hints; an unhinted param
;; shadows any same-named outer local to nil.
(defn- arity-env [tenv a]
  (let [nh (into {} (get a :nhints))
        pe (reduce (fn [e p] (assoc e p (get nh p))) tenv (get a :params))]
    (if (get a :rest) (assoc pe (get a :rest) nil) pe)))

(defn- an-invoke
  "Annotate an :invoke with its numeric kind. An arithmetic core op specializes to
  the Chez fl*/fx* op only when every operand is the same kind (:double or :long),
  except an integer literal is :wild — valid in either — so (+ ^double x 2) stays
  double. A call to a ^double/^long-returning var yields that kind without lowering
  the call (its body already coerces the return)."
  [node tenv]
  (let [fnode (get node :fn)
        nm (when (and (= :var (get fnode :op)) (= "clojure.core" (get fnode :ns)))
             (get fnode :name))
        ars (mapv (fn [a] (an a tenv)) (get node :args))
        argnodes (mapv (fn [r] (nth r 1)) ars)
        node1 (assoc node :args argnodes)
        n (count ars)]
    (cond
      ;; a call to a var with a declared numeric return (^double/^long) yields that
      ;; kind, so an accumulator over the result types. The call itself isn't an
      ;; arithmetic op to lower — its body already coerces the return.
      (get fnode :num-ret) [(get fnode :num-ret) node1]
      (nil? nm) [nil node1]
      :else
      (let [;; per-operand class: :double / :long / :bigdec (typed), :wild (integer
            ;; literal, usable in any), or :no (anything else — blocks specialization).
            cls (mapv (fn [r] (let [k (nth r 0) nd (nth r 1)]
                                (cond (= k :double) :double
                                      (= k :long) :long
                                      (= k :bigdec) :bigdec
                                      (int-lit? nd) :wild
                                      :else :no)))
                      ars)
            ok? (fn [allowed need]
                  (and (pos? n)
                       (every? (fn [c] (or (= c :wild) (= c allowed))) cls)
                       (some (fn [c] (= c need)) cls)))
            ds (dbl-spec nm n)
            ls (lng-spec nm n)
            bs (bd-spec nm n)]
        (cond
          (and ds (ok? :double :double))
          ;; coerce integer-literal operands to flonum so fl-ops never see an exact int.
          (let [args' (mapv (fn [nd] (if (int-lit? nd) (assoc nd :val (double (get nd :val))) nd))
                            argnodes)]
            [(propagate ds) (assoc node1 :args args' :num-kind :double)])
          (and ls (ok? :long :long))
          [(propagate ls) (assoc node1 :num-kind :long)]
          ;; bigdec: every operand a bigdec (integer literals allowed, coerced at
          ;; runtime). A flonum operand blocks this (double contagion) and falls
          ;; through to the generic op.
          (and bs (ok? :bigdec :bigdec))
          [(propagate bs) (assoc node1 :num-kind :bigdec)]
          :else [nil node1])))))

;; Returns [kind node'] — kind is :double, :long, or nil.
(defn- an [node tenv]
  (let [op (get node :op)]
    (cond
      (= op :const) [(if (float-lit? node) :double nil) node]
      ;; a bigdec (M) literal seeds the :bigdec kind so call-position arithmetic
      ;; over it (and let-bound copies of it) dispatches to the bigdec engine.
      (= op :bigdec) [:bigdec node]
      (= op :local) [(get tenv (get node :name)) node]
      (= op :coerce) [(get node :kind) (assoc node :expr (nth (an (get node :expr) tenv) 1))]
      (= op :invoke) (an-invoke node tenv)
      (= op :let)
      (let [res (reduce (fn [acc b]
                          (let [te (nth acc 0) binds (nth acc 1)
                                ir (an (nth b 1) te)]
                            [(assoc te (nth b 0) (nth ir 0)) (conj binds [(nth b 0) (nth ir 1)])]))
                        [tenv []] (get node :bindings))
            br (an (get node :body) (nth res 0))]
        [(nth br 0) (assoc node :bindings (nth res 1) :body (nth br 1))])
      (= op :loop)
      ;; inits evaluate in the OUTER env; loop vars get their fixpoint kinds for the body.
      (let [binds (get node :bindings)
            names (mapv (fn [b] (nth b 0)) binds)
            ik (mapv (fn [b] (nth (an (nth b 1) tenv) 0)) binds)
            rlists (recur-arg-lists (get node :body))
            ;; seed each var: an already-typed init keeps its kind; an integer-literal
            ;; init whose recur args are all counter steps is a fixnum counter (:long).
            seed (mapv (fn [j]
                         (let [k (nth ik j) b (nth binds j)]
                           (cond
                             k k
                             ;; an int-literal var is a fixnum counter only in a real
                             ;; iterating loop (>= 1 recur) whose every step is bounded.
                             ;; A recur-less loop is a let — its int literal stays
                             ;; generic (arbitrary precision), like a let binding.
                             (and (seq rlists)
                                  (int-lit? (nth b 1))
                                  (every? (fn [args] (counter-step? (nth args j) (nth b 0))) rlists))
                             :long
                             :else nil)))
                       (range (count names)))
            lk (loop-kinds names seed (get node :body) tenv)
            te (reduce (fn [t i] (assoc t (nth names i) (nth lk i))) tenv (range (count names)))]
        [nil (assoc node
                    :bindings (mapv (fn [b] [(nth b 0) (nth (an (nth b 1) tenv) 1)]) binds)
                    :body (nth (an (get node :body) te) 1))])
      (= op :if)
      (let [tr (an (get node :test) tenv)
            thn (an (get node :then) tenv)
            els (an (get node :else) tenv)
            tk (nth thn 0) ek (nth els 0)]
        [(if (= tk ek) tk nil)
         (assoc node :test (nth tr 1) :then (nth thn 1) :else (nth els 1))])
      (= op :do)
      (let [stmts (mapv (fn [s] (nth (an s tenv) 1)) (get node :statements))
            r (an (get node :ret) tenv)]
        [(nth r 0) (assoc node :statements stmts :ret (nth r 1))])
      (= op :fn)
      [nil (assoc node :arities
                  (mapv (fn [a] (assoc a :body (nth (an (get a :body) (arity-env tenv a)) 1)))
                        (get node :arities)))]
      (= op :def) [nil (assoc node :init (nth (an (get node :init) tenv) 1))]
      ;; every other op introduces no bindings and isn't numeric: descend with the
      ;; same env to specialize nested arithmetic, no kind.
      :else [nil (map-ir-children (fn [c] (nth (an c tenv) 1)) node)])))

(defn annotate
  "Tag arithmetic nodes with :num-kind from local numeric type-flow. Returns the
  rewritten IR (no kind escapes to the caller)."
  [node]
  (nth (an node {}) 1))
