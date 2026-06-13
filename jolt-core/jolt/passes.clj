(ns jolt.passes
  "IR optimization passes (nanopass-lite, jolt-2om). Each pass is a pure
  IR -> IR rewrite, total over node :ops (unknown ops pass through with
  folded children, so adding a node kind can't silently break a pass), run
  in a fixed order by run-passes between the analyzer and the back end.
  Portable Clojure: same constraint as jolt.analyzer — kernel-tier fns +
  seed primitives only (it loads with the compiler namespaces).

  Passes:
    const-fold      — bottom-up numeric folding + dead-branch removal (always).
    inline-node     — splice small direct-linked fns at their call sites.
    flatten-lets    — hoist a let bound directly to another let's bindings.
    scalar-replace  — AOT escape analysis: drop map allocations whose only use
                      is constant-keyword lookup ((:r {:r a ..}) -> a).

  inline + flatten + scalar-replace run only when host/inline-enabled? (user
  code opted into direct-linking, jolt-87f); core and the bootstrap compiler
  compile with const-fold alone, exactly as before."
  (:require [jolt.host :refer [inline-enabled? inline-ir]]))

;; Folding computes with THE ACTUAL jolt fns, so a folded result matches what
;; the unfolded code would produce at runtime by construction. Conservative:
;; numbers only, the op table only names pure numeric fns, and any throw
;; during folding (e.g. (mod x 0)) leaves the node alone for runtime.
(def ^:private foldable
  ;; SEED fns only: this ns loads with the compiler, BEFORE the later core
  ;; tiers — a name from 20-coll (min/max/abs) wouldn't resolve yet.
  {"+" + "-" - "*" * "/" /
   "<" < ">" > "<=" <= ">=" >= "=" =
   "inc" inc "dec" dec
   "mod" mod "rem" rem "quot" quot
   ;; the __bit-* seams: the PUBLIC bit fns are 20-coll variadic shells now,
   ;; which don't exist yet when this ns loads. Folding stays 2-arg (a 3+-arg
   ;; constant call throws arity inside the fold and is left for runtime).
   "bit-and" __bit-and "bit-or" __bit-or "bit-xor" __bit-xor})

(defn- const? [n] (= :const (get n :op)))
(defn- const-num? [n] (and (const? n) (number? (get n :val))))

(defn- fold-fn [fnode]
  (let [op (get fnode :op)]
    (when (or (and (= op :var) (= "clojure.core" (get fnode :ns)))
              (= op :host))
      (get foldable (get fnode :name)))))

(defn const-fold
  "Bottom-up constant folding: a call of a foldable numeric fn whose args are
  all constant numbers becomes a constant; an if with a constant test becomes
  the taken branch."
  [node]
  (let [op (get node :op)]
    (cond
      (= op :invoke)
      (let [f (const-fold (get node :fn))
            args (mapv const-fold (get node :args))
            ff (fold-fn f)
            folded (when (and ff (pos? (count args)) (every? const-num? args))
                     (try
                       {:op :const :val (apply ff (mapv (fn [a] (get a :val)) args))}
                       (catch Exception e nil)))]
        (or folded (assoc node :fn f :args args)))

      (= op :if)
      (let [t (const-fold (get node :test))]
        (if (const? t)
          ;; jolt truthiness = Clojure's: nil/false take else
          (if (or (nil? (get t :val)) (= false (get t :val)))
            (const-fold (get node :else))
            (const-fold (get node :then)))
          (assoc node
                 :test t
                 :then (const-fold (get node :then))
                 :else (const-fold (get node :else)))))

      (= op :do)
      (assoc node
             :statements (mapv const-fold (get node :statements))
             :ret (const-fold (get node :ret)))

      ;; let/loop bindings are [name-string init-ir] PAIRS (see
      ;; analyzer/analyze-bindings), not maps.
      (= op :let)
      (assoc node
             :bindings (mapv (fn [b] [(nth b 0) (const-fold (nth b 1))])
                             (get node :bindings))
             :body (const-fold (get node :body)))

      (= op :loop)
      (assoc node
             :bindings (mapv (fn [b] [(nth b 0) (const-fold (nth b 1))])
                             (get node :bindings))
             :body (const-fold (get node :body)))

      (= op :recur)
      (assoc node :args (mapv const-fold (get node :args)))

      (= op :fn)
      (assoc node
             :arities (mapv (fn [a] (assoc a :body (const-fold (get a :body))))
                            (get node :arities)))

      (= op :def)    (assoc node :init (const-fold (get node :init)))
      (= op :throw)  (assoc node :expr (const-fold (get node :expr)))
      (= op :vector) (assoc node :items (mapv const-fold (get node :items)))
      (= op :set)    (assoc node :items (mapv const-fold (get node :items)))
      (= op :map)    (assoc node :pairs (mapv (fn [pr] (mapv const-fold pr)) (get node :pairs)))

      ;; leaves and anything this pass doesn't know: unchanged
      :else node)))

;; ---------------------------------------------------------------------------
;; Shared state: a dirty flag the fixpoint loop reads, and a fresh-name counter
;; for alpha-renaming inlined bodies (same atom pattern as analyzer/gen-name).
;; ---------------------------------------------------------------------------
(def ^:private dirty (atom false))
(defn- mark! [] (reset! dirty true))

(def ^:private fresh-counter (atom 0))
(defn- fresh [base]
  (let [n @fresh-counter]
    (swap! fresh-counter inc)
    (str base "__il" n)))

;; ---------------------------------------------------------------------------
;; Inlining (jolt-87f). The back end stashes {:params [..] :body ir} on the var
;; cell of each single-fixed-arity defn compiled under :inline?; here we splice
;; that body at a call site. To stay capture-safe we ALPHA-RENAME the body —
;; every param and every inner let-bound name becomes a globally fresh name —
;; then bind the fresh params to the call's args in a wrapping let (args eval
;; once, in source order). After full renaming no name in the spliced body can
;; collide with a caller local, so flatten-lets and scalar-replace need no
;; shadowing logic.
;; ---------------------------------------------------------------------------

(defn- safe-op? [op]
  ;; ops an inline-eligible body may contain. recur/loop/fn/try/def are excluded
  ;; (binding/control forms the splicer doesn't handle), so a body containing one
  ;; is rejected by body-size below and never inlined or alpha-renamed.
  (or (= op :const) (= op :local) (= op :var) (= op :host) (= op :the-var)
      (= op :quote) (= op :if) (= op :do) (= op :let) (= op :invoke)
      (= op :map) (= op :vector) (= op :set) (= op :throw)))

(def ^:private inline-budget 120)

(defn- body-size
  "Node count of an inline-eligible body. A disallowed op contributes a number
  larger than any budget, so the caller's (<= size budget) test fails and we
  never try to inline (or alpha-rename) such a body."
  [node]
  (let [op (get node :op)]
    (cond
      (not (safe-op? op)) 100000
      (= op :if) (+ 1 (body-size (get node :test))
                      (body-size (get node :then))
                      (body-size (get node :else)))
      (= op :do) (+ 1 (reduce + 0 (mapv body-size (get node :statements)))
                      (body-size (get node :ret)))
      (= op :throw) (+ 1 (body-size (get node :expr)))
      (= op :invoke) (+ 1 (body-size (get node :fn))
                          (reduce + 0 (mapv body-size (get node :args))))
      (= op :let) (+ 1 (reduce + 0 (mapv (fn [b] (body-size (nth b 1))) (get node :bindings)))
                       (body-size (get node :body)))
      (= op :vector) (+ 1 (reduce + 0 (mapv body-size (get node :items))))
      (= op :set) (+ 1 (reduce + 0 (mapv body-size (get node :items))))
      (= op :map) (+ 1 (reduce + 0 (mapv (fn [pr] (+ (body-size (nth pr 0))
                                                     (body-size (nth pr 1))))
                                         (get node :pairs))))
      :else 1)))

(defn- subst
  "Substitute locals in node per env (a map name -> replacement IR node), and
  alpha-rename every inner :let binder to a globally fresh name (so the spliced
  body shares no name with the caller). env seeds the params: a trivial arg
  (local/const) maps a param straight to the arg node (copy propagation — this
  is what lets scalar-replace see a map-literal arg through the call boundary);
  a non-trivial arg maps the param to a fresh :local that a wrapping let binds."
  [node env]
  (let [op (get node :op)]
    (cond
      (= op :local) (let [r (get env (get node :name))]
                      ;; carry the param's ^:struct hint onto a let-bound fresh
                      ;; local, so lookups inside the inlined body keep the bare
                      ;; (no-guard) path (jolt-dad). The param hint asserts the
                      ;; arg is a struct; inlining doesn't change that contract.
                      (if r
                        (if (and (= :local (get r :op)) (get node :hint) (not (get r :hint)))
                          (assoc r :hint (get node :hint))
                          r)
                        node))
      (= op :if) (assoc node
                        :test (subst (get node :test) env)
                        :then (subst (get node :then) env)
                        :else (subst (get node :else) env))
      (= op :do) (assoc node
                        :statements (mapv (fn [s] (subst s env)) (get node :statements))
                        :ret (subst (get node :ret) env))
      (= op :throw) (assoc node :expr (subst (get node :expr) env))
      (= op :invoke) (assoc node
                            :fn (subst (get node :fn) env)
                            :args (mapv (fn [a] (subst a env)) (get node :args)))
      (= op :vector) (assoc node :items (mapv (fn [x] (subst x env)) (get node :items)))
      (= op :set) (assoc node :items (mapv (fn [x] (subst x env)) (get node :items)))
      (= op :map) (assoc node :pairs (mapv (fn [pr] [(subst (nth pr 0) env)
                                                     (subst (nth pr 1) env)])
                                           (get node :pairs)))
      (= op :let)
      (let [res (reduce (fn [acc b]
                          (let [e (nth acc 0)
                                binds (nth acc 1)
                                nm (nth b 0)
                                init (subst (nth b 1) e)
                                f (fresh nm)]
                            [(assoc e nm {:op :local :name f}) (conj binds [f init])]))
                        [env []]
                        (get node :bindings))]
        (assoc node :bindings (nth res 1) :body (subst (get node :body) (nth res 0))))
      ;; :const :var :host :the-var :quote — no locals to substitute
      :else node)))

(defn- trivial-arg? [n]
  ;; safe to substitute directly (immutable, free to duplicate): a local read or
  ;; a constant. Everything else is let-bound so it evaluates exactly once.
  (let [op (get n :op)] (or (= op :local) (= op :const))))

(defn- body-closed?
  "True if every :local in node is bound — by a param (in the initial scope set)
  or by an enclosing :let within the body. A self-recursive fn fails this: the
  analyzer binds the fn's own name as a local, so its body has a FREE local (the
  self-reference) that would dangle once the body is spliced elsewhere."
  [node scope]
  (let [op (get node :op)]
    (cond
      (= op :local) (contains? scope (get node :name))
      (= op :const) true
      (= op :var) true
      (= op :host) true
      (= op :the-var) true
      (= op :quote) true
      (= op :if) (and (body-closed? (get node :test) scope)
                      (body-closed? (get node :then) scope)
                      (body-closed? (get node :else) scope))
      (= op :do) (and (every? (fn [s] (body-closed? s scope)) (get node :statements))
                      (body-closed? (get node :ret) scope))
      (= op :throw) (body-closed? (get node :expr) scope)
      (= op :invoke) (and (body-closed? (get node :fn) scope)
                          (every? (fn [a] (body-closed? a scope)) (get node :args)))
      (= op :vector) (every? (fn [x] (body-closed? x scope)) (get node :items))
      (= op :set) (every? (fn [x] (body-closed? x scope)) (get node :items))
      (= op :map) (every? (fn [pr] (and (body-closed? (nth pr 0) scope)
                                        (body-closed? (nth pr 1) scope)))
                          (get node :pairs))
      (= op :let)
      (let [res (reduce (fn [acc b]
                          (let [sc (nth acc 0) ok (nth acc 1)]
                            (if (not ok)
                              acc
                              [(conj sc (nth b 0)) (body-closed? (nth b 1) sc)])))
                        [scope true]
                        (get node :bindings))]
        (and (nth res 1) (body-closed? (get node :body) (nth res 0))))
      :else false)))

(defn- try-inline
  "node is an :invoke whose children are already inlined. If its :fn is a var
  with a stashed, in-budget, arity-matching inline body, return the spliced
  let; else node."
  [node ctx]
  (let [f (get node :fn)]
    (if (= :var (get f :op))
      (let [stash (inline-ir ctx (get f :ns) (get f :name))]
        (if stash
          (let [params (get stash :params)
                body (get stash :body)
                args (get node :args)]
            (if (and (= (count params) (count args))
                     (<= (body-size body) inline-budget)
                     (body-closed? body (reduce conj #{} params)))
              (let [n (count params)
                    ;; trivial args (local/const) substitute straight in (copy
                    ;; propagation); the rest get a fresh local bound once in a
                    ;; wrapping let, so they evaluate exactly once in source order.
                    res (loop [i 0 env {} binds []]
                          (if (< i n)
                            (let [p (nth params i) a (nth args i)]
                              (if (trivial-arg? a)
                                (recur (inc i) (assoc env p a) binds)
                                (let [f (fresh p)]
                                  (recur (inc i)
                                         (assoc env p {:op :local :name f})
                                         (conj binds [f a])))))
                            [env binds]))
                    env (nth res 0)
                    binds (nth res 1)
                    rbody (subst body env)]
                (mark!)
                (if (= 0 (count binds))
                  rbody
                  {:op :let :bindings binds :body rbody}))
              node))
          node))
      node)))

(defn- inline-node
  "Bottom-up: inline children first, then attempt to inline this node."
  [node ctx]
  (let [op (get node :op)]
    (cond
      (= op :invoke)
      (try-inline (assoc node
                         :fn (inline-node (get node :fn) ctx)
                         :args (mapv (fn [a] (inline-node a ctx)) (get node :args)))
                  ctx)
      (= op :if) (assoc node
                        :test (inline-node (get node :test) ctx)
                        :then (inline-node (get node :then) ctx)
                        :else (inline-node (get node :else) ctx))
      (= op :do) (assoc node
                        :statements (mapv (fn [s] (inline-node s ctx)) (get node :statements))
                        :ret (inline-node (get node :ret) ctx))
      (= op :let) (assoc node
                         :bindings (mapv (fn [b] [(nth b 0) (inline-node (nth b 1) ctx)]) (get node :bindings))
                         :body (inline-node (get node :body) ctx))
      (= op :loop) (assoc node
                          :bindings (mapv (fn [b] [(nth b 0) (inline-node (nth b 1) ctx)]) (get node :bindings))
                          :body (inline-node (get node :body) ctx))
      (= op :recur) (assoc node :args (mapv (fn [a] (inline-node a ctx)) (get node :args)))
      (= op :fn) (assoc node :arities (mapv (fn [a] (assoc a :body (inline-node (get a :body) ctx)))
                                            (get node :arities)))
      (= op :def) (assoc node :init (inline-node (get node :init) ctx))
      (= op :throw) (assoc node :expr (inline-node (get node :expr) ctx))
      (= op :vector) (assoc node :items (mapv (fn [x] (inline-node x ctx)) (get node :items)))
      (= op :set) (assoc node :items (mapv (fn [x] (inline-node x ctx)) (get node :items)))
      (= op :map) (assoc node :pairs (mapv (fn [pr] [(inline-node (nth pr 0) ctx)
                                                     (inline-node (nth pr 1) ctx)])
                                           (get node :pairs)))
      (= op :try) (assoc node
                         :body (inline-node (get node :body) ctx)
                         :catch-body (when (get node :catch-body) (inline-node (get node :catch-body) ctx))
                         :finally (when (get node :finally) (inline-node (get node :finally) ctx)))
      :else node)))

;; ---------------------------------------------------------------------------
;; flatten-lets: (let [a (let [b X] Y) ..] body) -> (let [b X a Y ..] body).
;; Safe because inlined bodies are alpha-renamed (every binder unique), so the
;; hoisted bindings can't collide. Exposes a map-returning init directly to
;; scalar-replace when it was wrapped in an inlined arg's let.
;; ---------------------------------------------------------------------------
(defn- flatten-let-bindings [binds]
  ;; returns a flattened binding vector; sets dirty when it hoists.
  (reduce (fn [out b]
            (let [nm (nth b 0) init (nth b 1)]
              (if (= :let (get init :op))
                (do (mark!)
                    (conj (reduce conj out (get init :bindings))
                          [nm (get init :body)]))
                (conj out b))))
          []
          binds))

(defn- flatten-lets [node]
  (let [op (get node :op)]
    (cond
      (= op :let) (assoc node
                         :bindings (flatten-let-bindings
                                    (mapv (fn [b] [(nth b 0) (flatten-lets (nth b 1))]) (get node :bindings)))
                         :body (flatten-lets (get node :body)))
      (= op :if) (assoc node
                        :test (flatten-lets (get node :test))
                        :then (flatten-lets (get node :then))
                        :else (flatten-lets (get node :else)))
      (= op :do) (assoc node
                        :statements (mapv flatten-lets (get node :statements))
                        :ret (flatten-lets (get node :ret)))
      (= op :throw) (assoc node :expr (flatten-lets (get node :expr)))
      (= op :invoke) (assoc node
                            :fn (flatten-lets (get node :fn))
                            :args (mapv flatten-lets (get node :args)))
      (= op :vector) (assoc node :items (mapv flatten-lets (get node :items)))
      (= op :set) (assoc node :items (mapv flatten-lets (get node :items)))
      (= op :map) (assoc node :pairs (mapv (fn [pr] [(flatten-lets (nth pr 0))
                                                     (flatten-lets (nth pr 1))])
                                           (get node :pairs)))
      (= op :loop) (assoc node
                          :bindings (mapv (fn [b] [(nth b 0) (flatten-lets (nth b 1))]) (get node :bindings))
                          :body (flatten-lets (get node :body)))
      (= op :recur) (assoc node :args (mapv flatten-lets (get node :args)))
      (= op :fn) (assoc node :arities (mapv (fn [a] (assoc a :body (flatten-lets (get a :body))))
                                            (get node :arities)))
      (= op :def) (assoc node :init (flatten-lets (get node :init)))
      (= op :try) (assoc node
                         :body (flatten-lets (get node :body))
                         :catch-body (when (get node :catch-body) (flatten-lets (get node :catch-body)))
                         :finally (when (get node :finally) (flatten-lets (get node :finally))))
      :else node)))

;; ---------------------------------------------------------------------------
;; scalar-replace (AOT escape analysis). A map allocation whose ONLY use is
;; constant-keyword lookup is dead weight: replace each (:k m) with the literal
;; value at :k and drop the allocation. Two forms:
;;   (a) direct:    (:k {:k a ..})            -> a
;;   (b) let-bound: (let [m {:k a ..}] .. (:k m) ..) -> .. a ..   (m non-escaping)
;; Both require the dropped sibling values to be pure (we duplicate/discard them).
;; ---------------------------------------------------------------------------

(def ^:private pure-fns
  #{"+" "-" "*" "/" "<" ">" "<=" ">=" "=" "not=" "inc" "dec"
    "mod" "rem" "quot" "min" "max" "abs"
    "nil?" "some?" "not" "get" "zero?" "pos?" "neg?" "even?" "odd?"
    "bit-and" "bit-or" "bit-xor"})

(defn- pure-fn? [f]
  (let [op (get f :op)]
    (cond
      (and (= op :const) (keyword? (get f :val))) true
      (= op :var) (and (= "clojure.core" (get f :ns)) (contains? pure-fns (get f :name)))
      (= op :host) (contains? pure-fns (get f :name))
      :else false)))

(defn- pure?
  "Conservative: true only for expressions with no side effects that are safe to
  duplicate or discard. A var/host ref is a pure read; an invoke is pure only
  for a known-pure fn (arithmetic, comparison, keyword lookup, get)."
  [node]
  (let [op (get node :op)]
    (cond
      (= op :const) true
      (= op :local) true
      (= op :var) true
      (= op :host) true
      (= op :the-var) true
      (= op :quote) true
      (= op :if) (and (pure? (get node :test)) (pure? (get node :then)) (pure? (get node :else)))
      (= op :do) (and (every? pure? (get node :statements)) (pure? (get node :ret)))
      (= op :let) (and (every? (fn [b] (pure? (nth b 1))) (get node :bindings)) (pure? (get node :body)))
      (= op :vector) (every? pure? (get node :items))
      (= op :set) (every? pure? (get node :items))
      (= op :map) (every? (fn [pr] (and (pure? (nth pr 0)) (pure? (nth pr 1)))) (get node :pairs))
      (= op :invoke) (and (pure-fn? (get node :fn)) (every? pure? (get node :args)))
      :else false)))

(defn- scalar-const? [n]
  (and (= :const (get n :op))
       (let [v (get n :val)] (or (keyword? v) (string? v) (number? v) (boolean? v)))))

(defn- const-key-map? [node]
  (let [prs (get node :pairs)]
    (and (> (count prs) 0)
         (every? (fn [pr] (scalar-const? (nth pr 0))) prs))))

(defn- all-vals-pure? [node]
  (every? (fn [pr] (pure? (nth pr 1))) (get node :pairs)))

(defn- map-val
  "The value IR at scalar key k in a const-key map node, or a nil constant when k
  is absent (struct-eligible literal: a missing key reads nil, like the back end)."
  [mapnode k]
  (let [prs (get mapnode :pairs) n (count prs)]
    (loop [i 0]
      (if (< i n)
        (let [pr (nth prs i)]
          (if (= (get (nth pr 0) :val) k) (nth pr 1) (recur (inc i))))
        {:op :const :val nil}))))

(defn- lookup-key
  "If node is a constant-keyword lookup of (:local nm) — either (:k nm) or
  (get nm :k) — return the keyword k; else nil."
  [node nm]
  (if (= :invoke (get node :op))
    (let [f (get node :fn) args (get node :args)]
      (cond
        (and (= :const (get f :op)) (keyword? (get f :val))
             (= 1 (count args))
             (= :local (get (nth args 0) :op)) (= nm (get (nth args 0) :name)))
        (get f :val)

        (and (or (and (= :var (get f :op)) (= "clojure.core" (get f :ns)) (= "get" (get f :name)))
                 (and (= :host (get f :op)) (= "get" (get f :name))))
             (= 2 (count args))
             (= :local (get (nth args 0) :op)) (= nm (get (nth args 0) :name))
             (scalar-const? (nth args 1)))
        (get (nth args 1) :val)

        :else nil))
    nil))

(defn- any-binding-named? [binds nm]
  (loop [i 0]
    (if (< i (count binds))
      (if (= nm (nth (nth binds i) 0)) true (recur (inc i)))
      false)))

(defn- any-name? [names nm]
  (loop [i 0]
    (if (< i (count names))
      (if (= nm (nth names i)) true (recur (inc i)))
      false)))

(defn- local-escapes?
  "Does local nm escape in node — i.e. is it used anywhere other than as the
  subject of a constant-keyword lookup? Precise over straight-line expression
  ops; conservatively true for loop/fn/try/recur/def (and any rebinding of nm),
  so scalar replacement only fires where the whole use region is simple."
  [node nm]
  (let [op (get node :op)
        k (lookup-key node nm)]
    (cond
      ;; an ok lookup of nm: nm itself is consumed; still scan any extra args
      ;; (a get default could reference nm), never the subject local at arg 0.
      k (let [args (get node :args)]
          (if (> (count args) 1)
            (loop [i 1]
              (if (< i (count args))
                (if (local-escapes? (nth args i) nm) true (recur (inc i)))
                false))
            false))
      (= op :local) (= nm (get node :name))
      (= op :const) false
      (= op :var) false
      (= op :host) false
      (= op :the-var) false
      (= op :quote) false
      (= op :if) (or (local-escapes? (get node :test) nm)
                     (local-escapes? (get node :then) nm)
                     (local-escapes? (get node :else) nm))
      (= op :do) (or (loop [i 0 ss (get node :statements)]
                       (if (< i (count ss))
                         (if (local-escapes? (nth ss i) nm) true (recur (inc i) ss))
                         false))
                     (local-escapes? (get node :ret) nm))
      (= op :throw) (local-escapes? (get node :expr) nm)
      (= op :invoke) (or (local-escapes? (get node :fn) nm)
                         (loop [i 0 as (get node :args)]
                           (if (< i (count as))
                             (if (local-escapes? (nth as i) nm) true (recur (inc i) as))
                             false)))
      (= op :vector) (loop [i 0 xs (get node :items)]
                       (if (< i (count xs))
                         (if (local-escapes? (nth xs i) nm) true (recur (inc i) xs))
                         false))
      (= op :set) (loop [i 0 xs (get node :items)]
                    (if (< i (count xs))
                      (if (local-escapes? (nth xs i) nm) true (recur (inc i) xs))
                      false))
      (= op :map) (loop [i 0 ps (get node :pairs)]
                    (if (< i (count ps))
                      (if (or (local-escapes? (nth (nth ps i) 0) nm)
                              (local-escapes? (nth (nth ps i) 1) nm))
                        true (recur (inc i) ps))
                      false))
      (= op :let) (let [binds (get node :bindings)]
                    (if (any-binding-named? binds nm)
                      true ;; nm rebound here — bail (safe; inlined names are unique)
                      (or (loop [i 0]
                            (if (< i (count binds))
                              (if (local-escapes? (nth (nth binds i) 1) nm) true (recur (inc i)))
                              false))
                          (local-escapes? (get node :body) nm))))
      ;; recur binds nothing — its args are ordinary expressions (this is the
      ;; common loop-body tail; treating it as a blanket escape would block
      ;; scalar replacement in every loop).
      (= op :recur) (loop [i 0 as (get node :args)]
                      (if (< i (count as))
                        (if (local-escapes? (nth as i) nm) true (recur (inc i) as))
                        false))
      (= op :loop) (let [binds (get node :bindings)]
                     (if (any-binding-named? binds nm)
                       true
                       (or (loop [i 0]
                             (if (< i (count binds))
                               (if (local-escapes? (nth (nth binds i) 1) nm) true (recur (inc i)))
                               false))
                           (local-escapes? (get node :body) nm))))
      (= op :fn) (loop [i 0 ars (get node :arities)]
                   (if (< i (count ars))
                     (let [ar (nth ars i)
                           ps (get ar :params)]
                       ;; a param (or rest) shadowing nm hides ours in that arity
                       (if (or (any-name? ps nm) (= nm (get ar :rest)))
                         true
                         (if (local-escapes? (get ar :body) nm) true (recur (inc i) ars))))
                     false))
      (= op :try) (or (local-escapes? (get node :body) nm)
                      (let [cb (get node :catch-body)]
                        (and cb (not (= nm (get node :catch-sym))) (local-escapes? cb nm)))
                      (let [f (get node :finally)] (and f (local-escapes? f nm))))
      (= op :def) (local-escapes? (get node :init) nm)
      :else true)))

(defn- subst-lookup
  "Replace every (:k nm)/(get nm :k) in node with the map value at k. The caller
  guarantees (via local-escapes?) that nm is never rebound here and appears only
  as a lookup subject, so no shadowing logic is needed."
  [node nm mapnode]
  (let [op (get node :op)
        k (lookup-key node nm)]
    (cond
      k (map-val mapnode k)
      (= op :if) (assoc node
                        :test (subst-lookup (get node :test) nm mapnode)
                        :then (subst-lookup (get node :then) nm mapnode)
                        :else (subst-lookup (get node :else) nm mapnode))
      (= op :do) (assoc node
                        :statements (mapv (fn [s] (subst-lookup s nm mapnode)) (get node :statements))
                        :ret (subst-lookup (get node :ret) nm mapnode))
      (= op :throw) (assoc node :expr (subst-lookup (get node :expr) nm mapnode))
      (= op :invoke) (assoc node
                            :fn (subst-lookup (get node :fn) nm mapnode)
                            :args (mapv (fn [a] (subst-lookup a nm mapnode)) (get node :args)))
      (= op :vector) (assoc node :items (mapv (fn [x] (subst-lookup x nm mapnode)) (get node :items)))
      (= op :set) (assoc node :items (mapv (fn [x] (subst-lookup x nm mapnode)) (get node :items)))
      (= op :map) (assoc node :pairs (mapv (fn [pr] [(subst-lookup (nth pr 0) nm mapnode)
                                                     (subst-lookup (nth pr 1) nm mapnode)])
                                           (get node :pairs)))
      (= op :let) (assoc node
                         :bindings (mapv (fn [b] [(nth b 0) (subst-lookup (nth b 1) nm mapnode)]) (get node :bindings))
                         :body (subst-lookup (get node :body) nm mapnode))
      ;; the caller's escape check guarantees nm is not rebound in these, so we
      ;; recurse uniformly — leaving any lookup of nm un-substituted would dangle.
      (= op :recur) (assoc node :args (mapv (fn [a] (subst-lookup a nm mapnode)) (get node :args)))
      (= op :loop) (assoc node
                          :bindings (mapv (fn [b] [(nth b 0) (subst-lookup (nth b 1) nm mapnode)]) (get node :bindings))
                          :body (subst-lookup (get node :body) nm mapnode))
      (= op :fn) (assoc node :arities (mapv (fn [a] (assoc a :body (subst-lookup (get a :body) nm mapnode)))
                                            (get node :arities)))
      (= op :try) (assoc node
                         :body (subst-lookup (get node :body) nm mapnode)
                         :catch-body (when (get node :catch-body) (subst-lookup (get node :catch-body) nm mapnode))
                         :finally (when (get node :finally) (subst-lookup (get node :finally) nm mapnode)))
      :else node)))

(defn- fold-kw-literal
  "(a) (:k {:k a ..}) -> a (siblings pure)."
  [node]
  (let [f (get node :fn) args (get node :args)]
    (if (and (= :const (get f :op)) (keyword? (get f :val)) (= 1 (count args)))
      (let [m (nth args 0)]
        (if (and (= :map (get m :op)) (const-key-map? m) (all-vals-pure? m))
          (do (mark!) (map-val m (get f :val)))
          node))
      node)))

(defn- elim-let-maps
  "(b) Drop the first non-escaping let binding whose init is a pure const-key map
  literal, substituting its field lookups into the remaining bindings and body.
  Fixpoint re-runs us for the rest, so one elimination per call keeps it simple."
  [node]
  (let [binds (get node :bindings) n (count binds) body (get node :body)]
    (loop [i 0]
      (if (< i n)
        (let [b (nth binds i) nm (nth b 0) init (nth b 1)]
          (if (and (= :map (get init :op)) (const-key-map? init) (all-vals-pure? init)
                   (not (any-binding-named? (subvec binds (inc i) n) nm))
                   (not (loop [j (inc i)]
                          (if (< j n)
                            (if (local-escapes? (nth (nth binds j) 1) nm) true (recur (inc j)))
                            false)))
                   (not (local-escapes? body nm)))
            (let [head (subvec binds 0 i)
                  tail (mapv (fn [bb] [(nth bb 0) (subst-lookup (nth bb 1) nm init)])
                             (subvec binds (inc i) n))
                  newbinds (reduce conj head tail)
                  newbody (subst-lookup body nm init)]
              (mark!)
              (if (= 0 (count newbinds))
                newbody
                (assoc node :bindings newbinds :body newbody)))
            (recur (inc i))))
        node))))

(defn- scalar-replace
  "Bottom-up: scalar-replace children, then apply (a) at invokes / (b) at lets."
  [node]
  (let [op (get node :op)]
    (cond
      (= op :invoke)
      (fold-kw-literal (assoc node
                              :fn (scalar-replace (get node :fn))
                              :args (mapv scalar-replace (get node :args))))
      (= op :let)
      (elim-let-maps (assoc node
                            :bindings (mapv (fn [b] [(nth b 0) (scalar-replace (nth b 1))]) (get node :bindings))
                            :body (scalar-replace (get node :body))))
      (= op :if) (assoc node
                        :test (scalar-replace (get node :test))
                        :then (scalar-replace (get node :then))
                        :else (scalar-replace (get node :else)))
      (= op :do) (assoc node
                        :statements (mapv scalar-replace (get node :statements))
                        :ret (scalar-replace (get node :ret)))
      (= op :throw) (assoc node :expr (scalar-replace (get node :expr)))
      (= op :vector) (assoc node :items (mapv scalar-replace (get node :items)))
      (= op :set) (assoc node :items (mapv scalar-replace (get node :items)))
      (= op :map) (assoc node :pairs (mapv (fn [pr] [(scalar-replace (nth pr 0))
                                                     (scalar-replace (nth pr 1))])
                                           (get node :pairs)))
      (= op :loop) (assoc node
                          :bindings (mapv (fn [b] [(nth b 0) (scalar-replace (nth b 1))]) (get node :bindings))
                          :body (scalar-replace (get node :body)))
      (= op :recur) (assoc node :args (mapv scalar-replace (get node :args)))
      (= op :fn) (assoc node :arities (mapv (fn [a] (assoc a :body (scalar-replace (get a :body))))
                                            (get node :arities)))
      (= op :def) (assoc node :init (scalar-replace (get node :init)))
      (= op :try) (assoc node
                         :body (scalar-replace (get node :body))
                         :catch-body (when (get node :catch-body) (scalar-replace (get node :catch-body)))
                         :finally (when (get node :finally) (scalar-replace (get node :finally))))
      :else node)))

;; ---------------------------------------------------------------------------
;; Collection-type inference (jolt-99x), Phase 0: intra-procedural. A forward,
;; soft-typing-style pass (simplified HM: monovariant, never-fails, lattice top
;; = :any) that types expressions from literals/arithmetic and flows the type
;; through let bindings and if-joins. Where a keyword-lookup subject is PROVEN a
;; plain struct map it sets :hint :struct (the same channel a manual hint uses,
;; so the back end drops the guard); where the type is :any it leaves the
;; dynamic guard in place. Sound by construction: a concrete type is assigned
;; only when proven, so a wrong bare get is impossible.
;;
;; Lattice values: :struct-map (raw-get-safe), :phm-map, :set, :truthy (a
;; provably non-nil/non-false scalar), :any (top), and a PARAMETRIC vector type
;; {:vec ELEM} (jolt-d6u, Phase 3) carrying its element type so a reduce/map
;; closure over it can type its element param. {:vec ELEM} is a small struct, so
;; it compares by value on both the Clojure and the Janet (orchestrator) side.
(defn- velem [t] (get t :vec))
(defn- vec-type? [t] (some? (velem t)))
(defn- mk-vec [t] {:vec (if t t :any)})
(defn- join [a b]
  (cond
    (= a b) a
    (and (vec-type? a) (vec-type? b)) (mk-vec (join (velem a) (velem b)))
    :else :any))
(defn- struct-safe? [t] (= t :struct-map))
;; a value whose type guarantees it is neither nil nor false — the back end only
;; builds a struct (vs a phm) when every value is truthy, so a map literal is a
;; struct only when all its values have a truthy type. Collections are non-nil.
(defn- truthy-type? [t]
  (or (= t :truthy) (= t :struct-map) (= t :phm-map) (= t :set) (vec-type? t)))

(def ^:private truthy-ret-fns
  #{"+" "-" "*" "/" "inc" "dec" "mod" "rem" "quot" "min" "max" "abs"
    "bit-and" "bit-or" "bit-xor" "count"})
(def ^:private vector-ret-fns #{"vec" "vector" "mapv" "filterv" "subvec"})

;; Inter-procedural state (jolt-767, Phase 1). The Janet orchestrator (backend
;; infer-unit!) drives a whole-unit fixpoint: before typing a fn body it installs
;; the current return-type estimates of all unit fns here, and after typing it
;; reads back the call sites this body made (callee + inferred arg types) to
;; propagate into callee param types. Both are plain module state, like `dirty`.
(def ^:private rtenv-box (atom {}))   ;; "ns/name" -> inferred return type
(def ^:private calls-box (atom []))   ;; collected [ "ns/name" [arg-types...] ]
(def ^:private escapes-box (atom #{})) ;; var-keys used as a VALUE (not a call head)
;; jolt-d6u: a var reference's VALUE type — a fn var is :truthy (non-nil), a def
;; var carries its inferred init type (e.g. a color table -> {:vec :struct-map}).
;; The orchestrator populates this from sealed (opt-mode) cell roots + def inits.
(def ^:private vtype-box (atom {}))   ;; "ns/name" -> value type

;; fns that RETURN an element of their (first) collection arg, so a lookup on the
;; result of (rand-nth coll-of-structs) etc. types as the element.
(def ^:private elem-fns #{"rand-nth" "first" "peek" "last" "nth" "fnext" "second"})

(defn- var-key [fnode] (str (get fnode :ns) "/" (get fnode :name)))

(defn- call-ret-type [fnode]
  (let [op (get fnode :op)]
    (cond
      ;; a user fn whose return type the fixpoint has estimated
      (= op :var) (let [r (get @rtenv-box (var-key fnode))]
                    (if r r (let [nm (and (= "clojure.core" (get fnode :ns)) (get fnode :name))]
                              (cond (nil? nm) :any
                                    (contains? truthy-ret-fns nm) :truthy
                                    (contains? vector-ret-fns nm) (mk-vec :any)
                                    :else :any))))
      (= op :host) (let [nm (get fnode :name)]
                     (cond (contains? truthy-ret-fns nm) :truthy
                           (contains? vector-ret-fns nm) (mk-vec :any)
                           :else :any))
      :else :any)))

(declare infer)

;; HOFs that apply their fn arg to the ELEMENTS of a collection (jolt-d6u,
;; Phase 3). :epos is which param of the fn receives an element. reduce is
;; handled separately (its arity changes the coll position, and its closure
;; also takes an accumulator).
(def ^:private hof-table
  {"map" {:epos 0} "mapv" {:epos 0} "filter" {:epos 0} "filterv" {:epos 0}
   "keep" {:epos 0} "remove" {:epos 0} "run!" {:epos 0} "mapcat" {:epos 0}})

(defn- infer-fn-seeded
  "Infer a fn-literal passed to a HOF, seeding the given params to element/accum
  types (seeds: param-index -> type), other params :any, captured locals from
  tenv. Returns [ret-type node'] — ret is the lub of arity tail types, used to
  type the HOF result (e.g. reduce's accumulator, mapv's element)."
  [node seeds tenv]
  (let [res (mapv (fn [a]
                    (let [params (get a :params)
                          pe (reduce (fn [e i]
                                       (assoc e (nth params i)
                                              (let [s (get seeds i)] (if s s :any))))
                                     tenv (range (count params)))
                          pe (if (get a :rest) (assoc pe (get a :rest) :any) pe)
                          br (infer (get a :body) pe)]
                      [(nth br 0) (assoc a :body (nth br 1))]))
                  (get node :arities))
        rets (mapv (fn [r] (nth r 0)) res)
        ret (if (empty? rets) :any (reduce join (first rets) (rest rets)))]
    [ret (assoc node :arities (mapv (fn [r] (nth r 1)) res))]))

(defn- infer
  "Returns [type node'] — the inferred type of node and node with struct-safe
  :local references annotated :hint :struct. tenv maps in-scope local names to
  inferred types."
  [node tenv]
  (let [op (get node :op)]
    (cond
      (= op :const)
      [(let [v (get node :val)] (if (or (nil? v) (= false v)) :any :truthy)) node]
      (= op :local)
      (let [t (get tenv (get node :name))]
        [(if t t :any)
         (cond
           (struct-safe? t) (assoc node :hint :struct)
           (vec-type? t) (assoc node :hint :vector)
           :else node)])
      (= op :map)
      (let [res (mapv (fn [pr]
                        (let [kr (infer (nth pr 0) tenv)
                              vr (infer (nth pr 1) tenv)]
                          [(nth kr 1) (nth vr 1) (nth vr 0)]))
                      (get node :pairs))
            t (if (and (> (count res) 0)
                       (every? (fn [pr] (scalar-const? (nth pr 0))) (get node :pairs))
                       (every? (fn [r] (truthy-type? (nth r 2))) res))
                :struct-map :any)]
        [t (assoc node :pairs (mapv (fn [r] [(nth r 0) (nth r 1)]) res))])
      (= op :vector)
      (let [irs (mapv (fn [x] (infer x tenv)) (get node :items))
            ets (mapv (fn [r] (nth r 0)) irs)
            el (if (empty? ets) :any (reduce join (first ets) (rest ets)))]
        [(mk-vec el) (assoc node :items (mapv (fn [r] (nth r 1)) irs))])
      (= op :set)
      [:set (assoc node :items (mapv (fn [x] (nth (infer x tenv) 1)) (get node :items)))]
      (= op :if)
      (let [tr (infer (get node :test) tenv)
            thn (infer (get node :then) tenv)
            els (infer (get node :else) tenv)]
        [(join (nth thn 0) (nth els 0))
         (assoc node :test (nth tr 1) :then (nth thn 1) :else (nth els 1))])
      (= op :do)
      (let [stmts (mapv (fn [s] (nth (infer s tenv) 1)) (get node :statements))
            r (infer (get node :ret) tenv)]
        [(nth r 0) (assoc node :statements stmts :ret (nth r 1))])
      (= op :throw)
      [:any (assoc node :expr (nth (infer (get node :expr) tenv) 1))]
      ;; a :var reached HERE is in value position (an arg, a let init, ...), not
      ;; a call head — so the fn it names escapes and its params can't be inferred.
      ;; Its VALUE type comes from vtype-box (a fn is :truthy, a def carries its
      ;; inferred type); unknown -> :any.
      (= op :var) (do (swap! escapes-box conj (var-key node))
                      [(let [vt (get @vtype-box (var-key node))] (if vt vt :any)) node])
      (= op :invoke)
      (let [fnode (get node :fn)
            iscall-var (= :var (get fnode :op))
            cn (when (and iscall-var (= "clojure.core" (get fnode :ns))) (get fnode :name))
            args (get node :args)
            n (count args)]
        (cond
          ;; reduce over a typed vector with a fn-literal (jolt-d6u): seed the
          ;; closure's accumulator (param 0) to the init type and its element
          ;; (param 1) to the vector's element type, so its body — and any calls
          ;; it makes — see those types.
          (and (= cn "reduce") (>= n 2) (= :fn (get (nth args 0) :op)))
          (let [three (>= n 3)
                coll-r (infer (nth args (if three 2 1)) tenv)
                init-r (when three (infer (nth args 1) tenv))
                et (let [ct (nth coll-r 0)] (if (vec-type? ct) (velem ct) :any))
                init-t (if init-r (nth init-r 0) :any)
                fn-r (infer-fn-seeded (nth args 0) {0 init-t 1 et} tenv)]
            [(join init-t (nth fn-r 0))
             (assoc node :args (if three
                                 [(nth fn-r 1) (nth init-r 1) (nth coll-r 1)]
                                 [(nth fn-r 1) (nth coll-r 1)]))])
          ;; map/mapv/filter/... over a typed vector with a fn-literal: seed the
          ;; fn's element param; mapv/filterv produce a typed vector.
          (and cn (get hof-table cn) (>= n 2) (= :fn (get (nth args 0) :op)))
          (let [coll-r (infer (nth args 1) tenv)
                et (let [ct (nth coll-r 0)] (if (vec-type? ct) (velem ct) :any))
                fn-r (infer-fn-seeded (nth args 0) {(get (get hof-table cn) :epos) et} tenv)
                rt (cond (= cn "mapv") (mk-vec (nth fn-r 0))
                         (= cn "filterv") (mk-vec et)
                         :else :any)]
            [rt (assoc node :args [(nth fn-r 1) (nth coll-r 1)])])
          ;; conj/into: track the element type of a vector being grown.
          (and (or (= cn "conj") (= cn "into")) (>= n 1))
          (let [ares (mapv (fn [a] (infer a tenv)) args)
                base (nth (nth ares 0) 0)
                rest-ts (mapv (fn [r] (nth r 0)) (rest ares))
                rt (cond
                     (and (= cn "conj") (vec-type? base))
                     (mk-vec (reduce join (velem base) rest-ts))
                     (and (= cn "into") (vec-type? base) (= 2 n) (vec-type? (nth rest-ts 0)))
                     (mk-vec (join (velem base) (velem (nth rest-ts 0))))
                     :else (call-ret-type fnode))]
            [rt (assoc node :args (mapv (fn [r] (nth r 1)) ares))])
          ;; everything else: type args, collect the call (var callee), use the
          ;; declared/estimated return type. range produces a numeric vector.
          :else
          (let [fnode' (if iscall-var fnode (nth (infer fnode tenv) 1))
                ares (mapv (fn [a] (infer a tenv)) args)]
            (when iscall-var
              (swap! calls-box conj [(var-key fnode) (mapv (fn [r] (nth r 0)) ares)]))
            [(cond
               (= cn "range") (mk-vec :truthy)
               ;; element-returning fn over a typed vector -> the element type
               (and cn (contains? elem-fns cn) (> n 0))
               (let [a0 (nth (nth ares 0) 0)] (if (vec-type? a0) (velem a0) :any))
               :else (call-ret-type fnode))
             (assoc node :fn fnode' :args (mapv (fn [r] (nth r 1)) ares))])))
      (= op :let)
      (let [res (reduce (fn [acc b]
                          (let [te (nth acc 0) binds (nth acc 1)
                                ir (infer (nth b 1) te)]
                            [(assoc te (nth b 0) (nth ir 0)) (conj binds [(nth b 0) (nth ir 1)])]))
                        [tenv []] (get node :bindings))
            br (infer (get node :body) (nth res 0))]
        [(nth br 0) (assoc node :bindings (nth res 1) :body (nth br 1))])
      (= op :loop)
      ;; conservative + sound: loop bindings join across recur, which we don't
      ;; track in Phase 0, so they stay :any. Still descend to annotate any
      ;; known-type lookups inside the body.
      [:any (assoc node
                   :bindings (mapv (fn [b] [(nth b 0) (nth (infer (nth b 1) tenv) 1)]) (get node :bindings))
                   :body (nth (infer (get node :body) tenv) 1))]
      (= op :recur)
      [:any (assoc node :args (mapv (fn [a] (nth (infer a tenv) 1)) (get node :args)))]
      (= op :fn)
      ;; a closure inherits the enclosing tenv so CAPTURED locals keep their
      ;; types (e.g. a reduce closure that calls (f captured-struct ...)); its own
      ;; params/rest shadow to :any (unknown until Phase 1 types them via callers).
      [:any (assoc node :arities
                   (mapv (fn [a]
                           (let [pe (reduce (fn [e p] (assoc e p :any)) tenv (get a :params))
                                 pe (if (get a :rest) (assoc pe (get a :rest) :any) pe)]
                             (assoc a :body (nth (infer (get a :body) pe) 1))))
                         (get node :arities)))]
      (= op :def)
      [:any (assoc node :init (nth (infer (get node :init) tenv) 1))]
      (= op :try)
      [:any (assoc node
                   :body (nth (infer (get node :body) tenv) 1)
                   :catch-body (when (get node :catch-body) (nth (infer (get node :catch-body) tenv) 1))
                   :finally (when (get node :finally) (nth (infer (get node :finally) tenv) 1)))]
      :else [:any node])))

(defn- infer-top [node] (nth (infer node {}) 1))

;; --- Inter-procedural driver API (jolt-767) consumed by the back end --------
(defn set-rtenv!
  "Install the current return-type estimates (a map \"ns/name\" -> type) used to
  type call results during the fixpoint."
  [m] (reset! rtenv-box m))

(defn set-vtypes!
  "Install var VALUE types (a map \"ns/name\" -> type): fn vars are :truthy
  (non-nil), def vars carry their inferred init type (jolt-d6u)."
  [m] (reset! vtype-box m))

(defn reset-escapes! [] (reset! escapes-box #{}))
(defn collected-escapes [] (vec @escapes-box))

(defn infer-body
  "Type `body` under tenv (local-name -> type). Returns [ret-type node' calls],
  where calls is the [[\"ns/name\" [arg-types...]] ...] this body invokes (for
  propagating into callee param types). Also accumulates escapes (read with
  collected-escapes after a full sweep)."
  [body tenv]
  (reset! calls-box [])
  (let [r (infer body tenv)]
    [(nth r 0) (nth r 1) @calls-box]))

(defn reinfer-def
  "Re-run inference on a stashed :def's fn arity bodies with param types seeded
  (ptmap: param-name -> type), returning the def with annotated bodies. The back
  end emits the result directly (no further passes), so the param-typed lookups
  keep their specialization. Used by the inter-procedural recompile."
  [def-node ptmap]
  (let [fnode (get def-node :init)]
    (if (= :fn (get fnode :op))
      (assoc def-node :init
             (assoc fnode :arities
                    (mapv (fn [a] (assoc a :body (nth (infer (get a :body) ptmap) 1)))
                          (get fnode :arities))))
      def-node)))

(defn run-passes
  "All passes, in order. The back end applies this to every analyzed form. When
  inlining is enabled for the unit (user code under direct-linking, jolt-87f),
  run inline + flatten + scalar-replace + const-fold to a capped fixpoint —
  inlining exposes map literals to lookups, scalar-replace collapses them, which
  may expose more — then a collection-type inference pass (jolt-99x) that
  auto-drops the lookup guard where the type is proven. Otherwise (core +
  bootstrap) just const-fold, as before."
  [node ctx]
  (if (inline-enabled? ctx)
    (let [opt (loop [i 0 n (const-fold node)]
                (reset! dirty false)
                (let [n2 (const-fold (scalar-replace (flatten-lets (inline-node n ctx))))]
                  (if (and @dirty (< i 8))
                    (recur (inc i) n2)
                    n2)))]
      (infer-top opt))
    (const-fold node)))
