(ns jolt.passes.fold
  "Constant folding (always-on IR pass) plus the shared const-shape predicate.
  Bottom-up numeric folding + dead-branch removal, total over node :ops (unknown
  ops pass through with folded children). Portable Clojure: kernel-tier fns +
  seed primitives only — it loads with the compiler namespaces, before the later
  core tiers.")

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

;; A const node whose value is a scalar literal (kw/str/num/bool). Shared by the
;; scalar-replace pass (jolt.passes.inline) and the collection-type inference
;; (jolt.passes.types), which both reason about const-keyed maps.
(defn scalar-const? [n]
  (and (= :const (get n :op))
       (let [v (get n :val)] (or (keyword? v) (string? v) (number? v) (boolean? v)))))
