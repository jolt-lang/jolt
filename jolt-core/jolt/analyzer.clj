(ns jolt.analyzer
  "Portable Clojure analyzer: reader form -> host-neutral IR (see jolt.ir).

  Pure jolt-core — depends only on the host contract (jolt.host) for form
  introspection and symbol/macro resolution, never on Janet. ctx is an opaque
  host handle threaded to the contract fns; the analyzer never inspects it.

  Coverage grows toward compiler.janet; unsupported forms throw :jolt/uncompilable
  so the caller falls back to the interpreter (the hybrid contract)."
  (:require [jolt.ir :as ir]
            [jolt.host :as h]))

(declare analyze analyze-fn)

;; Special forms the analyzer compiles itself. Anything else with a special head
;; (ns, deftype, defmacro, …) is left to the interpreter via uncompilable.
(def ^:private handled
  #{"quote" "if" "do" "def" "fn*" "let*" "throw"})

(defn- uncompilable [why]
  (throw (str "jolt/uncompilable: " why)))

(defn- analyze-seq
  "Analyze a body of forms into IR statements+ret (a :do, or the single node)."
  [ctx forms locals]
  (let [v (mapv #(analyze ctx % locals) forms)
        n (count v)]
    (cond
      (zero? n) (ir/const nil)
      (= 1 n) (first v)
      :else (ir/do-node (subvec v 0 (dec n)) (peek v)))))

(defn- analyze-special [ctx op items locals]
  (case op
    "quote" (ir/quote-node (second items))
    "if" (ir/if-node (analyze ctx (nth items 1) locals)
                     (analyze ctx (nth items 2) locals)
                     (if (> (count items) 3)
                       (analyze ctx (nth items 3) locals)
                       (ir/const nil)))
    "do" (analyze-seq ctx (rest items) locals)
    "throw" (ir/throw-node (analyze ctx (nth items 1) locals))
    "def" (let [name-sym (nth items 1)
                nm (h/sym-name name-sym)
                cur (h/current-ns ctx)]
            (h/intern! ctx cur nm)
            (ir/def-node cur nm (analyze ctx (nth items 2) locals)))
    "let*" (let [bvec (vec (h/vector-items (nth items 1)))
                 locals* (atom locals)
                 pairs (loop [i 0 acc []]
                         (if (< i (count bvec))
                           (let [bsym (nth bvec i)
                                 _ (when-not (h/sym? bsym)
                                     (uncompilable "destructuring let binding"))
                                 nm (h/sym-name bsym)
                                 init (analyze ctx (nth bvec (inc i)) @locals*)]
                             (swap! locals* conj nm)
                             (recur (+ i 2) (conj acc [nm init])))
                           acc))]
             (ir/let-node pairs (analyze-seq ctx (drop 2 items) @locals*)))
    "fn*" (analyze-fn ctx items locals)
    (uncompilable (str "special form " op))))

(defn- parse-params [pvec]
  "Plain-symbol params only; & rest. Destructuring -> uncompilable."
  (loop [i 0 fixed [] rest-name nil]
    (if (< i (count pvec))
      (let [p (nth pvec i)]
        (when-not (h/sym? p) (uncompilable "destructuring fn param"))
        (if (= "&" (h/sym-name p))
          (let [r (nth pvec (inc i))]
            (when-not (h/sym? r) (uncompilable "destructuring fn rest"))
            (recur (+ i 2) fixed (h/sym-name r)))
          (recur (inc i) (conj fixed (h/sym-name p)) rest-name)))
      {:fixed fixed :rest rest-name})))

(defn- analyze-arity [ctx pvec body locals fn-name]
  (let [{:keys [fixed rest]} (parse-params (vec (h/vector-items pvec)))
        locals* (cond-> (reduce conj locals fixed)
                  rest (conj rest)
                  fn-name (conj fn-name))]
    {:params fixed :rest rest :body (analyze-seq ctx body locals*)}))

(defn- analyze-fn [ctx items locals]
  ;; (fn* name? params body...) | (fn* name? ([params] body...) ...)
  (let [named (h/sym? (nth items 1))
        fn-name (when named (h/sym-name (nth items 1)))
        rest-items (if named (drop 2 items) (drop 1 items))
        first* (first rest-items)]
    (cond
      (h/vector? first*)
        (ir/fn-node fn-name [(analyze-arity ctx first* (rest rest-items) locals fn-name)])
      (h/list? first*)
        (ir/fn-node fn-name
                    (mapv (fn [clause]
                            (let [cl (vec (h/elements clause))]
                              (analyze-arity ctx (first cl) (rest cl) locals fn-name)))
                          rest-items))
      :else (uncompilable "fn: bad params"))))

(defn- analyze-symbol [ctx form locals]
  (let [nm (h/sym-name form) ns (h/sym-ns form)]
    (cond
      ;; local (only unqualified)
      (and (nil? ns) (contains? locals nm)) (ir/local nm)
      ;; qualified: must resolve to a var, else interpret (handles janet/…,
      ;; Math/…, and any host interop the back end doesn't model).
      ns (let [r (h/resolve-global ctx form)]
           (if (= :var (:kind r))
             (ir/var-ref (:ns r) (:name r))
             (uncompilable (str "qualified ref " ns "/" nm))))
      :else (let [r (h/resolve-global ctx form)]
              (case (:kind r)
                :var (ir/var-ref (:ns r) (:name r))
                :host (ir/host-ref (:name r))
                ;; unresolved: forward reference in the current ns (resolved at call time)
                (ir/var-ref (h/current-ns ctx) nm))))))

(defn- analyze-list [ctx form locals]
  (let [items (vec (h/elements form))]
    (if (zero? (count items))
      (ir/quote-node form)
      (let [head (first items)
            hname (when (and (h/sym? head) (nil? (h/sym-ns head))) (h/sym-name head))
            shadowed (and hname (contains? locals hname))]
        (cond
          (and hname (not shadowed) (contains? handled hname))
            (analyze-special ctx hname items locals)
          ;; A special form the analyzer doesn't compile -> interpreter.
          (and hname (not shadowed) (h/special? hname))
            (uncompilable (str "special form " hname))
          (and (h/sym? head) (not shadowed) (h/macro? ctx head))
            (analyze ctx (h/expand-1 ctx form) locals)
          :else
            (ir/invoke (analyze ctx head locals)
                       (mapv #(analyze ctx % locals) (rest items))))))))

(defn analyze
  "Analyze form to IR in context ctx with the given set of local names in scope."
  ([ctx form] (analyze ctx form #{}))
  ([ctx form locals]
   (cond
     (h/literal? form) (ir/const form)
     (h/sym? form) (analyze-symbol ctx form locals)
     (h/vector? form) (ir/vector-node (mapv #(analyze ctx % locals) (h/vector-items form)))
     (h/map? form) (ir/map-node (mapv (fn [p] [(analyze ctx (first p) locals)
                                               (analyze ctx (second p) locals)])
                                      (h/map-pairs form)))
     (h/set? form) (uncompilable "set literal")
     (h/list? form) (analyze-list ctx form locals)
     ;; Anything else (tagged literals like #"regex"/#inst, unknown shapes) is
     ;; host-specific or not a value the back end can embed — interpret it.
     :else (uncompilable "unsupported form"))))
