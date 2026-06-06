(ns jolt.analyzer
  "Portable Clojure analyzer: reader form -> host-neutral IR (see jolt.ir).

  Pure jolt-core — depends only on the host contract (jolt.host) for form
  introspection and symbol/macro resolution, never on Janet. ctx is an opaque
  host handle threaded to the contract fns; the analyzer never inspects it.

  Coverage grows toward compiler.janet; unsupported forms throw :jolt/uncompilable
  so the caller falls back to the interpreter (the hybrid contract).

  `env` carries lexical state: {:locals #{names} :recur recur-target-name|nil}."
  (:require [jolt.ir :as ir]
            [jolt.host :as h]))

(declare analyze analyze-fn analyze-try)

;; Special forms the analyzer compiles itself. Anything else h/special? returns
;; true for is left to the interpreter via uncompilable.
(def ^:private handled
  #{"quote" "if" "do" "def" "fn*" "let*" "loop*" "recur" "throw" "try"})

(defn- uncompilable [why]
  (throw (str "jolt/uncompilable: " why)))

;; Fresh recur-target names. A plain counter (analyzer is single-threaded during
;; a compile); the leading "_r$" can't appear in source so it never collides.
(def ^:private gensym-counter (atom 0))
(defn- gen-name [prefix]
  (let [n @gensym-counter]
    (swap! gensym-counter inc)
    (str "_r$" prefix n)))

(defn- empty-env [] {:locals #{} :recur nil})
(defn- locals [env] (:locals env))
(defn- local? [env nm] (contains? (:locals env) nm))
(defn- add-locals [env names] (update env :locals #(reduce conj % names)))
(defn- with-recur [env name] (assoc env :recur name))

(defn- analyze-seq
  "Analyze a body of forms into IR statements+ret (a :do, or the single node)."
  [ctx forms env]
  (let [v (mapv #(analyze ctx % env) forms)
        n (count v)]
    (cond
      (zero? n) (ir/const nil)
      (= 1 n) (first v)
      :else (ir/do-node (subvec v 0 (dec n)) (peek v)))))

(defn- analyze-bindings
  "let*/loop* binding vector -> [pairs env'] where pairs is [[name init-ir]...]
  and env' has the bound names in scope (each init sees the prior bindings)."
  [ctx bvec env]
  (loop [i 0 env env pairs []]
    (if (< i (count bvec))
      (let [bsym (nth bvec i)]
        (when-not (h/sym? bsym) (uncompilable "destructuring binding"))
        (let [nm (h/sym-name bsym)
              init (analyze ctx (nth bvec (inc i)) env)]
          (recur (+ i 2) (add-locals env [nm]) (conj pairs [nm init]))))
      [pairs env])))

(defn- analyze-special [ctx op items env]
  (case op
    "quote" (ir/quote-node (second items))
    "if" (ir/if-node (analyze ctx (nth items 1) env)
                     (analyze ctx (nth items 2) env)
                     (if (> (count items) 3)
                       (analyze ctx (nth items 3) env)
                       (ir/const nil)))
    "do" (analyze-seq ctx (rest items) env)
    "throw" (ir/throw-node (analyze ctx (nth items 1) env))
    "def" (let [name-sym (nth items 1)
                nm (h/sym-name name-sym)
                cur (h/current-ns ctx)]
            (h/intern! ctx cur nm)
            (ir/def-node cur nm (analyze ctx (nth items 2) env)))
    "let*" (let [bvec (vec (h/vector-items (nth items 1)))
                 [pairs env*] (analyze-bindings ctx bvec env)]
             (ir/let-node pairs (analyze-seq ctx (drop 2 items) env*)))
    "loop*" (let [bvec (vec (h/vector-items (nth items 1)))
                  rname (gen-name "loop")
                  [pairs env*] (analyze-bindings ctx bvec env)
                  env** (with-recur env* rname)]
              {:op :loop :recur-name rname :bindings pairs
               :body (analyze-seq ctx (drop 2 items) env**)})
    "recur" (let [rt (:recur env)]
              (when-not rt (uncompilable "recur outside loop/fn"))
              {:op :recur :recur-name rt
               :args (mapv #(analyze ctx % env) (rest items))})
    "try" (analyze-try ctx items env)
    "fn*" (analyze-fn ctx items env)
    (uncompilable (str "special form " op))))

(defn- analyze-try [ctx items env]
  ;; (try body... (catch Class e handler...) (finally cleanup...))
  (let [clauses (rest items)
        body (atom [])
        catch-sym (atom nil)
        catch-body (atom nil)
        finally-body (atom nil)]
    (doseq [c clauses]
      (let [head (when (h/list? c) (first (vec (h/elements c))))
            hname (when (and head (h/sym? head)) (h/sym-name head))]
        (cond
          (= hname "catch")
            (let [cl (vec (h/elements c))]
              (reset! catch-sym (h/sym-name (nth cl 2)))
              (reset! catch-body (drop 3 cl)))
          (= hname "finally")
            (reset! finally-body (rest (vec (h/elements c))))
          :else (swap! body conj c))))
    {:op :try
     :body (analyze-seq ctx @body env)
     :catch-sym @catch-sym
     :catch-body (when @catch-body
                   (analyze-seq ctx @catch-body (add-locals env [@catch-sym])))
     :finally (when @finally-body (analyze-seq ctx @finally-body env))}))

(defn- parse-params [pvec]
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

(defn- analyze-arity [ctx pvec body env fn-name]
  (let [{:keys [fixed rest]} (parse-params (vec (h/vector-items pvec)))
        ;; recur into a variadic arity would re-wrap the rest seq under Janet's &,
        ;; so only fixed arities are recur targets; recur in a variadic arity then
        ;; hits a nil target -> uncompilable -> the whole fn interprets.
        rname (when-not rest (gen-name "arity"))
        names (cond-> (vec fixed) rest (conj rest) fn-name (conj fn-name))
        env* (-> (add-locals env names) (with-recur rname))]
    {:params fixed :rest rest :recur-name rname
     :body (analyze-seq ctx body env*)}))

(defn- analyze-fn [ctx items env]
  (let [named (h/sym? (nth items 1))
        fn-name (when named (h/sym-name (nth items 1)))
        rest-items (if named (drop 2 items) (drop 1 items))
        first* (first rest-items)]
    (cond
      (h/vector? first*)
        (ir/fn-node fn-name [(analyze-arity ctx first* (rest rest-items) env fn-name)])
      (h/list? first*)
        (ir/fn-node fn-name
                    (mapv (fn [clause]
                            (let [cl (vec (h/elements clause))]
                              (analyze-arity ctx (first cl) (rest cl) env fn-name)))
                          rest-items))
      :else (uncompilable "fn: bad params"))))

(defn- analyze-symbol [ctx form env]
  (let [nm (h/sym-name form) ns (h/sym-ns form)]
    (cond
      (and (nil? ns) (local? env nm)) (ir/local nm)
      ns (let [r (h/resolve-global ctx form)]
           (if (= :var (:kind r))
             (ir/var-ref (:ns r) (:name r))
             (uncompilable (str "qualified ref " ns "/" nm))))
      :else (let [r (h/resolve-global ctx form)]
              (case (:kind r)
                :var (ir/var-ref (:ns r) (:name r))
                :host (ir/host-ref (:name r))
                (ir/var-ref (h/current-ns ctx) nm))))))

(defn- analyze-list [ctx form env]
  (let [items (vec (h/elements form))]
    (if (zero? (count items))
      (ir/quote-node form)
      (let [head (first items)
            hname (when (and (h/sym? head) (nil? (h/sym-ns head))) (h/sym-name head))
            shadowed (and hname (local? env hname))]
        (cond
          (and hname (not shadowed) (contains? handled hname))
            (analyze-special ctx hname items env)
          (and hname (not shadowed) (h/special? hname))
            (uncompilable (str "special form " hname))
          (and (h/sym? head) (not shadowed) (h/macro? ctx head))
            (analyze ctx (h/expand-1 ctx form) env)
          :else
            (ir/invoke (analyze ctx head env)
                       (mapv #(analyze ctx % env) (rest items))))))))

(defn analyze
  "Analyze form to IR in context ctx. The 2-arg arity starts with an empty env."
  ([ctx form] (analyze ctx form (empty-env)))
  ([ctx form env]
   (cond
     (h/literal? form) (ir/const form)
     (h/sym? form) (analyze-symbol ctx form env)
     (h/vector? form) (ir/vector-node (mapv #(analyze ctx % env) (h/vector-items form)))
     (h/map? form) (ir/map-node (mapv (fn [p] [(analyze ctx (first p) env)
                                               (analyze ctx (second p) env)])
                                      (h/map-pairs form)))
     (h/set? form) (uncompilable "set literal")
     (h/list? form) (analyze-list ctx form env)
     :else (uncompilable "unsupported form"))))
