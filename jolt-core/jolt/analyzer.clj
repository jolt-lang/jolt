(ns jolt.analyzer
  "Portable Clojure analyzer: reader form -> host-neutral IR (see jolt.ir).

  Pure jolt-core — depends only on the host contract (jolt.host) and IR
  constructors (jolt.ir), never on Janet. The contract fns are referred unqualified
  (host form predicates are `form-*` to avoid colliding with clojure.core), so the
  bootstrap can compile this namespace via its plain :var path. ctx is an opaque
  host handle threaded to the contract fns; the analyzer never inspects it.

  Coverage grows toward compiler.janet; unsupported forms throw :jolt/uncompilable
  so the caller falls back to the interpreter (the hybrid contract).

  `env` carries lexical state: {:locals #{names} :recur recur-target-name|nil}.
  Definitions are ordered so only `analyze` (mutually recursive) is forward
  declared — the bootstrap compiles forward refs through var cells, but keeping
  them to one keeps the compiled namespace simple."
  (:require [jolt.ir :refer [const local var-ref host-ref if-node do-node invoke
                             def-node let-node fn-node vector-node map-node
                             quote-node throw-node]]
            [jolt.host :refer [form-sym? form-sym-name form-sym-ns form-list?
                               form-vec? form-map? form-set? form-char?
                               form-literal? form-elements form-vec-items
                               form-map-pairs form-special? compile-ns
                               form-macro? form-expand-1 resolve-global
                               form-sym-meta host-intern!]]))

(declare analyze)

(def ^:private handled
  #{"quote" "if" "do" "def" "fn*" "let*" "loop*" "recur" "throw" "try"})

(defn- uncompilable [why]
  (throw (str "jolt/uncompilable: " why)))

(def ^:private gensym-counter (atom 0))
(defn- gen-name [prefix]
  (let [n @gensym-counter]
    (swap! gensym-counter inc)
    (str "_r$" prefix n)))

(defn- empty-env [] {:locals #{}})
(defn- local? [env nm] (contains? (:locals env) nm))
(defn- add-locals [env names] (update env :locals #(reduce conj % names)))
(defn- with-recur [env name] (assoc env :recur name))

(defn- analyze-seq [ctx forms env]
  (let [v (mapv #(analyze ctx % env) forms)
        n (count v)]
    (cond
      (zero? n) (const nil)
      (= 1 n) (first v)
      :else (do-node (subvec v 0 (dec n)) (peek v)))))

(defn- analyze-bindings [ctx bvec env]
  (loop [i 0 env env pairs []]
    (if (< i (count bvec))
      (let [bsym (nth bvec i)]
        (when-not (form-sym? bsym) (uncompilable "destructuring binding"))
        (let [nm (form-sym-name bsym)
              init (analyze ctx (nth bvec (inc i)) env)]
          (recur (+ i 2) (add-locals env [nm]) (conj pairs [nm init]))))
      [pairs env])))

(defn- parse-params [pvec]
  (loop [i 0 fixed [] rest-name nil]
    (if (< i (count pvec))
      (let [p (nth pvec i)]
        (when-not (form-sym? p) (uncompilable "destructuring fn param"))
        (if (= "&" (form-sym-name p))
          (let [r (nth pvec (inc i))]
            (when-not (form-sym? r) (uncompilable "destructuring fn rest"))
            (recur (+ i 2) fixed (form-sym-name r)))
          (recur (inc i) (conj fixed (form-sym-name p)) rest-name)))
      {:fixed fixed :rest rest-name})))

(defn- analyze-arity [ctx pvec body env fn-name]
  (let [pp (parse-params (vec (form-vec-items pvec)))
        fixed (:fixed pp)
        rst (:rest pp)
        ;; Always a recur target, variadic included: the back end gives the rest
        ;; param an ordinary positional slot (holding the collected seq), so recur
        ;; is a self-call carrying the rest seq directly — Clojure semantics.
        rname (gen-name "arity")
        names (cond-> (vec fixed) rst (conj rst) fn-name (conj fn-name))
        env* (-> (add-locals env names) (with-recur rname))
        arity {:params fixed :recur-name rname
               :body (analyze-seq ctx body env*)}]
    ;; :rest only when variadic — an absent :rest reads back nil, same as before,
    ;; but keeps a fixed arity a nil-free struct rather than a phm.
    (if rst (assoc arity :rest rst) arity)))

(defn- analyze-fn [ctx items env]
  (let [named (form-sym? (nth items 1))
        fn-name (when named (form-sym-name (nth items 1)))
        rest-items (if named (drop 2 items) (drop 1 items))
        first* (first rest-items)]
    (cond
      (form-vec? first*)
        (fn-node fn-name [(analyze-arity ctx first* (rest rest-items) env fn-name)])
      (form-list? first*)
        (fn-node fn-name
                 (mapv (fn [clause]
                         (let [cl (vec (form-elements clause))]
                           (analyze-arity ctx (first cl) (rest cl) env fn-name)))
                       rest-items))
      :else (uncompilable "fn: bad params"))))

(defn- analyze-try [ctx items env]
  (let [clauses (rest items)
        body (atom [])
        catch-sym (atom nil)
        catch-body (atom nil)
        finally-body (atom nil)]
    (doseq [c clauses]
      (let [head (when (form-list? c) (first (vec (form-elements c))))
            hname (when (and head (form-sym? head)) (form-sym-name head))]
        (cond
          (= hname "catch")
            (let [cl (vec (form-elements c))]
              (reset! catch-sym (form-sym-name (nth cl 2)))
              (reset! catch-body (drop 3 cl)))
          (= hname "finally")
            (reset! finally-body (rest (vec (form-elements c))))
          :else (swap! body conj c))))
    {:op :try
     :body (analyze-seq ctx @body env)
     :catch-sym @catch-sym
     :catch-body (when @catch-body
                   (analyze-seq ctx @catch-body (add-locals env [@catch-sym])))
     :finally (when @finally-body (analyze-seq ctx @finally-body env))}))

(defn- analyze-special [ctx op items env]
  (case op
    "quote" (quote-node (second items))
    "if" (if-node (analyze ctx (nth items 1) env)
                  (analyze ctx (nth items 2) env)
                  (if (> (count items) 3)
                    (analyze ctx (nth items 3) env)
                    (const nil)))
    "do" (analyze-seq ctx (rest items) env)
    "throw" (throw-node (analyze ctx (nth items 1) env))
    "def" (let [name-sym (nth items 1)
                nm (form-sym-name name-sym)
                cur (compile-ns ctx)]
            (host-intern! ctx cur nm)
            (def-node cur nm (analyze ctx (nth items 2) env) (form-sym-meta name-sym)))
    "let*" (let [bvec (vec (form-vec-items (nth items 1)))
                 r (analyze-bindings ctx bvec env)]
             (let-node (first r) (analyze-seq ctx (drop 2 items) (second r))))
    "loop*" (let [bvec (vec (form-vec-items (nth items 1)))
                  rname (gen-name "loop")
                  r (analyze-bindings ctx bvec env)
                  env** (with-recur (second r) rname)]
              {:op :loop :recur-name rname :bindings (first r)
               :body (analyze-seq ctx (drop 2 items) env**)})
    "recur" (let [rt (:recur env)]
              (when-not rt (uncompilable "recur outside loop/fn"))
              {:op :recur :recur-name rt
               :args (mapv #(analyze ctx % env) (rest items))})
    "try" (analyze-try ctx items env)
    "fn*" (analyze-fn ctx items env)
    (uncompilable (str "special form " op))))

(defn- analyze-symbol [ctx form env]
  (let [nm (form-sym-name form) ns (form-sym-ns form)]
    (cond
      (and (nil? ns) (local? env nm)) (local nm)
      ns (let [r (resolve-global ctx form)]
           (if (= :var (:kind r))
             (var-ref (:ns r) (:name r))
             (uncompilable (str "qualified ref " ns "/" nm))))
      :else (let [r (resolve-global ctx form)]
              (case (:kind r)
                :var (var-ref (:ns r) (:name r))
                :host (host-ref (:name r))
                (var-ref (compile-ns ctx) nm))))))

(defn- analyze-list [ctx form env]
  (let [items (vec (form-elements form))]
    (if (zero? (count items))
      (quote-node form)
      (let [head (first items)
            hname (when (and (form-sym? head) (nil? (form-sym-ns head))) (form-sym-name head))
            shadowed (and hname (local? env hname))]
        (cond
          (and hname (not shadowed) (contains? handled hname))
            (analyze-special ctx hname items env)
          (and hname (not shadowed) (form-special? hname))
            (uncompilable (str "special form " hname))
          (and (form-sym? head) (not shadowed) (form-macro? ctx head))
            (analyze ctx (form-expand-1 ctx form) env)
          :else
            (invoke (analyze ctx head env)
                    (mapv #(analyze ctx % env) (rest items))))))))

(defn analyze
  ([ctx form] (analyze ctx form (empty-env)))
  ([ctx form env]
   (cond
     (form-literal? form) (const form)
     (form-sym? form) (analyze-symbol ctx form env)
     (form-vec? form) (vector-node (mapv #(analyze ctx % env) (form-vec-items form)))
     (form-map? form) (map-node (mapv (fn [p] [(analyze ctx (first p) env)
                                              (analyze ctx (second p) env)])
                                     (form-map-pairs form)))
     (form-set? form) (uncompilable "set literal")
     (form-list? form) (analyze-list ctx form env)
     :else (uncompilable "unsupported form"))))
