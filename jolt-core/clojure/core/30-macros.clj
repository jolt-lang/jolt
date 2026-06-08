;; clojure.core — macro tier. Macros expressed in Clojure (defmacro + syntax-quote)
;; rather than as hand-built Janet form-transformers. Loaded after the fn tiers,
;; so a macro here may use any already-frozen core fn/macro.
;;
;; IMPORTANT — only macros NOT used by the self-hosted compiler (jolt-core/jolt/*)
;; or by the earlier overlay tiers belong here; those (and/or/when/when-not/
;; when-let/cond/case/doseq/declare/cond->/->) must stay available before this
;; tier loads, so they remain in Janet for now. Everything here is user-facing.
;;
;; Migration: remove the Janet core-X macro fn AND its core-macro-names entry when
;; moving a macro here (defmacro installs the :macro flag itself).

(defmacro comment [& body] nil)

;; Single arglist (Jolt defmacro is single-arity); the optional else defaults nil
;; via rest-destructuring.
(defmacro if-not [test then & [else]]
  `(if (not ~test) ~then ~else))

;; Conditional binding macros: the name is bound ONLY in the taken branch (the
;; auto-gensym temp# tests the value; the else/empty branch sees the surrounding
;; scope). temp# is a single template-local gensym — referenced twice, same symbol.
(defmacro if-let [bindings then & [else]]
  (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (if temp# (let [~form temp#] ~then) ~else))))

;; when-let lives in 00-syntax (not here): 20-coll uses it, which loads before this tier.

(defmacro if-some [bindings then & [else]]
  (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (if (some? temp#) (let [~form temp#] ~then) ~else))))

(defmacro when-some [bindings & body]
  (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (if (some? temp#) (let [~form temp#] ~@body) nil))))

(defmacro while [test & body]
  `(loop [] (when ~test ~@body (recur))))

(defmacro dotimes [bindings & body]
  (let [i (bindings 0) n (bindings 1)]
    `(let [n# ~n]
       (loop [~i 0]
         (when (< ~i n#) ~@body (recur (inc ~i)))))))

;; A fresh jolt symbol inside a macro body: (gensym) here resolves to Janet's
;; builtin (a Janet symbol the destructurer rejects), so round-trip through str.
(defn- fresh-sym [] (symbol (str (gensym))))

;; Lazy-safe: take only the head via first (Clojure uses (seq coll), but Jolt's
;; eager seq would realize an infinite coll like (repeat nil) and hang). Matches
;; the prior Janet behavior; the nil/false-head distinction waits on Phase 5
;; laziness.
(defmacro when-first [bindings & body]
  (let [x (bindings 0) coll (bindings 1)]
    `(when-let [~x (first ~coll)] ~@body)))

;; doto threads a single fresh-bound value as the first arg of each form (side
;; effects), returning the value. A shared explicit gensym is needed because the
;; forms are built outside the let's template.
(defmacro doto [x & forms]
  (let [g (fresh-sym)
        steps (map (fn [f] (if (seq? f) (apply list (first f) g (rest f)) (list f g))) forms)]
    `(let [~g ~x] ~@steps ~g)))

;; Threading-with-rebinding macros. The binding pairs are spliced into a TEMPLATE
;; vector (so core-let sees a tuple form, not a runtime pvec value).
(defn- thread-binds [g steps]
  (reduce (fn [acc s] (conj (conj acc g) s)) [] (butlast steps)))

(defmacro as-> [expr name & forms]
  (let [pairs (reduce (fn [acc f] (conj (conj acc name) f)) [] (butlast forms))]
    `(let [~name ~expr ~@pairs] ~(if (empty? forms) name (last forms)))))

(defmacro some-> [expr & forms]
  (let [g (fresh-sym)
        steps (map (fn [f] `(if (nil? ~g) nil (-> ~g ~f))) forms)]
    `(let [~g ~expr ~@(thread-binds g steps)] ~(if (empty? steps) g (last steps)))))

(defmacro some->> [expr & forms]
  (let [g (fresh-sym)
        steps (map (fn [f] `(if (nil? ~g) nil (->> ~g ~f))) forms)]
    `(let [~g ~expr ~@(thread-binds g steps)] ~(if (empty? steps) g (last steps)))))

(defmacro cond->> [expr & clauses]
  (let [g (fresh-sym)
        steps (map (fn [pair] `(if ~(first pair) (->> ~g ~(second pair)) ~g))
                   (partition 2 clauses))]
    `(let [~g ~expr ~@(thread-binds g steps)] ~(if (empty? steps) g (last steps)))))

(defmacro assert [x & [message]]
  (let [msg (if message message (str "Assert failed: " (pr-str x)))]
    `(when-not ~x (throw (ex-info ~msg {})))))

(defmacro delay [& body]
  `(make-delay (fn [] ~@body)))

(defmacro future [& body]
  `(future-call (fn [] ~@body)))

;; Build the fn* form via a template (a reader-list array): cons/list in a macro
;; body produce a plist the evaluator can't call as a form.
(defmacro letfn [fnspecs & body]
  (let [binds (reduce (fn [acc spec] (conj (conj acc (first spec)) `(fn* ~@(rest spec))))
                      [] fnspecs)]
    `(let* [~@binds] ~@body)))

;; Dynamic binding: install a thread-binding frame of var->value (array-map keeps
;; var-get happy, unlike a phm), restore on exit.
(defmacro binding [bindings & body]
  (let [pairs (reduce (fn [acc p] (conj (conj acc `(var ~(first p))) (second p)))
                      [] (partition 2 bindings))]
    `(let* [frame# (array-map ~@pairs)]
       (push-thread-bindings frame#)
       (try (do ~@body) (finally (pop-thread-bindings))))))

;; condp: clauses are test-expr result-expr, or test-expr :>> result-fn (calls
;; result-fn on the truthy (pred test-expr value)); a lone trailing expr is the
;; default. The recursive emit builds a nested if chain.
(defmacro condp [pred expr & clauses]
  (let [gp (fresh-sym) ge (fresh-sym)
        emit (fn emit [args]
               (let [n (if (= :>> (second args)) 3 2)
                     clause (take n args)
                     more (drop n args)
                     cn (count clause)]
                 (cond
                   (= 0 cn) `(throw (ex-info (str "No matching clause: " ~ge) {}))
                   (= 1 cn) (first clause)
                   (= 2 cn) `(if (~gp ~(first clause) ~ge) ~(second clause) ~(emit more))
                   :else `(if-let [p# (~gp ~(first clause) ~ge)]
                            (~(nth clause 2) p#)
                            ~(emit more)))))]
    `(let [~gp ~pred ~ge ~expr] ~(emit clauses))))

;; --- protocols, records, types ---------------------------------------------
;; These emit Jolt's protocol/type special forms (protocol-dispatch,
;; register-method, make-reified, deftype).

;; Group a flat seq that starts with a head symbol followed by its list specs
;; into [[head spec spec ...] ...] runs. Used by extend-protocol and defrecord.
(defn- group-by-head [items]
  (reduce (fn [acc x]
            (if (symbol? x)
              (conj acc [x])
              (conj (pop acc) (conj (peek acc) x))))
          [] items))

;; The protocol value is built by make-protocol (a fn call) rather than an embedded
;; tagged map literal: the interpreter would otherwise self-evaluate such a struct
;; instead of evaluating its fields. methods is a {kw {:name str}} map (only :name
;; is consulted). Each method is a thin dispatch fn over protocol-dispatch.
(defmacro defprotocol [pname & sigs]
  (let [methods (reduce (fn [m sig]
                          (assoc m (keyword (name (first sig))) {:name (name (first sig))}))
                        {} sigs)]
    `(do
       (def ~pname (make-protocol ~(name pname) ~methods))
       ~@(map (fn [sig]
                `(def ~(first sig)
                   (fn* [this# & rest#] (protocol-dispatch ~pname ~(first sig) this# rest#))))
              sigs))))

(defmacro extend-type [tsym psym & impls]
  `(do ~@(map (fn [spec]
                `(register-method ~tsym ~psym ~(first spec)
                                  (fn* ~(nth spec 1) ~@(drop 2 spec))))
              impls)))

(defmacro extend-protocol [psym & type-impls]
  `(do ~@(map (fn [g] `(extend-type ~(first g) ~psym ~@(rest g)))
              (group-by-head type-impls))))

;; extend (the fn form) is not supported — stub to nil, as before.
(defmacro extend [& args] nil)
;; JVM proxies are unsupported.
(defmacro proxy [& args] nil)
;; definterface is JVM-only; bind the name to an empty marker.
(defmacro definterface [name-sym & body] `(def ~name-sym {}))

;; Build a method map {kw (fn* ...)} as an embedded map literal — make-reified
;; evaluates it (the fn* forms become fns) via build-eval-map, which yields a
;; struct it can iterate; a (hash-map ...) call would instead yield a phm it can't.
(defmacro reify [& forms]
  (loop [items (seq forms) proto nil methods {}]
    (if (empty? items)
      `(make-reified ~proto ~methods)
      (let [x (first items)]
        (if (symbol? x)
          (recur (rest items) (if proto proto x) methods)
          (recur (rest items) proto
                 (assoc methods (keyword (name (first x)))
                        `(fn* ~(nth x 1) ~@(drop 2 x)))))))))

(defmacro defrecord [name-sym fields & body]
  (let [tn (name name-sym)
        dot (symbol (str tn "."))
        arrow (symbol (str "->" tn))
        mapf (symbol (str "map->" tn))
        m (fresh-sym)
        ;; each method body sees the record fields, bound from the instance (the
        ;; method's first param), matching Clojure's defrecord method scope. vec the
        ;; spliced binding seq so ~@ splices its elements, not the lazy-seq itself.
        impl (fn [proto specs]
               `(extend-type ~name-sym ~proto
                  ~@(map (fn [spec]
                           (let [argv (nth spec 1)
                                 inst (first argv)
                                 binds (vec (mapcat (fn [f] [f `(get ~inst ~(keyword (name f)))]) fields))]
                             `(~(first spec) ~argv (let [~@binds] ~@(drop 2 spec)))))
                         specs)))]
    `(do
       (deftype ~name-sym ~fields)
       (def ~arrow (fn* ~fields (~dot ~@fields)))
       (def ~mapf (fn* [~m] (~arrow ~@(map (fn [f] `(get ~m ~(keyword (name f)))) fields))))
       ~@(map (fn [g] (impl (first g) (rest g))) (group-by-head body)))))

;; --- laziness --------------------------------------------------------------
;; lazy-seq / lazy-cat moved to the 00-syntax tier: the seq/coll tiers (10-seq,
;; 20-coll) use lazy-seq, and in compile mode a tier's forms are compiled as it
;; loads — so the macro must be registered BEFORE those tiers, else (lazy-seq …)
;; compiles as a call to the macro-as-function and leaks its expansion at runtime
;; (jolt-r81). They only need seed fns (make-lazy-seq/coll->cells/concat).
