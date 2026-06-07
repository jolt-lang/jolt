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

;; cond-> stays in Janet (the compiler uses it); cond->> (thread-last) is safe.
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
