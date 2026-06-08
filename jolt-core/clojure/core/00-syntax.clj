;; clojure.core — syntax tier. The control macros the compiler and every later
;; tier depend on (when/cond/and/or/...), expressed as defmacro. Loaded FIRST
;; (before 00-kernel), interpreted, so the macros exist before any code that uses
;; them is compiled — including the kernel tier, the self-hosted analyzer, and the
;; seq/coll tiers.
;;
;; CONSTRAINT: a macro here may use ONLY special forms (if/do/let*/fn*/not) and
;; core-renames SEED primitives (first/next/rest/nth/count/empty?/...). It must
;; NOT use kernel-tier fns (second/peek/subvec/...) or anything defined later —
;; those don't exist yet when this tier loads.

(defmacro when [test & body]
  `(if ~test (do ~@body)))

(defmacro when-not [test & body]
  `(if (not ~test) (do ~@body)))

(defmacro and [& exprs]
  (if (empty? exprs)
    true
    (if (empty? (rest exprs))
      (first exprs)
      `(let* [and# ~(first exprs)] (if and# (and ~@(rest exprs)) and#)))))

(defmacro or [& exprs]
  (if (empty? exprs)
    nil
    (if (empty? (rest exprs))
      (first exprs)
      `(let* [or# ~(first exprs)] (if or# or# (or ~@(rest exprs)))))))

;; :else (any truthy value) is just a test, so no special case — (if :else e ...)
;; takes e.
(defmacro cond [& clauses]
  (if (empty? clauses)
    nil
    `(if ~(first clauses) ~(nth clauses 1) (cond ~@(drop 2 clauses)))))

;; Threading: a list form threads x in as the first (->) or last (->>) arg; a bare
;; symbol becomes (form x). Recursive; the expand-once cache makes that free.
(defmacro -> [x & forms]
  (if (empty? forms)
    x
    (let [form (first forms)
          threaded (if (seq? form)
                     `(~(first form) ~x ~@(rest form))
                     `(~form ~x))]
      `(-> ~threaded ~@(rest forms)))))

(defmacro ->> [x & forms]
  (if (empty? forms)
    x
    (let [form (first forms)
          threaded (if (seq? form)
                     `(~(first form) ~@(rest form) ~x)
                     `(~form ~x))]
      `(->> ~threaded ~@(rest forms)))))

;; Forward declaration is a no-op on Jolt — the compiler resolves forward refs via
;; pending cells (matching the prior Janet macro).
(defmacro declare [& syms] `(do))

;; destructure — Clojure's binding-vector expander, ported from the Janet seed
;; (was core-destructure). Turns a binding vector that may contain destructuring
;; patterns into a plain binding vector (alternating symbol / init-form) built from
;; nth/nthnext/get, so the COMPILER only ever sees plain symbols (analyze-bindings
;; rejects patterns). `let` consumes it directly; `loop`/`fn` reuse it transitively
;; through `let`. Written with let*/fn* and seed primitives only — it never uses
;; let/loop/fn, so expanding its own body can't recurse back into destructure.
;; Note map? is true for symbol structs too, so the symbol? clause must come first.
;; def+fn* (not defn) because the defn macro is not defined until later in the tier.
(def destructure
 (fn* destructure [bindings]
  (let* [find-or
           (fn* [or-map nm]
             (reduce (fn* [acc k]
                       (if (and (symbol? k) (= nm (name k)))
                         [true (get or-map k)]
                         acc))
                     [false nil]
                     (if or-map (keys or-map) [])))
         amp? (fn* [x] (and (symbol? x) (= "&" (name x))))
         proc
           (fn* proc [pat init acc]
             (cond
               (symbol? pat) (conj (conj acc pat) init)
               (vector? pat)
                 (let* [g (symbol (str (gensym)))
                        n (count pat)
                        vloop
                          (fn* vloop [i idx a]
                            (if (< i n)
                              (let* [elem (nth pat i)]
                                (cond
                                  (amp? elem)
                                    (vloop (+ i 2) idx (proc (nth pat (inc i)) `(nthnext ~g ~idx) a))
                                  (= elem :as)
                                    (vloop (+ i 2) idx (proc (nth pat (inc i)) g a))
                                  :else
                                    (vloop (inc i) (inc idx) (proc elem `(nth ~g ~idx nil) a))))
                              a))]
                   (vloop 0 0 (conj (conj acc g) init)))
               (map? pat)
                 (let* [g (symbol (str (gensym)))
                        or-map (get pat :or)
                        as-sym (get pat :as)
                        base (if as-sym
                               (conj (conj (conj (conj acc g) init) as-sym) g)
                               (conj (conj acc g) init))
                        group
                          (fn* [a kw kind]
                            (let* [names (get pat kw)]
                              (if names
                                (reduce
                                  ;; s is a symbol (a b) or a keyword (:a :b); name/
                                  ;; namespace handle both, so :keys [:major] binds
                                  ;; `major` looking up :major (str would keep the colon).
                                  (fn* [aa s]
                                    (let* [local (name s)
                                           nsp (namespace s)
                                           keyform (cond
                                                     (= kind :kw) (keyword (if nsp (str nsp "/" local) local))
                                                     (= kind :str) local
                                                     :else `(quote ~(symbol nsp local)))
                                           fo (find-or or-map local)]
                                      (conj (conj aa (symbol local))
                                            (if (nth fo 0)
                                              `(get ~g ~keyform ~(nth fo 1))
                                              `(get ~g ~keyform)))))
                                  a names)
                                a)))
                        g1 (group base :keys :kw)
                        g2 (group g1 :strs :str)
                        g3 (group g2 :syms :sym)]
                   (reduce (fn* [a k]
                             (if (keyword? k)
                               a
                               (proc k `(get ~g ~(get pat k)) a)))
                           g3 (keys pat)))
               :else (throw (str "unsupported destructuring pattern"))))
         ploop
           (fn* ploop [i acc]
             (if (< i (count bindings))
               (ploop (+ i 2) (proc (nth bindings i) (nth bindings (inc i)) acc))
               acc))]
    (ploop 0 []))))

;; let desugars destructuring patterns to plain bindings (via destructure) so the
;; COMPILER sees only plain symbols — analyze-bindings rejects patterns as
;; uncompilable, relying on this macro to have expanded them. (The interpreter
;; could destructure let* directly, but the compiler can't.) let* is sequential, so
;; a later init can reference an earlier destructured name. Splice via [~@..] so the
;; binding vector is a tuple form (destructure returns a pvec), not a pvec literal.
(defmacro let [bindings & body]
  `(let* [~@(destructure bindings)] ~@body))

;; loop binds destructuring forms like let, but recur must target the loop* vars,
;; whose count can't change. So (matching Clojure): gensym one loop var per binding,
;; loop* over those, and destructure them via an inner let each iteration; an outer
;; let establishes the destructured names so later inits can see them. Plain loops
;; (no patterns) pass straight through to loop*.
(defmacro loop [bindings & body]
  (let [d (destructure bindings)]
    (if (= d bindings)
      `(loop* ~bindings ~@body)
      (let [bs (take-nth 2 bindings)
            vs (take-nth 2 (drop 1 bindings))
            gs (map (fn [b] (if (symbol? b) b (symbol (str (gensym))))) bs)
            outer (reduce (fn [acc t]
                            (let [b (nth t 0) v (nth t 1) g (nth t 2)]
                              (if (symbol? b) (conj (conj acc g) v)
                                  (conj (conj (conj (conj acc g) v) b) g))))
                          [] (map vector bs vs gs))
            inner (reduce (fn [acc t] (conj (conj acc (nth t 0)) (nth t 1)))
                          [] (map vector bs gs))
            loopv (reduce (fn [acc g] (conj (conj acc g) g)) [] gs)]
        ;; splice via [~@..] so the binding vectors are tuple forms, not pvecs.
        `(let [~@outer] (loop* [~@loopv] (let [~@inner] ~@body)))))))

;; fn: desugar destructuring params to plain symbols + a body let (matching
;; Clojure's maybe-destructured), so fn* only ever sees plain params (the compiler's
;; analyze-fn requires that). Plain params pass through untouched. Handles an
;; optional name and single- or multi-arity. md/mk are fn* (not fn) to avoid a cycle.
;; md walks a param seq, replacing non-symbol patterns with gensyms and recording
;; [pattern gensym] let-bindings; mk turns one arity (params . body) into a rewritten
;; arity. Output: single arity splices the arity's elements straight into fn*; multi
;; arity splices the rewritten clauses.
(defmacro fn [& raw]
  (let [nm (if (symbol? (first raw)) (first raw) nil)
        aftn (if nm (next raw) raw)
        md (fn* go [ps nps lets]
             (if (seq ps)
               (if (symbol? (first ps))
                 (go (next ps) (conj nps (first ps)) lets)
                 ;; bare (gensym) here is Janet's (a Janet symbol the destructurer
                 ;; rejects); round-trip through str for a jolt symbol.
                 (let [g (symbol (str (gensym)))]
                   (go (next ps) (conj nps g) (conj (conj lets (first ps)) g))))
               [nps lets]))
        mk (fn* [sig]
             (let [r (md (seq (first sig)) [] [])]
               (if (empty? (nth r 1))
                 sig
                 ;; build the params/let vectors via [~@..] so they are tuple forms
                 ;; (the accumulators are plain seqs, the wrong representation).
                 (let [pv `[~@(nth r 0)]
                       lv `[~@(nth r 1)]]
                   `(~pv (let ~lv ~@(rest sig)))))))]
    (if (vector? (first aftn))
      (let [a (mk aftn)]
        (if nm `(fn* ~nm ~@a) `(fn* ~@a)))
      (let [as (vec (map mk aftn))]
        (if nm `(fn* ~nm ~@as) `(fn* ~@as))))))

;; defn: drop an optional leading docstring and attr-map, then (def name (fn* ...)).
;; Both single- and multi-arity reduce to (fn* ~@body) — fn* takes either a params
;; vector + body or a sequence of ([params] body) clauses, so no arity branching is
;; needed. (map? is true for symbol forms too, so guard the attr-map with symbol?.)
;; Defined before fresh-sym below, which is a defn-.
(defmacro defn [fn-name & body]
  (let [body (if (and (seq body) (string? (first body))) (rest body) body)
        body (if (and (seq body) (map? (first body)) (not (symbol? (first body))))
               (rest body) body)]
    `(def ~fn-name (fn* ~@body))))

;; Jolt doesn't enforce privacy, so defn- is just defn (matching how Clojure's own
;; defn- delegates to defn with :private metadata).
(defmacro defn- [fn-name & body] `(defn ~fn-name ~@body))

;; A fresh jolt symbol inside a macro body (a bare (gensym) returns a Janet symbol
;; the destructurer rejects). This defn compiles fine: by the time a tier triggers
;; the analyzer build the kernel is in place (the build is gated until then).
(defn- fresh-sym [] (symbol (str (gensym))))

;; cond->: thread expr through each (test form) pair, only when the test is truthy.
;; Linear nested let*, a distinct fresh symbol per step.
(defmacro cond-> [expr & clauses]
  (let [step (fn step [prev cls]
               (if (empty? cls)
                 prev
                 (let [t (first cls)
                       f (nth cls 1)
                       gn (fresh-sym)
                       call (if (seq? f) `(~(first f) ~prev ~@(rest f)) `(~f ~prev))]
                   `(let* [~gn (if ~t ~call ~prev)] ~(step gn (drop 2 cls))))))
        g0 (fresh-sym)]
    `(let* [~g0 ~expr] ~(step g0 clauses))))

;; case: nested =/or tests (no jump table). Test constants are NOT evaluated —
;; symbols and list constants are quoted; a list in test position is a set (or).
(defmacro case [expr & clauses]
  (let [g (fresh-sym)
        mk-const (fn [c] (if (or (symbol? c) (seq? c)) `(quote ~c) c))
        mk-test (fn [c]
                  (if (seq? c)
                    `(or ~@(map (fn [v] `(= ~g ~(mk-const v))) c))
                    `(= ~g ~(mk-const c))))
        build (fn build [cls]
                (if (empty? cls)
                  nil
                  (if (empty? (rest cls))
                    (first cls)
                    `(if ~(mk-test (first cls)) ~(nth cls 1) ~(build (drop 2 cls))))))]
    `(let* [~g ~expr] ~(build clauses))))

;; for: list comprehension, desugared to nested map/mapcat over the binding colls.
;; Per binding group: :when wraps the inner form in (if test (list inner) []) so
;; mapcat drops it when false; :let wraps it in a let*; :while wraps the coll in
;; take-while. The last group with no modifiers is a plain map (no flatten needed).
;; Faithful port of the prior Janet macro (single body expr). The body uses only
;; kernel/seed fns so it runs at analyzer-build time. `fn` (not fn*) carries the
;; binding so destructuring forms work.
(defmacro for [bindings body]
  (let [scan (fn scan [bvec i bind coll mods]
               (if (and (< i (count bvec)) (keyword? (nth bvec i)))
                 (let [k (nth bvec i)
                       v (nth bvec (inc i))]
                   (cond
                     (= k :when)  (scan bvec (+ i 2) bind coll (conj mods [:when v]))
                     (= k :let)   (scan bvec (+ i 2) bind coll (conj mods [:let v]))
                     (= k :while) (scan bvec (+ i 2) bind `(take-while (fn [~bind] ~v) ~coll) mods)
                     :else        (scan bvec (inc i) bind coll mods)))
                 [i bind coll mods]))
        parse-groups (fn parse-groups [bvec i groups]
                       (if (>= i (count bvec))
                         groups
                         (let [r (scan bvec (+ i 2) (nth bvec i) (nth bvec (inc i)) [])]
                           (parse-groups bvec (nth r 0)
                                         (conj groups [(nth r 1) (nth r 2) (nth r 3)])))))
        ;; Apply the group's modifiers around a contribution that is ALREADY a seq
        ;; (a (list body) for the last group, an inner comprehension otherwise), so
        ;; :when just returns it or [] — no extra (list ...) that mapcat couldn't
        ;; flatten. :let binds around it; mods apply outer-to-inner (left to right).
        wrap-mods (fn wrap-mods [mods inner]
                    (if (empty? mods)
                      inner
                      (let [m (first mods)
                            sub (wrap-mods (rest mods) inner)]
                        (if (= (first m) :when)
                          `(if ~(nth m 1) ~sub [])
                          `(let* ~(nth m 1) ~sub)))))
        build (fn build [idx groups]
                (let [g (nth groups idx)
                      my-bind (nth g 0)
                      my-coll (nth g 1)
                      my-mods (nth g 2)
                      is-last (= idx (dec (count groups)))]
                  (if (and is-last (empty? my-mods))
                    ;; fast path: last group, no modifiers -> a plain map of body
                    `(map (fn [~my-bind] ~body) ~my-coll)
                    ;; general: mapcat over a seq contribution (wrap a last-group
                    ;; body in a one-element list so mapcat yields the bodies).
                    (let [base (if is-last `(list ~body) (build (inc idx) groups))]
                      `(mapcat (fn [~my-bind] ~(wrap-mods my-mods base)) ~my-coll)))))]
    (if (>= (count bindings) 2)
      (build 0 (parse-groups bindings 0 []))
      body)))

;; doseq runs body for side effects across the bindings, returning nil. Same
;; shortcut as the prior Janet macro: realize a `for` comprehension with count
;; (for handles :when/:let/:while and multiple bindings).
(defmacro doseq [bindings & body]
  `(do (count (for ~bindings (do ~@body nil))) nil))

;; when-let must live in this (early) tier, not 30-macros with its if-let/if-some/
;; when-some siblings: 20-coll uses it (not-empty), and 20-coll loads before 30. The
;; name binds only in the taken branch (temp# tests the value); via `let` so the
;; binding form may itself destructure, matching Clojure.
(defmacro when-let [bindings & body]
  (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (if temp# (let [~form temp#] ~@body) nil))))

;; lazy-seq / lazy-cat live here (not 30-macros) because the seq/coll tiers use
;; them and compile-as-they-load: the macro must be registered before those tiers
;; or (lazy-seq …) compiles to a call of the macro-as-function and leaks its
;; expansion at runtime (jolt-r81). They use only seed fns (make-lazy-seq/
;; coll->cells/concat) + map, all available from the start.
;; lazy-seq defers its body: make-lazy-seq holds a thunk that realizes the body
;; to cells when forced. lazy-cat wraps each coll in a lazy-seq and concats.
(defmacro lazy-seq [& body]
  `(make-lazy-seq (fn* [] (coll->cells (do ~@body)))))

(defmacro lazy-cat [& colls]
  `(concat ~@(map (fn [c] `(lazy-seq ~c)) colls)))
