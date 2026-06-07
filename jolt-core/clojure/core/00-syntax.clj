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

;; fn -> fn*: the analyzer treats fn* as the primitive (it handles params, &-rest,
;; multi-arity); fn is just the public spelling.
(defmacro fn [& args] `(fn* ~@args))

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
