(ns jolt.analyzer
  "Portable Clojure analyzer: reader form -> host-neutral IR (see jolt.ir).

  Depends only on the host contract (jolt.host), IR
  constructors (jolt.ir) and its own leaf namespaces — jolt.analyzer.diagnostics
  (positions and kinded errors), .hints (type hints, param parsing), .tree (walks
  over analyzed IR), .ffi (literal jolt.ffi forms) and .interop (host member and
  class names), none of which call back into `analyze`. The contract fns are referred unqualified
  (host form predicates are `form-*` to avoid colliding with clojure.core), so the
  bootstrap can compile this namespace via its plain :var path. ctx is an opaque
  host handle threaded to the contract fns; the analyzer never inspects it.

  Unsupported forms throw an ex-info carrying :jolt.error/kind and a position
  so the caller falls back to the interpreter (the hybrid contract).

  `env` carries lexical state: {:locals #{names} :recur recur-target-name|nil}.
  Definitions are ordered so only `analyze` (mutually recursive) is forward
  declared — the bootstrap compiles forward refs through var cells, but keeping
  them to one keeps the compiled namespace simple."
  (:require [jolt.ir :refer [const local var-ref the-var host-ref if-node do-node invoke
                             coerce-node
                             def-node let-node fn-node vector-node map-node set-node
                             quote-node throw-node host-static host-new]]
            [jolt.host :refer [form-sym? form-sym-name form-sym-ns form-list?
                               form-vec? form-map? form-set?
                               form-literal? form-keyword? form-elements form-vec-items
                               form-map-pairs form-set-items form-special? compile-ns
                               form-regex? form-regex-source
                               form-inst? form-inst-source form-uuid? form-uuid-source
                               form-bigdec? form-bigdec-source
                               form-bigdec-value? form-bigdec-value-source
                               form-float-value? form-float-value-double
                               form-inst-value? form-inst-value-source
                               form-uuid-value? form-uuid-value-source
                               form-ns-value? form-ns-value-name
                               form-var-value? form-var-value-ns form-var-value-name
                               form-class-value? form-class-value-name
                               form-tagged? form-tag-name
                               unchecked-math? allow-unresolved-vars?
                               form-macro? form-expand-1 resolve-global
                               form-sym-meta form-coll-meta host-intern! form-syntax-quote-lower
                               form-syntax-quote-expand
                               deftype-ctor-class form-position form-line
                               resolve-class-hint host-class-name?
                               embed-plan ctx-for-ns]]
            [jolt.analyzer.diagnostics :refer [*positioned-form-box* *expansion-box*
                                               *macro-run-box* analysis-error
                                               resolve-error as-analysis-diagnostic]]
            [jolt.analyzer.hints :refer [jvm-boolean-prim? jvm-non-boolean-class hint-of
                                         init-proves-hint add-hint tag->nkind ahint-of
                                         with-ret-nhint fi-tag-class fi-adapt-form
                                         parse-params strip-arglist-meta arglist-ret-nkind
                                         str-target-type kw-target-type sb-target-type]]
            [jolt.analyzer.tree :refer [check-recur-tails! fn-free-names check-fn-arities]]
            [jolt.analyzer.ffi :refer [analyze-ffi-layout analyze-ffi-fn]]
            [jolt.analyzer.interop :refer [field-head? method-head? ctor-head?
                                           array-class-dims array-class-name
                                           method-value-form static-class-ns
                                           unchecked-arith num-cast invoke-rewrite]]))

(declare analyze)
;; the quote arm reaches it well before its definition (it needs analyze itself)
(declare embedded-plan-node)

;; Special forms analyze-special has a dispatch arm for — the subset of the host
;; contract's reserved words (jolt.host/form-special?) the analyzer lowers itself.
;; The two differ deliberately (e.g. interop heads like `new`/`.` are reserved but
;; analyzed in analyze-list), so keep them in sync by intent, not by equality.
(def ^:private handled
  #{"quote" "if" "do" "def" "fn*" "let*" "loop*" "recur" "throw" "try"
    "syntax-quote" "var" "letfn*" "set!" "defmacro"
    "monitor-enter" "monitor-exit"})

;; ...of which these dispatch ONLY namespace-qualified. `syntax-quote` names no
;; Clojure special form — it is jolt's marker for a `, which the reader always
;; emits as clojure.core/syntax-quote. Letting the bare name dispatch reserved it
;; against every program: a var or local called syntax-quote compiled into a
;; syntax-quote of its first argument instead of a call (edamame names its own
;; resolver that). The qualified spelling is unreachable from user source in
;; practice, and matches how the reader spells ~ and ~@.
(def ^:private qualified-only #{"syntax-quote"})

;; Process-wide on purpose: an anon fn's generated name is REGISTERED (backend
;; rname), so it has to be unique across every thread compiling, not just within one
;; compilation. Read and bump in one swap! — reading the atom and then incrementing
;; it is two steps, and two threads that interleaved between them got the same name.
(def ^:private gensym-counter (atom 0))
(defn- gen-name [prefix]
  (str "_r$" prefix (dec (swap! gensym-counter inc))))

(defn set-name-counter!
  "Reseed the counter behind generated names. The build driver does this at each
  namespace so a namespace's emitted text does not depend on how many names the
  namespaces before it — or the in-process load before the build — used, which is
  what lets the build cache one compiled unit per namespace (#1059). Safe because
  every name this makes is either lexical (a catch, loop or duplicate-param local)
  or qualified by its compile namespace (a fn's rname), and the driver seeds each
  build phase above anything the load phase reached."
  [n]
  (reset! gensym-counter n))

(defn- empty-env [] {:locals #{} :hints {}})
(defn- local? [env nm] (contains? (:locals env) nm))
;; A new binding of a name forgets what was known about the one it shadows: its
;; type hint and its JVM primitive type. An outer ^String s used to lend its hint
;; to an inner, unhinted s, since add-hint only ever adds.
(defn- add-locals [env names]
  (-> env
      (update :locals #(reduce conj % names))
      (update :hints #(reduce dissoc % names))
      (update :jprims #(reduce dissoc % names))))
;; &env value handed to a macro: a map of each in-scope local SYMBOL to nil
;; (Clojure's &env maps locals to compiler binding objects; consumers like
;; core.logic's matche only read its keys to tell locals from fresh pattern vars).
(defn- amp-env-map [env]
  (reduce (fn [m n] (assoc m (symbol n) nil)) {} (:locals env)))
;; The macro var's qualified symbol, as the reference names an expanding macro
;; whose argument check failed (clojure.core/let for a bare `let`); the head as
;; written when it does not resolve to a var (it cannot fail to, having passed
;; form-macro?, but a name is better than a nil either way). The macro-run box
;; carries this AND the head as written, for the two spellings macro-run-symbol
;; picks between.
(defn- macro-symbol [ctx head]
  (let [r (resolve-global ctx head)]
    (if (and (= :var (:kind r)) (:ns r))
      (symbol (:ns r) (:name r))
      (symbol (form-sym-name head)))))
;; A recur target carries its name (the compiled self-call) and its ARITY (the
;; binding count) so a recur with the wrong number of args is a compile error,
;; like the JVM, instead of failing only at runtime on the branch that runs.
;; Establishing a target also lifts any `try` boundary crossed to get here: the
;; boundary is about reaching an OUTER target, and a fn or loop written inside a
;; try recurs to itself normally, exactly as it does on the JVM.
(defn- with-recur
  ([env name arity] (with-recur env name arity nil))
  ([env name arity prims]
   (-> env (assoc :recur name :recur-arity arity :recur-prims prims) (dissoc :recur-blocked))))

(defn- analyze-seq [ctx forms env]
  (let [v (mapv #(analyze ctx % env) forms)
        n (count v)]
    (cond
      (zero? n) (const nil)
      (= 1 n) (first v)
      :else (do-node (subvec v 0 (dec n)) (peek v)))))

(defn- binding-vector
  "The binding vector of a (let*|loop*|letfn* [...] body*) form, as a Clojure vector."
  [items]
  (let [bvec (nth items 1 nil)]
    (when-not (form-vec? bvec)
      (analysis-error :analyze/invalid-binding "Bad binding form, expected vector"))
    (vec (form-vec-items bvec))))

(defn- analyze-bindings [ctx bvec env]
  ;; Checked BEFORE the walk, because the walk reads pairs: an odd vector sent
  ;; (nth bvec (inc i)) past the end and the user got the raw fault that came
  ;; back — "java.lang.IndexOutOfBoundsException: index out of bounds" as the
  ;; compile error for (let [a 1 b] a). The reference reports the binding form.
  (when (odd? (count bvec))
    (analysis-error :analyze/invalid-binding
                    "Bad binding form, expected matched symbol expression pairs"))
  (loop [i 0 env env pairs []]
    (if (< i (count bvec))
      (let [bsym (nth bvec i)]
        (when-not (form-sym? bsym)
          (analysis-error :analyze/invalid-binding
                          "Bad binding form, expected symbol"))
        (let [nm (form-sym-name bsym)
              fi (fi-tag-class ctx bsym)
              init0 (analyze ctx (if fi
                                   (fi-adapt-form fi (nth bvec (inc i)))
                                   (nth bvec (inc i)))
                             env)
              ;; a ^doubles/^longs/^objects/... let binding wraps its init in a
              ;; :coerce of the array kind, so jolt.passes.numeric seeds the local
              ;; for the direct aget/aset paths. A node, not a key on the init:
              ;; the inline fixpoint rebuilds inits (flatten hoists a let-valued
              ;; one, a splice replaces an invoke), and a key on the old node was
              ;; lost with it. The back end emits an array-kind coerce as its expr.
              ak (ahint-of ctx bsym)
              init (if ak (coerce-node ak init0) init0)]
          ;; an explicit hint wins; init-proves-hint only fills in where the
          ;; programmer wrote nothing.
          (recur (+ i 2) (let [e (add-hint (add-locals env [nm]) nm
                                           (or (hint-of ctx bsym) (init-proves-hint init)))]
                           (if (jvm-boolean-prim? init env)
                             (assoc-in e [:jprims nm] :boolean)
                             e))
                 (conj pairs [nm init]))))
      [pairs env])))

;; Clojure lets a later param shadow an earlier same-named one (a macro expander
;; uses _ for both its &form and &env slots, so its param list is (_ _ …)); the
;; body binds the LAST occurrence. Chez rejects duplicate formals, so rename every
;; earlier duplicate to a fresh name — it is shadowed and unreferenceable.
(defn- uniquify-params [names]
  (let [n (count names)]
    (loop [i 0 out []]
      (if (< i n)
        (let [nm (nth names i)
              dup? (loop [j (inc i)]
                     (cond (>= j n) false
                           (= nm (nth names j)) true
                           :else (recur (inc j))))]
          (recur (inc i) (conj out (if dup? (gen-name (str nm "_")) nm))))
        out))))

;; The JVM's macro calling convention: an expander fn's parameters are
;; [&form &env & declared], so (apply macro-fn form env args) is a legal call.
;; That shape is not decoration — every tools.analyzer-style macroexpand-1
;; (typedclojure's included) expands a macro by applying its fn that way, and a
;; declared-params-only fn answers those with an ArityException. Inside the body
;; &form/&env are then ordinary lexical params, so a closure over them works
;; where a dynamically-bound var would already have been popped. The DECLARED
;; params are what :arglists keeps, as on the JVM.
(defn- macro-arity-params [pvec]
  (let [m (form-coll-meta pvec)
        v (vec (concat [(symbol "&form") (symbol "&env")] (form-vec-items pvec)))]
    (if m (with-meta v m) v)))

;; Split a defmacro's forms after the docstring and leading attr-map into
;; [arity-forms trailing-attr-map]. defmacro takes a trailing attr-map exactly as
;; defn does (the JVM's hands the whole fdecl, that map included, straight to
;; defn); jolt used to run it through the arity lowering, which made it a bogus
;; ([&form &env]) clause and lost it from the var's metadata. Only a MULTI-arity
;; body can carry one — a single `[params] body` is one clause already, so its
;; last form is a map-returning body, not an attr-map (the same rule defn states).
(defn- macro-trail-attr [after]
  (if (and (form-list? (first after)) (> (count after) 1))
    (let [lst (last after)]
      (if (form-map? lst) [(butlast after) lst] [after nil]))
    [after nil]))

;; The arity CLAUSES, each with the two implicit params in front. `after` here is
;; already past the docstring, the leading attr-map and the trailing one.
;; Always a list of clauses, never a spliced single arity — the reference
;; normalizes a lone `[params] body` into one clause before it prepends anything
;; ((if (vector? (first fdecl)) (list fdecl) fdecl)), and both readers of this
;; want that shape: `fn` accepts either, and the macroexpand-1 answer has to BE
;; the JVM's. Empty when the form declares no arity at all: (defmacro m) is a defn
;; with no body, not one taking [&form &env].
(defn- macro-fn-arities [after]
  (cond
    (empty? after) after
    (form-list? (first after))
    (map (fn [clause]
           (let [es (vec (form-elements clause))]
             (cons (macro-arity-params (first es)) (rest es))))
         after)
    :else (list (cons (macro-arity-params (first after)) (rest after)))))

;; What macroexpand-1 answers for a defmacro form. defmacro is a special form
;; here, so this never runs during compilation — but a tools.analyzer-style
;; consumer expands to a fixpoint and reads the result, and on the JVM that
;; result is a defn plus a setMacro, not an opaque call to defmacro. It shares
;; macro-arity-params with the special-form arm below so the reported shape
;; cannot drift from the compiled one: docstring and attr-map keep their place,
;; every arity becomes a clause, and &form/&env lead the declared params.
(defn defmacro-expansion [form]
  (let [items (vec (form-elements form))
        name-sym (nth items 1)
        after (drop 2 items)
        [after doc] (if (string? (first after)) [(rest after) (first after)] [after nil])
        [after attr] (if (form-map? (first after)) [(rest after) (first after)] [after nil])
        [after trail] (macro-trail-attr after)
        vform (list 'var name-sym)]
    (list 'do
          ;; the arity lowering is macro-fn-arities, the SAME one the special-form
          ;; arm compiles through — a second copy here is a shape that can drift
          ;; from the one the compiler actually uses.
          (concat (list (symbol "clojure.core" "defn") name-sym)
                  (when doc (list doc))
                  (when attr (list attr))
                  (macro-fn-arities after)
                  (when trail (list trail)))
          (list '. vform (list 'setMacro))
          vform)))

(defn- analyze-arity [ctx pvec body env fn-name]
  (let [pp (parse-params ctx (vec (form-vec-items pvec)))
        rst (:rest pp)
        ;; uniquify over fixed AND rest together: the rest param shares the one
        ;; Scheme formal list ((lambda (a b . r))), so a positional duplicating
        ;; it ([_ x & _]) must rename too. The rest is the last occurrence and
        ;; always keeps its name, so only fixed slots ever change.
        fixed (let [u (uniquify-params (cond-> (vec (:fixed pp)) rst (conj rst)))]
                (if rst (subvec u 0 (dec (count u))) u))
        ;; Always a recur target, variadic included: the back end gives the rest
        ;; param an ordinary positional slot (holding the collected seq), so recur
        ;; is a self-call carrying the rest seq directly — Clojure semantics.
        ;; rname is the env's recur target — a per-unit-unique token recur
        ;; resolves against (gen-name's counter keeps it unique; the ns/fn-name
        ;; prefix is only for readability). The back end labels the emitted frame
        ;; itself, so rname isn't carried on the node.
        rname (gen-name (str (compile-ns ctx) "/" (or fn-name "fn") "--"))
        names (cond-> (vec fixed) rst (conj rst) fn-name (conj fn-name))
        ;; recur arity: fixed params, +1 for a rest param (it takes the collected
        ;; seq as one positional slot). fn-name is the self-ref, not a param.
        env0 (-> (add-locals env names) (with-recur rname (+ (count fixed) (if rst 1 0))))
        env* (reduce (fn [e pr] (add-hint e (nth pr 0) (nth pr 1))) env0 (:hints pp))
        ;; One of the two recur-target roots (the other is loop*): the body is
        ;; where a recur against THIS arity may legally sit, so it is where the
        ;; tail-position check starts.
        body-node (analyze-seq ctx body env*)
        _ (check-recur-tails! body-node)
        arity {:params fixed
               :body body-node}
        ;; carry record param hints (name -> ctor-key) for the inference to seed
        ;; the param type; only when present so a hintless arity stays a struct.
        arity (if (seq (:phints pp)) (assoc arity :phints (:phints pp)) arity)
        ;; a ^Record hint the program wrote: a declaration, guarded by the
        ;; optimizing passes rather than trusted (jolt.passes.types)
        arity (if (seq (:uphints pp)) (assoc arity :uphints (:uphints pp)) arity)
        ;; numeric param hints (name -> :long/:double) for jolt.passes.numeric.
        arity (if (seq (:nhints pp)) (assoc arity :nhints (:nhints pp)) arity)
        ;; array param hints (name -> :doubles/:longs/:ints/:bytes/:objects) for
        ;; jolt.passes.numeric: aget/aset over them lower to the unboxed flvector
        ;; path (:doubles) or the boxed-vector one (the rest).
        arity (if (seq (:ahints pp)) (assoc arity :ahints (:ahints pp)) arity)
        ;; inert primitive hints (name -> tag), for the JOLT_CHECK lint alone.
        ;; Nothing in codegen reads them — that is the point of reporting them.
        arity (if (seq (:dhints pp)) (assoc arity :dead-hints (:dhints pp)) arity)]
    ;; :rest only when variadic — an absent :rest reads back nil, same as before,
    ;; but keeps a fixed arity a nil-free struct rather than a phm.
    (if rst (assoc arity :rest rst) arity)))

;; analyze-arity over the stripped params, with the arglist's own return hint
;; attached. An explicit :ret-nhint already on the arity wins.
(defn- analyze-arity-with-ret [ctx params-form body env fn-name]
  (let [a (analyze-arity ctx (strip-arglist-meta params-form) body env fn-name)
        k (arglist-ret-nkind params-form)]
    (if (and k (not (:ret-nhint a))) (assoc a :ret-nhint k) a)))

;; Is the form's own head already the bare, meta-free symbol fn*? Then the form IS
;; what :src-form rebuilds and the rebuild can be skipped — see analyze-fn.
(defn- fn-head-canonical? [items]
  (let [h (first items)]
    (and (form-sym? h)
         (nil? (form-sym-ns h))
         (= "fn*" (form-sym-name h))
         (let [m (form-sym-meta h)] (or (nil? m) (zero? (count m)))))))

(defn- analyze-fn [ctx form items env]
  (let [named (form-sym? (nth items 1))
        fn-name (when named (form-sym-name (nth items 1)))
        rest-items (if named (drop 2 items) (drop 1 items))
        first* (strip-arglist-meta (first rest-items))
        node (cond
               (form-vec? first*)
               (fn-node fn-name [(analyze-arity-with-ret ctx (first rest-items) (rest rest-items)
                                                         env fn-name)])
               (form-list? first*)
               (fn-node fn-name
                        (check-fn-arities
                         rest-items
                         (mapv (fn [clause]
                                 (when-not (form-list? clause)
                                   (analysis-error :analyze/invalid-fn-parameters
                                                   (str "fn* clause " (pr-str clause)
                                                        " should be a list of a parameter vector and a body")))
                                 (let [cl (vec (form-elements clause))]
                                   (analyze-arity-with-ret ctx (first cl) (rest cl) env fn-name)))
                               rest-items)))
               :else (analysis-error :analyze/invalid-fn-parameters
                                     "Parameter declaration should be a vector"))
        ;; :src-form is the original post-expansion (fn* params body…) form —
        ;; what the image rebuilds from; :free-names the original names of the
        ;; locals it captures. Attached to named fns too (invisible to emission
        ;; unless read; the prelude mint ignores them).
        ;;
        ;; The rebuild exists to normalize a clojure.core-QUALIFIED head (a macro's
        ;; syntax-quote can produce one, and analyze-list* dispatches on either
        ;; spelling) down to the bare fn*. When the head is already bare the rebuild
        ;; produces a form equal to the one we were handed, so hand back the ORIGINAL
        ;; OBJECT instead. That is not a micro-optimization: a nested literal's
        ;; :src-form is then the very object sitting inside its parent's :src-form,
        ;; which is what lets the back end emit the parent's registration by pointing
        ;; at the nested one rather than walking into it again (fnsrc-flush). Rebuilt,
        ;; the two are equal and not identical, and the back end has no cheap way to
        ;; discover the relationship — jolt seqs do not cache their hash, so testing
        ;; equality costs the subtree the sharing was meant to save.
        node (assoc node
                    :src-form (if (fn-head-canonical? items) form (cons 'fn* (rest items)))
                    :free-names (fn-free-names (:arities node) fn-name))
        ;; ^{:once true} on the fn* symbol, as the reference's lazy-seq writes it:
        ;; the fn runs at most once, so its captures may be let go as it runs
        ;; (backend_scheme.clj emit-fn)
        hm (form-sym-meta (first items))
        node (if (and (map? hm) (get hm :once)) (assoc node :once true) node)]
    node))

;; class names that catch everything (the JVM root types); a (catch Throwable e …)
;; clause matches any thrown value unconditionally.
(def ^:private catch-all-names #{"Throwable" "java.lang.Throwable" "Object" "java.lang.Object"})

;; Fold one try clause into {:body :catches :finally :finally?}.
;; Clause ordering, like the JVM: body exprs, then catches, then at most one
;; finally which must be last. Enforced eagerly (plain throw) so a misplaced
;; clause is a compile error rather than a silently-relocated body form.
(defn- add-try-clause [acc c]
  (let [head (when (form-list? c) (first (vec (form-elements c))))
        hname (when (and head (form-sym? head)) (form-sym-name head))]
    (when (:finally? acc)
      (analysis-error :analyze/invalid-try
                      "finally clause must be last in try expression"))
    (cond
      (= hname "catch")
        (let [cl (vec (form-elements c))]
          ;; (catch class binding body*) — binding (3rd elem) MUST be a symbol.
          ;; Validate eagerly (plain throw, NOT uncompilable, so it's a real
          ;; error rather than a compile->interpret punt) instead of letting
          ;; form-sym-name crash on a non-symbol.
          (when (or (< (count cl) 3) (not (form-sym? (nth cl 2))))
            (analysis-error :analyze/invalid-try
                          "Unable to parse catch clause; expected (catch class binding body*)"))
          (update acc :catches conj cl))
      (= hname "finally")
        (assoc acc :finally? true :finally (rest (vec (form-elements c))))
      :else
        (do
          ;; a body expr after a catch is illegal — only catch/finally may follow.
          (when (seq (:catches acc))
            (analysis-error :analyze/invalid-try
                  "Only catch or finally clause can follow catch in try expression"))
          (update acc :body conj c)))))

;; The catch clauses as ONE form over the caught value `evar`: a nested-if chain
;; testing each clause's class in order, re-throwing when none matches.
(defn- catch-dispatch-form [evar catches]
  (reduce
    (fn [else cl]
      (let [cform (nth cl 1)
            bindsym (nth cl 2)
            bodyf (drop 3 cl)
            ;; clojure.core-qualified: a ns that excludes (or
            ;; :only-omits) let/or/instance? and defines its
            ;; own must still get core's here (jolt#1095).
            letform (cons 'clojure.core/let (cons (vector bindsym evar) bodyf))
            fullname (when (form-sym? cform) (form-sym-name cform))
            catch-all? (or (not (form-sym? cform))
                           (contains? catch-all-names fullname))]
        (if catch-all?
          letform
          (list 'if (list 'clojure.core/or
                          (list 'clojure.core/instance? cform evar)
                          (list 'clojure.core/__catch-broad? fullname evar))
                letform else))))
    (list 'throw evar)
    (reverse catches)))

(defn- analyze-try [ctx items env]
  (let [parts (reduce add-try-clause
                      {:body [] :catches [] :finally nil :finally? false}
                      (rest items))]
    ;; Multiple catch clauses dispatch on the thrown value's class, in order. Lower
    ;; them to ONE guard binding a fresh local, then a nested-if chain testing each
    ;; clause's class with (instance? C e) — which respects the exception supertype
    ;; hierarchy — plus __catch-broad? for an untyped host condition. No match
    ;; re-throws. (The earlier single-catch IR ignored the class and caught
    ;; everything; this gives real per-class dispatch.) :catch-sym/:catch-body/
    ;; :finally are added only when present — an absent key must stay absent (a
    ;; nil-valued key would make the node a phm and force back-end densification).
    ;; `recur` may not cross a try. On the JVM that is a bytecode limit; here the
    ;; lowering re-enters the handler scope each iteration without leaving the
    ;; previous one, so a recur that crossed one leaked ~400 bytes an iteration and
    ;; ran `finally` bodies LIFO at loop exit rather than per iteration — and with
    ;; no loop condition it simply hung. The target stays VISIBLE (with-recur in an
    ;; inner fn/loop lifts this again); marking the boundary rather than dropping
    ;; :recur is what lets the recur arm tell "no target at all" from "a target it
    ;; may not reach", which are different diagnostics.
    ;;
    ;; Body and catch/finally are marked differently because the reference reports
    ;; them differently: the body is "Cannot recur across try", while a catch or
    ;; finally is not a tail position in the first place and reports as such.
    (let [benv (assoc env :recur-blocked :try)
          tenv (assoc env :recur-blocked :non-tail)
          n {:op :try :body (analyze-seq ctx (:body parts) benv)}
          n (if (seq (:catches parts))
              (let [evar-name (gen-name "catch")
                    raw-name (gen-name "catch-raw")
                    evar (symbol evar-name)]
                (assoc n :catch-sym evar-name
                         :catch-raw-sym raw-name
                         :catch-body (analyze-seq ctx (list (catch-dispatch-form evar (:catches parts)))
                                                  (add-locals tenv [evar-name]))))
              n)
          n (if (:finally parts)
              (assoc n :finally (analyze-seq ctx (:finally parts) tenv))
              n)]
      n)))

;; letfn*: (letfn* [name1 fn1 name2 fn2 …] body*) — the special form Clojure's
;; letfn macro expands to (flat name/fn-form pairs, the fn forms already named).
;; The named local fns are MUTUALLY recursive, so bind every name into the env
;; BEFORE analyzing any fn form — each then resolves its siblings (and itself) as
;; locals. Emitted as a :let flagged :letrec so the back end lowers it to
;; `letrec*`; the interpreter's shared mutable env gives the same semantics.
(defn- analyze-letfn* [ctx items env]
  (let [bvec  (binding-vector items)
        n     (quot (count bvec) 2)
        names (mapv (fn [i] (form-sym-name (nth bvec (* 2 i)))) (range n))
        env*  (add-locals env names)
        binds (mapv (fn [i]
                      [(nth names i)
                       (analyze ctx (nth bvec (inc (* 2 i))) env*)])
                    (range n))]
    {:op :let :letrec true :bindings binds
     :body (analyze-seq ctx (drop 2 items) env*)}))

;; Clojure evaluates def metadata values as expressions: ^{:k (f)} stores the
;; result of (f), ^{:a some-fn} stores the fn value. Build an IR map that evaluates
;; each value at def time. A resolved :tag class name becomes the Class object
;; (jolt.host/jolt-class-for), as the JVM stores the Class itself in var meta;
;; primitive hints like `double` stay quoted symbols. nil when there's no
;; metadata, so a plain def keeps the cheap static path.
;; Strip ONE (quote x) layer if there is one, else return the form unchanged. Used
;; for :arglists, which arrives either already-data (derived by defn/defmacro) or
;; as the user's quoted literal.
(defn- unquote-form [v]
  (if (and (form-list? v)
           (let [es (vec (form-elements v))]
             (and (= 2 (count es)) (form-sym? (first es))
                  (= "quote" (form-sym-name (first es))))))
    (second (vec (form-elements v)))
    v))

(defn- def-meta-expr [ctx base env]
  (when (pos? (count base))
    (map-node (mapv (fn [p]
                      (let [k (first p) v (second p)]
                        ;; :arglists is DATA, so it is quoted rather than evaluated.
                        ;; Two shapes reach here: the parameter vectors defn/defmacro
                        ;; derive (a bare seq of vectors) and a user's explicit
                        ;; {:arglists '([x])} (a (quote …) form, since attr-map values
                        ;; are ordinarily evaluated). Quoting the latter as-is would
                        ;; store the quote form itself, so unwrap it first — one
                        ;; (quote …) layer either way.
                        [(const k)
                         (cond (= k :arglists) (quote-node (unquote-form v))
                               (and (= k :tag) (string? v))
                               (invoke (var-ref "jolt.host" "jolt-class-for") [(const v)])
                               (= k :tag) (quote-node v)
                               :else (analyze ctx v env))]))
                    (seq base)))))

;; Stamp a top-level def's source position onto both the node (:pos, for the
;; success checker / native stack traces) and its var metadata (:line/:column/
;; :file, like the JVM Compiler). pos is the reader's {:line :column (:file)} for
;; the ORIGINAL form. The position merges into the static :meta so a plain
;; (def x 1) carries it; when the def already evaluates metadata (a defn's
;; :arglists/:doc, or ^{:k (f)}), the :meta-expr is rebuilt from the merged meta so
;; position rides the evaluated path too. Non-def nodes (a cast/:invoke) keep the
;; :pos-only stamp.
(defn- stamp-def-pos [ctx env node pos]
  (if (and pos (= :def (:op node)))
    (let [new-meta (merge (or (:meta node) {}) pos)
          node (assoc node :pos pos :meta new-meta)]
      (if (:meta-expr node)
        (assoc node :meta-expr (def-meta-expr ctx new-meta env))
        node))
    (if pos (assoc node :pos pos) node)))

(defn- analyze-def [ctx items env]
  ;; The reference's arity rule: a docstring may sit between name and value, so
  ;; four elements are allowed only when the third is a string.
  (let [n (count items)]
    (when (< n 2)
      (analysis-error :analyze/invalid-def "Too few arguments to def"))
    (when (or (> n 4) (and (= n 4) (not (string? (nth items 2)))))
      (analysis-error :analyze/invalid-def "Too many arguments to def")))
  (let [name-sym (nth items 1)]
    ;; ^{:map} metadata reads as (def (with-meta name m) v): the metadata is a
    ;; runtime expression, so the interpreter evaluates the whole def.
    (when-not (form-sym? name-sym)
      ;; :arg 1 — the name is the second element of the def form. A keyword or a
      ;; number carries no reader metadata, so its position cannot come from the
      ;; form itself; the reporter recovers it by scanning the source, and this
      ;; says which element to scan to.
      (analysis-error :analyze/invalid-def
                      "First argument to def must be a Symbol"
                      {:jolt.error/arg 1
                       :jolt.error/note "the name must be a symbol"}))
    (if (< (count items) 3)
      ;; (def name) with no init (declare): intern + reserve the cell so a forward
      ;; reference resolves; the back end keys on :no-init.
      (let [nm (form-sym-name name-sym) cur (compile-ns ctx)]
        (host-intern! ctx cur nm)
        ;; keep the reader meta: (def ^:dynamic *x*) must be bindable before
        ;; its root is set (the backend emits set-var-meta! beside declare-var!).
        {:op :def :ns cur :name nm :no-init true
         :meta (or (form-sym-meta name-sym) {})})
      ;; (def name docstring value): docstring is form 2, value form 3 — matching
      ;; the interpreter, else the docstring is taken as the value.
      (let [nm (form-sym-name name-sym)
            cur (compile-ns ctx)
            has-doc (and (> (count items) 3) (string? (nth items 2)))
            val-form (nth items (if has-doc 3 2))
            base0 (or (form-sym-meta name-sym) {})
            ;; resolve a ^Type hint to its canonical class name at def time, as the
            ;; JVM compiler does (^String -> java.lang.String); unknown hints pass.
            ;; def-meta-expr turns the resolved name into the Class object at
            ;; runtime (the JVM stores the Class itself in var meta).
            tag (get base0 :tag)
            tag-name (cond (form-sym? tag) (form-sym-name tag)
                           (string? tag) tag
                           :else nil)
            base-meta (if tag-name
                        (let [c (resolve-class-hint tag-name)]
                          (if c (assoc base0 :tag c) base0))
                        base0)
            node-meta (if has-doc (assoc base-meta :doc (nth items 2)) base-meta)]
        (host-intern! ctx cur nm)
        ;; a ^double/^long return hint on the name applies to all arities of the fn.
        (let [node (def-node cur nm (with-ret-nhint (analyze ctx val-form env) (tag->nkind tag)) node-meta)
              me (def-meta-expr ctx node-meta env)]
          (if me (assoc node :meta-expr me) node))))))

(defn- analyze-set-member [ctx ti thead val-node env]
  (cond
    ;; (set! (.-field obj) v) and (set! (.field obj) v) both set an instance
    ;; field in Clojure; accept either. The "." static-field head is handled
    ;; by the next arm (it isn't a .-/.name field head).
    (and (> (count thead) 1) (= \. (first thead)))
    {:op :set-field :obj (analyze ctx (nth ti 1) env)
     :field (if (field-head? thead) (subs thead 2) (subs thead 1)) :val val-node}
    ;; (set! (. Class member) val) — a host static-field set. clojure.spec.alpha
    ;; toggles clojure.lang.RT/checkSpecAsserts this way. Lowered to a runtime
    ;; jolt.host/set-static-field! call (the read side is a :host-static ref).
    (and (= thead ".") (>= (count ti) 3) (form-sym? (nth ti 1)) (form-sym? (nth ti 2))
         (= :class (:kind (resolve-global ctx (nth ti 1)))))
    (invoke (var-ref "jolt.host" "set-static-field!")
            [(const (:name (resolve-global ctx (nth ti 1))))
             (const (form-sym-name (nth ti 2))) val-node])
    ;; (set! (. obj -field) val) and (set! (. obj field) val) — the two-part
    ;; spelling of the instance-field set above; a deftype method writes its
    ;; mutable fields as (set! (. this -field) v). After the static arm, so a
    ;; class name in the object position still means a static field.
    (and (= thead ".") (= 3 (count ti)) (form-sym? (nth ti 2)))
    (let [mname (form-sym-name (nth ti 2))]
      {:op :set-field :obj (analyze ctx (nth ti 1) env)
       :field (if (= \- (first mname)) (subs mname 1) mname) :val val-node})
    :else (analysis-error :analyze/invalid-set "Invalid assignment target")))

(defn- analyze-set-var [ctx target val-node env]
  (when (local? env (form-sym-name target))
    (analysis-error :analyze/invalid-set
                    (str "Cannot assign to non-mutable: " (form-sym-name target))))
  (let [r (resolve-global ctx target)]
    (when-not (= :var (:kind r)) (analysis-error :analyze/invalid-set "Invalid assignment target"))
    {:op :set-var :the-var (the-var (:ns r) (:name r)) :val val-node}))

;; (set! (.-field obj) v) mutates a deftype instance field in place; (set! *var* v)
;; sets the var's innermost thread binding through jolt.host/set-var! — Var.set,
;; which throws when there is none rather than establishing a root. A local target
;; (jolt binds fields immutably) or any other shape is uncompilable.
(defn- analyze-set! [ctx items env]
  (when-not (= 3 (count items))
    (analysis-error :analyze/invalid-set "Malformed assignment, expecting (set! target val)"))
  (let [target (nth items 1)
        val-node (analyze ctx (nth items 2) env)
        ti (when (form-list? target) (vec (form-elements target)))
        thead (when (and ti (pos? (count ti)) (form-sym? (first ti)))
                (form-sym-name (first ti)))]
    (cond
      thead (analyze-set-member ctx ti thead val-node env)
      (form-sym? target) (analyze-set-var ctx target val-node env)
      :else (analysis-error :analyze/invalid-set "Invalid assignment target"))))

;; (monitor-enter x) / (monitor-exit x) — the raw monitor ops, lowered to the same
;; identity-keyed per-object mutex `locking` takes, so the two compose. Both
;; evaluate to nil, matching the JVM (which emits a NIL after the monitor op).
(defn- analyze-monitor-op [ctx items env op]
  (when-not (= 2 (count items))
    (analysis-error :analyze/invalid-arity
            (str "Wrong number of args (" (dec (count items)) ") passed to: " op)))
  (invoke (var-ref "jolt.host" op) [(analyze ctx (nth items 1) env)]))

;; The special forms below each get a function of their own, so analyze-special
;; reads as the dispatch table it is. Each checks its form's SHAPE before reading
;; it: an (nth items i) past the end, or form-vec-items over a symbol, surfaced as
;; :analyze/internal-failure with a raw IndexOutOfBounds or "Don't know how to
;; create ISeq" for (throw), (def), (let* x 1) and (set!). The messages are the
;; reference compiler's own.

;; A quoted collection keeps its USER metadata (rewrite-clj coerces
;; '^:x (4 5 6) and expects the meta back). A list form is the one thing the
;; reader stamps with :line/:column/:file, and that is its own bookkeeping
;; rather than data, so it comes back off — but only there: on a vector, map,
;; set or symbol the only possible source of a position key is the program
;; that wrote it. The kept metadata is part of the literal, so quote it.
(defn- analyze-quote [ctx items env]
  (when-not (= 2 (count items))
    (analysis-error :analyze/invalid-arity
                    (str "Wrong number of args (" (dec (count items)) ") passed to quote")))
  (let [qf (second items)
        m (form-coll-meta qf)
        m (when (map? m)
            (let [u (if (form-list? qf) (dissoc m :line :column :file) m)]
              (when (seq u) u)))]
    ;; Quoting a live VALUE a macro spliced is the value — (quote 5) is
    ;; 5, and (quote <fn>) is that fn — so it rebuilds the same way an
    ;; unquoted one does rather than going to the quoted-data emitter,
    ;; which can only render what has reader syntax. embed-plan answers
    ;; nil for every ordinary form, so this asks one question instead of
    ;; restating the list of shapes analyze already recognizes.
    (if-let [plan (embed-plan qf)]
      (embedded-plan-node ctx plan env)
      (if (nil? m)
        (quote-node qf)
        (invoke (var-ref "clojure.core" "with-meta")
                [(quote-node qf) (quote-node m)])))))

;; 2 or 3 argument forms only (spec 03-special-forms X1)
(defn- analyze-if [ctx items env]
  (when (or (< (count items) 3) (> (count items) 4))
    (analysis-error :analyze/invalid-if
                    (if (< (count items) 3)
                      "Too few arguments to if"
                      "Too many arguments to if")))
  (if-node (analyze ctx (nth items 1) env)
           (analyze ctx (nth items 2) env)
           (if (> (count items) 3)
             (analyze ctx (nth items 3) env)
             (const nil))))

(defn- analyze-throw [ctx items env]
  (case (count items)
    1 (analysis-error :analyze/invalid-throw
                      "Too few arguments to throw, throw expects a single Throwable instance")
    2 (throw-node (analyze ctx (nth items 1) env))
    (analysis-error :analyze/invalid-throw
                    "Too many arguments to throw, throw expects a single Throwable instance")))

(defn- analyze-let* [ctx items env]
  (let [[pairs env*] (analyze-bindings ctx (binding-vector items) env)]
    (let-node pairs (analyze-seq ctx (drop 2 items) env*))))

(defn- analyze-loop* [ctx items env]
  (let [bvec (binding-vector items)
        rname (gen-name "loop")
        [pairs env*] (analyze-bindings ctx bvec env)
        body-env (assoc (with-recur env* rname (quot (count bvec) 2)
                                    (mapv (fn [[nm _]] (get (:jprims env*) nm)) pairs))
                        :recur-locals (mapv first pairs))
        ;; The other recur-target root (see analyze-arity): a recur
        ;; against THIS loop may only sit in tail position of its body.
        body-node (analyze-seq ctx (drop 2 items) body-env)]
    (check-recur-tails! body-node)
    {:op :loop :bindings pairs :body body-node}))

;; A recur onto a ^boolean loop local must pass something that can be a boolean;
;; the reference rejects a provably non-boolean argument at compile time.
(defn- check-recur-prims! [env args]
  (doseq [[i prim] (map-indexed vector (:recur-prims env))
          :when (= :boolean prim)
          :let [had (jvm-non-boolean-class (nth args i) env)]
          :when had]
    (analysis-error :analyze/invalid-recur
                    (str " recur arg for primitive local: "
                         (nth (:recur-locals env) i)
                         " is not matching primitive, had: " had
                         ", needed: boolean"))))

(defn- analyze-recur [ctx form items env]
  (let [arity (:recur-arity env)
        n (dec (count items))]
    (when-not (:recur env)
      (analysis-error :analyze/invalid-recur
                      "Cannot recur here: no enclosing loop or fn"))
    ;; A target exists but a `try` stands between here and it. Reported
    ;; before the arity check because the arity is beside the point when
    ;; the recur cannot happen at all, and with the reference's own two
    ;; messages: a try body may not be recurred ACROSS, while a catch or
    ;; finally is not a tail position to begin with.
    (case (:recur-blocked env)
      :try (analysis-error :analyze/invalid-recur "Cannot recur across try")
      :non-tail (analysis-error :analyze/invalid-recur
                                "Can only recur from tail position")
      nil)
    (when (and arity (not= n arity))
      (analysis-error :analyze/invalid-recur
                      (str "Mismatched argument count to recur, expected: " arity
                           " args, got: " n)))
    ;; Carry the recur's own source position: the tail-position check is
    ;; a pass over the finished tree, long after the position box has
    ;; moved on, so the node is the only thing that still knows where
    ;; this recur was written. nil for a macro-built recur with no
    ;; reader metadata, which falls back to the enclosing form.
    (let [args (mapv #(analyze ctx % env) (rest items))
          _ (check-recur-prims! env args)
          node {:op :recur :args args}
          p (form-position form)]
      (if p (assoc node :pos p) node))))

;; (var sym): the var itself, resolved at compile time. A non-symbol
;; argument and an unresolvable symbol are distinct errors; the latter uses
;; the reference wording. Note (var String) succeeds where the JVM refuses:
;; jolt holds imported classes in vars, so the symbol resolves as :var.
(defn- analyze-var [ctx items]
  (let [sym (second items)]
    (when-not (form-sym? sym)
      (analysis-error :analyze/invalid-var-reference
                      (str "The argument to `var` must be a symbol, got: " (pr-str sym))))
    (let [r (resolve-global ctx sym)]
      (if (= :var (:kind r))
        (the-var (:ns r) (:name r))
        (analysis-error :analyze/unresolved-var
                        (str "Unable to resolve var: "
                             (if-let [ns (form-sym-ns sym)] (str ns "/") "")
                             (form-sym-name sym) " in this context"))))))

;; A defmacro that is not top-level (the spine intercepts those) — e.g. one
;; produced by a macro like (when … (defmacro …)). Lower it the way the spine
;; does: def the expander fn, then mark the var a macro at runtime so later
;; forms expand it. Strip a leading docstring / attr-map, as defmacro allows.
(defn- analyze-defmacro [ctx items env]
  (let [name-sym (nth items 1)
        nm (form-sym-name name-sym)
        cur (compile-ns ctx)
        after (drop 2 items)
        [after doc] (if (string? (first after)) [(rest after) (first after)] [after nil])
        [after attr] (if (form-map? (first after)) [(rest after) (first after)] [after nil])
        ;; …and a TRAILING attr-map, which defn takes too (the JVM's
        ;; defmacro hands it the whole fdecl, that map included). It
        ;; used to run through the arity lowering, becoming a bogus
        ;; ([&form &env]) clause and a [:k 1] entry in :arglists.
        [after trail] (macro-trail-attr after)
        ;; build (fn params body…) and analyze it through the fn MACRO
        ;; so a destructuring macro arglist desugars (the fn* primitive
        ;; would not), then def it and mark the var a macro. Head with
        ;; the QUALIFIED clojure.core/fn so it resolves to the real fn
        ;; macro even when the macro being defined is `fn` (schema/s/fn)
        ;; or the ns excluded it.
        fn-form (cons (symbol "clojure.core" "fn") (macro-fn-arities after))
        ;; var meta like defn: ^meta on the name, docstring, attr-map, arglists
        arglists (cond (empty? after) nil          ; (defmacro m) declares none
                       (form-list? (first after)) (map first after)
                       :else (list (first after)))
        ;; the derived arglists is a DEFAULT: an explicit :arglists in
        ;; the attr-map (or on the name) overrides it, as defn allows.
        ;; Merging it last instead silently discarded the user's value.
        ;; Precedence is DEFN'S, because on the JVM this form becomes a
        ;; defn: name metadata < the derived arglists < docstring <
        ;; leading attr-map < trailing attr-map. So ^{:arglists …} on
        ;; the NAME does not override (the JVM ignores it there),
        ;; {:arglists …} in an attr-map does, and an attr-map's :doc
        ;; beats the docstring — which is what jolt's own defn already
        ;; did while this arm had the docstring winning.
        base (merge (or (form-sym-meta name-sym) {})
                    (if arglists {:arglists arglists} {})
                    (if doc {:doc doc} {})
                    (or attr {})
                    (or trail {}))
        meta-expr (def-meta-expr ctx base env)]
    (host-intern! ctx cur nm)
    (merge {:op :defmacro :ns cur :name nm
            :fn (analyze ctx fn-form env)}
           (if meta-expr {:meta-expr meta-expr} {:meta base}))))

(defn- analyze-special [ctx op form items env]
  (case op
    "quote"         (analyze-quote ctx items env)
    "if"            (analyze-if ctx items env)
    "do"            (analyze-seq ctx (rest items) env)
    "throw"         (analyze-throw ctx items env)
    "def"           (analyze-def ctx items env)
    "let*"          (analyze-let* ctx items env)
    "loop*"         (analyze-loop* ctx items env)
    "recur"         (analyze-recur ctx form items env)
    "try"           (analyze-try ctx items env)
    ;; (monitor-enter x) / (monitor-exit x) — the bare halves of `locking`, which
    ;; is a macro over the same jolt.host per-object monitor. Libraries that
    ;; expand their own locking macro rather than calling clojure.core/locking
    ;; emit these directly (borkdude/dynaload does, under malli), so lower them
    ;; to that seam instead of punting. Like the JVM, both yield nil.
    "monitor-enter" (analyze-monitor-op ctx items env "monitor-enter")
    "monitor-exit"  (analyze-monitor-op ctx items env "monitor-exit")
    "letfn*"        (analyze-letfn* ctx items env)
    "fn*"           (analyze-fn ctx form items env)
    ;; Lower the backtick to construction code (zero runtime cost), then analyze
    ;; it — the macroexpand/compile-time step, per read -> macroexpand -> compile.
    "syntax-quote"  (analyze ctx (form-syntax-quote-lower ctx (second items)) env)
    "var"           (analyze-var ctx items)
    "defmacro"      (analyze-defmacro ctx items env)
    "set!"          (analyze-set! ctx items env)
    (analysis-error :analyze/unsupported-special-form
                    (str "Unsupported special form: " op))))

;; The list form's source position on a node, when the reader recorded one —
;; the same stamp an :invoke carries. A host call in tail position stores it as
;; its trace site (backend sited host call), and it is what a report names when
;; the host raises from inside the call.
(defn- stamp-pos [node form]
  (let [p (form-position form)]
    (if p (assoc node :pos p) node)))

(defn- analyze-host-call [ctx hname items env]
  (when (< (count items) 2)
    (analysis-error :analyze/invalid-member-access
      (str "Malformed member expression, expecting (.method target ...): " hname)))
  (let [raw (nth items 1)
        target (analyze ctx raw env)]
    (cond-> {:op :host-call
             :method (subs hname 1)
             :target target
             :args (mapv #(analyze ctx % env) (drop 2 items))}
      (str-target-type raw target) (assoc :target-type :str)
      (kw-target-type raw target) (assoc :target-type :kw)
      (sb-target-type raw target) (assoc :target-type (sb-target-type raw target)))))

;; `(Class. args*)` and `(new Class args*)` -> a :host-new node carrying the class
;; token and the analyzed args. The Chez back end lowers it to a runtime
;; constructor dispatch.
(defn- analyze-ctor [ctx class args env]
  ;; Qualify a bare (Name. …) to its deftype's FQN when THIS ns defined the deftype,
  ;; so a deftype named like a built-in host class (tools.reader's PushbackReader)
  ;; resolves to the deftype here while an unrelated ns's bare (PushbackReader. …)
  ;; still reaches java.io.PushbackReader.
  (host-new (or (deftype-ctor-class ctx class) class)
            (mapv #(analyze ctx % env) args)))

;; jolt.ffi/__ccallable: the foreign-CALLBACK form (via the jolt.ffi/foreign-callable
;; macro) — the inverse of __cfn. It wraps a jolt fn as a C-callable function
;; pointer so C can call back INTO jolt (GTK signal handlers, qsort comparators).
;; Shape:
;;   (jolt.ffi/__ccallable f [:argtype ...] :rettype)                ; thread stays active
;;   (jolt.ffi/__ccallable f [:argtype ...] :rettype :collect-safe)  ; may be invoked
;;                                                                   ; on a thread that is
;;                                                                   ; not active at the time
;; Unlike __cfn, the fn is a CHILD expression (analyzed + walked by the passes);
;; the types are literal keywords read at compile time. The Chez back end lowers
;; it to a locked `foreign-callable` and returns its entry-point address (a jolt
;; pointer). :collect-safe is required when the callback can arrive on a thread
;; that is not ACTIVE: one the runtime never started, or one in a :blocking call.
(defn- analyze-ffi-callable [ctx items env]
  (when-not (<= 4 (count items) 5)
    (analysis-error :ffi/invalid-signature (str "jolt.ffi/foreign-callable expects (foreign-callable f [argtypes] rettype [:collect-safe])")))
  {:op :ffi-callable
   :fn (analyze ctx (nth items 1) env)
   :argtypes (mapv name (form-vec-items (nth items 2)))
   :rettype (name (nth items 3))
   :collect-safe (and (= 5 (count items)) (= "collect-safe" (name (nth items 4))))})

;; The `.` special form: `(. target member arg*)` — member access / method call.
;; A symbol member whose name starts with "-" is a field read; otherwise it is a
;; method (call with the trailing args). Both lower to a :host-call carrying the
;; member name verbatim (the leading "-" survives so the runtime dispatcher reads
;; it as a field). The Chez back end dispatches it through record-method-dispatch.
(defn- analyze-dot [ctx items env]
  ;; The missing-member form names the shape it wanted, as the reference does;
  ;; the two-element (. 1 5), which HAS a member that is just not one, is the one
  ;; that says only "Malformed member expression".
  (when (< (count items) 3)
    (analysis-error :analyze/invalid-member-access
                    "Malformed member expression, expecting (. target member ...)"))
  (let [member0 (nth items 2)
        ;; (. target (member arg*)) is sugar for (. target member arg*) —
        ;; flatten the list-member form so the rest of the dispatch is uniform.
        items (if (form-list? member0)
                (let [ml (vec (form-elements member0))]
                  (into [(nth items 0) (nth items 1) (first ml)] (rest ml)))
                items)
        target (nth items 1)
        member (nth items 2)
        ;; (. Class method args*) with a class target is a static call —
        ;; equivalent to (Class/method args*). resolve-global tags a class
        ;; symbol :kind :class; a local of the same name shadows it. The value
        ;; classes (Long/Integer/String) self-evaluate through a clojure.core var,
        ;; so they resolve :var, not :class — host-class-name? catches those, so
        ;; (. Long parseLong x) dispatches statically like (Long/parseLong x).
        class-target (when (and (form-sym? target)
                                (not (local? env (form-sym-name target))))
                       (let [nm (form-sym-name target)
                             r (resolve-global ctx target)]
                         (cond
                           (= :class (:kind r)) (:name r)
                           (host-class-name? nm) nm)))]
    (cond
      ;; (. Class -MEMBER) is an EXPLICIT static field read — the leading dash is
      ;; the field spelling, so drop it and take the value without invoking.
      (and class-target (form-sym? member)
           (> (count (form-sym-name member)) 1)
           (= "-" (subs (form-sym-name member) 0 1)))
        (host-static class-target (subs (form-sym-name member) 1))
      ;; (. Class MEMBER) with no arguments is ambiguous, as it is on the JVM: a
      ;; static field if one exists, else a no-arg static method call. jolt keeps
      ;; one registry for both, so the decision happens at runtime on what is
      ;; registered. With arguments it is unambiguously a call.
      (and class-target (form-sym? member) (empty? (drop 3 items)))
        (invoke (var-ref "jolt.host" "static-member")
                [(const class-target) (const (form-sym-name member))])
      (and class-target (form-sym? member))
        (invoke (host-static class-target (form-sym-name member))
                (mapv #(analyze ctx % env) (drop 3 items)))
      (form-sym? member)
        {:op :host-call
         :method (form-sym-name member)
         :target (analyze ctx target env)
         :args (mapv #(analyze ctx % env) (drop 3 items))}
      ;; (. obj :kw) is a keyword lookup — invoke the keyword on the target.
      (form-keyword? member)
        (invoke (analyze ctx member env) [(analyze ctx (nth items 1) env)])
      :else (analysis-error :analyze/invalid-member-access
                            "Malformed member expression"))))

(defn- analyze-field [ctx hname items env]
  (when (< (count items) 2)
    (analysis-error :analyze/invalid-member-access "Malformed (.-field target) form"))
  ;; Only the no-argument form is a field read. Clojure treats (.-name target
  ;; args*) as a call of the member named "-name" -- which is how a protocol
  ;; method whose name genuinely begins with a dash gets invoked, e.g.
  ;; cognitect aws-api's (.-invoke-async client op-map). Dropping those args
  ;; silently read the field instead and then called it one argument short.
  {:op :host-call
   :method (subs hname 1)        ; ".-field" -> "-field"
   :target (analyze ctx (nth items 1) env)
   :args (mapv #(analyze ctx % env) (drop 2 items))})

;; A macro referenced in VALUE position — a bare reference or an argument, never
;; the head of a call (a macro head macroexpands in analyze-list before this is
;; reached) — is a real compile error on the JVM: "Can't take value of a macro:
;; #'ns/name". Plain throw (not uncompilable) so it surfaces rather than punting
;; to an interpreter fallback. #'foo and (var foo) of a macro are handled in
;; `analyze` (form-var-value?) and analyze-special ("var") and never reach here.
(defn- deny-macro-value [ctx form r]
  (when (form-macro? ctx form)
    (analysis-error :analyze/invalid-macro-value
           (str "Can't take value of a macro: #'" (:ns r) "/" (:name r)))))

;; A ^:private var of another namespace cannot be referenced at all, as on the
;; JVM (jolt#1095): resolve-global tags one :private, and this throws the
;; reference's IllegalStateException wording. A call head or macro gets
;; Compiler.isInline / isMacro's "var: #'a/hidden is not public"; a value gets
;; resolveIn's, which prints the symbol as written. (var a/hidden) is exempt —
;; analyze-special's "var" arm never calls this — so @#'a/hidden still works.
(defn- deny-private [form r head?]
  (when (:private r)
    (analysis-error :analyze/private-var
                    (str "var: "
                         (if head?
                           (str "#'" (:ns r) "/" (:name r))
                           (str (if-let [ns (form-sym-ns form)] (str ns "/") "") (form-sym-name form)))
                         " is not public")
                    {:jolt.error/symbol (str (:ns r) "/" (:name r))})))

;; instance? is a macro on jolt (so it can quote a bare class name — the class
;; model has no evaluable Class for every name), but the JVM has it as a plain fn,
;; so a value-position reference — (partial instance? SomeClass), (map (partial
;; instance? String) xs) — must yield a callable. clojure.core/instance-check is
;; the 2-arg (class-value obj) runtime fn the macro expands to, and a self-
;; evaluating java.*/clojure.lang.* class name evaluates to a usable class value,
;; so resolve the value-position ref to it. (A bare deftype/shim name passed as a
;; value still can't produce a class value — that residual keeps the macro's
;; known divergence.) Returns the node to emit, or nil to fall through.
(defn- macro-value-fn [ctx form r]
  (when (and (form-macro? ctx form)
             (= "clojure.core" (:ns r)) (= "instance?" (:name r)))
    (var-ref "clojure.core" "instance-check")))

;; A symbol that resolved to a var: the var's node, unless it names a macro or a
;; private var of another namespace.
(defn- var-symbol-node [ctx form r]
  (or (macro-value-fn ctx form r)
      (do (deny-private form r false)
          (deny-macro-value ctx form r)
          (cond-> (var-ref (:ns r) (:name r)) (:num-ret r) (assoc :num-ret (:num-ret r))))))

(defn- analyze-qualified-symbol [ctx form nm ns env]
  (cond
    ;; Qualified instance method (Clojure 1.12) used as a value: a fn of the
    ;; target and the method's arguments.
    (and (> (count nm) 1) (= "." (subs nm 0 1)))
      (analyze ctx (method-value-form ns nm (get (form-sym-meta form) :param-tags)) env)
    ;; `Class/new` used as a value — a fn over the constructor — unless the ns
    ;; half is a namespace that really has a var named `new`.
    (and (= nm "new") (not= :var (:kind (resolve-global ctx form))))
      (analyze ctx (method-value-form ns nm (get (form-sym-meta form) :param-tags)) env)
    ;; `Component/N` — an array class symbol (Clojure 1.12): the Class object,
    ;; as any class symbol evaluates to.
    (and (array-class-dims nm) (not= :var (:kind (resolve-global ctx form))))
      (let [cn (array-class-name ctx ns (array-class-dims nm))]
        (if cn
          (invoke (var-ref "jolt.host" "jolt-class-for") [(const cn)])
          ;; the reference's ClassNotFoundException, which the position box
          ;; (as-analysis-diagnostic) carries as the cause of the positioned
          ;; compile error, as a CompilerException carries it on the JVM
          (throw (ClassNotFoundException. (str "Unable to resolve component classname: " ns)))))
    :else
    (let [r (resolve-global ctx form)]
      (if (= :var (:kind r))
        (var-symbol-node ctx form r)
        ;; A non-var qualified ref `Class/member` is a host class static
        ;; (Math/sqrt, Long/MAX_VALUE, System/getenv). The Chez back end
        ;; lowers it to a runtime static dispatch.
        ;;
        ;; ...but a namespace-shaped ns that is not loaded AT ALL cannot be a
        ;; class either, and reporting it as one is how `(no.such.ns/foo 1)`
        ;; came back "Unknown class no.such.ns" from inside the call, naming a
        ;; class the program never mentioned. resolve-global cannot draw the
        ;; line itself: it answers :unresolved for `Math/sqrt` and for
        ;; `no.such.ns/foo` alike, because looking the name up and finding
        ;; nothing is all it can say. So check the shape, and report the JVM's
        ;; "No such namespace" (Compiler.resolveIn) at COMPILE time — which is
        ;; where a forgotten `require` or a typo in the ns half belongs.
        ;;
        ;; A LOADED namespace merely missing the var is the JVM's compile-time
        ;; "No such var" when the form runs as soon as it compiles (load,
        ;; require, eval: jolt.host/late-bound-namespace?). Left to the static
        ;; arm, the ns half was looked up again when the call RAN, against
        ;; whatever *ns* was current then, so an alias in a -main read as a
        ;; class: `(s/nope 1)` reported "Unknown class s".
        ;;
        ;; Everywhere else it stays on the host-static arm, and deliberately.
        ;; jolt-core and stdlib reach their host contract
        ;; this way on purpose — several hundred `jolt.host/…` references, and
        ;; jolt.ffi's and jolt.time's — and which vars exist depends on which host
        ;; files the current boot loaded: jolt.main names jolt.host/build-library
        ;; in a world where build.ss may not be loaded and the call cannot run.
        ;; Late binding is the point there, so the compile-time guard for that
        ;; namespace is a STATIC one (jolt-host-manifest.txt, pinned against
        ;; both the def-var! sites and every jolt-core reference by
        ;; manifestcheck), and the miss reports at the call, correctly, through
        ;; static-miss-message's live-namespace arm: "No such var: ns/name",
        ;; not "Unknown class".
        ;;
        ;; jolt#879 was in this second group — jolt.host loaded, `getenv` not
        ;; yet — so it is the seed's own gates that hold it: manifestcheck
        ;; rejects a namespace-shaped host-static in the minted seed, and
        ;; `make seeddefs` asserts every var the seed defines survives the load.
        ;;
        ;; static-class-ns draws both lines.
        (host-static (static-class-ns ctx ns nm) nm)))))

(defn- analyze-global-symbol [ctx form nm env]
  (let [r (resolve-global ctx form)]
    (case (:kind r)
      ;; :num-ret (a ^double/^long declared return) rides on the var node so
      ;; jolt.passes.numeric types a call to it (an accumulator over the result).
      :var (var-symbol-node ctx form r)
      :host (host-ref (:name r))
      ;; a class-name symbol (java.util.Map) self-evaluates to an interned
      ;; Class object via the runtime interner, so (= (class x) java.util.Date),
      ;; identity, and defmethod dispatch keys are all stable.
      :class (invoke (var-ref "jolt.host" "jolt-class-for") [(const (:name r))])
      ;; :unresolved — throw "Unable to resolve symbol", matching JVM
      ;; Clojure, wherever the symbol appears: at the compilation-unit
      ;; top level AND inside a scope (fn / loop / let body). The check
      ;; used to stop at the first enclosing scope, which turned a typo
      ;; in a nested body into a late-bound var whose unbound root then
      ;; blew up at runtime on whichever reference was used first —
      ;; naming a symbol that wasn't even the misspelled one.
      ;; Legitimate forward references are unaffected: declare and
      ;; (def name) intern a resolvable cell, so they resolve as :var,
      ;; as do the clojure.core primitives the host declares up front.
      ;; clojure.core/*allow-unresolved-vars* — the JVM's
      ;; RT.ALLOW_UNRESOLVED_VARS, read here the way Compiler.resolveIn
      ;; reads it — late-binds the name instead, for a program that
      ;; binds it itself. jolt's REPLs do not: nREPL, `jolt repl` and
      ;; -e are strict like the JVM's, and a name that arrives in a
      ;; later eval needs a declare.
      (if (allow-unresolved-vars?)
        (var-ref (compile-ns ctx) nm)
        (resolve-error ctx nm env)))))

(defn- analyze-symbol [ctx form env]
  (let [nm (form-sym-name form) ns (form-sym-ns form)]
    (cond
      (and (nil? ns) (local? env nm))
        (let [h (get (:hints env) nm)] (if h (assoc (local nm) :hint h) (local nm)))
      ns (analyze-qualified-symbol ctx form nm ns env)
      :else (analyze-global-symbol ctx form nm env))))

;; The special form a call head names, or nil. A special-form head may arrive
;; clojure.core-qualified: syntax-quote namespace-qualifies a macro like `letfn`
;; to `clojure.core/letfn` (matching Clojure, where it is a macro), so a
;; macro-emitted (clojure.core/letfn …) must still dispatch to the special form.
;; A `qualified-only` head is the reverse: bare, it is just a name.
(defn- special-form-name [head hname]
  (or (when-not (contains? qualified-only hname) hname)
      (when (and (form-sym? head)
                 (= "clojure.core" (form-sym-ns head))
                 (contains? handled (form-sym-name head)))
        (form-sym-name head))))

;; Does this head actually resolve to the clojure.core var it is named
;; after? `shadowed` sees only LOCALS, so it cannot answer that: a
;; namespace-level (defn byte …) or (defn + …) owns the name in call
;; position, with or without :refer-clojure :exclude, and rewriting
;; such a call to a cast or an unchecked op calls the wrong function.
;;
;; This is the test backend_scheme/native-op already makes on the
;; resolved :var node before lowering first/aset/+ to a primitive, so
;; the two lowering layers now agree about who owns a name — and a
;; user-defined `first` and a user-defined `double` behave alike. A
;; head written clojure.core/-qualified (syntax-quote emits those)
;; names its ns outright and needs no lookup.
;;
;; Called only once unchecked-arith / num-cast have matched a name AND
;; an arity, so an ordinary call — every other list head in the
;; program — never pays for the resolution.
(defn- core-head? [ctx head]
  (or (and (form-sym? head) (= "clojure.core" (form-sym-ns head)))
      (let [r (resolve-global ctx head)]
        (and (= :var (:kind r)) (= "clojure.core" (:ns r))))))

;; The name a core op is called by, for the unchecked-math and cast rewrites:
;; the op may arrive bare (+) or clojure.core-qualified (clojure.core/*), the
;; latter from a macro's syntax-quote — both must rewrite. A shadowed bare head
;; is the local's, not core's.
(defn- core-op-name [head hname shadowed]
  (cond (and hname (not shadowed)) hname
        (and (form-sym? head) (= "clojure.core" (form-sym-ns head)))
        (form-sym-name head)))

;; What `rewrite` (unchecked-arith or num-cast) answers for the call, or nil when
;; it names no such op at this arity or the head is not core's.
(defn- core-rewrite [ctx head opn rewrite n]
  (let [r (when opn (rewrite opn n))]
    (when (and r (core-head? ctx head)) r)))

;; Stamp the form's source offset onto a top-level def so the back end can
;; register it (jv$ns/name -> source) for native stack traces.
(defn- stamp-def [ctx env form node]
  (let [p (form-position form)]
    (if (and p (= :def (:op node))) (stamp-def-pos ctx env node p) node)))

;; defn/defn- expand to (def name (fn …)); carry the ORIGINAL form's
;; source offset onto the resulting def, since the macro builds a fresh
;; (def …) with no metadata. So the back end can register fn defs.
;; The box covers ANALYSIS of the expansion, not the macro function's
;; own run: an error the macro itself raises (destructure rejecting a
;; binding vector, defn rejecting a clause) is about code the user
;; WROTE, and naming the macro there would tell them their own `let`
;; is the problem.
;; The RUN of the expander is marked separately (macro-run box): a
;; throw out of it is filed under the reference's macro phases and
;; names the macro, which is a different thing from the note above.
(defn- analyze-macro-call [ctx head form env]
  (let* [_ (deny-private head (resolve-global ctx head) true)
         mbox *macro-run-box*
         mprev (when mbox @mbox)
         _ (when mbox (reset! mbox [(macro-symbol ctx head) head form]))
         expanded (form-expand-1 ctx form (amp-env-map env))
         _ (when mbox (reset! mbox mprev))
         ebox *expansion-box*
         prev (when ebox @ebox)
         ;; fn, let, loop and letfn expand to their own special form
         ;; (fn → fn*) around the code the user wrote, and Clojure
         ;; documents them AS special forms. A diagnostic on that fn*
         ;; is about the user's form, so no expansion is recorded and
         ;; the report says nothing about "expanding the `fn` macro".
         ;; An enclosing user macro's record stays in force. Keyed on
         ;; the resolved core macro, not on the foo → foo* shape: a
         ;; user macro expanding to its own foo* keeps its note.
         own-sf? (contains? '#{clojure.core/fn clojure.core/let
                               clojure.core/loop clojure.core/letfn}
                            (macro-symbol ctx head))
         _ (when (and ebox (not own-sf?)) (reset! ebox [(form-sym-name head) form]))
         node (analyze ctx expanded env)
         _ (when ebox (reset! ebox prev))]
    (stamp-def ctx env form node)))

;; The jolt.ffi special form a head names: always emitted fully-qualified by
;; the jolt.ffi macros, so aliases resolve.
(defn- ffi-form-name [head]
  (when (and (form-sym? head) (= "jolt.ffi" (form-sym-ns head))
             (contains? #{"__cfn" "__layout" "__ccallable"} (form-sym-name head)))
    (form-sym-name head)))

(defn- analyze-ffi-form [ctx fname items env]
  (case fname
    ;; jolt.ffi/__cfn — the foreign-function special form (the jolt.ffi/foreign-fn
    ;; macro).
    "__cfn" (analyze-ffi-fn ctx items env)
    ;; jolt.ffi/layout expands to this literal-only leaf. The back end
    ;; asks Chez for the actual ABI size, alignment and offsets.
    "__layout" (analyze-ffi-layout items)
    ;; jolt.ffi/__ccallable — the foreign-callback special form (the fn is a
    ;; child expression, analyzed here).
    "__ccallable" (analyze-ffi-callable ctx items env)))

;; Interop through an unqualified, unshadowed head, or nil when the head is no
;; interop form.
(defn- analyze-bare-interop [ctx hname form items env]
  (cond
    (method-head? hname)
      (stamp-pos (analyze-host-call ctx hname items env) form)
    ;; (Class. args*) — trailing-dot constructor sugar.
    (ctor-head? hname)
      (analyze-ctor ctx (subs hname 0 (dec (count hname))) (rest items) env)
    ;; (new Class args*) — explicit constructor.
    (and (= hname "new") (>= (count items) 2) (form-sym? (nth items 1)))
      (analyze-ctor ctx (form-sym-name (nth items 1)) (drop 2 items) env)
    ;; (. target member arg*) — the `.` special form.
    (= hname ".")
      (stamp-pos (analyze-dot ctx items env) form)
    ;; (.-field target) — field-access head.
    (field-head? hname)
      (stamp-pos (analyze-field ctx hname items env) form)
    (form-special? hname)
      (analysis-error :analyze/unsupported-special-form
                       (str "Unsupported special form: " hname))
    :else nil))

;; Interop through a namespace-qualified head, or nil when the head is no
;; interop form (a call through a var).
(defn- analyze-qualified-interop [ctx head form items env]
  (let [n (form-sym-name head)
        cns (form-sym-ns head)]
    (cond
      ;; (ns/Name. args*) — a QUALIFIED trailing-dot constructor (a cross-ns or
      ;; aliased deftype, e.g. sci.impl.types/Reified.). hname is nil for a
      ;; namespaced head, so the bare ctor-head? arm never sees it;
      ;; reconstruct the "ns/Name" class and let analyze-ctor resolve it.
      (and (> (count n) 1) (= "." (subs n (dec (count n)))))
        (analyze-ctor ctx (str cns "/" (subs n 0 (dec (count n)))) (rest items) env)
      ;; (Class/new arg*) — the qualified constructor (Clojure 1.12), the same
      ;; call as (Class. arg*). A namespace with a var named `new` keeps its call.
      (and (= "new" n) (not= :var (:kind (resolve-global ctx head))))
        ;; an alias or loaded namespace is not a class: (s/new 1) is the JVM's
        ;; compile-time "No such var: s/new", not "No matching ctor found for
        ;; class s" when it runs. A deftype reached through its namespace or an
        ;; alias (other.ns/Point. spelled other.ns.Point/new) is a class.
        (do (when-not (deftype-ctor-class ctx cns)
              (static-class-ns ctx cns "new"))
            (analyze-ctor ctx cns (rest items) env))
      ;; (Class/.method target arg*) — qualified instance method call (Clojure 1.12).
      ;; The class part is a type hint; dispatch on the first arg's runtime type.
      (and (> (count n) 1) (= "." (subs n 0 1)))
        (stamp-pos (analyze-host-call ctx n items env) form)
      ;; (Class/MEMBER) with no arguments carries the same ambiguity that
      ;; (. Class MEMBER) does, and Clojure reads it as a static field when one
      ;; exists: (Math/PI), (Integer/MAX_VALUE) and (Locale/US) all evaluate to
      ;; the field. Resolve it at runtime through the same seam the dot form
      ;; uses, instead of invoking whatever the member holds, which failed
      ;; outright when that was a value rather than a no-arg method. A var head
      ;; is excluded so a zero-arg (some.ns/f) call is untouched.
      (and (empty? (rest items))
           (not= :var (:kind (resolve-global ctx head))))
        ;; ...and a head whose ns half is an alias or a loaded namespace is a
        ;; var reference, so (s/nope) is "No such var: s/nope" at compile time
        ;; as (s/nope 1) is, not "Unknown class s" when it runs.
        (invoke (var-ref "jolt.host" "static-member")
                [(const (static-class-ns ctx cns n))
                 (const n)])
      :else nil)))

;; A clojure.core numeric cast (bare or clojure.core/-qualified, the latter from
;; syntax-quote) becomes a checked :coerce node.
(defn- analyze-cast [ctx cast form items env]
  (let [node (coerce-node (:kind cast) (analyze ctx (second items) env)
                          (:cast-fn cast))
        p (form-position form)]
    (if p (assoc node :pos p) node)))

(defn- analyze-invoke [ctx head form items env shadowed]
  ;; a call through a var the compile-time hook rewrites (jolt.host/
  ;; *invoke-rewrite*, bound by jolt.loader around a context's
  ;; source): the head is a symbol, not a local, and resolves to a
  ;; var, and the replacement analyzes as if written here. Last, so
  ;; a macro-emitted (clojure.core/require …) is seen and a special
  ;; form never is.
  (if-let [rw (and (form-sym? head) (not shadowed)
                   (invoke-rewrite ctx head form))]
    (analyze ctx rw env)
    ;; stamp the list form's source offset onto the :invoke
    ;; so the success checker can report file:line:col. nil when the
    ;; reader did not record it (synthetic/macro-built forms).
    ;; qualified heads only: an unqualified one resolves again in
    ;; analyze-symbol, which applies the same check.
    (let [_ (when (and (form-sym? head) (form-sym-ns head))
              (deny-private head (resolve-global ctx head) true))
          n (invoke (analyze ctx head env)
                    (mapv #(analyze ctx % env) (rest items)))
          p (form-position form)]
      (if p (assoc n :pos p) n))))

;; Interop by the head's shape, or nil when it is an ordinary call. Only for a
;; head no local shadows.
(defn- analyze-interop [ctx head hname form items env]
  (cond
    hname (analyze-bare-interop ctx hname form items env)
    (and (form-sym? head) (form-sym-ns head))
      (analyze-qualified-interop ctx head form items env)
    :else nil))

(defn- analyze-list* [ctx form env]
  (let [items (vec (form-elements form))]
    (if (zero? (count items))
      (quote-node form)
      (let [head (first items)
            hname (when (and (form-sym? head) (nil? (form-sym-ns head))) (form-sym-name head))
            sf-name (special-form-name head hname)
            shadowed (and hname (local? env hname))
            opn (core-op-name head hname shadowed)
            ;; under *unchecked-math*, a core +/-/*/inc/dec becomes its wrapping
            ;; unchecked-* (computed once; nil when off or not such an op).
            unm (when (unchecked-math?)
                  (core-rewrite ctx head opn unchecked-arith (count items)))
            ;; a clojure.core numeric cast becomes a checked :coerce node.
            cast (core-rewrite ctx head opn num-cast (count items))]
        (cond
          ;; *unchecked-math* rewrite, before macro/special dispatch (these are
          ;; ordinary core fns). The unchecked-* form re-analyzes normally.
          unm (analyze ctx (cons (symbol unm) (rest items)) env)
          ;; Canonical order (Clojure/CLJS analyze-seq): macroexpand FIRST, then
          ;; dispatch special forms / interop / invoke. A local shadows the macro.
          ;; A true special form is NOT shadowable by a same-named macro, matching
          ;; the reference macroexpand1's isSpecial check — so a ns that redefs a
          ;; macro `def`/`and`/`or` (clojure.spec.alpha) keeps the special form `def`.
          (and (form-sym? head) (not shadowed)
               (not (contains? handled sf-name)) (form-macro? ctx head))
            (analyze-macro-call ctx head form env)
          (ffi-form-name head)
            (analyze-ffi-form ctx (ffi-form-name head) items env)
          ;; special-form heads are NOT shadowable (unlike macros): a local named
          ;; `if` does not change the meaning of (if …) in operator position, per
          ;; spec §3 and the reference. No (not shadowed) guard here.
          (and sf-name (contains? handled sf-name))
            (stamp-def ctx env form (analyze-special ctx sf-name form items env))
          :else
            (or (when-not shadowed (analyze-interop ctx head hname form items env))
                (if cast
                  (analyze-cast ctx cast form items env)
                  (analyze-invoke ctx head form items env shadowed))))))))

;; Record `form` as the innermost positioned form while its children are analyzed,
;; then put back whatever was there, so a completed sibling subtree cannot leave a
;; deeper position behind for a later sibling's diagnostic. Restoring only on the
;; normal return is deliberate: an escaping throw is on its way to the reporter,
;; which wants the position of the form that failed.
(defn- analyze-list [ctx form env]
  (let [box *positioned-form-box*]
    (if (and box (some? (form-line form)))
      (let [prev @box]
        (reset! box form)
        (let [node (analyze-list* ctx form env)]
          (reset! box prev)
          node))
      (analyze-list* ctx form env))))

;; A vector/map/set literal carrying reader metadata (^:foo {…}, ^{:tag :int} [1])
;; keeps it as a runtime value: wrap the collection node in (with-meta coll meta).
;; The metadata is itself a form (its values may be expressions, ^{:a (f)}), so
;; analyze it. nil meta passes the node through. Arglist vectors never reach here —
;; analyze-arity reads their items directly — so a ^Type [args] hint is not wrapped.
(defn- with-coll-meta [ctx form env node]
  (let [m (form-coll-meta form)]
    (if (nil? m)
      node
      (invoke (var-ref "clojure.core" "with-meta") [node (analyze ctx m env)]))))

;; A live value a macro put in its expansion, rendered as code that rebuilds it.
;; embed-plan (state-image.ss) answers with the image writer's verdict:
;;
;;   :var   — the value is some var's root, so read that var. A named fn, a
;;            multimethod, a reify: the restoring code already has it. Note this
;;            tracks the var rather than freezing the object, which is the same
;;            choice the image's fn-ref arm makes and the only one that can be
;;            written as code at all.
;;   :fnsrc — a registered anonymous literal. Its source form and the names it
;;            closed over were recorded at load (fn-form-registry.ss), and the
;;            captured values come back live, so it rebuilds as
;;            ((fn* [free…] <source>) <captured…>) — the wrapper parameters
;;            shadow the outer names the body reads, which is what reconstructs
;;            the lexical environment it was compiled in. Analyzed in the ns it
;;            was compiled in, because that is where its free symbols resolve.
;;            Each captured value is analyzed normally and so lands back here if
;;            it is opaque too.
;;
;; nil means there is nothing to rebuild it from — a closure the runtime built
;; rather than one analyzed from a literal, or a host object with no reader
;; syntax. That is the same line the image draws, and it is a real boundary
;; rather than a missing case, so it reports instead of guessing.
(defn- embedded-plan-node [ctx plan env]
  (if (= (:kind plan) :var)
    (var-ref (:ns plan) (:name plan))
    (invoke (analyze (ctx-for-ns (:ns plan))
                     (list 'fn* (apply vector (map symbol (:frees plan))) (:form plan))
                     (empty-env))
            ;; Each captured value is a VALUE, so it is analyzed QUOTED: a captured
            ;; symbol or list analyzed bare would resolve or invoke instead of
            ;; being the datum it is (a closure over 'foo read back as the var foo;
            ;; one over (1 2) as a call). The quote arm hands a live fn straight
            ;; back here, so a captured closure still rebuilds the same way.
            (mapv #(analyze ctx (list 'quote %) env) (:vals plan)))))

(defn- embedded-value [ctx form env]
  (let [plan (embed-plan form)]
    (if (and plan (not= (:kind plan) :folded))
      (embedded-plan-node ctx plan env)
      (analysis-error
        :analyze/unsupported-form
        (str "Cannot compile this value into code: " (pr-str form) ". "
             "A macro put a live value in the form it returned, and this one "
             "cannot be rebuilt as code. "
             (if plan
               (str "Its source closes over `" (:name plan) "`, whose value the "
                    "compiler folded into the code, so there is no capture left "
                    "to read it back from. Take the constant out of the closure "
                    "— refer to it through a var, or make it a parameter.")
               (str "It is a fn the runtime built rather than one written as a "
                    "literal in a namespace, or a host object with no reader "
                    "syntax. Return a form that BUILDS the value instead of the "
                    "value itself, or store it in a var and splice the var.")))))))

;; Any form that is not a literal, symbol or collection: a reader literal the
;; compiler builds a leaf for, or a live value a macro put in its expansion.
(defn- analyze-leaf [ctx form env]
  (cond
    ;; regex literal #"…" -> a :regex IR node (leaf). The Chez back end emits a
    ;; jolt-regex value over the vendored irregex.
    (form-regex? form) {:op :regex :source (form-regex-source form)}
    ;; #inst / #uuid literals -> :inst / :uuid IR leaves. The Chez back
    ;; end emits a runtime inst/uuid value (host/chez/java/inst-time.ss).
    (form-inst? form) {:op :inst :source (form-inst-source form)}
    (form-uuid? form) {:op :uuid :source (form-uuid-source form)}
    ;; bigdecimal literal (1.5M) -> a :bigdec leaf; the back end emits a runtime
    ;; jbigdec built from the numeric text.
    (form-bigdec? form) {:op :bigdec :source (form-bigdec-source form)}
    ;; a live bigdec/inst/uuid VALUE embedded in a form handed to eval (read via
    ;; read-string, or an evaluated literal spliced by a macro) -> the same leaf as
    ;; the tagged literal, rebuilt from the value's canonical string. Without this
    ;; an embedded 1M / #inst / #uuid value died as "unsupported form".
    (form-bigdec-value? form) {:op :bigdec :source (form-bigdec-value-source form)}
    ;; a Float has no literal, so a live one embeds as the cast that makes it
    (form-float-value? form)
    (analyze ctx (list (symbol "clojure.core" "float") (form-float-value-double form)) env)
    (form-inst-value? form) {:op :inst :source (form-inst-value-source form)}
    (form-uuid-value? form) {:op :uuid :source (form-uuid-value-source form)}
    ;; a live namespace value spliced into a form (~*ns* in a macro) -> a
    ;; :the-ns leaf the back end reconstructs by name at the call site.
    (form-ns-value? form) {:op :the-ns :name (form-ns-value-name form)}
    ;; a live Var value spliced into a form (a macro that resolves a var and
    ;; splices it, e.g. core.contracts' defcurry-from) -> a :the-var reference,
    ;; same as (var ns/name); the back end emits (jolt-var ns name).
    (form-var-value? form) (the-var (form-var-value-ns form) (form-var-value-name form))
    ;; a live Class value spliced the same way — resolve returns the class for
    ;; a class mapping, so `~(resolve sym) lands one in the expansion. It
    ;; compiles to the interner call the class symbol itself compiles to.
    (form-class-value? form) (invoke (var-ref "jolt.host" "jolt-class-for")
                                     [(const (form-class-value-name form))])
    ;; Any other #tag form is one no reader function was found for — the four
    ;; above are the whole set the compiler builds a leaf for, and a registered
    ;; data reader is applied before the form reaches here (loader.ss
    ;; ldr-apply-readers). Name the tag, the way the JVM's reader does; the
    ;; generic "unsupported form" pointed at nothing to fix.
    (form-tagged? form) (analysis-error :read/invalid-data-reader
                                        (str "No reader function for tag "
                                             (form-tag-name form)))
    ;; ...and anything else is a live VALUE a macro put in the form it returned,
    ;; not a syntax error: every shape the reader can produce is handled above,
    ;; so what is left came from evaluation. Clojure's compiler falls through to
    ;; ConstantExpr here and the value simply IS the constant — verified on the
    ;; 1.12.5 oracle, AOT included. jolt compiles to Scheme TEXT, so it cannot
    ;; spell the value and has to rebuild it instead. sci's copy-var reaches
    ;; this with a macro var's root (jolt-l7tq).
    ;;
    ;; How to rebuild it is exactly the question the image writer answers, so
    ;; this reads that same verdict (state-image.ss image-proc-verdict) through
    ;; embed-plan rather than a second copy of the rules: the image scan side,
    ;; the image dump side and the compiler now agree by construction about
    ;; which fns can be rebuilt from source. Both shapes it can return emit as
    ;; ordinary self-contained code, so an embedded value travels into an AOT
    ;; fasl and a built binary and does not need a process-local side table.
    :else (embedded-value ctx form env)))

(defn analyze
  ([ctx form]
   ;; One position box per compilation, over the catch too — as-analysis-diagnostic
   ;; reads it. `or` rather than a fresh box every time: an analyze that re-enters
   ;; this arity (a macro analyzing a form it built) keeps the chain it is nested
   ;; inside, which is what the single shared atom used to give it on one thread.
   ;; The macro-run box is FRESH per entry, not shared like the other two: a
   ;; macro that re-enters the analyzer (eval on a form it built) is compiling
   ;; on its own account, and a diagnostic raised in there is that code's
   ;; :compile-syntax-check, not the outer macro's run.
   (binding [*positioned-form-box* (or *positioned-form-box* (atom nil))
             *expansion-box* (or *expansion-box* (atom nil))
             *macro-run-box* (atom nil)]
     (try
       ;; ` is a reader macro in Clojure, so a form is already past its backticks
       ;; by the time anything looks at it. jolt reads one to a marker and lowers
       ;; it here — up front, over the whole form, so a macro reading its own
       ;; argument forms and a quoted form see the expansion and not the marker.
       ;; Costs nothing on a form that has no backtick in it: the walk returns it
       ;; unchanged and unallocated (jolt-024c).
       ;;
       ;; Stamp the compile namespace on the TOP-LEVEL node so the back end can
       ;; gate anon-fn naming/registration on it even for a non-def form (a def
       ;; carries its own :ns). Invisible to emission unless read.
       (assoc (analyze ctx (form-syntax-quote-expand ctx form) (empty-env))
              :fnsrc-ns (compile-ns ctx))
       (catch Throwable e (throw (as-analysis-diagnostic e))))))
  ([ctx form env]
   (cond
     (form-literal? form) (const form)
     (form-sym? form) (analyze-symbol ctx form env)
     (form-vec? form) (with-coll-meta ctx form env
                        (vector-node (mapv #(analyze ctx % env) (form-vec-items form))))
     (form-map? form) (with-coll-meta ctx form env
                        (map-node (mapv (fn [p] [(analyze ctx (first p) env)
                                                 (analyze ctx (second p) env)])
                                        (form-map-pairs form))))
     (form-set? form) (with-coll-meta ctx form env
                        (set-node (mapv #(analyze ctx % env) (form-set-items form))))
     (form-list? form) (analyze-list ctx form env)
     :else (analyze-leaf ctx form env))))
