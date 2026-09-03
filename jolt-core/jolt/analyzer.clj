(ns jolt.analyzer
  "Portable Clojure analyzer: reader form -> host-neutral IR (see jolt.ir).

  Depends only on the host contract (jolt.host) and IR
  constructors (jolt.ir). The contract fns are referred unqualified
  (host form predicates are `form-*` to avoid colliding with clojure.core), so the
  bootstrap can compile this namespace via its plain :var path. ctx is an opaque
  host handle threaded to the contract fns; the analyzer never inspects it.

  Unsupported forms throw :jolt/uncompilable
  so the caller falls back to the interpreter (the hybrid contract).

  `env` carries lexical state: {:locals #{names} :recur recur-target-name|nil}.
  Definitions are ordered so only `analyze` (mutually recursive) is forward
  declared — the bootstrap compiles forward refs through var cells, but keeping
  them to one keeps the compiled namespace simple."
  (:require [jolt.ir :refer [const local var-ref the-var host-ref if-node do-node invoke
                             coerce-node
                             def-node let-node fn-node vector-node map-node set-node
                             quote-node throw-node host-static host-new reduce-ir-children]]
            [jolt.host :refer [form-sym? form-sym-name form-sym-ns form-list?
                               form-vec? form-map? form-set?
                               form-literal? form-keyword? form-elements form-vec-items
                               form-map-pairs form-set-items form-special? compile-ns
                               form-regex? form-regex-source
                               form-inst? form-inst-source form-uuid? form-uuid-source
                               form-bigdec? form-bigdec-source
                               form-bigdec-value? form-bigdec-value-source
                               form-inst-value? form-inst-value-source
                               form-uuid-value? form-uuid-value-source
                               form-ns-value? form-ns-value-name
                               form-var-value? form-var-value-ns form-var-value-name
                               form-class-value? form-class-value-name
                               form-tagged? form-tag-name
                               unchecked-math? allow-unresolved-vars?
                               form-macro? form-expand-1 resolve-global resolvable-names
                               form-sym-meta form-coll-meta host-intern! form-syntax-quote-lower
                               form-syntax-quote-expand
                               record-type? record-ctor-key deftype-ctor-class form-position form-line late-bind?
                               resolve-class-hint host-class-name? jolt-class-for]]))

(declare analyze)

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

(defn- uncompilable [why]
  (throw (str "jolt/uncompilable: " why)))

;; Process-wide on purpose: an anon fn's generated name is REGISTERED (backend
;; rname), so it has to be unique across every thread compiling, not just within one
;; compilation. Read and bump in one swap! — reading the atom and then incrementing
;; it is two steps, and two threads that interleaved between them got the same name.
(def ^:private gensym-counter (atom 0))
(defn- gen-name [prefix]
  (str "_r$" prefix (dec (swap! gensym-counter inc))))

;; The innermost list form currently being analyzed that carries reader position
;; metadata — what a diagnostic raised anywhere below it should name. Without it
;; the only position available to the reporter is the one the LOADER records per
;; TOP-LEVEL form, so an unresolved symbol 280 lines inside a defn reported the
;; defn's own first line.
;;
;; The form is stored, not its position map: hc-form-position allocates, and this
;; is written for every positioned list form the analyzer walks, whereas the map is
;; wanted only if something actually throws. analyze-list saves and restores it
;; around its children so a completed sibling subtree cannot leave a deeper
;; position behind; the reference compiler does the same thing by pushing thread
;; bindings of LINE/COLUMN in analyzeSeq, which is strictly more expensive.
;;
;; Only forms that HAVE a line are recorded, so descending through a macro-built
;; form (which carries none) keeps the nearest enclosing real position rather than
;; erasing it.
;;
;; The box is per COMPILATION, bound by `analyze` below, not one atom for the
;; process. Shared, two threads compiling at once overwrote each other between the
;; save and the restore, and a diagnostic from one namespace reported a line from
;; whatever the other thread happened to be analyzing. The var holds the box rather
;; than the form so the hot path stays what it was — one atom write per positioned
;; form, not a thread-binding push per form, which is the cost the reference
;; compiler pays to bind LINE/COLUMN in analyzeSeq.
(def ^:dynamic *positioned-form-box* nil)

;; The position a diagnostic should carry: the innermost positioned form's, or nil
;; when nothing under analysis had reader metadata (a macro-built form, or a form
;; handed straight to eval). nil box means nothing is under analysis on this thread.
(defn current-form-position []
  (let [f (when *positioned-form-box* @*positioned-form-box*)]
    (when (some? f) (form-position f))))

(defn- empty-env [] {:locals #{} :hints {}})
(defn- local? [env nm] (contains? (:locals env) nm))
(defn- add-locals [env names] (update env :locals #(reduce conj % names)))
;; &env value handed to a macro: a map of each in-scope local SYMBOL to nil
;; (Clojure's &env maps locals to compiler binding objects; consumers like
;; core.logic's matche only read its keys to tell locals from fresh pattern vars).
(defn- amp-env-map [env]
  (reduce (fn [m n] (assoc m (symbol n) nil)) {} (:locals env)))
;; A recur target carries its name (the compiled self-call) and its ARITY (the
;; binding count) so a recur with the wrong number of args is a compile error,
;; like the JVM, instead of failing only at runtime on the branch that runs.
(defn- with-recur [env name arity] (assoc env :recur name :recur-arity arity))

;; Type hints. The reader keeps ^hint metadata on the binding symbol.
;; Two hints resolve to the :struct fast path (a constant-keyword lookup skips
;; the :jolt/type guard and emits a bare get): ^:struct (a plain struct/record
;; map) and ^TypeName where TypeName is a defrecord/deftype (its instances are
;; tagged :jolt/deftype, not :jolt/type, so a raw get is correct). ^String
;; resolves to :str (the :host-call direct string-native path, str-target-type
;; below), and ^clojure.lang.Keyword to :kw (the keyword arm of the same path).
;; Every other hint (^long, ...) parses and is ignored, as before.
;;
;; Both new values are inert to the struct machinery: every consumer that ACTS on
;; a hint equality-checks :struct (backend_scheme.clj's field-access and 1-arg
;; lookup sites). The one consumer that does not is passes/inline.clj, which
;; tests truthiness — but only to copy a hint from an inlined call onto the local
;; it was bound to, which is propagation, not interpretation, and is what a :str
;; or :kw hint wants anyway.
(defn- str-tag? [t]
  (let [s (cond (form-sym? t) (form-sym-name t) (string? t) t :else nil)]
    (or (= s "String") (= s "java.lang.String"))))
(defn- kw-tag? [t]
  (let [s (cond (form-sym? t) (form-sym-name t) (string? t) t :else nil)]
    (or (= s "Keyword") (= s "clojure.lang.Keyword"))))
(defn- sb-tag? [t]
  (let [s (cond (form-sym? t) (form-sym-name t) (string? t) t :else nil)]
    (or (= s "StringBuilder") (= s "java.lang.StringBuilder"))))
(defn- hint-of [ctx sym]
  (let [m (form-sym-meta sym)]
    (cond
      (nil? m) nil
      (get m :struct) :struct
      :else (let [t (get m :tag)]
              (cond (and t (record-type? ctx t)) :struct
                    (str-tag? t) :str
                    (kw-tag? t) :kw
                    (sb-tag? t) :sb
                    :else nil)))))

;; A hint the INIT proves rather than the programmer writes. (StringBuilder.) can
;; only ever produce a StringBuilder, so a local bound directly to one is proven
;; :sb with nothing in the source — which matters because nobody writes
;; ^StringBuilder on `(let [sb (StringBuilder.)] …)`, and that let is how every
;; string-building loop in Clojure is shaped (honeysql's join, data.json's reader,
;; clojure.pprint). Deliberately just the direct case: a constructor call in the
;; binding position, no flow analysis, no reassignment to worry about since a let
;; local is immutable.
(defn- init-proves-hint [init]
  (when (and (= :host-new (get init :op))
             (sb-tag? (get init :class)))
    :sb))
(defn- add-hint [env nm h]
  (if h (assoc env :hints (assoc (:hints env) nm h)) env))

;; The resolved record ctor-key ("ns/->Name") for a ^Type param hint, or nil.
;; Unlike hint-of (which collapses any record hint to the coarse :struct guard-
;; skip marker), this carries the SPECIFIC record type — cross-namespace aware —
;; so the inference can seed the param's type and read its fields shaped/typed,
;; not just :any (the lever for a typed multi-namespace program without whole-
;; program inference).
(defn- phint-of [ctx sym]
  (let [m (form-sym-meta sym)]
    (when m (let [t (get m :tag)] (when t (record-ctor-key ctx t))))))

;; A ^long / ^double tag -> :long / :double, else nil. The tag is a string on the
;; data reader; tolerate a symbol from macroexpansion too.
(defn- tag->nkind [t]
  (let [s (cond (form-sym? t) (form-sym-name t) (string? t) t :else nil)]
    (cond (= s "double") :double (= s "long") :long :else nil)))

;; A primitive numeric hint (^long / ^double) on a binding symbol. Drives the
;; fl*/fx* fast path (jolt.passes.numeric).
(defn- nhint-of [ctx sym]
  (let [m (form-sym-meta sym)] (when m (tag->nkind (get m :tag)))))

;; A primitive ARRAY hint (^doubles / ^floats / ^longs / ^ints) on a param. Drives
;; the unboxed flvector-ref/-set! fast path for aget/aset (jolt.passes.numeric):
;; a double/float array reads back a proven :double. floats share the flvector kind.
(defn- tag->akind [t]
  (let [s (cond (form-sym? t) (form-sym-name t) (string? t) t :else nil)]
    (cond (= s "doubles") :doubles (= s "floats") :doubles
          (= s "longs") :longs (= s "ints") :ints :else nil)))
(defn- ahint-of [ctx sym]
  (let [m (form-sym-meta sym)] (when m (tag->akind (get m :tag)))))

;; Push a numeric return hint (from ^double/^long on a defn's name) onto each arity
;; of its fn, so the back end coerces the body's value to that kind on return —
;; making the hint a contract a caller's arithmetic can trust.
(defn- with-ret-nhint [node kind]
  (if (and kind (= :fn (:op node)))
    (assoc node :arities (mapv (fn [a] (if (:ret-nhint a) a (assoc a :ret-nhint kind)))
                               (:arities node)))
    node))

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
              init0 (analyze ctx (nth bvec (inc i)) env)
              ;; a ^doubles/^floats/^longs/^ints let binding tags its init with the
              ;; array kind so jolt.passes.numeric seeds the local for the unboxed
              ;; flvector aget/aset path (mirrors the :ahints param route).
              ak (ahint-of ctx bsym)
              init (if ak (assoc init0 :akind ak) init0)]
          ;; an explicit hint wins; init-proves-hint only fills in where the
          ;; programmer wrote nothing.
          (recur (+ i 2) (add-hint (add-locals env [nm]) nm
                                   (or (hint-of ctx bsym) (init-proves-hint init)))
                 (conj pairs [nm init]))))
      [pairs env])))

(defn- parse-params [ctx pvec]
  ;; :hints is a vector of [name hint] pairs (vector, not a map, so the caller
  ;; folds it with a plain reduce — no reduce-over-map in the kernel subset).
  ;; :phints is the parallel vector of [name ctor-key] for record param hints,
  ;; carrying the specific type for the inference to seed.
  (loop [i 0 fixed [] rest-name nil hints [] phints [] nhints [] ahints []]
    (if (< i (count pvec))
      (let [p (nth pvec i)]
        (when-not (form-sym? p) (uncompilable "destructuring fn param"))
        (if (= "&" (form-sym-name p))
          (let [r (nth pvec (inc i))]
            (when-not (form-sym? r) (uncompilable "destructuring fn rest"))
            (recur (+ i 2) fixed (form-sym-name r) hints phints nhints ahints))
          (let [nm (form-sym-name p) h (hint-of ctx p) ph (phint-of ctx p)
                nh (nhint-of ctx p) ah (ahint-of ctx p)]
            (recur (inc i) (conj fixed nm) rest-name
                   (if h (conj hints [nm h]) hints)
                   (if ph (conj phints [nm ph]) phints)
                   (if nh (conj nhints [nm nh]) nhints)
                   (if ah (conj ahints [nm ah]) ahints)))))
      {:fixed fixed :rest rest-name :hints hints :phints phints :nhints nhints :ahints ahints})))

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
        arity {:params fixed
               :body (analyze-seq ctx body env*)}
        ;; carry record param hints (name -> ctor-key) for the inference to seed
        ;; the param type; only when present so a hintless arity stays a struct.
        arity (if (seq (:phints pp)) (assoc arity :phints (:phints pp)) arity)
        ;; numeric param hints (name -> :long/:double) for jolt.passes.numeric.
        arity (if (seq (:nhints pp)) (assoc arity :nhints (:nhints pp)) arity)
        ;; array param hints (name -> :doubles/:longs/:ints) for jolt.passes.numeric:
        ;; aget/aset over them lower to the unboxed flvector fast path.
        arity (if (seq (:ahints pp)) (assoc arity :ahints (:ahints pp)) arity)]
    ;; :rest only when variadic — an absent :rest reads back nil, same as before,
    ;; but keeps a fixed arity a nil-free struct rather than a phm.
    (if rst (assoc arity :rest rst) arity)))

;; A reader that lowers ^meta on a collection to a runtime (with-meta <coll> <meta>)
;; form (the Chez data reader) wraps an arglist vector carrying a return-type hint
;; (^bytes [b] / ^String [x y]). Unwrap to the underlying vector so fn parsing sees
;; the params — the hint is ignored at runtime. Only the (with-meta <vec> _) shape
;; matches, so a real arity clause (head is a vector) and a
;; meta-on-vector arglist pass through unchanged.
(defn- strip-arglist-meta [form]
  (if (form-list? form)
    (let [es (vec (form-elements form))]
      (if (and (= 3 (count es))
               (form-sym? (first es))
               (= "with-meta" (form-sym-name (first es)))
               (form-vec? (nth es 1)))
        (nth es 1)
        form))
    form))

;; A ^long/^double on the ARGLIST VECTOR declares that arity's primitive return
;; type — (fn ^long [x] x) and (defn f ^long [x] x) both mean it, and the
;; reference honors both. jolt read the tag only off a defn's NAME, so the
;; arglist spelling was dropped and ((fn ^long [x] x) 18446744073709551616N)
;; answered 2^64 where the reference answers 0. strip-arglist-meta above throws
;; the hint away for parsing, which is right — the params are what fn parsing
;; needs — but the kind has to be read off the ORIGINAL form first.
;;
;; Both reader shapes are covered: the vector carries its metadata directly, and
;; a reader that lowers ^meta on a collection to (with-meta <vec> <meta>) puts it
;; in the third element. A non-numeric tag (^String [x]) still yields nil, so
;; class hints on an arglist stay ignored exactly as before.
(defn- arglist-ret-nkind [form]
  (let [m (cond
            (form-vec? form) (form-coll-meta form)
            (form-list? form)
            (let [es (vec (form-elements form))]
              (when (and (= 3 (count es))
                         (form-sym? (first es))
                         (= "with-meta" (form-sym-name (first es)))
                         (form-vec? (nth es 1)))
                (nth es 2)))
            :else nil)]
    (when (map? m) (tag->nkind (get m :tag)))))

;; analyze-arity over the stripped params, with the arglist's own return hint
;; attached. An explicit :ret-nhint already on the arity wins.
(defn- analyze-arity-with-ret [ctx params-form body env fn-name]
  (let [a (analyze-arity ctx (strip-arglist-meta params-form) body env fn-name)
        k (arglist-ret-nkind params-form)]
    (if (and k (not (:ret-nhint a))) (assoc a :ret-nhint k) a)))

;; The ORIGINAL (pre-munge) names of the locals a fn literal references that are
;; bound OUTSIDE it — its image-registration free-name list. Computed by walking
;; the analyzed arities with the LEXICAL environment (a set of in-scope names):
;; a :local not in the set is free; a binding form extends the set only for the
;; scope it covers, and a binding's init is walked BEFORE its own name binds
;; ((let [y y] …) reports the outer y). A nested :fn's body IS descended, with
;; its own params/rest/self-name added: the outer literal's :src-form textually
;; contains the nested one, so restore evaluates the whole outer form with the
;; outer's free names bound, and a name free only inside the nested literal must
;; be in the outer's list or it would resolve to a same-named global at eval.
;; Sorted so the registration is stable across builds.
;;
;; A nested :fn is READ, not descended. analyze-fn runs bottom-up, so a nested
;; literal already carries the answer for its own subtree: :free-names is exactly
;; the names it references that nothing inside it binds, which is what descending
;; would recompute. Filtering that list against the enclosing scope gives the same
;; set — a name is this literal's iff the nested one leaves it free AND nothing out
;; here binds it — and it costs the nested list rather than the nested subtree.
;;
;; Descending was quadratic, and it was the analyzer's whole cost on nested
;; literals: every literal walked its entire subtree, so a chain of N of them
;; walked O(N^2) nodes. The CPS pass in clojure.core.async builds that chain (one
;; closure per park site) and 240 sites spent 1.4 s here. Reading takes it to 30 ms.
;; A node with no :free-names (one a pass built, rather than analyze-fn) is
;; descended as before, so the fallback cannot silently drop a capture.
(defn- fn-free-names [arities fn-name]
  (let [refs (atom #{})
        ;; each arity's params bind only in that arity: (fn ([x] y) ([y] y))
        ;; references y free in the first arity even though the second binds it
        arity-own (fn [a] (into (set (if (:rest a) (conj (:params a) (:rest a)) (:params a)))
                                (when fn-name [fn-name])))]
    (letfn [(walk [n bound]
              (let [op (:op n)]
                (cond
                  (= op :local) (when-not (contains? bound (:name n))
                                  (swap! refs conj (:name n)))
                  (= op :fn)
                  (if-let [inner (:free-names n)]
                    (doseq [nm inner]
                      (when-not (contains? bound nm) (swap! refs conj nm)))
                    (doseq [a (:arities n)]
                      (walk (:body a)
                            (into (if (:name n) (conj bound (:name n)) bound)
                                  (if (:rest a) (conj (:params a) (:rest a)) (:params a))))))
                  (or (= op :let) (= op :loop))
                  ;; letrec (letfn, and the state-machine loops core.async's CPS
                  ;; transform builds from it) binds EVERY name across EVERY init,
                  ;; including the init's own. Walking those inits with only the
                  ;; earlier names bound — which is right for let* and loop, where
                  ;; an init sees only what precedes it — reports a letrec binding
                  ;; as free in its own init. That over-approximates :free-names,
                  ;; and a name the literal itself binds has no captured value to
                  ;; recover, so the closure refuses at dump for a variable that
                  ;; was never a capture.
                  (if (:letrec n)
                    (let [b* (into bound (map first (:bindings n)))]
                      (doseq [b (:bindings n)] (walk (second b) b*))
                      (walk (:body n) b*))
                    (let [b* (reduce (fn [b [nm init]]
                                       (walk init b)
                                       (conj b nm))
                                     bound (:bindings n))]
                      (walk (:body n) b*)))
                  (= op :try)
                  (do (walk (:body n) bound)
                      (when (:catch-body n)
                        (walk (:catch-body n) (if (:catch-sym n) (conj bound (:catch-sym n)) bound)))
                      (when (:finally n) (walk (:finally n) bound)))
                  :else
                  (reduce-ir-children (fn [acc c] (walk c bound) acc) nil n))))]
      (doseq [a arities] (walk (:body a) (arity-own a)))
      (vec (sort @refs)))))

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
                        (mapv (fn [clause]
                                (let [cl (vec (form-elements clause))]
                                  (analyze-arity-with-ret ctx (first cl) (rest cl) env fn-name)))
                              rest-items))
               :else (uncompilable "fn: bad params"))
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
                    :free-names (fn-free-names (:arities node) fn-name))]
    node))

;; class names that catch everything (the JVM root types); a (catch Throwable e …)
;; clause matches any thrown value unconditionally.
(def ^:private catch-all-names #{"Throwable" "java.lang.Throwable" "Object" "java.lang.Object"})

(defn- analyze-try [ctx items env]
  (let [clauses (rest items)
        body (atom [])
        catches (atom [])           ; ordered vector of (catch class binding body*) clauses
        finally-body (atom nil)
        seen-finally? (atom false)]
    (doseq [c clauses]
      (let [head (when (form-list? c) (first (vec (form-elements c))))
            hname (when (and head (form-sym? head)) (form-sym-name head))]
        ;; Clause ordering, like the JVM: body exprs, then catches, then at most one
        ;; finally which must be last. Enforced eagerly (plain throw) so a misplaced
        ;; clause is a compile error rather than a silently-relocated body form.
        (when @seen-finally?
          (throw "finally clause must be last in try expression"))
        (cond
          (= hname "catch")
            (let [cl (vec (form-elements c))]
              ;; (catch class binding body*) — binding (3rd elem) MUST be a symbol.
              ;; Validate eagerly (plain throw, NOT uncompilable, so it's a real
              ;; error rather than a compile->interpret punt) instead of letting
              ;; form-sym-name crash on a non-symbol.
              (when (or (< (count cl) 3) (not (form-sym? (nth cl 2))))
                (throw "Unable to parse catch clause; expected (catch class binding body*)"))
              (swap! catches conj cl))
          (= hname "finally")
            (do (reset! seen-finally? true)
                (reset! finally-body (rest (vec (form-elements c)))))
          :else
            (do
              ;; a body expr after a catch is illegal — only catch/finally may follow.
              (when (seq @catches)
                (throw "Only catch or finally clause can follow catch in try expression"))
              (swap! body conj c)))))
    ;; Multiple catch clauses dispatch on the thrown value's class, in order. Lower
    ;; them to ONE guard binding a fresh local, then a nested-if chain testing each
    ;; clause's class with (instance? C e) — which respects the exception supertype
    ;; hierarchy — plus __catch-broad? for an untyped host condition. No match
    ;; re-throws. (The earlier single-catch IR ignored the class and caught
    ;; everything; this gives real per-class dispatch.) :catch-sym/:catch-body/
    ;; :finally are added only when present — an absent key must stay absent (a
    ;; nil-valued key would make the node a phm and force back-end densification).
    (let [n {:op :try :body (analyze-seq ctx @body env)}
          n (if (seq @catches)
              (let [evar-name (gen-name "catch")
                    raw-name (gen-name "catch-raw")
                    evar (symbol evar-name)
                    dispatch
                    (reduce
                      (fn [else cl]
                        (let [cform (nth cl 1)
                              bindsym (nth cl 2)
                              bodyf (drop 3 cl)
                              letform (cons 'let (cons (vector bindsym evar) bodyf))
                              fullname (when (form-sym? cform) (form-sym-name cform))
                              catch-all? (or (not (form-sym? cform))
                                             (contains? catch-all-names fullname))]
                          (if catch-all?
                            letform
                            (list 'if (list 'or
                                            (list 'instance? cform evar)
                                            (list '__catch-broad? fullname evar))
                                  letform else))))
                      (list 'throw evar)
                      (reverse @catches))]
                (assoc n :catch-sym evar-name
                         :catch-raw-sym raw-name
                         :catch-body (analyze-seq ctx (list dispatch)
                                                  (add-locals env [evar-name]))))
              n)
          n (if @finally-body
              (assoc n :finally (analyze-seq ctx @finally-body env))
              n)]
      n)))

;; letfn*: (letfn* [name1 fn1 name2 fn2 …] body*) — the special form Clojure's
;; letfn macro expands to (flat name/fn-form pairs, the fn forms already named).
;; The named local fns are MUTUALLY recursive, so bind every name into the env
;; BEFORE analyzing any fn form — each then resolves its siblings (and itself) as
;; locals. Emitted as a :let flagged :letrec so the back end lowers it to
;; `letrec*`; the interpreter's shared mutable env gives the same semantics.
(defn- analyze-letfn* [ctx items env]
  (let [bvec  (vec (form-vec-items (nth items 1)))
        n     (quot (count bvec) 2)
        names (mapv (fn [i] (form-sym-name (nth bvec (* 2 i)))) (range n))
        env*  (add-locals env names)
        binds (mapv (fn [i]
                      [(nth names i)
                       (analyze ctx (nth bvec (inc (* 2 i))) env*)])
                    (range n))]
    {:op :let :letrec true :bindings binds
     :body (analyze-seq ctx (drop 2 items) env*)}))

;; A `.-field` head: `(.-field target)` is field access (the dash signals field
;; access to the host-call dispatcher). Defined above analyze-special so its set!
;; arm and analyze-list both reach it without a forward reference.
(defn- field-head? [nm]
  (and (> (count nm) 2) (= ".-" (subs nm 0 2))))

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
  (let [name-sym (nth items 1)]
    ;; ^{:map} metadata reads as (def (with-meta name m) v): the metadata is a
    ;; runtime expression, so the interpreter evaluates the whole def.
    (when-not (form-sym? name-sym)
      (uncompilable "def name with map metadata"))
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

;; (set! (.-field obj) v) mutates a deftype instance field in place; (set! *var* v)
;; sets the var's innermost thread binding through jolt.host/set-var! — Var.set,
;; which throws when there is none rather than establishing a root. A local target
;; (jolt binds fields immutably) or any other shape is uncompilable.
(defn- analyze-set! [ctx items env]
  (let [target (nth items 1)
        val-node (analyze ctx (nth items 2) env)
        ti (when (form-list? target) (vec (form-elements target)))
        thead (when (and ti (pos? (count ti)) (form-sym? (first ti)))
                (form-sym-name (first ti)))]
    (cond
      ;; (set! (.-field obj) v) and (set! (.field obj) v) both set an instance
      ;; field in Clojure; accept either. The "." static-field head is handled
      ;; by the next arm (it isn't a .-/.name field head).
      (and thead (> (count thead) 1) (= \. (first thead)))
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
      (form-sym? target)
      (do (when (local? env (form-sym-name target)) (uncompilable "set! of a local"))
          (let [r (resolve-global ctx target)]
            (when-not (= :var (:kind r)) (uncompilable "set! of a non-var"))
            {:op :set-var :the-var (the-var (:ns r) (:name r)) :val val-node}))
      :else (uncompilable "set! of an unsupported target"))))

;; (monitor-enter x) / (monitor-exit x) — the raw monitor ops, lowered to the same
;; identity-keyed per-object mutex `locking` takes, so the two compose. Both
;; evaluate to nil, matching the JVM (which emits a NIL after the monitor op).
(defn- analyze-monitor-op [ctx items env op]
  (when-not (= 2 (count items))
    (throw (str "Wrong number of args (" (dec (count items)) ") passed to: " op)))
  (invoke (var-ref "jolt.host" op) [(analyze ctx (nth items 1) env)]))

(defn- analyze-special [ctx op form items env]
  (case op
    ;; A quoted collection keeps its USER metadata (rewrite-clj coerces
    ;; '^:x (4 5 6) and expects the meta back). A list form is the one thing the
    ;; reader stamps with :line/:column/:file, and that is its own bookkeeping
    ;; rather than data, so it comes back off — but only there: on a vector, map,
    ;; set or symbol the only possible source of a position key is the program
    ;; that wrote it. The kept metadata is part of the literal, so quote it.
    "quote" (let [qf (second items)
                  m (form-coll-meta qf)
                  m (when (map? m)
                      (let [u (if (form-list? qf) (dissoc m :line :column :file) m)]
                        (when (seq u) u)))]
              (if (nil? m)
                (quote-node qf)
                (invoke (var-ref "clojure.core" "with-meta") [(quote-node qf) (quote-node m)])))
    "if" (do
           ;; 2 or 3 argument forms only (spec 03-special-forms X1)
           (when (or (< (count items) 3) (> (count items) 4))
             (throw (str "Wrong number of args (" (dec (count items)) ") passed to: if")))
           (if-node (analyze ctx (nth items 1) env)
                    (analyze ctx (nth items 2) env)
                    (if (> (count items) 3)
                      (analyze ctx (nth items 3) env)
                      (const nil))))
    "do" (analyze-seq ctx (rest items) env)
    "throw" (throw-node (analyze ctx (nth items 1) env))
    "def" (analyze-def ctx items env)
    "let*" (let [bvec (vec (form-vec-items (nth items 1)))
                 r (analyze-bindings ctx bvec env)]
             (let-node (first r) (analyze-seq ctx (drop 2 items) (second r))))
    "loop*" (let [bvec (vec (form-vec-items (nth items 1)))
                  rname (gen-name "loop")
                  r (analyze-bindings ctx bvec env)
                  env** (with-recur (second r) rname (quot (count bvec) 2))]
              {:op :loop :bindings (first r)
               :body (analyze-seq ctx (drop 2 items) env**)})
    "recur" (let [rt (:recur env)
                  arity (:recur-arity env)
                  n (dec (count items))]
              (when-not rt (uncompilable "recur outside loop/fn"))
              (when (and arity (not= n arity))
                (throw (str "Mismatched argument count to recur, expected: " arity
                            " args, got: " n)))
              {:op :recur
               :args (mapv #(analyze ctx % env) (rest items))})
    "try" (analyze-try ctx items env)
    ;; (monitor-enter x) / (monitor-exit x) — the bare halves of `locking`, which
    ;; is a macro over the same jolt.host per-object monitor. Libraries that
    ;; expand their own locking macro rather than calling clojure.core/locking
    ;; emit these directly (borkdude/dynaload does, under malli), so lower them
    ;; to that seam instead of punting. Like the JVM, both yield nil.
    "monitor-enter" (analyze-monitor-op ctx items env "monitor-enter")
    "monitor-exit" (analyze-monitor-op ctx items env "monitor-exit")
    "letfn*" (analyze-letfn* ctx items env)
    "fn*" (analyze-fn ctx form items env)
    ;; Lower the backtick to construction code (zero runtime cost), then analyze
    ;; it — the macroexpand/compile-time step, per read -> macroexpand -> compile.
    "syntax-quote" (analyze ctx (form-syntax-quote-lower ctx (second items)) env)
    ;; (var sym): the var itself, resolved at compile time. A non-symbol
    ;; argument and an unresolvable symbol are distinct errors; the latter uses
    ;; the reference wording. Note (var String) succeeds where the JVM refuses:
    ;; jolt holds imported classes in vars, so the symbol resolves as :var.
    "var" (let [sym (second items)]
            (if-not (form-sym? sym)
              (uncompilable (str "var argument must be a symbol: " (pr-str sym)))
              (let [r (resolve-global ctx sym)]
                (if (= :var (:kind r))
                  (the-var (:ns r) (:name r))
                  (uncompilable (str "Unable to resolve var: "
                                     (if-let [ns (form-sym-ns sym)] (str ns "/") "")
                                     (form-sym-name sym) " in this context"))))))
    ;; A defmacro that is not top-level (the spine intercepts those) — e.g. one
    ;; produced by a macro like (when … (defmacro …)). Lower it the way the spine
    ;; does: def the expander fn, then mark the var a macro at runtime so later
    ;; forms expand it. Strip a leading docstring / attr-map, as defmacro allows.
    "defmacro" (let [name-sym (nth items 1)
                     nm (form-sym-name name-sym)
                     cur (compile-ns ctx)
                     after (drop 2 items)
                     [after doc] (if (string? (first after)) [(rest after) (first after)] [after nil])
                     [after attr] (if (form-map? (first after)) [(rest after) (first after)] [after nil])
                     ;; build (fn params body…) and analyze it through the fn MACRO
                     ;; so a destructuring macro arglist desugars (the fn* primitive
                     ;; would not), then def it and mark the var a macro. Head with
                     ;; the QUALIFIED clojure.core/fn so it resolves to the real fn
                     ;; macro even when the macro being defined is `fn` (schema/s/fn)
                     ;; or the ns excluded it.
                     fn-form (cons (symbol "clojure.core" "fn") after)
                     ;; var meta like defn: ^meta on the name, docstring, attr-map, arglists
                     arglists (if (form-list? (first after))
                                (map first after)
                                (list (first after)))
                     ;; the derived arglists is a DEFAULT: an explicit :arglists in
                     ;; the attr-map (or on the name) overrides it, as defn allows.
                     ;; Merging it last instead silently discarded the user's value.
                     ;; precedence, matching the JVM for both defn and defmacro:
                     ;; name metadata < the derived arglists < attr-map < docstring.
                     ;; So ^{:arglists …} on the NAME does not override (the JVM
                     ;; ignores it there) but {:arglists …} in the attr-map does.
                     ;; Merging the derived value last discarded the attr-map's.
                     base (merge (or (form-sym-meta name-sym) {})
                                 {:arglists arglists}
                                 (or attr {})
                                 (if doc {:doc doc} {}))
                     meta-expr (def-meta-expr ctx base env)]
                 (host-intern! ctx cur nm)
                 (merge {:op :defmacro :ns cur :name nm
                         :fn (analyze ctx fn-form env)}
                        (if meta-expr {:meta-expr meta-expr} {:meta base})))
    "set!" (analyze-set! ctx items env)
    (uncompilable (str "special form " op))))

;; Host interop method call. `(.method target arg*)` — a head that
;; starts with "." but not ".-" (field access stays punted). Analyzes to a
;; :host-call node; the Chez back end lowers it to a jolt-host-call dispatch.
(defn- method-head? [nm]
  (and (> (count nm) 1)
       (= "." (subs nm 0 1))
       (not (= "-" (subs nm 1 2)))     ; .-field is field access
       (not (= "." (subs nm 1 2)))))   ; .. is the threading macro, not .method

;; The compile-time proof that a :host-call's target is a string, driving the
;; backend's direct string-native emission (no record-method-dispatch walk, no
;; jolt-vector rest-args). Three sources: the target's own ^String tag, a ^String
;; let/param binding the target reads through (:hint :str from hint-of), and a
;; string literal. NOT inferred types — this is the hint seam; widening it to the
;; :str lattice value is the follow-up.
(defn- str-target-type [raw target]
  (when (or (and (form-sym? raw) (str-tag? (get (form-sym-meta raw) :tag)))
            (= :str (:hint target))
            (and (= :const (:op target)) (string? (:val target))))
    :str))

;; Same seam for keywords. Sources mirror str-target-type: the ^Keyword tag, a
;; :kw-hinted binding, or a keyword literal. Note jolt does NOT reach honeysql's
;; (.sym ^clojure.lang.Keyword k) despite that being the shape this was written
;; for — jolt's reader advertises :bb and honeysql orders its conditional with :bb
;; first, so the pure-Clojure branch wins. See keyword-direct-emit.
(defn- kw-target-type [raw target]
  (when (or (and (form-sym? raw) (kw-tag? (get (form-sym-meta raw) :tag)))
            (= :kw (:hint target))
            (and (= :const (:op target)) (keyword? (:val target))))
    :kw))

;; And for StringBuilder, the one that pays for itself the most: a jhost method
;; call is the most expensive interop shape jolt has — the tag's method table is
;; found by hashing the tag STRING, the method by hashing its name, the rest args
;; arrive as a jolt-vector and are converted back to a list for `apply`, and the
;; shim lambda takes them as a further rest list. Measured 270 ns for a 0-arg
;; StringBuilder method and 500 ns for (.append sb "a"), against 20 ns on the JVM.
;; Sources are the ^StringBuilder tag and a :sb-hinted binding — which
;; init-proves-hint supplies for (let [sb (StringBuilder.)] …) with nothing written
;; in the source. Both give an IDENTIFIER as the emitted target, which sb-direct-emit
;; requires: its append arm splices the target twice, once to append and once to
;; return (the JVM's fluent contract).
;;
;; A constructor directly in target position, (.append (StringBuilder.) x), is
;; deliberately NOT a source even though it proves the type just as well. It is an
;; expression, so the second splice would run it again and append to a fresh
;; builder — the gate caught exactly that, answering "" for
;; (.toString (.append (StringBuilder.) "lit")). Binding the target to a temporary
;; here would fix it, but nothing writes that shape and the generic path already
;; handles it correctly.
(defn- sb-target-type [raw target]
  (when (or (and (form-sym? raw) (sb-tag? (get (form-sym-meta raw) :tag)))
            (= :sb (:hint target)))
    :sb))

;; The list form's source position on a node, when the reader recorded one —
;; the same stamp an :invoke carries. A host call in tail position stores it as
;; its trace site (backend sited host call), and it is what a report names when
;; the host raises from inside the call.
(defn- stamp-pos [node form]
  (let [p (form-position form)]
    (if p (assoc node :pos p) node)))

(defn- analyze-host-call [ctx hname items env]
  (when (< (count items) 2)
    (throw (str "Malformed member expression, expecting (.method target ...): " hname)))
  (let [raw (nth items 1)
        target (analyze ctx raw env)]
    (cond-> {:op :host-call
             :method (subs hname 1)
             :target target
             :args (mapv #(analyze ctx % env) (drop 2 items))}
      (str-target-type raw target) (assoc :target-type :str)
      (kw-target-type raw target) (assoc :target-type :kw)
      (sb-target-type raw target) (assoc :target-type :sb))))

;; A constructor head: `Class.` — a symbol ending in "." (but not the member
;; access `.method` / `..` forms). `(Class. args*)` builds an instance.
(defn- ctor-head? [nm]
  (and (> (count nm) 1)
       (= "." (subs nm (dec (count nm)) (count nm)))
       (not (= "." (subs nm 0 1)))))

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

;; Literal, data-only layout descriptors. Keep the analyzer representation free
;; of reader objects so it survives self-hosting and can be embedded in the IR.
(def ^:private ffi-layout-scalars
  #{"int" "uint" "int8" "i8" "uint8" "u8" "byte" "char" "bool"
    "int16" "short" "uint16" "ushort" "int32" "uint32"
    "long" "ulong" "int64" "uint64" "size_t" "ssize_t" "iptr" "uptr"
    "double" "float" "pointer" "void*"})

(declare analyze-ffi-layout-type)

(defn- ffi-layout-form-kind [form]
  (when (form-vec? form)
    (let [parts (vec (form-vec-items form))
          head (first parts)]
      (when (and (form-keyword? head) (nil? (namespace head)))
        (name head)))))

;; A struct and a union differ only in how the ABI lays the members out, so one
;; analyzer reads both and the :ffi-kind it answers is what the back end turns
;; into Chez's (struct ...) or (union ...) ftype. Everything downstream — offsets,
;; size, alignment, field paths — is computed by the SAME ftype machinery, so a
;; union needs no arithmetic of its own here.
(defn- analyze-ffi-layout-aggregate [form]
  (let [kind (ffi-layout-form-kind form)
        kind (when (or (= "struct" kind) (= "union" kind)) kind)]
    (when-not (form-vec? form)
      (throw (str "jolt.ffi layout descriptor must be [:struct [[field type] ...]] "
                  "or [:union [[field type] ...]], got " (pr-str form))))
    (let [parts (vec (form-vec-items form))]
      (when-not (and kind (= 2 (count parts)) (form-vec? (nth parts 1)))
        (throw (str "jolt.ffi layout descriptor must be [:struct [[field type] ...]] "
                    "or [:union [[field type] ...]], got " (pr-str form))))
      (let [field-forms (vec (form-vec-items (nth parts 1)))]
        (when (empty? field-forms)
          (throw (str "jolt.ffi " kind " descriptor must contain at least one field")))
        (loop [remaining field-forms names #{} fields []]
          (if (empty? remaining)
            {:ffi-kind (if (= "union" kind) :union :struct) :fields fields}
            (let [field (first remaining)]
              (when-not (form-vec? field)
                (throw (str "jolt.ffi " kind " field must be [keyword type], got "
                            (pr-str field))))
              (let [fp (vec (form-vec-items field))]
                (when-not (= 2 (count fp))
                  (throw (str "jolt.ffi " kind " field must be [keyword type], got "
                              (pr-str field))))
                (let [field-name (nth fp 0)]
                  (when-not (and (form-keyword? field-name)
                                 (nil? (namespace field-name)))
                    (throw (str "jolt.ffi " kind " field name must be an unqualified keyword, got "
                                (pr-str field-name))))
                  (let [nm (name field-name)]
                    (when (contains? names nm)
                      (throw (str "jolt.ffi " kind " field names must be unique; duplicate :" nm)))
                    (recur (rest remaining)
                           (conj names nm)
                           (conj fields {:name nm
                                         :type (analyze-ffi-layout-type (nth fp 1))}))))))))))))

(defn- analyze-ffi-layout-array [form]
  (let [parts (vec (form-vec-items form))]
    (when-not (= 3 (count parts))
      (throw (str "jolt.ffi array descriptor must be [:array element-type positive-count], got "
                  (pr-str form))))
    (let [count (nth parts 2)]
      (when-not (and (integer? count) (pos? count))
        (throw (str "jolt.ffi array count must be a positive integer literal, got "
                    (pr-str count))))
      {:ffi-kind :array
       :count count
       :type (analyze-ffi-layout-type (nth parts 1))})))

(defn- analyze-ffi-layout-type [form]
  (cond
    (form-keyword? form)
    (let [n (name form)]
      (when-not (and (nil? (namespace form)) (contains? ffi-layout-scalars n))
        (throw (str "jolt.ffi struct field type must be a fixed-size scalar, nested struct or union, or fixed array; got "
                    (pr-str form))))
      n)

    (form-vec? form)
    (case (ffi-layout-form-kind form)
      "struct" (analyze-ffi-layout-aggregate form)
      "union" (analyze-ffi-layout-aggregate form)
      "array" (analyze-ffi-layout-array form)
      (throw (str "jolt.ffi struct field type must be a fixed-size scalar, nested struct or union, or fixed array; got "
                  (pr-str form))))

    :else
    (throw (str "jolt.ffi struct field type must be a fixed-size scalar, nested struct or union, or fixed array; got "
                (pr-str form)))))

(defn- analyze-ffi-layout [items]
  (when-not (= 2 (count items))
    (throw "jolt.ffi/layout expects one literal struct or union descriptor"))
  {:op :ffi-layout :layout (analyze-ffi-layout-aggregate (nth items 1))})

(defn- ffi-by-value-form? [form]
  (when (form-vec? form)
    (let [parts (vec (form-vec-items form))]
      (and (= 2 (count parts))
           (form-keyword? (nth parts 0))
           (nil? (namespace (nth parts 0)))
           (= "by-value" (name (nth parts 0)))))))

;; A union has no by-value ABI a caller can rely on: which member is live is the
;; program's knowledge, not the type's, and the register classification of the
;; bytes differs by which one it is. babashka.ffi refuses it in a signature for
;; the same reason. So a union reaches C as a :pointer, alone or as a member of
;; a struct that would otherwise travel by value.
(defn- ffi-layout-holds-union? [type]
  (cond
    (string? type) false
    (= :union (:ffi-kind type)) true
    (= :struct (:ffi-kind type))
      (boolean (some (fn [f] (ffi-layout-holds-union? (:type f))) (:fields type)))
    (= :array (:ffi-kind type)) (ffi-layout-holds-union? (:type type))
    :else false))

(defn- analyze-ffi-signature-type [form position]
  (cond
    (form-keyword? form) (name form)
    (ffi-by-value-form? form)
      (let [parts (vec (form-vec-items form))
            analyzed (analyze-ffi-layout-aggregate (nth parts 1))]
        (when (ffi-layout-holds-union? analyzed)
          (throw (str "jolt.ffi " position
                      " type: a union is not passed by value, alone or inside a struct"
                      " — declare :pointer and read the member you know applies")))
        {:ffi-kind :by-value
         :type analyzed})
    :else
      (throw (str "jolt.ffi " position
                  " type must be a keyword or [:by-value [:struct ...]], got "
                  (pr-str form)))))

;; jolt.ffi/__cfn: the low-level foreign-function form a jolt library
;; uses (via the jolt.ffi/foreign-fn macro) to bind native code. Shape:
;;   (jolt.ffi/__cfn "c_symbol" [:argtype ...] :rettype)             ; non-blocking
;;   (jolt.ffi/__cfn "c_symbol" [:argtype ...] :rettype :blocking)   ; may block
;;   (jolt.ffi/__cfn "c_symbol" [:argtype ...] :rettype {opts})      ; options map
;; The C symbol is a string literal and the types are literal keywords, read here
;; at compile time; the Chez back end lowers it to a real `foreign-procedure`
;; (typed marshaling, no runtime eval). A :blocking call is emitted __collect_safe
;; so it deactivates the thread for the call — a blocking call (accept/recv/...)
;; must not pin the stop-the-world collector. A leaf IR node.
;;
;; Normalize the optional form to the two literal Boolean flags understood by
;; the back end. Direct __cfn calls and the public macros share this fail-closed
;; validation: keys must be unqualified keywords, values literal Booleans, and
;; unknown keys are rejected rather than ignored.
(defn- ffi-option
  ([]
   {:blocking false :capture-native-error false})
  ([opt]
   (cond
     (and (form-keyword? opt)
          (nil? (namespace opt))
          (= "blocking" (name opt)))
     {:blocking true :capture-native-error false}

     (form-map? opt)
     (reduce
       (fn [res pr]
         (let [k (nth pr 0) v (nth pr 1)]
           (when-not (and (form-keyword? k) (nil? (namespace k)))
             (throw (str "jolt.ffi: option key must be an unqualified keyword, got: " k)))
           (let [kn (name k)]
             (when-not (or (= kn "blocking") (= kn "capture-native-error"))
               (throw (str "jolt.ffi: unknown option :" kn)))
             (when-not (or (true? v) (false? v))
               (throw (str "jolt.ffi: option :" kn
                           " must be a literal Boolean, got: " v)))
             (assoc res
                    (if (= kn "blocking") :blocking :capture-native-error)
                    v))))
       {:blocking false :capture-native-error false}
       (form-map-pairs opt))

     :else
     (throw (str "jolt.ffi: option must be :blocking or an options map, got: " opt)))))

(defn- analyze-ffi-fn [ctx items env]
  (when-not (<= 4 (count items) 5)
    (throw (str "jolt.ffi/foreign-fn expects "
                "(foreign-fn \"sym\" [argtypes] rettype [:blocking | {opts}])")))
  (let [rettype (analyze-ffi-signature-type (nth items 3) "return")
        opt (if (= 5 (count items))
              (ffi-option (nth items 4))
              (ffi-option))
        blocking (:blocking opt)
        capture (:capture-native-error opt)]
    (when (and capture (= rettype "void"))
      (throw (str "jolt.ffi: :capture-native-error is not supported for :void "
                  "(no stable native result to pair with the error code)")))
    (when (and capture (map? rettype))
      (throw "jolt.ffi: :capture-native-error is not supported for by-value returns"))
    {:op :ffi-fn
     :csym (nth items 1)
     :argtypes (mapv #(analyze-ffi-signature-type % "argument")
                     (form-vec-items (nth items 2)))
     :rettype rettype
     :blocking blocking
     :capture-native-error capture}))

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
    (throw (str "jolt.ffi/foreign-callable expects (foreign-callable f [argtypes] rettype [:collect-safe])")))
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
  (when (< (count items) 3)
    (throw (str "Malformed (. target member ...) form")))
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
      :else (uncompilable "special form . (non-symbol member)"))))

(defn- analyze-field [ctx hname items env]
  (when (< (count items) 2)
    (throw (str "Malformed (.-field target) form")))
  {:op :host-call
   :method (subs hname 1)        ; ".-field" -> "-field"
   :target (analyze ctx (nth items 1) env)
    :args []})

;; A macro referenced in VALUE position — a bare reference or an argument, never
;; the head of a call (a macro head macroexpands in analyze-list before this is
;; reached) — is a real compile error on the JVM: "Can't take value of a macro:
;; #'ns/name". Plain throw (not uncompilable) so it surfaces rather than punting
;; to an interpreter fallback. #'foo and (var foo) of a macro are handled in
;; `analyze` (form-var-value?) and analyze-special ("var") and never reach here.
(defn- deny-macro-value [ctx form r]
  (when (form-macro? ctx form)
    (throw (str "Can't take value of a macro: #'" (:ns r) "/" (:name r)))))

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

;; --- unresolved-symbol diagnostics ("did you mean?") ------------------------
;; A Carp-inspired touch: when a bare symbol doesn't resolve, offer the closest
;; in-scope names by Levenshtein distance instead of a bare "unable to resolve".

;; Levenshtein edit distance over two strings, computed on char vectors (jolt's
;; strings are codepoint-modeled, so index via vec, not nth-on-string). Only ever
;; called on the error path, so the O(la*lb) DP is fine.
(defn- edit-distance [sa sb]
  (let [a (vec sa) b (vec sb) la (count a) lb (count b)]
    (cond
      (zero? la) lb
      (zero? lb) la
      :else
      (loop [i 1 prev (vec (range (inc lb)))]
        (if (> i la)
          (peek prev)
          (let [ca (nth a (dec i))
                row (reduce
                      (fn [cur j]
                        (let [cost (if (= ca (nth b (dec j))) 0 1)]
                          (conj cur (min (inc (peek cur))          ; insertion
                                         (inc (nth prev j))        ; deletion
                                         (+ cost (nth prev (dec j))))))) ; substitution
                      [i]
                      (range 1 (inc lb)))]
            (recur (inc i) row)))))))

;; Distance a candidate may be from the typed name to count as a "did you mean":
;; scales with length so a 2-char name can't match everything, and a longer name
;; tolerates a couple of typos.
(defn- suggest-threshold [n]
  (cond (<= n 3) 1 (<= n 6) 2 :else 3))

;; Up to 3 candidate names closest to nm within the threshold, nearest first,
;; ties broken alphabetically for a stable message.
(defn- suggestions-for [nm candidates]
  (let [thr (suggest-threshold (count nm))
        scored (filter (fn [p] (<= (second p) thr))
                       (map (fn [c] [c (edit-distance nm c)])
                            (remove #(= % nm) (distinct candidates))))
        ordered (sort (fn [a b]
                        (if (= (second a) (second b))
                          (compare (first a) (first b))
                          (compare (second a) (second b))))
                      scored)]
    (mapv first (take 3 ordered))))

;; The names an unresolved symbol is matched against: everything resolvable from
;; the compile ns (its own interns + clojure.core's publics, via the host) plus
;; the lexical locals in scope.
(defn- candidate-pool [ctx env]
  (concat (seq (resolvable-names ctx)) (seq (:locals env))))

;; Throw the structured "unable to resolve symbol" diagnostic. The human message
;; keeps the JVM wording (with any suggestions appended); the ex-data carries a
;; machine-readable :jolt/error map the CLI reporter emits as EDN under
;; JOLT_DIAG=edn, so editors/tools get the symbol, suggestions, and ns as data.
;; :line/:column/:file come from the innermost enclosing positioned form, so the
;; report names where the unknown name is WRITTEN. The reporter otherwise falls
;; back to the loader's per-top-level-form position, which in a long defn is its
;; opening line and says nothing about the offending expression.
(defn- resolve-error [ctx nm env]
  (let [sugg (suggestions-for nm (candidate-pool ctx env))
        base (str "Unable to resolve symbol: " nm " in this context")
        msg (if (seq sugg)
              (str base " (did you mean " (apply str (interpose ", " sugg)) "?)")
              base)
        err {:type :unresolved-symbol
             :symbol nm
             :suggestions (vec sugg)
             :ns (compile-ns ctx)}
        pos (current-form-position)]
    (throw (ex-info msg {:jolt/error (if pos (merge err pos) err)}))))

(defn- analyze-symbol [ctx form env]
  (let [nm (form-sym-name form) ns (form-sym-ns form)]
    (cond
      (and (nil? ns) (local? env nm))
        (let [h (get (:hints env) nm)] (if h (assoc (local nm) :hint h) (local nm)))
      ;; Qualified instance method (Clojure 1.12) used as a value — not yet
      ;; supported as a method reference. The call form (Class/.method target ...)
      ;; works; a bare Class/.method as a value is a residual.
      (and ns (> (count nm) 1) (= "." (subs nm 0 1)))
        (uncompilable
         (str "Qualified instance method " (str ns "/" nm)
              " used as value; value form not yet supported. Use (.method target ...) or (Class/.method target ...) instead."))
      ns (let [r (resolve-global ctx form)]
           (if (= :var (:kind r))
             (or (macro-value-fn ctx form r)
                 (do (deny-macro-value ctx form r)
                     (cond-> (var-ref (:ns r) (:name r)) (:num-ret r) (assoc :num-ret (:num-ret r)))))
             ;; A non-var qualified ref `Class/member` is a host class static
             ;; (Math/sqrt, Long/MAX_VALUE, System/getenv). The Chez back end
             ;; lowers it to a runtime static dispatch.
             (host-static ns nm)))
      :else (let [r (resolve-global ctx form)]
              (case (:kind r)
                ;; :num-ret (a ^double/^long declared return) rides on the var node so
                ;; jolt.passes.numeric types a call to it (an accumulator over the result).
                :var (or (macro-value-fn ctx form r)
                         (do (deny-macro-value ctx form r)
                             (cond-> (var-ref (:ns r) (:name r)) (:num-ret r) (assoc :num-ret (:num-ret r)))))
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
                ;; reads it — keeps nREPL permissive, where a name may
                ;; legitimately arrive in a later eval.
                (if (allow-unresolved-vars?)
                  (var-ref (compile-ns ctx) nm)
                  (resolve-error ctx nm env)))))))

;; The wrapping unchecked-* name a core arithmetic op rewrites to under
;; *unchecked-math*, or nil. n is the full item count (head + args); unary - is a
;; negate.
(defn- unchecked-arith [hname n]
  (cond
    (= hname "+") "unchecked-add"
    (= hname "*") "unchecked-multiply"
    (= hname "-") (if (= n 2) "unchecked-negate" "unchecked-subtract")
    (= hname "inc") "unchecked-inc"
    (= hname "dec") "unchecked-dec"
    :else nil))

;; A non-shadowed clojure.core numeric cast (double/long/int/float of one arg)
;; becomes a :coerce node carrying the checked runtime helper, so it feeds the
;; numeric lattice like a ^double/^long hint: (* (double x) 2.0) emits fl*. The
;; helper preserves clojure.core's full JVM semantics (checked, not bare
;; jolt->fl). float -> double-kind (Jolt has no single-float); int ->
;; long-kind (Jolt's integer-box model), routing to jolt-int-cast so the JVM int
;; range is still enforced. Returns {:kind .. :cast-fn ..} or nil. n is the full
;; item count (head + args).
(defn- num-cast [hname n]
  (when (= n 2)
    (cond (= hname "double") {:kind :double :cast-fn "jolt-double"}
          (= hname "long")   {:kind :long   :cast-fn "jolt-long-cast"}
          (= hname "int")    {:kind :long   :cast-fn "jolt-int-cast"}
          (= hname "float")  {:kind :double :cast-fn "jolt-float"}
          ;; the unchecked casts are casts too: a primitive long/int on the JVM,
          ;; so the result feeds the :long lattice the same way. Their wrap
          ;; result can be a bignum past the 61-bit fixnum, which is the case
          ;; every :long consumer already guards (the jolt-l* fixnum? tests) —
          ;; the same reason a ^long param is admitted. Before this they were
          ;; ordinary var calls and left every loop counter they fed untyped.
          (= hname "unchecked-long") {:kind :long :cast-fn "jolt-unchecked-long"}
          (= hname "unchecked-int")  {:kind :long :cast-fn "jolt-unchecked-int"})))

(defn- analyze-list* [ctx form env]
  (let [items (vec (form-elements form))]
    (if (zero? (count items))
      (quote-node form)
      (let [head (first items)
            hname (when (and (form-sym? head) (nil? (form-sym-ns head))) (form-sym-name head))
            ;; a special-form head may arrive clojure.core-qualified: syntax-quote
            ;; namespace-qualifies a macro like `letfn` to `clojure.core/letfn`
            ;; (matching Clojure, where it is a macro), so a macro-emitted
            ;; (clojure.core/letfn …) must still dispatch to the special form.
            ;; A `qualified-only` head is the reverse: bare, it is just a name.
            sf-name (or (when-not (contains? qualified-only hname) hname)
                        (when (and (form-sym? head)
                                   (= "clojure.core" (form-sym-ns head))
                                   (contains? handled (form-sym-name head)))
                          (form-sym-name head)))
            shadowed (and hname (local? env hname))
            ;; under *unchecked-math*, a core +/-/*/inc/dec becomes its wrapping
            ;; unchecked-* (computed once; nil when off or not such an op). The op
            ;; may arrive bare (+) or clojure.core-qualified (clojure.core/*), the
            ;; latter from a macro's syntax-quote — both must wrap.
            unm (when (unchecked-math?)
                  (let [opn (cond (and hname (not shadowed)) hname
                                  (and (form-sym? head) (= "clojure.core" (form-sym-ns head)))
                                  (form-sym-name head))]
                    (when opn (unchecked-arith opn (count items)))))
            ;; a non-shadowed clojure.core numeric cast (bare or clojure.core/-
            ;; qualified, the latter from syntax-quote) becomes a checked :coerce
            ;; node. qn mirrors unm's opn so (clojure.core/double x) specializes too.
            cast (let [qn (cond (and hname (not shadowed)) hname
                                (and (form-sym? head) (= "clojure.core" (form-sym-ns head)))
                                (form-sym-name head))]
                   (when qn (num-cast qn (count items))))]
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
            ;; defn/defn- expand to (def name (fn …)); carry the ORIGINAL form's
            ;; source offset onto the resulting def, since the macro builds a fresh
            ;; (def …) with no metadata. So the back end can register fn defs.
            (let [node (analyze ctx (form-expand-1 ctx form (amp-env-map env)) env)
                  p (form-position form)]
              (if (and p (= :def (:op node))) (stamp-def-pos ctx env node p) node))
          ;; jolt.ffi/__cfn — the foreign-function special form (always emitted
          ;; fully-qualified by the jolt.ffi/foreign-fn macro, so aliases resolve).
          (and (form-sym? head) (= "jolt.ffi" (form-sym-ns head))
               (= "__cfn" (form-sym-name head)))
            (analyze-ffi-fn ctx items env)
          ;; jolt.ffi/layout expands to this literal-only leaf. The back end
          ;; asks Chez for the actual ABI size, alignment and offsets.
          (and (form-sym? head) (= "jolt.ffi" (form-sym-ns head))
               (= "__layout" (form-sym-name head)))
            (analyze-ffi-layout items)
          ;; jolt.ffi/__ccallable — the foreign-callback special form (the fn is a
          ;; child expression, analyzed here).
          (and (form-sym? head) (= "jolt.ffi" (form-sym-ns head))
               (= "__ccallable" (form-sym-name head)))
            (analyze-ffi-callable ctx items env)
          ;; special-form heads are NOT shadowable (unlike macros): a local named
          ;; `if` does not change the meaning of (if …) in operator position, per
          ;; spec §3 and the reference. No (not shadowed) guard here.
          (and sf-name (contains? handled sf-name))
            ;; stamp the form's source offset onto a top-level def so the back end
            ;; can register it (jv$ns$name -> source) for native stack traces.
            (let [node (analyze-special ctx sf-name form items env)
                  p (form-position form)]
              (if (and p (= :def (:op node))) (stamp-def-pos ctx env node p) node))
          (and hname (not shadowed) (method-head? hname))
            (stamp-pos (analyze-host-call ctx hname items env) form)
          ;; (Class. args*) — trailing-dot constructor sugar.
          (and hname (not shadowed) (ctor-head? hname))
            (analyze-ctor ctx (subs hname 0 (dec (count hname))) (rest items) env)
          ;; (new Class args*) — explicit constructor.
          (and (= hname "new") (not shadowed) (>= (count items) 2)
               (form-sym? (nth items 1)))
            (analyze-ctor ctx (form-sym-name (nth items 1)) (drop 2 items) env)
          ;; (. target member arg*) — the `.` special form.
          (and (= hname ".") (not shadowed))
            (stamp-pos (analyze-dot ctx items env) form)
          ;; (.-field target) — field-access head.
          (and hname (not shadowed) (field-head? hname))
            (stamp-pos (analyze-field ctx hname items env) form)
          (and hname (not shadowed) (form-special? hname))
            (uncompilable (str "special form " hname))
          ;; (ns/Name. args*) — a QUALIFIED trailing-dot constructor (a cross-ns or
          ;; aliased deftype, e.g. sci.impl.types/Reified.). hname is nil for a
          ;; namespaced head, so the bare ctor-head? arm above never sees it;
          ;; reconstruct the "ns/Name" class and let analyze-ctor resolve it.
          (and (form-sym? head) (form-sym-ns head) (not shadowed)
               (let [n (form-sym-name head)]
                 (and (> (count n) 1) (= "." (subs n (dec (count n)))))))
            (let [n (form-sym-name head)]
              (analyze-ctor ctx (str (form-sym-ns head) "/" (subs n 0 (dec (count n))))
                            (rest items) env))
          ;; (Class/.method target arg*) — qualified instance method call (Clojure 1.12).
          ;; The class part is a type hint; dispatch on the first arg's runtime type.
          (and (form-sym? head) (form-sym-ns head)
               (let [n (form-sym-name head)]
                 (and (> (count n) 1) (= "." (subs n 0 1)))))
            (stamp-pos (analyze-host-call ctx (form-sym-name head) items env) form)
          ;; (Class/MEMBER) with no arguments carries the same ambiguity that
          ;; (. Class MEMBER) does, and Clojure reads it as a static field when one
          ;; exists: (Math/PI), (Integer/MAX_VALUE) and (Locale/US) all evaluate to
          ;; the field. Resolve it at runtime through the same seam the dot form
          ;; uses, instead of invoking whatever the member holds, which failed
          ;; outright when that was a value rather than a no-arg method. A var head
          ;; is excluded so a zero-arg (some.ns/f) call is untouched.
          (and (form-sym? head) (form-sym-ns head) (not shadowed)
               (empty? (rest items))
               (not= :var (:kind (resolve-global ctx head))))
            (invoke (var-ref "jolt.host" "static-member")
                    [(const (form-sym-ns head)) (const (form-sym-name head))])
          cast (let [node (coerce-node (:kind cast) (analyze ctx (second items) env)
                                       (:cast-fn cast))
                     p (form-position form)]
                 (if p (assoc node :pos p) node))
          :else
            ;; stamp the list form's source offset onto the :invoke
            ;; so the success checker can report file:line:col. nil when the
            ;; reader did not record it (synthetic/macro-built forms).
            (let [n (invoke (analyze ctx head env)
                            (mapv #(analyze ctx % env) (rest items)))
                  p (form-position form)]
              (if p (assoc n :pos p) n)))))))

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

;; Anything raised while analyzing a form is a COMPILE-time failure, and the
;; reporter can only tell — and only knows where to point — when the throw carries
;; a :jolt/error map. With one it names the innermost positioned form and drops the
;; analyzer's own recursion from the trace; without one it falls back to the
;; LOADER's per-top-level-form position and prints thirty lines of jolt internals.
;; resolve-error was the only thing that built one, so every other compile failure
;; — an uncompilable form, a destructuring pattern the desugarer rejects, a macro
;; that threw while expanding — reported the top-level form's opening line over a
;; trace naming nothing the reader can act on.
;;
;; Attaching it here, at the one entry every top-level analysis goes through, costs
;; a single try per top-level form and covers all of them. A throw that already
;; carries :jolt/error passes through untouched: it knows its own position better.
(defn- analysis-diagnostic? [e]
  (let [d (ex-data e)] (and (some? d) (contains? d :jolt/error))))

(defn- throw-message [e]
  (cond (string? e) e
        ;; an ex-info reports as its message alone; anything else keeps the
        ;; "java.lang.Foo: msg" text the reporter would have printed for it
        (some? (ex-data e)) (ex-message e)
        :else (str e)))

;; No position to add (a macro-built form, or a form handed straight to eval) means
;; nothing to improve on, so leave the throw exactly as it was rather than trading
;; its trace for a diagnostic that says no more than the fallback already does.
(defn- as-analysis-diagnostic [e]
  (let [pos (current-form-position)]
    (if (or (nil? pos) (analysis-diagnostic? e))
      e
      (ex-info (throw-message e) {:jolt/error (merge {:type :analysis-error} pos)}))))

(defn analyze
  ([ctx form]
   ;; One position box per compilation, over the catch too — as-analysis-diagnostic
   ;; reads it. `or` rather than a fresh box every time: an analyze that re-enters
   ;; this arity (a macro analyzing a form it built) keeps the chain it is nested
   ;; inside, which is what the single shared atom used to give it on one thread.
   (binding [*positioned-form-box* (or *positioned-form-box* (atom nil))]
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
     (form-tagged? form) (uncompilable (str "No reader function for tag "
                                            (form-tag-name form)))
     :else (uncompilable "unsupported form"))))
