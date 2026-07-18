(ns jolt.ir
  "Host-neutral intermediate representation for the Jolt compiler.

  The analyzer (jolt.analyzer) produces IR; a host back end consumes it. IR nodes
  are plain maps tagged with :op — no host values embedded. Globals reference vars
  by name (:ns/:name), never by a host var cell, so the IR is portable and
  AOT-safe. This namespace is pure Clojure (portable jolt-core): it depends on
  nothing host-specific.")

;; Node constructors. Kept as data so any back end can pattern-match on :op.

(defn const [v] {:op :const :val v})

(defn local [name] {:op :local :name name})

;; A global var reference, by name. The back end resolves it to a host var.
(defn var-ref [ns name] {:op :var :ns ns :name name})

;; The var object itself — (var x) / #'x. Unlike var-ref (which derefs), the back
;; end emits the embedded var cell so `binding`'s thread-binding frame can key on it.
(defn the-var [ns name] {:op :the-var :ns ns :name name})

;; A name that resolves only via the host's own environment (e.g. + or int?) —
;; the back end emits a host-appropriate reference.
(defn host-ref [name] {:op :host :name name})

;; A qualified static reference to a host class member, `Class/member` (e.g.
;; Math/sqrt, Long/MAX_VALUE, System/getenv). A leaf node carrying the class and
;; member names. The Chez back end lowers a value ref to host-static-ref and a
;; call head to host-static-call (host-static.ss).
(defn host-static [class member] {:op :host-static :class class :member member})

;; A host constructor, `(Class. args*)` / `(new Class args*)`. Carries the class
;; name and the analyzed argument nodes. Chez lowers to host-new (host-static.ss
;; class-ctor registry).
(defn host-new [class args] {:op :host-new :class class :args args})

(defn if-node [test then else] {:op :if :test test :then then :else else})

(defn do-node [statements ret] {:op :do :statements statements :ret ret})

(defn invoke [f args] {:op :invoke :fn f :args args})

;; meta is the var metadata (e.g. {:dynamic true} / {:redef true}) the back end
;; applies to the cell; absent when the def name carried none.
(defn def-node
  ([ns name init] {:op :def :ns ns :name name :init init})
  ([ns name init meta]
   (if meta
     {:op :def :ns ns :name name :init init :meta meta}
     {:op :def :ns ns :name name :init init})))

(defn let-node [bindings body] {:op :let :bindings bindings :body body})

;; A fn is one or more arities. Each arity: {:params [..] :body ir}, plus :rest
;; name when variadic. :name is absent for an anonymous fn.
(defn fn-node [name arities]
  (if name
    {:op :fn :name name :arities arities}
    {:op :fn :arities arities}))

(defn vector-node [items] {:op :vector :items items})
(defn map-node [pairs] {:op :map :pairs pairs})
(defn set-node [items] {:op :set :items items})

(defn quote-node [form] {:op :quote :form form})
(defn throw-node [expr] {:op :throw :expr expr})

;; Numeric coercion of a value to a primitive kind (:double / :long), the way a JVM
;; ^double/^long parameter or return coerces. The back end lowers it (exact->inexact
;; / jolt->fx) and jolt.passes.numeric reads its :kind as the value's numeric kind.
;; Carrying coercion as an IR node (rather than a back-end string wrap) lets it
;; travel with inlining and keeps the typed-arithmetic fast path sound.
;;
;; The 3-arg form carries :cast-fn, the checked runtime helper a USER cast lowers to
;; (jolt-double / jolt-long-cast / jolt-int-cast / jolt-float): a (double x) cast must
;; preserve clojure.core/double's full JVM semantics (ClassCastException on non-number,
;; (long ##NaN) => 0, out-of-range throw), so it can NOT use the bare exact->inexact a
;; proven-typed param coercion uses. The 2-arg form (inline.clj typed params) stays bare.
(defn coerce-node
  ([kind expr] {:op :coerce :kind kind :expr expr})
  ([kind expr cast-fn] {:op :coerce :kind kind :expr expr :cast-fn cast-fn}))

;; ---------------------------------------------------------------------------
;; IR schema.
;;
;; Every node is a map with an :op tag. Some ops have a constructor above; the
;; analyzer builds the rest as map literals (they carry host-specific leaf data —
;; regex/inst/uuid/bigdec sources, ffi signatures, host-call method names — that a
;; constructor would only wrap). The full op vocabulary and the keys each op
;; carries are listed here so the schema has a single written source, and
;; node-problems / tree-problems below check a node against it.
;;
;; op            required keys (besides :op)      child-node positions*
;; ------------  ------------------------------   ------------------------------
;; :const        :val                             —  (leaf; :val is a literal)
;; :local        :name                            —
;; :var          :ns :name                        —
;; :the-var      :ns :name                        —
;; :host         :name                            —
;; :host-static  :class :member                   —
;; :host-new     :class :args                     :args
;; :if           :test :then :else                :test :then :else
;; :do           :statements :ret                 :statements :ret
;; :invoke       :fn :args                        :fn :args
;; :def          :ns :name                        :init? :meta-expr?   (init absent = declare)
;; :let          :bindings :body                  binding inits, :body
;; :loop         :bindings :body                  binding inits, :body
;; :recur        :args                            :args
;; :fn           :arities                          each arity :body
;; :vector       :items                           :items
;; :map          :pairs                           each pair key + value
;; :set          :items                           :items
;; :quote        :form                            —  (:form is unanalyzed)
;; :throw        :expr                            :expr
;; :coerce       :kind :expr                       :expr
;; :try          :body                            :body :catch-body? :finally?
;; :host-call    :target :method :args            :target :args
;; :set-var      :the-var :val                    :val   (:the-var is a leaf the-var node)
;; :set-field    :obj :field :val                 :obj :val
;; :defmacro     :ns :name :fn                     :fn
;; :ffi-fn       :csym :argtypes :rettype         —
;; :ffi-callable :fn :argtypes :rettype           :fn
;; :regex        :source                          —
;; :inst         :source                          —
;; :uuid         :source                          —
;; :bigdec       :source                          —
;; :the-ns       :name                            —
;;
;;  * the positions map-ir-children / reduce-ir-children recurse. A `?` marks an
;;    optional position recursed only when present.
;;
;; Annotation keys — optional, attached to any node by a later pass; a back end or
;; pass reads them but a node is valid without them. WHO attaches / reads each:
;;   :hint :shape :nilable   collection-type inference (jolt.passes.types) — the
;;                           inferred type / struct shape / nilable flag on a node.
;;   :num-kind :num-read     numeric pass (jolt.passes.numeric) — an :invoke's
;;                           proven :double/:long arithmetic kind, and a field
;;                           read's numeric kind.
;;   :devirt-type :devirt-*  a monomorphic protocol call's resolved impl (backend).
;;   :num-ret                a ^double/^long declared return, on a :var node.
;;   :phints :nhints         per-arity ^Record / ^double param hints (analyzer).
;;   :ret-nhint              a fn arity's declared numeric return kind.
;;   :recur-name             the loop/fn recur target's emitted label.
;;   :no-init                a :def with no initializer (declare).
;;   :meta-expr              a :def's evaluated metadata expression.
;;   :pos                    reader source position {:line :column :file}.
;;   :letrec                 a :let that must lower to letrec* (mutual recursion).
(def node-ops
  #{:const :local :var :the-var :host :host-static :host-new :if :do :invoke :def
    :let :loop :recur :fn :vector :map :set :quote :throw :coerce :try :host-call
    :set-var :set-field :defmacro :ffi-fn :ffi-callable :regex :inst :uuid :bigdec
    :the-ns})

;; op -> the keys a node of that op must carry. Optional keys (:init, annotations)
;; are not listed. Kept conservative for the leaf/host ops so a shape change
;; upstream is a deliberate edit here, not a silent validator false-positive.
(def required-node-keys
  {:const [:val] :local [:name] :var [:ns :name] :the-var [:ns :name]
   :host [:name] :host-static [:class :member] :host-new [:class :args]
   :if [:test :then :else] :do [:statements :ret] :invoke [:fn :args]
   :def [:ns :name] :let [:bindings :body] :loop [:bindings :body] :recur [:args]
   :fn [:arities] :vector [:items] :map [:pairs] :set [:items] :quote [:form]
   :throw [:expr] :coerce [:kind :expr] :try [:body]
   :host-call [:target :method :args] :set-var [:the-var :val]
   :set-field [:obj :field :val] :defmacro [:ns :name :fn]
   :ffi-fn [:csym :argtypes :rettype] :ffi-callable [:fn :argtypes :rettype]
   :regex [:source] :inst [:source] :uuid [:source] :bigdec [:source]
   :the-ns [:name]})

;; Problems with THIS node's shape (not its children): an unknown or missing :op,
;; or a missing required key. Returns a seq of message strings (empty when valid).
(defn node-problems [node]
  (let [op (get node :op)]
    (cond
      (not (map? node)) (list (str "IR node is not a map: " (pr-str node)))
      (nil? op)         (list (str "IR node has no :op: " (pr-str node)))
      (not (contains? node-ops op)) (list (str "unknown IR :op " op))
      :else (let [missing (remove (fn [k] (contains? node k))
                                  (get required-node-keys op []))]
              (when (seq missing)
                (list (str op " node missing required key(s) " (vec missing))))))))

;; ---------------------------------------------------------------------------
;; Structural recursion over IR child nodes.
;;
;; A tree-rewriting pass recurses into each op's child NODE positions and
;; rebuilds the node; this combinator does that one place, so the per-op child
;; layout is single-sourced and adding an op is a one-site change here (was: an
;; edit to every walk). `(map-ir-children f node)` returns node with f applied to
;; each child IR node — re-applied per element for seq positions (:args/:items/
;; :statements), per value for :map pairs, per init for :let/:loop bindings, and
;; per arity :body for :fn. Non-node positions (binding NAMES, fn :params/:rest,
;; the :op tag, :ns/:name/:val) are left intact. Leaf ops and any op with no
;; child nodes pass through unchanged, so walks built on this are TOTAL over the
;; op set (an unknown op recurses nowhere rather than being silently dropped).
;;
;; Uses cond/=/get only — same constructs as the passes that consume it, so it
;; loads at the same compiler tier with no new macro dependency.
(defn map-ir-children [f node]
  (let [op (get node :op)]
    (cond
      (= op :if)     (assoc node :test (f (get node :test))
                                 :then (f (get node :then))
                                 :else (f (get node :else)))
      (= op :do)     (assoc node :statements (mapv f (get node :statements))
                                 :ret (f (get node :ret)))
      (= op :throw)  (assoc node :expr (f (get node :expr)))
      (= op :coerce) (assoc node :expr (f (get node :expr)))
      (= op :set-var) (assoc node :val (f (get node :val)))
      (= op :set-field) (assoc node :obj (f (get node :obj)) :val (f (get node :val)))
      (= op :defmacro) (assoc node :fn (f (get node :fn)))
      (= op :ffi-callable) (assoc node :fn (f (get node :fn)))
      (= op :invoke) (assoc node :fn (f (get node :fn))
                                 :args (mapv f (get node :args)))
      (= op :vector) (assoc node :items (mapv f (get node :items)))
      (= op :set)    (assoc node :items (mapv f (get node :items)))
      (= op :map)    (assoc node :pairs (mapv (fn [pr] [(f (nth pr 0)) (f (nth pr 1))])
                                              (get node :pairs)))
      (= op :let)    (assoc node :bindings (mapv (fn [b] [(nth b 0) (f (nth b 1))])
                                                 (get node :bindings))
                                 :body (f (get node :body)))
      (= op :loop)   (assoc node :bindings (mapv (fn [b] [(nth b 0) (f (nth b 1))])
                                                 (get node :bindings))
                                 :body (f (get node :body)))
      (= op :recur)  (assoc node :args (mapv f (get node :args)))
      (= op :fn)     (assoc node :arities (mapv (fn [a] (assoc a :body (f (get a :body))))
                                                (get node :arities)))
      (= op :def)    (let [n (if (get node :init) (assoc node :init (f (get node :init))) node)]
                       (if (get node :meta-expr)
                         (assoc n :meta-expr (f (get node :meta-expr)))
                         n))
      (= op :host-call) (assoc node :target (f (get node :target))
                                    :args (mapv f (get node :args)))
      (= op :host-new) (assoc node :args (mapv f (get node :args)))
      ;; :catch-body / :finally are optional; recurse them only when PRESENT.
      ;; Assoc'ing them nil-when-absent would turn the node into a phm (jolt's
      ;; nil-valued-key representation) and force backend densification — so we
      ;; preserve the node's shape and never introduce a nil key.
      (= op :try)
      (let [n (assoc node :body (f (get node :body)))
            n (if (get node :catch-body) (assoc n :catch-body (f (get node :catch-body))) n)
            n (if (get node :finally) (assoc n :finally (f (get node :finally))) n)]
        n)
      ;; :const :local :var :host :host-static :the-var :quote — no child nodes
      :else node)))

;; The read-only companion to map-ir-children: fold f over node's child IR nodes,
;; left to right, threading acc — same single-sourced child layout, so a read-only
;; analysis (size/closedness/purity) built on it is TOTAL over the op set (an
;; unknown op, or a leaf, folds over no children and returns acc unchanged). Skips
;; the same non-node positions map-ir-children does (binding NAMES, fn :params/
;; :rest, :op/:ns/:name/:val). f is (acc child) -> acc.
(defn reduce-ir-children [f acc node]
  (let [op (get node :op)]
    (cond
      (= op :if) (f (f (f acc (get node :test)) (get node :then)) (get node :else))
      (= op :do) (f (reduce f acc (get node :statements)) (get node :ret))
      (= op :throw) (f acc (get node :expr))
      (= op :coerce) (f acc (get node :expr))
      (= op :set-var) (f acc (get node :val))
      (= op :set-field) (f (f acc (get node :obj)) (get node :val))
      (= op :defmacro) (f acc (get node :fn))
      (= op :ffi-callable) (f acc (get node :fn))
      (= op :invoke) (reduce f (f acc (get node :fn)) (get node :args))
      (= op :vector) (reduce f acc (get node :items))
      (= op :set) (reduce f acc (get node :items))
      (= op :map) (reduce (fn [a pr] (f (f a (nth pr 0)) (nth pr 1))) acc (get node :pairs))
      (= op :let) (f (reduce (fn [a b] (f a (nth b 1))) acc (get node :bindings)) (get node :body))
      (= op :loop) (f (reduce (fn [a b] (f a (nth b 1))) acc (get node :bindings)) (get node :body))
      (= op :recur) (reduce f acc (get node :args))
      (= op :fn) (reduce (fn [a ar] (f a (get ar :body))) acc (get node :arities))
      (= op :def) (let [a (if (get node :init) (f acc (get node :init)) acc)]
                    (if (get node :meta-expr) (f a (get node :meta-expr)) a))
      (= op :host-call) (reduce f (f acc (get node :target)) (get node :args))
      (= op :host-new) (reduce f acc (get node :args))
      (= op :try)
      (let [a (f acc (get node :body))
            a (if (get node :catch-body) (f a (get node :catch-body)) a)
            a (if (get node :finally) (f a (get node :finally)) a)]
        a)
      ;; leaves and any op with no child nodes
      :else acc)))

;; All schema problems in the tree rooted at node, via reduce-ir-children (so an
;; unknown op — which recurses over no children — is still reported for itself).
;; Returns a seq of message strings, empty when the whole tree conforms.
(defn tree-problems [node]
  (reduce-ir-children (fn [acc child] (concat acc (tree-problems child)))
                      (node-problems node)
                      node))
