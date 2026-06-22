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

;; A runtime primitive (cons, +, get, apply, …) the back end maps to the host RT.
(defn rt [name] {:op :rt :name name})

;; A name that resolves only via the host's own environment (e.g. + or int? on
;; Janet) — the back end emits a host-appropriate reference.
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

(defn op [node] (:op node))

;; ---------------------------------------------------------------------------
;; Structural recursion over IR child nodes (jolt-26dm / phase 3a).
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
      (= op :set-var) (assoc node :val (f (get node :val)))
      (= op :set-field) (assoc node :obj (f (get node :obj)) :val (f (get node :val)))
      (= op :defmacro) (assoc node :fn (f (get node :fn)))
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
      (= op :def)    (assoc node :init (f (get node :init)))
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
      ;; :const :local :var :host :host-static :the-var :rt :quote — no child nodes
      :else node)))
