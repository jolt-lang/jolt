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

;; A runtime primitive (cons, +, get, apply, …) the back end maps to the host RT.
(defn rt [name] {:op :rt :name name})

;; A name that resolves only via the host's own environment (e.g. + or int? on
;; Janet) — the back end emits a host-appropriate reference.
(defn host-ref [name] {:op :host :name name})

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
