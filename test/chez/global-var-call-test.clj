;; Semantics of global var calls (JOLT_GLOBAL_VAR_CALL=1, rt.ss "global vars").
;; Run with `jolt run`, knob set
;; or not: every check holds in both modes except the one marked GLOBAL, whose
;; changed answer is the edge case the write-up records. Prints
;; GLOBAL-VAR-CALL OK <mode> when every check holds. Each form is eval'd on its
;; own, as a REPL compiles it, so a caller is compiled after its callee's def.
(def failures (atom []))
(defn chk [label expected actual]
  (when-not (= expected actual)
    (swap! failures conj [label :expected expected :got actual])))
(def global? (let [e (System/getenv "JOLT_GLOBAL_VAR_CALL")]
               (and e (not= e "") (not= e "0"))))
(defmacro ev [& forms] `(do ~@(map (fn [f] `(eval '~f)) forms)))
(defn outcome [f] (try (f) (catch Throwable t [(symbol (.getName (class t))) (ex-message t)])))

;; --- redefinition reaches compiled callers ----------------------------------
(ev (defn r1 [x] [:v1 x])
    (defn c1 [x] (r1 x))
    (defn h1 [] (mapv r1 [1 2])))
(chk "first def" [:v1 1] (ev (c1 1)))
(ev (defn r1 [x] [:v2 x]))
(chk "def again" [:v2 1] (ev (c1 1)))
(chk "def again, value reference" [[:v2 1] [:v2 2]] (ev (h1)))
(ev (alter-var-root #'r1 (constantly (fn [x] [:altered x]))))
(chk "alter-var-root" [:altered 1] (ev (c1 1)))
(chk "with-redefs" [:redef 1] (ev (with-redefs [r1 (fn [x] [:redef x])] (c1 1))))
(chk "with-redefs restores" [:altered 1] (ev (c1 1)))
;; a different closure shape each time: a closure over a local, a variadic fn
(ev (let [k 41] (defn r1 [x] [:closure (+ k x)])))
(chk "redefined as a closure" [:closure 42] (ev (c1 1)))
(ev (defn r1 [& xs] [:variadic xs]))
(chk "redefined as variadic" [:variadic [1]] (ev (c1 1)))
(ev (.bindRoot #'r1 (fn [x] [:bound-root x])))
(chk ".bindRoot" [:bound-root 1] (ev (c1 1)))
(ev (intern 'user 'r1 (fn [x] [:interned x])))
(chk "intern over the var" [:interned 1] (ev (c1 1)))

;; --- declare, then define ----------------------------------------------------
(ev (declare later)
    (defn call-later [] (later 5))
    (defn ref-later [] later))
(chk "declared, unbound: call" ['java.lang.IllegalStateException
                               "Attempting to call unbound fn: #'user/later"]
     (outcome #(ev (call-later))))
(chk "declared, unbound: reference is the Unbound value"
     "clojure.lang.Var$Unbound" (.getName (class (ev (ref-later)))))
(chk "declared, unbound: Unbound value prints" "Unbound: #'user/later" (str (ev (ref-later))))
(ev (defn later [x] [:later x]))
(chk "declared, then defined" [:later 5] (ev (call-later)))
(chk "declared, then defined: reference" [:later 6] (ev ((ref-later) 6)))
;; a var whose def evaluates to the unbound state again is unbound again
(ev (def unb 1) (defn call-unb [] (unb)))
(ev (.unbindRoot #'unb))
(chk "unbound again after .unbindRoot" 'java.lang.IllegalStateException
     (first (outcome #(ev (call-unb)))))

;; --- every IFn kind held in a var --------------------------------------------
(ev (def kv :a)
    (def mv {:a 1 :b 2})
    (def sv #{:x})
    (def vv [10 20 30])
    (def symv 'q)
    (defn target [x] [:target x])
    (def varv #'target)
    (defmulti mm (fn [x] (if (odd? x) :odd :even)))
    (defmethod mm :odd [x] [:odd x])
    (defmethod mm :even [x] [:even x])
    (deftype IFnT [n] clojure.lang.IFn (invoke [_ x] [:type n x]))
    (defrecord IFnR [n] clojure.lang.IFn (invoke [_ x] [:record n x]))
    (def tv (->IFnT 1))
    (def rv (->IFnR 2))
    (def reiv (reify clojure.lang.IFn (invoke [_ x] [:reify x])))
    (defn kinds []
      [(kv {:a 7}) (kv {} :dflt) (mv :b) (mv :z :none) (sv :x) (sv :y) (vv 1)
       (symv '{q 3}) (varv 4) (mm 3) (mm 4) (tv 5) (rv 6) (reiv 7)]))
(chk "every IFn kind in a var is called as an IFn"
     [7 :dflt 2 :none :x nil 20 3 [:target 4] [:odd 3] [:even 4] [:type 1 5]
      [:record 2 6] [:reify 7]]
     (ev (kinds)))
(chk "IFn kinds passed as values" [[1 nil] [:x nil] [20 30]]
     (ev [(mapv kv [{:a 1} {}]) (mapv sv [:x :y]) (mapv vv [1 2])]))
;; a fn var that turns into a map and back
(ev (defn flip [x] :fn) (defn call-flip [] (flip :a)))
(ev (def flip {:a :map}))
(chk "fn var redefined as a map" :map (ev (call-flip)))
(ev (defn flip [x] :fn-again))
(chk "map var redefined as a fn" :fn-again (ev (call-flip)))

;; --- arity errors name the Clojure fn ----------------------------------------
(ev (defn ar1 [x] x)
    (defn ar-many [a b & more] [a b more])
    (defn call-ar1 [] (ar1 1 2))
    (defn call-ar-many [] (ar-many 1))
    (defn call-kv3 [] (kv {} 1 2)))
(chk "wrong arity" ['clojure.lang.ArityException "Wrong number of args (2) passed to: user/ar1"]
     (outcome #(ev (call-ar1))))
(chk "below a variadic fn's minimum" 'clojure.lang.ArityException
     (first (outcome #(ev (call-ar-many)))))
(chk "keyword with three args" 'clojure.lang.ArityException
     (first (outcome #(ev (call-kv3)))))

;; --- dynamic vars ------------------------------------------------------------
(ev (def ^:dynamic *d* :root)
    (defn ^:dynamic *df* [] :root-fn)
    (defn read-d [] *d*)
    (defn call-df [] (*df*)))
(chk "binding a dynamic var: value" :bound (ev (binding [*d* :bound] (read-d))))
(chk "binding a dynamic var: call" :bound-fn (ev (binding [*df* (fn [] :bound-fn)] (call-df))))
(chk "binding ends" [:root :root-fn] (ev [(read-d) (call-df)]))
(chk "binding seen by a future (conveyance)" :conveyed
     (ev (binding [*d* :conveyed] @(future (read-d)))))

;; A caller and a value reference compiled BEFORE .setDynamic see a later binding.
(ev (defn late [x] [:late-root x])
    (def late-v :v-root)
    (defn call-late [x] (late x))
    (defn read-late-v [] late-v))
(.setDynamic #'late)
(.setDynamic #'late-v)
(chk "late .setDynamic: compiled call sees the binding" [:late-bound 1]
     (ev (binding [late (fn [x] [:late-bound x])] (call-late 1))))
(chk "late .setDynamic: compiled value reference sees the binding" :v-bound
     (ev (binding [late-v :v-bound] (read-late-v))))
(chk "late .setDynamic: the root again" [[:late-root 2] :v-root]
     (ev [(call-late 2) (read-late-v)]))
;; a plain Thread conveys no binding (a future would)
(chk "late .setDynamic: a thread started inside the binding sees the root" [:late-root 3]
     (do (def late-p (promise))
         (ev (binding [late (fn [x] [:late-bound x])]
               (.start (Thread. (fn [] (deliver late-p (call-late 3)))))))
         (deref late-p 10000 :timeout)))
(ev (alter-var-root #'late (constantly (fn [x] [:late-altered x]))))
(chk "late .setDynamic: a root write after binding" [:late-altered 4] (ev (call-late 4)))

;; --- evaluation order --------------------------------------------------------
(def order (atom []))
(ev (defn ord [a b c] [a b c])
    (defn call-ord []
      (ord (do (swap! order conj 1) :a) (do (swap! order conj 2) :b) (do (swap! order conj 3) :c))))
(chk "operands evaluate left to right" [[:a :b :c] [1 2 3]] (ev [(call-ord) @order]))
(ev (defn self-redef [x] [:old x])
    (defn call-self-redef []
      (self-redef (do (alter-var-root #'self-redef (constantly (fn [x] [:new x]))) 7))))
;; GLOBAL: Clojure reads the operator before the operands and calls :old. A
;; global call reads the var's symbol when the call is made, after the operand
;; redefined it, and calls :new.
(chk "an operand that redefines the called var itself"
     (if global? [:new 7] [:old 7]) (ev (call-self-redef)))
(chk "...and the next call sees the redefinition" [:new 8] (ev (self-redef 8)))

;; --- ns-unmap and remove-ns --------------------------------------------------
;; jolt's ns-unmap keeps the cell and unbinds its root, so both modes agree.
(ev (defn um [] :um-1) (defn call-um [] (um)))
(ns-unmap 'user 'um)
(chk "after ns-unmap a compiled caller finds the var unbound" 'java.lang.IllegalStateException
     (first (outcome #(ev (call-um)))))
(ev (defn um [] :um-2))
(chk "a new def after ns-unmap" :um-2 (ev (call-um)))
;; remove-ns drops the cells. A caller compiled against the removed var holds
;; that var (and its root) in the default emission, as on the JVM, until the
;; namespace is defined again. GLOBAL: the symbol keeps the removed var's
;; wrapper until a new var of that name is interned, then names the new one.
(ev (create-ns 'gvc.gone) (intern 'gvc.gone 'f (fn [] :gone-1))
    (defn call-gone [] (gvc.gone/f)))
(remove-ns 'gvc.gone)
(chk "after remove-ns a compiled caller keeps the removed root" :gone-1 (ev (call-gone)))
(ev (create-ns 'gvc.gone) (intern 'gvc.gone 'f (fn [] :gone-2)))
(chk "a namespace defined again after remove-ns" (if global? :gone-2 :gone-1) (ev (call-gone)))

;; --- concurrent redefinition --------------------------------------------------
;; Callers race whole-value root writes that move the var between closures of
;; different shapes, a map and a multimethod. Every call must see one complete
;; value; none may fault.
(ev (defn race [x] :plain)
    (defn call-race [] (race :k)))
(let [n-threads 6 n-calls 20000 n-writes 6000
      shapes [(fn [x] :plain)
              (let [a 1 b 2] (fn [x] (if (= a 1) :closure b)))
              (fn [& xs] :variadic)
              {:k :map}
              (fn [x] :plain2)]
      ok #{:plain :closure :variadic :map :plain2}
      start (promise)
      workers (mapv (fn [_]
                      (future @start
                              (loop [i 0]
                                (if (= i n-calls) :done
                                    (let [v (call-race)]
                                      (if (ok v) (recur (inc i)) [:bad v]))))))
                    (range n-threads))]
  (deliver start true)
  (dotimes [i n-writes]
    (alter-var-root #'race (constantly (nth shapes (mod i (count shapes))))))
  (chk "callers through concurrent redefinitions" (repeat n-threads :done) (map deref workers)))

(shutdown-agents)
(if (empty? @failures)
  (println "GLOBAL-VAR-CALL OK" (if global? "global" "default"))
  (do (doseq [f @failures] (println "FAIL" f))
      (System/exit 1)))
