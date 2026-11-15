;; Redefinition semantics of the var-call experiments (pellet jolt-7,
;; benchmarks/chez-primitives/jolt-7-clj-call.md). Run with `jolt run` under each
;; JOLT_NATIVE_VAR_CALL / JOLT_DIRECT_LINK setting; prints CLJ-CALL-REDEF OK when every
;; check holds for the mode in the environment. Each form is eval'd on its own,
;; as a REPL would compile it, so a caller is compiled after its callee's def.
(def failures (atom []))
(defn chk [label expected actual]
  (when-not (= expected actual)
    (swap! failures conj [label :expected expected :got actual])))
(def dl (System/getenv "JOLT_DIRECT_LINK"))
;; JOLT_DIRECT_LINK=full is *compiler-options* {:direct-linking true}, whose
;; contract test/chez/repl-direct-link-test.clj covers; this file checks the
;; open world, native var calls and JOLT_DIRECT_LINK=link.
(when (= dl "full") (println "CLJ-CALL-REDEF SKIP full") (System/exit 0))
(defmacro ev [& forms] `(do ~@(map (fn [f] `(eval '~f)) forms)))
(defn try-call [f] (try (f) (catch Throwable t (symbol (.getName (class t))))))

;; A plain redefinition of the callee. Open world and link: the caller sees it
;; (a linked def writes the new root through to the jv$ binding).
(ev (defn f1 [x] (* x 10))
    (defn g1 [x] (f1 x)))
(chk "before redefinition" 10 (ev (g1 1)))
(ev (defn f1 [x] (* x 100)))
(chk "defn redefinition" 100 (ev (g1 1)))

;; alter-var-root and with-redefs write the root, which the linked binding follows.
(ev (defn f2 [x] (inc x))
    (defn g2 [x] (f2 x)))
(ev (alter-var-root #'f2 (constantly (fn [x] :altered))))
(chk "alter-var-root" :altered (ev (g2 1)))
(ev (defn f3 [x] (inc x))
    (defn g3 [x] (f3 x)))
(chk "with-redefs" :redef (ev (with-redefs [f3 (fn [x] :redef)] (g3 1))))

;; The callee becomes a value that is not a procedure. The open-world call
;; dispatches it as an IFn. A direct-linked caller compiled against the fn
;; applies the binding as a procedure and throws ClassCastException -- when it
;; was linked: on this base whether the caller links varies from run to run
;; (see jolt-7-clj-call.md), so under direct linking either outcome is accepted.
(ev (defn f4 [x] (inc x))
    (defn g4 [x] (f4 x)))
(ev (def f4 {1 :one}))
(let [r (try-call #(ev (g4 1)))]
  (if dl
    (chk "callee redefined as a map (linked or not)" true
         (contains? #{'java.lang.ClassCastException :one} r))
    (chk "callee redefined as a map" :one r)))

(ev (defn f4 [x] :function-again))
(chk "callee goes from map back to fn" :function-again (ev (g4 1)))

;; A local with the raw accessor's name cannot shadow the qualified primitive.
(ev (defn raw-accessor-shadow [$record-ref] (f1 1)))
(chk "raw accessor primitive cannot be shadowed" 100 (ev (raw-accessor-shadow :local)))

;; A wrong-arity call is an ArityException naming the fn in every mode.
(ev (defn f5 [x] x)
    (defn g5 [] (f5 1 2)))
(chk "wrong arity" 'clojure.lang.ArityException (try-call #(ev (g5))))
(chk "wrong arity message" "Wrong number of args (2) passed to: user/f5"
     (try (ev (g5)) (catch Throwable t (ex-message t))))

;; Keywords, maps and multimethods held in a var are still called as IFns.
(ev (def kv :a)
    (def mv {:x 1})
    (defmulti mm identity)
    (defmethod mm 3 [_] :three)
    (defn g6 [] [(kv {:a 7}) (mv :x) (mm 3)]))
(chk "non-procedure IFns in vars" [7 1 :three] (ev (g6)))

;; ^:redef opts a var out of linking in every mode.
(ev (defn ^:redef f7 [x] (inc x))
    (defn g7 [x] (f7 x)))
(ev (defn f7 [x] :redefined))
(chk "^:redef" :redefined (ev (g7 1)))

;; A caller compiled before .setDynamic must see later thread bindings.
;; This is stronger than compiling a fresh caller after the metadata changes.
(ev (defn f8 [x] :root)
    (defn g8 [x] (f8 x)))
(when-not dl
  (.setDynamic #'f8)
  (chk "already-compiled caller sees a later dynamic binding" :bound
       (ev (binding [f8 (fn [_] :bound)] (g8 1))))
  (chk "later dynamic binding restores the root" :root (ev (g8 1))))

;; Operator capture precedes argument effects even for a var made dynamic
;; after its caller was compiled. A late handler lookup cannot choose it again.
(ev (defn eager-f [x] [:root x])
    (defn eager-binding-g []
      (eager-f (do (var-set #'eager-f (fn [x] [:changed-binding x])) 7)))
    (defn eager-root-g []
      (eager-f (do (alter-var-root #'eager-f
                                  (constantly (fn [x] [:changed-root x]))) 7))))
(when-not dl
  (.setDynamic #'eager-f)
  (chk "late dynamic callee is captured before var-set argument" [:bound 7]
       (ev (binding [eager-f (fn [x] [:bound x])] (eager-binding-g))))
  ;; The binding above leaves dyn-bound? true after its frame has gone.
  (chk "formerly-bound callee is captured before root-write argument" [:root 7]
       (ev (eager-root-g))))

(ev (declare unbound-call)
    (defn g-unbound [] (unbound-call 1)))
(chk "unbound callee class" 'java.lang.IllegalStateException (try-call #(ev (g-unbound))))
(chk "unbound callee message" "Attempting to call unbound fn: #'user/unbound-call"
     (try (ev (g-unbound)) (catch Throwable t (ex-message t))))

;; Reading the operator precedes argument effects, including redefining it.
(def call-order (atom []))
(ev (defn ordered-f [x] [:old x])
    (defn ordered-g []
      (ordered-f (do (swap! call-order conj :argument)
                     (alter-var-root #'ordered-f (constantly (fn [x] [:new x])))
                     7))))
(when-not dl
  (chk "callee is read before argument redefinition" [:old 7] (ev (ordered-g)))
  (chk "argument evaluated once" [:argument] @call-order)
  (chk "next call sees argument's redefinition" [:new 7] (ev (ordered-g))))

;; Calls race whole-value root writes. Every read may see either complete
;; callable; none may retain a procedure-only assumption after the map write.
(ev (defn racing-f [x] :fn)
    (defn racing-g [] (racing-f :x)))
(when-not dl
  (let [start (promise)
        ready (vec (repeatedly 4 promise))
        workers (mapv (fn [r]
                        (future (deliver r true) @start
                                (loop [i 0]
                                  (if (= i 400) true
                                    (let [v (racing-g)]
                                      (if (contains? #{:fn :map} v)
                                        (recur (inc i)) v)))))) ready)]
    (doseq [r ready] @r)
    (deliver start true)
    (dotimes [i 400]
      (alter-var-root #'racing-f (constantly (if (even? i) {:x :map} (fn [_] :fn)))))
    (chk "threads call through concurrent fn/map redefinitions" [true true true true]
         (mapv deref workers))))

(shutdown-agents)

(if (empty? @failures)
  (println "CLJ-CALL-REDEF OK" (or dl "open-world") (or (System/getenv "JOLT_NATIVE_VAR_CALL") "default"))
  (do (doseq [f @failures] (println "FAIL" f))
      (System/exit 1)))
