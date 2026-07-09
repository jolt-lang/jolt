;; Context-bound dynamic vars, run as a FILE with args (smoke.sh passes a1 a2):
;; *file*/*source-path* bind during a load, *command-line-args* carries the app
;; args, *agent* binds inside an agent action. Prints CTXVARS OK when all hold.
(def failures (atom []))
(defn chk [label ok] (when-not ok (swap! failures conj label)))

(chk "*file* ends with ctxvars-test.clj"
     (clojure.string/ends-with? (str *file*) "ctxvars-test.clj"))
(chk "*source-path* is the bare name" (= *source-path* "ctxvars-test.clj"))
(chk "*command-line-args*" (= *command-line-args* '("a1" "a2")))
(chk "*repl* false outside a repl" (false? *repl*))
(chk "*agent* nil outside an action" (nil? *agent*))

(def ag (agent 0))
(def seen (atom nil))
(send ag (fn [s] (reset! seen (identical? *agent* ag)) s))
(await ag)
(chk "*agent* bound inside an action" (true? @seen))

(chk "ns-map resolves core fns" (var? (get (ns-map *ns*) 'inc)))
(chk "ns-refers includes implicit refer-clojure" (var? (get (ns-refers 'user) 'map)))

(if (empty? @failures)
  (println "CTXVARS OK")
  (doseq [f @failures] (println "FAIL:" f)))
