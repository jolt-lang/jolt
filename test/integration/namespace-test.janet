(use ../../src/jolt/reader)
(use ../../src/jolt/types)
(use ../../src/jolt/evaluator)
(import ../../src/jolt/api :as api)

# ns/in-ns/require/use are overlay macros + clojure.core fns now (Stage 2 jolt-eaa),
# so these interpreter tests need the full env (init loads the overlay + installs
# the stateful fns), not a bare make-ctx.
(defn- fresh-ctx [] (api/init))

# Helper: parse and eval in a fresh ctx
(defn eval-str [s]
  (let [ctx (fresh-ctx)
        form (parse-string s)]
    (eval-form ctx @{} form)))

(print "1: in-ns...")
(let [ctx (fresh-ctx)]
  (def form (parse-string "(in-ns 'my.app)"))
  (eval-form ctx @{} form)
  (assert (= "my.app" (ctx-current-ns ctx)) "in-ns switches namespace"))
(print "  passed")

(print "2: def in different namespace...")
(let [ctx (fresh-ctx)]
  (eval-form ctx @{} (parse-string "(in-ns 'my.app)"))
  (eval-form ctx @{} (parse-string "(def x 42)"))
  (let [ns (ctx-find-ns ctx "my.app")
        v (ns-find ns "x")]
    (assert (= 42 (var-get v)) "def works in new namespace")))
(print "  passed")

(print "3: ns form...")
(let [ctx (fresh-ctx)]
  (eval-form ctx @{} (parse-string "(ns my.lib)"))
  (assert (= "my.lib" (ctx-current-ns ctx)) "ns sets current namespace"))
(print "  passed")

(print "4: ns with require...")
(let [ctx (fresh-ctx)]
  # Set up a namespace with some vars
  (let [other-ns (ctx-find-ns ctx "other.lib")]
    (ns-intern other-ns "f" (fn [x] (inc x))))
  # Now ns with require
  (eval-form ctx @{} (parse-string "(ns my.app (:require [other.lib :as o]))"))
  # current-ns should be my.app
  (assert (= "my.app" (ctx-current-ns ctx)) "ns with require sets current namespace")
  # Alias should be registered
  (let [ns (ctx-find-ns ctx "my.app")
        aliased (ns-import-lookup ns "o")]
    (assert (= "other.lib" aliased) "alias o -> other.lib registered")))
(print "  passed")

(print "5: require form (standalone)...")
(let [ctx (fresh-ctx)]
  (eval-form ctx @{} (parse-string "(require '[other.lib :as o])"))
  (let [ns (ctx-find-ns ctx "user")
        aliased (ns-import-lookup ns "o")]
    (assert (= "other.lib" aliased) "standalone require registers alias")))
(print "  passed")

(print "6: qualified symbol via alias...")
(let [ctx (fresh-ctx)]
  # Set up target ns
  (let [target (ctx-find-ns ctx "other.lib")]
    (ns-intern target "f" (fn [x] (inc x))))
  # Register alias
  (let [ns (ctx-find-ns ctx "user")]
    (ns-import ns "o" "other.lib"))
  # Resolve o/f and call it
  (let [form (parse-string "(o/f 41)")
        result (eval-form ctx @{} form)]
    (assert (= 42 result) "qualified call via alias works")))
(print "  passed")

(print "7: require then use alias...")
(let [ctx (fresh-ctx)]
  # Set up target ns
  (let [target (ctx-find-ns ctx "math.lib")]
    (ns-intern target "add" (fn [a b] (+ a b))))
  # require + use
  (eval-form ctx @{} (parse-string "(require '[math.lib :as m])"))
  (let [result (eval-form ctx @{} (parse-string "(m/add 1 2)"))]
    (assert (= 3 result) "require + alias + call chain works")))
(print "  passed")

(print "8: ns form requires multiple...")
(let [ctx (fresh-ctx)]
  (let [ns1 (ctx-find-ns ctx "a.lib")]
    (ns-intern ns1 "f" (fn [x] (inc x))))
  (let [ns2 (ctx-find-ns ctx "b.lib")]
    (ns-intern ns2 "g" (fn [x] (dec x))))
  (eval-form ctx @{} (parse-string "(ns user (:require [a.lib :as a] [b.lib :as b]))"))
  (assert (= 43 (eval-form ctx @{} (parse-string "(a/f 42)"))) "alias a works")
  (assert (= 41 (eval-form ctx @{} (parse-string "(b/g 42)"))) "alias b works"))
(print "  passed")

(print "\nAll namespace tests passed!")
