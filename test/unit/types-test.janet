(use ../../src/jolt/types)

# ============================================================
# Var tests
# ============================================================

# make-var
(let [v (make-var 'x 42)]
  (assert (var? v) "var? returns true")
  (assert (= 42 (var-get v)) "var-get returns root binding")
  (assert (deep= {:name 'x} (var-meta v)) "var-meta returns metadata")
  (assert (deep= 'x (var-name v)) "var-name returns name symbol"))

# var without init value
(let [v (make-var 'y)]
  (assert (var? v) "unbound var is still a var"))

# dynamic var
(let [v (make-var '*dyn* 1 {:dynamic true})]
  (assert (var-dynamic? v) "var-dynamic? true")
  (assert (not (var-macro? v)) "var-macro? false for dynamic var"))

# macro var
(let [v (make-var 'when  nil {:macro true})]
  (assert (var-macro? v) "var-macro? true"))

# var-set — set root binding
(let [v (make-var 'x 1)]
  (var-set v 99)
  (assert (= 99 (var-get v)) "var-set changes root binding"))

# alter-var-root
(let [v (make-var 'c 0)]
  (alter-var-root v inc)
  (assert (= 1 (var-get v)) "alter-var-root applies fn"))

# with-meta — returns new var with updated meta
(let [v (make-var 'x 42)
      v2 (with-meta v {:private true})]
  (assert (deep= {:name 'x :private true} (var-meta v2)) "with-meta merges meta")
  (assert (= 42 (var-get v2)) "with-meta preserves root binding"))

# generation counter — bumps on every root change (substrate for direct-link
# staleness detection and redefinition-aware dispatch caches)
(let [v (make-var 'g 1)]
  (assert (= 0 (v :gen)) "fresh var starts at generation 0")
  (bind-root v 2)
  (assert (= 1 (v :gen)) "bind-root bumps generation")
  (var-set v 3)
  (assert (= 2 (v :gen)) "var-set (root) bumps generation")
  (alter-var-root v inc)
  (assert (= 3 (v :gen)) "alter-var-root bumps generation")
  (assert (= 4 (var-get v)) "value still tracks through gen bumps"))
(let [v (make-var 'g 1)
      v2 (with-meta v {:doc "x"})]
  (assert (= (v :gen) (v2 :gen)) "with-meta carries generation"))

# var with namespace
(let [ns (make-ns 'my.ns)
      v (make-var 'my.ns/x 1 {:ns ns})]
  (assert (= ns (var-ns v)) "var-ns returns namespace"))

# ============================================================
# Namespace tests
# ============================================================

(let [ns (make-ns 'foo.bar)]
  (assert (ns? ns) "ns? returns true")
  (assert (deep= 'foo.bar (ns-name ns)) "ns-name returns name symbol")
  (assert (table? (ns-map ns)) "ns-map returns table")
  (assert (= 0 (length (ns-map ns))) "empty namespace has no mappings"))

# ns-intern
(let [ns (make-ns 'test.ns)
      v (ns-intern ns 'x 42)]
  (assert (var? v) "ns-intern returns a var")
  (assert (= 42 (var-get v)) "ns-intern sets root binding")
  # check ns-find returns the same var (by reference, not deep=)
  (assert (= v (ns-find ns 'x)) "ns-find returns interned var"))

# ns-intern without value
(let [ns (make-ns 'test.ns)
      v (ns-intern ns 'y)]
  (assert (var? v) "ns-intern without value creates unbound var"))

# ns-unmap
(let [ns (make-ns 'test.ns)
      _ (ns-intern ns 'x 1)
      _ (ns-unmap ns 'x)]
  (assert (nil? (ns-find ns 'x)) "ns-unmap removes mapping"))

# ns-resolve — own ns
(let [ns (make-ns 'test.ns)
      v (ns-intern ns 'x 10)]
  (assert (= v (ns-resolve ns 'x)) "ns-resolve finds var in own ns"))

# ns-import
(let [ns (make-ns 'test.ns)]
  (ns-import ns 'Date 'java.util.Date)
  (assert (= 'java.util.Date (ns-import-lookup ns 'Date)) "ns-import-lookup returns import"))

# ============================================================
# Context tests
# ============================================================

(let [ctx (make-ctx)]
  (assert (ctx? ctx) "ctx? returns true"))

# ctx with initial namespaces
(let [ctx (make-ctx {:namespaces {"user" {"x" 1 "y" 2}}})]
  (let [ns (ctx-find-ns ctx "user")]
    (assert (ns? ns) "ctx-find-ns returns namespace for user")
    (let [v (ns-find ns "x")]
      (assert (var? v) "user/x is a var")
      (assert (= 1 (var-get v)) "user/x has correct value"))))

# ctx-find-ns creates ns if not present
(let [ctx (make-ctx)
      ns (ctx-find-ns ctx "foo")]
  (assert (ns? ns) "ctx-find-ns creates namespace on demand"))

# ============================================================
# Dynamic binding support (thread-local bindings table)
# ============================================================

# push-thread-bindings / pop-thread-bindings
(let [v (make-var '*dyn* 0 {:dynamic true})]
  (push-thread-bindings @{v 100})
  (assert (= 100 (var-get v)) "push-thread-bindings sets binding")
  (pop-thread-bindings)
  (assert (= 0 (var-get v)) "pop-thread-bindings restores root"))

(print "All types tests passed!")
