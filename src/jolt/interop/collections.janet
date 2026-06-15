# Host interop — collection interop: wires the evaluator's late-bound hooks to
# core's collection ops so Java-style interop (.iterator/.nth/.count/.seq/...)
# works over jolt values. Split from host_interop.janet (jolt-jx5l, phase 1).
# Late-bound because the evaluator loads before core.
(use ../evaluator)
(use ../core)
(use ../types)
(use ../lazyseq)
(use ../pv)
(use ../plist)
(import ../phm)

# Collection interop: wires the evaluator's late-bound hooks to core's
# collection ops so Java-style interop works over jolt values (moved here from
# api — jolt-jx5l). Late-bound because the evaluator loads before core.
(defn install-collections! []
  # (.iterator coll) -> a jolt iterator over any seqable (hiccup's iterate!).
  (set-coll-realizer! realize-for-iteration)
  # clj-targeted libs build collections via JVM statics in their :clj branches;
  # malli's entry parser uses these. createOwning takes ownership of an object
  # array -> a vector; createWithCheck builds a map and throws on duplicate keys
  # (malli catches that), detected by the built map being smaller than the kvs.
  (register-class-statics! "LazilyPersistentVector"
    @{"createOwning" (fn [arr] (make-vec arr))})
  (register-class-statics! "PersistentArrayMap"
    @{"createWithCheck"
      (fn [arr]
        (def m (phm/make-phm arr))
        (if (= (* 2 (phm/phm-count m)) (length arr)) m
          (error "PersistentArrayMap: duplicate key")))})
  # .nth/.count/.valAt/.get/.seq/.containsKey on a jolt collection -> the
  # clojure.core equivalent. :jolt/ci-none means "not a collection method here".
  (set-coll-interop!
    (fn [target name args]
      (if-not (or (pvec? target) (phm/phm? target) (plist? target) (lazy-seq? target)
                  (and (table? target) (= :jolt/set (get target :jolt/type)))
                  (shape-rec? target)                                    # map-as-tuple record
                  (and (struct? target) (nil? (get target :jolt/type)))) # plain map literal
        :jolt/ci-none
      (cond
        (= name "nth")     (if (>= (length args) 2) (core-nth target (in args 0) (in args 1))
                                                    (core-nth target (in args 0)))
        (= name "count")   (core-count target)
        (or (= name "valAt") (= name "get"))
                           (if (>= (length args) 2) (core-get target (in args 0) (in args 1))
                                                    (core-get target (in args 0)))
        (= name "seq")     (core-seq target)
        (= name "containsKey") (core-contains? target (in args 0))
        :jolt/ci-none)))))
