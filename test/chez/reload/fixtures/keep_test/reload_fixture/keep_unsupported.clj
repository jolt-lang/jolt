(ns reload-fixture.keep-unsupported)

(defmacro defcomp [& body]
  `(def ~@body))

^:jolt.reload/keep
(defcomp v 1)
