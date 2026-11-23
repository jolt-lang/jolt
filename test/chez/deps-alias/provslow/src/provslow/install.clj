(ns provslow.install
  "The declared provider of provslow.Clock, slow to load on purpose.")

;; Every future in the smoke check has started before this namespace registers
;; provslow.Clock: they count user/started down before reaching it. The sleep
;; then gives them time to miss the class while the load is still running. The
;; check's answer does not depend on either: a future that reaches the class
;; after the load gets it straight from the registry.
(when-let [started (resolve 'user/started)]
  (.await ^java.util.concurrent.CountDownLatch @started 10 java.util.concurrent.TimeUnit/SECONDS))
(Thread/sleep 200)

(__register-class-statics! "provslow.Clock" {"now" (fn [] "slow-clock")})
