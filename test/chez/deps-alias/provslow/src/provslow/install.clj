(ns provslow.install
  "The declared provider of provslow.Clock, slow to load on purpose.")

;; Long enough that every future in the smoke check misses the class while this
;; namespace is still loading on the first thread to reach it.
(Thread/sleep 500)

(__register-class-statics! "provslow.Clock" {"now" (fn [] "slow-clock")})
