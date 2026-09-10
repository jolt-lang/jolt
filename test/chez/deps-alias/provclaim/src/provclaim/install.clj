(ns provclaim.install
  "The declared provider of java.security.Signature.")

(__register-class-statics! "java.security.Signature"
                           {"getInstance" (fn [algo] (str "claimer-sig:" algo))})
(__register-class-ctor! "java.security.Signature" (fn [& _] "claimer-sig-ctor"))
(__register-class-ctor! "Signature" (fn [& _] "claimer-sig-ctor"))

;; ...and the base tier is EXTENDED, not owned. jolt.time.base (a provider jolt
;; ships) declares java.time.Instant and implements parse; jolt-lang/time adds a
;; DateTimeFormatter arm to exactly these members, so a registration over one has
;; to land. Stands in for that: what jolt ships claims the NAME, not the
;; implementation of every member under it.
(__register-class-statics! "java.time.Instant"
                           {"parse" (fn [_ & _] "claimer-instant")})
