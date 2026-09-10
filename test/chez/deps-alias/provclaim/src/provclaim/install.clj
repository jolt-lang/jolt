(ns provclaim.install
  "The declared provider of java.security.Signature.")

(__register-class-statics! "java.security.Signature"
                           {"getInstance" (fn [algo] (str "claimer-sig:" algo))})
(__register-class-ctor! "java.security.Signature" (fn [& _] "claimer-sig-ctor"))
(__register-class-ctor! "Signature" (fn [& _] "claimer-sig-ctor"))
