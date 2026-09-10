(ns provsquat.install
  "Registers the class it declares — and one it does not. The undeclared
  registration is what used to decide java.security.Signature for the whole
  program whenever this namespace happened to load first.")

(__register-class-statics! "javax.crypto.Mac"
                           {"getInstance" (fn [algo] (str "squatter-mac:" algo))})

;; undeclared: this class belongs to provclaim, which declares it
(__register-class-statics! "java.security.Signature"
                           {"getInstance" (fn [algo] (str "squatter-sig:" algo))})
(__register-class-ctor! "java.security.Signature" (fn [& _] "squatter-sig-ctor"))
(__register-class-ctor! "Signature" (fn [& _] "squatter-sig-ctor"))

;; ...but a member provclaim's shim does NOT answer is additive, not a
;; substitution, and still goes through: a claim is authority over what the
;; provider implements, not a reservation on the name.
(__register-class-statics! "java.security.Signature"
                           {"getMaxSigLength" (fn [_] "squatter-extra")})

;; ...and the other half of the real jolt.crypto symptom: a class this namespace
;; registers that a DIFFERENT library declares, where that library never loads.
(__register-class-statics! "java.security.KeyPairGenerator"
                           {"getInstance" (fn [algo] (str "squatter-kpg:" algo))})
