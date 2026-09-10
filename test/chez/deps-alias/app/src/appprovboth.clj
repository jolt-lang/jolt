(ns appprovboth
  "The other order: the claimed class resolves FIRST, so provclaim is already
  loaded when the Mac reference pulls provsquat in. provsquat's undeclared
  java.security.Signature registration must not take the class over — a
  registration is not an argument about who implements it (jolt#914).")

(defn -main [& _]
  (println (java.security.Signature/getInstance "a"))
  (println (javax.crypto.Mac/getInstance "b"))
  (println (java.security.Signature/getInstance "c"))
  ;; ...while a member the provider's shim does not answer is additive and lands
  (println (java.security.Signature/getMaxSigLength "x")))
