(ns appprovgone
  "A held registration outlives the provider that could not deliver. The Mac
  reference loads provsquat, whose install namespace also registers
  java.security.KeyPairGenerator without declaring it; provgone declares that class
  and has no install namespace on the roots. The hold is a wait for the claimer, not
  a veto, so once the claim settles unanswered the registration lands (jolt#914).")

(defn -main [& _]
  (println (javax.crypto.Mac/getInstance "HmacSHA256"))
  (println (java.security.KeyPairGenerator/getInstance "RSA")))
