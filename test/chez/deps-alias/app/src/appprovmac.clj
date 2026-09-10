(ns appprovmac
  "Reference order B: a class provsquat DECLARES first, which loads provsquat's
  install namespace — and that also registers java.security.Signature, which it
  does not declare. The Signature reference below then found a registry HIT and
  never autoloaded provclaim, so the same deps.edn resolved Signature two
  different ways depending on which namespace compiled first (jolt#914). The
  declared provider must win in both orders."
  (:import (java.security Signature)))

(defn -main [& _]
  (println (javax.crypto.Mac/getInstance "HmacSHA256"))
  (println (java.security.Signature/getInstance "SHA256withECDSA"))
  ;; the imported simple name and the constructor resolve through the same claim
  (println (Signature/getInstance "SHA256withECDSA"))
  (println (Signature.))
  ;; ...and the member provclaim's shim does NOT answer, held while provsquat had
  ;; no claim to compare against, lands once the claim settles — the additive half
  ;; survives the hold in this order too.
  (println (Signature/getMaxSigLength "x")))
