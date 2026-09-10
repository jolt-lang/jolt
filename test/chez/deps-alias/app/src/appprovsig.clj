(ns appprovsig
  "Reference order A: the CLAIMED class first. provclaim declares
  java.security.Signature, so this reference autoloads provclaim and resolves to
  its registration — the case that already worked before jolt#914.")

(defn -main [& _]
  (println (java.security.Signature/getInstance "SHA256withECDSA")))
