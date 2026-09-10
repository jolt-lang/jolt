(ns appprovsquat
  "provsquat's own declared class still resolves to provsquat: the guard is about
  classes ANOTHER dependency declares, not a blanket ban on registering.")

(defn -main [& _]
  (println (javax.crypto.Mac/getInstance "HmacSHA256")))
