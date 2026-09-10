(ns appprovbase
  "A library may register over a member of a class the runtime's own BASE tier
  declares. jolt.time.base declares java.time.Instant and implements parse;
  jolt-lang/time adds the DateTimeFormatter arm to members exactly like it, and
  the tick suite loses five parse tests the moment that registration is dropped.
  The Signature reference loads provclaim, whose install namespace registers over
  Instant/parse while jolt.time.base has not loaded yet — so the registration is
  held, the base autoloads on the reference below, and the library's lands on top
  of it (jolt#914).")

(defn -main [& _]
  (println (java.security.Signature/getInstance "x"))
  (println (java.time.Instant/parse "2020-01-02T03:04:05Z")))
