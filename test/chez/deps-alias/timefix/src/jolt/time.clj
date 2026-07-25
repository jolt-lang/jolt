(ns jolt.time
  "Test stand-in for the jolt-lang/time install namespace: registers one
  java.time zone static so the roots-autoload gate can assert the require
  fired without the real library.")
(__register-class-statics! "java.time.ZoneId"
                           {"of" (fn [id] (str "fixture-zone:" id))})
