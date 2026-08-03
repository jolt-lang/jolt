(ns jolt.time
  "Test stand-in for the jolt-lang/time install namespace: registers one
  java.time zone static, and one constructor, so the roots-autoload gate can
  assert the require fired without the real library.")
(__register-class-statics! "java.time.ZoneId"
                           {"of" (fn [id] (str "fixture-zone:" id))})
;; A CONSTRUCTOR for a library-provided class, to pin that host-new autoloads for
;; an imported simple name and not only for a fully-qualified one. The real
;; library registers java.time.format.DateTimeFormatterBuilder here; malli builds
;; one as (DateTimeFormatterBuilder.) after importing it.
(__register-class-ctor! "java.time.format.DateTimeFormatterBuilder"
                        (fn [& _] "fixture-builder"))
(__register-class-ctor! "DateTimeFormatterBuilder"
                        (fn [& _] "fixture-builder"))
