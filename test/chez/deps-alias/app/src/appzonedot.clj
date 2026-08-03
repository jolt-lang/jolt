(ns appzonedot
  "The DOT form of a static call, (. Class method args), on an IMPORTED simple
  name for a class that is only registered by an autoloaded library. The
  fully-qualified form (. java.time.ZoneId of \"UTC\") resolves as a static
  because the dotted name can only be a class; an imported simple name instead
  evaluates to a class token and did instance dispatch on it, throwing \"No
  matching method of for class\" unless some earlier slash-form call happened to
  have loaded the library first. time-literals' data readers are written exactly
  this way — (:import (java.time LocalDate)) then (. LocalDate parse x)."
  (:import (java.time ZoneId)))

(defn -main [& _] (println (. ZoneId of "UTC")))
