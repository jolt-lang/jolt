(ns appzonector
  "Constructing a library-provided class through an IMPORTED SIMPLE NAME has to
  autoload the provider, the same as a static call does. host-new does try the
  autoload, but the java.time library's name list only carried the classes it
  knew about — DateTimeFormatterBuilder was absent, so the fully-qualified form
  worked and (DateTimeFormatterBuilder.) did not. malli's transform.cljc builds
  one exactly that way, and could not load."
  (:import (java.time.format DateTimeFormatterBuilder)))

(defn -main [& _] (println (DateTimeFormatterBuilder.)))
