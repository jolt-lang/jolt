(ns app.core (:require app.readers))
(defn -main [& _]
  ;; the tag is resolved at RUNTIME through *data-readers* (no compile-time literal),
  ;; so the only thing keeping app.readers/reverse-str live in a shake is the baked
  ;; data-readers map.
  (println (read-string "#prog/rev \"hello\"")))
