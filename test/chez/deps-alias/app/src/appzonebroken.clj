(ns appzonebroken)

(defn -main [& args]
  ;; First miss: the autoload fires and jolt.time's own load error is what
  ;; surfaces. Swallow it — the roots-autoload latch is one-shot, so it is the
  ;; SECOND miss that has to explain itself.
  (try (java.time.ZoneId/of "UTC") (catch Throwable _ nil))
  (try (java.time.ZoneId/of "UTC")
       (catch Throwable e (println (.getMessage e)))))
