(ns app.core
  (:require [allowlib.core :as lib]))

;; The allow-dynamic-app with one more reachable `resolve` caller, `lookup`,
;; that nothing vouches for. An allowed site next to a non-allowed one must
;; not let the non-allowed one through: this app BAILS, and the hint names
;; app.core/lookup alone — not res (allowed by the app's deps.edn) and not
;; allowlib.core/dynaload (allowed by the library's).
;;
;; ^:redef on both, so the bail names these defs and not the caller the
;; inline pass would have spliced them into (see allow-dynamic-app).
(defn ^:redef res [s]
  (if (resolve s) (symbol "app.core" (name s)) s))

(defn describe [form]
  (str (res form)))

(defn ^:redef lookup [s]
  (resolve s))

(defn -main [& args]
  (println (lib/describe (if (seq args) (describe 'inc) "plain")))
  (println (boolean (lookup 'app.core/-main))))
