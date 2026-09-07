(ns app.core
  (:require [allowlib.core :as lib]))

;; Tree-shake fixture for deps.edn :jolt/tree-shake {:allow-dynamic […]}.
;;
;; Two runtime var lookups sit on paths -main reaches and never takes: this
;; ns's `res` (spec.alpha/res's shape — qualify a symbol for a description) and
;; the library's `dynaload` (behind a delay, spec.gen's shape). Both are real
;; `resolve` calls in reachable code, so without the key the shake bails. The
;; app's deps.edn vouches for res, the library's own deps.edn for dynaload, and
;; the union lets this app SHAKE: `dead` must be pruned and the compiler image
;; dropped. Bails against a jolt that does not read the key.
;;
;; ^:redef on res: the inline pass would otherwise splice it into describe and
;; the bail would name app.core/describe (or -main), not res — and the allow
;; entry would be inert. The bail names the def a ref ends up in.
(defn ^:redef res [s]
  (if (resolve s) (symbol "app.core" (name s)) s))

(defn describe [form]
  (str (res form)))

(defn dead [] :never)

(defn -main [& args]
  (println (lib/describe (if (seq args) (describe 'inc) "plain"))))
