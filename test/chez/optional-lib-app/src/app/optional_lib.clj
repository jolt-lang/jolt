(ns app.optional-lib
  (:require [jolt.ffi :as ffi]))

;; A defcfn against the missing library's symbol. With the lazy-resolution fix,
;; this form evaluates to a closure — the foreign-procedure is only resolved
;; when the fn is actually called. Since -main never calls it, the build must
;; succeed and the binary must run without touching the missing lib.
(ffi/defcfn nonexistent-fn "symbol_only_in_no_such_lib" [:int] :int)

(defn -main [& args]
  (println "optional lib app ran successfully"))
