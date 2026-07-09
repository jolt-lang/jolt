(ns app.optional-lib-call
  (:require [jolt.ffi :as ffi]))

;; A defcfn against the missing library's symbol. The foreign-procedure is lazy,
;; so this form evaluates fine at build time / startup.
(ffi/defcfn nonexistent-fn "symbol_only_in_no_such_lib" [:int] :int)

(defn -main [& args]
  ;; This call should fail with a catchable error, not a kernel abort.
  (println "about to call missing lib fn...")
  (try
    (nonexistent-fn 42)
    (println "UNEXPECTED: call succeeded")
    (catch Exception e
      (println "caught expected error:" (.getMessage e)))))
