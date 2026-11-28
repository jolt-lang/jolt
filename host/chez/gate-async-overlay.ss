;; gate-async-overlay.ss — load the core.async overlay into a gate boot.
;;
;; A gate boot stops at compile-eval.ss, so there is no loader and the
;; overlay's ns :require does not pull anything in. Production loads
;; clojure.core.async.impl.protocols through that :require before the overlay
;; body compiles, and the overlay refers to it by alias, so a gate loads the two
;; files in the same order here. Load after gate-boot.ss.
(define (gate-load-async-overlay!)
  (for-each
    (lambda (path)
      (jolt-load-string (call-with-input-file path get-string-all)))
    '("stdlib/clojure/core/async/impl/protocols.clj"
      "stdlib/clojure/core/async.clj")))
