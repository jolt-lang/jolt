;; Direct-linking emission (jolt build, closed world). With direct-link on, a
;; top-level app def emits a Scheme binding jv$<ns>$<name> aliased to its var cell,
;; and an app->app call/value-ref binds to it directly instead of going through
;; (jolt-invoke (var-deref ...)). ^:dynamic/^:redef defs and nested defs opt out.
;; Off direct-link mode the emission is byte-identical to plain `emit`. Run:
;;   chez --script test/chez/directlink-test.ss

(import (chezscheme))
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")
(load "host/chez/emit-image.ss")

(define total 0) (define fails 0)
(define (ok name pred) (set! total (+ total 1)) (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))

(define set-direct-link! (var-deref "jolt.backend-scheme" "set-direct-link!"))
(define direct-link-reset! (var-deref "jolt.backend-scheme" "direct-link-reset!"))

(define (contains? s sub)
  (let ((ns (string-length s)) (nsub (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i nsub) ns) #f)
            ((string=? (substring s i (+ i nsub)) sub) #t)
            (else (loop (+ i 1)))))))

;; Analyze+emit one form (string) in a namespace through the real build entry
;; (ei-compile-form -> emit-top-form), no optimization passes.
(define (emit-form ns-name str)
  (let-values (((f j) (rdr-read-form str 0 (string-length str))))
    (ei-compile-form (make-analyze-ctx ns-name) f #f)))

;; Register var cells so resolve-global classifies references as :var (the build
;; loads the namespaces before re-emitting; here we eval the defs with direct-link
;; off first). Use fn* so no macro expansion is involved.
(set-direct-link! #f)
(jolt-compile-eval "(def a (fn* ([] 1)))" "app")
(jolt-compile-eval "(def b (fn* ([] (a))))" "app")
(jolt-compile-eval "(def hof (fn* ([] a)))" "app")
(jolt-compile-eval "(def ^:dynamic d 5)" "app")
(jolt-compile-eval "(def usesd (fn* ([] (d))))" "app")
(jolt-compile-eval "(def ^:redef r 5)" "app")
(jolt-compile-eval "(def usesr (fn* ([] (r))))" "app")
(jolt-compile-eval "(def cfg {:a 1 :b 2})" "app")
(jolt-compile-eval "(def usecfg (fn* ([] (cfg :a))))" "app")

;; --- direct-link OFF: every reference stays indirect (var-deref) ---
(let ((eb (emit-form "app" "(def b (fn* ([] (a))))")))
  (ok "off: call to a routes through jolt-invoke + var-deref"
      (and (contains? eb "(jolt-invoke") (contains? eb "(var-deref \"app\" \"a\")")))
  (ok "off: no jv$ direct call" (not (contains? eb "(jv$app$a)")))
  ;; a def carries source position in its var meta (:line/:column/:file), so it
  ;; emits def-var-with-meta! — but still NO jv$ binding off direct-link.
  (ok "off: def emits def-var-with-meta! (no jv$ binding)"
      (and (contains? (emit-form "app" "(def a (fn* ([] 1)))") "(def-var-with-meta! \"app\" \"a\"")
           (not (contains? (emit-form "app" "(def a (fn* ([] 1)))") "(define jv$app$a")))))

;; --- direct-link ON ---
(set-direct-link! #t)
(direct-link-reset!)

(let ((ea (emit-form "app" "(def a (fn* ([] 1)))")))   ; registers app/a in the set
  (ok "on: a's def emits a jv$ binding aliased to its var cell"
      (and (contains? ea "(begin (define jv$app$a ")
           (contains? ea "(def-var-with-meta! \"app\" \"a\" jv$app$a"))))

(let ((eb (emit-form "app" "(def b (fn* ([] (a))))")))
  (ok "on: b's call to a is a direct (jv$app$a) call" (contains? eb "(jv$app$a)"))
  (ok "on: b's call to a is NOT var-deref'd" (not (contains? eb "(var-deref \"app\" \"a\")")))
  (ok "on: b's call to a is NOT jolt-invoke'd" (not (contains? eb "(jolt-invoke"))))

(let ((eh (emit-form "app" "(def hof (fn* ([] a)))")))
  (ok "on: a used as a value references the binding directly" (contains? eh " jv$app$a)"))
  (ok "on: value-ref to a is NOT var-deref'd" (not (contains? eh "(var-deref \"app\" \"a\")"))))

;; A map-valued (non-fn) def is invokable in Clojure but is NOT a Scheme procedure;
;; a direct-link call to it must route through jolt-invoke, never raw-apply the
;; binding (which crashed with "attempt to apply non-procedure" before the fix).
(let ((ec (emit-form "app" "(def cfg {:a 1 :b 2})")))   ; registers app/cfg (non-fn) in the set
  (ok "on: a non-fn def still gets a jv$ binding" (contains? ec "(define jv$app$cfg ")))
(let ((eu (emit-form "app" "(def usecfg (fn* ([] (cfg :a))))")))
  (ok "on: call to a map-valued def routes through jolt-invoke" (contains? eu "(jolt-invoke"))
  (ok "on: call to a map-valued def still uses the direct binding" (contains? eu "jv$app$cfg"))
  (ok "on: a map-valued def is NOT raw-applied as a procedure" (not (contains? eu "(jv$app$cfg"))))

;; ^:dynamic opts out: no jv$ binding, callers stay indirect.
(let ((ed (emit-form "app" "(def ^:dynamic d 5)")))
  (ok "on: ^:dynamic def gets no jv$ binding" (not (contains? ed "(define jv$app$d"))))
(let ((eu (emit-form "app" "(def usesd (fn* ([] (d))))")))
  (ok "on: call to a ^:dynamic var stays indirect" (contains? eu "(var-deref \"app\" \"d\")"))
  (ok "on: ^:dynamic var not direct-linked" (not (contains? eu "(jv$app$d)"))))

;; ^:redef opts out too (a def redefinable after build stays var-routed).
(let ((er (emit-form "app" "(def ^:redef r 5)")))
  (ok "on: ^:redef def gets no jv$ binding" (not (contains? er "(define jv$app$r"))))
(let ((eu (emit-form "app" "(def usesr (fn* ([] (r))))")))
  (ok "on: call to a ^:redef var stays indirect" (contains? eu "(var-deref \"app\" \"r\")"))
  (ok "on: ^:redef var not direct-linked" (not (contains? eu "(jv$app$r)"))))

;; A var only defined LATER in emission order is not yet in the set -> indirect.
(direct-link-reset!)
(let ((efwd (emit-form "app" "(def caller (fn* ([] (a))))")))  ; a not (re)emitted since reset
  (ok "on: forward/undefined ref stays indirect" (contains? efwd "(var-deref \"app\" \"a\")")))

(set-direct-link! #f)
(printf "~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
