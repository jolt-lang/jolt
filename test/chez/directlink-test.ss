;; Direct-linking emission (jolt build, closed world). With direct-link on, a
;; top-level app def emits a Scheme binding jv$<ns>$<name> aliased to its var cell,
;; and an app->app call/value-ref binds to it directly instead of reading the var
;; cell (see var-routed? below for the two spellings that read counts as).
;; ^:dynamic/^:redef defs and nested defs opt out. A direct-linked def is LINKED
;; (def-var-linked!): it hands the runtime a setter over the binding, so a root
;; write — alter-var-root, with-redefs, a later def — reaches the binding the
;; direct sites read, and the cell and the binding never split (jolt#1009).
;; Off direct-link mode the emission is byte-identical to plain `emit`. Run:
;;   chez --script test/chez/directlink-test.ss

(import (chezscheme))
(load "host/chez/gate-boot.ss")
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

;; "the reference stays INDIRECT" — it reads the var cell rather than binding to a
;; jv$ name — in either spelling the emitter uses. Inside a def (a const pool is
;; bound) a late-bound ref hoists its cell: (var-cell-deref _kc$N) over a
;; (jolt-var "ns" "name") binding in the def's let*. Outside one it resolves per
;; access: (var-deref "ns" "name"). Both are the var route; only jv$<ns>$<name> is
;; not, and that is what every assertion here is actually about.
(define (var-routed? s ns name)
  (or (contains? s (string-append "(var-deref \"" ns "\" \"" name "\")"))
      (contains? s (string-append "(jolt-var \"" ns "\" \"" name "\")"))))

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
      (and (contains? eb "(jolt-invoke") (var-routed? eb "app" "a")))
  (ok "off: no jv$ direct call" (not (contains? eb "(jv$app$a)")))
  ;; a def carries source position in its var meta (:line/:column/:file), so it
  ;; emits def-var-with-meta! — but still NO jv$ binding off direct-link.
  (ok "off: def emits def-var-with-meta! (no jv$ binding, not linked)"
      (and (contains? (emit-form "app" "(def a (fn* ([] 1)))") "(def-var-with-meta! \"app\" \"a\"")
           (not (contains? (emit-form "app" "(def a (fn* ([] 1)))") "(define jv$app$a"))
           (not (contains? (emit-form "app" "(def a (fn* ([] 1)))") "(def-var-linked!")))))

;; --- direct-link ON ---
(set-direct-link! #t)
(direct-link-reset!)

(let ((ea (emit-form "app" "(def a (fn* ([] 1)))")))   ; registers app/a in the set
  (ok "on: a's def emits a jv$ binding aliased to its var cell"
      (and (contains? ea "(begin (define jv$app$a ")
           (contains? ea "(def-var-linked! \"app\" \"a\" 'jv$app$a jv$app$a")))
  ;; jolt#1009: the def is LINKED — it registers a setter over the binding, so a
  ;; later alter-var-root / with-redefs / def writes the new root through to the
  ;; jv$ name every direct call site applies and every value-ref reads. Without
  ;; it the binding froze at load and the var cell alone moved, so `(var-get #'a)`
  ;; and a compiled `a` disagreed for the rest of the process.
  (ok "on: a's def registers a write-through setter over the binding"
      (contains? ea "(lambda (v) (set! jv$app$a v))")))

(let ((eb (emit-form "app" "(def b (fn* ([] (a))))")))
  (ok "on: b's call to a is a direct (jv$app$a) call" (contains? eb "(jv$app$a)"))
  (ok "on: b's call to a is NOT var-routed" (not (var-routed? eb "app" "a")))
  (ok "on: b's call to a is NOT jolt-invoke'd" (not (contains? eb "(jolt-invoke"))))

(let ((eh (emit-form "app" "(def hof (fn* ([] a)))")))
  (ok "on: a used as a value references the binding directly" (contains? eh " jv$app$a)"))
  (ok "on: value-ref to a is NOT var-routed" (not (var-routed? eh "app" "a"))))

;; A map-valued (non-fn) def is invokable in Clojure but is NOT a Scheme procedure;
;; a direct-link call to it must route through jolt-invoke, never raw-apply the
;; binding (which crashed with "attempt to apply non-procedure" before the fix).
(let ((ec (emit-form "app" "(def cfg {:a 1 :b 2})")))   ; registers app/cfg (non-fn) in the set
  (ok "on: a non-fn def still gets a jv$ binding" (contains? ec "(define jv$app$cfg "))
  ;; jolt#1009 was reported against exactly this shape — a plain value def, whose
  ;; only reachable uses are value-position reads of the binding.
  (ok "on: a non-fn def is linked too"
      (and (contains? ec "(def-var-linked! \"app\" \"cfg\" 'jv$app$cfg jv$app$cfg")
           (contains? ec "(lambda (v) (set! jv$app$cfg v))"))))
(let ((eu (emit-form "app" "(def usecfg (fn* ([] (cfg :a))))")))
  (ok "on: call to a map-valued def routes through jolt-invoke" (contains? eu "(jolt-invoke"))
  (ok "on: call to a map-valued def still uses the direct binding" (contains? eu "jv$app$cfg"))
  (ok "on: a map-valued def is NOT raw-applied as a procedure" (not (contains? eu "(jv$app$cfg"))))

;; ^:dynamic opts out: no jv$ binding, callers stay indirect.
(let ((ed (emit-form "app" "(def ^:dynamic d 5)")))
  (ok "on: ^:dynamic def gets no jv$ binding" (not (contains? ed "(define jv$app$d")))
  (ok "on: ^:dynamic def is not linked" (not (contains? ed "(def-var-linked!"))))
(let ((eu (emit-form "app" "(def usesd (fn* ([] (d))))")))
  (ok "on: call to a ^:dynamic var stays indirect" (var-routed? eu "app" "d"))
  (ok "on: ^:dynamic var not direct-linked" (not (contains? eu "(jv$app$d)"))))

;; ^:redef opts out too (a def redefinable after build stays var-routed).
(let ((er (emit-form "app" "(def ^:redef r 5)")))
  (ok "on: ^:redef def gets no jv$ binding" (not (contains? er "(define jv$app$r")))
  (ok "on: ^:redef def is not linked" (not (contains? er "(def-var-linked!"))))
(let ((eu (emit-form "app" "(def usesr (fn* ([] (r))))")))
  (ok "on: call to a ^:redef var stays indirect" (var-routed? eu "app" "r"))
  (ok "on: ^:redef var not direct-linked" (not (contains? eu "(jv$app$r)"))))

;; A var only defined LATER in emission order is not yet in the set -> indirect.
(direct-link-reset!)
(let ((efwd (emit-form "app" "(def caller (fn* ([] (a))))")))  ; a not (re)emitted since reset
  (ok "on: forward/undefined ref stays indirect" (var-routed? efwd "app" "a")))

(set-direct-link! #f)
(printf "~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
