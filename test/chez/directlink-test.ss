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
;; Every datum in the emitted text reads without error — an unescaped delimiter in
;; a binding name (a bare | or a space) makes the read raise or stop short.
(define (reads-clean? s)
  (guard (e (#t #f))
    (let ((p (open-input-string s)))
      (let loop () (if (eof-object? (read p)) #t (loop))))))

(define (var-routed? s ns name)
  (or (contains? s (string-append "(var-deref \"" ns "\" \"" name "\")"))
      (contains? s (string-append "(jolt-var \"" ns "\" \"" name "\")"))))

;; A direct call through linked binding B: the binding read into a local and
;; applied when it is a procedure (backend calls.clj linked-call), never a call of
;; the top-level variable itself, which races a redefinition on Chez.
(define (linked-call? s b)
  (let ((pre "(let ((_lp$") (suf (string-append " " b ")")))
    (let loop ((i 0))
      (let ((j (let find ((k i))
                 (cond ((> (+ k (string-length pre)) (string-length s)) #f)
                       ((string=? (substring s k (+ k (string-length pre))) pre) k)
                       (else (find (+ k 1)))))))
        (and j
             (let digits ((k (+ j (string-length pre))))
               (if (and (< k (string-length s)) (char-numeric? (string-ref s k)))
                   (digits (+ k 1))
                   (or (and (<= (+ k (string-length suf)) (string-length s))
                            (string=? (substring s k (+ k (string-length suf))) suf))
                       (loop (+ j 1))))))))))
(define (symbol-call? s b) (contains? s (string-append "(" b ")")))

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
;; a direct call needs the callee's loaded root to take that arity
(jolt-compile-eval "(def ||| (fn* ([] 7)))" "app")
(jolt-compile-eval "(def ^:redef r 5)" "app")
(jolt-compile-eval "(def usesr (fn* ([] (r))))" "app")
(jolt-compile-eval "(def cfg {:a 1 :b 2})" "app")
(jolt-compile-eval "(def usecfg (fn* ([] (cfg :a))))" "app")

;; --- direct-link OFF: every reference stays indirect (var-deref) ---
(let ((eb (emit-form "app" "(def b (fn* ([] (a))))")))
  (ok "off: call to a routes through jolt-invoke + var-deref"
      (and (contains? eb "(jolt-invoke") (var-routed? eb "app" "a")))
  (ok "off: no jv$ direct call" (not (or (linked-call? eb "jv$app/a") (symbol-call? eb "jv$app/a"))))
  ;; a def carries source position in its var meta (:line/:column/:file), so it
  ;; emits def-var-with-meta! — but still NO jv$ binding off direct-link.
  (ok "off: def emits def-var-with-meta! (no jv$ binding, not linked)"
      (and (contains? (emit-form "app" "(def a (fn* ([] 1)))") "(def-var-with-meta! \"app\" \"a\"")
           (not (contains? (emit-form "app" "(def a (fn* ([] 1)))") "(define jv$app/a"))
           (not (contains? (emit-form "app" "(def a (fn* ([] 1)))") "(def-var-linked!")))))

;; --- direct-link ON ---
(set-direct-link! #t)
(direct-link-reset!)

(let ((ea (emit-form "app" "(def a (fn* ([] 1)))")))   ; registers app/a in the set
  (ok "on: a's def emits a jv$ binding aliased to its var cell"
      (and (contains? ea "(begin (define jv$app/a ")
           (contains? ea "(def-var-linked! \"app\" \"a\" 'jv$app/a jv$app/a")))
  ;; jolt#1009: the def is LINKED — it registers a setter over the binding, so a
  ;; later alter-var-root / with-redefs / def writes the new root through to the
  ;; jv$ name every direct call site applies and every value-ref reads. Without
  ;; it the binding froze at load and the var cell alone moved, so `(var-get #'a)`
  ;; and a compiled `a` disagreed for the rest of the process.
  (ok "on: a's def registers a write-through setter over the binding"
      (contains? ea "(lambda (v) (set! jv$app/a v))")))

(let ((ep (emit-form "app" "(def ||| (fn* ([] 7)))")))
  (ok "on: a pipe-named def emits a valid direct binding"
      (contains? ep "(define jv$app/$V$V$V "))
  (ok "on: a pipe-named def's emission reads back as Scheme" (reads-clean? ep)))
(let ((eu (emit-form "app" "(def uses-pipe (fn* ([] (|||))))")))
  (ok "on: a pipe-named call uses its escaped direct binding"
      (linked-call? eu "jv$app/$V$V$V")))
;; The binding name is jv$ + the munged FQN (ns/name), the same identity the
;; direct-link registry keys on, so two distinct vars can never share one: the
;; old per-part _V_/_D_/_H_/_Q_ substitutions were plain text a user can write
;; ((def _V_ …) and (def | …) both bound jv$app/_V_), and a $ separator between
;; separately-munged parts could be forged by an escape in the ns name.
(let ((ev (emit-form "app" "(def _V_ (fn* ([] 1)))"))
      (ep (emit-form "app" "(def ||| (fn* ([] 7)))")))
  (ok "on: a def literally named _V_ binds jv$app/_V_, not the pipe's name"
      (and (contains? ev "(define jv$app/_V_ ") (not (contains? ep "jv$app/_V_")))))
(let ((ed (emit-form "app" "(def $ (fn* ([] 1)))")))
  (ok "on: a $-named def escapes the $ ($$) so it cannot read as a separator"
      (and (contains? ed "(define jv$app/$$ ") (reads-clean? ed))))
(let ((eq (emit-form "app" "(def a'# (fn* ([] 1)))")))
  (ok "on: ' and # in a def name take the same $P/$H escapes locals do"
      (and (contains? eq "(define jv$app/a$P$H ") (reads-clean? eq))))

(let ((eb (emit-form "app" "(def b (fn* ([] (a))))")))
  (ok "on: b's call to a is a direct call of jv$app/a" (linked-call? eb "jv$app/a"))
  (ok "on: b's call to a never applies the top-level variable" (not (symbol-call? eb "jv$app/a")))
  (ok "on: b's call to a is NOT var-routed" (not (var-routed? eb "app" "a")))
  ;; jolt-invoke only in the arm a root that is no longer a procedure takes
  (ok "on: b's call to a applies the local when it is a procedure"
      (contains? eb "(if (#3%procedure? _lp$"))
  (ok "on: b's call to a jolt-invokes only from the fallback arm"
      (contains? eb ") (jolt-invoke _lp$")))

(let ((eh (emit-form "app" "(def hof (fn* ([] a)))")))
  (ok "on: a used as a value references the binding directly" (contains? eh " jv$app/a)"))
  (ok "on: value-ref to a is NOT var-routed" (not (var-routed? eh "app" "a"))))

;; A map-valued (non-fn) def is invokable in Clojure but is NOT a Scheme procedure;
;; a direct-link call to it must route through jolt-invoke, never raw-apply the
;; binding (which crashed with "attempt to apply non-procedure" before the fix).
(let ((ec (emit-form "app" "(def cfg {:a 1 :b 2})")))   ; registers app/cfg (non-fn) in the set
  (ok "on: a non-fn def still gets a jv$ binding" (contains? ec "(define jv$app/cfg "))
  ;; jolt#1009 was reported against exactly this shape — a plain value def, whose
  ;; only reachable uses are value-position reads of the binding.
  (ok "on: a non-fn def is linked too"
      (and (contains? ec "(def-var-linked! \"app\" \"cfg\" 'jv$app/cfg jv$app/cfg")
           (contains? ec "(lambda (v) (set! jv$app/cfg v))"))))
(let ((eu (emit-form "app" "(def usecfg (fn* ([] (cfg :a))))")))
  (ok "on: call to a map-valued def routes through jolt-invoke" (contains? eu "(jolt-invoke"))
  (ok "on: call to a map-valued def still uses the direct binding" (contains? eu "jv$app/cfg"))
  (ok "on: a map-valued def is NOT raw-applied as a procedure"
      (not (or (contains? eu "(jv$app/cfg") (linked-call? eu "jv$app/cfg")))))

;; ^:dynamic opts out: no jv$ binding, callers stay indirect.
(let ((ed (emit-form "app" "(def ^:dynamic d 5)")))
  (ok "on: ^:dynamic def gets no jv$ binding" (not (contains? ed "(define jv$app/d")))
  (ok "on: ^:dynamic def is not linked" (not (contains? ed "(def-var-linked!"))))
(let ((eu (emit-form "app" "(def usesd (fn* ([] (d))))")))
  (ok "on: call to a ^:dynamic var stays indirect" (var-routed? eu "app" "d"))
  (ok "on: ^:dynamic var not direct-linked"
      (not (or (symbol-call? eu "jv$app/d") (linked-call? eu "jv$app/d")))))

;; ^:redef opts out too (a def redefinable after build stays var-routed).
(let ((er (emit-form "app" "(def ^:redef r 5)")))
  (ok "on: ^:redef def gets no jv$ binding" (not (contains? er "(define jv$app/r")))
  (ok "on: ^:redef def is not linked" (not (contains? er "(def-var-linked!"))))
(let ((eu (emit-form "app" "(def usesr (fn* ([] (r))))")))
  (ok "on: call to a ^:redef var stays indirect" (var-routed? eu "app" "r"))
  (ok "on: ^:redef var not direct-linked"
      (not (or (symbol-call? eu "jv$app/r") (linked-call? eu "jv$app/r")))))

;; A var only defined LATER in emission order is not yet in the set -> indirect.
(direct-link-reset!)
(let ((efwd (emit-form "app" "(def caller (fn* ([] (a))))")))  ; a not (re)emitted since reset
  (ok "on: forward/undefined ref stays indirect" (var-routed? efwd "app" "a")))

(set-direct-link! #f)
(printf "~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
