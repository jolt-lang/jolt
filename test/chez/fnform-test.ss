;; fnform-test.ss — R1 (bead jolt-hqpn): unique anon-fn letrec names +
;; source-form registration. A user-ns anon literal must be registered under a
;; deterministic jfn$<ns>/<def>$<n> name, that name must be what Chez's
;; inspector reports for the live closure ((io 'code) 'name), and the registry
;; must carry {form, ns, free-names}. Covers a literal inside a map, a nested
;; literal, a variadic literal, a literal capturing a local ONLY through a
;; nested literal, and the shadow case (fn [x] (+ y (let [y 1] y))). A
;; clojure.core-produced closure (partial) must NOT be jfn$-named (system gate).
;;   chez --script test/chez/fnform-test.ss
(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred) (set! total (+ total 1)) (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))

(define (string-prefix? s pre)
  (let ((n (string-length s)) (m (string-length pre)))
    (and (>= n m) (string=? (substring s 0 m) pre))))

;; Compile+eval a Clojure source string in a namespace — the runtime eval path a
;; REPL-defined form goes through — returning the value.
(define (jolt-eval src ns)
  (jolt-compile-eval-form (jolt-ce-read src) ns))

;; The procedure name Chez's inspector reports, or #f for an unnamed procedure.
(define (closure-name c)
  (guard (e (#t #f))
    (let ((io (inspect/object c)))
      (let ((code (guard (e (#t #f)) (io 'code))))
        (and code (guard (e (#t #f)) (code 'name)))))))

;; A jolt vector's elements as a Scheme list.
(define (jvec->list v)
  (let loop ((s (jolt-seq v)) (acc '()))
    (if (jolt-nil? s) (reverse acc)
        (loop (seq-more s) (cons (seq-first s) acc)))))

(define (reg-ns name)
  (let ((e (image-fn-form-lookup name))) (and e (vector-ref e 1))))
(define (reg-frees name)
  (let ((e (image-fn-form-lookup name))) (and e (jvec->list (vector-ref e 2)))))
(define (reg-form-head name)
  (let ((e (image-fn-form-lookup name)))
    (and e (let ((s (jolt-seq (vector-ref e 0))))
             (and (not (jolt-nil? s)) (symbol-t-name (seq-first s)))))))

;; --- fixture: load a small app ns through the runtime eval path ---
(jolt-eval "(def config {:handler (fn [x] (* x 2)) :nested (fn [y] (fn [z] (+ y z)))})" "app")
(jolt-eval "(def v2 {:f (fn [a & more] (apply + a more))})" "app")
(jolt-eval "(def captured (let [base 10] (fn [x] (let [inner (fn [n] (+ n base))] (inner x)))))" "app")
(jolt-eval "(def shadowed (let [y 100] (fn [x] (+ y (let [y 1] y)))))" "app")
(jolt-eval "(def p (partial + 1))" "app")

;; --- registry: one entry per literal, name -> {form, ns, free-names} ---
(ok "config$0 registered with ns" (string=? (reg-ns "jfn$app/config$0") "app"))
(ok "config$0 form is an fn* form" (string=? (reg-form-head "jfn$app/config$0") "fn*"))
(ok "config$0 free-names empty" (equal? (reg-frees "jfn$app/config$0") '()))
(ok "config$1 (outer nested literal) registered"
    (and (string=? (reg-ns "jfn$app/config$1") "app")
         (equal? (reg-frees "jfn$app/config$1") '())))
(ok "config$2 (inner literal) free-names = y"
    (equal? (reg-frees "jfn$app/config$2") '("y")))
(ok "v2$0 (variadic) registered"
    (and (string=? (reg-ns "jfn$app/v2$0") "app")
         (equal? (reg-frees "jfn$app/v2$0") '())))
(ok "captured$0 free-names = base (captured only through the nested literal)"
    (equal? (reg-frees "jfn$app/captured$0") '("base")))
(ok "captured$1 free-names = base"
    (equal? (reg-frees "jfn$app/captured$1") '("base")))
(ok "shadowed$0 free-names = y (shadow case)"
    (equal? (reg-frees "jfn$app/shadowed$0") '("y")))

;; --- the live closure's inspector name equals the registered name ---
(define cfg (var-deref "app" "config"))
(define kh (keyword #f "handler"))
(define kn (keyword #f "nested"))
(ok "handler closure name" (string=? (closure-name (jolt-get cfg kh jolt-nil)) "jfn$app/config$0"))
(ok "nested closure name" (string=? (closure-name (jolt-get cfg kn jolt-nil)) "jfn$app/config$1"))
(ok "inner closure name" (string=? (closure-name ((jolt-get cfg kn jolt-nil) 3)) "jfn$app/config$2"))
(define v2 (var-deref "app" "v2"))
(define kf (keyword #f "f"))
(ok "variadic closure name" (string=? (closure-name (jolt-get v2 kf jolt-nil)) "jfn$app/v2$0"))
(ok "captured closure name" (string=? (closure-name (var-deref "app" "captured")) "jfn$app/captured$0"))
(ok "shadowed closure name" (string=? (closure-name (var-deref "app" "shadowed")) "jfn$app/shadowed$0"))

;; --- the closures still work (the letrec wrapper changed nothing) ---
(ok "handler calls" (eqv? ((jolt-get cfg kh jolt-nil) 5) 10))
(ok "nested calls" (eqv? (((jolt-get cfg kn jolt-nil) 3) 4) 7))
(ok "variadic calls" (eqv? ((jolt-get v2 kf jolt-nil) 1 2 3) 6))
(ok "captured calls" (eqv? ((var-deref "app" "captured") 7) 17))
(ok "shadowed calls" (eqv? ((var-deref "app" "shadowed") 2) 101))

;; --- clojure.core's own literals are registered too ---
;; They used to be excluded, so the seed prelude would stay byte-identical across
;; a mint -- and the consequence was that a closure core made (partial, comp, a
;; lazy seq from an overlay fn) could not be written to a state image at all.
;; Now core carries its source like any other namespace: the closure partial
;; returns is a registered literal, with a name and a registration to match.
(define pn (closure-name (var-deref "app" "p")))
(ok "a partial closure IS named" (and pn (string-prefix? pn "jfn$")))
(ok "...and its name resolves to a registration" (vector? (image-fn-form-lookup pn)))


;; a macro can splice a LIVE value (here the namespace object) into a fn body;
;; emit-quoted has no rendering for it, so the literal compiles UNREGISTERED
;; instead of failing the compilation (the Selmer regression)
(jolt-eval "(defmacro spliced-ns-fn [] (list 'fn '[x] (list 'str 'x *ns*)))" "app")
(jolt-eval "(def spliced {:f (spliced-ns-fn)})" "app")
(define spl (var-deref "app" "spliced"))
(define spl-f (jolt-get spl (keyword #f "f") jolt-nil))
(ok "spliced-live-value literal compiles and runs"
    (string? (jolt-invoke spl-f "pfx")))
(ok "spliced-live-value literal is unregistered (skipped, not fatal)"
    (let ((nm (closure-name spl-f)))
      (or (not nm) (not (image-fn-form-lookup nm)))))

;; --- registrations are SOURCE TEXT, parsed on the first lookup -------------
;; The emitted registration carries the literal's source as text, wrapped in
;; (image-fn-form-src "…"): a UTF-8 bytevector constant in the compiled runtime
;; -- one byte per character, where a Chez string is four and the quoted
;; construction this replaces was a let* of allocations run at every process
;; start. The registry parses the text the first time a lookup asks for it.
(define (emit-src ns str)   ; the emitted Scheme text of one top-level form
  (let-values (((f j) (rdr-read-form str 0 (string-length str))))
    (let ((ctx (make-analyze-ctx ns)))
      (jolt-ce-emit (jolt-ce-run-passes (jolt-ce-analyze ctx f) ctx)))))
(define (has? s sub)
  (let ((ns (string-length s)) (nsub (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i nsub) ns) #f)
            ((string=? (substring s i (+ i nsub)) sub) #t)
            (else (loop (+ i 1)))))))
;; the table slot as registered, before any parse (image-fn-form-entry files
;; pending rows but does not parse)
(define (raw-form name)
  (let ((e (image-fn-form-entry name))) (and e (vector-ref e 0))))

;; JOLT_LAZY_META=1 writes the rows of a form as one quoted table, filed into
;; the registry on the first lookup (fn-form-registry.ss image-register-fn-forms!).
(define lazy-rows? (eq? (jolt-lazy-meta-mode) 'all))
(let ((e (emit-src "app" "(def lazy1 {:f (fn [x] (* x 2))})")))
  (ok "a registration is emitted as source text"
      (if lazy-rows?
          (has? e "(image-register-fn-forms! \"app\" (image-fn-form-rows #(\"jfn$app/lazy1$0\" \"(fn* [x] (* x 2))\" #())))")
          (has? e "(image-register-fn-form! \"jfn$app/lazy1$0\" (image-fn-form-src \"(fn* [x] (* x 2))\") \"app\" (jolt-vector ))")))
  (ok "...and builds no quoted structure at load" (not (has? e "(jolt-symbol "))))
(let ((e (emit-src "app" "(def lazy2 {:a (fn [x] x) :b (fn [y] y)})")))
  (ok "several literals register as sibling calls, no let* header"
      (if lazy-rows?
          (and (has? e "(begin (image-register-fn-forms! \"app\" (image-fn-form-rows #(\"jfn$app/lazy2$")
               (has? e "#() \"jfn$app/lazy2$")
               (not (has? e "(let* ((_q$")))
          (and (has? e "(begin (image-register-fn-form! \"jfn$app/lazy2$0\"")
               (has? e " (image-register-fn-form! \"jfn$app/lazy2$1\"")
               (not (has? e "(let* ((_q$"))))))
(jolt-eval "(def lazy1 {:f (fn [x] (* x 2))})" "app")
;; under JOLT_LAZY_META=1 the row is still in its pending table here; reading
;; through the API files it, so the reader sees it as eager registration left it
(ok "before the first lookup the slot holds the bytes" (bytevector? (raw-form "jfn$app/lazy1$0")))
(ok "the first lookup parses the form" (string=? (reg-form-head "jfn$app/lazy1$0") "fn*"))
(ok "...and caches it in the slot" (not (bytevector? (raw-form "jfn$app/lazy1$0"))))
(ok "the parsed form carries no reader position"
    (let ((form (vector-ref (image-fn-form-lookup "jfn$app/lazy1$0") 0)))
      (jolt-nil? (jolt-get (jolt-meta form) (keyword #f "line") jolt-nil))))

;; Round-trip fidelity: the text reads back to the SAME construction the
;; registration used to carry (emit-quoted, the image writer's view of a form),
;; for every literal kind a fn body can hold. The back end checks exactly this
;; before it emits text, so a form that fails here would fall back instead; the
;; rows pin that the common kinds never do.
(define emit-quoted (var-deref "jolt.backend-scheme" "emit-quoted"))
(define fnsrc-src (var-deref "jolt.backend-scheme" "fnsrc-src"))
(define (quoted-text f) (jolt-invoke1 emit-quoted f))
(define (round-trips? src)
  (let* ((f (jolt-ce-read src))
         (t (jolt-invoke1 fnsrc-src f))
         (back (image-fn-form-parse t)))
    (string=? (quoted-text back) (quoted-text f))))
(for-each
  (lambda (src) (ok (string-append "source round-trips: " src) (round-trips? src)))
  (list "(fn* [^long n ^String s] (/ (+ n 1/2) 2))"
        "(fn* [] [##Inf ##-Inf ##NaN 1.5 -2 12345678901234567890 1.0E10 -0.0 0])"
        "(fn* [] [\\a \\newline \\space \\tab \\( \\\\ \\u00e9])"
        "(fn* [] [\"a\\\"b\\nc\\\\d\\t\\u00e9\" :k :ns/k nil true false])"
        "(fn* [] [#{3 1 2} {:b 2 :a 1} (quote sym) (quote ns/sym) () (1 2) [1 [2]]])"
        "(fn* [] {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10})"
        "(fn* [] [#\"a\\\\d\\\"\" #inst \"2020-01-02T00:00:00Z\" #uuid \"3b241101-e2bb-4255-8caf-4136c566a962\" 1.5M #foo/bar [1 2]])"
        "(fn* [x] (fn* [y] (fn* [z] (+ x y z))))"
        "(fn* [^{:tag long :foo true} n] (let [x (quote ^:kw q)] n))"
        "(fn* [] (clojure.core// 1 2))"
        "(fn* [] (.foo Foo. a.b/c))"))

;; A form with no source rendering (a macro spliced a live class value into the
;; body) still registers, through the quoted construction it always used -- as
;; a form, never as text.
(jolt-eval "(defmacro cls-fn [] (list 'fn '[x] (list 'instance? String 'x)))" "app")
(let ((e (emit-src "app" "(def clsf {:f (cls-fn)})")))
  (ok "an unrenderable literal falls back to the quoted construction"
      (and (has? e "(let* ((_q$0") (has? e "(jolt-class-for "))))
(jolt-eval "(def clsf {:f (cls-fn)})" "app")
(ok "...and registers a form, not text"
    (and (vector? (image-fn-form-lookup "jfn$app/clsf$0"))
         (not (bytevector? (raw-form "jfn$app/clsf$0")))))

;; The minted seed carries every core literal as text: no quoted construction
;; is left in it.
(define (file-has? path sub)
  (has? (call-with-port (open-input-file path) get-string-all) sub))
(ok "the seed prelude registers every literal as source text"
    (not (file-has? "host/chez/seed/prelude.ss" "(let* ((_q$0")))
(ok "the seed image registers every literal as source text"
    (not (file-has? "host/chez/seed/image.ss" "(let* ((_q$0")))


;; --- direct-link: a top-level do splices per statement, and each statement
;; inherits the namespace. The direct-link arm re-enters emit-top-form per
;; statement, and a statement carries no :ns of its own -- rebound to nil,
;; every fn literal in a non-def statement (a deftype method body, a defmethod's
;; fn) was emitted unnamed and unregistered, and a reify instance holding one
;; refused to dump.
(jolt-eval "(def dl-holder (atom nil))" "app")
((var-deref "jolt.backend-scheme" "set-direct-link!") #t)
(define dl-closure
  (guard (e (#t ((var-deref "jolt.backend-scheme" "set-direct-link!") #f) (raise e)))
    (jolt-eval "(do (reset! dl-holder (fn [x] (+ x 1))) @dl-holder)" "app")))
((var-deref "jolt.backend-scheme" "set-direct-link!") #f)
(ok "do-spliced literal is named under direct-link"
    (string-prefix? (or (closure-name dl-closure) "") "jfn$app/$"))
(ok "do-spliced literal is registered"
    (and (closure-name dl-closure) (image-fn-form-lookup (closure-name dl-closure)) #t))

;; --- non-def literals: the counter is per NAMESPACE, not per top-level form.
;; Per form, every deftype method body and every defmethod in a namespace was
;; jfn$<ns>/$0 and the registrations overwrote each other -- an image restore
;; of one such closure came back with the LAST form's source.
(define anon-a (jolt-eval "(let [f (fn [x] (* x 2))] f)" "app2"))
(define anon-b (jolt-eval "(let [f (fn [x] (* x 3))] f)" "app2"))
(ok "two top-level forms' literals have distinct names"
    (and (closure-name anon-a) (closure-name anon-b)
         (not (string=? (closure-name anon-a) (closure-name anon-b)))))
(ok "...and both registrations survive"
    (and (closure-name anon-a) (closure-name anon-b)
         (image-fn-form-lookup (closure-name anon-a))
         (image-fn-form-lookup (closure-name anon-b))
         (string=? (reg-form-head (closure-name anon-a)) "fn*")
         #t))

;; --- the label's separators cannot be forged by a name's own characters.
;; The name is jfn$<ns>/<def>$<n>: munge-chars never emits a `/`, and the
;; reader never lets one into a namespace or def name, so the `/` is
;; unambiguous. A `$` is not: it also starts an escape ($$ for a literal `$`,
;; $U..$ for a control char), so with `$` as the separator ns "a$" + def "c"
;; and ns "a" + def "$c" were both jfn$a$$$c$0 and the second registration
;; overwrote the first. Rows use the runtime eval path with the ns handed in
;; directly, since that is how an unusual ns name reaches the emitter.
(define (reg-form-text name)
  (let ((e (image-fn-form-lookup name))) (and e (jolt-pr-str (vector-ref e 0)))))
(define (closure-of ns nm) (var-deref ns nm))
(jolt-eval "(def c (let [f (fn [x] (* x 10))] f))" "a$")
(jolt-eval "(def $c (let [f (fn [x] (* x 20))] f))" "a")
(ok "a `$` in a namespace name cannot forge the ns/def separator"
    (let ((n1 (closure-name (closure-of "a$" "c")))
          (n2 (closure-name (closure-of "a" "$c"))))
      (and n1 n2 (not (string=? n1 n2)))))
(ok "...and both registrations survive with their own source"
    (and (has? (or (reg-form-text (closure-name (closure-of "a$" "c"))) "") "10")
         (has? (or (reg-form-text (closure-name (closure-of "a" "$c"))) "") "20")))
(jolt-eval "(def $c (let [f (fn [x] (* x 30))] f))" "x\x01;")
(jolt-eval "(def c (let [f (fn [x] (* x 40))] f))" "x\x01;$")
(ok "a control char + `$` in a namespace name cannot forge it either"
    (let ((n1 (closure-name (closure-of "x\x01;" "$c")))
          (n2 (closure-name (closure-of "x\x01;$" "c"))))
      (and n1 n2 (not (string=? n1 n2))
           (has? (or (reg-form-text n1) "") "30")
           (has? (or (reg-form-text n2) "") "40"))))
(ok "the label is munge-chars over the fqn, not a plain-text substitution"
    (string=? (or (closure-name (closure-of "a$" "c")) "") "jfn$a$$/c$0"))

;; --- a NAMED inner literal is keyed by its scope, not by its name alone.
;; Bound under <name>$jf<n> with the counter per def, `mapi` in def a and
;; `mapi` in def b (or in two namespaces) both registered as mapi$jf0, and
;; an image restore of a's closure came back with b's source. The label is
;; now jfn$<ns>/<def>$<n>/<name>: the anon label plus the name, so the
;; registry key is unique and a backtrace still knows what the user called it.
(jolt-eval "(def a (let [mapi (fn mapi [x] (* x 2))] mapi))" "app")
(jolt-eval "(def b (let [mapi (fn mapi [x] (* x 3))] mapi))" "app")
(jolt-eval "(def a (let [mapi (fn mapi [x] (* x 4))] mapi))" "app.other")
(ok "same-named inner fns in two defs have distinct names"
    (let ((n1 (closure-name (closure-of "app" "a")))
          (n2 (closure-name (closure-of "app" "b"))))
      (and n1 n2 (not (string=? n1 n2)))))
(ok "...and in two namespaces"
    (let ((n1 (closure-name (closure-of "app" "a")))
          (n3 (closure-name (closure-of "app.other" "a"))))
      (and n1 n3 (not (string=? n1 n3)))))
(ok "each keeps its own registration"
    (and (has? (or (reg-form-text (closure-name (closure-of "app" "a"))) "") "(* x 2)")
         (has? (or (reg-form-text (closure-name (closure-of "app" "b"))) "") "(* x 3)")
         (has? (or (reg-form-text (closure-name (closure-of "app.other" "a"))) "") "(* x 4)")))
(ok "a named inner literal's label is the anon label plus its name"
    (string=? (or (closure-name (closure-of "app" "a")) "") "jfn$app/a$0/mapi"))
;; The runtime eval path used to bind a named inner literal under <ns>/<name>
;; and register nothing, so the closure dumped from a build but refused from
;; a `jolt run` session. Registration is the same on both paths now.
(ok "a named inner literal is registered on the runtime eval path"
    (string=? (or (reg-form-head "jfn$app/a$0/mapi") "") "fn*"))
(let ((e (emit-src "app" "(def a (let [mapi (fn mapi [x] (* x 2))] mapi))")))
  (ok "...and the emitted text registers it under the same label"
      (has? e (if lazy-rows? "\"jfn$app/a$0/mapi\" \"(fn* mapi" "(image-register-fn-form! \"jfn$app/a$0/mapi\""))))
;; A def's DIRECT named init is the var's root: it is not a literal to
;; register, and its frame keeps the <ns>/<name> the source registry keys on.
(jolt-eval "(defn direct [x] (inc x))" "app")
(ok "a def's direct named init still binds under ns/name"
    (string=? (or (closure-name (closure-of "app" "direct")) "") "app/direct"))
(ok "...and is not registered as a literal" (not (image-fn-form-lookup "app/direct")))
;; A backtrace shows the named literal as <ns>/<def>/<name>, the way
;; clojure.stacktrace demunges user$f$mapi__12; the counter is a compiler
;; artifact. An anonymous literal's label is shown as is.
(ok "display: named literal" (string=? (srcreg-display-name "jfn$app/a$0/mapi") "app/a/mapi"))
(ok "display: named literal outside a def" (string=? (srcreg-display-name "jfn$app/$0/f") "app/f"))
(ok "display: anon literal is shown as is" (string=? (srcreg-display-name "jfn$app/a$0") "jfn$app/a$0"))
(ok "display: a user name is left alone" (string=? (srcreg-display-name "jfn") "jfn"))
;; the inline splicer alpha-renames a named literal inside a spliced callee
;; (step-boom -> step-boom__il22); that artifact is stripped from the label too
(ok "display: splicer rename stripped from the name part"
    (string=? (srcreg-display-name "jfn$app.core/-main$0/step-boom__il22") "app.core/-main/step-boom"))
(ok "display: splicer rename stripped from a bare frame" (string=? (srcreg-display-name "step-boom__il7") "step-boom"))

;; A source row registered for later (JOLT_LAZY_META=1's def-var-lazy-meta!) is
;; seen by the first reader, whichever public read it is.
(jolt-register-source-later! "pq.later" "f1" (vector #t "pq/later.clj" 3))
(jolt-register-source-later! "pq.later" "f2" (vector "f2-proc" #f 4))
(ok "a later row answers a by-fqn read" (equal? (srcreg-record-for-fqn "pq.later/f1")
                                                (vector "pq.later" "f1" "pq/later.clj" 3)))
(ok "...and a by-name read, #t naming ns/name" (vector? (srcreg-record-for-name "pq.later/f1")))
(ok "...a named procname, with no file" (equal? (srcreg-record-for-name "f2-proc")
                                                (vector "pq.later" "f2" jolt-nil 4)))
(jolt-register-source-later! "pq.later" "f3" (vector #t #f 5))
(ok "...and the fqn snapshot"
    (let-values (((ks vs) (srcreg-fqn-entries)))
      (and (vector? ks) (exists (lambda (k) (equal? k "pq.later/f3")) (vector->list ks)))))

;; The registries' tables and pending queues are private: only their own file
;; names them (fn-form-registry.ss, source-registry.ss, rt.ss's def ordinals), so
;; no reader can bypass the API that files pending rows under the lock.
(define (file-text path)
  (call-with-input-file path (lambda (p) (get-string-all p))))
(define (private-names-outside owner prefix)
  (let loop ((fs (append (map (lambda (f) (string-append "host/chez/" f))
                              (directory-list "host/chez"))
                         (map (lambda (f) (string-append "host/chez/java/" f))
                              (directory-list "host/chez/java"))))
             (acc '()))
    (cond ((null? fs) acc)
          ((and (not (string=? (car fs) owner))
                (let ((n (string-length (car fs))))
                  (and (> n 3) (string=? (substring (car fs) (- n 3) n) ".ss")))
                (file-regular? (car fs))
                (has? (file-text (car fs)) prefix))
           (loop (cdr fs) (cons (car fs) acc)))
          (else (loop (cdr fs) acc)))))
(for-each
  (lambda (row)
    (let ((bad (private-names-outside (car row) (cadr row))))
      (ok (string-append (cadr row) " names are private to " (car row))
          (or (null? bad) (begin (printf "  named in: ~s\n" bad) #f)))))
  '(("host/chez/fn-form-registry.ss" "%fn-form-")
    ("host/chez/source-registry.ss" "%srcreg-")
    ("host/chez/rt.ss" "%var-def-ordinals")))

(printf "\nfnform gate: ~a/~a passed\n" (- total fails) total)
(exit (if (> fails 0) 1 0))
