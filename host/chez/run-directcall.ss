;; run-directcall.ss — the call shapes a --direct-link build lowers to a direct
;; Scheme call, and the guards that keep them sound (backend_scheme emit):
;;   * a SEED var (clojure.core and the other namespaces the runtime image boots
;;     with) is called through its root procedure, bound once at load — no
;;     var-cell-deref, no jolt-invokeN — when jolt.host/seed-callable? says so:
;;     the root is a procedure whose arity mask admits the call, the var is not
;;     ^:dynamic/^:redef, and the app has not redefined it. Off direct-link, and
;;     for a wrong arity, a non-procedure root or an app var, the site keeps the
;;     var-routed jolt-invoke.
;;   * an interop call on an UNPROVEN receiver tests the receiver at the site and
;;     takes the string/keyword direct form, with record-method-dispatch as the
;;     slow arm; a method with no direct form keeps the plain dispatch.
;;   * the unchecked-* family lowers to its helpers (jolt-uncadd2, jolt-uncinc)
;;     and unchecked-long/-int are casts that type their result :long, so a loop
;;     written in the hinted-Clojure idiom keeps fixnum counters.
;;   * `case` compares keyword/nil/boolean constants with identical?.
;;
;;   chez --script host/chez/run-directcall.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")

(define analyze          (var-deref "jolt.analyzer" "analyze"))
(define numeric-annotate (var-deref "jolt.passes.numeric" "annotate"))
(define emit-top-form    (var-deref "jolt.backend-scheme" "emit-top-form"))
(define set-direct-link! (var-deref "jolt.backend-scheme" "set-direct-link!"))
(define seed-callable?   (var-deref "jolt.host" "seed-callable?"))
(define U ((var-deref "jolt.passes.types" "new-unit")))
((var-deref "jolt.backend-scheme" "set-emit-unit!") U)
((var-deref "jolt.backend-scheme" "set-prelude-mode!") #t)

(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))
(define (emit-dl src)
  (set-direct-link! #t)
  (let ((e (emit-top-form (numeric-annotate (anode src)))))
    (set-direct-link! #f)
    e))
(define (emit-nodl src) (emit-top-form (numeric-annotate (anode src))))
(define (run-emit scm) (eval (read (open-input-string scm)) (interaction-environment)))
(define (ev s) (jolt-compile-eval s "user"))
(define (call name . args) (apply jolt-invoke (var-deref "user" name) args))
(define (raises? thunk) (guard (e (#t #t)) (thunk) #f))

;; ---- seed direct-link ------------------------------------------------------
;; A var the seed minted direct-linked (bootstrap.ss): the site applies its jv$
;; binding, and a redefinition writes through to that binding (rt.ss
;; var-root-set!), so the site follows it.
(let ((e (emit-dl "(def usetrue (fn [x] (true? x)))")))
  (gate-check "seed call applies the linked jv$ binding" (gate-sub? e "(jv$clojure.core/true? ") #t)
  (gate-check "seed call hoists no root" (gate-sub? e "jolt-seed-root") #f)
  (gate-check "seed call is a direct application, not jolt-invoke1" (gate-sub? e "jolt-invoke1") #f)
  (gate-check "seed call does not deref the var cell" (gate-sub? e "var-cell-deref") #f)
  (run-emit e)
  (gate-check "direct seed call answers true" (call "usetrue" #t) #t)
  (gate-check "direct seed call answers false" (call "usetrue" 1) #f)
  (let ((cell (jolt-var "clojure.core" "true?")) (orig (var-deref "clojure.core" "true?")))
    (jolt-alter-var-root cell (lambda (old) (lambda (x) 'patched)))
    (gate-check "a redefined linked seed var is seen by a direct site" (call "usetrue" #t) 'patched)
    (jolt-alter-var-root cell (lambda (old) orig))
    (gate-check "...and so is the restore" (call "usetrue" #t) #t)))
;; A seed var the runtime defined itself (a Scheme def-var!, no jv$ binding):
;; its root is hoisted once at load, as before.
(let ((e (emit-dl "(def usearraymap (fn [k v] (array-map k v)))")))
  (gate-check "an unlinked seed var hoists its root at load" (gate-sub? e "(jolt-seed-root (jolt-var \"clojure.core\" \"array-map\"))") #t)
  (gate-check "an unlinked seed call is not jolt-invoke2" (gate-sub? e "jolt-invoke2") #f)
  (run-emit e)
  (gate-check "the hoisted seed call answers" (jolt-get (call "usearraymap" (keyword #f "a") 1) (keyword #f "a") jolt-nil) 1))
(let ((e (emit-nodl "(def usetrue2 (fn [x] (true? x)))")))
  (gate-check "off direct-link the site stays var-routed" (gate-sub? e "jolt-invoke1") #t)
  (gate-check "off direct-link no root is bound" (gate-sub? e "jolt-seed-root") #f))
(let ((e (emit-dl "(def wrongarity (fn [x] (true? x 1)))")))
  (gate-check "a wrong arity keeps jolt-invoke (it owns the ArityException)" (gate-sub? e "jolt-invoke2") #t)
  (gate-check "a wrong arity binds no root" (gate-sub? e "jolt-seed-root") #f))
(let ((e (emit-dl "(def nonproc (fn [] (*print-length*)))")))
  (gate-check "a non-procedure root keeps jolt-invoke" (gate-sub? e "jolt-invoke0") #t)
  (gate-check "a non-procedure root binds no root" (gate-sub? e "jolt-seed-root") #f))
(ev "(defn helper [x] (+ x 1))")
(let ((e (emit-dl "(def useapp (fn [] (helper 1)))")))
  (gate-check "an app var is not a seed var" (gate-sub? e "jolt-seed-root") #f))
;; deref is NOT a lowered native. A library that rebinds clojure.core/deref for
;; its own reference type (glimmer.ratom, so @ tracks a reactive cell) has to be
;; seen by every @ compiled in the app, in a build as much as under `jolt run`;
;; as a native op the site called jolt-deref itself and the rebind reached
;; nothing. Under direct-link the site hoists the var's root at app load, like
;; any other seed native: the boot asserts deref six times, once per
;; set!-extended layer of jolt-deref, and that used to read as a redefinition
;; (var-redefined?) and keep every app call var-routed — the overhead #1008
;; chased by lowering it. A boot re-assertion is not an app redefinition: the
;; mark is cleared when the runtime image finishes booting (rt.ss
;; var-redefined-clear!, at hc-runtime-image-booted!), so a var the boot
;; defined twice is one definition to everything compiled after it. The
;; hoist runs after every dependency's top level, so the rebind is seen.
(let ((e (emit-dl "(def usederef (fn [a] @a))")))
  (gate-check "@ under direct-link does not call jolt-deref" (gate-sub? e "jolt-deref") #f)
  (gate-check "@ under direct-link hoists deref's root at load" (gate-sub? e "(jolt-seed-root (jolt-var \"clojure.core\" \"deref\"))") #t)
  (gate-check "@ under direct-link is not var-routed" (gate-sub? e "jolt-invoke1") #f)
  ;; The rebind comes FIRST, as a library's does (its top level runs before any
  ;; namespace that requires it loads), and the site loaded after it hoists the
  ;; rebound root. A rebind after the site has loaded is invisible to it, as to
  ;; every direct-linked seed call; `jolt run` never direct-links.
  (let ((cell (jolt-var "clojure.core" "deref")) (orig (var-deref "clojure.core" "deref")))
    (jolt-alter-var-root cell (lambda (old) (lambda (x) 'rebound)))
    (run-emit e)
    (gate-check "@ loaded after a rebind of deref sees the rebind" (call "usederef" 1) 'rebound)
    (jolt-alter-var-root cell (lambda (old) orig))
    (run-emit (emit-dl "(def usederef3 (fn [a] @a))"))
    (gate-check "...and a site loaded after the restore sees the original" (call "usederef3" (jolt-atom-new 7)) 7)))
(let ((e (emit-nodl "(def usederef2 (fn [a] @a))")))
  (gate-check "@ off direct-link does not call jolt-deref" (gate-sub? e "jolt-deref") #f)
  (gate-check "@ off direct-link is var-routed" (gate-sub? e "jolt-invoke1") #t))
;; The same for the other boot-layered natives (49 of them: map? and coll?
;; three times, __close four, var-get/with-meta/type/slurp/spit twice, …): each
;; is one definition once the image has booted, and direct-calls.
(gate-check "the boot's re-assertions are not redefinitions once the image has booted"
            (var-redefined? "clojure.core" "deref") #f)
(gate-check "seed-callable?: a boot-layered native answers #t" (seed-callable? jolt-nil "clojure.core" "map?" 1) #t)
(let ((e (emit-dl "(def usemap? (fn [x] (map? x)))")))
  (gate-check "a boot-layered native hoists its root under direct-link" (gate-sub? e "(jolt-seed-root (jolt-var \"clojure.core\" \"map?\"))") #t)
  (run-emit e)
  (gate-check "the hoisted layered native answers" (call "usemap?" (jolt-hash-map)) #t))
;; ...and an APP var defined twice is still a redefinition after it.
(ev "(defn twice [] 1)")
(ev "(defn twice [] 2)")
(gate-check "an app var defined twice is redefined" (var-redefined? "user" "twice") #t)
(gate-check "seed-callable?: clojure.core/true? at 1 arg names its binding" (seed-callable? jolt-nil "clojure.core" "true?" 1) "jv$clojure.core/true?")
(gate-check "seed-callable?: a runtime-defined seed var answers #t" (seed-callable? jolt-nil "clojure.core" "array-map" 2) #t)
(gate-check "seed-callable?: wrong arity refused" (jolt-nil? (seed-callable? jolt-nil "clojure.core" "true?" 3)) #t)
(gate-check "seed-callable?: a dynamic var refused" (jolt-nil? (seed-callable? jolt-nil "clojure.core" "*print-length*" 0)) #t)
(gate-check "seed-callable?: an app var refused" (jolt-nil? (seed-callable? jolt-nil "user" "helper" 1)) #t)
(gate-check "seed-callable?: an unknown var refused" (jolt-nil? (seed-callable? jolt-nil "clojure.core" "no-such-fn-here" 1)) #t)
(gate-check "jolt-seed-root binds a stub on a non-procedure root (the raise is at the call)"
            (and (not (raises? (lambda () (jolt-seed-root (jolt-var "clojure.core" "*print-length*")))))
                 (raises? (lambda () ((jolt-seed-root (jolt-var "clojure.core" "*print-length*")) 1))))
            #t)

;; ---- the unhinted interop guard --------------------------------------------
(let ((e (emit-dl "(def uselen (fn [s] (.length s)))")))
  (gate-check "unhinted .length tests for a string at the site" (gate-sub? e "(string? _ht$") #t)
  (gate-check "unhinted .length takes the direct form" (gate-sub? e "(string-length _ht$") #t)
  (gate-check "unhinted .length keeps the generic slow arm" (gate-sub? e "record-method-dispatch") #t)
  (run-emit e)
  (gate-check "a string receiver answers" (call "uselen" "abcd") 4)
  (gate-check "a StringBuilder receiver still reaches the generic arm"
              (ev "(user/uselen (doto (StringBuilder.) (.append \"ab\")))") 2))
(let ((e (emit-dl "(def usecharat (fn [s i] (.charAt s i)))")))
  ;; the direct form is String.charAt's checked read, not a bare string-ref
  (gate-check "unhinted .charAt takes the direct form" (gate-sub? e "(jolt-char-at _ht$") #t)
  (run-emit e)
  (gate-check "unhinted .charAt on a string answers" (call "usecharat" "abc" 1) #\b)
  (gate-check "unhinted .charAt out of range is the JVM's message"
              (ev "(try (user/usecharat \"abc\" 3) (catch StringIndexOutOfBoundsException e (ex-message e)))")
              "Index 3 out of bounds for length 3"))
(let ((e (emit-dl "(def usename (fn [k] (.getName k)))")))
  (gate-check "unhinted .getName tests for a keyword" (gate-sub? e "(keyword-t? _ht$") #t)
  (gate-check "unhinted .getName takes the keyword direct form" (gate-sub? e "(keyword-t-name _ht$") #t)
  (run-emit e)
  (gate-check "a keyword receiver answers" (call "usename" (keyword "a" "b")) "b"))
(let ((e (emit-dl "(def usefoo (fn [x] (.frobnicate x)))")))
  (gate-check "a method with no direct form keeps the plain dispatch" (gate-sub? e "_ht$") #f)
  (gate-check "...as record-method-dispatch" (gate-sub? e "record-method-dispatch") #t))
;; receiver and args are evaluated once each, in order
(ev "(def calls (atom []))")
(ev "(defn note [v] (swap! calls conj v) v)")
(let ((e (emit-dl "(def order (fn [] (.indexOf (note \"hello\") (note \"l\"))))")))
  (run-emit e)
  (gate-check "guarded site evaluates receiver then args once" (call "order") 2)
  (gate-check "...in source order" (jolt=2 (ev "@user/calls") (jolt-vector "hello" "l")) #t))

;; ---- the unchecked family --------------------------------------------------
(let ((e (emit-dl "(def uadd (fn [a b] (unchecked-add a b)))")))
  (gate-check "unchecked-add lowers to jolt-uncadd2" (gate-sub? e "(jolt-uncadd2 a b)") #t)
  (gate-check "unchecked-add is not a var call" (gate-sub? e "var-cell-deref") #f)
  (run-emit e)
  (gate-check "unchecked-add wraps at 64 bits" (call "uadd" 9223372036854775807 1) -9223372036854775808))
(let ((e (emit-dl "(def uinc (fn [a] (unchecked-inc a)))")))
  (gate-check "unchecked-inc lowers to jolt-uncinc" (gate-sub? e "(jolt-uncinc a)") #t))
(let ((e (emit-dl "(def scan (fn [^String s] (let [n (long (.length s))] (loop [i (unchecked-long 0) acc 0] (if (>= i n) acc (recur (unchecked-inc i) (unchecked-add acc (unchecked-long (unchecked-int (.charAt s (unchecked-int i)))))))))))")))
  (gate-check "unchecked-long init types the loop counter :long" (gate-sub? e "jolt-l>=") #t)
  (gate-check "the counter's unchecked-inc stays on the wrapping helper" (gate-sub? e "jolt-uncinc") #t)
  (gate-check "no cast in the loop is a var call" (gate-sub? e "var-cell-deref") #f)
  (gate-check "the long casts pass a fixnum through inline" (gate-sub? e "(fixnum? _lc$") #t)
  (run-emit e)
  (gate-check "the scan sums code points" (call "scan" "abc") 294))

;; ---- case on interned constants ---------------------------------------------
(let ((e (emit-dl "(def kcase (fn [x] (case x :a 1 :b 2 nil 0 3)))")))
  (gate-check "keyword/nil case clauses compare by identity" (gate-sub? e "jolt-identical?") #t)
  (gate-check "...and not by =" (gate-sub? e "jolt=2") #f)
  (run-emit e)
  (gate-check "case hits a keyword" (call "kcase" (keyword #f "b")) 2)
  (gate-check "case hits nil" (call "kcase" jolt-nil) 0)
  (gate-check "case fed a symbol takes the default" (call "kcase" (jolt-symbol #f "a")) 3))
(let ((e (emit-dl "(def scase (fn [x] (case x \"s\" :str q :sym 7 :num :other)))")))
  (gate-check "string/symbol/number clauses keep =" (gate-sub? e "jolt=2") #t)
  (run-emit e)
  (gate-check "case hits a string" (call "scase" "s") (keyword #f "str"))
  (gate-check "case hits a symbol" (call "scase" (jolt-symbol #f "q")) (keyword #f "sym"))
  (gate-check "case fed a keyword takes the default" (call "scase" (keyword #f "s")) (keyword #f "other")))

;; --- a seed var this runtime lacks binds a stub that raises at the CALL -------
;; The hoist runs when the referencing namespace loads. A kept def that names a
;; var of a file the binary left out (jolt.host/scheme-eval-string in a build
;; that dropped the compiler half) must still load -- a default build prunes
;; nothing -- and fail only when called, naming the var.
(let* ((cell (jolt-var "gate.seed" "absent"))
       (stub (jolt-seed-root cell)))
  (gate-check "an unbound seed root hoists a stub, not a load error" (procedure? stub) #t)
  (gate-check "the stub raises at the call, naming the var"
              (guard (e (#t (and (string? (condition-message e))
                                 (gate-sub? (condition-message e) "gate.seed/absent"))))
                (stub 1) #f)
              #t))

;; A StringBuilder local takes the unsynchronized direct forms; a StringBuffer
;; local takes the locking ones, because the JDK's StringBuffer methods are
;; synchronized. Routing a StringBuffer through sb-append! would race.
(let ((e (emit-dl "(def usesbld (fn [] (let [sb (StringBuilder.)] (.append sb \"x\") (.charAt sb 0) (.toString sb))))")))
  (gate-check "a StringBuilder local appends unsynchronized" (gate-sub? e "(sb-append! ") #t)
  (gate-check "...and never through the locking form" (gate-sub? e "sb-append*!") #f))
(let ((e (emit-dl "(def usesbuf (fn [] (let [sb (StringBuffer.)] (.append sb \"x\") (.charAt sb 0) (.length sb) (.toString sb))))")))
  (gate-check "a StringBuffer local appends under its lock" (gate-sub? e "(sb-append*! ") #t)
  (gate-check "...reads under it too" (and (gate-sub? e "(sb-char-at* ") (gate-sub? e "(sb-length* ") (gate-sub? e "(sb-str* ")) #t)
  (gate-check "...and never through the unsynchronized forms"
              (or (gate-sub? e "(sb-append! ") (gate-sub? e "(sb-str ") (gate-sub? e "(sb-char-at ")) #f)
  (run-emit e)
  (gate-check "the locked direct forms answer" (call "usesbuf") "x"))
(gate-summary "directcall")

