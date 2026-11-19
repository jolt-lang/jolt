;; IR inlining (jolt.passes.inline), enabled under optimization. A small
;; single-arity defn is stashed and spliced at its call sites, removing the call.
;; A ^double/^long fn's param-entry and return coercions travel with the splice
;; (via :coerce nodes) so an inlined call matches the called one — incl. coercing a
;; non-double arg — and the body's fl*/fx* fast path still fires. Run:
;;   chez --script test/chez/inline-test.ss

(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred) (set! total (+ total 1)) (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
(define (has? s sub)
  (let ((ns (string-length s)) (nsub (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i nsub) ns) #f)
            ((string=? (substring s i (+ i nsub)) sub) #t)
            (else (loop (+ i 1)))))))
(define (emitf ns str)            ; analyze + run-passes (optimize on) + emit
  (let-values (((f j) (rdr-read-form str 0 (string-length str))))
    (let ((ctx (make-analyze-ctx ns)))
      (jolt-ce-emit (jolt-ce-run-passes (jolt-ce-analyze ctx f) ctx)))))
;; The CODE portion of an emission. An anon literal's registration replays its
;; original SOURCE form, so a callee name that inlining removed from the code is
;; still there verbatim — drop the registrations before asserting.
;;
;; They are a PREAMBLE, not a tail: emit-top-form puts them first, as
;; (begin <registrations> <code>). Cutting at the first (image-register-fn-form!
;; and keeping the prefix therefore kept "(begin " and threw the code away, so
;; every assertion below that used this passed no matter what the code said. It
;; only surfaced when the registrations grew a (let* …) header binding the shared
;; constructions, which put the callee's name into the part being kept.
;;
;; So match parens instead of scanning for a marker: skip the one balanced form
;; that follows "(begin " and keep the rest. String literals are opaque to the
;; scan — the registrations are full of them, and a symbol named ")" would
;; otherwise unbalance it.
(define (skip-form s i)
  (let ((n (string-length s)))
    (let loop ((i i) (depth 0) (in-str #f))
      (if (>= i n)
          i
          (let ((c (string-ref s i)))
            (cond
              (in-str (cond ((char=? c #\\) (loop (+ i 2) depth #t))
                            ((char=? c #\") (loop (+ i 1) depth #f))
                            (else (loop (+ i 1) depth #t))))
              ((char=? c #\") (loop (+ i 1) depth #t))
              ((char=? c #\() (loop (+ i 1) (+ depth 1) #f))
              ((char=? c #\)) (if (= depth 1) (+ i 1) (loop (+ i 1) (- depth 1) #f)))
              (else (loop (+ i 1) depth #f))))))))
;; Matched against the two shapes the preamble can take and nothing else: a
;; top-level `do` also emits (begin …, and dropping its first statement would be a
;; silent hole in whatever asserted on it.
(define (starts-with? s pre)
  (and (>= (string-length s) (string-length pre))
       (string=? (substring s 0 (string-length pre)) pre)))
;; The code is the LAST form of that begin: the preamble is registrations and,
;; for a literal with no source rendering, a (let* …) header — but the code can
;; open with a (let* …) of its own too (a top-level fn's constant pool), so
;; guessing which leading forms are preamble by their head threw the code away
;; and left ")" to assert on. Taking the last form cannot mistake one for the other.
(define (code-part s)
  (if (or (starts-with? s "(begin (let* (")
          (starts-with? s "(begin (image-register-fn-form!"))
      (let loop ((i 7) (last-start #f))   ; 7 = past "(begin "
        (let ((i (let ws ((i i))
                   (if (and (< i (string-length s)) (char=? (string-ref s i) #\space))
                       (ws (+ i 1)) i))))
          (if (or (>= i (string-length s)) (char=? (string-ref s i) #\)))
              (if last-start
                  (let ((end (skip-form s last-start))) (substring s last-start end))
                  s)
              (loop (skip-form s i) i))))
      s))
(define (ev s) (jolt-compile-eval s "u"))

;; inlining is a closed-world optimization — requires optimize + direct-link.
(set-optimize! #t)
(set-direct-link-flag! #t)

;; a small plain fn is spliced; the call to it disappears.
(ev "(def add1 (fn* ([x] (+ x 1))))")
(let ((e (emitf "u" "(fn* ([y] (add1 y)))")))
  (ok "plain fn is inlined (call to add1 gone)" (not (has? (code-part e) "add1")))
  (ok "inlined body present (jolt-n+ ... 1)" (has? e "(jolt-n+")))
(ok "inlined plain fn runtime: (add1 41) = 42" (= 42 (jnum->exact (ev "((fn* ([y] (add1 y))) 41)"))))

;; a ^double fn: body fl-ops fire after inlining, and the call is gone.
(ev "(def ^double dwork (fn* ([^double a ^double b] (+ (* a a) (* b b)))))")
(let ((e (emitf "u" "(fn* ([] (dwork 3.0 4.0)))")))
  (ok "inlined ^double fn body uses fl*" (has? e "(#3%fl*"))
  (ok "inlined ^double fn call to dwork is gone" (not (has? (code-part e) "dwork"))))
(ok "inlined ^double call: 3^2+4^2 = 25" (= 25 (jnum->exact (ev "((fn* ([] (dwork 3.0 4.0))))"))))
;; coercion travels with the splice: int args become doubles, so the result is a
;; flonum 25.0 — matching the called fn, not an exact 25.
(ok "inlined ^double with int args still returns a flonum" (flonum? (ev "((fn* ([] (dwork 3 4))))")))

;; the cast fast path is decided in the CALLER's context after inlining: a typed
;; body inlined where the argument is untyped still carries its entry coercion
;; (lowered), and an untyped body inlined under a ^double argument is lowered too.
(ev "(def dlong (fn* ([^double d] (long d))))")
(ev "(def ulong (fn* ([d] (long d))))")
(ok "inlined ^double cast under an untyped caller is lowered through its entry coercion"
    (has? (emitf "u" "(fn* ([x] (dlong x)))") "(flonum->fixnum"))
(ok "inlined untyped cast under a ^double caller is lowered"
    (has? (emitf "u" "(fn* ([^double x] (ulong x)))") "(flonum->fixnum"))
(ok "inlined untyped cast under an untyped caller is not lowered"
    (not (has? (emitf "u" "(fn* ([x] (ulong x)))") "(flonum->fixnum")))
;; which entry a lowered cast falls back to is the CALLEE's: an untyped param is
;; boxed at the JVM's call, whatever the caller proves about the argument.
(ev "(def dbyte (fn* ([^double d] (byte d))))")
(ev "(def ubyte (fn* ([d] (byte d))))")
(ev "(def ubyte-copy (fn* ([d] (let* [e d] (byte e)))))")
(ev "(def ubyte-outer (fn* ([^double d] (ubyte d))))")
(ok "inlined ^double byte falls back to the primitive entry"
    (has? (emitf "u" "(fn* ([x] (dbyte x)))") "(jolt-byte-cast-prim "))
(ok "inlined untyped byte under a ^double caller keeps the fast path"
    (has? (emitf "u" "(fn* ([^double x] (ubyte x)))") "(flonum->fixnum"))
(ok "inlined untyped byte under a ^double caller falls back boxed"
    (not (has? (emitf "u" "(fn* ([^double x] (ubyte x)))") "-prim ")))
(ok "a let copy of an untyped param stays boxed"
    (not (has? (emitf "u" "(fn* ([^double x] (ubyte-copy x)))") "-prim ")))
(ok "boxed survives a second splice"
    (not (has? (emitf "u" "(fn* ([x] (ubyte-outer x)))") "-prim ")))
(ok "lowered cast answers: 2.9 -> 2, -2.9 -> -2"
    (equal? '(2 -2) (list (jnum->exact (ev "(dlong 2.9)")) (jnum->exact (ev "(dlong -2.9)")))))

;; a ^long fn inlines with long coercion + the checked long ops.
(ev "(def ^long lsum (fn* ([^long a ^long b] (+ a b))))")
(let ((e (emitf "u" "(fn* ([] (lsum 3 4)))")))
  (ok "inlined ^long fn body uses jolt-l+" (has? e "(jolt-l+")))
(ok "inlined ^long call: 3+4 = 7 (fixnum)" (let ((r (ev "((fn* ([] (lsum 3 4))))"))) (and (fixnum? r) (= r 7))))

;; an accumulator over an inlined ^double call: the whole loop body fuses to fl-ops.
(ev "(def ^double sq (fn* ([^double x] (* x x))))")
(let ((e (emitf "u" "(fn* ([] (loop [acc 0.0 i 0] (if (< i 3) (recur (+ acc (sq 2.0)) (inc i)) acc))))")))
  (ok "accumulator over inlined ^double call lowers to fl+" (has? e "(#3%fl+"))
  (ok "the sq call is inlined away" (not (has? (code-part e) "sq"))))
(ok "accumulator over inlined ^double call: 3*4.0 = 12" (= 12 (jnum->exact (ev "((fn* ([] (loop [acc 0.0 i 0] (if (< i 3) (recur (+ acc (sq 2.0)) (inc i)) acc)))))"))))

;; --- recursion cycles are never inlined (jolt-682) ---------------------------
;; A fn whose stashed body can reach itself through other stashes (mutual
;; recursion) must not be spliced: the spliced body re-exposes a call into the
;; cycle, so each inline-fixpoint round pastes one more layer and the body grows
;; by the cycle's branching factor per round until the cap. Ruuter's 4-way
;; matcher cycle unrolled to 4^5 copies — a 3.4MB match-trie and +73MiB idle RSS.
;; The cycle member keeps its real call, exactly as it compiles without --opt.
(define (count-occ s sub)
  (let ((ns (string-length s)) (nsub (string-length sub)))
    (let loop ((i 0) (c 0))
      (cond ((> (+ i nsub) ns) c)
            ((string=? (substring s i (+ i nsub)) sub) (loop (+ i nsub) (+ c 1)))
            (else (loop (+ i 1) c))))))

;; two-fn cycle, branching 1
(ev "(declare pongf)")
(ev "(def pingf (fn* ([n] (if (< n 1) 0 (pongf (- n 1))))))")
(ev "(def pongf (fn* ([n] (if (< n 1) 1 (pingf (- n 1))))))")
(let ((e (emitf "u" "(fn* ([n] (pingf n)))")))
  (ok "a mutual-recursion member is not inlined (one pingf call remains)"
      (= 1 (count-occ (code-part e) "pingf")))
  (ok "its cycle partner is not pasted in" (= 0 (count-occ (code-part e) "pongf"))))
(ok "mutual pair still runs: (pingf 4) = 0" (= 0 (jnum->exact (ev "((fn* ([] (pingf 4))))"))))
(ok "mutual pair still runs: (pingf 3) = 1" (= 1 (jnum->exact (ev "((fn* ([] (pingf 3))))"))))

;; branching cycle (the jolt-682 shape): bx-a calls bx-b TWICE, bx-b calls back.
;; Unrolling this doubles per round — 2^rounds copies — so the count assertion
;; is the size gate.
(ev "(declare bx-b)")
(ev "(def bx-a (fn* ([n] (if (< n 1) 0 (+ (bx-b (- n 1)) (bx-b (- n 2)))))))")
(ev "(def bx-b (fn* ([n] (bx-a (- n 1)))))")
(let ((e (emitf "u" "(fn* ([n] (bx-a n)))")))
  (ok "a branching cycle member is not inlined" (= 1 (count-occ (code-part e) "bx-a")))
  (ok "no cycle layer is pasted in" (= 0 (count-occ (code-part e) "bx-b"))))
(ok "branching cycle still runs: (bx-a 3) = 0" (= 0 (jnum->exact (ev "((fn* ([] (bx-a 3))))"))))

;; an ACYCLIC helper called from cycle code still inlines — only membership in
;; the cycle disqualifies, not proximity to one.
(ev "(def tinyh (fn* ([x] (+ x 1))))")
(let ((e (emitf "u" "(def bx-caller (fn* ([n] (bx-a (tinyh n)))))")))
  (ok "an acyclic helper beside a cycle call is still inlined"
      (= 0 (count-occ (code-part e) "tinyh")))
  (ok "while the cycle call stays real" (= 1 (count-occ (code-part e) "bx-a"))))

;; --- code growth is bounded per top-level form (#1059) ------------------------
;; Each splice passes the per-site size check on its own, so a form with many
;; call sites of a mid-size callee used to grow by the callee's size at EVERY
;; site: clojure.test's `is` inlined do-report twice per assertion, a 41-line
;; deftest emitted 69KB of Scheme, and inlining was half of a test app's build.
;; The growth a form may take is bounded, charged at the NET size of a splice
;; (the body minus the call it replaces), so tiny helpers stay free.
(define (vec-of-calls f n)
  (let loop ((i 0) (acc ""))
    (if (= i n) (string-append "[" acc "]")
        (loop (+ i 1) (string-append acc " (" f " (+ y " (number->string i) "))")))))
(ev "(def midh (fn* ([x] (if (< x 0) (- 0 x) (if (> x 100) (* x 2) (+ (* x 3) (* x 4) (* x 5) (* x 6) (* x 7) (* x 8) (* x 9) (* x 10) (* x 11) (* x 12) (* x 13) (* x 14)))))))")
(let* ((src (string-append "(fn* ([y] " (vec-of-calls "midh" 300) "))"))
       (e (emitf "u" src))
       (left (count-occ (code-part e) "midh")))
  (ok "a mid-size callee at 300 sites is inlined at some of them" (< left 300))
  (ok "…but not at all of them: the form's growth is bounded" (> left 0)))
(ok "a partly inlined form still computes the same values"
    (equal? (map jnum->exact (seq->list (ev (string-append "((fn* ([y] " (vec-of-calls "midh" 300) ")) 1)"))))
            (let loop ((i 299) (acc '()))
              (if (< i 0) acc
                  (loop (- i 1) (cons (let ((x (+ 1 i))) (if (> x 100) (* x 2) (* x 102))) acc))))))
(let ((e (emitf "u" (string-append "(fn* ([y] " (vec-of-calls "add1" 300) "))"))))
  (ok "a tiny callee is inlined at every one of 300 sites" (= 0 (count-occ (code-part e) "add1"))))

;; --- scalar-replace moves field values only where they mean the same ----------
;; Dropping (let [m {:x a ..}] ..) moves `a` to each (:x m). Under a binder that
;; rebinds `a` it read the inner one (12, not 3); into a fn literal it left the
;; literal's captures naming m, which no longer exists, so a build's image maker
;; read an unbound variable. Both keep the map; the plain case still folds.
(ok "scalar-replace does not move a value under a binder that shadows its local"
    (= 3 (jnum->exact (ev "((fn* ([a] (let [m {:x a :y 2}] (let [a 10] (+ (:x m) (:y m)))))) 1)"))))
(ok "scalar-replace does not move a value under a loop binding that shadows it"
    (= 6 (jnum->exact (ev "((fn* ([a] (let [m {:x a :y 2}] (loop [a 0 acc 0] (if (< a 2) (recur (inc a) (+ acc (:x m) (:y m))) acc))))) 1)"))))
(ok "scalar-replace does not move a value under a fn param that shadows it"
    (= 3 (jnum->exact (ev "(((fn* ([a] (let [m {:x a :y 2}] (fn* ([a] (+ (:x m) (:y m))))))) 1) 100)"))))
(let ((e (code-part (emitf "u" "(fn* ([a] (let [m {:x a :y 2}] (fn* ([] (+ (:x m) (:y m)))))))"))))
  (ok "a map a fn literal captures is not scalar-replaced into it" (has? e "amap-slots->pmap")))
(let ((e (code-part (emitf "u" "(fn* ([a] (let [m {:x a :y 2}] (+ (:x m) (:y m)))))"))))
  (ok "a map read only in its own scope is still scalar-replaced" (not (has? e "amap-slots->pmap"))))
;; Moving a value that allocates to two reads made two objects where the map
;; held one: (identical? (:x m) (:x m)) was false, and a read in a loop body made
;; one per iteration. The value is bound once instead (records: build-smoke.sh).
(ok "a field read twice whose value allocates is read as one object"
    (eq? #t (ev "((fn* ([a] (let [m {:x [a] :y 2}] (identical? (:x m) (:x m))))) 1)")))
(ok "a field read once per loop iteration whose value allocates is one object"
    (eq? #t (ev "((fn* ([a] (let [m {:x [a] :y 2}] (loop [i 0 prev (:x m)] (if (< i 2) (recur (inc i) (:x m)) (identical? prev (:x m))))))) 1)")))
(let ((e (code-part (emitf "u" "(fn* ([a] (let [m {:x [a] :y 2}] (+ (count (:x m)) (:y m)))))"))))
  (ok "a map with an allocating field is still scalar-replaced" (not (has? e "amap-slots->pmap"))))
(let ((e (code-part (emitf "u" "(fn* ([a] (let [m {:x [a] :y (nil? a)}] (+ (count (:x m)) (if (:y m) 1 2) (if (:y m) 3 4)))))"))))
  (ok "a non-allocating field read twice is still scalar-replaced" (not (has? e "amap-slots->pmap"))))
(ok "a nested map read twice computes the same value" (= 4 (jnum->exact (ev "((fn* ([a] (let [m {:p {:x a :y 2} :q 1}] (+ (:x (:p m)) (:y (:p m)) (:q m))))) 1)"))))

;; Lexical facts describe occurrences in a particular IR revision. Scheme
;; paths below use symbols for the IR's keyword keys and integers for indices.
(define facts-unit ((var-deref "jolt.passes.types" "new-unit")))
(define body-facts (var-deref "jolt.passes.facts" "body-facts"))
(define binding-at (var-deref "jolt.passes.facts" "binding-at"))
(define origins-at (var-deref "jolt.passes.facts" "origins-at"))
(define clj= (var-deref "clojure.core" "="))
(define (fact-kw name) (keyword #f (symbol->string name)))
(define (ir-path steps)
  (apply jolt-vector (map (lambda (s) (if (symbol? s) (fact-kw s) s)) steps)))
(define (ir-at ir steps)
  (fold-left (lambda (x s)
               (if (symbol? s) (jolt-get x (fact-kw s) jolt-nil) (jolt-nth x s)))
             ir steps))
(define (fact-ir src) (jolt-ce-analyze (make-analyze-ctx "u") (jolt-ce-read src)))
(define (facts-for src) (body-facts facts-unit (fact-ir src)))
(define (binds? facts use binder)
  (clj= (binding-at facts (ir-path use)) (ir-path binder)))
(define (origin? facts use kind . path)
  (let* ((base (jolt-hash-map (fact-kw 'kind) (fact-kw kind)))
         (entry (if (eq? kind 'unknown)
                    base
                    (jolt-assoc base (fact-kw (if (eq? kind 'argument) 'binding 'site))
                                (ir-path path)))))
    (clj= (origins-at facts (ir-path use)) (jolt-hash-set entry))))

(let ((f (facts-for "(fn* ([^double x ^long y z] (let* [a x b y c z] [a b c])))")))
  (ok "numeric entry coercions give aliases an unknown origin"
      (and (origin? f '(arities 0 body body items 0) 'unknown)
           (origin? f '(arities 0 body body items 1) 'unknown)))
  (ok "the adjacent uncoerced alias still has its incoming origin"
      (origin? f '(arities 0 body body items 2) 'argument 'arities 0 'params 2)))

(let ((f (facts-for "(fn* ([x] (let* [outer x x [x] alias x alias2 alias] [outer x alias2])))")))
  (ok "a shadowing let initializer sees the outer parameter"
      (binds? f '(arities 0 body bindings 1 1 items 0) '(arities 0 params 0)))
  (ok "after shadowing, the same name resolves to the new binding"
      (binds? f '(arities 0 body body items 1) '(arities 0 body bindings 1 0)))
  (ok "a direct alias initializer resolves its original binding"
      (binds? f '(arities 0 body bindings 2 1) '(arities 0 body bindings 1 0)))
  (ok "the use still resolves to the last alias's own lexical binder"
      (binds? f '(arities 0 body body items 2) '(arities 0 body bindings 3 0)))
  (ok "an alias of a parameter has the caller-supplied origin"
      (origin? f '(arities 0 body body items 0) 'argument 'arities 0 'params 0))
  (ok "an alias chain retains the original execution-time constructor"
      (origin? f '(arities 0 body body items 2) 'allocation 'arities 0 'body 'bindings 1 1)))

(let* ((outer (fact-ir "(let* [x 1] (let* [x x] x))"))
       (inner (ir-at outer '(body)))
       (use (ir-at inner '(bindings 0 1)))
       (shared (jolt-assoc outer (fact-kw 'body) (jolt-assoc inner (fact-kw 'body) use)))
       (f (body-facts facts-unit shared)))
  (ok "one shared local node resolves independently in two lexical scopes"
      (and (eq? use (ir-at shared '(body body)))
           (binds? f '(body bindings 0 1) '(bindings 0 0))
           (binds? f '(body body) '(body bindings 0 0)))))

(let ((f (facts-for "(letfn [(a [] [a b]) (b [] a)] [a b])")))
  (ok "letfn binds a later sibling in an earlier closure"
      (binds? f '(bindings 0 1 arities 0 body items 1) '(bindings 1 0)))
  (ok "letfn binds an earlier sibling in a later closure"
      (binds? f '(bindings 1 1 arities 0 body) '(bindings 0 0)))
  (ok "the function self name shadows its letfn binder inside the function"
      (binds? f '(bindings 0 1 arities 0 body items 0) '(bindings 0 1 name)))
  (ok "the earlier closure sees the later sibling's constructor origin"
      (origin? f '(bindings 0 1 arities 0 body items 1) 'allocation 'bindings 1 1)))

(let ((f (facts-for "(fn* self ([self] self) ([x y & more] [self x y more]))")))
  (ok "a parameter can shadow the fn self name in just its own arity"
      (binds? f '(arities 0 body) '(arities 0 params 0)))
  (ok "the fn self binder is visible in the second arity"
      (binds? f '(arities 1 body items 0) '(name)))
  (ok "rest has its own formal and an incoming origin"
      (and (binds? f '(arities 1 body items 3) '(arities 1 rest))
           (origin? f '(arities 1 body items 3) 'argument 'arities 1 'rest))))

(let ((f (facts-for "(let* [e [:outer]] (try e (catch Throwable e e) (finally e)))")))
  (ok "the source catch binding receives the exception from the IR catch binder"
      (binds? f '(body catch-body bindings 0 1) '(body catch-sym)))
  (ok "the catch name shadows the outer local only in the handler"
      (and (binds? f '(body catch-body body) '(body catch-body bindings 0 0))
           (binds? f '(body body) '(bindings 0 0))
           (binds? f '(body finally) '(bindings 0 0))
           (origin? f '(body catch-body body) 'unknown))))

(let* ((ir (fact-ir "(fn* ([x] (let* [a [x] b [x] c [[1] :k] q '(x) r (identity x)] [a b c q r])))"))
       (b '(arities 0 body))
       (let-ir (ir-at ir b))
       (alloc (ir-at let-ir '(bindings 0 1)))
       ;; The two constructor occurrences now use the same map object.
       (bindings (ir-at let-ir '(bindings)))
       (shared (jolt-assoc let-ir (fact-kw 'bindings)
                 (jolt-assoc bindings 1 (jolt-vector "b" alloc))))
       (original (body-facts facts-unit ir))
       (f (body-facts facts-unit shared)))
  (ok "equal constructor syntax yields two distinct origin paths"
      (and (origin? original '(arities 0 body body items 0) 'allocation 'arities 0 'body 'bindings 0 1)
           (origin? original '(arities 0 body body items 1) 'allocation 'arities 0 'body 'bindings 1 1)))
  (ok "even the same constructor node object has distinct occurrences"
      (and (eq? (ir-at shared '(bindings 0 1)) (ir-at shared '(bindings 1 1)))
           (origin? f '(body items 0) 'allocation 'bindings 0 1)
           (origin? f '(body items 1) 'allocation 'bindings 1 1)))
  (ok "constant collection and quoted data have potentially shared literal origins"
      (and (origin? f '(body items 2) 'literal 'bindings 2 1)
           (origin? f '(body items 3) 'literal 'bindings 3 1)))
  (ok "an opaque invoke and a binding outside the requested root have unknown origins"
      (and (origin? f '(body items 4) 'unknown)
           (origin? f '(bindings 0 1 items 0) 'unknown)
           (jolt-nil? (binding-at f (ir-path '(bindings 0 1 items 0)))))))

(let* ((src "(fn* ([x] (loop* [i 0 prev nil] (let* [obj [x] alias obj] (if (= i 0) (recur 1 alias) [(= alias prev) (identical? alias prev)])))))")
       (f (facts-for src)))
  (ok "a loop body's alias carries its constructor site onto the backedge"
      (origin? f '(arities 0 body body body then args 1) 'allocation 'arities 0 'body 'body 'bindings 0 1))
  (ok "the outer fn parameter remains incoming across an inner loop's recur"
      (origin? f '(arities 0 body body bindings 0 1 items 0) 'argument 'arities 0 'params 0))
  (ok "a loop-carried local might have been rebound by recur"
      (and (binds? f '(arities 0 body body body else items 0 args 1) '(arities 0 body bindings 1 0))
           (origin? f '(arities 0 body body body else items 0 args 1) 'unknown)))
  (ok "two evaluations of one loop site produce equal objects with distinct identities"
      (equal? (seq->list (ev (string-append "(" src " 7)"))) '(#t #f))))
(let ((f (facts-for "(fn* ([x] (let* [alias x] (if x alias (recur [x])))))")))
  (ok "aliases of a fn parameter can see a value rebound by fn recur"
      (origin? f '(arities 0 body body then) 'unknown)))
(let ((f (facts-for "(fn* ([x] (loop* [x [x] y x] y)))")))
  (ok "loop initializers have sequential scope and see first-entry values"
      (and (binds? f '(arities 0 body bindings 0 1 items 0) '(arities 0 params 0))
           (origin? f '(arities 0 body bindings 1 1) 'allocation 'arities 0 'body 'bindings 0 1))))
(let ((f (facts-for "(fn* ([x] {x x}))")))
  (ok "a map key and value are separate child occurrences"
      (and (binds? f '(arities 0 body pairs 0 0) '(arities 0 params 0))
           (binds? f '(arities 0 body pairs 0 1) '(arities 0 params 0)))))

(ev "(def facts-copy (fn* ([p] [p p])))")
(let* ((ctx (make-analyze-ctx "u"))
       (before (jolt-ce-analyze ctx (jolt-ce-read "(fn* ([x] (let* [a [x]] (facts-copy a))))")))
       (original (body-facts facts-unit before))
       (after (jolt-ce-run-passes before ctx))
       (rewritten (body-facts facts-unit after))
       (other (body-facts ((var-deref "jolt.passes.types" "new-unit")) after)))
  (ok "the same body revision hits the unit's fact cache"
      (eq? rewritten (body-facts facts-unit after)))
  (ok "different compilation units compute separate snapshots" (not (eq? rewritten other)))
  (ok "the pass pipeline splices the call"
      (not (has? (code-part (jolt-ce-emit after)) "facts-copy")))
  (ok "recomputing after a splice finds both substituted uses in caller scope"
      (and (origin? original '(arities 0 body body) 'unknown)
           (binds? original '(arities 0 body body args 0) '(arities 0 body bindings 0 0))
           (binds? rewritten '(arities 0 body body items 0) '(arities 0 body bindings 0 0))
           (binds? rewritten '(arities 0 body body items 1) '(arities 0 body bindings 0 0))
           (origin? rewritten '(arities 0 body body items 1) 'allocation 'arities 0 'body 'bindings 0 1)))
  (let* ((opaque (jolt-hash-map (fact-kw 'op) (fact-kw 'future-op)))
         (mixed (jolt-hash-map (fact-kw 'op) (fact-kw 'vector)
                               (fact-kw 'items) (jolt-vector after opaque)))
         (f (body-facts facts-unit mixed)))
    (ok "an unrecognized IR op discards partial facts"
        (and (origin? f '(items 1) 'unknown)
             (origin? f '(items 0 arities 0 body body items 1) 'unknown)))))

(let* ((leaf (fact-ir "1"))
       (deep (let loop ((n 200) (ir leaf))
               (if (zero? n) ir
                   (loop (- n 1) (jolt-hash-map (fact-kw 'op) (fact-kw 'vector)
                                                (fact-kw 'items) (jolt-vector ir))))))
       (node (jolt-hash-map (fact-kw 'op) (fact-kw 'vector)
                            (fact-kw 'items) (jolt-vector leaf deep))))
  (ok "an exhausted analysis budget discards even the prefix's facts"
      (origin? (body-facts facts-unit node) '(items 0) 'unknown)))

(define projection-at (var-deref "jolt.passes.facts" "projection-at"))
(define guarded-record-projection-at (var-deref "jolt.passes.facts" "guarded-record-projection-at"))
(define contained-at (var-deref "jolt.passes.facts" "contained-at"))
(define captures-at (var-deref "jolt.passes.facts" "captures-at"))
(define confined? (var-deref "jolt.passes.facts" "confined?"))
(define may-publish? (var-deref "jolt.passes.facts" "may-publish?"))
(define observations-at (var-deref "jolt.passes.facts" "observations-at"))
(define (in-body . path) (ir-path (append '(arities 0 body) path)))
(define map-site (in-body 'bindings 0 1))
(define box-site (in-body 'bindings 1 1))
(define ret-site (in-body 'body))
(define (fact-get facts key) (jolt-get facts (fact-kw key) jolt-nil))
(define (projects? f path present? from)
  (let ((p (projection-at f path)))
    (and (not (jolt-nil? p))
         (eq? (fact-get p 'present?) present?)
         (clj= (fact-get p 'origins) (origins-at f from)))))

(let ((f (facts-for "(fn [x] (let [m {:k x} box [m]] (:k (nth box 0))))")))
  (ok "nested projections forward only the selected field"
      (and (projects? f (in-body 'body 'args 0) #t map-site)
           (projects? f ret-site #t (in-body 'bindings 0 1 'pairs 0 1))))
  (ok "projecting through both containers confines both allocations"
      (and (confined? f map-site) (confined? f box-site)
           (may-publish? f (in-body 'bindings 0 1 'pairs 0 1))
           (clj= (contained-at f box-site) (origins-at f map-site)))))

(for-each
 (lambda (tail)
   (let ((f (facts-for (string-append "(fn [x sink] (let [m {:k x} box [[m] [m]] other [x]] " tail "))"))))
     (ok (string-append "shared child publishes transitively via " tail)
         (and (not (confined? f map-site)) (not (confined? f box-site))
              (confined? f (in-body 'bindings 2 1))))))
 '("box" "(reset! sink box)" "(set! *print-length* box)" "(throw box)"
   "(try (throw box) (catch Throwable e nil))"))

(let ((f (facts-for "(fn [x callback] (let [field [x] m {:k field} box [m]] (callback (:k (nth box 0)))))")))
  (ok "passing only a projected child to an opaque callback confines its holders"
      (and (not (confined? f (in-body 'bindings 0 1)))
           (confined? f (in-body 'bindings 1 1))
           (confined? f (in-body 'bindings 2 1)))))

(let ((f (facts-for "(fn [x i] (let [m {:k x} box [m] other [x]] (nth box i)))")))
  (ok "unknown indexing conservatively exposes possible children and leaves unrelated sites local"
      (and (jolt-nil? (projection-at f ret-site))
           (not (confined? f map-site)) (not (confined? f box-site))
           (confined? f (in-body 'bindings 2 1)))))

(let ((f (facts-for "(fn [x] (let [m {:k x} box [m]] (identical? m (nth box 0))))")))
  (ok "local identity is observable even while both objects are confined"
      (and (confined? f map-site) (confined? f box-site)
           (clj= (observations-at f map-site) (jolt-hash-set (fact-kw 'identity)))
           (clj= (observations-at f box-site) (jolt-hash-set)))))

(let ((f (facts-for "(fn [x] (let [m {:k x}] (class m)))")))
  (ok "representation and opaque uses are reported separately"
      (and (not (confined? f map-site))
           (clj= (observations-at f map-site)
                 (jolt-hash-set (fact-kw 'representation) (fact-kw 'unknown))))))

(let ((f (facts-for "(fn [x] (let [m {:a nil :b false :c (x)} v [nil false (x)]] [(get m :a x) (:b m x) (:c m) (nth v 0 x) (nth v 1) (nth v 2) (:z m x)]))")))
  (ok "nil, false, and unknown values preserve literal key and slot presence"
      (and (projects? f (in-body 'body 'items 0) #t (in-body 'bindings 0 1 'pairs 0 1))
           (projects? f (in-body 'body 'items 1) #t (in-body 'bindings 0 1 'pairs 1 1))
           (projects? f (in-body 'body 'items 2) #t (in-body 'bindings 0 1 'pairs 2 1))
           (projects? f (in-body 'body 'items 3) #t (in-body 'bindings 1 1 'items 0))
           (projects? f (in-body 'body 'items 4) #t (in-body 'bindings 1 1 'items 1))
           (projects? f (in-body 'body 'items 5) #t (in-body 'bindings 1 1 'items 2))
           (projects? f (in-body 'body 'items 6) #f (in-body 'body 'items 6 'args 1)))))

(let ((f (facts-for "(fn [x] (let [key [x] val [x] m {key val} members #{key val}] [m members]))")))
  (ok "maps retain both keys and values and sets retain members"
      (let ((children (jolt-conj2 (origins-at f map-site)
                                  (jolt-first (origins-at f box-site)))))
        (and (clj= (contained-at f (in-body 'bindings 2 1)) children)
             (clj= (contained-at f (in-body 'bindings 3 1)) children)))))
(let ((f (facts-for "(fn [] (get #{false} false :missing))")))
  (ok "set lookup forwards the stored member"
      (projects? f (in-body) #t (in-body 'args 0 'items 0))))

(let* ((f (facts-for "(fn [x] (let [m {:k x} f (fn [m] (fn [] m)) g (fn [] (fn [] m))] g))"))
       (g (captures-at f (in-body 'bindings 2 1)))
       (capture (jolt-get g "m" jolt-nil)))
  (ok "nested capture follows lexical shadowing and returning a closure publishes its capture"
      (and (clj= (captures-at f box-site) (jolt-hash-map))
           (clj= (fact-get capture 'binding) (in-body 'bindings 0 0))
           (clj= (fact-get capture 'origins) (origins-at f map-site))
           (not (confined? f map-site)))))

(ev "(def facts-make-fn (fn* ([p q] (fn* ([] [p q])))))")
(let* ((ctx (make-analyze-ctx "u"))
       (ir (jolt-ce-analyze ctx (jolt-ce-read "(fn [x] (let [m {:k x}] (facts-make-fn m false)))")))
       (spliced (jolt-ce-run-passes ir ctx))
       (closure (ir-at spliced '(arities 0 body body)))
       (captures (captures-at (body-facts facts-unit spliced) ret-site)))
  (ok "spliced image metadata retains a renamed live binding and carries false as data"
      (and (clj= (fact-get closure 'free-names) (jolt-vector "p" "q"))
           (clj= (fact-get closure 'live-names) (jolt-vector "m" (jolt-vector #f)))
           (clj= (fact-get (jolt-get captures "m" jolt-nil) 'binding) (in-body 'bindings 0 0))
           (= 1 (jolt-count captures)))))

(let ((f (facts-for "(fn [x] (let [m {:k x} f (fn [] m) a f] (:k (a))))")))
  (ok "a proven local zero-arg invocation forwards its capture without publication"
      (and (confined? f map-site) (confined? f box-site)
           (projects? f ret-site #t (in-body 'bindings 0 1 'pairs 0 1)))))
(let ((f (facts-for "(fn [x] (let [m {:k x}] ((fn [] (throw m)))))")))
  (ok "an immediately invoked closure can still publish a capture through its effects"
      (not (confined? f map-site))))

(let* ((f (facts-for "(fn [x] (let [field [x] m {:k field} f (^:once fn* [] (let [box [m]] (:k (nth box 0) :missing)))] (f)))"))
       (once-site (in-body 'bindings 2 1))
       (capture (jolt-get (captures-at f once-site) "m" jolt-nil))
       (read-site (in-body 'bindings 2 1 'arities 0 'body 'bindings 0 1 'items 0))
       (lookup-site (in-body 'bindings 2 1 'arities 0 'body 'body))
       (receiver-site (in-body 'bindings 2 1 'arities 0 'body 'body 'args 0)))
  (ok "a once closure keeps the lexical binder and possible first-call value"
      (and (clj= (fact-get capture 'binding) (in-body 'bindings 1 0))
           (clj= (binding-at f read-site) (in-body 'bindings 1 0))
           (clj= (fact-get capture 'origins) (origins-at f box-site))
           (jolt-contains? (origins-at f read-site) (jolt-hash-map (fact-kw 'kind) (fact-kw 'unknown)))
           (jolt-contains? (origins-at f read-site) (jolt-first (origins-at f box-site)))))
  (ok "a cleared capture invalidates derived field proofs and may still publish its child"
      (and (projects? f receiver-site #t read-site)
           (jolt-nil? (projection-at f lookup-site))
           (origin? f '(arities 0 body body) 'unknown)
           (may-publish? f map-site))))

(ok "a once closure retried after throwing reads the cleared capture"
    (clj= (ev "(let [m {:k [1]} f (^:once fn* [fail?] (let [v (:k m :missing)] (if fail? (throw (ex-info \"retry\" {})) v)))] [(try (f true) (catch Throwable e :threw)) (f false)])")
          (jolt-vector (fact-kw 'threw) (fact-kw 'missing))))

(for-each
  (lambda (hint)
    (let ((f (facts-for (string-append "(fn [x] ((fn* ^" hint " [] x)))"))))
      (ok (string-append "^" hint " closure call discards the pre-coercion origin")
          (and (origin? f '(arities 0 body fn arities 0 body) 'argument 'arities 0 'params 0)
               (origin? f '(arities 0 body) 'unknown)))))
  '("double" "long"))
(let ((d (ev "((fn [x] ((fn* ^double [] x))) 1)"))
      (n (ev "((fn [x] ((fn* ^long [] x))) 18446744073709551616N)")))
  (ok "fixed-arity return coercion widens to double and wraps long"
      (and (flonum? d) (= d 1.0) (fixnum? n) (= n 0))))

(define (record-layout-matches? r candidate)
  (let* ((guard (fact-get candidate 'guard))
         (arity (fact-get guard 'arity)))
    (and (jrec? r)
         (= arity (jrec-nfields r) (jrdesc-nfields (jrec-desc r)))
         (eqv? (fact-get guard 'index) (jrec-get-index r (fact-get guard 'key))))))

(ev "(defrecord FactRecord [a b])")
(let* ((src "(fn [a b] (get (FactRecord. a b) :a :missing))")
       (ir (fact-ir src))
       (lookup-site (in-body))
       (first-arg (in-body 'args 0 'args 0))
       (before (body-facts facts-unit ir))
       (lookup (ev src))
       (ctx (make-analyze-ctx "u"))
       (ctor-code (jolt-direct-link-emit
                   (jolt-ce-analyze ctx (jolt-ce-read "(fn [a b] (FactRecord. a b))")) ctx))
       (ctor-form (read (open-input-string ctor-code)))
       (left (fact-kw 'left))
       (right (fact-kw 'right)))
  (ok "the direct-link path emits the positional constructor"
      (has? (code-part ctor-code) "(make-jrec2 "))
  ((var-deref "jolt.passes.types" "set-record-shapes!") facts-unit (chez-record-shapes-map))
  (let* ((f (body-facts facts-unit ir))
         (candidate (guarded-record-projection-at f lookup-site)))
    (ok "a positional record retains both field arguments"
        (clj= (contained-at f (in-body 'args 0))
              (jolt-hash-set (jolt-first (origins-at f first-arg))
                             (jolt-first (origins-at f (in-body 'args 0 'args 1))))))
    (ok "an updated registry supplies a guarded candidate for the same IR revision"
        (and (jolt-nil? (guarded-record-projection-at before lookup-site))
             (not (eq? before f))
             (jolt-nil? (projection-at f lookup-site))
             (origin? f '(arities 0 body) 'unknown)
             (clj= (fact-get candidate 'origins) (origins-at f first-arg))))
    (ok "a fresh record with the matching layout reads the proposed argument"
        (and (record-layout-matches? ((eval ctor-form (interaction-environment)) left right) candidate)
             (eq? (lookup left right) left)))
    (ev "(defrecord FactRecord [b a])")
    (ok "reordering fields defeats the guard and the previously compiled lookup reads the new slot"
        (let ((r ((eval ctor-form (interaction-environment)) left right)))
          (and (not (record-layout-matches? r candidate))
               (eq? (jolt-get r (fact-kw 'a) (fact-kw 'missing)) right)
               (eq? (lookup left right) right))))))
(let ((f (facts-for "(fn [x sink] (let [m {:k x} r (FactRecord. m false)] (sink r) (:a r)))")))
  (ok "record fields exposed to mutation have no guarded candidate or unconditional proof"
      (and (jolt-nil? (projection-at f (in-body 'body 'ret)))
           (jolt-nil? (guarded-record-projection-at f (in-body 'body 'ret)))
           (not (confined? f map-site)) (not (confined? f box-site)))))

(ev "(defrecord Collision.Record [a])")
(jolt-compile-eval "(defrecord Record [^double a])" "u.Collision")
(let ((shapes (chez-record-shapes-map)))
  ((var-deref "jolt.passes.types" "set-record-shapes!") facts-unit shapes)
  (let* ((ir (fact-ir "(fn [x] (get (u.Collision.Record. x) :a :missing))"))
         (f (body-facts facts-unit ir)))
    (ok "distinct constructor entries sharing a class tag have no field candidate"
        (and (clj= (fact-get (jolt-get shapes "u/->Collision.Record") 'type) "u.Collision.Record")
             (clj= (fact-get (jolt-get shapes "u.Collision/->Record") 'type) "u.Collision.Record")
             (clj= (ir-at ir '(arities 0 body args 0 class)) "u.Collision.Record")
             (jolt-nil? (guarded-record-projection-at f (in-body)))
             (origin? f '(arities 0 body args 0) 'unknown)))))

(set-optimize! #f)
(set-direct-link-flag! #f)
(printf "~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
