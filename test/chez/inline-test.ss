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

(let ((f (facts-for "(fn [x] (loop* [m {:k x} saved m] (identical? m saved)))")))
  (ok "identity observed through a loop slot reaches its confined initializer"
      (and (confined? f map-site)
           (clj= (observations-at f map-site) (jolt-hash-set (fact-kw 'identity))))))
(let ((f (facts-for "(fn [x] (loop* [m {:k x} saved m] (class saved)))")))
  (ok "class through a loop slot retains its representation and opaque observations"
      (clj= (observations-at f map-site)
            (jolt-hash-set (fact-kw 'representation) (fact-kw 'unknown)))))

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
    (let* ((f (facts-for (string-append "(fn [x] ((fn* ^" hint " [] x)))")))
           (read (in-body 'fn 'arities 0 'body))
           (returns (origins-at f (in-body))))
      (ok (string-append "^" hint " closure return may preserve or replace the captured argument")
          (and (origin? f '(arities 0 body fn arities 0 body) 'argument 'arities 0 'params 0)
               (jolt-contains? returns (jolt-first (origins-at f read)))
               (jolt-contains? returns (jolt-hash-map (fact-kw 'kind) (fact-kw 'unknown)))
               (may-publish? f read)))))
  '("double" "long"))
(let* ((source "(fn [v] (let [m {:k v}] (try ((fn* ^double [] m)) (catch Throwable e (ex-message e)))))")
       (f (facts-for source)))
  (ok "a caught return coercion can report the map's runtime representation"
      (and (jolt-contains? (observations-at f map-site) (fact-kw 'representation))
           (has? (ev (string-append "(" source " 1)")) "PersistentArrayMap"))))
(let* ((body (fact-ir "(fn [x] (let [m {:k x}] (try ((fn* ^double [] m)) nil (catch Throwable e e))))"))
       (unit ((var-deref "jolt.passes.types" "new-unit")))
       (plain (body-facts unit body)))
  ((var-deref "clojure.core" "reset!") (fact-get unit 'debug-locals?) #t)
  (let ((debug (body-facts unit body)))
    (ok "a returned debug exception may retain the map even if the normal cast result is discarded"
        (and (confined? plain map-site) (may-publish? debug map-site)
             (not (confined? debug map-site))))))
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

(define loop-update-at (var-deref "jolt.passes.facts" "loop-update-at"))
(define loop-facts-unit ((var-deref "jolt.passes.types" "new-unit")))
(define set-fact-flag! (var-deref "clojure.core" "reset!"))
(set-fact-flag! (fact-get loop-facts-unit 'direct-link?) #t)
(define (loop-facts src) (body-facts loop-facts-unit (fact-ir src)))
(define direct-edit (in-body 'body 'then 'args 1))
(define (update-fact f path k) (fact-get (loop-update-at f path) k))

(define direct-loop
  "(fn [x] (loop* [i 0 m {:k x}] (if (< i 3) (recur (inc i) (assoc m :k (inc (:k m)))) m)))")
(let* ((ctx (make-analyze-ctx "u"))
       (ir (jolt-ce-run-passes (jolt-ce-analyze ctx (jolt-ce-read direct-loop)) ctx))
       (f (body-facts loop-facts-unit ir))
       (update (loop-update-at f direct-edit)))
  (ok "a linked fixed-key update can reuse storage even when the final map escapes"
      (and (eq? #t (fact-get update 'reuse?)) (eq? #f (fact-get update 'confined?))
           (eq? #f (fact-get update 'old-retained?))
           (clj= (fact-get update 'binding) (in-body 'bindings 1 0))
           (clj= (fact-get update 'recur) (in-body 'body 'then))
           (has? (code-part (jolt-ce-emit ir)) "jolt-assoc3")
           (clj= (ev (string-append "(" direct-loop " 5)")) (ev "{:k 8}"))))
  (set-fact-flag! (fact-get loop-facts-unit 'direct-link?) #f)
  (ok "changing link mode invalidates the loop proof for the same IR"
      (jolt-nil? (loop-update-at (body-facts loop-facts-unit ir) direct-edit)))
  (set-fact-flag! (fact-get loop-facts-unit 'direct-link?) #t)
  (set-fact-flag! (fact-get loop-facts-unit 'debug-locals?) #t)
  (ok "debug frames can retain old versions across an exceptional transfer"
      (jolt-nil? (loop-update-at (body-facts loop-facts-unit ir) direct-edit)))
  (set-fact-flag! (fact-get loop-facts-unit 'debug-locals?) #f))

(let* ((src "(fn [x] (loop* [i 0 m {:k x} seen nil] (if (< i 3) (let* [alias m] (recur (inc i) (assoc alias :k (inc (:k alias))) (:k alias))) [(:k m) seen])))")
       (update (loop-update-at (loop-facts src) (in-body 'body 'then 'body 'args 1))))
  (ok "a dead alias and a later recur argument's scalar read allow reuse at transfer"
      (and (eq? #t (fact-get update 'confined?)) (eq? #t (fact-get update 'reuse?))
           (clj= (ev (string-append "(" src " 5)")) (ev "[8 7]")))))

(for-each
  (lambda (snapshot)
    (let* ((src (string-append
                  "(fn [x refresh?] (loop* [i 0 m {:k x} prev nil] (if (< i 3) "
                  "(recur (inc i) (assoc m :k (inc (:k m))) " snapshot ") [(:k m) (:k prev)])))"))
           (update (loop-update-at (loop-facts src) direct-edit)))
      (ok (string-append "old snapshot keeps a distinct local version via " snapshot)
          (and (eq? #t (fact-get update 'confined?))
               (eq? #t (fact-get update 'old-retained?))
               (eq? #f (fact-get update 'reuse?))
               (clj= (ev (string-append "(" src " 5 true)")) (ev "[8 7]"))
               (clj= (ev (string-append "(" src " 5 false)"))
                     (ev (if (string=? snapshot "m") "[8 7]" "[8 nil]")))))))
  '("m" "(if refresh? m prev)"))

(let* ((src "(fn [x] (loop* [i 0 m {:k x :old nil}] (if (< i 2) (if (= i 0) (recur 1 (assoc m :old [m])) (recur 2 (assoc m :k (inc (:k m))))) (:k m))))")
       (f (loop-facts src)))
  (ok "the next map can retain its predecessor directly or through an unchanged field"
      (and (eq? #t (update-fact f (in-body 'body 'then 'then 'args 1) 'old-retained?))
           (eq? #t (update-fact f (in-body 'body 'then 'else 'args 1) 'old-retained?))
           (eq? #f (update-fact f (in-body 'body 'then 'else 'args 1) 'reuse?))
           (clj= (ev (string-append "(" src " 5)")) 6))))

(for-each
  (lambda (snapshot)
    (let ((f (loop-facts
               (string-append
                 "(fn [x] (loop* [i 0 m {:k x} prev nil] (if (< i 3) "
                 "(recur (inc i) (assoc m :k (inc (:k m))) " snapshot ") (:k m))))"))))
      (ok (string-append "a backedge can retain a version via " snapshot)
          (and (eq? #t (update-fact f direct-edit 'confined?))
               (eq? #t (update-fact f direct-edit 'old-retained?))
               (eq? #f (update-fact f direct-edit 'reuse?))))))
  '("[m]" "(fn [] m)" "(loop* [j 0 v nil] (if (= j 0) (recur 1 [m]) v))"))

(let* ((src "(fn [x] (loop* [i 0 m {:k x} read-prev nil] (if (< i 3) (recur (inc i) (assoc m :k (inc (:k m))) (fn [] (:k m))) [(:k m) (read-prev)])))")
       (f (loop-facts src)))
  (ok "a closure used after the transfer reads its captured older version"
      (and (eq? #t (update-fact f direct-edit 'old-retained?))
           (eq? #f (update-fact f direct-edit 'reuse?))
           (clj= (ev (string-append "(" src " 5)")) (ev "[8 7]")))))

(let* ((src "(fn [x] (loop* [i 0 m {:k x} prev nil] (if (< i 3) (recur (inc i) (assoc m :k (inc (:k m))) m) [(identical? m prev) (:k m) (:k prev)])))")
       (f (loop-facts src)))
  (ok "the same assoc occurrence makes distinct dynamic versions"
      (and (eq? #t (update-fact f direct-edit 'old-retained?))
           (eq? #f (update-fact f direct-edit 'reuse?))
           (clj= (ev (string-append "(" src " 5)")) (ev "[false 8 7]")))))

(let* ((src "(fn [x] (loop* [i 0 m {:k x} prev nil] (if (< i 3) (recur (inc i) (assoc m :k (inc (:k m))) (assoc m :k (:k m))) [(:k m) (:k prev)])))")
       (f (loop-facts src)))
  (ok "a sibling assoc occurrence can retain the old map by returning it unchanged"
      (and (eq? #t (update-fact f direct-edit 'old-retained?))
           (eq? #f (update-fact f direct-edit 'reuse?))
           (clj= (ev (string-append "(" src " 5)")) (ev "[8 7]")))))

(for-each
  (lambda (observer)
    (let* ((f (loop-facts
                (string-append
                  "(fn [x sink] (loop* [i 0 m {:k x}] (if (< i 3) (do " observer
                  " (recur (inc i) (assoc m :k (inc (:k m))))) (:k m))))")))
           (update (loop-update-at f (in-body 'body 'then 'ret 'args 1))))
      (ok (string-append "reuse declines independently of retained slots for " observer)
          (and (eq? #f (fact-get update 'old-retained?))
               (eq? #f (fact-get update 'reuse?))))))
  '("(identical? m m)" "(class m)" "(meta m)" "(sink m)"
    "(try (throw (ex-info \"old\" {:map m})) (catch Throwable e nil))"))

(let* ((src "(fn [x events] (try (loop* [m {:k x} i 0] (if (= i 0) (recur (assoc m :k (do (swap! events conj :field) (inc (:k m)))) (do (swap! events conj (:k m)) (throw (ex-info \"stop\" {})))) (:k m))) (catch Throwable e @events)))")
       (f (loop-facts src)))
  (ok "a later recur argument can throw after reading the old value"
      (and (eq? #t (update-fact f (in-body 'body 'body 'then 'args 0) 'reuse?))
           (clj= (ev (string-append "(" src " 5 (atom []))")) (ev "[:field 5]")))))

(for-each
  (lambda (src)
    (ok (string-append "no loop update proof for " src)
        (jolt-nil? (loop-update-at (loop-facts src) direct-edit))))
  '("(fn [x] (loop* [i 0 m {:k 0}] (if (< i 3) (recur (inc i) (assoc m :k x)) m)))"
    "(fn [x] (loop* [i 0 m x] (if (< i 3) (recur (inc i) (assoc m :k x)) m)))"
    "(fn [x] (loop* [i 0 m {:k x}] (if (< i 3) (recur (inc i) (assoc m :extra x)) m)))"
    "(fn [x assoc] (loop* [i 0 m {:k x}] (if (< i 3) (recur (inc i) (assoc m :k x)) m)))"))

(let ((f (loop-facts "(fn [x] (let* [other {:k x}] (loop* [i 0 m {:k x}] (if (< i 3) (if (= i 0) (recur 1 other) (recur (inc i) (assoc m :k x))) m))))")))
  (ok "a backedge replacing the accumulator with a different map of the same layout declines"
      (jolt-nil? (loop-update-at f (in-body 'body 'body 'then 'else 'args 1)))))

;; A single chain of nested loops grows the IR linearly. Each level must still
;; prove reuse, including after shadowing the enclosing level's names.
(define (nested-loops depth)
  (if (= depth 0)
      "nil"
      (string-append "(loop* [i 0 m {:k x}] (if (< i 1) (do "
                     (nested-loops (- depth 1))
                     " (recur (inc i) (assoc m :k (inc (:k m))))) (:k m)))")))
(define (measure-loop-facts depth)
  (let* ((ir (fact-ir (string-append "(fn [x] " (nested-loops depth) ")")))
         (cache (fact-get loop-facts-unit 'body-facts))
         (count-updates (var-deref "clojure.core" "count")))
    (define (analyze)
      (set-fact-flag! cache jolt-nil)
      (body-facts loop-facts-unit ir))
    (let* ((warm (analyze))
           (updates (fact-get warm 'loop-updates))
           (outer (in-body 'body 'then 'ret 'args 1)))
      (ok (format "each of ~a nested loops proves reuse for its own slot" depth)
          (and (pmap? updates) (= (count-updates updates) depth)
               (eq? #t (update-fact warm outer 'reuse?))
               (pmap-fold-fwd updates
                 (lambda (k u all?) (and all? (eq? #t (fact-get u 'reuse?)))) #t))))
    (let ((allocated (sstats-bytes (statistics))) (before (cpu-time)) (samples 24))
      (do ((i 0 (+ i 1))) ((= i samples)) (analyze))
      (let ((bytes (/ (- (sstats-bytes (statistics)) allocated) samples))
            (msec (/ (- (cpu-time) before) samples)))
        (printf "loop facts depth ~a: ~,2f ms, ~,0f bytes per uncached query~n"
                depth (inexact msec) (inexact bytes))
        bytes))))
(let* ((small (measure-loop-facts 4))
       (large (measure-loop-facts 16)))
  (ok "uncached fact allocation stays bounded on four times the nesting"
      (< 0 small large (* 8 small))))

(define (shifted-snapshot slots)
  (let* ((names (map (lambda (i) (format "s~a" i)) (iota slots)))
         (inits (apply string-append (map (lambda (n) (string-append n " nil ")) names)))
         (shift (apply string-append (map (lambda (n) (string-append n " ")) (cdr names)))))
    (string-append "(fn [x] (loop* [i 0 m {:k x} " inits "] "
                   "(if (< i 50) (recur (inc i) (assoc m :k x) " shift "m) (:k s0))))")))
(ok "slot joins converge on a short snapshot chain and withhold facts beyond the budget"
    (and (eq? #t (update-fact (loop-facts (shifted-snapshot 4)) direct-edit 'old-retained?))
         (jolt-nil? (loop-update-at (loop-facts (shifted-snapshot 20)) direct-edit))))

(define call-summary-at (var-deref "jolt.passes.facts" "call-summary-at"))
(define (argument-effects summary i)
  (jolt-nth (fact-get summary 'arguments) i))
(define (effects? summary i . effects)
  (clj= (argument-effects summary i) (apply jolt-hash-set (map fact-kw effects))))

(let* ((source "(fn [x] (let [m {:k x} read-k (fn [a] (:k a))] (read-k m)))")
       (f (facts-for source)))
  (ok "a fixed-arity helper reads the caller's map and returns just its field"
      (and (confined? f map-site)
           (effects? (call-summary-at f ret-site) 0 'read)
           (origin? f '(arities 0 body body) 'argument 'arities 0 'params 0)
           (clj= (ev (string-append "(" source " 42)")) 42))))

;; The same request lookup shape that Ring uses for path-info with a URI fallback.
(let* ((source "(fn [path uri] (let [request {:path-info path :uri uri}] (letfn [(path-for [r] (or (:path-info r) (:uri r)))] (path-for request))))")
       (f (facts-for source))
       (call (in-body 'body 'body)))
  (ok "a closed request helper reads both fields without exposing the request"
      (and (confined? f map-site) (effects? (call-summary-at f call) 0 'read)
           (equal? (ev (string-append "(" source " nil \"/fallback\")")) "/fallback")
           (equal? (ev (string-append "(" source " \"/path\" \"/fallback\")")) "/path"))))

(let* ((f (facts-for "(fn [x] (let [m {:k x} pass (fn [a] a) r (pass m)] (:k r)))"))
       (call (in-body 'bindings 2 1)))
  (ok "returning an argument forwards its original alias"
      (and (confined? f map-site) (effects? (call-summary-at f call) 0 'return-alias)
           (clj= (origins-at f call) (origins-at f map-site)))))

(for-each
  (lambda (pack read-out)
    (let* ((prefix (string-append "(fn [x] (let [m {:k x} pack " pack " r (pack m)] "))
           (inside (facts-for (string-append prefix read-out "))")))
           (outside (facts-for (string-append prefix "r))")))
           (call (in-body 'bindings 2 1)))
      (ok (string-append "the result retains its argument until it escapes: " pack)
          (and (effects? (call-summary-at inside call) 0 'return-contains)
               (confined? inside map-site) (may-publish? outside map-site)))))
  '("(fn [a] [a])" "(fn [a] (fn [] a))")
  '("(:k (nth r 0))" "(:k (r))"))

(ev "(def call-summary-sink (atom nil))")
(let* ((prefix "(fn [x] (let [m {:k x} pack (fn [a] (fn [] (reset! call-summary-sink a))) run (pack m)] ")
       (discarded (facts-for (string-append prefix "nil))")))
       (returned (facts-for (string-append prefix "run))")))
       (called (facts-for (string-append prefix "(do (run) nil)))")))
       (call (in-body 'bindings 2 1)))
  (ok "a returned closure's publication stays latent until it is called or escapes"
      (and (effects? (call-summary-at discarded call) 0 'return-contains)
           (confined? discarded map-site)
           (may-publish? returned map-site) (may-publish? called map-site))))

(let* ((f (facts-for "(fn [x] (let [m {:k x} helper (fn ([a] (:k a)) ([a b] [a b]))] (do (helper m) (helper m x) nil)))"))
       (one (call-summary-at f (in-body 'body 'statements 0)))
       (two (call-summary-at f (in-body 'body 'statements 1))))
  (ok "one closure has independent fixed-arity summaries"
      (and (confined? f map-site)
           (= (fact-get one 'arity) 1) (effects? one 0 'read)
           (= (fact-get two 'arity) 2) (effects? two 0 'return-contains)
           (effects? two 1 'return-contains))))

(let* ((source "(fn [x] (let [m {:k x} same? (fn [a b] (identical? a b))] (same? m m)))")
       (f (facts-for source))
       (s (call-summary-at f ret-site)))
  (ok "identity observations cross the call without publishing either argument"
      (and (confined? f map-site) (effects? s 0 'identity) (effects? s 1 'identity)
           (clj= (observations-at f map-site) (jolt-hash-set (fact-kw 'identity)))
           (eq? #t (ev (string-append "(" source " 1)"))))))

(ev "(defprotocol CallSummaryProtocol (call-summary-step [receiver a]))")
(for-each
  (lambda (action)
    (let* ((f (facts-for (string-append "(fn [x cb] (let [m {:k x} helper (fn [a] "
                                       action ")] (do (helper m) nil)))")))
           (summary (call-summary-at f (in-body 'body 'statements 0))))
      (ok (string-append "an argument can escape during " action)
          (and (may-publish? f map-site)
               (jolt-contains? (argument-effects summary 0) (fact-kw 'publish))
               (jolt-contains? (argument-effects summary 0) (fact-kw 'unknown))))))
  '("(reset! call-summary-sink a)" "(.toString a)" "(cb a)" "(call-summary-step cb a)"
    "(throw a)" "(class a)" "((jolt.ffi/__cfn \"puts\" [:string] :int) a)"))

(let* ((f (facts-for "(fn [x cb] (let [field [x] m {:k field} helper (fn [a] (cb (:k a)))] (helper m)))"))
       (field (in-body 'bindings 0 1))
       (map (in-body 'bindings 1 1)))
  (ok "an opaque callback can publish a field while the helper's map stays confined"
      (and (confined? f map) (may-publish? f field)
           (effects? (call-summary-at f ret-site) 0 'read))))

(ev "(deftype CallSummaryLookup [value] clojure.lang.ILookup (valAt [this k] (reset! call-summary-sink this) value) (valAt [this k d] (reset! call-summary-sink this) value))")
(let* ((source "(fn [receiver] (let [lookup (fn [a] (:k a))] (lookup receiver)))")
       (f (facts-for source)))
  (ok "a literal-key helper cannot assume how a custom receiver looks up a key"
      (and (effects? (call-summary-at f ret-site) 0 'publish 'unknown)
           (eq? #t (ev (string-append "(let [custom (CallSummaryLookup. :found)] "
                                      "(and (= :found (" source " custom)) "
                                      "(identical? custom @call-summary-sink)))"))))))

(let* ((source "(fn [x] (let [m {:k x} fail (fn [a] (throw (ex-info \"stop\" {:value a})))] (try (fail m) (catch Throwable e (:value (ex-data e))))))")
       (f (facts-for source)))
  (ok "an argument can reach the caller through a thrown exception"
      (and (may-publish? f map-site)
           (clj= (ev (string-append "(" source " 7)")) (ev "{:k 7}")))))

(let ((f (facts-for "(fn [x] (let [m {:k x} inspect (fn [a] (class a))] (inspect m)))")))
  (ok "a representation observer is carried through its helper"
      (jolt-contains? (argument-effects (call-summary-at f ret-site) 0)
                      (fact-kw 'representation))))

(let* ((f (facts-for "(fn [x cb] (let [m {:k x}] (letfn [(apply1 [f a] (f a)) (read-k [a] (:k a))] [(apply1 read-k m) (apply1 cb m)])))"))
       (inside (in-body 'body 'body 'items 0))
       (opaque (in-body 'body 'body 'items 1)))
  (ok "higher-order dispatch uses the supplied closure, with an opaque fallback"
      (and (effects? (call-summary-at f inside) 1 'read)
           (effects? (call-summary-at f opaque) 1 'publish 'unknown)
           (may-publish? f map-site))))

(for-each
  (lambda (helper)
    (let* ((f (facts-for (string-append "(fn [x] (let [m {:k x} helper " helper "] (helper m true)))")))
           (summary (call-summary-at f ret-site)))
      (ok "a recursive transfer is conservative"
          (and (may-publish? f map-site)
               (jolt-contains? (argument-effects summary 0) (fact-kw 'unknown))))))
  '("(fn self [a again?] (if again? (self a false) (:k a)))"
    "(fn [a again?] (if again? (recur a false) (:k a)))"))

(let* ((f (facts-for "(fn [x] (let [m {:k x}] (letfn [(a [v] (b v)) (b [v] (a v))] (a m))))")))
  (ok "a mutually recursive cycle does not yield confinement"
      (and (may-publish? f map-site)
           (jolt-contains? (argument-effects (call-summary-at f (in-body 'body 'body)) 0)
                           (fact-kw 'unknown)))))

(let* ((f (facts-for "(fn [x] (let [m {:k x} helper (fn [a] a)] (helper m x)))"))
       (summary (call-summary-at f ret-site)))
  (ok "unsupported arities use the conservative path"
      (and (eq? #f (fact-get summary 'resolved?)) (may-publish? f map-site))))

(set-optimize! #f)
(set-direct-link-flag! #f)

(define linked-call-summary (var-deref "jolt.passes.facts" "linked-call-summary"))
(define set-summary-unit! (var-deref "jolt.backend-scheme" "set-emit-unit!"))
(define set-summary-link! (var-deref "jolt.backend-scheme" "set-direct-link!"))
(define reset-summary-links! (var-deref "jolt.backend-scheme" "direct-link-reset!"))
(ev "(def summary-link (fn* ([a] (:k a)) ([a b] [a b])))")
(ev "(def ^:redef summary-redef (fn [a] a))")
(ev "(def ^:dynamic summary-dynamic (fn [a] a))")
(let* ((previous ((var-deref "jolt.backend-scheme" "current-unit")))
       (unit ((var-deref "jolt.passes.types" "new-unit")))
       (revision (fact-ir "(def summary-link (fn* ([a] (:k a)) ([a b] [a b])))"))
       (source "(fn [x] (let [m {:k x}] (summary-link m)))")
       (one (fact-ir source))
       (two (fact-ir "(fn [x] (let [m {:k x}] (summary-link m x)))")))
  (set-summary-unit! unit)
  (set-summary-link! #t)
  (ok "a forward Var reference has no call target from this unit"
      (jolt-nil? (linked-call-summary unit one ret-site)))
  (jolt-ce-emit revision)
  (let ((one-candidate (linked-call-summary unit one ret-site))
        (two-candidate (linked-call-summary unit two ret-site)))
    (ok "a linked Var summary is conditional on the exact emitted revision and arity"
        (and (eq? revision (fact-get one-candidate 'definition))
             (effects? (fact-get one-candidate 'summary) 0 'read)
             (effects? (fact-get two-candidate 'summary) 0 'return-contains)
             (may-publish? (body-facts unit one) map-site)
             (eq? #f (fact-get (call-summary-at (body-facts unit one) ret-site) 'resolved?)))))
  (for-each
    (lambda (name meta)
      (jolt-ce-emit (fact-ir (string-append "(def ^:" meta " " name " (fn [a] a))")))
      (let ((ir (fact-ir (string-append "(fn [x] (let [m {:k x}] (" name " m)))"))))
        (ok (string-append "no linked revision proof for ^:" meta)
            (and (jolt-nil? (linked-call-summary unit ir ret-site))
                 (may-publish? (body-facts unit ir) map-site)))))
    '("summary-redef" "summary-dynamic") '("redef" "dynamic"))
  (set-summary-link! #f)
  (ok "the same caller IR loses the linked summary when linkage is off"
      (jolt-nil? (linked-call-summary unit one ret-site)))
  (set-summary-link! #t)
  (reset-summary-links!)
  (ok "resetting a compilation unit drops its definition revisions"
      (jolt-nil? (linked-call-summary unit one ret-site)))
  (let* ((replacement (fact-ir "(def summary-link (fn* ([a] a) ([a b] b)))")))
    (jolt-ce-emit replacement)
    (let ((new (linked-call-summary unit one ret-site)))
      (ok "re-emission replaces the summary for the same caller and Var name"
          (and (eq? replacement (fact-get new 'definition))
               (effects? (fact-get new 'summary) 0 'return-alias)))))
  (set-summary-unit! previous)
  (ok "ordinary Var calls follow temporary replacements and preserve publication"
      (clj= (ev (string-append "(with-redefs [summary-link (fn [m] (reset! call-summary-sink m))] "
                               "(let [result (" source " 9)] (identical? result @call-summary-sink)))"))
            #t))
  (ev "(def summary-link (fn [a] (reset! call-summary-sink a)))")
  (ok "a live redefinition disqualifies the older emitted candidate"
      (jolt-nil? (linked-call-summary unit one ret-site))))

(ok "ordinary evaluation computes the same fixed-field result"
    (clj= (ev (string-append "(" direct-loop " 5)")) (ev "{:k 8}")))
(printf "~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
