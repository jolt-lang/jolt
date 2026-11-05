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

(set-optimize! #f)
(set-direct-link-flag! #f)
(printf "~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
