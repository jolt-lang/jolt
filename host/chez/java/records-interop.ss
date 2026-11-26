;; records-interop.ss — JVM-emulation taxonomy split out of records.ss: the
;; ex-info class accessors, the exception supertype hierarchy, and instance-check
;; / case-string (the (instance? Class x) decision table). Loaded right after
;; records.ss; instance-check forward-refs nothing in records.ss at load time.

;; jolt-ex-info-record predicate (defined in rt.ss).
(define (ex-info-map? v)
  (jolt-ex-info-record? v))
(define (ex-info-class v)
  (jolt-ex-info-record-class-name v))
;; Is `wanted` (simple name) `cls` or a supertype of it? The exception hierarchy
;; lives in the one class graph (class-hierarchy.ss) — resolve the simple name to
;; its graph key and ask jch-isa?, so exceptions and every other class share a
;; single source of truth (ExceptionInfo -> IExceptionInfo is a graph edge).
(define (exception-isa? cls wanted)
  (jch-isa? (jch-fqn-of-simple cls) wanted))

;; A raw Chez condition (an arity or type error Chez itself raised) carries no
;; jolt exception class. Operation sites throw typed jolt throwables
;; (ArityException, IllegalArgumentException, ClassCastException, etc.) before
;; Chez can raise; one that still escapes is classified into a typed throwable
;; at the catch boundary (host-faults.ss), so nothing below ever sees a bare
;; condition as a throwable.
;; instance-check: (type-sym val) — type/protocol membership. Host shims loaded
;; later (io, inst-time, natives-array, natives-queue, host-static-classes)
;; register an arm with register-instance-check-arm! instead of set!-wrapping
;; instance-check; an arm returns #t/#f to decide or 'pass to defer to the next.
;; Newest arm is checked first (matches the old outermost-wins set! order).
;; instance-check-base is the JVM taxonomy fallback when no arm decides.
(define instance-check-registry '())
(define (register-instance-check-arm! f)   ; f: (type-sym val) -> #t | #f | 'pass
  (set! instance-arms-epoch (fx+ instance-arms-epoch 1))
  (set! instance-check-registry (cons f instance-check-registry)))
;; Bumped by every instance-check AND class arm registration (host-class.ss): what
;; the arms answer for a value kind can only change when one is added, the class
;; graph moves, or the protocol registry does — instance-kind-epoch below sums the
;; three for the verdict memo.
(define instance-arms-epoch 0)
;; The one arm that runs LIBRARY code (__register-instance-check!, host-static-
;; classes.ss) — the memo below never caches what it says, and asks it live, at
;; its own place in the order. Registered through here so instance-check can find
;; its position.
(define instance-check-user-arm #f)
(define (register-instance-check-user-arm! f)
  (register-instance-check-arm! f)
  (set! instance-check-user-arm f))
;; The fns that arm asks, in registration order. Defined here, beside the memo
;; that reads it, not in the host file that registers the arm: Gambit includes
;; that file (host-vars.ss) after the seed prelude, and an instance? miss while
;; the prelude loads -- clojure.pprint compiling a format with a numeric param
;; -- died on the unbound name and silently dropped the def.
(define user-instance-checks '())
(define (user-instance-checks-empty?) (null? user-instance-checks))

;; Object / java.lang.Object is the root of the type hierarchy: every non-nil
;; value is an instance of Object; nil is not an instance of anything. This is
;; NOT an arm — instance-check decides it BEFORE the registry, so no arm can
;; answer the root type. An arm that models its own values (the java.time
;; value-semantics seam registers one through __register-instance-check!)
;; naturally answers a definitive false for any class its value's set does not
;; list, and the root rule — registered first, hence asked LAST — never got to
;; speak. That made (instance? Object <java.time value>) false and, through it,
;; (.cast Object v) throw: SCI's box-arg casts every interop argument to its
;; reflected parameter type and jolt reports every parameter as Object, so every
;; interpreted call passing such a value died in the cast (#985).
(define (root-object-type? ts)
  (let ((tn (cond ((symbol-t? ts) (symbol-t-name ts))
                  ((string? ts) ts)
                  (else #f))))
    (and tn (or (string=? tn "Object") (string=? tn "java.lang.Object")))))

;; Does a deftype/defrecord tagged TAG name TNAME as its own type or a protocol/
;; interface it implements? The cheap half of the record arm below (no class-graph
;; walk); type-implements-class? is memoized per (tag, name).
(define (jrec-declares-class? tag tname)
  (or (tag-names-class? tag tname)
      ;; a protocol/interface the type implements (defprotocol generates an
      ;; interface; (instance? SomeProtocol record) is true when the record
      ;; implements it — core.match dispatches on instance? IPatternCompile).
      (type-implements-class? tag tname)))
;; Does TNAME name the type tagged TAG itself?
(define (tag-names-class? tag tname)
  (or (string=? tag tname)
      ;; a simple name matches a qualified tag only at a `.` boundary:
      ;; "a.b.IntervalFD" is an IntervalFD, but "a.b.MultiIntervalFD" is NOT
      ;; (a raw string-suffix would wrongly match the latter).
      (let ((tl (string-length tag)) (nl (string-length tname)))
        (and (fx>? tl nl)
             (char=? (string-ref tag (fx- (fx- tl nl) 1)) #\.)
             (string=? (substring tag (fx- tl nl) tl) tname)))))
;; instance?'s no for a value whose class was REDEFINED after it was built: the
;; old class is not the new one (records.ss jrdesc-rtag), whatever the class
;; graph -- whose row for the name is the new definition's -- or an arm keyed
;; by the class name would say. Its own name spelled any way, the JVM's munged
;; one included.
(define (retired-own-class? tname val)
  (cond ((jrec-retired? val)
         (let ((tag (jrec-tag val)))
           (or (tag-names-class? tag tname)
               (string=? (jch-munge-segments tag) tname))))
        ;; ...and a reify's protocol redefined after its site was laid out
        ;; (records-dispatch.ss reify-retired-protos)
        ((jreify? val)
         (let ((rs (if (unbox reify-retired-box) (reify-retired-protos val) '())))
           (and (pair? rs) (exists (lambda (p) (proto-class-match? p tname)) rs) #t)))
        (else #f)))

;; A yes that the value's OWN type gives — the record or reify declares the class —
;; is the JVM's answer whatever any arm would add, so instance-check takes it before
;; walking the arms. Every arm is a host shim modeling some other kind of value, and
;; each one ran on every instance? of a record or reify only to pass: core.logic asks
;; (instance? IVar lvar) and (instance? IConstraintId c) of every var and constraint
;; on every propagation step, and 23 arms cost ~500 ns of each ~700 ns question. A
;; proxy is excluded — its answer also consults the delegate (proxy.ss). A no still
;; walks the arms exactly as before, so nothing an arm decides can change.
(define (own-type-declares? tname val)
  (cond ((jrec? val)
         ;; memoized per type in its descriptor cache (retired when the class
         ;; graph or the protocol registry moves), keyed eq? on the name object:
         ;; the name is the interned symbol's string, the same object every call
         (let* ((t (vector-ref (jrdesc-ifc-of val) 8))
                (hit (hashtable-ref t tname 'none)))
           (if (eq? hit 'none)
               (let ((ans (and (jrec-declares-class? (jrec-rtag val) tname) #t)))
                 (jolt-with-mutex jrdesc-ifc-mutex (hashtable-set! t tname ans))
                 ans)
               hit)))
        ((and (jreify? val) (not (jreify-delegate val)))
         (or (reify-declares-class? (jreify-protos val) tname) (reify-class-isa? val tname)))
        (else #f)))
;; a reify form's or proxy's generated class and what the class graph puts above
;; it (a proxy is an IProxy, and is its abstract superclass)
(define (reify-class-isa? val tname)
  (let ((c (reify-class-name val))) (and c (jch-isa? c tname) #t)))

(define (instance-check-base type-sym val)
  (let ((tname (symbol-t-name type-sym)))
    (cond
      ((jrec? val)
       (let ((tag (jrec-tag val)))
         (or (jrec-declares-class? (jrec-rtag val) tname)
             ;; the class graph: a declared interface's own ancestry answers too
             ;; (IPersistentMap is an Associative is an IPersistentCollection).
             (and (not (retired-own-class? tname val)) (jch-isa? tag tname)))))
      ((jreify? val) (or (reify-declares-class? (jreify-protos val) tname) (reify-class-isa? val tname)))
      ((ex-info-map? val) (exception-isa? (ex-info-class val) tname))
      (else (case-string tname val)))))

;; Does a reify declaring PROTOS answer instance? for class name TNAME? Every
;; Clojure reify implements IObj/IMeta (it carries metadata); otherwise one of
;; its declared protocols/interfaces must name the class. A pure function of the
;; two, memoized per interned protocol list (make-reified-delegating) and then
;; eq? on the name object (the interned symbol's string, the same each call) — each
;; proto-class-match? munges both names, and a reify of seven protocols paid
;; that seven times per question.
(define reify-class-memo (make-weak-eq-hashtable))
(define (reify-declares-class? protos tname)
  (let* ((inner (hashtable-ref reify-class-memo protos #f))
         (hit (if inner (hashtable-ref inner tname 'none) 'none)))
    (if (not (eq? hit 'none))
        hit
        (let ((ans (or (and (member (last-dot tname) '("IObj" "IMeta")) #t)
                       (and (memp (lambda (p) (proto-class-match? p tname)) protos) #t))))
          (jolt-with-mutex jch-cache-mutex
            (let ((t (or (hashtable-ref reify-class-memo protos #f)
                         (let ((t (make-weak-eq-hashtable)))
                           (hashtable-set! reify-class-memo protos t)
                           t))))
              (hashtable-set! t tname ans)))
          ans))))

;; ---- the verdict memo for the runtime's own value kinds -----------------------
;; A no from instance? walked every arm: ~23 host shims, each asked in turn, two of
;; them (the class-name arm and the value-host-tags arm) costing ~170 ns apiece,
;; then the JVM taxonomy — 500-700 ns to answer (instance? IVar 5), which
;; core.logic asks of every term it walks, and (instance? IConstraintId c) of
;; every constraint that does not implement it.
;;
;; For the runtime's own values the builtin arms' answer is a function of the
;; value's KIND and the class name: every builtin arm decides from type
;; predicates, the class graph and the protocol registry, never from what a value
;; holds, and (class x) for these kinds cannot be claimed by an arm at all
;; (host-class.ss class-arm-reject-fast-type!). The kinds, and the key each is
;; memoized under:
;;   - a record: its descriptor (a redefinition is a new descriptor)
;;   - a plain reify: its interned protocol list (make-reified-delegating)
;;   - a scalar, collection or seq: its class name — the fast path's own string
;;     constant for that class, so one object per class
;;   - a reference or deferred value (atom, var, ref, volatile, delay, promise,
;;     future, agent): its class name, from ref-kind-key below. Their classes
;;     come from class arms, not the (class x) fast path, so a library class arm
;;     is refused for these too (host-class.ss ref-kind-probes)
;; A proxy is excluded: its answer consults the delegate.
;;
;; The LIBRARY arm is never memoized. The chain is split at it: the builtin arms
;; ahead of it, then it (asked live, every time), then the arms after it and the
;; taxonomy — the same order, and so the same answer, as the walk. The verdict is
;; stamped with instance-kind-epoch, read BEFORE it is computed so a racing
;; registration can only understamp.
(define (instance-kind-key val)
  (cond ((jrec? val) (jrec-desc val))
        ;; a reify whose site lost a protocol answers apart from the sites that
        ;; share its protocol list, so it is not memoized under that list
        ((jreify? val) (and (not (jreify-delegate val))
                            (or (not (unbox reify-retired-box)) (null? (reify-retired-protos val)))
                            (jreify-protos val)))
        ((class-fast-name val))
        (else (ref-kind-key val))))
;; Without a key every (instance? IRef an-atom) walked the arms and the taxonomy,
;; 20-45x a record's check (#1299). future / promise / agent / delay live in
;; java/concurrency.ss, which loads long after this file and is no part of the
;; Gambit host, so it keys its own four through the hook, and hands over a probe
;; of each for host-class.ss ref-kind-probes.
(define ref-kind-extra-hook #f)
(define ref-kind-extra-probes (lambda () '()))
(define (set-ref-kind-extra-hook! key-fn probes)
  (set! ref-kind-extra-probes probes)
  (set! ref-kind-extra-hook key-fn))
(define (ref-kind-key x)
  (cond ((jolt-atom? x) "clojure.lang.Atom")
        ((var-cell? x) "clojure.lang.Var")
        ((jolt-ref? x) "clojure.lang.Ref")
        ((jvol? x) "clojure.lang.Volatile")
        (ref-kind-extra-hook (ref-kind-extra-hook x))
        (else #f)))
(define (instance-kind-epoch)
  (fx+ instance-arms-epoch (fx+ jch-graph-epoch jolt-proto-epoch)))
(define instance-kind-memo (make-weak-eq-hashtable))
(define instance-kind-mu (make-mutex))
;; Walk arms from RS until STOP (exclusive); 'pass when none decided.
(define (instance-arms-until rs stop ts val)
  (let loop ((rs rs))
    (cond ((or (null? rs) (eq? (car rs) stop)) 'pass)
          (else (let ((r ((car rs) ts val)))
                  (if (eq? r 'pass) (loop (cdr rs)) r))))))
;; #(epoch before after): BEFORE is the builtin arms ahead of the library arm
;; (#t/#f, or 'pass); AFTER the rest of the chain and the taxonomy, consulted only
;; when neither BEFORE nor the library arm decided.
(define (instance-kind-verdict ts val)
  (let* ((epoch (instance-kind-epoch))
         (user instance-check-user-arm)
         (before (instance-arms-until instance-check-registry user ts val))
         (after (if (eq? before 'pass)
                    (let ((tail (memq user instance-check-registry)))
                      (let ((r (instance-arms-until (if tail (cdr tail) '()) #f ts val)))
                        (if (eq? r 'pass) (instance-check-base ts val) r)))
                    before)))
    (vector epoch before after)))
(define (instance-kind-entry key tname ts val)
  (let* ((inner (hashtable-ref instance-kind-memo key #f))
         (e (and inner (hashtable-ref inner tname #f))))
    (if (and e (fx= (vector-ref e 0) (instance-kind-epoch)))
        e
        (let ((fresh (instance-kind-verdict ts val)))
          (jolt-with-mutex instance-kind-mu
            (let ((t (or (hashtable-ref instance-kind-memo key #f)
                         (let ((t (make-weak-eq-hashtable)))
                           (hashtable-set! instance-kind-memo key t)
                           t))))
              (hashtable-set! t tname fresh)))
          fresh))))
(define (instance-check-kind key tname ts val)
  (let* ((e (instance-kind-entry key tname ts val))
         (before (vector-ref e 1)))
    (if (not (eq? before 'pass))
        before
        (let ((r (if instance-check-user-arm (instance-check-user-arm ts val) 'pass)))
          (if (eq? r 'pass) (vector-ref e 2) r)))))

;; ---- the (instance? T x) call site's inline cache -----------------------------
;; The back end gives each 2-argument (instance-check T x) call a site object
;; (backend_scheme.clj emit-invoke), hoisted once per site. It holds ONE state,
;; #(t ts tname kind answer epoch), replaced whole and never edited in place, so a
;; racing reader sees one consistent state or the other:
;;   - t: the type argument last seen, compared by identity. For a literal (a
;;     protocol key, a quoted class name) it is the same object every call; for a
;;     deftype it is the type's ctor value, which a redefinition replaces — a new
;;     object, so the site re-normalizes.
;;   - ts/tname: t normalized exactly as instance-check normalizes it.
;;   - kind/answer/epoch: the last receiver kind and its answer.
;; A call with the same t, a receiver of the cached kind and the current epoch
;; answers with two eq?s and a fixnum compare — against the symbol intern, the
;; type-argument cascade and two memo tables of the full path (37-54 ns, where the
;; JVM inlines instanceof). An answer is cached only when the LIBRARY arm cannot
;; have changed it: the value's own type declared the class, or the builtin arms
;; ahead of it decided, or no library arm is registered — and a library arm
;; registering bumps instance-arms-epoch (host-static-classes.ss), retiring an
;; answer cached before it existed. Anything else takes the full path.
(define (instance-type-arg t)
  (let ((t (cond ((jclass? t) (jclass-name t))
                 ((and (procedure? t) (deftype-ctor-tag t)))
                 (else t))))
    (if (and (string? t)
             (or (fx= 0 (string-length t)) (not (char=? (string-ref t 0) #\[))))
        (jolt-symbol #f t)
        t)))
(define (jolt-instance-site-make) (vector #f))
;; Is T the type argument the state was built for? A quoted class name may be a
;; fresh symbol per evaluation, but its name and ns are the intern pool's strings,
;; so two spellings of one name compare eq? on both.
(define (instance-site-same-t? t st-t)
  (or (eq? t st-t)
      (and (symbol-t? t) (symbol-t? st-t)
           (eq? (symbol-t-name t) (symbol-t-name st-t))
           (eq? (symbol-t-ns t) (symbol-t-ns st-t)))))
(define (retired-class-arg? t) (and (jclass? t) (jclass-retired? t)))
(define (retired-class-instance? t val)
  (and (jrec-retired? val) (eq? t (jolt-retired-class-token (jrec-desc val)))))
(define (jolt-instance-site site t val)
  (let ((st (vector-ref site 0))
        (k (instance-kind-key val)))
    (if (and st k (eq? k (vector-ref st 3)) (instance-site-same-t? t (vector-ref st 0))
             (fx= (vector-ref st 5) (instance-kind-epoch)))
        (vector-ref st 4)
        (instance-site-miss site st t val k))))
(define (instance-site-miss site st t val k)
  (let* ((same-t (and st (instance-site-same-t? t (vector-ref st 0))))
         (ts (if same-t (vector-ref st 1) (instance-type-arg t)))
         (tname (if same-t (vector-ref st 2) (and (symbol-t? ts) (symbol-t-name ts))))
         (epoch (instance-kind-epoch))
         ;; a retired class's answer is a function of the token and the
         ;; receiver's descriptor alone, so it caches like any other
         (ans (cond ((and k (retired-class-arg? t)) (retired-class-instance? t val))
                    ((or (not k) (not tname) (root-object-type? ts)) 'uncached)
                    ((retired-own-class? tname val) #f)
                    ((own-type-declares? tname val) #t)
                    (else
                     (let* ((e (instance-kind-entry k tname ts val))
                            (before (vector-ref e 1)))
                       (cond ((not (eq? before 'pass)) (if before #t #f))
                             ((user-instance-checks-empty?) (if (vector-ref e 2) #t #f))
                             (else 'uncached)))))))
    ;; each entry is published behind a release: on a weakly ordered machine
    ;; (ARM64) another thread could otherwise see the new entry before its slots
    (if (eq? ans 'uncached)
        (begin
          (unless same-t (memory-order-release) (vector-set! site 0 (vector t ts tname #f #f -1)))
          (if (instance-check ts val) #t #f))
        (begin (memory-order-release) (vector-set! site 0 (vector t ts tname k ans epoch)) ans))))

;; The plain walk, which the memo must always agree with (the dispatch-caches
;; unit rows compare the two).
(define (instance-check-walk ts val)
  (let loop ((rs instance-check-registry))
    (if (or (null? rs) (and (symbol-t? ts) (retired-own-class? (symbol-t-name ts) val)))
        (instance-check-base ts val)
        (let ((r ((car rs) ts val)))
          (if (eq? r 'pass) (loop (cdr rs)) r)))))

(define (instance-check type-sym0 val)
  ;; a Class value as the type arg (instance? (class x) y) -> use its name string.
  ;; A deftype/defrecord type token is its make-deftype-ctor closure; use the tag
  ;; ("ns.Name") it carries so (instance? Bar x) works when Bar is passed by value
  ;; (schema's record*/class-schema hold the type as a value, not a literal symbol).
  (let ((ts (instance-type-arg type-sym0)))
    (cond
      ;; a redefined type's OLD class (class-model.ss jolt-retired-class-token)
      ;; holds exactly the instances built before the redefinition
      ((retired-class-arg? type-sym0) (retired-class-instance? type-sym0 val))
      ((root-object-type? ts) (not (jolt-nil? val)))
      ((and (symbol-t? ts) (retired-own-class? (symbol-t-name ts) val)) #f)
      ((and (symbol-t? ts) (own-type-declares? (symbol-t-name ts) val)) #t)
      ((and (symbol-t? ts) (instance-kind-key val))
       => (lambda (key) (instance-check-kind key (symbol-t-name ts) ts val)))
      (else (instance-check-walk ts val)))))
(define (case-string tname val)
  (cond
    ((member tname '("Number" "java.lang.Number")) (or (number? val) (jfloat? val)))
    ;; long-range only (the printer's N-suffix boundary, not the 61-bit fixnum
    ;; range — Long/MAX_VALUE is a Chez bignum but a JVM Long): beyond it a
    ;; value is the JVM's BigInt, which is not a Long (issue #627) and answers
    ;; through its BigInt/BigInteger tags instead.
    ((member tname '("Long" "java.lang.Long" "Integer" "java.lang.Integer"))
     (and (number? val) (exact? val) (integer? val) (not (jolt-bigint-print? val))))
    ((member tname '("Double" "java.lang.Double")) (flonum? val))
    ((member tname '("Float" "java.lang.Float")) (jfloat? val))
    ((member tname '("Ratio" "clojure.lang.Ratio")) (and (number? val) (exact? val) (rational? val) (not (integer? val))))
    ((member tname '("String" "java.lang.String" "CharSequence" "java.lang.CharSequence")) (string? val))
    ((member tname '("Boolean" "java.lang.Boolean")) (boolean? val))
    ((member tname '("Character" "java.lang.Character")) (or (char? val) (jsurr? val)))
    ((member tname '("Keyword" "clojure.lang.Keyword")) (keyword? val))
    ((member tname '("Symbol" "clojure.lang.Symbol")) (jolt-symbol? val))
    ((member tname '("Atom" "clojure.lang.Atom")) (jolt-atom? val))
    ((member tname '("IFn" "clojure.lang.IFn" "Fn" "clojure.lang.Fn")) (procedure? val))
    ((member tname '("Pattern" "java.util.regex.Pattern")) (regex-t? val))
    ((member tname '("Matcher" "java.util.regex.Matcher"
                     "MatchResult" "java.util.regex.MatchResult"))
     (matcher-t? val))
    ((member tname '("URI" "java.net.URI"))
     (and (jhost? val) (string=? (jhost-tag val) "uri")))
    ((member tname '("File" "java.io.File")) (jfile? val))
    ((member tname '("UUID" "java.util.UUID")) (juuid? val))
    ;; clojure.lang.IPending — the realized?-able types (Promise/Future/Delay/
    ;; LazySeq all implement isRealized on the JVM). A tap> that hands a promise to
    ;; a tap fn relies on this so the fn can deliver it.
    ((member tname '("IPending" "clojure.lang.IPending"))
     (or (jolt-promise? val) (jolt-future? val) (jolt-delay? val) (jolt-lazyseq? val)))
    (else #f)))

;; str of a record uses a custom (Object toString) impl if the type defines one
;; (deftype with no default toString relies on this); otherwise the map form
;; without the leading # (Clojure's record .toString). converters.ss loads before
;; records.ss, so this set! sees the registry — forward refs resolve at call time.

(def-var! "clojure.core" "instance-check" instance-check)

;; instance? is Class.isInstance on the JVM: its type argument is cast to Class
;; first, so anything else -- a protocol's map, a keyword, a string, a quoted
;; symbol -- is the JDK's ClassCastException, and nil the NullPointerException of
;; calling isInstance on it, whatever the value. The instance? macro hands every
;; type argument it does not know to be a class through here (a class name it
;; resolved itself goes straight to instance-check). jolt's own class values: a
;; Class token, and a deftype/defrecord's type token (its ctor).
(define (jolt-class-arg t)
  (cond ((jclass? t) t)
        ((and (procedure? t) (deftype-ctor-tag t)) t)
        ((jolt-nil? t)
         (throw-jvm 'NullPointerException
                    "Cannot invoke \"java.lang.Class.isInstance(Object)\" because \"c\" is null"))
        (else (jolt-checkcast-fail t "java.lang.Class"))))
(def-var! "jolt.host" "class-arg" jolt-class-arg)
;; instance? taken as a value ((partial instance? C)): the same cast, then the test
(def-var! "jolt.host" "instance-of" (lambda (t x) (instance-check (jolt-class-arg t) x)))

;; ---- argument casts: the two ClassCastException wordings ---------------------
;; A host method's parameter refuses an argument of another class in one of two
;; wordings on the JVM, and the COMPILER picks which, not the method. A call it
;; resolved (it knew the receiver's class) casts each argument with a checkcast,
;; whose message names both classes and their modules; a call it could not
;; resolve goes through the Reflector, whose Class.cast says "Cannot cast X to
;; Y". jolt splits the same way: for a resolved call the back end emits
;; jolt-checkcast on each argument whose parameter is registered below
;; (backend_scheme/interop.clj cast-args), so a method's own argument check is
;; reached only by an unresolved call and fails with jolt-reflect-cast-fail. A
;; method that casts INSIDE its body (Keyword.compareTo(Object)) raises the
;; checkcast wording whatever the call, with jolt-checkcast-fail.
(define (jvm-base-class-name? n)
  (and (fx>? (string-length n) 0)
       (or (char=? (string-ref n 0) #\[)
           (and (fx>? (string-length n) 5) (string=? (substring n 0 5) "java.")))))
(define (jvm-checkcast-message from to)
  (let ((fb (jvm-base-class-name? from)) (tb (jvm-base-class-name? to)))
    (string-append
     "class " from " cannot be cast to class " to " ("
     (cond ((and fb tb) (string-append from " and " to " are in module java.base of loader 'bootstrap'"))
           ((and (not fb) (not tb)) (string-append from " and " to " are in unnamed module of loader 'app'"))
           (else (string-append from (if fb " is in module java.base of loader 'bootstrap'; "
                                         " is in unnamed module of loader 'app'; ")
                                to (if tb " is in module java.base of loader 'bootstrap'"
                                       " is in unnamed module of loader 'app'"))))
     ")")))
(define (jolt-cast-class-name x)
  (let ((n (guard (e (#t #f)) (jolt-class-name x)))) (if (string? n) n "java.lang.Object")))
(define (jolt-checkcast-fail x to)
  (throw-jvm 'ClassCastException (jvm-checkcast-message (jolt-cast-class-name x) to)))
(define (jolt-reflect-cast-fail x to)
  (throw-jvm 'ClassCastException
             (string-append "Cannot cast " (jolt-cast-class-name x) " to " to)))
(define (jolt-checkcast to x)
  (if (or (jolt-nil? x) (instance-check to x)) x (jolt-checkcast-fail x to)))

;; The parameter classes of a host method, for the casts above: by method name,
;; a list of (declaring-class . parameter-classes). Only a method that is its
;; class's one method of that name and arity belongs here — with an overload the
;; JVM compiler cannot pick a method from the receiver alone and reflects. Filled
;; at load, before any thread can read it.
(define method-params-tbl (make-hashtable string-hash string=?))
(define (register-method-params! cls method params)
  (hashtable-set! method-params-tbl method
                  (cons (cons cls params) (hashtable-ref method-params-tbl method '()))))
;; The compiler's question: the parameter classes (cls).method takes at argc
;; arguments, as a vector of class names, or nil when no registered method fits.
(define (method-param-classes cls method argc)
  (let loop ((es (hashtable-ref method-params-tbl method '())))
    (cond ((null? es) jolt-nil)
          ((and (fx=? (length (cdar es)) argc) (jch-isa? cls (caar es)))
           (apply jolt-vector (cdar es)))
          (else (loop (cdr es))))))
(def-var! "jolt.host" "method-param-classes" method-param-classes)
;; Character's compareTo lives in records-dispatch.ss and surrogates.ss, which
;; load on either side of this table; the other registrations sit beside their
;; methods.
(register-method-params! "java.lang.Character" "compareTo" '("java.lang.Character"))

;; ---- java.lang.Throwable: the surface EVERY throwable inherits ---------------
;; On the JVM these are declared on Throwable itself, so every exception class has
;; them by inheritance and no shim ever needs to restate them. jolt had no such
;; place: the method bodies were duplicated across the `condition?` arm in
;; records.ss and dot-object-method in dot-forms.ss, which is exactly how the two
;; drifted — .printStackTrace existed on a raw Chez condition and was missing on
;; every ex-info / (Exception. …) / typed host throwable, and .getLocalizedMessage
;; the other way round. This is the single table both paths call.
;;
;; jolt models a throwable two ways: a jolt-ex-info-record (ex-info, and every
;; typed host throwable via jolt-host-throwable) or a raw Chez condition (an error
;; the host itself raised). Both answer here.
;;
;; jolt-throwable-method returns a BOXED result (a one-element list) or #f for
;; "not a Throwable method", matching dot-object-method — a legitimate nil/#f
;; result has to stay distinguishable from "no such method".
(define (jolt-throwable-message v)
  (cond ((jolt-ex-info-record? v) (jolt-ex-info-record-message v))
        ((condition? v) (condition->message-string v))
        (else jolt-nil)))

;; "class: message", the JVM Throwable.toString. jolt-str-render-one already
;; renders an ex-info record that way (source-registry.ss); a raw condition has no
;; class of its own, so its message stands alone.
(define (jolt-throwable-tostring v)
  (if (condition? v) (condition->message-string v) (jolt-str-render-one v)))

;; Suppressed throwables live beside the throwable, not in it: a table from the
;; throwable to its list, newest first. The record has no slot for them, and a
;; new field changes the record's uid, which means a re-mint and a state-image
;; layout bump. The same table also covers a raw Chez condition, which has no
;; fields to extend. Several threads can add to one throwable, so every access
;; takes the mutex. The table is an ephemeron one, so a throwable's list goes
;; with it: a weak table holds its values strongly, so a list that reaches back
;; to its own key (a suppresses o, o suppresses a) would never go.
(define throwable-suppressed-tbl (make-ephemeron-eq-hashtable))
(define throwable-suppressed-mu (make-mutex))
(define (throwable-suppressed-list v)
  (reverse (jolt-with-mutex throwable-suppressed-mu
             (hashtable-ref throwable-suppressed-tbl (throwable-identity v) '()))))
;; Throwable.addSuppressed: the self check comes first, as in the JDK, and its
;; IllegalArgumentException carries the throwable as its cause. A non-throwable
;; argument gets here only from a call the compiler did not resolve (a resolved
;; one casts it first, jolt-checkcast above), so it is Class.cast's refusal.
(register-method-params! "java.lang.Throwable" "addSuppressed" '("java.lang.Throwable"))

;; A proxy over a throwable class is a reify whose delegate is the base
;; throwable; which of the two a call hands over depends on the route it took,
;; so identity checks compare the throwable underneath.
(define (throwable-identity x)
  (if (and (jreify? x) (jreify-delegate x)) (jreify-delegate x) x))
;; Throwables built with enableSuppression false (the protected 4-argument
;; constructor, which a proxy reaches): addSuppressed still checks its argument
;; and then records nothing. A side table, like the suppressed lists.
(define throwable-no-suppression-tbl (make-ephemeron-eq-hashtable))
(define (throwable-disable-suppression! v)
  (jolt-with-mutex throwable-suppressed-mu
    (hashtable-set! throwable-no-suppression-tbl (throwable-identity v) #t)))
(define (throwable-add-suppressed! v other)
  (cond
    ((not (or (jolt-nil? other) (instance-check throwable-type-sym other)))
     (jolt-reflect-cast-fail other "java.lang.Throwable"))
    ((eq? (throwable-identity other) (throwable-identity v))
     (jolt-throw (jolt-host-throwable "java.lang.IllegalArgumentException"
                                      "Self-suppression not permitted" other)))
    ((jolt-nil? other)
     (throw-jvm 'NullPointerException "Cannot suppress a null exception."))
    ((jolt-with-mutex throwable-suppressed-mu
       (hashtable-ref throwable-no-suppression-tbl (throwable-identity v) #f))
     (void))
    (else
     (jolt-with-mutex throwable-suppressed-mu
       (let ((k (throwable-identity v)))
         (hashtable-set! throwable-suppressed-tbl k
                         (cons other (hashtable-ref throwable-suppressed-tbl k '())))))))
  jolt-nil)

;; Throwable.printStackTrace: the header line, then the same Clojure backtrace the
;; uncaught reporter prints, then each suppressed throwable laid out as the JDK
;; does: "Suppressed: <toString>" one tab in, its frames and its own suppressed
;; one tab further. jolt prints no "Caused by" chain and elides no common frames,
;; so neither appears here. A throwable already printed anywhere in the walk, a
;; cycle or a second branch, ends in the JDK's CIRCULAR REFERENCE line.
;; Called from a catch clause the throw's captured
;; continuation is still live (the emitted catch runs jolt-catch-complete! only
;; AFTER the body), so the trace is the one that led to this throwable.
;; jolt-backtrace-string lives in source-registry.ss, which loads after this file —
;; a top-level forward reference, resolved at call time.
(define (jolt-throwable-print-stack-trace v port)
  ;; SEEN is one set for the whole walk, as the JDK's dejaVu is: a throwable
  ;; printed on one branch is a circular reference on every later one.
  (jolt-throwable-print-enclosed v port "" "" (box (list (throwable-identity v))))
  jolt-nil)
(define (jolt-throwable-print-enclosed v port prefix caption seen)
  (display prefix port) (display caption port)
  (display (jolt-throwable-tostring v) port)
  (newline port)
  (let ((bt (guard (e (#t #f)) (jolt-throwable-backtrace-string v))))
    (when bt
      (let loop ((i 0) (start 0))
        (cond ((= i (string-length bt))
               (when (< start i) (display prefix port) (display (substring bt start i) port)))
              ((char=? (string-ref bt i) #\newline)
               (display prefix port) (display (substring bt start (+ i 1)) port)
               (loop (+ i 1) (+ i 1)))
              (else (loop (+ i 1) start))))))
  (let ((p (string-append prefix "\t")))
    (for-each
      (lambda (x)
        (if (memq (throwable-identity x) (unbox seen))
            (begin (display p port) (display "Suppressed: [CIRCULAR REFERENCE: " port)
                   (display (jolt-throwable-tostring x) port) (display "]" port) (newline port))
            (begin (set-box! seen (cons (throwable-identity x) (unbox seen)))
                   (jolt-throwable-print-enclosed x port p "Suppressed: " seen))))
      (throwable-suppressed-list v))))

;; The target of a 1-arg .printStackTrace — a PrintStream / PrintWriter shim. Route
;; through the target's own .write, so any writer (io.ss's, a library's) works.
;; rest-args is a JOLT seq, not a Scheme list: passing a raw list made every
;; dispatch miss. A target with no .write raises from there, which is the right
;; report — swallowing it and quietly printing to stderr instead is what hid this.
(define (jolt-throwable-print-to v target)
  (let ((s (let ((p (open-output-string)))
             (jolt-throwable-print-stack-trace v p)
             (get-output-string p))))
    (record-method-dispatch target "write" (jolt-list s))
    jolt-nil))

;; The accessors of the classes that keep values apart from their message, by the
;; slot of the detail vector each one reads. A FileSystemException thrown without
;; one was built from a message alone, which is its file, as FileSystemException's
;; one-argument constructor has it.
(define throwable-detail-accessors
  '(("FileSystemException" ("getFile" . 0) ("getOtherFile" . 1) ("getReason" . 2))
    ("URISyntaxException" ("getInput" . 0) ("getReason" . 1) ("getIndex" . 2))
    ("PatternSyntaxException" ("getDescription" . 0) ("getIndex" . 1) ("getPattern" . 2))
    ("java.sql.BatchUpdateException" ("getUpdateCounts" . 2) ("getLargeUpdateCounts" . 2))
    ("java.sql.SQLException" ("getSQLState" . 0) ("getErrorCode" . 1))))
;; What an accessor answers for a throwable built without a detail vector (one
;; raised through jolt.host/throwable with just a message).
(define (throwable-detail-default name obj)
  (cond ((string=? name "getFile") (jolt-ex-info-record-message obj))
        ((string=? name "getErrorCode") 0)
        (else jolt-nil)))
;; The value an accessor hands out of a slot. A stored array is copied per call
;; (getUpdateCounts copies on the JVM); the arrays are a later file's, so
;; host-static-classes.ss installs the copy alongside the ctor that stores them.
(define throwable-detail-value (lambda (name v) v))
(define (throwable-detail-method obj name)
  (and (jolt-ex-info-record? obj)
       (let loop ((cs throwable-detail-accessors))
         (cond
           ((null? cs) #f)
           ((and (assoc name (cdar cs))
                 (exception-isa? (jolt-ex-info-record-class-name obj) (caar cs)))
            (let ((d (jolt-ex-info-record-error-offset obj)))
              (list
               (if (vector? d)
                   (throwable-detail-value name (or (vector-ref d (cdr (assoc name (cdar cs)))) jolt-nil))
                   (throwable-detail-default name obj)))))
           (else (loop (cdr cs)))))))

(define (throwable-method obj name args)
  (cond
    ((or (string=? name "getMessage") (string=? name "getLocalizedMessage"))
     (list (jolt-throwable-message obj)))
    ((string=? name "toString") (list (jolt-throwable-tostring obj)))
    ((string=? name "getCause")
     (list (if (jolt-ex-info-record? obj) (jolt-ex-info-record-cause obj) jolt-nil)))
    ;; java.sql.SQLException chaining — jolt throwables don't chain.
    ((string=? name "getNextException") (list jolt-nil))
    ;; java.text.ParseException.getErrorOffset — the int its ctor stashed.
    ((string=? name "getErrorOffset")
     (list (if (and (jolt-ex-info-record? obj) (fixnum? (jolt-ex-info-record-error-offset obj)))
               (jolt-ex-info-record-error-offset obj)
               0)))
    ;; The frames printStackTrace renders, as elements: the ones that map to
    ;; Clojure source, from the continuation the throwable was thrown with
    ;; (source-registry.ss). A tail call leaves no frame to report, so a caller
    ;; erased by one is missing, as it is from Thread.getStackTrace.
    ;; A StackTraceElement[], as the JVM's is (jolt-ref-array).
    ((string=? name "getStackTrace")
     (list (jolt-ref-array (jolt-throwable-stack-trace obj) "java.lang.StackTraceElement")))
    ((string=? name "getSuppressed")
     (list (jolt-ref-array (throwable-suppressed-list obj) "java.lang.Throwable")))
    ((and (string=? name "addSuppressed") (pair? args))
     (list (throwable-add-suppressed! obj (car args))))
    ;; JVM contract is "returns this"; jolt has no stack to refill.
    ((string=? name "fillInStackTrace") (list obj))
    ((string=? name "printStackTrace")
     (list (if (pair? args)
               (jolt-throwable-print-to obj (car args))
               (jolt-throwable-print-stack-trace obj (current-error-port)))))
    (else (throwable-detail-method obj name))))

;; Broad-catch fallback for catch-clause dispatch (analyze-try desugars
;; (catch C e …) to (or (instance? C e) (__catch-broad? "C" e))). A raised value
;; that is no throwable at all (a throw of a keyword or a string) carries no
;; exception class, so instance? can't place it; a Clojure (catch C e) over such
;; a value matches when C is RuntimeException (or a subclass) / Exception /
;; Throwable. Typed throwables (ex-info records, (SomeException. …), and every
;; host fault by the time a catch binds it — host-faults.ss) are recognized by
;; instance? as Throwable, so untyped? is false and they dispatch precisely
;; through the instance? arm instead.
(define throwable-type-sym (jolt-symbol #f "Throwable"))
(define (simple-class-name nm)
  (let loop ((i (- (string-length nm) 1)))
    (cond ((< i 0) nm)
          ((char=? (string-ref nm i) #\.) (substring nm (+ i 1) (string-length nm)))
          (else (loop (- i 1))))))
(define (jolt-catch-broad? nm v)
  (and (not (instance-check throwable-type-sym v))
       (let ((s (simple-class-name nm)))
         (or (exception-isa? s "RuntimeException")
             (string=? s "Exception")
             (string=? s "Throwable")))))
(def-var! "clojure.core" "__catch-broad?"
  (lambda (nm v) (if (jolt-catch-broad? nm v) #t #f)))
