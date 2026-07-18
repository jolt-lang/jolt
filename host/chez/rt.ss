;; The minimal Chez RT the emitted Scheme rests on.
;;
;; Sits above the value model (values.ss) and below an emitted program. Adds the
;; two things the back end's output references that aren't in the value layer:
;;   1. the var-cell late-binding registry (Clojure vars — a global root that a
;;      reference reads at call time, so redefinition / mutual recursion work);
;;   2. the rt primitive shims the emitter names (jolt-inc/dec/not) and jolt's
;;      number printing (all jolt numbers model Clojure doubles; integer-valued
;;      print without a trailing ".0").
;;
;; Emitted programs do `(load "host/chez/rt.ss")`; this loads values.ss in turn.

(load "host/chez/values.ss")
(load "host/chez/hasheq.ss")
;; Resolve a libc entry point at RUN time; #f when the entry doesn't exist
;; (chmod/sigaddset on Windows). A macro so the platforms can differ:
;;
;; - POSIX: a compiled (foreign-procedure …) guarded at creation time. The
;;   foreign entry resolves when the closure is created (this define running),
;;   not when the fasl loads, so a missing symbol raises here and the guard
;;   returns #f. Compiled creation is what lets these run under a petite-only
;;   boot (a compiler-dropped binary): Chez's interpreter cannot build a
;;   foreign-procedure, so an eval'd form would silently yield #f there.
;;
;; - Windows: eval the form instead. A foreign reference in compiled fasl is a
;;   load-time relocation there and a missing symbol aborts the boot (exit 3)
;;   before any guard runs; eval keeps the form out of the fasl entirely.
;;   Windows builds always carry the compiler boot, so eval compiles.
(define-syntax jolt-foreign-proc-safe
  (lambda (x)
    (syntax-case x (quote)
      ((_ name (quote args) (quote res))
       (if (memq (machine-type) '(a6nt ta6nt i3nt ti3nt))
           #'(guard (e (#t #f))
               (load-shared-object #f)
               (and (foreign-entry? name)
                    (eval `(foreign-procedure ,name ,'args ,'res))))
           #'(guard (e (#t #f))
               (load-shared-object #f)
               (and (foreign-entry? name)
                    (foreign-procedure name args res))))))))

(load "host/chez/collections.ss")
(load "host/chez/seq.ss")

;; --- version ------------------------------------------------------------------
;; One source of truth for the jolt version string, read by jolt.host/jolt-version
;; (loader.ss), (System/getProperty "jolt.version"), and clojure.core/*jolt-version*
;; (dynamic-var-defaults.ss). A self-contained binary bakes the release tag by
;; emitting (define jolt-baked-version-early "…") at the TOP of flat.ss
;; (build-joltc.ss) — early so every consumer that loads later sees it. A dev run
;; has no baked define and falls back to $JOLT_VERSION (bin/joltc sets it from
;; `git describe`), then "dev".
(define (jolt-version-string)
  (or (and (top-level-bound? 'jolt-baked-version-early)
           (top-level-value 'jolt-baked-version-early))
      (let ((v (getenv "JOLT_VERSION"))) (and v (> (string-length v) 0) v))
      "dev"))

;; --- rt arithmetic / logic shims (named in the emitter's native-ops) ----------
(define (jolt-inc x) (+ x 1))
(define (jolt-dec x) (- x 1))
;; Coerce a ^long-hinted argument to a fixnum at fn entry, the way the JVM's
;; longCast coerces a primitive-long parameter: truncate a flonum toward zero,
;; pass an exact integer through, error if it doesn't fit a fixnum or isn't a
;; number. The hint is a promise the value is a fixnum-range long; the body's fx*
;; ops rely on it. (^double params coerce with the built-in exact->inexact.)
;; A ^long is a 64-bit value; a Chez fixnum is only 61-bit, so a value that
;; overflows the fixnum range (a full-width long, e.g. from unchecked / wrapping
;; arithmetic) passes through as an exact integer rather than erroring. fx ops in
;; the body still require fixnums (they raise on a bignum), but generic /
;; unchecked-* ops handle it.
(define (jolt->fx x)
  (cond ((fixnum? x) x)
        ((and (number? x) (exact? x) (integer? x)) x)
        ((flonum? x) (exact (truncate x)))
        ((rational? x) (exact (truncate x)))
        (else (error 'jolt "^long hint: not a number" x))))
;; jolt `not`: only nil and false are falsey.
(define (jolt-not x) (if (jolt-truthy? x) #f #t))

;; --- ex-info record type -----------------------------------------------------
;; A throwable (ex-info or host-constructed typed throwable) is a distinct
;; record type — NOT a pmap — so pmap?/coll?/seqable?/ifn?/associative?/
;; counted? are naturally false without per-kind exclusion arms.
;; Equality: identity (records default to identity equality — (= e e2) false,
;; (= e e) true, matching the JVM where ExceptionInfo does NOT implement
;; equals). get / keyword lookup: MISS (record is NOT ILookup).
;; error-offset stores ParseException.getErrorOffset (0 when not set).
(define-record-type jolt-ex-info-record
  (fields class-name message cause data (mutable error-offset))
  (nongenerative jolt-ex-info-record-v1))

;; --- exceptions --------------------------------------------------------------
;; throw raises a Chez condition WRAPPING the jolt value; catch (emitted as
;; `guard`) and jolt-report-uncaught unwrap it back via jolt-unwrap-throw.
;; Raising the value RAW broke when a throw crossed the host/`eval` boundary:
;; Chez re-wrapped the non-condition into a compound condition whose
;; message-extraction APPLIES the value (crashing on an empty-map :data ->
;; "attempt to apply non-procedure"), and the real message was lost. A real
;; condition propagates intact through any number of eval boundaries.
;; Capture the live continuation at the throw site (identity-tagged with the
;; thrown value) so an uncaught error can walk the native frames back to a Clojure
;; stack trace (source-registry.ss). call/cc is paid only on a throw, never per
;; call; the captured k is walked, never invoked.
(define jolt-throw-cont (make-thread-parameter #f))

;; Cleared after a catch handler completes normally so the parked continuation
;; (and its captured frames) does not root live data until the next throw.
(define (jolt-catch-complete!) (jolt-throw-cont #f))

;; --- tail-frame history: a ring of rings (opt-in) ----------------------------
;; TCO erases tail-called frames from the native continuation, so an uncaught
;; error's backtrace shows only the surviving non-tail spine — the immediate error
;; site is often a tail call and is missing. When tracing is enabled (JOLT_TRACE,
;; wired in compile-eval.ss), each compiled fn records its frame-name on entry, and
;; the reporter reads this history to recover TCO-elided frames.
;;
;; The store is MIT-Scheme's "history" shape — a ring of rings. The OUTER ring
;; holds one RIB per non-tail subproblem (the real call spine); each rib's INNER
;; ring holds the recent tail-calls made AT that subproblem. A non-tail entry
;; advances the outer ring (a fresh rib); a tail entry rotates the current rib's
;; inner ring. So a tight tail loop (mutual recursion, a non-recur self-tail-call)
;; churns ONE rib's small inner ring instead of flushing the outer spine — the
;; caller context that led into the loop survives. Both rings are fixed-size, so
;; the whole history is bounded: a constant space factor, NOT a change to the
;; asymptotic space TCO guarantees.
;;
;; Whether an entry is tail or non-tail is set by the CALLER: the emitter marks a
;; tail call with (jolt-trace-mark! #t) right before it; a non-tail entry is the
;; default. NOTE this is best-effort: a tail call routed through jolt-invoke to a
;; target that has no entry prologue (a core/native fn, an anonymous fn held in a
;; var) does not consume the mark, so a following non-tail frame can be mislabeled
;; as a tail rotation — a cosmetic mis-grouping in the trace, never a wrong result.
(define jolt-trace-outer-size 48)          ; ribs (non-tail spine depth kept)
(define jolt-trace-inner-size 6)           ; tail-calls kept per subproblem
;; A history: #(ribs-vector outer-head outer-count). A rib: #(name-vector head count).
(define (jolt-make-rib) (vector (make-vector jolt-trace-inner-size #f) 0 0))
(define (jolt-make-history)
  (let ((ribs (make-vector jolt-trace-outer-size #f)))
    (let loop ((i 0))
      (when (fx<? i jolt-trace-outer-size)
        (vector-set! ribs i (jolt-make-rib)) (loop (fx+ i 1))))
    (vector ribs 0 0)))
;; A global switch (all threads) plus a per-thread ring, lazily created on first
;; use — so code run on a spawned thread (a future/agent) records into ITS OWN
;; history, not the enabling thread's (make-thread-parameter hands a new thread the
;; initial #f, so we can't rely on inheritance).
(define jolt-trace-on? #f)
(define jolt-trace-ring (make-thread-parameter #f))
(define jolt-trace-tail? (make-thread-parameter #f))   ; caller-set, consumed per entry
(define (jolt-trace-enable!) (set! jolt-trace-on? #t) (jolt-trace-ring (jolt-make-history)))
;; this thread's ring, created on demand while tracing is on
(define (jolt-trace-cur-ring)
  (or (jolt-trace-ring)
      (and jolt-trace-on? (let ((h (jolt-make-history))) (jolt-trace-ring h) h))))
;; Drop accumulated history at a top-level boundary (compile-eval.ss calls this per
;; top-level form) so an error's trace shows only the forms that led to it, not the
;; frames of earlier, already-returned REPL/eval forms.
(define (jolt-trace-reset!)
  (when (jolt-trace-ring) (jolt-trace-ring (jolt-make-history)) (jolt-trace-tail? #f)))
(define (jolt-trace-mark! t) (jolt-trace-tail? t))

;; push name into a rib's inner ring
(define (jolt-rib-push! rib name)
  (let ((buf (vector-ref rib 0)) (i (vector-ref rib 1)) (cnt (vector-ref rib 2)))
    (vector-set! buf i name)
    (vector-set! rib 1 (fxmod (fx+ i 1) jolt-trace-inner-size))
    (when (fx<? cnt jolt-trace-inner-size) (vector-set! rib 2 (fx+ cnt 1)))))
;; a non-tail entry: advance the outer ring, reset the new rib, seed it with name
(define (jolt-history-nontail! h name)
  (let* ((ribs (vector-ref h 0)) (oh (vector-ref h 1)) (oc (vector-ref h 2))
         (rib (vector-ref ribs oh)))
    (vector-set! rib 1 0) (vector-set! rib 2 0)
    (jolt-rib-push! rib name)
    (vector-set! h 1 (fxmod (fx+ oh 1) jolt-trace-outer-size))
    (when (fx<? oc jolt-trace-outer-size) (vector-set! h 2 (fx+ oc 1)))))
;; a tail entry: rotate the CURRENT rib's inner ring (bootstrap a rib if none yet)
(define (jolt-history-tail! h name)
  (if (fx=? (vector-ref h 2) 0)
      (jolt-history-nontail! h name)
      (let* ((ribs (vector-ref h 0))
             (cur (fxmod (fx+ (fx- (vector-ref h 1) 1) jolt-trace-outer-size)
                         jolt-trace-outer-size)))
        (jolt-rib-push! (vector-ref ribs cur) name))))
;; Record a frame entry, routed by the caller's tail mark; then reset the mark so a
;; subsequent entry reached WITHOUT a mark (e.g. via apply) defaults to non-tail.
(define (jolt-trace-push! name)
  (let ((h (jolt-trace-cur-ring)))
    (when h
      (if (jolt-trace-tail?) (jolt-history-tail! h name) (jolt-history-nontail! h name))
      (jolt-trace-tail? #f)))
  jolt-nil)

;; a rib's inner names, most-recent (deepest) tail first
(define (jolt-rib-names rib)
  (let ((buf (vector-ref rib 0)) (head (vector-ref rib 1)) (cnt (vector-ref rib 2)))
    (let loop ((k 1) (acc '()))
      (if (fx>? k cnt)
          (reverse acc)
          (loop (fx+ k 1)
                (cons (vector-ref buf (fxmod (fx+ (fx- head k) jolt-trace-inner-size)
                                             jolt-trace-inner-size))
                      acc))))))
;; The whole history flattened to frame-names, most-recent (deepest) first:
;; current rib's tail-history, then its non-tail caller's, and so on outward.
(define (jolt-trace-snapshot)
  (let ((h (jolt-trace-ring)))
    (if (not h) '()
        (let* ((ribs (vector-ref h 0)) (oh (vector-ref h 1)) (oc (vector-ref h 2)))
          (let loop ((k 1) (acc '()))
            (if (fx>? k oc)
                (apply append (reverse acc))
                (let ((idx (fxmod (fx+ (fx- oh k) jolt-trace-outer-size) jolt-trace-outer-size)))
                  (loop (fx+ k 1) (cons (jolt-rib-names (vector-ref ribs idx)) acc)))))))))

(define-condition-type &jolt-throw &condition
  make-jolt-throw-condition jolt-throw-condition?
  (value jolt-throw-condition-value))
;; Fallback &message for a leaked condition; the real message always comes from
;; the unwrapped value via ex-message.
(define (jolt-throw-message v)
  (if (jolt-ex-info-record? v)
      (let ((m (jolt-ex-info-record-message v)))
        (if (string? m) m "jolt error"))
      "jolt error"))
(define (jolt-throw v)
  (call/cc (lambda (k)
             (jolt-throw-cont (cons v k))
             (raise (condition (make-message-condition (jolt-throw-message v))
                               (make-jolt-throw-condition v))))))
(define (jolt-unwrap-throw x)
  (if (jolt-throw-condition? x) (jolt-throw-condition-value x) x))
;; ex-info builds a jolt-ex-info-record (NOT a pmap — pmap?/coll?/seqable?/ifn?
;; /associative?/counted? are naturally false). Arity 2 (msg data) or 3 (msg data cause).
;; No :jolt/class field on plain ex-info — class defaults to clojure.lang.ExceptionInfo
;; via ex-info-class in records-interop.ss.
(define (jolt-ex-info msg data . more)
  (make-jolt-ex-info-record "clojure.lang.ExceptionInfo" msg
                             (if (null? more) jolt-nil (car more))
                             data 0))
;; A host-constructed throwable (RuntimeException. etc.): a jolt-ex-info-record
;; carrying its canonical JVM class-name, so (class …) / instance? / .getMessage /
;; ex-message all reflect the real type.
;; java.text.ParseException carries an int error offset (getErrorOffset). Stored
;; in the record's error-offset field.
(define (jolt-host-throwable class-name msg . more)
  (make-jolt-ex-info-record class-name msg
                             (if (null? more) jolt-nil (car more))
                             jolt-nil 0))

;; throw-jvm: raise a typed JVM throwable by simple class name.
;; (throw-jvm 'NoSuchElementException msg) -> (jolt-throw (jolt-host-throwable
;;   "java.util.NoSuchElementException" msg)). The symbol->FQN table covers the
;; common exception types so call sites read as a bare symbol; an explicit FQN
;; string is also accepted for anything not in the table.
;; An unlisted simple name resolves through the modeled class hierarchy once
;; class-hierarchy.ss loads (it patches this to jch-fqn-of-simple). Until then —
;; during early boot before that file loads — a bare name is the only answer.
(define jvm-throwable-fqn-fallback (lambda (sym) (symbol->string sym)))
(define jvm-throwable-fqn
  (lambda (sym)
    (case sym
      ((IllegalArgumentException) "java.lang.IllegalArgumentException")
      ((IllegalStateException) "java.lang.IllegalStateException")
      ((ArithmeticException) "java.lang.ArithmeticException")
      ((NumberFormatException) "java.lang.NumberFormatException")
      ((UnsupportedOperationException) "java.lang.UnsupportedOperationException")
      ((NoSuchElementException) "java.util.NoSuchElementException")
      ((IndexOutOfBoundsException) "java.lang.IndexOutOfBoundsException")
      ((ClassCastException) "java.lang.ClassCastException")
      ((NullPointerException) "java.lang.NullPointerException")
      ((ArityException) "clojure.lang.ArityException")
      ((IllegalAccessError) "java.lang.IllegalAccessError")
      (else (jvm-throwable-fqn-fallback sym)))))
(define (throw-jvm type msg)
  (jolt-throw (jolt-host-throwable (jvm-throwable-fqn type) msg)))

;; --- host interop ------------------------------------------------------------
;; (.method target arg*) lowers to (jolt-host-call "method" target arg*). JVM
;; interop has no general Chez analog, but the few methods jolt-core's io tier
;; calls map onto Chez equivalents: a writer's .write is a port display; a File's
;; .isDirectory / .listFiles work over a path string (Chez has no File type, so
;; file-seq's File branch is unreachable here — these keep the forms honest). An
;; unsupported method raises rather than silently returning nil.
(define (jolt-host-call method target . args)
  (cond
    ((string=? method "write") (display (car args) target) jolt-nil)
    ((string=? method "isDirectory") (if (file-directory? target) #t #f))
    ((string=? method "listFiles") (list->cseq (directory-list target)))
    (else (error 'jolt-host-call (string-append "unsupported host method: ." method)))))

;; --- var cells: late-bound global roots (Clojure vars) -----------------------
;; A var is a mutable cell keyed by "ns/name". A `:def` sets the root; a `:var`
;; reference reads it at use time (late binding), so a forward/mutually-recursive
;; reference resolves to whatever the cell holds when the call actually runs.
;; declare / (def name) with no init, and a forward var-deref on a not-yet-defined
;; name, reserve a cell whose root is a per-cell unbound sentinel. Per-cell (not a
;; single global) so it names its var like the JVM's Var$Unbound, and every read
;; surface (a plain read, var-get, deref/@) returns the SAME object.
(define-record-type jolt-var-unbound (fields ns name) (nongenerative jolt-var-unbound-v1))
;; `defined?` distinguishes a genuinely interned var (def / declare / a native-op
;; cell) from a cell lazily materialised by a forward `var-deref` / `(var x)` on a
;; not-yet-defined name — `resolve` returns the cell iff defined?.
;; ns-unmap clears it. Avoids the (def x nil) edge of probing the root.
(define-record-type var-cell (fields ns name (mutable root) (mutable defined?)) (nongenerative var-cell-v2))
(define var-table (make-hashtable string-hash string=?))
(define (jolt-var ns name)
  (let ((k (string-append ns "/" name)))
    (or (hashtable-ref var-table k #f)
        (let ((c (make-var-cell ns name (make-jolt-var-unbound ns name) #f)))
          (hashtable-set! var-table k c)
          c))))
;; non-creating lookup (resolve / find-var / ns-unmap): #f when absent, so a
;; probe never interns an empty cell.
(define (var-cell-lookup ns name) (hashtable-ref var-table (string-append ns "/" name) #f))
(define (var-deref ns name) (var-cell-root (jolt-var ns name)))
;; def-var! / declare-var! return the VAR CELL, not the value — Clojure's `def`
;; evaluates to #'ns/name (a first-class var), so (var? (def x 1)) is true and
;; (pr-str (def x 1)) is "#'ns/x". The prelude's def-var! forms discard the
;; return, so this is transparent there.
;; proc -> (ns . name) for the var it was def'd into, so (class a-fn) can report a
;; JVM-style class name and clojure.spec.alpha's fn-sym can recover the symbol of a
;; bare-fn predicate. Weak so GC'd fns drop out. Last def of a given proc wins.
(define proc-name-tbl (make-weak-eq-hashtable))
(define (def-var! ns name v)
  ;; first def of a given proc wins, so an alias like (def inc' inc) — which binds
  ;; the SAME proc to a second var — doesn't rename inc.
  (when (and (procedure? v) (not (hashtable-contains? proc-name-tbl v)))
    (hashtable-set! proc-name-tbl v (cons ns name)))
  (hashtable-set! ns-has-vars-set ns #t)
  (let ((c (jolt-var ns name))) (var-cell-root-set! c v) (var-cell-defined?-set! c #t) c))
;; Set of ns-name strings that have at least one var — makes ns-has-vars? O(1)
;; instead of scanning the entire var-table per require-miss. Updated in def-var!
;; (and wherever vars are removed, though removal is rare).
(define ns-has-vars-set (make-hashtable string-hash string=?))
;; jolt.host/throwable — build a typed throwable a library can throw so (class …),
;; instance?, .getMessage and ex-message all reflect the named JVM class (e.g. an
;; http client throwing java.net.ConnectException). Strictly better than a
;; hand-rolled :jolt/ex-info table, which carries only the class.
(def-var! "jolt.host" "throwable" jolt-host-throwable)
;; var def-time metadata: the :def emit passes the def's reader meta
;; (^:private / ^Type tag / docstring -> {:doc}) here, stored in an eq side-table
;; keyed by the cell. jolt-meta (natives-meta.ss) merges it onto {:ns :name},
;; which it derives from the cell — so EVERY var (plain def, native-op, declare)
;; reports {:ns :name} like Clojure, with the user meta layered on when present.
(define var-meta-table (make-eq-hashtable))
(define jolt-kw-var-ns (keyword #f "ns"))
(define jolt-kw-var-name (keyword #f "name"))
(define jolt-kw-var-macro (keyword #f "macro"))
(define (def-var-with-meta! ns name v m)
  (let ((c (def-var! ns name v))) (hashtable-set! var-meta-table c m) c))
;; A runtime-defined DYNAMIC var (the *earmuffed* core vars): tagged :dynamic so
;; push-thread-bindings accepts it — with no meta entry a var is non-dynamic and
;; binding throws, like the JVM.
(define (def-dynvar! ns name v)
  (def-var-with-meta! ns name v
    (jolt-hash-map (keyword #f "dynamic") #t)))
;; Attach meta to an already-interned var (the declare/no-init emission path:
;; (def ^:dynamic *x*) must be bindable before its root is set).
(define (set-var-meta! ns name m)
  (hashtable-set! var-meta-table (jolt-var ns name) m))
;; runtime-macro registry: a var whose root holds a macro
;; expander fn is flagged here, so the ON-CHEZ analyzer's form-macro?/form-expand-1
;; (host-contract.ss) expand it. The prelude emits each core/stdlib defmacro as a
;; def-var! of its (cross-compiled) expander followed by (mark-macro! ns name).
;; Keyed by cell (eq), like var-meta-table — survives a later (def name ...) that
;; replaces the expander but keeps the same cell, matching Clojure (a defmacro IS a
;; def whose var carries :macro).
(define var-macro-table (make-eq-hashtable))
(define (mark-macro! ns name)
  (let ((c (jolt-var ns name))) (hashtable-set! var-macro-table c #t) c))
(define (macro-var? cell) (and cell (hashtable-ref var-macro-table cell #f) #t))
;; declare / (def name) with no init: reserve the cell ONLY if absent. An
;; existing root is left intact — Clojure's (def x) with no init does not clobber
;; a prior binding (do (def x 7) (def x) x) => 7. Returns the cell either way.
(define (declare-var! ns name)
  (let ((k (string-append ns "/" name)))
    (or (hashtable-ref var-table k #f)
        (let ((c (make-var-cell ns name (make-jolt-var-unbound ns name) #t)))  ; declared => interned/resolvable
          (hashtable-set! var-table k c)
          c))))

;; regex: defines regex-t + the re-* fns (def-var!'d into
;; clojure.core), so it loads after def-var! and before the printer below (which
;; renders a regex-t as #"source").
(load "host/chez/regex-translate.ss")
(load "host/chez/regex.ss")

;; atoms: host-coupled mutable cells; def-var!'d into clojure.core
;; (atom/deref/swap!/reset! + the compare/vals kernel). Loads after def-var! and
;; jolt-invoke (seq.ss) / jolt= (values.ss) / jolt-vector (collections.ss).
(load "host/chez/atoms.ss")

;; refs: Clojure refs and serialized transactions (STM).  Loaded after atoms
;; (shares the IRef seam and jolt-deref); must load before loader.ss (wires
;; *loaded-libs*) and before concurrency.ss (which chains jolt-deref further).
(load "host/chez/refs.ss")

;; type predicates + simple accessors: seed natives the overlay
;; assumes (map?/vector?/nil?/number?/.../name/namespace), def-var!'d into
;; clojure.core. Loads after the value-model record predicates they wrap.
(load "host/chez/predicates.ss")

;; --- jolt number printing ----------------------------------------------------
;; jolt has a numeric tower (exact integer / ratio / double, distinguished by
;; class). Exact integer-valued values print without a ".0" ((+ 1 2) -> "3");
;; a double prints with one ((* 1.0 5) -> "5.0", as the JVM does).

;; Double.toString layout: plain decimal when 1e-3 <= |x| < 1e7, otherwise
;; scientific d.dddE±x with one digit before the point; the mantissa always
;; carries a decimal point ("1.0E100", "2.3E-4", "1.2345678E7"). Chez's
;; shortest-round-trip digits are kept; only the layout is rearranged.
(define (jolt-flonum->string x)
  (let* ((s (number->string x))
         (neg? (char=? (string-ref s 0) #\-))
         (body0 (if neg? (substring s 1 (string-length s)) s))
         ;; Chez appends a "|prec" suffix to subnormal strings (e.g. "5e-324|1").
         ;; Strip it before the exponent substring is parsed, else string->number
         ;; misreads "-324|1" as a precision-qualified flonum (-256.0) and corrupts
         ;; the value.
         (bar (let loop ((i 0))
                (cond ((fx>=? i (string-length body0)) #f)
                      ((char=? (string-ref body0 i) #\|) i)
                      (else (loop (fx+ i 1))))))
         (body (if bar (substring body0 0 bar) body0))
         (blen (string-length body))
         (epos (let loop ((i 0))
                 (cond ((fx>=? i blen) #f)
                       ((memv (string-ref body i) '(#\e #\E)) i)
                       (else (loop (fx+ i 1))))))
         (mant (if epos (substring body 0 epos) body))
         (eexp (if epos (string->number (substring body (fx+ epos 1) blen)) 0))
         (mlen (string-length mant))
         (dot (let loop ((i 0))
                (cond ((fx>=? i mlen) #f)
                      ((char=? (string-ref mant i) #\.) i)
                      (else (loop (fx+ i 1))))))
         (digits (if dot
                     (string-append (substring mant 0 dot) (substring mant (fx+ dot 1) mlen))
                     mant))
         (point (+ (if dot dot mlen) eexp)))
    ;; normalize: drop leading zeros (adjusting the point), then trailing zeros
    (let* ((dlen0 (string-length digits))
           (lead (let loop ((i 0))
                   (if (and (fx<? i (fx- dlen0 1)) (char=? (string-ref digits i) #\0))
                       (loop (fx+ i 1)) i)))
           (digits (substring digits lead dlen0))
           (point (- point lead))
           (dlen (let loop ((i (string-length digits)))
                   (if (and (fx>? i 1) (char=? (string-ref digits (fx- i 1)) #\0))
                       (loop (fx- i 1)) i)))
           (digits (substring digits 0 dlen))
           (res (cond
                  ((string=? digits "0") "0.0")
                  ((and (>= point -2) (<= point 7))   ; 1e-3 <= |x| < 1e7
                   (cond
                     ((<= point 0)
                      (string-append "0." (make-string (- point) #\0) digits))
                     ((>= point dlen)
                      (string-append digits (make-string (- point dlen) #\0) ".0"))
                     (else (string-append (substring digits 0 point) "."
                                          (substring digits point dlen)))))
                  (else
                   (string-append (substring digits 0 1) "."
                                  (if (fx>? dlen 1) (substring digits 1 dlen) "0")
                                  "E" (number->string (- point 1)))))))
      (if neg? (string-append "-" res) res))))

(define (jolt-num->string x)
  (cond
    ;; the -e / element printer renders the infinities and NaN in READABLE form
    ;; (##Inf reads back, like Clojure's REPL/pr); str/print uses "Infinity"/"NaN"
    ;; (see jolt-str-render-one in converters.ss).
    ((and (flonum? x) (fl= x +inf.0)) "##Inf")
    ((and (flonum? x) (fl= x -inf.0)) "##-Inf")
    ((and (flonum? x) (not (fl= x x))) "##NaN")
    ;; str of a bigint has NO N suffix (BigInt.toString); only the readable
    ;; printer adds it (see jolt-pr-readable-base).
    ((and (exact? x) (integer? x)) (number->string x))
    ((flonum? x) (jolt-flonum->string x))
    (else (number->string x))))
;; true when an exact integer prints with the BigInt N suffix under pr.
;; number? first — Chez's exact? raises on a non-number, and the readable
;; printer probes every value through this.
(define (jolt-bigint-print? x)
  (and (number? x) (exact? x) (integer? x)
       (or (> x 9223372036854775807) (< x -9223372036854775808))))

;; Program-final-value printer. jolt's `-e` prints in str-style: strings raw (no
;; quotes), chars as `\c`/`\newline`, collections recursively. NOTE: maps/sets
;; render in HAMT-iteration order, which is not a stable insertion order —
;; so unordered values are compared via `=` (true/false), not printed form.
(define (jolt-str-join strs)
  (cond ((null? strs) "") ((null? (cdr strs)) (car strs))
        (else (string-append (car strs) " " (jolt-str-join (cdr strs))))))
;; map ENTRIES join with ", " like the reference printer: {:a 1, :b 2}
(define (jolt-str-join-comma strs)
  (cond ((null? strs) "") ((null? (cdr strs)) (car strs))
        (else (string-append (car strs) ", " (jolt-str-join-comma (cdr strs))))))
(define (jolt-char->string c)
  (if (jolt-truthy? (jolt-var-get (jolt-var "clojure.core" "*print-readably*")))
      (string-append "\\" (case c ((#\newline) "newline") ((#\space) "space") ((#\tab) "tab")
                                  ((#\return) "return") ((#\backspace) "backspace") ((#\page) "formfeed")
                                  (else (string c))))
      (string c)))
;; Program-final printer: jolt's `-e` is str-style at the top level, where a
;; bare nil renders as the empty string (a nil ELEMENT inside a collection still
;; prints "nil", which jolt-pr-str handles).
(define (jolt-final-str x) (if (jolt-nil? x) "" (jolt-pr-str x)))
;; --- *print-level* / *print-length* -----------------------------------------
;; Both vars default to nil (= unlimited). A non-nil number limits collection
;; nesting depth / element count in BOTH printers (jolt-pr-str here and
;; jolt-pr-readable in printing.ss). Cells captured lazily — the vars are def'd
;; after rt.ss. The nil default takes a fast path: jolt-print-hash? is #f and the
;; limited-string walkers never truncate.
(define plevel-cell #f)
(define plength-cell #f)
(define (jolt-print-level)
  (unless plevel-cell (set! plevel-cell (jolt-var "clojure.core" "*print-level*")))
  (let ((v (jolt-var-get plevel-cell))) (and (number? v) v)))
(define (jolt-print-length)
  (unless plength-cell (set! plength-cell (jolt-var "clojure.core" "*print-length*")))
  (let ((v (jolt-var-get plength-cell))) (and (number? v) v)))
(define jolt-print-depth (make-thread-parameter 0))
;; A collection at depth >= *print-level* renders as "#". The top-level collection
;; is depth 0, so *print-level* 0 collapses any collection, 1 keeps the outermost.
(define (jolt-print-hash?)
  (let ((lvl (jolt-print-level))) (and lvl (fx>=? (jolt-print-depth) lvl))))
;; Rendered element strings of a vector (by index), honoring *print-length*: at
;; most N, then "...". render-one runs at the current (already bumped) depth.
(define (jolt-limited-vec-strs x render-one)
  (let ((len (pvec-count x)) (lim (jolt-print-length)))
    (let loop ((i 0) (acc '()))
      (cond ((fx>=? i len) (reverse acc))
            ((and lim (fx>=? i lim)) (reverse (cons "..." acc)))
            (else (loop (fx+ i 1) (cons (render-one (pvec-nth-d x i jolt-nil)) acc)))))))
;; Rendered element strings of a seq, walked lazily so an infinite seq is realized
;; only up to *print-length*.
(define (jolt-limited-seq-strs s render-one)
  (let ((lim (jolt-print-length)))
    (let loop ((s s) (i 0) (acc '()))
      (cond ((jolt-nil? s) (reverse acc))
            ((and lim (fx>=? i lim)) (reverse (cons "..." acc)))
            (else (loop (jolt-seq (seq-more s)) (fx+ i 1) (cons (render-one (seq-first s)) acc)))))))
;; Truncate an already-collected element-string list (set / map, finite) to
;; *print-length*, appending "..." when more remain.
(define (jolt-limited-list-strs strs)
  (let ((lim (jolt-print-length)))
    (if (not lim) strs
        (let loop ((s strs) (i 0) (acc '()))
          (cond ((null? s) (reverse acc))
                ((fx>=? i lim) (reverse (cons "..." acc)))
                (else (loop (cdr s) (fx+ i 1) (cons (car s) acc))))))))
;; bump the print depth around a collection's element rendering — but only when
;; *print-level* is set, since depth is consulted only to enforce it. With the
;; common nil default this is a plain begin, so printing pays no parameterize.
(define-syntax with-deeper-print
  (syntax-rules ()
    ((_ body ...) (if (jolt-print-level)
                      (parameterize ((jolt-print-depth (fx+ (jolt-print-depth) 1))) body ...)
                      (begin body ...)))))

;; A host shim registers a type's str-style rendering via register-pr-str-arm! (or
;; register-pr-arm! in printing.ss for both printers at once) instead of
;; set!-wrapping jolt-pr-str. Disjoint types, checked before the base cases.
(define jolt-pr-str-arms '())
(define (register-pr-str-arm! pred render)
  (set! jolt-pr-str-arms (cons (cons pred render) jolt-pr-str-arms)))
(define (jolt-pr-str-base x)
  (cond
    ((jolt-nil? x) "nil")
    ((eq? x #t) "true")
    ((eq? x #f) "false")
    ((number? x) (jolt-num->string x))
    ((string? x) x)
    ((char? x) (jolt-char->string x))
    ((keyword? x) (let ((ns (keyword-t-ns x)))
                    (if ns (string-append ":" ns "/" (keyword-t-name x)) (string-append ":" (keyword-t-name x)))))
    ((jolt-symbol? x) (let ((ns (symbol-t-ns x)))
                        (if (or (jolt-nil? ns) (not ns) (eq? ns '())) (symbol-t-name x)
                            (string-append ns "/" (symbol-t-name x)))))
    ((regex-t? x) (string-append "#\"" (regex-t-source x) "\""))
    ((pvec? x) (if (jolt-print-hash?) "#"
                   (with-deeper-print
                     (string-append "[" (jolt-str-join (jolt-limited-vec-strs x jolt-pr-str)) "]"))))
    ((pset? x) (if (jolt-print-hash?) "#"
                   (with-deeper-print
                     (string-append "#{" (jolt-str-join (jolt-limited-list-strs
                       (pset-fold x (lambda (e a) (cons (jolt-pr-str e) a)) '()))) "}"))))
    ((pmap? x) (if (jolt-print-hash?) "#"
                   (with-deeper-print
                     (string-append "{" (jolt-str-join-comma (jolt-limited-list-strs
                       (pmap-fold x (lambda (k v a) (cons (string-append (jolt-pr-str k) " " (jolt-pr-str v)) a)) '()))) "}"))))
    ;; lists / cons / lazy seqs all print as (...) — forces a finite seq (or up to
    ;; *print-length* of an infinite one).
    ((empty-list-t? x) (if (jolt-print-hash?) "#" "()"))
    ((cseq? x) (if (jolt-print-hash?) "#"
                   (with-deeper-print
                     (string-append "(" (jolt-str-join (jolt-limited-seq-strs x jolt-pr-str)) ")"))))
    (else (format "~a" x))))
(define (jolt-pr-str x)
  (let loop ((as jolt-pr-str-arms))
    (cond ((null? as) (jolt-pr-str-base x))
          (((caar as) x) ((cdar as) x))
          (else (loop (cdr as))))))

;; converters + string ops: str/subs/vec/keyword/symbol/compare/int/
;; double/gensym — host-coupled seed natives def-var!'d into clojure.core. Loaded
;; LAST because `str` reuses jolt-pr-str (defined just above).
(load "host/chez/converters.ss")

;; transients: copy-on-write transient collections + persistent disj;
;; extends get/count/contains? to see through a transient. After collections.ss
;; (the persistent ops it delegates to).
(load "host/chez/transients.ss")

;; seq-native shims: mapcat/take-while/drop-while/partition/sort +
;; reduced/reduced?/identical? — seed-native fns the overlay assumes are core
;; natives. Over the seq layer + jolt-compare, so loaded after converters.ss.
(load "host/chez/natives-seq.ss")

;; readable printer + output seams: __pr-str1/__write/
;; __with-out-str/__eprint/__eprintf — the host seams the overlay print family
;; (pr-str/pr/prn/print/println/*-str) is built on. After converters.ss (uses
;; jolt-pr-str/jolt-str-join) + seq.ss (jolt-invoke).
(load "host/chez/printing.ss")

;; collection constructors + rand: bind the public
;; clojure.core names hash-map/hash-set/array-map/set/rand to the existing
;; pmap/pset ctors. After collections.ss (the ctors) + seq.ss (seq->list).
(load "host/chez/natives-coll.ss")

;; bit ops + parse-long/parse-double: host-coupled scalar
;; seed natives over the all-flonum number model.
(load "host/chez/natives-num.ss")

;; multimethods: defmulti/defmethod dispatch runtime. Needs jolt-invoke
;; (seq.ss), jolt=/key-hash/jolt-hash-map (collections.ss), jolt-atom? (atoms.ss),
;; jolt-pr-str (above), and the var-cell machinery — so loaded last.
(load "host/chez/multimethods.ss")

;; the single JVM class/interface graph — value-host-tags, instance?, isa?/supers,
;; and the exception hierarchy all derive from it. Before records.ss so
;; value-host-tags can build on jch-tags.
(load "host/chez/java/class-hierarchy.ss")

;; records + protocols: defrecord/deftype/defprotocol/
;; extend-type/reify. A jrec record type set!-extended into the collection
;; dispatchers + a protocol registry. After multimethods.ss (chez-current-ns) and
;; the dispatchers/printers it wraps (collections/seq/values/converters/printing/
;; transients).
(load "host/chez/records.ss")
(load "host/chez/java/records-interop.ss")   ; exception hierarchy + instance-check taxonomy

;; metadata: meta / with-meta over an identity-keyed
;; side-table. After records.ss (jrec) + the collection ctors it copies.
(load "host/chez/natives-meta.ss")

;; host class tokens: bare class names (String/Keyword/File...) ->
;; canonical JVM class-name strings + (class x). After natives-meta.ss (jolt-type)
;; and the printer (jolt-str-render-one).
(load "host/chez/java/host-class.ss")

;; dynamic vars: *clojure-version* / *unchecked-math* constants the host
;; binds natively. After collections.ss (jolt-hash-map) + def-var!.
(load "host/chez/dynamic-var-defaults.ss")

;; host tables + sorted collections: jolt.host/tagged-table/
;; ref-put!/ref-get + the 25-sorted tier's runtime (sorted-map/sorted-set routed
;; through their :ops table). Loaded LAST — wraps the jrec-extended dispatchers
;; (records.ss), jolt-disj (transients.ss), and value-host-tags (records.ss).
(load "host/chez/host-table.ss")

;; lazy-seq bridge: make-lazy-seq / coll->cells over the
;; cseq model — unblocks every overlay fn built on the lazy-seq macro (repeat/
;; iterate/cycle/dedupe/take-nth/keep/interpose/reductions/tree-seq/lazy-cat).
;; Loaded LAST so %ls-seq captures the fully-extended (sorted-aware) jolt-seq.
(load "host/chez/lazy-bridge.ss")

;; transducer surface: native volatile boxes, cat, +
;; the transduce/sequence entry points over into-xform/reduce-seq. After
;; natives-seq.ss (into-xform), seq.ss (reduce-seq) + atoms.ss (deref).
(load "host/chez/natives-transduce.ss")

;; vars as first-class objects: var?/var-get/deref/invoke/=/
;; pr-str over the rt.ss var-cell. After natives-transduce.ss (chains deref) + the
;; printers. emit lowers :the-var to (jolt-var ns name).
(load "host/chez/vars.ss")

;; misc scalar natives: UUID (random-uuid/parse-uuid/uuid?), format/
;; printf, tagged-literal, bigint. After the printers + converters (str/pr-str of
;; a uuid). Overlay names (uuid?/random-uuid/parse-uuid/tagged-literal?) re-asserted
;; in post-prelude.ss.
(load "host/chez/natives-misc.ss")

;; format / printf: the %-directive engine. After natives-misc.ss + converters.ss
;; (jolt-str-render-one).
(load "host/chez/natives-format.ss")

;; namespaces: the namespace value model — find-ns/ns-name/
;; all-ns/the-ns/create-ns/in-ns/ns-publics/ns-map/ns-interns/ns-aliases/resolve/
;; find-var/ns-unmap/*ns*, over the var-table + chez-current-ns. Loaded LAST: needs
;; var-cell + var-cell-defined?, jolt-symbol/jolt-hash-map/jolt-assoc, chez-current-ns
;; (multimethods.ss), list->cseq (seq.ss), and the fully-patched printers (vars.ss).
(load "host/chez/ns.ss")

;; dynamic var binding: the per-thread binding stack +
;; push/pop/get-thread-bindings/__thread-bound?/var-set/alter-var-root/__local-var.
;; Chains var-deref (rt.ss) and jolt-var-get (vars.ss) onto the stack, so a `binding`
;; frame is seen by every var read. Loaded LAST: needs the fully-extended var-read
;; paths + jolt-hash-map/pmap-fold/pmap-assoc (collections.ss).
(load "host/chez/dyn-binding.ss")

;; java.lang.String method interop: jolt-string-method, the
;; portable String/CharSequence surface record-method-dispatch falls through to on
;; a string target. After regex.ss (jolt-re-pattern/regex-t-irx) + records.ss
;; (which references jolt-string-method).
(load "host/chez/java/natives-str.ss")

;; host class statics + constructors: host-static-ref/
;; host-static-call/host-new + the jhost method registry. Loads LAST — it extends
;; record-method-dispatch (records.ss) and reuses natives-str helpers (str-trim,
;; ascii-string-down, re-split, str-split-drop-trailing) + the regex-t accessors.
(load "host/chez/java/host-static.ss")          ; registries + jhost + coercion helpers
(load "host/chez/java/host-static-methods.ss")  ; Class/member static methods + fields
(load "host/chez/java/host-static-classes.ss")  ; instantiable host object classes
(load "host/chez/java/byte-buffer.ss")          ; java.nio.ByteBuffer over a byte-array

;; generic dot-form dispatch: field access + map/vector member access
;; for the `.` / `.-field` desugar. Loads after host-static.ss so it wraps every
;; record-method-dispatch arm (jhost/number/regex/jrec/string) and falls through.
(load "host/chez/java/dot-forms.ss")

;; java.io.File + host file I/O: path-backed jfile record, slurp/spit/
;; flush, file-seq dir primitives, clojure.java.io/file. Loads LAST so its jfile
;; arm wraps the fully-built record-method-dispatch and the str/type/instance-check
;; extensions sit over every prior shim.
(load "host/chez/java/io.ss")
(load "host/chez/java/nio-file.ss")             ; java.nio.file: Path / Paths / PathMatcher

;; #inst values + java.time formatting: jinst (RFC3339 ms) +
;; DateTimeFormatter/Instant/ZoneId/LocalDateTime/FormatStyle/Locale/Date. Loads
;; LAST — it extends record-method-dispatch / jolt-get / jolt= / jolt-hash /
;; jolt-pr-str / jolt-type / instance-check and uses host-static.ss's registries.
;; libc time primitives (zone offset, locale names) exposed as jolt.host vars.
;; The java.time.* implementation is the jolt-lang/time library (portable Clojure);
;; these are the two things it can't express without libc.
(load "host/chez/java/tz-primitives.ss")
(load "host/chez/java/inst-time.ss")

;; The full java.time.* surface is the jolt-lang/time library — portable Clojure
;; over the __register-class-* seams and the tz-primitives above. Core keeps only
;; the #inst / java.util.Date layer (inst-time.ss).

;; Chez-side data reader: read-string / __parse-next /
;; __read-tagged. Loads after inst-time.ss — __read-tagged reuses its #uuid/#inst
;; constructors, and the reader needs the full value/collection layer above.
(load "host/chez/reader.ss")

;; clojure.math: native flonum-math shims def-var!'d into the
;; clojure.math ns. Self-contained (only def-var! + Chez math), order-independent.
(load "host/chez/java/math.ss")

;; reader/macro runtime support: #?() feature set, reader-conditional + re-matcher
;; tagged-map ctors, macroexpand. After ns.ss; macroexpand call-time-refs the macro
;; table (host-contract) + analyzer ctx.
(load "host/chez/natives-reader.ss")

;; Java-style arrays: object/typed array constructors + a jolt-array
;; backing; extends count/nth/seq/get/ref-put! so the overlay aget/aset/alength see
;; it. After the dispatchers it chains.
(load "host/chez/java/natives-array.ss")

;; java.io byte/char streams (FileInputStream/…/ByteArrayOutputStream/Buffered*)
;; over Chez ports. After io.ss (extends its slurp/__close/reader-jhost?) and
;; natives-array.ss (the byte-array <-> bytevector bridge).
(load "host/chez/java/io-streams.ss")

;; clojure.lang.PersistentQueue: a functional queue + EMPTY static.
;; Chains seq/count/empty?/peek/pop/conj/sequential?/class/instance?/printer, so
;; load after natives-array (the dispatchers it extends).
(load "host/chez/java/natives-queue.ss")

;; syntax-quote form builders: __sqcat/__sqvec/__sqmap/__sqset/
;; __sq1, def-var!'d into clojure.core. A cross-compiled macro expander (analyzer
;; on Chez) calls these to build its expansion as reader forms. Needs the
;; collection/seq layer + def-var!; order-independent past those.
(load "host/chez/syntax-quote.ss")

;; concurrency: real OS-thread futures + blocking promises, shared-heap
;; (JVM) semantics. Loaded LAST — chains the fully-built jolt-deref and conveys the
;; thread-local binding stack (dyn-binding.ss) into workers. pmap/pcalls/pvalues
;; (overlay, over `future`) light up once future-call exists here.
(load "host/chez/java/concurrency.ss")

;; clojure.core.async: real-thread blocking channels + go/go-loop/
;; thread macros, def-var!'d into clojure.core.async. After concurrency.ss (reuses
;; ms->duration) and the collection/seq layer.
(load "host/chez/java/async.ss")

;; BigDecimal: the jbigdec value type + bigdec/decimal?/class/equality/
;; printing. Loads LAST so its set!-wraps of jolt-class/jolt=2/the printers sit
;; outermost over every earlier extension.
(load "host/chez/java/bigdec.ss")

;; Native stack traces: jv$ns$name -> source registry + continuation frame walk +
;; uncaught-throwable renderer. After the printers/equality it relies on.
(load "host/chez/source-registry.ss")
