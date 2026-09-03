;; rt-core.ss — the Gambit target's kernel: the port of host/chez/rt.ss's
;; kernel surface to native gsi (Gambit 4.9.7). G2 (jolt-mj95.3).
;;
;; PORT-LOG (every rt.ss region: ported / skipped):
;;   ported  rt arithmetic/logic shims (jolt-inc/dec/->fx/->fl/not) [151-175]
;;   ported  ex-info record type [177-187]
;;   ported  exceptions: jolt-throw-cont/sitep, jolt-catch-complete!,
;;           jolt-throw/jolt-throw-message/jolt-unwrap-throw, jolt-ex-info,
;;           jolt-host-throwable, throw-jvm, jolt-capture-fault! [189-211, 366-447]
;;           -- ADAPTED: Chez's compound-condition raise (make-message-condition +
;;           define-condition-type &jolt-throw) becomes a tagged (cons 'jolt-throw v);
;;           guard still catches it and jolt-unwrap-throw recovers v. No manifest
;;           file reads the condition surface, only raises and catches.
;;   ported  tail-frame recovery vregs [213-306] -- via the virtual-register/
;;           set-virtual-register! shims in prelude-shims.ss (one parameter per
;;           claimed slot; Gambit has no vregs). jolt-vreg-site/catch-line/
;;           print-readably keep their numbers.
;;   skipped compile-time callsite tables (jolt-callsite-*, jolt-register-callsite!)
;;           [308-364] -- consumed only by source-registry.ss (excluded from G2).
;;   ported  host interop jolt-host-call [449-461] -- record-method-dispatch
;;           resolves at call time (records.ss); directory-list is Gambit-native.
;;   ported  var cells: jolt-var, var-cell-lookup, var-deref, def-var!,
;;           def-var-with-meta!, def-dynvar!, set-var-meta!, mark-macro!,
;;           macro-var?, declare-var!, the var-meta/macro tables, proc-name-tbl,
;;           ns-has-vars-set [463-524, 582-630]
;;   skipped telemetry clocks/memory (jolt-wall-nanos/jolt-mono-nanos, the
;;           sa-stats def-vars, machine-type/scheme-version) [526-580] -- Chez
;;           time objects; no manifest consumer. The jolt.host/throwable and
;;           available-processors def-vars ARE kept.
;;   ported  jolt number printing [652-759] -- verbatim (Chez's number->string
;;           layout is what jolt re-arranges; Gambit's number->string for a
;;           flonum prints the same shortest-round-trip digits).
;;   ported  *print-level*/*print-length* + with-deeper-print [760-812]
;;   ported  pr-str fast path + arms + #object fallback [813-924]
;;   ported  randomness [947-986] -- ADAPTED: no lazy clock-seed; Gambit's
;;           default random source is process-seeded, so jolt-random is
;;           (random-integer n) directly.
;;   skipped OS entropy (jolt-entropy-source + the /dev/urandom / Windows
;;           paths) [987-1065] -- ADAPTED: jolt-random-bytes fills a bytevector
;;           with (random-integer 256) per byte. Divergence, documented: not a
;;           CSPRNG; uniqueness holds, unpredictability does not.
;;   skipped jolt-foreign-proc-safe macro [52-69] -- a procedural transformer
;;           needing with-syntax in its body (Gambit cannot see top-level macros
;;           there); its only users were the cpu-count region, which is skipped.
;;   skipped cpu-count region [71-133] -- jolt-available-processors returns 1.
;;   skipped the (load "host/chez/...") orchestration -- boot.ss splices this
;;           file; the manifest is boot.ss's job.
;;
;; Sits above the value model (values.ss) and below an emitted program. Adds the
;; two things the back end's output references that aren't in the value layer:
;;   1. the var-cell late-binding registry (Clojure vars — a global root that a
;;      reference reads at call time, so redefinition / mutual recursion work);
;;   2. the rt primitive shims the emitter names (jolt-inc/dec/not) and jolt's
;;      number printing (all jolt numbers model Clojure doubles; integer-valued
;;      print without a trailing ".0").
;; Emitted programs do `(load "host/chez/rt.ss")`; this loads values.ss in turn.

;; Chez-compat preamble — must precede EVERYTHING else in the runtime bring-up
;; (every load path enters through this file). Two adaptations for vendored
;; portable R[457]RS code (vendor/irregex) and for host code generally:
;; a cond-expand at expression position (Chez's is library-only), and `error`
;; called with a lone string (Chez's error wants who+msg). Both are normalized
;; without changing behavior for standard-shape calls.
(define-syntax cond-expand
  (syntax-rules (else)
    ((_ (else e ...)) (begin e ...))
    ((_ (else e ...) c ...) (begin e ...))
    ((_ (req e ...) c ...) (cond-expand c ...))
    ((_) (if #f #f))))
(define %chez-error error)
(define (error . args)
  (if (and (pair? args) (string? (car args)))
      (apply %chez-error #f args)
      (apply %chez-error args)))

;; The scheme-adapter runtime loads FIRST: top-levels below (and in the
;; java/*.ss loads) call sa-* entry points. rt.ss owning this load makes every
;; loader of rt.ss — boot scripts, gate harnesses, emitted programs — correct
;; by construction; a boot file loading it earlier is a harmless re-define.
;; SKIPPED on the gambit target: jolt-foreign-proc-safe (a procedural
;; transformer whose body needs with-syntax — Gambit cannot see top-level
;; macros there) and the whole cpu-count region, which was its only user.
;; jolt-available-processors: this target reports 1 usable processor (G2
;; divergence; pmap look-ahead windows read it).
(define (jolt-available-processors) 1)

;; --- version ------------------------------------------------------------------
;; One source of truth for the jolt version string, read by jolt.host/jolt-version
;; (loader.ss), (System/getProperty "jolt.version"), and clojure.core/*jolt-version*
;; (dynamic-var-defaults.ss). A self-contained binary bakes the release tag by
;; emitting (define jolt-baked-version-early "…") at the TOP of flat.ss
;; (build-jolt.ss) — early so every consumer that loads later sees it. A dev run
;; has no baked define and falls back to $JOLT_VERSION (bin/jolt sets it from
;; `git describe`), then "dev".
(define (jolt-version-string)
  (or (sa-baked-global 'jolt-baked-version-early)
      (let ((v (getenv "JOLT_VERSION"))) (and v (> (string-length v) 0) v))
      "dev"))

;; --- rt arithmetic / logic shims (named in the emitter's native-ops) ----------
(define (jolt-inc x) (+ (jolt-need-num x) 1))
(define (jolt-dec x) (- (jolt-need-num x) 1))
;; longCast/doubleCast coerce a primitive-hinted parameter or return: ^long
;; truncates a flonum toward zero and passes an exact integer through, ^double
;; widens any number. The hint is a promise the body's fx*/fl* ops rely on, so a
;; value outside the tower is the JVM's cast failure (ClassCastException, or NPE
;; for nil) — a raw host error would escape with no class for a catch to select.
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
        (else (jolt-num-cast-throw x))))
(define (jolt->fl x)
  (cond ((flonum? x) x)
        ((number? x) (exact->inexact x))
        (else (jolt-num-cast-throw x))))
;; jolt `not`: only nil and false are falsey.
;; Mirrors rt.ss's spliced jolt-not (see values.ss jolt-nil? for why these are
;; macros); rt-core.ss is the Gambit port of rt.ss's kernel, so the shape has to
;; match or the Gambit host keeps paying the call.
(define (jolt-not-fn x) (if (jolt-truthy? x) #f #t))
(define-syntax jolt-not
  (syntax-rules ()
    ((_ e) (if (jolt-truthy? e) #f #t))
    ((_ e ...) (jolt-not-fn e ...))))

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
;; The site vreg's ('ns/fn' . line) pair as it stood AT the raise (R2, bead
;; jolt-knn8). Only tail sites write the vreg, so its live value can be a
;; returned call's residue — the reporter must see the throw-time snapshot,
;; validated against the callsite tables, never the live slot.
(define jolt-throw-sitep (make-thread-parameter #f))

;; Cleared after a catch handler completes normally so the parked continuation
;; (and its captured frames) does not root live data until the next throw.
(define (jolt-catch-complete!)
  (jolt-throw-cont #f) (jolt-throw-sitep #f))

;; --- tail-frame recovery: compile-time tables + one vreg store (R4) ----------
;; TCO erases tail-called frames from the native continuation, so an uncaught
;; error's backtrace shows only the surviving non-tail spine — the immediate
;; error site is often a tail call and is missing. When tracing is enabled
;; (JOLT_TRACE, wired in compile-eval.ss), the emitter instruments TAIL CALL
;; SITES ONLY, and the instrumentation is ONE virtual-register store of a
;; static ('ns/fn' . line) pair — no allocation, no marks, no per-entry work.
;; Erased chains are reconstructed at REPORT time from the callsite tables
;; (above): backward from the throw-time pair through unambiguous tail-entry
;; edges, forward from each live frame's registered callee through unambiguous
;; tail exits. Two earlier designs died here: the R0 ring push cost ~10x on
;; call-heavy code, and the R1-R3 continuation-marks ribs ballooned the heap
;; to tens of GB on delegation-heavy code (rewrite-clj's zipper suite: 40s ->
;; 300s timeout; every hop's mark op allocated). What ships is the JVM model:
;; all metadata compile-time, the stack itself the only runtime record.
(define jolt-trace-on? #f)
;; Per-thread trace state lives in Chez VIRTUAL REGISTERS, not thread parameters:
;; a thread-parameter WRITE costs ~32ns against ~1ns for a virtual-register write
;; (measured, Chez 10.4 arm64), and jolt-site! sits on the hot path. Reads are
;; ~2.4ns vs ~3.3ns, a smaller but free win.
;;
;; Virtual registers are a fixed global resource: (virtual-register-count) slots for
;; the whole process (16 on every platform jolt targets). jolt claims three, allocated
;; here so the assignment is in one place; nothing else in the runtime uses them.
;; A freshly forked thread starts every slot at fixnum 0, NOT #f, so "unset" means
;; fixnum 0 (the site slots hold a site pair or 0). Slots 0 and 1 are FREE since
;; R3 (jolt-230w) removed the R1 ring/mark vregs — the tail marks live on the
;; continuation, not in a vreg — so new virtual-register users should claim them
;; before renumbering anything. The surviving slots keep their R2 numbers.
(define jolt-vreg-site 2)        ; ('ns/fn' . line) of the innermost live call site
(define jolt-vreg-catch-line 3)  ; the site at the throw a catch clause is handling
(define jolt-vreg-print-readably 4)  ; the print family's *print-readably* override; 0 = unset
;; Effective *print-readably* for the readable renderer's string/char cases. The
;; print family stashes its override in the slot above — a virtual-register write
;; is ~1ns vs a pmap alloc + fold + two thread-parameter writes per dynamic
;; binding — and only consults the var when the slot is unset. A fresh thread
;; starts the slot at fixnum 0, so a stored #f (readably off) stays distinct from
;; "unset"; jolt-var/-get resolve at call time (vars.ss / rt.ss load later).
;; Resolved once and reused: jolt-var rebuilds "clojure.core/*print-readably*"
;; with string-append and hashes it on every call, which on a per-item path is
;; the whole cost of the lookup. The cell is stable under redefinition —
;; def-var! mutates the root in place — and jolt-var-get still consults the
;; binding stack, so a (binding [*print-readably* …]) is honoured. Lazily, since
;; vars.ss/rt.ss finish loading after this point. Same shape as pm-cell
;; (io-streams.ss) and the cells the backend emits for compiled code.
(define pr-readably-cell #f)
(define (jolt-pr-readable?)
  (let ((o (virtual-register jolt-vreg-print-readably)))
    (if (eq? o 0)
        (begin
          (unless pr-readably-cell
            (set! pr-readably-cell (jolt-var "clojure.core" "*print-readably*")))
          (jolt-truthy? (jolt-var-get pr-readably-cell)))
        (jolt-truthy? o))))
(define (jolt-trace-enable!) (set! jolt-trace-on? #t))

;; TAIL call emissions store their static ('ns/fn' . line) pair here right
;; before the call (sited-tail-call / the :throw case). Since R2 (bead
;; jolt-knn8) NOTHING else writes it — non-tail code carries no per-call
;; instrumentation — so the live slot can hold a returned chain's residue.
;; Consumers therefore read the THROW-TIME snapshot (jolt-throw-sitep), and the
;; reporter validates it against the callsite table before splicing.
(define (jolt-site! p) (set-virtual-register! jolt-vreg-site p))
;; The line to report for the INNERMOST frame. Inside a catch clause that is the
;; line the throw came from, snapshotted on the way in; else the pair stashed at
;; the raise. Never the live vreg — it can be stale between throws.
(define (jolt-throw-line)
  (let ((c (virtual-register jolt-vreg-catch-line)))
    (if (pair? c)
        (let ((l (cdr c))) (and (fixnum? l) (fx>? l 0) l))
        (let ((s (jolt-throw-sitep)))
          (if (pair? s)
              (let ((l (cdr s))) (and (fixnum? l) (fx>? l 0) l))
              #f)))))
;; The site pair ('ns/fn' . line) of the innermost call at the throw — the
;; catch-line snapshot when a handler is running, else the raise-time stash.
;; #f when unset. The reporter must validate this against the callsite table
;; (jolt-callsite-callee) before trusting the name.
(define (jolt-throw-site)
  (let ((c (virtual-register jolt-vreg-catch-line)))
    (if (pair? c)
        c
        (let ((s (jolt-throw-sitep)))
          (and (pair? s) s)))))
;; Emitted around a catch clause's body: snapshot the throwing site and return the
;; previous snapshot, which the paired leave restores. Save/restore rather than a
;; bare set so a nested catch cannot leave an inner throw's site behind for an
;; outer handler that runs afterwards. Reads the raise-time stash, not the live
;; vreg — by handler time the handler's context is what the vreg describes.
(define (jolt-catch-enter!)
  (let ((prev (virtual-register jolt-vreg-catch-line))
        (cur (jolt-throw-sitep)))
    (set-virtual-register! jolt-vreg-catch-line (if (pair? cur) cur 0))
    prev))

;; --- compile-time callsite tables (R2 + R4, beads jolt-knn8 / jolt-hm1p) -----
;; (jolt-register-callsite! "ns/f" line "ns/callee" tail?): the emitter
;; registers, at def load time, the STATIC callee of each call site in a traced
;; fn. Costs nothing per call — this is the metadata the reporter reconstructs
;; TCO-erased chains from (R4: there is NO runtime chain recording at all; Chez
;; continuation marks at production volume ballooned the heap to tens of GB on
;; delegation-heavy code, so the whole marks layer was replaced by these tables
;; plus the two vreg site stores).
;; Three views, all load-time-built, report-time-read:
;;   all sites:  (fqn:line) -> callees     — the staleness validator's evidence
;;   tail exits: fqn -> ((line . callee)…) — forward walk: how a fn left its
;;                                           frame (the TCO hop it made)
;;   tail entries: callee -> ((fqn . line)…) — backward walk: who could have
;;                                             tail-called the erased fn
;; A line can carry several calls (an operand and its outer call share it), so
;; every view holds LISTS of distinct entries; walks follow only UNAMBIGUOUS
;; edges and stop at the first fork, so a reconstruction can be incomplete but
;; never invented.
(define jolt-callsite-table (make-hashtable string-hash string=?))
(define jolt-tail-exits (make-hashtable string-hash string=?))
(define jolt-tail-entries (make-hashtable string-hash string=?))
;; fn -> every callee it registers anywhere: the validator's second evidence
;; tier, because a CATCH context's frame can resolve to an imprecise line (the
;; guard's establishment point, not the failing try's) and a line-keyed lookup
;; then reads the wrong site's callees.
(define jolt-fn-callees-table (make-hashtable string-hash string=?))
(define (jolt-callsite-key fqn line)
  (string-append fqn ":" (number->string line)))
(define (jolt-table-add! tbl key entry)
  (let ((cur (hashtable-ref tbl key '())))
    (unless (member entry cur)
      (hashtable-set! tbl key (cons entry cur)))))
(define (jolt-register-callsite! fqn line callee tail?)
  (jolt-table-add! jolt-callsite-table (jolt-callsite-key fqn line) callee)
  (jolt-table-add! jolt-fn-callees-table fqn callee)
  (when tail?
    (jolt-table-add! jolt-tail-exits fqn (cons line callee))
    (jolt-table-add! jolt-tail-entries callee (cons fqn line)))
  jolt-nil)
;; The registered static callees at (fqn, line) as a non-empty list, or #f
;; (unknown / dynamic site — nothing was registered).
(define (jolt-callsite-callees fqn line)
  (and (string? fqn) (fixnum? line)
       (let ((v (hashtable-ref jolt-callsite-table (jolt-callsite-key fqn line) '())))
         (and (pair? v) v))))
;; A fn's registered tail exits ((line . callee)…) / tail entries ((fqn . line)…).
(define (jolt-callsite-tail-exits fqn)
  (and (string? fqn) (hashtable-ref jolt-tail-exits fqn '())))
(define (jolt-callsite-tail-entries fqn)
  (and (string? fqn) (hashtable-ref jolt-tail-entries fqn '())))
;; Every callee a fn registers, at any line, or #f when the fn is unregistered.
(define (jolt-callsite-fn-callees fqn)
  (and (string? fqn)
       (let ((v (hashtable-ref jolt-fn-callees-table fqn '())))
         (and (pair? v) v))))
(define (jolt-catch-leave! prev)
  (set-virtual-register! jolt-vreg-catch-line (if (pair? prev) prev 0)))



;; GAMBIT ADAPTATION (see port-log): Chez's compound-condition raise becomes a
;; tagged pair (cons 'jolt-throw v). guard still catches it, and
;; jolt-unwrap-throw recovers v. jolt-throw-condition?/jolt-throw-condition-value
;; keep their names and meaning.
(define (jolt-throw-condition? x) (and (pair? x) (eq? (car x) 'jolt-throw)))
(define (jolt-throw-condition-value x) (cdr x))
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
             (jolt-throw-sitep (let ((s (virtual-register jolt-vreg-site)))
                                 (and (pair? s) s)))
             (raise (cons 'jolt-throw v)))))
;; The same capture for a HOST condition (a fault raised outside jolt-throw:
;; car of a non-pair, a bad flvector index). Installed by the cli's run wrapper
;; via with-exception-handler, which runs BEFORE the stack unwinds — a guard's
;; handler runs after, when the frames are already gone. Identity-tagged with
;; the condition itself, which is what jolt-unwrap-throw hands the reporter for
;; a non-&jolt-throw raise. jolt throws skip this (they captured already, with
;; the RIGHT identity — overwriting would orphan their k).
(define (jolt-capture-fault! c)
  (unless (jolt-throw-condition? c)
    ;; NO call/cc here: Chez already attaches &continuation to a serious
    ;; condition, and jolt-error-continuation reads it — a per-raise capture
    ;; would heap-freeze a whole stack for every INTERNALLY-CAUGHT host
    ;; condition, which a hot raise path cannot afford. Only the site pair is
    ;; stashed; an O(1) read.
    (jolt-throw-sitep (let ((s (virtual-register jolt-vreg-site)))
                        (and (pair? s) s)))))
(define (jolt-unwrap-throw x)
  (if (jolt-throw-condition? x) (jolt-throw-condition-value x) x))
;; ex-info builds a jolt-ex-info-record (NOT a pmap — pmap?/coll?/seqable?/ifn?
;; /associative?/counted? are naturally false). Arity 2 (msg data) or 3 (msg data cause).
;; No :jolt/class field on plain ex-info — class defaults to clojure.lang.ExceptionInfo
;; via ex-info-class in records-interop.ss.
;;
;; nil data reads back as {}, not nil: ExceptionInfo's constructor rejects a null
;; map, so an ExceptionInfo whose data is nil cannot exist and (ex-data (ex-info
;; "m" nil)) is {}. Coercing HERE covers every caller — the emitter lowers the
;; ex-info native op to a direct call to this procedure, so a wrapper around the
;; clojure.core/ex-info var root would miss every compiled call site. It is also
;; what makes (some? (ex-data e)) a sound "is this an ExceptionInfo" test, which
;; is how the analyzer's throw-message tells one from a host throwable.
;;
;; A throwable that genuinely has NO data is a different construction:
;; jolt-host-throwable / throw-jvm, which is what the JVM raises wherever
;; ex-data is nil.
(define (jolt-ex-info msg data . more)
  (make-jolt-ex-info-record "clojure.lang.ExceptionInfo" msg
                             (if (null? more) jolt-nil (car more))
                             (if (jolt-nil? data) empty-pmap data) 0))
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
      ((NoSuchFieldException) "java.lang.NoSuchFieldException")
      ((IndexOutOfBoundsException) "java.lang.IndexOutOfBoundsException")
      ((ClassCastException) "java.lang.ClassCastException")
      ((NullPointerException) "java.lang.NullPointerException")
      ((ArityException) "clojure.lang.ArityException")
      ((IllegalAccessError) "java.lang.IllegalAccessError")
      (else (jvm-throwable-fqn-fallback sym)))))
(define (throw-jvm type msg)
  (jolt-throw (jolt-host-throwable (jvm-throwable-fqn type) msg)))

;; --- host interop ------------------------------------------------------------
;; (.method target arg*) with method in backend supported-host-methods
;; (isDirectory/listFiles) lowers to (jolt-host-call "method" target arg*). Those
;; map onto Chez path operations when the receiver is a path STRING; a File value
;; is intercepted by io.ss's wrapper before reaching here. Any other receiver (a
;; deftype/record with its own .isDirectory) routes to the normal method dispatch
;; instead of misapplying file ops to it. record-method-dispatch is loaded later
;; (records.ss) and resolved at call time.
(define (jolt-host-call method target . args)
  (cond
    ((and (string=? method "isDirectory") (string? target)) (if (file-directory? target) #t #f))
    ((and (string=? method "listFiles") (string? target)) (list->cseq (directory-list target)))
    (else (record-method-dispatch target method (apply jolt-vector args)))))

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
;; Mirrors host/chez/rt.ss var-cell-v4 field for field: shared chez files
;; (ns.ss meta/macro?, dyn-binding.ss dyn-bound?) read these accessors, so the
;; two hosts' cells must not drift (this one sat at v2 while chez moved twice).
(define-record-type var-cell
  (fields ns name (mutable root) (mutable defined?) (mutable meta) (mutable macro?)
          (mutable dyn-bound?))
  (nongenerative var-cell-v4))
(define var-table (make-hashtable string-hash string=?))
(define (jolt-var ns name)
  (let ((k (string-append ns "/" name)))
    (or (hashtable-ref var-table k #f)
        (let ((c (make-var-cell ns name (make-jolt-var-unbound ns name) #f #f #f #f)))
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
(define var-redefined-set (make-hashtable string-hash string=?))
(define (var-redefined? ns name)
  (hashtable-contains? var-redefined-set (string-append ns "/" name)))
(define (def-var! ns name v)
  ;; first def of a given proc wins, so an alias like (def inc' inc) — which binds
  ;; the SAME proc to a second var — doesn't rename inc.
  (when (and (procedure? v) (not (hashtable-contains? proc-name-tbl v)))
    (hashtable-set! proc-name-tbl v (cons ns name)))
  (hashtable-set! ns-has-vars-set ns #t)
  (let ((c (jolt-var ns name)))
    ;; see host/chez/rt.ss def-var! -- a var defined more than once with a value
    ;; may not have its body spliced (jolt-rtjm). (declare x) emits declare-var!,
    ;; so a forward declaration is not a redefinition.
    (when (not (jolt-var-unbound? (var-cell-root c)))
      (hashtable-set! var-redefined-set (string-append ns "/" name) #t))
    (var-cell-root-set! c v) (var-cell-defined?-set! c #t) c))
;; Value-position comparison references compile to the seq.ss chain singletons
;; (jolt-lt/gt/le/ge), not to the clojure.core var roots — the roots were later
;; re-bound by the checked numeric layer, so def-var! never saw these procs.
;; Register them so a stored comparator like (sorted-map-by >) travels as a
;; fn-ref (by name) like any other named core fn.
(hashtable-set! proc-name-tbl jolt-lt (cons "clojure.core" "<"))
(hashtable-set! proc-name-tbl jolt-gt (cons "clojure.core" ">"))
(hashtable-set! proc-name-tbl jolt-le (cons "clojure.core" "<="))
(hashtable-set! proc-name-tbl jolt-ge (cons "clojure.core" ">="))
;; Set of ns-name strings that have at least one var — makes ns-has-vars? O(1)
;; instead of scanning the entire var-table per require-miss. Updated in def-var!
;; (and wherever vars are removed, though removal is rare).
(define ns-has-vars-set (make-hashtable string-hash string=?))
;; jolt.host/throwable — build a typed throwable a library can throw so (class …),
;; instance?, .getMessage and ex-message all reflect the named JVM class (e.g. an
;; http client throwing java.net.ConnectException). Strictly better than a
;; hand-rolled :jolt/ex-info table, which carries only the class.
(def-var! "jolt.host" "throwable" jolt-host-throwable)
;; jolt.host/available-processors — the host's usable CPU count (see above). The
;; JVM spelling of the same question, Runtime.availableProcessors, is mapped in
;; the java layer (java/process.ss); clojure.core reaches it through here.
(def-var! "jolt.host" "available-processors" (lambda () (jolt-available-processors)))

;; SKIPPED on the gambit target: the telemetry primitives region (wall/mono
;; clocks, sa-stats counters, thread-id, machine-type/scheme-version) — Chez
;; time objects and Chez-shipped statistics; no manifest consumer needs them
;; (see port-log). jolt.host/throwable + available-processors above are kept.

;; var def-time metadata: the :def emit passes the def's reader meta
;; (^:private / ^Type tag / docstring -> {:doc}) here, stored in an eq side-table
;; keyed by the cell. jolt-meta (natives-meta.ss) merges it onto {:ns :name},
;; which it derives from the cell — so EVERY var (plain def, native-op, declare)
;; reports {:ns :name} like Clojure, with the user meta layered on when present.
;; Meta and the macro flag live IN the cell (var-cell-v4 fields), matching
;; chez: the shared files read and write the fields directly (dyn-binding.ss's
;; dynamic check reads meta, ns.ss's alter-meta! sync writes macro?), so the
;; eq-side-tables this file used to keep were invisible to them — a var defined
;; ^:dynamic through the seed still threw "non-dynamic" at the first binding.
(define jolt-kw-var-ns (keyword #f "ns"))
(define jolt-kw-var-name (keyword #f "name"))
(define jolt-kw-var-macro (keyword #f "macro"))
(define (def-var-with-meta! ns name v m)
  (let ((c (def-var! ns name v))) (var-cell-meta-set! c m) c))
;; A runtime-defined DYNAMIC var (the *earmuffed* core vars): tagged :dynamic so
;; push-thread-bindings accepts it — with no meta entry a var is non-dynamic and
;; binding throws, like the JVM.
(define (def-dynvar! ns name v)
  (def-var-with-meta! ns name v
    (jolt-hash-map (keyword #f "dynamic") #t)))
;; Attach meta to an already-interned var (the declare/no-init emission path:
;; (def ^:dynamic *x*) must be bindable before its root is set).
(define (set-var-meta! ns name m)
  (var-cell-meta-set! (jolt-var ns name) m))
;; runtime-macro flag: a var whose root holds a macro expander fn, so the
;; analyzer's form-macro?/form-expand-1 (host-contract.ss) expand it. The
;; prelude emits each core/stdlib defmacro as a def-var! of its expander
;; followed by (mark-macro! ns name). The flag is the cell's macro? FIELD —
;; shared ns.ss (alter-meta! :macro sync) writes it directly, so a side table
;; here would miss those writes. The field survives a later (def name ...) that
;; replaces the expander but keeps the same cell, matching Clojure.
(define (mark-macro! ns name)
  (let ((c (jolt-var ns name))) (var-cell-macro?-set! c #t) c))
(define (macro-var? cell) (and cell (var-cell-macro? cell) #t))
;; declare / (def name) with no init: reserve the cell ONLY if absent. An
;; existing root is left intact — Clojure's (def x) with no init does not clobber
;; a prior binding (do (def x 7) (def x) x) => 7. Returns the cell either way.
(define (declare-var! ns name)
  (let* ((k (string-append ns "/" name))
         (c (hashtable-ref var-table k #f)))
    (if c
        ;; a declare marks an ALREADY-interned cell resolvable — set-var-meta!
        ;; (jolt-var) may have interned it undefined? first (the no-init def with
        ;; metadata emits set-var-meta! then declare-var!), so without this a
        ;; declaration-only var stays defined?=#f and resolve/find-var/ns-interns
        ;; miss it in an AOT build. The existing root is left intact.
        (begin (var-cell-defined?-set! c #t) c)
        (let ((c (make-var-cell ns name (make-jolt-var-unbound ns name) #t #f #f #f)))  ; declared => interned/resolvable
          (hashtable-set! var-table k c)
          c))))

;; regex: defines regex-t + the re-* fns (def-var!'d into
;; clojure.core), so it loads after def-var! and before the printer below (which
;; renders a regex-t as #"source").

;; atoms: host-coupled mutable cells; def-var!'d into clojure.core
;; (atom/deref/swap!/reset! + the compare/vals kernel). Loads after def-var! and
;; jolt-invoke (seq.ss) / jolt= (values.ss) / jolt-vector (collections.ss).

;; refs: Clojure refs and serialized transactions (STM).  Loaded after atoms
;; (shares the IRef seam and jolt-deref); must load before loader.ss (wires
;; *loaded-libs*) and before concurrency.ss (which chains jolt-deref further).

;; type predicates + simple accessors: seed natives the overlay
;; assumes (map?/vector?/nil?/number?/.../name/namespace), def-var!'d into
;; clojure.core. Loads after the value-model record predicates they wrap.

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
;; One pass through a string port: a right-fold of string-append re-copied the
;; whole joined suffix per element — O(n*L) on every collection render (same
;; fix as host/chez/rt.ss, in Gambit's port idiom).
(define (jolt-str-join-sep strs sep)
  (cond ((null? strs) "") ((null? (cdr strs)) (car strs))
        (else
         (let ((op (open-output-string)))
           (display (car strs) op)
           (let loop ((r (cdr strs)))
             (unless (null? r)
               (display sep op)
               (display (car r) op)
               (loop (cdr r))))
           (get-output-string op)))))
(define (jolt-str-join strs) (jolt-str-join-sep strs " "))
;; map ENTRIES join with ", " like the reference printer: {:a 1, :b 2}
(define (jolt-str-join-comma strs) (jolt-str-join-sep strs ", "))
(define (jolt-char->string c)
  (if (jolt-pr-readable?)
      (string-append "\\" (case c ((#\newline) "newline") ((#\space) "space") ((#\tab) "tab")
                                  ((#\return) "return") ((#\backspace) "backspace") ((#\page) "formfeed")
                                  (else (string c))))
      (string c)))
;; Render a value for a MESSAGE — a host exception's text ("<x> cannot be cast
;; to …"), a gate's divergence report. str-style at the top level, where a bare
;; nil renders as the empty string (a nil ELEMENT inside a collection still prints
;; "nil", which jolt-pr-str handles). The `-e` / REPL result printer is
;; jolt-repl-str (printing.ss), which is readable instead.
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

;; The value types the runtime itself owns: Chez immediates plus the two value
;; records (keyword, symbol) no host shim can model. Every printer base case
;; renders these directly, so walking the arm registries for one is dead work —
;; and in print-heavy code they are nearly every value. Both printers and the str
;; renderer skip straight to their base case for a value that answers true here.
;;
;; The correctness condition is that NO registered arm may match one of these, or
;; the fast path would silently bypass it. That is enforced at registration
;; (pr-arm-reject-fast-type!) rather than left to a comment, so a library shim
;; registering through __register-pr! / __register-str! cannot introduce an arm
;; the fast path would skip: it fails loudly at the point of registration instead
;; of rendering wrongly at some later print.
(define (pr-fast-type? x)
  (or (number? x)
      (string? x)
      (jolt-nil? x)
      (eq? x #t)
      (eq? x #f)
      (char? x)
      (keyword? x)
      (jolt-symbol? x)))

;; One representative per fast-path type, covering the numeric tower (fixnum,
;; bignum, ratio, flonum) since an arm could plausibly claim only one of those.
(define pr-fast-probes
  (list 0 (expt 2 70) 1/2 1.5 "s" jolt-nil #t #f #\a
        (keyword #f "k") (jolt-symbol #f "s")))
;; Shares reject-fast-type-claim! (values.ss) with the eq and hash registries,
;; which enforce the same invariant over their own — narrower — fast paths.
(define (pr-arm-reject-fast-type! who pred)
  (reject-fast-type-claim! who pred pr-fast-probes
                           "the printer fast path (see pr-fast-type? in rt.ss)"))

;; A host shim registers a type's str-style rendering via register-pr-str-arm! (or
;; register-pr-arm! in printing.ss for both printers at once) instead of
;; set!-wrapping jolt-pr-str. Disjoint types, checked before the base cases.
(define jolt-pr-str-arms '())
(define (register-pr-str-arm! pred render)
  (pr-arm-reject-fast-type! 'register-pr-str-arm! pred)
  (set! jolt-pr-str-arms (cons (cons pred render) jolt-pr-str-arms)))
;; Fallback rendering for a value no printer branch claims. The JVM prints an
;; unknown object as #object[<class> <hash> <toString>]; jolt used to hand the
;; value to Chez's writer, which leaked the internal record layout into
;; user-visible output — (pr-str (atom 1)) read #[jolt-atom-v3 1 () …] and
;; (pr-str (io/file "/tmp/x")) read #[jolt-jfile-v1 /tmp/x]. Rendering the
;; #object[…] shape here fixes every host type at once, so a per-type print arm
;; becomes a refinement rather than the only thing standing between a value and
;; Chez syntax.
;;
;; Content is the value's str rendering, taken STRAIGHT from the str registry
;; rather than through jolt-str-render-one, whose own fallback is this function —
;; going through it would recurse. Types with no str arm print bare, like the
;; JVM's default Object.toString. readable? quotes the content, which is the
;; (pr x) vs (print x) difference on the JVM. The identity hash is omitted, as
;; jolt omits it in the print arms it already has.
(define (jolt-object-content x)
  (let loop ((rs str-render-registry))
    (cond ((null? rs) #f)
          (((caar rs) x) ((cdar rs) x))
          (else (loop (cdr rs))))))
;; The class a value reports. This host has no class model, so the answer is no
;; class: jolt-object-repr below prints the value plainly, and the last arm of
;; value-host-tags (records-gambit.ss, from host/chez/protocols.ss) leaves it a
;; plain Object — the same outcomes the guarded call used to reach by raising.
(define (jolt-class-name x) #f)
;; alength in call position (the op registry's native, natives-array.ss on
;; Chez): the count of any array-like, nil refused — post-prelude names it.
(define (jolt-alength a)
  (if (jolt-nil? a) (error 'alength "null") (jolt-count a)))
(define (jolt-object-repr x readable?)
  (let ((cls (guard (e (#t #f)) (jolt-class-name x)))
        (content (guard (e (#t #f)) (jolt-object-content x))))
    (cond ((not (string? cls)) (format "~a" x))
          ((not (string? content)) (string-append "#object[" cls "]"))
          (readable? (string-append "#object[" cls " \"" (jolt-str-escape content) "\"]"))
          (else (string-append "#object[" cls " " content "]")))))

;; readable? reaches only the #object[…] fallback: every other branch renders the
;; same either way, and the readable printer handles the types that differ (string
;; quoting, ##Inf) before it delegates here.
(define (jolt-pr-str-base x) (jolt-pr-str-base/readable x #f))
(define (jolt-pr-str-base/readable x readable?)
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
    (else (jolt-object-repr x readable?))))
(define (jolt-pr-str x) (jolt-pr-str/readable x #f))
(define (jolt-pr-str/readable x readable?)
  (if (pr-fast-type? x)
      (jolt-pr-str-base/readable x readable?)
      (let loop ((as jolt-pr-str-arms))
        (cond ((null? as) (jolt-pr-str-base/readable x readable?))
              (((caar as) x) ((cdar as) x))
              (else (loop (cdr as)))))))

;; converters + string ops: str/subs/vec/keyword/symbol/compare/int/
;; double/gensym — host-coupled seed natives def-var!'d into clojure.core. Loaded
;; LAST because `str` reuses jolt-pr-str (defined just above).

;; transients: copy-on-write transient collections + persistent disj;
;; extends get/count/contains? to see through a transient. After collections.ss
;; (the persistent ops it delegates to).

;; seq-native shims: mapcat/take-while/drop-while/partition/sort +
;; reduced/reduced?/identical? — seed-native fns the overlay assumes are core
;; natives. Over the seq layer + jolt-compare, so loaded after converters.ss.

;; readable printer + output seams: __pr-str1/__write/
;; __with-out-str/__eprint/__eprintf — the host seams the overlay print family
;; (pr-str/pr/prn/print/println/*-str) is built on. After converters.ss (uses
;; jolt-pr-str/jolt-str-join) + seq.ss (jolt-invoke).

;; --- randomness -------------------------------------------------------------
;; Chez's PRNG starts every thread from the SAME fixed seed, and the state is
;; per-thread rather than shared. Both halves of that diverge from Clojure, where
;; rand/rand-int/Math.random run off one process-global java.util.Random seeded
;; from the clock:
;;
;;   - across processes, every jolt run replayed one identical stream, so two
;;     unrelated processes agreed on every "random" value — including every
;;     random-uuid, so a fleet minted colliding UUIDs;
;;   - across threads, each forked thread restarted that same stream from the
;;     top, so N request threads drew N identical values in ONE process.
;;
;; Seeding lazily on first use covers both: a thread that never draws pays
;; nothing, and one that does is seeded before its first value. Seed from the
;; clock (as the JVM does) mixed with pid and thread id so two threads starting
;; inside the same nanosecond still diverge, and a counter so that even a
;; coarse clock cannot hand out one seed twice.
;; GAMBIT ADAPTATION (see port-log): no lazy clock-seed. Gambit's default
;; random source is already seeded from time+pid at process start, so the
;; Chez seeding machinery (random-seeded?/random-seed-counter/seed-random!)
;; is deleted; every jolt-level draw is (random-integer n), same [0, n) contract
;; as Chez's (random n).
(define (jolt-random n) (random-integer n))

;; GAMBIT ADAPTATION (see port-log): no OS CSPRNG on this target — the whole
;; entropy-source region (/dev/urandom, BCryptGenRandom, the warn-once
;; fallback) is deleted. jolt-random-bytes fills a bytevector with
;; (random-integer 256) per byte. Divergence, documented: uniqueness holds,
;; unpredictability does not (random-uuid values stay unique per process, but
;; are guessable — acceptable for G2, wrong for anything security-sensitive).
(define (jolt-random-bytes n)
  (let ((bv (make-bytevector n 0)))
    (let loop ((i 0))
      (if (fx=? i n)
          bv
          (begin (bytevector-u8-set! bv i (random-integer 256))
                 (loop (fx+ i 1)))))))

;; collection constructors + rand: bind the public
;; clojure.core names hash-map/hash-set/array-map/set/rand to the existing
;; pmap/pset ctors. After collections.ss (the ctors) + seq.ss (seq->list).

;; bit ops + parse-long/parse-double: host-coupled scalar
;; seed natives over the all-flonum number model.

;; multimethods: defmulti/defmethod dispatch runtime. Needs jolt-invoke
;; (seq.ss), jolt=/key-hash/jolt-hash-map (collections.ss), jolt-atom? (atoms.ss),
;; jolt-pr-str (above), and the var-cell machinery — so loaded last.

;; the single JVM class/interface graph — value-host-tags, instance?, isa?/supers,
;; and the exception hierarchy all derive from it. Before records.ss so
;; value-host-tags can build on jch-tags.

;; records + protocols: defrecord/deftype/defprotocol/
;; extend-type/reify. A jrec record type set!-extended into the collection
;; dispatchers + a protocol registry. After multimethods.ss (chez-current-ns) and
;; the dispatchers/printers it wraps (collections/seq/values/converters/printing/
;; transients).

;; metadata: meta / with-meta over an identity-keyed
;; side-table. After records.ss (jrec) + the collection ctors it copies.

;; host class tokens: bare class names (String/Keyword/File...) ->
;; canonical JVM class-name strings + (class x). After natives-meta.ss (jolt-type)
;; and the printer (jolt-str-render-one).

;; dynamic vars: *clojure-version* / *unchecked-math* constants the host
;; binds natively. After collections.ss (jolt-hash-map) + def-var!.

;; host tables + sorted collections: jolt.host/tagged-table/
;; ref-put!/ref-get + the 25-sorted tier's runtime (sorted-map/sorted-set routed
;; through their :ops table). Loaded LAST — wraps the jrec-extended dispatchers
;; (records.ss), jolt-disj (transients.ss), and value-host-tags (records.ss).

;; lazy-seq bridge: make-lazy-seq / coll->cells over the
;; cseq model — unblocks every overlay fn built on the lazy-seq macro (repeat/
;; iterate/cycle/dedupe/take-nth/keep/interpose/reductions/tree-seq/lazy-cat).
;; Loaded LAST so %ls-seq captures the fully-extended (sorted-aware) jolt-seq.

;; transducer surface: native volatile boxes, cat, +
;; the transduce/sequence entry points over into-xform/reduce-seq. After
;; natives-seq.ss (into-xform), seq.ss (reduce-seq) + atoms.ss (deref).

;; vars as first-class objects: var?/var-get/deref/invoke/=/
;; pr-str over the rt.ss var-cell. After natives-transduce.ss (chains deref) + the
;; printers. emit lowers :the-var to (jolt-var ns name).

;; misc scalar natives: UUID (random-uuid/parse-uuid/uuid?), format/
;; printf, tagged-literal, bigint. After the printers + converters (str/pr-str of
;; a uuid). Overlay names (uuid?/random-uuid/parse-uuid/tagged-literal?) re-asserted
;; in post-prelude.ss.

;; format / printf: the %-directive engine. After natives-misc.ss + converters.ss
;; (jolt-str-render-one).

;; extension points: the keyed provider registry core declares a
;; contract on and a library fills in (jolt.host/register-extension-point! /
;; register-extension! / refine-extension! / extension-value). Host-neutral — the
;; java layer and libraries are clients. After collections.ss (jolt maps),
;; converters.ss + the printers (error messages render values), and rt.ss's
;; throw-jvm.

;; namespaces: the namespace value model — find-ns/ns-name/
;; all-ns/the-ns/create-ns/in-ns/ns-publics/ns-map/ns-interns/ns-aliases/resolve/
;; find-var/ns-unmap/*ns*, over the var-table + chez-current-ns. Loaded LAST: needs
;; var-cell + var-cell-defined?, jolt-symbol/jolt-hash-map/jolt-assoc, chez-current-ns
;; (multimethods.ss), list->cseq (seq.ss), and the fully-patched printers (vars.ss).

;; dynamic var binding: the per-thread binding stack +
;; push/pop/get-thread-bindings/__thread-bound?/var-set/alter-var-root/__local-var.
;; Chains var-deref (rt.ss) and jolt-var-get (vars.ss) onto the stack, so a `binding`
;; frame is seen by every var read. Loaded LAST: needs the fully-extended var-read
;; paths + jolt-hash-map/pmap-fold/pmap-assoc (collections.ss).

;; java.lang.String method interop: jolt-string-method, the
;; portable String/CharSequence surface record-method-dispatch falls through to on
;; a string target. After regex.ss (jolt-re-pattern/regex-t-irx) + records.ss
;; (which references jolt-string-method).

;; host class statics + constructors: host-static-ref/
;; host-static-call/host-new + the jhost method registry. Loads LAST — it extends
;; record-method-dispatch (records.ss) and reuses natives-str helpers (str-trim,
;; ascii-string-down, re-split, str-split-drop-trailing) + the regex-t accessors.

;; generic dot-form dispatch: field access + map/vector member access
;; for the `.` / `.-field` desugar. Loads after host-static.ss so it wraps every
;; record-method-dispatch arm (jhost/number/regex/jrec/string) and falls through.

;; java.io.File + host file I/O: path-backed jfile record, slurp/spit/
;; flush, file-seq dir primitives, clojure.java.io/file. Loads LAST so its jfile
;; arm wraps the fully-built record-method-dispatch and the str/type/instance-check
;; extensions sit over every prior shim.

;; #inst values + the java.util/java.text date layer: jinst (RFC3339 ms), Date,
;; sql.Date/Timestamp, Calendar, TimeZone, SimpleDateFormat. Loads LAST — it
;; extends record-method-dispatch / jolt-get / jolt= / jolt-hash / jolt-pr-str /
;; jolt-type / instance-check and uses host-static.ss's registries. libc time
;; primitives (zone offset, locale names) exposed as jolt.host vars, used by the
;; library's zone/localized layer.

;; java.time is split (RFC 0008): the base VALUE types (Instant, LocalDate/Time/
;; DateTime, Duration, Period, Year/YearMonth, enums) are portable Clojure under
;; stdlib/jolt/time/, autoloaded on first use by host-static.ss with no dependency.
;; Everything that formats or names a zone (DateTimeFormatter, ZoneOffset/ZoneId,
;; ZonedDateTime/OffsetDateTime, localized formatting, java.util.Locale) is the
;; jolt-lang/time library — one implementation of each, not carried in core.

;; Chez-side data reader: read-string / __parse-next /
;; __read-tagged. Loads after inst-time.ss — __read-tagged reuses its #uuid/#inst
;; constructors, and the reader needs the full value/collection layer above.

;; clojure.math: native flonum-math shims def-var!'d into the
;; clojure.math ns. Self-contained (only def-var! + Chez math), order-independent.

;; reader/macro runtime support: #?() feature set, reader-conditional + re-matcher
;; tagged-map ctors, macroexpand. After ns.ss; macroexpand call-time-refs the macro
;; table (host-contract) + analyzer ctx.

;; Java-style arrays: object/typed array constructors + a jolt-array
;; backing; extends count/nth/seq/get/ref-put! so the overlay aget/aset/alength see
;; it. After the dispatchers it chains.

;; java.io byte/char streams (FileInputStream/…/ByteArrayOutputStream/Buffered*)
;; over Chez ports. After io.ss (extends its slurp/__close/reader-jhost?) and
;; natives-array.ss (the byte-array <-> bytevector bridge).

;; java.lang.ProcessBuilder / Process. After io-streams (make-in-stream /
;; make-out-stream) and host-static-methods (all-env-pairs).
;; proxy: extends-by-delegation over a concrete host class. After host-static.ss
;; (host-new + the ctor table it probes), records-interop.ss (instance-check) and
;; io-streams.ss, so a proxy over a stream class finds its constructor.

;; clojure.lang.PersistentQueue: a functional queue + EMPTY static.
;; Chains seq/count/empty?/peek/pop/conj/sequential?/class/instance?/printer, so
;; load after natives-array (the dispatchers it extends).

;; syntax-quote form builders: __sqcat/__sqvec/__sqmap/__sqset/
;; __sq1, def-var!'d into clojure.core. A cross-compiled macro expander (analyzer
;; on Chez) calls these to build its expansion as reader forms. Needs the
;; collection/seq layer + def-var!; order-independent past those.

;; concurrency: real OS-thread futures + blocking promises, shared-heap
;; (JVM) semantics. Loaded LAST — chains the fully-built jolt-deref and conveys the
;; thread-local binding stack (dyn-binding.ss) into workers. pmap/pcalls/pvalues
;; (overlay, over `future`) light up once future-call exists here.

;; clojure.core.async: real-thread blocking channels + go/go-loop/
;; thread macros, def-var!'d into clojure.core.async. After concurrency.ss (reuses
;; ms->duration) and the collection/seq layer.

;; BigDecimal: the jbigdec value type + bigdec/decimal?/class/equality/
;; printing. Loads LAST so its set!-wraps of jolt-class/jolt=2/the printers sit
;; outermost over every earlier extension.

;; Native stack traces: jv$ns$name -> source registry + continuation frame walk +
;; uncaught-throwable renderer. After the printers/equality it relies on.

;; Unique anon-fn names -> {source form, ns, free locals} for the image write
;; side. Plain defines (no def-var! / manifest lines): only emitted code and the
;; image writer call them, never Clojure.

;; State images: dump the value graph to a file and read it back. Loads LAST —
;; walks jolt collections, var cells and atoms, prints paths through the printers,
;; and reads proc-name-tbl to write a fn as its var's name.

;; clojure.core/str-join — the chez definition lives in java/natives-str.ss, which
;; the gambit boot EXCLUDES (G2 java/ cut). The printer (printing.ss, rt-core) and
;; clojure.core/join resolve this var at runtime, so the gambit kernel must define
;; it: render each element with jolt-str-render-one, join by sep (default "").
(define (jolt-str-join-var coll . opt)
  (let ((sep (if (pair? opt) (jolt-str-render-one (car opt)) "")))
    ;; seq-more may return jolt-empty-list (map tail) — normalize via jolt-seq
    ;; like seq->list, or the next round calls cseq accessors on the empty list
    (let loop ((s (jolt-seq coll)) (acc '()))
      (if (jolt-nil? s)
          (apply string-append (reverse acc))
          (let ((piece (jolt-str-render-one (seq-first s))))
            (loop (jolt-seq (seq-more s))
                  (if (null? acc)
                      (list piece)
                      (cons piece (cons sep acc)))))))))
(def-var! "clojure.core" "str-join" jolt-str-join-var)

;; jolt-string-method — chez defines it in java/natives-str.ss, which the gambit
;; boot EXCLUDES (G2 java/ cut), but records-gambit.ss's record-method-dispatch
;; falls through a string target to it and the compiler (image + prelude) calls
;; string methods (indexOf, startsWith, substring, ...) while compiling. Port of
;; the string surface the compiler path exercises, on Gambit primitives (JVM
;; semantics: -1 when a search misses).
(define (jolt-string-method method s rest)
  (define (arg n) (list-ref rest n))
  (define (arg-idx n)
    (let ((v (arg n))) (if (integer? v) v (exact (truncate v)))))
  (cond
    ((string=? method "toString") s)
    ((string=? method "length") (string-length s))
    ((string=? method "charAt") (string-ref s (arg-idx 0)))
    ((string=? method "indexOf")
     (let ((needle (jolt-need-str (arg 0))))
       (or (if (fx>? (length rest) 1)
               (string-index s needle (arg-idx 1))
               (string-index s needle))
           -1)))
    ((string=? method "lastIndexOf")
     (or (string-rindex s (jolt-need-str (arg 0))) -1))
    ((string=? method "startsWith") (string-prefix? (jolt-need-str (arg 0)) s))
    ((string=? method "endsWith") (string-suffix? (jolt-need-str (arg 0)) s))
    ((string=? method "substring")
     (substring s (arg-idx 0)
                (if (fx>? (length rest) 1) (arg-idx 1) (string-length s))))
    (else (error 'jolt-string-method "unhandled string method on gambit" method))))

;; ---- clojure.core/str-* natives ---------------------------------------------
;; chez defines these in java/natives-str.ss (excluded, G2 java/ cut); the
;; prelude's clojure.string overlay and the compiler image var-deref them at
;; runtime. Self-contained ports on Gambit primitives. Regex branches raise a
;; clear error — the compiler path only passes literal needles.
(define (jolt->idx x) (if (integer? x) x (exact (truncate x))))
(define (jolt-char-by-char-match? s si needle nlen)
  (let loop ((j 0))
    (cond ((fx=? j nlen) #t)
          ((char=? (string-ref s (fx+ si j)) (string-ref needle j)) (loop (fx+ j 1)))
          (else #f))))
(define (str-index-of s needle from)
  (let ((nlen (string-length needle)) (slen (string-length s)))
    (let loop ((i (max 0 from)))
      (cond ((fx>? (fx+ i nlen) slen) -1)
            ((jolt-char-by-char-match? s i needle nlen) i)
            (else (loop (fx+ i 1)))))))
(define (str-last-index-of s needle)
  (let ((nlen (string-length needle)) (slen (string-length s)))
    (let loop ((i (fx- slen nlen)) (found -1))
      (cond ((fx<? i 0) found)
            ((jolt-char-by-char-match? s i needle nlen) i)
            (else (loop (fx- i 1) found))))))
(define (str-needle x)
  (cond ((char? x) (string x))
        ((number? x) (string (integer->char (exact (truncate x)))))
        ((string? x) x)
        (else (jolt-str-render-one x))))
(define (str-replace-literal s a b)
  (let ((alen (string-length a)) (slen (string-length s)))
    (if (fx=? alen 0)
        ;; JVM String.replace with an empty match inserts the replacement at
        ;; every position 0..slen: "" -> "b", "aaa" -> "bababab".
        (let ((op (open-output-string)))
          (let loop ((i 0))
            (if (fx>? i slen)
                (get-output-string op)
                (begin (display b op)
                       (if (fx<? i slen) (write-char (string-ref s i) op))
                       (loop (fx+ i 1))))))
        (let ((op (open-output-string)))
          (let loop ((i 0))
            (if (fx>? (fx+ i alen) slen)
                (begin (display (substring s i slen) op) (get-output-string op))
                (if (jolt-char-by-char-match? s i a alen)
                    (begin (display b op) (loop (fx+ i alen)))
                    (begin (display (string-ref s i) op) (loop (fx+ i 1))))))))))
(define (str-replace-literal-first s a b)
  (let ((alen (string-length a)) (i (str-index-of s a 0)))
    (if (fx<? i 0) s
        (string-append (substring s 0 i) b (substring s (fx+ i alen) (string-length s))))))
(define (str-triml s)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (cond ((fx=? i len) "")
            ((char<=? (string-ref s i) #\space) (loop (fx+ i 1)))
            (else (substring s i len))))))
(define (str-trimr s)
  (let loop ((j (fx- (string-length s) 1)))
    (cond ((fx<? j 0) "")
          ((char<=? (string-ref s j) #\space) (loop (fx- j 1)))
          (else (substring s 0 (fx+ j 1))))))
(define (str-upper s) (string-upcase s))
(define (str-lower s) (string-downcase s))
(define (str-reverse-b s) (list->string (reverse (string->list s))))
(define (str-find needle s)
  (let ((i (str-index-of s needle 0)))
    (if (fx<? i 0) jolt-nil i)))
(define (str-literal-split s sep)
  (let ((slen (string-length (jolt-need-str s))) (plen (string-length sep)))
    (if (fx=? plen 0)
        (map (lambda (c) (list->string (list c))) (string->list s))
        (let loop ((i 0) (start 0) (acc '()))
          (cond ((fx>? (fx+ i plen) slen)
                 (reverse (cons (substring s start slen) acc)))
                ((string=? (substring s i (fx+ i plen)) sep)
                 (loop (fx+ i plen) (fx+ i plen) (cons (substring s start i) acc)))
                (else (loop (fx+ i 1) start acc)))))))
(define (jolt-str-join-strs strs sep)
  (let loop ((xs strs) (first #t) (acc '()))
    (cond ((null? xs) (apply string-append (reverse acc)))
          (first (loop (cdr xs) #f (cons (car xs) acc)))
          (else (loop (cdr xs) #f (cons (car xs) (cons sep acc)))))))
(define (str-split pat s . opt)
  (let ((limit (if (and (pair? opt) (not (jolt-nil? (car opt)))) (jolt->idx (car opt)) #f)))
    (if (jolt-regex? pat)
        (error 'str-split "regex split unsupported on the gambit boot" pat)
        (let ((parts (str-literal-split s pat)))
          (apply jolt-vector
            (if (and limit (fx>? limit 0) (fx>? (length parts) limit))
                (append (list-head parts (fx- limit 1))
                        (list (jolt-str-join-strs (list-tail parts (fx- limit 1)) pat)))
                parts))))))
(define (str-replace-all pat repl s)
  (if (jolt-regex? pat)
      (error 'str-replace-all "regex replace unsupported on the gambit boot" pat)
      (str-replace-literal s (str-needle pat) (str-needle repl))))
(define (str-replace pat repl s)
  (if (jolt-regex? pat)
      (error 'str-replace "regex replace unsupported on the gambit boot" pat)
      (str-replace-literal-first s (str-needle pat) (str-needle repl))))
(def-var! "clojure.core" "str-upper" str-upper)
(def-var! "clojure.core" "str-lower" str-lower)
(def-var! "clojure.core" "str-trim" str-trim)
(def-var! "clojure.core" "str-triml" str-triml)
(def-var! "clojure.core" "str-trimr" str-trimr)
(def-var! "clojure.core" "str-find" str-find)
(def-var! "clojure.core" "str-reverse-b" str-reverse-b)
(def-var! "clojure.core" "str-split" str-split)
(def-var! "clojure.core" "str-replace" str-replace)
(def-var! "clojure.core" "str-replace-all" str-replace-all)

;; source-registry.ss is excluded (introspect off on this target); the def
;; path emits registration calls — a no-op keeps them inert, matching the
;; degraded-introspection mode where backtraces carry no frames.
(define (jolt-register-source! . _) #f)

;; fn-form-registry.ss is excluded (image capability raises on this target);
;; the anon-fn emission registers source forms for image closure capture — a
;; no-op keeps those calls inert, matching the image-off degradation.
(define (image-register-fn-form! . _) #f)
;; chez's def-var! records the var name of a code value (a multimethod, a reify)
;; so a state image can write it as a reference. Gambit has no image, so the
;; registration is a no-op and nothing is ever a named code value.
(define (register-code-value! . _) #f)
;; chez names a procedure def-var! never saw, so a state image can write it as a
;; var reference. Gambit has no image; the registration is a no-op.
(define (register-proc-name! v . _) v)
(define (code-value? x) #f)

;; jclass?/jclass-name and the class objects they read live in host-vars.ss.

;; ---- concurrency tier stubs (demo boot: single-threaded) ---------------------
;; java/concurrency.ss + natives-queue.ss are excluded from this boot. Gambit
;; satisfies the threads CONTRACT (G0: parameters fork-inherit, mutexes
;; non-recursive), so a real port is possible — tracked on the epic; the demo
;; REPL is single-threaded. Predicates answer #f (nothing can construct these
;; types here); constructors and operations raise a message-carrying condition.
(define (jolt-conc-unsupported who)
  (lambda _ (error who "futures/promises/agents are unsupported in the gambit demo boot")))
(define (jolt-future? x) #f)
(define (jolt-promise? x) #f)
(define (jolt-agent? x) #f)
(define (jolt-delay? x) #f)
(define (jolt-queue? x) #f)
(define (jolt-conc-realized? x) #f)
(define (jolt-native-future-done? x) #f)
(define (jolt-native-future-cancelled? x) #f)
(define jolt-agent-new (jolt-conc-unsupported 'agent))
(define jolt-agent-send (jolt-conc-unsupported 'send))
(define jolt-agent-await (jolt-conc-unsupported 'await))
(define jolt-agent-error (jolt-conc-unsupported 'agent-error))
(define jolt-agent-restart (jolt-conc-unsupported 'restart-agent))
(define jolt-promise-new (jolt-conc-unsupported 'promise))
(define jolt-deliver (jolt-conc-unsupported 'deliver))

;; ---- class-registry seams (host-static-classes.ss is excluded) ---------------
;; deftype/defrecord registration calls: recorded in minimal tables (nothing on
;; this boot reads them back — no host interop), so definitions succeed.
(define class-ctors-tbl (make-hashtable string-hash string=?))
(define (register-class-ctor! tag ctor) (hashtable-set! class-ctors-tbl tag ctor))
(define class-methods-tbl (make-hashtable string-hash string=?))
(define (register-class-methods! tag methods) (hashtable-set! class-methods-tbl tag methods))
(define instance-check-arms '())
(define (register-instance-check-arm! h) (set! instance-check-arms (cons h instance-check-arms)))
(define (register-instance-check! cls pred) #f)
