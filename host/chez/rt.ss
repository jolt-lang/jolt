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
(load "host/chez/scheme-adapter-runtime.ss")
;; Before everything else that locks: jolt-with-mutex is a MACRO, so it has to be
;; defined before any file that uses it is loaded. It depends on nothing but Chez.
(load "host/chez/locks.ss")
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
       ;; Decide at RUNTIME, not expansion: under compile-file a transformer
       ;; cannot call the runtime binding sa-os-family at expansion time
       ;; ("variable is not bound" in the compile-time environment), so emit
       ;; both branches under a runtime os-family test.
       (with-syntax
           ((win #'(guard (e (#t #f))
                   (sa-load-shared-object #f)
                   (and (sa-foreign-entry? name)
                        (sa-foreign-procedure-runtime name (quote args) (quote res) #f))))
            (unx #'(guard (e (#t #f))
                   (sa-load-shared-object #f)
                   (and (sa-foreign-entry? name)
                        (sa-foreign-procedure name args res)))))
         #'(if (eq? (sa-os-family) 'windows) win unx)))
      ;; With a calling convention — (__varargs_after n) for a VARIADIC C
      ;; function like ioctl or fcntl. The convention is what puts the variadic
      ;; arguments where the callee's va_list reads them: Apple arm64 passes
      ;; them on the stack, and a fixed-arity binding leaves them in registers,
      ;; so the call returns SUCCESS having read whatever was on the stack.
      ;; The Windows branch drops it — x64 passes variadic and named arguments
      ;; alike there, and the runtime (eval) construction has no slot for one.
      ((_ conv name (quote args) (quote res))
       (with-syntax
           ((win #'(guard (e (#t #f))
                   (sa-load-shared-object #f)
                   (and (sa-foreign-entry? name)
                        (sa-foreign-procedure-runtime name (quote args) (quote res) #f))))
            (unx #'(guard (e (#t #f))
                   (sa-load-shared-object #f)
                   (and (sa-foreign-entry? name)
                        (sa-foreign-procedure conv name args res)))))
         #'(if (eq? (sa-os-family) 'windows) win unx))))))

;; Same, for an entry point that BLOCKS (a pipe read, sigwait). The call must be
;; __collect_safe: a plain foreign call keeps the thread "active" for the
;; stop-the-world collector, so a call parked on an idle pipe would freeze every
;; other thread's GC. The caller owes it one more thing — anything C keeps a
;; pointer to across the wait has to live in foreign memory, since the collector
;; is free to move a bytevector while the thread is deactivated.
(define-syntax jolt-foreign-proc-blocking
  (lambda (x)
    (syntax-case x (quote)
      ((_ name (quote args) (quote res))
       ;; Same runtime os-family test as jolt-foreign-proc-safe, for the same
       ;; compile-file reason.
       (with-syntax
           ((win #'(guard (e (#t #f))
                   (sa-load-shared-object #f)
                   (and (sa-foreign-entry? name)
                        (sa-foreign-procedure-runtime name (quote args) (quote res) #t))))
            (unx #'(guard (e (#t #f))
                   (sa-load-shared-object #f)
                   (and (sa-foreign-entry? name)
                        (sa-foreign-procedure-blocking name args res)))))
         #'(if (eq? (sa-os-family) 'windows) win unx))))))

;; --- process-wide exit --------------------------------------------------------
;; System.exit halts the VM from ANY thread on the JVM. Chez's exit unwinds only
;; the CALLING thread, so off the main thread it was a silent no-op as far as the
;; process was concerned: a worker that decided the program should stop could not
;; stop it, and nothing said it had tried (jolt-7xls). A watchdog thread is the
;; shape that wants this — it can describe the hang it found and not end it.
;;
;; The boot thread keeps the normal path, exit handlers and all, including the
;; stdout/stderr flush cli-core installs as one. Any other thread flushes those
;; same two ports by hand and calls libc _exit. _exit rather than exit: the C exit
;; runs atexit handlers and flushes C stdio from a thread the C runtime is not
;; expecting it from, and jolt's buffered output is Chez's, not stdio's, so the
;; handlers buy nothing here and can deadlock. Skipping the unwind is right on its
;; own terms too — the JVM does not run finally blocks on System.exit either.
;;
;; jolt-foreign-proc-safe answers #f where the entry does not resolve, and then
;; this is exactly what it was before: an exit that only the boot thread can act
;; on. Nothing depends on the hard path existing, so a host without _exit degrades
;; rather than failing to boot.
(define jolt-boot-thread-id (get-thread-id))
(define jolt-c-exit (jolt-foreign-proc-safe "_exit" '(int) 'void))
(define (jolt-exit-process code)
  (if (or (eqv? (get-thread-id) jolt-boot-thread-id) (not jolt-c-exit))
      (exit code)
      (begin
        (guard (_ (#t #f)) (flush-output-port (current-output-port)))
        (guard (_ (#t #f)) (flush-output-port (current-error-port)))
        (jolt-c-exit code))))

;; Build a foreign procedure whose invocation returns both the native result
;; and the calling thread's native error slot as Scheme values. Chez captures
;; the slot in the foreign-call return path, before collect-safe reactivation or
;; later Scheme/native work can overwrite it.
;;
;; Select from the compiler target, not this process's machine-type: cross-image
;; builds rebind #%$target-machine while the build host remains unchanged. An
;; unrecognized target fails during expansion rather than guessing a nearby ABI.
(define-syntax jolt-ffi-native-error-convention-case
  (lambda (x)
    (syntax-case x ()
      ((_ get-last-error-form errno-form)
       (case (eval '(#%$target-machine))
         ((i3nt ti3nt a6nt ta6nt arm64nt tarm64nt)
          #'get-last-error-form)
         ((i3le ti3le a6le ta6le
           ppc32le tppc32le arm32le tarm32le
           arm64le tarm64le rv64le trv64le la64le tla64le
           i3osx ti3osx a6osx ta6osx
           ppc32osx tppc32osx arm64osx tarm64osx)
          #'errno-form)
         (else
          (error 'jolt-ffi-native-error-convention-case
                 "unsupported target machine"
                 (eval '(#%$target-machine)))))))))

(define-syntax jolt-ffi-native-error-procedure
  (lambda (x)
    (syntax-case x ()
      ((_ (conv ...) name args res)
       #'(jolt-ffi-native-error-convention-case
           (sa-foreign-procedure-native-error
            __get_last_error (conv ...) name args res)
           (sa-foreign-procedure-native-error
            __errno (conv ...) name args res))))))

;; --- how many processors can this process use ---------------------------------
;; Backs jolt.host/available-processors, which Runtime.availableProcessors and
;; pmap's look-ahead window read. Each host is asked the question the JVM asks
;; there, not merely "how many CPUs exist":
;;
;;   - Linux: sched_getaffinity, so a process pinned with taskset or confined to
;;     a cpuset sees only the CPUs it may actually run on. This is what the JVM
;;     and nproc report; sysconf(_SC_NPROCESSORS_ONLN) would name the whole
;;     machine regardless of the mask.
;;   - Darwin: sysctlbyname("hw.logicalcpu"), there being no affinity API.
;;   - Windows: NUMBER_OF_PROCESSORS, which the OS always sets.
;;
;; sysconf is the last resort for other POSIX hosts. _SC_NPROCESSORS_ONLN is a
;; per-libc number rather than a standard one (84 on glibc and musl, 58 on the
;; BSDs), and an unrecognised name yields -1, so both are tried and an
;; out-of-range answer is rejected.
;;
;; A cgroup CPU quota (`docker --cpus=2` on a 16-core host) is NOT honoured: the
;; JVM's container support divides quota by period and would say 2 where this
;; says 16. Tracked as a divergence (jolt-j4sd).
;;
;; Resolved once on first call, not at load time — in a built binary top-level
;; forms run during heap build on every startup, and this is a syscall.
(define cpu-count-getaffinity (jolt-foreign-proc-safe "sched_getaffinity" '(int size_t u8*) 'int))
(define cpu-count-sysctlbyname (jolt-foreign-proc-safe "sysctlbyname" '(string u8* u8* void* size_t) 'int))
(define cpu-count-sysconf (jolt-foreign-proc-safe "sysconf" '(int) 'long))
(define (cpu-count-sane n) (and n (exact? n) (fx>? n 0) (fx<=? n 4096) n))
;; cpu_set_t is 1024 bits; the kernel fills as many bytes as it has CPUs for.
(define (cpu-count-from-affinity)
  (and cpu-count-getaffinity
       (guard (e (#t #f))
         (let ((mask (make-bytevector 128 0)))
           (and (fx=? 0 (cpu-count-getaffinity 0 128 mask))
                (let loop ((i 0) (n 0))
                  (if (fx=? i 128)
                      (cpu-count-sane n)
                      (loop (fx+ i 1) (fx+ n (bitwise-bit-count (bytevector-u8-ref mask i)))))))))))
(define (cpu-count-from-sysctl)
  (and cpu-count-sysctlbyname
       (guard (e (#t #f))
         (let ((out (make-bytevector 4 0))
               (len (make-bytevector 8 0)))
           (bytevector-u64-native-set! len 0 4)
           (and (fx=? 0 (cpu-count-sysctlbyname "hw.logicalcpu" out len 0 0))
                (cpu-count-sane (bytevector-u32-native-ref out 0)))))))
(define (cpu-count-from-sysconf)
  (and cpu-count-sysconf
       (guard (e (#t #f))
         (or (cpu-count-sane (cpu-count-sysconf 84))
             (cpu-count-sane (cpu-count-sysconf 58))))))
(define (cpu-count-from-env)
  (let ((v (getenv "NUMBER_OF_PROCESSORS")))
    (and v (cpu-count-sane (string->number v)))))
(define cpu-count-cached #f)
(define (jolt-available-processors)
  (or cpu-count-cached
      (let ((n (or (cpu-count-from-affinity)
                   (cpu-count-from-sysctl)
                   (cpu-count-from-sysconf)
                   (cpu-count-from-env)
                   1)))                      ; never 0: callers size windows with it
        (set! cpu-count-cached n)
        n)))

(load "host/chez/collections.ss")
(load "host/chez/seq.ss")

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
;; The ^long hint coercion IS the reference's long cast (RT.longCast), so defer
;; to jolt-long-cast rather than re-deriving it — the hand-rolled version here
;; accepted an exact integer of ANY magnitude (so (fn [^long x] x) handed 2^64
;; returned it instead of raising "Value out of range for long"), and reached
;; Chez's truncate on a NaN or an infinity, which raises a raw host condition
;; where the reference answers 0 and IllegalArgumentException respectively.
;; jolt-long-cast fast-paths a fixnum first, and the emitter's own inline
;; (emit-nhint-coerce) already short-circuits that case before the call, so the
;; delegation costs nothing on the path that matters.
;; A char must NOT come through here even though jolt-long-cast takes one. The
;; reference's `long` CAST reaches RT.longCast's char overload -- (long \a) is
;; 97 on both runtimes -- but a ^long PARAMETER is RT.longCast(Object), which
;; rejects a Character outright. jolt-num-cast-throw raises the matching
;; ClassCastException (and NullPointerException for nil).
(define (jolt->fx x)
  (cond ((fixnum? x) x)
        ((char? x) (jolt-num-cast-throw x))
        (else (jolt-long-cast x))))
;; The ^long RETURN coercion is NOT the ^long PARAMETER coercion, and the
;; difference is observable. A parameter goes through RT.longCast, which
;; range-checks and raises "Value out of range for long". A return is
;; ((Number) x).longValue(), which never raises for a number:
;;
;;   exact integer   WRAPS to the low 64 bits     2^64 -> 0, 2^63 -> Long/MIN
;;   flonum          SATURATES (Java's d2l)       Inf -> Long/MAX, NaN -> 0
;;   ratio           truncates toward zero, then the same
;;
;; ((fn ^long [x] x) 18446744073709551616N) is 0 on the reference and was 2^64
;; here; every case above is checked against it. Anything not a number keeps the
;; cast throw rather than inventing an answer.
(define (jolt->fx-ret x)
  (cond ((fixnum? x) x)
        ((flonum? x)
         (cond ((nan? x) 0)
               ((>= x 9223372036854775807.0) 9223372036854775807)
               ((<= x -9223372036854775808.0) -9223372036854775808)
               (else (exact (truncate x)))))
        ((and (number? x) (exact? x) (integer? x)) (wrap64 x))
        ((and (number? x) (exact? x) (rational? x)) (wrap64 (exact (truncate x))))
        ;; a char is not a Number here either -- see jolt->fx
        ((char? x) (jolt-num-cast-throw x))
        (else (jolt-long-cast x))))
(define (jolt->fl x)
  (cond ((flonum? x) x)
        ((number? x) (exact->inexact x))
        (else (jolt-num-cast-throw x))))
;; jolt `not`: only nil and false are falsey.
;; Spliced, like the predicates in values.ss (see jolt-nil? there for why).
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
;; the whole process (16 on every platform jolt targets). jolt claims 0-9, listed
;; here so the assignment is in one place; nothing else in the runtime uses them.
;; A freshly forked thread starts every slot at fixnum 0, NOT #f, so "unset" means
;; fixnum 0 (the site slots hold a site pair or 0). Slot 0 was claimed by R1's
;; fibers (jolt-nvpr.2): it holds the current fiber record — a per-switch vreg
;; write is ~2ns against ~33ns for a thread-parameter write, which is what keeps
;; the 3.4M switches/sec design point (R0(c)); the fibers define re-defines the
;; value in fibers.ss so the standalone gate can load it without rt.ss. Slot 1
;; was freed by R3 (jolt-230w), which moved the R1 ring/mark vregs onto the
;; continuation, and re-claimed by fibers.ss for park-unwinding; the next free
;; slot is 10. The surviving slots keep their R2 numbers.
(define jolt-vreg-site 2)        ; ('ns/fn' . line) of the innermost live call site
(define jolt-vreg-catch-line 3)  ; the site at the throw a catch clause is handling
(define jolt-vreg-print-readably 4)  ; the print family's *print-readably* override; 0 = unset
(define jolt-vreg-current-fiber 0)  ; fibers.ss: the running fiber record, or 0 (fixnum) when not on a fiber
;; slot 1: fibers.ss jolt-vreg-park-unwinding — a park escape is unwinding this carrier
;; slot 5: hasheq.ss jolt-vreg-hasheq-caches — this thread's (symbol . string) hasheq tables
;; slot 7: locks.ss jolt-vreg-locks — how many locks this carrier holds; the
;;   scheduler refuses to preempt a fiber while it is non-zero
;; slot 6: fibers.ss jolt-vreg-fiber-winder-base — the winder chain this carrier
;;   dispatched the running fiber with, so the park's finally walk knows where to stop
;; slot 8: values.ss jolt-vreg-symcell-cache — this thread's bounded identity
;;   front cache over the symbol-string pool (intern-symbol-cell)
;; slot 9: java/host-static-methods.ss jolt-vreg-interrupt-box — this thread's
;;   interrupt flag. A vreg and NOT a thread parameter on purpose: a thread
;;   parameter is inherited by a forked thread, so the box had to carry the
;;   owning thread's id and be re-checked on every read; a vreg starts at
;;   fixnum 0 in a fresh thread, which is the property that workaround was
;;   buying.
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
;; A top-level form is a root: nothing tail-called it, so whatever the slot holds
;; when one starts is a returned call's residue — the reporter's validator cannot
;; tell it from a live pair when the innermost live frame is a host fn (the
;; loader, the eval loop), which registers no callees. compile-eval.ss clears the
;; slot when a form starts to compile and again when its compiled code starts to
;; run, since macroexpansion runs user code in between.
(define (jolt-site-reset!) (set-virtual-register! jolt-vreg-site 0))
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
;; The four tables above are written by jolt-register-callsite!, which the back
;; end emits into every compiled namespace and which therefore runs at LOAD —
;; concurrently, now that namespaces load in parallel. Each add is a
;; read-modify-write, so unlocked the adds simply lose each other: 8 threads
;; registering 6000 distinct sites each dropped 297 of 48000. A dropped edge is
;; a stack-trace reconstruction that stops early rather than a wrong one, but
;; "incomplete, never invented" is the claim this table makes, and losing entries
;; to a race is not how it was meant to hold.
;;
;; Writes only. The reads below are single-key reads of a STRONG hashtable, which
;; is safe unlocked for the reasons set out at var-table, and they sit on the
;; backtrace path where a lock would be pointless anyway. One mutex for all four
;; because one registration writes up to four of them and they are never read
;; under it.
(define jolt-callsite-mu (make-mutex))
(define (jolt-table-add! tbl key entry)
  (jolt-with-mutex jolt-callsite-mu
    (let ((cur (hashtable-ref tbl key '())))
      (unless (member entry cur)
        (hashtable-set! tbl key (cons entry cur))))))
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
             (jolt-throw-sitep (let ((s (virtual-register jolt-vreg-site)))
                                 (and (pair? s) s)))
             (raise (condition (make-message-condition (jolt-throw-message v))
                               (make-jolt-throw-condition v))))))
;; The same capture for a HOST condition (a fault raised outside jolt-throw:
;; car of a non-pair, a bad flvector index). Installed by the cli's run wrapper
;; via with-exception-handler, which runs BEFORE the stack unwinds — a guard's
;; handler runs after, when the frames are already gone. Identity-tagged with
;; the condition itself, which is what jolt-unwrap-throw hands the reporter for
;; a non-&jolt-throw raise. jolt throws skip this (they captured already, with
;; the RIGHT identity — overwriting would orphan their k).
;; The condition the stash below describes. A fault a `guard` catches never
;; reaches this handler (the guard's own is nearer), so the catch boundary
;; snapshots the site itself when it converts the condition (java/
;; host-faults.ss) — unless this handler already did, which is what the
;; identity says.
(define jolt-fault-captured (make-thread-parameter #f))
(define (jolt-capture-fault! c)
  (unless (jolt-throw-condition? c)
    ;; NO call/cc here: Chez already attaches &continuation to a serious
    ;; condition, and jolt-error-continuation reads it — a per-raise capture
    ;; would heap-freeze a whole stack for every INTERNALLY-CAUGHT host
    ;; condition, which a hot raise path cannot afford. Only the site pair is
    ;; stashed; an O(1) read.
    (jolt-fault-captured c)
    (jolt-throw-sitep (jolt-live-site))))
;; The site pair the vreg holds now, or #f.
(define (jolt-live-site)
  (let ((s (virtual-register jolt-vreg-site)))
    (and (pair? s) s)))
;; The value a raise carries, as jolt code sees it. A &jolt-throw condition
;; unwraps to the value it wraps. A raw Chez condition — a fault the host itself
;; raised, such as a primitive handed nil — becomes a typed jolt throwable, so a
;; catch binds something with a class, a message and the Throwable surface, and
;; a catch clause dispatches on that class like on any other. The conversion is
;; the java layer's (java/host-faults.ss installs it); until that file loads a
;; condition passes through as itself.
(define jolt-fault->throwable (lambda (c) c))
(define (jolt-unwrap-throw x)
  (cond ((jolt-throw-condition? x) (jolt-throw-condition-value x))
        ((condition? x) (jolt-fault->throwable x))
        (else x)))
;; The raw condition a converted fault came from, or #f: the reporter reads the
;; continuation Chez attached to it (source-registry.ss). Installed with the
;; conversion.
(define jolt-fault-condition-of (lambda (v) #f))
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
;; meta and macro? live IN the cell, not in side-tables keyed by it. They used to
;; be two eq-hashtables, which a Chez hashtable's lack of thread safety made a
;; hazard: a def on one thread writing them while another reads is unsynchronized
;; mutation, and the damage surfaces as a fault inside the collector. A field is
;; safe (two threads writing one field write a whole value, never a torn
;; structure) and it is FASTER than the lookup it replaces, which matters because
;; macro-var? is on the analyzer's hot path.
;;
;; dyn-bound? is the "has this var EVER had a thread binding" flag — Clojure's
;; Var.threadBound, and the whole reason a var read is not O(binding depth). See
;; dyn-binding.ss, which owns it; it lives here because it has to be a field for
;; the same reason meta and macro? are.
(define-record-type var-cell
  (fields ns name (mutable root) (mutable defined?) (mutable meta) (mutable macro?)
          (mutable dyn-bound?))
  (nongenerative var-cell-v4))
(define var-table (make-hashtable string-hash string=?))
(define var-table-mu (make-mutex))
;; var-table-mu covers EVERY mutation of var-table and of ns-has-vars-set below
;; (jolt-var's insert, declare-var!'s insert, def-var!'s index write, remove-ns's
;; sweep in ns.ss, the gate harnesses' prune) and every WHOLE-TABLE scan
;; (var-table-cells / var-table-entries). Single-key reads deliberately take no
;; lock. The asymmetry is not an oversight, so here is the reasoning in full,
;; against Chez 10's implementation.
;;
;; A STRONG general hashtable — which this is, unlike the weak-eq side-tables
;; elsewhere in the runtime — never mutates existing structure on the read path or
;; on a resize. s/newhash.ss: a non-resizing insert is one vector-set! of a
;; fully-formed list (`(cons (cons x v) bucket)`), an update is one set-cdr! of a
;; value, and adjust! builds a COMPLETE new bucket vector out of fresh spine
;; conses, leaving the old one untouched, then publishes it with a single
;; ht-vec-set!. $gen-hashtable-ref snapshots ht-vec once and takes its mask from
;; that same vector, so the index cannot be torn. A reader therefore walks a
;; consistent structure and the worst it can observe is a STALE MISS — which
;; jolt-var's double-check turns into a lock acquisition and a re-read.
;;
;; Measured, 16 threads probing-then-inserting 15k fresh keys each: with writers
;; unlocked, ~8.6k of 240k entries lost and no crash; with writers serialized and
;; reads still unlocked, 240k of 240k, repeatably. Lost entries were the real bug
;; (a vanished cell means the next lookup interns a fresh one and the previous
;; root is gone), and serializing writers is the whole fix.
;;
;; This does NOT generalize to the weak-eq tables. Their adjust! (s/library.ss)
;; relinks live cells in place with $set-tlc-next! while a reader may be walking
;; them, and the reader is unsafe primitive code, so an unlocked read there hangs
;; or faults. That is why those tables take locks (or go per-thread) and this one
;; does not. Do not "unify" the two policies.
;;
;; Locking the single-key read would cost 70 -> 95 ns on a probe, on a path
;; var-deref hits per var reference at an uncached site. It buys nothing here.
;;
;; Interning has to be atomic or two threads racing the same NEW name would each
;; get their own cell, and a def through one would be invisible through the other.
;; Double-checked: the hit path (every name after the first) takes no lock, and the
;; insert re-checks under the lock.
;; ns -> hashtable(name -> cell): the per-namespace index of var-table, kept in
;; lockstep with it under var-table-mu (both insert sites below, remove-ns's
;; sweep in ns.ss, and rebuild-ns-cells-index! for the gate harnesses' prune).
;; refer / ns-publics / ns-map / all-ns answer from a bucket instead of
;; scanning every interned var in the image — the scan made ONE refer cost
;; O(total vars) and a program load O(namespaces x total vars). The hotscaling
;; gate pins the shape (a tiny ns's ns-publics is independent of image size).
(define ns-cells-index (make-hashtable string-hash string=?))
(define (ns-cells-add! c)                ; caller holds var-table-mu
  (let* ((ns (var-cell-ns c))
         (b (or (hashtable-ref ns-cells-index ns #f)
                (let ((b (make-hashtable string-hash string=?)))
                  (hashtable-set! ns-cells-index ns b)
                  b))))
    (hashtable-set! b (var-cell-name c) c)))
;; a LOCKED snapshot of one namespace's cells (like var-table-cells, the lock
;; covers only the snapshot — callers iterate outside it).
(define (ns-cells-list ns)
  (jolt-with-mutex var-table-mu
    (let ((b (hashtable-ref ns-cells-index ns #f)))
      (if b (vector->list (hashtable-values b)) '()))))
(define (ns-index-names)
  (jolt-with-mutex var-table-mu (hashtable-keys ns-cells-index)))
;; the gate harnesses prune var-table wholesale by KEY; they call this after,
;; instead of teaching the prune loop about buckets.
(define (rebuild-ns-cells-index!)
  (jolt-with-mutex var-table-mu
    (hashtable-clear! ns-cells-index)
    (vector-for-each ns-cells-add! (hashtable-values var-table))))

(define (jolt-var ns name)
  (let ((k (string-append ns "/" name)))
    (or (hashtable-ref var-table k #f)
        (jolt-with-mutex var-table-mu
          (or (hashtable-ref var-table k #f)
              (let ((c (make-var-cell ns name (make-jolt-var-unbound ns name) #f #f #f #f)))
                (hashtable-set! var-table k c)
                (ns-cells-add! c)
                c))))))
;; A whole-table scan is the one read that DOES need the lock, and for a different
;; reason than a rehash: hashtable-keys/values read ht-size, allocate a
;; zero-filled vector of that length, then walk the buckets bounded by both. A
;; concurrent insert truncates the result; a concurrent remove-ns leaves trailing
;; FILL slots, and every caller here immediately does (var-cell-ns c) on each
;; element, which on a fill 0 is an error that takes down whatever was running.
;; Measured: 3000 scans racing a grow/shrink writer produced 1.66M slots that were
;; fill rather than entries.
;;
;; The lock covers only the snapshot. Callers iterate the returned vector outside
;; it, so no caller's body (jolt-assoc, intern-ns!, the image walker) runs under
;; the lock.
(define (var-table-cells) (jolt-with-mutex var-table-mu (hashtable-values var-table)))
(define (var-table-entries)
  (jolt-with-mutex var-table-mu
    (let-values (((ks vs) (hashtable-entries var-table))) (cons ks vs))))
;; non-creating lookup (resolve / find-var / ns-unmap): #f when absent, so a
;; probe never interns an empty cell.
(define (var-cell-lookup ns name) (hashtable-ref var-table (string-append ns "/" name) #f))
;; A direct-linked call to a seed var binds the var's root ONCE, when the def
;; that holds the site loads (backend emit-invoke, jolt.host/seed-callable?).
;; The compile-time check proved the root a procedure in the same seed, so
;; anything else here is a runtime that does not match the build — say so
;; rather than let the site fail on its first call with a bare Chez error.
(define (jolt-seed-root cell)
  (let ((r (var-cell-root cell)))
    (if (procedure? r)
        r
        (error 'jolt-seed-root
               (string-append "direct-linked seed var " (var-cell-ns cell) "/" (var-cell-name cell)
                              " is not bound to a procedure in this runtime")))))
(define (var-deref ns name) (var-cell-root (jolt-var ns name)))
;; def-var! / declare-var! return the VAR CELL, not the value — Clojure's `def`
;; evaluates to #'ns/name (a first-class var), so (var? (def x 1)) is true and
;; (pr-str (def x 1)) is "#'ns/x". The prelude's def-var! forms discard the
;; return, so this is transparent there.
;; proc -> (ns . name) for the var it was def'd into, so (class a-fn) can report a
;; JVM-style class name and clojure.spec.alpha's fn-sym can recover the symbol of a
;; bare-fn predicate. Weak so GC'd fns drop out. Last def of a given proc wins.
(define proc-name-tbl (make-weak-eq-hashtable))
;; The check and the set have to be one atomic step (first def of a proc wins), and
;; a def can come from any thread — nREPL sessions evaluate on their own.
;;
;; The READS take the same mutex, through proc-name-of. Serializing writers alone
;; would leave the case that actually bites: a hashtable-ref concurrent with a
;; hashtable-set! that rehashes reads a table mid-move, and the damage surfaces
;; later inside the collector naming nothing. Every read is cold — printing a fn's
;; arity name (seq.ss), the image walker (state-image.ss), the class arm
;; (java/host-class.ss) — so none of them can afford to skip it either.
(define proc-name-mu (make-mutex))
(define (proc-name-of v) (jolt-with-mutex proc-name-mu (hashtable-ref proc-name-tbl v #f)))
;; Name a procedure def-var! never saw. A core fn in VALUE position compiles to
;; the runtime's own procedure, not the var's root, and a native that is
;; set!-extended after its def-var! leaves that procedure unnamed -- so an image,
;; which writes a procedure as its var name, could not write values built from it.
;; post-prelude.ss calls this once everything has finished extending; the shared
;; file reaches it through this rather than the table, so the Gambit host can shim
;; it (jolt-6cwk).
(define (register-proc-name! v ns name)
  (jolt-with-mutex proc-name-mu (hashtable-set! proc-name-tbl v (cons ns name)))
  v)
;; "ns/name" of every var defined more than once with a value. Guarded by
;; var-table-mu like the other var-table side sets; reads are single-key.
(define var-redefined-set (make-hashtable string-hash string=?))
(define (var-redefined? ns name)
  (jolt-with-mutex var-table-mu
    (hashtable-contains? var-redefined-set (string-append ns "/" name))))
;; A var root that is CODE rather than data. A procedure always is; a multimethod
;; and a reify are code too, but they are RECORDS, so `procedure?` misses them and
;; nothing recorded their name -- which is why a state image walked a multimethod's
;; dispatch tables and refused it, instead of writing the var's name and resolving
;; it back to the live one (jolt-2cny). Registered rather than hardcoded because
;; multimethods.ss and records.ss both load after this file.
(define code-value-arms '())
(define (register-code-value! pred) (set! code-value-arms (cons pred code-value-arms)))
(define (code-value? v)
  (let loop ((ps code-value-arms))
    (cond ((null? ps) #f)
          (((car ps) v) #t)
          (else (loop (cdr ps))))))

(define (def-var! ns name v)
  ;; first def of a given proc wins, so an alias like (def inc' inc) — which binds
  ;; the SAME proc to a second var — doesn't rename inc.
  (when (or (procedure? v) (code-value? v))
    (jolt-with-mutex proc-name-mu
      (unless (hashtable-contains? proc-name-tbl v)
        (hashtable-set! proc-name-tbl v (cons ns name)))))
  (jolt-with-mutex var-table-mu (hashtable-set! ns-has-vars-set ns #t))
  (let ((c (jolt-var ns name)))
    ;; A var this def is REDEFINING -- it already had a value. Recorded because the
    ;; inline pass may not splice such a var's body: a caller compiled between two
    ;; defs would freeze the first one while a later caller splices the second, and
    ;; the same binary then answers two ways (jolt-rtjm). The build loads the whole
    ;; app before it emits any of it, so by stash time this set is complete.
    ;;
    ;; "Already had a value" is the root not being the unbound marker -- NOT
    ;; var-cell-defined?, which means interned/resolvable and is set by
    ;; declare-var! too (and by the compiler interning a global as it classifies
    ;; it), so it is true on the very first def of every var.
    ;;
    ;; That distinction is the point: (declare x) leaves the unbound root intact,
    ;; so a forward declaration ahead of the real defn is ONE definition and stays
    ;; spliceable. The string-append is inside the branch, so it runs once per
    ;; actual redefinition rather than once per def.
    (when (not (jolt-var-unbound? (var-cell-root c)))
      (jolt-with-mutex var-table-mu
        (hashtable-set! var-redefined-set (string-append ns "/" name) #t)))
    (var-cell-root-set! c v) (var-cell-defined?-set! c #t) c))
;; Value-position comparison references compile to the seq.ss chain singletons
;; (jolt-lt/gt/le/ge), not to the clojure.core var roots — the roots were later
;; re-bound by the checked numeric layer, so def-var! never saw these procs.
;; Register them so a stored comparator like (sorted-map-by >) travels as a
;; fn-ref (by name) like any other named core fn.
(jolt-with-mutex proc-name-mu
  (hashtable-set! proc-name-tbl jolt-lt (cons "clojure.core" "<"))
  (hashtable-set! proc-name-tbl jolt-gt (cons "clojure.core" ">"))
  (hashtable-set! proc-name-tbl jolt-le (cons "clojure.core" "<="))
  (hashtable-set! proc-name-tbl jolt-ge (cons "clojure.core" ">=")))
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

;; --- telemetry primitives ----------------------------------------------------
;; Chez already keeps everything an observability layer needs — two clocks and the
;; collector's counters — behind procedures returning Chez-specific record types
;; (time objects, sstats). These republish them as plain exact integers, the only
;; shape a Clojure caller can do arithmetic on, so a telemetry library reads them
;; without knowing about `time` or `sstats`.
;;
;; The two clocks are NOT interchangeable and both are needed:
;;   wall-nanos  ('time-utc)       nanoseconds since the Unix epoch. The only clock
;;                                 that can date an event for a remote collector, so
;;                                 a span's start/end timestamp uses it. It is not
;;                                 monotonic — ntp can step it backwards, which is
;;                                 exactly why it must not be used to time anything.
;;   mono-nanos  ('time-monotonic) nanoseconds from an arbitrary origin, never
;;                                 steps. A DURATION must be measured with this,
;;                                 else a clock adjustment mid-span yields a
;;                                 negative or wildly long elapsed time.
;; A span therefore records a wall start AND a mono start, and derives its end as
;; wall-start + (mono-end - mono-start).
(define nanos/sec 1000000000)
(define (time->nanos t) (+ (* nanos/sec (time-second t)) (time-nanosecond t)))
(define (jolt-wall-nanos) (time->nanos (current-time 'time-utc)))
(define (jolt-mono-nanos) (time->nanos (current-time 'time-monotonic)))
(def-var! "jolt.host" "wall-nanos" jolt-wall-nanos)
(def-var! "jolt.host" "mono-nanos" jolt-mono-nanos)

;; Collector + memory counters. sa-stats takes one statistics snapshot per read,
;; so these are polling-cadence primitives (a metrics reader every N seconds),
;; not per-span ones. All six fields come from that one snapshot: at the OTel
;; layer the fields are independent instruments observed separately, so sharing
;; a snapshot is not observable in the exported data.
(def-var! "jolt.host" "cpu-nanos"     (lambda () (vector-ref (sa-stats) 0)))
(def-var! "jolt.host" "real-nanos"    (lambda () (vector-ref (sa-stats) 1)))
(def-var! "jolt.host" "gc-count"      (lambda () (vector-ref (sa-stats) 2)))
(def-var! "jolt.host" "gc-cpu-nanos"  (lambda () (vector-ref (sa-stats) 3)))
(def-var! "jolt.host" "gc-real-nanos" (lambda () (vector-ref (sa-stats) 4)))
(def-var! "jolt.host" "gc-bytes"      (lambda () (vector-ref (sa-stats) 5)))
;; bytes-allocated is the live heap (what survived the last collection, plus what
;; has been allocated since); current/maximum-memory-bytes are what Chez holds from
;; the OS now and at its peak — the RSS-shaped numbers a runtime memory gauge wants.
(def-var! "jolt.host" "bytes-allocated"      (lambda () (sa-bytes-allocated)))
(def-var! "jolt.host" "current-memory-bytes" (lambda () (sa-total-memory-bytes)))
(def-var! "jolt.host" "maximum-memory-bytes" (lambda () (sa-max-memory-bytes)))
;; The calling thread's id, so telemetry can be read per-thread. Wrapped in a lambda
;; so the get-thread-id reference resolves at CALL time: a non-threaded Chez build
;; lacks the binding, and only a caller that actually asks for a thread id should
;; fail there rather than the whole runtime failing to load.
(def-var! "jolt.host" "thread-id" (lambda () (get-thread-id)))
;; The underlying runtime's identity, as strings. A telemetry resource has to name
;; the runtime and machine it is reporting from (process.runtime.*, host.arch);
;; System/getProperty answers neither here, since jolt has no JVM behind it.
;; sa-host-tag is the runtime's own tag, e.g. tarm64osx / a6le, which encodes both
;; the architecture and the OS.
(def-var! "jolt.host" "scheme-version" (lambda () (scheme-version)))
(def-var! "jolt.host" "machine-type" (lambda () (sa-host-tag)))

;; var def-time metadata: the :def emit passes the def's reader meta
;; (^:private / ^Type tag / docstring -> {:doc}) here, stored in an eq side-table
;; keyed by the cell. jolt-meta (natives-meta.ss) merges it onto {:ns :name},
;; which it derives from the cell — so EVERY var (plain def, native-op, declare)
;; reports {:ns :name} like Clojure, with the user meta layered on when present.
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
;; runtime-macro registry: a var whose root holds a macro
;; expander fn is flagged here, so the ON-CHEZ analyzer's form-macro?/form-expand-1
;; (host-contract.ss) expand it. The prelude emits each core/stdlib defmacro as a
;; def-var! of its (cross-compiled) expander followed by (mark-macro! ns name).
;; Kept in the cell, like meta — survives a later (def name ...) that
;; replaces the expander but keeps the same cell, matching Clojure (a defmacro IS a
;; def whose var carries :macro).
(define (mark-macro! ns name)
  (let ((c (jolt-var ns name))) (var-cell-macro?-set! c #t) c))
(define (macro-var? cell) (and cell (var-cell-macro? cell) #t))
;; declare / (def name) with no init: reserve the cell ONLY if absent. An
;; existing root is left intact — Clojure's (def x) with no init does not clobber
;; a prior binding (do (def x 7) (def x) x) => 7. Returns the cell either way.
;; Same double-check as jolt-var, and for the same reason: the insert is a
;; var-table mutation and has to be serialized against jolt-var's.
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
        (jolt-with-mutex var-table-mu
          (let ((c (hashtable-ref var-table k #f)))
            (if c
                (begin (var-cell-defined?-set! c #t) c)
                (let ((c (make-var-cell ns name (make-jolt-var-unbound ns name) #t #f #f #f)))  ; declared => interned/resolvable
                  (hashtable-set! var-table k c)
                  (ns-cells-add! c)
                  c)))))))

;; regex: defines regex-t + the re-* fns (def-var!'d into
;; clojure.core), so it loads after def-var! and before the printer below (which
;; renders a regex-t as #"source").
(load "host/chez/java/regex-translate.ss")
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
;; One pass through a string port: a right-fold of string-append re-copied the
;; whole joined suffix per element — O(n*L) on every collection render, and a
;; recursion as deep as the list (`make printscaling` gates the shape).
(define (jolt-str-join-sep strs sep)
  (cond ((null? strs) "") ((null? (cdr strs)) (car strs))
        (else
         (let-values (((op get) (open-string-output-port)))
           (put-string op (car strs))
           (let loop ((r (cdr strs)))
             (unless (null? r)
               (put-string op sep)
               (put-string op (car r))
               (loop (cdr r))))
           (get)))))
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
(define random-seeded? (make-thread-parameter #f))
(define random-seed-counter 0)
(define random-seed-mutex (make-mutex))

(define (seed-random!)
  (let* ((t (current-time 'time-utc))
         (n (jolt-with-mutex random-seed-mutex
              (set! random-seed-counter (+ random-seed-counter 1))
              random-seed-counter))
         (mix (bitwise-xor (time-nanosecond t)
                           (* 1000000007 (time-second t))
                           (* 2654435761 (get-process-id))
                           (* 40503 (get-thread-id))
                           (* 2246822519 n))))
    ;; random-seed's domain is 1 .. 2^32-1
    (random-seed (+ 1 (modulo mix 4294967294)))
    (random-seeded? #t)))

;; Every jolt-level draw goes through this, never `random` directly.
(define (jolt-random n)
  (unless (random-seeded?) (seed-random!))
  (random n))

;; --- OS entropy ---------------------------------------------------------------
;; jolt-random is seeded from the clock, which makes it unique per process but
;; still guessable: an observer who knows roughly when a process started can
;; narrow the seed to a small range and enumerate it. Clojure backs random-uuid
;; with SecureRandom because callers do use v4 UUIDs as session ids, CSRF nonces
;; and password-reset links, where guessable means forgeable. The bytes behind a
;; UUID therefore come from the OS, not from any seeded PRNG.
;;
;; Resolved lazily on first use and cached, never at load time: in a built binary
;; top-level forms run during heap build, so a port opened there would be baked
;; into the image and be a stale descriptor — shared by every process started
;; from it — by the time anything drew from it.
(define entropy-source 'unset)      ; 'unset | #f | input port (posix) | procedure (windows)

(define (entropy-open-urandom)
  (guard (e (#t #f))
    (and (file-exists? "/dev/urandom")
         (open-file-input-port "/dev/urandom" (file-options) (buffer-mode none)))))

;; Windows has no /dev/urandom. BCryptGenRandom with a null algorithm handle and
;; BCRYPT_USE_SYSTEM_PREFERRED_RNG (2) is the documented system CSPRNG;
;; RtlGenRandom — exported from advapi32 only under its ordinal alias
;; SystemFunction036 — covers anything without bcrypt. eval rather than a
;; compiled foreign-procedure for the same reason jolt-foreign-proc-safe uses it
;; there: on Windows a foreign reference inside a fasl is a load-time relocation
;; that aborts the boot outright if the symbol is missing, before any guard runs.
(define (entropy-open-windows)
  (guard (e (#t #f))
    (or (and (guard (e2 (#t #f)) (sa-load-shared-object "bcrypt.dll") #t)
             (sa-foreign-entry? "BCryptGenRandom")
             (let ((f (sa-foreign-procedure-runtime "BCryptGenRandom"
                                                    '(void* u8* unsigned-32 unsigned-32)
                                                    'int #f)))
               (lambda (bv n) (fx=? 0 (f 0 bv n 2)))))
        (and (guard (e2 (#t #f)) (sa-load-shared-object "advapi32.dll") #t)
             (sa-foreign-entry? "SystemFunction036")
             (let ((f (sa-foreign-procedure-runtime "SystemFunction036"
                                                    '(u8* unsigned-32) 'boolean #f)))
               (lambda (bv n) (f bv n)))))))

(define (jolt-entropy-source)
  (when (eq? entropy-source 'unset)
    (set! entropy-source
          (if (eq? (sa-os-family) 'windows)
              (entropy-open-windows)
              (entropy-open-urandom))))
  entropy-source)

(define entropy-warned? #f)
(define (entropy-warn-once!)
  (unless entropy-warned?
    (set! entropy-warned? #t)
    ;; Loud, once: silently degrading a CSPRNG to a clock-seeded PRNG is how
    ;; guessable session ids ship. Uniqueness still holds; unpredictability does not.
    (display "jolt: no OS entropy source available -- random-uuid is falling back to the seeded PRNG\n"
             (console-error-port))))

;; `n` bytes from the OS CSPRNG. Falls back to the seeded PRNG only if the host
;; offers no entropy source at all, which on every platform jolt ships is never.
(define (jolt-random-bytes n)
  (let ((bv (make-bytevector n 0))
        (src (jolt-entropy-source)))
    (cond
      ((and (port? src)
            (guard (e (#t #f))
              ;; short reads are legal on a character device — loop to n
              (let loop ((got 0))
                (or (fx>=? got n)
                    (let ((k (get-bytevector-n! src bv got (fx- n got))))
                      (and (fixnum? k) (fx>? k 0) (loop (fx+ got k))))))))
       bv)
      ((and (procedure? src) (guard (e (#t #f)) (src bv n))) bv)
      (else
       (entropy-warn-once!)
       (let loop ((i 0))
         (if (fx=? i n)
             bv
             (begin (bytevector-u8-set! bv i (jolt-random 256))
                    (loop (fx+ i 1)))))))))

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
;; extend-type/reify. Four files, order load-bearing: the jrec layout, its arms
;; on the collection dispatchers, the protocol registry + resolution, and the
;; .method interop dispatcher. After multimethods.ss (chez-current-ns) and the
;; dispatchers/printers the arms wrap (collections/seq/values/converters/
;; printing/transients).
(load "host/chez/records.ss")
(load "host/chez/records-coll.ss")
(load "host/chez/protocols.ss")
(load "host/chez/records-dispatch.ss")
(load "host/chez/java/records-interop.ss")   ; exception hierarchy + instance-check taxonomy
(load "host/chez/java/host-faults.ss")       ; a raw host fault caught = a typed throwable

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

;; extension points: the keyed provider registry core declares a
;; contract on and a library fills in (jolt.host/register-extension-point! /
;; register-extension! / refine-extension! / extension-value). Host-neutral — the
;; java layer and libraries are clients. After collections.ss (jolt maps),
;; converters.ss + the printers (error messages render values), and rt.ss's
;; throw-jvm.
(load "host/chez/extensions.ss")

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

;; #inst values + the java.util/java.text date layer: jinst (RFC3339 ms), Date,
;; sql.Date/Timestamp, Calendar, TimeZone, SimpleDateFormat. Loads LAST — it
;; extends record-method-dispatch / jolt-get / jolt= / jolt-hash / jolt-pr-str /
;; jolt-type / instance-check and uses host-static.ss's registries. libc time
;; primitives (zone offset, locale names) exposed as jolt.host vars, used by the
;; library's zone/localized layer.
(load "host/chez/java/tz-primitives.ss")
(load "host/chez/java/inst-time.ss")

;; java.time is split (RFC 0008): the base VALUE types (Instant, LocalDate/Time/
;; DateTime, Duration, Period, Year/YearMonth, enums) are portable Clojure under
;; stdlib/jolt/time/, autoloaded on first use by host-static.ss with no dependency.
;; Everything that formats or names a zone (DateTimeFormatter, ZoneOffset/ZoneId,
;; ZonedDateTime/OffsetDateTime, localized formatting, java.util.Locale) is the
;; jolt-lang/time library — one implementation of each, not carried in core.

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

;; java.lang.ProcessBuilder / Process. After io-streams (make-in-stream /
;; make-out-stream) and host-static-methods (all-env-pairs).
;; proxy: extends-by-delegation over a concrete host class. After host-static.ss
;; (host-new + the ctor table it probes), records-interop.ss (instance-check) and
;; io-streams.ss, so a proxy over a stream class finds its constructor.
(load "host/chez/java/proxy.ss")
(load "host/chez/java/process.ss")

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

;; Fibers (R1, jolt-nvpr.2): the fiber primitive + single-carrier scheduler
;; behind the CONTRACT.txt `coroutines` tier. Also loaded by
;; scheme-adapter-runtime.ss (which loads first) so the adaptercheck gate sees
;; the sa-fiber-* names; this second load is a harmless re-define. After
;; async.ss — the R3/R4 channel path is the first consumer, and fibers.ss
;; itself needs nothing from the runtime.
(load "host/chez/fibers.ss")

;; Fiber-aware <! / >! (R3, jolt-nvpr.4): the one waiter protocol for threads
;; and fibers. After BOTH async.ss (the channel + handler machinery) and
;; fibers.ss (sa-fiber-resume) — it registers the fiber wakeup strategy and
;; the jolt-fiber-<! / jolt-fiber->! primitives. Not loaded by the adapter
;; runtime (it depends on async.ss).
(load "host/chez/java/fibers-async.ss")

;; Escape continuations as a jolt API (issue #736): jolt.host/call-cc, which
;; stdlib/jolt/continuations.clj presents as call-cc / letcc. After fibers.ss —
;; the guard that refuses an escape captured on another fiber reads the fiber
;; vreg, and refusing is what keeps a cross-fiber invoke from hanging.
(load "host/chez/continuations.ss")

;; The cheap park (R7, jolt-nvpr.9): __sm-spawn/__sm-take/__sm-put, the ops a
;; CPS'd go body (the pass in clojure.core.async) calls. A
;; lexically-parking body stores the rest of the computation as an ordinary
;; closure in the fiber's SM field, clears k, and switches without capturing a
;; continuation — no stack segment held while parked. Those two writes are the
;; whole resume rule: jolt-fiber-resume* takes k when it is set and re-enters
;; through the thunk when it is clear, which is what puts the driver (and the
;; body's exception handler) back for a cheap resume. Loaded after
;; fibers-async.ss (reuses its waiter handler and channel protocol).
(load "host/chez/java/sm.ss")

;; BigDecimal: the jbigdec value type + bigdec/decimal?/class/equality/
;; printing. Loads LAST so its set!-wraps of jolt-class/jolt=2/the printers sit
;; outermost over every earlier extension.
(load "host/chez/java/bigdec.ss")

;; The library seam for extending / overriding a class jolt already part-shims.
;; After every java shim and after bigdec.ss's class-arm wraps, so the class name
;; a lookup resolves against is the final one.
(load "host/chez/java/class-extensions.ss")

;; Native stack traces: jv$ns$name -> source registry + continuation frame walk +
;; uncaught-throwable renderer. After the printers/equality it relies on.
(load "host/chez/source-registry.ss")

;; Unique anon-fn names -> {source form, ns, free locals} for the image write
;; side. Plain defines (no def-var! / manifest lines): only emitted code and the
;; image writer call them, never Clojure.
(load "host/chez/fn-form-registry.ss")

;; State images: dump the value graph to a file and read it back. Loads LAST —
;; walks jolt collections, var cells and atoms, prints paths through the printers,
;; and reads proc-name-tbl to write a fn as its var's name.
(load "host/chez/state-image.ss")
