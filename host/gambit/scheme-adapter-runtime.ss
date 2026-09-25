;; scheme-adapter-runtime.ss — the RUNTIME half of the portable-scheme-layer
;; adapter (PSL R3, epic jolt-867l) — GAMBIT TARGET (native gsi, G1 demo).
;;
;; The Chez adapter (host/chez/scheme-adapter-runtime.ss) owns the system-tier
;; FORBIDDEN names and exposes them through named capability entry points; a
;; non-Chez target implements (or explicitly degrades) this one small surface
;; instead of chasing call sites. This file is the Gambit implementation:
;; same sa-* names, same call shapes, same doc discipline, with the G1 demo
;; degradations below. Loaded FIRST in every boot path, after prelude-shims.ss
;; (which owns the (gambit) import) — so this file needs no import of its own.
;;
;; G1 degradation summary (each is documented at its entry point):
;;   - real:  sa-real-time-ms, sa-file-mtime-ms, sa-host-tag and the identity
;;            derived values. Everything else that can be honest is honest.
;;   - ffi:   the ENTIRE tier raises a message-carrying condition
;;            ((error 'sa-foreign-alloc "ffi is unsupported on the gambit
;;            target")) — never a bare raise (jolt surfaces ex-message, and a
;;            bare raise surfaces nil; that lesson is paid for). The G2 boot
;;            manifest excludes java/ffi.ss entirely, so these are only
;;            reached if jolt-level ffi is attempted.
;;   - native-compile / image: raise, message-carrying — the AOT cache and
;;            jolt.image treat the raise as a clean cache disable / unsupported.
;;   - introspect: off (sa-introspect-enabled? is #f) — frames '() / proc-info
;;            #f / zero-vector stats; backtraces render from the compile-time
;;            tables alone, exactly the documented degraded mode.
;;   - subprocess: sa-run-process raises (demo target; Gambit's open-process
;;            ports exist but the demo scope does not ship a process tier).

;; ---- system tier (capability: system) --------------------------------------

;; (sa-run-process cmd transcoder) -> (values stdin stdout stderr pid)
;; Spawn CMD (a shell string) with block buffering and the given transcoder
;; (#f = binary), returning the four process ports and the pid. Contract: a
;; target must provide exactly this shape. Degradation: a target without
;; subprocess support must raise 'unsupported — every caller genuinely needs
;; the subprocess, so none of them may be silently degraded. The G1 demo
;; raises with a message-carrying condition.
(define (sa-run-process cmd transcoder)
  (error 'sa-run-process "subprocess support is unsupported on the gambit target" cmd))

;; (sa-environment-pairs) -> list of (name . value)
;; The whole process environment, the alist R7RS get-environment-variables
;; answers, which Gambit provides. Contract: that alist. Degradation: none.
(define (sa-environment-pairs)
  (get-environment-variables))

;; (sa-os-release) -> string | #f
;; The OS's own version string (the product version on macOS, the kernel
;; release elsewhere), or #f. Contract: that string or #f. Degradation: #f,
;; which leaves os.version out of the system properties; the demo target does
;; not probe the OS.
(define (sa-os-release)
  #f)

;; (sa-gc-collect) -> void
;; A full collection hint (every generation) — what the Runtime.gc /
;; System.gc callers mean: weak references clear and guardians fire.
;; Contract: perform a full collection. Degradation: may no-op — callers
;; already guard the call and the JVM semantic is only a hint. Gambit's
;; collector runs on its own schedule; no-op.
;; sa-record-cas!: a record here is a vector with the type id in slot 0, so
;; field i is slot i+1; the swap runs under one mutex because a green thread
;; can be preempted between the read and the write.
(define sa-record-cas-mu (make-mutex))
(define (sa-record-cas! r i old new)
  (jwm-call sa-record-cas-mu
    (lambda ()
      (let ((k (+ i 1)))
        (if (eq? (vector-ref r k) old)
            (begin (vector-set! r k new) #t)
            #f)))))

(define (sa-gc-collect)
  #f)

;; (sa-gc-max-generation) -> exact integer
;; The deepest collectable generation, for callers mapping JVM generations.
;; Contract: return the maximum generation argument sa-gc-collect accepts.
;; Degradation: a single-generation collector returns 0.
(define (sa-gc-max-generation)
  0)

;; (sa-bytes-allocated) -> exact integer
;; Live-heap bytes — the "used" reading under current-memory-bytes that
;; Runtime.freeMemory is built from. Contract: bytes currently allocated and
;; live. Degradation: an approximation is acceptable; callers only subtract it
;; from total to compute free memory. Gambit's ##bytes-allocated is used when
;; bound (gsi native); 0 otherwise.
(define (sa-bytes-allocated)
  (guard (e (#t 0))
    (##bytes-allocated)))

;; (sa-total-memory-bytes) -> exact integer
;; Total process heap bytes — what the collector has reserved from the OS
;; (the JVM's totalMemory; Runtime.freeMemory is the difference against
;; sa-bytes-allocated). Contract: total heap bytes. Degradation: an
;; approximation is fine — callers only display it or subtract
;; sa-bytes-allocated from it. A large constant is the documented fallback
;; (the JVM arm already maps an unbounded heap to Long/MAX_VALUE).
(define (sa-total-memory-bytes)
  9223372036854775807)

;; (sa-max-memory-bytes) -> exact integer
;; Peak heap bytes since the last sa-reset-max-memory-bytes! -- the high-water
;; mark behind jolt.host/maximum-memory-bytes (not the JVM's maxMemory, which
;; is the heap ceiling). Contract: never below sa-total-memory-bytes.
;; Degradation: the current total -- Gambit keeps no high-water mark, so this
;; answers "now", which here is the same constant as the total.
(define (sa-max-memory-bytes)
  (sa-total-memory-bytes))

;; (sa-reset-max-memory-bytes!) -> void
;; Start the high-water mark over. Contract: right after it, sa-max-memory-bytes
;; answers the current total. Degradation: a no-op, which is exact here since
;; sa-max-memory-bytes already answers the current total.
(define (sa-reset-max-memory-bytes!)
  #f)

;; (sa-gc-install-after-collect! maintain observe) -> boolean
;; Permitted degradation: Gambit exposes no hook equivalent to Chez's
;; collect-request-handler, so answer #f and install nothing. The heap is then
;; unbounded and the nursery fixed, which is what every jolt before 0.8.5 did on
;; every target, and the caller reports maxMemory as unbounded rather than
;; promising a bound it cannot enforce.
(define (sa-gc-install-after-collect! maintain observe)
  #f)

;; (sa-gc-install-stall-watch! seconds on-stall) -> boolean
;; Permitted degradation: Gambit's collector is not a rendezvous of active
;; threads that a foreign call can hold off, and it exposes no hook on the
;; wait; answer #f and install nothing. The diagnostic is then absent, which
;; is what every release before 0.8.9 did on every target.
(define (sa-gc-install-stall-watch! seconds on-stall)
  #f)

;; (sa-thread-id-of t) -> fixnum | #f
;; Permitted degradation: #f — no per-thread number is readable from a
;; thread object here, so the fork wrapper records nothing for the child and
;; nothing consumes the record (no stall watch is installed on this host).
(define (sa-thread-id-of t)
  #f)

;; (sa-real-time-ms) -> exact integer
;; Wall-clock milliseconds, monotonic within a process — used for elapsed
;; deltas (build profiling) and unique temp-file stamps. Contract: an
;; exact-integer ms clock usable for both. Gambit's (real-time) returns a
;; monotonic elapsed-seconds FLONUM (millisecond precision); convert to an
;; exact ms count.
(define (sa-real-time-ms)
  (inexact->exact (floor (* (real-time) 1000))))

;; ---- R5: io remainder (mtime) + the last GC hook -----------------------------

;; (sa-file-mtime-ms path) -> exact integer
;; Epoch milliseconds of PATH's last modification. Contract: a per-file mtime
;; usable for newer-than comparisons (build freshness, AOT cache keys, the
;; gate-boot staleness predicate). Degradation: none — stat is universal on
;; real targets; do not fake. Gambit's file-info-modification-time returns a
;; time object (converted via the bound time->seconds) or seconds directly.
(define (sa-file-mtime-ms path)
  ;; Gambit spells it file-info-last-modification-time (returns a time object);
  ;; time->seconds gives fractional epoch seconds.
  (let ((t (time->seconds (file-info-last-modification-time (file-info path)))))
    (if (number? t)
        (inexact->exact (floor (* t 1000)))
        (inexact->exact (floor (* (time->seconds t) 1000))))))

;; (sa-gc-trip-bytes! n) -> void
;; Set the allocation threshold at which a trip collection triggers — the
;; dev-cache CLI's GC tuning knob. Contract: honor N as a collection-trip
;; hint. Degradation: may no-op — the call tunes a dev cache only; collection
;; still happens on its own schedule.
(define (sa-gc-trip-bytes! n)
  #f)

;; (sa-gc-trip-bytes) -> exact integer
;; The allocation threshold at which a trip collection triggers. Contract: the
;; threshold in bytes. Degradation: 0 -- Gambit exposes no such threshold, so
;; nothing is invisible by construction and a floor built on it is no floor.
(define (sa-gc-trip-bytes)
  0)

;; ---- R6: introspection tier (capability: introspect) -------------------------

;; sa-introspect-enabled? — dynamic parameter, FIXED #f on this target. The
;; degraded-backtrace gate flips it to #f on Chez to prove that throw surfaces
;; still carry type+message while every introspect entry point returns
;; empty/#f; the gambit target IS that mode, permanently.
(define sa-introspect-enabled? (make-parameter #f))

;; (sa-host-tag) -> string
;; The runtime's host tag. NAMING ONLY: release/fasl directory names, image
;; headers, telemetry strings, error text. No logic may branch on it — logic
;; branches use sa-os-family / sa-arch / sa-endian. Contract: an opaque,
;; per-build-stable host string.
(define (sa-host-tag)
  "gambit")

;; (sa-os-family) -> 'macos | 'windows | 'linux
;; The OS family every host OS branch derives. Contract: one of the three
;; symbols. Degradation: none — the sites have no safe assumed default; an
;; unrecognized host falls back to 'linux, matching the Chez adapter's
;; else-branches. The G1 demo runs no OS branches, so 'linux is documented as
;; the else-default, not probed.
(define (sa-os-family)
  'linux)

;; (sa-arch) -> 'x86-64 | 'arm64 | 'i386 | 'other
;; The machine architecture. Contract: the architecture symbol. Degradation:
;; 'other for an unrecognized host; callers treat it as unverified.
(define (sa-arch)
  'other)

;; (sa-endian) -> 'little | 'big | #f
;; Byte order of the host. Contract: the byte order. The gambit target's
;; double-to-raw-bits byte layout (hasheq.ss) assumes little-endian; both
;; supported platforms (arm64/x86-64 macOS and linux) are little-endian.
(define (sa-endian)
  'little)

;; (sa-stats) -> #(cpu-nanos real-nanos gc-count gc-cpu-nanos gc-real-nanos
;;                gc-bytes)
;; One statistics snapshot as the six exact-integer fields rt.ss reads into
;; the jolt.host telemetry vars (time fields pre-converted to nanos). Contract:
;; the six fields in this order. Degradation: a zero vector — the OTel layer
;; maps zeros to absent metrics.
(define (sa-stats)
  (vector 0 0 0 0 0 0))

;; (sa-continuation-frames k) -> list of frame inspectors | '()
;; The continuation's frames, innermost first. Contract: stepping a throw
;; continuation. Degradation: '() — the backtrace then renders from the
;; compile-time tables alone (jolt-backwalk) or reports no frames.
(define (sa-continuation-frames k)
  '())

;; (sa-procedure-info x) -> (name . ((free-name . value) ...)) | #f
;; A procedure's inspector name and live free-variable captures. Contract:
;; name + captured values. Degradation: #f — the image writer refuses the
;; closure ('image-no), the same verdict as today's no-inspector builds.
(define (sa-procedure-info x)
  #f)

;; (sa-procedure-code-name p) -> string | #f. Gambit names a procedure by its
;; define, not by a let binding, so a closure of an inner lambda answers #f;
;; the contract permits #f.
(define (sa-procedure-code-name p)
  (let ((n (##procedure-name p)))
    (and (symbol? n) (symbol->string n))))

;; ---- R7: ffi tier (capability: ffi) — entirely unsupported, all raise -------

;; (sa-ffi-raise who) -> never returns
;; The single raising helper the whole ffi tier lowers to. ALWAYS a
;; message-carrying condition — jolt surfaces the message through ex-message,
;; and a bare raise surfaces nil.
(define (sa-ffi-raise who)
  (error who "ffi is unsupported on the gambit target"))

;; (sa-foreign-procedure name args res) -> foreign procedure
;; SYNTAX: compile-time-typed foreign-procedure creation. Contract: build a
;; foreign procedure for a statically-known signature. Degradation: the gambit
;; target has no ffi tier — expands to a call of the raising helper, so any
;; attempt surfaces the documented jolt-level unsupported error.
(define-syntax sa-foreign-procedure
  (syntax-rules ()
    ((_ name args res) (sa-ffi-raise 'sa-foreign-procedure))
    ((_ conv name args res) (sa-ffi-raise 'sa-foreign-procedure))))

;; (sa-foreign-procedure-native-error error-convention conv name args res)
;; -> foreign procedure
;; SYNTAX: an atomic native-error-capturing foreign procedure. Degradation: the
;; gambit target has no ffi tier, so every shape raises the same documented
;; unsupported error as the rest of the tier.
(define-syntax sa-foreign-procedure-native-error
  (syntax-rules ()
    ((_ error-convention conv name args res)
     (sa-ffi-raise 'sa-foreign-procedure-native-error))))

;; The compiler emits this target wrapper so Chez can select errno versus
;; GetLastError at expansion time. Gambit has neither native FFI convention;
;; route it through the adapter capability so it degrades honestly.
(define-syntax jolt-ffi-native-error-procedure
  (syntax-rules ()
    ((_ conv name args res)
     (sa-foreign-procedure-native-error unsupported-native-error
                                        conv name args res))))

;; (sa-foreign-procedure-blocking name args res) -> foreign procedure
;; SYNTAX: like sa-foreign-procedure, but the call is __collect_safe. Contract:
;; mark the call so a blocking foreign invocation does not stop other threads'
;; GC. Degradation: no ffi tier on this target — raise, like the rest.
(define-syntax sa-foreign-procedure-blocking
  (syntax-rules ()
    ((_ name args res) (sa-ffi-raise 'sa-foreign-procedure-blocking))
    ((_ conv name args res) (sa-ffi-raise 'sa-foreign-procedure-blocking))))

;; (sa-foreign-callable proc args res) -> foreign callable
;; SYNTAX: compile-time-typed foreign-callable creation, mirroring
;; sa-foreign-procedure. Contract: build a foreign callable around a Scheme
;; procedure for a statically-known signature. Degradation: raise, like the rest.
(define-syntax sa-foreign-callable
  (syntax-rules ()
    ((_ proc args res) (sa-ffi-raise 'sa-foreign-callable))))

;; (sa-foreign-callable-collect-safe proc args res) -> foreign callable
;; SYNTAX: like sa-foreign-callable, but the callable entry uses the
;; __collect_safe convention. Degradation: raise, like the rest.
(define-syntax sa-foreign-callable-collect-safe
  (syntax-rules ()
    ((_ proc args res) (sa-ffi-raise 'sa-foreign-callable-collect-safe))))

;; (sa-foreign-procedure-runtime name args res blocking?) -> foreign procedure | #f
;; Construct a foreign procedure from a RUNTIME-known signature. Contract:
;; create a foreign procedure for a runtime signature. Degradation: raise —
;; callers only ask when sa-foreign-entry? has said yes, which never happens
;; on this target.
(define (sa-foreign-procedure-runtime name args res blocking?)
  (sa-ffi-raise 'sa-foreign-procedure-runtime))

;; (sa-foreign-alloc n) -> pointer
;; Allocate N raw bytes of foreign memory. Contract: malloc-style foreign
;; allocation. Degradation: raise — jolt.ffi surfaces that as a clean
;; jolt-level error.
(define (sa-foreign-alloc n)
  (sa-ffi-raise 'sa-foreign-alloc))

;; (sa-foreign-free p) -> void
;; Release foreign memory allocated by sa-foreign-alloc. Degradation: raise.
(define (sa-foreign-free p)
  (sa-ffi-raise 'sa-foreign-free))

;; (sa-foreign-ref type addr off) -> value
;; Typed read of one value at byte offset OFF of the foreign block at ADDR.
;; Degradation: raise.
(define (sa-foreign-ref type addr off)
  (sa-ffi-raise 'sa-foreign-ref))

;; (sa-foreign-set! type addr off v) -> void
;; Typed write of V at byte offset OFF of the foreign block at ADDR.
;; Degradation: raise.
(define (sa-foreign-set! type addr off v)
  (sa-ffi-raise 'sa-foreign-set!))

;; (sa-foreign-bytes-ref! addr bv n) -> void
;; (sa-foreign-bytes-set! addr bv n) -> void
;; The BULK octet moves: one block copy between foreign memory and a bytevector,
;; in place of N sa-foreign-ref!/-set! calls. The CONTRACT lets a target without
;; a block move implement them as a loop over its own foreign-ref/-set!, which
;; gives the constant back but stays correct; gambit has no ffi at all, so like
;; every other data-plane entry here they raise. They must still be BOUND — the
;; contract-name pass in gambitcheck.ss asserts every CONTRACT.txt name resolves,
;; and an unbound one is indistinguishable from a target that forgot to port it.
(define (sa-foreign-bytes-ref! addr bv n)
  (sa-ffi-raise 'sa-foreign-bytes-ref!))
(define (sa-foreign-bytes-set! addr bv n)
  (sa-ffi-raise 'sa-foreign-bytes-set!))

;; (sa-foreign-sizeof type) -> exact integer
;; Size in bytes of a foreign type. Degradation: raise.
(define (sa-foreign-sizeof type)
  (sa-ffi-raise 'sa-foreign-sizeof))

;; (sa-lock-object x) -> void
;; Pin X against the collector while C holds a reference to it. Contract: keep
;; X address-stable and live. Degradation: the CONTRACT permits no-oping BOTH
;; sa-lock-object and sa-unlock-object on a non-moving collector; here the
;; ffi tier is entirely unsupported so no foreign reference can exist — a
;; consistent raise is the honest degradation (never one without the other).
(define (sa-lock-object x)
  (sa-ffi-raise 'sa-lock-object))

;; (sa-unlock-object x) -> void
;; Release a sa-lock-object pin. Degradation: raise, exactly alongside
;; sa-lock-object.
(define (sa-unlock-object x)
  (sa-ffi-raise 'sa-unlock-object))

;; (sa-load-shared-object name-or-#f) -> void
;; dlopen of the named shared object. Degradation: raise — and jolt.ffi/
;; load-library must surface that as a clean jolt-level error, not a VM abort.
(define (sa-load-shared-object name)
  (sa-ffi-raise 'sa-load-shared-object))

;; (sa-foreign-entry? name) -> boolean
;; Does the named C entry resolve. Contract: an existence probe for C symbols.
;; Degradation: the CONTRACT allows #f for anything unresolved, but the ffi
;; tier is entirely unsupported here — raise carries the unsupported story
;; through jolt.ffi's single load-library choke point.
(define (sa-foreign-entry? name)
  (sa-ffi-raise 'sa-foreign-entry?))

;; (sa-foreign-entry-address name) -> pointer
;; The address of the named C entry. Degradation: raise — callers only ask
;; for symbols they know exist, which never happens here.
(define (sa-foreign-entry-address name)
  (sa-ffi-raise 'sa-foreign-entry-address))

;; (sa-foreign-callable-entry-point co) -> pointer
;; A foreign-callable code object's C-visible entry-point address.
;; Degradation: raise.
(define (sa-foreign-callable-entry-point co)
  (sa-ffi-raise 'sa-foreign-callable-entry-point))

;; ---- R8: eval/compile/AOT (capabilities: native-compile, image) --------------

;; (sa-baked-global sym) -> value | #f
;; The top-level value of SYM, or #f when unbound. Callers probe optionally-
;; baked globals (jolt-baked-runtime-fingerprint, jolt-baked-version-early,
;; jolt-compile-eval-form) whose values are never #f, so #f is a safe absent
;; sentinel. Contract: reflect on the running top level by symbol. Degradation:
;; #f always — the gambit target has no baked-image globals; all three callers
;; tolerate absent (documented degradation).
(define (sa-baked-global sym)
  #f)

;; (sa-compile-file src so profile) -> void
;; Compile the Scheme source file SRC to the native object SO under PROFILE.
;; Contract: native compilation of SRC to SO. Degradation: raise a
;; message-carrying condition, never a bare raise — the AOT cache (loader.ss)
;; treats the raise as a cache disable and falls back to loading from source;
;; `jolt build` surfaces it as a jolt-level error whose message is the
;; condition's.
(define (sa-compile-file src so profile)
  (error 'sa-compile-file "native compilation is unsupported on the gambit target" src))

;; (sa-make-boot-file out base-boots) -> void
;; Assemble the boot file OUT from the base boot files BASE-BOOTS. Contract:
;; write a boot file the target's runtime can boot from. Degradation: raise —
;; same story as sa-compile-file.
(define (sa-make-boot-file out base-boots)
  (error 'sa-make-boot-file "native compilation is unsupported on the gambit target" out))

;; (sa-fasl-write obj port [externals-pred]) -> void
;; Serialize OBJ to PORT. Contract: serialize a value graph to a byte image.
;; Degradation: raise — jolt.image's dump surfaces as a clean jolt-level
;; unsupported error carrying the condition's message; the raise must be
;; message-carrying, never a bare raise.
(define (sa-fasl-write obj port . rest)
  (error 'sa-fasl-write "fasl serialization is unsupported on the gambit target"))

;; (sa-fasl-read port [who exts]) -> value
;; Deserialize the next object from PORT. Contract: read back what
;; sa-fasl-write wrote. Degradation: raise — same story as sa-fasl-write.
(define (sa-fasl-read port . rest)
  (error 'sa-fasl-read "fasl serialization is unsupported on the gambit target"))

;; ---- continuations tier (capability: continuations) -------------------------

;; (sa-call-with-escape-continuation proc) -> value
;; The one-shot ESCAPE continuation jolt.continuations is built on. Gambit's
;; usable primitive is call/cc (the same R0(e) finding the fiber scheduler
;; below rests on: ##continuation-capture/##continuation-graft SIGBUS gsi on
;; same-stack re-entry), and call/cc is MULTI-SHOT — so the one-shot half of
;; the contract is this adapter's job, not something the primitive gives.
;;
;; The spent flag is what supplies it. Chez's call/1cc refuses a second
;; invocation and refuses one after the capturing call returned; both refusals
;; are reproduced here, because without them a re-invocation would graft
;; control back into a frame that already finished and silently re-run the
;; caller's half-completed expression — the exact trap the fiber scheduler
;; below avoids by construction rather than by checking.
;;
;; The flag is set on the normal return as well as on the escape: after PROC
;; answers, this capture is no longer live, and a saved k invoked later must
;; raise rather than re-enter. The layer above (host/chez/continuations.ss)
;; adds the thread/fiber ownership rule and the jolt-level error; a target owes
;; only the one-shot primitive.
(define (sa-call-with-escape-continuation proc)
  (call/cc
   (lambda (k)
     (let ((spent #f))
       (let ((v (proc (lambda (val)
                        (if spent
                            (error 'sa-call-with-escape-continuation
                                   "escape continuation is spent")
                            (begin (set! spent #t) (k val)))))))
         (set! spent #t)
         v)))))

;; ---- fibers R1: coroutines tier (capability: coroutines) --------------------
;; Stackful green threads sharing one OS thread, per CONTRACT.txt's coroutines
;; tier. R0(e) pinned the primitive: ##continuation-capture/##continuation-graft
;; SIGBUS gsi on same-stack re-entry; call/cc is the usable primitive (the
;; switch is ~10x the Chez cost interpreted, and there is no transparent IO
;; parking on this target, so `go` stays on OS threads — the plan's documented
;; degradation). The scheduler mirrors host/chez/fibers.ss: each park captures
;; a FRESH continuation and the scheduler invokes each continuation exactly
;; once, so call/cc's multi-shot-ness is never exercised (the plan's one-shot
;; discipline — a resumed fiber must never re-enter the caller's half-finished
;; expression). The current fiber rides a plain global here — the virtual
;; register is a Chez perf choice (R0(c): 2ns vs 33ns thread-parameter write),
;; not a contract shape.

(define-record-type jolt-fiber
  (fields (mutable state)
          thunk
          (mutable k)
          (mutable result)
          (mutable error)
          (mutable next)
          (mutable slice))
  (nongenerative jolt-fiber-v1))

(define jolt-fiber-q-head #f)
(define jolt-fiber-q-tail #f)
(define jolt-sched-k #f)
(define jolt-gambit-current-fiber #f)

(define (jolt-current-fiber) jolt-gambit-current-fiber)

(define (jolt-fiber-enqueue! f)
  (if jolt-fiber-q-tail
      (begin (jolt-fiber-next-set! jolt-fiber-q-tail f)
             (set! jolt-fiber-q-tail f))
      (begin (set! jolt-fiber-q-head f)
             (set! jolt-fiber-q-tail f))))

(define (jolt-fiber-dequeue!)
  (let ((f jolt-fiber-q-head))
    (if f
        (begin
          (set! jolt-fiber-q-head (jolt-fiber-next f))
          (if (not jolt-fiber-q-head) (set! jolt-fiber-q-tail #f))
          (jolt-fiber-next-set! f #f)
          f)
        #f)))

(define (jolt-fiber-to-scheduler! f)
  (set! jolt-gambit-current-fiber #f)
  (call/cc
    (lambda (k)
      (jolt-fiber-k-set! f k)
      (jolt-sched-k))))

(define (sa-fiber-yield)
  (let ((f (jolt-current-fiber)))
    (if f
        (begin (jolt-fiber-state-set! f 'ready)
               (jolt-fiber-enqueue! f)
               (jolt-fiber-to-scheduler! f))
        (error 'sa-fiber-yield "yield called outside a fiber"))))

(define (jolt-fiber-park!)
  (let ((f (jolt-current-fiber)))
    (if f
        (begin (jolt-fiber-state-set! f 'parked)
               (jolt-fiber-to-scheduler! f))
        (error 'jolt-fiber-park! "park called outside a fiber"))))

(define (sa-fiber-resume f)
  (if (eq? (jolt-fiber-state f) 'parked)
      (begin (jolt-fiber-state-set! f 'ready)
             (jolt-fiber-enqueue! f))
      #f))

(define (sa-fiber-spawn thunk)
  (let ((f (make-jolt-fiber 'ready thunk #f #f #f #f #f)))
    (jolt-fiber-enqueue! f)
    f))

(define (jolt-fiber-run f)
  (set! jolt-gambit-current-fiber f)
  (call/cc
    (lambda (k)
      (set! jolt-sched-k k)
      (jolt-fiber-resume* f))))

(define (jolt-fiber-resume* f)
  (case (jolt-fiber-state f)
    ((ready)
     (if (jolt-fiber-k f)
         ((jolt-fiber-k f))
         (begin
           (jolt-fiber-state-set! f 'running)
           (let ((r (guard (e (#t (jolt-fiber-dead! f e)))
                      ((jolt-fiber-thunk f)))))
             (jolt-fiber-done! f r)))))
    (else (error 'jolt-fiber-run "fiber in unexpected state"
                 (jolt-fiber-state f)))))

(define (jolt-fiber-done! f r)
  (jolt-fiber-state-set! f 'done)
  (jolt-fiber-result-set! f r)
  (jolt-fiber-k-set! f #f)
  (set! jolt-gambit-current-fiber #f)
  (jolt-sched-k))

(define (jolt-fiber-dead! f e)
  (jolt-fiber-state-set! f 'dead)
  (jolt-fiber-error-set! f e)
  (jolt-fiber-k-set! f #f)
  (set! jolt-gambit-current-fiber #f)
  (jolt-sched-k))

(define (sa-fiber-run-all)
  (let loop ()
    (let ((f (jolt-fiber-dequeue!)))
      (if f
          (begin (jolt-fiber-run f) (loop))
          #f))))

;; --- capability-unchecked ---------------------------------------------------
;; The unchecked fixnum / vector primitives (CONTRACT.txt): this target expands
;; them to the checked primitives — the permitted degradation.
(define-syntax sa-ufx+ (syntax-rules () ((_ a b) (fx+ a b))))
(define-syntax sa-ufx- (syntax-rules () ((_ a b) (fx- a b))))
(define-syntax sa-ufx<? (syntax-rules () ((_ a b) (fx<? a b))))
(define-syntax sa-ufx>=? (syntax-rules () ((_ a b) (fx>=? a b))))
(define-syntax sa-ufx=? (syntax-rules () ((_ a b) (fx=? a b))))
(define-syntax sa-uvector-ref (syntax-rules () ((_ v i) (vector-ref v i))))
(define-syntax sa-uvector-set! (syntax-rules () ((_ v i x) (vector-set! v i x))))
;; gambit's vector-copy! / string-copy! have the R7RS shape already
(define (sa-vector-copy-range! to at from start end)
  (vector-copy! to at from start end))
(define (sa-string-copy-range! to at from start end)
  (string-copy! to at from start end))
