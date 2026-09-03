;; scheme-adapter-runtime.ss — the RUNTIME half of the portable-scheme-layer
;; adapter (PSL R3, epic jolt-867l).
;;
;; host/scheme-adapter/chez.ss is the gate-time half (assert-only, zero
;; definitions). THIS file is the part that actually runs: it owns the system-
;; tier FORBIDDEN names (blocklist tier "system") and exposes them to the rest
;; of the host only through named capability entry points. A non-Chez target
;; implements (or explicitly degrades) this one small surface instead of
;; chasing call sites; the portability gate's allowlist for this file IS the
;; sanctioned inventory of direct forbidden-name use.
;;
;; Every entry point is a transparent one-liner over the native — cp0 sees
;; through it within the flat runtime compilation unit, so there is zero
;; runtime cost and behavior is identical to the direct call. Each doc line
;; states the contract a target must meet and the degradation it may take
;; (derived from what the callers actually tolerate — see .dirge/specs/
;; psl-r3-system-tier.md for the per-site tolerance table).
;;
;; Loaded FIRST in bld-runtime-manifest (before rt.ss) so the sa-* names are
;; bound before rt.ss's own top level and the java/*.ss files it loads run —
;; rt.ss:51 and process.ss:338 are MACROS that resolve sa-os-family at
;; expansion time. PSL R5+R6 pinned this order; the R3 comment claiming
;; "second, after rt.ss" is obsolete.
;; NO (import (chezscheme)) here: this file is INLINED into the flat runtime
;; (bld-runtime-manifest, and again via rt.ss's own load) — a mid-program
;; import re-exposes Chez's error/warning bindings over rt.ss's %chez-error
;; shadowing and turned compile warnings fatal in standalone `jolt build`
;; (the bare-directory smoke caught it). Runtime files never import; the
;; top level already sees (chezscheme), including under chez --script.

;; (sa-run-process cmd transcoder) -> (values stdin stdout stderr pid)
;; Spawn CMD (a shell string) with block buffering and the given transcoder
;; (#f = binary), returning the four process ports and the pid. Contract: a
;; target must provide exactly this shape. Degradation: a target without
;; subprocess support must raise 'unsupported — every caller (jolt.host/
;; sh-out, java ProcessBuilder fallback, all-env-pairs) genuinely needs the
;; subprocess, so none of them may be silently degraded.
(define (sa-run-process cmd transcoder)
  (open-process-ports cmd (buffer-mode block) transcoder))

;; (sa-gc-collect) -> void
;; A full collection hint (every generation) — what the Runtime.gc /
;; System.gc callers mean: weak references clear and guardians fire.
;; Contract: perform a full collection. Degradation: may no-op (WASM) —
;; callers already guard the call and the JVM semantic is only a hint.
;;
;; An explicit (collect) raises "cannot collect when multiple threads are
;; active" when any OTHER thread is active at that instant, and jolt always
;; has service threads (io-poller, fiber carriers, the timer) that are
;; collect-safe while blocked but active for the microseconds between waits —
;; so a single attempt can lose that race at any time. Retry over a short
;; window; a thread that stays active through all of it (a compute loop)
;; degrades the call to a no-op, because System.gc must hint, never throw.
;; Any other collector error still raises.
(define (sa-gc-collect)
  (let loop ((tries 100))
    (let ((r (guard (e (#t (if (and (message-condition? e)
                                    (string=? (condition-message e)
                                              "cannot collect when multiple threads are active"))
                               'busy
                               (raise e))))
               (collect (collect-maximum-generation))
               'ok)))
      (when (and (eq? r 'busy) (fx>? tries 0))
        (sleep (make-time 'time-duration 1000000 0))   ; 1 ms
        (loop (fx- tries 1))))))

;; (sa-gc-max-generation) -> exact integer
;; The deepest collectable generation, for callers mapping JVM generations.
;; Contract: return the maximum generation argument sa-gc-collect accepts.
;; Degradation: a single-generation (non-generational) collector returns 0.
(define (sa-gc-max-generation)
  (collect-maximum-generation))

;; (sa-bytes-allocated) -> exact integer
;; Live-heap bytes — the "used" reading under current-memory-bytes that
;; Runtime.freeMemory is built from. Contract: bytes currently allocated and
;; live. Degradation: an approximation is acceptable; callers only subtract it
;; from total to compute free memory.
(define (sa-bytes-allocated)
  (bytes-allocated))

;; (sa-total-memory-bytes) -> exact integer
;; Total process heap bytes — what the collector has reserved from the OS
;; (the JVM's totalMemory; Runtime.freeMemory is the difference against
;; sa-bytes-allocated). Contract: total heap bytes. Degradation: an
;; approximation is fine — callers only display it or subtract
;; sa-bytes-allocated from it.
(define (sa-total-memory-bytes)
  (current-memory-bytes))

;; (sa-max-memory-bytes) -> exact integer
;; Upper bound on the heap the runtime may use — the JVM's maxMemory, which
;; jolt maps to Long/MAX_VALUE when the heap is unbounded. Contract: an upper
;; bound on heap bytes. Degradation: a large constant is acceptable — the JVM
;; arm already falls back to Long.MAX_VALUE semantics.
(define (sa-max-memory-bytes)
  (maximum-memory-bytes))

;; (sa-real-time-ms) -> exact integer
;; Wall-clock milliseconds, monotonic within a process — used for elapsed
;; deltas (build profiling) and unique temp-file stamps. Contract: an
;; exact-integer ms clock usable for both. Degradation: a target may use any
;; monotonic ms clock (JVM nanoTime/1000000 is fine); a no-op is NOT — the
;; stamps must differ between runs.
(define (sa-real-time-ms)
  (real-time))

;; ---- R5: io remainder (mtime) + the last GC hook -----------------------------

;; (sa-file-mtime-ms path) -> exact integer
;; Epoch milliseconds of PATH's last modification. Contract: a per-file mtime
;; usable for newer-than comparisons (build freshness, AOT cache keys, the
;; gate-boot staleness predicate). Degradation: none — stat is universal on
;; real targets; do not fake.
(define (sa-file-mtime-ms path)
  (let ((t (file-modification-time path)))
    (+ (* (time-second t) 1000) (div (time-nanosecond t) 1000000))))

;; (sa-gc-trip-bytes! n) -> void
;; Set the allocation threshold at which a trip collection triggers — the
;; dev-cache CLI's GC tuning knob (cli-devcache.ss). Contract: honor N as a
;; collection-trip hint. Degradation: may no-op — the call tunes a dev cache
;; only; collection still happens on its own schedule.
(define (sa-gc-trip-bytes! n)
  (collect-trip-bytes n))

;; ---- R6: introspection tier (capability: introspect) -------------------------

;; sa-introspect-enabled? — dynamic parameter, default #t. The degraded-
;; backtrace gate flips it to #f to prove that throw surfaces still carry
;; type+message while every introspect entry point returns empty/#f and the
;; backtrace renders without continuation frames.
(define sa-introspect-enabled? (make-parameter #t))

;; (sa-host-tag) -> string
;; The runtime's host tag (here Chez's machine type, e.g. "tarm64osx"). NAMING
;; ONLY: release/fasl directory names, image headers, telemetry strings, error
;; text. No logic may branch on it — logic branches use sa-os-family /
;; sa-arch / sa-endian. Contract: an opaque, per-build-stable host string.
;; Degradation: any identifier string the target names itself with.
(define (sa-host-tag)
  (symbol->string (machine-type)))

(define (sa-tag-contains? tag needle)
  (let ((n (string-length tag)) (m (string-length needle)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring tag i (+ i m)) needle) #t)
            (else (loop (+ i 1)))))))

;; (sa-probed-os-family) -> 'macos | 'windows | 'linux
;; The OS family for a host tag that does not carry one, probed from the
;; filesystem. Contract: the family the process is actually running on.
;; Degradation: 'linux, matching the else-branch it replaces. Cached because
;; sa-os-family is consulted from hot paths and the answer cannot change while
;; the process runs.
(define sa-os-family-cache #f)
(define (sa-probed-os-family)
  (or sa-os-family-cache
      (let ((fam (cond ((file-exists? "/System/Library/CoreServices/SystemVersion.plist") 'macos)
                       ((file-exists? "/proc/self/status") 'linux)
                       ((getenv "SystemRoot") 'windows)
                       (else 'linux))))
        (set! sa-os-family-cache fam)
        fam)))

;; (sa-os-family) -> 'macos | 'windows | 'linux
;; The OS family every host OS branch derives: SIGCHLD/SIG_BLOCK numerics
;; (process.ss, concurrency.ss), LC_TIME (tz-primitives.ss), struct-stat
;; offsets (nio-file.ss), the chmod and entropy fallbacks (io.ss, rt.ss),
;; os.name (host-static-methods.ss), link libraries (build.ss). Contract: one
;; of the three symbols. Degradation: none — the sites have no safe assumed
;; default; an unrecognized host falls back to 'linux, matching today's
;; else-branches.
;;
;; Portable-bytecode tags (pb, pb64l, tpb64l, ...) name the threading, word
;; size and endianness and deliberately name no OS, because the same bytecode
;; is meant to run on any of them. The tag therefore cannot answer for them and
;; the else-branch called every bytecode build 'linux, including one running on
;; macOS — which picks the Linux SIGCHLD, EAGAIN, O_NONBLOCK, LC_TIME and
;; struct-stat values on a Darwin host. Probe instead.
(define (sa-os-family)
  (sa-os-family-for-tag (sa-host-tag)))

;; (sa-os-family-for-tag tag) -> 'macos | 'windows | 'linux
;; The derivation above, as a function OF the tag, so the gate can pin the whole
;; table on one host instead of only the row that host happens to be
;; (test/chez/host-derived-props-test.ss). The bug this splits out of was in
;; this cond and was reported with the cond transcribed into Clojure, because
;; there was no way to ask it about a tag. NOT for callers: anything with a tag
;; to pass is branching on the tag, which sa-host-tag's contract forbids.
(define (sa-os-family-for-tag m)
  ;; "ios" joins the macos branch rather than getting one of its own: iOS is
  ;; Darwin, so every constant this selects is already the right one, and the
  ;; three-symbol contract has nowhere else to put it. Chez has four iOS tags
  ;; (a6ios, arm64ios, ta6ios, tarm64ios; BUILDING documents tarm64ios as the
  ;; cross-target), and not one of them contains "osx", "macos", "nt" or "pb",
  ;; so all four reached the else-branch and called a Darwin system Linux.
  ;; The substring is exact rather than lucky: those four are the only tags in
  ;; the Chez tree containing "ios", and a tag that contained it by accident
  ;; would have to be an "osx" tag, which wants 'macos anyway.
  ;;
  ;; Android takes no branch, and that is not an omission: it has no Chez tag of
  ;; its own — it cross-builds as tarm64le (tools/cross-compile/README.md) — and
  ;; Bionic is Linux for every constant this selects, SIGCHLD 17, EAGAIN 11,
  ;; O_NONBLOCK, SOCK_CLOEXEC and the struct-stat offsets alike. Where Android
  ;; does diverge is the link libraries (Bionic has no -lrt/-luuid/-ltinfo), and
  ;; the tag cannot answer that: tarm64le is glibc arm64 Linux too. The target
  ;; pack owns those flags, which is why they are not derived here.
  (cond ((or (sa-tag-contains? m "osx") (sa-tag-contains? m "macos")
             (sa-tag-contains? m "ios")) 'macos)
        ((or (sa-tag-contains? m "nt") (sa-tag-contains? m "windows")) 'windows)
        ((sa-tag-contains? m "pb") (sa-probed-os-family))
        (else 'linux)))

;; (sa-arch) -> 'x86-64 | 'arm64 | 'i386 | 'other
;; The machine architecture — the nio-file stat-layout guard keys on x86-64.
;; Contract: the architecture symbol. Degradation: 'other for an unrecognized
;; host; callers treat it as unverified.
(define (sa-arch)
  (let ((a (sa-arch-for-tag (sa-host-tag))))
    (if (eq? a 'other) (sa-probed-arch) a)))

;; (sa-arch-for-tag tag) -> the derivation above as a function OF the tag, on
;; the same terms as sa-os-family-for-tag: for the gate, not for callers.
;; 'other means THE TAG DOES NOT SAY, which is not the same as "unknown" —
;; sa-arch probes past it.
(define (sa-arch-for-tag m)
  (cond ((sa-tag-contains? m "arm64") 'arm64)
        ((sa-tag-contains? m "a6") 'x86-64)
        ((sa-tag-contains? m "i3") 'i386)
        (else 'other)))

;; (sa-probed-arch) -> 'x86-64 | 'arm64 | 'i386 | 'other
;; The architecture for a host tag that does not carry one — every pb tag, and
;; any native tag outside the a6/i3/arm64 vocabulary. Contract: the architecture
;; the process is actually running on. Degradation: 'other, matching the
;; else-branch it replaces; every caller already treats that as unverified.
;;
;; uname(2) rather than the tag, because a pb tag's own "64" and "l" fields
;; describe the BYTECODE's encoding and not the machine under it. The machine
;; field's offset is the one platform constant here: struct utsname is
;; fixed-width char arrays, 65 on Linux (machine is the 5th, at 260) and 256 on
;; Darwin (at 1024), and the OS is already settled by sa-os-family before this
;; runs. Windows has no uname and answers from the environment instead.
(define sa-arch-cache #f)
(define (sa-probed-arch)
  (or sa-arch-cache
      (let ((a (or (sa-probe-arch-name) 'other)))
        (set! sa-arch-cache a)
        a)))

(define (sa-arch-name->symbol s)
  (cond ((not s) #f)
        ((or (string=? s "x86_64") (string=? s "amd64") (string=? s "AMD64")) 'x86-64)
        ((or (string=? s "aarch64") (string=? s "arm64") (string=? s "ARM64")) 'arm64)
        ((or (string=? s "i386") (string=? s "i686") (string=? s "x86")) 'i386)
        (else #f)))

(define (sa-probe-arch-name)
  (if (eq? (sa-os-family) 'windows)
      (sa-arch-name->symbol (getenv "PROCESSOR_ARCHITECTURE"))
      (sa-arch-name->symbol (sa-uname-machine))))

;; The `machine` field of uname(2), or #f when it cannot be had. Probe-only and
;; fully guarded: a statically linked build may not carry the symbol at all, and
;; a missing arch is a documented degradation rather than a boot failure.
;;
;; No sa-load-shared-object here, on sa-fbytes-init!'s reasoning and for its
;; reason: the boot took the process-global handle long before anything can ask
;; for an arch (rt.ss binds _exit through jolt-foreign-proc-safe at its line
;; 135, and the first caller of sa-arch is host-static-methods.ss at 1553), and
;; re-taking it would re-promote it above every :jolt/native loaded since.
(define (sa-uname-machine)
  (guard (e (#t #f))
    (and (foreign-entry? "uname")
         (let* ((linux? (eq? (sa-os-family) 'linux))
                (off (if linux? 260 1024))
                (size (if linux? 512 2048))
                (buf (foreign-alloc size)))
           (dynamic-wind
             (lambda ()
               (let loop ((i 0))
                 (when (fx<? i size)
                   (foreign-set! 'unsigned-8 buf i 0)
                   (loop (fx+ i 1)))))
             (lambda ()
               (and (= 0 ((foreign-procedure "uname" (void*) int) buf))
                    (let loop ((i 0) (acc '()))
                      (let ((b (foreign-ref 'unsigned-8 buf (fx+ off i))))
                        (if (or (fx=? b 0) (fx>? i 62))
                            (and (pair? acc) (list->string (reverse acc)))
                            (loop (fx+ i 1) (cons (integer->char b) acc)))))))
             (lambda () (foreign-free buf)))))))

;; (sa-endian) -> 'little | 'big
;; Byte order of the host. Contract: the byte order. Degradation: none — the
;; runtime always knows its own byte order, so a tag that does not name one is
;; answered by native-endianness rather than by #f.
(define (sa-endian)
  (or (sa-endian-for-tag (sa-host-tag)) (native-endianness)))

;; (sa-endian-for-tag tag) -> the derivation above as a function OF the tag, on
;; the same terms as sa-os-family-for-tag: for the gate, not for callers. #f
;; means THE TAG DOES NOT SAY — the osx and nt tags carry no le/be suffix and
;; neither does any pb tag, whose own trailing l/b names the bytecode's encoding
;; rather than a suffix in this shape. sa-endian answers native-endianness
;; there, which is exact on every host and needs no probe.
(define (sa-endian-for-tag m)
  (let ((n (string-length m)))
    (if (>= n 2)
        (let ((suf (substring m (- n 2) n)))
          (cond ((string=? suf "le") 'little)
                ((string=? suf "be") 'big)
                (else #f)))
        #f)))

;; (sa-stats) -> #(cpu-nanos real-nanos gc-count gc-cpu-nanos gc-real-nanos
;;                gc-bytes)
;; One statistics snapshot as the six exact-integer fields rt.ss reads into
;; the jolt.host telemetry vars (time fields pre-converted to nanos). Contract:
;; the six fields in this order. Degradation: a zero vector — the OTel layer
;; maps zeros to absent metrics.
(define (sa-stats)
  (let ((s (statistics)))
    (vector (time->nanos (sstats-cpu s))
            (time->nanos (sstats-real s))
            (sstats-gc-count s)
            (time->nanos (sstats-gc-cpu s))
            (time->nanos (sstats-gc-real s))
            (sstats-gc-bytes s))))

;; (sa-continuation-frames k) -> list of frame inspectors | '()
;; The continuation's frames, innermost first, each an inspector object the
;; walker queries with the messages it already uses ('type 'code 'name
;; 'source-object 'link 'ref 'length). The 400-frame cap and the guard live
;; here, so a walker cannot crash the render. Contract: stepping a throw
;; continuation. Degradation: '() when sa-introspect-enabled? is #f or the
;; target has no inspector — the backtrace then renders from the compile-time
;; tables alone (jolt-backwalk) or reports no frames.
(define (sa-continuation-frames k)
  (guard (e (#t '()))
    (if (sa-introspect-enabled?)
        (let loop ((io (inspect/object k)) (n 0) (acc '()))
          (if (or (not io) (fx>=? n 400))
              (reverse acc)
              (loop (guard (e (#t #f)) (io 'link)) (fx+ n 1) (cons io acc))))
        '())))

;; (sa-procedure-info x) -> (name . ((free-name . value) ...)) | #f
;; A procedure's inspector name and live free-variable captures, in
;; registration order — what the image graph needs to serialize closures
;; (state-image.ss). Contract: name + captured values. Degradation: #f — the
;; image writer refuses the closure ('image-no), the same verdict as today's
;; no-inspector builds.
;; (sa-procedure-free-values p) -> list of the closure's captured values, in the
;; order Chez stores them, or #f. POSITIONS, no names: the names are inspector
;; information, which a release build does not generate, while the values are
;; there either way. What position means which name is not knowable from here —
;; see image-fnsrc-layout, which learns it per site from a probe.
(define (sa-procedure-free-values x)
  (guard (e (#t #f))
    (let* ((io (inspect/object x))
           (n  (io 'length)))
      (and n
           (let loop ((i 0) (acc '()))
             (if (fx>=? i n)
                 (reverse acc)
                 (loop (fx+ i 1)
                       (cons (let ((vo (io 'ref i))) ((vo 'ref) 'value)) acc))))))))

(define (sa-procedure-info x)
  (guard (e (#t #f))
    (if (sa-introspect-enabled?)
        (let* ((io (inspect/object x))
               (code (io 'code))
               (nm0 (and code (code 'name)))
               (nm (cond ((string? nm0) nm0)
                         ((symbol? nm0) (symbol->string nm0))
                         (else #f)))
               (n (io 'length)))
          (let loop ((i 0) (acc '()))
            (if (or (not n) (fx>=? i n))
                (cons nm (reverse acc))
                (let* ((vo (io 'ref i))
                       (vn0 (vo 'name))
                       (vn (if (symbol? vn0) (symbol->string vn0) vn0))
                       (v ((vo 'ref) 'value)))
                  (loop (fx+ i 1)
                        (if (string? vn) (cons (cons vn v) acc) acc))))))
        #f)))

;; ---- R7: ffi tier (capability: ffi) -----------------------------------------

;; (sa-foreign-procedure name args res) -> foreign procedure
;; SYNTAX: compile-time-typed foreign-procedure creation. (sa-foreign-procedure
;; "f" (int) int) lowers to (foreign-procedure "f" (int) int) — the compiled
;; (non-Windows) branch of jolt-foreign-proc-safe / -blocking, where
;; the type signature is literal at the call site. Contract: build a foreign
;; procedure for a statically-known signature. Degradation: none — a target
;; expands this to its native foreign-procedure form.
(define-syntax sa-foreign-procedure
  (syntax-rules ()
    ((_ name args res) (foreign-procedure name args res))
    ;; Optional calling-convention slot (e.g. (__varargs_after 2) for a variadic
    ;; libc function like fcntl — Apple arm64 passes variadic args in the caller's
    ;; register-save area, so a fixed-arity call silently corrupts them).
    ((_ conv name args res) (foreign-procedure conv name args res))))

;; (sa-foreign-procedure-native-error error-convention (conv ...) name args res)
;; -> foreign procedure
;; SYNTAX: like sa-foreign-procedure, with Chez's native-error convention kept
;; separate from any other calling conventions. The result is a two-value
;; answer: the declared C result and the calling thread's captured native error
;; slot. `error-convention` is __errno on POSIX and __get_last_error on Windows.
;; A target adapter must provide the equivalent atomic call-boundary capture;
;; reading errno/GetLastError in a later host call is racy and is not equivalent.
(define-syntax sa-foreign-procedure-native-error
  (syntax-rules ()
    ((_ error-convention (conv ...) name args res)
     (foreign-procedure error-convention conv ... name args res))))

;; (sa-foreign-procedure-blocking name args res) -> foreign procedure
;; SYNTAX: like sa-foreign-procedure, but the call is __collect_safe — a blocking
;; foreign call must not freeze the stop-the-world collector process-wide (the
;; R4-pinned blocking-FFI collect-safety semantic; process.ss's pipe pump depends
;; on it). Contract: mark the call so a blocking foreign invocation does not stop
;; other threads' GC. Degradation: a target whose collector never stops other
;; threads may collapse this to plain sa-foreign-procedure.
(define-syntax sa-foreign-procedure-blocking
  (syntax-rules ()
    ((_ name args res) (foreign-procedure __collect_safe name args res))))

;; (sa-foreign-callable proc args res) -> foreign callable
;; SYNTAX: compile-time-typed foreign-callable creation, mirroring
;; sa-foreign-procedure. (sa-foreign-callable f (int) int) lowers to
;; (foreign-callable f (int) int) — the compile-time lowering of jolt.ffi/
;; __ccallable (backend_scheme.clj emit-ffi-callable), wrapped by
;; jolt-ffi-register-callable! so the collector neither moves nor reclaims the
;; callable while C may call through it. Contract: build a foreign callable
;; around a Scheme procedure for a statically-known signature. Degradation:
;; none — a target expands this to its native foreign-callable form.
(define-syntax sa-foreign-callable
  (syntax-rules ()
    ((_ proc args res) (foreign-callable proc args res))))

;; (sa-foreign-callable-collect-safe proc args res) -> foreign callable
;; SYNTAX: like sa-foreign-callable, but the callable entry uses the
;; __collect_safe convention that reactivates the thread — for a callback that
;; can arrive on a thread which is not an ACTIVE Scheme thread at that moment:
;; one the runtime never started (a dispatch queue or a pthread the C library
;; spawned), or one parked in a :blocking foreign call. Both need the entry to
;; activate the thread before any Scheme runs; without it the process takes a
;; nonrecoverable memory fault instead of an exception.
(define-syntax sa-foreign-callable-collect-safe
  (syntax-rules ()
    ((_ proc args res) (foreign-callable __collect_safe proc args res))))

;; (sa-foreign-procedure-runtime name args res blocking?) -> foreign procedure | #f
;; Construct a foreign procedure from a RUNTIME-known signature under the
;; Windows-machine-type policy: eval the constructed (foreign-procedure ...) form
;; (with __collect_safe when blocking?) rather than compiling it. On Windows a
;; compiled foreign reference is a load-time fasl relocation that aborts the boot
;; if the symbol is missing, before any guard runs, and petite cannot create FPs —
;; the eval defers creation to a point where the caller has already proven the
;; entry exists. Contract: create a foreign procedure for a runtime signature.
;; Degradation: raise — callers only ask when sa-foreign-entry? has said yes.
(define (sa-foreign-procedure-runtime name args res blocking?)
  (eval (if blocking?
            `(foreign-procedure __collect_safe ,name ,args ,res)
            `(foreign-procedure ,name ,args ,res))))

;; (sa-foreign-alloc n) -> pointer
;; Allocate N raw bytes of foreign memory; the caller owns them and must release
;; them with sa-foreign-free. Contract: malloc-style foreign allocation.
;; Degradation: none — a target with the ffi capability provides it; without it,
;; raise 'unsupported (jolt.ffi surfaces that as a clean jolt-level error).
(define (sa-foreign-alloc n) (foreign-alloc n))

;; (sa-foreign-free p) -> void
;; Release foreign memory allocated by sa-foreign-alloc. Contract: free foreign
;; memory. Degradation: none — see sa-foreign-alloc.
(define (sa-foreign-free p) (foreign-free p))

;; (sa-foreign-ref type addr off) -> value
;; Typed read of one value at byte offset OFF of the foreign block at ADDR. The
;; TYPE VOCABULARY is part of the contract: Chez's foreign type symbols (int,
;; unsigned-8, void*, ...) — the values jolt.ffi's ffi-type->chez produces.
;; Contract: the typed read. Degradation: none.
(define (sa-foreign-ref type addr off) (foreign-ref type addr off))

;; (sa-foreign-set! type addr off v) -> void
;; Typed write of V at byte offset OFF of the foreign block at ADDR. Contract:
;; the typed write. Degradation: none.
(define (sa-foreign-set! type addr off v) (foreign-set! type addr off v))

;; --- bulk octet moves --------------------------------------------------------
;; sa-foreign-ref/-set! carry ONE value per call. Moving a buffer that way costs
;; ~30ns per byte on Chez (measured against ~4ns for the same loop over a
;; bytevector, and ~0 for bytevector-copy!), so every socket read, every FFI
;; buffer hand-off, and every jolt.ffi/read-array paid a per-byte constant that
;; dwarfed the work around it. These two move a whole block at once.
;;
;; The bytevector crosses to C BY ADDRESS (Chez's u8* argument type). A plain
;; foreign call keeps the calling thread ACTIVE for the collector, so the
;; bytevector cannot be moved out from under C for the duration of the call --
;; which is exactly why neither of these may ever be made __collect_safe.
;;
;; Resolution is lazy and probe-only: the boot already loaded the process-global
;; handle, so memcpy resolves without another sa-load-shared-object -- and must
;; not take one, since re-loading that handle re-promotes it above every
;; :jolt/native loaded so far (the libboringssl hijack, see java/ffi.ss).
(define sa-fbytes-probed? #f)
(define sa-fbytes-in #f)     ; memcpy(bytevector, foreign, n)
(define sa-fbytes-out #f)    ; memcpy(foreign, bytevector, n)
(define (sa-fbytes-init!)
  (unless sa-fbytes-probed?
    (set! sa-fbytes-probed? #t)                ; probe once, even if it throws
    (guard (e (#t #f))
      (when (foreign-entry? "memcpy")
        (set! sa-fbytes-in (foreign-procedure "memcpy" (u8* void* size_t) void*))
        (set! sa-fbytes-out (foreign-procedure "memcpy" (void* u8* size_t) void*))))))

;; (sa-foreign-bytes-ref! addr bv n) -> void
;; Copy the N octets at foreign address ADDR into BV[0,N). Contract: a
;; binary-faithful block copy, foreign -> bytevector. Degradation: a target with
;; no block move may loop over its foreign-ref (correct, just slow) -- the
;; fallback below is that loop, taken when memcpy does not resolve.
(define (sa-foreign-bytes-ref! addr bv n)
  (sa-fbytes-init!)
  (if sa-fbytes-in
      (begin (sa-fbytes-in bv addr n) (if #f #f))
      (let loop ((i 0))
        (when (fx< i n)
          (bytevector-u8-set! bv i (foreign-ref 'unsigned-8 addr i))
          (loop (fx+ i 1))))))

;; (sa-foreign-bytes-set! addr bv n) -> void
;; Copy BV[0,N) to the N octets at foreign address ADDR. Contract: a
;; binary-faithful block copy, bytevector -> foreign. Degradation: as
;; sa-foreign-bytes-ref!.
(define (sa-foreign-bytes-set! addr bv n)
  (sa-fbytes-init!)
  (if sa-fbytes-out
      (begin (sa-fbytes-out addr bv n) (if #f #f))
      (let loop ((i 0))
        (when (fx< i n)
          (foreign-set! 'unsigned-8 addr i (bytevector-u8-ref bv i))
          (loop (fx+ i 1))))))

;; (sa-foreign-sizeof type) -> exact integer
;; Size in bytes of a foreign type (struct layout, array allocation). Contract:
;; the type's byte size. Degradation: none.
(define (sa-foreign-sizeof type) (foreign-sizeof type))

;; (sa-lock-object x) -> void
;; Pin X against the collector while C holds a reference to it (the callable
;; registry locks the code object behind a C-visible entry point). Contract: keep
;; X address-stable and live. Degradation: a target whose collector never moves
;; objects may no-op BOTH sa-lock-object and sa-unlock-object (say so in the doc);
;; no-oping only one is a leak or a crash.
(define (sa-lock-object x) (lock-object x))

;; (sa-unlock-object x) -> void
;; Release a sa-lock-object pin. Contract: undo the pin. Degradation: no-op
;; exactly when sa-lock-object no-ops.
(define (sa-unlock-object x) (unlock-object x))

;; (sa-load-shared-object name-or-#f) -> void
;; dlopen of the named shared object; #f = the running process's own symbols.
;; Contract: make the object's symbols resolvable by name. Degradation: raise —
;; and jolt.ffi/load-library must surface that as a clean jolt-level error, not a
;; VM abort (verified on Chez: a missing library raises a guardable error and the
;; jolt-level message is unchanged).
(define (sa-load-shared-object name) (load-shared-object name))

;; (sa-foreign-entry? name) -> boolean
;; Does the named C entry resolve (in the process or a loaded object). Contract:
;; an existence probe for C symbols. Degradation: #f for anything unresolved.
(define (sa-foreign-entry? name) (foreign-entry? name))

;; (sa-foreign-entry-address name) -> pointer
;; The address of the named C entry — the embedded boot arrays' symbols in a
;; built binary resolve through this (build-jolt.ss's emitted launcher). Contract:
;; resolve a C symbol to its address. Degradation: raise — callers only ask for
;; symbols they know exist.
(define (sa-foreign-entry-address name) (foreign-entry name))

;; (sa-foreign-callable-entry-point co) -> pointer
;; A foreign-callable code object's C-visible entry-point address — what the
;; callable registry hands to C. Contract: the entry address of a callable.
;; Degradation: none.
(define (sa-foreign-callable-entry-point co) (foreign-callable-entry-point co))

;; ---- continuations tier (capability: continuations) --------------------------

;; (sa-call-with-escape-continuation proc) -> value
;; Call PROC with a one-shot ESCAPE procedure k. Invoking (k v) returns v from
;; this sa-call-with-escape-continuation call, unwinding the dynamic-wind chain
;; between the two — an escape is a real exit, so a jolt `finally` in between
;; RUNS (unlike a fiber park, which drops those winders first).
;;
;; The contract is call/1cc's, and a target must enforce it rather than merely
;; offer it: k is valid AT MOST ONCE, and only while this call is still on the
;; stack. Invoking it a second time, or after PROC has returned normally, must
;; RAISE — never re-enter, and never hang. That is what lets the layer above
;; (host/chez/continuations.ss) present a single one-shot escape semantic on
;; every target instead of one per target's continuation model.
;;
;; Degradation: a target with no continuations at all must raise a
;; message-carrying condition; jolt.continuations then fails honestly rather
;; than silently doing nothing. Chez: call/1cc natively, so this is the whole
;; implementation — zero wrappers, capture is O(1) and depth-independent.
;; Gambit: call/cc plus the spent flag its adapter carries, because call/cc is
;; multi-shot and would otherwise re-enter a dead frame.
(define (sa-call-with-escape-continuation proc) (call/1cc proc))

;; ---- R8: eval/compile/AOT (capabilities: native-compile, image) --------------

;; (sa-baked-global sym) -> value | #f
;; The top-level value of SYM, or #f when unbound. Callers probe optionally-
;; baked globals (jolt-baked-runtime-fingerprint, jolt-baked-version-early,
;; jolt-compile-eval-form) whose values are never #f, so #f is a safe absent
;; sentinel. Contract: reflect on the running top level by symbol. Degradation:
;; none — every target has SOME notion of its global environment; a target that
;; truly cannot reflect returns #f always (all three callers tolerate absent).
(define (sa-baked-global sym)
  (guard (e (#t #f))
    (and (top-level-bound? sym) (top-level-value sym))))

;; (sa-compile-file src so profile) -> void
;; Compile the Scheme source file SRC to the native object SO under PROFILE:
;; #f = target defaults, or an alist with TARGET-NEUTRAL keys
;; (optimize . 0..3), (inspector-info . bool), (source-info . bool),
;; (compressed . bool). The Chez impl parameterizes optimize-level /
;; generate-inspector-information / generate-procedure-source-information /
;; fasl-compressed and calls compile-file; a target maps the keys it has and
;; ignores the rest. Contract: native compilation of SRC to SO. Degradation:
;; raise a message-carrying condition ((error 'sa-compile-file "...")), never a
;; bare raise — the AOT cache (loader.ss) treats the raise as a cache disable
;; and falls back to loading from source (verified with the entry point forced
;; to raise: programs run correctly from source); `jolt build` surfaces it as a
;; jolt-level error whose message is the condition's.
(define (sa-compile-file src so profile)
  (if profile
      (let ((pv (lambda (k) (cdr (assq k profile)))))
        (parameterize ((optimize-level (pv 'optimize))
                       (generate-inspector-information (pv 'inspector-info))
                       (generate-procedure-source-information (pv 'source-info))
                       (fasl-compressed (pv 'compressed)))
          (compile-file src so)))
      (compile-file src so)))

;; (sa-make-boot-file out base-boots) -> void
;; Assemble the boot file OUT from the base boot files BASE-BOOTS — exactly the
;; shape build.ss:1388 uses, (apply make-boot-file out '() base-boots), with the
;; boot program '() (the running executable loads the boots directly). Contract:
;; write a boot file the target's runtime can boot from. Degradation: raise —
;; same story as sa-compile-file.
(define (sa-make-boot-file out base-boots)
  (apply make-boot-file out '() base-boots))

;; (sa-fasl-write obj port [externals-pred]) -> void
;; fasl-serialize OBJ to PORT, optionally under the externals predicate
;; state-image.ss passes so refused objects are COLLECTED as externals instead
;; of failing the write (jolt.image's dump path). Contract: serialize a value
;; graph to a byte image, honoring the externals hook. Degradation: raise — a
;; target without fasl serialization surfaces jolt.image's dump as a clean
;; jolt-level unsupported error carrying the condition's message — so the raise
;; must be a message-carrying condition ((error 'sa-fasl-write "...")), never a
;; bare raise, whose ex-message is nil (verified both ways with the entry point
;; forced to raise).
(define (sa-fasl-write obj port . rest)
  (apply fasl-write obj port rest))

;; (sa-fasl-read port [who exts]) -> value
;; Deserialize the next object from PORT; the (sa-fasl-read port 'load exts)
;; shape restores a body whose externals were resolved by the caller (jolt.image's
;; restore path). Contract: read back what sa-fasl-write wrote, resolving
;; externals through EXTS. Degradation: raise — same story as sa-fasl-write.
(define (sa-fasl-read port . rest)
  (apply fasl-read port rest))

;; ---- fibers R1: coroutines tier (capability: coroutines) --------------------
;; The fiber primitive + single-carrier scheduler (fibers epic, R1 jolt-nvpr.2).
;; The entry points are defined in fibers.ss, NOT here — this load is what makes
;; them visible to the gate-time adaptercheck (which loads ONLY this file) and to
;; every runtime loader. rt.ss loads fibers.ss again in the usual place; the
;; duplicate define is the harmless re-define pattern this file already relies on
;; (rt.ss:35 loads this file, and the flat build inlines both). fibers.ss is
;; self-contained — it uses only Chez natives, so loading it here, before the
;; value layer, is safe.

;; The dynamic-wind chain, for the scheduler's park path. NOT contract names:
;; these are Chez internals the fiber switch needs and no other target has to
;; supply — fibers.ss degrades to "change nothing" when sa-winder-in answers #f
;; for everything, which is what a target without them gets.
;;
;; They live HERE rather than in fibers.ss because $primitive access is the
;; adapter's job: fibers.ss is not target-owned, so the portability gate makes
;; it route through this file.
;;
;; Chez models the chain as a list of records — `winder` for dynamic-wind, with
;; fields #(in out attachments), and `critical-winder` for parameterize and
;; friends. The rtd is not exported, so it is recovered by building one winder
;; and reading its type back.
(define sa-winder-rtd
  (guard (e (#t #f))
    (dynamic-wind (lambda () #f)
                  (lambda () (record-rtd (car (#%$current-winders))))
                  (lambda () #f))))
(define sa-winder-in-ref
  (guard (e (#t (lambda (r) #f)))
    (record-accessor sa-winder-rtd 0)))

;; (sa-winder-in w) -> the winder's before-thunk, or #f when W is not a
;; dynamic-wind winder (a parameterize's critical-winder answers #f).
(define (sa-winder-in w)
  (and sa-winder-rtd
       (eq? (record-rtd w) sa-winder-rtd)
       (sa-winder-in-ref w)))

;; (sa-current-winders) -> the chain, innermost first.
;; (sa-current-winders-set! w) -> void. Replaces it wholesale.
;;
;; A write must happen in the frame that escapes, NOT inside a guard,
;; dynamic-wind, with-mutex or parameterize: every one of those restores the
;; chain on exit and would silently undo it. See jolt-park-drop-finallys!.
(define (sa-current-winders) (#%$current-winders))
(define (sa-current-winders-set! w) (#%$current-winders w))

;; (sa-disable-count) -> how many nested disable-interrupts this thread is
;; inside; 0 when interrupts are on. Chez keeps it in the thread context, and
;; swish reads it from there (erlang.ss:792, current-disable-count) rather than
;; deriving it.
;;
;; Deriving it is what fibers.ss used to do — (disable-interrupts) returns the
;; new count, so a disable/enable pair answers the question — and the pair is
;; NOT equivalent to a read. It momentarily re-enables, and an enable that
;; brings the count to 0 is a delivery point for anything deferred while
;; interrupts were off. That put a delivery point inside jolt-fiber-park!, on
;; the far side of jolt-park-drop-finallys! and with the fiber already committed
;; to leaving: a Chez timer handler runs at disable-count 0 (probed, not
;; assumed), so the preempt handler's own park measured from 0 and the pair
;; enabled right back to it. A read cannot deliver anything. It is also about
;; cheaper, which matters because the scheduler does this on every switch.
;;
;; #3% and not #%, which is swish's spelling too. The safe entry point resolves
;; the field NAME at run time and costs 10 ns, twice what the disable/enable pair
;; it replaces costs; the unsafe one compiles to the field access and costs 2 ns.
;; What #3% gives up is argument checking, and both arguments here are literal.
(define (sa-disable-count) (#3%$tc-field 'disable-count (#3%$tc)))


;; --- capability-unchecked ---------------------------------------------------
;; Unchecked fixnum / vector primitives, SYNTAX (CONTRACT.txt). A call site
;; carries its own range proof; here each is the #3% primitive — what the
;; whole runtime would be at optimize-level 3, applied to the one loop that
;; has proven it.
(define-syntax sa-ufx+ (syntax-rules () ((_ a b) (#3%fx+ a b))))
(define-syntax sa-ufx- (syntax-rules () ((_ a b) (#3%fx- a b))))
(define-syntax sa-ufx<? (syntax-rules () ((_ a b) (#3%fx<? a b))))
(define-syntax sa-ufx>=? (syntax-rules () ((_ a b) (#3%fx>=? a b))))
(define-syntax sa-ufx=? (syntax-rules () ((_ a b) (#3%fx=? a b))))
(define-syntax sa-uvector-ref (syntax-rules () ((_ v i) (#3%vector-ref v i))))
(define-syntax sa-uvector-set! (syntax-rules () ((_ v i x) (#3%vector-set! v i x))))
;; (sa-vector-copy-range! to at from start end): the R7RS shape over Chez's
;; (vector-copy! from from-start to to-start count).
(define (sa-vector-copy-range! to at from start end)
  (vector-copy! from start to at (fx- end start)))

;; locks.ss first: fibers.ss uses the counting lock wrapper, and jolt-with-mutex
;; is a macro, so it must be defined before this load rather than captured at
;; run time the way the sa-* seams are.
(load "host/chez/locks.ss")
(load "host/chez/fibers.ss")
