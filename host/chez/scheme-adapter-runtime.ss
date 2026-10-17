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
;; so a single attempt can lose that race at any time. Where jolt's own
;; collect-request handler is installed, a refused attempt asks it for the full
;; collection and joins the rendezvous every automatic collection uses: the
;; handler runs once all threads have stopped, which is when Chez allows it, so
;; a compute loop on another thread no longer turns System.gc into a no-op (nor
;; a startup check that needs the boot's garbage gone, rt.ss). Without the
;; handler, retry over a short window and degrade to a no-op, because
;; System.gc must hint, never throw. Any other collector error still raises.
(define sa-full-collect-requested? #f)
(define sa-collect-handler-installed? #f)
(define (sa-gc-collect)
  (let loop ((tries (if sa-collect-handler-installed? 0 100)))
    (let ((r (guard (e (#t (if (and (message-condition? e)
                                    (string=? (condition-message e)
                                              "cannot collect when multiple threads are active"))
                               'busy
                               (raise e))))
               (collect (collect-maximum-generation))
               'ok)))
      (case r
        ;; no collect-request-handler ran, so undo the pins of arrays this one
        ;; found dead here, or they stay locked (and live) until the next pin
        ((ok) (sa-pin-drain-after-collect!))
        ((busy) (cond
                  (sa-collect-handler-installed?
                   (set! sa-full-collect-requested? #t)
                   (collect-rendezvous))
                  ((fx>? tries 0)
                   (sleep (make-time 'time-duration 1000000 0))   ; 1 ms
                   (loop (fx- tries 1)))))))))

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

;; (sa-gc-reserve-ratio! r) -> void
;; How much free memory the collector keeps from the OS after a collection, as a
;; ratio of the memory in use (a nonnegative real): the rest it returns. Chez's
;; heap-reserve-ratio, default 1.0 -- one free page kept for each page in use,
;; which alone lets a process hold twice its data. The heap ceiling lowers it as
;; the heap nears the limit (rt.ss). Contract: best effort. Degradation: a target
;; that does not return memory ignores it.
(define (sa-gc-reserve-ratio! r)
  (heap-reserve-ratio (inexact r)))

;; (sa-gc-tight! on?) -> void
;; Collect TIGHT while ON?: objects already in the older generations are marked
;; where they are rather than copied, so a collection needs little memory beyond
;; what the heap already holds. Copying needs a second copy's worth of room for
;; everything it moves -- a 100MB structure promoted by a scheduled collection
;; carried a 150MB-live program 20MB past a 384MB ceiling. The price is
;; fragmentation (more memory held over a long run: writ's prover peaked 2.3GB ->
;; 2.8GB with it on throughout), so the heap ceiling turns it on only near the
;; limit. Chez's in-place-minimum-generation at 2 (1 fragmented worse: 380MB for
;; that program against 343MB), and back to the maximum generation, its default.
;; Degradation: a target without the choice ignores it.
(define sa-gc-tight-generation 2)
(define (sa-gc-tight! on?)
  (set! sa-young-tight? on?)
  (in-place-minimum-generation
    (if on? (min sa-gc-tight-generation (collect-maximum-generation)) (collect-maximum-generation))))

;; (sa-make-large-bytevector n) -> bytevector
;; A fresh zero-filled bytevector of N bytes that the collector marks where it is
;; instead of copying, for a large pointer-free backing (rt natives-array.ss: byte
;; arrays from 64KB up). Chez pins a huge allocation on its own (segment.c
;; S_find_segments: over 128 segments, must_mark), but only when the request
;; takes FRESH segments. One served from the free segments of an existing chunk
;; is an ordinary mobile object, and a full collection marks a segment in place
;; only when its chunk is at least a quarter used (gc.c
;; chunk_sufficiently_compact) -- so in a chunk a bigger freed object left nearly
;; empty it is copied, at every full collection, each copy reusing free segments
;; again. A live 8MB byte array after a freed 64MB long array cost ~1ms a full
;; collection instead of ~100us that way, on a layout no program controls
;; (#1225). An immobile object is always marked, and still reclaimed when
;; unreachable. Contract: a mutable bytevector of length N, all zero.
;; Degradation: an ordinary bytevector.
(define (sa-make-large-bytevector n)
  (let ((bv (make-immobile-bytevector n)))
    (bytevector-fill! bv 0)
    bv))

;; (sa-max-memory-bytes) -> exact integer
;; Peak heap bytes: the most the collector has held from the OS since the last
;; sa-reset-max-memory-bytes! (or since boot) -- the high-water mark behind
;; jolt.host/maximum-memory-bytes. Not the JVM's maxMemory: that is the heap
;; ceiling (rt.ss jolt-heap-max-bytes), and Runtime.maxMemory reads it there.
;; Contract: never below sa-total-memory-bytes. Degradation: the current total
;; is acceptable -- a target that keeps no high-water mark answers "now".
(define (sa-max-memory-bytes)
  (maximum-memory-bytes))

;; (sa-reset-max-memory-bytes!) -> void
;; Start the high-water mark over from the current total, so a caller can read
;; the peak growth of one stretch of work (the apply-scaling gate). Contract:
;; right after it, sa-max-memory-bytes answers the current total. Degradation:
;; may no-op where sa-max-memory-bytes already answers the current total.
(define (sa-reset-max-memory-bytes!)
  (reset-maximum-memory-bytes!))

;; (sa-real-time-ms) -> exact integer
;; Wall-clock milliseconds, monotonic within a process — used for elapsed
;; deltas (build profiling) and unique temp-file stamps. Contract: an
;; exact-integer ms clock usable for both. Degradation: a target may use any
;; monotonic ms clock (JVM nanoTime/1000000 is fine); a no-op is NOT — the
;; stamps must differ between runs.
(define (sa-real-time-ms)
  (real-time))

;; (sa-epoch-ms) -> exact integer
;; Wall-clock milliseconds since the Unix epoch. Contract: differs between two
;; processes started at different times, which sa-real-time-ms (measured from
;; process start on Chez) does not -- used to salt ids that code compiled in one
;; process must not share with code another emitted. Degradation: none.
(define (sa-epoch-ms)
  (let ((t (current-time 'time-utc)))
    (+ (* (time-second t) 1000) (quotient (time-nanosecond t) 1000000))))

;; ---- records: open-coded predicates, accessors, mutators, constructors -------
;;
;; A nongenerative type's rtd is a compile-time constant in Chez, and cp0
;; open-codes (record? v 'rtd), (record-accessor 'rtd i) and (record-constructor
;; rcd) when the rtd is a constant. What it cannot see through is the top-level
;; VARIABLE define-record-type binds each generated name to: the runtime is a
;; sequence of top-level forms, so a (pvec? x) in a later form is a procedure
;; call through the symbol's value cell (~1.3 ns alone, 4-6 ns per arm inside
;; a dispatch chain), and the predicate's test cannot merge into the accessor
;; that follows it.
;;
;; This define-record-type keeps the R6RS surface and, after the R6RS form has
;; bound the generated names as variables, rebinds them as SYNTAX: in operator
;; position a name expands to the open-coded form on the constant rtd; as a
;; value it denotes a procedure over the same rtd, kept under a %rec/ alias, so
;; (map pvec? xs) and the arm registries that take a predicate keep working on
;; one stable object. (if (pvec? x) (pvec-cnt x) ...) compiles to a
;; single type test.
;;
;; The variable binding stays on purpose: a file loaded EARLIER than the type's
;; (collections.ss calls jrec? before records.ss loads) compiled its reference
;; as a variable call, and Chez keeps a symbol's value cell when the name is
;; later rebound as syntax, so those sites keep working as they did.
;;
;; What is kept, and what changes:
;;   - a wrong-arity call falls through to the procedure, so it fails at run
;;     time exactly as before rather than as a syntax error;
;;   - set! of a generated name is rejected at expansion (nothing in the host
;;     assigns one, and an assignment would silently miss the open-coded sites);
;;   - a type error from an open-coded accessor or mutator names the ENCLOSING
;;     procedure as who; the message and the value/type irritants are the same;
;;   - a generative type, a name spec that is not identifiers, or a field spec
;;     this parser does not recognise gets the plain R6RS definition; a type
;;     with a protocol keeps its protocol constructor as a procedure, and so
;;     does a child type (its constructor arity includes the parent's fields,
;;     which are not visible here) -- their predicates and accessors are still
;;     open-coded.
;; TOP LEVEL ONLY: a body cannot hold both the variable and the syntax binding
;; of one name, so a define-record-type inside a body fails to expand with
;; "multiple definitions ... in body" -- write %r6rs-define-record-type there.
;; Every host record is defined at top level. Gambit has its own
;; define-record-type (host/gambit/prelude-shims.ss) and never loads this file;
;; the shared host sources are untouched.
;;
;; The import is deliberately this narrow: one binding under a private name,
;; so nothing here re-exposes Chez's error/warning over rt.ss's shadowing.
(import (rename (only (rnrs records syntactic) define-record-type)
                (define-record-type %r6rs-define-record-type)))

;; (%define-record-op name proc (arg ...) body): NAME as syntax -- PROC as a
;; value, BODY over ARG ... in operator position, PROC again on any other arity.
(define-syntax %define-record-op
  (syntax-rules ()
    ((_ name proc (arg ...) body)
     (define-syntax name
       (lambda (x)
         (syntax-case x ()
           (id (identifier? #'id) #'proc)
           ((_ arg ...) #'body)
           ((_ . rest) #'(proc . rest))))))))

(define-syntax define-record-type
  (lambda (x)
    (define (sym . parts)
      (string->symbol
       (apply string-append
              (map (lambda (p) (if (symbol? p) (symbol->string p) p)) parts))))
    (define (hidden name) (sym "%rec/" name))
    ;; one field spec -> (accessor mutator-or-#f), or #f when unrecognised
    (define (field-info type-name fs)
      (cond ((symbol? fs) (list (sym type-name "-" fs) #f))
            ((and (pair? fs) (memq (car fs) '(immutable mutable))
                  (pair? (cdr fs)) (symbol? (cadr fs)))
             (let* ((f (cadr fs))
                    (explicit-acc (and (pair? (cddr fs)) (symbol? (caddr fs)) (caddr fs)))
                    (explicit-mut (and explicit-acc (pair? (cdddr fs)) (symbol? (cadddr fs)) (cadddr fs)))
                    (acc (or explicit-acc (sym type-name "-" f)))
                    (mut (and (eq? (car fs) 'mutable)
                              (or explicit-mut (sym type-name "-" f "-set!")))))
               (list acc mut)))
            (else #f)))
    (syntax-case x ()
      ((_ spec clause ...)
       (let* ((d (syntax->datum x))
              (spec-d (cadr d))
              (clauses (cddr d))
              (name (if (pair? spec-d) (car spec-d) spec-d))
              (ctor (if (pair? spec-d) (cadr spec-d) (sym "make-" name)))
              (pred (if (pair? spec-d) (caddr spec-d) (sym name "?")))
              (fields (let ((fc (assq 'fields clauses))) (if fc (cdr fc) '())))
              (infos (map (lambda (fs) (field-info name fs)) fields))
              (ok? (and (symbol? name) (symbol? ctor) (symbol? pred)
                        (assq 'nongenerative clauses)
                        (not (memq #f infos))))
              (ctor-inline? (and ok?
                                 (not (assq 'protocol clauses))
                                 (not (assq 'parent clauses)))))
         (if (not ok?)
             #'(%r6rs-define-record-type spec clause ...)
             (let* ((ctx (syntax-case #'spec () ((n . _) #'n) (n #'n)))
                    (ctor-args (map (lambda (i) (sym "a" (number->string i)))
                                    (iota (length fields))))
                    ;; (name args inline-form procedure-form) per generated name.
                    ;; The procedure is rebuilt from the rtd rather than read from
                    ;; the public name: a top-level begin installs every
                    ;; definition head before it expands a right-hand side, so
                    ;; by then the public name is already the syntax below.
                    (rtd `(record-type-descriptor ,name))
                    (ops
                      (append
                       (list (list pred '(v) `(record? v ,rtd) `(record-predicate ,rtd)))
                       (if ctor-inline?
                           (let ((rc `(record-constructor (record-constructor-descriptor ,name))))
                             (list (list ctor ctor-args `(,rc ,@ctor-args) rc)))
                           '())
                       (let loop ((infos infos) (i 0) (acc '()))
                         (if (null? infos)
                             (reverse acc)
                             (let ((a (car (car infos))) (m (cadr (car infos))))
                               (loop (cdr infos) (+ i 1)
                                     (append
                                      (if m
                                          (list (list m '(v n)
                                                      `((record-mutator ,rtd ,i) v n)
                                                      `(record-mutator ,rtd ,i)))
                                          '())
                                      (list (list a '(v)
                                                  `((record-accessor ,rtd ,i) v)
                                                  `(record-accessor ,rtd ,i)))
                                      acc))))))))
               (datum->syntax ctx
                 `(begin
                    (%r6rs-define-record-type ,spec-d ,@clauses)
                    ,@(map (lambda (op) `(define ,(hidden (car op)) ,(cadddr op))) ops)
                    ,@(map (lambda (op) `(%define-record-op ,(car op) ,(hidden (car op)) ,(cadr op) ,(caddr op)))
                           ops))))))))))

;; ---- R5: io remainder (mtime) + the last GC hook -----------------------------

;; (sa-file-mtime-ms path) -> exact integer
;; Epoch milliseconds of PATH's last modification. Contract: a per-file mtime
;; usable for newer-than comparisons (build freshness, AOT cache keys, the
;; gate-boot staleness predicate). Degradation: none — stat is universal on
;; real targets; do not fake.
(define (sa-file-mtime-ms path)
  (let ((t (file-modification-time path)))
    (+ (* (time-second t) 1000) (div (time-nanosecond t) 1000000))))

;; (sa-file-size path) -> exact integer
;; PATH's size in bytes. Contract: the size a reader of the whole file would
;; see (a cache's byte budget reads it). Degradation: none — a file that cannot
;; be opened raises, as the caller's other file operations would.
(define (sa-file-size path)
  (let ((p (open-file-input-port path)))
    (let ((n (port-length p)))
      (close-port p)
      n)))

;; (sa-gc-trip-bytes! n) -> void
;; Set the allocation threshold at which a trip collection triggers — the
;; dev-cache CLI's GC tuning knob (cli-devcache.ss). Contract: honor N as a
;; collection-trip hint. Degradation: may no-op — the call tunes a dev cache
;; only; collection still happens on its own schedule.
(define (sa-gc-trip-bytes! n)
  (collect-trip-bytes n))

;; (sa-gc-trip-bytes) -> exact integer
;; The allocation threshold at which a trip collection triggers: between two
;; collections at most this much is allocated, which bounds how far work that
;; holds nothing can raise the heap footprint (the apply-scaling gate's floor).
;; Contract: the threshold in bytes. Degradation: 0 -- the target has no such
;; threshold, so nothing is invisible by construction.
(define (sa-gc-trip-bytes)
  (collect-trip-bytes))

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
;;
;; Memoized, which is what the note on sa-probed-os-family below already asks
;; for and does not deliver: that cache sits on the PROBE, which only a pb tag
;; reaches, so every real tag (ta6le, ta6nt, tarm64osx) re-derived on every
;; call. The derivation is not free — sa-host-tag allocates a fresh string per
;; call through symbol->string, and each sa-tag-contains? walks it allocating a
;; substring per position, five needles deep. machine-type cannot change while
;; the process runs, so one derivation is all there is to do.
;;
;; It matters because path classification is host-specific: java.io.File's
;; isAbsolute and the project-relative base every filesystem touch goes through
;; (java/io.ss jfile-fs) ask this per call. Uncached, .isAbsolute measured 355ms
;; per 200k calls against 141ms; .exists, syscall included, 167ms per 50k
;; against 116ms.
;;
;; The memo is module state in a file the runtime LOADS, not a saved heap, so it
;; starts empty in every process and a cross-built binary cannot carry the build
;; host's answer.
(define sa-os-family-memo #f)
(define (sa-os-family)
  (or sa-os-family-memo
      (let ((fam (sa-os-family-for-tag (sa-host-tag))))
        (set! sa-os-family-memo fam)
        fam)))

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

;; Field N of uname(2) — 0 sysname, 1 nodename, 2 release, 3 version, 4
;; machine — or #f when it cannot be had. Probe-only and fully guarded: a
;; statically linked build may not carry the symbol at all, and a missing arch
;; is a documented degradation rather than a boot failure. struct utsname is
;; five (Linux: six, with domainname) char arrays of one fixed width each:
;; _UTSNAME_LENGTH is 65 on glibc, musl and bionic, _SYS_NAMELEN 256 on the
;; BSDs and macOS.
;;
;; No sa-load-shared-object here, on sa-fbytes-init!'s reasoning and for its
;; reason: the boot took the process-global handle long before anything can ask
;; for an arch (rt.ss binds _exit through jolt-foreign-proc-safe at its line
;; 135, and the first caller of sa-arch is host-static-methods.ss at 1553), and
;; re-taking it would re-promote it above every :jolt/native loaded since.
(define (sa-uname-field n)
  (guard (e (#t #f))
    (and (foreign-entry? "uname")
         (let* ((linux? (eq? (sa-os-family) 'linux))
                (width (if linux? 65 256))
                (off (* n width))
                (size (* 6 width))
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
                        (if (or (fx=? b 0) (fx>=? i (fx- width 1)))
                            (and (pair? acc) (list->string (reverse acc)))
                            (loop (fx+ i 1) (cons (integer->char b) acc)))))))
             (lambda () (foreign-free buf)))))))
(define (sa-uname-machine) (sa-uname-field 4))

;; (sa-os-release) -> string | #f
;; The operating system's own version string, what its version tool prints:
;; the product version on macOS (sysctl kern.osproductversion, what `sw_vers
;; -productVersion` reads), the kernel release elsewhere on POSIX (uname(2), what
;; `uname -r` prints). Contract: that string, or #f where the target does not
;; know it. Degradation: #f — the caller leaves the property out, which is the
;; JVM's answer for an os.version it cannot read. Read in process: this used to
;; run those two programs through the shell, so a machine with an empty PATH,
;; or Windows, where the shell is cmd.exe, answered nothing.
(define (sa-os-release)
  (guard (e (#t #f))
    (case (sa-os-family)
      ((macos)
       (and (foreign-entry? "sysctlbyname")
            (let* ((f (foreign-procedure "sysctlbyname" (string u8* u8* void* size_t) int))
                   (out (make-bytevector 64 0))
                   (len (make-bytevector 8 0)))
              (bytevector-u64-native-set! len 0 64)
              (and (= 0 (f "kern.osproductversion" out len 0 0))
                   (let ((n (let scan ((i 0))
                              (if (or (fx=? i 64) (fx=? 0 (bytevector-u8-ref out i))) i (scan (fx+ i 1))))))
                     (and (fx>? n 0)
                          (let ((bv (make-bytevector n)))
                            (bytevector-copy! out 0 bv 0 n)
                            (utf8->string bv))))))))
      ((windows) #f)
      (else (sa-uname-field 2)))))

;; (sa-environment-pairs) -> list of (name . value)
;; The whole process environment, as the R7RS get-environment-variables alist;
;; the source (getenv "NAME") reads, so the two agree. Contract: that alist, in
;; the table's order. Degradation: none — every target has an environment. On
;; Chez it is read from the C runtime's own table: _NSGetEnviron() on macOS
;; (the form a dylib is documented to use), the environ variable elsewhere on
;; POSIX (glibc, musl and bionic all export it), and GetEnvironmentStringsW on
;; Windows, whose block is UTF-16 and double-NUL terminated. An entry with no
;; "=" or an empty name (the hidden "=C:=C:\…" drive entries Windows keeps) is
;; dropped, as the JVM's ProcessEnvironment drops it. This used to spawn `env -0`
;; through the shell, which needed an env program on PATH and, on Windows, a
;; command cmd.exe does not have: (System/getenv) answered an empty map and a
;; ProcessBuilder child inherited nothing.
;; The two kernel32 entry points the Windows read uses, resolved ONCE: a
;; (get . free-or-#f) pair, or #f when kernel32 has no GetEnvironmentStringsW.
;; load-shared-object prepends a handle to Chez's search list every time it is
;; called, so loading kernel32 per read — every (System/getenv) map and every
;; ProcessBuilder child — would lengthen every later foreign-entry lookup and
;; re-promote kernel32 above each :jolt/native loaded since.
(define sa-windows-env-entries
  (let ((resolved #f) (entries #f))
    (lambda ()
      (unless resolved
        (set! resolved #t)
        (guard (e (#t #f)) (load-shared-object "kernel32.dll"))
        (when (foreign-entry? "GetEnvironmentStringsW")
          (set! entries
                (cons (sa-foreign-procedure-runtime "GetEnvironmentStringsW" '() 'void* #f)
                      (and (foreign-entry? "FreeEnvironmentStringsW")
                           (sa-foreign-procedure-runtime "FreeEnvironmentStringsW" '(void*) 'int #f))))))
      entries)))
(define (sa-environment-pairs)
  (define (c-string addr)
    (let loop ((n 0))
      (if (fx=? 0 (foreign-ref 'unsigned-8 addr n))
          (let ((bv (make-bytevector n)))
            (sa-foreign-bytes-ref! addr bv n)
            (utf8->string bv))
          (loop (fx+ n 1)))))
  (define (split entry)
    (let ((i (let scan ((j 0))
               (cond ((fx=? j (string-length entry)) #f)
                     ((char=? (string-ref entry j) #\=) j)
                     (else (scan (fx+ j 1)))))))
      (and i (fx>? i 0)
           (cons (substring entry 0 i) (substring entry (fx+ i 1) (string-length entry))))))
  (define (from-envp envp)
    (let ((w (foreign-sizeof 'void*)))
      (let loop ((i 0) (acc '()))
        (let ((p (foreign-ref 'void* envp (fx* i w))))
          (if (eqv? p 0)
              (reverse acc)
              (let ((kv (split (c-string p))))
                (loop (fx+ i 1) (if kv (cons kv acc) acc))))))))
  (define (posix-envp)
    (cond
      ((foreign-entry? "_NSGetEnviron")
       (foreign-ref 'void* ((foreign-procedure "_NSGetEnviron" () void*)) 0))
      ((foreign-entry? "environ")
       (foreign-ref 'void* (foreign-entry "environ") 0))
      (else #f)))
  (define (windows-block)
    (and (sa-windows-env-entries)
         (let* ((get (car (sa-windows-env-entries)))
                (free (cdr (sa-windows-env-entries)))
                (block (get)))
           (and (not (eqv? block 0))
                (dynamic-wind
                  (lambda () #f)
                  (lambda ()
                    ;; one UTF-16 string per entry, an empty string ends the block
                    (let loop ((off 0) (acc '()))
                      (let ((n (let scan ((k 0))
                                 (if (fx=? 0 (foreign-ref 'unsigned-16 block (fx+ off (fx* 2 k))))
                                     k
                                     (scan (fx+ k 1))))))
                        (if (fx=? n 0)
                            (reverse acc)
                            (let ((bv (make-bytevector (fx* 2 n))))
                              (sa-foreign-bytes-ref! (fx+ block off) bv (fx* 2 n))
                              (let ((kv (split (utf16->string bv 'little))))
                                (loop (fx+ off (fx* 2 (fx+ n 1))) (if kv (cons kv acc) acc))))))))
                  (lambda () (when free (free block))))))))
  (guard (e (#t '()))
    (if (eq? (sa-os-family) 'windows)
        (or (windows-block) '())
        (let ((envp (posix-envp)))
          (if envp (from-envp envp) '())))))

;; (sa-endian) -> 'little | 'big
;; Byte order of the host. Contract: the byte order. Degradation: none — the
;; runtime always knows its own byte order, so a tag that does not name one is
;; answered by native-endianness rather than by #f.
;; (sa-flonum->bits x) / (sa-bits->flonum b): binary64 pattern as an unsigned
;; 64-bit integer and back, through an 8-byte buffer of the call's own (a
;; shared one would race between threads).
(define (sa-flonum->bits x)
  (let ((bv (make-bytevector 8)))
    (bytevector-ieee-double-set! bv 0 x (endianness little))
    (bytevector-u64-ref bv 0 (endianness little))))
(define (sa-bits->flonum b)
  (let ((bv (make-bytevector 8)))
    (bytevector-u64-set! bv 0 b (endianness little))
    (bytevector-ieee-double-ref bv 0 (endianness little))))
;; The high word is read straight out of the flonum, without a buffer, which is
;; most of a bit cast's cost (fdlibm's cbrt makes four). The payload's offset
;; from the tagged pointer is found once at load by probing 2.5 for its own
;; bytes. Every probe has to stay inside the object: one below the payload reads
;; the bytes in front of it, and one above reads past its end, either of which is
;; unmapped memory when the flonum sits at the edge of a segment (a probe from
;; below killed scheme.exe at load on Windows, #1337). On 64-bit Chez the 8-byte
;; payload is the object's last 8 bytes, 6 past the tagged pointer, so the probe
;; starts there and only counts down if that misses. The high word is the
;; payload's upper half in the machine's byte order.
(define (sa-flonum-payload-disp x)
  (let loop ((d (if (fx> (fixnum-width) 32) 6 8)))
    (cond ((fx< d 0) (error 'sa-flonum-disp "no flonum payload"))
          ((eqv? (#%$object-ref 'double x d) x) d)
          (else (loop (fx- d 1))))))
(define sa-flonum-disp (sa-flonum-payload-disp 2.5))
(define sa-flonum-hi-disp
  (if (eq? (native-endianness) (endianness little)) (fx+ sa-flonum-disp 4) sa-flonum-disp))
(define (sa-flonum-hi32 x) (#%$object-ref 'unsigned-32 x sa-flonum-hi-disp))
;; Built arithmetically, exactly: (1 + m/2^20) * 2^(e-1023) for a normal field,
;; m * 2^-1042 for a subnormal one, each power of two from a table.
(define sa-pow2-base 1100)
(define sa-pow2
  (let ((v (make-flvector 2200)))
    (do ((k 0 (fx+ k 1))) ((fx= k 2200) v)
      (flvector-set! v k (inexact (expt 2 (- k sa-pow2-base)))))))
(define (sa-hi32->flonum h)
  (let* ((e (fxand (fxsrl h 20) #x7ff))
         (m (fxand h #xfffff))
         (mag (cond ((fx= e #x7ff) (if (fx= m 0) +inf.0 +nan.0))
                    ((fx= e 0) (fl* (fixnum->flonum m) (flvector-ref sa-pow2 (fx- sa-pow2-base 1042))))
                    (else (fl* (fixnum->flonum (fx+ #x100000 m))
                               (flvector-ref sa-pow2 (fx+ sa-pow2-base (fx- e 1043))))))))
    (if (fx= 0 (fxand h #x80000000)) mag (fl- mag))))

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

;; (sa-procedure-code-name p) -> string | #f: the name the procedure's CODE
;; object carries — the variable its lambda was bound under. Every closure of
;; one lambda answers the same string, and it survives a build with inspector
;; information off (it is what #<procedure name> prints), which is what makes
;; it an identity for a wrapper kind (continuations.ss jolt-escape-fn?). #f
;; for a procedure whose code carries no name.
(define (sa-procedure-code-name p)
  (let ((n (#%$code-name (#%$closure-code p))))
    (and (string? n) n)))

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
;;
;; The process handle is loaded ONCE. Chez pushes a handle onto its dynamic
;; lookup list on every load-shared-object, and every foreign-entry lookup
;; walks that list with dlsym before it reaches the static "(cs)…" table
;; (c/foreign.c lookup). rt.ss asks for the process at each
;; jolt-foreign-proc-safe site — 57 by the end of boot — so a lookup that
;; misses the process (every Chez-internal "(cs)" entry: inspect/object binds
;; two per call; every optional-entry probe at boot) walked 57 handles, 2 ms a
;; miss. And re-loading #f after a native library re-promotes the process's
;; symbols over the library's (the BoringSSL/OpenSSL EVP_* flip). Named
;; libraries load as before; test/chez/foreign-handles-test.ss pins this.
(define sa-process-symbols-loaded? #f)
(define (sa-load-shared-object name)
  (if name
      (load-shared-object name)
      (unless sa-process-symbols-loaded?
        (set! sa-process-symbols-loaded? #t)
        (load-shared-object #f))))

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

;; A conversion that fails part way leaves a truncated OUT, which a caller
;; testing (file-exists? out) would take for a result; #f leaves no OUT.
(define (sa-delete-partial! out)
  (guard (e (#t #f)) (when (file-exists? out) (delete-file out))))

;; (sa-vfasl-convert-file in out [codec]) -> boolean
;; Rewrite the boot file IN to OUT in Chez's vfasl format: a prebuilt image of
;; what loading the fasl would have produced, laid out per space and loaded
;; straight into the static generation, which is worth roughly a third of jolt's
;; own startup (see build-jolt.ss). Contract: produce a boot the target's runtime
;; can boot from, or answer #f. Degradation: #f rather than raise — an app that
;; boots slower is strictly better than an app that fails to build, and the
;; caller keeps the plain boot it already has.
;;
;; CODEC is 'default, or 'wide for an image the target's default entry codec
;; cannot carry — the caller has measured the image and found it over a limit
;; (build.ss bld-lz4-image-ceiling). A target with one codec ignores the
;; argument; Chez has two, and 'wide picks gzip over LZ4, which is the whole
;; point: gzip has no 256MiB ceiling and LZ4 does.
(define (sa-vfasl-convert-file in out . codec)
  (guard (e (#t (sa-delete-partial! out) #f))
    (if (and (pair? codec) (eq? (car codec) 'wide))
        (parameterize ((compress-format 'gzip)) (vfasl-convert-file in out '()))
        (vfasl-convert-file in out '()))
    #t))

;; (sa-vfasl-convert-object-file in out [codec]) -> boolean
;; The same conversion for ONE compiled object file rather than a whole boot:
;; OUT keeps IN's object-file header instead of gaining a boot header, so it can
;; follow a converted boot in the same image (a boot file is its inputs
;; concatenated, and the kernel skips each input's header). This is what lets a
;; build convert only what changed — the runtime prefix and each app unit are
;; converted once and cached (build.ss). Output entries are compressed, as a
;; whole-boot conversion's are. Same contract and degradation as above.
(define (sa-vfasl-convert-object-file in out . codec)
  (guard (e (#t (sa-delete-partial! out) #f))
    (parameterize ((fasl-compressed #t)
                   (compress-format (if (and (pair? codec) (eq? (car codec) 'wide))
                                        'gzip
                                        (compress-format))))
      (vfasl-convert-file in out #f))
    #t))

;; (sa-strip-compile-time-info! in out) -> boolean
;; Copy the compiled object file IN to OUT without its compile-time
;; information: the visit-time code and expander data of its macros
;; (define-syntax transformers, their syntax objects and ribcages), plus any
;; inspector and source annotations. What remains is the code that runs. For a
;; binary that cannot expand Scheme code at run time (booted from petite, no
;; compiler): 3.3MB of a closed-world app's 20MB runtime heap, and a third of
;; its runtime fasl. Contract: OUT loads and runs as IN does. Degradation: #f
;; (OUT not written), and the caller ships IN.
(define (sa-strip-compile-time-info! in out)
  (guard (e (#t (sa-delete-partial! out) #f))
    (strip-fasl-file in out (fasl-strip-options compile-time-information inspector-source
                                                source-annotations profile-source))
    #t))

;; (sa-gc-install-after-collect! maintain observe) -> boolean
;; Hook every collection: the target performs its normal collection, then calls
;; (MAINTAIN collect-full!) -- collect-full! collects EVERY generation now, the
;; collection a generational collector defers; (collect-full! room) does it TIGHT
;; (see sa-gc-tight!) within ROOM free bytes, in steps when the younger
;; generations hold more than that (sa-collect-tight) -- and then
;; (OBSERVE gc-ns elapsed-ns): how long the whole of this collection took,
;; MAINTAIN's work included, and how long since the previous one ended, both
;; monotonic nanoseconds. Answers whether the target could install the hook.
;;
;; The policy lives in the caller (rt.ss jolt-install-gc-policy!): the heap
;; ceiling, when to collect the older generations, and the nursery size. This is
;; only the seam that hooks collection, because doing that needs a target-specific
;; native. Both run where the collect request is handled, with the world stopped:
;; MAINTAIN may collect everything (collect-full!, and only that way: sa-gc-collect
;; is the out-of-handler entry), both may read the heap (sa-bytes-allocated) and
;; set the trip threshold (sa-gc-trip-bytes!); raising from either is how a
;; ceiling or the GC overhead limit reports.
;;
;; Contract: both are called after each collection the target runs on its own
;; schedule. Degradation: answer #f without installing anything. The caller then
;; enforces no ceiling -- what every jolt before 0.8.5 did -- and keeps a fixed
;; nursery, and reports maxMemory as unbounded rather than promising a bound it
;; lacks.
(define (sa-gc-install-after-collect! maintain observe)
  (let ((last-end (sa-monotonic-ns))
        (collect-full!
          (case-lambda
            (() (collect (collect-maximum-generation)))
            ;; ROOM: #f for an ordinary full collection, else the free bytes the tight
            ;; one has to work in
            ((room)
             (if room
                 (let ((saved (in-place-minimum-generation)))
                   (dynamic-wind
                     (lambda () (in-place-minimum-generation 1))
                     (lambda () (sa-collect-tight room))
                     (lambda () (in-place-minimum-generation saved))))
                 (collect (collect-maximum-generation)))))))
    (collect-request-handler
      (lambda ()
        (if sa-full-collect-requested?
            ;; sa-gc-collect's full collection, run here because another thread
            ;; was active: what its direct path does, and no more -- it is not a
            ;; young collection for the nursery's time share to count
            (begin
              (set! sa-full-collect-requested? #f)
              (collect (collect-maximum-generation))
              (sa-pin-drain-after-collect!))
            (let ((t0 (sa-monotonic-ns)))
              (sa-collect-young!)
              (maintain collect-full!)
              (sa-pin-drain-after-collect!)
              (let ((t1 (sa-monotonic-ns)))
                (observe (- t1 t0) (- t1 last-end))
                (set! last-end t1)))))))
  (set! sa-collect-handler-installed? #t)
  #t)
;; A tight full collection within ROOM free bytes: every generation from 1 up
;; is marked where it is rather than copied, in ONE collection. Chez marks a
;; segment only when its generation is at least both in-place-minimum-generation
;; and the collection's min-tg (gc.c), so (collect max 1 max) with the minimum at
;; 1 copies generation 0 and the sparse segments Chez always recopies (a chunk
;; under a quarter used, a segment marked before and now under three quarters
;; live), and promotes the rest in place.
;;
;; A copying collection peaks at what the heap holds plus everything it copies,
;; since a source is freed only when the collection ends. The collection this
;; replaces staged 1 into 2, 2 into 3, then everything, so each step copied a
;; whole generation: on bionic a scheduled promotion had just left 251MB held
;; under a 256MB ceiling, the first step copied generation 1's 35MB on top, and
;; the gcpolicy gate peaked at 317MB. Marking in place needs no to-space, which
;; is how GHC treats the oldest generation near -M and why HotSpot's full
;; collections are in place. The young collections keep the minimum at 2 (1
;; fragmented more over a long run); only this one marks generation 1.
;;
;; ROOM is unused: the collection no longer needs any to fit.
(define (sa-collect-tight room)
  (let ((cmg (collect-maximum-generation)))
    (collect cmg 1 cmg)))

;; The collection the hook runs in place of Chez's (collect). Chez's schedule
;; collects generation g every radix^g collections and otherwise only
;; generation 0, promoting what survives into generation 1. That breeds
;; nepotism in lazy seqs: the cell a walk is on when a collection comes is live,
;; so it is promoted; the walk then realizes its tail, storing a young cell into
;; it, and moves on. The promoted cell is dead, but its generation is not
;; collected next time, so the write barrier's remembered set roots that young
;; cell, and through it every cell realized since -- the whole window. Measured
;; in bench/seqs: ~70% of every nursery survived (209k seq cells at 16MB, 0.8.12
;; alike), every collection copied it, and the cost grew with the nursery, so
;; the nursery policy's larger windows ran the bench 1.1-1.4x slower.
;;
;; So every collection takes generation 1 with it: (collect g 1 g+1) with g at
;; least 1, where generation 0's survivors go to 1 and everything collected
;; above that moves up one. The cell a walk was on last time is in generation 1
;; and is collected with the rest, so nothing it points at survives through it.
;; The price is that generation 1 is copied into 2 at every collection rather
;; than every fourth: it only ever holds one window's survivors, and in
;; practice cost nothing measurable (writ's prover: same time and GC time;
;; bench/seqs 245ms -> 165ms against 0.8.12, the heap-churn gate 2.3s -> 2.1s
;; and 404MB -> 320MB). The maximum generation is never on this schedule: the
;; policy's own full collections take it (rt.ss, heap growth and the ceiling).
;;
;; Near a heap ceiling (sa-gc-tight!) the collection copies only generation 0,
;; Chez's own: copying generation 1 as well needs room the ceiling may not have
;; (the gcpolicy gate peaked 30% over a 256MB ceiling that way).
(define sa-young-count 0)
(define sa-young-tight? #f)
(define (sa-collect-young!)
  (set! sa-young-count (fx+ sa-young-count 1))
  (if sa-young-tight?
      (collect)
      (let* ((cmg (collect-maximum-generation))
             (radix (collect-generation-radix))
             (g (let loop ((g (fx- cmg 1)))
                  (if (or (fx<= g 1) (fx= 0 (modulo sa-young-count (expt radix g))))
                      g
                      (loop (fx- g 1))))))
        (collect (fxmax 1 g) 1 (fxmin cmg (fx+ (fxmax 1 g) 1))))))
(define (sa-monotonic-ns)
  (let ((t (current-time 'time-monotonic)))
    (+ (* (time-second t) 1000000000) (time-nanosecond t))))
;; (sa-setenv! name value) -> void
;; Set NAME in this process's environment, which getenv reads back and a child
;; process inherits. Used by the gate runners to give themselves a private
;; TMPDIR (gate-private-tmpdir.ss).
(define (sa-setenv! name value) (putenv name value))
;; (sa-gc-install-stall-watch! seconds on-stall) -> boolean
;; Make a stalled collection observable. Chez stops the world by rendezvous:
;; the thread whose allocation tripped runs $collect-rendezvous, and unless it
;; is the last ACTIVE thread it waits on $collect-cond for the others to reach
;; a safe point; the last one to arrive runs the collect-request-handler and
;; broadcasts. A thread parked in a foreign call that is not __collect_safe
;; stays active and never arrives, so every other thread waits for that call
;; to return — and when the call is itself waiting for one of the waiting
;; threads (a :collect-safe callback producing its answer), for good. Nothing
;; in the wait can allocate or print: a Scheme watchdog is one more thread
;; that reaches the rendezvous and waits, and a C thread cannot read the
;; symbols involved through scheme.h (Stop_level_value calls into Scheme).
;;
;; So the rendezvous itself is where the stall is seen. This installs a copy
;; of Chez 10.4's $collect-rendezvous (s/7.ss) whose waits carry a timeout:
;; a thread that has waited SECONDS with the request still pending and other
;; threads still active calls ON-STALL once per stalled request — with the tc
;; mutex released and interrupts disabled, so it may allocate and print
;; without re-entering the rendezvous — then goes on waiting exactly as
;; before. SECONDS is a positive real, or a thunk answering one, read at each
;; wait: a caller that changes its threshold later does so through the thunk
;; rather than by installing again, which would swap the copy under a thread
;; already waiting in it. The protocol is otherwise the original's: the same conditions,
;; the same last-one-standing rule, the same hand-over to the main thread
;; when it is waiting, so a stall that ends (the call returned) completes the
;; collection as it always did. $collect-rendezvous is immutable in the system
;; environment; $set-top-level-value! writes the slot the event handler
;; (s/library.ss) reads on every collect request, which is why every
;; collection on every thread, explicit or trip-driven, goes through the copy.
;;
;; The tc mutex goes through the counting pair (locks.ss jolt-lock! /
;; jolt-unlock!) and the waits through jolt-stop-the-world-wait, the one
;; condition-wait beneath the fiber layer: a collect request stops the whole
;; carrier, fibers and all, so this is the wait that must block it whatever is
;; running there.
;;
;; Contract: ON-STALL receives the number of active threads other than the
;; caller, i.e. the threads that have not reached a safe point.
;; Degradation: answer #f without installing anything, leaving the target's
;; own rendezvous in place (no diagnostic, the behaviour of every release
;; before 0.8.9). The copy is of ONE protocol, so it is installed only on a
;; Chez whose protocol was compared with it, and only when every name it needs
;; is bound; any other Chez answers #f rather than run a rendezvous it might
;; disagree with. Compared: 10.4 (read from), and 10.5, whose s/7.ss
;; $collect-rendezvous and s/library.ss are unchanged from v10.4.1 (checked at
;; cisco/ChezScheme 35c2f4b3, 10.5.0 pre-release; the c/ changes since touch
;; W^X code pages and gen-0 marking, not the rendezvous). The ffi gate (make ffi) asserts the
;; watch is live on the Chez CI builds, so a Chez bump re-asks the question.
(define (sa-gc-install-stall-watch! seconds on-stall)
  (let ((se (#%$system-environment)))
    (define (bound? name) (top-level-bound? name se))
    (define (sysval name) (top-level-value name se))
    (and (call-with-values scheme-version-number
           (lambda (major minor sub) (and (= major 10) (memv minor '(4 5)))))
         (bound? '$collect-rendezvous)
         (bound? '$collect-cond) (bound? '$collect-thread0-cond)
         (bound? '$tc-mutex) (bound? '$active-threads)
         (bound? '$collect-request-pending)
         (let ((tc-mutex (sysval '$tc-mutex))
               (collect-cond (sysval '$collect-cond))
               (thread0-cond (sysval '$collect-thread0-cond))
               ;; the time object for the threshold last read, rebuilt only
               ;; when the threshold changes
               (timeout-for #f)
               (timeout #f)
               ;; #t once ON-STALL has run for the request now pending; cleared
               ;; where the request is cleared, so the next stall reports again.
               (reported? #f)
               (thread0-waiting? #f))
           (define (active-threads) (sysval '$active-threads))
           (define (pending?) (sysval '$collect-request-pending))
           (define (pending-set! v) (#%$set-top-level-value! '$collect-request-pending v))
           ;; condition-wait's #t is a signal, #f the timeout. On a timeout with
           ;; the request still pending and others still active, report once.
           (define (current-timeout)
             (let ((secs (if (procedure? seconds) (seconds) seconds)))
               (unless (eqv? secs timeout-for)
                 (let* ((ns (exact (round (* secs 1000000000))))
                        (s (quotient ns 1000000000)))
                   (set! timeout (make-time 'time-duration (- ns (* s 1000000000)) s))
                   (set! timeout-for secs)))
               timeout))
           (define (timed-wait c)
             (or (jolt-stop-the-world-wait c tc-mutex (current-timeout))
                 (begin
                   (when (and (pending?) (> (active-threads) 1) (not reported?))
                     (set! reported? #t)
                     (jolt-unlock! tc-mutex)
                     (on-stall (- (active-threads) 1))
                     (jolt-lock! tc-mutex))
                   #f)))
           (define (stall-rendezvous)
             (define once
               (let ((once #f))
                 (lambda ()
                   (when (eq? once #t)
                     (#%$oops '$collect-rendezvous
                              "cannot return to the collect-request-handler"))
                   (set! once #t))))
             (dynamic-wind
               (lambda () (disable-interrupts) (jolt-lock! tc-mutex))
               (lambda ()
                 (let f ()
                   (when (pending?)
                     (cond
                       ((= (active-threads) 1)   ; last one standing
                        (cond
                          ((or (eqv? 0 (get-thread-id)) (not thread0-waiting?))
                           (dynamic-wind
                             once
                             (collect-request-handler)
                             (lambda ()
                               (pending-set! #f)
                               (set! reported? #f)
                               (condition-broadcast collect-cond)
                               (condition-broadcast thread0-cond))))
                          (else
                           ;; get the main thread to perform the GC instead
                           (condition-broadcast thread0-cond)
                           (timed-wait collect-cond)
                           (f))))
                       ((eqv? 0 (get-thread-id))
                        (set! thread0-waiting? #t)
                        (timed-wait thread0-cond)
                        (set! thread0-waiting? #f)
                        (f))
                       (else
                        (timed-wait collect-cond)
                        (f))))))
               (lambda () (jolt-unlock! tc-mutex) (enable-interrupts))))
           (#%$set-top-level-value! '$collect-rendezvous
             (lambda () (stall-rendezvous) (sa-run-after-rendezvous)))
           (set! sa-rendezvous-hooked? #t)
           #t))))

;; (sa-gc-install-after-rendezvous! thunk) -> boolean
;; Run THUNK on every thread that takes part in a collection, on that thread,
;; once the collection is over: the thread that ran the collect-request handler
;; and every one that waited for it, each from the event check that brought it
;; to the rendezvous. So THUNK runs at a safe point of whatever the thread was
;; doing, with interrupts enabled, and may raise there: the raise unwinds that
;; thread's own computation, as an escape from a timer interrupt does. A thread
;; parked in a foreign call takes no part and runs nothing.
;;
;; Every thread that allocates comes through here at the rate it allocates,
;; which is what the stack limit (rt.ss jolt-install-stack-limit!) needs: Chez
;; grows a stack by allocating segments on the heap, so a thread recursing
;; without bound allocates, and is stopped at the next collection — on its own
;; thread, which a collect-request handler (run by the main thread when it is
;; active, whoever tripped the collection) cannot do. Nothing runs per call or
;; per allocation: the cost is THUNK, once per thread per collection.
;;
;; The slot is the one sa-gc-install-stall-watch! writes. Its copy calls the hook
;; itself; on a Chez the copy was not installed on, the target's own rendezvous
;; is wrapped instead, once. One hook: a second install replaces the first.
;; Contract: THUNK runs after each collection, on each participating thread.
;; Degradation: answer #f without installing anything (no collection
;; rendezvous to hook); the caller then enforces nothing.
(define sa-after-rendezvous #f)
(define sa-rendezvous-hooked? #f)       ; the installed $collect-rendezvous calls it
(define (sa-run-after-rendezvous)
  (let ((h sa-after-rendezvous)) (when h (h))))
(define (sa-gc-install-after-rendezvous! thunk)
  (let ((se (#%$system-environment)))
    (and (top-level-bound? '$collect-rendezvous se)
         (begin
           (unless sa-rendezvous-hooked?
             (let ((orig (top-level-value '$collect-rendezvous se)))
               (#%$set-top-level-value! '$collect-rendezvous
                 (lambda () (orig) (sa-run-after-rendezvous)))
               (set! sa-rendezvous-hooked? #t)))
           (set! sa-after-rendezvous thunk)
           #t))))

;; (sa-current-stack-bytes) -> exact integer | #f
;; How much stack the calling thread holds: the frames of every segment below
;; the one it is running on. Chez starts a thread on a 64KB segment and, when a
;; call would overflow it, moves the frames below into a continuation object on
;; the heap and carries on in a fresh segment (S_overflow, c/schsig.c), so a
;; deep stack is a chain of those, linked from the thread's stack link, each
;; with the length of the frames it holds. The segment being run on is left
;; out: it is at most a few segments' worth, and reading its fill needs the
;; frame pointer.
;;
;; The chain only changes at the top -- a segment is pushed, or popped when a
;; return underflows into it -- so the walk stops at the segment it reached
;; last time on this thread and adds what it measured there: the cost is the
;; segments pushed since, not the depth. The memo holds that segment WEAKLY;
;; holding it would keep the whole stack below it alive after it unwound, which
;; is exactly the memory a caught overflow has to give back.
;;
;; Identity alone does not name a segment: a one-shot continuation that returns
;; normally is parked in the thread's cached-frame and the next call/1cc reuses
;; the object with a new link and length (reify-cc-help, s/cpnanopass.ss), so a
;; letcc at depth then one near the bottom would add the old depth to the new.
;; The memo also keeps the segment's link (weakly) and length, and a segment
;; that no longer has both is walked past.
;; Contract: the calling thread's stack depth in bytes, monotone in depth,
;; within a few segments. Degradation: #f (cannot measure); the caller then
;; enforces nothing.
(define sa-stack-memo (make-thread-parameter #f))   ; weak (segment . #(words link-box length))
(define sa-ptr-bytes (foreign-sizeof 'ptr))
(define (sa-current-stack-bytes)
  (let ((top (#%$current-stack-link))
        (bottom #%$null-continuation)
        (memo (sa-stack-memo)))
    (define (remember! words)
      (sa-stack-memo
        (and (#%$continuation? top) (not (eq? top bottom))
             (weak-cons top (vector words
                                    (weak-cons (#%$continuation-link top) #f)
                                    (#%$continuation-stack-clength top))))))
    (let loop ((k top) (words 0))
      (cond
        ((or (eq? k bottom) (not (#%$continuation? k)))
         (remember! words)
         (* words sa-ptr-bytes))
        ((and memo
              (eq? k (car memo))
              (let ((v (cdr memo)))
                (and (eq? (#%$continuation-link k) (car (vector-ref v 1)))
                     (eqv? (#%$continuation-stack-clength k) (vector-ref v 2)))))
         (let ((words (+ words (vector-ref (cdr memo) 0))))
           (remember! words)
           (* words sa-ptr-bytes)))
        (else
         (loop (#%$continuation-link k)
               (+ words (#%$continuation-stack-clength k))))))))

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


;; (sa-current-winders) -> the chain, innermost first.
;; (sa-current-winders-set! w) -> void. Replaces it wholesale.
;;
;; A write must happen in the frame that escapes, NOT inside a guard,
;; dynamic-wind, with-mutex or parameterize: every one of those restores the
;; chain on exit and would silently undo it. See jolt-park-drop-finallys!.
(define (sa-current-winders) (#%$current-winders))
(define (sa-current-winders-set! w) (#%$current-winders w))

;; (sa-record-cas! r i old new): compare-and-swap field i (0-based) of record r,
;; answering whether it swapped. What lets a lazy cell be claimed for forcing
;; with no lock in the way and nothing allocated (seq.ss force-claimed!). A
;; system primitive here; a target whose records are vectors swaps the slot
;; under whatever makes that atomic for its threads.
;;
;; STRONG, and that is the whole of the definition below. Chez's $record-cas! is
;; one ldxr/stxr attempt on AArch64 (s/arm64.ss asm-cas): the stxr fails
;; whenever this core's exclusive monitor was cleared between the two — a
;; context switch, or another core storing into the same reservation granule,
;; which a store to the NEXT FIELD of the same record is — and the primitive
;; then answers #f with the field still holding `old`. Every caller here reads
;; #f as "somebody else got there first": compare-and-set! hands it straight to
;; the program, force-claimed!'s release leaves a claim in place on it. Under
;; load that was one refusal in a few hundred thousand on Apple silicon
;; (test/chez/cas-test.ss counts them), and one connection in ~1400 served by
;; nobody in ring-chez-adapter, whose worker took the refusal to mean another
;; owner had the conn. x86's cmpxchg cannot fail this way, which is why it was
;; only ever seen on the Mac. So: a #f is taken at its word only once the field
;; is seen NOT holding `old`; while it still does, the attempt is repeated.
;; The re-read is the plain field read the failure path already paid for, and
;; the loop ends the instant either the swap lands or another writer moves the
;; field.
(define (sa-record-cas! r i old new)
  (let retry ()
    (cond ((#%$record-cas! r i old new) #t)
          ((eq? (#%$record-ref r i) old) (retry))
          (else #f))))
;; (sa-box-cas! b old new) -> the same strong compare-and-swap on a box: box-cas!
;; refuses spuriously on Apple silicon just as $record-cas! does, so a one-shot
;; claim built on it can be lost by every claimant.
(define (sa-box-cas! b old new)
  (let retry ()
    (cond ((box-cas! b old new) #t)
          ((eq? (unbox b) old) (retry))
          (else #f))))

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

;; (sa-collect-safe-pause) -> unspecified
;; A __collect_safe call that returns at once: sched_yield, or SwitchToThread on
;; Windows. A collect-safe call deactivates the thread whatever its interrupt
;; disable count, which a blocking mutex-acquire, condition-wait or sleep does
;; not, so locks.ss waits in this between tries of a mutex it cannot block on.
;; Contract: return promptly and let a collection run meanwhile. Degradation: a
;; target with no such entry, or whose collector never stops other threads,
;; may answer a plain no-op.
;; Windows builds it at run time: a compiled foreign reference there is a load
;; relocation that aborts the boot when the entry is missing, before any guard
;; runs (see sa-foreign-procedure-runtime), which is how a literal sched_yield
;; stopped every Windows binary from starting.
(define sa-collect-safe-pause
  (or (guard (e (#t #f))
        (sa-load-shared-object #f)
        (if (eq? (sa-os-family) 'windows)
            (and (sa-foreign-entry? "SwitchToThread")
                 (sa-foreign-procedure-runtime "SwitchToThread" '() 'int #t))
            (and (sa-foreign-entry? "sched_yield")
                 (foreign-procedure __collect_safe "sched_yield" () int))))
      (lambda () 0)))


;; ---- handler-backed output ports ---------------------------------------------
;; (sa-make-output-port name put! put-some! flush! clear! close!) -> port | #f
;; A textual output port with no buffer of its own: every character written to
;; it reaches PUT! (char) or PUT-SOME! (string start count -> count), and
;; flush-output-port / clear-output-port / close-port reach FLUSH!, CLEAR! and
;; CLOSE!. R6RS custom ports cannot be this: they buffer, and their flush only
;; drains that buffer into write!, so a port that owns a buffer further down
;; never hears about the flush. It is how Chez builds its own console ports
;; (s/io.ss thread-safe-console-output-port), from a port-handler record whose
;; type is read off an existing port, since the constructor is not exported.
;; Contract: the port described. Degradation: #f, on a Chez whose port-handler
;; record does not have the fields compared here (10.4 and 10.5 do), or any
;; target without handler-backed ports; the caller keeps the ports it has.
(define sa-port-handler-fields
  '#(ready? lookahead unget get get-some clear-input put put-some flush
     clear-output close-port port-position set-port-position! port-length
     set-port-length! port-nonblocking? set-port-nonblocking!))
(define sa-port-handler-maker
  (let ((memo 'unresolved))
    (lambda ()
      (when (eq? memo 'unresolved)
        (set! memo
          (guard (e (#t #f))
            (let ((rtd (#%$record-type-descriptor (#%$port-handler (current-output-port)))))
              (and (equal? (record-type-field-names rtd) sa-port-handler-fields)
                   (record-constructor (make-record-constructor-descriptor rtd #f #f)))))))
      memo)))
(define (sa-make-output-port name put! put-some! flush! clear! close!)
  (let ((mk (sa-port-handler-maker)))
    (and mk
         (#%$make-textual-output-port name
           (mk #f #f #f #f #f #f
               (lambda (who p c) (put! c))
               (lambda (who p s start count) (put-some! s start count))
               (lambda (who p) (flush!))
               (lambda (who p) (clear!))
               (lambda (who p) (close!))
               #f #f #f #f #f #f)
           ""))))

;; ---- terminal tier (capability: terminal) -----------------------------------
;; The interactive REPL's line editor drives the console through the terminal
;; layer of Chez's own expression editor: the kernel's "(cs)ee_*" entries, a
;; terminfo implementation on Unix and a console-API one on Windows, where key
;; presses come back as the same ANSI escape sequences a Unix terminal sends.
;; Cursor motion and clearing go through them rather than through escape
;; sequences written to stdout, which is what makes the editor portable to the
;; Windows console.
;;
;; A kernel configured --disable-curses has no expeditor and none of these
;; entries, so they are looked up when the terminal is first claimed, never at
;; load: sa-term-open answers #f there and the REPL keeps its plain reader.
;; ee_read_char deactivates the calling thread while it blocks, so a collection
;; or another thread's work proceeds while the editor waits for a key.
(define sa-term-procs #f)  ; vector of the entry points once claimed, else #f

(define (sa-term-proc i)
  (if sa-term-procs
      (vector-ref sa-term-procs i)
      (error 'sa-term "the terminal is not open")))

(define (sa-term-open)
  (or (and sa-term-procs #t)
      (and (foreign-entry? "(cs)ee_init_term")
           ((foreign-procedure "(cs)ee_init_term" (iptr iptr) boolean) -1 -1)
           (begin
             (set! sa-term-procs
               (vector
                 (foreign-procedure "(cs)ee_raw" () void)                       ; 0
                 (foreign-procedure "(cs)ee_noraw" () void)                     ; 1
                 (foreign-procedure "(cs)ee_read_char" (boolean) scheme-object) ; 2
                 (foreign-procedure "(cs)ee_get_screen_size" () scheme-object)  ; 3
                 (foreign-procedure "(cs)ee_write_char" (wchar_t) void)         ; 4
                 (foreign-procedure "(cs)ee_char_width" (wchar_t) int)          ; 5
                 (foreign-procedure "(cs)ee_flush" () void)                     ; 6
                 (foreign-procedure "(cs)ee_up" (integer-32) void)              ; 7
                 (foreign-procedure "(cs)ee_down" (integer-32) void)            ; 8
                 (foreign-procedure "(cs)ee_left" (integer-32) void)            ; 9
                 (foreign-procedure "(cs)ee_right" (integer-32) void)           ; 10
                 (foreign-procedure "(cs)ee_clr_eol" () void)                   ; 11
                 (foreign-procedure "(cs)ee_clr_eos" () void)                   ; 12
                 (foreign-procedure "(cs)ee_clear_screen" () void)              ; 13
                 (foreign-procedure "(cs)ee_carriage_return" () void)           ; 14
                 (foreign-procedure "(cs)ee_line_feed" () void)                 ; 15
                 (foreign-procedure "(cs)ee_bell" () void)                      ; 16
                 (foreign-procedure "(cs)ee_pause" () void)                     ; 17
                 (foreign-procedure "(cs)ee_set_color" (int boolean) void)))    ; 18
             #t))))

(define (sa-term-raw!) ((sa-term-proc 0)))
(define (sa-term-cooked!) ((sa-term-proc 1)))

;; A char, the eof object, or 'resize when the window changed size; without
;; block?, #f when no input is waiting.
(define (sa-term-read-char block?)
  (let ((c ((sa-term-proc 2) block?)))
    (if (eq? c #t) 'resize c)))

(define (sa-term-size) ((sa-term-proc 3)))
(define (sa-term-write-char c) ((sa-term-proc 4) c))
(define (sa-term-char-width c) ((sa-term-proc 5) c))
(define (sa-term-flush) ((sa-term-proc 6)))

(define (sa-term-move! dir n)
  (when (> n 0)
    ((sa-term-proc (case dir ((up) 7) ((down) 8) ((left) 9) ((right) 10)
                     (else (error 'sa-term-move! "unknown direction" dir))))
     n)))

(define (sa-term-clear! what)
  ((sa-term-proc (case what ((eol) 11) ((eos) 12) ((screen) 13)
                   (else (error 'sa-term-clear! "unknown region" what))))))

(define (sa-term-cr!) ((sa-term-proc 14)))
(define (sa-term-lf!) ((sa-term-proc 15)))
(define (sa-term-bell!) ((sa-term-proc 16)))
(define (sa-term-pause!) ((sa-term-proc 17)))

;; Color ids are the expeditor's: -1 resets, 0-7 the base colors (black red
;; green yellow blue magenta cyan white), 8-15 their bright forms.
(define (sa-term-color! id background?) ((sa-term-proc 18) id background?))


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
(define-syntax sa-ustring-ref (syntax-rules () ((_ s i) (#3%string-ref s i))))
(define-syntax sa-uflvector-ref (syntax-rules () ((_ v i) (#3%flvector-ref v i))))
(define-syntax sa-uflvector-set! (syntax-rules () ((_ v i x) (#3%flvector-set! v i x))))
(define-syntax sa-ubytevector-ieee-single-native-ref
  (syntax-rules () ((_ bv at) (#3%bytevector-ieee-single-native-ref bv at))))
(define-syntax sa-ubytevector-ieee-single-native-set!
  (syntax-rules () ((_ bv at x) (#3%bytevector-ieee-single-native-set! bv at x))))
(define-syntax sa-ubytevector-ieee-double-native-ref
  (syntax-rules () ((_ bv at) (#3%bytevector-ieee-double-native-ref bv at))))
(define-syntax sa-ubytevector-ieee-double-native-set!
  (syntax-rules () ((_ bv at x) (#3%bytevector-ieee-double-native-set! bv at x))))
;; (sa-vector-copy-range! to at from start end): the R7RS shape over Chez's
;; (vector-copy! from from-start to to-start count).
(define (sa-vector-copy-range! to at from start end)
  (vector-copy! from start to at (fx- end start)))
;; (sa-vector-copy v): a fresh copy of v. (sa-subvector v start end): a fresh
;; copy of v[start, end), over Chez's (vector-copy v start count). The #%
;; forms name the primitives themselves: the vendored irregex redefines
;; vector-copy at top level as a one-argument loop, and in an app's runtime that
;; definition is what a bare vector-copy reaches.
(define (sa-vector-copy v) (#%vector-copy v))
(define (sa-subvector v start end)
  (#%vector-copy v start (fx- end start)))
(define sa-vector-append #%vector-append)
;; (sa-string-copy-range! to at from start end): the same reorder over Chez's
;; (string-copy! from from-start to to-start count).
(define (sa-string-copy-range! to at from start end)
  (string-copy! from start to at (fx- end start)))
;; (sa-bytevector-copy-range! to at from start end): the same reorder over Chez's
;; R6RS (bytevector-copy! from from-start to to-start count).
(define (sa-bytevector-copy-range! to at from start end)
  (bytevector-copy! from start to at (fx- end start)))

;; Every Chez mutex the runtime allocates is counted. A mutex is a finalized
;; object — the collector visits each one — so how many a workload allocates is
;; a cost in itself, and the lazy-seq scaling gate (test/lazyseq_mt_scaling_test.clj)
;; asserts on this count: realizing lazy cells must allocate none per cell, before
;; or after a thread has existed. Read through jolt.host/mutex-allocations.
;; #%make-mutex names Chez's primitive outright: the devboot and the binary are
;; compiled as ONE program, where a plain `make-mutex` here would be this file's
;; own definition below and the wrapper would call itself. Here and not in
;; locks.ss because naming a primitive that way is the Chez target's business.
(define %chez-make-mutex #%make-mutex)
(define jolt-mutex-allocations (box 0))
(define (count-mutex-allocation!)
  (let retry ()
    (let ((n (unbox jolt-mutex-allocations)))
      (unless (box-cas! jolt-mutex-allocations n (fx+ n 1)) (retry)))))
(define make-mutex
  (case-lambda
    (() (count-mutex-allocation!) (%chez-make-mutex))
    ((name) (count-mutex-allocation!) (%chez-make-mutex name))))

;; locks.ss first: fibers.ss uses the counting lock wrapper, and jolt-with-mutex
;; is a macro, so it must be defined before this load rather than captured at
;; run time the way the sa-* seams are.
(load "host/chez/locks.ss")
(load "host/chez/fibers.ss")

;; (sa-thread-id-of t) -> fixnum | #f
;; The numeric id a thread OBJECT's own get-thread-id answers, read from any
;; thread: its context's thread number. Contract: the id, or #f once the
;; thread has exited (its context is released and the object marks it dead).
;; Degradation: #f — the caller then records nothing for the thread.
;;
;; Both reads happen under Chez's thread-context mutex, interrupts off, as its own
;; with-tc-mutex does. An exiting thread frees its context and only then marks
;; the object dead, both under that mutex, and a new thread's context is malloc'd
;; and copied from its parent's under it too. Read without it, a child that had
;; just finished could hand back a freed context, and the threadno read from that
;; memory was whatever was there now: often 0, the boot thread's number, copied
;; in for a thread the boot thread was creating. Thread.start then filed the
;; child's object under the boot thread, whose Thread/currentThread became the
;; child. Defined after locks.ss, since jolt-with-mutex is a macro.
(define (sa-thread-id-of t)
  (guard (e (#t #f))
    (dynamic-wind
      disable-interrupts
      (lambda ()
        (jolt-with-mutex #%$tc-mutex
          (let ((tc (#%$thread-tc t)))
            (and (not (eqv? tc 0))
                 (let ((n (#%$tc-field 'threadno tc)))
                   (and (fixnum? n) n))))))
      enable-interrupts)))

;; (sa-pin-for-owner! owner obj) -> void
;; (sa-unpin-for-owner! owner) -> void
;; Keep OBJ where it is -- marked in place by every collection, never copied --
;; for as long as OWNER is reachable, or until sa-unpin-for-owner! (which also
;; runs when OWNER pins something else: an owner holds at most one pin). Then OBJ
;; is an ordinary object again, reclaimed once nothing else holds it. It is the
;; large-array backings that have no immobile constructor, the fxvector and
;; flvector a long or double array lives in (rt natives-array.ss), hazarded
;; exactly as a byte array was before sa-make-large-bytevector: a mobile backing
;; in a mostly free chunk is copied at every full collection (#1227).
;;
;; A locked object is a root, so a guardian cannot watch OBJ itself: it watches
;; OWNER, and the box it hands back is what says OBJ is still locked. Every lock
;; is undone exactly once -- Chez's unlock-object decrements the segment's pin
;; count without checking the object was locked, so a second unlock would unpin
;; whatever else shares the segment. Dead owners are drained on each pin and after
;; each collection (sa-gc-install-after-collect!'s handler). The handler only
;; TRIES the mutex: a thread parked at the collection rendezvous may hold it, and
;; waiting there would deadlock; the next pin or collection drains instead.
;; Contract: OBJ is not relocated while pinned, and nothing pinned outlives its
;; owner by more than a collection. Degradation: both no-op (a collector that
;; does not move such objects, or one that cannot be asked not to).
(define sa-pin-guardian (make-guardian))
(define sa-pin-boxes (make-weak-eq-hashtable))
(define sa-pin-mutex (make-mutex))
;; under sa-pin-mutex
(define (sa-pin-release! b)
  (when (unbox b)
    (unlock-object (unbox b))
    (set-box! b #f)))
(define (sa-pin-drain-locked!)
  (let loop ()
    (let ((b (sa-pin-guardian)))
      (when b (sa-pin-release! b) (loop)))))
(define (sa-pin-for-owner! owner obj)
  (jolt-with-mutex sa-pin-mutex
    (sa-pin-drain-locked!)
    (let ((old (hashtable-ref sa-pin-boxes owner #f)))
      (when old (sa-pin-release! old)))
    (lock-object obj)
    (let ((b (box obj)))
      (hashtable-set! sa-pin-boxes owner b)
      (sa-pin-guardian owner b))))
(define (sa-unpin-for-owner! owner)
  (jolt-with-mutex sa-pin-mutex
    (let ((b (hashtable-ref sa-pin-boxes owner #f)))
      (when b
        (sa-pin-release! b)
        (hashtable-delete! sa-pin-boxes owner)))))
(define (sa-pin-drain-after-collect!)
  (when (jolt-lock! sa-pin-mutex #f)
    (dynamic-wind
      void
      sa-pin-drain-locked!
      (lambda () (jolt-unlock! sa-pin-mutex)))))

