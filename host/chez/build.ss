;; build.ss — `jolt build`: AOT-compile an app into a standalone executable.
;;
;; Loaded on demand by cli.ss when the command is `build`. Defines the host
;; primitive jolt.host/build-binary, which jolt.main's build command calls after
;; resolving the project's deps + source roots.
;;
;; The pipeline (Phase 4 stage 2):
;;   1. load the entry namespace — registers its macros/vars and follows requires,
;;      recording the app namespaces in dependency order (loader's ns-loaded-hook).
;;   2. re-emit each app namespace to Scheme (the emit-image cross-compile path),
;;      now that its macros are registered.
;;   3. textually inline the cli.ss runtime load sequence into one flat source,
;;      append the app emission + a launcher that calls the entry's -main.
;;   4. compile-file -> make-boot-file -> embed the boot as C bytes -> cc-link
;;      against libkernel.a into a single self-contained binary.
;;
;; emit-image.ss supplies the cross-compiler (ei-* helpers); it's loaded here so a
;; normal run never pays for it.

(load "host/chez/emit-image.ss")
(load "host/chez/dce.ss")

;; --- shell helpers ----------------------------------------------------------
;; Run a command, return its stdout as one trimmed string ("" on no output).
(define (bld-sh-capture cmd)
  (let* ((p (process (bld-sh-wrap cmd))) (in (car p)))
    (let loop ((acc '()))
      (let ((l (get-line in)))
        (if (eof-object? l)
            (begin (close-port in)
                   ;; rejoin with newlines (get-line stripped them). Callers use
                   ;; single-line output; this just avoids silently concatenating
                   ;; two lines into one corrupt token if a command emits more.
                   (let ((ls (reverse acc)))
                     (if (null? ls) ""
                         (fold-left (lambda (s x) (string-append s "\n" x)) (car ls) (cdr ls)))))
            (loop (cons l acc)))))))

(define (bld-system cmd)
  (let ((rc (system (bld-sh-wrap cmd))))
    (unless (zero? rc)
      (error 'jolt-build (string-append "command failed (" (number->string rc) "): " cmd)))))

;; mkdir -p without a subprocess (the self-contained build shells out to nothing).
(define (bld-mkdir-p dir)
  (unless (or (string=? dir "") (string=? dir "/") (string=? dir ".") (file-exists? dir))
    (bld-mkdir-p (path-parent dir))
    ;; tolerate only the benign race (someone else created it) — a real mkdir
    ;; failure (permissions) used to surface later as a less specific
    ;; open-output-file error.
    (guard (e (#t (unless (file-exists? dir) (raise e))))
      (mkdir dir))))

(define (bld-contains? s sub)
  (let ((ns (string-length s)) (nsub (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i nsub) ns) #f)
            ((string=? (substring s i (+ i nsub)) sub) #t)
            (else (loop (+ i 1)))))))

;; Shell-quote a path: wrap in single quotes. Paths in this project are assumed
;; to not contain single quotes (which would break the quoting).
(define (bld-sh-quote s)
  (string-append "'" s "'"))

;; --- toolchain discovery ----------------------------------------------------
;; bld-machine / bld-osx? / bld-nt? describe the HOST — the machine the build
;; RUNS on (shell wrapping, cc discovery, loading a native archive into the build
;; process). Where a decision is about the OUTPUT binary instead (link libs, the
;; boots, the .exe/.dylib suffix, symbol export), use the target-aware predicates
;; below so `jolt build --target <machine>` cross-compiles correctly.
(define bld-machine (sa-host-tag))
(define bld-osx? (eq? (sa-os-family) 'macos))
(define bld-nt? (eq? (sa-os-family) 'windows))

;; The target machine: #f = build for the host; a Chez machine string
;; ("ta6osx", "tarm64le", "ta6nt", …) = cross-compile. Set by jolt build --target.
(define bld-target (make-parameter #f))
;; A prepared target pack: a directory holding the target's petite.boot,
;; scheme.boot, libkernel.a and scheme.h (the csv layout), the cross xpatch, a
;; `link-libs` file with the target link flags, and static lz4/zlib under lib/.
;; Required when bld-target is set. Produced by the one-time ChezScheme cross
;; setup — see tools/cross-compile/README.md.
(define bld-target-pack (make-parameter #f))
;; The effective target machine, and whether this is a cross build.
(define (bld-eff-machine) (or (bld-target) bld-machine))
(define (bld-cross?) (and (bld-target) (not (string=? (bld-target) bld-machine)) #t))
(define (bld-tgt-osx?) (bld-contains? (bld-eff-machine) "osx"))
(define (bld-tgt-nt?) (bld-contains? (bld-eff-machine) "nt"))
;; The C compiler + arch flag for the OUTPUT binary. Cross overrides via env
;; (JOLT_TARGET_CC, e.g. aarch64-linux-gnu-gcc or a zig-cc wrapper;
;; JOLT_TARGET_ARCH_FLAG, e.g. "-arch x86_64" for a macOS x-arch link).
(define (bld-cc) (if (bld-cross?) (or (getenv "JOLT_TARGET_CC") "cc") "cc"))
(define (bld-arch-flag) (if (bld-cross?) (or (getenv "JOLT_TARGET_ARCH_FLAG") "") ""))

;; Platform-appropriate flag to export executable symbols so a statically-linked
;; native lib's symbols resolve via (load-shared-object #f). macOS keeps unstripped
;; dlsym visibility; Windows needs an explicit export table; ELF (Linux) needs -rdynamic.
(define (bld-export-symbols-flag)
  (cond (bld-osx? "")
        (bld-nt? "-Wl,--export-all-symbols ")
        (else "-rdynamic ")))

;; Chez's system/process run through cmd.exe on Windows; every build command
;; here is written for sh (MSYS2 provides it). On nt, spill the command to a
;; script and run `sh <file>` — workspace paths carry no spaces, and the
;; script file sidesteps cmd's quoting entirely. Identity elsewhere.
(define bld-shell-counter 0)
;; On nt, spill the command to a script and run `sh <file>`. Chez has no getpid,
;; so per-process uniqueness comes from first-use millis + a counter (the same
;; scheme spit's temp files use) — concurrent builds sharing TEMP don't collide.
;; The stamp resolves lazily: jolt bakes this file into a saved heap, and a
;; load-time stamp would freeze identical across every process run from it.
;; Delete the script on success; leave it on failure for debugging.
(define bld-shell-stamp #f)
(define (bld-sh-wrap cmd)
  (if bld-nt?
      (let* ((stamp (or bld-shell-stamp
                        (let ((s (number->string (sa-real-time-ms))))
                          (set! bld-shell-stamp s) s)))
             (tmp (or (getenv "TEMP") (getenv "TMP") "."))
             (f (begin (set! bld-shell-counter (+ bld-shell-counter 1))
                       (string-append tmp "\\jolt-sh-" stamp "-"
                                      (number->string bld-shell-counter) ".sh"))))
        (let ((p (open-output-file f 'replace)))
          (put-string p cmd)
          (close-port p))
        (let ((qf (string-append "'"
                    (apply string-append
                      (map (lambda (c) (if (char=? c #\') "'\\''" (string c)))
                           (string->list f)))
                    "'")))
          (string-append "sh " qf " && rm -f " qf)))
      cmd))

;; The Chez executable, for the isolated compile pass (see build-binary step 4).
(define bld-chez
  (let ((p (bld-sh-capture "command -v chez || command -v scheme || command -v petite")))
    (if (> (string-length p) 0) p "chez")))

;; Chez version off (scheme-version) "Chez Scheme Version X.Y.Z" — last token.
(define bld-version
  (let* ((s (scheme-version)) (n (string-length s)))
    (let loop ((i n))
      (if (or (= i 0) (char=? (string-ref s (- i 1)) #\space))
          (substring s i n)
          (loop (- i 1))))))

;; The HOST csv<ver>/<machine> dir holding scheme.h, libkernel.a, *.boot. Derived
;; from the chez executable's location; JOLT_CHEZ_CSV overrides.
(define bld-host-csv-dir
  (let ((env (getenv "JOLT_CHEZ_CSV")))
    (or (and env (> (string-length env) 0) env)
        (let* ((bindir (bld-sh-capture "dirname \"$(command -v chez || command -v scheme || command -v petite)\""))
               (cand (string-append bindir "/../lib/csv" bld-version "/" bld-machine)))
          cand))))
;; The csv dir supplying the boots + kernel + scheme.h that get baked into the
;; OUTPUT binary: the target pack when cross-compiling, else the host csv. (For a
;; cross build the host chez still runs the compile, finding its own boots via its
;; install; only make-boot-file / the kernel link read the target's, via the pack.)
(define (bld-csv-dir) (if (bld-cross?) (bld-target-pack) bld-host-csv-dir))
;; The cross xpatch that retargets compile-file / make-boot-file to the target.
(define (bld-xpatch) (string-append (bld-target-pack) "/xpatch"))

(define (bld-have-cc?)
  (> (string-length (bld-sh-capture "command -v cc")) 0))

(define (bld-check-toolchain)
  (let ((hint (if (bld-cross?)
                  "\nProvide a target pack (--target-pack DIR) — see tools/cross-compile/README.md."
                  "\nSet JOLT_CHEZ_CSV to the csv<ver>/<machine> dir.")))
    (for-each
      (lambda (f)
        (let ((p (string-append (bld-csv-dir) "/" f)))
          (unless (file-exists? p)
            (error 'jolt-build (string-append "Chez build file missing: " p hint)))))
      '("scheme.h" "libkernel.a" "petite.boot" "scheme.boot"))
    ;; a cross pack additionally supplies the xpatch and the target link flags.
    (when (bld-cross?)
      (unless (bld-target-pack)
        (error 'jolt-build "cross build (--target) needs a target pack (--target-pack DIR)"))
      (for-each
        (lambda (f)
          (let ((p (string-append (bld-target-pack) "/" f)))
            (unless (file-exists? p)
              (error 'jolt-build (string-append "target pack file missing: " p hint)))))
        '("xpatch" "link-libs")))))

;; Link flags. macOS Homebrew layout for the kernel's lz4/zlib/ncurses deps. The
;; host branches double as the target flags for a non-cross build (host = target).
(define (bld-link-libs)
  (cond
    ;; cross: the static lz4/zlib live in the pack (lib/), and the pack's
    ;; `link-libs` file lists the remaining -l/-framework flags — which depend on
    ;; how its kernel was configured (a cross kernel is often --disable-curses /
    ;; --disable-x11). JOLT_TARGET_LINK_LIBS overrides the whole string.
    ((bld-cross?)
     (or (getenv "JOLT_TARGET_LINK_LIBS")
         (string-append "-L" (bld-sh-quote (string-append (bld-target-pack) "/lib")) " "
           (bld-sh-capture (string-append "cat " (bld-sh-quote (string-append (bld-target-pack) "/link-libs")))))))
    (bld-osx?
     (let ((lz4 (bld-sh-capture "brew --prefix lz4 2>/dev/null")))
       (if (> (string-length lz4) 0)
           (string-append "-L" lz4 "/lib -llz4 -lz -lncurses -framework Foundation -liconv -lm")
           (let ((pc (bld-sh-capture "pkg-config --libs-only-L liblz4 2>/dev/null")))
             (if (> (string-length pc) 0)
                 (string-append pc " -llz4 -lz -lncurses -framework Foundation -liconv -lm")
                 (begin
                   (display "jolt build: warning: lz4 library path not found via brew or pkg-config")
                   (display " — linker may not find -llz4\n")
                   "-llz4 -lz -lncurses -framework Foundation -liconv -lm"))))))
    ;; Windows (ta6nt, MinGW-w64 under MSYS2): the Chez kernel pulls in
    ;; compression, winsock, COM/UUID, and the registry.
    (bld-nt?
          ;; -static: a single-file exe (no libwinpthread/libgcc/lz4 DLL deps) —
     ;; required for a distributable binary and for TLS init consistency.
     "-static -llz4 -lz -lws2_32 -lrpcrt4 -lole32 -luuid -ladvapi32 -luser32 -lshell32 -lm")
    ;; Linux: the Chez kernel pulls in compression (lz4/z), the expression
    ;; editor (ncurses + terminfo), threads, dlopen, libuuid, and clock_gettime.
    (else "-llz4 -lz -lncurses -ltinfo -ldl -lm -lpthread -luuid -lrt")))

;; --- optional built-binary startup profile ----------------------------------
;; JOLT_STARTUP_PROFILE=1 reports wall time, process CPU, collections,
;; reclaimed GC bytes, and current heap size at coarse runtime/app boundaries.
;; The definitions use Chez primitives only, so they can run before Jolt's runtime is initialized.
;; Calls stay in every built image but return immediately when the variable is
;; absent; this keeps one binary usable for normal runs and startup diagnosis.
(define (bld-emit-startup-profile-preamble out)
  (put-string out
    "(define jolt-startup-profile? (and (getenv \"JOLT_STARTUP_PROFILE\") #t))\n\
(define jolt-startup-profile-start-real\n\
  (and jolt-startup-profile? (real-time)))\n\
(define jolt-startup-profile-last-real jolt-startup-profile-start-real)\n\
(define jolt-startup-profile-last-cpu\n\
  (and jolt-startup-profile? (cpu-time)))\n\
(define jolt-startup-profile-initial-stats\n\
  (and jolt-startup-profile? (statistics)))\n\
(define jolt-startup-profile-last-collections\n\
  (and jolt-startup-profile?\n\
       (sstats-gc-count jolt-startup-profile-initial-stats)))\n\
(define jolt-startup-profile-last-gc-bytes\n\
  (and jolt-startup-profile?\n\
       (sstats-gc-bytes jolt-startup-profile-initial-stats)))\n\
(define (jolt-startup-profile-mark! label)\n\
  (when jolt-startup-profile?\n\
    (let* ((now-real (real-time))\n\
           (now-cpu (cpu-time))\n\
           (now-stats (statistics))\n\
           (now-collections (sstats-gc-count now-stats))\n\
           (now-gc-bytes (sstats-gc-bytes now-stats)))\n\
      (display\n\
        (string-append\n\
          \"jolt startup: [profile] scheme \" label\n\
          \"   wall \" (number->string (- now-real jolt-startup-profile-last-real)) \" ms\"\n\
          \"   cpu \" (number->string (- now-cpu jolt-startup-profile-last-cpu)) \" ms\"\n\
          \"   gc \" (number->string (- now-collections jolt-startup-profile-last-collections))\n\
          \"   gc-reclaimed \" (number->string (- now-gc-bytes jolt-startup-profile-last-gc-bytes)) \" bytes\"\n\
          \"   heap \" (number->string (current-memory-bytes)) \" bytes\"\n\
          \"   (cumulative \" (number->string (- now-real jolt-startup-profile-start-real)) \" ms)\\n\")\n\
        (current-error-port))\n\
      (let ((after-stats (statistics)))\n\
        (set! jolt-startup-profile-last-real (real-time))\n\
        (set! jolt-startup-profile-last-cpu (cpu-time))\n\
        (set! jolt-startup-profile-last-collections (sstats-gc-count after-stats))\n\
        (set! jolt-startup-profile-last-gc-bytes (sstats-gc-bytes after-stats))))))\n"))

(define (bld-startup-profile-form label)
  (string-append "(jolt-startup-profile-mark! " (ei-str-lit label) ")"))

(define (bld-emit-startup-profile-mark! out label)
  (put-string out (bld-startup-profile-form label))
  (put-string out "\n"))

(define (bld-runtime-entry-label entry)
  (cond
    ((symbol? entry) (symbol->string entry))
    ((bld-load-path entry) => (lambda (path) path))
    (else entry)))

;; --- runtime manifest (mirrors host/chez/cli.ss's load order) ---------------
;; A line is either literal Scheme text to inline, or a tag whose emission the build
;; controls: 'prelude (the clojure.core blob, replaced by the shaken core under
;; tree-shake), 'image + 'compile-eval (the compiler, dropped for a no-eval app).
;; Tagging keeps the splice/drop decisions off fragile substring matching.
(define bld-runtime-manifest
  (list
    ;; The runtime adapter loads FIRST: its sa-* entry points are referenced at
    ;; top level and inside macros during rt.ss's own load (rt.ss:51, and the
    ;; java/*.ss files rt.ss loads), so a later slot would resolve them
    ;; unbound. PSL R5+R6 pinned this order.
    "(load \"host/chez/scheme-adapter-runtime.ss\")"
    "(load \"host/chez/rt.ss\")"
    "(set-chez-ns! \"clojure.core\")"
    'prelude
    "(load \"host/chez/post-prelude.ss\")"
    "(load \"host/chez/post-prelude-str.ss\")"
    "(set-chez-ns! \"user\")"
    "(load \"host/chez/host-contract.ss\")"
    'image
    'compile-eval
    "(load \"host/chez/cli-core.ss\")"
    "(load \"host/chez/png.ss\")"
    "(load \"host/chez/loader.ss\")"
    "(load \"host/chez/java/ffi.ss\")"
    (string-append "(set-source-roots! " (ldr-install-roots-str) ")")))

(define bld-tagged-loads
  '((prelude . "(load \"host/chez/seed/prelude.ss\")")
    (image . "(load \"host/chez/seed/image.ss\")")
    (compile-eval . "(load \"host/chez/compile-eval.ss\")")))

;; A single-line top-level `(load "PATH")` -> PATH, else #f. Only STRING-LITERAL
;; loads count (a `(load so)` runtime form must not be mistaken for a manifest
;; directive — it once tripped this into an error branch referencing an unbound
;; `die`). Bounded: each quote scan checks end-of-string; a missing close quote
;; yields #f (line not a recognized directive) rather than a crash.
(define (bld-load-path line)
  (let ((s (let trim ((i 0) (n (string-length line)))
             (if (and (< i n) (memv (string-ref line i) '(#\space #\tab)))
                 (trim (+ i 1) n)
                 (if (< i n) (substring line i n) "")))))
    (and (>= (string-length s) 8)                 ; "(load \"" minimum
         (string=? (substring s 0 6) "(load ")
         (char=? (string-ref s 6) #\")            ; only string-literal loads
         (let ((end (string-length s)))
           (let ((q2 (let scan ((i 7))
                       (if (>= i end) #f
                           (if (char=? (string-ref s i) #\") i (scan (+ i 1)))))))
             (and q2 (substring s 7 q2)))))))

;; runtime source for PATH: from the binary's embedded store if present (a
;; self-contained jolt building an app, with no jolt checkout on disk), else read
;; from disk (running from a source checkout). build-jolt embeds every runtime
;; .ss the manifest inlines, so `build` never touches the filesystem for them.
(define (bld-source-string path)
  (let ((emb (hashtable-ref embedded-resources path #f)))
    (cond ((string? emb) emb)
          ;; source embeds are UTF-8 bytevectors since the heap-size work —
          ;; missing this arm sent the standalone binary's `build` to disk for
          ;; host/chez/*.ss, which only exists inside a checkout (v0.4.0
          ;; release-smoke failure on all three platforms).
          ((bytevector? emb) (utf8->string emb))
          (else (read-file-string path)))))

(define (bld-string-lines s)
  ;; a line drops its trailing \r: a CRLF checkout (Windows git autocrlf) must
  ;; parse identically to an LF one — the stdlib-fasl manifest read through
  ;; here failed set-equality on Windows with every name carrying \r.
  (let ((n (string-length s)))
    (define (slice start end)
      (let ((end (if (and (> end start) (char=? (string-ref s (- end 1)) #\return))
                     (- end 1) end)))
        (substring s start end)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond ((>= i n) (reverse (if (> i start) (cons (slice start i) acc) acc)))
            ((char=? (string-ref s i) #\newline)
             (loop (+ i 1) (+ i 1) (cons (slice start i) acc)))
            (else (loop (+ i 1) start acc))))))

(define (bld-file-lines path) (bld-string-lines (bld-source-string path)))

;; Emit one line to OUT, recursively inlining a `(load ...)` of a repo file.
(define (bld-inline-line line out depth)
  (when (> depth 50) (error 'jolt-build "load nesting too deep"))
  (let ((p (bld-load-path line)))
    (if p
        (for-each (lambda (l) (bld-inline-line l out (+ depth 1))) (bld-file-lines p))
        (begin (put-string out line) (put-string out "\n")))))

;; Inline the runtime manifest, dispatching on the manifest tags. core-strs (the
;; shaken clojure.core defs, or #f) replaces the 'prelude blob; drop-compiler? (a
;; closed AOT app that never compiles from source) omits 'image + 'compile-eval —
;; the analyzer/back end are dead weight in the binary (~0.8MB).
(define (bld-emit-runtime out drop-compiler? core-strs)
  (bld-emit-startup-profile-preamble out)
  (bld-emit-startup-profile-mark! out "runtime begin")
  (for-each
    (lambda (entry)
      (let ((emitted?
              (cond
                ((eq? entry 'prelude)
                 (if core-strs
                     (begin
                       (for-each (lambda (s) (put-string out s) (put-string out "\n"))
                                 core-strs)
                       #t)
                     (begin
                       (bld-inline-line (cdr (assq 'prelude bld-tagged-loads)) out 0)
                       #t)))
                ((memq entry '(image compile-eval))
                 (if drop-compiler?
                     #f
                     (begin
                       (bld-inline-line (cdr (assq entry bld-tagged-loads)) out 0)
                       #t)))
                (else
                 (bld-inline-line entry out 0)
                 #t))))
        (when emitted?
          (bld-emit-startup-profile-mark!
            out
            (string-append "runtime " (bld-runtime-entry-label entry))))))
    bld-runtime-manifest))

;; --- app emission -----------------------------------------------------------
;; Re-emit one app namespace to a list of Scheme strings: run-passes (const-fold +
;; numeric-annotate in every mode; inference also in release/optimized; inline +
;; scalar-replace additionally with direct-link) and stay strict — a form that
;; fails to emit must fail the build, not vanish.
;; The loop itself is emit-image's ei-emit-ns* (optimize? #t, guard? #f).
(define (bld-emit-ns ns-name src) (ei-emit-ns* ns-name src #t #f))

;; --- whole-program inference pre-pass ---------------------------------------
;; Analyze every app form (all namespaces, deps-first) to IR and run the
;; closed-world param-type fixpoint, so each fn's param types pick up the record
;; types its callers pass. The per-ns emit below then bare-indexes field reads and
;; devirtualizes protocol calls at those sites (the back end reads the resulting
;; :hint/:devirt annotations). Optimized builds only; registries come from the
;; runtime tables populated as the app loaded.
(define jolt-wp-infer!             (var-deref "jolt.passes.types" "wp-infer!"))
(define jolt-wp-set-record-shapes! (var-deref "jolt.passes.types" "set-record-shapes!"))
(define jolt-wp-set-proto-methods! (var-deref "jolt.passes.types" "set-protocol-methods!"))
(define jolt-wp-host-record-shapes (var-deref "jolt.host" "record-shapes"))
(define jolt-wp-host-proto-methods (var-deref "jolt.host" "protocol-methods"))
(define jolt-contagion-prepass!      (var-deref "jolt.backend-scheme" "contagion-prepass!"))
(define jolt-contagion-prepass-done! (var-deref "jolt.backend-scheme" "contagion-prepass-done!"))
(define jolt-reset-clone-prepass!    (var-deref "jolt.backend-scheme" "reset-clone-prepass!"))

(define (bld-wp-infer! ordered)
  ;; the build's compilation unit (ei-unit) is created + published by the build setup
  ;; before any flag is set, so the whole-program seeds set here — and the mode flags —
  ;; land on the one unit the per-form emit reads.
  (jolt-wp-set-record-shapes! (ei-unit) (jolt-wp-host-record-shapes #f))
  (jolt-wp-set-proto-methods! (ei-unit) (jolt-wp-host-proto-methods #f))
  (let ((nodes '()) (ns-nodes '()))
    (for-each
      (lambda (nf)
        (set-chez-ns! (car nf))
        (let ((src (ei-timed "wp: read source" (lambda () (ldr-read-source (cdr nf))))) (per-ns '()))
          ;; This walk, not the emit walk, is where an --opt build's IR is
          ;; produced, so a file's top-level (set! *unchecked-math* …) has to be
          ;; in effect HERE for the analyzer to lower the following forms'
          ;; arithmetic to its wrapping variants. Bracketed per namespace, like
          ;; the loader does per file, so the flag doesn't leak into the next one.
          (dynamic-wind
            jolt-ns-load-vars-push!
            (lambda ()
          (parameterize ((rdr-source-file (cdr nf)))
            (jolt-enter-file! (cdr nf))   ; so a failure here names the file
            (for-each
              (lambda (f)
                (ce-scan-requires! f (car nf))
                (when (ei-flag-set-form? f)
                  (jolt-compile-eval-form f (car nf)))
                ;; per-ns is consumed POSITIONALLY by the emit walk
                ;; (ei-next-cached, one pop per form ei-for-each-form
                ;; dispatches). The emit walk compiles MACRO forms too, and
                ;; keeps going past a form this analysis rejects — so both get
                ;; a #f placeholder (ei-compile-form falls back to a fresh
                ;; analysis on #f). Skipping them here shifted every later
                ;; form's cached IR by one: a macro's def-var! captured the
                ;; NEXT def's emission — invalid Scheme under direct-link, a
                ;; silently corrupted expander before it. Only the ns form is
                ;; skipped by BOTH walks.
                (unless (ei-ns-form? f)
                  (if (ce-macro-form? f)
                      (set! per-ns (cons #f per-ns))
                      ;; a form the analyzer rejects here only loses
                      ;; whole-program type info (per-form emit still errors
                      ;; the build if it's truly broken) — but say so, or an
                      ;; optimized build silently loses inference for the ns.
                      (guard (e (#t (display (string-append
                                              "jolt build: note: whole-program inference skipped a form in "
                                              (car nf) "\n")
                                             (current-error-port))
                                    (set! per-ns (cons #f per-ns))))
                        (let ((n (ei-timed "wp: analyze"
                                   (lambda () (jolt-ce-analyze (make-analyze-ctx (car nf)) f)))))
                          (set! nodes (cons n nodes))
                          (set! per-ns (cons n per-ns)))))))
              (ei-timed "wp: parse" (lambda () (ei-read-all src))))))
            jolt-ns-load-vars-pop!)
          (set! ns-nodes (cons (cons (car nf) (reverse per-ns)) ns-nodes))))
      ordered)
    (ei-timed "wp: fixpoint"
      (lambda () (jolt-wp-infer! (ei-unit) (apply jolt-vector (reverse nodes)))))
    ;; contagion clone-site pre-pass: an impl worth a specialized clone is one that is
    ;; BOTH contagion-eligible (:num field beside a proven :double) AND reached by a
    ;; devirtualized call site. Run per-ns after wp-infer! (rich field types must be
    ;; live) so a devirt site can resolve the clone regardless of emit order.
    (jolt-reset-clone-prepass! (ei-unit))
    ;; drop the #f alignment placeholders — the prepass wants real IR only.
    (ei-timed "wp: contagion prepass"
      (lambda ()
        (for-each (lambda (p) (jolt-contagion-prepass! (ei-unit)
                                (apply jolt-vector (filter (lambda (n) n) (cdr p))) (car p)))
                  ns-nodes)
        (jolt-contagion-prepass-done! (ei-unit))))
    (reverse ns-nodes)))

;; Strings emitted before each app ns's forms, replaying what the source loader
;; does per file: (1) set chez-current-ns so runtime ns-sensitive setup forms
;; (defmulti/defmethod resolve their target var through it) land in the right ns;
;; (2) register the ns's :as aliases so a quoted alias resolves at runtime — a
;; (defmethod ig/foo …) passes 'ig/foo to defmethod-setup, which needs ig -> the
;; real ns, but the build strips the (ns …) form that would register it.
(define (bld-scan-spec! ns-name spec emit!)
  (let ((items (cond ((pvec? spec) (seq->list spec))
                     ((cseq? spec) (seq->list spec))
                     (else '()))))
    (when (and (pair? items) (symbol-t? (car items)))
      (let ((target (symbol-t-name (car items))))
        (let loop ((xs (cdr items)))
          (when (and (pair? xs) (pair? (cdr xs)))
            (let ((k (car xs)) (v (cadr xs)))
              (when (keyword? k)
                (cond
                  ;; :as-alias registers the alias exactly like :as; what it does
                  ;; NOT do is pull the target into the build (bld-ns-requires).
                  ((and (or (string=? (keyword-t-name k) "as")
                            (string=? (keyword-t-name k) "as-alias"))
                        (symbol-t? v))
                   (emit! (string-append "(chez-register-alias! " (ei-str-lit ns-name)
                                         " " (ei-str-lit (symbol-t-name v))
                                         " " (ei-str-lit target) ")")))
                  ;; :refer [a b] / :refer :all — a defmethod on a referred multifn
                  ;; resolves the bare name through the refer table at runtime.
                  ((or (string=? (keyword-t-name k) "refer") (string=? (keyword-t-name k) "only"))
                   (cond
                     ((and (keyword? v) (string=? (keyword-t-name v) "all"))
                      (emit! (string-append "(chez-register-refer-all! " (ei-str-lit ns-name)
                                            " " (ei-str-lit target) ")")))
                     ((or (pvec? v) (cseq? v))
                      (for-each (lambda (n)
                                  (when (symbol-t? n)
                                    (emit! (string-append "(chez-register-refer! " (ei-str-lit ns-name)
                                                          " " (ei-str-lit (symbol-t-name n))
                                                          " " (ei-str-lit target) ")"))))
                                (seq->list v))))))))
            (loop (cddr xs))))))))

(define (bld-ns-prelude ns-name src)
  (let ((acc (list (string-append "(set-chez-ns! " (ei-str-lit ns-name) ")")))
        (nsf (let loop ((fs (ei-read-all src)))
               (cond ((null? fs) #f)
                     ((ei-ns-form? (car fs)) (car fs))
                     (else (loop (cdr fs)))))))
    (when nsf
      (for-each
        (lambda (clause)
          (when (cseq? clause)
            (let ((citems (seq->list clause)))
              (when (and (pair? citems) (keyword? (car citems))
                         (let ((kn (keyword-t-name (car citems))))
                           (or (string=? kn "require") (string=? kn "use"))))
                (for-each (lambda (spec)
                            (bld-scan-spec! ns-name spec
                                            (lambda (s) (set! acc (cons s acc)))))
                          (cdr citems)))
              ;; :import must be reconstructed too: ei-for-each-form skips the
              ;; ns form entirely, so without this a built binary never runs
              ;; __import and the class-valued short-name vars (Path,
              ;; FileAttribute, …) stay unbound — late-bind used to mask that.
              (when (and (pair? citems) (keyword? (car citems))
                         (string=? (keyword-t-name (car citems)) "import"))
                (for-each
                  (lambda (spec)
                    (set! acc (cons (string-append
                                      "(chez-runtime-import (jolt-read-string "
                                      (ei-str-lit (jolt-pr-str spec)) "))")
                                    acc)))
                  (cdr citems))))))
        (seq->list nsf)))
    (reverse acc)))

;; --- AOT the CLI entry closure (jolt.main + jolt.deps) -----------------------
;; jolt.main + jolt.deps and their on-demand Clojure require closure (clojure.string,
;; clojure.edn, jolt.mvn-http, jolt.ffi, grenadine.*) must NOT be baked as top-level
;; (load-namespace …) forms in flat.ss: flat.so is a Chez boot file whose top-level
;; forms re-execute at every Sbuild_heap (every process start), so those load-namespace
;; calls re-analyze and re-emit the whole graph from Clojure source on EVERY invocation
;; — the measured ~380ms release floor, ~1.3s in the dev boot cache. Instead we emit
;; their Scheme HERE, at image-emit time, via the same emit-image path an app build
;; uses, so at boot the vars are defined by running compiled Scheme (a few ms) exactly
;; like the rest of the runtime image.
;;
;; The runtime + compiler image + clojure.core are already emitted (bld-emit-runtime,
;; which the caller ran before this). We load jolt.main + jolt.deps in THIS build
;; process to populate the compiler's registries, capturing the on-demand load order
;; via the loader's ns-loaded-hook — which fires only for namespaces NOT already in
;; the image, i.e. exactly the CLI's on-demand closure. Each ns is emitted var-routed
;; (prelude mode, direct-link OFF) so its runtime behavior is identical to the
;; interpreted load it replaces. A form that fails to emit fails the build
;; (bld-emit-ns is strict), same as an app build. jolt.deps's lazy in-fn
;; (require 'clojure.data.json) is not on the load path here, so it stays
;; load-on-demand at runtime — unchanged. Used by BOTH the release build
;; (build-jolt.ss) and the dev boot cache (make-devboot.ss): one artifact shape.
(define (bld-emit-cli-aot out)
  (put-string out "\n;; === AOT jolt.main + jolt.deps (emitted Scheme) ===\n")
  (let ((order '()))
    (set-ns-loaded-hook! (lambda (name file) (set! order (cons (cons name file) order))))
    (parameterize ((ldr-source-only? #t))    ; emit from source, never a compiled artifact
      (load-namespace "jolt.main")
      (load-namespace "jolt.deps"))
    (set-ns-loaded-hook! (lambda (name file) #f))
    (let ((ordered (reverse order)))   ; deps complete loading before requirers -> deps-first
      (when (null? ordered)
        (error 'bld-emit-cli-aot "no CLI namespace captured for jolt.main — is jolt-core on the source roots?"))
      (dynamic-wind
        (lambda ()
          (ei-fresh-unit!)
          ((var-deref "jolt.backend-scheme" "set-prelude-mode!") #t)
          (set-optimize! #t)
          (set-release! #t)
          ((var-deref "jolt.backend-scheme" "set-var-cache!") #t))
        (lambda ()
          (for-each
            (lambda (nf)
              (let ((name (car nf)) (src (ldr-read-source (cdr nf))))
                (put-string out (string-append "\n;; --- AOT " name " ---\n"))
                (parameterize ((rdr-source-file (cdr nf)))
                  (put-string out "(jolt-ns-load-vars-push!)\n")
                  (for-each (lambda (s) (put-string out s) (put-string out "\n"))
                            (bld-ns-prelude name src))
                  (for-each (lambda (s) (put-string out s) (put-string out "\n"))
                            (bld-emit-ns name src))
                  (put-string out "(jolt-ns-load-vars-pop!)\n")
                  ;; Record the ns as loaded so the runtime dispatch's
                  ;; (load-namespace "jolt.main") in cli-core.ss is a no-op — the
                  ;; defines above already installed every var. Without this the
                  ;; loader sees an unmarked ns and recompiles it from source on the
                  ;; first command that enters jolt.main/-main (run/build/version).
                  (put-string out (string-append "(ldr-mark-loaded! " (ei-str-lit name) ")\n")))))
            ordered))
        (lambda ()
          (set-optimize! #f)
          (set-release! #f)
          ((var-deref "jolt.backend-scheme" "set-var-cache!") #f)
          (ei-clear-cached!))))))

;; --- bundling: native libs + resources --------------------------------------
;; A jolt seq of jolt strings -> a Scheme list of Scheme strings.
(define (bld-strs x) (map jolt-str-render-one (seq->list x)))

;; Emit native-library loads. `natives` is the encoded jolt seq jolt.main/
;; encode-natives produced: each entry is ["process"] | ["static" form…] |
;; ["req" cand…] | ["opt" cand…]. `which` selects 'required (process + static +
;; req) or 'optional. Required loads are emitted before the app forms (the app's
;; defcfn foreign-procedures are now lazily resolved on first call, so they can
;; be emitted before the library is loaded — the binding only becomes callable
;; after the lib loads); a load-shared-object failure there is fatal — correct
;; for a required lib. A "static" lib is cc-linked into the binary (see
;; bld-native-link-flags), so its symbols are already in the process: it loads
;; them the same way a "process" lib does. Optional loads run in the scheme-start
;; launcher, where guard catches a missing lib (the defcfn's foreign-procedure is
;; only resolved when the closure is first called, so the defining form can
;; evaluate before the library is loaded).
(define (bld-emit-natives out natives which)
  (for-each
    (lambda (entry)
      (let* ((parts (bld-strs entry)) (kind (car parts)) (cands (cdr parts))
             (cand-lits (fold-left (lambda (s c) (string-append s (ei-str-lit c) " ")) "" cands)))
        (cond
          ((and (eq? which 'required) (or (string=? kind "process") (string=? kind "static")))
           (put-string out "(jolt-build-load-native '() #f #t)\n"))
          ((and (eq? which 'required) (string=? kind "req"))
           (put-string out (string-append "(jolt-build-load-native (list " cand-lits ") #f #f)\n")))
          ((and (eq? which 'optional) (string=? kind "opt"))
           (put-string out (string-append "(jolt-build-load-native (list " cand-lits ") #t #f)\n"))))))
    (seq->list natives)))

;; The cc link fragment for the "static" natives: each archive must be FORCE-loaded
;; (the linker would otherwise drop an archive member main.c never references) and,
;; on Linux, the executable's symbols exported into the dynamic table so the
;; startup (load-shared-object #f) + foreign-procedure can resolve them (-rdynamic,
;; added by build-with-cc when this fragment is non-empty). Returns "" when no lib
;; is statically linked. Entry forms: ["static" "archive" path] | ["static" "lib"
;; name libdir].
(define (bld-native-link-flags natives)
  (fold-left
    (lambda (acc entry)
      (let ((parts (bld-strs entry)))
        (if (string=? (car parts) "static")
            (string-append acc " " (bld-one-static-link (cdr parts)))
            acc)))
    "" (seq->list natives)))

;; A statically-linked native is only in the OUTPUT binary, but build step 1
;; evaluates the app's `foreign-procedure` forms in THIS process (to register its
;; macros/vars), and Chez resolves a foreign entry eagerly. So make the archive's
;; symbols resolvable here: build a throwaway shared object from it (force-loading
;; every member) and load it. The output binary still cc-links the static archive;
;; this temp .so is build-time only. Only the "archive" form is preloaded — the
;; "lib" form names a system library the OS loader already finds by soname.
(define (bld-preload-static-natives! natives builddir)
  (let ((n 0))
    (for-each
      (lambda (entry)
        (let ((parts (bld-strs entry)))
          (when (and (string=? (car parts) "static") (string=? (cadr parts) "archive"))
            (let* ((archive (caddr parts))
                   (so (string-append builddir "/native-" (number->string n)
                                      (if bld-osx? ".dylib" ".so"))))
              (set! n (+ n 1))
              (bld-system
                (if bld-osx?
                    (string-append "cc -dynamiclib -undefined dynamic_lookup -Wl,-all_load '"
                                   archive "' -o '" so "'")
                    (string-append "cc -shared -Wl,--whole-archive '" archive
                                   "' -Wl,--no-whole-archive -Wl,--unresolved-symbols=ignore-all -o '" so "'")))
              (sa-load-shared-object so)))))
      (seq->list natives))))

(define (bld-one-static-link form)
  (let ((kind (car form)))
    (cond
      ((string=? kind "archive")
       (let ((path (cadr form)))
         (if bld-osx?
             (string-append "-Wl,-force_load," (bld-sh-quote path))
             (string-append "-Wl,--whole-archive " (bld-sh-quote path) " -Wl,--no-whole-archive"))))
      ((string=? kind "lib")
       (let* ((lib (cadr form)) (dir (caddr form))
              (L (if (> (string-length dir) 0) (string-append "-L" dir " ") "")))
         ;; -Bstatic forces the .a over a .so of the same -l name (GNU ld). macOS's
         ;; ld64 has no -Bstatic; there an :archive path is the reliable form.
         (if bld-osx?
             (string-append (if (> (string-length dir) 0) (string-append "-L" (bld-sh-quote dir) " ") "") "-l" lib)
             (string-append (if (> (string-length dir) 0) (string-append "-L" (bld-sh-quote dir) " ") "") "-Wl,-Bstatic -l" lib " -Wl,-Bdynamic"))))
      (else ""))))

;; Walk an embed root recursively; return (resource-name . abspath) pairs, where
;; resource-name is the "/"-joined path under the root (what io/resource is asked for).
(define (bld-walk-files root rel acc)
  (let ((dir (if (string=? rel "") root (string-append root "/" rel))))
    (fold-left
      (lambda (acc name)
        (let* ((relpath (if (string=? rel "") name (string-append rel "/" name)))
               (full (string-append root "/" relpath)))
          (if (file-directory? full)
              (bld-walk-files root relpath acc)
              (cons (cons relpath full) acc))))
      acc
      (directory-list dir))))

;; Emit register-embedded-resource! per file under each embed dir. Emitted BEFORE
;; the app forms. File contents are read at BUILD time and emitted as bytevector
;; literals (1B/char) — flat.ss top-level forms run at every startup with no source
;; on disk, so read-file-string at runtime would fail.
(define (bld-emit-embeds out embed-dirs)
  (for-each
    (lambda (root)
      (when (file-directory? root)
        (for-each
          (lambda (rp)
            (put-string out (string-append
                              "(register-embedded-resource! " (ei-str-lit (car rp))
                              " " (ei-bytes-lit (read-file-string (cdr rp))) ")\n")))
          (bld-walk-files root "" '()))))
    (bld-strs embed-dirs)))

;; Namespaces already defined at boot in the BUILD process (snapshotted before
;; step 1 loads anything) — the driver image's set. bld-require-closure still
;; skips these as preloaded; only LAZY stdlib (not in the image) is emitted.
(define bld-boot-loaded #f)

;; --- the build --------------------------------------------------------------
;; entry-ns: the app's main namespace (a string). out-path: the binary to write.
;; mode: "dev" | "release" | "optimized". Every form runs through jolt.passes/
;; run-passes (const-fold always; type inference in release+optimized; inline +
;; scalar-replace additionally when optimized). Deps + source roots are already
;; applied by the caller.
;; natives: encoded :jolt/native libs to load at startup. embed-dirs: dirs whose
;; files bake into the binary (single-file). ext-roots: project-relative io/resource
;; roots resolved at runtime against JOLT_PWD (ship-alongside resources).
;; direct-link?: closed-world direct-linking (app->app calls bind directly; a plain
;; def is frozen, ^:redef/^:dynamic stay var-routed). The caller (jolt.main) turns
;; this ON for release and optimized and OFF for --dev / --no-direct-link.
(define (bld-suffix? s suf)
  (let ((n (string-length s)) (m (string-length suf)))
    (and (>= n m) (string=? (substring s (- n m) n) suf))))
;; --- derive namespace roots from the require graph ---------------------------
;; Data-reader namespaces load during project setup, before build-binary arms its
;; ns-loaded hook, so the entry walk records nothing for them. Collect their ns
;; names (symbol ns parts) from *data-readers*; build-binary runs the require
;; closure over them to pull in their transitive deps too.
(define (bld-data-reader-ns-names)
  (let ((tbl (var-deref "clojure.core" "*data-readers*")) (acc '()))
    (when (pmap? tbl)
      (pmap-fold tbl
        (lambda (k v a)
          (when (and (symbol-t? v) (symbol-t-ns v) (not (jolt-nil? (symbol-t-ns v))))
            (let ((nm (symbol-t-ns v)))
              (unless (member nm acc)
                (set! acc (cons nm acc)))))
          a)
        #f))
    (reverse acc)))

;; Walk top-level forms in a source file and return the list of namespace name
;; STRINGS that this file requires (via ns :require/:use clauses and top-level
;; require/use forms). Only top-level forms are inspected — no recursion into
;; subforms (the quoted-data bug ce-scan-requires! has). Specs are parsed through
;; the shared expand-spec + parse-libspec (loader.ss / ns.ss), matching the
;; loader's semantics exactly.
;; A libspec that only establishes an alias pulls nothing into the build. At
;; runtime `require` interns the namespace without loading it (loader.ss
;; ldr-load+register), so counting it as a dependency would emit the target into
;; the binary and run its top level — the opposite of what :as-alias asks for. The
;; alias itself is still replayed, by bld-scan-spec!. Mirrors clojure.core's
;; load-lib, which picks its loader with `need-ns (or as use)`.
(define (bld-spec-alias-only? parsed use?)
  (let ((opt-names (map car (cdr parsed))))
    (and (member "as-alias" opt-names)
         (not (member "as" opt-names))
         (not use?))))

(define (bld-ns-requires file)
  (let ((src (ldr-read-source file)) (reqs '()))
    (for-each
      (lambda (form)
        (when (cseq? form)
          (let* ((items (seq->list form))
                 (h (and (pair? items) (car items)))
                 (hn (and (symbol-t? h) (symbol-t-name h))))
            (cond
              ;; (ns name (:require spec...) ...)
              ((and hn (string=? hn "ns"))
               (for-each
                 (lambda (clause)
                   (when (cseq? clause)
                     (let ((cl (seq->list clause)))
                       (when (and (pair? cl) (keyword? (car cl))
                                  (let ((kn (keyword-t-name (car cl))))
                                    (or (string=? kn "require") (string=? kn "use"))))
                         (for-each
                           (lambda (spec)
                             (for-each
                               (lambda (s)
                                 (let ((parsed (parse-libspec s)))
                                   (when (and parsed
                                              (not (bld-spec-alias-only?
                                                     parsed
                                                     (string=? (keyword-t-name (car cl)) "use"))))
                                     (set! reqs (cons (car parsed) reqs)))))
                               (expand-spec spec)))
                           (cdr cl))))))
                 (if (pair? (cdr items)) (cddr items) '())))
              ;; (require spec...) / (use spec...) — specs are quoted
              ((and hn (or (string=? hn "require") (string=? hn "use")))
               (for-each
                 (lambda (a)
                   (let ((unquoted (ce-unquote a)))
                     (for-each
                       (lambda (s)
                         (let ((parsed (parse-libspec s)))
                           (when (and parsed
                                      (not (bld-spec-alias-only? parsed (string=? hn "use"))))
                             (set! reqs (cons (car parsed) reqs)))))
                       (expand-spec unquoted))))
                  (cdr items)))))))
      ;; scan mode: this read happens BEFORE any namespace is loaded, so
      ;; alias-resolved auto keywords (::alias/kw) can't resolve yet — read
      ;; them leniently; only require clauses are extracted from these forms.
      ;; rdr-source-file scopes the file the way the inference and emit walks below
      ;; already do, so a reader error carries it in the message; jolt-enter-file!
      ;; records it for the uncaught reporter, which runs after every dynamic
      ;; binding here has unwound. This walk evaluates nothing, so the two of them
      ;; are the only record of which file a failure came from.
      (parameterize ((rdr-scan-mode #t) (rdr-source-file file))
        (jolt-enter-file! file)
        (map rdr-form->data (ei-read-all src))))
    (reverse reqs)))

;; Host classes a file's forms reference that a LIBRARY provides (jolt-lang/time,
;; jolt-crypto, …). At runtime a class-miss autoloads the provider's install
;; namespace off the source roots (host-static.ss jt-try-autoload! /
;; lib-try-autoload!); a built binary has no source roots, so the scan must pull
;; the provider into flat.ss instead — otherwise the binary throws the RFC 0008
;; "add io.github.jolt-lang/time" error for a dependency the project did declare.
;; Mirrors the runtime predicates: a jt-library class (or java.time.*) ->
;; "jolt.time"; anything else consults lib-class-providers. A provider is pulled
;; only when its source is actually on the roots (find-ns-file) — off the roots
;; the runtime's unknown-class message is the contract and the build must keep
;; succeeding exactly as before.
(define (bld-ns-class-providers file)
  (let ((src (ldr-read-source file))
        (cands '()))
    (define (add! class)
      (let ((cand (cond ((or (member class jt-library-names) (java-time-prefixed? class))
                         "jolt.time")
                        ((lib-provider-for class) => (lambda (p) (vector-ref p 0)))
                        (else #f))))
        (when (and cand (not (member cand cands)))
          (set! cands (cons cand cands)))))
    (define (walk x)
      (cond ((symbol-t? x)
             (let ((ns (symbol-t-ns x)))
               ;; two reference shapes: java.util.Locale/US (the ns segment IS the
               ;; class) and a bare java.time.ZonedDateTime (no slash — the whole
               ;; name is the class, namespace is nil).
               (if (and ns (not (jolt-nil? ns)))
                   (add! ns)
                   (add! (symbol-t-name x)))))
            ((cseq? x) (for-each walk (seq->list x)))
            ((pvec? x) (for-each walk (seq->list x)))
            ((pmap? x) (pmap-fold x (lambda (k v a) (walk k) (walk v) #f) #f))))
    (parameterize ((rdr-scan-mode #t) (rdr-source-file file))
      (jolt-enter-file! file)
      (for-each (lambda (f) (walk (rdr-form->data f))) (ei-read-all src)))
    (filter (lambda (c) (find-ns-file c)) cands)))

;; Post-order DFS from a list of root namespace names: for each name, find its
;; file, recurse into its requires, then append (name . file). Already-visited
;; names are skipped (cycles terminate). Names whose source file can't be found
;; (AOT/in-memory) are skipped — they resolve elsewhere.
;; IMPORTANT: namespaces whose resolved file is jolt-runtime-owned (embedded
;; resource or under ldr-install-roots) are skipped ONLY when already defined
;; at boot (bld-boot-loaded, the seed image's set) — they are preloaded at jolt
;; boot, and emitting them into the app section would bloat the binary and
;; break direct-link bindings. LAZY stdlib (in the install roots but NOT in
;; the seed — e.g. jolt.time.impl) IS included and emitted: a built binary
;; has no disk roots, so its compiled Scheme must define those vars at boot —
;; the same reason bld-emit-cli-aot emits jolt.main into the release image.
;; Result: deps first, roots last.
(define (bld-require-closure names)
  (let ((visited (make-hashtable string-hash string=?))
        (order '()))
    (let dfs ((ns names))
      (unless (null? ns)
        (let ((name (car ns)))
          (unless (hashtable-ref visited name #f)
            (hashtable-set! visited name #t)
            (let ((file (find-ns-file name)))
              (when (and file
                         (or (not (ldr-install-file? file))
                             (not (hashtable-ref bld-boot-loaded name #f))))
                (dfs (append (bld-ns-class-providers file) (bld-ns-requires file)))
                (set! order (cons (cons name file) order)))))
          (dfs (cdr ns)))))
    (reverse order)))

;; Bake the *data-readers* table into the binary so a runtime (read-string
;; "#my/tag …") resolves its reader fn like it does under jolt run. Tag and
;; reader are symbols; the reader path var-derefs the fn at use time.
(define (bld-sym-lit s)
  (let ((ns (symbol-t-ns s)))
    (if (and ns (not (jolt-nil? ns)))
        (string-append "(jolt-symbol " (ei-str-lit ns) " " (ei-str-lit (symbol-t-name s)) ")")
        (string-append "(jolt-symbol #f " (ei-str-lit (symbol-t-name s)) ")"))))
(define (bld-emit-data-readers out)
  (let ((tbl (var-deref "clojure.core" "*data-readers*")))
    (when (and (pmap? tbl) (> (pmap-cnt tbl) 0))
      (put-string out "\n;; === data readers ===\n")
      (put-string out "(def-var! \"clojure.core\" \"*data-readers*\"\n  (jolt-assoc empty-pmap")
      (pmap-fold tbl
        (lambda (k v a)
          (put-string out (string-append "\n    " (bld-sym-lit k) " " (bld-sym-lit v)))
          a)
        #f)
      (put-string out "))\n"))))

(define (build-binary entry-ns out-path mode natives embed-dirs ext-roots direct-link? tree-shake? library?)
  (ei-profile-init!)
  ;; Windows executables carry .exe; normalize here so the append-payload and
  ;; cc paths agree and the shell can run the result. A library keeps its own
  ;; suffix (.dll/.so/.dylib) — never rewrite it to .exe.
  (let ((out-path (if (and (bld-tgt-nt?) (not library?) (not (bld-suffix? out-path ".exe")))
                      (string-append out-path ".exe")
                      out-path)))
  ;; The self-contained path (jolt-embedded-bytes "stub/launcher") needs no csv
  ;; kernel files, no Chez, no cc — only the legacy cc path does. A --library build
  ;; ALWAYS takes the cc path (build-shared), and a cross build (--target) always
  ;; takes build-with-cc, so both need the toolchain even from the self-contained jolt.
  (when (or library? (bld-cross?) (not (jolt-embedded-bytes "stub/launcher"))) (bld-check-toolchain))
  (when (> (string-length (bld-native-link-flags natives)) 0)
    ;; :static natives are cc-linked into the binary, so a C compiler must be on
    ;; PATH — the self-contained jolt bundles the Chez kernel (libkernel.a +
    ;; scheme.h) and relinks a custom stub (see build-self-contained), but still
    ;; needs the system cc for that link. Fail early (before the app's foreign-
    ;; procedure forms eval below) with an actionable message.
    (unless (bld-have-cc?)
      (error 'jolt-build
        "static native linking needs a C compiler (cc) on PATH; install one, or pass --dynamic to load the library at runtime."))
    ;; Preload static archives' symbols into this process so step 1's foreign-
    ;; procedure evals resolve; the .build dir must exist first.
    (bld-mkdir-p (string-append out-path ".build"))
    (bld-preload-static-natives! natives (string-append out-path ".build")))
   ;; 1. record app namespaces in dependency order as they finish loading.
   (let ((app-order '()))
     (set! bld-boot-loaded
       (hashtable-copy loaded-ns #f))
     (set-ns-loaded-hook!
      (lambda (name file) (set! app-order (cons (cons name file) app-order))))
    (ei-mark! "startup")
    (parameterize ((ldr-source-only? #t))    ; emit from source, never a compiled artifact
      (load-namespace entry-ns))
    (set-ns-loaded-hook! (lambda (name file) #f))
    (ei-mark! "load app from source")
    ;; Build ordered ns list from the require graph (static scan of source files)
    ;; merged with the hook's load order. The graph gives post-order deps; the
    ;; hook captures dynamic requires the static scan can't see.
    (let* ((graph (bld-require-closure (list entry-ns)))
           (_prof-graph (ei-mark! "require-graph DFS"))
           (walked (reverse app-order))
           ;; graph without the entry-ns pair (it goes last)
           (graph-rest (if (and (pair? graph)
                                (string=? (caar (reverse graph)) entry-ns))
                           (reverse (cdr (reverse graph)))
                           graph))
           ;; reader namespaces with transitive closure
           (reader-ns-names (bld-data-reader-ns-names))
           (reader-pairs (bld-require-closure reader-ns-names))
           ;; only keep reader pairs not already in graph-rest or walked
           (reader-pairs
             (filter (lambda (p)
                       (not (or (assoc (car p) graph-rest)
                                (assoc (car p) walked))))
                     reader-pairs))
           ;; merge: reader pairs, then the static-graph namespaces the hook never
           ;; saw, then the hook's own order.
           ;;
           ;; walked is authoritative. The hook fires AFTER a namespace finishes
           ;; loading, so every dependency is already in the list — including the
           ;; ones bld-require-closure drops for being install-owned (jolt's own
           ;; stdlib: jolt/time/impl.clj, util.clj, …). Appending walked LAST put
           ;; those behind the library namespaces whose top level calls them, so a
           ;; built binary died at startup on (impl/register-type! …) with
           ;; "Attempting to call unbound fn".
           ;;
           ;; A graph-rest entry missing from walked was already loaded before this
           ;; load-namespace (dep resolution, boot), and so was everything it
           ;; requires — nothing in walked can be its dependency. Those go in front,
           ;; keeping bld-require-closure's post-order among themselves.
           (pre (remp (lambda (p) (assoc (car p) walked)) graph-rest))
           (merged (append reader-pairs pre walked))
           ;; ensure entry-ns is last
           (entry-pair (or (assoc entry-ns merged)
                           (assoc entry-ns walked)
                           (cons entry-ns (find-ns-file entry-ns))))
           (ordered (append (remp (lambda (p) (string=? (car p) entry-ns)) merged)
                            (list entry-pair))))
       (when (null? ordered)
         (error 'jolt-build (string-append "no source namespace loaded for " entry-ns
                                           " — is it on the source roots?")))
       ;; Namespaces the CLASS scan pulled in (lib providers like jolt.time) are in
       ;; `ordered` without ever being loaded in-process: step 1 only loads what
       ;; the require graph reaches, and the runtime class-miss autoload fires on
       ;; USE, which the build never triggers. The strict emit below re-analyzes
       ;; their source against process ns state — an unloaded provider has no
       ;; refer/alias tables, so its own :refer'd names would not resolve. Load
       ;; any still-unloaded ns of the closure now, source-only like step 1.
       (for-each
         (lambda (p)
           (unless (hashtable-ref loaded-ns (car p) #f)
             (parameterize ((ldr-source-only? #t))
               (load-namespace (car p)))))
         ordered)
      ;; 2. emit each app namespace. Release and optimized modes enable the
      ;; inference + record-shape setup passes (inference-enabled?); optimized
      ;; mode additionally runs the inline + flatten + scalar-replace fixpoint
      ;; (inline-enabled?). Dev mode gets const-fold + numeric-annotate only.
      ;; direct-link? commits to a closed world: app->app calls bind directly, a
      ;; plain def is frozen in the binary (^:redef/^:dynamic stay var-routed).
      ;; The caller (jolt.main) turns it ON for release and optimized and OFF for
      ;; --dev / --no-direct-link. The defined-set accumulates across the
      ;; dependency-ordered namespaces, so a dep's defs are direct-linkable by the
      ;; time the entry that calls them is emitted.
      ;; set-optimize!/set-direct-link! are process-global flags in the back end;
      ;; dynamic-wind guarantees they revert even if a strict form errors mid-emit
      ;; (a failing form errors the build by design), so the compiler isn't left in
      ;; optimize/direct-link mode for a later caller.
      (let*-values
          (((core-strs app-strs drop-compiler?)
            (dynamic-wind
              (lambda ()
                ;; Create + publish this build's compilation unit FIRST, so every
                ;; mode flag below lands on it (the unit the per-form emit reads).
                ;; The build emits app + core forms that reference clojure.core, which
                ;; must lower to var-deref, so prelude mode is on for the whole build.
                (ei-fresh-unit!)
                ((var-deref "jolt.backend-scheme" "set-prelude-mode!") #t)
                (set-optimize! (string=? mode "optimized"))
                (set-release! (string=? mode "release"))
                (when direct-link?
                  ((var-deref "jolt.backend-scheme" "set-direct-link!") #t)
                  ((var-deref "jolt.backend-scheme" "direct-link-reset!"))
                  (set-direct-link-flag! #t))
                ;; Register each fn def's source, so an uncaught error in the built
                ;; binary maps its frames to "ns/name (file:line)" instead of a bare
                ;; procedure name. A direct-link build already emits these from
                ;; emit-def-cached, which gates on direct-link — so WITHOUT it a
                ;; built binary printed `deep-boom` where the direct-linked one
                ;; printed `app.util/deep-boom (…/util.clj:24)`. On only for the
                ;; open-world build, so a direct-link build's emitted bytes are
                ;; unchanged and nothing registers twice. The runtime eval path
                ;; turns this on for the same reason (compile-eval.ss); the seed
                ;; mint keeps it off, since its output must not carry this machine's
                ;; absolute paths (emit-image.ss).
                ((var-deref "jolt.backend-scheme" "set-source-reg!") (not direct-link?))
                ;; Bake tracing into built binaries by default (0.6.2): the tail-
                ;; site instrumentation is marks at user tail sites plus one vreg
                ;; store at native/throw tail sites — measured at ~1.0-1.3x of the
                ;; untraced floor — and the chain's site literals carry their own
                ;; lines, so a deployed binary's trace needs no marker files.
                ;; JOLT_TRACE=0 at BUILD time opts out (build-time axis, like the
                ;; dev-mode emission toggle; the baked binary has no runtime knob).
                ;; The seed mint is untouched — its emission stays untraced.
                ((var-deref "jolt.backend-scheme" "set-trace-frames!")
                 (not (jolt-trace-env-off? (getenv "JOLT_TRACE"))))
                ;; Cache resolved var cells per reference site in the APP forms
                ;; (bld-emit-ns / ei-emit-ns-records). A user build is a single
                ;; compile of fixed source, so the gensym-numbered cell names are
                ;; deterministic — the byte-fixpoint concern (the compiler re-
                ;; compiling itself) does NOT apply here, only to the seed mint,
                ;; which keeps var-cache OFF (emit-image.ss). ON in both modes.
                ((var-deref "jolt.backend-scheme" "set-var-cache!") #t)
                ;; whole-program param-type fixpoint before per-form emit — runs in
                ;; release and optimized (the inference modes). JOLT_NO_WP_INFER=1
                ;; skips it: a documented escape for very large apps where the
                ;; fixpoint's cost matters; emit then falls back to per-ns inference
                ;; (run-passes still annotates from explicit ^double/^long hints).
                (when (and (not (getenv "JOLT_NO_WP_INFER"))
                           (or (string=? mode "release") (string=? mode "optimized")))
                  (let ((wp-cached (bld-wp-infer! ordered)))
                    (for-each (lambda (p) (ei-set-cached! (car p) (cdr p))) wp-cached)))
                (ei-mark! "whole-program inference")
                (ei-acc-report!))
              (lambda ()
                ;; A #tag data-reader literal must compile in the binary the same as
                ;; it loads interpreted — apply the reader rewrite to each emitted
                ;; form too (no-op unless the app registered data readers).
                (parameterize ((ei-emit-form-hook
                                (lambda (form) (if data-readers-active (ldr-apply-readers form) form))))
                  (if tree-shake?
                      (dce-shake
                        (dce-blob-records "host/chez/seed/prelude.ss")
                        ;; EAGER per-ns accumulation (see the non-shake branch):
                        ;; the emit lambdas carry side effects — cell/gensym
                        ;; allocation and direct-link-defined registration — and
                        ;; jolt's lazy `map` realizes them in a non-list order,
                        ;; which can emit the entry ns before its dependencies
                        ;; (var-routed calls, out-of-order cells). The named let
                        ;; runs strictly in `ordered` order (deps first).
                        (let ((per-ns '()))
                          (let loopfe ((rest ordered))
                            (unless (null? rest)
                              (let* ((nf (car rest))
                                     (src (ldr-read-source (cdr nf)))
                                     (profile-form
                                       (bld-startup-profile-form
                                         (string-append "namespace " (car nf)))))
                                (jolt-enter-file! (cdr nf))   ; name the file on a failure
                                (parameterize ((rdr-source-file (cdr nf)))
                                  ;; RT.load-parity bracket (dyn-binding.ss): the
                                  ;; ns's replayed forms run under fresh
                                  ;; *warn-on-reflection*/*assert* bindings.
                                  (set! per-ns
                                    (cons (append
                                            (list (dce-rec #t #f '() "(jolt-ns-load-vars-push!)"))
                                            (map (lambda (s) (dce-rec #t #f '() s))
                                                 (bld-ns-prelude (car nf) src))
                                            (ei-emit-ns-records (car nf) src)
                                            (list
                                              (dce-rec #t #f '() "(jolt-ns-load-vars-pop!)")
                                              (dce-rec #t #f '() profile-form)))
                                          per-ns)))
                              (loopfe (cdr rest))))
                          (apply append (reverse per-ns))))
                        (string-append entry-ns "/-main"))
                      (values
                        #f
                        ;; EAGER per-ns accumulation, NOT (apply append (map …)):
                        ;; `map` here is jolt's LAZY map, and the per-ns emit lambdas carry side
                        ;; effects (analysis, cell/gensym allocation, direct-link-defined
                        ;; registration). Their realization order under apply/append is not the
                        ;; list order, so an entry-ns lambda could run before a dependency's —
                        ;; leaving cross-ns calls var-routed instead of direct-linked (and cell
                        ;; names allocated out of order). for-each runs strictly in `ordered`
                        ;; order (deps first), matching the loader.
                        (let ((per-ns '()))
                          (let loopfe ((rest ordered))
                            (unless (null? rest)
                              (let* ((nf (car rest))
                                     (src (ei-timed "emit: read source"
                                            (lambda () (ldr-read-source (cdr nf))))))
                                (jolt-enter-file! (cdr nf))   ; name the file on a failure
                                (parameterize ((rdr-source-file (cdr nf)))
                                  ;; RT.load-parity bracket, matching the tree-shake path.
                                  (set! per-ns
                                    (cons (append
                                            (list "(jolt-ns-load-vars-push!)")
                                            (ei-timed "emit: ns-prelude"
                                              (lambda () (bld-ns-prelude (car nf) src)))
                                            (ei-timed "emit: per-ns total"
                                              (lambda () (bld-emit-ns (car nf) src)))
                                            (list
                                              "(jolt-ns-load-vars-pop!)"
                                              (bld-startup-profile-form
                                                (string-append "namespace " (car nf)))))
                                          per-ns)))
                              (loopfe (cdr rest))))
                          (apply append (reverse per-ns))))
                        #f))))
              (lambda ()
                (set-optimize! #f)
                (set-release! #f)
                (set-direct-link-flag! #f)
                ((var-deref "jolt.backend-scheme" "set-direct-link!") #f)
                ((var-deref "jolt.backend-scheme" "set-source-reg!") #f)
                ;; restore the DEV-mode emission state, not #f — an in-process
                ;; build (nREPL, cmd-build without exec) must not turn off tracing
                ;; for code the session compiles afterwards.
                ((var-deref "jolt.backend-scheme" "set-trace-frames!") jolt-trace-on?)
                ;; drop the accumulated direct-link fqn set too — a later
                ;; in-process build would otherwise bind calls against defs
                ;; recorded for THIS one. (bld-wp-infer!'s record/protocol
                ;; seeds self-heal: the next build replaces them wholesale.)
                ((var-deref "jolt.backend-scheme" "direct-link-reset!"))
                ((var-deref "jolt.backend-scheme" "set-var-cache!") #f)
                ;; clear the build unit's record-shapes: the emit pointer still
                ;; points at this finished build unit, and the direct-ctor emit
                ;; (make-jrecN) is gated only on shape presence, not direct-link —
                ;; so a hypothetical in-process build-then-eval would otherwise fire
                ;; it off stale build-time shapes. Harmless under today's control
                ;; flow (build XOR eval per process), cheap to make robust.
                (jolt-wp-set-record-shapes! (ei-unit) (jolt-hash-map))
                (ei-clear-cached!)))))
        (when drop-compiler? (display "jolt build: dropping compiler image (no runtime eval)\n"))
      (ei-mark! "emit app namespaces")
      (ei-acc-report!)
      (let* ((builddir (string-append out-path ".build"))
             (flat-ss  (string-append builddir "/flat.ss"))
             (flat-so  (string-append builddir "/flat.so"))
             (rt-ss    (string-append builddir "/runtime.ss"))
             (rt-so    (string-append builddir "/runtime.so"))
             (boot     (string-append builddir "/jolt.boot"))
             (boot-h   (string-append builddir "/boot_data.h"))
             (main-c   (string-append builddir "/main.c"))
             ;; Emit the runtime half to its own file when it is app-independent,
             ;; so its compile can be cached (bld-compile-runtime!). Tree-shaking
             ;; rewrites the prelude per app (core-strs), and the cc / cross /
             ;; library paths compile a single file in a spawned Chez, so all of
             ;; those keep the one-file form. JOLT_NO_FLAT_SPLIT=1 forces the
             ;; one-file form everywhere — an escape hatch for telling a build
             ;; problem caused by the split apart from one merely revealed by it.
             (split? (and (jolt-embedded-bytes "stub/launcher")
                          (not library?) (not (bld-cross?)) (not core-strs)
                          (not (getenv "JOLT_NO_FLAT_SPLIT")))))
        (bld-mkdir-p builddir)
        ;; 3. flat source = runtime + app + launcher. When split, runtime.ss holds
        ;; the runtime half and flat.ss holds everything the app contributes; the
        ;; two are compiled separately and loaded into the boot in that order.
        (when split?
          (let ((out (open-output-file rt-ss 'replace)))
            ;; The mode rides in the content so each mode keys its own cache entry:
            ;; the text is mode-independent but the fasl is NOT — the Chez compile
            ;; parameters (inspector info, fasl compression) differ by mode.
            (put-string out (string-append ";; jolt runtime half — mode: " mode "\n"))
            (bld-emit-runtime out drop-compiler? core-strs)
            (close-port out)))
        (let ((out (open-output-file flat-ss 'replace)))
          (if split?
              (put-string out ";; app half — the runtime half is compiled separately (runtime.ss)\n")
              (bld-emit-runtime out drop-compiler? core-strs))
          ;; Load native libs, bake embedded resources, and point source roots at
          ;; the build-time app roots — all BEFORE the app forms. The app's
          ;; top-level forms run at binary startup (Sbuild_heap), and they include
          ;; foreign-procedure evals (a library's defcfn) and (slurp (io/resource …))
          ;; reads. So the libraries must be loaded and resources resolvable by the
          ;; time those forms run, not later in the scheme-start launcher.
          (bld-emit-startup-profile-mark! out "app image begin")
          (put-string out "\n;; === native libraries (required) ===\n")
          (bld-emit-natives out natives 'required)
          (bld-emit-startup-profile-mark! out "required native libraries")
           (put-string out "\n;; === embedded resources ===\n")
           (bld-emit-embeds out embed-dirs)
            (bld-emit-data-readers out)
           ;; set-source-roots!* (not the scanning set-source-roots!): data readers
           ;; are baked just above, and re-scanning would eagerly reload reader
           ;; namespaces via jolt-compile-eval-form — dropped by a tree-shaken binary.
           (put-string out (string-append
                             "(set-source-roots!* (list "
                             (fold-left (lambda (s r) (string-append s (ei-str-lit r) " ")) ""
                                        (get-source-roots))
                             "))\n"))
          (bld-emit-startup-profile-mark! out "embedded resources and source roots")
          ;; Pre-register every app namespace in ns-registry BEFORE any app form
          ;; runs, so a boot-time (require 'x) of an AOT'd namespace no-ops (the
          ;; loader's ns-registry arm) instead of hunting for absent source. Needed
          ;; when a namespace requires a later one at load time — e.g.
          ;; babashka.process conditionally requires babashka.process.pprint, which
          ;; defines no vars of its own (only a defmethod) so ns-has-vars? can't
          ;; vouch for it and its own (ns) form hasn't run yet.
          (put-string out "\n;; === app namespace pre-registration ===\n")
          (for-each (lambda (p) (put-string out (string-append "(intern-ns! " (ei-str-lit (car p)) ")\n")))
                    ordered)
          (bld-emit-startup-profile-mark! out "app namespace registration")
          (put-string out "\n;; === app ===\n")
          (bld-emit-startup-profile-mark! out "app namespaces begin")
          (for-each (lambda (s) (put-string out s) (put-string out "\n")) app-strs)
          ;; The launcher runs as Chez's scheme-start (so argv reaches -main —
          ;; top-level boot forms run during heap build, before args are set), and
          ;; suppresses the interactive greeting. It resets source roots to the
          ;; app's resource dirs resolved against JOLT_PWD (or cwd) so a runtime
          ;; io/resource that wasn't embedded still resolves next to the binary.
          (put-string out "\n;; === launcher ===\n")
          (put-string out "(suppress-greeting #t)\n")
          ;; GC tuning: larger nursery for allocation-heavy workloads (binary-trees,
          ;; ray tracer, etc.). Default 16 MB; override via JOLT_GC_TRIP_BYTES
          ;; environment variable (integer bytes, e.g. \"33554432\" for 32 MB).
          (put-string out
            (string-append
              "(sa-gc-trip-bytes!\n"
              "  (let ((trip (getenv \"JOLT_GC_TRIP_BYTES\"))\n"
              "        (default (* 16 1024 1024)))\n"
              "    (if trip (or (string->number trip) default) default)))\n"))
          (put-string out "(scheme-start\n  (lambda args\n")
          (bld-emit-startup-profile-mark! out "scheme-start begin")
          ;; Shutdown hooks (`:shutdown` on a jolt.process, jolt.host/
          ;; add-shutdown-hook) run from Chez's exit-handler, which is a THREAD
          ;; parameter — so the wrapper has to be installed on the thread that
          ;; calls (exit), and for an app that is this one. Installed before the
          ;; guard so the (exit 1) an uncaught throw takes runs the hooks too.
          ;; The CLI's own twin of this is at the top of jolt-cli-run.
          (unless library? (put-string out "    (jolt-install-exit-handler!)\n"))
          ;; The prologue (optional native loads + source-root setup) and the -main
          ;; call (or library export publish) run under one guard so a throw in
          ;; either surfaces as jolt-report-throwable + a non-zero exit/return
          ;; instead of Chez's opaque dump — the prologue previously ran before any
          ;; guard. A library returns 1 (so Sscheme_start returns non-zero to its
          ;; caller); an executable exits 1.
          (put-string out
            (string-append
              "    (guard (v (#t (jolt-report-throwable v (current-error-port))"
              (if library? " 1))\n" " (exit 1)))\n")))
          (bld-emit-natives out natives 'optional)
           (put-string out (string-append
                              "      (let ((base (or (getenv \"JOLT_PWD\") \".\")))\n"
                              "        (set-source-roots!*\n"
                              "          (append (map (lambda (r) (string-append base \"/\" r)) (list "
                             (fold-left (lambda (s r) (string-append s (ei-str-lit r) " ")) "" (bld-strs ext-roots))
                             "))\n"
                             "                  " (ldr-install-roots-str) ")))\n"))
          (bld-emit-startup-profile-mark! out "scheme-start setup")
          (if library?
              (put-string out (bld-library-launcher-body))
              (put-string out (string-append
                            ;; Call -main only if the entry namespace defines one;
                            ;; a script ns (top-level side effects, no -main) has
                            ;; already run its forms at heap build, so invoking a nil
                            ;; -main would crash ("nil cannot be cast to IFn") — just
                            ;; exit cleanly instead.
                            "      (let ((maincell (var-cell-lookup " (ei-str-lit entry-ns) " \"-main\")))\n"
                            ;; Loading the app left the current ns at the entry ns; reset
                            ;; it to `user` before -main, matching clojure.main (*ns* is
                            ;; `user` when a `-m` -main runs, so a runtime resolve of an
                            ;; aliased symbol behaves the same as on the JVM / interpreted
                            ;; jolt, not off the entry ns's alias table).
                            "        (set-chez-ns! \"user\")\n"
                            ;; The same host-fault capture the cli's run path has
                            ;; (cli-core.ss jolt-cli-run): a raw Chez condition gets
                            ;; its k/marks/site stashed BEFORE the unwind, so a
                            ;; traced binary maps the fault to fn + line. jolt
                            ;; throws skip it (jolt-capture-fault! tests) and
                            ;; raise-continuable preserves warning semantics.
                            "        (when (and maincell (var-cell-defined? maincell))\n"
                            "          (with-exception-handler\n"
                            "            (lambda (c) (when (serious-condition? c) (jolt-capture-fault! c)) (raise-continuable c))\n"
                            "            (lambda ()\n"
                            "              (let ((jolt-main-result (apply jolt-invoke (var-cell-root maincell) args)))\n"
                            "                " (bld-startup-profile-form "entry -main") "\n"
                            "                jolt-main-result))))))\n"
                            "    (exit 0)))\n")))
          (close-port out))
        (ei-mark! "write flat.ss")
        ;; 4. compile -> boot -> link. Two paths, chosen by whether this process
        ;; carries the bundled Chez boots + launcher stub:
        ;;  - SELF-CONTAINED (the distributed jolt, jolt-eaj): compile-file +
        ;;    make-boot-file run IN PROCESS (the compiler is resident — jolt is
        ;;    built from scheme.boot), then the boot is appended to a copy of the
        ;;    embedded stub. No external Chez, no cc.
        ;;  - LEGACY (dev bin/jolt): spawn a fresh Chez for compile-file/
        ;;    make-boot-file, then xxd the boot into a C array and cc-link against
        ;;    libkernel.a. Kept so `make buildsmoke` still exercises the cc path.
        (cond
          ;; cross-compiling (--target) always takes the spawn/cc path: the
          ;; self-contained in-process compile can't load a target xpatch, and the
          ;; xpatch retargets make-boot-file for the whole spawned process.
          ((bld-cross?)
           (when library?
             (error 'jolt-build "cross build (--target) does not support --library yet"))
           (when (> (string-length (bld-native-link-flags natives)) 0)
             (error 'jolt-build
               "cross build (--target) does not support :jolt/native archives yet (they need per-target-arch archives)"))
           (build-with-cc entry-ns out-path mode builddir flat-ss flat-so boot boot-h main-c
                          "" (and drop-compiler? (not (bld-tgt-nt?)))))
          (library?
           (build-shared entry-ns out-path mode builddir flat-ss flat-so boot boot-h
                         (bld-native-link-flags natives)))
          ;; petite-only is POSIX-only: on Windows jolt-foreign-proc-safe still
          ;; evals its foreign-procedure forms (fasl relocations abort the boot
          ;; there), and eval needs the compiler boot resident.
          ((jolt-embedded-bytes "stub/launcher")
           (build-self-contained entry-ns out-path mode builddir
                                 (if split?
                                     (list (list rt-ss rt-so 'runtime)
                                           (list flat-ss flat-so 'app))
                                     (list (list flat-ss flat-so 'whole)))
                                 boot
                                 (bld-native-link-flags natives)
                                 (and drop-compiler? (not bld-nt?))))
          (else
           (build-with-cc entry-ns out-path mode builddir flat-ss flat-so boot boot-h main-c
                          (bld-native-link-flags natives)
                          (and drop-compiler? (not bld-nt?)))))))))))

;; --- self-contained link (in-process compile + append the boot to the stub) ---
;; compile-file runs against the DEFAULT interaction environment, so the boot's
;; top-level defines land in the real symbol cells — the runtime compiler's
;; eval'd code must resolve them (var-deref, jolt-invoke, the jolt-n* macros)
;; when the built binary dynamically requires a namespace. Compiling in a clean
;; copy-environment instead orphans every define in locations eval can't see,
;; and the binary dies with "variable var-deref is not bound" the moment a
;; runtime require compiles source.
;;
;; The default env has a wrinkle the legacy fresh-Chez path doesn't: THIS
;; process's cells hold jolt's redefinitions of some kernel names (`error`,
;; regex.ss), so references to them compile as cell reads — and a read that
;; runs before the redefining form would find the fresh binary's cell unbound.
;; The prologue closes that: it first binds each redefined kernel name's cell
;; to its kernel value, making the boot's earliest reads identical to the
;; legacy path's primitive references.

;; every top-level (define nm …)/(define (nm …) …) name in the flat file that
;; shadows a scheme-environment VARIABLE (syntax names don't eval; skip them).
(define (bld-kernel-prologue flat-ss)
  (let ((seen (make-eq-hashtable))
        (kenv (scheme-environment))
        (names '()))
    (let ((ip (open-input-file flat-ss)))
      (let loop ()
        (let ((f (read ip)))
          (unless (eof-object? f)
            (when (and (pair? f) (eq? (car f) 'define) (pair? (cdr f)))
              (let* ((h (cadr f))
                     (nm (if (pair? h) (car h) h)))
                (when (and (symbol? nm)
                           (not (hashtable-ref seen nm #f))
                           (guard (e (#t #f)) (begin (eval nm kenv) #t)))
                  (hashtable-set! seen nm #t)
                  (set! names (cons nm names)))))
            (loop))))
      (close-port ip))
    (apply string-append
           (map (lambda (nm)
                  (let ((s (symbol->string nm)))
                    (string-append "(define " s " (eval '" s " (scheme-environment)))\n")))
                (reverse names)))))

;; prepend the prologue to the flat file in place, then bake the runtime
;; fingerprint the AOT namespace cache keys on (loader.ss). An app binary carries
;; no version string, so without this every one of them would key its cached
;; fasls under the same "dev" and load namespaces another binary's runtime had
;; emitted. This file is the binary's runtime, so its content hash names it.
;; The fingerprint covers the header + prologue + body, i.e. the file as it stands
;; before the fingerprint itself is appended — so it is assembled in memory once
;; and written once, rather than writing the file, reading it back to hash it, and
;; appending. Same bytes, same fingerprint, one pass over a multi-megabyte file
;; instead of three.
(define (bld-prepend-prologue! flat-ss)
  (let* ((prologue (bld-kernel-prologue flat-ss))
         (src (string-append
                ";; kernel-name cells pre-bound so early reads match the kernel primitives\n"
                prologue
                (read-file-string flat-ss)))
         (fp (string-append (number->string (string-length src) 16) "-"
                            (number->string (aot-content-hash src) 16)))
         (out (open-output-file flat-ss 'replace)))
    (put-string out src)
    (put-string out (string-append
                      "\n;; === runtime fingerprint (AOT cache key) ===\n"
                      "(define jolt-baked-runtime-fingerprint " (ei-str-lit fp) ")\n"))
    (close-port out)))

;; Per-mode Chez compile parameters for app binaries. Mirrors the pattern in
;; build-jolt.ss (optimize-level 2, fasl-compressed #t for release/optimized).
;; "release" keeps inspector + proc-source ON so Clojure backtraces (via
;; inspect/object walking the continuation) survive. "optimized" turns them OFF
;; for max speed. "dev" has no entry (Chez defaults: optimize-level 2, inspector
;; ON, proc-source ON, fasl uncompressed — full debuggability). Single table
;; referenced by both the prologue-string builder and the parameterize block.
;;
;; optimize-level 2, not 3: level 3 is Chez's UNSAFE mode — fx/fl/car/vector
;; ops skip their type checks, and jolt's error semantics depend on those
;; raising ((take nil coll) must throw, not walk off a nil count). Level 2
;; keeps every check with nearly all of the optimization.
(define bld-chez-params
  '(("optimized" (optimize-level 2)
                 (generate-inspector-information #f)
                 (generate-procedure-source-information #f)
                 (fasl-compressed #t))
    ("release"   (optimize-level 2)
                 (generate-inspector-information #t)
                 (generate-procedure-source-information #t)
                 (fasl-compressed #t))))

(define (bld-chez-param-forms mode)
  (let ((params (assoc mode bld-chez-params)))
    (if params
        (fold-left
          (lambda (s p) (string-append s "(" (symbol->string (car p)) " "
                                      (let ((v (cadr p)))
                                        (cond ((boolean? v) (if v "#t" "#f"))
                                              ((number? v) (number->string v))
                                              (else (format "~s" v))))
                                      ")\n"))
          "" (cdr params))
        "")))

;; Compile one flat source file under the mode's Chez parameters. The mode->params
;; table stays as jolt build policy; this is a thin translation of those params
;; into the target-neutral profile sa-compile-file consumes.
(define (bld-chez-compile-file mode src so)
  (let ((params (assoc mode bld-chez-params)))
    (if params
        (let ((pv (lambda (k) (cadr (assq k (cdr params))))))
          (sa-compile-file src so
            `((optimize . ,(pv 'optimize-level))
              (inspector-info . ,(pv 'generate-inspector-information))
              (source-info . ,(pv 'generate-procedure-source-information))
              (compressed . ,(pv 'fasl-compressed)))))
        (sa-compile-file src so #f))))

;; --- runtime-half fasl cache -------------------------------------------------
;; The runtime half of the flat source (rt.ss + the clojure.core prelude +
;; host-contract + the compiler image + loader + ffi) is byte-identical for every
;; app a given jolt builds in a given mode — only its trailing set-source-roots!
;; depends on the install, not on the app. Compiling it is ~2.6s, which for a
;; small app is most of the build. Compile it once per (content, mode) and keep
;; the fasl, so that cost is paid on the first build and never again.
;;
;; Keyed on the content of the runtime source BEFORE bld-prepend-prologue! runs,
;; because both the kernel prologue and the baked fingerprint are deterministic
;; functions of that content — so the key identifies the finished fasl exactly,
;; and a hit skips the prologue pass as well as the compile.
;;
;; NOT used when --tree-shake rewrites the prelude (the runtime half becomes
;; app-specific), for a --library or cross build, or on the legacy cc path.
;; Its own directory and its own variable, deliberately not JOLT_CACHE_DIR: that
;; one names the AOT namespace cache, and pointing both at one directory would
;; leave each pruning around the other's files.
(define (bld-runtime-cache-dir)
  (or (getenv "JOLT_RUNTIME_CACHE_DIR")
      (string-append (or (getenv "HOME") ".") "/.jolt/runtime-cache")))
(define (bld-runtime-cache-enabled?)
  (let ((e (getenv "JOLT_RUNTIME_CACHE")))
    (if (and (string? e) (fx>? (string-length e) 0))
        (not (or (string=? e "0") (string-ci=? e "false")
                 (string-ci=? e "no") (string-ci=? e "off")))
        #t)))
(define (bld-runtime-cache-path body mode)
  (string-append (bld-runtime-cache-dir) "/runtime-" mode "-"
                 (number->string (string-length body) 16) "-"
                 (number->string (aot-content-hash body) 16) ".so"))
;; Keep the newest few entries. One accumulates per jolt build × mode, so a
;; developer re-minting often would otherwise grow this without bound.
(define bld-runtime-cache-keep 8)
(define (bld-prune-runtime-cache!)
  (guard (e (#t #f))
    (let* ((dir (bld-runtime-cache-dir))
           (fs (map (lambda (f) (let ((p (string-append dir "/" f)))
                                   (cons p (sa-file-mtime-ms p))))
                     (filter (lambda (f) (bld-suffix? f ".so")) (directory-list dir)))))
      (when (> (length fs) bld-runtime-cache-keep)
        (for-each (lambda (p) (guard (e (#t #f)) (delete-file (car p))))
                  (list-tail (sort (lambda (a b) (> (cdr a) (cdr b))) fs)
                             bld-runtime-cache-keep))))))
;; Remove an existing output before writing the new one, so the new binary lands on
;; a FRESH inode.
;;
;; macOS caches a code-signature verdict per vnode. Rewriting an executable in place
;; leaves the stale verdict attached to it, and the kernel then SIGKILLs the next run
;; with no output whatsoever — `Killed: 9`, exit 137, nothing on stderr. A
;; rebuild-and-run loop over one output path (the build smoke; anyone iterating on an
;; app) therefore works a handful of times and then starts dying for no visible
;; reason, on a binary that runs fine the moment it is built somewhere else.
;;
;; It also means a failed link leaves no output rather than a half-overwritten one.
(define (bld-clear-output! out-path)
  (when (file-exists? out-path) (delete-file out-path)))

(define (bld-copy-file! from to)
  (let ((bs (read-file-bytes from)))
    (let ((out (open-file-output-port to (file-options no-fail))))
      (put-bytevector out bs)
      (close-port out))))

;; Compile the runtime half, reusing a cached fasl when one matches.
(define (bld-compile-runtime! mode src so)
  (let* ((body (read-file-string src))
         (cache (and (bld-runtime-cache-enabled?) (bld-runtime-cache-path body mode))))
    (if (and cache (file-exists? cache))
        (begin
          (bld-copy-file! cache so)
          (ei-mark! "runtime fasl (cached)"))
        (begin
          (bld-prepend-prologue! src)
          (ei-mark! "kernel prologue + hash")
          (bld-chez-compile-file mode src so)
          (ei-mark! "compile runtime half")
          (when cache
            (guard (e (#t #f))          ; an unwritable cache must not fail the build
              (bld-mkdir-p (bld-runtime-cache-dir))
              (bld-copy-file! so cache)
              (bld-prune-runtime-cache!)))))))

;; units: a list of (src so kind) compiled in order and loaded into the boot in
;; that order, so the runtime half's defines precede the app half's reads.
;;   'whole   — one unsplit flat file: kernel prologue + baked fingerprint, no cache
;;   'runtime — the app-independent half: same, plus the fasl cache
;;   'app     — the app half: compiled plain. It needs no kernel prologue (its
;;              defines are jv$-munged and so cannot shadow a Chez name) and no
;;              fingerprint (the runtime unit carries the one that identifies it).
(define (build-self-contained entry-ns out-path mode builddir units boot native-link petite-only?)
  (let ((petite (string-append builddir "/petite.boot"))
        (scheme (string-append builddir "/scheme.boot")))
    (jolt-spill-embedded! "csv/petite.boot" petite)
    (unless petite-only? (jolt-spill-embedded! "csv/scheme.boot" scheme))
    (display (string-append "jolt build: compiling " entry-ns " (" mode " mode, self-contained)\n"))
    (for-each
      (lambda (u)
        (let ((src (car u)) (so (cadr u)) (kind (caddr u)))
          (case kind
            ((runtime) (bld-compile-runtime! mode src so))
            ((app)
             (bld-chez-compile-file mode src so)
             (ei-mark! "compile app half"))
            (else
             (bld-prepend-prologue! src)
             (ei-mark! "kernel prologue + hash")
             (bld-chez-compile-file mode src so)
             (ei-mark! "Chez compile-file")))))
      units)
    ;; A compiler-dropped binary (no runtime eval) boots from petite alone —
    ;; scheme.boot is the Chez compiler, ~5 MB of heap and ~1 MB of binary it
    ;; would never call. Chez's interpreter (petite) can't create a
    ;; foreign-procedure at runtime, but every defcfn in the image was
    ;; AOT-compiled, so the FFI is unaffected.
    ;; The unit fasls go in after the Chez boots, in the order they were compiled.
    (sa-make-boot-file boot
      (append (list petite)
              (if petite-only? '() (list scheme))
              (map cadr units)))
    (ei-mark! "make-boot-file")
    ;; The stub is the native launcher the boot is appended to. With no :static
    ;; natives it's the prebuilt one bundled in jolt (no cc needed); with :static
    ;; natives it's re-linked here from the bundled kernel + launcher source so the
    ;; archives are baked in and their symbols resolve in the running binary.
    (bld-clear-output! out-path)
    (if (> (string-length native-link) 0)
        (bld-relink-stub builddir native-link out-path)
        (jolt-spill-embedded! "stub/launcher" out-path))
    ;; link: stub bytes ++ boot ++ frame, then make it executable.
    (jolt-append-payload! out-path (read-file-bytes boot))
    (jolt-chmod-755 out-path)
    (ei-mark! "stub + payload link")
    (display (string-append "jolt build: wrote " out-path "\n"))
    (when bld-osx?
      (display (string-append
                 "jolt build: note — on macOS this binary is unsigned; to share it,\n"
                 "  `xattr -d com.apple.quarantine " out-path "` on the target, or sign it.\n")))))

;; Re-link the launcher stub with the app's static native archives baked in, to
;; OUT-PATH. The self-contained jolt bundles the Chez kernel (libkernel.a),
;; header, and launcher source; spill them and drive the system cc — the same link
;; build-jolt.ss ran once at jolt-build time, plus the force-load archive flags
;; (native-link) and, on Linux, -rdynamic so the baked-in symbols stay dlsym-
;; visible for (load-shared-object #f) + foreign-procedure at startup.
(define (bld-relink-stub builddir native-link out-path)
  (let ((h  (string-append builddir "/scheme.h"))
        (lk (string-append builddir "/libkernel.a"))
        (lc (string-append builddir "/launcher.c")))
    (jolt-spill-embedded! "csv/scheme.h" h)
    (jolt-spill-embedded! "csv/libkernel.a" lk)
    (jolt-spill-embedded! "stub/launcher.c" lc)
    (display "jolt build: relinking launcher stub with static native libraries\n")
    (bld-system (string-append
      "cc -O2 " (bld-export-symbols-flag)
      "-I'" builddir "' '" lc "' '" lk "' -o '" out-path "' "
      native-link " " (bld-link-libs)))))

;; --- legacy cc link (dev bin/jolt): fresh Chez compile + xxd + cc ------------
(define (build-with-cc entry-ns out-path mode builddir flat-ss flat-so boot boot-h main-c native-link petite-only?)
  (display (string-append "jolt build: compiling " entry-ns " (" mode " mode)\n"))
  (let ((cs (string-append builddir "/compile.ss")))
    (let ((p (open-output-file cs 'replace)))
      (put-string p
        (string-append
          "(import (chezscheme))\n"
          ;; cross: the xpatch retargets compile-file / make-boot-file to the
          ;; target machine (ChezScheme/BUILDING, "CROSS COMPILING SCHEME
          ;; PROGRAMS"); the boots below come from the target pack.
          (if (bld-cross?) (string-append "(load " (ei-str-lit (bld-xpatch)) ")\n") "")
          (bld-chez-param-forms mode)
          "(compile-file " (ei-str-lit flat-ss) " " (ei-str-lit flat-so) ")\n"
          ;; petite-only boot when the compiler image was dropped (see
          ;; build-self-contained).
          "(make-boot-file " (ei-str-lit boot) " '()\n  "
          (ei-str-lit (string-append (bld-csv-dir) "/petite.boot")) "\n  "
          (if petite-only?
              ""
              (string-append (ei-str-lit (string-append (bld-csv-dir) "/scheme.boot")) "\n  "))
          (ei-str-lit flat-so) ")\n"))
      (close-port p))
    (bld-system (string-append bld-chez " --script '" cs "'")))
  (bld-system (string-append "xxd -i '" boot "' > '" boot-h "'"))
  ;; The xxd symbol is derived from the path; normalize to jolt_boot.
  (bld-system (string-append
    "sed -i.bak -E 's/unsigned char [A-Za-z0-9_]+\\[\\]/unsigned char jolt_boot[]/; "
    "s/unsigned int [A-Za-z0-9_]+_len/unsigned int jolt_boot_len/' '" boot-h "'"))
  (let ((mc (open-output-file main-c 'replace)))
    (put-string mc
      (string-append
        "#include \"scheme.h\"\n#include \"boot_data.h\"\n"
        "int main(int argc, char *argv[]) {\n"
        "  Sscheme_init(0);\n"
        "  Sregister_boot_file_bytes(\"jolt\", jolt_boot, jolt_boot_len);\n"
        "  Sbuild_heap(0, 0);\n"
        "  int status = Sscheme_start(argc, (const char **)argv);\n"
        "  Sscheme_deinit();\n  return status;\n}\n"))
    (close-port mc))
  ;; -rdynamic (Linux) exports the executable's symbols into the dynamic table so
  ;; a statically-linked native lib's symbols resolve via (load-shared-object #f)
  ;; at startup. macOS keeps unstripped executable symbols dlsym-visible already.
  (bld-clear-output! out-path)
  (bld-system (string-append
    (bld-cc) " " (bld-arch-flag) " -O2 " (if (> (string-length native-link) 0) (bld-export-symbols-flag) "")
    "-I'" (bld-csv-dir) "' '" main-c "' '" (bld-csv-dir) "/libkernel.a' "
    "-o '" out-path "' " native-link " " (bld-link-libs)))
  (display (string-append "jolt build: wrote " out-path "\n")))

;; --- shared-library link (jolt build --library) -----------------------------
;; The cc path adapted to emit a shared object instead of an executable: the same
;; compile-file + make-boot-file + xxd boot embedding, but a library.c stub
;; (jolt_library_init / jolt_lookup / jolt_library_shutdown instead of main) and
;; a -shared/-dynamiclib link. Only the cc path supports libraries today — the
;; self-contained append-to-prebuilt-stub path would need a library stub variant
;; baked into the distributed jolt (a follow-up).
;; last path segment of p (after the final '/'), for a dylib's -install_name.
(define (bld-basename p)
  (let loop ((i (fx- (string-length p) 1)))
    (cond ((fx<? i 0) p)
          ((char=? (string-ref p i) #\/) (substring p (fx+ i 1) (string-length p)))
          (else (loop (fx- i 1))))))

(define (bld-library-stub)
  (string-append
    "#include \"scheme.h\"\n"
    "#include <string.h>\n"
    "#include \"boot_data.h\"\n"
    "/* jolt_set_lookup_addr is called from the built library's scheme-start\n"
    "   handler (registered via Sforeign_symbol after Sbuild_heap) to hand the\n"
    "   stub the Scheme lookup callable's address. */\n"
    "static void* (*jolt_lookup_fn)(const char*) = 0;\n"
    "void jolt_set_lookup_addr(void* fn) { jolt_lookup_fn = (void*(*)(const char*))fn; }\n"
    "void* jolt_lookup(const char* name) { return jolt_lookup_fn ? jolt_lookup_fn(name) : 0; }\n"
    "int jolt_library_init(int argc, char** argv) {\n"
    "  if (!argv) argc = 0;  /* Sscheme_start reads argv[0..argc-1]; a NULL argv means no args */\n"
    "  Sscheme_init(0);\n"
    "  Sregister_boot_file_bytes(\"jolt\", jolt_boot, (iptr)jolt_boot_len);\n"
    "  Sbuild_heap(0, 0);\n"
    "  Sforeign_symbol(\"jolt_set_lookup_addr\", (void*)jolt_set_lookup_addr);\n"
    "  return Sscheme_start(argc, (const char**)argv); }\n"
    "void jolt_library_shutdown(void) { Sscheme_deinit(); }\n"))

;; The library scheme-start tail BODY: publish the export table to the embedder,
;; then return 0 so Sscheme_start returns to jolt_library_init's caller. The guard
;; (returning 1 on failure) is emitted by build-binary around the whole launcher —
;; prologue + this body — so an init failure anywhere reports and returns non-zero;
;; otherwise jolt_set_lookup_addr never runs and jolt_lookup silently returns NULL.
(define (bld-library-launcher-body)
  (string-append
    "      ;; publish the export table to the embedder\n"
    "      (let* ((lk (foreign-callable jolt-ffi-lookup-export (string) uptr))\n"
    "             (lk-addr (jolt-ffi-register-callable! lk)))\n"
    "        ((foreign-procedure \"jolt_set_lookup_addr\" (void*) void) lk-addr))\n"
    "      0)))\n"))

(define (build-shared entry-ns out-path mode builddir flat-ss flat-so boot boot-h native-link)
  (display (string-append "jolt build: compiling " entry-ns " (" mode " mode, shared library)\n"))
  (let ((cs (string-append builddir "/compile.ss")))
    (let ((p (open-output-file cs 'replace)))
      (put-string p
        (string-append
          "(import (chezscheme))\n"
          (bld-chez-param-forms mode)
          "(compile-file " (ei-str-lit flat-ss) " " (ei-str-lit flat-so) ")\n"
          "(make-boot-file " (ei-str-lit boot) " '()\n  "
          (ei-str-lit (string-append (bld-csv-dir) "/petite.boot")) "\n  "
          (ei-str-lit (string-append (bld-csv-dir) "/scheme.boot")) "\n  "
          (ei-str-lit flat-so) ")\n"))
      (close-port p))
    (bld-system (string-append bld-chez " --script '" cs "'")))
  (bld-system (string-append "xxd -i '" boot "' > '" boot-h "'"))
  (bld-system (string-append
    "sed -i.bak -E 's/unsigned char [A-Za-z0-9_]+\\[\\]/unsigned char jolt_boot[]/; "
    "s/unsigned int [A-Za-z0-9_]+_len/unsigned int jolt_boot_len/' '" boot-h "'"))
  (let ((lc (string-append builddir "/library.c")))
    (let ((p (open-output-file lc 'replace)))
      (put-string p (bld-library-stub))
      (close-port p))
    (bld-clear-output! out-path)
    (bld-system (string-append
      "cc -O2 -fPIC "
      ;; -install_name @rpath/<base> so a binary that link-edits against the dylib
      ;; (rather than dlopen'ing it) can locate it via its rpath, not a build-dir path.
      (if bld-osx?
          (string-append "-dynamiclib -install_name '@rpath/" (bld-basename out-path) "' ")
          "-shared ")
      "-I'" (bld-csv-dir) "' '" lc "' '" (bld-csv-dir) "/libkernel.a' "
      "-o '" out-path "' " native-link " " (bld-link-libs))))
  (display (string-append "jolt build: wrote " out-path "\n")))

;; optional trailing (target target-pack): a Chez machine string + a prepared
;; target pack dir when cross-compiling (jolt build --target). Absent/nil = host.
(define (bld-opt-str opt i)
  (let loop ((o opt) (i i))
    (cond ((or (null? o) (< i 0)) #f)
          ((= i 0) (and (not (jolt-nil? (car o))) (jolt-str-render-one (car o))))
          (else (loop (cdr o) (- i 1))))))
(def-var! "jolt.host" "build-binary"
  (lambda (entry out mode natives embed-dirs ext-roots direct-link? tree-shake? . opt)
    (parameterize ((bld-target (bld-opt-str opt 0)) (bld-target-pack (bld-opt-str opt 1)))
      (build-binary (jolt-str-render-one entry)
                    (jolt-str-render-one out)
                    (jolt-str-render-one mode)
                    natives embed-dirs ext-roots (jolt-truthy? direct-link?) (jolt-truthy? tree-shake?) #f))
    jolt-nil))
(def-var! "jolt.host" "build-library"
  (lambda (entry out mode natives embed-dirs ext-roots direct-link? tree-shake?)
    (build-binary (jolt-str-render-one entry)
                  (jolt-str-render-one out)
                  (jolt-str-render-one mode)
                  natives embed-dirs ext-roots (jolt-truthy? direct-link?) (jolt-truthy? tree-shake?) #t)
    jolt-nil))
