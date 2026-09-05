;; build-jolt.ss — build jolt itself as a self-contained native binary (jolt-eaj).
;;
;;   chez --script host/chez/build-jolt.ss <profile> <out-path>
;;   profile: "release" | "debug"   out-path: e.g. target/release/jolt
;;
;; Runs on a dev/CI machine that HAS Chez + cc. Produces a binary that needs
;; NEITHER: it bakes the full runtime + compiler image + all jolt-core/stdlib
;; source + the Chez petite/scheme boots + a prebuilt launcher stub into one
;; cc-linked executable, so the resulting jolt can run AND `build` jolt apps on
;; its own. jolt itself is cc-linked (not appended) so its signature stays clean
;; for Homebrew/codesign, like dirge's binaries; only the apps it later builds use
;; the appended-stub path (host/chez/build.ss build-self-contained).
;;
;; Pipeline:
;;   0. cc-compile host/chez/stub/launcher.c against the Chez kernel.
;;   1. emit flat.ss = runtime + compiler image (cli.ss load order) + inlined
;;      build.ss + every jolt-core/stdlib file as a baked string literal + the
;;      jolt launcher.
;;   2. in-process compile-file + make-boot-file (profile Chez settings), error
;;      restored around the call (the runtime shadows it; rt.ss/%chez-error).
;;   3. xxd the jolt boot + petite/scheme boots + stub into C arrays, generate
;;      main.c, cc-link -> out-path. The launcher reads the petite/scheme/stub
;;      arrays via FFI on `build` (jolt-materialize-bundles!).

(import (chezscheme))

(load "host/chez/scheme-adapter-runtime.ss")  ; before rt.ss: macros + top-levels in rt.ss/java/*.ss call sa-*
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(load "host/chez/post-prelude-str.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")
;; cli-core.ss is inlined into the emitted image below, but it also has to be
;; loaded HERE: it defines jolt.host/run-expr-string, and bld-emit-cli-aot emits
;; jolt.main in this process — an unresolved jolt.host var would emit as a class
;; reference and the binary's -e arm would die with "Unknown class jolt.host".
(load "host/chez/cli-core.ss")
(load "host/chez/png.ss")
(load "host/chez/loader.ss")
(load "host/chez/java/ffi.ss")
(set-source-roots! ldr-install-roots)
(load "host/chez/build.ss")   ; bld-* helpers, ei-* (emit-image), dce

(define jb-args (cdr (command-line)))
(define jb-profile (if (pair? jb-args) (car jb-args) "release"))
(define jb-out (if (and (pair? jb-args) (pair? (cdr jb-args))) (cadr jb-args)
                   (string-append "target/" jb-profile "/jolt")))
(define jb-release? (string=? jb-profile "release"))
(unless (or jb-release? (string=? jb-profile "debug"))
  (error 'build-jolt "profile must be \"release\" or \"debug\"" jb-profile))

;; Cross-compilation: an optional 3rd arg is the target Chez machine, and the
;; target pack comes from $JOLT_TARGET_PACK — cross-builds jolt itself for
;; another platform (e.g. restoring the x86_64-macos release artifact from an
;; arm64 runner). The bld-* helpers below key off these parameters (build.ss).
(when (and (pair? jb-args) (pair? (cdr jb-args)) (pair? (cddr jb-args)))
  (let ((tgt (caddr jb-args)) (pack (getenv "JOLT_TARGET_PACK")))
    (when (and tgt (> (string-length tgt) 0))
      (unless (and pack (> (string-length pack) 0))
        (error 'build-jolt "cross build (target arg) needs $JOLT_TARGET_PACK — see tools/cross-compile/README.md"))
      (bld-target tgt)
      (bld-target-pack pack))))

;; Version baked into the binary's saved heap. Prefer $JOLT_VERSION (CI sets it to
;; the release tag, or a nightly's describe string); else tools/version.sh, the
;; same answer bin/jolt gives for this checkout (it says "dev" outside git).
(define jb-version
  (let ((env (getenv "JOLT_VERSION")))
    (if (and env (> (string-length env) 0))
        env
        (let ((s (bld-sh-capture "sh tools/version.sh 2>/dev/null")))
          (if (> (string-length s) 0) s "dev")))))

(define jb-build (string-append jb-out ".build"))
(bld-check-toolchain)
(bld-system (string-append "mkdir -p '" (path-parent jb-out) "' '" jb-build "'"))

;; --- 0. compile the launcher stub -------------------------------------------
(define jb-stub (string-append jb-build "/launcher"))
(display "build-jolt: compiling launcher stub\n")
(bld-system (string-append
  (bld-cc) " " (bld-arch-flag) " -O2 -I'" (bld-csv-dir) "' 'host/chez/stub/launcher.c' '"
  (bld-csv-dir) "/libkernel.a' -o '" jb-stub "' " (bld-link-libs)))

;; --- 1. emit flat.ss --------------------------------------------------------
(define jb-flat-ss (string-append jb-build "/flat.ss"))
(define (str-suffix? s suf)
  (let ((n (string-length s)) (m (string-length suf)))
    (and (>= n m) (string=? (substring s (- n m) n) suf))))

;; Bake every jolt-core/stdlib source file as an in-heap string literal keyed by
;; its root-relative path ("jolt/main.clj", "clojure/string.clj") — exactly what
;; resolve-on-roots probes. Literals (not read-file-string at startup) because
;; flat.ss top-level forms run at every startup, with no source on disk.
;;
;; FIRST ROOT WINS, because that is what resolve-on-roots does on disk. Two roots
;; can hold the same namespace — a vendored library shipping a host adapter under
;; a jolt name — and register-embedded-resource! is a plain hashtable-set!, so
;; emitting both would hand the binary the LAST one and silently disagree with
;; the source tree it was built from.
(define (jb-emit-source-embeds out)
  (let ((baked (make-hashtable string-hash string=?)))
    (for-each
      (lambda (root)
        (for-each
          (lambda (rp)
            (let ((rel (car rp)) (abs (cdr rp)))
              (when (and (ldr-source-path? rel)
                         (not (hashtable-ref baked rel #f)))
                (hashtable-set! baked rel #t)
                (put-string out (string-append
                  "(register-embedded-resource! " (ei-str-lit rel) " "
                  (ei-bytes-lit (read-file-string abs)) ")\n")))))
          (bld-walk-files root "" '())))
      ldr-install-roots)))

;; Embed every runtime .ss the build inlines into an app (the transitive closure of
;; the manifest's loads: rt.ss + all it loads, the seed, compile-eval, loader, ffi,
;; png, vendored irregex). Keyed by the exact path the (load "…") forms use, so
;; build.ss's bld-source-string reads them from the binary with no jolt source on
;; disk. Traversal mirrors bld-emit-runtime/bld-inline-line via the same
;; bld-file-lines + bld-load-path, so the embedded set is exactly what build reads.
(define (jb-collect-load-paths)
  (let ((seen (make-hashtable string-hash string=?)) (order '()))
    (define (walk path)
      (when (and path (not (hashtable-ref seen path #f)))
        (hashtable-set! seen path #t)
        (set! order (cons path order))
        (for-each (lambda (l) (walk (bld-load-path l))) (bld-file-lines path))))
    (for-each (lambda (entry) (when (string? entry) (walk (bld-load-path entry))))
              bld-runtime-manifest)
    (for-each (lambda (kv) (walk (bld-load-path (cdr kv)))) bld-tagged-loads)
    (reverse order)))

(define (jb-emit-runtime-embeds out)
  (for-each
    (lambda (path)
      (put-string out (string-append
        "(register-embedded-resource! " (ei-str-lit path) " "
        (ei-bytes-lit (read-file-string path)) ")\n")))
    (jb-collect-load-paths)))

;; The launcher (Chez scheme-start): replicates host/chez/cli.ss but reads argv
;; from the scheme-start lambda and has no repo root to cd into (all source is
;; embedded; JOLT_PWD defaults to cwd via io/jolt.main). build.ss is already
;; inlined, so `build` dispatches straight to jolt.host/build-binary after the
;; bundled boots/stub are materialized from the binary's own C arrays.
(define (jb-emit-launcher out)
  (put-string out (string-append "
;; Materialize the bundled Chez boots + launcher stub (cc-linked into this binary
;; as C arrays) into the embedded-bytes store, so build-self-contained can spill
;; them. Done lazily on `build` only.
;;
;; No (sa-load-shared-object #f): the boot loaded the process-global handle once,
;; which is enough to resolve the jolt_* C-array symbols exported by THIS binary.
;; Re-loading would re-promote the global handle to the head of Chez's
;; foreign-entry search order and outrank any user native loaded before a `build`
;; invocation that loads an embedded fasl — the same class of hazard the
;; launcher's jolt-stdlib-fasl-fetch had. `build` runs in the SAME process as the
;; user's app, so it cannot safely reorder foreign-entry either.
(define (jolt-materialize-bundles!)
  (let ((memcpy (sa-foreign-procedure \"memcpy\" (u8* uptr uptr) void*)))
    (for-each
      (lambda (spec)
        ;; A zero length means the file was absent at jolt-build time — only the
        ;; compression archives can be — so register nothing and let the reader's
        ;; #f branch handle it. Every other bundle here is always non-empty.
        (let ((len (sa-foreign-ref 'unsigned-int (sa-foreign-entry-address (caddr spec)) 0)))
          (when (> len 0)
            (let ((bv (make-bytevector len)))
              (memcpy bv (sa-foreign-entry-address (cadr spec)) len)
              (register-embedded-bytes! (car spec) bv)))))
      '((\"csv/petite.boot\" \"jolt_petite_boot\" \"jolt_petite_boot_len\")
        (\"csv/scheme.boot\" \"jolt_scheme_boot\" \"jolt_scheme_boot_len\")
        (\"stub/launcher\" \"jolt_stub\" \"jolt_stub_len\")
        (\"csv/scheme.h\" \"jolt_scheme_h\" \"jolt_scheme_h_len\")
        (\"csv/libkernel.a\" \"jolt_libkernel_a\" \"jolt_libkernel_a_len\")
        (\"csv/liblz4.a\" \"jolt_liblz4_a\" \"jolt_liblz4_a_len\")
        (\"csv/libz.a\" \"jolt_libz_a\" \"jolt_libz_a_len\")
        (\"stub/launcher.c\" \"jolt_launcher_c\" \"jolt_launcher_c_len\")))))

(suppress-greeting #t)
;; GC tuning: larger nursery for allocation-heavy workloads. Default 16 MB;
;; override via JOLT_GC_TRIP_BYTES env (integer bytes).
(sa-gc-trip-bytes!
  (let ((trip (getenv \"JOLT_GC_TRIP_BYTES\"))
        (default (* 16 1024 1024)))
    (if trip (or (string->number trip) default) default)))
(scheme-start
  (lambda args
    (set-source-roots! " (ldr-install-roots-str) ")
    ;; JOLT_TRACE at RUNTIME (the env is unset at heap-build), before any app ns
    ;; compiles, so a `-M:run` traces the app's own code.
    (jolt-trace-init-from-env!)
    (jolt-stdlib-fasls-attach! '" (jb-stdlib-index-str) ")
    ;; shared dispatch (cli-core.ss, inlined via the runtime manifest): the -e
    ;; arm, end-of-options, and uncaught reporting are the same code the script
    ;; driver runs — the launcher once carried a stale fork of the -e arm.
    (jolt-cli-run args (lambda () (jolt-materialize-bundles!) (jb-load-build-subsystem!)))
    (exit 0)))
")))

;; --- AOT the install-owned stdlib namespaces as embedded fasls --------------
;; install-owned source (jolt-core/, stdlib/, ...) is excluded from the on-disk AOT
;; cache (aot-load-or-compile's `(not (ldr-install-file? file))` guard), so every
;; `(require 'clojure.test)` recompiled ~671 lines from source on EACH process
;; start. Instead: load each remaining install-owned namespace HERE, emit its
;; Scheme, compile it to a fasl in a FRESH Chez with the release profile, and bake
;; the bytes into flat.ss as `(register-embedded-fasl! "<ns>" <bytevector>)`. At
;; require time the loader finds the embedded fasl for an install-owned ns and
;; loads it via load-compiled-from-port instead of recompiling.

(define jb-stdlib-manifest-path "host/chez/stdlib-fasl-manifest.txt")

;; Shared profile boolean renderer (also used by the flat.ss compile step below).
;; Defined here, before jb-compile-stdlib-so!, because the stdlib fasl compile
;; runs during flat.ss emission — earlier than the step-2 compile block that
;; once held the only definition.
(define jb-bool (lambda (b) (if b "#t" "#f")))

;; Filled by jb-emit-stdlib-fasls! during flat.ss emission: the single blob path
;; (xxd'd into the binary as jolt_stdlib_fasls in step 3) and the (ns offset
;; length) index (baked into flat.ss via the launcher's jolt-stdlib-fasls-attach!
;; call). #f / () until then.
(define jb-stdlib-blob-path #f)
(define jb-stdlib-index '())

;; Render jb-stdlib-index as a Scheme source list for the launcher's attach call:
;; (("clojure.test" 0 123456) ...). This literal is the only thing baked into
;; flat.ss for the stdlib fasls — small, regardless of how big the blob is.
(define (jb-stdlib-index-str)
  (string-append "("
    (apply string-append
      (map (lambda (e)
             (string-append "(" (ei-str-lit (car e)) " "
                            (number->string (cadr e)) " "
                            (number->string (caddr e)) ")"))
           jb-stdlib-index))
    ")"))

;; Parse the manifest into (values embedded-names skip-alist). Skip lines look
;; like "skip <ns> — <reason>"; the alist entry is (ns . reason). The reason is
;; what jb-compile-stdlib-fasls! reports and the manifest author verified, and
;; the ns drives compile EXCLUSION: a skip is not even load-attempted, so a ns
;; whose emitted .scm Chez rejects cannot kill the batch compile.
(define (jb-read-stdlib-manifest)
  (let loop ((lines (bld-file-lines jb-stdlib-manifest-path))
             (names '()) (skips '()))
    (if (null? lines)
        (values (reverse names) (reverse skips))
        (let* ((ln (car lines)) (n (string-length ln)))
          (cond
            ((or (string=? ln "") (char=? (string-ref ln 0) #\#))
             (loop (cdr lines) names skips))
            ((and (>= n 5) (string=? (substring ln 0 5) "skip "))
             (let* ((rest (substring ln 5 n)) (di (jb-str-index rest " — ")))
               (loop (cdr lines) names
                     (cons (if di
                               (cons (substring rest 0 di)
                                     (substring rest (+ di 3) (string-length rest)))
                               (cons rest ""))
                           skips))))
            (else (loop (cdr lines) (cons ln names) skips)))))))

(define (jb-stdlib-manifest-check! discovered)
  (define-values (man-names man-skips) (jb-read-stdlib-manifest))
  (let ((man-keys (sort string<? (append man-names (map car man-skips))))
        (disc-keys (sort string<? discovered)))
    (unless (and (= (length man-keys) (length disc-keys))
                 (equal? man-keys disc-keys))
      (error 'build-jolt
        (string-append
          "stdlib-fasl manifest drift — discovered set does not match "
          jb-stdlib-manifest-path "\n"
          "  discovered (" (number->string (length disc-keys)) "): "
          (apply string-append (map (lambda (n) (string-append n " ")) disc-keys)) "\n"
          "  manifest   (" (number->string (length man-keys)) "): "
          (apply string-append (map (lambda (n) (string-append n " ")) man-keys)) "\n"
          "  Edit the manifest to match the discovered set.")))))

(define (jb-strip-source-ext rel)
  (let loop ((es ldr-source-exts))
    (and (pair? es)
         (let* ((suf (car es)) (m (string-length suf)) (n (string-length rel)))
           (if (and (>= n m) (string=? (substring rel (- n m) n) suf))
               (substring rel 0 (- n m))
               (loop (cdr es)))))))

;; Discover every install-owned source ns in first-root-wins order, dropping ones
;; already loaded (image + CLI closure). Returns ((name . abspath) ...) in walk
;; order. Same walk + ldr-source-path? + first-root-wins discipline as
;; jb-emit-source-embeds.
(define (jb-stdlib-candidates)
  (let ((seen (make-hashtable string-hash string=?)) (out '()))
    (for-each
      (lambda (root)
        (for-each
          (lambda (rp)
            (let* ((rel (car rp)) (abs (cdr rp)) (core (jb-strip-source-ext rel)))
              (when (and core (ldr-source-path? rel) (not (hashtable-ref seen core #f)))
                (hashtable-set! seen core #t)
                (let ((ns (jb-ns-from-rel core)))
                  ;; A file under clojure/core/ named 10-seq.clj derives the ns
                  ;; name clojure.core.10-seq, but that is a clojure.core OVERLAY
                  ;; SHARD (its ns form is (ns clojure.core)), not a standalone
                  ;; namespace: load-namespace would fail to find it (ns-name->rel
                  ;; munges the dash to an underscore) and there is nothing to
                  ;; AOT — it is already in the image as part of clojure.core.
                  ;; Drop a candidate whose derived name does not round-trip to a
                  ;; findable source file.
                  (unless (or (hashtable-ref loaded-ns ns #f)
                              (member ns '("jolt.main" "jolt.deps"))
                              (not (find-ns-file ns)))
                    (set! out (cons (cons ns abs) out)))))))
          (bld-walk-files root "" '())))
      ldr-install-roots)
    (reverse out)))

;; ONE batch compile of every emitted stdlib .scm, in a FRESH Chez that has the
;; SAME runtime preamble loaded as build-jolt's own header. The emitted Scheme
;; references runtime MACROS — jolt-n-, jolt-n+, jolt-n*, jolt-n-div, jolt-n-min
;; are define-syntax in seq.ss — which a bare (import (chezscheme)) leaves as
;; top-level variable references, so a fasl compiled that way throws "variable
;; jolt-n- is not bound" the moment a loaded namespace does arithmetic. Loading
;; the preamble (rt.ss loads seq.ss) gives compile-file the macro environment
;; the Scheme was produced under. The on-disk AOT cache never hits this because
;; loader.ss compiles emitted Scheme IN-PROCESS (sa-compile-file) with the
;; runtime already loaded.
;;
;; PAIRS is a list of (scm . so) in load order. The script loads the preamble,
;; THEN the xpatch (cross only — the runtime must load as host-executed code
;; first, the xpatch then retargets compile-file's code generation for the
;; .scm files), sets the per-profile flags mirrored from the flat.ss compile
;; step, and compile-file's every .scm -> .so. A compile failure here is a BUILD
;; FAILURE (not a skip): with the macro environment present there is no
;; legitimate reason emitted Scheme should fail to compile.
(define (jb-compile-stdlib-sos! pairs)
  (let ((cs (string-append jb-stdlib-dir "/compile-all.ss")))
    (let ((p (open-output-file cs 'replace)))
      (put-string p
        (string-append
          "(import (chezscheme))\n"
          ;; runtime preamble — mirror build-jolt.ss's own header (lines 27-43)
          ;; exactly, so the macro environment matches what emitted the Scheme.
          "(load \"host/chez/scheme-adapter-runtime.ss\")\n"
          "(load \"host/chez/rt.ss\")\n"
          "(set-chez-ns! \"clojure.core\")\n"
           "(load \"host/chez/seed/prelude.ss\")\n"
           "(load \"host/chez/post-prelude.ss\")\n"
           "(load \"host/chez/post-prelude-str.ss\")\n"
           "(set-chez-ns! \"user\")\n"
          "(load \"host/chez/host-contract.ss\")\n"
          "(load \"host/chez/seed/image.ss\")\n"
          "(load \"host/chez/compile-eval.ss\")\n"
          "(load \"host/chez/cli-core.ss\")\n"
          "(load \"host/chez/png.ss\")\n"
          "(load \"host/chez/loader.ss\")\n"
          "(load \"host/chez/java/ffi.ss\")\n"
          (if (bld-cross?) (string-append "(load " (ei-str-lit (bld-xpatch)) ")\n") "")
          ;; per-profile flags, mirrored from the flat.ss compile step.
          "(optimize-level " (if jb-release? "2" "0") ")\n"
          "(generate-inspector-information " (jb-bool (not jb-release?)) ")\n"
          "(generate-procedure-source-information " (jb-bool (not jb-release?)) ")\n"
          "(debug-on-exception " (jb-bool (not jb-release?)) ")\n"
          "(fasl-compressed " (jb-bool jb-release?) ")\n"
          (apply string-append
            (map (lambda (sp)
                   (string-append "(compile-file "
                     (ei-str-lit (car sp)) " "
                     (ei-str-lit (cdr sp)) ")\n"))
                 pairs))))
      (close-port p))
    (bld-system (string-append bld-chez " --script '" cs "'"))))

;; Capture one ns's emitted Scheme to its .scm. aot-capture-load evaluates the
;; FILE through the normal load loop while teeing the per-form Scheme — the SAME
;; path the on-disk AOT cache (aot-compile-and-cache) and clojure.core/compile
;; (cpath-compile-load) use — so the captured string INCLUDES the ns form's
;; requires. bld-emit-ns (whole-program APP emission) drops requires, since in an
;; app image every dep is already present; an on-demand fasl is different —
;; requiring jolt.fs loads its fasl but babashka.fs never arrives, leaving every
;; proxied var unbound. aot-capture-load evals the file directly (re-evaluation
;; at build time is fine; defonce guards hold), tees ONLY this file's forms
;; (loader.ss:438 nested-capture discipline keeps a nested require out of this
;; capture), and runs under the process's own compile settings — no
;; prelude-mode/optimize/release/var-cache dynamic-wind, matching what the
;; binary's own cache-miss path would produce at runtime. A capture returning a
;; non-string or empty string is a BUILD FAILURE (reported), not a skip —
;; matching cpath-compile-load's guard. A load EXCEPTION is a skip.
(define (jb-capture-stdlib-scm! name abs scm)
  (let ((captured-or-skip
          (guard (e (else
                      (let ((msg (guard (_ (#t "(unprintable)"))
                                  (let ((m ((var-deref "jolt.host" "condition-message") e)))
                                    (and (string? m) m)))))
                        (display (string-append "build-jolt: stdlib-fasl FAILED " name ": " msg "\n")
                                 (current-error-port))
                        (cons 'skip (string-append "load failed: " msg)))))
            (cons 'ok (aot-capture-load abs (ldr-read-source abs))))))
    (cond
      ((eq? (car captured-or-skip) 'skip) (cons 'skip (cdr captured-or-skip)))
      ((or (not (string? (cdr captured-or-skip)))
           (fx=? (string-length (cdr captured-or-skip)) 0))
       ;; empty capture for a real ns is a build failure — escapes this function
       ;; (no guard here) so it aborts the build rather than silently skipping.
       (error 'build-jolt
         (string-append "stdlib-fasl capture produced no code for " name)))
      (else
       (let ((outp (open-output-file scm 'replace)))
         (put-string outp (cdr captured-or-skip))
         (close-port outp))
       'ok))))

;; Capture (aot-capture-load) every candidate ns to its own fasl, then ONE batch
;; compile. Returns two values: (name . so) that captured OK (in load order) and
;; (name . reason) that were skipped. A manifest-declared skip is never
;; load-attempted. A load exception is a skip with a reason; an empty capture is
;; a build failure (see jb-capture-stdlib-scm!).
(define (jb-compile-stdlib-fasls!)
  (define-values (man-embed man-skips) (jb-read-stdlib-manifest))
  (bld-system (string-append "mkdir -p '" jb-stdlib-dir "'"))
  (let ((order '()) (to-compile '()) (skips (reverse man-skips)))
    (parameterize ((ldr-source-only? #t))
      (set-ns-loaded-hook! (lambda (name file) #f))
      (for-each
        (lambda (nf)
          (let* ((name (car nf)) (abs (cdr nf)) (san (jb-sanitize name))
                 (scm (string-append jb-stdlib-dir "/" san ".scm"))
                 (so (string-append jb-stdlib-dir "/" san ".so")))
            (if (assoc name skips)
                ;; manifest-declared skip — already in skips with its verified
                ;; reason; do not load-attempt. jb-emit-stdlib-fasls! prints
                ;; all skips once at the end.
                (if #f #f)
                (let ((r (jb-capture-stdlib-scm! name abs scm)))
                  (cond
                    ((eq? r 'ok)
                     (set! order (cons (cons name so) order))
                     (set! to-compile (cons (cons scm so) to-compile)))
                    ((pair? r)
                     (set! skips (cons (cons name (cdr r)) skips))))))))
        (jb-stdlib-candidates))
      (set-ns-loaded-hook! (lambda (name file) #f)))
    ;; ONE batch compile with the runtime preamble loaded (see jb-compile-stdlib-sos!).
    ;; A failure here is a BUILD FAILURE — captured Scheme must compile once the
    ;; macro environment is present.
    (jb-compile-stdlib-sos! (reverse to-compile))
    (values (reverse order) (reverse skips))))

;; Compile the stdlib fasls (jb-compile-stdlib-fasls!, unchanged), concatenate
;; them into ONE blob file, and record an index of (ns offset length) into
;; jb-stdlib-index. The blob is xxd'd into the binary as the C array
;; jolt_stdlib_fasls (step 3, below); the index — small — is the ONLY thing baked
;; into flat.ss for the stdlib, via the launcher's jolt-stdlib-fasls-attach! call.
;; Putting the multi-MB fasl bytes into flat.ss as boot-image literals regresses
;; startup (the hard floor), and ei-bytes-lit corrupts them anyway — it round-
;; trips through utf8->string, which arbitrary binary does not survive. Runs the
;; manifest drift check. Called inside the flat.ss emit block, before the
;; launcher (which renders jb-stdlib-index) and before step 3 (which xxd's the
;; blob).
(define (jb-emit-stdlib-fasls! out)
  (define-values (embedded skips) (jb-compile-stdlib-fasls!))
  (jb-stdlib-manifest-check! (append (map car embedded) (map car skips)))
  (set! jb-stdlib-blob-path (string-append jb-build "/stdlib_fasls.blob"))
  (let ((outp (open-file-output-port jb-stdlib-blob-path (file-options no-fail) (buffer-mode block)))
        (index '())
        (total 0))
    (for-each
      (lambda (nf)
        (let* ((name (car nf)) (bv (read-file-bytes (cdr nf))) (len (bytevector-length bv)))
          (put-bytevector outp bv)
          (set! index (cons (list name total len) index))
          (set! total (+ total len))))
      embedded)
    ;; Empty blob (every candidate skipped): keep the C array non-degenerate so
    ;; the symbol is well-defined and the link is unconditional. The index stays
    ;; empty, so nothing derefs it — behavior is exactly today's source path.
    (when (fx=? total 0)
      (put-bytevector outp (bytevector 0)))
    (close-port outp)
    (set! jb-stdlib-index (reverse index)))
  (put-string out "\n;; === embedded stdlib fasls (per-namespace, in one C-array blob) ===\n")
  (for-each
    (lambda (s)
      (display (string-append "build-jolt: stdlib-fasl skip " (car s) " — " (cdr s) "\n")))
    skips))

(define jb-stdlib-dir (string-append jb-build "/stdlib"))

;; index of needle in s, or #f
(define (jb-str-index s needle)
  (let ((n (string-length s)) (m (string-length needle)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) needle) i)
            (else (loop (+ i 1)))))))

(define (jb-demunge-seg s)
  (list->string (map (lambda (c) (if (char=? c #\_) #\- c)) (string->list s))))

;; Inverse of ns-name->rel for a root-relative path WITH its extension stripped:
;; split on '/' and '.', unmunge each segment '_'->'-', join with '.'.
;; "jolt/backend_scheme" -> "jolt.backend-scheme".
(define (jb-ns-from-rel core)
  (let loop ((cs (string->list core)) (seg '()) (segs '()))
    (cond
      ((null? cs)
       (let ((all (reverse (cons (list->string (reverse seg)) segs))))
         (let join ((xs all) (acc ""))
           (if (null? xs)
               acc
               (let ((piece (jb-demunge-seg (car xs))))
                 (join (cdr xs)
                       (if (string=? acc "") piece (string-append acc "." piece))))))))
      ((or (char=? (car cs) #\/ ) (char=? (car cs) #\.))
       (loop (cdr cs) '() (cons (list->string (reverse seg)) segs)))
      (else (loop (cdr cs) (cons (car cs) seg) segs)))))

;; Filesystem-safe form of an ns name for per-ns .scm/.so under the build dir.
(define (jb-sanitize name)
  (list->string
    (map (lambda (c) (cond ((char=? c #\.) #\_) ((char=? c #\/) #\_) (else c)))
         (string->list name))))

(display "build-jolt: emitting flat source\n")
(let ((out (open-output-file jb-flat-ss 'replace)))
  ;; Bake the version FIRST: rt.ss's jolt-version-string probes this binding via
  ;; top-level-bound?, and load-time consumers (*jolt-version* in
  ;; dynamic-var-defaults.ss) read it while the runtime below loads.
  (put-string out (string-append ";; === baked version ===\n(define jolt-baked-version-early "
                                 (ei-str-lit jb-version) ")\n"))
  ;; full runtime + compiler image: keep the compiler (jolt evals at runtime).
  (bld-emit-runtime out #f #f)
  ;; The build subsystem (build.ss + emit-image.ss + dce.ss, fully inlined) and the
  ;; runtime .ss source embeds it reads are needed ONLY by `jolt build`. As eager
  ;; top-level forms they cost ~45ms at EVERY startup (build.ss defines ~18ms, the
  ;; .ss embed registration ~27ms) — dead weight for run / -e / every non-build
  ;; command. Bake the subsystem as source and the embeds as a thunk, loaded lazily
  ;; on the first `build` (prepare-build! in the launcher). Safe because no boot-path
  ;; code references bld-*/ei-*/dce-*: the runtime eval path uses jolt-compile-eval-
  ;; form, not the emit-image cross-compiler, and .clj namespace loads read the
  ;; embedded-resources table directly (not via build.ss's bld-source-string).
  (put-string out "\n;; === build subsystem (deferred: loaded on first `jolt build`) ===\n")
  (let ((build-src (let ((sp (open-output-string)))
                     (bld-inline-line "(load \"host/chez/build.ss\")" sp 0)
                     (get-output-string sp))))
    (put-string out (string-append "(define jb-build-subsystem-src " (ei-str-lit build-src) ")\n")))
  ;; Register the .ss embeds inside a thunk: the string literals stay compiled into
  ;; the heap but the utf8 allocation + hashtable insert only run when called.
  (put-string out "(define (jb-register-build-embeds!)\n")
  (jb-emit-runtime-embeds out)
  (put-string out "  (if #f #f))\n")
  (put-string out "(define jb-build-subsystem-loaded #f)\n")
  (put-string out
    (string-append
      "(define (jb-load-build-subsystem!)\n"
      "  (unless jb-build-subsystem-loaded\n"
      "    (set! jb-build-subsystem-loaded #t)\n"
      ;; eval the inlined subsystem source form-by-form into the top-level env, so
      ;; build.ss's defines (bld-*, ei-*, dce-*) and its def-var! of jolt.host/
      ;; build-binary land exactly as an eager (load) would have. It has no (load)
      ;; forms left — bld-inline-line already spliced emit-image.ss and dce.ss in.
      "    (let ((p (open-input-string jb-build-subsystem-src)))\n"
      "      (let loop ()\n"
      "        (let ((form (read p)))\n"
      "          (unless (eof-object? form)\n"
      "            (eval form (interaction-environment))\n"
      "            (loop)))))\n"
      "    (jb-register-build-embeds!)))\n"))
  (put-string out "\n;; === embedded jolt-core + stdlib source ===\n")
  (jb-emit-source-embeds out)
  ;; AOT jolt.main + jolt.deps (and their on-demand Clojure closure) as emitted
  ;; Scheme so CLI dispatch never recompiles them from source at startup. Shared
  ;; with make-devboot.ss via bld-emit-cli-aot (build.ss) — one artifact shape;
  ;; the section marker + per-ns emission live there. This replaces the old
  ;; (load-namespace …) calls that paid the ~380ms analyze/emit cost on EVERY start.
  (bld-emit-cli-aot out)
  ;; Embedded stdlib fasls: load+emit+compile each remaining install-owned ns and
  ;; bake its fasl as register-embedded-fasl!, so a require loads compiled code
  ;; instead of recompiling from source. MUST run before flat.ss is written (it
  ;; emits into out) and before the fingerprint step (bytes are part of the hash).
  (jb-emit-stdlib-fasls! out)
  (put-string out "\n;; === jolt launcher ===\n")
  (jb-emit-launcher out)
  (close-port out))

;; --- 1b. bake the runtime fingerprint ----------------------------------------
;; The AOT namespace cache (loader.ss) keys its fasls on the runtime that emitted
;; them, not just the version string — two builds out of one working tree report
;; the same `git describe` and would otherwise share a key. flat.ss IS this
;; binary's runtime + compiler, so its content hash identifies it exactly.
;; Appended rather than emitted inline for the obvious reason: the hash can't be
;; part of what it hashes. Nothing reads it at load time (aot-cache-subdir asks
;; for it on the first cached require), so the position doesn't matter.
(let* ((src (read-file-string jb-flat-ss))
       (fp (string-append (number->string (string-length src) 16) "-"
                          (number->string (aot-content-hash src) 16)))
       (out (open-output-file jb-flat-ss 'append)))
  (put-string out (string-append
                    "\n;; === runtime fingerprint (AOT cache key) ===\n"
                    "(define jolt-baked-runtime-fingerprint " (ei-str-lit fp) ")\n"))
  (close-port out))

;; --- 2. compile + boot in a FRESH Chez (profile Chez settings) --------------
;; jolt is a compiler/REPL: it evals jolt-compiled Scheme at runtime, which must
;; resolve the runtime's top-level procedures (var-deref, jolt-inc, …) through the
;; boot's interaction-environment. compile-file's top-level defines are visible
;; there only when compiled in the REAL interaction-environment, and `error` (and
;; other primitives the inlined runtime references before redefining) bind to the
;; kernel primitive only when compiled against a clean chezscheme env. A fresh
;; Chez process gives both at once — exactly the legacy build-with-cc pass. The
;; in-process compile in build.ss/build-self-contained is for the distributed
;; jolt building (non-eval) apps, where no Chez is available.
(define jb-flat-so (string-append jb-build "/flat.so"))
(define jb-boot (string-append jb-build "/jolt.boot"))
(display (string-append "build-jolt: compiling (" jb-profile " profile)\n"))
(let ((cs (string-append jb-build "/compile.ss")))
  (let ((p (open-output-file cs 'replace)))
    (put-string p
      (string-append
        "(import (chezscheme))\n"
        ;; cross: retarget compile-file / make-boot-file to the target machine.
        (if (bld-cross?) (string-append "(load " (ei-str-lit (bld-xpatch)) ")\n") "")
        ;; level 2 not 3: 3 is unsafe mode (no fx/car type checks) and jolt error
        ;; paths rely on those raising — see bld-chez-param-forms.
        "(optimize-level " (if jb-release? "2" "0") ")\n"
        "(generate-inspector-information " (jb-bool (not jb-release?)) ")\n"
        "(generate-procedure-source-information " (jb-bool (not jb-release?)) ")\n"
        "(debug-on-exception " (jb-bool (not jb-release?)) ")\n"
        "(fasl-compressed " (jb-bool jb-release?) ")\n"
        "(compile-file " (ei-str-lit jb-flat-ss) " " (ei-str-lit jb-flat-so) ")\n"
        "(make-boot-file " (ei-str-lit jb-boot) " '()\n  "
        (ei-str-lit (string-append (bld-csv-dir) "/petite.boot")) "\n  "
        (ei-str-lit (string-append (bld-csv-dir) "/scheme.boot")) "\n  "
        (ei-str-lit jb-flat-so) ")\n"))
    (close-port p))
  (bld-system (string-append bld-chez " --script '" cs "'")))

;; --- 3. embed boots/stub as C arrays + cc-link ------------------------------
;; xxd a file into header H and rename its symbol to NAME / NAME_len.
(define (jb-c-array file h name)
  (bld-system (string-append "xxd -i '" file "' > '" h "'"))
  (bld-system (string-append
    "sed -i.bak -E 's/unsigned char [A-Za-z0-9_]+\\[\\]/unsigned char " name "[]/; "
    "s/unsigned int [A-Za-z0-9_]+_len/unsigned int " name "_len/' '" h "'")))

;; The same header for a file that is not there: main.c's #include and the symbol
;; still have to exist, and a zero length is how jolt-materialize-bundles! tells
;; an absent bundle from a real one (it registers nothing, so the reader sees #f
;; and falls back). Only the compression archives are ever optional.
(define (jb-empty-c-array h name)
  (let ((p (open-output-file h 'replace)))
    (put-string p (string-append
      "unsigned char " name "[] = { 0x00 };\n"
      "unsigned int " name "_len = 0;\n"))
    (close-port p)))

(display "build-jolt: embedding boots + stub, linking\n")
(jb-c-array jb-boot (string-append jb-build "/boot_data.h") "jolt_boot")
(jb-c-array (string-append (bld-csv-dir) "/petite.boot") (string-append jb-build "/petite_data.h") "jolt_petite_boot")
(jb-c-array (string-append (bld-csv-dir) "/scheme.boot") (string-append jb-build "/scheme_data.h") "jolt_scheme_boot")
(jb-c-array jb-stub (string-append jb-build "/stub_data.h") "jolt_stub")
;; Also bundle the Chez kernel (libkernel.a + scheme.h) and the launcher source,
;; so a `build` with :static native libs can re-link a custom stub with those
;; archives baked in — the appended-stub path can't add object code to a prebuilt
;; stub, so it relinks (build.ss bld-relink-stub). Needs the system cc at build.
(jb-c-array (string-append (bld-csv-dir) "/scheme.h") (string-append jb-build "/schemeh_data.h") "jolt_scheme_h")
(jb-c-array (string-append (bld-csv-dir) "/libkernel.a") (string-append jb-build "/libkernel_data.h") "jolt_libkernel_a")
;; ...and the static liblz4.a / libz.a that kernel was built against. That relink
;; is the one link the distributed jolt performs, and it runs where there is no
;; Chez install to take an archive from — without these it resolves lz4 and zlib
;; off the user's machine, so an app with a :static native would carry runtime
;; compression dependencies that no other `jolt build` output has. Together they
;; are ~300K on a ~27M binary.
(for-each
  (lambda (lib)
    (let ((archive (bld-static-archive lib))
          (header (string-append jb-build "/" lib "_data.h"))
          (sym (string-append "jolt_lib" lib "_a")))
      (if archive
          (jb-c-array archive header sym)
          ;; Nothing to embed: this jolt was built against a Chez with no such
          ;; archive (bld-link-libs warned then), and the relink degrades to -l
          ;; as before.
          (jb-empty-c-array header sym))))
  '("lz4" "z"))
(jb-c-array "host/chez/stub/launcher.c" (string-append jb-build "/launcherc_data.h") "jolt_launcher_c")
;; The embedded stdlib fasl blob (one concatenated .so per install-owned ns).
;; jb-emit-stdlib-fasls! wrote it during flat.ss emission; it is absent only when
;; that step never ran, which never happens in a real build. A 1-byte placeholder
;; keeps the symbol non-degenerate when every candidate was skipped (empty index,
;; so nothing derefs it).
(when (and jb-stdlib-blob-path (file-exists? jb-stdlib-blob-path))
  (jb-c-array jb-stdlib-blob-path (string-append jb-build "/stdlib_fasls_data.h") "jolt_stdlib_fasls"))

(define jb-main-c (string-append jb-build "/main.c"))
(let ((mc (open-output-file jb-main-c 'replace)))
  (put-string mc
    (string-append
      "#include \"scheme.h\"\n"
      "#include \"boot_data.h\"\n"
      "#include \"petite_data.h\"\n"
      "#include \"scheme_data.h\"\n"
      "#include \"stub_data.h\"\n"
      "#include \"schemeh_data.h\"\n"
      "#include \"libkernel_data.h\"\n"
      "#include \"lz4_data.h\"\n"
      "#include \"z_data.h\"\n"
      "#include \"launcherc_data.h\"\n"
      "#include \"stdlib_fasls_data.h\"\n"
      "int main(int argc, char *argv[]) {\n"
      "  Sscheme_init(0);\n"
      "  Sregister_boot_file_bytes(\"jolt\", jolt_boot, jolt_boot_len);\n"
      "  Sbuild_heap(0, 0);\n"
      "  int status = Sscheme_start(argc, (const char **)argv);\n"
      "  Sscheme_deinit();\n  return status;\n}\n"))
  (close-port mc))

;; -rdynamic puts the embedded jolt_* boot/stub symbols in the dynamic symbol
;; table so `build` can foreign-entry them to spill the bundled Chez boots. On
;; Linux dlsym can't see executable symbols otherwise (macOS exports them anyway).
(bld-system (string-append
  ;; the embedded jolt_* arrays must be foreign-entry-visible at runtime:
  ;; -rdynamic on ELF; on Windows an exe needs an export table (GetProcAddress).
  (bld-cc) " " (bld-arch-flag) " -O2 " (if (bld-tgt-nt?) "-Wl,--export-all-symbols " "-rdynamic ") "-I'" (bld-csv-dir) "' -I'" jb-build "' '" jb-main-c "' '"
  (bld-csv-dir) "/libkernel.a' -o '" jb-out "' " (bld-link-libs)))
(display (string-append "build-jolt: wrote " jb-out "\n"))
