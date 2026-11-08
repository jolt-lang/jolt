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
;;   2. compile-file + make-boot-file in a fresh Chez (profile Chez settings),
;;      with the stdlib fasls compiling in a second Chez beside it.
;;   3. .incbin the jolt boot + petite/scheme boots + stub (one generated .S),
;;      generate main.c, cc-link -> out-path. The launcher reads the petite/scheme/stub
;;      arrays via FFI on `build` (jolt-materialize-bundles!).

(import (chezscheme))

(load "host/chez/build-boot.ss")  ; the runtime, from target/dev/build-boot.so when fresh
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
;; a bare output name has no directory part (path-parent answers #f): its
;; directory is the current one
(bld-system (string-append "mkdir -p '" (or (path-parent jb-out) ".") "' '" jb-build "'"))

;; --- 0. compile the launcher stub -------------------------------------------
;; A Windows linker names its output "<name>.exe" whatever -o says, so the stub
;; is named that way for a Windows target: a native MSYS2 build only found
;; "launcher" through MSYS's own .exe lookup, and a cross build has none.
(define jb-stub (string-append jb-build (if (bld-tgt-nt?) "/launcher.exe" "/launcher")))
(display "build-jolt: compiling launcher stub\n")
(bld-system (string-append
  (bld-cc) " " (bld-arch-flag) " -O2 -I'" (bld-csv-dir) "' 'host/chez/stub/launcher.c' '"
  (bld-csv-dir) "/libkernel.a' -o '" jb-stub "' " (bld-link-libs)))

;; --- 1. emit flat.ss --------------------------------------------------------
(define jb-flat-ss (string-append jb-build "/flat.ss"))

;; Embed every jolt-core/stdlib source file keyed by its root-relative path
;; ("jolt/main.clj", "clojure/string.clj") — exactly what resolve-on-roots probes
;; — so a built binary can load a namespace with no source on disk.
;;
;; The bytes go into ONE blob (linked into the binary as the C array
;; jolt_source_blob), each file its own bytevector-compress frame, and flat.ss
;; gets only the index. Compressed because the blob is read lazily — a namespace
;; that loads from source, which the boot path never does — so the ratio is free
;; here in a way it is NOT for the boot image, where unpacking outruns readahead
;; and serializes a read that used to overlap the parse. They used to be one
;; (register-embedded-resource! "<path>" (string->utf8 "…")) per file, and a
;; flat.ss top-level form runs at EVERY startup: each start rebuilt ~1.8MB of
;; string literals and allocated a fresh utf8 bytevector from each one, measured
;; at 59ms and +47MB of heap on `jolt --version`, with that heap then paid for
;; again by the Scompact_heap at the end of Sbuild_heap. Nothing on the boot path
;; reads them — they are the fallback for a namespace that is neither in the
;; runtime image nor carries an embedded fasl — so the whole cost was for a table
;; most runs never touch. This is the same move jb-emit-stdlib-fasls! makes for
;; the fasl bytes and the build subsystem makes for its .ss embeds; java/io.ss
;; embedded-source-index is the reading end.
;;
;; FIRST ROOT WINS, because that is what resolve-on-roots does on disk. Two roots
;; can hold the same namespace — a vendored library shipping a host adapter under
;; a jolt name — and the index is a plain hashtable-set! like the old registry
;; was, so emitting both would hand the binary the LAST one and silently disagree
;; with the source tree it was built from.
;; Every producer of embedded text writes through here, so there is one blob and
;; one index: the jolt-core/stdlib .clj source below, the runtime .ss closure the
;; build subsystem reads (jb-emit-runtime-embeds), and the subsystem's own inlined
;; source. Each entry is its own bytevector-compress frame, decompressed by
;; java/io.ss jolt-source-blob-fetch when something actually asks for it.
(define jb-source-blob-path #f)
(define jb-blob-port #f)
(define jb-blob-index '())
(define jb-blob-offset 0)
(define (jb-blob-open!)
  (set! jb-source-blob-path (string-append jb-build "/source_blob.bin"))
  (set! jb-blob-port
        (open-file-output-port jb-source-blob-path (file-options replace) (buffer-mode block)))
  (set! jb-blob-index '())
  (set! jb-blob-offset 0))
(define (jb-blob-add! key text)
  (let* ((bv (parameterize ((compress-format 'gzip))
               (bytevector-compress (string->utf8 text))))
         (n (bytevector-length bv)))
    (put-bytevector jb-blob-port bv)
    (set! jb-blob-index (cons (list key jb-blob-offset n) jb-blob-index))
    (set! jb-blob-offset (+ jb-blob-offset n))))
;; Close the blob and bake the index. The attach form is the ONLY thing about all
;; this that lands in flat.ss, and it has to precede any reader — every reader is
;; at runtime, so emitting it at the end of the assembly is early enough.
(define (jb-blob-close! out)
  (close-port jb-blob-port)
  (put-string out
    (string-append "(jolt-source-blob-attach! '("
      (apply string-append
        (map (lambda (e)
               (string-append "(" (ei-str-lit (car e)) " "
                              (number->string (cadr e)) " "
                              (number->string (caddr e)) ")"))
             (reverse jb-blob-index)))
      "))\n")))

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
                (jb-blob-add! rel (read-file-string abs)))))
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
        ;; a (lazy-load "…") file is inlined into a program that names it, so
        ;; build reads it like any other
        (for-each (lambda (l) (walk (or (bld-load-path l) (bld-lazy-load-path l))))
                  (bld-file-lines path))))
    (for-each (lambda (entry) (when (string? entry) (walk (bld-load-path entry))))
              bld-runtime-manifest)
    (for-each (lambda (kv) (walk (bld-load-path (cdr kv)))) bld-tagged-loads)
    (reverse order)))

;; The (lazy-load "…") files of the runtime, in the order rt.ss declares them.
;; jolt itself carries each one as a fasl in the stdlib blob, named "lazy:PATH"
;; (lazy-shims.ss lazy-shim-load-code!), compiled beside the stdlib namespaces.
(define (jb-collect-lazy-paths)
  (let ((all (jb-collect-load-paths)))
    (let loop ((ps all) (acc '()))
      (if (null? ps)
          (reverse acc)
          (loop (cdr ps)
                (append (reverse (filter values (map bld-lazy-load-path (bld-file-lines (car ps))))) acc))))))
(define (jb-lazy-fasl-name path) (string-append "lazy:" path))
(define (jb-lazy-fasl-name? name)
  (and (> (string-length name) 5) (string=? (substring name 0 5) "lazy:")))

(define (jb-emit-runtime-embeds out)
  (for-each (lambda (path) (jb-blob-add! path (read-file-string path)))
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
        (\"stub/launcher.c\" \"jolt_launcher_c\" \"jolt_launcher_c_len\")
        (\"host/chez/stub/jolt_zlib.h\" \"jolt_zlib_h\" \"jolt_zlib_h_len\")
        (\"host/chez/stub/jolt_code_region.h\" \"jolt_code_region_h\" \"jolt_code_region_h_len\")
        (\"host/chez/stub/jolt_host_faults.h\" \"jolt_host_faults_h\" \"jolt_host_faults_h_len\")))))

(suppress-greeting #t)
;; The collector policy: a nursery sized by the time collection takes (16MB
;; floor, JOLT_GC_TRIP_BYTES pins it) and a heap ceiling matching the JVM's
;; MaxRAMPercentage default (rt.ss jolt-install-gc-policy!). Installed HERE and
;; not at heap-build: it reads syscalls and the environment, both of which
;; belong to the running process rather than the build.
(jolt-install-gc-policy!)
;; The stack depth limit (rt.ss jolt-install-stack-limit!), for the same reason.
(jolt-install-stack-limit!)

(scheme-start
  (lambda args
    (jolt-startup-profile-mark! \"heap built (scheme-start entered)\")
    ;; this process can spawn copies of itself: the async AOT worker's gate
    ;; (loader.ss aot-worker-capable?)
    (set! jolt-standalone-binary #t)
    (set-source-roots! " (ldr-install-roots-str) ")
    (jolt-startup-profile-mark! \"source roots + data readers\")
    ;; JOLT_TRACE at RUNTIME (the env is unset at heap-build), before any app ns
    ;; compiles, so a `-M:run` traces the app's own code.
    (jolt-trace-init-from-env!)
    (jolt-stdlib-fasls-attach! '" (jb-stdlib-index-str) ")
    (jolt-startup-profile-mark! \"trace init + fasl index attach\")
    ;; shared dispatch (cli-core.ss, inlined via the runtime manifest): the -e
    ;; arm, end-of-options, and uncaught reporting are the same code the script
    ;; driver runs — the launcher once carried a stale fork of the -e arm.
    (jolt-cli-run args (lambda () (jolt-materialize-bundles!) (jb-load-build-subsystem!)))
    (jolt-startup-profile-mark! \"cli dispatch\")
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

;; Filled by jb-emit-stdlib-fasls! during flat.ss emission: the namespaces whose
;; fasls go in the blob, in blob order (baked into flat.ss via the launcher's
;; jolt-stdlib-fasls-attach! call), and their (name . so) pairs. The blob itself
;; is assembled after the fasls compile, alongside flat.ss (step 2), and its path
;; set then. #f / () until then.
(define jb-stdlib-blob-path #f)
(define jb-stdlib-index '())
(define jb-stdlib-sos '())

;; Render jb-stdlib-index as a Scheme source list for the launcher's attach call:
;; ("clojure.test" ...). This literal is the only thing baked into flat.ss for the
;; stdlib fasls — small, regardless of how big the blob is.
(define (jb-stdlib-index-str)
  (string-append "("
    (apply string-append (map (lambda (n) (string-append (ei-str-lit n) " ")) jb-stdlib-index))
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
;;
;; This only writes the script; step 2 runs it beside the flat.ss compile.
(define (jb-stdlib-compile-script) (string-append jb-stdlib-dir "/compile-all.ss"))
(define (jb-write-stdlib-compile-script! pairs)
  (let ((cs (jb-stdlib-compile-script)))
    (let ((p (open-output-file cs 'replace)))
      (put-string p
        (string-append
          "(import (chezscheme))\n"
          ;; runtime preamble — the same build-boot.ss build-jolt.ss loads, so
          ;; the macro environment matches what emitted the Scheme.
          "(load \"host/chez/build-boot.ss\")\n"
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
      (close-port p))))

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
                                  ;; the throw a load's CompilerException wraps
                                  (let ((m ((var-deref "jolt.host" "condition-message")
                                            (jolt-load-wrap-origin e))))
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

;; Capture (aot-capture-load) every candidate ns to its own .scm, and write the
;; ONE batch compile script for them. Returns two values: (name . so) that
;; captured OK (in load order) and
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
    ;; the lazy host shims compile from their own source: they are Scheme already
    (for-each
      (lambda (path)
        (let ((so (string-append jb-stdlib-dir "/" (jb-sanitize (jb-lazy-fasl-name path)) ".so")))
          (set! order (cons (cons (jb-lazy-fasl-name path) so) order))
          (set! to-compile (cons (cons path so) to-compile))))
      (jb-collect-lazy-paths))
    (jb-write-stdlib-compile-script! (reverse to-compile))
    (values (reverse order) (reverse skips)
            ;; the captured Scheme is what the fasls are compiled from, so its
            ;; hash stands in for their bytes in the runtime fingerprint
            (aot-content-hash
              (apply string-append
                (map (lambda (sp) (read-file-string (car sp))) (reverse to-compile)))))))

;; Capture the stdlib namespaces (jb-compile-stdlib-fasls!) and record which go
;; in the blob. The blob is linked into the binary as the C array
;; jolt_stdlib_fasls (step 3, below); the names — small — are the ONLY thing
;; baked into flat.ss for the stdlib, via the launcher's jolt-stdlib-fasls-attach!
;; call. Putting the multi-MB fasl bytes into flat.ss as boot-image literals
;; regresses startup (the hard floor), and ei-bytes-lit corrupts them anyway — it
;; round-trips through utf8->string, which arbitrary binary does not survive.
;; Runs the manifest drift check. Called inside the flat.ss emit block, before
;; the launcher (which renders jb-stdlib-index).
;;
;; The fasls compile in step 2, in parallel with flat.ss; nothing in flat.ss
;; depends on their bytes. The fingerprint step hashes flat.ss, so the hash of
;; the Scheme they compile from goes in here, and a stdlib change still moves it.
(define (jb-emit-stdlib-fasls! out)
  (define-values (embedded skips scm-hash) (jb-compile-stdlib-fasls!))
  (jb-stdlib-manifest-check! (append (filter (lambda (n) (not (jb-lazy-fasl-name? n))) (map car embedded))
                                     (map car skips)))
  (set! jb-stdlib-index (map car embedded))
  (set! jb-stdlib-sos embedded)
  (put-string out "\n;; === embedded stdlib fasls (per-namespace, in one C-array blob) ===\n")
  (put-string out (string-append ";; stdlib scheme hash " (number->string scm-hash 16) "\n"))
  (for-each
    (lambda (s)
      (display (string-append "build-jolt: stdlib-fasl skip " (car s) " — " (cdr s) "\n")))
    skips))

;; Concatenate the compiled stdlib fasls into ONE blob file behind a table of
;; (offset, length) pairs, little-endian u64s, one per jb-stdlib-sos entry
;; (java/io.ss jolt-stdlib-fasl-fetch reads it).
(define (jb-write-stdlib-blob!)
  (set! jb-stdlib-blob-path (string-append jb-build "/stdlib_fasls.blob"))
  (let* ((bvs (map (lambda (nf) (read-file-bytes (cdr nf))) jb-stdlib-sos))
         (table (make-bytevector (* 16 (length bvs))))
         (outp (open-file-output-port jb-stdlib-blob-path (file-options no-fail) (buffer-mode block))))
    (let loop ((bvs bvs) (i 0) (offset (bytevector-length table)))
      (when (pair? bvs)
        (let ((len (bytevector-length (car bvs))))
          (bytevector-u64-set! table (* 16 i) offset (endianness little))
          (bytevector-u64-set! table (+ (* 16 i) 8) len (endianness little))
          (loop (cdr bvs) (fx+ i 1) (+ offset len)))))
    (put-bytevector outp table)
    (for-each (lambda (bv) (put-bytevector outp bv)) bvs)
    ;; Empty blob (every candidate skipped): keep the C array non-degenerate so
    ;; the symbol is well-defined and the link is unconditional. The index stays
    ;; empty, so nothing derefs it — behavior is exactly today's source path.
    (when (null? bvs)
      (put-bytevector outp (bytevector 0)))
    (close-port outp)))

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
    ;; ':' too: a lazy shim's entry is "lazy:PATH", and Windows refuses a ':' in a name
    (map (lambda (c) (cond ((char=? c #\.) #\_) ((char=? c #\/) #\_) ((char=? c #\:) #\_) (else c)))
         (string->list name))))

(display "build-jolt: emitting flat source\n")
(let ((out (open-output-file jb-flat-ss 'replace)))
  ;; Bake the version FIRST: rt.ss's jolt-version-string probes this binding via
  ;; top-level-bound?, and load-time consumers (*jolt-version* in
  ;; dynamic-var-defaults.ss) read it while the runtime below loads.
  (jb-blob-open!)
  (put-string out (string-append ";; === baked version ===\n(define jolt-baked-version-early "
                                 (ei-str-lit jb-version) ")\n"))
  ;; full runtime + compiler image: keep the compiler (jolt evals at runtime).
  ;; Its lazy host shims stay lazy, read from the embedded fasls (lazy-shims.ss).
  (set! bld-lazy-shims-mode 'binary)
  (bld-emit-runtime out #f #f)
  (set! bld-lazy-shims-mode 'source)
  ;; The build subsystem (build.ss + emit-image.ss + dce.ss, fully inlined) and the
  ;; runtime .ss source embeds it reads are needed ONLY by `jolt build`. As eager
  ;; top-level forms they cost ~45ms at EVERY startup (build.ss defines ~18ms, the
  ;; .ss embed registration ~27ms) — dead weight for run / -e / every non-build
  ;; command. Bake the subsystem as source and the embeds as a thunk, loaded lazily
  ;; on the first `build` (prepare-build! in the launcher). Safe because no boot-path
  ;; code references bld-*/ei-*/dce-*: the runtime eval path uses jolt-compile-eval-
  ;; form, not the emit-image cross-compiler, and .clj namespace loads read the
  ;; embedded-resources table directly (not via build.ss's bld-source-string).
  (put-string out "\n;; === build subsystem (in the source blob: read on first `jolt build`) ===\n")
  ;; Into the blob, not a literal. A thunk used to defer the utf8 allocation and
  ;; the hashtable insert, but it could not defer the string literals themselves:
  ;; loading flat.so materializes every literal in a code object whether or not
  ;; the code ever runs. The blob defers the bytes, which is what was wanted.
  (let ((build-src (let ((sp (open-output-string)))
                     (bld-inline-line "(load \"host/chez/build.ss\")" sp 0)
                     (get-output-string sp))))
    (jb-blob-add! "@build-subsystem" build-src))
  (jb-emit-runtime-embeds out)
  (put-string out "(define jb-build-subsystem-loaded #f)\n")
  (put-string out
    (string-append
      "(define (jb-load-build-subsystem!)\n"
      "  (unless jb-build-subsystem-loaded\n"
      "    (set! jb-build-subsystem-loaded #t)\n"
      ;; the subsystem source is a blob entry; everything it then reads (the
      ;; runtime .ss closure) is in the same blob, found through the same index.
      "    (let ((jb-build-subsystem-src\n"
      "            (utf8->string (embedded-resource-ref \"@build-subsystem\"))))\n"
      ;; eval the inlined subsystem source form-by-form into the top-level env, so
      ;; build.ss's defines (bld-*, ei-*, dce-*) and its def-var! of jolt.host/
      ;; build-binary land exactly as an eager (load) would have. It has no (load)
      ;; forms left — bld-inline-line already spliced emit-image.ss and dce.ss in.
      "    (let ((p (open-input-string jb-build-subsystem-src)))\n"
      "      (let loop ()\n"
      "        (let ((form (read p)))\n"
      "          (unless (eof-object? form)\n"
      "            (eval form (interaction-environment))\n"
      "            (loop))))))\n"
      "    ))\n"))
  (put-string out "\n;; === embedded jolt-core + stdlib source ===\n")
  (jb-emit-source-embeds out)
  ;; The four marks from here to the launcher close the gap the profile used to
  ;; have. Everything in flat.ss after the runtime manifest still EXECUTES on
  ;; every start — Chez runs a boot file's top level each time the heap is built,
  ;; and there is no saved-heap escape in Chez 10 — so the CLI AOT section and the
  ;; fasl registrations are startup cost, not build cost, and were invisible.
  ;; last producer done: close the blob and bake the index here, so it is in place
  ;; before the CLI AOT section below runs its top levels — the same order the
  ;; eager register-embedded-resource! forms had.
  (jb-blob-close! out)
  (bld-emit-startup-profile-mark! out "source embeds")
  ;; AOT jolt.main + jolt.deps (and their on-demand Clojure closure) as emitted
  ;; Scheme so CLI dispatch never recompiles them from source at startup. Shared
  ;; with make-devboot.ss via bld-emit-cli-aot (build.ss) — one artifact shape;
  ;; the section marker + per-ns emission live there. This replaces the old
  ;; (load-namespace …) calls that paid the ~380ms analyze/emit cost on EVERY start.
  (bld-emit-cli-aot out)
  (bld-emit-startup-profile-mark! out "cli aot (jolt.main + jolt.deps top levels)")
  ;; Embedded stdlib fasls: load+emit+compile each remaining install-owned ns and
  ;; bake its fasl as register-embedded-fasl!, so a require loads compiled code
  ;; instead of recompiling from source. MUST run before flat.ss is written (it
  ;; emits into out) and before the fingerprint step (bytes are part of the hash).
  (jb-emit-stdlib-fasls! out)
  (bld-emit-startup-profile-mark! out "stdlib fasl registrations")
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
;; --- the build-boot image (build-boot.ss) ----------------------------------
;; On a miss, write the image's source and input list here and answer the command
;; that compiles it, which step 2 runs beside the flat.ss compile: ~6s against
;; flat.ss's ~9s, so it costs no wall time, and the next build boots in ~0.4s
;; instead of ~4.8s. #f on a hit, or when the image cannot be cached.
;;
;; Order is what keeps a stale image from reading as fresh. The old image goes
;; first; the input list is hashed BEFORE the compile reads the files, so an edit
;; landing in between leaves the image newer than its list (read as stale, the
;; safe way round); and the image only appears, by rename, once its compile has
;; succeeded. A failed compile leaves no image and the next build falls back to
;; source.
(define jb-build-boot-dir "target/dev")
(define (jb-build-boot-lines)
  ;; the literal block in build-boot.ss, minus comments and blank lines
  (let loop ((ls (bld-file-lines "host/chez/build-boot.ss")) (in? #f) (acc '()))
    (if (null? ls)
        (error 'build-jolt "no build-boot loads block in host/chez/build-boot.ss")
        (let* ((l (car ls))
               (t (let trim ((i 0))
                    (if (and (< i (string-length l)) (memv (string-ref l i) '(#\space #\tab)))
                        (trim (+ i 1))
                        (substring l i (string-length l))))))
          (cond ((string=? t ";; build-boot loads begin") (loop (cdr ls) #t acc))
                ((string=? t ";; build-boot loads end") (reverse acc))
                ((or (not in?) (string=? t "") (char=? (string-ref t 0) #\;)) (loop (cdr ls) in? acc))
                (else (loop (cdr ls) in? (cons t acc))))))))
(define (jb-prepare-build-boot!)
  (and (build-boot-cacheable?)
       (not (gate-boot-image-fresh? build-boot-so build-boot-inputs))
       (let* ((lines (jb-build-boot-lines))
              (pid (number->string (get-process-id)))
              ;; per-process like the rest: make -j builds release and debug at once
              (src (string-append jb-build-boot-dir "/build-boot-image." pid ".ss"))
              (so-tmp (string-append build-boot-so "." pid ".tmp"))
              (cs (string-append jb-build-boot-dir "/build-boot-compile." pid ".ss")))
         (bld-system (string-append "mkdir -p '" jb-build-boot-dir "'"))
         (when (file-exists? build-boot-so) (delete-file build-boot-so))
         ;; the transitive load closure, and build-boot.ss itself: a load added to
         ;; its block changes the image without touching any loaded file
         (let ((seen (make-hashtable string-hash string=?)) (order '()))
           (define (walk path)
             (when (and path (not (hashtable-ref seen path #f)))
               (hashtable-set! seen path #t)
               (set! order (cons path order))
               (for-each (lambda (l) (walk (bld-load-path l))) (bld-file-lines path))))
           (for-each (lambda (l) (walk (bld-load-path l))) lines)
           ;; hashed, not walked: its other load is the image itself
           (set! order (cons "host/chez/build-boot.ss" order))
           ;; the Chez the image is compiled for: another install, or the same
           ;; version rebuilt with a different kernel, has other boot files
           (for-each (lambda (b)
                       (let ((p (string-append bld-host-csv-dir "/" b)))
                         (when (file-exists? p) (set! order (cons p order)))))
                     '("petite.boot" "scheme.boot"))
           (let* ((tmp (string-append build-boot-inputs "." pid ".tmp"))
                  (out (open-output-file tmp 'replace)))
             (for-each (lambda (p) (put-string out (string-append (gate-boot-hash-string p) " " p "\n")))
                       (reverse order))
             (close-port out)
             (when (file-exists? build-boot-inputs) (delete-file build-boot-inputs))
             ;; make -j runs release and debug builds at once, and on Windows a
             ;; rename onto a name the other just wrote raises (MoveFile does not
             ;; replace). Both hashed the same sources, so its file stands.
             (guard (e (#t (when (file-exists? tmp) (delete-file tmp))))
               (rename-file tmp build-boot-inputs))))
         (let ((out (open-output-file src 'replace)))
           (for-each (lambda (l) (bld-inline-line l out 0)) lines)
           (close-port out))
         ;; compiled like make-gateboot's gate.so, in a fresh Chez so `error` and
         ;; the other primitives the runtime shadows bind to the kernel's
         (let ((p (open-output-file cs 'replace)))
           (put-string p
             (string-append
               "(import (chezscheme))\n"
               "(optimize-level 2)\n"
               "(generate-inspector-information #f)\n"
               "(generate-procedure-source-information #f)\n"
               "(debug-on-exception #f)\n"
               "(fasl-compressed #t)\n"
               "(compile-file " (ei-str-lit src) " " (ei-str-lit so-tmp) ")\n"
               ;; as for the inputs above: the other build's image stands
               "(guard (e (#t (delete-file " (ei-str-lit so-tmp) ")))"
               " (rename-file " (ei-str-lit so-tmp) " " (ei-str-lit build-boot-so) "))\n"
               "(delete-file " (ei-str-lit src) ")\n"
               "(delete-file " (ei-str-lit cs) ")\n"))
           (close-port p))
         (string-append bld-chez " --script '" cs "'"))))

(define jb-flat-so (string-append jb-build "/flat.so"))
(define jb-boot (string-append jb-build "/jolt.boot"))
;; the vfasl form of jb-boot — what actually gets embedded (see the conversion
;; in the compile script below for why).
(define jb-vboot (string-append jb-build "/jolt.vboot"))
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
        (ei-str-lit jb-flat-so) ")\n"
        ;; --- vfasl --------------------------------------------------------
        ;; Convert the boot to Chez's "very fast load" format. An ordinary boot
        ;; is a fasl stream the kernel walks object by object, allocating as it
        ;; goes; a vfasl boot is a prebuilt image of what that walk would have
        ;; produced, laid out per space and loaded DIRECTLY INTO THE STATIC
        ;; GENERATION (ChezScheme c/vfasl.c). Both halves of that matter here:
        ;; the load stops allocating, and the Scompact_heap at the end of
        ;; Sbuild_heap has far less to compact, which was 66ms of a 240ms start.
        ;;
        ;; Measured on this boot, `scheme -b <boot> -- --version`, best of 7:
        ;;   fasl    0.25s   14,932,209 bytes
        ;;   vfasl   0.16s   11,593,854 bytes
        ;;
        ;; It runs in THIS script rather than the parent so a cross build gets
        ;; the xpatch's retargeted constants, the same way make-boot-file above
        ;; does — $fasl-to-vfasl lays the image out for a specific machine.
        "(vfasl-convert-file " (ei-str-lit jb-boot) " " (ei-str-lit jb-vboot) " '())\n"))
    (close-port p))
  ;; The stdlib fasls compile in a second Chez beside this one: each process
  ;; spends most of its time alone on one core (the stdlib batch loads the whole
  ;; runtime preamble first), and neither reads the other's output. Both run to
  ;; the end before either failure is reported; the stdlib log is shown after.
  ;; A build-boot image compile (on a miss) is the third: waited for so it does
  ;; not outlive the build, but only a cache, so its failure is a warning.
  (let ((log (string-append jb-stdlib-dir "/compile-all.log"))
        (boot-cmd (jb-prepare-build-boot!))
        (boot-log (string-append jb-build-boot-dir "/build-boot.log")))
    (bld-system (string-append
      "( " (if boot-cmd (string-append boot-cmd " > '" boot-log "' 2>&1 & b=$!; ") "")
      bld-chez " --script '" (jb-stdlib-compile-script) "' > '" log "' 2>&1 & s=$!; "
      bld-chez " --script '" cs "'; f=$?; wait $s; g=$?; cat '" log "'; "
      (if boot-cmd
          (string-append "wait $b || echo \"build-jolt: warning: build-boot image not written, see "
                         boot-log "\"; ")
          "")
      "[ $f -eq 0 ] && [ $g -eq 0 ] )")))
  (jb-write-stdlib-blob!)
  ;; …and re-run just that conversion under gzip if the image it produced is over
  ;; Chez's LZ4 fasl ceiling (build.ss). jolt's own image is nowhere near it, but
  ;; nothing here bounds it, and the failure mode is a binary that dies in
  ;; Sbuild_heap rather than one that boots slowly.
  (bld-vfasl-regzip! jb-build jb-boot jb-vboot))

;; --- 3. embed boots/stub + cc-link -------------------------------------------
;; Each embedded file becomes a symbol NAME (its bytes) and NAME_len (an unsigned
;; int). The bytes go in through `.incbin` in one generated assembly file rather
;; than as `xxd -i` C arrays: those were ~190MB of hex text that cc spent ~11s
;; parsing on every build, plus ~5s of xxd and sed. The assembler copies the file
;; as-is. Header H only declares the array and defines the length, so main.c and
;; the runtime's foreign-entry lookups see the same two symbols as before.
;;
;; The array is const: nothing writes it, and in writable data the boot
;; prefetch's madvise made every page a private dirty copy (14MB a process on
;; macOS); read-only, the pages stay clean and shared with every other jolt
;; running.
(define jb-incbins '())   ; (name . path), emitted into jb-embed-s at link time
(define jb-embed-s (string-append jb-build "/jolt_embed.S"))
(define (jb-c-array file h name)
  (let ((n (call-with-port (open-file-input-port file) port-length)))
    (if (= n 0)
        (jb-empty-c-array h name)
        (let ((p (open-output-file h 'replace)))
          (put-string p (string-append
            "extern const unsigned char " name "[];\n"
            "unsigned int " name "_len = " (number->string n) ";\n"))
          (close-port p)
          (set! jb-incbins (cons (cons name file) jb-incbins))))))

;; The assembly for jb-incbins. It goes through the C preprocessor (.S), so the
;; object format is the compiler's target, not this build host's: a cross build
;; gets the target's section names and symbol prefix for free. The file's
;; position is not part of its identity, so .incbin takes the path as given,
;; relative to the cc's working directory (the repo root, like every other path
;; in the link line); backslashes would be escapes inside the string.
(define (jb-write-embed-s!)
  (let ((p (open-output-file jb-embed-s 'replace)))
    (put-string p (string-append
      "/* generated by host/chez/build-jolt.ss */\n"
      "#ifndef __USER_LABEL_PREFIX__\n"
      "#define __USER_LABEL_PREFIX__\n"
      "#endif\n"
      "#define JOLT_CAT2(a, b) a##b\n"
      "#define JOLT_CAT(a, b) JOLT_CAT2(a, b)\n"
      "#define JOLT_SYM(n) JOLT_CAT(__USER_LABEL_PREFIX__, n)\n"
      "#if defined(__APPLE__)\n"
      "#define JOLT_RODATA .section __TEXT,__const\n"
      "#define JOLT_OBJECT(s, n)\n"
      "#elif defined(_WIN32)\n"
      "#define JOLT_RODATA .section .rdata,\"dr\"\n"
      "#define JOLT_OBJECT(s, n)\n"
      "#else\n"
      "#define JOLT_RODATA .section .rodata\n"
      "#define JOLT_OBJECT(s, n) .type s, %object; .size s, n\n"
      "#endif\n"
      "JOLT_RODATA\n"))
    (for-each
      (lambda (e)
        (let ((sym (string-append "JOLT_SYM(" (car e) ")"))
              (n (call-with-port (open-file-input-port (cdr e)) port-length))
              (path (list->string (map (lambda (c) (if (char=? c #\\) #\/ c))
                                       (string->list (cdr e))))))
          (put-string p (string-append
            ".globl " sym "\n"
            ".balign 16\n"
            "JOLT_OBJECT(" sym ", " (number->string n) ")\n"
            sym ":\n"
            ".incbin " (ei-str-lit path) "\n"))))
      (reverse jb-incbins))
    ;; ELF linkers read a missing note as a request for an executable stack.
    (put-string p (string-append
      "#if !defined(__APPLE__) && !defined(_WIN32)\n"
      ".section .note.GNU-stack,\"\",%progbits\n"
      "#endif\n"))
    (close-port p)))

;; The same header for a file that is not there: main.c's #include and the symbol
;; still have to exist, and a zero length is how jolt-materialize-bundles! tells
;; an absent bundle from a real one (it registers nothing, so the reader sees #f
;; and falls back). Only the compression archives are ever optional.
(define (jb-empty-c-array h name)
  (let ((p (open-output-file h 'replace)))
    (put-string p (string-append
      "const unsigned char " name "[] = { 0x00 };\n"
      "unsigned int " name "_len = 0;\n"))
    (close-port p)))

(display "build-jolt: embedding boots + stub, linking\n")
(jb-c-array jb-vboot (string-append jb-build "/boot_data.h") "jolt_boot")
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
;; The zlib registration header, for the C files `jolt build` generates or
;; relinks from this binary with no checkout on disk (build.ss
;; bld-write-zlib-header!).
(jb-c-array "host/chez/stub/jolt_zlib.h" (string-append jb-build "/joltzlibh_data.h") "jolt_zlib_h")
;; The library stub's code-region header (build.ss bld-write-code-region-header!).
(jb-c-array "host/chez/stub/jolt_code_region.h" (string-append jb-build "/joltcoderegionh_data.h") "jolt_code_region_h")
;; The library stub's host-fault header (build.ss bld-write-host-faults-header!).
(jb-c-array "host/chez/stub/jolt_host_faults.h" (string-append jb-build "/jolthostfaultsh_data.h") "jolt_host_faults_h")
;; The embedded stdlib fasl blob (one concatenated .so per install-owned ns).
;; jb-emit-stdlib-fasls! wrote it during flat.ss emission; it is absent only when
;; that step never ran, which never happens in a real build. A 1-byte placeholder
;; keeps the symbol non-degenerate when every candidate was skipped (empty index,
;; so nothing derefs it).
(when (and jb-stdlib-blob-path (file-exists? jb-stdlib-blob-path))
  (jb-c-array jb-stdlib-blob-path (string-append jb-build "/stdlib_fasls_data.h") "jolt_stdlib_fasls"))
;; The embedded jolt-core/stdlib source, as one blob rather than boot literals
;; (jb-emit-source-embeds). Always written by that step, so no absent-file arm.
(jb-c-array jb-source-blob-path (string-append jb-build "/source_blob_data.h") "jolt_source_blob")

(jb-write-embed-s!)
(define jb-main-c (string-append jb-build "/main.c"))
(let ((mc (open-output-file jb-main-c 'replace)))
  (put-string mc
    (string-append
      "#include \"scheme.h\"\n"
      "#include \"jolt_zlib.h\"\n"
      "#include \"boot_data.h\"\n"
      "#include \"petite_data.h\"\n"
      "#include \"scheme_data.h\"\n"
      "#include \"stub_data.h\"\n"
      "#include \"schemeh_data.h\"\n"
      "#include \"libkernel_data.h\"\n"
      "#include \"lz4_data.h\"\n"
      "#include \"z_data.h\"\n"
      "#include \"launcherc_data.h\"\n"
      "#include \"joltzlibh_data.h\"\n"
      "#include \"joltcoderegionh_data.h\"\n"
      "#include \"jolthostfaultsh_data.h\"\n"
      "#include \"stdlib_fasls_data.h\"\n"
      "#include \"source_blob_data.h\"\n"
      (bld-boot-prefetch-defn)
      "int main(int argc, char *argv[]) {\n"
      ;; before Sscheme_init: the 18MB boot's readahead then overlaps kernel
      ;; init and the runtime image top levels instead of stalling behind them.
      (bld-boot-prefetch-call)
      "  Sscheme_init(0);\n"
      "  Sregister_boot_file_bytes(\"jolt\", (void *)jolt_boot, jolt_boot_len);\n"
      "  Sbuild_heap(0, jolt_register_zlib);\n"
      "  int status = Sscheme_start(argc, (const char **)argv);\n"
      "  Sscheme_deinit();\n  return status;\n}\n"))
  (close-port mc))

;; -rdynamic puts the embedded jolt_* boot/stub symbols in the dynamic symbol
;; table so `build` can foreign-entry them to spill the bundled Chez boots. On
;; Linux dlsym can't see executable symbols otherwise (macOS exports them anyway).
(bld-system (string-append
  ;; the embedded jolt_* arrays must be foreign-entry-visible at runtime:
  ;; -rdynamic on ELF; on Windows an exe needs an export table (GetProcAddress).
  (bld-cc) " " (bld-arch-flag) " -O2 " (if (bld-tgt-nt?) "-Wl,--export-all-symbols " "-rdynamic ") "-I'" (bld-csv-dir) "' -I'" jb-build "' -I'host/chez/stub' '" jb-main-c "' '" jb-embed-s "' '"
  (bld-csv-dir) "/libkernel.a' -o '" jb-out "' " (bld-link-libs)))
(display (string-append "build-jolt: wrote " jb-out "\n"))
