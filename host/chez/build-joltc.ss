;; build-joltc.ss — build joltc itself as a self-contained native binary (jolt-eaj).
;;
;;   chez --script host/chez/build-joltc.ss <profile> <out-path>
;;   profile: "release" | "debug"   out-path: e.g. target/release/joltc
;;
;; Runs on a dev/CI machine that HAS Chez + cc. Produces a binary that needs
;; NEITHER: it bakes the full runtime + compiler image + all jolt-core/stdlib
;; source + the Chez petite/scheme boots + a prebuilt launcher stub into one
;; cc-linked executable, so the resulting joltc can run AND `build` jolt apps on
;; its own. joltc itself is cc-linked (not appended) so its signature stays clean
;; for Homebrew/codesign, like dirge's binaries; only the apps it later builds use
;; the appended-stub path (host/chez/build.ss build-self-contained).
;;
;; Pipeline:
;;   0. cc-compile host/chez/stub/launcher.c against the Chez kernel.
;;   1. emit flat.ss = runtime + compiler image (cli.ss load order) + inlined
;;      build.ss + every jolt-core/stdlib file as a baked string literal + the
;;      joltc launcher.
;;   2. in-process compile-file + make-boot-file (profile Chez settings), error
;;      restored around the call (the runtime shadows it; regex.ss/%chez-error).
;;   3. xxd the joltc boot + petite/scheme boots + stub into C arrays, generate
;;      main.c, cc-link -> out-path. The launcher reads the petite/scheme/stub
;;      arrays via FFI on `build` (jolt-materialize-bundles!).

(import (chezscheme))

(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")
(load "host/chez/png.ss")
(load "host/chez/loader.ss")
(load "host/chez/java/ffi.ss")
(set-source-roots! ldr-install-roots)
(load "host/chez/build.ss")   ; bld-* helpers, ei-* (emit-image), dce

(define jb-args (cdr (command-line)))
(define jb-profile (if (pair? jb-args) (car jb-args) "release"))
(define jb-out (if (and (pair? jb-args) (pair? (cdr jb-args))) (cadr jb-args)
                   (string-append "target/" jb-profile "/joltc")))
(define jb-release? (string=? jb-profile "release"))
(unless (or jb-release? (string=? jb-profile "debug"))
  (error 'build-joltc "profile must be \"release\" or \"debug\"" jb-profile))

;; Cross-compilation: an optional 3rd arg is the target Chez machine, and the
;; target pack comes from $JOLT_TARGET_PACK — cross-builds joltc itself for
;; another platform (e.g. restoring the x86_64-macos release artifact from an
;; arm64 runner). The bld-* helpers below key off these parameters (build.ss).
(when (and (pair? jb-args) (pair? (cdr jb-args)) (pair? (cddr jb-args)))
  (let ((tgt (caddr jb-args)) (pack (getenv "JOLT_TARGET_PACK")))
    (when (and tgt (> (string-length tgt) 0))
      (unless (and pack (> (string-length pack) 0))
        (error 'build-joltc "cross build (target arg) needs $JOLT_TARGET_PACK — see tools/cross-compile/README.md"))
      (bld-target tgt)
      (bld-target-pack pack))))

;; Version baked into the binary's saved heap. Prefer $JOLT_VERSION (CI sets it to
;; the release tag); else derive it from git in this checkout; else "dev".
(define jb-version
  (let ((env (getenv "JOLT_VERSION")))
    (if (and env (> (string-length env) 0))
        env
        (let ((s (bld-sh-capture "git describe --tags --always --dirty 2>/dev/null")))
          (if (> (string-length s) 0) s "dev")))))

(define jb-build (string-append jb-out ".build"))
(bld-check-toolchain)
(bld-system (string-append "mkdir -p '" (path-parent jb-out) "' '" jb-build "'"))

;; --- 0. compile the launcher stub -------------------------------------------
(define jb-stub (string-append jb-build "/launcher"))
(display "build-joltc: compiling launcher stub\n")
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
(define (jb-emit-source-embeds out)
  (for-each
    (lambda (root)
      (for-each
        (lambda (rp)
          (let ((rel (car rp)) (abs (cdr rp)))
            (when (or (str-suffix? rel ".clj") (str-suffix? rel ".cljc"))
              (put-string out (string-append
                "(register-embedded-resource! " (ei-str-lit rel) " "
                (ei-bytes-lit (read-file-string abs)) ")\n")))))
        (bld-walk-files root "" '())))
    ldr-install-roots))

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
(define (jolt-materialize-bundles!)
  (load-shared-object #f)
  (let ((memcpy (foreign-procedure \"memcpy\" (u8* uptr uptr) void*)))
    (for-each
      (lambda (spec)
        (let* ((len (foreign-ref 'unsigned-int (foreign-entry (caddr spec)) 0))
               (bv (make-bytevector len)))
          (memcpy bv (foreign-entry (cadr spec)) len)
          (register-embedded-bytes! (car spec) bv)))
      '((\"csv/petite.boot\" \"jolt_petite_boot\" \"jolt_petite_boot_len\")
        (\"csv/scheme.boot\" \"jolt_scheme_boot\" \"jolt_scheme_boot_len\")
        (\"stub/launcher\" \"jolt_stub\" \"jolt_stub_len\")
        (\"csv/scheme.h\" \"jolt_scheme_h\" \"jolt_scheme_h_len\")
        (\"csv/libkernel.a\" \"jolt_libkernel_a\" \"jolt_libkernel_a_len\")
        (\"stub/launcher.c\" \"jolt_launcher_c\" \"jolt_launcher_c_len\")))))

(suppress-greeting #t)
;; GC tuning: larger nursery for allocation-heavy workloads. Default 16 MB;
;; override via JOLT_GC_TRIP_BYTES env (integer bytes).
(collect-trip-bytes
  (let ((trip (getenv \"JOLT_GC_TRIP_BYTES\"))
        (default (* 16 1024 1024)))
    (if trip (or (string->number trip) default) default)))
(scheme-start
  (lambda args
    (set-source-roots! " (ldr-install-roots-str) ")
    ;; JOLT_TRACE at RUNTIME (the env is unset at heap-build), before any app ns
    ;; compiles, so a `-M:run` traces the app's own code.
    (jolt-trace-init-from-env!)
    ;; shared dispatch (cli-core.ss, inlined via the runtime manifest): the -e
    ;; arm, end-of-options, and uncaught reporting are the same code the script
    ;; driver runs — the launcher once carried a stale fork of the -e arm.
    (jolt-cli-run args (lambda () (jolt-materialize-bundles!) (jb-load-build-subsystem!)))
    (exit 0)))
")))

;; --- AOT the CLI entry closure ----------------------------------------------
;; jolt.main + jolt.deps and their on-demand Clojure require closure (clojure.string,
;; clojure.edn, jolt.mvn-http, …) used to be baked as top-level (load-namespace …)
;; forms. flat.so is a Chez boot file whose top-level forms re-execute at every
;; Sbuild_heap (every process start), so those load-namespace calls re-analyzed and
;; re-emitted the whole graph from Clojure source on EVERY invocation — ~380ms, about
;; 70% of joltc's startup floor. Instead we emit their Scheme HERE, at build time,
;; via the same emit-image path an app build uses, so at boot the vars are defined by
;; running compiled Scheme (a few ms) exactly like the rest of the runtime image.
;;
;; The runtime + compiler image + clojure.core are already emitted above (bld-emit-
;; runtime). We load jolt.main in THIS build process to populate the compiler's
;; registries, capturing the on-demand load order via the loader's ns-loaded-hook —
;; which fires only for namespaces NOT already in the image, i.e. exactly the CLI's
;; on-demand closure. Each ns is emitted var-routed (prelude mode, direct-link OFF)
;; so its runtime behavior is identical to the interpreted load it replaces. A form
;; that fails to emit fails the build (bld-emit-ns is strict), same as an app build.
;; jolt.deps's lazy in-fn (require 'clojure.data.json) is not on the load path here,
;; so it stays load-on-demand at runtime — unchanged.
(define (jb-emit-cli-ns out)
  (let ((order '()))
    (set-ns-loaded-hook! (lambda (name file) (set! order (cons (cons name file) order))))
    (load-namespace "jolt.main")
    (load-namespace "jolt.deps")
    (set-ns-loaded-hook! (lambda (name file) #f))
    (let ((ordered (reverse order)))   ; deps complete loading before requirers -> deps-first
      (when (null? ordered)
        (error 'build-joltc "no CLI namespace captured for jolt.main — is jolt-core on the source roots?"))
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

(display "build-joltc: emitting flat source\n")
(let ((out (open-output-file jb-flat-ss 'replace)))
  ;; Bake the version FIRST: rt.ss's jolt-version-string probes this binding via
  ;; top-level-bound?, and load-time consumers (*jolt-version* in
  ;; dynamic-var-defaults.ss) read it while the runtime below loads.
  (put-string out (string-append ";; === baked version ===\n(define jolt-baked-version-early "
                                 (ei-str-lit jb-version) ")\n"))
  ;; full runtime + compiler image: keep the compiler (joltc evals at runtime).
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
  ;; Scheme so CLI dispatch never recompiles them from source at startup. See
  ;; jb-emit-cli-ns — this replaces the old (load-namespace …) calls that paid the
  ;; ~380ms analyze/emit cost on EVERY process start.
  (put-string out "\n;; === AOT jolt.main + jolt.deps (emitted Scheme) ===\n")
  (jb-emit-cli-ns out)
  (put-string out "\n;; === joltc launcher ===\n")
  (jb-emit-launcher out)
  (close-port out))

;; --- 2. compile + boot in a FRESH Chez (profile Chez settings) --------------
;; joltc is a compiler/REPL: it evals jolt-compiled Scheme at runtime, which must
;; resolve the runtime's top-level procedures (var-deref, jolt-inc, …) through the
;; boot's interaction-environment. compile-file's top-level defines are visible
;; there only when compiled in the REAL interaction-environment, and `error` (and
;; other primitives the inlined runtime references before redefining) bind to the
;; kernel primitive only when compiled against a clean chezscheme env. A fresh
;; Chez process gives both at once — exactly the legacy build-with-cc pass. The
;; in-process compile in build.ss/build-self-contained is for the distributed
;; joltc building (non-eval) apps, where no Chez is available.
(define jb-flat-so (string-append jb-build "/flat.so"))
(define jb-boot (string-append jb-build "/joltc.boot"))
(define jb-bool (lambda (b) (if b "#t" "#f")))
(display (string-append "build-joltc: compiling (" jb-profile " profile)\n"))
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

(display "build-joltc: embedding boots + stub, linking\n")
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
(jb-c-array "host/chez/stub/launcher.c" (string-append jb-build "/launcherc_data.h") "jolt_launcher_c")

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
      "#include \"launcherc_data.h\"\n"
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
(display (string-append "build-joltc: wrote " jb-out "\n"))
