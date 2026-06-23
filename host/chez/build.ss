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

;; --- shell helpers ----------------------------------------------------------
;; Run a command, return its stdout as one trimmed string ("" on no output).
(define (bld-sh-capture cmd)
  (let* ((p (process cmd)) (in (car p)))
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
  (let ((rc (system cmd)))
    (unless (zero? rc)
      (error 'jolt-build (string-append "command failed (" (number->string rc) "): " cmd)))))

(define (bld-contains? s sub)
  (let ((ns (string-length s)) (nsub (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i nsub) ns) #f)
            ((string=? (substring s i (+ i nsub)) sub) #t)
            (else (loop (+ i 1)))))))

;; --- toolchain discovery ----------------------------------------------------
(define bld-machine (symbol->string (machine-type)))
(define bld-osx? (bld-contains? bld-machine "osx"))

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

;; The csv<ver>/<machine> dir holding scheme.h, libkernel.a, *.boot. Derived from
;; the chez executable's location; JOLT_CHEZ_CSV overrides.
(define bld-csv-dir
  (let ((env (getenv "JOLT_CHEZ_CSV")))
    (or (and env (> (string-length env) 0) env)
        (let* ((bindir (bld-sh-capture "dirname \"$(command -v chez || command -v scheme || command -v petite)\""))
               (cand (string-append bindir "/../lib/csv" bld-version "/" bld-machine)))
          cand))))

(define (bld-check-toolchain)
  (for-each
    (lambda (f)
      (let ((p (string-append bld-csv-dir "/" f)))
        (unless (file-exists? p)
          (error 'jolt-build (string-append "Chez build file missing: " p
                                             "\nSet JOLT_CHEZ_CSV to the csv<ver>/<machine> dir.")))))
    '("scheme.h" "libkernel.a" "petite.boot" "scheme.boot")))

;; Link flags. macOS Homebrew layout for the kernel's lz4/zlib/ncurses deps.
(define (bld-link-libs)
  (if bld-osx?
      (let ((lz4 (bld-sh-capture "brew --prefix lz4 2>/dev/null")))
        (string-append
          (if (> (string-length lz4) 0) (string-append "-L" lz4 "/lib ") "")
          "-llz4 -lz -lncurses -framework Foundation -liconv -lm"))
      ;; Linux: the Chez kernel pulls in compression (lz4/z), the expression
      ;; editor (ncurses + terminfo), threads, dlopen, libuuid, and clock_gettime.
      "-llz4 -lz -lncurses -ltinfo -ldl -lm -lpthread -luuid -lrt"))

;; --- runtime manifest (mirrors host/chez/cli.ss's load order) ---------------
(define bld-runtime-manifest
  (list
    "(load \"host/chez/rt.ss\")"
    "(set-chez-ns! \"clojure.core\")"
    "(load \"host/chez/seed/prelude.ss\")"
    "(load \"host/chez/post-prelude.ss\")"
    "(set-chez-ns! \"user\")"
    "(load \"host/chez/host-contract.ss\")"
    "(load \"host/chez/seed/image.ss\")"
    "(load \"host/chez/compile-eval.ss\")"
    "(load \"host/chez/png.ss\")"
    "(load \"host/chez/loader.ss\")"
    "(load \"host/chez/ffi.ss\")"
    "(set-source-roots! (list \"jolt-core\" \"stdlib\"))"))

;; A single-line top-level `(load "PATH")` -> PATH, else #f.
(define (bld-load-path line)
  (let ((s (let trim ((i 0))
             (if (and (< i (string-length line))
                      (memv (string-ref line i) '(#\space #\tab)))
                 (trim (+ i 1))
                 (substring line i (string-length line))))))
    (and (>= (string-length s) 7)
         (string=? (substring s 0 6) "(load ")
         (let* ((q1 (let scan ((i 6)) (if (char=? (string-ref s i) #\") i (scan (+ i 1)))))
                (q2 (let scan ((i (+ q1 1))) (if (char=? (string-ref s i) #\") i (scan (+ i 1))))))
           (substring s (+ q1 1) q2)))))

(define (bld-file-lines path)
  (call-with-input-file path
    (lambda (p)
      (let loop ((acc '()))
        (let ((l (get-line p)))
          (if (eof-object? l) (reverse acc) (loop (cons l acc))))))))

;; Emit one line to OUT, recursively inlining a `(load ...)` of a repo file.
(define (bld-inline-line line out depth)
  (when (> depth 50) (error 'jolt-build "load nesting too deep"))
  (let ((p (bld-load-path line)))
    (if p
        (for-each (lambda (l) (bld-inline-line l out (+ depth 1))) (bld-file-lines p))
        (begin (put-string out line) (put-string out "\n")))))

(define (bld-emit-runtime out)
  (for-each (lambda (l) (bld-inline-line l out 0)) bld-runtime-manifest))

;; --- app emission -----------------------------------------------------------
;; Re-emit one app namespace to a list of Scheme strings: optimize (run-passes)
;; and stay strict — a form that fails to emit must fail the build, not vanish.
;; The loop itself is emit-image's ei-emit-ns* (optimize? #t, guard? #f).
(define (bld-emit-ns ns-name src) (ei-emit-ns* ns-name src #t #f))

;; --- bundling: native libs + resources --------------------------------------
;; A jolt seq of jolt strings -> a Scheme list of Scheme strings.
(define (bld-strs x) (map jolt-str-render-one (seq->list x)))

;; Emit native-library loads. `natives` is the encoded jolt seq jolt.main/
;; encode-natives produced: each entry is ["process"] | ["req" cand…] | ["opt" cand…].
;; `which` selects 'required (process + req) or 'optional. Required + process loads
;; are emitted before the app forms (the app's defcfn foreign-procedures resolve
;; their symbols at top-level eval during startup, so the libs must be loaded
;; first); a load-shared-object failure there is fatal — correct for a required
;; lib. Optional loads run in the scheme-start launcher, where guard catches a
;; missing lib (an optional lib's namespace is only present when the app requires
;; it, so its foreign-procedures aren't among the baked top-level forms).
(define (bld-emit-natives out natives which)
  (for-each
    (lambda (entry)
      (let* ((parts (bld-strs entry)) (kind (car parts)) (cands (cdr parts))
             (cand-lits (fold-left (lambda (s c) (string-append s (ei-str-lit c) " ")) "" cands)))
        (cond
          ((and (eq? which 'required) (string=? kind "process"))
           (put-string out "(jolt-build-load-native '() #f #t)\n"))
          ((and (eq? which 'required) (string=? kind "req"))
           (put-string out (string-append "(jolt-build-load-native (list " cand-lits ") #f #f)\n")))
          ((and (eq? which 'optional) (string=? kind "opt"))
           (put-string out (string-append "(jolt-build-load-native (list " cand-lits ") #t #f)\n"))))))
    (seq->list natives)))

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
;; the app forms so the (read-file-string ABSPATH) runs at heap build — the file's
;; contents bake into the boot image and io/resource serves them with no file on
;; disk. ABSPATH only has to exist at build time.
(define (bld-emit-embeds out embed-dirs)
  (for-each
    (lambda (root)
      (when (file-directory? root)
        (for-each
          (lambda (rp)
            (put-string out (string-append
                              "(register-embedded-resource! " (ei-str-lit (car rp))
                              " (read-file-string " (ei-str-lit (cdr rp)) "))\n")))
          (bld-walk-files root "" '()))))
    (bld-strs embed-dirs)))

;; --- the build --------------------------------------------------------------
;; entry-ns: the app's main namespace (a string). out-path: the binary to write.
;; mode: "dev" | "release" | "optimized". Every form runs through jolt.passes/
;; run-passes (const-fold always; inline + type inference when optimized turns on
;; direct-linking). Deps + source roots are already applied by the caller.
;; natives: encoded :jolt/native libs to load at startup. embed-dirs: dirs whose
;; files bake into the binary (single-file). ext-roots: project-relative io/resource
;; roots resolved at runtime against JOLT_PWD (ship-alongside resources).
;; direct-link?: opt-in closed-world direct-linking (app->app calls bind directly,
;; no runtime redefinition). Off by default in every mode — release stays
;; dynamically linked.
(define (build-binary entry-ns out-path mode natives embed-dirs ext-roots direct-link?)
  (bld-check-toolchain)
  ;; 1. record app namespaces in dependency order as they finish loading.
  (let ((app-order '()))
    (set-ns-loaded-hook!
      (lambda (name file) (set! app-order (cons (cons name file) app-order))))
    (load-namespace entry-ns)
    (set-ns-loaded-hook! (lambda (name file) #f))
    (let ((ordered (reverse app-order)))   ; deps first, entry last
      (when (null? ordered)
        (error 'jolt-build (string-append "no source namespace loaded for " entry-ns
                                          " — is it on the source roots?")))
      ;; 2. emit each app namespace. `optimized` turns on the inference + flatten
      ;; + scalar-replace passes; release/dev get const-fold only.
      ;; direct-link? (opt-in) commits to a closed world: app->app calls bind
      ;; directly, giving up runtime redefinition of those vars. Off by default in
      ;; every mode. The defined-set accumulates across the dependency-ordered
      ;; namespaces, so a dep's defs are direct-linkable by the time the entry that
      ;; calls them is emitted.
      (set-optimize! (string=? mode "optimized"))
      (when direct-link?
        ((var-deref "jolt.backend-scheme" "set-direct-link!") #t)
        ((var-deref "jolt.backend-scheme" "direct-link-reset!")))
      (let* ((app-strs (apply append
                         (map (lambda (nf) (bld-emit-ns (car nf) (read-file-string (cdr nf))))
                              ordered)))
             (_ (set-optimize! #f))
             (_ ((var-deref "jolt.backend-scheme" "set-direct-link!") #f))
             (builddir (string-append out-path ".build"))
             (flat-ss  (string-append builddir "/flat.ss"))
             (flat-so  (string-append builddir "/flat.so"))
             (boot     (string-append builddir "/jolt.boot"))
             (boot-h   (string-append builddir "/boot_data.h"))
             (main-c   (string-append builddir "/main.c")))
        (bld-system (string-append "mkdir -p '" builddir "'"))
        ;; 3. flat source = runtime + app + launcher.
        (let ((out (open-output-file flat-ss 'replace)))
          (bld-emit-runtime out)
          ;; Load native libs, bake embedded resources, and point source roots at
          ;; the build-time app roots — all BEFORE the app forms. The app's
          ;; top-level forms run at binary startup (Sbuild_heap), and they include
          ;; foreign-procedure evals (a library's defcfn) and (slurp (io/resource …))
          ;; reads. So the libraries must be loaded and resources resolvable by the
          ;; time those forms run, not later in the scheme-start launcher.
          (put-string out "\n;; === native libraries (required) ===\n")
          (bld-emit-natives out natives 'required)
          (put-string out "\n;; === embedded resources ===\n")
          (bld-emit-embeds out embed-dirs)
          (put-string out (string-append
                            "(set-source-roots! (list "
                            (fold-left (lambda (s r) (string-append s (ei-str-lit r) " ")) ""
                                       (get-source-roots))
                            "))\n"))
          (put-string out "\n;; === app ===\n")
          (for-each (lambda (s) (put-string out s) (put-string out "\n")) app-strs)
          ;; The launcher runs as Chez's scheme-start (so argv reaches -main —
          ;; top-level boot forms run during heap build, before args are set), and
          ;; suppresses the interactive greeting. It resets source roots to the
          ;; app's resource dirs resolved against JOLT_PWD (or cwd) so a runtime
          ;; io/resource that wasn't embedded still resolves next to the binary.
          (put-string out "\n;; === launcher ===\n")
          (put-string out "(suppress-greeting #t)\n")
          (put-string out "(scheme-start\n  (lambda args\n")
          (bld-emit-natives out natives 'optional)
          (put-string out (string-append
                            "    (let ((base (or (getenv \"JOLT_PWD\") \".\")))\n"
                            "      (set-source-roots!\n"
                            "        (append (map (lambda (r) (string-append base \"/\" r)) (list "
                            (fold-left (lambda (s r) (string-append s (ei-str-lit r) " ")) "" (bld-strs ext-roots))
                            "))\n"
                            "                (list \"jolt-core\" \"stdlib\"))))\n"))
          (put-string out (string-append
                            "    (let ((mainv (var-deref " (ei-str-lit entry-ns) " \"-main\")))\n"
                            "      (apply jolt-invoke mainv args))\n"
                            "    (exit 0)))\n"))
          (close-port out))
        ;; 4. compile -> boot -> embed -> link.
        ;; compile-file/make-boot-file run in a FRESH Chez, not this process: the
        ;; loaded runtime shadows `error` (regex.ss, for irregex), which would
        ;; otherwise bake a broken `error` reference into the boot.
        (display (string-append "jolt build: compiling " entry-ns " (" mode " mode)\n"))
        (let ((cs (string-append builddir "/compile.ss")))
          (let ((p (open-output-file cs 'replace)))
            (put-string p
              (string-append
                "(import (chezscheme))\n"
                "(compile-file " (ei-str-lit flat-ss) " " (ei-str-lit flat-so) ")\n"
                "(make-boot-file " (ei-str-lit boot) " '()\n  "
                (ei-str-lit (string-append bld-csv-dir "/petite.boot")) "\n  "
                (ei-str-lit (string-append bld-csv-dir "/scheme.boot")) "\n  "
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
        (bld-system (string-append
          "cc -O2 -I'" bld-csv-dir "' '" main-c "' '" bld-csv-dir "/libkernel.a' "
          "-o '" out-path "' " (bld-link-libs)))
        (display (string-append "jolt build: wrote " out-path "\n"))))))

(def-var! "jolt.host" "build-binary"
  (lambda (entry out mode natives embed-dirs ext-roots direct-link?)
    (build-binary (jolt-str-render-one entry)
                  (jolt-str-render-one out)
                  (jolt-str-render-one mode)
                  natives embed-dirs ext-roots (jolt-truthy? direct-link?))
    jolt-nil))
