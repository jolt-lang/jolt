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
;; Re-emit one app namespace to a list of Scheme strings. Like emit-image's
;; ei-emit-ns but WITHOUT the silent (guard ...) wrapper — a form that fails to
;; emit must fail the build, not vanish.
(define (bld-emit-ns ns-name src)
  (let loop ((forms (ei-read-all src)) (acc '()))
    (if (null? forms)
        (reverse acc)
        (let ((f (car forms)))
          (ce-scan-requires! f ns-name)
          (cond
            ((ei-ns-form? f) (loop (cdr forms) acc))
            ((ce-macro-form? f)
             (let-values (((nm fn-form) (ce-defmacro->fn f)))
               (let ((scm (let ((ctx (make-analyze-ctx ns-name)))
                            (jolt-ce-emit (jolt-ce-run-passes (jolt-ce-analyze ctx fn-form) ctx)))))
                 (loop (cdr forms)
                       (cons (string-append
                               "(def-var! " (ei-str-lit ns-name) " " (ei-str-lit nm) "\n  "
                               scm ")\n(mark-macro! "
                               (ei-str-lit ns-name) " " (ei-str-lit nm) ")")
                             acc)))))
            (else
             (let* ((ctx (make-analyze-ctx ns-name))
                    (scm (jolt-ce-emit (jolt-ce-run-passes (jolt-ce-analyze ctx f) ctx))))
               (loop (cdr forms) (cons scm acc)))))))))

;; --- the build --------------------------------------------------------------
;; entry-ns: the app's main namespace (a string). out-path: the binary to write.
;; mode: "dev" | "release" | "optimized". Every form runs through jolt.passes/
;; run-passes (const-fold always; inline + type inference when optimized turns on
;; direct-linking). Deps + source roots are already applied by the caller.
(define (build-binary entry-ns out-path mode)
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
      ;; + scalar-replace passes (closed world); release/dev get const-fold only.
      (set-optimize! (string=? mode "optimized"))
      (let* ((app-strs (apply append
                         (map (lambda (nf) (bld-emit-ns (car nf) (read-file-string (cdr nf))))
                              ordered)))
             (_ (set-optimize! #f))
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
          (put-string out "\n;; === app ===\n")
          (for-each (lambda (s) (put-string out s) (put-string out "\n")) app-strs)
          ;; The launcher runs as Chez's scheme-start (so argv reaches -main —
          ;; top-level boot forms run during heap build, before args are set), and
          ;; suppresses the interactive greeting.
          (put-string out "\n;; === launcher ===\n")
          (put-string out (string-append
                            "(suppress-greeting #t)\n"
                            "(scheme-start\n"
                            "  (lambda args\n"
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
  (lambda (entry out mode)
    (build-binary (jolt-str-render-one entry)
                  (jolt-str-render-one out)
                  (jolt-str-render-one mode))
    jolt-nil))
