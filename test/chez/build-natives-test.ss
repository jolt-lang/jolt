;; build-natives-test.ss — the build driver's handling of :static natives and of
;; the directories it creates for them, below `jolt build`.
;;
;;   a. a bare JOLT_CHEZ (a name PATH finds, like "scheme") lives where PATH
;;      says. build.ss reads that directory while it loads, through path-parent,
;;      which answers #f for a bare name — and bld-exe-dir handed the #f to
;;      string=?, so loading build.ss at all raised.
;;   b. bld-mkdir-p creates a missing chain and treats #f (no parent: a root or a
;;      bare name) as the end of the walk rather than an argument
;;      (jolt-lang/jolt#1207; the Windows spellings are win-path-test.ss's rows).
;;   c. the build-time preload of :static archives resolves one archive against
;;      another (jolt-lang/jolt#1205). Each archive used to become its own shared
;;      object, so one that calls into a second (libssl.a into libcrypto.a) was
;;      left with unresolved references: a PE loader refuses such a DLL outright,
;;      and an ELF/Mach-O one binding at load time does too when the dependent
;;      archive loads first. The preload is now ONE object built from every
;;      archive, so here the dependent one is listed first.
;;   d. the same archive named twice is linked once — twice force-loaded, it is a
;;      duplicate definition of every symbol in it.
;;   e. a non-PIC archive in the set is skipped with the warning it always got,
;;      and does not take the others' build-time resolution down with it (Linux
;;      with a -no-pie capable cc only: arm64 macOS has no non-PIC code).
;;   f. macOS: the link searches the directories of the libraries the Chez
;;      executable itself links. A Nix Chez links lz4/zlib/ncurses/libiconv out
;;      of /nix/store, none of it on the linker's default path, so -llz4 and then
;;      -lz did not resolve. OS libraries and @rpath entries add nothing, and
;;      Nix's chez is a wrapper script, so the binary it execs is what answers.
;;
;;   chez --script test/chez/build-natives-test.ss
(import (chezscheme))
;; (a): set before build.ss loads, since that is when it reads the directory. "sh"
;; rather than "chez" because every runner has one on PATH.
(putenv "JOLT_CHEZ" "sh")
(putenv "JOLT_CHEZ_CSV" "")
(load "host/chez/gate-boot.ss")
(load "host/chez/cli-core.ss")
(load "host/chez/png.ss")
(load "host/chez/loader.ss")
(load "host/chez/java/ffi.ss")
(load "host/chez/build.ss")

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (if pred (printf "PASS: ~a\n" name)
      (begin (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name))))

(define work (string-append (host-temp-dir) "/jolt-natives-test-" (number->string (get-process-id))))
(define (sh cmd) (zero? (system cmd)))
(define (spit path s)
  (let ((p (open-output-file path 'replace))) (put-string p s) (close-port p)))

;; --- (a) ----------------------------------------------------------------------
(ok "(a) build.ss loads with a bare JOLT_CHEZ" (string? bld-host-csv-dir))
(ok "(a) a bare name's directory is where PATH finds it"
    (equal? (bld-exe-dir "sh") (bld-sh-capture "dirname \"$(command -v sh)\"")))
(ok "(a) a name with a directory keeps it" (equal? (bld-exe-dir "/usr/bin/sh") "/usr/bin"))

;; --- (b) ----------------------------------------------------------------------
(bld-mkdir-p (string-append work "/x/y/z.build"))
(ok "(b) bld-mkdir-p creates the whole chain" (file-directory? (string-append work "/x/y/z.build")))
(ok "(b) bld-mkdir-p ends at #f" (begin (bld-mkdir-p #f) #t))
(ok "(b) bld-mkdir-p leaves a bare existing name alone" (begin (bld-mkdir-p ".") #t))

;; --- (c)-(e) ----------------------------------------------------------------------
(define (natives . archives)
  (apply jolt-vector (map (lambda (a) (jolt-vector "static" "archive" a)) archives)))
(define (cc-archive! name src pic?)
  (let ((c (string-append work "/" name ".c"))
        (o (string-append work "/" name ".o"))
        (a (string-append work "/lib" name ".a")))
    (spit c src)
    (and (sh (string-append "cc " (if pic? "-fPIC" "-fno-pie -fno-PIC") " -c '" c "' -o '" o "'"))
         (sh (string-append "ar rcs '" a "' '" o "'"))
         a)))
(define (entry-value name)
  (and (foreign-entry? name) ((foreign-procedure name () int))))

(if (not (bld-have-cc?))
    (printf "SKIP: (c)-(e) need cc\n")
    (let ((base (cc-archive! "plbase" "int jolt_pl_base(void) { return 40; }\n" #t))
          (dep (cc-archive! "pldep" "int jolt_pl_base(void);\nint jolt_pl_dep(void) { return jolt_pl_base() + 2; }\n" #t)))
      (let ((dir (string-append work "/c.build")))
        (bld-mkdir-p dir)
        ;; the dependent archive FIRST, and base named twice (d)
        (bld-preload-static-natives! (natives dep base base) dir)
        (ok "(c) the dependent archive's symbol resolves at build time" (eqv? (entry-value "jolt_pl_dep") 42))
        (ok "(c) ...and so does the archive it depends on" (eqv? (entry-value "jolt_pl_base") 40)))
      (when (and (eq? (sa-os-family) 'linux)
                 (sh "cc -no-pie -E -x c /dev/null -o /dev/null 2>/dev/null"))
        (let ((nopic (cc-archive! "plnopic"
                                  "const char jolt_pl_greeting[] = \"static\";\nconst char *jolt_pl_nopic(void) { return jolt_pl_greeting; }\nint jolt_pl_other(void) { return 7; }\n"
                                  #f))
              (other (cc-archive! "plother" "int jolt_pl_more(void) { return 9; }\n" #t))
              (dir (string-append work "/e.build"))
              (log (string-append work "/e.log")))
          (bld-mkdir-p dir)
          (let ((p (open-output-file log 'replace)))
            (parameterize ((current-output-port p))
              (bld-preload-static-natives! (natives nopic other) dir))
            (close-port p))
          (let ((out (bld-log-string log)))
            (ok "(e) the non-PIC archive is named in the warning"
                (and (bld-contains? out "is not position-independent")
                     (bld-contains? out "plnopic")))
            (ok "(e) the PIC archive beside it still resolves" (eqv? (entry-value "jolt_pl_more") 9)))))))

(when bld-osx?
  (let* ((bin (string-append work "/fbin"))
         (path (getenv "PATH"))
         (real (string-append bin "/chez.real"))
         (shell (string-append bin "/bash.real"))
         (wrapper (string-append bin "/chez"))
         (spit (lambda (f text)
                 (let ((p (open-output-file f 'replace))) (put-string p text) (close-port p))))
         (want "-L'/nix/store/aaa-lz4-1.10.0-lib/lib' -L'/nix/store/bbb-zlib-1.3.1/lib' "))
    (bld-mkdir-p bin)
    ;; A stand-in otool: files named *.real are Mach-O, anything else is not.
    (spit (string-append bin "/otool")
      "#!/bin/sh\n\
case \"$2\" in *.real) ;; *) echo \"$2: is not an object file\"; exit 1 ;; esac\n\
[ \"$1\" = -h ] && { echo 'Mach header'; exit 0; }\n\
cat <<'EOF'\n\
$2:\n\
\t/nix/store/aaa-lz4-1.10.0-lib/lib/liblz4.1.dylib (compatibility version 1.0.0)\n\
\t/nix/store/bbb-zlib-1.3.1/lib/libz.dylib (compatibility version 1.0.0)\n\
\t/nix/store/bbb-zlib-1.3.1/lib/libz.1.dylib (compatibility version 1.0.0)\n\
\t/usr/lib/libSystem.B.dylib (compatibility version 1.0.0)\n\
\t/System/Library/Frameworks/Foundation.framework/Versions/C/Foundation (compatibility version 300.0.0)\n\
\t@rpath/libfoo.dylib (compatibility version 1.0.0)\n\
EOF\n")
    (spit real "")
    (spit shell "")
    ;; makeWrapper's shape: the #! names a Mach-O shell, the exec line the binary.
    (spit wrapper (string-append "#!" shell "\n\
export SCHEMEHEAPDIRS='" bin "'\n\
exec -a \"$0\" \"" real "\"  \"$@\"\n"))
    (sh (string-append "chmod +x '" bin "/otool' '" wrapper "' '" real "'"))
    (putenv "PATH" (string-append bin ":" path))
    (let ((direct (bld-osx-lib-dir-flags real))
          (wrapped (bld-osx-lib-dir-flags wrapper))
          (cached (bld-osx-chez-lib-dir-flags))
          (libs (bld-link-libs)))
      (putenv "PATH" path)
      (ok "(f) -L for each non-OS library directory the Chez executable links, once each"
          (string=? direct want))
      (ok "(f) a wrapper script answers for the binary it execs, not its #! shell"
          (string=? wrapped want))
      (ok "(f) the -L flags lead the macOS link libraries"
          (and (>= (string-length libs) (string-length cached))
               (string=? (substring libs 0 (string-length cached)) cached))))))

(sh (string-append "rm -rf '" work "'"))
(printf "\nbuild natives: ~a passed, ~a failed\n" (- total fails) fails)
(when (> fails 0) (exit 1))
