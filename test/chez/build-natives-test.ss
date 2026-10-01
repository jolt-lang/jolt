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
;;   f. the system libraries an archive declares (:link-libs, encoded as a
;;      ["link" lib…] entry) follow the archives in the link once each, and leave
;;      the global list (bld-link-libs) rather than appearing twice.
;;   g. the preload links them too: an archive calling into a system library
;;      (sqlite3 on macOS, libcrypt on Linux) resolves with it declared, and on
;;      Linux, where the process has not loaded libcrypt, only then.
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

;; --- (f) ----------------------------------------------------------------------
(define (with-link nats . libs)
  (apply jolt-vector (append (seq->list nats) (list (apply jolt-vector "link" libs)))))
(let ((flags (bld-native-link-flags (with-link (natives "/x/liba.a") "m" "iconv" "m"))))
  (ok "(f) declared libs follow the archives, once each"
      (let ((n (string-length flags)))
        (and (>= n 12) (string=? (substring flags (- n 12) n) " -lm -liconv"))))
  (ok "(f) no static native, no libs" (string=? (bld-native-link-flags (jolt-vector)) "")))
(let ((after (bld-link-libs-after " -lm")))
  (ok "(f) the global list drops what the archives already declared"
      (and (bld-contains? (bld-link-libs) "-lm")
           (not (bld-contains? (string-append after " ") "-lm "))))
  (ok "(f) ...and keeps the rest"
      (string=? (bld-link-libs-after "") (bld-link-libs))))

;; --- (g) ----------------------------------------------------------------------
(define sys-lib
  (case (sa-os-family)
    ((macos) '("sqlite3" "int sqlite3_libversion_number(void);\nint jolt_pl_needs(void) { return sqlite3_libversion_number() > 0 ? 7 : 0; }\n"))
    ((linux) '("crypt" "char *crypt(const char *, const char *);\nint jolt_pl_needs(void) { return crypt(\"a\", \"ab\") ? 7 : 0; }\n"))
    (else #f)))
(when (and sys-lib (bld-have-cc?)
           (sh (string-append "printf 'int main(void){return 0;}' | cc -x c - -l" (car sys-lib)
                              " -o '" work "/probe' 2>/dev/null")))
  (let ((needs (cc-archive! "plneeds" (cadr sys-lib) #t)))
    ;; macOS has every system library in the process already (the shared cache),
    ;; so only Linux can show the preload failing without it; the final link
    ;; fails on both, which static-native-smoke.sh checks
    (when (eq? (sa-os-family) 'linux)
     (let ((dir (string-append work "/g0.build")))
      (bld-mkdir-p dir)
      (ok "(g) without the library declared the preload fails"
          (guard (e (#t #t))
            (let ((p (open-output-file (string-append work "/g0.log") 'replace)))
              (parameterize ((current-output-port p))
                (bld-preload-static-natives! (natives needs) dir))
              (close-port p))
            #f))))
    (let ((dir (string-append work "/g.build")))
      (bld-mkdir-p dir)
      (bld-preload-static-natives! (with-link (natives needs) (car sys-lib)) dir)
      (ok "(g) with it declared the archive resolves at build time"
          (eqv? (entry-value "jolt_pl_needs") 7)))))

(sh (string-append "rm -rf '" work "'"))
(printf "\nbuild natives: ~a passed, ~a failed\n" (- total fails) fails)
(when (> fails 0) (exit 1))
