;; make-devboot.ss — precompile the runtime to target/dev/flat.so for dev boot cache.
;;
;;   chez --script host/chez/make-devboot.ss
;;
;; Two-phase (same as build-joltc steps 1-2):
;;   1. emit flat.ss (runtime + compiler + embeds) + a compile helper into target/dev/
;;   2. run the helper in a FRESH Chez, so `error` and other shadowed primitives
;;      resolve to the kernel bindings before the runtime redefines them.

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
(load "host/chez/build.ss")

(define jb-build "target/dev")
(bld-system (string-append "mkdir -p '" jb-build "'"))

(define (str-suffix? s suf)
  (let ((n (string-length s)) (m (string-length suf)))
    (and (>= n m) (string=? (substring s (- n m) n) suf))))

;; --- collect inputs (same algorithm as build-joltc's jb-collect-load-paths) ---
(define (db-collect-load-paths)
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

;; --- write input list (after emit, not after compile, so failures don't skip) ---
(define db-input-file (string-append jb-build "/flat.inputs"))
(define db-paths (db-collect-load-paths))

;; --- 1. emit flat.ss ---------------------------------------------------------
(define jb-flat-ss (string-append jb-build "/flat.ss"))
(define jb-flat-so (string-append jb-build "/flat.so"))
(display "make-devboot: emitting flat source\n")
(let ((out (open-output-file jb-flat-ss 'replace)))
  ;; Full runtime + compiler image.
  (bld-emit-runtime out #f #f)
  ;; build.ss inlined (for `jolt build` from the cache).
  (put-string out "\n;; === embedded build driver ===\n")
  (bld-inline-line "(load \"host/chez/build.ss\")" out 0)
  ;; Runtime source embeds (bytevector values, 1B/char).
  (put-string out "\n;; === embedded runtime source ===\n")
  (for-each (lambda (path)
              (put-string out
                (string-append
                  "(register-embedded-resource! " (ei-str-lit path) " "
                  (ei-bytes-lit (read-file-string path)) ")\n")))
            db-paths)
  ;; jolt-core + stdlib source embeds (bytevector values, 1B/char).
  (put-string out "\n;; === embedded jolt-core + stdlib source ===\n")
  (for-each
    (lambda (root)
      (for-each
        (lambda (rp)
          (let ((rel (car rp)) (abs (cdr rp)))
            (when (or (str-suffix? rel ".clj") (str-suffix? rel ".cljc"))
              (put-string out
                (string-append
                  "(register-embedded-resource! " (ei-str-lit rel) " "
                  (ei-bytes-lit (read-file-string abs)) ")\n")))))
        (bld-walk-files root "" '())))
    ldr-install-roots)
  ;; Preload jolt.main + jolt.deps into the image.
  (put-string out "\n;; === AOT jolt.main + jolt.deps ===\n")
  (put-string out "(load-namespace \"jolt.main\")\n")
  (put-string out "(load-namespace \"jolt.deps\")\n")
  (close-port out))

;; --- write input list (before compile, so the list is always consistent) ------
(display "make-devboot: writing input list\n")
(let ((out (open-output-file db-input-file 'replace))
      (clj-files '()))
  (for-each (lambda (p) (put-string out p) (put-string out "\n")) db-paths)
  ;; Also list all .clj/.cljc source files.
  (for-each
    (lambda (root)
      (for-each
        (lambda (rp)
          (let ((rel (car rp)))
            (when (or (str-suffix? rel ".clj") (str-suffix? rel ".cljc"))
              (put-string out (cdr rp)) (put-string out "\n"))))
        (bld-walk-files root "" '())))
    ldr-install-roots)
  (close-port out))

;; --- 2. compile in a FRESH Chez (same approach as build-joltc step 2) ---------
;; compile-file must run against a clean chezscheme env so `error` and other
;; primitives the runtime shadows bind to the kernel versions.
(display "make-devboot: compiling flat.so (fresh Chez)\n")
;; Compile to a temp path and rename into place: a concurrent bin/joltc (e.g.
;; parallel make ci gates) must never load a partially written image — a
;; truncated fasl can load a prefix of the runtime and fail on late defines.
(define jb-flat-so-tmp (string-append jb-flat-so ".tmp"))
(let ((cs (string-append jb-build "/dev-compile.ss")))
  (let ((p (open-output-file cs 'replace)))
    (put-string p
      (string-append
        "(import (chezscheme))\n"
        "(optimize-level 2)\n"
        "(generate-inspector-information #f)\n"
        "(generate-procedure-source-information #f)\n"
        "(debug-on-exception #f)\n"
        "(fasl-compressed #t)\n"
        "(compile-file " (ei-str-lit jb-flat-ss) " " (ei-str-lit jb-flat-so-tmp) ")\n"))
    (close-port p))
  (bld-system (string-append bld-chez " --script '" cs "'")))
(when (file-exists? jb-flat-so) (delete-file jb-flat-so))
(rename-file jb-flat-so-tmp jb-flat-so)

(display (string-append "make-devboot: wrote " jb-flat-so "\n"))
