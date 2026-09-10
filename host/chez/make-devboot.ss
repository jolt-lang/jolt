;; make-devboot.ss — precompile the runtime to target/dev/flat.so for dev boot cache.
;;
;;   chez --script host/chez/make-devboot.ss
;;
;; Two-phase (same as build-jolt steps 1-2):
;;   1. emit flat.ss (runtime + compiler + embeds) + a compile helper into target/dev/
;;   2. run the helper in a FRESH Chez, so `error` and other shadowed primitives
;;      resolve to the kernel bindings before the runtime redefines them.

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
;; loaded HERE, exactly as in build-jolt.ss: it defines jolt.host/run-expr-string,
;; and bld-emit-cli-aot emits jolt.main in this process — an unresolved jolt.host
;; var emits as a host-static class reference, so the devcache's -M/-A/-Sdeps
;; -e arm died with "No such var: jolt.host/run-expr-string" while a bare -e
;; (which skips jolt.main) worked.
(load "host/chez/cli-core.ss")
(load "host/chez/png.ss")
(load "host/chez/loader.ss")
(load "host/chez/java/ffi.ss")
(set-source-roots! ldr-install-roots)
(load "host/chez/build.ss")

(define jb-build "target/dev")
(bld-system (string-append "mkdir -p '" jb-build "'"))


;; --- collect inputs (same algorithm as build-jolt's jb-collect-load-paths) ---
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
  ;; emit-image.ss (inlined by build.ss above) turns per-site var-cell caching AND
  ;; source-map registration OFF at load time, because the seed mint and `jolt
  ;; build` must emit byte-identical output that carries no absolute paths. Here
  ;; that runs AFTER compile-eval.ss turned both on for runtime eval, so the
  ;; settings baked into the image are the build ones. The built jolt escapes this
  ;; by loading the build subsystem lazily; this image loads it eagerly, so restore
  ;; both. A later `jolt build` from the cache sets them back off for itself.
  ;;
  ;; var-cache: every namespace compiled at runtime resolves each var by name on
  ;; every access — around half speed on var-reference-heavy code.
  ;; source-reg: no def records its source, so an uncaught error's frames print as
  ;; bare names instead of "ns/name (file:line)". That is the documented trade-off
  ;; for an open-world BUILD, not for `jolt run`, and it made the dev CLI's traces
  ;; strictly worse than the released binary's — the source-mapped-trace smoke
  ;; checks pass in source mode and failed only against a fresh cache.
  (put-string out "\n;; === restore runtime compile settings after the build driver ===\n")
  (put-string out "(let ((scv (var-deref \"jolt.backend-scheme\" \"set-var-cache!\")))\n")
  (put-string out "  (when (procedure? scv) (scv #t)))\n")
  (put-string out "(let ((ssr (var-deref \"jolt.backend-scheme\" \"set-source-reg!\")))\n")
  (put-string out "  (when (procedure? ssr) (ssr #t)))\n")
  ;; Runtime source embeds (bytevector values, 1B/char).
  (put-string out "\n;; === embedded runtime source ===\n")
  (for-each (lambda (path)
              (put-string out
                (string-append
                  "(register-embedded-resource! " (ei-str-lit path) " "
                  (ei-bytes-lit (read-file-string path)) ")\n")))
            db-paths)
  ;; jolt-core + stdlib source embeds (bytevector values, 1B/char).
  ;; First root wins, matching resolve-on-roots — see jb-emit-source-embeds.
  (put-string out "\n;; === embedded jolt-core + stdlib source ===\n")
  (let ((baked (make-hashtable string-hash string=?)))
    (for-each
      (lambda (root)
        (for-each
          (lambda (rp)
            (let ((rel (car rp)) (abs (cdr rp)))
              (when (and (ldr-source-path? rel)
                         (not (hashtable-ref baked rel #f)))
                (hashtable-set! baked rel #t)
                (put-string out
                  (string-append
                    "(register-embedded-resource! " (ei-str-lit rel) " "
                    (ei-bytes-lit (read-file-string abs)) ")\n")))))
          (bld-walk-files root "" '())))
      ldr-install-roots))
  ;; AOT jolt.main + jolt.deps (and their on-demand Clojure closure) into the image
  ;; as emitted Scheme — the SAME path build-jolt.ss uses (bld-emit-cli-aot), so the
  ;; CLI closure is compiled here, not recompiled from source on every dev invocation.
  ;; The old top-level (load-namespace …) forms re-executed at every Sbuild_heap and
  ;; cost ~1.3s/invocation. bld-emit-cli-aot emits the section marker + per-ns Scheme.
  (bld-emit-cli-aot out)
  (close-port out))

;; --- write input list (before compile, so the list is always consistent) ------
(display "make-devboot: writing input list\n")
(let ((out (open-output-file db-input-file 'replace))
      (clj-files '()))
  (for-each (lambda (p) (put-string out p) (put-string out "\n")) db-paths)
  ;; Also list every source file (ldr-source-exts).
  (for-each
    (lambda (root)
      (for-each
        (lambda (rp)
          (let ((rel (car rp)))
            (when (ldr-source-path? rel)
              (put-string out (cdr rp)) (put-string out "\n"))))
        (bld-walk-files root "" '())))
    ldr-install-roots)
  (close-port out))

;; The Chez that wrote flat.so decides who can read it: a fasl only loads in the
;; version that produced it. Record which one that was, so bin/jolt can tell it
;; apart from the Chez it is about to run and fall back to source mode instead of
;; dying on "incompatible fasl-object version" — the failure otherwise reports
;; nothing about its own cause, and no jolt source has changed to explain it.
;; $JOLT_CHEZ is what make hands down; a hand-run build just records nothing.
(let ((exe (getenv "JOLT_CHEZ")))
  (when (and exe (> (string-length exe) 0))
    (let ((out (open-output-file (string-append jb-build "/flat.chez") 'replace)))
      (put-string out exe)
      (put-string out "\n")
      (close-port out))))

;; --- 2. compile in a FRESH Chez (same approach as build-jolt step 2) ---------
;; compile-file must run against a clean chezscheme env so `error` and other
;; primitives the runtime shadows bind to the kernel versions.
(display "make-devboot: compiling flat.so (fresh Chez)\n")
;; Compile to a temp path and rename into place: a concurrent bin/jolt (e.g.
;; parallel make ci gates) must never load a partially written image — a
;; truncated fasl can load a prefix of the runtime and fail on late defines.
;;
;; The temp names carry THIS PROCESS's pid, because the readers are not the only
;; concurrency here: devbootsmoke runs `make devboot` four times and gatebootsmoke
;; rebuilds it too, and both gates run in the same parallel `make ci`. On one
;; fixed scratch path the two writers raced — one renamed the shared .tmp into
;; place and the other's rename died with "cannot rename target/dev/flat.so.tmp:
;; no such file or directory", failing the whole gate on a target nothing had
;; changed. The rename into the final name stays atomic, so the last writer wins
;; and every reader sees one complete image either way.
(define jb-pid (number->string (get-process-id)))
(define jb-flat-so-tmp (string-append jb-flat-so "." jb-pid ".tmp"))
(let ((cs (string-append jb-build "/dev-compile." jb-pid ".ss")))
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
  (bld-system (string-append bld-chez " --script '" cs "'"))
  (when (file-exists? cs) (delete-file cs)))
(when (file-exists? jb-flat-so) (delete-file jb-flat-so))
(rename-file jb-flat-so-tmp jb-flat-so)

(display (string-append "make-devboot: wrote " jb-flat-so "\n"))
