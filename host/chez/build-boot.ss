;; build-boot.ss — the runtime preamble build-jolt.ss runs under, cached.
;;
;;   (import (chezscheme))
;;   (load "host/chez/build-boot.ss")
;;
;; build-jolt loads this preamble twice per build: in its own process, and in
;; the Chez that compiles the stdlib fasls. From source that is ~4.8s each time,
;; most of it rt.ss. target/dev/build-boot.so is the same loads compiled, which
;; boots in ~0.4s; when it is present and every file that went into it still has
;; the content it was built from (gate-boot-fresh.ss), load it instead.
;;
;; build-jolt writes the image itself: on a miss it compiles one beside the
;; flat.ss compile, from exactly the loads in the block below (jb-build-boot-
;; lines reads them out of this file), so the next build hits. Like gate-boot.ss,
;; a missing or stale image only costs time — the literal loads run and the build
;; is the same. manifest-check.sh pins the block against bld-runtime-manifest.

(load "host/chez/gate-boot-fresh.ss")

(define build-boot-so "target/dev/build-boot.so")
(define build-boot-inputs "target/dev/build-boot.inputs")

;; JOLT_NARROW_HASH is read at EXPAND time (hasheq.ss define-width-op), so an
;; image compiled without it holds the wide arms whatever its inputs say. Take
;; the source arm whenever it is set; build-jolt also writes no image then.
(define (build-boot-cacheable?)
  ;; any value, "" included, as define-width-op reads it
  (not (getenv "JOLT_NARROW_HASH")))

;; The receipt identifies the selected executable and native capabilities, and
;; hashes the host Chez's boot files, so a different or rebuilt Chez reads stale.
;; The guard covers a Chez the list cannot see (a
;; JOLT_CHEZ_CSV that names another install): its fasl is refused at the
;; header, before any of it runs, and the source loads take over.
(if (and (build-boot-cacheable?)
         (gate-boot-image-fresh? build-boot-so build-boot-inputs)
         (guard (e (#t (display "buildboot: target/dev/build-boot.so did not load, using source\n"
                                (current-error-port))
                       #f))
           (load "target/dev/build-boot.so")
           #t))
    (when (let ((m (getenv "JOLT_BUILDBOOT"))) (and m (not (string=? m ""))))
      (display "buildboot: used target/dev/build-boot.so\n" (current-error-port)))
    (begin
      ;; build-boot loads begin
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
      ;; cli-core.ss is inlined into the emitted image, but build-jolt also needs
      ;; it loaded: it defines jolt.host/run-expr-string, and bld-emit-cli-aot
      ;; emits jolt.main in that process — an unresolved jolt.host var would emit
      ;; as a class reference and the binary's -e arm would die with "Unknown
      ;; class jolt.host".
      (load "host/chez/cli-core.ss")
      (load "host/chez/png.ss")
      (load "host/chez/loader.ss")
      (load "host/chez/java/ffi.ss")
      ;; build-boot loads end
      ))
