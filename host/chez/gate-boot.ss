;; gate-boot.ss — the runtime boot preamble every gate script shares.
;;
;;   (import (chezscheme))
;;   (load "host/chez/gate-boot.ss")
;;   … gate-specific code …
;;
;; Loading the runtime's six preamble files from Chez source costs ~1.3s of a
;; pass gate's ~1.5s runtime, paid two dozen times over in `make ci` and on every
;; single-gate run while iterating. `make gateboot` precompiles exactly this
;; preamble to target/dev/gate.so, which loads in ~0.2s; when that image is
;; present and newer than every file that went into it, load it instead.
;;
;; Purely an optimization. With no image, or a stale one, the literal loads below
;; run and the gate behaves identically — verified byte-identical output across
;; all 24 gates both ways. Nothing takes gateboot as a prerequisite, so CI is
;; unaffected unless someone builds it.
;;
;; It cannot reuse target/dev/flat.so (the bin/jolt dev boot cache): that image
;; also loads loader.ss, which turns `require` into real file loading, and the
;; gates deliberately stop at compile-eval.ss so their alias-only `require` keeps
;; working. See loader.ss's header.
;;
;; The literal loads are bld-runtime-manifest's prefix through 'compile-eval, and
;; manifest-check.sh pins them against it — make-gateboot.ss generates the image
;; from that same manifest prefix, so the two cannot drift. They stay literal
;; here for the reason cli.ss's do: loading the runtime through a loop changes
;; the visibility of the loaded files' top-level defines.

(load "host/chez/gate-boot-fresh.ss")

;; The image's freshness is decided by the CONTENT of the files that went into
;; it, which cannot see JOLT_NARROW_HASH: that knob is read at EXPAND time by
;; hasheq.ss's define-width-op, so an image compiled without it holds the wide
;; arms no matter how current its inputs are. Loading it under the narrow gate
;; would silently test the wide path — the one thing that gate exists to catch.
;; Take the source arm whenever it is set.
(define (gate-boot-narrow-hash?)
  (let ((v (getenv "JOLT_NARROW_HASH"))) (and v (not (string=? v "")))))

;; JOLT_GATEBOOT=1 announces which path was taken, mirroring JOLT_DEVCACHE for
;; the bin/jolt image — the only way to tell from the outside, since both paths
;; produce identical behavior.
;;
;; The image is OPENED before it is trusted, and a failure anywhere in the
;; freshness check or the open takes the source arm. make-gateboot deletes the old
;; image before it rewrites the input list (so a failed compile cannot leave a
;; stale image reading as fresh), and a parallel `make ci` rebuilds it while other
;; gates boot: a gate that found it fresh and then loaded it by name a moment
;; later died on "no such file or directory" when the delete landed between the
;; two. An open port keeps the image it found readable whatever happens to the
;; name after.
(define gate-boot-image-port
  (and (not (gate-boot-narrow-hash?))
       (gate-boot-image-fresh? "target/dev/gate.so" "target/dev/gate.inputs")
       (guard (e (#t #f)) (open-file-input-port "target/dev/gate.so"))))
(if gate-boot-image-port
    (begin
      (when (let ((m (getenv "JOLT_GATEBOOT"))) (and m (not (string=? m ""))))
        (display "gateboot: using target/dev/gate.so\n" (current-error-port)))
      (load-compiled-from-port gate-boot-image-port)
      (close-port gate-boot-image-port))
    (begin
      (load "host/chez/scheme-adapter-runtime.ss")  ; before rt.ss: macros + top-levels in rt.ss/java/*.ss call sa-*
      (load "host/chez/rt.ss")
      (set-chez-ns! "clojure.core")
      (load "host/chez/seed/prelude.ss")
      (load "host/chez/post-prelude.ss")
      (load "host/chez/post-prelude-str.ss")
      (set-chez-ns! "user")
      (load "host/chez/host-contract.ss")
      (load "host/chez/seed/image.ss")
      (load "host/chez/compile-eval.ss")))  ; manifest prefix ends here
;; The seed-var direct-link check (host-contract.ss hc-seed-ns?) needs the set
;; of namespaces the image booted with; the CLI gets it from loader.ss, which a
;; gate does not load, so snapshot it here — every namespace with vars at this
;; point is image-defined, and no gate has evaluated user code yet.
(hc-runtime-image-booted!
  (let ((t (make-hashtable string-hash string=?)))
    (vector-for-each (lambda (c) (hashtable-set! t (var-cell-ns c) #t)) (var-table-cells))
    (lambda () t)))
