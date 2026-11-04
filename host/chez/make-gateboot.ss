;; make-gateboot.ss — precompile the gate boot preamble to target/dev/gate.so.
;;
;;   chez --script host/chez/make-gateboot.ss
;;
;; The twenty run-*.ss pass gates (infer, wp, pic, narrow, …) each spend ~1.3s of
;; their ~1.5s runtime loading the same six runtime files from Chez source via
;; run-gate-harness.ss. This precompiles that preamble once so each gate boots in
;; ~0.2s instead — the same trick `make devboot` plays for bin/jolt.
;;
;; It cannot reuse target/dev/flat.so: that image also loads loader.ss, which
;; turns `require` into real file loading, and the gates deliberately stop at
;; compile-eval.ss so their alias-only `require` keeps working (see loader.ss's
;; header). Hence a second, smaller artifact.
;;
;; The emitted preamble is bld-runtime-manifest's prefix through 'compile-eval —
;; taken from the manifest rather than hand-listed, so the cache cannot drift
;; from the runtime. run-gate-harness.ss's literal fallback loads are pinned
;; against the same prefix by manifest-check.sh.
;;
;; Two-phase like make-devboot: emit gate.ss, then compile it in a FRESH Chez so
;; `error` and the other primitives the runtime shadows bind to the kernel
;; versions during compile-file.

(import (chezscheme))

(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(load "host/chez/post-prelude-str.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")
(load "host/chez/png.ss")
(load "host/chez/loader.ss")
(load "host/chez/java/ffi.ss")
(set-source-roots! ldr-install-roots)
(load "host/chez/build.ss")

(define gb-build "target/dev")
(bld-system (string-append "mkdir -p '" gb-build "'"))

(define gb-gate-ss (string-append gb-build "/gate.ss"))
(define gb-gate-so (string-append gb-build "/gate.so"))
(define gb-inputs  (string-append gb-build "/gate.inputs"))

;; The manifest entries the gates boot: everything up to and including
;; 'compile-eval. cli-core.ss and what follows (png, loader, ffi, source roots)
;; are the CLI's, not a gate's.
(define (gb-preamble-entries)
  (let loop ((es bld-runtime-manifest) (acc '()))
    (cond ((null? es) (error 'make-gateboot "manifest has no 'compile-eval entry"))
          ((eq? (car es) 'compile-eval) (reverse (cons (car es) acc)))
          (else (loop (cdr es) (cons (car es) acc))))))

(define gb-entries (gb-preamble-entries))

;; Resolve a manifest entry to its literal line ('prelude / 'image /
;; 'compile-eval stand for seed loads; everything else is already a line).
(define (gb-entry-line entry)
  (if (symbol? entry) (cdr (assq entry bld-tagged-loads)) entry))

;; --- 1. emit gate.ss --------------------------------------------------------
(display "make-gateboot: emitting gate source\n")
(let ((out (open-output-file gb-gate-ss 'replace)))
  (for-each (lambda (entry) (bld-inline-line (gb-entry-line entry) out 0)) gb-entries)
  (close-port out))

;; The staleness predicate AND its content hash, so the list this writes is
;; hashed by exactly the function that will check it.
(load "host/chez/gate-boot-fresh.ss")

;; --- input list (for run-gate-harness.ss's staleness check) -----------------
;; The transitive load closure of the preamble entries — the same walk
;; make-devboot uses, restricted to the prefix. Written before compiling, so a
;; failed compile can't leave a list that claims a stale .so is fresh (the .so is
;; only renamed into place on success, and a missing .so falls back to source).
(display "make-gateboot: writing input list\n")
(define (gb-collect-paths)
  (let ((seen (make-hashtable string-hash string=?)) (order '()))
    (define (walk path)
      (when (and path (not (hashtable-ref seen path #f)))
        (hashtable-set! seen path #t)
        (set! order (cons path order))
        (for-each (lambda (l) (walk (bld-load-path l))) (bld-file-lines path))))
    (for-each (lambda (entry) (walk (bld-load-path (gb-entry-line entry)))) gb-entries)
    (reverse order)))
;; Written via tmp+rename like the .so: a gate checking freshness concurrently
;; (parallel make) must never read a half-written list and conclude from the
;; truncated prefix that a stale image is fresh. The old image goes first: this
;; list describes the image about to be compiled, and if that compile fails, an
;; old gate.so beside it would read as fresh.
(define gb-pid (number->string (get-process-id)))
(when (file-exists? gb-gate-so) (delete-file gb-gate-so))
(let* ((tmp (string-append gb-inputs "." gb-pid ".tmp"))
       (out (open-output-file tmp 'replace)))
  ;; "<hash> <path>": the freshness question is whether an input still has the
  ;; content that went into the image, which mtime cannot answer (see
  ;; gate-boot-fresh.ss). Hashed with that file's own function, loaded below, so
  ;; writer and reader cannot drift.
  (for-each (lambda (p)
              (put-string out (gate-boot-hash-string p))
              (put-string out " ")
              (put-string out p)
              (put-string out "\n"))
            (gb-collect-paths))
  (close-port out)
  ;; rename over, never delete-then-rename: a gate reading the list in the gap
  ;; found it missing (gate-boot.ss)
  (rename-replace! tmp gb-inputs))

;; --- 2. compile in a FRESH Chez ---------------------------------------------
;; Compile to a temp path and rename into place: a concurrent gate (parallel make
;; ci) must never load a half-written image. The temp names carry this process's
;; pid for the same reason make-devboot.ss's do: several gates rebuild this in one
;; parallel `make ci`, and on a fixed scratch path two writers race until one
;; rename finds the file the other already moved.
(display "make-gateboot: compiling gate.so (fresh Chez)\n")
(define gb-gate-so-tmp (string-append gb-gate-so "." gb-pid ".tmp"))
(let ((cs (string-append gb-build "/gate-compile." gb-pid ".ss")))
  (let ((p (open-output-file cs 'replace)))
    (put-string p
      (string-append
        "(import (chezscheme))\n"
        "(optimize-level 2)\n"
        "(generate-inspector-information #f)\n"
        "(generate-procedure-source-information #f)\n"
        "(debug-on-exception #f)\n"
        "(fasl-compressed #t)\n"
        "(compile-file " (ei-str-lit gb-gate-ss) " " (ei-str-lit gb-gate-so-tmp) ")\n"))
    (close-port p))
  (bld-system (string-append bld-chez " --script '" cs "'"))
  (when (file-exists? cs) (delete-file cs)))
(rename-replace! gb-gate-so-tmp gb-gate-so)

(display (string-append "make-gateboot: wrote " gb-gate-so "\n"))
