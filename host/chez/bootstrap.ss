;; bootstrap.ss — the pure-Chez self-build.
;;
;; Given a SEED (prelude, image) pair — the bootstrap compiler, checked in under
;; host/chez/seed/ — it loads them, then rebuilds the clojure.core prelude AND the
;; compiler image from the .clj/.ss sources using the on-Chez compiler
;; (emit-image.ss), writing fresh artifacts: read -> analyze -> emit all run on
;; Chez. The seed is a JOINT fixpoint, so a rebuild from an up-to-date seed
;; reproduces it byte-for-byte (`make selfhost` checks this); when the sources
;; change, run it twice to reconverge and re-mint the seed.
;;
;; Run from the repo root:
;;   chez --script host/chez/bootstrap.ss SEED-PRELUDE SEED-IMAGE OUT-PRELUDE OUT-IMAGE
(import (chezscheme))

(define bs-args (cdr (command-line)))   ; drop the script name
(when (< (length bs-args) 4)
  (display "usage: bootstrap.ss SEED-PRELUDE SEED-IMAGE OUT-PRELUDE OUT-IMAGE\n")
  (exit 2))
(define bs-seed-prelude (list-ref bs-args 0))
(define bs-seed-image   (list-ref bs-args 1))
(define bs-out-prelude  (list-ref bs-args 2))
(define bs-out-image    (list-ref bs-args 3))

;; Load the runtime + the SEED compiler (prelude for macros, image for the
;; analyzer/emitter), exactly as the spine assembles a program.
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load bs-seed-prelude)
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load bs-seed-image)
(load "host/chez/compile-eval.ss")
(load "host/chez/emit-image.ss")

;; Rebuild both artifacts from source ON CHEZ and write them out. Any overlay/
;; compiler form that fails to compile is skipped (guarded) so a partial build
;; still boots; ei-skipped-count records how many, and the summary line lets
;; remint.sh fail the fixpoint pass on a nonzero count.
(ei-reset-skipped!)
(let ((p (open-output-file bs-out-prelude 'replace)))
  (put-string p (jolt-emit-prelude)) (close-port p))
(let ((p (open-output-file bs-out-image 'replace)))
  (put-string p (jolt-emit-image)) (close-port p))
(fprintf (current-error-port) "mint: ~a form(s) skipped\n" ei-skipped-count)
(display "bootstrap: rebuilt prelude + compiler image on Chez\n")
