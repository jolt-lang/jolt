;; gen-seed.ss — cross-mint the GAMBIT seed from the Chez seed, ON CHEZ.
;; jolt-mj95.4 (G3). Run via `make gambitseed`:
;;   $(CHEZ) --script host/gambit/gen-seed.ss
;;
;; WHY: the checked-in Chez seed (host/chez/seed/{prelude,image}.ss) is emitted
;; with the backend's :chez target — its unsafe #3% op spellings cannot load on
;; Gambit. This script loads the Chez seed exactly as bootstrap.ss does, flips
;; the backend target to :gambit (R9 set-target!), re-emits both artifacts on
;; Chez, and writes them to host/gambit/seed/{prelude,image}.ss. The output is
;; the SAME compiler (analyzer/emitter/passes) and the same clojure.core prelude,
;; emitted as portable Scheme: every #3%-derived spelling degrades to the safe
;; op (unsafe-prefix -> nil -> "").
;;
;; Safety net: the script greps its own output for "#3%" and "(foreign-procedure"
;; and FAILS (nonzero exit, no files written) rather than emitting a chez-flavored
;; seed — the R9 table is the only sanctioned source of target-specific spelling,
;; and anything else chez-only that surfaces here must go through the supervisor.
;;
;; Generated files, checked in like the chez seed (the seed-mint pattern).

(import (chezscheme))

(load "host/chez/scheme-adapter-runtime.ss")  ; before rt.ss: macros + top-levels in rt.ss/java/*.ss call sa-*
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")
(load "host/chez/emit-image.ss")

;; Flip the emission target to :gambit (R9). Every unsafe-prim site derives its
;; spelling from (unsafe-prefix) -> (get-in target-prims [(target) :unsafe-prefix]);
;; :gambit is not in the table, so the prefix degrades to the checked op.
;; The target lives on the PUBLISHED unit (backend_scheme.clj (cur) ->
;; jolt.op-registry/current-unit-box), and emit-image's ei-publish-unit! swaps in
;; its own fresh unit (new-unit defaults :target to :chez) — so publish FIRST,
;; then set, or the mint reads :chez.
(ei-fresh-unit!)
;; compile-eval.ss turned prelude-mode ON (needed to emit clojure.core refs) on
;; the unit that was current at ITS load time; the fresh unit we just published
;; defaults prelude-mode #f — restore it, then set the target.
(let ((spm (var-deref "jolt.backend-scheme" "set-prelude-mode!")))
  (if (procedure? spm) (spm #t)
      (begin (display "gen-seed: WARNING set-prelude-mode! not found\n") (exit 1))))
;; Same reason as prelude-mode above: emit-image.ss turned var cell-hoisting ON on
;; the unit current at ITS load time, and the fresh unit published here defaults it
;; off. Without this the Gambit seed silently keeps the un-hoisted (var-deref ns
;; name) shape — correct, but ~102ns per core var reference where the Chez seed
;; pays ~1ns. dyn-binding.ss (which defines var-cell-deref) is ##included at
;; boot.ss:83 / boot-full.ss:90, ahead of seed/prelude.ss, so the hoisted reader
;; is bound before any hoisted cell is read.
(let ((svc (var-deref "jolt.backend-scheme" "set-var-cache!")))
  (if (procedure? svc) (svc #t)
      (begin (display "gen-seed: WARNING set-var-cache! not found\n") (exit 1))))
(let ((st (var-deref "jolt.backend-scheme" "set-target!")))
  (if (procedure? st)
      (begin (st (keyword #f "gambit"))
             (display "gen-seed: backend target -> :gambit\n"))
      (begin (display "gen-seed: WARNING set-target! not found in seed image\n")
             (exit 1))))

;; Emit both artifacts to strings FIRST, verify, then write. A chez-flavored
;; emission must fail without producing files.
(ei-reset-skipped!)
(define gs-prelude (jolt-emit-prelude))
(define gs-image (jolt-emit-image))



;; A "#3%" occurrence is a chez-only EMISSION only when it is a USE
;; (e.g. "(#3%vector-ref ...)" / "(#3%fl+ ...)" — a paren/whitespace right
;; before it). A "#3%" inside a string literal is DATA — the compiled
;; target-prims table itself carries the :unsafe-prefix value "#3%", and that
;; literal must survive into the gambit image (the runtime consults it).
(define (gs-use? text i)
  (let ((c (if (> i 0) (string-ref text (- i 1)) #\space)))
    (or (char=? c #\() (char=? c #\space) (char=? c #\newline))))

(define (gs-verify text what)
  (let loop ((i 0) (uses '()) (n 0))
    (let ((j (gs-index-at text "#3%" i)))
      (if (< j 0)
          (begin
            (when (pair? uses)
              (fprintf (current-error-port)
                       "gen-seed: FAIL ~a has ~a #3% USE(s) — chez-flavored emission, not writing a gambit seed\n"
                       what (length uses))
              (for-each (lambda (k)
                          (let ((lo (max 0 (- k 60))) (hi (min (string-length text) (+ k 80))))
                            (fprintf (current-error-port) "gen-seed:   ...~a...\n"
                                     (substring text lo hi))))
                        (reverse uses))
              (exit 1))
            (display (string-append "gen-seed: " what " #3% occurrences: " (number->string n)
                                    " (all inside string data, none as op uses)\n")))
          (let ((use? (gs-use? text j)))
            (loop (+ j 3)
                  (if use? (cons j uses) uses)
                  (+ n 1))))))
  ;; "(foreign-procedure" is chez-only as an OP USE. Inside a string literal it
  ;; is DATA: emit-cfn's own emitter strings carry the text (the compiler must
  ;; be able to DESCRIBE chez output while running on gambit), so the preceding-
  ;; char heuristic above cannot classify it — the needle sits mid-string after
  ;; ordinary characters. Scan with a string-literal state machine instead and
  ;; fail only on an occurrence OUTSIDE any string literal.
  (let ((uses (gs-op-uses text "(foreign-procedure")))
    (when (pair? uses)
      (fprintf (current-error-port)
               "gen-seed: FAIL ~a has ~a (foreign-procedure op use(s) — chez-only, not writing\n"
               what (length uses))
      (for-each (lambda (k)
                  (let ((lo (max 0 (- k 60))) (hi (min (string-length text) (+ k 80))))
                    (fprintf (current-error-port) "gen-seed:   ...~a...\n"
                             (substring text lo hi))))
                uses)
      (exit 1))))

;; Occurrences of needle (which starts with #\() OUTSIDE string literals.
;; Tracks Chez `write` string syntax: \" and \\ escapes inside strings, and a
;; #\< char literal outside them (#\" must not open a string).
(define (gs-op-uses text needle)
  (let ((nlen (string-length needle)) (tlen (string-length text)))
    (let loop ((i 0) (in-str #f) (uses '()))
      (if (>= i tlen)
          (reverse uses)
          (let ((c (string-ref text i)))
            (cond
              (in-str
               (cond ((char=? c #\\) (loop (+ i 2) #t uses))
                     ((char=? c #\") (loop (+ i 1) #f uses))
                     (else (loop (+ i 1) #t uses))))
              ((and (char=? c #\#) (< (+ i 1) tlen)
                    (char=? (string-ref text (+ i 1)) #\\))
               (loop (+ i 3) #f uses))          ; #\" / #\\ / first char of a name
              ((char=? c #\") (loop (+ i 1) #t uses))
              ((and (char=? c #\() (<= (+ i nlen) tlen)
                    (string=? (substring text i (+ i nlen)) needle))
               (loop (+ i nlen) #f (cons i uses)))
              (else (loop (+ i 1) #f uses))))))))

(define (gs-index-at text needle from)
  (let loop ((i from))
    (cond
      ((>= i (string-length text)) -1)
      ((string=? (substring text i (min (+ i (string-length needle)) (string-length text))) needle) i)
      (else (loop (+ i 1))))))

(gs-verify gs-prelude "prelude")
(gs-verify gs-image "image")
(display (string-append "gen-seed: prelude grep-verified (no #3% / (foreign-procedure op uses)\n"))
(display (string-append "gen-seed: image  grep-verified (no #3% / (foreign-procedure op uses)\n"))

(define (gs-write path text)
  (let ((p (open-output-file path 'replace)))
    (put-string p text)
    (close-port p))
  (display (string-append "gen-seed: wrote " path " ("
                          (number->string (string-length text)) " bytes)\n")))

;; GEN_SEED_OUT_DIR redirects the writes, which is what `make gambitseedcheck`
;; uses to mint into a temp dir and diff against the checked-in seed.
(define gs-out-dir (or (getenv "GEN_SEED_OUT_DIR") "host/gambit/seed"))
(gs-write (string-append gs-out-dir "/prelude.ss") gs-prelude)
(gs-write (string-append gs-out-dir "/image.ss") gs-image)
(fprintf (current-error-port) "mint: ~a form(s) skipped\n" ei-skipped-count)
(display "gen-seed: gambit seed minted on Chez\n")
