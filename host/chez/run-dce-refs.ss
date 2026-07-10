;; run-dce-refs.ss — DCE reference-collection gate (dce.ss).
;;
;; App-form ref collection must union an IR walk (dce-collect-refs over :var/
;; :the-var nodes) with a text scan (dce-sexp-refs) of the emitted Scheme, so a
;; literal (var-deref "ns" "nm") spliced into an emitted form by a macro — with no
;; corresponding :var IR node — still roots its target. The IR walk alone misses it;
;; the prelude path (dce-blob-records) already scans text, and dce-app-refs mirrors
;; that for app records. This gate pins both halves and the gap between them.
;;
;;   chez --script host/chez/run-dce-refs.ss
(import (chezscheme))
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")
(load "host/chez/dce.ss")

(define analyze (var-deref "jolt.analyzer" "analyze"))

(define fails 0) (define total 0)
(define (check label actual expected)
  (set! total (+ total 1))
  (unless (equal? actual expected)
    (set! fails (+ fails 1))
    (printf "  FAIL ~a: got ~s expected ~s\n" label actual expected)))
(define (has? x lst) (if (member x lst) #t #f))

;; an IR node with NO :var/:the-var child (a plain constant) — its emitted scheme
;; carries no var reference, so the IR walk finds nothing.
(define ir (analyze (make-analyze-ctx "app.core") (jolt-ce-read "42")))
(check "constant IR has no var refs" (dce-collect-refs '() ir) '())

;; the same form's emitted scheme, but carrying a literal string-keyed var-deref
;; spliced in (as a macro might emit raw scheme). The IR walk misses it; the text
;; scan and the union both catch it.
(define str "(begin (var-deref \"app.core\" \"target\"))")
(check "IR-only misses string-keyed var-deref" (has? "app.core/target" (dce-collect-refs '() ir)) #f)
(check "text scan catches string-keyed var-deref" (has? "app.core/target" (dce-sexp-refs-str str)) #t)
(check "union (dce-app-refs) roots the target" (has? "app.core/target" (dce-app-refs ir str)) #t)

;; jolt-var (the #'x / cached var form) is caught the same way.
(define str2 "(jolt-var \"app.core\" \"other\")")
(check "union catches jolt-var form" (has? "app.core/other" (dce-app-refs ir str2)) #t)

;; a normal :var node is still caught by the IR walk (regression guard) — the union
;; is additive, not a replacement.
(define ir2 (analyze (make-analyze-ctx "user") (jolt-ce-read "(clojure.core/inc)")))
(check ":var node caught by IR walk" (has? "clojure.core/inc" (dce-collect-refs '() ir2)) #t)

;; a string-keyed var-deref whose args are NOT literals (computed) is intentionally
;; not matched — a static graph can't follow a runtime-resolved name.
(define str3 "(var-deref (f) (g))")
(check "computed var-deref args not matched" (dce-sexp-refs-str str3) '())

(if (= fails 0)
    (begin (printf "dce-refs gate: ~a/~a passed\n" total total) (exit 0))
    (begin (printf "dce-refs gate: ~a/~a passed (~a failed)\n" (- total fails) total fails) (exit 1)))
