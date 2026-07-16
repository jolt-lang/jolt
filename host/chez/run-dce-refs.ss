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
(load "host/chez/run-gate-harness.ss")
(load "host/chez/dce.ss")
;; load the full stdlib so var-cell-lookup resolves every bail/compile-ref
(load "host/chez/loader.ss")

(define analyze (var-deref "jolt.analyzer" "analyze"))

(define (has? x lst) (if (member x lst) #t #f))

;; an IR node with NO :var/:the-var child (a plain constant) — its emitted scheme
;; carries no var reference, so the IR walk finds nothing.
(define ir (analyze (make-analyze-ctx "app.core") (jolt-ce-read "42")))
(gate-check "constant IR has no var refs" (dce-collect-refs '() ir) '())

;; the same form's emitted scheme, but carrying a literal string-keyed var-deref
;; spliced in (as a macro might emit raw scheme). The IR walk misses it; the text
;; scan and the union both catch it.
(define str "(begin (var-deref \"app.core\" \"target\"))")
(gate-check "IR-only misses string-keyed var-deref" (has? "app.core/target" (dce-collect-refs '() ir)) #f)
(gate-check "text scan catches string-keyed var-deref" (has? "app.core/target" (dce-sexp-refs-str str)) #t)
(gate-check "union (dce-app-refs) roots the target" (has? "app.core/target" (dce-app-refs ir str)) #t)

;; jolt-var (the #'x / cached var form) is caught the same way.
(define str2 "(jolt-var \"app.core\" \"other\")")
(gate-check "union catches jolt-var form" (has? "app.core/other" (dce-app-refs ir str2)) #t)

;; a normal :var node is still caught by the IR walk (regression guard) — the union
;; is additive, not a replacement.
(define ir2 (analyze (make-analyze-ctx "user") (jolt-ce-read "(clojure.core/inc)")))
(gate-check ":var node caught by IR walk" (has? "clojure.core/inc" (dce-collect-refs '() ir2)) #t)

;; a string-keyed var-deref whose args are NOT literals (computed) is intentionally
;; not matched — a static graph can't follow a runtime-resolved name.
(define str3 "(var-deref (f) (g))")
(gate-check "computed var-deref args not matched" (dce-sexp-refs-str str3) '())

;; a var-cell-lookup literal (the host shim's native form for looking up a var cell
;; by ns + name) is caught the same as var-deref and jolt-var.
(define str4 "(var-cell-lookup \"app.core\" \"target\")")
(gate-check "text scan catches var-cell-lookup literal" (has? "app.core/target" (dce-sexp-refs-str str4)) #t)
(gate-check "union catches var-cell-lookup form" (has? "app.core/target" (dce-app-refs ir str4)) #t)

;; --- dce-runtime-core-roots guard -------------------------------------------
;; A runtime .ss shim that references a clojure.core fn by name (a literal
;; (var-deref "clojure.core" "NAME") or jolt-var) is invisible to the app IR
;; graph, so the named fn must survive the prelude shake — i.e. be a root in
;; dce-runtime-core-roots — or a tree-shaken app silently ships a prunable var
;; the shim dereferences at runtime. Scan the runtime shims (everything under
;; host/chez except the test drivers run-*.ss, the build/cli entry build*.ss /
;; bootstrap.ss / emit-image.ss, and the seed compiler) and assert every
;; clojure.core FN reference (dynamic vars, names starting with *, are never
;; pruned and are excluded) is rooted. A new shim reference that isn't rooted
;; fails this gate instead of shipping a shakeable root.
(define (dce-shim-files)
  (let ((skip? (lambda (f)
                 (or (and (fx>? (string-length f) 4)
                          (string=? (substring f 0 4) "run-"))
                     (string=? f "dce.ss")            ; this gate's own machinery
                     (string=? f "emit-image.ss")     ; compiler-image emitter
                     (and (fx>? (string-length f) 5)
                          (string=? (substring f 0 5) "build"))
                     (string=? f "bootstrap.ss")))))
    (let loop-top ((fs (directory-list "host/chez")) (acc '()))
      (cond
        ((null? fs)
         (let loop-java ((js (directory-list "host/chez/java")) (a acc))
           (cond ((null? js) (reverse a))
                 ((string=? (substring (car js) (- (string-length (car js)) 3) (string-length (car js))) ".ss")
                  (loop-java (cdr js) (cons (string-append "host/chez/java/" (car js)) a)))
                 (else (loop-java (cdr js) a)))))
        ((and (fx>? (string-length (car fs)) 3)
              (string=? (substring (car fs) (- (string-length (car fs)) 3) (string-length (car fs))) ".ss")
              (not (skip? (car fs))))
         (loop-top (cdr fs) (cons (string-append "host/chez/" (car fs)) acc)))
        (else (loop-top (cdr fs) acc))))))

;; the clojure.core/ FN names (not dynamic vars) a shim references by name.
(define (dce-shim-core-fn-refs path)
  (let ((p (open-input-file path)))
    (let loop ((acc '()))
      (let ((form (read p)))
        (cond ((eof-object? form) (close-port p) acc)
              (else (loop (dce-sexp-refs form acc))))))))

(let ((rooted (make-hashtable string-hash string=?)))
  (for-each (lambda (r) (hashtable-set! rooted r #t)) dce-runtime-core-roots)
  (let loop-files ((files (dce-shim-files)))
    (unless (null? files)
      (let loop-refs ((refs (dce-shim-core-fn-refs (car files))))
        (cond
          ((null? refs) (loop-files (cdr files)))
          ;; only clojure.core fns (dynamic vars — names beginning with * — are
          ;; never pruned and don't need rooting). "clojure.core/" is 13 chars.
          ((and (fx>? (string-length (car refs)) 13)
                (string=? (substring (car refs) 0 13) "clojure.core/")
                (not (char=? (string-ref (car refs) 13) #\*)))
           (gate-check (string-append "core-root: " (car refs) " (" (car files) ")")
                  (hashtable-ref rooted (car refs) #f) #t)
           (loop-refs (cdr refs)))
           (else (loop-refs (cdr refs))))))))

;; --- dce-bail-refs / dce-compile-refs existence gate -------------------------
;; Every name in the hand-maintained bail/compile lists must resolve to a runtime
;; binding. A stale entry (one that no longer exists in the runtime) fails here
;; instead of silently widening the bail set — the static graph treats an unknown
;; bail name as "everything is reachable" and drops no code, but the list rot is
;; invisible. This gate makes it visible.
;;
;; Known-unimplemented core vars referenced in the bail/compile lists that don't
;; yet have a runtime def-var! are allowlisted — they still widen the bail set
;; correctly (an unknown name in the graph acts as "keep everything"), but the
;; existence check skips them so a planned-but-unbuilt var doesn't fail the gate.
(define dce-gate-allowlist (make-hashtable string-hash string=?))
(for-each (lambda (n) (hashtable-set! dce-gate-allowlist n #t))
          '("clojure.core/load-reader"))
(let ((missing (lambda (lst label)
                  (let loop ((ns lst) (bad '()))
                    (if (null? ns) bad
                        (let* ((name (car ns))
                               (slash (let loop2 ((i 0))
                                        (if (char=? (string-ref name i) #\/) i
                                            (loop2 (+ i 1))))))
                          (if (or (hashtable-ref dce-gate-allowlist name #f)
                                  (var-cell-lookup (substring name 0 slash)
                                                   (substring name (+ slash 1)
                                                              (string-length name))))
                              (loop (cdr ns) bad)
                              (loop (cdr ns) (cons name bad)))))))))
  (for-each (lambda (n) (gate-check (string-append "bail-ref exists: " n) #f #t))
            (missing dce-bail-refs "bail-refs"))
  (for-each (lambda (n) (gate-check (string-append "compile-ref exists: " n) #f #t))
            (missing dce-compile-refs "compile-refs")))

(gate-summary "dce-refs")
