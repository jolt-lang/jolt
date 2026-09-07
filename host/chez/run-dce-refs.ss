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

;; --- dce-reachable: closure correctness + linear scaling ---------------------
;; Correctness on a small graph with a diamond (a->b->d, a->c->d) and a back edge
;; (d->a): every node reached once, and the cycle terminates instead of looping.
(let ((edges (make-hashtable string-hash string=?)))
  (hashtable-set! edges "a" '("b" "c"))
  (hashtable-set! edges "b" '("d"))
  (hashtable-set! edges "c" '("d"))
  (hashtable-set! edges "d" '("a"))
  (let ((r (dce-reachable edges '("a"))))
    (gate-check "reachable: diamond+cycle closure" (hashtable-size r) 4)
    (gate-check "reachable: leaf node reached" (hashtable-ref r "d" #f) #t))
  (let ((r2 (dce-reachable edges '("d" "zz"))))
    (gate-check "reachable: root without edges still reached" (hashtable-ref r2 "zz" #f) #t)
    (gate-check "reachable: cycle back-edge closure" (hashtable-size r2) 5)))

;; Scaling: the walk must stay O(V+E) — in particular, INDEPENDENT of the live
;; work-list length. A wide graph (root -> V children, each child -> sink) keeps
;; V entries on the work list; a chain of 2V nodes keeps ~1. Node and edge
;; counts are comparable, so a correct walk costs the same on both (ratio ~1,
;; and cache/GC pressure cancels out — a plain 1x-vs-4x size ratio measured
;; 6-8x here from memory effects alone). A bad rewrite that copies the
;; REMAINING work per visit is O(work-list^2): ~V^2/2 copies on the wide graph
;; and nothing extra on the chain, so the ratio explodes (~250x at this V).
;; Judged best-of-3 within this one process; V is small enough that a REGRESSED
;; walk (~112M copies) still finishes and fails rather than hanging the gate.
(define (dce-gate-wide-graph v)
  (let ((edges (make-hashtable string-hash string=?)))
    (hashtable-set! edges "root"
      (let loop ((i 0) (acc '()))
        (if (fx=? i v) acc (loop (fx+ i 1) (cons (string-append "c" (number->string i)) acc)))))
    (let loop ((i 0))
      (unless (fx=? i v)
        (hashtable-set! edges (string-append "c" (number->string i)) (list "sink"))
        (loop (fx+ i 1))))
    edges))
(define (dce-gate-chain-graph n)
  (let ((edges (make-hashtable string-hash string=?)))
    (let loop ((i 0))
      (unless (fx=? i n)
        (hashtable-set! edges (string-append "c" (number->string i))
                        (list (string-append "c" (number->string (fx+ i 1)))))
        (loop (fx+ i 1))))
    edges))
;; No explicit GC before timing (collect is Chez-only and portcheck-blocked):
;; a collection landing inside one run is absorbed by the best-of-3 minimum.
(define (dce-gate-reach-ms edges root want)
  (let ((t0 (current-time 'time-monotonic)))
    (let ((r (dce-reachable edges (list root))))
      (let ((t1 (current-time 'time-monotonic)))
        (gate-check "reachable: closure size" (hashtable-size r) want)
        (+ (* 1000.0 (- (time-second t1) (time-second t0)))
           (/ (- (time-nanosecond t1) (time-nanosecond t0)) 1000000.0))))))
(define (dce-gate-best-of k thunk)
  (let loop ((i 1) (best (thunk)))
    (if (fx=? i k) best (loop (fx+ i 1) (min best (thunk))))))
(let* ((v1 15000)
       (wide (dce-gate-wide-graph v1))          ; v1+2 nodes, work list ~v1 deep
       (chain (dce-gate-chain-graph (* 2 v1)))  ; 2*v1+1 nodes, work list ~1 deep
       (tw (dce-gate-best-of 3 (lambda () (dce-gate-reach-ms wide "root" (+ v1 2)))))
       (tc (max 0.05 (dce-gate-best-of 3 (lambda () (dce-gate-reach-ms chain "c0" (+ (* 2 v1) 1))))))
       (ratio (/ tw tc)))
  (printf "dce-reachable work-list independence: wide ~ams, chain ~ams, ratio ~a (independent ~~1, work-list-copying ~~250, ceiling 5)\n"
          tw tc ratio)
  (gate-check "reachable: cost independent of work-list depth" (<= ratio 5.0) #t))

;; --- dce-def-var-form: which prelude records are prunable ---------------------
;; A def whose value holds an anonymous fn literal is minted with its source
;; registration first, (begin (let* …) (def-var-with-meta! …)); it is a prunable
;; def under that name like a bare def-var! is. Anything else stays a keep form:
;; a defrecord's several defs under one begin (one record, several fqns), a
;; def-var-plain! group, and a side-effecting top-level form.
(let ((bare '(def-var! "app.core" "f" 1))
      (meta '(def-var-with-meta! "clojure.repl" "find-doc" (lambda (x) x) (jolt-hash-map)))
      (registered '(begin (let* ((_q$0 (jolt-symbol #f "fn*")))
                            (image-register-fn-form! "jfn$clojure.repl$find-doc$0" _q$0 "clojure.repl" (jolt-vector)))
                          (def-var-with-meta! "clojure.repl" "find-doc" (lambda (x) x) (jolt-hash-map))))
      (record-group '(begin (begin (def-var-plain! "clojure.pprint" "nl-t" 1)
                                   (def-var-plain! "clojure.pprint" "->nl-t" 2))
                            (def-var-with-meta! "clojure.pprint" "make-nl-t" 3 (jolt-hash-map))
                            (def-var-with-meta! "clojure.pprint" "nl-t?" 4 (jolt-hash-map))))
      (side-effect '(jolt-invoke4 (var-deref "clojure.core" "attach-core-doc-meta!") "clojure.repl" "special-doc" jolt-nil jolt-nil)))
  (gate-check "def-var-form: bare def-var! is its own def" (dce-def-var-form bare) bare)
  (gate-check "def-var-form: bare def-var-with-meta! is its own def" (dce-def-var-form meta) meta)
  (gate-check "def-var-form: registration then def is the def"
              (dce-def-var-form registered) (caddr registered))
  (gate-check "def-var-form: a defrecord group is not one def" (dce-def-var-form record-group) #f)
  (gate-check "def-var-form: a side-effecting form is not a def" (dce-def-var-form side-effect) #f)
  (gate-check "def-var-form: guard-unwrapped registration form"
              (dce-def-var-form (dce-unwrap (list 'guard '(e (#t #f)) registered)))
              (caddr registered)))

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

;; --- spliced callees: kept for frame identity, not roots of the bail scan ----
;; The inline pass records every callee it spliced (hc-mark-spliced!), and the
;; shake keeps those defs so an inlined frame still maps back to ns/name. They are
;; not reachable code — every call site is a copy — so a spliced helper that
;; resolves a var by name must not bail the shake, while a def nothing reaches
;; is still pruned and everything a kept callee names stays defined.
(let* ((rec (lambda (fqn refs) (dce-rec #f fqn refs (string-append "(" fqn ")"))))
       (app (list (rec "gate.app/-main" '("gate.app/live"))
                  (rec "gate.app/live" '())
                  ;; spliced everywhere it was called, so no record references it
                  (rec "gate.app/walker" '("clojure.core/resolve" "gate.app/named-by-walker"))
                  (rec "gate.app/named-by-walker" '())
                  (rec "gate.app/dead" '()))))
  (hc-mark-spliced! #f "gate.app" "walker")
  (let-values (((core-strs app-strs drop-compiler?) (dce-shake '() app "gate.app/-main" '())))
    (gate-check "spliced: a spliced resolve caller does not bail the shake" (and core-strs #t) #t)
    (gate-check "spliced: the compiler image is dropped" drop-compiler? #t)
    (gate-check "spliced: the entry and what it reaches are kept"
                (and (member "(gate.app/-main)" app-strs) (member "(gate.app/live)" app-strs) #t) #t)
    (gate-check "spliced: the spliced callee is kept for frame identity"
                (and (member "(gate.app/walker)" app-strs) #t) #t)
    (gate-check "spliced: what the kept callee names stays defined"
                (and (member "(gate.app/named-by-walker)" app-strs) #t) #t)
    (gate-check "spliced: an unreferenced def is still pruned"
                (and (member "(gate.app/dead)" app-strs) #t) #f))
  ;; the same graph with the walker genuinely reachable bails as before
  (let-values (((core-strs app-strs drop-compiler?)
                (dce-shake '() (cons (rec "gate.app/-main" '("gate.app/walker")) (cdr app)) "gate.app/-main" '())))
    (gate-check "spliced: a reachable resolve caller still bails" core-strs #f)))

;; --- :allow-dynamic: a vouched-for def is not a bail --------------------------
;; deps.edn :jolt/tree-shake {:allow-dynamic [ns/name …]} names the defs whose
;; runtime var lookups the author asserts never run in a built binary (or name
;; only vars the graph keeps anyway). An allowed def is skipped by the bail scan
;; and by the compiler-needed scan — a site vouched never to run needs no
;; compiler — but nothing is kept on its behalf: it is pruned or kept exactly as
;; reachability says, and what it names stays defined when it is kept.
;; (The block above marked gate.app/walker spliced, and that mark is process-
;; global; none of the records here is named walker, so it does not reach in.)
(let* ((rec (lambda (fqn refs) (dce-rec #f fqn refs (string-append "(" fqn ")"))))
       (app (list (rec "gate.app/-main" '("gate.app/res"))
                  (rec "gate.app/res" '("clojure.core/resolve" "gate.app/named-by-res"))
                  (rec "gate.app/named-by-res" '())
                  (rec "gate.app/dead" '()))))
  ;; the empty allow list is today's behaviour: a reachable resolve caller bails
  (let-values (((core-strs app-strs drop-compiler?) (dce-shake '() app "gate.app/-main" '())))
    (gate-check "allow: a reachable resolve caller bails with an empty allow list" core-strs #f)
    (gate-check "allow: that bail keeps the compiler image" drop-compiler? #f))
  (let-values (((core-strs app-strs drop-compiler?) (dce-shake '() app "gate.app/-main" '("gate.app/res"))))
    (gate-check "allow: an allowed resolve caller does not bail" (and core-strs #t) #t)
    (gate-check "allow: the compiler image is dropped" drop-compiler? #t)
    (gate-check "allow: the allowed def is kept because it is reachable"
                (and (member "(gate.app/res)" app-strs) #t) #t)
    (gate-check "allow: what the allowed def names stays defined"
                (and (member "(gate.app/named-by-res)" app-strs) #t) #t)
    (gate-check "allow: an unreferenced def is still pruned"
                (and (member "(gate.app/dead)" app-strs) #t) #f))
  ;; an allowed def that is NOT reachable is pruned like any other def
  (let-values (((core-strs app-strs drop-compiler?)
                (dce-shake '() (cons (rec "gate.app/-main" '()) (cdr app)) "gate.app/-main" '("gate.app/res"))))
    (gate-check "allow: an unreachable allowed def is pruned, not kept"
                (and (member "(gate.app/res)" app-strs) #t) #f))
  ;; a second, non-allowed reachable caller still bails, and the hint names it
  ;; alone. Its resolve ref is listed twice, as dce-app-refs' IR+text union
  ;; produces in a real build, and must print once.
  (let* ((app2 (cons (rec "gate.app/-main" '("gate.app/res" "gate.app/lookup"))
                     (cons (rec "gate.app/lookup" '("clojure.core/resolve" "clojure.core/resolve")) (cdr app))))
         (got-core #t) (got-drop #t)
         (out (with-output-to-string
                (lambda ()
                  (let-values (((core-strs app-strs drop-compiler?)
                                (dce-shake '() app2 "gate.app/-main" '("gate.app/res"))))
                    (set! got-core core-strs)
                    (set! got-drop drop-compiler?))))))
    (gate-check "allow: a non-allowed reachable caller still bails" got-core #f)
    (gate-check "allow: a non-allowed bail keeps the compiler image" got-drop #f)
    (gate-check "allow: the bail lists the non-allowed caller"
                (gate-sub? out "  gate.app/lookup -> clojure.core/resolve\n") #t)
    (gate-check "allow: a ref counted twice in one record is listed once"
                (gate-sub? out "resolve\n  gate.app/lookup -> clojure.core/resolve\n") #f)
    (gate-check "allow: the bail does not list the allowed def"
                (gate-sub? out "gate.app/res ->") #f)
    (gate-check "allow: the hint is the paste-ready deps.edn key"
                (gate-sub? out "  :jolt/tree-shake {:allow-dynamic [gate.app/lookup]}\n") #t))
  ;; two non-allowed bailing defs join the hint space-separated, in record order
  (let ((out (with-output-to-string
               (lambda ()
                 (dce-shake '() (list (rec "gate.app/-main" '("gate.app/a" "gate.app/b"))
                                      (rec "gate.app/a" '("clojure.core/resolve"))
                                      (rec "gate.app/b" '("clojure.core/ns-resolve")))
                            "gate.app/-main" '())))))
    (gate-check "allow: the hint names every bailing def, space-joined in record order"
                (gate-sub? out "  :jolt/tree-shake {:allow-dynamic [gate.app/a gate.app/b]}\n") #t))
  ;; an allowed caller of a compile-ref (eval) is skipped by the compile scan too
  (let-values (((core-strs app-strs drop-compiler?)
                (dce-shake '() (list (rec "gate.app/-main" '("gate.app/ev"))
                                     (rec "gate.app/ev" '("clojure.core/eval")))
                           "gate.app/-main" '("gate.app/ev"))))
    (gate-check "allow: an allowed eval caller does not bail" (and core-strs #t) #t)
    (gate-check "allow: an allowed eval caller drops the compiler image" drop-compiler? #t))
  ;; a top-level non-def form has no fqn and cannot be allowed by name: no hint
  (let ((out (with-output-to-string
               (lambda ()
                 (dce-shake '() (list (dce-rec #t #f '("clojure.core/resolve") "(resolve-at-load)")
                                      (rec "gate.app/-main" '()))
                            "gate.app/-main" '())))))
    (gate-check "allow: a <form> bail lists the form" (gate-sub? out "  <form> -> clojure.core/resolve\n") #t)
    (gate-check "allow: a <form> bail prints no hint" (gate-sub? out ":jolt/tree-shake") #f)))

(gate-summary "dce-refs")
