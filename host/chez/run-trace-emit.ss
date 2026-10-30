;; run-trace-emit.ss — trace-r4: the emitter emits NO ring save/restore at all.
;;
;; R3 paired a save/restore around every non-tail call that could reach a jolt fn
;; prologue, so a returned subproblem's ribs stopped showing up in a later
;; backtrace (trace-smoke.sh's app.stale). That wrapper sat in
;; emit-invoke-maybe-clone, DOWNSTREAM of every branch of emit-invoke — so it also
;; wrapped the branches that lower to a single Chez primitive and can never enter
;; a jolt fn prologue at all:
;;
;;   (aget ^doubles a ^long i)  ->  (flvector-ref (jolt-array-vec a) i)
;;
;; became
;;
;;   (let ((_tu$ (jolt-trace-save)))
;;     (let ((_tr$ (flvector-ref (jolt-array-vec a) i))) (jolt-trace-unwind! _tu$) _tr$))
;;
;; — two procedure calls and a let-bound flonum around one machine instruction. The
;; let binding was the expensive half: holding a flonum across a call forces it onto
;; the heap, so the whole surrounding unboxed fl+ chain re-boxes. Measured 19x on a
;; (dotimes [i n] (aset b i (+ (aget a i) 0.5))) loop, and 4.5-8.6x across every
;; phase of a real image pipeline, against ~3% for the ring push itself. The
;; `:leaf?` registry fact and `leaf!` existed only to keep that wrapper off the
;; primitive sites.
;;
;; trace-r4 deletes the wrapper outright: the reporter reads the non-tail spine
;; off the LIVE CONTINUATION and takes only the current rib's TCO-erased tail
;; frames from the ring (source-registry.ss jolt-backtrace-string), so the
;; save/restore paid a call per call site for a read nobody does. Nothing emits
;; jolt-trace-save or jolt-trace-unwind! any more — every case below asserts the
;; negation. What survives is pinned too: the inline primitive lowerings, the
;; #|L<line>|# position markers, and the tracing-off behaviour.
;;
;;   chez --script host/chez/run-trace-emit.ss
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")
(define analyze (var-deref "jolt.analyzer" "analyze"))
(define numeric-annotate (var-deref "jolt.passes.numeric" "annotate"))
(define emit (var-deref "jolt.backend-scheme" "emit"))
(define set-trace-frames! (var-deref "jolt.backend-scheme" "set-trace-frames!"))
(define U ((var-deref "jolt.passes.types" "new-unit")))
((var-deref "jolt.backend-scheme" "set-emit-unit!") U)
((var-deref "jolt.backend-scheme" "set-prelude-mode!") #t)
(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))
(define (emit-num src) (emit (numeric-annotate (anode src))))
(define (ev s) (jolt-compile-eval s "user"))
;; every check below is about what tracing emits, so tracing is ON throughout
(set-trace-frames! #t)
(define (saves? e) (gate-sub? e "jolt-trace-save"))
(define (unwinds? e) (gate-sub? e "jolt-trace-unwind!"))

;; NOTE every primitive case below is written in NON-TAIL position (the operand of
;; an enclosing form). A tail call never took the wrapper to begin with — see (8) —
;; so a case in tail position would pass no matter what the fix does.

;; --- (1) primitive-lowering sites take NO wrapper --------------------------------
;; Each of these is a single Chez primitive after lowering. None can reach a jolt
;; prologue, so none can leave a rib behind, so none needs the save/restore.
(let ((e (emit-num "(def _ (fn [^doubles a ^long i] (+ (aget a i) (aget a i))))")))
  (gate-check "(1) proven aget still lowers inline" (gate-sub? e "(flvector-ref _av$") #t)
  (gate-check "(1) proven aget takes no trace-save" (saves? e) #f)
  (gate-check "(1) proven aget takes no trace-unwind" (unwinds? e) #f))
(let ((e (emit-num "(def _ (fn [^doubles a ^long i] (do (aset a i 7.25) 1.0)))")))
  (gate-check "(2) proven aset still lowers inline" (gate-sub? e "(flvector-set! _av$") #t)
  (gate-check "(2) proven aset takes no trace-save" (saves? e) #f))
;; the aget/aset pair inside a loop — the shape the regression was measured on
(let ((e (emit-num "(def _ (fn [^doubles a ^doubles b ^long n] (dotimes [i n] (aset b i (+ (aget a i) 0.5)))))")))
  (gate-check "(3) aget+aset+fl arithmetic loop still unboxes" (gate-sub? e "fl+") #t)
  (gate-check "(3) ...and takes no trace-save anywhere in it" (saves? e) #f))
;; proven flonum arithmetic (:num-kind) and a proven Math static (:fl-op)
(let ((e (emit-num "(def _ (fn [^double x ^double y] (+ (* x y) 1.0)))")))
  (gate-check "(4) proven fl arithmetic emits fl ops" (gate-sub? e "fl*") #t)
  (gate-check "(4) ...and takes no trace-save" (saves? e) #f))
(let ((e (emit-num "(def _ (fn [^double x] (+ (Math/sqrt x) 1.0)))")))
  (gate-check "(5) proven Math/sqrt emits flsqrt" (gate-sub? e "flsqrt") #t)
  (gate-check "(5) ...and takes no trace-save" (saves? e) #f))

;; --- (5b) UNTYPED numeric ops ------------------------------------------------
;; Untyped code falls to the generic `nop` branch and emits the generic numeric
;; op (jolt-n< / jolt-n+ / jolt-n-inc). trace-r4 removed the per-call ring
;; save/restore entirely, so NO site emits jolt-trace-save / jolt-trace-unwind!
;; any more — the reporter reads the non-tail spine off the live continuation
;; and takes only the current (innermost) rib's TCO-erased tail frames from the
;; ring. What the gate still pins below: the primitive/inline lowerings survive,
;; the markers survive, and the tracing-off behaviour survives.
(let ((e (emit-num "(def _ (fn [a b] (if (< a b) (+ a 1) (dec b))))")))
  (gate-check "(5b) untyped < emits the generic numeric op" (gate-sub? e "jolt-n<") #t)
  (gate-check "(5b) ...and takes no trace-save" (saves? e) #f))
(let ((e (emit-num "(def _ (fn [c] (+ 1 (count c))))")))
  (gate-check "(5c) count over a collection emits jolt-count" (gate-sub? e "jolt-count") #t)
  (gate-check "(5c) ...and takes no trace-save (save/restore is gone entirely)" (saves? e) #f))
(let ((e (emit-num "(def _ (fn [c i] (+ 1 (nth c i))))")))
  (gate-check "(5d) nth over a collection emits jolt-nth" (gate-sub? e "jolt-nth") #t)
  (gate-check "(5d) ...and takes no trace-save" (saves? e) #f))
(let ((e (emit-num "(def _ (fn [x y] (if (= x y) 1 2)))")))
  (gate-check "(5e) = emits the generic equality op" (gate-sub? e "jolt=") #t)
  (gate-check "(5e) ...and takes no trace-save" (saves? e) #f))

;; --- (6) a real call takes no save/restore either ----------------------------------
;; The other direction, and what trace-r4 changed: an ordinary non-tail invoke of
;; an unknown fn CAN enter a prologue and push a rib, but it no longer wraps the
;; call in a save/restore. The continuation is the exact spine; a returned call's
;; rib merely sits behind the ring head until the next top-level reset.
(let ((e (emit-num "(def _ (fn [f x] (+ 1 (f x))))")))
  (gate-check "(6) non-tail invoke of an unknown fn takes no trace-save" (saves? e) #f)
  (gate-check "(6) ...and no trace-unwind" (unwinds? e) #f))
;; a non-tail call to a named user fn, the shape trace-smoke's app.stale exercises
(ev "(def user-fn (fn [x] x))")
(let ((e (emit-num "(def _ (fn [x] (+ 1 (user-fn x))))")))
  (gate-check "(7) non-tail call to a user fn takes no trace-save" (saves? e) #f))
;; TAIL position never took a wrapper (consuming the result would defeat TCO) —
;; unchanged by this fix, pinned so it stays that way.
(let ((e (emit-num "(def _ (fn [f x] (f x)))")))
  (gate-check "(8) tail call takes no trace-save (TCO)" (saves? e) #f))

;; --- (9) tracing OFF emits neither, on any shape ---------------------------------
(set-trace-frames! #f)
(let ((e (emit-num "(def _ (fn [f x] (+ 1 (f x))))")))
  (gate-check "(9) tracing off: no trace-save" (saves? e) #f)
  (gate-check "(9) tracing off: no trace-unwind" (unwinds? e) #f))
(set-trace-frames! #t)

;; --- (10) position markers: #|L<n>|# before each traced call site --------------
;; The marker must carry the CORRECT clj line, so the source is multi-line and the
;; two calls sit on distinct lines — an off-by-one in either direction would fail.
(define (count-sub? s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0) (c 0))
      (cond ((> (+ i m) n) c)
            ((string=? (substring s i (+ i m)) sub) (loop (+ i 1) (+ c 1)))
            (else (loop (+ i 1) c))))))
;; remove every #|L<digits>|# from an emitted string (for the round-trip check)
(define (strip-markers e)
  (define (marker-end? i)
    ;; e[i..] starts with "#|L": index just past the closing "|#" of a marker, or #f
    (let ((n (string-length e)))
      (let loop ((j (+ i 3)))
        (cond
          ((>= j n) #f)
          ((char<=? #\0 (string-ref e j) #\9) (loop (+ j 1)))
          ((and (< (+ j 1) n)
                (char=? (string-ref e j) #\|)
                (char=? (string-ref e (+ j 1)) #\#))
           (+ j 2))
          (else #f)))))
  (let ((n (string-length e)))
    (let loop ((i 0) (out '()))
      (cond
        ((>= i n) (apply string-append (reverse out)))
        ((and (<= (+ i 3) n) (string=? (substring e i (+ i 3)) "#|L"))
         (let ((j (marker-end? i)))
           (if j (loop j out) (loop (+ i 1) (cons (substring e i (+ i 1)) out)))))
        (else (loop (+ i 1) (cons (substring e i (+ i 1)) out)))))))
(define marker-src "(def mdemo (fn [f x]\n         (let [a (f 1)]\n           (f a))))")
(let ((e (emit-num marker-src)))
  (gate-check "(10) tracing on: marker before the line-2 call" (gate-sub? e "#|L2|#") #t)
  (gate-check "(10) tracing on: marker before the line-3 call" (gate-sub? e "#|L3|#") #t)
  (gate-check "(10) tracing on: no marker on the def line" (gate-sub? e "#|L1|#") #f)
  ;; R2 (bead jolt-knn8): the per-call fixnum store is gone — a marker is a
  ;; comment only. mdemo's fn literal is UNNAMED, so it has no *trace-site*
  ;; and its tail call emits plain (R4: a tail site is one jolt-site! store
  ;; of the enclosing fn's static pair, which an anonymous fn cannot name).
  (gate-check "(10) tracing on: no per-call site store (R2)"
              (gate-sub? e "(jolt-site! ") #f)
  ;; the marker is genuinely a comment: reading the emitted string yields the
  ;; same datum with or without the markers
  (gate-check "(10) round-trip: marker reads as a comment"
              (equal? (read (open-input-string e))
                      (read (open-input-string (strip-markers e))))
              #t))
(set-trace-frames! #f)
(let ((e (emit-num marker-src)))
  (gate-check "(10) tracing off: no marker" (gate-sub? e "#|L") #f)
  (gate-check "(10) tracing off: no jolt-site!" (gate-sub? e "(jolt-site! ") #f))
(set-trace-frames! #t)

;; --- (10b) R2: the site vreg pair at native-op tail sites, callsite table ----
;; A NAMED fn whose body ends in a native-op tail call: exactly one jolt-site!
;; store, carrying the static ('fn . line) pair (sited-tail-call), and the def
;; wrapper registers the site's static callee for the reporter's staleness
;; validator (jolt-register-callsite!). A dynamic-callee NON-tail site registers
;; nothing; a dynamic TAIL site registers "?" so a live stack read can tell its
;; pair went somewhere and returned (source-registry jolt-site-exited?).
(define dyn-src "(def ddemo (fn ddemo [f x]\n  (let [a (f x)]\n    (f a))))")
(let ((e (emit-num dyn-src)))
  (gate-check "(10b) dynamic tail site registers ?"
              (gate-sub? e "(jolt-register-callsite! \"ddemo\" 3 \"?\" #t)") #t)
  (gate-check "(10b) dynamic non-tail site registers nothing"
              (gate-sub? e "(jolt-register-callsite! \"ddemo\" 2 ") #f))
(define sited-src "(def sdemo (fn sdemo [x]\n  (+ x 1)))")
(let ((e (emit-num sited-src)))
  (gate-check "(10b) native tail site stores the pair" (gate-sub? e "(jolt-site! '(") #t)
  (gate-check "(10b) exactly one site store" (= (count-sub? e "(jolt-site! ") 1) #t)
  ;; this harness emits with source registration OFF, so the callee registers
  ;; under its bare munged name (the direct-link-build form); with it on (dev,
  ;; open-world build) the same site registers "clojure.core/+". The trailing
  ;; #t is the tail? flag (R4) — chain reconstruction follows tail edges only.
  ;; + is $R+: munge-name renames it, as the back end emits + bare.
  (gate-check "(10b) callsite registered with its static callee"
              (gate-sub? e "(jolt-register-callsite! \"sdemo\" 2 \"$R+\" #t)") #t))
(set-trace-frames! #f)
(let ((e (emit-num sited-src)))
  (gate-check "(10b) tracing off: no site store" (gate-sub? e "(jolt-site! ") #f)
  (gate-check "(10b) tracing off: no callsite registration"
              (gate-sub? e "(jolt-register-callsite! ") #f))
(set-trace-frames! #t)

;; --- (11) clj-line lookup: nearest preceding #|L<n>|# marker --------------------
;; #|L10|# (foo) #|L20|# (bar) #|L30|# (baz)  — L10 sits at 0..6, L20 at 14..20,
;; L30 at 28..34; the calls' parens open at 8 / 22 / 36.
(let* ((t3 "#|L10|# (foo) #|L20|# (bar) #|L30|# (baz)")
       (t1 "(foo) #|L7|# (bar)"))
  (gate-check "(11) nearest preceding marker" (jolt-marker-line-at-offset t3 12) 10)
  (gate-check "(11) later marker wins between markers" (jolt-marker-line-at-offset t3 22) 20)
  (gate-check "(11) multiple markers: last wins past the end" (jolt-marker-line-at-offset t3 39) 30)
  (gate-check "(11) offset before any marker" (jolt-marker-line-at-offset t1 2) #f)
  (gate-check "(11) offset inside the marker itself" (jolt-marker-line-at-offset t3 18) 20)
  (gate-check "(11) offset past the end clamps" (jolt-marker-line-at-offset t3 1000) 30))

;; --- (11b) lexical scan: bytes inside strings/comments are never markers -------
;; A user string literal can contain "#|L999|#": the FORWARD scan must not record
;; it, so every offset after the literal resolves to the REAL marker (3), never
;; 999. s2 also exercises \" (escaped quote does not end the string), s3 an
;; unclosed "#|L" inside a string, s5 a marker hidden in a NESTED block comment,
;; and s7 a #\" CHARACTER literal (not a string opener).
(let* ((s1 "#|L3|# (sink \"#|L999|# oops\")")
       (s2 "#|L3|# (sink \"say \\\"#|L999|#\\\"\")")
       (s3 "#|L3|# (sink \"#|L\")")
       (s4 "(sink \"#|L999|#\")")
       (s5 "#| outer #|L5|# |# (sink)")
       (s6 "#|L3|# #| outer #|L5|# |# (sink)")
       (s7 "#|L3|# (f #\\\") #|L7|# (g)"))
  (gate-check "(11b) offset inside the fake marker in a string" (jolt-marker-line-at-offset s1 16) 3)
  (gate-check "(11b) offset past the fake marker in a string" (jolt-marker-line-at-offset s1 30) 3)
  (gate-check "(11b) escaped quote: marker bytes stay in the string" (jolt-marker-line-at-offset s2 24) 3)
  (gate-check "(11b) escaped quote: offset past the string" (jolt-marker-line-at-offset s2 33) 3)
  (gate-check "(11b) '#|L' with no close stays in the string" (jolt-marker-line-at-offset s3 16) 3)
  (gate-check "(11b) '#|L' with no close: offset past the string" (jolt-marker-line-at-offset s3 19) 3)
  (gate-check "(11b) fake marker before any real marker -> #f" (jolt-marker-line-at-offset s4 10) #f)
  (gate-check "(11b) nested comment hides its inner marker" (jolt-marker-line-at-offset s5 24) #f)
  (gate-check "(11b) nested comment: real marker still wins" (jolt-marker-line-at-offset s6 27) 3)
  (gate-check "(11b) #\\\" is a char literal, not a string opener" (jolt-marker-line-at-offset s7 14) 3)
  (gate-check "(11b) marker after a #\\\" char literal registers" (jolt-marker-line-at-offset s7 24) 7))

;; --- (12) integration: lookup resolves markers in REAL emitted output ----------
;; The R3 consumption path: a frame's src byte offset into the generated .scm is
;; fed to jolt-marker-line-at-offset, which must recover the original clj line.
;; Use the real emitted string from (10) — #|L2|# / #|L3|# sit 6 bytes apart-shape
;; (the L is 2 in, the digit 3 in), so offsets just past the 'L' and well into
;; each call must resolve to 2 and 3 respectively, and an offset before the first
;; marker must resolve to #f.
(define (find-sub s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i m) n) #f)
            ((string=? (substring s i (+ i m)) sub) i)
            (else (loop (+ i 1)))))))
(let* ((e (emit-num marker-src))
       (i2 (find-sub e "#|L2|#"))
       (i3 (find-sub e "#|L3|#")))
  (gate-check "(12) offset before the first marker" (jolt-marker-line-at-offset e 1) #f)
  (gate-check "(12) offset inside the line-2 marker" (jolt-marker-line-at-offset e (+ i2 3)) 2)
  (gate-check "(12) offset at the line-2 call" (jolt-marker-line-at-offset e (+ i2 8)) 2)
  (gate-check "(12) offset inside the line-3 marker" (jolt-marker-line-at-offset e (+ i3 3)) 3)
  (gate-check "(12) offset at the line-3 call" (jolt-marker-line-at-offset e (+ i3 8)) 3)
  (gate-check "(12) offset past the last call" (jolt-marker-line-at-offset e (+ i3 30)) 3)
  ;; the file wrapper reads the same generated text from disk
  (let ((p (format "~a/jolt-marker-r1-~a.scm" (host-temp-dir) (random 1000000))))
    (call-with-output-file p (lambda (out) (display e out)))
    (gate-check "(12) file wrapper resolves the same line"
                (jolt-marker-line-in-file p (+ i3 3)) 3)
    (delete-file p)))

;; --- (12b) integration: the R1B defect — a user string's fake marker ----------
;; (def s (fn [] (sink "#|L999|# oops"))) — the string literal carries the exact
;; bytes of a marker. The lookup must resolve every offset after it to the REAL
;; clj line (3, the sink call's line), never 999. The escaped-quote and
;; unclosed-"#|L" variants keep the marker-shaped bytes from ending the string
;; prematurely or leaking into the scan.
(define marker-str-src "(def s\n  (fn [sink]\n    (sink \"#|L999|# oops\")))")
(define marker-esc-src "(def s2\n  (fn [sink]\n    (sink \"say \\\"#|L999|#\\\"\")))")
(define marker-open-src "(def s3\n  (fn [sink]\n    (sink \"#|L\")))")
(let* ((e (emit-num marker-str-src))
       (i3 (find-sub e "#|L3|#"))
       (istr (find-sub e "\"#|L999|# oops\"")))
  (gate-check "(12b) real marker precedes the sink call" (jolt-marker-line-at-offset e (+ i3 3)) 3)
  (gate-check "(12b) offset inside the fake marker in the string" (jolt-marker-line-at-offset e (+ istr 6)) 3)
  (gate-check "(12b) offset past the fake marker, still in the string" (jolt-marker-line-at-offset e (+ istr 14)) 3)
  (gate-check "(12b) offset past the string literal" (jolt-marker-line-at-offset e (+ istr 20)) 3)
  (gate-check "(12b) offset past the whole form" (jolt-marker-line-at-offset e (+ istr 40)) 3))
(let* ((e (emit-num marker-esc-src))
       (istr (find-sub e "\"say \\\"#|L999|#\\\"\"")))
  (gate-check "(12b) escaped quotes: fake marker still inert" (jolt-marker-line-at-offset e (+ istr 8)) 3)
  (gate-check "(12b) escaped quotes: offset past the string" (jolt-marker-line-at-offset e (+ istr 30)) 3))
(let* ((e (emit-num marker-open-src))
       (istr (find-sub e "\"#|L\"")))
  (gate-check "(12b) unclosed '#|L' in a string is inert" (jolt-marker-line-at-offset e (+ istr 2)) 3)
  (gate-check "(12b) unclosed '#|L': offset past the form" (jolt-marker-line-at-offset e (+ istr 20)) 3))

;; --- (13) a non-tail call puts the site slot back when it returns ----------------
;; The emitter spells the restore with the slot's NUMBER (backend_scheme.clj
;; site-restore, so a cached fasl needs nothing of the runtime to expand). That
;; literal must be rt.ss's jolt-vreg-site: if the slot moves, these fail rather
;; than the restore silently clobbering another vreg.
(define site-rd (string-append "(virtual-register " (number->string jolt-vreg-site) ")"))
(define site-wr (string-append "(set-virtual-register! " (number->string jolt-vreg-site) " "))
(let ((e (emit-num "(def _ (fn rt13a [g x] (+ 1 (g x))))")))
  (gate-check "(13) non-tail call to a jolt fn reads jolt-vreg-site" (gate-sub? e site-rd) #t)
  (gate-check "(13) ...and writes it back" (gate-sub? e site-wr) #t))
(let ((e (emit-num "(def _ (fn rt13b [x y] (+ 1 (* x y))))")))
  (gate-check "(13) a numeric op takes no restore" (gate-sub? e "virtual-register") #f))
;; fib's shape: every tail site stores the fn's own pair, so its self-calls skip
;; the restore and no placeholder survives
(let ((e (emit-num "(def _ (fn fib13 [n] (if (< n 2) n (+ (fib13 (- n 1)) (fib13 (- n 2))))))")))
  (gate-check "(13) safe self-calls take no restore" (gate-sub? e "virtual-register") #f)
  (gate-check "(13) ...and leave no placeholder" (gate-sub? e "_srself$") #f))
;; a tail call that can run a jolt fn makes the self-calls restore
(let ((e (emit-num "(def _ (fn fibu13 [g n] (if (< n 2) (g n) (+ (fibu13 g (- n 1)) 1))))")))
  (gate-check "(13) unsafe self-calls restore" (gate-sub? e site-rd) #t)
  (gate-check "(13) ...and leave no placeholder" (gate-sub? e "_srself$") #f))
(set-trace-frames! #f)
(let ((e (emit-num "(def _ (fn rt13c [g x] (+ 1 (g x))))")))
  (gate-check "(13) tracing off: no restore" (gate-sub? e "virtual-register") #f))
(set-trace-frames! #t)

;; --- (14) the restore is spelled once per fn clause, not once per site ---------
;; With inspector information on (jolt build's release mode), Chez keeps every
;; lambda's whole expanded body as data (code-info sexpr), so each inline restore
;; was ~30 pairs of it per site and per enclosing lambda: 0.8.22's restore made a
;; 4,000-fn app's binary 32% bigger (#1423). A site is now a call to a helper the
;; outermost fn's clause binds once, which cp0 inlines back to the same code.
(define (count-sub s sub)
  (let ((m (string-length sub)))
    (let loop ((i 0) (n 0))
      (let ((j (find-sub (substring s i (string-length s)) sub)))
        (if j (loop (+ i j m) (+ n 1)) n)))))
(define site-call-head "(jolt-site-call (lambda () ")
;; four non-tail sites (g m) (h xs) (g a) (h a); (h b) and (g …) are tail calls
(define rt14-src "(def _ (fn rt14 [g h m xs] (let [a (g m) b (h xs)] (if (g a) (h b) (g (h a))))))")
(let ((e (emit-num rt14-src)))
  (gate-check "(14) the restore is spelled once in the fn" (count-sub e "(virtual-register 12)") 1)
  (gate-check "(14) each non-tail site calls it" (count-sub e site-call-head) 4))
;; a nested fn binds its own: sharing the outer clause's left the calls out of
;; line in a built program (bench/keyed_lookup 1.15x)
(let ((e (emit-num "(def _ (fn rt14b [g xs] (g (map (fn [x] (+ 1 (g x))) xs))))")))
  (gate-check "(14) a nested fn binds its own restore" (count-sub e "(virtual-register 12)") 2)
  (gate-check "(14) ...and its site calls it" (gate-sub? e site-call-head) #t))
;; each arity is its own clause, and binds the helper only if it has a site
(let ((e (emit-num "(def _ (fn rt14c ([g] (+ 1 (g 1))) ([g x] (+ 1 (g x) (g x))) ([g x y] x)))")))
  (gate-check "(14) one restore per clause with a site" (count-sub e "(virtual-register 12)") 2))
;; outside any fn there is no clause to bind it in, so the site keeps the inline form
(let ((e (emit-num "(def x14 (str (identity 1)))")))
  (gate-check "(14) a top-level site still restores inline" (gate-sub? e "(virtual-register 12)") #t)
  (gate-check "(14) ...without the helper" (gate-sub? e "jolt-site-call") #f))
;; cp0 must fold every helper call away: what runs is the inline restore, with no
;; closure per site
(let* ((e (emit-num rt14-src))
       (opt (format "~s" (expand/optimize (read (open-input-string e))))))
  (gate-check "(14) cp0 inlines the helper at every site" (gate-sub? opt "jolt-site-call") #f)
  (gate-check "(14) ...leaving no thunk behind" (gate-sub? opt "(lambda ()") #f)
  (gate-check "(14) ...and the restore inline" (count-sub opt "virtual-register) 12)") 4))
;; also where the helper is free in a nested fn, and bound in several arities
(for-each
  (lambda (src)
    (let ((opt (format "~s" (expand/optimize (read (open-input-string (emit-num src)))))))
      (gate-check (string-append "(14) cp0 inlines every helper call: " src)
                  (gate-sub? opt "jolt-site-call") #f)
      (gate-check (string-append "(14) ...and every thunk: " src)
                  (gate-sub? opt "(lambda ()") #f)))
  '("(def _ (fn rt14e [g xs] (g (map (fn [x] (+ 1 (g x))) xs))))"
    "(def _ (fn rt14f ([g] (+ 1 (g 1))) ([g x] (+ 1 (g x) (g x))) ([g x y] x)))"))
;; and it still runs
(ev "(defn rt14d [g h m xs] (let [a (g m) b (h xs)] (if (g a) (h b) (g (h a)))))")
(gate-check "(14) a fn with helper sites runs" (ev "(rt14d identity identity 3 7)") 7)

(gate-summary "trace-emit")
