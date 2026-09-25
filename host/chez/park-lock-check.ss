;; park-lock-check.ss — no fiber may leave the CPU while its carrier holds a
;; counted lock, checked as a SHAPE before it can run.
;;
;; THE RULE, which host/chez/locks.ss states and both switch points enforce:
;;
;;   a fiber never leaves the CPU while its carrier holds a counted lock.
;;
;; The runtime check (jolt-locks-assert-none!) is sound and complete for parks
;; that HAPPEN — it sees a park one call away, or inside user code the lock never
;; wrote, because it reads the lock count at the switch. What it cannot do is see
;; a park that nothing exercises. jolt-04ee was exactly that: loader.ss parked a
;; waiting fiber inside ldr-load-mu for two releases, and the window needed
;; concurrent requires of one namespace from several fibers and threads at once,
;; so it sat there reported-but-unreproduced. This check is the half that does not
;; need the window. It reads the code.
;;
;; WHAT IT DOES. Every handwritten host .ss file is read AS DATA (comments and
;; strings cannot lie that way, and the previous mechanism was grep). Then:
;;
;;   1. build the call graph over host definitions — every symbol in OPERATOR
;;      position in each definition's body, with quote bodies, binding names and
;;      case datums walked past so a local named like a primitive is not a call to
;;      it.
;;   2. close "can park" over that graph from the two switch points and their
;;      wrappers, to a fixpoint. So a helper that parks is a parker, and so is
;;      anything that calls the helper, however deep.
;;   3. report every call to a parker that is lexically inside a jolt-with-mutex
;;      body, naming the file, the enclosing definition and the callee.
;;
;; Step 2 is the whole value: the lexical scan this replaces found NOTHING in the
;; pre-fix loader, because ldr-begin-load!'s region called ldr-wait-for-load! and
;; the park was in there. One level of indirection was enough to hide it.
;;
;; TWO KINDS OF FINDING, because the two hazards are found two different ways and
;; mixing them made the check useless before it made it wrong:
;;
;;   park       a call, at any depth in the call graph, to one of the runtime's own
;;              park primitives. This is jolt-04ee, and the transitive closure is
;;              what sees it.
;;   user-code  jolt-invoke named directly inside the region: the lock hands
;;              control to code it did not write, and that code may park anywhere.
;;              Three bugs in this family were exactly that (jolt-3a87 the object
;;              monitor, jolt-232k the delay, jolt-pb2s the transaction).
;;
;; user-code is deliberately LEXICAL and not closed over the call graph, which was
;; tried first and reported 27 sites across nine files. The reason is not that the
;; long chains are false — (swap! …) under the atom lock reaches jolt= reaches a
;; record's user-defined equality — it is that "calls something that can eventually
;; call user code" is true of most of the runtime, so closing over it produces a
;; list nobody can act on and an allowlist that hides the two entries that matter.
;; Named directly in the region, it is a decision somebody made at that spot.
;;
;; WHAT IT DOES NOT DO, stated rather than left to be discovered:
;;
;;   - a lock held by HAND (jolt-lock! … jolt-unlock!) is not a region it can
;;     read, because whether the unlock precedes the park is a question about
;;     control flow, not about nesting. The channel ops (java/fibers-async.ss) are
;;     that shape on purpose — they hold the channel mutex by hand precisely so
;;     they can release it before parking — and a scan that guessed would either
;;     accuse them or learn nothing. Those sites are the runtime check's.
;;   - a park through a value rather than a name — a hook, a var, a record field
;;     holding a procedure — is invisible here. Also the runtime check's.
;;
;; So neither half is sufficient and the pair is: this one rejects the shape
;; before it runs, and the other one catches what a static reading cannot see.
;;
;; It also checks its own teeth. If jolt-locks-assert-none! ever stops being
;; called from a switch point, the runtime half is gone and every gate still
;; passes — so the switch points are checked BY NAME for that call, and losing it
;; fails here.
;;
;;   sh host/chez/park-lock-check.sh          check against the allowlist
;;   sh host/chez/park-lock-check.sh --regen  regenerate it (review the diff!)
(import (chezscheme))

(define scope-roots '("host/chez" "host/chez/java"))
(define allowlist-file "host/chez/park-lock-allowlist.txt")

;; The two switch points. Every park in the runtime ends at one of them, and each
;; one must call the assertion — checked below, because a check nobody calls is
;; indistinguishable from a check that passes.
(define switch-points '(jolt-fiber-to-scheduler! jolt-sm-park!))
(define assertion 'jolt-locks-assert-none!)

;; A switch is a SEQUENCE: commit the fiber's new state (mark it parked or ready,
;; enqueue it, register it as a waiter, which happens under a mutex), then switch.
;; The assertion inside the switch point comes too late to protect that: when it
;; raises, the commit has already happened, and the fiber runs on marked parked,
;; queued, or registered with a waker that will resume it a second time — a second
;; dispatch of a fiber that finished is "fiber in unexpected state". So in every
;; definition that calls a switch point, a lock check must come before the first
;; commit operation, and before the switch itself.
(define lock-checks '(jolt-locks-assert-none! jolt-locks-held jolt-fiber-may-park!))
;; What commits a fiber to leaving, wherever it appears: its state, its place on
;; a run queue, its registration as a channel waiter.
(define commit-ops
  '(jolt-fiber-state-set! jolt-fiber-enqueue!
    async-chan-alt-takers-set! async-chan-alt-putters-set!))
;; ...and, in a definition that calls a switch point directly, the opening of the
;; sequence itself: the non-preemptible region, or the mutex a waiter registers
;; under (jolt-lock-wait's decide runs inside it).
(define sequence-ops '(disable-interrupts jolt-with-mutex))

;; The closure's seeds: the switch points themselves and the two wrappers that
;; exist only to reach them.
(define park-seeds
  '(jolt-fiber-to-scheduler! jolt-sm-park! jolt-fiber-park! sa-fiber-yield))

;; Definitions that park only when no counted lock is held, and otherwise return
;; without parking (locks.ss jolt-fiber-wait-turn!). Calling one from inside a
;; region cannot leave the CPU, so they do not grow the closure — which is what
;; lets a lazy seq's forcer wait by giving its carrier away without making every
;; seq traversal "can park". The teeth: each must test jolt-locks-held before its
;; first call to anything that parks (guarded-parks-unguarded below).
(define guarded-parks '(jolt-fiber-wait-turn!))

;; Calling into code the lock did not write. Counted wherever it appears in a
;; region, not only in operator position, because (apply jolt-invoke f args) is
;; every bit as much a call.
(define user-code-calls '(jolt-invoke))

;; The lock region. Only jolt-with-mutex: a MONITOR is not a counted lock —
;; ownership is a field, which is what lets a fiber hold one across a park — so
;; jolt-with-monitor bodies are not regions and must not be flagged.
(define lock-form 'jolt-with-mutex)

;; ---------------------------------------------------------------------------
;; small helpers
;; ---------------------------------------------------------------------------

(define (has-suffix? s p)
  (let ((ls (string-length s)) (lp (string-length p)))
    (and (>= ls lp) (string=? (substring s (- ls lp) ls) p))))

(define (sort-list lst cmp) (sort cmp lst))

(define (files-in dir)
  (let loop ((es (directory-list dir)) (acc '()))
    (cond
      ((null? es) acc)
      ((and (has-suffix? (car es) ".ss")
            (not (file-directory? (string-append dir "/" (car es)))))
       (loop (cdr es) (cons (string-append dir "/" (car es)) acc)))
      (else (loop (cdr es) acc)))))

(define (find-files)
  (sort-list
    (let loop ((ds scope-roots) (acc '()))
      (if (null? ds) acc (loop (cdr ds) (append (files-in (car ds)) acc))))
    string<?))

(define (read-datums path)
  (let ((p (open-input-file path)))
    (let loop ((acc '()))
      (let ((x (read p)))
        (if (eof-object? x)
            (begin (close-port p) (reverse acc))
            (loop (cons x acc)))))))

;; ---------------------------------------------------------------------------
;; the walk
;; ---------------------------------------------------------------------------
;; One walker, three jobs, distinguished by what the caller does with each
;; emission: the call graph (operator position), the park findings (operator
;; position inside a region), and the user-code findings (any position inside a
;; region). Written once so the three can never disagree about what a call is.
;;
;; emit is called as (emit sym in-lock? operator?) for every symbol reached, with
;; quote bodies, binding names, lambda formals and case datums walked past.

(define (walk-forms forms in-lock? emit)
  (for-each (lambda (f) (walk f in-lock? emit)) forms))

;; binding lists: walk the INITS, not the names. (let ((jolt-invoke 1)) …) binds a
;; local; it is not a call, and a scan that counted it would be answered with an
;; allowlist entry rather than a fix.
(define (walk-bindings bs in-lock? emit)
  (for-each (lambda (b) (when (and (pair? b) (pair? (cdr b)))
                          (walk (cadr b) in-lock? emit)))
            bs))

(define (walk-let d in-lock? emit)
  (let* ((named? (and (pair? (cdr d)) (symbol? (cadr d))))
         (bs (if named? (caddr d) (cadr d)))
         (body (if named? (cdddr d) (cddr d))))
    (when (list? bs) (walk-bindings bs in-lock? emit))
    (walk-forms body in-lock? emit)))

(define (walk-lambda-body d in-lock? emit)
  ;; (lambda formals body …) — formals are names, never calls
  (walk-forms (cddr d) in-lock? emit))

(define (walk-case-lambda d in-lock? emit)
  (for-each (lambda (cl) (when (pair? cl) (walk-forms (cdr cl) in-lock? emit)))
            (cdr d)))

(define (walk-define d in-lock? emit)
  ;; (define (name . formals) body …) or (define name expr)
  (if (pair? (cadr d))
      (walk-forms (cddr d) in-lock? emit)
      (walk-forms (cddr d) in-lock? emit)))

(define (walk-case d in-lock? emit)
  ;; (case key ((datum …) expr …) … (else expr …)) — the datum lists are DATA
  (walk (cadr d) in-lock? emit)
  (for-each (lambda (cl) (when (pair? cl) (walk-forms (cdr cl) in-lock? emit)))
            (cddr d)))

(define (walk-do d in-lock? emit)
  (for-each (lambda (b)
              (when (and (pair? b) (pair? (cdr b)))
                (walk (cadr b) in-lock? emit)
                (when (pair? (cddr b)) (walk (caddr b) in-lock? emit))))
            (cadr d))
  (when (pair? (cddr d))
    (walk-forms (caddr d) in-lock? emit)
    (walk-forms (cdddr d) in-lock? emit)))

(define (walk x in-lock? emit)
  (cond
    ((symbol? x) (emit x in-lock? #f))   ; a value position: (apply jolt-invoke …)
    ((not (pair? x)) (void))
    ((not (list? x))                     ; improper: (a . b) — walk both halves
     (walk (car x) in-lock? emit)
     (walk (cdr x) in-lock? emit))
    (else
     (let ((head (car x)))
       (case head
         ((quote) (void))                ; data, not code
         ((let let* letrec letrec* let-values let*-values) (walk-let x in-lock? emit))
         ((lambda) (walk-lambda-body x in-lock? emit))
         ((case-lambda) (walk-case-lambda x in-lock? emit))
         ((define define-syntax) (walk-define x in-lock? emit))
         ((case) (walk-case x in-lock? emit))
         ((do) (walk-do x in-lock? emit))
         (else
          (cond
            ;; THE REGION. (jolt-with-mutex mu body …): the mutex expression is
            ;; evaluated before the lock is taken, the body under it.
            ((eq? head lock-form)
             (when (pair? (cdr x)) (walk (cadr x) in-lock? emit))
             (when (pair? (cdr x)) (walk-forms (cddr x) #t emit)))
            (else
             (if (symbol? head)
                 (emit head in-lock? #t)
                 ;; the head may itself be a form: ((f x) y)
                 (walk head in-lock? emit))
             (walk-forms (cdr x) in-lock? emit)))))))))

;; ---------------------------------------------------------------------------
;; per-file collection
;; ---------------------------------------------------------------------------
;; A "unit" is one top-level definition: (file name calls locked-calls locked-any).
;; calls is every operator-position symbol in the body (the call graph edge set),
;; locked-calls the operator-position subset inside a jolt-with-mutex, and
;; locked-any every symbol inside one in any position. Top-level forms that are not
;; definitions are collected under the name |toplevel| so a lock region at file
;; scope is still read.

(define (unit-file u) (car u))
(define (unit-name u) (cadr u))
(define (unit-calls u) (caddr u))
(define (unit-locked u) (cadddr u))
(define (unit-locked-any u) (car (cddddr u)))
(define (unit-heads u) (cadr (cddddr u)))

;; Every operator-position symbol in textual (evaluation) order, the region form
;; included — the ordering rule above needs order, which the call sets do not keep.
(define (ordered-heads forms)
  (let ((acc '()))
    (let walk ((x forms))
      (when (pair? x)
        (cond
          ((eq? (car x) 'quote) (void))
          ((list? x)
           (when (symbol? (car x)) (set! acc (cons (car x) acc)))
           (for-each walk x))
          (else (walk (car x)) (walk (cdr x))))))
    (reverse acc)))

(define (definition-name d)
  (and (pair? d) (eq? 'define (car d)) (pair? (cdr d))
       (if (pair? (cadr d)) (car (cadr d)) (cadr d))))

(define (collect-unit file name forms)
  (let ((calls '()) (locked '()) (locked-any '()))
    (walk-forms forms #f
      (lambda (sym in-lock? operator?)
        (when operator? (set! calls (cons sym calls)))
        (when (and in-lock? operator?) (set! locked (cons sym locked)))
        (when in-lock? (set! locked-any (cons sym locked-any)))))
    (list file name calls locked locked-any (ordered-heads forms))))

(define (collect-file file)
  (let loop ((ds (read-datums file)) (acc '()))
    (cond
      ((null? ds) (reverse acc))
      (else
       (let* ((d (car ds))
              (nm (definition-name d)))
         (loop (cdr ds)
               (cons (if nm
                         (collect-unit file nm (cddr d))
                         (collect-unit file '|toplevel| (list d)))
                     acc)))))))

;; ---------------------------------------------------------------------------
;; the closure: which host definitions can park
;; ---------------------------------------------------------------------------
;; Seeded with the switch points and jolt-invoke, then grown to a fixpoint: a
;; definition parks if it calls anything that parks. This is what sees a park one
;; call away from a lock region, which is the shape a lexical scan misses and the
;; shape jolt-04ee actually had.
;;
;; Deliberately name-based and therefore over-approximate in one direction: two
;; definitions with one name in different files are one node. The runtime has no
;; such pair (the host is one flat namespace, so it could not), and erring towards
;; "parks" is the safe direction for a check whose false answers should be
;; false ALARMS, not false silence.

(define (build-parkers units)
  (let ((parks (make-eq-hashtable)))
    (for-each (lambda (s) (hashtable-set! parks s #t)) park-seeds)
    (let grow ()
      (let ((changed #f))
        (for-each
          (lambda (u)
            (unless (or (hashtable-ref parks (unit-name u) #f)
                        (memq (unit-name u) guarded-parks))
              (when (let loop ((cs (unit-calls u)))
                      (cond ((null? cs) #f)
                            ((hashtable-ref parks (car cs) #f) #t)
                            (else (loop (cdr cs)))))
                (hashtable-set! parks (unit-name u) #t)
                (set! changed #t))))
          units)
        (when changed (grow))))
    parks))

;; ---------------------------------------------------------------------------
;; findings
;; ---------------------------------------------------------------------------
;; (file definition kind callee count), sorted, one line each.

(define (tally syms keep?)
  (let ((t (make-eq-hashtable)))
    (for-each (lambda (s) (when (keep? s) (hashtable-set! t s (+ 1 (hashtable-ref t s 0)))))
              syms)
    t))

(define (tally->findings file name kind t)
  (map (lambda (c) (list file name kind c (hashtable-ref t c 0)))
       (sort-list (vector->list (hashtable-keys t))
                  (lambda (a b) (string<? (symbol->string a) (symbol->string b))))))

(define (findings units parks)
  (sort-list
    (let loop ((us units) (out '()))
      (if (null? us)
          out
          (let ((u (car us)))
            (loop (cdr us)
                  (append
                    (tally->findings (unit-file u) (unit-name u) 'park
                      (tally (unit-locked u) (lambda (s) (hashtable-ref parks s #f))))
                    (tally->findings (unit-file u) (unit-name u) 'user-code
                      (tally (unit-locked-any u) (lambda (s) (memq s user-code-calls))))
                    out)))))
    (lambda (a b) (string<? (finding->line a) (finding->line b)))))

(define (finding->line f)
  (string-append (car f) " " (symbol->string (cadr f)) " " (symbol->string (caddr f))
                 " " (symbol->string (cadddr f)) " "
                 (number->string (car (cddddr f)))))

;; ---------------------------------------------------------------------------
;; the teeth check: the switch points must still call the assertion
;; ---------------------------------------------------------------------------

(define (missing-assertions units)
  (let loop ((sps switch-points) (missing '()))
    (cond
      ((null? sps) (reverse missing))
      (else
       (let* ((sp (car sps))
              (defs (let f ((us units) (acc '()))
                      (cond ((null? us) acc)
                            ((eq? (unit-name (car us)) sp) (cons (car us) acc))
                            (else (f (cdr us) acc))))))
         (loop (cdr sps)
               (cond
                 ((null? defs) (cons (cons sp "not defined anywhere") missing))
                 ((let g ((ds defs))
                    (cond ((null? ds) #f)
                          ((memq assertion (unit-calls (car ds))) #t)
                          (else (g (cdr ds)))))
                  missing)
                 (else (cons (cons sp "does not call the assertion") missing)))))))))

(define (index-of pred xs)
  (let loop ((xs xs) (i 0))
    (cond ((null? xs) #f) ((pred (car xs)) i) (else (loop (cdr xs) (+ i 1))))))

;; Definitions that can park (the closure), other than the switch points
;; themselves, whose first commit operation — or first call to anything that parks
;; — comes before any lock check. The commit is often a level above the switch (a
;; channel op registers its waiter, then calls the wait), so this reads every
;; definition in the closure, not only the switch points' direct callers.
(define (unchecked-commits units parks)
  (let loop ((us units) (bad '()))
    (if (null? us)
        (reverse bad)
        (let* ((u (car us)) (hs (unit-heads u)))
          (loop (cdr us)
                (if (and (not (memq (unit-name u) switch-points))
                         (hashtable-ref parks (unit-name u) #f)
                         (or (index-of (lambda (h) (memq h commit-ops)) hs)
                             (index-of (lambda (h) (memq h switch-points)) hs)))
                    (let* ((direct? (index-of (lambda (h) (memq h switch-points)) hs))
                           (chk (index-of (lambda (h) (memq h lock-checks)) hs))
                           (first-commit
                             (index-of (lambda (h) (or (memq h commit-ops)
                                                       (and direct? (memq h sequence-ops))
                                                       (memq h switch-points)))
                                       hs)))
                      (if (and chk (< chk first-commit))
                          bad
                          (cons (string-append (unit-file u) " "
                                               (symbol->string (unit-name u)))
                                bad)))
                    bad))))))

;; A guarded park whose guard is gone, or comes after the park.
(define (guarded-parks-unguarded units parks)
  (let loop ((gs guarded-parks) (bad '()))
    (if (null? gs)
        (reverse bad)
        (let* ((defs (filter (lambda (u) (eq? (unit-name u) (car gs))) units))
               (ok? (and (pair? defs)
                         (let* ((hs (unit-heads (car defs)))
                                (guard (index-of (lambda (h) (eq? h 'jolt-locks-held)) hs))
                                (park (index-of (lambda (h) (hashtable-ref parks h #f)) hs)))
                           (and guard park (< guard park))))))
          (loop (cdr gs) (if ok? bad (cons (symbol->string (car gs)) bad)))))))

;; ---------------------------------------------------------------------------
;; allowlist
;; ---------------------------------------------------------------------------

(define (allowlist-lines)
  (if (file-exists? allowlist-file)
      (let ((p (open-input-file allowlist-file)))
        (let loop ((acc '()))
          (let ((l (get-line p)))
            (cond
              ((eof-object? l) (close-port p) (reverse acc))
              ((or (string=? l "")
                   (and (> (string-length l) 0) (char=? #\# (string-ref l 0))))
               (loop acc))
              (else (loop (cons l acc)))))))
      '()))

(define (write-allowlist! lines)
  (let ((p (open-output-file allowlist-file 'truncate)))
    (for-each (lambda (l) (put-string p l) (put-string p "\n"))
      (list "# host/chez/park-lock-allowlist.txt — generated. Regenerate with:"
            "#   sh host/chez/park-lock-check.sh --regen"
            "# Lines: <file> <definition> <kind> <callee> <count>, for a call made from"
            "# inside a jolt-with-mutex region. kind `park` is a callee that can reach"
            "# one of the runtime's park primitives; kind `user-code` is a call into code"
            "# the lock did not write. A count that DROPS is stale and fails the gate, so"
            "# fixing a site must update this file. Empty is the goal and the current"
            "# state: no fiber leaves the CPU while its carrier holds a counted lock."))
    (for-each (lambda (l) (put-string p l) (put-string p "\n")) lines)
    (close-port p)))

;; ---------------------------------------------------------------------------
;; main
;; ---------------------------------------------------------------------------

;; A line is "<file> <definition> <kind> <callee> <count>". The KEY is everything
;; but the count, so a site keeps its identity while its count moves.
(define (last-space l)
  (let loop ((i (- (string-length l) 1)))
    (cond ((< i 0) #f)
          ((char=? #\space (string-ref l i)) i)
          (else (loop (- i 1))))))

(define (line-key l)
  (let ((i (last-space l))) (if i (substring l 0 i) l)))

(define (line-count l)
  (let ((i (last-space l)))
    (if i (or (string->number (substring l (+ i 1) (string-length l))) 0) 0)))

;; "…/loader.ss ldr-begin-load! park ldr-wait-for-load! 1" -> the kind field
(define (line-kind l)
  (let loop ((i 0) (spaces 0) (start 0))
    (cond
      ((>= i (string-length l)) "")
      ((char=? #\space (string-ref l i))
       (cond ((= spaces 2) (substring l start i))
             (else (loop (+ i 1) (+ spaces 1) (+ i 1)))))
      (else (loop (+ i 1) spaces start)))))

(define (fix-hint l)
  (if (string=? "user-code" (line-kind l))
      (string-append " — this region hands control to code the lock did not write,"
                     " which may park; move the call out of the region")
      (string-append " — commit under the lock and switch outside it"
                     " (jolt-lock-wait, host/chez/locks.ss)")))

(define (find-line key lines)
  (let loop ((ls lines))
    (cond ((null? ls) #f)
          ((string=? key (line-key (car ls))) (car ls))
          (else (loop (cdr ls))))))

(define (main args)
  (let* ((files (find-files))
         (units (let loop ((fs files) (acc '()) (errs '()))
                  (if (null? fs)
                      (cons acc (reverse errs))
                      (guard (e (#t (loop (cdr fs) acc
                                          (cons (car fs) errs))))
                        (loop (cdr fs) (append (collect-file (car fs)) acc) errs)))))
         (bad-reads (cdr units))
         (units (car units)))
    ;; A file that will not read is a HOLE, not something to skip past.
    (unless (null? bad-reads)
      (for-each (lambda (f) (printf "  UNREADABLE: ~a\n" f)) bad-reads)
      (printf "park/lock check: FAILED\n")
      (exit 1))
    (let* ((parks (build-parkers units))
           (got (map finding->line (findings units parks)))
           (missing (missing-assertions units))
           (unchecked (unchecked-commits units parks))
           (unguarded (guarded-parks-unguarded units parks)))
      (cond
        ((and (pair? args) (string=? (car args) "--regen"))
         (write-allowlist! got)
         (printf "park/lock check: regenerated ~a entries\n" (length got))
         (exit 0))
        (else
         (let ((want (allowlist-lines)) (problems '()))
           (define (add! s) (set! problems (cons s problems)))
           ;; the teeth first: without the runtime half this check is a lint
           (for-each
             (lambda (m)
               (add! (string-append "  SWITCH POINT UNGUARDED: " (symbol->string (car m))
                                    " " (cdr m) " — the runtime half of the rule is"
                                    " gone (host/chez/locks.ss)")))
             missing)
           (for-each
             (lambda (u)
               (add! (string-append "  COMMIT BEFORE CHECK: " u " — call"
                                    " jolt-locks-assert-none! before it commits the"
                                    " fiber's state (see commit-ops above)")))
             unchecked)
           (for-each
             (lambda (g)
               (add! (string-append "  GUARDED PARK UNGUARDED: " g " — it must test"
                                    " jolt-locks-held before it parks, or it is an"
                                    " ordinary park and belongs in the closure")))
             unguarded)
           (for-each
             (lambda (g)
               (let ((w (find-line (line-key g) want)))
                 (cond
                   ((not w)
                    (add! (string-append "  NEW site: " g (fix-hint g))))
                   ((> (line-count g) (line-count w))
                    (add! (string-append "  MORE of them: " g " (was "
                                         (number->string (line-count w)) ")"
                                         (fix-hint g)))))))
             got)
           (for-each
             (lambda (w)
               (let ((g (find-line (line-key w) got)))
                 (when (< (if g (line-count g) 0) (line-count w))
                   (add! (string-append "  STALE allowlist entry: " w " -> "
                                        (number->string (if g (line-count g) 0))
                                        " — rerun with --regen")))))
             want)
           (cond
             ((pair? problems)
              (for-each (lambda (p) (printf "~a\n" p)) (reverse problems))
              (printf "park/lock check: FAILED\n")
              (exit 1))
             (else
              (printf "park/lock check: passed (~a files, ~a definitions, ~a can park, ~a allowlisted sites, target is 0)\n"
                      (length files) (length units)
                      (vector-length (hashtable-keys parks)) (length got))
              (exit 0)))))))))

(main (cdr (command-line)))
