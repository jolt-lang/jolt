;; dynamic var binding — binding / with-bindings* / var-set / thread-bound? /
;; with-local-vars / with-redefs / bound-fn* / get-thread-bindings.
;;
;; A per-thread dynamic-binding stack: a list of frames, innermost (most recently
;; pushed) at the HEAD. Each frame is (owner . alist): the alist holds
;; (var-cell . value) MUTABLE pairs — so var-set can update the innermost binding
;; in place (set-cdr!), matching Clojure where var-set targets the current
;; binding, not the root — and `owner` is who pushed it (dyn-owner below), the
;; JVM's Var$TBox.thread. A frame is SHARED across threads by conveyance (a
;; future, an agent action, a go block restore the parent's stack, frames and
;; pairs and all, as Frame.clone shares the TBoxes), so the owner is what lets
;; Var.set refuse a write from any thread but the one that bound the var.
;;
;; The binding macro builds a frame as a jolt map (array-map of (var x) -> value);
;; push-thread-bindings folds it into the alist. Lookups walk frames by cell
;; IDENTITY (eq?) — vars are interned, so (var x) always yields the same cell, and
;; this sidesteps a persistent-hash-map-can't-find-a-var-key quirk.
;;
;; var reads (var-deref in compiled code, jolt-var-get / deref on a cell) consult
;; the stack before falling back to the cell root. Loaded LAST (after vars.ss and
;; ns.ss) so it chains the fully-extended jolt-var-get and overrides rt.ss var-deref.

;; THREAD-LOCAL: a Chez thread parameter, so each OS thread (a future / go block)
;; has its own binding stack. Chez initializes a new thread's parameter
;; to the spawning thread's value at fork time, giving Clojure binding conveyance
;; for free (the future shim also installs an explicit snapshot, belt-and-suspenders).
(define dyn-binding-stack (make-thread-parameter '()))

;; --- pushing a frame ---------------------------------------------------------
;; THE one place a frame reaches the stack. Everything else — the loader's
;; per-file vars, cpath-with-compile-files, the agent's *agent*, and
;; push-thread-bindings itself — goes through here, so the dyn-bound? flags below
;; cannot be set in some paths and forgotten in others. (Restoring a whole stack
;; captured elsewhere, which is what the future/agent/executor conveyance does, is
;; not a push: those frames were built by a push that already flagged them.)
;; A hand-rolled loop rather than for-each: this is on every push, and the closure
;; for-each wants cost a one-var frame ~7 ns of its 140.
;; The owner of a frame pushed now: the fiber running here, else the OS thread.
;; A fiber is the unit that binds — its slice carries its own stack, and a fiber
;; spawned inside a binding conveys the parent's frames exactly as a thread
;; does — so on a carrier the thread id alone would let one fiber write another's
;; binding. Gambit has no fibers; its shim answers #f.
(define (dyn-owner) (or (jolt-current-fiber) (get-thread-id)))
;; A frame: who pushed it, its (cell . value) pairs, and the lookups answered
;; through it (dyn-walk-frames below). A frame is made by exactly one push, so it
;; heads exactly one stack, and what a lookup found under it stays true: the
;; frames below are immutable, a binding's value lives in its pair (cached as
;; the pair, so var-set through it is still seen), and "not bound here" cannot
;; change for a stack nobody can extend from below.
(define-record-type dyn-frame (fields owner alist (mutable cache)) (nongenerative dyn-frame-v1))
(define dyn-cache-none (list 'dyn-cache-none))
(define dyn-cache-empty (vector dyn-cache-none #f dyn-cache-none #f dyn-cache-none #f dyn-cache-none #f))
(define (dyn-push-frame! pairs)
  (let loop ((p pairs))
    (when (pair? p)
      (var-cell-dyn-bound?-set! (caar p) #t)
      (loop (cdr p))))
  (dyn-binding-stack (cons (make-dyn-frame (dyn-owner) pairs dyn-cache-empty) (dyn-binding-stack))))

;; (dyn-with-frame pairs thunk) — THE way to scope a binding frame over a
;; dynamic extent. Three sites used to hand-roll it and all three were wrong
;; across a park:
;;
;;     (dynamic-wind (lambda () (dyn-push-frame! pairs))
;;                   thunk
;;                   (lambda () (dyn-binding-stack (cdr (dyn-binding-stack)))))
;;
;; jolt-fiber-to-scheduler! saves the fiber's slice — which already contains the
;; pushed frame — BEFORE the escape unwinds. The unwind pops it. On resume the
;; slice is restored, putting the frame back, and THEN the continuation rewinds
;; and that before-thunk pushes a SECOND one. Measured on an ordinary explicit
;; park, no preemption involved: depth went 1 before the park and 2 after,
;; leaking a frame past the end of the extent.
;;
;; The general rule, which is what the shape below encodes: a winder must not
;; re-establish state the jolt-dslice already carries. The dslice owns
;; dyn-binding-stack, so the push happens ONCE, OUTSIDE the dynamic-wind, and
;; the before-thunk does nothing. A park then costs nothing to get right — the
;; slice was saved before the after-thunk ran, and restoring it on resume is the
;; whole recovery.
;;
;; The after-thunk restores the stack it found rather than popping one frame.
;; Absolute, not relative, for the same reason parameterize is written that way:
;; a relative edit is only correct if the stack is exactly as deep as it was, and
;; an escape is precisely when it is not.
;;
;; Masked at both edges (jolt-wind*, host/chez/locks.ss): an interrupt's escape
;; between the push and the wind left the frame on the stack, and one between the
;; thunk's return and the after-thunk left it there too.
(define (dyn-with-frame pairs thunk)
  (let ((outer (dyn-binding-stack)))
    (jolt-wind* (lambda () #f) (dyn-push-frame! pairs) thunk (dyn-binding-stack outer))))

;; --- reading a var -----------------------------------------------------------
;; THE GATE, and why every read below opens with it.
;;
;; dyn-bound? is Clojure's Var.threadBound. Without it every var reference in
;; compiled code — every clojure.core/map?, every user fn — crossed the whole
;; binding stack to discover there was nothing to find, because the stack is
;; non-empty for the duration of any compile or load and `assq` cannot answer
;; "absent" early. Measured, counting frames crossed:
;;
;;   emitting 120 nested fn literals   2.04M frames, 92.5% of them by lookups that
;;                                     found nothing, 16 vars ever bound in the
;;                                     whole process
;;   compiling clojure/set.clj         194k frames, 99.4% by lookups that found
;;                                     nothing, 20 vars ever bound
;;
;; So the walk was almost entirely spent proving a negative about vars nobody
;; binds, and the flag answers exactly that in one field read: 6 ns flat against
;; 1425 ns at depth 512 (bench/dyn-binding, `make dynbench`).
;;
;; WHAT MAKES IT SOUND is one invariant: if a cell appears in any frame of THIS
;; THREAD's stack, its flag is set. Every way a stack comes to be preserves it —
;; a push flags the frame's cells before publishing it (dyn-push-frame!, the only
;; one), a pop only removes, and the future / agent / executor / fiber-slice paths
;; install a stack some earlier push already built. The flag is never cleared, so
;; nothing can un-flag a cell that is still bound somewhere.
;;
;; That is also why it needs no synchronisation. A binding is visible only to the
;; thread that pushed it and to threads that inherited that stack at fork, and in
;; both cases the flag write happened before the stack the reader can see. Cells
;; flagged by an unrelated thread only cost this one a fruitless walk, which is
;; the direction that is safe. A field write is a whole-value write — the same
;; argument meta and macro? rest on.
;;
;; MONOTONE and process-wide, like the JVM's: a var bound once pays the walk
;; forever after. That costs the ~16 vars that are genuinely dynamic nothing they
;; were not already paying.
;;
;; What it does NOT do is make a var that IS bound cheap to find: that is still a
;; walk, and still O(depth). It is 7.5% of the frames above and was not worth a
;; second mechanism — jolt-3bo records the cumulative index that would kill it and
;; the measurement that said not to bother yet.

;; The walk itself, ungated, so the two readers below test the flag exactly once.
;; Testing it in both dyn-find-binding and dyn-binding-value cost a bound var
;; 8.5 -> 11.3 ns for nothing.
;;
;; A read of a var that IS bound somewhere walked every frame, and a reader binds
;; one per nesting level: tools.reader's read-delimited binds *read-delim* for
;; every list, and read* asks *read-eval* -- bound at the bottom by the load --
;; for every form, so each read crossed the whole stack (#1713). The innermost
;; frame remembers the last four cells asked through it, with the pair found (or
;; #f): a repeat under the same frame is a few eq?s. The memo is a vector
;; replaced whole, never written in place, so a thread sharing the frame (a
;; conveyed stack) reads either the old memo or the new one.
(define (dyn-walk-frames cell)
  (let ((frames (dyn-binding-stack)))
    (and (pair? frames)
         (let* ((top (car frames)) (c (dyn-frame-cache top)))
           (cond ((eq? (vector-ref c 0) cell) (vector-ref c 1))
                 ((eq? (vector-ref c 2) cell) (vector-ref c 3))
                 ((eq? (vector-ref c 4) cell) (vector-ref c 5))
                 ((eq? (vector-ref c 6) cell) (vector-ref c 7))
                 (else
                  (let ((p (let loop ((fs frames))
                             (and (pair? fs)
                                  (or (assq cell (dyn-frame-alist (car fs)))
                                      (loop (cdr fs)))))))
                    (dyn-frame-cache-set! top
                      (vector cell p (vector-ref c 0) (vector-ref c 1)
                              (vector-ref c 2) (vector-ref c 3) (vector-ref c 4) (vector-ref c 5)))
                    p)))))))
;; The FRAME holding the innermost binding of CELL, or #f — for the one caller
;; that needs the owner as well as the pair (jolt-set-var!).
(define (dyn-find-binding-frame cell)
  (and (var-cell-dyn-bound? cell)
       (let loop ((frames (dyn-binding-stack)))
         (and (pair? frames)
              (if (assq cell (dyn-frame-alist (car frames))) (car frames) (loop (cdr frames)))))))

;; the innermost (cell . value) pair binding CELL, or #f
(define (dyn-find-binding cell)
  (and (var-cell-dyn-bound? cell) (dyn-walk-frames cell)))

;; a unique sentinel: distinguishes "no thread binding" from a binding whose
;; value happens to be jolt-nil.
(define dyn-no-binding (list 'no-binding))
;; No empty-stack check of its own: the walk answers that in one pair?, and the
;; gate has already turned away every var that is not dynamic.
(define (dyn-binding-value cell)
  (if (var-cell-dyn-bound? cell)
      (let ((p (dyn-walk-frames cell)))
        (if p
             (let ((val (cdr p)))
               val)   ; no auto-deref — a var-cell value is the value, like JVM
            dyn-no-binding))
      dyn-no-binding))

;; push-thread-bindings: frame is a jolt map of var-cell -> value. Validate each
;; var is ^:dynamic (matching JVM — non-dynamic vars throw), then fold into an
;; identity-keyed alist of mutable pairs and push. Vars without metadata yet
;; (early bootstrap) pass through: the check fires once metadata is settled.
(define (jolt-push-thread-bindings frame)
  (dyn-push-frame! (thread-binding-pairs frame))
  jolt-nil)
;; The scoped form, and the one to reach for: dyn-with-frame restores the whole
;; stack instead of popping a head, so a throw (or a park) inside the extent
;; cannot leave the frame standing for the rest of the thread. Same relationship
;; jolt-with-ns-load-vars has to its own push!/pop! pair.
(define (jolt-with-thread-bindings frame thunk)
  (dyn-with-frame (thread-binding-pairs frame) thunk))
;; A jolt map of var-cell -> value as the alist dyn-push-frame! wants, refusing a
;; non-dynamic var on the way (the check is the JVM's, and belongs to both forms).
(define (thread-binding-pairs frame)
  (let ((pairs (pmap-fold frame
                 (lambda (cell v acc)
                   ;; JVM semantics: only an explicitly ^:dynamic var binds. The
                   ;; FLAG is what says so — a field set by the def that declared
                   ;; it (rt.ss def-var-with-meta!/set-var-meta!, and def-dynvar!
                   ;; for the runtime ones) or by .setDynamic. Reading :dynamic
                   ;; back out of the metadata instead let alter-meta! hand a var
                   ;; the ability to bind, and take it away again, neither of
                   ;; which moves the JVM's flag.
                   (begin
                     (when (not (var-cell-dynamic? cell))
                       ;; Var.pushThreadBindings raises IllegalStateException, not
                       ;; an ExceptionInfo — so ex-data on it is nil, and only a
                       ;; typed host throwable can say that.
                       (throw-jvm (quote IllegalStateException)
                                  (string-append "Can't dynamically bind non-dynamic var: "
                                                 (var-cell-ns cell) "/" (var-cell-name cell)))))
                   (cons (cons cell v) acc))
                 '())))
    pairs))

(define (jolt-pop-thread-bindings)
  (when (pair? (dyn-binding-stack))
    (dyn-binding-stack (cdr (dyn-binding-stack))))
  jolt-nil)

;; RT.load parity for BUILT binaries. An AOT'd namespace's top-level forms
;; replay at boot outside the loader, so a vendored library's top-level
;; (set! *unchecked-math* true) would hit the no-thread-binding throw.
;; `jolt build` brackets each emitted namespace's forms with this pair,
;; mirroring ldr-with-file-vars (and the JVM's RT.load, which binds
;; WARN_ON_REFLECTION/UNCHECKED_MATH around compiled-class inits as well as
;; source loads). Frames push directly like the loader's; an empty frame keeps
;; push/pop balanced if the cells are ever absent.
(define jolt-nsload-cells #f)
(define (jolt-ns-load-var-pairs)
  (unless jolt-nsload-cells
    (set! jolt-nsload-cells
      (let ((cells (map (lambda (nm) (var-cell-lookup "clojure.core" nm))
                        '("*warn-on-reflection*" "*assert*" "*unchecked-math*"))))
        (if (for-all values cells) cells 'missing))))
  (if (pair? jolt-nsload-cells)
      ;; The CURRENT value, not the root. A load nested inside another load --
      ;; a file that requires a namespace, a -main that load-strings -- inherits
      ;; the enclosing file's flags on the JVM, because Compiler.load pushes
      ;; WARN_ON_REFLECTION.deref() rather than its root. Binding the root here
      ;; reset every nested load to the defaults, so an outer (set!
      ;; *unchecked-math* true) stopped applying at the first require and the
      ;; arithmetic under it compiled differently from the arithmetic above it.
      ;; Where this frame is the OUTERMOST one -- an entry (-e, a built binary's
      ;; -main) and the AOT replay this was written for -- nothing is bound
      ;; above it, so the current value IS the root and this reads the same.
      (map (lambda (c) (cons c (var-cell-deref c))) jolt-nsload-cells)
      '()))
(define (jolt-ns-load-vars-push!)
  (dyn-push-frame! (jolt-ns-load-var-pairs))
  jolt-nil)
(define (jolt-ns-load-vars-pop!)
  (when (pair? (dyn-binding-stack))
    (dyn-binding-stack (cdr (dyn-binding-stack))))
  jolt-nil)
;; The same frame for a caller that has a thunk rather than a pair of emitted
;; statements: dyn-with-frame restores the whole stack instead of popping a head,
;; so a nested load that leaves a frame standing cannot make the pop take the
;; wrong one, and a fiber parking mid-load re-enters without pushing twice.
(define (jolt-with-ns-load-vars thunk)
  (dyn-with-frame (jolt-ns-load-var-pairs) thunk))

;; Compiler.load and RT.load bind *ns* to its current value for the duration of
;; a load, so an (ns …) or in-ns in the loaded source switches only the load's
;; own binding and the caller is back in its namespace when the load returns,
;; however it returns. in-ns writes the innermost binding (set-chez-ns!), so a
;; frame here is all it takes. The frame pairs for a caller that pushes its own
;; frame; the thunk form for one that does not.
(define (jolt-ns-binding-pairs)
  (if star-ns-cell (list (cons star-ns-cell (intern-ns! (chez-current-ns)))) '()))
(define (jolt-with-ns-bound thunk)
  (dyn-with-frame (jolt-ns-binding-pairs) thunk))

;; get-thread-bindings: a jolt map of every currently-bound cell -> value,
;; innermost wins. Merge oldest-frame-first (the stack head is innermost). The
;; result can be re-pushed by with-bindings* / bound-fn*.
(define (jolt-get-thread-bindings)
  (let loop ((frames (reverse (dyn-binding-stack))) (m (jolt-hash-map)))
    (if (null? frames)
        m
        (loop (cdr frames)
              (let frame-loop ((alist (dyn-frame-alist (car frames))) (m m))
                (if (null? alist)
                    m
                    (frame-loop (cdr alist)
                                (pmap-assoc m (caar alist) (cdar alist)))))))))

;; __thread-bound? — single var; true iff it has a thread binding.
(define (jolt-thread-bound? v)
  (and (var-cell? v) (dyn-find-binding v) #t))

;; Var.getThreadBinding — the Var$TBox for the innermost binding, or nil.
;;
;; The JVM's frame is a map var -> TBox, so two reads inside one `binding` hand
;; back the SAME box, a read from an inner frame hands back a different one, and
;; a (set! *v* x) through the box's own var leaves the box identical?. jolt's
;; frame entry is the mutable (cell . value) pair dyn-walk-frames already finds,
;; and that pair has precisely that identity: one per frame entry, alive as long
;; as the frame is, mutated in place by jolt-set-var!. So the box is interned OFF
;; the pair rather than built per call, which is the only way the identical?
;; behaviour comes out right.
;;
;; Interning is lazy and off every hot path. Nothing in jolt or clojure.core calls
;; getThreadBinding — SCI's IVar does not name it either — so a binding nobody
;; asks about never allocates a box, and `binding` itself is untouched. The table
;; is global-behind-a-mutex rather than per-thread because the PAIRS it keys on
;; are NOT thread-local: conveyance restores the parent's stack in a future, an
;; agent action or a go block, frames and pairs and all, so the box for one
;; binding can be asked for from more than one thread — and on the JVM it is
;; the same TBox there too, since Frame.clone shares the map. Weak keys so a
;; popped frame's boxes go with it.
(define tbox-tbl (make-weak-eq-hashtable))
(define tbox-mutex (make-mutex))
(define (jolt-var-thread-binding v)
  (let ((p (and (var-cell? v) (dyn-find-binding v))))
    (if (not p)
        jolt-nil
        (jolt-with-mutex tbox-mutex
          (or (hashtable-ref tbox-tbl p #f)
              (let ((b (make-jolt-var-tbox p)))
                (hashtable-set! tbox-tbl p b)
                b))))))

;; jolt-set-var!: Var.set — the ONE write path behind both the set! special form
;; and clojure.core/var-set, which is `(. x (set val))` and nothing else. It
;; writes the innermost thread binding; with no binding it throws, because
;; establishing a root is bindRoot's job (alter-var-root / def / with-redefs) and
;; a var-set that quietly did it instead let code that reference Clojure refuses
;; to run mutate a root shared with every other thread.
;;
;; The validator runs FIRST, ahead of the binding lookup, so a value the var
;; rejects reports the validator rather than the missing binding — the order
;; Var.set has, and observable: (var-set #'v bad) on an unbound var says
;; "Invalid reference state", not "Can't change/establish root binding".
;;
;; A binding found in a frame this thread (or fiber) did not push is a CONVEYED
;; one — the parent's own pair, shared — and Var.set refuses it: "Can't set!: x
;; from non-binding thread" (TBox.thread is checked before the write). Without
;; the check a future's set! wrote straight into the parent's binding, on the
;; parent's thread, with nothing between them; a body that needs its own copy
;; pushes one (binding / bound-fn / with-bindings), as it does on the JVM.
(define (jolt-set-var! v val)
  (if (var-cell? v)
      (begin
        (iref-validate v val)
        (let ((f (dyn-find-binding-frame v)))
          (cond ((not f)
                 (throw-jvm (quote IllegalStateException)
                            (string-append "Can't change/establish root binding of: "
                                           (var-cell-name v) " with set")))
                ((not (eqv? (dyn-frame-owner f) (dyn-owner)))
                 (throw-jvm (quote IllegalStateException)
                            (string-append "Can't set!: " (var-cell-name v)
                                           " from non-binding thread")))
                (else (set-cdr! (assq v (dyn-frame-alist f)) val) val))))
      (throw-jvm (quote ClassCastException) "set!: not a var")))

;; alter-var-root: apply f to the current root plus args, atomically.
;;
;; ATOMICALLY IS THE CONTRACT, and it used to be only the comment. Var.alterRoot is
;; `synchronized` on the JVM, so the read, the compute and the write-back are one
;; step; unlocked, two threads doing (alter-var-root #'n inc) both read the same root
;; and the second write drops the first's increment. Measured, 8 threads x 400
;; increments: the root came back short every run.
;;
;; The lock is the VAR's own object monitor, which is exactly what
;; `synchronized (theVar)` means, rather than one global mutex for every var. A global
;; one would be held across `f`, and f is user code: a thread whose f waits on
;; another thread that wants to alter a DIFFERENT var would deadlock where the JVM
;; does not. Per var, that pair is unrelated.
;;
;; f runs INSIDE the lock, as it does on the JVM (alterRoot calls fn.applyTo under
;; the monitor), and so do the validator and the watches, because doReset notifies
;; under it too. A monitor is now a lock a fiber can hold across a park (jolt-3a87),
;; so an f that parks is fine here.
;;
;; jolt-with-monitor lives in java/concurrency.ss, which rt.ss loads AFTER this file;
;; the reference resolves at call time, the same forward reference
;; jolt-run-interruptible makes to fibers.ss and for the same reason — the earliest
;; caller is the seed prelude (defprotocol emits an alter-var-root to attach the
;; protocol's own :var, and clojure.core's IReader is one), which loads well after
;; rt.ss has read every host file.
;;
;; The READ path is deliberately not locked. jolt-asj records the measurement that
;; rules it out (70 -> 95 ns on the probe), and nothing here needs it: a lost update
;; is a write racing a write, and var-cell-root-set! is a whole-value field write.
(define (jolt-alter-var-root v f . args)
  (jolt-with-monitor v
    (lambda ()
      (let* ((old (var-cell-root v))
             (new (apply jolt-invoke f old args)))
        (iref-validate v new)
        (var-root-set! v new)
        (var-cell-defined?-set! v #t)
        (iref-notify v old new)
        new))))

;; __local-var: a fresh free-standing var cell (not interned). with-local-vars
;; binds these as lexical locals and gives each a THREAD binding for the extent of
;; the form; var-get/var-set read and write that binding. Each gets a unique name
;; so two locals never compare/hash equal as map keys.
;;
;; With no init the root is the Unbound marker, as Var/create leaves it: the cell
;; only ever holds a thread binding, so a local var carried out of the form past
;; the pop reads back unbound rather than nil.
;; The bump and the read are one step — "two locals never compare/hash equal" is
;; the whole reason for the counter, and unlocked two threads draw the same
;; number. See jolt-gensym in converters.ss.
(define local-var-counter 0)
(define local-var-mutex (make-mutex))
(define local-var-meta (jolt-hash-map (keyword #f "dynamic") #t))
(define (jolt-local-var . args)
  (let* ((name (string-append "local-"
                              (number->string
                               (jolt-with-mutex local-var-mutex
                                 (set! local-var-counter (fx+ local-var-counter 1))
                                 local-var-counter))))
         (c (make-var-cell "" name
                           (if (pair? args) (car args) (make-jolt-var-unbound "" name))
                           #t #f #f #f #t)))
    ;; Clojure builds these with Var/create + setDynamic, so a local var takes a
    ;; thread binding like any other — tools.reader hands one to with-bindings.
    ;; Hence the flag in the constructor above, beside the metadata that says the
    ;; same thing to a reader of (meta v).
    (var-cell-meta-set! c local-var-meta)
    c))

;; --- chain the var-read paths onto the binding stack -------------------------

;; var-deref (rt.ss): the compiled-code read path for every clojure.core var
;; reference. Consult the stack first; fall straight back to the root (NOT through
;; jolt-var-get's unbound-error path) so undefined-var reads keep prior behaviour.
;; The *ns* var cell — its reads are thread-local: with no thread-binding they
;; derive from chez-current-ns (a thread-parameter), so *ns* tracks in-ns per
;; thread and a (binding [*ns* ..]) drives resolution. Captured now that *ns* is
;; defined (ns.ss loaded earlier); chez-current-ns consults it too.
(set! star-ns-cell (jolt-var "clojure.core" "*ns*"))

(set! var-deref
  (lambda (ns name)
    (let ((cell (jolt-var ns name)))
      (let ((bv (dyn-binding-value cell)))
        (cond ((not (eq? bv dyn-no-binding)) bv)
              ((eq? cell star-ns-cell) (intern-ns! (chez-current-ns)))
              (else (var-cell-root cell)))))))

;; var-deref's read on an ALREADY-RESOLVED cell — what compiled code emits when it
;; caches the cell at a reference site. Binding stack first, then *ns* thread-local,
;; else the raw root. Lenient on an unbound root (returns the sentinel), matching
;; var-deref — NOT the strict jolt-var-get, which throws "Unbound var".
;;
;; Every var-routed call in compiled code opens with this read, so the common
;; case — a var nobody ever bound, which is not *ns* — is one flag test and the
;; root, with the gate (dyn-binding-value's) and the *ns* test inline instead of
;; behind two more calls: 7 -> ~1 ns on a call through the var (#1713).
(define (var-cell-deref cell)
  (if (or (var-cell-dyn-bound? cell) (eq? cell star-ns-cell))
      (var-cell-deref-bound cell)
      (var-cell-root cell)))
(define (var-cell-deref-bound cell)
  (let ((bv (dyn-binding-value cell)))
    (cond ((not (eq? bv dyn-no-binding)) bv)
          ((eq? cell star-ns-cell) (intern-ns! (chez-current-ns)))
          (else (var-cell-root cell)))))

;; jolt-var-get (vars.ss): the var-get fn + deref/@ on a cell. Stack first, then
;; the original (which errors on an unbound root, matching Clojure).
(define %dyn-var-get jolt-var-get)
(set! jolt-var-get
  (lambda (v)
    (if (var-cell? v)
        (let ((bv (dyn-binding-value v)))
          (cond ((not (eq? bv dyn-no-binding)) bv)
                ((eq? v star-ns-cell) (intern-ns! (chez-current-ns)))
                (else (%dyn-var-get v))))
        (%dyn-var-get v))))

;; bound? (vars.ss): Var.isBound is hasRoot() OR a thread binding — a var with no
;; root is still bound while one is in scope, which is the only state a
;; with-local-vars local is ever in. Chained here for the same reason var-get is:
;; vars.ss loads before the binding stack exists.
(define %dyn-var-bound-one? jolt-var-bound-one?)
(set! jolt-var-bound-one?
  (lambda (v)
    (or (%dyn-var-bound-one? v)
        (and (var-cell? v) (dyn-find-binding v) #t))))

;; var-cell keys hash/compare by ns/name (jolt=2 in vars.ss already compares
;; ns/name) — stable under root mutation, so a var works as a map key (with-redefs
;; builds (hash-map (var f) v); get-thread-bindings returns a var-keyed map).
(register-hash-arm! var-cell? (lambda (x) (equal-hash (cons (var-cell-ns x) (var-cell-name x)))))

;; --- bind the host seams the overlay references -----------------------------
(def-var! "clojure.core" "push-thread-bindings" jolt-push-thread-bindings)
(def-var! "clojure.core" "pop-thread-bindings" jolt-pop-thread-bindings)
(def-var! "clojure.core" "get-thread-bindings" jolt-get-thread-bindings)
(def-var! "clojure.core" "__thread-bound?" jolt-thread-bound?)
(def-var! "clojure.core" "var-set" jolt-set-var!)
(def-var! "clojure.core" "alter-var-root" jolt-alter-var-root)
(def-var! "clojure.core" "__local-var" jolt-local-var)
;; jolt.host/set-var! is the set! special form backend — the same Var.set as
;; clojure.core/var-set above, under the name the analyzer emits.
(def-var! "jolt.host" "set-var!" jolt-set-var!)
;; re-assert var-get / deref to the new (stack-aware) closures (vars.ss captured
;; the pre-chain values).
(def-var! "clojure.core" "var-get" jolt-var-get)
(def-var! "clojure.core" "deref" jolt-deref)
