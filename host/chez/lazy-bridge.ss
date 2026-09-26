;; lazy-seq bridge — make-lazy-seq / coll->cells.
;;
;; The `lazy-seq` macro (00-syntax.clj) expands to
;;   (make-lazy-seq (fn* [] (coll->cells (do body))))
;; and `lazy-cat` to (concat (lazy-seq c) ...). These back every overlay fn
;; built on lazy-seq — repeat / iterate / cycle / dedupe / take-nth / keep /
;; interpose / reductions / tree-seq (-> flatten) / lazy-cat.
;;
;; Bridge to the cseq model (seq.ss): a `jolt-lazyseq` is a deferred seq — a 0-arg
;; thunk that, when forced once, yields a seq (cseq | nil). coll->cells coerces the
;; body result to a seq (= jolt-seq), so the thunk already returns a seq; jolt-seq
;; is extended to force a lazyseq. The one trap: (cons x (a-lazy-seq)) must NOT
;; force the tail (else (repeat x) = (lazy-seq (cons x (repeat x))) loops forever),
;; so jolt-cons defers a lazyseq tail into a lazy cseq cell.
;;
;; Loaded LAST (after host-table.ss): %ls-seq then captures the fully-extended
;; jolt-seq (sorted-aware), so a lazy body returning a sorted coll still seqs.

;; The jolt-lazyseq record (thunk val realized? error? lock meta,
;; jolt-lazyseq-v3) is defined in values.ss with the other collection layouts:
;; hasheq.ss and natives-meta.ss dispatch on it long before this file loads.
;; The thunk field is the node's ONE published word, exactly as a cell's tail is
;; (seq.ss seq-tail-realized?): the thunk -- a procedure, or a lazy-src
;; descriptor -- until the node is forced, and after it the seq (cseq | jolt-nil)
;; or, for a body that threw, a lazyseq-fail carrying the condition. A reader
;; decides from that word alone and never locks. val, realized? and error? are
;; mirrors written before it, for the image (the layout is frozen, and a node
;; written by the two-field protocol arrives with thunk #f and its answer in
;; val/error?, which deliver below still reads).
(define-record-type lazyseq-fail (fields condition) (nongenerative jolt-lazyseq-fail-v1))
(define (lazyseq-pending? t) (or (procedure? t) (lazy-src? t)))
;; realized once it holds its seq (or its failure); a node that has run and only
;; points on to the next is not, as the reference's inner LazySeq is not
(define (jolt-lazyseq-realized? x)
  (let ((t (jolt-lazyseq-thunk x)))
    (not (or (lazyseq-pending? t) (jolt-lazyseq? t) (eq? t lazyseq-walking)))))
;; the lock field's position for the claiming CAS (seq.ss force-claimed!),
;; checked at load like seq.ss's cseq-tail-index
(define jolt-lazyseq-lock-index 4)
(let ((x (make-jolt-lazyseq 'th 'v #f #f #f jolt-nil)))
  (unless (and (sa-record-cas! x jolt-lazyseq-lock-index #f 'probe)
               (eq? (jolt-lazyseq-lock x) 'probe)
               (eq? (jolt-lazyseq-thunk x) 'th) (eq? (jolt-lazyseq-val x) 'v)
               (eq? (jolt-lazyseq-error-flag x) #f))
    (error 'lazy-bridge.ss "jolt-lazyseq-lock-index does not address the lock field")))

;; Thread-safety for lazy realization is only needed once a second OS thread can
;; touch a shared, not-yet-realized node. In single-threaded programs — all of ys
;; and the overwhelming majority of code — a lazy node needs no exclusion at all,
;; and because iterate/repeat/cycle and every map/filter chunk tail is a lazy
;; node, anything paid per node is paid per element of idiomatic seq pipelines.
;;
;; `jolt-mt?` (values.ss) starts #f and flips to #t the first time a real OS
;; thread is spawned (fork-thread is shadowed below). This is race-free: a
;; single thread is either forking or forcing, never both, so no node is being
;; realized on the lock-free path at the instant the flag turns on; and
;; fork-thread establishes happens-before, so the spawned child observes the
;; flip. Once multi-threaded, a first force claims the node by compare-and-swap
;; for the duration (seq.ss force-claimed!) and publishes behind a release
;; fence; reads stay free.

(define (jolt-make-lazy-seq thunk) (make-jolt-lazyseq thunk jolt-nil #f #f #f jolt-nil))
;; A `lazy-seq` form's node (the clojure.core/make-lazy-seq var the macro calls).
;; Its thunk is ^:once and lets go of its captures as it starts, so the node may
;; keep it while it runs, for the rerun after a failure the reference does
;; (lazyseq-take-call!); the val mirror, unused until the node is realized, says so.
(define lazyseq-rerun-tag (list 'lazyseq-rerun))
(define (jolt-make-lazy-seq/once thunk) (make-jolt-lazyseq thunk lazyseq-rerun-tag #f #f #f jolt-nil))
;; the descriptor form: a producer that records what it is instead of closing
;; over it, so the cell can be written to a state image (seq.ss lazy-src).
(define (jolt-make-lazy-src fn a b)
  (make-jolt-lazyseq (make-lazy-src fn a b) jolt-nil #f #f #f jolt-nil))

;; force once and memoize, the reference's LazySeq.realize. A thunk may answer
;; another lazy seq -- (keep f (rest s)) on a skip, (dedupe)'s run of repeats,
;; any `lazy-seq` whose body is a lazy seq -- and coll->cells hands that back
;; UNFORCED, as the reference's force() leaves it in sv. The chain is then walked
;; here, in a loop (the reference's unwrap): each node's thunk runs once, its
;; answer is published as the node's word -- the next node, or the seq -- and the
;; thunk and everything it closed over are released as it goes, as fn = null
;; does. Forcing the inner seq INSIDE the thunk instead made a run of skips a
;; recursion as deep as the run: 50,000 frames in writ's prover, each holding its
;; place in the source, which pinned 31 million cells (2.4GB) the reference
;; frees as it walks.
;;
;; The node asked for never points INTO the chain while it is walked -- the
;; reference nulls sv before unwrap and walks with a local -- so each node the walk
;; passes is garbage as soon as it is passed, unless something else holds it.
;; Publishing the next node on the node asked for kept the whole chain reachable
;; from it, the same retention on the heap. Only the final seq is published there
;; (the reference's s), under the node's claim for the whole walk on the
;; multi-threaded path, as realize() holds the lock. A node the walk passes records
;; its answer -- the next node -- for whoever else holds it (sv), which is why a
;; node may hold another node: it has run but is not realized, as the reference's
;; inner LazySeq has sv and a lock. A body that threw is cached on its own node and
;; re-raised by every force that reaches it, the body running exactly once.
;; The fast test names the two answers a realized node gives; everything else --
;; a thunk, a next node, a fail record, an older image's #f -- is the slow path's.
(define (force-lazyseq x)
  (let ((t (jolt-lazyseq-thunk x)))
    (if (or (cseq? t) (jolt-nil? t)) t (force-lazyseq-slow x t))))
(define (force-lazyseq-slow x t)
  (cond
    ((lazyseq-settled x t) => lazyseq-settle-value)
    ((not jolt-mt?) (lazyseq-realize! x))
    (else
     (force-claimed! x jolt-lazyseq-lock jolt-lazyseq-lock-index jolt-lazyseq-thunk
       (lambda ()
         (let ((t (jolt-lazyseq-thunk x)))
           (cond ((lazyseq-settled x t) => lazyseq-settle-value)
                 (else (lazyseq-realize! x)))))))))
;; A word that needs no running: the node's seq, its failure, or an older image's
;; mirrors -- boxed so a nil answer is still an answer. #f while the node has a
;; thunk to run or a next node to follow.
(define (lazyseq-settled x t)
  (cond ((or (cseq? t) (jolt-nil? t)) (list t))
        ;; a thunk that has been called and has not answered: running now (a
        ;; reentrant force, which the reference also runs again) or failed (which
        ;; it reruns) -- either way, run what the node kept (lazyseq-take-call!)
        ((eq? t lazyseq-walking) #f)
        ((lazyseq-fail? t) (list t))
        ((not t) (list (if (jolt-lazyseq-error-flag x)
                           (make-lazyseq-fail (jolt-lazyseq-val x))
                           (jolt-lazyseq-val x))))
        (else #f)))
(define (lazyseq-settle-value box)
  (let ((v (car box)))
    (if (lazyseq-fail? v) (raise (lazyseq-fail-condition v)) v)))
;; Realize X: its answer (its own thunk's, or the node it already records), then
;; the chain walked from there with nothing but a local, then the seq -- or the
;; failure -- published on X.
;; A node's thunk is taken off it BEFORE it is called, so only the call holds it.
;; A `lazy-seq` thunk also lets go of its own captures as it starts (^:once, as
;; the reference makes it: backend emit-fn), but clojure.core's own producers
;; build their thunks in Scheme, and a thunk still on the node pinned whatever it
;; closed over for the whole call -- drop-while over a 3M run, mapcat over empty
;; colls, ran out of a 256MB heap the JVM does in 96MB. Chez frames keep only
;; live values, so once the node lets go the call frame is the only holder, and a
;; thunk that tail-calls its loop drops even that.
(define lazyseq-walking force-walking)
;; While the thunk runs the node's word is the walking marker, and its val mirror
;; holds what to run if it is forced again before an answer is published -- the
;; reference's LazySeq keeps fn until invoke returns, so a failed body runs again
;; on the next force and a reentrant one runs again at once. A `lazy-seq` node
;; keeps its own thunk there (its captures are already released, so a rerun sees
;; them nil, as the reference's cleared locals are); a clojure.core producer's
;; Scheme thunk is not kept, since it would pin its source, and a rerun answers
;; nil -- where the reference's producers, being ^:once lazy-seqs themselves, end
;; up too.
;; A `lazy-seq` node keeps its thunk across every failed run, not just the first:
;; its val holds the tag before the first run and the thunk itself after, and a
;; rerun is handed that same thunk.
(define (lazyseq-take-call! node t)
  (jolt-lazyseq-val-set! node
    (let ((v (jolt-lazyseq-val node)))
      (if (or (eq? v lazyseq-rerun-tag) (eq? v t)) t lazyseq-nil-thunk)))
  (jolt-lazyseq-thunk-set! node lazyseq-walking)
  (lazyseq-call t))
(define lazyseq-nil-thunk (lambda () jolt-nil))
;; the thunk to run for a node found mid-call or failed (see lazyseq-take-call!)
(define (lazyseq-rerun-thunk node)
  (let ((v (jolt-lazyseq-val node)))
    (if (or (procedure? v) (lazy-src? v)) v lazyseq-nil-thunk)))
;; A body that raises is not recorded: the condition goes on to whoever catches
;; it and the node is left to run again, as the reference leaves fn in place. Once
;; X's own thunk has answered, a failure further down the chain leaves X answering
;; nil, as realize() does after nulling sv. No handler is installed per force --
;; it cost 22ns of every realized element, the most of any part of a force.
(define (lazyseq-realize! x)
  (let* ((t (jolt-lazyseq-thunk x))
         (first (cond ((lazyseq-pending? t) (lazyseq-take-call! x t))
                      ((eq? t lazyseq-walking) (lazyseq-take-call! x (lazyseq-rerun-thunk x)))
                      (else t))))
    (jolt-lazyseq-val-set! x lazyseq-nil-thunk)
    (let ((r (lazyseq-walk first)))
      (lazyseq-publish! x r #f)
      r)))
;; a thunk is a Scheme procedure of no arguments: called directly, not through
;; the generic invoke (which spreads an argument list)
(define (lazyseq-call t) (if (lazy-src? t) (lazy-src-force t) (t)))
;; Walk a chain of lazy seqs to the seq it ends in, in constant stack.
(define (lazyseq-walk v)
  (let loop ((v v))
    (if (jolt-lazyseq? v) (loop (lazyseq-step v)) v)))
;; One node of the chain: its answer if it has one, else its thunk run once and
;; the answer recorded on it (under its claim while the thunk runs, when threads
;; may share it). A failure is recorded on the node and raised.
(define (lazyseq-step node)
  (define (settled-or-run)
    (let ((t (jolt-lazyseq-thunk node)))
      (cond ((lazyseq-settled node t) => lazyseq-settle-value)
            ((jolt-lazyseq? t) t)
            (else
             (let ((r (lazyseq-take-call! node (if (eq? t lazyseq-walking) (lazyseq-rerun-thunk node) t))))
               (lazyseq-publish! node r #f)
               r)))))
  (let ((t (jolt-lazyseq-thunk node)))
    (cond ((or (cseq? t) (jolt-nil? t)) t)
          ((jolt-lazyseq? t) t)
          ((not jolt-mt?) (settled-or-run))
          (else (force-claimed! node jolt-lazyseq-lock jolt-lazyseq-lock-index
                                jolt-lazyseq-thunk settled-or-run)))))
;; mirrors first, then the word readers decide from -- behind a fence on the
;; multi-threaded path so the answer's own fields (and the fail record's, which is
;; why it is built up front) are visible before the word that points to them.
(define (lazyseq-publish! x v fail?)
  (let ((w (if fail? (make-lazyseq-fail v) v)))
    (jolt-lazyseq-val-set! x v)
    (jolt-lazyseq-error-flag-set! x fail?)
    (jolt-lazyseq-realized-flag-set! x (not (jolt-lazyseq? v)))
    (when jolt-mt? (memory-order-release))
    (jolt-lazyseq-thunk-set! x w)))

;; Shadow fork-thread so any spawn (future/agent/core.async/process, all loaded
;; after this file) flips jolt-mt? on and joins the live-thread set. Captured in a
;; prior define so the RHS sees the primitive, not the top-level binding being
;; defined (Chez top-level letrec*).
;;
;; Chez exposes no list of running threads, so the shadow keeps one: a thread
;; enters the set as its body starts and leaves when the body returns. This backs
;; Thread/getAllStackTraces (io.ss), whose callers are leak checks counting
;; threads before and after some work.
;;
;; It is also where a new thread's per-thread state is reset. A Chez thread
;; parameter hands a forked thread the CREATING thread's value, and two of
;; jolt's are a read's transient state that must not travel: the reader's mode
;; switches (rdr-edn-mode, rdr-scan-mode, …, reader.ss) and the STM transaction
;; (*txn*, refs.ss). A go block forked from inside an edn :readers fn used to
;; read every later form on that thread in edn mode, for the life of the
;; thread. Every spawn site did its own reset and five of them (core.async's
;; go/thread/put!/take!/timeout, the subprocess pump, a future's completion
;; callback) had none; doing it here once is what makes the invariant hold for
;; the next spawn site too. A site that needs the parent's dynamic bindings
;; installs them itself, after this (dyn-binding-stack snap).
;; live-threads holds every thread jolt started and has not seen exit, keyed by
;; id: #t while it runs, 'done for the one race below. Recorded by the FORKING
;; thread, from the child's thread object (sa-thread-id-of), rather than by the
;; child on its way in: a child forked while a collection is pending traps at
;; its very first safe point — its thunk's entry — and waits for the
;; collection before any line of it runs, so a self-record would not exist
;; yet when the stall report asks who is waiting (rt.ss jolt-report-gc-stall
;; tells a thread jolt started from one it did not by this table; a thread it
;; did not start can only be running jolt through a :collect-safe callback).
;; The forking thread records the child before it returns from fork-thread,
;; so nothing the parent does next (park in a foreign call, say) can precede
;; the record. The child deletes itself on exit; if it exits before the parent
;; has recorded it — a thunk that finishes in the parent's next few
;; instructions — it leaves 'done, which the parent's late record consumes
;; instead of outliving the thread; a child gone entirely (context released)
;; before the parent reads its id leaves that marker behind, one fixnum key
;; that no later thread's id can equal. jolt-started-thread? reads #t only.
(define live-threads (make-eqv-hashtable))
(define live-threads-mutex (make-mutex))
(define (live-thread-ids)
  (jolt-with-mutex live-threads-mutex
    (let loop ((ks (vector->list (hashtable-keys live-threads))) (acc '()))
      (cond ((null? ks) acc)
            ((eq? #t (hashtable-ref live-threads (car ks) #f)) (loop (cdr ks) (cons (car ks) acc)))
            (else (loop (cdr ks) acc))))))
(define (jolt-started-thread? id)
  (jolt-with-mutex live-threads-mutex (eq? #t (hashtable-ref live-threads id #f))))
;; A thread is born with its creator's signal mask, and jolt has one: the
;; SIGTERM/SIGHUP/SIGINT the shutdown watcher takes over must be blocked in every
;; thread, or the kernel delivers the signal to whichever one does not block it
;; instead of leaving it pending for sigwait (concurrency.ss, #1098). The guard
;; wraps the SPAWN — it blocks on this thread, creates, and restores — so the
;; child has the mask from its first instruction. set! by concurrency.ss once the
;; POSIX mask primitives are up; the identity below is what a host without them
;; (Windows, Gambit) keeps.
(define jolt-fork-sigmask-guard (lambda (fork) (fork)))
(define %ls-orig-fork-thread fork-thread)
(define (%ls-fork-thread mark-mt? thunk)
  (when mark-mt? (jolt-mark-mt!))
  (let* ((t (jolt-fork-sigmask-guard
             (lambda ()
               (%ls-orig-fork-thread
                (lambda ()
                  (*txn* #f)
                  (rdr-default-modes!)
                  (let ((id (get-thread-id)))
                    (dynamic-wind
                      (lambda () #f)
                      thunk
                      (lambda ()
                        (jolt-with-mutex live-threads-mutex
                          (if (hashtable-contains? live-threads id)
                              (hashtable-delete! live-threads id)
                              (hashtable-set! live-threads id 'done)))))))))))
         (id (sa-thread-id-of t)))
    (when id
      (jolt-with-mutex live-threads-mutex
        (if (eq? 'done (hashtable-ref live-threads id #f))
            (hashtable-delete! live-threads id)
            (hashtable-set! live-threads id #t))))
    t))
(define (fork-thread thunk) (%ls-fork-thread #t thunk))

;; A thread that parks in a foreign call and runs no jolt code until something
;; wakes it has not made the process multi-threaded, and saying that it has is not
;; free: jolt-mt? is what puts every lazy cell on the claim path for the rest of
;; the program (1.03-1.06x on a seq pipeline, measured). The shutdown watcher is
;; one of these — armed in every CLI process, asleep in sigwait for the whole life
;; of a program that may never register a hook — so it forks this way instead. The
;; OWNER of a dormant thread is then responsible for marking the process
;; multi-threaded before any jolt code can reach it: for the watcher that is hook
;; registration (concurrency.ss), which is the moment a second mutator becomes
;; possible at all.
(define (fork-thread-dormant thunk) (%ls-fork-thread #f thunk))

;; coll->cells: coerce a `lazy-seq` body's result to a seq | nil -- except a lazy
;; seq, handed back unforced for force-lazyseq to walk in its loop (the
;; reference's sv). Forcing it here, inside the thunk, is what made a chain of
;; lazy seqs a recursion as deep as the chain.
(define (jolt-coll->cells c) (if (jolt-lazyseq? c) c (jolt-seq c)))

;; extend jolt-seq to force a lazyseq (a lazyseq is seqable -> its realized seq).
(register-seq-arm! jolt-lazyseq? force-lazyseq)

;; (cons x lazyseq): the cell's pending tail IS the lazy seq, as the reference's
;; Cons holds a LazySeq as _more -- forced only when the cell is walked, so an
;; infinite (repeat/iterate/cycle) stays productive. A lazy seq is data (its
;; thunk a descriptor or a registered fn), so the cell can still be written to a
;; state image. It used to be wrapped in a cons-tail descriptor: one more record
;; per element of every user `lazy-seq` whose body is (cons x (recur ...)).
(define %ls-cons jolt-cons)
;; kept registered so an image written with one still reads
(define lz-cons-tail
  (register-lazy-src! 'cons-tail (lambda (coll _b) (force-lazyseq coll))))
(set! jolt-cons (lambda (x coll)
  (if (jolt-lazyseq? coll)
      (cseq-lazy x coll)
      (%ls-cons x coll))))

;; (conj lazyseq x): conj onto a seq prepends, like any seq — (conj (rest xs) y).
;; rest returns a lazyseq, so this is a common path; without it conj reports the
;; lazyseq as an "unsupported collection".
(register-conj-arm! jolt-lazyseq? (lambda (coll x) (jolt-cons x coll)))

;; A lazyseq is a NEW value type, so the dispatchers that DON'T route through
;; jolt-seq must learn it or a raw (unrealized) lazyseq escapes — e.g. the corpus
;; compares (= [1 3 5] (take-nth 2 …)) against the raw lazyseq, and jolt=2 would
;; see an unknown type and return false. Recognizing it as sequential is enough
;; for equality + hash (seq=? / seq-hash coerce via jolt-seq); count / empty? /
;; nth / the printers don't, so coerce those explicitly.
(define %ls-sequential? jolt-sequential?)
(set! jolt-sequential? (lambda (x) (or (jolt-lazyseq? x) (%ls-sequential? x))))
(register-count-arm! jolt-lazyseq?
  (lambda (x) (jolt-count (jolt-seq x))))
(register-empty-arm! jolt-lazyseq? (lambda (x) (jolt-empty? (jolt-seq x))))
(define %ls-nth jolt-nth)
(set! jolt-nth (case-lambda
  ((coll i)   (if (jolt-lazyseq? coll) (%ls-nth (jolt-seq coll) i)   (%ls-nth coll i)))
  ((coll i d) (if (jolt-lazyseq? coll) (%ls-nth (jolt-seq coll) i d) (%ls-nth coll i d)))))
;; a lazy seq prints as its realized seq — force, then re-dispatch through the
;; printer. An empty realized lazy seq is still a sequence, printing "()" (like a
;; JVM LazySeq), not "nil" — so (lazy-seq nil) and (rest '(1)) render "()".
(register-pr-str-arm! jolt-lazyseq?
  (lambda (x) (let ((s (jolt-seq x))) (if (jolt-nil? s) "()" (jolt-pr-str s)))))
(register-pr-readable-arm! jolt-lazyseq?
  (lambda (x) (let ((s (jolt-seq x))) (if (jolt-nil? s) "()" (jolt-pr-readable s)))))
(register-str-render! jolt-lazyseq?
  (lambda (x) (let ((s (jolt-seq x))) (if (jolt-nil? s) "()" (jolt-str-render-one s)))))

;; seq? — a lazy seq IS a seq (predicates.ss's jolt-seq? predates the lazyseq
;; record). Unlike the native-op dispatchers above (called via a direct top-level
;; reference, so the set! is enough), seq? is reached through var-deref, which
;; reads the var-cell root — so the patched closure must be re-def-var!'d, not just
;; set!. (Exposed once dynamic binding let with-in-str/line-seq reach seq?.)
(define %ls-seq? jolt-seq?)
(set! jolt-seq? (lambda (x) (or (jolt-lazyseq? x) (%ls-seq? x))))
(def-var! "clojure.core" "seq?" jolt-seq?)

(def-var! "clojure.core" "make-lazy-seq" jolt-make-lazy-seq/once)
(def-var! "clojure.core" "coll->cells" jolt-coll->cells)
