;; refs.ss — Clojure refs and STM for the Chez host.
;;
;; Multiversion concurrency with a global commit clock, the shape of the JVM's
;; LockingTransaction. A transaction reads the world as of its READ POINT (the
;; clock when it started), buffers its writes in its own table, and commits under
;; one short lock: it re-checks that nothing it wrote or ensured was committed
;; after its read point, writes every ref at a new commit point, and only then
;; advances the clock. A read that finds no version at or before the read point,
;; or a commit that finds a newer version, throws the retry and the body runs
;; again from the start, up to the JVM's 10000 attempts.
;;
;; No lock is held while the body runs. A body may block on another thread's
;; transaction (deref a future that runs a dosync) and a fiber may park inside a
;; body, and neither can deadlock or lose an update: the waiter holds nothing, and
;; a write derived from a stale read is caught at commit and retried.
;;
;; Watches fire once per changed ref after commit, outside the commit lock.
;; Refs participate in the IRef seam (watches/validators/metadata) like
;; atom/var/agent.  Loaded after atoms.ss (shares jolt-iref-state-throw and
;; the iref tables).

;; Refs travel in state images by VALUE (state-image.ss substitutes an
;; image-ref descriptor on dump and re-mints through make-jolt-ref on
;; restore), so this layout is NOT image-format surface and may change
;; freely. The uids jolt-ref-v1 and jolt-ref-v2 are RETIRED: format-2 images
;; carry refs as raw nongenerative jolt-ref-v1 records (fields: val lock), and
;; the legacy restore arm depends on materializing that rtd from the fasl
;; without a live conflict — never reuse a retired uid for a different layout.
;;
;;   tvals       — the committed history, newest first: a list of (point . val).
;;                 Never mutated, only replaced, so a reader that loads it holds a
;;                 consistent history without a lock.
;;   faults      — reads that found no version old enough since the last commit
;;                 that kept one more; a fault lets the next commit grow the history
;;   min-hist / max-hist — the history the ref keeps (ref-min-history)
(define-record-type (jolt-ref %make-jolt-ref jolt-ref?)
  (fields (mutable tvals) (mutable faults) (mutable min-hist) (mutable max-hist))
  (nongenerative jolt-ref-v3))

(define (make-jolt-ref v) (%make-jolt-ref (list (cons 0 v)) 0 0 10))
;; the newest committed value
(define (jolt-ref-val ref) (cdar (jolt-ref-tvals ref)))
;; A write outside any transaction (the loader's *loaded-libs* mark, an image
;; restore): the newest version takes the value and keeps its point.
(define (jolt-ref-val-set! ref v)
  (let ((tv (jolt-ref-tvals ref)))
    (let ((ntv (cons (cons (caar tv) v) (cdr tv))))
      (when jolt-mt? (memory-order-release))
      (jolt-ref-tvals-set! ref ntv))))

;; IRef arm: refs are watchable/validatable through the iref tables.
(register-iref-arm! jolt-ref?)

;; --- transaction record -------------------------------------------------------
;; A per-transaction record, held in the thread-parameter *txn* (#f when
;; no transaction is running). One record per attempt: a retry starts a new one.
;;   read-point    — the clock when the attempt started
;;   vals          — eq-hashtable: ref -> (in-txn value . set?), set? true once
;;                   ref-set / alter wrote it (a commuted ref's is #f)
;;   commutes      — alist: ref -> its commute fns, newest first, as (f . args)
;;   ensures       — list of the refs ensured
;;   pending-sends — thunks dispatching the agent sends made inside the txn
(define-record-type jolt-txn
  (fields read-point vals (mutable commutes) (mutable ensures) (mutable pending-sends))
  (nongenerative jolt-txn-v2))

(define (make-txn rp)
  (make-jolt-txn rp (make-eq-hashtable) '() '() '()))

;; --- transaction state -------------------------------------------------------
;; stm-clock is the point of the last commit. Only a commit writes it, under
;; stm-lock, and it advances AFTER the commit's refs are written: a transaction
;; that reads point c then sees every ref commit c wrote.
;;
;; stm-lock is an object MONITOR and not a mutex because the commit runs user code
;; (commute fns, validators), and a fiber that parks there must keep the lock: a
;; mutex is released by jolt-with-mutex's wind on a park, a monitor's ownership is
;; a field keyed on the execution context, which a switch does not disturb. A
;; fiber contender parks on the monitor's waiter list rather than blocking its
;; carrier. jolt-with-monitor lives in java/concurrency.ss, which rt.ss loads
;; AFTER this file; the reference resolves at call time.
(define stm-clock 0)
(define stm-lock (vector 'stm-lock))
(define *txn* (make-thread-parameter #f))
(define stm-retry-limit 10000)

;; (dyn-with-txn txn thunk) — scope *txn* over a dynamic extent.
;;
;; NOT `parameterize`, and the reason is the rule dyn-binding.ss already states at
;; dyn-with-frame: a winder must not re-establish state the jolt-dslice already
;; carries. The dslice carries *txn* — that is how a fiber parked inside a dosync
;; resumes inside its own transaction — and Chez's parameterize is a SWAP: one
;; thunk is both the wind's before and its after, exchanging the parameter with a
;; saved slot. jolt-fiber-slice-restore! writes *txn* BEFORE the rewind runs that
;; swap, so the saved slot would come back holding the transaction instead of the
;; outer nil, and the way out would then restore the TRANSACTION: the fiber's next
;; dosync would see (*txn*) non-nil, take the nested arm and join a dead
;; transaction whose log nobody commits.
;;
;; So the same shape dyn-with-frame uses, for the same reason. The set happens ONCE,
;; outside the wind, so a rewind cannot re-establish anything; and the after-thunk
;; restores an ABSOLUTE value rather than swapping, so it is right however many times
;; the extent is left and re-entered. The slice is then the single owner of *txn*
;; across a switch, which is what fibers.ss says the design is.
;;
;; The after-thunk DOES fire on a park: the slice owns *txn*, so letting it revert
;; and be restored is correct. That rests on a park being a CAPTURE park that
;; rewinds, which is what fn* opacity gives dosync (java/sm.ss's invariant;
;; run-gosm.ss checks it).
(define (dyn-with-txn txn thunk)
  (let ((outer (*txn*)))
    ;; masked at its edges (jolt-wind*, locks.ss): an interrupt's escape at either
    ;; left *txn* set, and the thread ran on as if inside the transaction
    (jolt-wind* (lambda () #f) (*txn* txn) thunk (*txn* outer))))

;; --- the retry ------------------------------------------------------------------
;; A clojure.lang.LockingTransaction$RetryEx, which is an Error on the JVM, so a
;; body's (catch Exception e) does not swallow it. One instance serves every retry,
;; made on first use under a mutex so every thread raises and recognizes the same one.
(define stm-retry-ex #f)
(define stm-retry-mu (make-mutex))
(define (stm-retry!)
  (unless stm-retry-ex
    (jolt-with-mutex stm-retry-mu
      (unless stm-retry-ex
        (set! stm-retry-ex (jolt-host-throwable "clojure.lang.LockingTransaction$RetryEx" jolt-nil)))))
  (jolt-throw stm-retry-ex))
(define (stm-retry? e)
  (and stm-retry-ex (eq? (jolt-unwrap-throw e) stm-retry-ex)))

;; --- in-txn reads and writes -------------------------------------------------

;; Sentinel for hashtable-ref to distinguish "not found" from a valid value.
(define txn-not-found (list 'txn-not-found))

;; The newest version at or before point rp, or txn-not-found.
(define (ref-val-at ref rp)
  (let loop ((tv (jolt-ref-tvals ref)))
    (cond ((null? tv) txn-not-found)
          ((<= (caar tv) rp) (cdar tv))
          (else (loop (cdr tv))))))

;; Read a ref inside a transaction: its in-txn value, else the version the read
;; point sees. No version that old is a fault, and the attempt retries.
(define (txn-get txn ref)
  (let ((cell (hashtable-ref (jolt-txn-vals txn) ref #f)))
    (if cell
        (car cell)
        (let ((v (ref-val-at ref (jolt-txn-read-point txn))))
          (when (eq? v txn-not-found)
            (jolt-ref-faults-set! ref (+ 1 (jolt-ref-faults ref)))
            (stm-retry!))
          v))))

(define (txn-read ref)
  (let ((txn (*txn*)))
    (if txn (txn-get txn ref) (jolt-ref-val ref))))

;; A ref committed after the read point is a write conflict.
(define (txn-check-unchanged! txn ref)
  (when (> (caar (jolt-ref-tvals ref)) (jolt-txn-read-point txn))
    (stm-retry!)))

;; ref-set / alter: a commuted ref cannot be set; the first set checks for a
;; conflict, which the commit checks again.
(define (txn-set! txn ref v)
  (let ((cell (hashtable-ref (jolt-txn-vals txn) ref #f)))
    (cond
      ((and cell (cdr cell)) (set-car! cell v))
      ((assq ref (jolt-txn-commutes txn))
       (jolt-throw (jolt-host-throwable "java.lang.IllegalStateException" "Can't set after commute")))
      (else
       (txn-check-unchanged! txn ref)
       (hashtable-set! (jolt-txn-vals txn) ref (cons v #t)))))
  v)

;; --- commit ---------------------------------------------------------------------

;; The history a commit leaves: one version more when the ref faulted and is
;; under its max, or is under its min; otherwise the oldest version goes.
(define (ref-next-tvals ref point v)
  (let ((tv (jolt-ref-tvals ref)))
    (if (and (null? (cdr tv)) (eqv? 0 (jolt-ref-faults ref)) (eqv? 0 (jolt-ref-min-hist ref)))
        (list (cons point v))
        (let* ((hcount (- (length tv) 1))
               (grow? (or (and (> (jolt-ref-faults ref) 0) (< hcount (jolt-ref-max-hist ref)))
                          (< hcount (jolt-ref-min-hist ref)))))
          (when grow? (jolt-ref-faults-set! ref 0))
          (cons (cons point v)
                (if grow? tv (let drop-last ((l tv)) (if (null? (cdr l)) '() (cons (car l) (drop-last (cdr l)))))))))))

;; Run under stm-lock. Returns the (ref old new) notifications, or throws the
;; retry when something this transaction wrote or ensured changed underneath it.
(define (txn-commit! txn)
  (let-values (((refs cells) (hashtable-entries (jolt-txn-vals txn))))
    (let ((n (vector-length refs)))
      (do ((i 0 (fx+ i 1))) ((fx= i n))
        (when (cdr (vector-ref cells i)) (txn-check-unchanged! txn (vector-ref refs i))))
      (for-each (lambda (ref) (txn-check-unchanged! txn ref)) (jolt-txn-ensures txn))
      ;; a commuted ref not also set takes its commutes over the newest value
      (for-each
        (lambda (rc)
          (let ((cell (hashtable-ref (jolt-txn-vals txn) (car rc) #f)))
            (unless (cdr cell)
              (set-car! cell
                (let loop ((v (jolt-ref-val (car rc))) (cfs (reverse (cdr rc))))
                  (if (null? cfs)
                      v
                      (loop (apply jolt-invoke (caar cfs) v (cdar cfs)) (cdr cfs))))))))
        (jolt-txn-commutes txn))
      (do ((i 0 (fx+ i 1))) ((fx= i n))
        (iref-validate (vector-ref refs i) (car (vector-ref cells i))))
      ;; the (ref old new) notifications, when anything watches, and the history
      ;; each ref is left with
      (let ((point (+ stm-clock 1))
            (notes (if (iref-any?)
                       (let loop ((i (fx- n 1)) (acc '()))
                         (if (fx< i 0)
                             acc
                             (loop (fx- i 1)
                                   (let ((ref (vector-ref refs i)))
                                     (cons (list ref (jolt-ref-val ref) (car (vector-ref cells i))) acc)))))
                       '()))
            (ntvs (make-vector n)))
        (do ((i 0 (fx+ i 1))) ((fx= i n))
          (vector-set! ntvs i (ref-next-tvals (vector-ref refs i) point (car (vector-ref cells i)))))
        (when jolt-mt? (memory-order-release))
        (do ((i 0 (fx+ i 1))) ((fx= i n))
          (jolt-ref-tvals-set! (vector-ref refs i) (vector-ref ntvs i)))
        (when jolt-mt? (memory-order-release))
        (set! stm-clock point)
        notes))))

(define (txn-writes? txn)
  (or (fx> (hashtable-size (jolt-txn-vals txn)) 0)
      (pair? (jolt-txn-ensures txn))))

;; --- constructor -------------------------------------------------------------
;; (ref init :validator f :meta m :min-history n :max-history n) — the ARef ctor
;; contract: validator runs against the initial value; :meta must be a map.
(define (jolt-ref-new v . opts)
  (let loop ((o opts) (validator jolt-nil) (m #f) (minh 0) (maxh 10))
    (cond
      ((or (null? o) (null? (cdr o)))
       (let ((r (make-jolt-ref v)))
         (jolt-ref-min-hist-set! r minh)
         (jolt-ref-max-hist-set! r maxh)
         ;; validate init via iref validator table
         (when (and (not (jolt-nil? validator)) (jolt-not (jolt-invoke validator v)))
           (jolt-iref-state-throw))
         (unless (jolt-nil? validator)
           (jolt-with-mutex iref-tbl-mu
             (iref-tbl-bump!)
             (hashtable-set! iref-validator-tbl r validator)))
         (when (and m (not (jolt-nil? m)))
           (unless (jolt-map? m)
             (jolt-throw (jolt-host-throwable
                          "java.lang.ClassCastException"
                          (string-append "class " (jolt-class-name m)
                                         " cannot be cast to class clojure.lang.IPersistentMap"))))
           (meta-table-set! r m))
         r))
      ((keyword-t? (car o))
       (let ((k (keyword-t-name (car o))))
         (cond ((string=? k "validator") (loop (cddr o) (cadr o) m minh maxh))
               ((string=? k "meta") (loop (cddr o) validator (cadr o) minh maxh))
               ((string=? k "min-history") (loop (cddr o) validator m (cadr o) maxh))
               ((string=? k "max-history") (loop (cddr o) validator m minh (cadr o)))
               (else (loop (cddr o) validator m minh maxh)))))
      (else (loop (cddr o) validator m minh maxh)))))

;; --- transaction-guarded operations ------------------------------------------
;; Inside a transaction, ref-set/alter/commute/ensure write through the
;; per-txn table; the commit validates and writes them. On exception the table
;; is discarded — rollback is implicit.

(define (jolt-ref-ensure-txn)
  (or (*txn*)
      (jolt-throw (jolt-host-throwable
                   "java.lang.IllegalStateException"
                   "No transaction running"))))

(define (jolt-ref-set ref v)
  (txn-set! (jolt-ref-ensure-txn) ref v))

(define (jolt-alter ref f . args)
  (let* ((txn (jolt-ref-ensure-txn))
         (v (apply jolt-invoke f (txn-get txn ref) args)))
    (txn-set! txn ref v)))

;; commute never conflicts: the in-txn value starts from the newest committed
;; one, and the commit applies the fns again to whatever is newest then.
(define (jolt-commute ref f . args)
  (let* ((txn (jolt-ref-ensure-txn))
         (vals (jolt-txn-vals txn))
         (cell (or (hashtable-ref vals ref #f)
                   (let ((c (cons (jolt-ref-val ref) #f)))
                     (hashtable-set! vals ref c)
                     c)))
         (v (apply jolt-invoke f (car cell) args)))
    (let* ((cs (jolt-txn-commutes txn)) (rc (assq ref cs)))
      (if rc
          (set-cdr! rc (cons (cons f args) (cdr rc)))
          (jolt-txn-commutes-set! txn (cons (list ref (cons f args)) cs))))
    (set-car! cell v)
    v))

;; ensure: the commit fails if the ref changed after the read point, so a
;; transaction that read it cannot commit on a stale value of it.
(define (jolt-ensure ref)
  (let ((txn (jolt-ref-ensure-txn)))
    (let ((v (txn-get txn ref)))
      (unless (memq ref (jolt-txn-ensures txn))
        (jolt-txn-ensures-set! txn (cons ref (jolt-txn-ensures txn))))
      v)))

;; __sync-call: run a thunk inside a transaction — the seam the sync/dosync
;; MACROS (30-macros.clj) expand through; sync itself is a macro with the
;; reference's (sync flags & body) shape. Nested calls join the outer
;; transaction (re-entrant through the thread-local parameter).
;;
;; The body is a thunk because dosync reaches here as (__sync-call (fn* [] body)) —
;; an fn*, which is opaque to the CPS pass, so a park inside a transaction takes a
;; CAPTURE park and rewinds properly through dyn-with-txn.
(define (jolt-sync thunk)
  (if (*txn*)
      ;; nested — just run the body under the existing transaction
      (jolt-invoke thunk)
      (let attempt ((n 0))
        (when (fx= n stm-retry-limit)
          (jolt-throw (jolt-host-throwable "java.lang.RuntimeException"
                                           "Transaction failed after reaching retry limit")))
        (let ((rp stm-clock))
          (when jolt-mt? (memory-order-acquire))
          ;; outcome: (result . notifications), #f to retry, or #(raised)
          (let* ((txn (make-txn rp))
                 (outcome
                  (guard (e (#t (if (stm-retry? e) #f (vector e))))
                    (let ((result (dyn-with-txn txn (lambda () (jolt-invoke thunk)))))
                      (cons result
                            (if (txn-writes? txn)
                                (jolt-with-monitor stm-lock (lambda () (txn-commit! txn)))
                                '()))))))
            (cond
              ((not outcome) (attempt (fx+ n 1)))
              ((vector? outcome) (raise (vector-ref outcome 0)))
              (else
               ;; the commit lock is released and *txn* is back to #f
               (for-each (lambda (nt) (apply iref-notify nt)) (cdr outcome))
               (for-each (lambda (send) (send)) (reverse (jolt-txn-pending-sends txn)))
               (car outcome))))))))

;; io! is a MACRO (30-macros.clj): its body must NOT evaluate when the
;; transaction check throws. The macro tests this seam.
(define (jolt-txn-running?) (and (*txn*) #t))

;; --- history ops -------------------------------------------------------------
;; ref-history-count is how many versions besides the newest a ref keeps. A ref
;; keeps min-history of them at least, and grows toward max-history when a read
;; finds no version old enough (a fault).
;;
;; case-lambda rather than a rest-arg: a rest-arg accepted ANY arg count and
;; silently took the getter branch for 3+ (so (ref-min-history r 1 2) read the
;; value instead of raising), and (ref-min-history) died in `car` with a raw
;; host error that carried no class, so no catch clause could select it.
;; nil and a wrong type are different answers on the JVM, because the ^Ref hint
;; makes these an interface call: a null target is a NullPointerException, a
;; non-Ref is the cast failure. jolt-class-name itself raises on nil, so the nil
;; arm has to come first regardless.
(define (ref-history-arg-check ref)
  (cond
    ((jolt-ref? ref) #t)
    ((jolt-nil? ref)
     (throw-jvm (quote NullPointerException)
                "Cannot invoke \"clojure.lang.Ref\" method because \"ref\" is null"))
    (else
     (jolt-throw (jolt-host-throwable
                  "java.lang.ClassCastException"
                  (string-append "class " (jolt-class-name ref)
                                 " cannot be cast to class clojure.lang.Ref"))))))
(define (jolt-ref-history-count ref)
  (ref-history-arg-check ref)
  (- (length (jolt-ref-tvals ref)) 1))

(define (make-ref-history-op get set)
  (case-lambda
    ((ref)
     (ref-history-arg-check ref)
     (get ref))
    ((ref n)
     (ref-history-arg-check ref)
     (set ref n)
     ref)))
(define jolt-ref-min-history (make-ref-history-op jolt-ref-min-hist jolt-ref-min-hist-set!))
(define jolt-ref-max-history (make-ref-history-op jolt-ref-max-hist jolt-ref-max-hist-set!))

;; --- deref -------------------------------------------------------------------
;; Inside a transaction, the in-txn value or the version the read point sees.
;; Outside a transaction, the newest committed value.
(define (jolt-ref-deref ref)
  (txn-read ref))

;; Chain jolt-deref to handle refs (capture the pre-ref jolt-deref from atoms).
(define %pre-ref-deref jolt-deref)
(set! jolt-deref
  (lambda (x . opts)
    (if (jolt-ref? x)
        (jolt-ref-deref x)
        (apply %pre-ref-deref x opts))))

;; --- bind into clojure.core -------------------------------------------------
;; sync/dosync/io! are macros in the overlay (30-macros.clj) over the
;; __sync-call / __txn-running? seams.
(def-var! "clojure.core" "ref" jolt-ref-new)
(def-var! "clojure.core" "ref?" jolt-ref?)
(def-var! "clojure.core" "ref-set" jolt-ref-set)
(def-var! "clojure.core" "alter" jolt-alter)
(def-var! "clojure.core" "commute" jolt-commute)
(def-var! "clojure.core" "ensure" jolt-ensure)
(def-var! "clojure.core" "__sync-call" jolt-sync)
(def-var! "clojure.core" "__txn-running?" jolt-txn-running?)
(def-var! "clojure.core" "ref-history-count" jolt-ref-history-count)
;; ref-min-history / ref-max-history are multi-arity (getter / setter)
(def-var! "clojure.core" "ref-min-history" jolt-ref-min-history)
(def-var! "clojure.core" "ref-max-history" jolt-ref-max-history)
;; deref is already bound; the chain above extends jolt-deref, and
;; concurrency.ss will re-chain over us.

;; *loaded-libs* — seeded empty; loader.ss populates and wires it.
(def-dynvar! "clojure.core" "*loaded-libs*" (jolt-ref-new (jolt-hash-set)))
;; loaded-libs fn returns the derefed set.
(def-var! "clojure.core" "loaded-libs"
  (lambda () (jolt-deref (var-deref "clojure.core" "*loaded-libs*"))))
