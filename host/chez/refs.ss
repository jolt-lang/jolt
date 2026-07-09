;; refs.ss — Clojure refs and STM for the Chez host.
;;
;; Single global transaction mutex gives correct serializable semantics on
;; jolt's shared-heap threads — no MVCC needed.  Transactions buffer writes
;; in a per-txn log and only commit (write ref values) on success, providing
;; rollback on exception.  Watches fire once per changed ref after commit,
;; outside the transaction lock, matching JVM semantics.
;;
;; Refs participate in the IRef seam (watches/validators/metadata) like
;; atom/var/agent.  Loaded after atoms.ss (shares jolt-iref-state-throw and
;; the iref tables).

(define-record-type jolt-ref
  (fields (mutable val) lock)
  (nongenerative jolt-ref-v1))

;; IRef arm: refs are watchable/validatable through the iref tables.
(register-iref-arm! jolt-ref?)

;; Per-ref min/max history (defaults 0 and 10), stored in weak side tables.
(define ref-min-history-tbl (make-weak-eq-hashtable))
(define ref-max-history-tbl (make-weak-eq-hashtable))

;; --- transaction record -------------------------------------------------------
;; A per-transaction record, held in the thread-parameter *txn* (#f when
;; no transaction is running).
;;   log         — eq-hashtable: ref -> in-txn value
;;   old-vals    — eq-hashtable: ref -> committed value at first mutation
;;                 (captured once per ref for the single watch notification)
;;   pending-sends — list of (agent f args) enqueued during this txn (Round 3)
(define-record-type jolt-txn
  (fields (mutable log) (mutable old-vals) (mutable pending-sends))
  (nongenerative jolt-txn-v1))

(define (make-txn)
  (make-jolt-txn (make-eq-hashtable) (make-eq-hashtable) '()))

;; --- transaction state -------------------------------------------------------
;; A single global mutex serializes all transactions.  Per-thread *txn* detects
;; nested dosync (joins the outer transaction) and guards against io!/ref-set/
;; alter/commute/ensure outside a transaction.
(define stm-lock (make-mutex))
(define *txn* (make-thread-parameter #f))

;; --- in-txn log helpers ------------------------------------------------------

;; Sentinel for hashtable-ref to distinguish "not found" from a valid value.
(define txn-not-found (list 'txn-not-found))

;; Read a ref's in-txn value from the transaction log, falling back to the
;; ref's committed value if this txn has not touched it.
(define (txn-read ref)
  (let ((txn (*txn*)))
    (if txn
        (let ((v (hashtable-ref (jolt-txn-log txn) ref txn-not-found)))
          (if (eq? v txn-not-found) (jolt-ref-val ref) v))
        (jolt-ref-val ref))))

;; Write a ref's in-txn value to the log.  Captures the pre-txn committed
;; value on first mutation if not already recorded.
(define (txn-write! ref v)
  (let* ((log (jolt-txn-log (*txn*)))
         (ov (jolt-txn-old-vals (*txn*))))
    ;; capture pre-txn value on first write to this ref
    (unless (hashtable-contains? ov ref)
      (hashtable-set! ov ref (jolt-ref-val ref)))
    (hashtable-set! log ref v)))

;; Commit: write all buffered values to the refs.  Must be called while
;; still holding stm-lock.
(define (txn-commit! txn)
  (let ((log (jolt-txn-log txn)))
    (vector-for-each
      (lambda (ref) (jolt-ref-val-set! ref (hashtable-ref log ref #f)))
      (hashtable-keys log))))

;; Fire watch notifications for committed changes.  Called AFTER releasing
;; stm-lock and clearing *txn*, so watches can open their own dosync.
(define (txn-fire-watches! txn)
  (let ((log (jolt-txn-log txn))
        (ov (jolt-txn-old-vals txn)))
    (vector-for-each
      (lambda (ref)
        (let ((new-val (hashtable-ref log ref #f))
              (old (hashtable-ref ov ref txn-not-found)))
          (unless (eq? old txn-not-found)
            (iref-notify ref old new-val))))
      (hashtable-keys log))))

;; --- constructor -------------------------------------------------------------
;; (ref init :validator f :meta m) — the ARef ctor contract: validator runs
;; against the initial value; :meta must be a map.
(define (jolt-ref-new v . opts)
  (let loop ((o opts) (validator jolt-nil) (m #f))
    (cond
      ((or (null? o) (null? (cdr o)))
       (let ((r (make-jolt-ref v (make-mutex))))
         ;; validate init via iref validator table
         (when (and (not (jolt-nil? validator)) (jolt-not (jolt-invoke validator v)))
           (jolt-iref-state-throw))
         (unless (jolt-nil? validator)
           (hashtable-set! iref-validator-tbl r validator))
         (when (and m (not (jolt-nil? m)))
           (unless (jolt-map? m)
             (jolt-throw (jolt-host-throwable
                          "java.lang.ClassCastException"
                          (string-append "class " (jolt-class-name m)
                                         " cannot be cast to class clojure.lang.IPersistentMap"))))
           (hashtable-set! meta-table r m))
         r))
      ((and (keyword-t? (car o)) (string=? (keyword-t-name (car o)) "validator"))
       (loop (cddr o) (cadr o) m))
      ((and (keyword-t? (car o)) (string=? (keyword-t-name (car o)) "meta"))
       (loop (cddr o) validator (cadr o)))
      (else (loop (cddr o) validator m)))))

;; --- transaction-guarded operations ------------------------------------------
;; Inside a transaction, ref-set/alter/commute/ensure write through the
;; per-txn log; on commit the buffered values are written to the refs.
;; On exception the log is discarded — rollback is implicit.

(define (jolt-ref-ensure-txn)
  (unless (*txn*)
    (jolt-throw (jolt-host-throwable
                 "java.lang.IllegalStateException"
                 "No transaction running"))))

(define (jolt-ref-set ref v)
  (jolt-ref-ensure-txn)
  (iref-validate ref v)
  (txn-write! ref v)
  v)

(define (jolt-alter ref f . args)
  (jolt-ref-ensure-txn)
  (let* ((old (txn-read ref))
         (v (apply jolt-invoke f old args)))
    (iref-validate ref v)
    (txn-write! ref v)
    v))

;; Under serialized transactions, commute is equivalent to alter (no
;; commutative optimization needed).
(define (jolt-commute ref f . args)
  (apply jolt-alter ref f args))

;; ensure: under serialized transactions this is a no-op beyond the
;; transaction-enforcement guard — there is no ref to "touch" because no
;; other thread can mutate it while we hold the global lock.
(define (jolt-ensure ref)
  (jolt-ref-ensure-txn)
  (txn-read ref))

;; __sync-call: run a thunk inside a serialized transaction — the seam the
;; sync/dosync MACROS (30-macros.clj) expand through; sync itself is a macro
;; with the reference's (sync flags & body) shape.  Nested calls join the
;; outer transaction (re-entrant through the thread-local parameter).
(define (jolt-sync thunk)
  (if (*txn*)
      ;; nested — just run the body under the existing transaction
      (jolt-invoke thunk)
      ;; outer transaction: acquire lock, buffer writes, commit or rollback
      (let ((txn (make-txn))
            (aborted #f)
            (result #f))
        (with-mutex stm-lock
          (parameterize ((*txn* txn))
            (guard (e (#t (set! aborted #t) (set! result e)))
              (set! result (jolt-invoke thunk)))
            (unless aborted
              (txn-commit! txn))))
        ;; after with-mutex releases lock and parameterize restores *txn* to #f
        (unless aborted
          (txn-fire-watches! txn)
          ;; dispatch deferred agent sends inside a txn.  Look up send from
          ;; clojure.core (resolved at runtime, after concurrency.ss loads).
          (let ((sends (jolt-txn-pending-sends txn)))
            (when (pair? sends)
              (let ((send-fn (or jolt-txn-send-fn
                                 (let ((v (var-deref "clojure.core" "send")))
                                   (set! jolt-txn-send-fn v) v))))
                (for-each (lambda (entry)
                            (apply send-fn entry))
                          (reverse sends))))))
        (if aborted (raise result) result))))

;; io! is a MACRO (30-macros.clj): its body must NOT evaluate when the
;; transaction check throws. The macro tests this seam.
(define (jolt-txn-running?) (and (*txn*) #t))

;; --- history ops -------------------------------------------------------------
;; On the JVM these control how many prior values a ref keeps for snapshot
;; isolation.  Our serialized transactions need no history, so ref-history-count
;; returns 0; the 2-arity setter forms store min/max on the ref's side table
;; and return the ref.  ref-history-count is the ONLY one that MUST run
;; outside a transaction (the JVM's LockingTransaction/Ref returns the
;; configured count unconditionally).
(define (jolt-ref-history-count ref)
  0)

(define (jolt-ref-min-history . args)
  (let ((ref (car args)))
    (if (= (length args) 2)
        (begin (hashtable-set! ref-min-history-tbl ref (cadr args)) ref)
        (hashtable-ref ref-min-history-tbl ref 0))))

(define (jolt-ref-max-history . args)
  (let ((ref (car args)))
    (if (= (length args) 2)
        (begin (hashtable-set! ref-max-history-tbl ref (cadr args)) ref)
        (hashtable-ref ref-max-history-tbl ref 10))))

;; --- deref -------------------------------------------------------------------
;; Inside a transaction, return the in-txn value from the log (falling back
;; to the ref's committed value).  Outside a transaction, return the ref's
;; committed value directly.
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
(def-var! "clojure.core" "*loaded-libs*" (jolt-ref-new (jolt-hash-set)))
;; loaded-libs fn returns the derefed set.
(def-var! "clojure.core" "loaded-libs"
  (lambda () (jolt-deref (var-deref "clojure.core" "*loaded-libs*"))))

;; Cached send fn for dispatching deferred agent sends after txn commit.
;; Resolved lazily at runtime (after concurrency.ss has loaded send into core).
(define jolt-txn-send-fn #f)
