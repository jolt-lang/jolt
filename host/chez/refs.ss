;; refs.ss — Clojure refs and STM for the Chez host.
;;
;; Single global transaction mutex gives correct serializable semantics on
;; jolt's shared-heap threads — no MVCC needed.  alter/ref-set/commute/ensure
;; throw IllegalStateException outside a dosync, matching the JVM.
;;
;; Refs participate in the IRef seam (watches/validators/metadata) like
;; atom/var/agent.  Loaded after atoms.ss (shares jolt-iref-state-throw and
;; the iref tables).

(define-record-type jolt-ref
  (fields (mutable val) lock)
  (nongenerative jolt-ref-v1))

;; IRef arm: refs are watchable/validatable through the iref tables.
(register-iref-arm! jolt-ref?)

;; --- transaction state -------------------------------------------------------
;; A single global mutex serializes all transactions.  Per-thread *txn* detects
;; nested dosync (joins the outer transaction) and guards against io!/ref-set/
;; alter/commute/ensure outside a transaction.
(define stm-lock (make-mutex))
(define *txn* (make-thread-parameter #f))

;; --- constructor -------------------------------------------------------------
;; (ref init :validator f :meta m) — the ARef ctor contract: validator runs
;; against the initial value; :meta must be a map.
(define (jolt-ref-new v . opts)
  (let loop ((o opts) (validator jolt-nil) (m #f))
    (cond
      ((or (null? o) (null? (cdr o)))
       (let ((r (make-jolt-ref v (make-mutex))))
         ;; validate init via iref validator table (register with a predicate
         ;; running before the ref exists — set-validator! on the new ref)
         (when (and (not (jolt-nil? validator)) (jolt-not (jolt-invoke validator v)))
           (jolt-iref-state-throw))
         (unless (jolt-nil? validator)
           (hashtable-set! iref-validator-tbl r validator))
         ;; :meta uses the global meta-table (natives-meta.ss), same as atoms
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
;; Inside a transaction (under the global mutex), ref-set/alter/commute/ensure
;; mutate the ref's value directly — no CAS, no retry: serialized transactions
;; guarantee no concurrent writer.
(define (jolt-ref-ensure-txn)
  (unless (*txn*)
    (jolt-throw (jolt-host-throwable
                 "java.lang.IllegalStateException"
                 "No transaction running"))))

(define (jolt-ref-set ref v)
  (jolt-ref-ensure-txn)
  (iref-validate ref v)
  (let ((old (jolt-ref-val ref)))
    (jolt-ref-val-set! ref v)
    (iref-notify ref old v))
  v)

(define (jolt-alter ref f . args)
  (jolt-ref-ensure-txn)
  (let* ((old (jolt-ref-val ref))
         (v (apply jolt-invoke f old args)))
    (iref-validate ref v)
    (jolt-ref-val-set! ref v)
    (iref-notify ref old v)
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
  (jolt-ref-val ref))

;; sync: run a thunk inside a serialized transaction.  Nested calls join the
;; outer transaction (re-entrant through the thread-local flag).
(define (jolt-sync thunk)
  (if (*txn*)
      ;; nested — just run the body under the existing transaction
      (jolt-invoke thunk)
      (with-mutex stm-lock
        (parameterize ((*txn* #t))
          (jolt-invoke thunk)))))

;; io!: throws if called inside a transaction; runs the thunk otherwise
;; (matching the JVM's clojure.lang.LockingTransaction/io!).
(define (jolt-io! . body)
  (when (*txn*)
    (jolt-throw (jolt-host-throwable
                 "java.lang.IllegalStateException"
                 "I/O in transaction")))
  (if (null? body) jolt-nil
      (let loop ((body body))
        (if (null? (cdr body)) (car body)
            (begin (car body) (loop (cdr body)))))))

;; --- history ops (stubs — no MVCC) ------------------------------------------
;; On the JVM these control how many prior values a ref keeps for snapshot
;; isolation.  Our serialized transactions need no history, so we return
;; constants; set! is accepted (compatibility with code that configures them).
;; ref-history-count is the ONLY one that MUST run outside a transaction (the
;; JVM's LockingTransaction/Ref returns the configured count unconditionally).
(define (jolt-ref-history-count ref)
  0)

(define (jolt-ref-min-history ref)
  0)

(define (jolt-ref-set-min-history! ref n)
  jolt-nil)

(define (jolt-ref-max-history ref)
  10)

(define (jolt-ref-set-max-history! ref n)
  jolt-nil)

;; --- deref -------------------------------------------------------------------
(define (jolt-ref-deref ref)
  (jolt-ref-val ref))

;; Chain jolt-deref to handle refs (capture the pre-ref jolt-deref from atoms).
(define %pre-ref-deref jolt-deref)
(set! jolt-deref
  (lambda (x . opts)
    (if (jolt-ref? x)
        (jolt-ref-deref x)
        (apply %pre-ref-deref x opts))))

;; --- dosync macro (host-level, for corpus/standalone use) --------------------
;; The overlay (30-macros.clj) re-defines this.  This one lets corpus/seed tests
;; use dosync before the overlay loads.  Syntax: (dosync & body) -> (sync (fn* [] ~@body))
(guard (e (#t #f))
  (def-var! "clojure.core" "dosync"
    (lambda body
      (let* ((sqcat (var-deref "clojure.core" "__sqcat"))
             (sq1   (var-deref "clojure.core" "__sq1"))
             ;; body is a raw Scheme rest list; convert to cseq for sq-flatten
             (body  (list->cseq body)))
        (jolt-invoke2 sqcat
          (jolt-invoke1 sq1 (jolt-symbol "clojure.core" "sync"))
          (jolt-invoke1 sq1
            (jolt-invoke3 sqcat
              (jolt-invoke1 sq1 (jolt-symbol #f "fn*"))
              (jolt-invoke1 sq1 (jolt-invoke0 (var-deref "clojure.core" "__sqvec")))
              body))))))
  (mark-macro! "clojure.core" "dosync"))

;; --- bind into clojure.core -------------------------------------------------
(def-var! "clojure.core" "ref" jolt-ref-new)
(def-var! "clojure.core" "ref?" jolt-ref?)
(def-var! "clojure.core" "ref-set" jolt-ref-set)
(def-var! "clojure.core" "alter" jolt-alter)
(def-var! "clojure.core" "commute" jolt-commute)
(def-var! "clojure.core" "ensure" jolt-ensure)
(def-var! "clojure.core" "sync" jolt-sync)
(def-var! "clojure.core" "io!" jolt-io!)
(def-var! "clojure.core" "ref-history-count" jolt-ref-history-count)
(def-var! "clojure.core" "ref-min-history" jolt-ref-min-history)
(def-var! "clojure.core" "ref-max-history" jolt-ref-max-history)
;; deref is already bound; the chain above extends jolt-deref, and
;; concurrency.ss will re-chain over us.

;; *loaded-libs* — seeded empty; loader.ss populates and wires it.
(def-var! "clojure.core" "*loaded-libs*" (jolt-ref-new (jolt-hash-set)))
;; loaded-libs fn returns the derefed set.
(def-var! "clojure.core" "loaded-libs"
  (lambda () (jolt-deref (var-deref "clojure.core" "*loaded-libs*"))))
