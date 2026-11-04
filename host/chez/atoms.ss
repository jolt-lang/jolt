;; atoms — host-coupled mutable reference cells for the Chez host.
;;
;; atom/deref/swap!/reset! are host primitives (not the clojure.core overlay),
;; so the runtime provides native shims, def-var!'d into clojure.core. They
;; lower to var-deref in prelude mode. The hierarchy machinery
;; (global-hierarchy = (atom (make-hierarchy))) calls `atom` at the prelude's
;; LOAD time, so without this shim the whole prelude fails to load.
;;
;; compare-and-set!/swap-vals!/reset-vals! are overlay fns over the native kernel
;; in the live system; provided here natively too so the host is self-sufficient
;; for atoms without the full prelude (the overlay versions, when the full prelude
;; loads, override these but compose the same native kernel).

;; watches is an alist of (key . watch-fn); validator is a jolt fn or jolt-nil.
;; The peripheral ops + the notify/validate behaviour live natively here, and
;; post-prelude.ss re-asserts them over the overlay's def-var!.
;; Every state transition is a compare-and-swap on the val field
;; (sa-record-cas!), the JVM's AtomicReference shape: swap! computes outside
;; any lock and commits by CAS, retrying when another thread moved the value,
;; and reset!/compare-and-set!/the -vals! pair are the same CAS. The watches
;; field moves by CAS too. Nothing here takes a mutex: the per-atom mutex that
;; used to guard these was a malloc'd pthread mutex and a guardian per atom
;; (101 ns to make one, 64 ns to reset! through it against 7 to deref) and,
;; on a program that wraps every node of a tree in an atom, the collector's
;; largest population of foreign objects. `lock` stays in the layout — the
;; record is image surface (jolt-atom-v3) — and holds #f; an atom restored from
;; an older image carries its dead mutex there, which nothing reads.
;;
;; The release fence before a CAS orders the new value's contents ahead of the
;; pointer that publishes it, so a reader that sees the pointer sees the
;; object; a reader's own loads are dependent and need nothing (the lock
;; version never fenced a deref either).
(define-record-type jolt-atom
  (fields (mutable val) (mutable watches) (mutable validator) lock)
  (nongenerative jolt-atom-v3))
(define jolt-atom-val-slot 0)
(define jolt-atom-watches-slot 1)

;; a rejected reference value is IllegalStateException, like ARef.validate.
(define (jolt-iref-state-throw)
  (jolt-throw (jolt-host-throwable "java.lang.IllegalStateException" "Invalid reference state")))

;; (atom init :meta m :validator f) — the ARef ctor contract: the validator runs
;; against the initial value (an invalid init never constructs), :meta must be a
;; map (anything else is the JVM's IPersistentMap cast failure).
(define (jolt-atom-new v . opts)
  (let loop ((o opts) (validator jolt-nil) (m #f))
    (cond
      ((or (null? o) (null? (cdr o)))
       (let ((a (make-jolt-atom v '() validator #f)))
         (jolt-atom-validate a v)
         (when (and m (not (jolt-nil? m)))
           (unless (jolt-map? m)
             (jolt-throw (jolt-host-throwable
                          "java.lang.ClassCastException"
                          (string-append "class " (jolt-class-name m)
                                         " cannot be cast to class clojure.lang.IPersistentMap"))))
           (meta-table-set! a m))
         a))
      ((and (keyword-t? (car o)) (string=? (keyword-t-name (car o)) "validator"))
       (loop (cddr o) (cadr o) m))
      ((and (keyword-t? (car o)) (string=? (keyword-t-name (car o)) "meta"))
       (loop (cddr o) validator (cadr o)))
      (else (loop (cddr o) validator m)))))

;; The five mutators reach into the atom record directly, so a non-atom receiver
;; would raise a raw host error with no class for a catch to select — one naming
;; jolt's own record type ("is not of type #<record type jolt-atom-v3>"), which
;; classes as :object and therefore matches EVERY catch clause. Name the class the
;; JVM names instead (nil is its NullPointerException). Every entry point that can
;; take a receiver from user code opens with this, BEFORE it validates or reads a
;; field: compare-and-set! validates the new value first, so the check has to come
;; ahead of that or the raw error is back.
(define (jolt-need-atom a)
  (if (jolt-atom? a) a (jolt-cast-throw a "clojure.lang.IAtom")))

;; validate a candidate value: a non-nil validator that returns falsey rejects.
(define (jolt-atom-validate a v)
  (let ((vf (jolt-atom-validator a)))
    (when (and (not (jolt-nil? vf)) (jolt-not (jolt-invoke1 vf v)))
      (jolt-iref-state-throw))))

;; notify each watch (k ref old new), in insertion order (alist is reverse-built,
;; so walk it reversed to match add order).
(define (jolt-atom-notify a old new)
  (let ((ws (jolt-atom-watches a)))
    ;; nearly every atom has none, and this runs on every swap!/reset!
    (unless (null? ws)
      (for-each (lambda (kv) (jolt-invoke4 (cdr kv) (car kv) a old new))
                (reverse ws)))))

;; deref reads an atom; it also unwraps a `reduced` (Clojure @(reduced x) => x,
;; which the overlay's `unreduced` relies on). The reduced record is in seq.ss.
(define (jolt-deref x)
  (cond
    ((jolt-atom? x) (jolt-atom-val x))
    ((jolt-reduced? x) (jolt-reduced-val x))
    ;; the reference's last resort is a Future cast: nil is a NullPointerException
    (else (jolt-cast-throw x "java.util.concurrent.Future"))))

;; CAS the val from `old` to `nv` by identity (eq?), atomically. Returns #t on
;; success. The compute step (f) runs outside this; a loser recomputes.
(define (jolt-atom-cas! a old nv)
  (when jolt-mt? (memory-order-release))
  (sa-record-cas! a jolt-atom-val-slot old nv))
;; Unconditional exchange (AtomicReference.getAndSet): the old value.
(define (jolt-atom-exchange! a v)
  (let retry ()
    (let ((o (jolt-atom-val a)))
      (if (jolt-atom-cas! a o v) o (retry)))))

;; (swap! a f arg*): JVM-style CAS loop — read, compute f OUTSIDE the lock, then
;; atomically compare-and-set; retry if another thread changed it. Validate the
;; new value before storing, notify watches after.
;; Validate, CAS and notify — the half of swap! that does not depend on how the
;; new value was computed. #f means the CAS lost and the caller must retry; the
;; caller returns the new value itself, so a #f new value is not ambiguous here.
(define (jolt-swap-commit! a old nv)
  (jolt-atom-validate a nv)
  (and (jolt-atom-cas! a old nv)
       (begin (jolt-atom-notify a old nv) #t)))

;; `(swap! a f)`, `(swap! a f x)` and `(swap! a f x y)` are almost every swap! a
;; program writes — (swap! n inc), (swap! m assoc k v), (swap! v conj x) — and a
;; single `. args` signature charged each of them a rest-arg list plus an `apply`
;; to call f. The fixed clauses allocate nothing and call f directly. The retry
;; loop is spelled out per clause rather than shared through a thunk: a thunk
;; would trade the rest-arg list for a closure and give the fixed arities back
;; what they came for. The variadic clause is the original body.
(define jolt-swap!
  (case-lambda
    ((a f)
     (jolt-need-atom a)
     (let retry ()
       (let* ((old (jolt-atom-val a)) (nv (jolt-invoke1 f old)))
         (if (jolt-swap-commit! a old nv) nv (retry)))))
    ((a f x)
     (jolt-need-atom a)
     (let retry ()
       (let* ((old (jolt-atom-val a)) (nv (jolt-invoke2 f old x)))
         (if (jolt-swap-commit! a old nv) nv (retry)))))
    ((a f x y)
     (jolt-need-atom a)
     (let retry ()
       (let* ((old (jolt-atom-val a)) (nv (jolt-invoke3 f old x y)))
         (if (jolt-swap-commit! a old nv) nv (retry)))))
    ((a f . args)
     (jolt-need-atom a)
     (let retry ()
       (let* ((old (jolt-atom-val a)) (nv (apply jolt-invoke f old args)))
         (if (jolt-swap-commit! a old nv) nv (retry)))))))

(define (jolt-reset! a v)
  (jolt-need-atom a)
  (jolt-atom-validate a v)
  (let ((old (jolt-atom-exchange! a v)))
    (jolt-atom-notify a old v)
    v))

;; compare-and-set! is the CAS helper with the JVM's identity comparison.
(define (jolt-compare-and-set! a oldv newv)
  (jolt-need-atom a)
  (jolt-atom-validate a newv)
  (let ((swapped (jolt-atom-cas! a oldv newv)))
    (when swapped (jolt-atom-notify a oldv newv))
    swapped))

(define (jolt-swap-vals! a f . args)
  (jolt-need-atom a)
  (let retry ()
    (let* ((old (jolt-atom-val a))
           (nv (apply jolt-invoke f old args)))
      (jolt-atom-validate a nv)
      (if (jolt-atom-cas! a old nv)
          (begin (jolt-atom-notify a old nv) (jolt-vector old nv))
          (retry)))))

(define (jolt-reset-vals! a v)
  (jolt-need-atom a)
  (jolt-atom-validate a v)
  (let ((old (jolt-atom-exchange! a v)))
    (jolt-atom-notify a old v)
    (jolt-vector old v)))

;; --- watches / validators: the IRef seam --------------------------------------
;; On the JVM these are the ARef contract shared by atom/var/agent/ref. The atom
;; keeps its record slots (the hot swap!/reset! path); every OTHER watchable
;; reference type registers a predicate here and stores its watches/validator in
;; identity-keyed side tables. A ref type makes itself notify by calling
;; iref-notify at its mutation points (vars do at root set).
(define iref-arms '())
(define (register-iref-arm! pred) (set! iref-arms (cons pred iref-arms)))
(define (iref? r)
  (let loop ((as iref-arms))
    (cond ((null? as) #f) (((car as) r) #t) (else (loop (cdr as))))))
;; Watches and validators for the IRefs that are NOT atoms (vars, refs) live in
;; side-tables — an atom keeps its own in the record. A Chez hashtable is not
;; thread-safe, and add-watch/set-validator! can be called from any thread while
;; def-var! reads both tables on EVERY def, so both go through one mutex.
;;
;; The read side keeps a lock-free fast path, because the read side is the hot
;; one: iref-writes counts the add-watch / set-validator! calls that put an entry
;; in either table, so a program that watches nothing pays one box read instead of
;; two weak-table probes per def — cheaper than before the lock. It is a call
;; COUNT, not a live-entry count: nothing decrements it, so once a watch has
;; existed the fast path is off for good even if remove-watch took it away again.
;; That direction is the safe one, and it is why remove-watch does not bump — it
;; can only shrink an entry some earlier bump already accounted for.
;;
;; The bump is inside the mutex and BEFORE the table write, which is what makes
;; the fast path sound: a def-var! racing an add-watch either misses the bump and
;; the entry both (the watch was not registered yet) or sees the bump and takes
;; the lock, where it finds whatever the writer left. Bumping after the write
;; would leave a window where the entry is in the table and iref-any? still reads
;; 0, and that def would silently skip validation and notification. A fixnum box
;; read cannot tear.
(define iref-watch-tbl (make-weak-eq-hashtable))
(define iref-validator-tbl (make-weak-eq-hashtable))
(define iref-tbl-mu (make-mutex))
(define iref-writes (box 0))
(define (iref-any?) (not (eqv? 0 (unbox iref-writes))))
;; call with iref-tbl-mu HELD — the read-modify-write is not atomic on its own.
(define (iref-tbl-bump!) (set-box! iref-writes (fx+ 1 (unbox iref-writes))))
(define (iref-watches-of r)
  (if (iref-any?) (jolt-with-mutex iref-tbl-mu (hashtable-ref iref-watch-tbl r '())) '()))
(define (iref-validator-of r)
  (if (iref-any?)
      (jolt-with-mutex iref-tbl-mu (hashtable-ref iref-validator-tbl r jolt-nil))
      jolt-nil))
(define (iref-notify r old new)
  (for-each (lambda (kv) (jolt-invoke4 (cdr kv) (car kv) r old new))
            (reverse (iref-watches-of r))))
(define (iref-validate r v)
  (let ((vf (iref-validator-of r)))
    (when (and (not (jolt-nil? vf)) (jolt-not (jolt-invoke1 vf v)))
      (jolt-iref-state-throw))))

;; add-watch interns (key . fn) (replacing any existing key, keeping order);
;; remove-watch drops it; both return the reference. set-validator! installs a
;; validator and validates the CURRENT value immediately (Clojure throws if it's
;; already invalid); get-validator reads the slot.
;;
;; An ATOM keeps its watches in a record slot rather than the side tables below, and
;; that slot needs the same treatment the tables got: rebuilding the list from the
;; value just read is a read-modify-write, so two threads registering DIFFERENT keys
;; could leave only one behind. ARef.addWatch and removeWatch are both `synchronized`
;; on the JVM. Measured, 8 threads x 200 distinct keys: the list came back short.
;; The atom's own lock, held only across the rebuild — no user code runs inside it, so
;; this stays one of the short regions locks.ss is about. The NOTIFY still runs
;; outside, as it does on the JVM: holding a lock across a watch would let one watch
;; deadlock every other registration.
;; set-validator! needs none of this: it overwrites a slot rather than deriving the
;; new value from the old, so there is nothing to lose.
(define (jolt-watch-add alist key f)
  (cons (cons key f) (remp (lambda (kv) (jolt=2 (car kv) key)) alist)))
;; The watch alist moves by CAS like the value: two threads adding at once
;; both land (a loser recomputes from the winner's list), which the mutex
;; guaranteed and a plain set! would not.
(define (jolt-atom-watches-update! a f)
  (let retry ()
    (let* ((ws (jolt-atom-watches a)) (nws (f ws)))
      ;; NWS is a fresh alist that a swap! on another thread walks
      (memory-order-release)
      (unless (sa-record-cas! a jolt-atom-watches-slot ws nws) (retry)))))
(define (jolt-add-watch a key f)
  (cond
    ((jolt-atom? a)
     (jolt-atom-watches-update! a (lambda (ws) (jolt-watch-add ws key f)))
     a)
    ((iref? a)
     (jolt-with-mutex iref-tbl-mu
       (iref-tbl-bump!)
       (hashtable-set! iref-watch-tbl a
         (jolt-watch-add (hashtable-ref iref-watch-tbl a '()) key f)))
     a)
    (else (throw-jvm (quote ClassCastException) "add-watch: not a watchable reference"))))
(define (jolt-remove-watch a key)
  (cond
    ((jolt-atom? a)
     (jolt-atom-watches-update! a (lambda (ws) (remp (lambda (kv) (jolt=2 (car kv) key)) ws)))
     a)
    ((iref? a)
     (jolt-with-mutex iref-tbl-mu
       (hashtable-set! iref-watch-tbl a
         (remp (lambda (kv) (jolt=2 (car kv) key)) (hashtable-ref iref-watch-tbl a '()))))
     a)
    (else (throw-jvm (quote ClassCastException) "remove-watch: not a watchable reference"))))
(define (jolt-set-validator! a f)
  (let ((vf (if (jolt-nil? f) jolt-nil f)))
    (cond
      ((jolt-atom? a)
       (when (and (not (jolt-nil? vf)) (jolt-not (jolt-invoke1 vf (jolt-atom-val a))))
         (jolt-iref-state-throw))
       (jolt-atom-validator-set! a vf))
      ((iref? a)
       (when (and (not (jolt-nil? vf)) (jolt-not (jolt-invoke1 vf (jolt-deref a))))
         (jolt-iref-state-throw))
       (jolt-with-mutex iref-tbl-mu
         (iref-tbl-bump!)
         (hashtable-set! iref-validator-tbl a vf)))
      (else (throw-jvm (quote ClassCastException) "set-validator!: not a reference")))
    jolt-nil))
(define (jolt-get-validator a)
  (cond ((jolt-atom? a) (jolt-atom-validator a))
        ((iref? a) (iref-validator-of a))
        (else jolt-nil)))

;; The rest of the ARef surface, which only the java interop layer reaches:
;; records-dispatch.ss's rd-iref-method answers .getWatches and .notifyWatches
;; with these. clojure.core has no fn for either — add-watch/remove-watch are the
;; only public door — so they exist for the METHOD arm alone, and they live here
;; beside the seam they read rather than in the java layer because both hosts
;; share this file and neither shares that one.
;;
;; jolt-iref-watchable? is that arm's receiver guard: the four watchable
;; reference types (atom / var / ref / agent) and nothing else, so a receiver
;; that merely spells a method the same way keeps its own dispatch. It has to
;; ASK on every call rather than close over a fixed answer: iref? walks the arm
;; registry, and the agent arm is not registered until java/concurrency.ss loads,
;; long after this file.
(define (jolt-iref-watchable? r) (or (jolt-atom? r) (iref? r)))

;; ARef.getWatches is an IPersistentMap, so an unwatched reference reads {} and
;; not nil. Both alists are reverse-built (jolt-watch-add conses), and this walk
;; front-to-back prepending key and fn undoes that — a seq over the map is not
;; ordered anyway, but the flat kv list going in matches add order, which is the
;; order notifyWatches uses.
(define (jolt-get-watches r)
  (let ((alist (cond ((jolt-atom? r) (jolt-atom-watches r))
                     ((iref? r) (iref-watches-of r))
                     (else (throw-jvm (quote ClassCastException)
                                      "getWatches: not a watchable reference")))))
    (let loop ((as alist) (kvs (quote ())))
      (if (null? as)
          (jolt-hash-map-build kvs)
          (loop (cdr as) (cons (caar as) (cons (cdar as) kvs)))))))

;; ARef.notifyWatches(old, new) is void and fires the watches WITHOUT touching
;; the value — the JVM lets a subclass drive notification at its own mutation
;; points, and jolt's ref types do exactly that (refs at commit, vars at root
;; set). An atom keeps its watches in a record slot, every other reference type
;; in the side table, so the two notifiers stay separate here as they do there.
(define (jolt-notify-watches r old new)
  (cond ((jolt-atom? r) (jolt-atom-notify r old new))
        ((iref? r) (iref-notify r old new))
        (else (throw-jvm (quote ClassCastException)
                         "notifyWatches: not a watchable reference")))
  jolt-nil)

;; vars are watchable IRefs: a root change (def / var-set on the root /
;; alter-var-root) validates and notifies like Var.bindRoot. The def-var! wrap
;; pays two weak-table probes per def and only does IRef work on a watched var.
(register-iref-arm! var-cell?)
(define def-var!-pre-iref def-var!)
(set! def-var!
  (lambda (ns name v)
    (let ((c (jolt-var ns name)))
      (if (and (iref-any?)
               (or (pair? (iref-watches-of c))
                   (not (jolt-nil? (iref-validator-of c)))))
          (let ((old (var-cell-root c)))
            (iref-validate c v)
            (let ((r (def-var!-pre-iref ns name v)))
              (iref-notify c old v)
              r))
          (def-var!-pre-iref ns name v)))))

(def-var! "clojure.core" "atom" jolt-atom-new)
(def-var! "clojure.core" "deref" jolt-deref)
(def-var! "clojure.core" "swap!" jolt-swap!)
(def-var! "clojure.core" "reset!" jolt-reset!)
(def-var! "clojure.core" "compare-and-set!" jolt-compare-and-set!)
(def-var! "clojure.core" "swap-vals!" jolt-swap-vals!)
(def-var! "clojure.core" "reset-vals!" jolt-reset-vals!)
(def-var! "clojure.core" "atom?" jolt-atom?)
;; peripheral ops: the overlay (20-coll) re-defs these over jolt.host/ref-put!,
;; which fails on an atom record — post-prelude.ss re-asserts the natives.
(def-var! "clojure.core" "add-watch" jolt-add-watch)
(def-var! "clojure.core" "remove-watch" jolt-remove-watch)
(def-var! "clojure.core" "set-validator!" jolt-set-validator!)
(def-var! "clojure.core" "get-validator" jolt-get-validator)
