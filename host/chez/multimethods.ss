;; multimethods (jolt-9ls5) — the multimethod dispatch runtime on the Chez host.
;;
;; defmulti/defmethod are macros that expand to ctx-capturing setup CALLS
;; (defmulti-setup / defmethod-setup, + the table ops get-method/methods/
;; remove-method/prefer-method/prefers), implemented here against
;; the runtime's ns/var machinery.
;;
;; A multimethod VALUE is a jolt-multifn record carrying its dispatch fn and a
;; mutable method table (dispatch-val -> method fn, keyed with jolt= so keyword/
;; vector/number dispatch values match by value). jolt-invoke dispatches it:
;; an exact method, else an isa?/hierarchy match (resolved through prefer-method
;; and the overlay's isa?/derive/hierarchy), else the :default method.
;;
;; NS resolution: defmulti expands to (defmulti-setup (quote name) ...) with a
;; BARE symbol — the Chez RT has no compile-time current ns at the call site, so a
;; runtime `chez-current-ns` box names where to def-var! the multifn. It defaults
;; to "user" (matching the analyzer's ns for -e user code); the assembled prelude
;; sets it to "clojure.core" around its own load (program-with-prelude), so the
;; print-method/print-dup defmultis land in clojure.core. defmethod-setup and the
;; symbol-taking table ops resolve the multifn via (var-deref (chez-current-ns) …),
;; so they agree with defmulti. Loaded from rt.ss after seq.ss (jolt-invoke),
;; collections.ss (jolt=/key-hash/jolt-hash-map) and the var-cell machinery.

(define chez-current-ns-box (vector "user"))
(define (chez-current-ns) (vector-ref chez-current-ns-box 0))
(define (set-chez-ns! ns) (vector-set! chez-current-ns-box 0 ns))

(define-record-type jolt-multifn
  (fields name dispatch-fn methods default hierarchy prefers)
  (nongenerative jolt-multifn-v1))

(define kw-default (keyword #f "default"))
(define (new-mm-table) (make-hashtable key-hash jolt=))

;; (defmulti-setup 'name dispatch & opts) — opts is a flat :default/:hierarchy plist.
(define (parse-mm-opts opts)
  (let loop ((o opts) (dk kw-default) (h #f))
    (if (or (null? o) (null? (cdr o)))
        (values dk h)
        (let ((k (car o)) (v (cadr o)))
          (cond
            ((and (keyword? k) (not (keyword-t-ns k)) (string=? (keyword-t-name k) "default"))
             (loop (cddr o) v h))
            ((and (keyword? k) (not (keyword-t-ns k)) (string=? (keyword-t-name k) "hierarchy"))
             (loop (cddr o) dk v))
            (else (loop (cddr o) dk h)))))))

(define (jolt-defmulti-setup name-sym dispatch . opts)
  (let-values (((dk h) (parse-mm-opts opts)))
    (let ((mf (make-jolt-multifn (symbol-t-name name-sym) dispatch
                                 (new-mm-table) dk h (new-mm-table))))
      (def-var! (chez-current-ns) (symbol-t-name name-sym) mf)
      mf)))

;; (defmethod-setup 'mm dispatch-val impl) — add a method. Auto-creates the multifn
;; if absent (defmethod before defmulti — rare; identity dispatch as a fallback).
(define (jolt-defmethod-setup mm-sym dval impl)
  (let* ((nm (symbol-t-name mm-sym))
         (sns (symbol-t-ns mm-sym))
         (qns (and sns (not (jolt-nil? sns)) (not (null? sns)) sns))
         ;; resolve the multifn's HOME ns like a var: a qualified name in its own ns
         ;; (cross-ns defmethod); an unqualified name via current ns -> :refer ->
         ;; clojure.core, so (defmethod print-method ...) finds clojure.core's multifn
         ;; instead of auto-creating a stray one in the current ns.
         (mns (cond
                (qns (or (chez-resolve-alias (chez-current-ns) qns) qns))
                ((let ((c (var-cell-lookup (chez-current-ns) nm))) (and c (var-cell-defined? c)))
                 (chez-current-ns))
                ((chez-resolve-refer (chez-current-ns) nm))
                ((let ((c (var-cell-lookup "clojure.core" nm))) (and c (var-cell-defined? c)))
                 "clojure.core")
                (else (chez-current-ns))))
         (cur (var-deref mns nm))
         (mf (if (jolt-multifn? cur) cur
                 (let ((m (make-jolt-multifn nm (var-deref "clojure.core" "identity")
                                             (new-mm-table) kw-default #f (new-mm-table))))
                   (def-var! mns nm m) m))))
    (hashtable-set! (jolt-multifn-methods mf) dval impl)
    mf))

;; --- dispatch ----------------------------------------------------------------
(define (mm-isa? mf)
  ;; the overlay's isa? (the hierarchy system is pure Clojure); a per-mm :hierarchy
  ;; is an atom (deref each dispatch, like a Clojure var) or a plain map.
  (let* ((isa (var-deref "clojure.core" "isa?"))
         (h (jolt-multifn-hierarchy mf))
         (hval (and h (if (jolt-atom? h) (jolt-atom-val h) h))))
    (lambda (x y) (jolt-truthy? (if hval (jolt-invoke isa hval x y) (jolt-invoke isa x y))))))

(define (mm-find-isa mf dv)
  (let* ((methods (jolt-multifn-methods mf))
         (isa? (mm-isa? mf))
         (default (jolt-multifn-default mf))
         (keys (filter (lambda (k) (not (jolt= k default)))
                       (vector->list (hashtable-keys methods))))
         (matches (filter (lambda (k) (isa? dv k)) keys)))
    (cond
      ((null? matches) #f)
      ((null? (cdr matches)) (hashtable-ref methods (car matches) #f))
      (else
       ;; >1 isa-match: pick the dominant key (x dominates y when x is
       ;; prefer-method'd over y, or (isa? x y)); ambiguity with no dominant is an
       ;; error, as in Clojure.
       (let* ((prefers (jolt-multifn-prefers mf))
              (pref? (lambda (x y)
                       (let ((px (hashtable-ref prefers x #f)))
                         (and px (hashtable-ref px y #f) #t))))
              (dom? (lambda (x y) (or (pref? x y) (isa? x y))))
              (best (fold-left (lambda (b k) (if (dom? k b) k b)) (car matches) (cdr matches))))
         (for-each
          (lambda (k)
            (when (and (not (jolt= k best)) (not (dom? best k)))
              (error #f (string-append "Multiple methods in multimethod '" (jolt-multifn-name mf)
                                       "' match dispatch value - and neither is preferred"))))
          matches)
         (hashtable-ref methods best #f))))))

(define (multifn-dispatch mf . args)
  (let* ((dv (apply jolt-invoke (jolt-multifn-dispatch-fn mf) args))
         (methods (jolt-multifn-methods mf))
         (direct (hashtable-ref methods dv #f)))
    (cond
      (direct (apply jolt-invoke direct args))
      ((mm-find-isa mf dv) => (lambda (m) (apply jolt-invoke m args)))
      ((hashtable-ref methods (jolt-multifn-default mf) #f)
       => (lambda (m) (apply jolt-invoke m args)))
      (else (error #f (string-append "No method in multimethod '" (jolt-multifn-name mf)
                                     "' for dispatch value: " (jolt-pr-str dv)))))))

;; jolt-invoke dispatches a multifn (otherwise falls through to the prior logic).
(define %prev-jolt-invoke jolt-invoke)
(set! jolt-invoke
  (lambda (f . args)
    (if (jolt-multifn? f)
        (apply multifn-dispatch f args)
        (apply %prev-jolt-invoke f args))))

;; --- table ops ---------------------------------------------------------------
;; prefer-method/remove-method/remove-all-methods/prefers take the name QUOTED;
;; get-method/methods take the multifn VALUE (Clojure semantics).
(define (mm-of-sym sym) (let ((v (var-deref (chez-current-ns) (symbol-t-name sym))))
                          (and (jolt-multifn? v) v)))

(define (jolt-prefer-method-setup mm-sym dval-a dval-b)
  (let ((mf (mm-of-sym mm-sym)))
    (when mf
      (let ((sub (or (hashtable-ref (jolt-multifn-prefers mf) dval-a #f)
                     (let ((h (new-mm-table)))
                       (hashtable-set! (jolt-multifn-prefers mf) dval-a h) h))))
        (hashtable-set! sub dval-b #t)))
    mf))

(define (jolt-remove-method-setup mm-sym dval)
  (let ((mf (mm-of-sym mm-sym)))
    (when mf (hashtable-delete! (jolt-multifn-methods mf) dval))
    mf))

(define (jolt-remove-all-methods-setup mm-sym)
  (let ((mf (mm-of-sym mm-sym)))
    (when mf (hashtable-clear! (jolt-multifn-methods mf)))
    mf))

(define (jolt-get-method-setup mf dval)
  (if (jolt-multifn? mf)
      (or (hashtable-ref (jolt-multifn-methods mf) dval #f)
          (hashtable-ref (jolt-multifn-methods mf) (jolt-multifn-default mf) #f)
          jolt-nil)
      jolt-nil))

(define (jolt-methods-setup mf)
  (if (jolt-multifn? mf)
      (let-values (((ks vs) (hashtable-entries (jolt-multifn-methods mf))))
        (let loop ((i 0) (m (jolt-hash-map)))
          (if (fx>=? i (vector-length ks)) m
              (loop (fx+ i 1) (jolt-assoc m (vector-ref ks i) (vector-ref vs i))))))
      jolt-nil))

(define (jolt-prefers-setup mm-sym)
  (let ((mf (mm-of-sym mm-sym)))
    (if (not mf) (jolt-hash-map)
        (let-values (((ks vs) (hashtable-entries (jolt-multifn-prefers mf))))
          (let loop ((i 0) (m (jolt-hash-map)))
            (if (fx>=? i (vector-length ks)) m
                ;; each value is an inner set of preferred-over keys -> a jolt set
                (loop (fx+ i 1)
                      (jolt-assoc m (vector-ref ks i)
                                  (apply jolt-hash-set
                                         (vector->list (hashtable-keys (vector-ref vs i))))))))))))

(def-var! "clojure.core" "defmulti-setup" jolt-defmulti-setup)
(def-var! "clojure.core" "defmethod-setup" jolt-defmethod-setup)
(def-var! "clojure.core" "prefer-method-setup" jolt-prefer-method-setup)
(def-var! "clojure.core" "remove-method-setup" jolt-remove-method-setup)
(def-var! "clojure.core" "remove-all-methods-setup" jolt-remove-all-methods-setup)
(def-var! "clojure.core" "get-method-setup" jolt-get-method-setup)
(def-var! "clojure.core" "methods-setup" jolt-methods-setup)
(def-var! "clojure.core" "prefers-setup" jolt-prefers-setup)
