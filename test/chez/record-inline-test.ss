;; record-inline-test.ss — the define-record-type wrapper in
;; scheme-adapter-runtime.ss binds a nongenerative type's predicate, accessors,
;; mutators and constructor as syntax that open-codes on the constant rtd. This
;; pins the shape of the expansion (no call to the generated name remains) and
;; the semantics the wrapper promises to keep: the name is still a procedure as
;; a value, a wrong-arity call still fails at run time, a type error still
;; carries the same message and irritants, set! is rejected, and the fallbacks
;; (generative type, protocol constructor, child constructor) still work.
(import (chezscheme))
(load "host/chez/rt.ss")

(define total 0)
(define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))

(define (expansion-string form)
  (parameterize ((print-gensym #f))
    (with-output-to-string (lambda () (write (expand/optimize form))))))
(define (contains? hay needle)
  (let ((n (string-length needle)) (h (string-length hay)))
    (let loop ((i 0))
      (and (<= (+ i n) h)
           (or (string=? (substring hay i (+ i n)) needle)
               (loop (+ i 1)))))))
;; open-coded: the generated name is gone and the primitive test is there
(define (open-coded? form name prim)
  (let ((s (expansion-string form)))
    (and (not (contains? s name)) (contains? s prim))))

;; --- the runtime's own types ---------------------------------------------------
(ok "pvec? open-codes to record?"
    (open-coded? '(lambda (x) (pvec? x)) "pvec?" "record?"))
(ok "pmap? open-codes" (open-coded? '(lambda (x) (pmap? x)) "pmap?" "record?"))
(ok "cseq? open-codes" (open-coded? '(lambda (x) (cseq? x)) "cseq?" "record?"))
(ok "keyword-t? open-codes (the back end emits it)"
    (open-coded? '(lambda (x) (keyword-t? x)) "keyword-t?" "record?"))
(ok "jrec? open-codes (records.ss family root)"
    (open-coded? '(lambda (x) (jrec? x)) "jrec?" "record?"))
(ok "accessor open-codes to $object-ref"
    (open-coded? '(lambda (x) (cseq-kind x)) "cseq-kind" "$object-ref"))
(ok "mutator open-codes to $object-set!"
    (open-coded? '(lambda (x v) (pvec-meta-set! x v)) "pvec-meta-set!" "$object-set!"))
(ok "constructor open-codes to $record"
    (open-coded? '(lambda (a b c) (%mk-pset a b c)) "%mk-pset" "$record"))
(ok "predicate then accessor is one type test"
    (let ((s (expansion-string '(lambda (x) (if (pvec? x) (pvec-cnt x) 0)))))
      (and (contains? s "record?")
           ;; exactly one record? test: the accessor's own check merged into it
           (let loop ((i 0) (n 0))
             (cond ((> (+ i 7) (string-length s)) (= n 1))
                   ((string=? (substring s i (+ i 7)) "record?") (loop (+ i 7) (+ n 1)))
                   (else (loop (+ i 1) n)))))))

;; --- values: same answers as the procedures ------------------------------------
(define v (jolt-vector 1 2 3))
(define l (cseq-list 1 jolt-nil))
(ok "predicate hit"  (pvec? v))
(ok "predicate miss on another record" (not (pvec? l)))
(ok "predicate miss on a fixnum" (not (pvec? 5)))
(ok "accessor reads"  (= (pvec-cnt v) 3))
(ok "name as a value is a procedure" (procedure? pvec?))
(ok "value position gives one stable object" (eq? pvec? pvec?))
(ok "map over the predicate" (equal? (map pvec? (list 1 v l)) '(#f #t #f)))
(ok "apply the accessor" (= (apply pvec-cnt (list v)) 3))
(ok "accessor as a value answers the same" (= ((lambda (f) (f v)) pvec-cnt) 3))

;; --- errors keep their shape -----------------------------------------------------
(define (catch thunk) (guard (e (#t e)) (thunk)))
(let ((e (catch (lambda () (pvec-cnt 5)))))
  (ok "type error is a condition" (condition? e))
  (ok "type error message unchanged"
      (and (message-condition? e) (string=? (condition-message e) "~s is not of type ~s")))
  (ok "type error irritants: the value and the rtd"
      (and (irritants-condition? e) (eqv? (car (condition-irritants e)) 5))))
(let ((e (catch (lambda () (eval '(pvec-cnt (jolt-vector 1) 2))))))
  (ok "wrong arity is a run-time error, not a syntax error"
      (and (condition? e) (message-condition? e)
           (contains? (condition-message e) "number of arguments"))))
(ok "set! of a generated name is rejected at expansion"
    (let ((e (catch (lambda () (eval '(set! pvec? 5))))))
      (and (condition? e) (syntax-violation? e))))

;; --- the definition shapes the host uses -------------------------------------
(define-record-type (rit-plain mk-rit-plain rit-plain?)
  (fields a (mutable b) (immutable c rit-c) (mutable d rit-d rit-d!))
  (nongenerative rit-plain-v1))
(define p (mk-rit-plain 1 2 3 4))
(ok "explicit accessor names" (and (= (rit-c p) 3) (= (rit-d p) 4)))
(ok "explicit mutator name" (begin (rit-d! p 40) (= (rit-d p) 40)))
(ok "default mutator name" (begin (rit-plain-b-set! p 20) (= (rit-plain-b p) 20)))
(ok "default constructor open-codes" (open-coded? '(lambda (a b c d) (mk-rit-plain a b c d)) "mk-rit-plain" "$record"))

(define-record-type rit-bare (fields x) (nongenerative rit-bare-v1))
(ok "bare name spec: make-/? defaults" (and (rit-bare? (make-rit-bare 1)) (= (rit-bare-x (make-rit-bare 9)) 9)))

(define-record-type (rit-child mk-rit-child rit-child?)
  (parent rit-plain) (fields e) (nongenerative rit-child-v1))
(define c (mk-rit-child 1 2 3 4 5))
(ok "child constructor takes parent fields too" (= (rit-child-e c) 5))
(ok "parent predicate answers the child" (rit-plain? c))
(ok "parent accessor open-codes on a child instance" (= (rit-c c) 3))
(ok "child predicate open-codes" (open-coded? '(lambda (x) (rit-child? x)) "rit-child?" "record?"))
(ok "child predicate rejects the parent" (not (rit-child? p)))

(define-record-type rit-prot (fields s) (nongenerative rit-prot-v1)
  (protocol (lambda (n) (lambda (s) (n (string-append "/" s))))))
(ok "protocol constructor kept" (string=? (rit-prot-s (make-rit-prot "x")) "/x"))
(ok "protocol type's predicate still open-codes" (open-coded? '(lambda (x) (rit-prot? x)) "rit-prot?" "record?"))

(define-record-type rit-gen (fields g))
(ok "generative type: plain definition" (and (rit-gen? (make-rit-gen 1)) (= (rit-gen-g (make-rit-gen 2)) 2)))

(define-record-type (rit-sealed mk-rit-sealed rit-sealed?) (fields a)
  (nongenerative rit-sealed-v1) (sealed #t) (opaque #t))
(ok "sealed type open-codes to $sealed-record?"
    (open-coded? '(lambda (x) (rit-sealed? x)) "rit-sealed?" "$sealed-record?"))

(ok "a body keeps the plain R6RS form under its private name"
    (let ()
      (%r6rs-define-record-type (rit-loc mk-rit-loc rit-loc?) (fields a) (nongenerative rit-loc-v1))
      (and (rit-loc? (mk-rit-loc 1)) (= (rit-loc-a (mk-rit-loc 7)) 7))))

;; re-loading a file re-evaluates its definitions: same type, still consistent
(define-record-type rit-bare (fields x) (nongenerative rit-bare-v1))
(ok "redefinition keeps the earlier instances" (rit-bare? (make-rit-bare 1)))
(ok "redefinition still open-codes" (open-coded? '(lambda (x) (rit-bare? x)) "rit-bare?" "record?"))
(ok "an early forward reference still resolves through the variable"
    ;; compiled before rit-late exists, exactly like collections.ss -> jrec?
    (let ((early (eval '(lambda (x) (rit-late? x)))))
      (eval '(define-record-type rit-late (fields a) (nongenerative rit-late-v1)))
      (and (early (eval '(make-rit-late 1))) (not (early 5)))))

;; --- load order: the dispatch files see every layout they test -----------------
;; A record op is open-coded only in forms compiled AFTER its definition; a
;; reference compiled earlier is a call on every arm. So the files that hold the
;; hot dispatch chains (jolt=, jolt-hash, hasheq.ss, the collection and seq ops,
;; the metadata carry) must load after every record type they name -- which is
;; why values.ss defines the collection layouts. Computed from the sources: walk
;; rt.ss in order, index each loaded file (the adapter loads locks + fibers
;; first), collect the names every define-record-type binds, and fail on a
;; checked file naming an op that a later-indexed file defines. The cold arms a
;; checked file is allowed to reach forward to are listed per file, each one a
;; type no hot chain tests first.
(define (read-forms path)
  (call-with-input-file path
    (lambda (p)
      (let loop ((acc '()))
        (let ((x (guard (e (#t (eof-object))) (read p))))
          (if (eof-object? x) (reverse acc) (loop (cons x acc))))))))
(define (record-op-names form)
  ;; the names one define-record-type form binds (predicate, ctor, accessors,
  ;; mutators), or '() for a form that is not one
  (define (sym . parts)
    (string->symbol (apply string-append (map (lambda (p) (if (symbol? p) (symbol->string p) p)) parts))))
  (if (and (pair? form) (eq? (car form) 'define-record-type) (pair? (cdr form)))
      (let* ((spec (cadr form)) (clauses (cddr form))
             (name (if (pair? spec) (car spec) spec)))
        (if (not (symbol? name)) '()
            (let* ((ctor (if (pair? spec) (cadr spec) (sym "make-" name)))
                   (pred (if (pair? spec) (caddr spec) (sym name "?")))
                   (fields (let ((fc (assq 'fields clauses))) (if fc (cdr fc) '()))))
              (append
               (list pred ctor)
               (apply append
                      (map (lambda (fs)
                             (cond ((symbol? fs) (list (sym name "-" fs)))
                                   ((and (pair? fs) (memq (car fs) '(immutable mutable)) (pair? (cdr fs)))
                                    (let* ((f (cadr fs))
                                           (acc (if (and (pair? (cddr fs)) (symbol? (caddr fs))) (caddr fs) (sym name "-" f)))
                                           (mut (and (eq? (car fs) 'mutable)
                                                     (if (and (pair? (cddr fs)) (pair? (cdddr fs)) (symbol? (cadddr fs)))
                                                         (cadddr fs) (sym name "-" f "-set!")))))
                                      (if mut (list acc mut) (list acc))))
                                   (else '())))
                           fields))))))
      '()))
(define (all-symbols form)
  (let walk ((x form) (acc '()))
    (cond ((symbol? x) (cons x acc))
          ((pair? x) (walk (car x) (walk (cdr x) acc)))
          ((vector? x) (let loop ((i 0) (acc acc)) (if (= i (vector-length x)) acc (loop (+ i 1) (walk (vector-ref x i) acc)))))
          (else acc))))
;; file -> load index, and record op -> (defining index . file)
(define load-index (make-hashtable string-hash string=?))
(define op-def (make-eq-hashtable))
(define (index-file! path i)
  (hashtable-set! load-index path i)
  (for-each (lambda (f)
              (for-each (lambda (n) (unless (hashtable-ref op-def n #f) (hashtable-set! op-def n (cons i path))))
                        (record-op-names f)))
            (read-forms path)))
(let ((i 0))
  (define (next!) (set! i (+ i 1)) i)
  (index-file! "host/chez/scheme-adapter-runtime.ss" (next!))
  (index-file! "host/chez/locks.ss" (next!))
  (index-file! "host/chez/fibers.ss" (next!))
  ;; rt.ss's own record definitions sit between its loads: index them where
  ;; they fall in the sequence
  (for-each
   (lambda (f)
     (cond ((and (pair? f) (eq? (car f) 'load) (pair? (cdr f)) (string? (cadr f))
                 (not (hashtable-ref load-index (cadr f) #f)))
            (index-file! (cadr f) (next!)))
           ((and (pair? f) (eq? (car f) 'define-record-type))
            (let ((k (next!)))
              (for-each (lambda (n) (unless (hashtable-ref op-def n #f) (hashtable-set! op-def n (cons k "host/chez/rt.ss"))))
                        (record-op-names f))))))
   (read-forms "host/chez/rt.ss")))
(define forward-allowed
  ;; the cold forward arms a checked file may keep: a var or reify called as a
  ;; function, a transient's count, an unbound-var marker
  '(("host/chez/seq.ss" jolt-var-unbound? jolt-var-unbound-ns jolt-var-unbound-name
                        jreify? jreify-protos jolt-transient?)
    ("host/chez/natives-meta.ss" juuid? jvol?)))
(define (forward-refs path)
  (let ((here (hashtable-ref load-index path #f))
        (allowed (cond ((assoc path forward-allowed) => cdr) (else '())))
        (seen (make-eq-hashtable)))
    (for-each (lambda (f) (for-each (lambda (s) (hashtable-set! seen s #t)) (all-symbols f)))
              (read-forms path))
    (let loop ((names (vector->list (hashtable-keys seen))) (bad '()))
      (if (null? names) bad
          (let* ((n (car names)) (d (hashtable-ref op-def n #f)))
            (loop (cdr names)
                  (if (and d (> (car d) here) (not (memq n allowed)))
                      (cons (list n (cdr d)) bad)
                      bad)))))))
(for-each
 (lambda (path)
   (let ((bad (forward-refs path)))
     (ok (string-append path " names no record op a later file defines")
         (or (null? bad) (begin (printf "  forward refs in ~a: ~s\n" path bad) #f)))))
 '("host/chez/values.ss" "host/chez/hasheq.ss" "host/chez/collections.ss"
   "host/chez/seq.ss" "host/chez/natives-meta.ss"))
(ok "the allowlist is not stale"
    (let ((stale
           (apply append
                  (map (lambda (entry)
                         (let* ((path (car entry))
                                (here (hashtable-ref load-index path #f))
                                (used (let ((seen (make-eq-hashtable)))
                                        (for-each (lambda (f) (for-each (lambda (s) (hashtable-set! seen s #t)) (all-symbols f)))
                                                  (read-forms path))
                                        seen)))
                           (filter (lambda (n)
                                     (let ((d (hashtable-ref op-def n #f)))
                                       (not (and d (> (car d) here) (hashtable-ref used n #f)))))
                                   (cdr entry))))
                       forward-allowed))))
      (or (null? stale) (begin (printf "  stale allowlist names: ~s\n" stale) #f))))

;; --- the cost, as a ratio measured in this process ------------------------------
;; The expansion checks above say the open-coded form is emitted; this says it
;; is cheaper than the call it replaces, judged the only way a timing gate is
;; allowed to be judged here: two arms in ONE run on ONE machine, so speed and
;; load cancel. The reference arm is the same chain through the procedures the
;; names denote as values, held in top-level variables -- exactly what every
;; generated name WAS before the wrapper -- so the ratio is the wrapper's whole
;; effect and nothing else. Measured: 0.45 with the wrapper, 1.00 on the tree
;; before it (the two arms are then the same code). The ceiling sits between.
;;
;; Timed as test/complexity_test.clj times its rows: the two arms alternate, one
;; sample of each per round with the order swapped every round, so a busy stretch
;; lands on both instead of on one arm's whole best-of block; each arm is judged
;; on its fastest sample over all rounds, since interference only ever makes a
;; loop slower; each round starts from a full collection; and a miss is measured
;; once more before it counts. A sample is one 1M-iteration loop, a few
;; milliseconds, on the nanosecond clock. There is no fail-on-sight band: one
;; reading is never enough to fail on, and the unwrapped tree reads ~1.0 on the
;; re-measure too.
(define called-pmap? pmap?)
(define called-pvec? pvec?)
(define called-pset? pset?)
(define called-lazyseq? jolt-lazyseq?)
(define called-empty? empty-list-t?)
(define called-cseq? cseq?)
(define called-cseq-kind cseq-kind)
(define called-pvec-cnt pvec-cnt)
(define ratio-iters 1000000)
(define ratio-rounds 11)
;; This thread's CPU time, not the wall clock: a preempted sample reads as a regression.
(define (now-ns)
  (let ((t (current-time 'time-thread)))
    (+ (* (time-second t) 1000000000) (time-nanosecond t))))
(define (sample-ns f x)
  (let ((t0 (now-ns))) (f x) (- (now-ns) t0)))
;; the loop body is a boolean over X; the count keeps it live
(define-syntax timed-loop
  (syntax-rules ()
    ((_ (x) expr)
     (lambda (x)
       (let loop ((i 0) (acc 0))
         (if (fx=? i ratio-iters) acc (loop (fx+ i 1) (if expr (fx+ acc 1) acc))))))))
;; the metadata carry's shape: five misses, then the hit and a slot read
(define chain-open
  (timed-loop (x) (cond ((pmap? x) #f) ((pvec? x) #f) ((pset? x) #f) ((jolt-lazyseq? x) #f)
                        ((empty-list-t? x) #f) ((cseq? x) (fx=? (cseq-kind x) sk-list)) (else #f))))
(define chain-called
  (timed-loop (x) (cond ((called-pmap? x) #f) ((called-pvec? x) #f) ((called-pset? x) #f)
                        ((called-lazyseq? x) #f) ((called-empty? x) #f)
                        ((called-cseq? x) (fx=? (called-cseq-kind x) sk-list)) (else #f))))
;; a predicate guarding an accessor: one type test when open-coded
(define pair-open   (timed-loop (x) (and (pvec? x) (fx=? (pvec-cnt x) 3))))
(define pair-called (timed-loop (x) (and (called-pvec? x) (fx=? (called-pvec-cnt x) 3))))
;; -> open-coded / called, each arm its fastest sample over the rounds
(define (paired-ratio open called x)
  (open x) (called x)
  (let loop ((i 0) (to +inf.0) (tc +inf.0))
    (if (fx=? i ratio-rounds)
        (/ to (max tc 1.0))
        (begin
          (collect (collect-maximum-generation))
          (if (even? i)
              (let* ((a (sample-ns open x)) (b (sample-ns called x)))
                (loop (fx+ i 1) (min to a) (min tc b)))
              (let* ((b (sample-ns called x)) (a (sample-ns open x)))
                (loop (fx+ i 1) (min to a) (min tc b))))))))
(define (judge-ratio label open called x ceiling)
  (let* ((r1 (paired-ratio open called x))
         (r (if (> r1 ceiling)
                (begin (printf "  ~a: open-coded / called = ~,3f, over ~a, measuring again\n" label r1 ceiling)
                       (paired-ratio open called x))
                r1)))
    (printf "  ~a: open-coded / called = ~,3f (ceiling ~a)\n" label r ceiling)
    (ok (string-append label " is cheaper open-coded than called") (<= r ceiling))))
(judge-ratio "six-arm kind chain" chain-open chain-called l 0.75)
(judge-ratio "predicate + accessor" pair-open pair-called v 0.75)

(printf "record-inline: ~a/~a passed\n" (- total fails) total)
(exit (if (= fails 0) 0 1))
