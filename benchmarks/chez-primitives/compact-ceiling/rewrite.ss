;; rewrite.ss -- offline source-to-source rewriter for the compact-code ceiling
;; experiment (pellet jolt-2). It reads the .scm files a cold AOT cache holds and
;; writes them back in the shape Clojure-friendly Chez primitives would allow.
;; The output is for compiling, not running: the primitives are opaque stubs.
;;
;;   chez --script rewrite.ss VARIANT OUTDIR FILE.scm...
;;
;; VARIANT is one of
;;   R  read and write back, no rewrite (control: the round trip drops the
;;      #|L<n>|# comments and re-spaces the text, nothing else)
;;   A  tracing:   (jolt-site-call (lambda () X)) and the inline site restore
;;                 -> (clj-site N X); the per-fn jolt-site-call binding dropped;
;;                 (jolt-site! '(fqn . line)) -> (clj-site! line);
;;                 jolt-register-callsite! calls -> one quoted table per namespace
;;   B  constants: keyword/symbol/vector/list/map/set/metadata construction with
;;                 constant parts -> quoted data, _kc$ constant bindings
;;                 substituted away; image-register-fn-form! rows and the
;;                 interned names -> one (intern-constants! '#(...) '#(...)) per
;;                 namespace
;;   C  calls:     _kc$ (jolt-var ns name) bindings -> 'ns/name at each use;
;;                 (jolt-invoke-tN (var-cell-deref 'v) a ...) and
;;                 (jolt-invoke-tN (var-deref "ns" "name") a ...) -> (clj-callN 'v a ...);
;;                 a let* that binds every argument of a jolt-invoke to an _a$
;;                 temporary is collapsed into the call
;;   D  A, then B, then C
;;   AB, AC, BC  the pairs, in the same order
;;
;; N in (clj-site N X) is the site's ordinal in its file, a stand-in for the
;; line: the line is in a #|L|# comment that `read` drops, and any fixnum
;; costs Chez the same to compile. Counts of every rewrite go to stdout.
(import (chezscheme))

(define args (cdr (command-line)))
(define variant (string->symbol (car args)))
(define outdir (cadr args))
(define files (cddr args))
(define passes
  (case variant
    ((R) '()) ((A) '(a)) ((B) '(b)) ((C) '(c)) ((D) '(a b c))
    ((AB) '(a b)) ((AC) '(a c)) ((BC) '(b c))
    (else (error 'rewrite "variant must be R A B C D AB AC or BC" variant))))

;; ---- stats -------------------------------------------------------------------
(define stats (make-eq-hashtable))
(define (bump! k) (hashtable-update! stats k add1 0))

;; ---- helpers -----------------------------------------------------------------
(define (sym-prefix? x p)
  (and (symbol? x)
       (let ((s (symbol->string x)) (n (string-length p)))
         (and (>= (string-length s) n) (string=? (substring s 0 n) p)))))
(define (kc-sym? x) (sym-prefix? x "_kc$"))
(define (tmp-sym? x) (sym-prefix? x "_a$"))
(define (str-or-f? x) (or (string? x) (eq? x #f)))
(define (literal? x)
  (or (number? x) (string? x) (char? x) (boolean? x) (bytevector? x)))
(define (quoted? x) (and (pair? x) (eq? (car x) 'quote) (pair? (cdr x)) (null? (cddr x))))
(define (const? x) (or (literal? x) (quoted? x) (eq? x 'jolt-nil)))
(define (const-datum x)
  (cond ((quoted? x) (cadr x)) ((eq? x 'jolt-nil) '%nil) (else x)))
(define (q d) (list 'quote d))
(define (qualified ns name) (if ns (string-append ns "/" name) name))
(define (occurs? s x)
  (cond ((eq? x s) #t)
        ((pair? x) (and (not (eq? (car x) 'quote)) (or (occurs? s (car x)) (occurs? s (cdr x)))))
        ((vector? x) #f)
        (else #f)))

;; A rewrite that deletes a statement returns DROP; the enclosing body splices it out.
(define DROP (list '%drop))
(define (drop? x) (eq? x DROP))
(define (clean-seq xs)
  ;; drop DROPs from a body; a body that ends in one ends in (void) instead
  (let ((kept (filter (lambda (e) (not (drop? e))) xs)))
    (cond ((null? kept) (list '(void)))
          ((drop? (car (last-pair xs))) (append kept (list '(void))))
          (else kept))))
(define (body-start x)
  ;; index of the first body expression of a binding/sequencing form, or #f
  (case (car x)
    ((begin) 1)
    ((lambda let* letrec letrec*) 2)
    ((let) (if (and (pair? (cdr x)) (symbol? (cadr x))) 3 2))
    (else #f)))
(define (clean x)
  (if (not (and (list? x) (memp drop? x)))
      x
      (let ((k (body-start x)))
        (if k
            (append (list-head x k) (clean-seq (list-tail x k)))
            (begin (bump! 'drop-in-arg)
                   (map (lambda (e) (if (drop? e) '(void) e)) x))))))
(define (clean-case-lambda x)
  (cons (car x)
        (map (lambda (cl)
               (if (and (pair? cl) (list? cl) (memp drop? (cdr cl)))
                   (cons (car cl) (clean-seq (cdr cl)))
                   cl))
             (cdr x))))
(define (body->expr body)
  (if (and (pair? body) (null? (cdr body))) (car body) (cons* 'let '() body)))

;; ---- per-file state ----------------------------------------------------------
(define subst (make-eq-hashtable))   ; _kc$ symbol -> replacement expression
(define site-n 0)
(define callsites '())               ; A: register-callsite! rows
(define fn-forms '())                ; B: image-register-fn-form! rows
(define interned (make-hashtable equal-hash equal?)) ; B: names to intern
(define interned-order '())
(define (intern! d)
  (unless (hashtable-contains? interned d)
    (hashtable-set! interned d #t)
    (set! interned-order (cons d interned-order))))
(define (reset-file!)
  (hashtable-clear! subst)
  (set! site-n 0) (set! callsites '()) (set! fn-forms '())
  (hashtable-clear! interned) (set! interned-order '()))
(define (next-site!) (set! site-n (+ site-n 1)) site-n)

;; ---- pass A: tracing ---------------------------------------------------------
;; (let ((S (virtual-register 2)) (O (virtual-register 12)))
;;   (if (pair? S) (set-virtual-register! 12 S)) (set-virtual-register! 2 0)
;;   (let ((R X)) (set-virtual-register! 2 S) (set-virtual-register! 12 O) R))  -> X
(define (inline-restore-body x)
  (and (list? x) (= (length x) 5) (eq? (car x) 'let)
       (equal? (map cadr (cadr x)) '((virtual-register 2) (virtual-register 12)))
       (let ((inner (list-ref x 4)))
         (and (list? inner) (= (length inner) 5) (eq? (car inner) 'let)
              (= (length (cadr inner)) 1)
              (cadr (car (cadr inner)))))))
(define (site-call-thunk-body x)
  (and (list? x) (= (length x) 2) (eq? (car x) 'jolt-site-call)
       (let ((th (cadr x)))
         (and (list? th) (= (length th) 3) (eq? (car th) 'lambda) (null? (cadr th))
              (caddr th)))))
(define (site-helper-binding? x)
  (and (list? x) (eq? (car x) 'let) (list? (cadr x)) (= (length (cadr x)) 1)
       (eq? (car (car (cadr x))) 'jolt-site-call)))

(define (pre-a x)
  (cond
    ((site-call-thunk-body x)
     => (lambda (b) (bump! 'a-site-call) (list 'clj-site (next-site!) (rw b))))
    ((inline-restore-body x)
     => (lambda (b) (bump! 'a-inline-restore) (list 'clj-site (next-site!) (rw b))))
    ((site-helper-binding? x)
     (bump! 'a-helper-binding)
     (rw (body->expr (cddr x))))
    ((and (eq? (car x) 'jolt-site!) (= (length x) 2) (quoted? (cadr x)) (pair? (cadr (cadr x))))
     (bump! 'a-tail-marker)
     (list 'clj-site! (cdr (cadr (cadr x)))))
    ((and (eq? (car x) 'jolt-register-callsite!) (= (length x) 5) (for-all literal? (cdr x)))
     (bump! 'a-register-callsite)
     (set! callsites (cons (list->vector (cdr x)) callsites))
     DROP)
    (else #f)))

;; ---- pass B: constants -------------------------------------------------------
(define (kw-datum ns name) (string->symbol (string-append ":" (qualified ns name))))
(define (sym-datum ns name) (string->symbol (qualified ns name)))
(define (pre-b x)
  (cond
    ((and (memq (car x) '(keyword jolt-symbol)) (= (length x) 3)
          (str-or-f? (cadr x)) (string? (caddr x)))
     (bump! (if (eq? (car x) 'keyword) 'b-keyword 'b-symbol))
     (let ((d ((if (eq? (car x) 'keyword) kw-datum sym-datum) (cadr x) (caddr x))))
       (intern! d)
       (q d)))
    ((and (eq? (car x) 'image-register-fn-form!) (= (length x) 5)
          (string? (cadr x))
          (let ((s (caddr x))) (and (list? s) (= (length s) 2) (eq? (car s) 'image-fn-form-src) (string? (cadr s))))
          (str-or-f? (cadddr x))
          (let ((c (list-ref x 4))) (and (list? c) (eq? (car c) 'jolt-vector) (for-all string? (cdr c)))))
     (bump! 'b-fn-form)
     (set! fn-forms (cons (vector (cadr x) (cadr (caddr x)) (cadddr x) (list->vector (cdr (list-ref x 4))))
                          fn-forms))
     DROP)
    (else #f)))
(define (post-b x)
  (define (all-const? xs) (for-all const? xs))
  (case (car x)
    ((jolt-vector)
     (if (all-const? (cdr x)) (begin (bump! 'b-vector) (q (list->vector (map const-datum (cdr x))))) x))
    ((jolt-list)
     (if (all-const? (cdr x)) (begin (bump! 'b-list) (q (map const-datum (cdr x)))) x))
    ((jolt-hash-map)
     (if (all-const? (cdr x)) (begin (bump! 'b-map) (q (list->vector (cons '%map (map const-datum (cdr x)))))) x))
    ((jolt-hash-set)
     (if (all-const? (cdr x)) (begin (bump! 'b-set) (q (list->vector (cons '%set (map const-datum (cdr x)))))) x))
    ((amap-slots->pmap)
     (let ((v (and (= (length x) 2) (pair? (cadr x)) (eq? (car (cadr x)) 'vector) (cdr (cadr x)))))
       (if (and v (all-const? v))
           (begin (bump! 'b-map) (q (list->vector (cons '%map (map const-datum v)))))
           x)))
    ((jolt-symbol/meta)
     (if (and (= (length x) 4) (str-or-f? (cadr x)) (string? (caddr x)) (const? (cadddr x)))
         (begin (bump! 'b-symbol)
                (let ((d (sym-datum (cadr x) (caddr x)))) (intern! d)
                  (q (vector '%meta d (const-datum (cadddr x))))))
         x))
    (else x)))
(define (bind-b v rhs)
  ;; a constant _kc$ binding is substituted at its uses and dropped
  (and (kc-sym? v) (const? rhs)
       (begin (bump! 'b-kc-binding) (hashtable-set! subst v rhs) #t)))

;; ---- pass C: calls -----------------------------------------------------------
(define invoke-heads
  '(jolt-invoke jolt-invoke-t0 jolt-invoke-t1 jolt-invoke-t2 jolt-invoke-t3
    jolt-invoke-t4 jolt-invoke-t5 jolt-invoke-t6 jolt-invoke-t7 jolt-invoke-t8))
(define (call-stub n) (string->symbol (string-append "clj-call" (number->string n))))
(define (pre-c x) #f)
(define (callee-name f)
  ;; 'v for a var callee in either emitted spelling, else #f
  (cond ((and (list? f) (= (length f) 2) (eq? (car f) 'var-cell-deref) (quoted? (cadr f))
              (symbol? (cadr (cadr f))))
         (cadr f))
        ((and (list? f) (= (length f) 3) (eq? (car f) 'var-deref) (string? (cadr f)) (string? (caddr f)))
         (q (sym-datum (cadr f) (caddr f))))
        (else #f)))
(define (convert-call c)
  (if (and (pair? c) (memq (car c) invoke-heads) (pair? (cdr c)))
      (let ((v (callee-name (cadr c))))
        (if v
            (begin (bump! 'c-var-call) (cons* (call-stub (length (cddr c))) v (cddr c)))
            c))
      c))
;; the call under a site wrapper (any spelling A leaves or produces), with a
;; procedure that puts a new call back under the same wrapper
(define (peel b)
  (cond
    ((site-call-thunk-body b)
     => (lambda (c) (values c (lambda (c2) (list 'jolt-site-call (list 'lambda '() c2))))))
    ((and (list? b) (= (length b) 3) (eq? (car b) 'clj-site))
     (values (caddr b) (lambda (c2) (list 'clj-site (cadr b) c2))))
    ((inline-restore-body b)
     => (lambda (c)
          (values c (lambda (c2)
                      (let* ((inner (list-ref b 4))
                             (r (car (car (cadr inner)))))
                        (append (list-head b 4)
                                (list (cons* 'let (list (list r c2)) (cddr inner)))))))))
    (else (values b (lambda (c2) c2)))))
(define (collapse-temps x)
  ;; (let* ((_a$1 e1) ... (_a$n en)) [wrapper] (jolt-invoke* _a$1 ... _a$n)) -> call on e1..en
  (if (and (eq? (car x) 'let*) (list? x) (= (length x) 3) (list? (cadr x)) (pair? (cadr x))
           (for-all (lambda (b) (and (list? b) (= (length b) 2) (tmp-sym? (car b)))) (cadr x)))
      (let ((ts (map car (cadr x))) (es (map cadr (cadr x))))
        (let-values (((c rebuild) (peel (caddr x))))
          (if (and (list? c) (memq (car c) invoke-heads) (equal? (cdr c) ts)
                   ;; no temporary is read by any initializer
                   (for-all (lambda (t) (for-all (lambda (e) (not (occurs? t e))) es)) ts))
              (begin (bump! 'c-temps-collapsed)
                     (rebuild (convert-call (cons (car c) es))))
              x)))
      x))
(define (post-c x) (collapse-temps (convert-call x)))
(define (bind-c v rhs)
  (and (kc-sym? v) (list? rhs) (= (length rhs) 3) (eq? (car rhs) 'jolt-var)
       (string? (cadr rhs)) (string? (caddr rhs))
       (begin (bump! 'c-var-binding) (hashtable-set! subst v (q (sym-datum (cadr rhs) (caddr rhs)))) #t)))

;; ---- the walk ----------------------------------------------------------------
(define pass #f)
(define (pre x) (case pass ((a) (pre-a x)) ((b) (pre-b x)) ((c) (pre-c x))))
(define (post x) (case pass ((b) (post-b x)) ((c) (post-c x)) (else x)))
(define (bind-drop? v rhs) (case pass ((b) (bind-b v rhs)) ((c) (bind-c v rhs)) (else #f)))

(define (rw-list x)
  ;; map rw over a possibly improper list
  (cond ((pair? x) (cons (rw (car x)) (rw-list (cdr x))))
        ((null? x) '())
        (else (rw x))))
(define (rw-let x)
  ;; (let|let* BINDINGS BODY...): RHS first, so a dropped binding is in subst
  ;; before the body (and later RHS of a let*) are walked; _kc$ names are unique
  ;; per file, so one flat table is enough
  (let* ((kept (fold-left
                 (lambda (acc b)
                   (if (and (list? b) (= (length b) 2))
                       (let ((rhs (rw (cadr b))))
                         (if (bind-drop? (car b) rhs) acc (cons (list (car b) rhs) acc)))
                       (cons (rw-list b) acc)))
                 '() (cadr x)))
         (body (map rw (cddr x))))
    (if (and (null? kept) (pair? (cadr x)))
        (body->expr body)
        (cons* (car x) (reverse kept) body))))
(define (rw x)
  (cond
    ((symbol? x) (hashtable-ref subst x x))
    ((not (pair? x)) x)
    ((eq? (car x) 'quote) x)
    ((not (list? x)) (rw-list x))
    ((pre x) => (lambda (y) y))
    (else
     (let ((y (cond ((and (memq (car x) '(let let*)) (pair? (cdr x)) (list? (cadr x)))
                     (rw-let x))
                    ((eq? (car x) 'case-lambda) (clean-case-lambda (rw-list x)))
                    (else (rw-list x)))))
       (if (pair? y) (post (clean y)) y)))))

;; ---- files -------------------------------------------------------------------
(define (read-forms f)
  (let ((p (open-input-file f)))
    (let loop ((acc '()))
      (let ((x (read p)))
        (if (eof-object? x) (begin (close-port p) (reverse acc)) (loop (cons x acc)))))))
(define (basename f)
  (let loop ((i (- (string-length f) 1)))
    (cond ((< i 0) f) ((char=? (string-ref f i) #\/) (substring f (+ i 1) (string-length f))) (else (loop (- i 1))))))
(define (rewrite-file f)
  (reset-file!)
  (let* ((forms (read-forms f))
         (forms (fold-left (lambda (fs p)
                             (set! pass p)
                             (filter (lambda (x) (not (drop? x))) (map rw fs)))
                           forms passes))
         ;; per-namespace tables, first, as a loader would want them
         (forms (if (memq 'b passes)
                    (cons (list 'intern-constants!
                                (q (list->vector (reverse interned-order)))
                                (q (list->vector (reverse fn-forms))))
                          forms)
                    forms))
         (forms (if (memq 'a passes)
                    (cons (list 'clj-register-callsites! (q (list->vector (reverse callsites)))) forms)
                    forms)))
    (let ((out (open-output-file (string-append outdir "/" (basename f)) 'replace)))
      (parameterize ((print-graph #f) (print-gensym #t))
        (for-each (lambda (x) (write x out) (newline out)) forms))
      (close-port out))))

(for-each rewrite-file files)
(let-values (((ks vs) (hashtable-entries stats)))
  (for-each (lambda (p) (printf "~a ~a\n" (car p) (cdr p)))
            (list-sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b))))
                       (map cons (vector->list ks) (vector->list vs)))))
