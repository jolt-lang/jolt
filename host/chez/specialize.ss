;; specialize.ss -- runtime specialization of hot protocol call sites (#1861).
;;
;; A protocol call on a record goes through its receiver's dispatch vector
;; (protocols.ss, rt.ss jrdesc-vtable-ref) and then calls the impl it finds. The
;; JVM inlines a monomorphic interface call behind a class check; jolt pays for
;; the call, and the impl checks its receiver again. tools.reader reads each
;; char through three of these in a row.
;;
;; So `jolt run` code counts calls instead. The back end numbers every protocol
;; site of a top-level fn literal (jolt.passes.inline number-psites) and gives
;; it a counter box (backend_scheme psite-ticked); every 1024th call through the
;; dispatch vector samples the impl and the receiver's descriptor here, against
;; the literal that holds the site -- its OWNER, named by its fn registry name
;; (fn-form-registry.ss). Once a site's last samples agree, the owner is
;; recompiled from its registered source with that impl's body copied into the
;; site behind a guard (jolt.passes.inline specialize-sites; the compile is in
;; compile-eval.ss), and the new procedure replaces the owner's in each dispatch
;; vector that held it.
;;
;; The guard is the dispatch the site makes anyway: the var still holds the
;; protocol fn, the receiver's descriptor is the one sampled, and its dispatch
;; vector still holds the sampled impl. Each holds or the site makes its call,
;; so a redefined type (a new descriptor), method (a new impl in the vector) or
;; var (another fn) takes the call. The recompiled owner is equivalent to the
;; one it replaces, so it goes where that one was and nowhere else; a vector
;; that no longer holds the old one (the method was redefined meanwhile) keeps
;; what it has.
;;
;; The specialized code counts too, under the same owner, so a site inside a
;; copied body ("2/0") can be inlined by the next recompile, and a guard that
;; keeps failing is sampled at its fallback call. An owner is recompiled at most
;; spec-max-gens times. JOLT_SPECIALIZE=0 turns the sampling off;
;; JOLT_SPEC_DEBUG=1 reports what is specialized and why one is not.

(define spec-on (not (equal? (getenv "JOLT_SPECIALIZE") "0")))
(define spec-debug? (equal? (getenv "JOLT_SPEC_DEBUG") "1"))
(define (set-specialize-flag! on) (set! spec-on on))
;; samples that must agree before a site is specialized, and how many each site
;; keeps
(define spec-samples-to-trip 4)
(define spec-samples-kept 8)
(define spec-max-gens 4)

;; The per-call half, inlined at each site: count down, and sample on every
;; 1024th call. The box is the site's alone; a lost update between threads only
;; moves a sample.
(define-syntax jolt-psite-tick!
  (syntax-rules ()
    ((_ b0 f d)
     (let* ((b b0) (n (sa-ufx- (unbox b) 1)))
       (set-box! b n)
       (when (sa-ufx=? 0 (sa-ufxand n 1023)) (jolt-psite-sample! b f d))))))

;; box -> (owner . path). Weak: a box lives as long as the code holding it.
;; Under a lock of its own, never held across anything else: code loading on
;; one thread makes boxes while another may be in a recompile.
(define psite-info (make-weak-eq-hashtable))
(define psite-mu (make-mutex))
;; owner name -> spec-owner
(define spec-owners (make-hashtable string-hash string=?))
;; One specialization at a time, and never one inside another: the compile runs
;; user macros, which can reach a sampling site.
(define spec-mu (make-mutex))
(define spec-busy #f)

(define-record-type spec-owner
  (fields
    ;; path -> list of (impl . desc), newest first
    (immutable profile)
    ;; recompiles installed so far
    (mutable gens)
    ;; the procedure the dispatch vectors hold for it now, #f until the first
    (mutable current)
    ;; 'open, or 'done once it cannot or need not be recompiled again
    (mutable state)
    ;; path -> (impl . desc) the installed code inlined there
    (mutable spliced)
    ;; samples taken since that code was installed. A site it inlined samples
    ;; only from its fallback, so this stays 0 while every guard holds.
    (mutable since))
  (nongenerative jolt-spec-owner-v1))

(define (spec-owner-of owner)
  (or (hashtable-ref spec-owners owner #f)
      (let ((o (make-spec-owner (make-hashtable string-hash string=?) 0 #f 'open
                                (make-hashtable string-hash string=?) 0)))
        (hashtable-set! spec-owners owner o)
        o)))

(define (jolt-psite-make owner path)
  (let ((b (box 0)))
    (jolt-with-mutex psite-mu (hashtable-set! psite-info b (cons owner path)))
    b))

;; The specializer's constants -- a sampled descriptor, an impl -- by index, for
;; the guards a recompile emits ((jolt-spec-const N), hoisted into the owner's
;; pool when it loads).
(define spec-consts (make-vector 64 #f))
(define spec-const-count 0)
(define spec-const-index (make-eq-hashtable))
(define (spec-const-id! x)
  (or (hashtable-ref spec-const-index x #f)
      (let ((i spec-const-count))
        (when (fx= i (vector-length spec-consts))
          (let ((v (make-vector (fx* 2 i) #f)))
            (do ((j 0 (fx+ j 1))) ((fx= j i)) (vector-set! v j (vector-ref spec-consts j)))
            (set! spec-consts v)))
        (vector-set! spec-consts i x)
        (set! spec-const-count (fx+ i 1))
        (hashtable-set! spec-const-index x i)
        i)))
(define (jolt-spec-const i) (vector-ref spec-consts i))

;; A recompiled procedure -> the owner it was compiled for, so a caller that
;; samples it inlines its owner's source.
(define spec-proc-owner (make-weak-eq-hashtable))
(define (spec-impl-owner f)
  (or (hashtable-ref spec-proc-owner f #f) (sa-procedure-code-name f)))
;; The version of impl F the dispatch vectors hold now: a sample may predate the
;; recompile of F's owner, which replaced F where it was installed.
(define (spec-current-impl f)
  (let* ((y (spec-impl-owner f))
         (o (and y (hashtable-ref spec-owners y #f))))
    (or (and o (spec-owner-current o)) f)))

(define (spec-log fmt . args)
  (when spec-debug?
    (apply fprintf (current-error-port) (string-append "spec: " fmt "\n") args)))

(define (spec-take l n)
  (if (or (null? l) (fx= n 0)) '() (cons (car l) (spec-take (cdr l) (fx- n 1)))))

;; The (impl . desc) a site's samples agree on, newest MIN of them, else #f.
(define (spec-agreed samples min)
  (and (fx>= (length samples) min)
       (let ((s (car samples)))
         (and (let loop ((l (cdr (spec-take samples min))))
                (or (null? l)
                    (and (eq? (caar l) (car s)) (eq? (cdar l) (cdr s)) (loop (cdr l)))))
              s))))

(define (jolt-psite-sample! b f d)
  (when (and spec-on (not spec-busy) (jolt-lock! spec-mu #f))
    (dynamic-wind
      void
      (lambda ()
        (unless spec-busy
          (let ((info (jolt-with-mutex psite-mu (hashtable-ref psite-info b #f))))
            (when info
              (let ((owner (car info)) (path (cdr info)))
                (let ((o (spec-owner-of owner)))
                  (when (eq? (spec-owner-state o) 'open)
                    (let ((l (cons (cons f d)
                                   (spec-take (hashtable-ref (spec-owner-profile o) path '())
                                              (fx- spec-samples-kept 1)))))
                      (hashtable-set! (spec-owner-profile o) path l)
                      (spec-owner-since-set! o (fx+ 1 (spec-owner-since o)))
                      (let ((s (spec-agreed l spec-samples-to-trip))
                            (done (hashtable-ref (spec-owner-spliced o) path #f)))
                        (when (and s (not (and done (eq? (car done) (car s)) (eq? (cdr done) (cdr s)))))
                          (set! spec-busy #t)
                          (dynamic-wind
                            void
                            (lambda () (spec-run! owner o))
                            (lambda () (set! spec-busy #f)))))))))))))
      (lambda () (jolt-unlock! spec-mu)))))

;; Where the owner's procedure is installed: each (tag proto method) whose
;; registered impl is the owner's literal.
(define (spec-install-targets owner)
  (jolt-with-mutex rec-tbl-mu
    (let ((acc '()))
      (let-values (((tags tis) (hashtable-entries type-registry)))
        (vector-for-each
          (lambda (tag ti)
            (let-values (((protos pis) (hashtable-entries ti)))
              (vector-for-each
                (lambda (proto pi)
                  (let-values (((ms fs) (hashtable-entries pi)))
                    (vector-for-each
                      (lambda (m f)
                        (when (and (procedure? f) (equal? (sa-procedure-code-name f) owner))
                          (set! acc (cons (list tag proto m f) acc))))
                      ms fs)))
                protos pis)))
          tags tis))
      acc)))

;; Swap P in for the owner's procedure in each dispatch vector still holding it.
(define (spec-install! owner o p targets)
  (let ((cur (spec-owner-current o)) (n 0))
    (for-each
      (lambda (t)
        (let ((desc (hashtable-ref chez-tag-desc (car t) #f))
              (k (intern-pm-key (cadr t) (caddr t))))
          (when desc
            (jolt-with-mutex rec-tbl-mu
              (let ((pt (jrdesc-ptable desc)))
                (when (and (vector? pt) (fx< k (vector-length pt))
                           (let ((x (vector-ref pt k))) (or (eq? x (cadddr t)) (and cur (eq? x cur)))))
                  (let ((new (vector-copy pt)))
                    (vector-set! new k p)
                    (when jolt-mt? (memory-order-release))
                    (jrdesc-ptable-set! desc new)
                    (set! n (fx+ n 1))))))
            (protocol-impl-hook (cadr t) (caddr t) p))))
      targets)
    (hashtable-set! spec-proc-owner p owner)
    (spec-owner-current-set! o p)
    n))

;; Set by compile-eval.ss: (owner o) -> (procedure . spliced-table) or #f.
(define spec-compile-hook (lambda (owner o) #f))

(define (spec-run! owner o) (spec-run/callees! owner o '()))

;; Callees first. An owner's hot site usually calls another owner whose own
;; sites are as hot -- each layer of a reader chain is one -- and recompiling
;; the caller first would inline the callee's procedure of the moment, which the
;; callee's own recompile then replaces, failing the caller's guard until it is
;; recompiled again. So a callee with samples of its own is recompiled first,
;; and the caller inlines that.
(define (spec-run/callees! owner o seen)
  (let ((seen (cons owner seen)))
    (when (fx< (length seen) 4)
      (let-values (((paths ls) (hashtable-entries (spec-owner-profile o))))
        (vector-for-each
          (lambda (l)
            (let* ((s (spec-agreed l (fxmin (length l) spec-samples-to-trip)))
                   (y (and s (spec-impl-owner (car s))))
                   (yo (and y (not (member y seen)) (hashtable-ref spec-owners y #f))))
              (when (and yo (eq? (spec-owner-state yo) 'open) (fx= 0 (spec-owner-gens yo))
                         (fx> (hashtable-size (spec-owner-profile yo)) 0))
                (spec-run/callees! y yo seen))))
          ls)))
    (spec-run1! owner o)))

(define (spec-run1! owner o)
  (guard (e (#t (spec-log "~a: not specialized: ~a" owner
                          (if (condition? e)
                              (with-output-to-string (lambda () (display-condition e)))
                              e))
                (spec-owner-state-set! o 'done)))
    (let ((targets (spec-install-targets owner)))
      (cond
        ((null? targets)
         (spec-log "~a: no dispatch vector holds it" owner)
         (spec-owner-state-set! o 'done))
        ((fx>= (spec-owner-gens o) spec-max-gens)
         (spec-log "~a: recompiled ~a times already" owner spec-max-gens)
         (spec-owner-state-set! o 'done))
        (else
         (let ((r (spec-compile-hook owner o)))
           (if (not r)
               (spec-owner-state-set! o 'done)
               (let ((n (spec-install! owner o (car r) targets)))
                 (spec-owner-gens-set! o (fx+ 1 (spec-owner-gens o)))
                 (spec-owner-spliced-set! o (cdr r))
                 (spec-owner-since-set! o 0)
                 (spec-log "~a: generation ~a, ~a site(s) inlined, installed in ~a vector(s)"
                           owner (spec-owner-gens o) (hashtable-size (cdr r)) n)))))))))
