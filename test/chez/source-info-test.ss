;; Patch 0015: sources survive without inspector expressions or variable names.
;; Run with stock and patched Chez; stock verifies the no-op adapter fallback.
(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define checks 0)
(define (check name pred)
  (set! checks (+ checks 1))
  (unless pred (error 'source-info-test name)))
(define (annotated text)
  (let* ((bytes (string->utf8 text))
         (sfd (make-source-file-descriptor "source-info-fixture.ss"
                (open-bytevector-input-port bytes))))
    (call-with-values
      (lambda () (get-datum/annotations
                   (transcoded-port (open-bytevector-input-port bytes) (native-transcoder))
                   sfd 0))
      (lambda (ann pos) ann))))
(define fixture
  "(let* ((kept (list 'kept-value)) (worker (lambda (capture) (let ((answer (capture))) (cons kept answer))))) worker)")
(define (source-pair io)
  (let ((so (io 'source-object)))
    (and so (list (source-file-descriptor-path (source-object-sfd so))
                  (source-object-bfp so) (source-object-efp so)))))
(define (frame k)
  (let loop ((io (inspect/object k)))
    (cond ((not (eq? (io 'type) 'continuation)) #f)
          ((equal? ((io 'code) 'name) "worker") io)
          (else (loop (io 'link))))))
(define (variables io)
  (map (lambda (i) (let ((v (io 'ref i))) (cons (v 'name) ((v 'ref) 'value))))
       (iota (io 'length))))

;; A source-hashed gate image built by stock Chez must not masquerade as
;; the stock fallback when this process actually has the patched compiler.
(check "adapter capability matches the compiling Chez"
  (eq? (and (guard (e (#t #f))
              (procedure? (eval 'generate-return-point-source-information))) #t)
       (and (sa-source-info-mode) #t)))

(if (not (sa-source-info-mode))
    (begin
      (check "stock cache generation is unchanged" (equal? (sa-source-info-cache-tag) ""))
      (check "stock compile wrapper preserves parameters"
        (parameterize ((generate-inspector-information #f))
          (sa-source-info-compile (lambda () (not (generate-inspector-information))) #t))))
    (let* ((rp (eval 'generate-return-point-source-information))
           (compile-one
             (lambda (full? sources?)
               (parameterize ((generate-inspector-information full?)
                              (generate-procedure-source-information sources?)
                              (rp sources?) (optimize-level 0))
                 (eval (annotated fixture)))))
           (full (compile-one #t #t))
           (source (compile-one #f #t))
           (bare (compile-one #f #f))
           (capture (lambda () (call/cc (lambda (k) k))))
           (full-frame (frame (cdr (full capture))))
           (source-frame (frame (cdr (source capture))))
           (bare-frame (frame (cdr (bare capture)))))
      (check "all compile modes preserve the frame" (and full-frame source-frame bare-frame))
      (check "return-point source is unchanged"
        (and (source-pair full-frame)
             (equal? (source-pair full-frame) (source-pair source-frame))))
      (check "off mode has no return-point source" (not (source-pair bare-frame)))
      (check "procedure source is unchanged"
        (equal? (source-pair ((inspect/object full) 'code))
                (source-pair ((inspect/object source) 'code))))
      (check "source mode drops the procedure expression"
        (and (((inspect/object full) 'code) 'source)
             (not (((inspect/object source) 'code) 'source))))
      (check "source mode drops the call expression"
        (and (full-frame 'source) (not (source-frame 'source))))
      (check "source mode keeps unnamed frame values"
        (let ((vars (variables source-frame)))
          (and (pair? vars) (for-all (lambda (v) (not (car v))) vars)
               (exists (lambda (v) (equal? (cdr v) '(kept-value))) vars))))
      (check "source mode drops closure free-variable names"
        (and (exists car (variables (inspect/object full)))
             (for-all (lambda (v) (not (car v))) (variables (inspect/object source)))))
      (check "debug compiles retain named locals"
        (sa-source-info-compile (lambda () (and (generate-inspector-information) (not (rp)))) #t))
      (let ((warning-text
              (lambda (full? text)
                (call-with-string-output-port
                  (lambda (out)
                    (parameterize ((generate-inspector-information full?)
                                   (rp (not full?))
                                   (undefined-variable-warnings #t)
                                   (enable-error-source-expression #t)
                                   (optimize-level 2))
                      (with-exception-handler
                        (lambda (c)
                          (if (warning? c) (display-condition c out) (raise c)))
                        (lambda () (compile (annotated text))))))))))
        (for-each
          (lambda (text)
            (let ((full (warning-text #t text)) (source (warning-text #f text)))
              (unless (and (> (string-length full) 0) (string=? full source))
                (printf "warning fixture ~s: full=~s source=~s\n" text full source))
              (check "source mode preserves compiler warnings"
                (and (> (string-length full) 0) (string=? full source)))))
          '("(lambda () (car))"
            "(lambda () (let () (define x x) x))"
            "(lambda () (format \"~\"))")))))
(printf "source-info: ~a checks passed\n" checks)
