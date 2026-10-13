(import (chezscheme))
(call-with-input-file "host/chez/scheme-adapter-runtime.ss"
  (lambda (in)
    (let loop ((i 0))
      (let ((x (read in)))
        (unless (eof-object? x)
          (call-with-output-file "stage2.txt"
            (lambda (p) (write (list 'before i (if (pair? x) (list (car x) (if (pair? (cdr x)) (let ((h (cadr x))) (if (pair? h) (car h) h)) '?)) x)) p) (newline p))
            'append)
          (eval x)
          (loop (+ i 1)))))))
(call-with-output-file "stage2.txt" (lambda (p) (display "done\n" p)) 'append)
