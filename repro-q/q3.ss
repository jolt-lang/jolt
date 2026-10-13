(import (chezscheme))
(when (file-exists? "nope-xyz") (display "a
b
c
d
e
f
" (current-error-port)) (display "g
" (current-error-port)) (display "h
" (current-error-port)) (display "i
" (current-error-port)) (display "j
" (current-error-port)) (display "k
" (current-error-port)) (exit 1))
(exit 0)
