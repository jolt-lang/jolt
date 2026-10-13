(import (chezscheme))
(when (file-exists? "nope-xyz") (display "g" (current-output-port)) (display "h" (current-output-port)) (display "i" (current-output-port)) (display "j" (current-output-port)) (display "k" (current-output-port)) (display "l" (current-output-port)) (exit 1))
(exit 0)
