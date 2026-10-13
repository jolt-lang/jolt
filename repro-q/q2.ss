(import (chezscheme))
(when (file-exists? "nope-xyz") (display "a" (current-error-port)))
(exit 0)
