(import (chezscheme))
(unless (file-exists? "vendor/irregex/irregex.scm")
  (display "jolt: vendor submodules are missing (vendor/irregex).
" (current-error-port))
  (display "GitHub's 'Source code' release archives don't include submodules.
" (current-error-port))
  (display "Clone the repo instead:
" (current-error-port))
  (display "  git clone --recurse-submodules https://github.com/jolt-lang/jolt.git
" (current-error-port))
  (display "or, in an existing checkout:
" (current-error-port))
  (display "  git submodule update --init --recursive
" (current-error-port))
  (exit 1))

(exit 0)
