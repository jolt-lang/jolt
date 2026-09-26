;; cli-devcache.ss — dev-boot-cache CLI entry. bin/jolt execs this instead of
;; cli.ss when target/dev/flat.so is fresh (see `make devboot`). The flat image
;; contains the full runtime manifest (cli-core, loader, ffi included), so the
;; only things that belong here are the image load, GC tuning, and the shared
;; CLI tail. Do not add forms here that cli.ss would also need — put them in
;; cli-tail.ss.
(load "target/dev/flat.so")
(when (let ((m (getenv "JOLT_DEVCACHE"))) (and m (not (string=? m ""))))
  (display "devcache: using target/dev/flat.so\n" (current-error-port)))
;; The collector policy, as the binary's launcher installs it (rt.ss).
(jolt-install-gc-policy!)
(load "host/chez/cli-tail.ss")
