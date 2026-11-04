;; cli-devcache.ss — dev-boot-cache CLI entry. bin/jolt execs this instead of
;; cli.ss when target/dev/flat.so is fresh (see `make devboot`). The flat image
;; contains the full runtime manifest (cli-core, loader, ffi included), so the
;; only things that belong here are the image load, GC tuning, and the shared
;; CLI tail. Do not add forms here that cli.ss would also need — put them in
;; cli-tail.ss.
;;
;; bin/jolt decided the image was there and fresh a moment ago; it may be gone by
;; now (a `make devboot` on Windows, where a rename cannot replace a file, or a
;; `make clean`). Opening it first and taking source mode when the open fails
;; means a vanished image costs a slow start, not a run that dies printing
;; nothing ("failed for target/dev/flat.so: no such file or directory"). Once
;; open, the port keeps the image readable whatever happens to its name.
(define devcache-image-port
  (guard (e (#t #f)) (open-file-input-port "target/dev/flat.so")))
(if devcache-image-port
    (begin
      (load-compiled-from-port devcache-image-port)
      (close-port devcache-image-port)
      (when (let ((m (getenv "JOLT_DEVCACHE"))) (and m (not (string=? m ""))))
        (display "devcache: using target/dev/flat.so\n" (current-error-port)))
      ;; The collector policy, as the binary's launcher installs it (rt.ss).
      (jolt-install-gc-policy!)
      (load "host/chez/cli-tail.ss"))
    (load "host/chez/cli.ss"))
