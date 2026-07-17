;; cli-devcache.ss — dev-boot-cache CLI entry. bin/joltc execs this instead of
;; cli.ss when target/dev/flat.so is fresh (see `make devboot`). The flat image
;; contains the full runtime manifest (cli-core, loader, ffi included), so the
;; only things that belong here are the image load, GC tuning, and the shared
;; CLI tail. Do not add forms here that cli.ss would also need — put them in
;; cli-tail.ss.
(load "target/dev/flat.so")
(when (let ((m (getenv "JOLT_DEVCACHE"))) (and m (not (string=? m ""))))
  (display "devcache: using target/dev/flat.so\n" (current-error-port)))
;; GC tuning (same as the binary's launcher).
(collect-trip-bytes
  (let ((trip (getenv "JOLT_GC_TRIP_BYTES"))
        (default (* 16 1024 1024)))
    (if trip (or (string->number trip) default) default)))
(load "host/chez/cli-tail.ss")
