;; The runtime's own child processes: a copy of jolt doing work for this one
;; (the AOT cache's worker, `jolt build`'s compile workers), and the pid probe
;; the AOT worker watches its parent with.
;;
;; They go through java/process.ss, which spawns the program directly on every
;; platform (posix_spawn, CreateProcessW) with no /bin/sh in between. That file
;; loads on first reference, and this one asks for it by name. Only a jolt
;; binary runs these paths (loader.ss jolt-standalone-binary, build.ss
;; bld-self-exe), and a jolt binary carries every lazy file; anywhere the file
;; is absent the spawn answers #f and the caller compiles in process. That is
;; why this file, and only this file, is allowed to name process.ss
;; (tools/lazy-shims-gen.ss lazy-shims-ref-allow / lazy-shims-name-allow).

(define (worker-spawn-load!)
  (lazy-shims-class! "java.lang.ProcessBuilder"))

;; ARGV run directly, stdin the null device, stdout and stderr appended to LOG.
;; -> the Process, or #f when it could not be started.
(define (jolt-spawn-worker argv log)
  (guard (e (#t #f))
    (worker-spawn-load!)
    (proc-spawn-worker argv log)))

;; Block until P exits. -> its exit status, or #f.
(define (jolt-wait-worker p)
  (guard (e (#t #f)) (proc-wait-blocking p)))

;; Is PID a live process? kill(pid, 0) on POSIX, OpenProcess on Windows
;; (process.ss proc-pid-alive?). A failed probe reads as gone.
(define (jolt-pid-alive? pid)
  (guard (e (#t #f))
    (worker-spawn-load!)
    (proc-pid-alive? pid)))
