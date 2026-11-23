;; The runtime's own child processes, run directly with no /bin/sh in between:
;; a copy of jolt doing work for this one (the AOT cache's worker, `jolt build`'s
;; compile workers), the pid probe the AOT worker watches its parent with, and
;; the build's fixed-argv tools (brew, pkg-config, nm, otool) and PATH lookups.
;;
;; They go through java/process.ss, which spawns on every platform (posix_spawn,
;; CreateProcessW). That file loads on first reference, and this one asks for it
;; by name. Only a jolt binary or the build drivers reach these paths, and both
;; carry every lazy file; anywhere the file is absent a spawn answers #f and the
;; caller falls back (a worker's units compile in process, a probe finds
;; nothing). That is why this file, and only this file, is allowed to name
;; process.ss (tools/lazy-shims-gen.ss lazy-shims-ref-allow / -name-allow).
;;
;; build.ss loads this file itself, so every driver that loads build.ss has it.

(define (spawn-load!)
  (lazy-shims-class! "java.lang.ProcessBuilder"))

;; ARGV run directly, stdin the null device, stdout and stderr written to LOG
;; (truncated first). -> the Process, or #f when it could not be started.
(define (jolt-spawn-worker argv log)
  (guard (e (#t #f))
    (spawn-load!)
    (proc-spawn-worker argv log #f)))

;; jolt-spawn-worker for a worker that outlives this process (the AOT cache's):
;; in a process group of its own, so the terminal's ^C, which ends the run, does
;; not end it before it has cleaned up.
(define (jolt-spawn-detached-worker argv log)
  (guard (e (#t #f))
    (spawn-load!)
    (proc-spawn-worker argv log #t)))

;; P's pid, or #f.
(define (jolt-worker-pid p)
  (guard (e (#t #f)) (proc-p-pid p)))

;; Block until P exits. -> its exit status, or #f.
(define (jolt-wait-worker p)
  (guard (e (#t #f)) (proc-wait-blocking p)))

;; Is PID a live process? kill(pid, 0) on POSIX, OpenProcess on Windows
;; (process.ss proc-pid-alive?). A failed probe reads as gone.
(define (jolt-pid-alive? pid)
  (guard (e (#t #f))
    (spawn-load!)
    (proc-pid-alive? pid)))

;; A liveness lock (process.ss proc-lock-hold!): held while this process lives,
;; dropped by the OS when it dies however it dies. -> a token, or #f.
(define (jolt-lock-hold! path)
  (guard (e (#t #f))
    (spawn-load!)
    (proc-lock-hold! path)))
(define (jolt-lock-release! token)
  (guard (e (#t #f)) (proc-lock-release! token)))
;; -> 'held, 'free, 'missing or 'unknown.
(define (jolt-lock-state path)
  (guard (e (#t 'unknown))
    (spawn-load!)
    (proc-lock-state path)))

;; Where a spawn of PROG would find it: `command -v`, without the shell. PATHEXT
;; on Windows. -> the file, or #f.
(define (jolt-which prog)
  (guard (e (#t #f))
    (spawn-load!)
    (proc-which prog)))

;; ARGV run directly; stderr discarded. -> (values exit-status stdout-text), or
;; (values #f "") when it could not be started (a program that is not there).
(define (jolt-run-capture argv)
  (guard (e (#t (values #f "")))
    (spawn-load!)
    (proc-run-capture argv)))
