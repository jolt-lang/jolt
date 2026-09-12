;; java.lang.ProcessBuilder / java.lang.Process over posix_spawn.
;;
;; babashka.process (vendored under jolt.process) is built entirely on the JVM
;; ProcessBuilder / Process API; this file provides that surface so the library
;; runs unmodified. A subprocess is a `/bin/sh -c CMD` spawned with posix_spawn,
;; handing back binary stdin/stdout/stderr ports plus the pid. We drive it as
;; ProcessBuilder does:
;;
;;   - the argv list is shell-quoted and `exec`'d, so the shell performs no word
;;     splitting or globbing (matching ProcessBuilder, which execs directly) and
;;     the pid is the target program, not the intermediate sh.
;;   - :dir  -> `cd 'DIR' &&` prefix
;;   - :env  -> `env -i K=V …` prefix (the env map starts as a copy of the parent
;;     environment, so `env -i` reproduces exactly the intended set)
;;   - file / discard redirects -> shell `1> 'f'` / `2>> 'f'` / `1>/dev/null`
;;   - INHERIT -> no file action, so the child keeps jolt's real descriptor (true
;;     fd-level inheritance: isatty holds, stdin offsets are shared); every other
;;     stream gets a pipe.
;;
;; Chez's own open-process-ports is the FALLBACK, for machine types where that
;; FFI surface is missing (Windows), with a pump thread copying between the pipe
;; and jolt's stdio to emulate INHERIT. It is not the primary path: its fork
;; leaves SIGINT ignored in the child, so a subprocess spawned through it cannot
;; be interrupted with ^C (jolt-a4hs).
;;
;; Exit status, liveness and signalling go through libc waitpid/kill via FFI. A
;; per-process mutex serialises reaping so isAlive/waitFor/exitValue never race a
;; second waitpid (which would ECHILD); the decoded status is cached in a box.
;;
;; Loaded after io-streams.ss (make-in-stream / make-out-stream) and the jhost
;; registries (host-static.ss), and after host-static-methods.ss (all-env-pairs).

;; --- libc entry points -------------------------------------------------------
;; Only ever called WNOHANG (see proc-wait-blocking): a blocking waitpid parks in
;; the kernel, where SIGCHLD=SIG_IGN can hold it indefinitely, and a plain foreign
;; call also keeps the thread "active" for the stop-the-world collector while it
;; waits. Polling sidesteps both.
(define proc-waitpid (jolt-foreign-proc-safe "waitpid" '(int void* int) 'int))
(define proc-kill    (jolt-foreign-proc-safe "kill"    '(int int)       'int))
;; libc signal(2). Named apart from the proc-signal recorder further down — this
;; file is load-ed at top level, so a second define of the same name silently wins
;; and the SIGCHLD restore below would reach the recorder instead.
(define proc-libc-signal (jolt-foreign-proc-safe "signal" '(int void*) 'void*))
;; errno, to tell a waitpid that was merely interrupted (EINTR — retry) from one
;; that can never succeed (ECHILD — the child is gone, retrying is an infinite
;; loop). All three spellings of the location accessor: Darwin/BSD, glibc/musl,
;; then bionic (Android — __errno_location does not exist there, and leaving it
;; out reads every error as 0, so an EINTR retry never fires).
(define proc-errno-loc
  (or (jolt-foreign-proc-safe "__error" '() 'void*)
      (jolt-foreign-proc-safe "__errno_location" '() 'void*)
      (jolt-foreign-proc-safe "__errno" '() 'void*)))
(define (proc-errno)
  (if proc-errno-loc (guard (e (#t 0)) (sa-foreign-ref 'int (proc-errno-loc) 0)) 0))

(define proc-WNOHANG 1)        ; macOS + Linux
(define proc-SIGTERM 15)
(define proc-SIGKILL 9)
(define proc-EINTR 4)          ; macOS + Linux
(define proc-ECHILD 10)        ; macOS + Linux
;; SIGCHLD is 20 on Darwin/BSD and 17 on Linux. Same os-family test as
;; concurrency.ss uses for the SIG_BLOCK numerics.
(define proc-SIGCHLD (if (eq? (sa-os-family) 'macos) 20 17))

;; EAGAIN/EWOULDBLOCK, for the R8-style non-blocking pipe read/write retry
;; loop (proc-fd-input-port / proc-fd-output-port below). Genuinely
;; platform-dependent, unlike proc-EINTR -- same os-family test process.ss
;; already uses for proc-SIGCHLD.
(define proc-EAGAIN (if (eq? (sa-os-family) 'macos) 35 11))

;; fcntl(F_GETFL)/(F_SETFL) for setting a pipe fd non-blocking, mirroring
;; stdlib/jolt/io_poller.clj's own private c-fcntl binding
;; (io_poller.clj:59-63,70) at the Scheme level. fcntl(2) is C-variadic;
;; F_GETFL is called with NO variadic argument (plain 2-arg binding is
;; correct). F_SETFL is ALWAYS called with one variadic argument (the new
;; flags value) and REQUIRES the (__varargs_after 2) calling-convention
;; token -- "2" is the count of fixed/named params before the first true
;; variadic arg. Without this marker, F_SETFL on Apple arm64 returns
;; success but silently fails to apply the flags (verified by
;; test/chez/ffi-binding-test.ss:82-104, which proves the identical fix
;; via the Clojure FFI :varargs marker on a real socket with F_SETFL
;; before/after flags readback). Mirrors stdlib/jolt/ffi.clj's own
;; :varargs marker (io_poller.clj's `(ffi/defcfn c-fcntl "fcntl"
;; [:int :int :varargs :int] :int)`), exposed here as the raw Scheme-level
;; jolt-foreign-proc-safe 4-arg form (rt.ss:73-88) -- a Chez/Apple-ABI
;; feature, not a jolt invention.
(define proc-fcntl-get (jolt-foreign-proc-safe "fcntl" '(int int) 'int))
(define proc-fcntl-set (jolt-foreign-proc-safe (__varargs_after 2) "fcntl" '(int int int) 'int))
(define proc-F-GETFL 3)
(define proc-F-SETFL 4)
(define proc-O-NONBLOCK (if (eq? (sa-os-family) 'macos) #x4 #x800))

;; Nothing here happens without a working fcntl and a readable errno: a pipe
;; set non-blocking whose EAGAIN cannot be recognised reads as EOF instead
;; (the (else 0) branch in proc-fd-input-port), which is worse than a blocking
;; pipe. So the whole R8 extension is gated on the three bindings, and the
;; degradation when one is missing is "no parking" — the pipes stay blocking
;; and EAGAIN never arrives. Deliberately NOT folded into
;; proc-spawn-fd-ok?: that gate decides posix_spawn vs Chez's fork, and the
;; fork path leaves SIGINT at SIG_IGN in the child (see the comment above
;; proc-spawn-fd-mutex). Correct child signal dispositions are not worth
;; trading for O_NONBLOCK.
(define proc-nonblock-ok? (and proc-fcntl-get proc-fcntl-set proc-errno-loc #t))

(define (proc-set-nonblocking! fd)
  (when proc-nonblock-ok?
    (let ((flags (proc-fcntl-get fd proc-F-GETFL)))
      ;; A negative flags value means F_GETFL itself failed -- don't hand that
      ;; straight to F_SETFL: (fxior -1 proc-O-NONBLOCK) is still negative, and
      ;; the kernel would mask it down, silently leaving the fd blocking with
      ;; no diagnostic that anything went wrong.
      (when (>= flags 0)
        (proc-fcntl-set fd proc-F-SETFL (fxior flags proc-O-NONBLOCK))))))

;; jolt.socket is the only thing in the tree that requires jolt.io-poller, so a
;; program that drives subprocesses from fibers without ever touching a socket
;; — a build-tool wrapper, a shell pipeline — would find wait-ready unbound and
;; take the blocking fallback below, pinning its carrier: exactly the starvation
;; this whole file's R8 extension exists to remove. Autoload the poller instead,
;; at the one moment it is known to be needed, and only when there IS a fiber to
;; park: a plain thread keeps the cheaper blocking read rather than paying a
;; private kqueue/epoll per wait.
;;
;; Safe from inside a port read even though a load can park and can race another
;; thread's: loader.ss's claim protocol owns both cases (ldr-begin-load! steps
;; 2-3), and leaning on it rather than on a latch of our own is what makes the
;; CONCURRENT case come out right. Two fibers on two carriers reach their first
;; EAGAIN at once routinely -- it is the ordinary shape of "read several
;; subprocesses from go blocks", the thing this whole extension is for. A
;; boolean "already tried" latch set before the load would let the second fiber
;; skip straight past it, find the var still unbound because the first is only
;; part way through, and take the blocking fallback -- clearing O_NONBLOCK on
;; its own fd for good, so that one port never parks again for the life of the
;; process. Calling load-namespace instead makes the second fiber WAIT on the
;; first's claim (fiber-aware, via jolt-lock-parked, so it parks rather than
;; pinning), then find a fully built namespace. Already-loaded is a hashtable
;; lookup, and we only get here when the var is unbound anyway.
;;
;; The one thing the loader cannot decide for us is a load that FAILS
;; (jolt.io-poller off the source roots in a trimmed install): the loader rolls
;; its own mark back on a throw, deliberately, so a retry can work. Nothing
;; about the next EAGAIN will have changed, though, so record the failure here
;; and stop asking.
(define proc-poller-load-failed #f)
(define (proc-poller-autoload!)
  (unless proc-poller-load-failed
    (guard (e (#t (set! proc-poller-load-failed #t) #f))
      (load-namespace "jolt.io-poller"))))

;; Bridge to jolt.io-poller/wait-ready: var-deref never throws for a
;; missing var -- jolt-var auto-vivifies a jolt-var-unbound placeholder
;; instead (rt.ss:662-669), checkable via the record type's own
;; predicate. Off a fiber an unbound var stays unbound (see
;; proc-poller-autoload! above for why the load is fiber-only) and this
;; returns #f, so the caller falls back to a real blocking wait it
;; implements itself (the EAGAIN branches in
;; proc-fd-input-port/proc-fd-output-port below -- a bare 0-byte return
;; there would misread as EOF, since the pipe fd is genuinely
;; non-blocking by then). On a fiber the same #f is only reachable when
;; the autoload itself failed.
(define (proc-poller-wait-ready fd filt-kw)
  (let* ((f0 (var-deref "jolt.io-poller" "wait-ready"))
         (f (if (and (jolt-var-unbound? f0) (jolt-current-fiber))
                (begin (proc-poller-autoload!)
                       (var-deref "jolt.io-poller" "wait-ready"))
                f0)))
    (if (jolt-var-unbound? f)
        #f
        (begin (jolt-invoke2 f fd filt-kw) #t))))

;; Bridge to jolt.io-poller/forget!, same shape as proc-poller-wait-ready above.
;; A closed fd is auto-removed from the kernel's kqueue/epoll set, so no
;; event is ever coming for a fiber still parked on it -- without telling
;; the poller to drop its registration, that fiber sleeps forever, and a
;; leaked ready=true tombstone in the poller's shared :fds table (keyed by
;; bare fd integer, shared with jolt.socket) can then be consumed by a
;; REUSED fd number belonging to an unrelated later socket. Mirrors
;; jolt.socket's socket-close! (stdlib/jolt/socket.clj), which does the
;; identical close-then-forget for the same reason. Same unbound-var guard
;; as proc-poller-wait-ready, and no autoload: a poller that was never loaded
;; holds no registration for this fd, so there is nothing to forget.
(define (proc-poller-forget! fd)
  (let ((f (var-deref "jolt.io-poller" "forget!")))
    (unless (jolt-var-unbound? f) (jolt-invoke1 f fd))))

;; A jolt that spawns children has to be able to reap them, so SIGCHLD must not be
;; left at an INHERITED SIG_IGN: with that disposition the kernel reaps every child
;; itself and waitpid can only ever fail with ECHILD, making exit statuses
;; unknowable. The disposition does survive exec, so jolt can arrive with it set
;; through no choice of its own — a CI runner, a supervisor, any parent that
;; ignored SIGCHLD. Restore SIG_DFL once, before the first spawn, as a JVM does
;; when it installs its own child handling.
;;
;; On the first spawn rather than at load: a jolt that never starts a process has
;; no business touching the process's signal dispositions. Only SIG_IGN is
;; replaced — an inherited real HANDLER is left alone, since something in the
;; process legitimately wants those notifications.
(define proc-sigchld-checked? (box #f))
(define (proc-ensure-reapable!)
  (unless (unbox proc-sigchld-checked?)
    (set-box! proc-sigchld-checked? #t)
    (when proc-libc-signal
      (guard (e (#t #f))
        (let ((prev (proc-libc-signal proc-SIGCHLD 0)))   ; install SIG_DFL, get the old one
          ;; SIG_IGN is 1; anything else (SIG_DFL = 0, or a handler address) is put
          ;; back. SIG_ERR is -1 and means the call did not take, so leave it alone.
          (unless (or (eqv? prev 1) (eqv? prev -1))
            (proc-libc-signal proc-SIGCHLD prev)))))))

;; WEXITSTATUS / signalled-process convention: a process killed by signal N
;; reports 128+N, matching the JVM's Process.exitValue on Unix.
(define (proc-decode-status raw)
  (let ((termsig (bitwise-and raw #x7f)))
    (if (= termsig 0)
        (bitwise-and (bitwise-arithmetic-shift-right raw 8) #xff)
        (+ 128 termsig))))

;; One waitpid call; returns (values rc decoded-or-#f errno). rc = pid on reap, 0
;; when WNOHANG and still running, -1 on error (errno says which).
(define (proc-waitpid-once pid nohang?)
  (if (not proc-waitpid)
      (values -1 #f proc-ECHILD)
      (let ((buf (sa-foreign-alloc 4)))
        (let* ((rc (proc-waitpid pid buf (if nohang? proc-WNOHANG 0)))
               (err (if (< rc 0) (proc-errno) 0))
               (raw (sa-foreign-ref 'int buf 0)))
          (sa-foreign-free buf)
          (values rc (and (= rc pid) (proc-decode-status raw)) err)))))

;; The status to report for a child that can no longer be waited on (ECHILD:
;; something else reaped it). If we signalled it, Unix convention gives the answer
;; — 128+signal, the same encoding proc-decode-status uses. Otherwise the status is
;; genuinely unrecoverable and 0 is reported, a documented divergence from the JVM
;; (which always reaps its own children and so always knows). proc-ensure-reapable!
;; is what keeps this from arising in the first place.
(define (proc-lost-status st)
  (or (unbox (proc-p-signalled st)) 0))

;; --- shell command construction ----------------------------------------------
(define (proc-sh-quote s)      ; single-quote a token for /bin/sh
  (let ((s (if (string? s) s (jolt-str-render-one s))))
    (string-append "'"
      (apply string-append
        (map (lambda (c) (if (char=? c #\') "'\\''" (string c))) (string->list s)))
      "'")))

(define (proc-join sep xs)
  (if (null? xs) ""
      (fold-left (lambda (a x) (string-append a sep x)) (car xs) (cdr xs))))

;; A redirect descriptor is a Redirect jhost (kind + optional file) or #f (the
;; default: pipe). Returns the shell redirection fragment for fd `n` ("1"/"2"/"0"),
;; or "" when the pipe is kept (PIPE / INHERIT / a stream target — those are
;; handled by pump threads after start).
(define (proc-redir-fragment n redir)
  (if (not (proc-redirect? redir)) ""
      (let ((kind (proc-redirect-kind redir))
            (file (proc-redirect-file redir)))
        (case kind
          ((write)   (string-append " " n "> "  (proc-sh-quote file)))
          ((append)  (string-append " " n ">> " (proc-sh-quote file)))
          ((read)    (string-append " " n "< "  (proc-sh-quote file)))
          ((discard) (string-append " " n ">/dev/null"))
          (else "")))))                 ; inherit / pipe -> pump or passthrough

(define (proc-env-prefix env-map)
  (if (not env-map) ""
      (let ((pairs (proc-env-map-pairs env-map)))
        (string-append "env -i "
          (proc-join " "
            (map (lambda (p) (proc-sh-quote (string-append (car p) "=" (cdr p)))) pairs))
          " "))))

;; The child's cwd: a JVM child inherits user.dir (the user's cwd), but jolt's OS
;; cwd is the repo root its launcher cd'd to — the user's logical cwd is JOLT_PWD.
;; So default the child to JOLT_PWD and resolve a relative :dir against it (like
;; io.ss project-relative), matching ProcessBuilder.directory semantics.
(define (proc-effective-dir dir)
  (if dir
      (project-relative dir)
      (let ((pwd (getenv "JOLT_PWD"))) (and pwd (> (string-length pwd) 0) pwd))))

(define (proc-build-shell-command st)
  (let* ((cmd     (proc-pb-cmd st))
         (env-map (proc-pb-env st))
         (dir     (proc-effective-dir (proc-pb-dir st)))
         (rin     (proc-pb-redir-in st))
         (rout    (proc-pb-redir-out st))
         (rerr    (proc-pb-redir-err st))
         (merge?  (proc-pb-merge-err? st)))
    (string-append
      (if dir (string-append "cd " (proc-sh-quote dir) " && ") "")
      "exec "
      (proc-env-prefix env-map)
      (proc-join " " (map proc-sh-quote cmd))
      (proc-redir-fragment "0" rin)
      (proc-redir-fragment "1" rout)
      (if merge? " 2>&1" (proc-redir-fragment "2" rerr)))))

;; --- java.lang.ProcessBuilder$Redirect ---------------------------------------
;; state: #(kind file) — kind in {inherit discard pipe write append read}.
(define (make-proc-redirect kind file) (make-jhost "process-redirect" (vector kind file)))
(define (proc-redirect? x) (and (jhost? x) (string=? (jhost-tag x) "process-redirect")))
(define (proc-redirect-kind r) (vector-ref (jhost-state r) 0))
(define (proc-redirect-file r) (vector-ref (jhost-state r) 1))

;; Named so inheritIO() can hand back the SAME object Redirect/INHERIT is. On the
;; JVM these are singletons — (= (.redirectInput pb) Redirect/INHERIT) is true
;; after .inheritIO() — and a fresh instance per call would read as a different
;; redirect to anything comparing them.
(define proc-redirect-inherit (make-proc-redirect 'inherit #f))
(define proc-redirect-pipe (make-proc-redirect 'pipe #f))

(define proc-redirect-statics
  (list (cons "INHERIT" proc-redirect-inherit)
        (cons "DISCARD" (make-proc-redirect 'discard #f))
        (cons "PIPE"    proc-redirect-pipe)
        (cons "to"       (lambda (f) (make-proc-redirect 'write  (file-path-of f))))
        (cons "appendTo" (lambda (f) (make-proc-redirect 'append (file-path-of f))))
        (cons "from"     (lambda (f) (make-proc-redirect 'read   (file-path-of f))))))
;; register-class-statics! mirrors the FQN table to the short name, so a single
;; call serves both java.lang.ProcessBuilder$Redirect/… and ProcessBuilder$Redirect/….
(register-class-statics! "java.lang.ProcessBuilder$Redirect" proc-redirect-statics)
(register-host-methods! "process-redirect"
  (list (cons "type" (lambda (self) (symbol->string (proc-redirect-kind self))))
        (cons "toString" (lambda (self) (string-append "Redirect." (symbol->string (proc-redirect-kind self)))))))

;; JDK 9 gave every redirect setter a File overload — redirectInput(File) is
;; defined as redirectInput(Redirect.from(file)), redirectOutput/redirectError as
;; Redirect.to(file). Without them the File was STORED as the redirect and then
;; ignored by proc-redir-fragment, which only understands a Redirect jhost: the
;; child silently kept jolt's own fd, so `:in (fs/file "/dev/null")` through
;; babashka.process left the child reading the terminal forever (jolt#947).
;; `kind` is the one Redirect.from/to would build for this stream.
(define (proc-redirect-arg who kind x)
  (cond ((proc-redirect? x) x)
        ((jfile? x) (make-proc-redirect kind (jfile-path x)))
        ;; a Path (nio-file.ss) or a plain path string: not JDK overloads, but
        ;; .directory already takes either through file-path-of, and refusing
        ;; them here would make the redirect setters the odd ones out.
        ((string? x) (make-proc-redirect kind x))
        ((nio-path? x) (make-proc-redirect kind (nio-path-str x)))
        (else (throw-jvm (quote IllegalArgumentException)
                (string-append "ProcessBuilder." who ": expected a ProcessBuilder$Redirect or a File, got "
                               (jolt-str-render-one x))))))

;; --- environment map (ProcessBuilder.environment()) --------------------------
;; A live mutable Map<String,String>, seeded from the parent environment. jolt's
;; babashka.process only calls clear/putAll, but put/get/remove are provided too.
;; state: a Scheme string->string hashtable.
;; The environment a child starts from: the parent's, less JOLT_PWD. That variable
;; is the launcher's message to THIS process — bin/jolt exports the user's cwd
;; before cd'ing to its checkout — and a child's cwd is whatever the spawn chose
;; (proc-effective-dir), so an inherited copy hands a child jolt the PARENT's
;; project as its user.dir: (slurp "README.md") under :dir read the spawner's
;; README. A caller that puts JOLT_PWD in the env map asked for it and keeps it.
;; Both the inherited envp and the seed of ProcessBuilder.environment() come from
;; here, so there is no spawn shape that forwards it.
(define (proc-child-env-pairs)
  (filter (lambda (p) (not (string=? (car p) "JOLT_PWD"))) (all-env-pairs)))
(define (make-proc-env-map)
  (let ((h (make-hashtable string-hash string=?)))
    (for-each (lambda (p) (hashtable-set! h (car p) (cdr p))) (proc-child-env-pairs))
    (make-jhost "jolt-env-map" h)))
(define (proc-env-map-pairs em)
  (let ((h (jhost-state em)))
    (vector->list
      (vector-map (lambda (k) (cons k (hashtable-ref h k ""))) (hashtable-keys h)))))
(define (proc-env-put-all! em m)
  (let ((h (jhost-state em)))
    (unless (jolt-nil? m)
      (for-each (lambda (e)
                  (hashtable-set! h (jolt-str-render-one (jolt-nth e 0))
                                    (jolt-str-render-one (jolt-nth e 1))))
                (seq->list (jolt-seq m))))))
(register-host-methods! "jolt-env-map"
  (list (cons "clear"  (lambda (self) (hashtable-clear! (jhost-state self)) jolt-nil))
        (cons "putAll" (lambda (self m) (proc-env-put-all! self m) jolt-nil))
        (cons "put"    (lambda (self k v)
                         (hashtable-set! (jhost-state self) (jolt-str-render-one k) (jolt-str-render-one v)) jolt-nil))
        (cons "get"    (lambda (self k)
                         (let ((v (hashtable-ref (jhost-state self) (jolt-str-render-one k) #f))) (or v jolt-nil))))
        (cons "remove" (lambda (self k) (hashtable-delete! (jhost-state self) (jolt-str-render-one k)) jolt-nil))
        (cons "containsKey" (lambda (self k) (and (hashtable-contains? (jhost-state self) (jolt-str-render-one k)) #t)))))

;; --- java.lang.ProcessBuilder ------------------------------------------------
;; state: #(cmd env-map dir redir-in redir-out redir-err merge-err?)
(define (make-proc-builder cmd)
  (make-jhost "process-builder" (vector cmd #f #f #f #f #f #f)))
(define (proc-pb-cmd st)         (vector-ref (jhost-state st) 0))
(define (proc-pb-env st)         (vector-ref (jhost-state st) 1))
(define (proc-pb-dir st)         (vector-ref (jhost-state st) 2))
(define (proc-pb-redir-in st)    (vector-ref (jhost-state st) 3))
(define (proc-pb-redir-out st)   (vector-ref (jhost-state st) 4))
(define (proc-pb-redir-err st)   (vector-ref (jhost-state st) 5))
(define (proc-pb-merge-err? st)  (vector-ref (jhost-state st) 6))
(define (proc-pb-set! st i v)    (vector-set! (jhost-state st) i v))

;; the ProcessBuilder ctor: (java.lang.ProcessBuilder. cmd) where cmd is a jolt
;; vector/list of strings (or its varargs form).
(define (proc-builder-ctor . args)
  (let ((cmd (cond ((null? args) '())
                   ((and (null? (cdr args)) (not (string? (car args))) (not (jolt-nil? (car args))))
                    ;; a single collection argument -> its elements
                    (map jolt-str-render-one (seq->list (jolt-seq (car args)))))
                   (else (map jolt-str-render-one args)))))
    (make-proc-builder cmd)))
(register-class-ctor! "java.lang.ProcessBuilder" proc-builder-ctor)
(register-class-ctor! "ProcessBuilder" proc-builder-ctor)

(register-host-methods! "process-builder"
  (list (cons "command" (lambda (self . args)
          (if (null? args)
              (apply jolt-vector (proc-pb-cmd self))               ; getter
              (begin (proc-pb-set! self 0 (map jolt-str-render-one (seq->list (jolt-seq (car args))))) self))))
        (cons "directory" (lambda (self f) (proc-pb-set! self 2 (file-path-of f)) self))
        (cons "environment" (lambda (self)
          (or (proc-pb-env self)
              (let ((em (make-proc-env-map))) (proc-pb-set! self 1 em) em))))
        ;; Each redirect method is a setter with an argument and a GETTER with
        ;; none, like the JDK's. An unset stream reads back as PIPE, which is the
        ;; documented default rather than nil.
        (cons "redirectInput"  (lambda (self . r)
          (if (null? r) (or (proc-pb-redir-in self) proc-redirect-pipe)
              (begin (proc-pb-set! self 3 (proc-redirect-arg "redirectInput" 'read (car r))) self))))
        (cons "redirectOutput" (lambda (self . r)
          (if (null? r) (or (proc-pb-redir-out self) proc-redirect-pipe)
              (begin (proc-pb-set! self 4 (proc-redirect-arg "redirectOutput" 'write (car r))) self))))
        (cons "redirectError"  (lambda (self . r)
          (if (null? r) (or (proc-pb-redir-err self) proc-redirect-pipe)
              (begin (proc-pb-set! self 5 (proc-redirect-arg "redirectError" 'write (car r))) self))))
        (cons "redirectErrorStream" (lambda (self . b)
          (if (null? b) (and (proc-pb-merge-err? self) #t)
              (begin (proc-pb-set! self 6 (jolt-truthy? (car b))) self))))
        ;; inheritIO(): set all three standard streams to INHERIT and return this
        ;; — the JDK defines it as exactly redirectInput(INHERIT)
        ;; .redirectOutput(INHERIT).redirectError(INHERIT), and the start path
        ;; already understands the 'inherit kind. Without it a caller had to spell
        ;; those three out, and the miss reported as "No matching field found:
        ;; inheritIO" because jolt reads a 0-arg method miss as a field probe
        ;; (jolt-674). It does NOT clear redirectErrorStream: on the JVM the two
        ;; are independent, and a merge set before or after still wins over the
        ;; error redirect.
        (cons "inheritIO" (lambda (self)
                            (proc-pb-set! self 3 proc-redirect-inherit)
                            (proc-pb-set! self 4 proc-redirect-inherit)
                            (proc-pb-set! self 5 proc-redirect-inherit)
                            self))
        (cons "start" (lambda (self) (proc-pb-start self)))))

;; startPipeline: connect N builders stdout->stdin with pump threads, returning a
;; jolt list of the resulting Processes (JDK9 semantics).
(define (proc-start-pipeline pbs)
  (let* ((pb-list (seq->list (jolt-seq pbs)))
         (procs   (map proc-pb-start pb-list)))
    (let loop ((ps procs))
      (when (and (pair? ps) (pair? (cdr ps)))
        (proc-pump (proc-p-stdout-port (car ps)) (proc-p-stdin-port (cadr ps)) #t)
        (loop (cdr ps))))
    (list->cseq procs)))
(register-class-statics! "java.lang.ProcessBuilder" (list (cons "startPipeline" proc-start-pipeline)))

;; --- pump threads ------------------------------------------------------------
;; Copy a binary input port to a binary output port until EOF; optionally close
;; the destination at EOF (so a downstream process sees end-of-input). Returns a
;; latch (mutex + condition + done box, like Thread.join in concurrency.ss) so a
;; caller can block until the copy is complete — an INHERIT redirect must have
;; forwarded all output before the process is reported finished.
;; Copy one chunk-worth from src to dst, handling either port being binary or
;; textual: a child pipe is binary (bytes), while jolt's own stdio (INHERIT's
;; target) is textual, so bytes are transcoded UTF-8 across the boundary. Returns
;; #f at EOF, #t otherwise.
(define (proc-copy-chunk src dst)
  (if (binary-port? src)
      (let ((bv (get-bytevector-some src)))
        (and (not (eof-object? bv))
             (begin (if (textual-port? dst) (put-string dst (utf8->string bv)) (put-bytevector dst bv))
                    (flush-output-port dst) #t)))
      (let ((s (get-string-some src)))
        (and (not (eof-object? s))
             (begin (if (binary-port? dst) (put-bytevector dst (string->utf8 s)) (put-string dst s))
                    (flush-output-port dst) #t)))))
(define (proc-pump src dst close-dst?)
  (let ((m (make-mutex)) (c (make-condition)) (done (box #f)))
    (fork-thread
      (lambda ()
        (guard (e (#t #f))
          (let loop () (when (proc-copy-chunk src dst) (loop))))
        (when close-dst? (guard (e (#t #f)) (close-port dst)))
        (jolt-with-mutex m (set-box! done #t) (jolt-cv-wake! c))))
    (vector m c done)))
;; A fiber waiting for a subprocess's output collector PARKS. Blocking the carrier
;; would stop every other fiber on it for the life of the subprocess, which is
;; exactly the kind of wait a go block is the natural place for (jolt-x1no).
(define (proc-latch-wait latch)
  (jolt-cv-wait (vector-ref latch 0) (vector-ref latch 1) #f
    (lambda (_timed-out?)
      (if (unbox (vector-ref latch 2)) #t jolt-cv-again))))

;; --- fd-level spawn (the primary path) ---------------------------------------
;; A child fd that gets no file action IS the parent's descriptor — tty answers
;; true, writes land without an intermediary, successive INHERIT-stdin children
;; share the read offset — and pipes are built for the streams that ask for one.
;; open-process-ports can do none of that: it pipes all three streams
;; unconditionally (its child closes every other descriptor), so INHERIT through
;; it can only ever be pump emulation, where the child sees a pipe, isatty is
;; false, output detours through jolt's ports, and stdin reads ahead of what the
;; child consumed. It also leaves SIGINT ignored in its child. So this is the
;; path every spawn takes where the FFI surface exists, and the pump emulation
;; below is what remains where it does not (Windows machine types). Everything
;; downstream — waitpid reaping, kill, the stream wrappers — is shared.

(define proc-c-pipe  (jolt-foreign-proc-safe "pipe"  '(void*) 'int))
(define proc-c-close (jolt-foreign-proc-safe "close" '(int)   'int))
(define proc-fa-init    (jolt-foreign-proc-safe "posix_spawn_file_actions_init"     '(void*) 'int))
(define proc-fa-dup2    (jolt-foreign-proc-safe "posix_spawn_file_actions_adddup2"  '(void* int int) 'int))
(define proc-fa-close   (jolt-foreign-proc-safe "posix_spawn_file_actions_addclose" '(void* int) 'int))
(define proc-fa-destroy (jolt-foreign-proc-safe "posix_spawn_file_actions_destroy"  '(void*) 'int))
(define proc-c-spawn (jolt-foreign-proc-safe "posix_spawn" '(void* string void* void* void* void*) 'int))
(define proc-c-read  (jolt-foreign-proc-blocking "read"  '(int void* size_t) 'ssize_t))
(define proc-c-write (jolt-foreign-proc-blocking "write" '(int void* size_t) 'ssize_t))

;; posix_spawn_file_actions_addclosefrom_np(fa, 3): one file action that closes
;; every descriptor at or above 3 in the child, after the dup2s below have put
;; the pipes on 0/1/2. glibc 2.34+ and Solaris have it (glibc lowers it to a
;; single close_range(2) syscall); older glibcs — jolt's release binary is built
;; for a floor well below 2.34 — and macOS do not, which is what
;; proc-close-inherited-fds! falls back for.
(define proc-fa-closefrom
  (jolt-foreign-proc-safe "posix_spawn_file_actions_addclosefrom_np" '(void* int) 'int))

;; macOS has no closefrom action, but it has the flag the question was asked
;; for: POSIX_SPAWN_CLOEXEC_DEFAULT starts the child with EVERY descriptor
;; closed except the ones a file action names — a dup2 target, an open, or an
;; explicit posix_spawn_file_actions_addinherit_np. The set is decided by the
;; kernel at spawn time, so there is no parent-side snapshot to go stale (the
;; enumeration fallback's race, below). Both entries are Darwin-only; where they
;; do not resolve the flag is not used.
(define proc-attr-init     (jolt-foreign-proc-safe "posix_spawnattr_init"     '(void*) 'int))
(define proc-attr-setflags (jolt-foreign-proc-safe "posix_spawnattr_setflags" '(void* short) 'int))
(define proc-attr-destroy  (jolt-foreign-proc-safe "posix_spawnattr_destroy"  '(void*) 'int))
(define proc-fa-inherit
  (jolt-foreign-proc-safe "posix_spawn_file_actions_addinherit_np" '(void* int) 'int))
(define proc-POSIX-SPAWN-CLOEXEC-DEFAULT #x4000)   ; <sys/spawn.h>, Darwin

;; What posix_spawn-with-pipes needs, and nothing more. The R8 fiber-parking
;; extension's own bindings (fcntl, errno) are gated separately by
;; proc-nonblock-ok? above: losing parking is a performance story, losing this
;; path means falling back to Chez's fork with SIGINT ignored in the child.
(define proc-spawn-fd-ok?
  (and proc-c-pipe proc-c-close proc-fa-init proc-fa-dup2 proc-fa-close
       proc-fa-destroy proc-c-spawn proc-c-read proc-c-write #t))

;; marshal a string list into a NULL-terminated char**; returns (array . cstrs)
;; so the caller can free every allocation once posix_spawn has copied them.
(define (proc-marshal-cstr s)
  (let* ((bv (string->utf8 s))
         (n (bytevector-length bv))
         (p (sa-foreign-alloc (+ n 1))))
    (let loop ((i 0))
      (when (< i n)
        (sa-foreign-set! 'unsigned-8 p i (bytevector-u8-ref bv i))
        (loop (+ i 1))))
    (sa-foreign-set! 'unsigned-8 p n 0)
    p))
(define (proc-marshal-argv strs)
  (let* ((ps (map proc-marshal-cstr strs))
         (arr (sa-foreign-alloc (* 8 (+ 1 (length ps))))))
    (let loop ((i 0) (rest ps))
      (if (null? rest)
          (sa-foreign-set! 'void* arr (* 8 i) 0)
          (begin (sa-foreign-set! 'void* arr (* 8 i) (car rest))
                 (loop (+ i 1) (cdr rest)))))
    (cons arr ps)))
(define (proc-free-argv m)
  (for-each sa-foreign-free (cdr m))
  (sa-foreign-free (car m)))

;; binary ports over the raw pipe fds. EINTR is retried; EAGAIN parks the
;; current fiber (or blocks this thread) via jolt.io-poller when it's loaded
;; -- symmetric on both sides below: proc-fd-input-port waits on :read,
;; proc-fd-output-port waits on :write, over the same mk-pipe-created
;; non-blocking fd and the same proc-poller-wait-ready bridge. When no poller is
;; loaded, EAGAIN clears O_NONBLOCK on this fd (fd is genuinely non-blocking
;; now per mk-pipe above, so a no-poller EAGAIN is a live "not ready yet"
;; signal, not a real failure) and falls through to a real blocking
;; proc-c-read/proc-c-write from then on -- exact behavioral parity with a
;; plain blocking fd once triggered, verified standalone: a blocked
;; read/write genuinely waits rather than busy-looping or returning a
;; spurious result. Any OTHER error still ends the port, but the two sides
;; report it differently -- pre-existing asymmetry, unchanged by this: the
;; read side reads it as EOF (0, the child is gone and the pipe with it);
;; the write side raises (child gone, `error 'process "write to child
;; failed"`). A short write returns its count and Chez's port machinery
;; re-calls for the rest.
;;
;; The `closed?` box each port allocates alongside its buf is what makes the
;; close proc safe against a fiber PARKED in that retry loop. Close is three
;; steps -- free buf, close fd, forget the fd -- and the third wakes any parked
;; waiter through jolt.io-poller. That wake is ASYNCHRONOUS: jolt.host's
;; fiber-resume -> sa-fiber-resume (host/chez/fibers.ss) only flips the fiber to
;; 'ready and enqueues it on its carrier's run queue; the fiber's continuation
;; runs later, on the carrier thread. Between the wake and that continuation
;; there is a real scheduling gap, and by then buf is already free()d
;; (sa-foreign-free is a bare foreign-free -- no pooling, no quarantine) and fd
;; is already closed. A closed fd number is immediately eligible for reuse by
;; ANY concurrent pipe/socket/open/accept anywhere in the process -- POSIX hands
;; out the lowest free number, and jolt.process exists to support many
;; concurrent spawns -- so without the flag the woken fiber's (retry) calls
;; proc-c-read/proc-c-write with a fd that is valid again but belongs to
;; something else entirely, handing the kernel a pointer into freed memory. That
;; stale retry could also re-register the reused fd with jolt.io-poller,
;; cross-talking with its new owner's registration. proc-spawn-fd-mutex does not
;; help: it covers only the pipe()-through-posix_spawn sequence, not close and
;; not fd allocation in general.
;;
;; So the close proc sets closed? FIRST, before it frees, closes, or forgets,
;; and the retry loop tests it at the TOP of EVERY iteration -- the first one
;; included, not just the one after a park. The ordering is what makes that test
;; sound rather than hopeful: set-box! happens before proc-poller-forget!, which
;; reaches sa-fiber-resume, which takes and releases the carrier's mutex to
;; enqueue the fiber; the carrier thread takes that SAME mutex in
;; jolt-fiber-dequeue! before it can run the fiber at all. A release/acquire
;; pair on one mutex, so the #t is published to the woken fiber -- it cannot
;; observe a stale #f, and it cannot have cached the read across the park, since
;; the park sits behind proc-poller-wait-ready's opaque var-deref'd call. Same
;; box/set-box!/unbox shape concurrency.ss uses for its own cross-thread flag
;; (agents-shutdown?).
;;
;; Each side short-circuits the way it already reports a dead pipe: the read
;; side returns 0 (the `(else 0)` EOF convention below), the write side raises
;; (the `error 'process` convention below) -- with its own message, so a port
;; closed under a parked write is not misreported as a failing child.
;;
;; Deliberately NOT covered: a close racing a syscall already IN FLIGHT on
;; another thread rather than parked. That caller read fd and buf before any
;; flag could be set, so no flag can help it; it is a pre-existing hazard of
;; closing a port other threads are actively using, not something this adds.
;;
;; Also NOT covered: a fiber that has decided to call proc-poller-wait-ready but
;; has not yet registered when close runs -- forget! finds nothing to drop,
;; then the fiber registers on an already-dead fd. That is a strand (a hang),
;; not a use-after-free; closing it needs coordination on the jolt.io-poller
;; side (a register/forget race), left out of scope here. Pre-existing,
;; not introduced by this fix.
(define proc-fd-buf-size 32768)
(define (proc-fd-input-port fd)
  (let ((buf (sa-foreign-alloc proc-fd-buf-size))
        (closed? (box #f)))
    (make-custom-binary-input-port
      (string-append "process-fd-" (number->string fd))
      (lambda (bv start n)
        (let ((want (min n proc-fd-buf-size)))
          (let retry ()
            ;; Top of EVERY iteration, first one included: a fiber woken by the
            ;; close proc's proc-poller-forget! resumes HERE, and buf is freed and fd
            ;; closed (possibly reused) by then. Read as EOF without touching
            ;; either -- same answer the (else 0) branch below would give for a
            ;; closed fd's EBADF, reached without the syscall.
            (if (unbox closed?)
                0
                (let ((got (proc-c-read fd buf want)))
                  (cond
                    ((> got 0)
                     (let loop ((i 0))
                       (when (< i got)
                         (bytevector-u8-set! bv (+ start i) (sa-foreign-ref 'unsigned-8 buf i))
                         (loop (+ i 1))))
                     got)
                    ((= got 0) 0)
                    ((= (proc-errno) proc-EINTR) (retry))
                    ((= (proc-errno) proc-EAGAIN)
                     (if (proc-poller-wait-ready fd (jolt-keyword "read"))
                         (retry)
                         (begin
                           (let ((flags (proc-fcntl-get fd proc-F-GETFL)))
                             (proc-fcntl-set fd proc-F-SETFL (fxand flags (fxnot proc-O-NONBLOCK))))
                           (retry))))
                    (else 0)))))))
      #f #f
      ;; closed? goes up BEFORE the free/close/forget below, so it is already #t
      ;; on the far side of the carrier-mutex handoff every woken fiber crosses.
      (lambda ()
        (set-box! closed? #t)
        (sa-foreign-free buf) (proc-c-close fd) (proc-poller-forget! fd)))))
(define (proc-fd-output-port fd)
  (let ((buf (sa-foreign-alloc proc-fd-buf-size))
        (closed? (box #f)))
    (make-custom-binary-output-port
      (string-append "process-fd-" (number->string fd))
      (lambda (bv start n)
        (let ((want (min n proc-fd-buf-size)))
          (let loop ((i 0))
            (when (< i want)
              (sa-foreign-set! 'unsigned-8 buf i (bytevector-u8-ref bv (+ start i)))
              (loop (+ i 1))))
          (let retry ()
            ;; Top of EVERY iteration, first one included -- the read side's
            ;; twin, and the branch a fiber woken by the close proc lands on.
            ;; Raises rather than returning a count, matching the (else ...)
            ;; convention below; its own message because the child is fine here,
            ;; the port under this write is not.
            (if (unbox closed?)
                (error 'process "write to closed pipe" fd)
                (let ((wrote (proc-c-write fd buf want)))
                  (cond
                    ((>= wrote 0) wrote)
                    ((= (proc-errno) proc-EINTR) (retry))
                    ((= (proc-errno) proc-EAGAIN)
                     (if (proc-poller-wait-ready fd (jolt-keyword "write"))
                         (retry)
                         (begin
                           (let ((flags (proc-fcntl-get fd proc-F-GETFL)))
                             (proc-fcntl-set fd proc-F-SETFL (fxand flags (fxnot proc-O-NONBLOCK))))
                           (retry))))
                    (else (error 'process "write to child failed" fd))))))))
      #f #f
      ;; closed? goes up BEFORE the free/close/forget below, so it is already #t
      ;; on the far side of the carrier-mutex handoff every woken fiber crosses.
      (lambda ()
        (set-box! closed? #t)
        (sa-foreign-free buf) (proc-c-close fd) (proc-poller-forget! fd)))))

;; What the API hands back for a stream that was INHERITED: the JVM's null
;; streams. Reads are at EOF from the start; writes are accepted and dropped.
(define (proc-null-input-port)
  (open-bytevector-input-port (make-bytevector 0)))
(define (proc-null-output-port)
  (make-custom-binary-output-port "process-null" (lambda (bv start n) n) #f #f #f))

;; The pipe fds are not close-on-exec: macOS has no pipe2()/O_CLOEXEC (unlike
;; Linux), so FD_CLOEXEC can only be applied with a SEPARATE fcntl(F_SETFD)
;; call after pipe() returns -- and that leaves a window, between pipe() and
;; that fcntl call, where a concurrent spawn's child could still inherit the
;; fd and hold a read end open past this child's exit. This is true even now
;; that this file has a working arm64 fcntl binding (proc-fcntl-get/-set
;; above, via the (__varargs_after 2) marker): a working fcntl fixes whether
;; we CAN make the F_SETFD call safely, not the fact that pipe() and
;; F_SETFD are still two separate syscalls with a gap between them. Exclusion
;; by mutex instead: no other fd-level spawn runs between pipe() and
;; posix_spawn.
(define proc-spawn-fd-mutex (make-mutex))

;; --- the child gets its own stdio and nothing else ---------------------------
;; A descriptor with no file action IS the parent's descriptor, and posix_spawn
;; hands the child EVERY one of them: the source files jolt has open, its nREPL
;; listener, a socket a library is serving on. The JVM's child gets the three
;; stdio streams and nothing else, and the difference is load-bearing rather
;; than cosmetic. A child holding a copy of a listening socket keeps that port
;; BOUND after the parent closes it, for as long as the child lives — and an
;; ORPHANED child (parent killed by a test runner's per-namespace timeout, say)
;; holds it for as long as IT lives, so the next run cannot bind the port at all
;; (jolt-fhv, #910: "bind failed on port 54603" behind curl children aged hours).
;;
;; Three ways to say "and close the rest", in preference order:
;;
;;   1. posix_spawn_file_actions_addclosefrom_np(fa, 3), added LAST so it runs
;;      after the dup2s have put the pipes on 0/1/2. One action, one
;;      close_range(2) in the child, and no window at all: the set it closes is
;;      decided in the child, after this process can no longer add to it.
;;   2. POSIX_SPAWN_CLOEXEC_DEFAULT on macOS (proc-cloexec-default?): the kernel
;;      starts the child with everything closed except what a file action names,
;;      so the dup2'd pipes survive and an INHERITED stdio stream is named with
;;      addinherit_np. Decided at spawn time like 1; no snapshot.
;;   3. Where neither resolves — every glibc below 2.34, which is most of the
;;      range jolt's released Linux binary targets — enumerate this process's
;;      own open descriptors (/proc/self/fd on Linux, /dev/fd on BSD) and add one
;;      addclose per fd. The snapshot is taken in the PARENT, so a descriptor
;;      another thread opens between the listing and the spawn is still
;;      inherited; proc-spawn-fd-mutex serialises spawns against each other, not
;;      against the rest of the program. That residual window is one this file
;;      cannot close without the syscall in 1, and it is a far smaller one than
;;      inheriting the whole table.
;;
;; Each enumerated fd is confirmed open with fcntl(F_GETFD) before its action is
;; added, because an addclose of an already-closed fd is a file action that
;; FAILS. The check does not close the window either: a descriptor another
;; thread closes between it and posix_spawn — a sibling's drained pipe, a file, a
;; port a finalizer released — leaves a close action on a dead fd. glibc and musl
;; ignore that in the child (a close below the fd limit that fails is skipped);
;; the Darwin kernel fails the WHOLE spawn with EBADF, which is how three worker
;; threads sharing a ThreadLocal<Process> lost one subprocess to "posix_spawn
;; failed (errno 9)" — and why macOS takes tier 2, where nothing is snapshotted.
;; The listing is itself served by an open directory descriptor which is gone
;; again by the time directory-list returns, so the snapshot always contains at
;; least one such fd. Without a working fcntl there is nothing to confirm with and
;; the fallback declines rather than risk a spawn that cannot exec — that is
;; today's inheritance, which is the honest degradation here.
(define proc-F-GETFD 1)        ; macOS + Linux
(define (proc-fd-live? fd)
  (and proc-fcntl-get (>= (proc-fcntl-get fd proc-F-GETFD) 0)))
(define (proc-open-fd-dir)
  (let loop ((ds '("/proc/self/fd" "/dev/fd")))
    (cond ((null? ds) #f)
          ((guard (e (#t #f)) (file-directory? (car ds))) (car ds))
          (else (loop (cdr ds))))))

;; Tier 1 is the only tier a machine with a new enough glibc would ever run, so
;; the gate could not see tier 2 at all. JOLT_NO_SPAWN_CLOSEFROM=1 makes this
;; process take the enumeration path, and test/chez/process-test.clj re-runs the
;; inheritance case in a child jolt with it set.
(define (proc-closefrom-disabled?)
  (let ((v (getenv "JOLT_NO_SPAWN_CLOSEFROM"))) (and v (not (string=? v "")) #t)))

;; Tier 2: Darwin, with the attribute and inherit entries resolved. The same
;; switch turns it off, so the enumeration fallback stays reachable from a Mac
;; for the gate that exercises it.
(define (proc-cloexec-default?)
  (and (eq? (sa-os-family) 'macos)
       proc-attr-init proc-attr-setflags proc-attr-destroy proc-fa-inherit
       (not (proc-closefrom-disabled?))
       #t))

;; `keep` names the fds that already have a close action of their own (the
;; pipe ends), so the fallback does not add a second one and fail it.
(define (proc-close-inherited-fds! fa keep)
  (if (and proc-fa-closefrom (not (proc-closefrom-disabled?)))
      (proc-fa-closefrom fa 3)
      (let ((dir (proc-open-fd-dir)))
        (when (and dir proc-fcntl-get)
          (for-each
            (lambda (name)
              (let ((fd (string->number name)))
                (when (and fd (integer? fd) (exact? fd) (>= fd 3)
                           (not (memv fd keep)) (proc-fd-live? fd))
                  (proc-fa-close fa fd))))
            (guard (e (#t '())) (directory-list dir)))))))

;; Spawn `/bin/sh -c sh-cmd` with fd-level stdio: an inherited stream gets no
;; file action (the child keeps the parent's descriptor); the rest get pipes.
;; Returns (values stdin-port stdout-port stderr-port pid), #f for inherited
;; ends. attrp is NULL: signal mask and dispositions are inherited, matching
;; the pipe path — so the mask is corrected on this side, around the spawn
;; itself (see the call below).
(define (proc-spawn-fd-level sh-cmd inherit-in? inherit-out? inherit-err?)
  ;; nonblocking-read? / nonblocking-write? -- set O_NONBLOCK unconditionally
  ;; on the PARENT's own RETAINED end at creation (never poller-gated), so
  ;; the retry loops below
  ;; (proc-fd-input-port / proc-fd-output-port) can park a fiber on EAGAIN
  ;; instead of pinning the carrier. Which end is "the parent's own" is
  ;; per-role and asymmetric: for out-p/err-p the parent keeps the read end
  ;; (car) and the child gets the write end (cdr) via dup2, so only
  ;; nonblocking-read? applies; in-p is the mirror image -- the CHILD gets
  ;; the read end (dup2'd to its own stdin) and the PARENT keeps the write
  ;; end (cdr, feeding proc-fd-output-port), so only nonblocking-write?
  ;; applies. Per POSIX, dup2/dup/fork-duplicated descriptors share file
  ;; status flags -- including O_NONBLOCK -- via the shared open file
  ;; description; only per-descriptor flags like FD_CLOEXEC are NOT shared
  ;; (verified standalone: a dup()'d fd inherits O_NONBLOCK from the fd it
  ;; was dup'd from). So flipping O_NONBLOCK on the end that gets dup2'd into
  ;; the child would silently change the CHILD's own stdio behavior, not just
  ;; the parent's -- a non-blocking in-p read end makes the child's own
  ;; stdin reads see spurious EAGAIN; a non-blocking out-p/err-p write end
  ;; would do the same to the child's own stdout/stderr writes. Each of the
  ;; three pipes below only ever sets non-blocking on the end the parent
  ;; itself retains.
  (define (mk-pipe nonblocking-read? nonblocking-write?)
    (let ((fds (sa-foreign-alloc 8)))
      (if (= 0 (proc-c-pipe fds))
          (let ((p (cons (sa-foreign-ref 'int fds 0) (sa-foreign-ref 'int fds 4))))
            (sa-foreign-free fds)
            (when nonblocking-read? (proc-set-nonblocking! (car p)))
            (when nonblocking-write? (proc-set-nonblocking! (cdr p)))
            p)
          (begin (sa-foreign-free fds)
                 (throw-jvm (quote java.io.IOException) "pipe: cannot allocate")))))
  ;; spawn-locked hands back RAW FDS and the ports are built after it returns,
  ;; not inside. proc-fd-input-port / proc-fd-output-port can park now (an EAGAIN on
  ;; a fiber waits on jolt.io-poller, and the first such wait may autoload it),
  ;; and a park inside a jolt-with-mutex region is the deadlock shape
  ;; test/parkcheck refuses outright -- host/chez/locks.ss. Only their
  ;; read!/write! callbacks can actually reach a park, and those run when
  ;; someone reads the port rather than here, so nothing was wrong before; but
  ;; the lock has no reason to span the construction either, and its own
  ;; contract is pipe() through posix_spawn and nothing else. Narrowing it makes
  ;; the call graph say what is true instead of asking for an exemption.
  (define (spawn-locked)
    (jolt-with-mutex proc-spawn-fd-mutex
      (let* ((in-p  (and (not inherit-in?)  (mk-pipe #f #t)))
             (out-p (and (not inherit-out?) (mk-pipe #t #f)))
             (err-p (and (not inherit-err?) (mk-pipe #t #f)))
             (fa (sa-foreign-alloc 128))
             ;; posix_spawnattr_t is one pointer on Darwin, the only place this
             ;; is allocated; 64 bytes leaves room for a wider layout regardless.
             (attr (and (proc-cloexec-default?) (sa-foreign-alloc 64)))
             (pidbuf (sa-foreign-alloc 8)))
        (proc-fa-init fa)
        (when attr
          (proc-attr-init attr)
          (proc-attr-setflags attr proc-POSIX-SPAWN-CLOEXEC-DEFAULT)
          ;; An inherited stream has no dup2 naming it, so under the flag it
          ;; would be closed with everything else: name it.
          (when inherit-in?  (proc-fa-inherit fa 0))
          (when inherit-out? (proc-fa-inherit fa 1))
          (when inherit-err? (proc-fa-inherit fa 2)))
        (when in-p  (proc-fa-dup2 fa (car in-p) 0)
                    (proc-fa-close fa (car in-p)) (proc-fa-close fa (cdr in-p)))
        (when out-p (proc-fa-dup2 fa (cdr out-p) 1)
                    (proc-fa-close fa (cdr out-p)) (proc-fa-close fa (car out-p)))
        (when err-p (proc-fa-dup2 fa (cdr err-p) 2)
                    (proc-fa-close fa (cdr err-p)) (proc-fa-close fa (car err-p)))
        ;; LAST of the file actions, so the dup2s above have already moved the
        ;; child's ends onto 0/1/2 by the time everything else goes. Under
        ;; CLOEXEC_DEFAULT the kernel does this part.
        (unless attr
          (proc-close-inherited-fds!
            fa (append (if in-p  (list (car in-p)  (cdr in-p))  '())
                       (if out-p (list (car out-p) (cdr out-p)) '())
                       (if err-p (list (car err-p) (cdr err-p)) '()))))
        (let* ((argv (proc-marshal-argv (list "/bin/sh" "-c" sh-cmd)))
               (envp (proc-marshal-argv
                      (map (lambda (p) (string-append (car p) "=" (cdr p)))
                           (proc-child-env-pairs))))
               ;; attrp is NULL — or carries only the CLOEXEC_DEFAULT flag, never
               ;; SETSIGMASK — so the child inherits this thread's signal mask,
               ;; which must carry none of jolt's own blocking (concurrency.ss).
               (rc (jolt-with-empty-sigmask
                     (lambda () (proc-c-spawn pidbuf "/bin/sh" fa (or attr 0) (car argv) (car envp)))))
               (pid (sa-foreign-ref 'int pidbuf 0)))
          (proc-fa-destroy fa)
          (when attr (proc-attr-destroy attr) (sa-foreign-free attr))
          (sa-foreign-free fa) (sa-foreign-free pidbuf)
          (proc-free-argv argv) (proc-free-argv envp)
          ;; parent side: the child's pipe ends close unconditionally; on a failed
          ;; spawn the parent ends close too, before the throw.
          (when in-p  (proc-c-close (car in-p)))
          (when out-p (proc-c-close (cdr out-p)))
          (when err-p (proc-c-close (cdr err-p)))
          (if (= rc 0)
              (vector (and in-p  (cdr in-p))
                      (and out-p (car out-p))
                      (and err-p (car err-p))
                      pid)
              (begin
                (when in-p  (proc-c-close (cdr in-p)))
                (when out-p (proc-c-close (car out-p)))
                (when err-p (proc-c-close (car err-p)))
                (throw-jvm (quote java.io.IOException)
                  (string-append "posix_spawn failed (errno " (number->string rc) ")"))))))))
  (let ((fds (spawn-locked)))
    (values (let ((fd (vector-ref fds 0))) (and fd (proc-fd-output-port fd)))
            (let ((fd (vector-ref fds 1))) (and fd (proc-fd-input-port fd)))
            (let ((fd (vector-ref fds 2))) (and fd (proc-fd-input-port fd)))
            (vector-ref fds 3))))

;; --- java.lang.Process -------------------------------------------------------
;; state: #(stdin-os stdout-is stderr-is pid exit-box cmd mutex stdout-port
;;          stdin-port inherit-latches signalled-box)
(define (proc-p-stdin-os st)   (vector-ref (jhost-state st) 0))
(define (proc-p-stdout-is st)  (vector-ref (jhost-state st) 1))
(define (proc-p-stderr-is st)  (vector-ref (jhost-state st) 2))
(define (proc-p-pid st)        (vector-ref (jhost-state st) 3))
(define (proc-p-exit-box st)   (vector-ref (jhost-state st) 4))
(define (proc-p-mutex st)      (vector-ref (jhost-state st) 6))
(define (proc-p-stdout-port st) (vector-ref (jhost-state st) 7))
(define (proc-p-stdin-port st)  (vector-ref (jhost-state st) 8))
(define (proc-p-inherit-latches st) (vector-ref (jhost-state st) 9))
;; The 128+signal status of a signal WE sent, if any — the one recoverable answer
;; when the child turns out to be unwaitable (see proc-lost-status).
(define (proc-p-signalled st)  (vector-ref (jhost-state st) 10))

;; ProcessBuilder.start resolves the program before spawning and throws
;; IOException("…No such file or directory") when it can't be found; our shell
;; would otherwise fail at exec (127) with a different message. Mirror it:
;;   - absolute program: the file must exist
;;   - slash-bearing relative program: resolves against the child cwd, like exec
;;   - bare name: an entry of that name must be on PATH
(define (proc-path-join a b)
  (if (or (= (string-length a) 0) (char=? (string-ref a (- (string-length a) 1)) #\/))
      (string-append a b)
      (string-append a "/" b)))
(define (proc-has-slash? s)
  (let loop ((i 0)) (cond ((= i (string-length s)) #f)
                          ((char=? (string-ref s i) #\/) #t)
                          (else (loop (+ i 1))))))
(define (proc-on-path? prog)
  (let ((path (getenv "PATH")))
    (and path
         (let loop ((dirs (str-literal-split path ":")))
           (cond ((null? dirs) #f)
                 ((and (> (string-length (car dirs)) 0)
                       (file-exists? (proc-path-join (car dirs) prog))) #t)
                 (else (loop (cdr dirs))))))))
(define (proc-program-resolvable? prog effective-dir)
  (let ((prog (if (string? prog) prog (jolt-str-render-one prog))))
    (cond
      ((= (string-length prog) 0) #f)
      ((char=? (string-ref prog 0) #\/) (file-exists? prog))
      ((proc-has-slash? prog)
       (file-exists? (proc-path-join (or effective-dir (getenv "JOLT_PWD") ".") prog)))
      (else (proc-on-path? prog)))))

(define (proc-pb-start self)
  (let* ((st (jhost-state self))
         (cmd (proc-pb-cmd self)))
    (when (and (pair? cmd)
               (not (proc-program-resolvable? (car cmd) (proc-effective-dir (proc-pb-dir self)))))
      (throw-jvm (quote java.io.IOException)
        (string-append "Cannot run program \""
                       (if (string? (car cmd)) (car cmd) (jolt-str-render-one (car cmd)))
                       "\": error=2, No such file or directory")))
    (proc-ensure-reapable!)
    (let* ((rin  (proc-pb-redir-in self))
           (rout (proc-pb-redir-out self))
           (rerr (proc-pb-redir-err self))
           (inherit? (lambda (r) (and (proc-redirect? r) (eq? (proc-redirect-kind r) 'inherit)))))
      ;; posix_spawn drives EVERY spawn where the FFI surface exists, not only the
      ;; ones with an INHERIT stream — it already pipes each stream that is not
      ;; inherited, so the two differ in how the child is created, not in what it
      ;; gets. Chez's fork leaves SIGINT set to SIG_IGN in the child, which no
      ;; amount of care on this side can undo (it happens between its fork and its
      ;; exec), and a child that cannot be interrupted by ^C is not what
      ;; ProcessBuilder hands you anywhere else. That is the system(3) leak the
      ;; convention exists to avoid, not the convention (jolt-a4hs); through
      ;; posix_spawn a child's dispositions match a plain shell's exactly.
      ;; The fork path stays as the fallback for machine types without the FFI,
      ;; INHERIT emulation and all.
      (if proc-spawn-fd-ok?
          ;; An INHERITED stream means the child writes jolt's real descriptors,
          ;; so anything jolt has buffered must land first to keep its order.
          (begin
            (when (or (inherit? rout) (inherit? rerr))
              (guard (e (#t #f)) (flush-output-port (current-output-port)))
              (guard (e (#t #f)) (flush-output-port (current-error-port))))
            (call-with-values
              (lambda () (proc-spawn-fd-level (proc-build-shell-command self)
                                              (inherit? rin) (inherit? rout) (inherit? rerr)))
              (lambda (cin cout cerr pid)
                (let* ((child-stdin  (or cin  (proc-null-output-port)))
                       (child-stdout (or cout (proc-null-input-port)))
                       (child-stderr (or cerr (proc-null-input-port)))
                       (pst (vector (make-out-stream child-stdin)
                                    (make-in-stream child-stdout)
                                    (make-in-stream child-stderr)
                                    pid (box #f) (proc-pb-cmd self) (make-mutex)
                                    child-stdout child-stdin (box '()) (box #f))))
                  (make-jhost "process" pst)))))
          (call-with-values
            ;; Chez forks for this one, so the child inherits this thread's
            ;; signal mask just as the posix_spawn path does. (It also leaves
            ;; SIGINT ignored in the child, which is why this is the fallback.)
            (lambda () (jolt-with-empty-sigmask
                         (lambda () (sa-run-process (proc-build-shell-command self) #f))))
            (lambda (child-stdin child-stdout child-stderr pid)
              (let* ((latches (box '()))
                     (pst (vector (make-out-stream child-stdin)
                                  (make-in-stream child-stdout)
                                  (make-in-stream child-stderr)
                                  pid (box #f) (proc-pb-cmd self) (make-mutex)
                                  child-stdout child-stdin latches (box #f))))
                ;; INHERIT emulation fallback (no posix_spawn FFI): pump between
                ;; the pipe and jolt's own stdio. The output pumps are latched so
                ;; waitFor can join them — INHERIT output must be flushed before
                ;; the process is reported finished.
                (when (inherit? rin)  (proc-pump (current-input-port) child-stdin #t))
                (when (inherit? rout) (set-box! latches (cons (proc-pump child-stdout (current-output-port) #f) (unbox latches))))
                (when (inherit? rerr) (set-box! latches (cons (proc-pump child-stderr (current-error-port) #f) (unbox latches))))
                (make-jhost "process" pst))))))))

;; Block until the process exits, caching and returning the decoded status. Any
;; INHERIT output pumps are joined first, so all forwarded output has landed by
;; the time the exit status is returned (matching fd-level INHERIT).
;; EVERY branch here has to reach a decision. This loop runs while holding the
;; process mutex, so a branch that retries forever does not merely spin — it
;; deadlocks every other method on the process, silently and for as long as the
;; caller is willing to wait. That is what sat on a CI gate for 3h42m (jolt-pgbh):
;; a waitpid failing with ECHILD fell into the `else` retry, which could never
;; succeed. Only EINTR is retried, because only EINTR means "ask again".
;;
;; POLLS with WNOHANG rather than issuing a blocking waitpid, because a blocking
;; one can park in the KERNEL forever, where no amount of care in this loop
;; reaches it: when SIGCHLD is SIG_IGN, POSIX has wait block until EVERY child has
;; terminated before failing with ECHILD, and a child that became a zombie before
;; the disposition changed leaves it parked indefinitely. A program that sets
;; SIG_IGN itself, or inherits it, then called .waitFor and hung with the whole
;; process at 0% CPU — the same shape as jolt-pgbh, one level lower down.
;; proc-wait-timed already polls for exactly this reason; this is the last caller
;; that did not. Backs off 0.2ms -> 10ms so a short-lived child is still reaped
;; promptly while a long-lived one costs ~100 wakeups a second.
(define proc-poll-step-max 10)                   ; 10ms
;; ONE reap attempt, under the mutex. -> the exit status, or #f meaning "ask again".
;; The mutex is what stops two callers reaping the same child at once, and one
;; attempt is all it has to cover: the exit-box is written under it and read under
;; it, so a caller that loses the race sees the winner's answer on its next pass.
(define (proc-reap-once st)
  (jolt-with-mutex (proc-p-mutex st)
    (or (unbox (proc-p-exit-box st))
        (call-with-values (lambda () (proc-waitpid-once (proc-p-pid st) #t))
          (lambda (rc decoded err)
            (cond
              ((and decoded (= rc (proc-p-pid st)))
               (set-box! (proc-p-exit-box st) decoded) decoded)
              ;; another caller reaped it between our check and our wait
              ((unbox (proc-p-exit-box st)))
              ;; still running (WNOHANG rc = 0), or merely interrupted: both mean
              ;; "ask again", which is #f here and a pause in the caller.
              ((or (= rc 0) (and (< rc 0) (= err proc-EINTR))) #f)
              ;; unwaitable (ECHILD) or waitpid unavailable — no number of retries
              ;; changes that.
              (else (let ((c (proc-lost-status st)))
                      (set-box! (proc-p-exit-box st) c) c))))))))

;; THE PAUSE IS OUTSIDE THE MUTEX, and the loop is out here with it. This used to
;; hold proc-p-mutex across the entire poll, which is for as long as the child runs.
;; Two things were wrong with that and the second is the sharper: a counted lock held
;; for an unbounded time makes the whole CARRIER unpreemptible, because the scheduler
;; refuses to preempt a fiber while its carrier holds one — so a `.waitFor` from a go
;; block froze every fiber on that carrier for the life of the subprocess, out of
;; reach of even the preemption that is supposed to be the backstop. And the pause
;; itself slept the carrier, so a fiber could not have parked in there anyway
;; (jolt-x1no). Now the lock covers one waitpid attempt, and jolt-pause-ms parks a
;; fiber between attempts while a thread sleeps.
;;
;; The interrupt check is per ROUND rather than a registration, because this is a
;; poll and not a condition wait: there is no cv for .interrupt to poke, so the flag
;; is simply read (and cleared) each time round, which is the same
;; check-clear-throw jolt-cv-wait-interruptibly does at the top of its decide.
;; Process.waitFor throws InterruptedException on the JVM.
(define (proc-wait-blocking st)
  (let ((code
          (let loop ((step 1))                   ; 1ms
            (or (proc-reap-once st)
                (begin
                  (jolt-interrupt-poll-check! "Process.waitFor")
                  (jolt-pause-ms step)
                  (loop (min proc-poll-step-max (* step 2))))))))
    (for-each proc-latch-wait (unbox (proc-p-inherit-latches st)))
    code))

;; Non-blocking liveness poll (reaps and caches on exit). An unwaitable child is
;; reported dead AND has its status cached, so a waitFor after it cannot go looking
;; for a child that will never be there.
(define (proc-alive? st)
  (jolt-with-mutex (proc-p-mutex st)
    (if (unbox (proc-p-exit-box st)) #f
        (call-with-values (lambda () (proc-waitpid-once (proc-p-pid st) #t))
          (lambda (rc decoded err)
            (cond ((= rc 0) #t)                      ; still running
                  (decoded (set-box! (proc-p-exit-box st) decoded) #f)
                  ((and (< rc 0) (= err proc-EINTR)) #t)   ; no answer yet, assume alive
                  (else (set-box! (proc-p-exit-box st) (proc-lost-status st)) #f)))))))

;; Records a terminating signal we sent, so proc-lost-status can still give the
;; right answer for a child that something else reaps before we get to it.
(define (proc-signal st sig)
  (when proc-kill
    (proc-kill (proc-p-pid st) sig)
    (when (or (= sig proc-SIGTERM) (= sig proc-SIGKILL))
      (set-box! (proc-p-signalled st) (+ 128 sig))))
  st)

(register-host-methods! "process"
  (list (cons "getOutputStream" (lambda (self) (proc-p-stdin-os self)))
        (cons "getInputStream"  (lambda (self) (proc-p-stdout-is self)))
        (cons "getErrorStream"  (lambda (self) (proc-p-stderr-is self)))
        (cons "pid"             (lambda (self) (->num (proc-p-pid self))))
        (cons "isAlive"         (lambda (self) (proc-alive? self)))
        (cons "destroy"         (lambda (self) (proc-signal self proc-SIGTERM) jolt-nil))
        (cons "destroyForcibly" (lambda (self) (proc-signal self proc-SIGKILL) self))
        (cons "waitFor" (lambda (self . args)
          (if (null? args)
              (->num (proc-wait-blocking self))
              ;; (waitFor timeout unit), scaled by the unit through the one
              ;; conversion every (timeout, unit) method uses (concurrency.ss).
              ;; This read the amount as milliseconds whatever the unit said —
              ;; babashka passes MILLISECONDS, so it never showed there — and a
              ;; (.waitFor p 10 SECONDS) gave the child 10ms.
              (proc-wait-timed self (tu-args->ms args)))))
        (cons "exitValue" (lambda (self)
          (jolt-with-mutex (proc-p-mutex self)
            (or (unbox (proc-p-exit-box self))
                (call-with-values (lambda () (proc-waitpid-once (proc-p-pid self) #t))
                  (lambda (rc decoded err)
                    (cond (decoded (set-box! (proc-p-exit-box self) decoded) (->num decoded))
                          ;; unwaitable: it HAS exited (something else reaped it), so
                          ;; report the recoverable status rather than claiming it is
                          ;; still running — exitValue would otherwise throw forever.
                          ((and (< rc 0) (not (= err proc-EINTR)))
                           (let ((c (proc-lost-status self)))
                             (set-box! (proc-p-exit-box self) c) (->num c)))
                          (else (throw-jvm (quote IllegalThreadStateException) "process has not exited")))))))))
        (cons "toHandle" (lambda (self) (make-proc-handle (proc-p-pid self))))
        (cons "onExit"   (lambda (self) (make-proc-completable self)))
        (cons "toString" (lambda (self) (string-append "#<Process pid=" (number->string (proc-p-pid self)) ">")))))

;; timed waitFor -> #t if exited within `ms`, else #f (polls at ~10ms).
(define (proc-wait-timed st ms)
  (let ((step 10))
    (let loop ((remaining ms))
      (cond ((not (proc-alive? st)) #t)
            ((<= remaining 0) #f)
            ;; a fiber parks for the step rather than sleeping its carrier
            (else (jolt-interrupt-poll-check! "Process.waitFor")
                  (jolt-pause-ms step)
                  (loop (- remaining step)))))))

;; --- java.lang.ProcessHandle (destroy-tree) ----------------------------------
;; descendants asks the OS for the live tree under a pid — jolt tracks nothing
;; itself. Darwin: libproc's proc_listchildpids per pid (returns a COUNT of
;; pid_t entries; the size argument is in BYTES — probed, jolt-hpdu). Linux: one
;; pass over /proc/*/stat building ppid -> pids, then a walk from the root.
;; Windows (no arm here) and a missing entry point answer empty — the old
;; behavior, where destroy-tree reduces to destroy. babashka.process's
;; destroy-tree destroys (cons handle descendants), so a real answer here is
;; what makes killing a wrapper also kill the work it spawned: `lake env repl`
;; killed via :shutdown destroy-tree used to leave the repl grandchild running
;; as an orphan.
(define proc-listchildpids
  (and (eq? (sa-os-family) 'macos)
       (jolt-foreign-proc-safe "proc_listchildpids" '(int u8* int) 'int)))

;; Direct children via libproc. A count equal to the capacity may be a truncated
;; answer: retry doubled, up to a cap no real tree reaches.
(define (proc-children-darwin pid)
  (let loop ((cap 256))
    (let* ((buf (make-bytevector (* cap 4) 0))
           (n (proc-listchildpids pid buf (* cap 4))))
      (cond ((or (not (fixnum? n)) (<= n 0)) '())
            ((and (>= n cap) (<= cap 65536)) (loop (* cap 2)))
            (else (let col ((i 0) (acc '()))
                    (if (= i n) acc
                        (col (+ i 1) (cons (bytevector-s32-native-ref buf (* i 4)) acc)))))))))

;; Linux: /proc/<pid>/stat is "pid (comm) state ppid ..." and comm may contain
;; spaces and ')', so the ppid is parsed after the LAST ')'. Each read is
;; guarded — a process is free to exit between the listing and the read.
(define (proc-linux-ppid-map)
  (let ((tbl (make-eqv-hashtable)))
    (for-each
      (lambda (name)
        (let ((pid (string->number name)))
          (when (fixnum? pid)
            (guard (e (#t #f))
              (let* ((s (call-with-port (open-input-file (string-append "/proc/" name "/stat"))
                          get-string-all))
                     (rp (let scan ((i (- (string-length s) 1)))
                           (cond ((< i 0) #f)
                                 ((char=? (string-ref s i) #\)) i)
                                 (else (scan (- i 1))))))
                     (ppid (and rp
                                (let ((parts (filter (lambda (t) (> (string-length t) 0))
                                                     (str-literal-split
                                                       (substring s (+ rp 1) (string-length s)) " "))))
                                  (and (pair? parts) (pair? (cdr parts))
                                       (string->number (cadr parts)))))))
                (when (fixnum? ppid)
                  (hashtable-set! tbl ppid (cons pid (hashtable-ref tbl ppid '())))))))))
      (guard (e (#t '())) (directory-list "/proc")))
    tbl))

;; All live descendants of pid, depth-first. State is per-call (nothing shared
;; across threads); the seen set stops a walk that pid reuse made cyclic.
(define (proc-descendants pid)
  (let ((kids (cond (proc-listchildpids proc-children-darwin)
                    ((eq? (sa-os-family) 'linux)
                     (let ((tbl (proc-linux-ppid-map)))
                       (lambda (p) (hashtable-ref tbl p '()))))
                    (else (lambda (p) '()))))
        (seen (make-eqv-hashtable)))
    (let walk ((frontier (kids pid)) (acc '()))
      (cond ((null? frontier) (reverse acc))
            ((hashtable-ref seen (car frontier) #f) (walk (cdr frontier) acc))
            (else
             (hashtable-set! seen (car frontier) #t)
             (walk (append (kids (car frontier)) (cdr frontier))
                   (cons (car frontier) acc)))))))

(define (make-proc-handle pid) (make-jhost "process-handle" pid))
(register-host-methods! "process-handle"
  (list (cons "destroy" (lambda (self) (when proc-kill (proc-kill (jhost-state self) proc-SIGTERM)) #t))
        (cons "pid"     (lambda (self) (->num (jhost-state self))))
        (cons "descendants" (lambda (self)
          (apply jolt-vector (map make-proc-handle (proc-descendants (jhost-state self))))))))

;; --- CompletableFuture (Process.onExit().thenRun(f)) -------------------------
;; A minimal one-shot: thenRun spawns a thread that waits for the process to exit
;; and then runs the callback. Enough for babashka's :shutdown / :exit-fn hooks.
(define (make-proc-completable proc-st) (make-jhost "jolt-completable" proc-st))
(register-host-methods! "jolt-completable"
  (list (cons "thenRun" (lambda (self f)
          (let ((proc-st (jhost-state self)))
            (fork-thread (lambda () (guard (e (#t #f)) (proc-wait-blocking proc-st) (jolt-invoke f)))))
          self))
        (cons "thenApply" (lambda (self f) self))))

;; --- java.lang.Runtime shutdown hooks ----------------------------------------
;; addShutdownHook registers a Thread hook to run at jolt exit; babashka.process's
;; `:shutdown` option registers one to kill the child if jolt dies mid-run.
;; The registry, the runner and the SIGTERM/SIGHUP watcher all live in
;; concurrency.ss — these are the JVM-shaped door onto them.
(define the-jolt-runtime (make-jhost "jolt-runtime" #f))

;; A String[] / collection of strings -> a Scheme list of strings; a lone String
;; is whitespace-split (Runtime.exec(String) tokenizes on whitespace).
(define (proc-strings->list x)
  (if (string? x)
      (filter (lambda (s) (> (string-length s) 0))
              (str-literal-split x " "))
      (map jolt-str-render-one (seq->list (jolt-seq x)))))
;; envp: a String[] of "K=V" -> a fully-specified env-map (no parent seed), so it
;; reproduces exactly the given environment (Runtime.exec envp semantics).
(define (make-proc-env-from-strings envp)
  (let ((h (make-hashtable string-hash string=?)))
    (for-each (lambda (kv)
                (let* ((s (jolt-str-render-one kv))
                       (eq (let scan ((i 0))
                             (cond ((= i (string-length s)) #f)
                                   ((char=? (string-ref s i) #\=) i)
                                   (else (scan (+ i 1)))))))
                  (when eq (hashtable-set! h (substring s 0 eq) (substring s (+ eq 1) (string-length s))))))
              (seq->list (jolt-seq envp)))
    (make-jhost "jolt-env-map" h)))

;; Runtime.exec(cmdarray [envp [dir]]): the classic spawn API clojure.java.shell
;; uses. envp/dir may be nil (inherit / cwd). Returns a Process.
(define (proc-runtime-exec args)
  (let* ((cmd  (proc-strings->list (car args)))
         (envp (and (pair? (cdr args)) (cadr args)))
         (dir  (and (pair? (cdr args)) (pair? (cddr args)) (caddr args)))
         (pb   (make-proc-builder cmd)))
    (when (and envp (not (jolt-nil? envp))) (proc-pb-set! pb 1 (make-proc-env-from-strings envp)))
    (when (and dir  (not (jolt-nil? dir)))  (proc-pb-set! pb 2 (file-path-of dir)))
    (proc-pb-start pb)))

(register-host-methods! "jolt-runtime"
  (list (cons "addShutdownHook"
          (lambda (self hook)
            (jolt-register-shutdown-hook! hook
              (lambda () (let ((body (jolt-thread-body hook))) (when body (jolt-invoke body)))))
            jolt-nil))
        (cons "removeShutdownHook"
          (lambda (self hook) (jolt-remove-shutdown-hook! hook)))
        (cons "availableProcessors" (lambda (self) (->num (jolt-available-processors))))
        ;; The memory trio, over Chez's own heap accounting: current-memory-bytes
        ;; is what the collector has reserved from the OS (the JVM's totalMemory)
        ;; and bytes-allocated is what is live inside it, so free is the
        ;; difference. criterium reads all four for its report, and without them a
        ;; benchmark namespace crashes rather than running.
        (cons "totalMemory" (lambda (self) (->num (sa-total-memory-bytes))))
        (cons "freeMemory"
          (lambda (self) (->num (max 0 (- (sa-total-memory-bytes) (sa-bytes-allocated))))))
        ;; maxMemory is -Xmx on the JVM. jolt has a ceiling of its own now
        ;; (rt.ss jolt-install-heap-ceiling!, 25% of RAM by default, the same
        ;; share MaxRAMPercentage uses), so report that. Long/MAX_VALUE is still
        ;; the answer under JOLT_MAX_HEAP=off, which is what unbounded means and
        ;; what every release before 0.8.5 reported unconditionally.
        (cons "maxMemory" (lambda (self)
                            (->num (or (jolt-heap-max-bytes) 9223372036854775807))))
        ;; Runtime.gc routes to System/gc on the JVM, so it gets the same guarded
        ;; hint semantics — Chez's collect refuses while multiple threads are live,
        ;; and neither of these ever throws on the JVM.
        (cons "gc" (lambda (self)
                     (guard (e (#t #f)) (sa-gc-collect))
                     jolt-nil))
        ;; No finalizers on this host, so running them is genuinely a no-op — which
        ;; is also all the JVM promises (a hint, deprecated for removal since 18).
        (cons "runFinalization" (lambda (self) jolt-nil))
        (cons "exec" (lambda (self . args) (proc-runtime-exec args)))))
(register-class-statics! "java.lang.Runtime" (list (cons "getRuntime" (lambda () the-jolt-runtime))))

;; instance? and (class x) for the ProcessBuilder / Process / Redirect shims are
;; DERIVED from the jhost-tag->fqn rows in class-hierarchy.ss (via the arms in
;; host-static-classes.ss) — no per-class instance-check arm here.
