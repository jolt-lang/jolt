;; java.lang.ProcessBuilder / java.lang.Process over Chez's open-process-ports.
;;
;; babashka.process (vendored under jolt.process) is built entirely on the JVM
;; ProcessBuilder / Process API; this file provides that surface so the library
;; runs unmodified. A subprocess is spawned by open-process-ports, which forks a
;; `/bin/sh -c CMD` and hands back binary stdin/stdout/stderr ports plus the pid.
;; We drive it as ProcessBuilder does:
;;
;;   - the argv list is shell-quoted and `exec`'d, so the shell performs no word
;;     splitting or globbing (matching ProcessBuilder, which execs directly) and
;;     the pid is the target program, not the intermediate sh.
;;   - :dir  -> `cd 'DIR' &&` prefix
;;   - :env  -> `env -i K=V …` prefix (the env map starts as a copy of the parent
;;     environment, so `env -i` reproduces exactly the intended set)
;;   - file / discard redirects -> shell `1> 'f'` / `2>> 'f'` / `1>/dev/null`
;;   - INHERIT / stream redirects -> a pump thread copying between the pipe and
;;     the jolt process's own stdio (open-process-ports always pipes, so fd-level
;;     inheritance is emulated; the fidelity gap is a documented divergence).
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
;; loop). Both spellings of the location accessor: Darwin/BSD, then glibc/musl.
(define proc-errno-loc
  (or (jolt-foreign-proc-safe "__error" '() 'void*)
      (jolt-foreign-proc-safe "__errno_location" '() 'void*)))
(define (proc-errno)
  (if proc-errno-loc (guard (e (#t 0)) (foreign-ref 'int (proc-errno-loc) 0)) 0))

(define proc-WNOHANG 1)        ; macOS + Linux
(define proc-SIGTERM 15)
(define proc-SIGKILL 9)
(define proc-EINTR 4)          ; macOS + Linux
(define proc-ECHILD 10)        ; macOS + Linux
;; SIGCHLD is 20 on Darwin/BSD and 17 on Linux. Same machine-type test as
;; concurrency.ss uses for the SIG_BLOCK numerics.
(define proc-SIGCHLD
  (let* ((s (symbol->string (machine-type))) (n (string-length s)))
    (let loop ((i 0))
      (cond ((> (+ i 3) n) 17)                            ; default: Linux
            ((string=? (substring s i (+ i 3)) "osx") 20) ; Darwin/macOS
            (else (loop (+ i 1)))))))

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
      (let ((buf (foreign-alloc 4)))
        (let* ((rc (proc-waitpid pid buf (if nohang? proc-WNOHANG 0)))
               (err (if (< rc 0) (proc-errno) 0))
               (raw (foreign-ref 'int buf 0)))
          (foreign-free buf)
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

(define proc-redirect-statics
  (list (cons "INHERIT" (make-proc-redirect 'inherit #f))
        (cons "DISCARD" (make-proc-redirect 'discard #f))
        (cons "PIPE"    (make-proc-redirect 'pipe #f))
        (cons "to"       (lambda (f) (make-proc-redirect 'write  (file-path-of f))))
        (cons "appendTo" (lambda (f) (make-proc-redirect 'append (file-path-of f))))
        (cons "from"     (lambda (f) (make-proc-redirect 'read   (file-path-of f))))))
;; register-class-statics! mirrors the FQN table to the short name, so a single
;; call serves both java.lang.ProcessBuilder$Redirect/… and ProcessBuilder$Redirect/….
(register-class-statics! "java.lang.ProcessBuilder$Redirect" proc-redirect-statics)
(register-host-methods! "process-redirect"
  (list (cons "type" (lambda (self) (symbol->string (proc-redirect-kind self))))
        (cons "toString" (lambda (self) (string-append "Redirect." (symbol->string (proc-redirect-kind self)))))))

;; --- environment map (ProcessBuilder.environment()) --------------------------
;; A live mutable Map<String,String>, seeded from the parent environment. jolt's
;; babashka.process only calls clear/putAll, but put/get/remove are provided too.
;; state: a Scheme string->string hashtable.
(define (make-proc-env-map)
  (let ((h (make-hashtable string-hash string=?)))
    (for-each (lambda (p) (hashtable-set! h (car p) (cdr p))) (all-env-pairs))
    (make-jhost "jolt-env-map" h)))
(define (proc-env-map? x) (and (jhost? x) (string=? (jhost-tag x) "jolt-env-map")))
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
(define (proc-builder? x) (and (jhost? x) (string=? (jhost-tag x) "process-builder")))
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
        (cons "redirectInput"  (lambda (self r) (proc-pb-set! self 3 r) self))
        (cons "redirectOutput" (lambda (self r) (proc-pb-set! self 4 r) self))
        (cons "redirectError"  (lambda (self r) (proc-pb-set! self 5 r) self))
        (cons "redirectErrorStream" (lambda (self b) (proc-pb-set! self 6 (jolt-truthy? b)) self))
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
        (with-mutex m (set-box! done #t) (condition-broadcast c))))
    (vector m c done)))
(define (proc-latch-wait latch)
  (with-mutex (vector-ref latch 0)
    (let loop () (unless (unbox (vector-ref latch 2))
                   (condition-wait (vector-ref latch 1) (vector-ref latch 0)) (loop)))))

;; --- java.lang.Process -------------------------------------------------------
;; state: #(stdin-os stdout-is stderr-is pid exit-box cmd mutex stdout-port
;;          stdin-port inherit-latches signalled-box)
(define (proc-p-stdin-os st)   (vector-ref (jhost-state st) 0))
(define (proc-p-stdout-is st)  (vector-ref (jhost-state st) 1))
(define (proc-p-stderr-is st)  (vector-ref (jhost-state st) 2))
(define (proc-p-pid st)        (vector-ref (jhost-state st) 3))
(define (proc-p-exit-box st)   (vector-ref (jhost-state st) 4))
(define (proc-p-cmd st)        (vector-ref (jhost-state st) 5))
(define (proc-p-mutex st)      (vector-ref (jhost-state st) 6))
(define (proc-p-stdout-port st) (vector-ref (jhost-state st) 7))
(define (proc-p-stdin-port st)  (vector-ref (jhost-state st) 8))
(define (proc-p-inherit-latches st) (vector-ref (jhost-state st) 9))
;; The 128+signal status of a signal WE sent, if any — the one recoverable answer
;; when the child turns out to be unwaitable (see proc-lost-status).
(define (proc-p-signalled st)  (vector-ref (jhost-state st) 10))
(define (proc-process? x) (and (jhost? x) (string=? (jhost-tag x) "process")))

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
    (call-with-values
      (lambda () (open-process-ports (proc-build-shell-command self) (buffer-mode block) #f))
      (lambda (child-stdin child-stdout child-stderr pid)
        (let* ((rin  (proc-pb-redir-in self))
               (rout (proc-pb-redir-out self))
               (rerr (proc-pb-redir-err self))
               (inherit? (lambda (r) (and (proc-redirect? r) (eq? (proc-redirect-kind r) 'inherit))))
               (latches (box '()))
               (pst (vector (make-out-stream child-stdin)
                            (make-in-stream child-stdout)
                            (make-in-stream child-stderr)
                            pid (box #f) (proc-pb-cmd self) (make-mutex)
                            child-stdout child-stdin latches (box #f))))
          ;; INHERIT emulation: pump between the pipe and jolt's own stdio. The
          ;; output pumps are latched so waitFor can join them — INHERIT output
          ;; must be flushed before the process is reported finished.
          (when (inherit? rin)  (proc-pump (current-input-port) child-stdin #t))
          (when (inherit? rout) (set-box! latches (cons (proc-pump child-stdout (current-output-port) #f) (unbox latches))))
          (when (inherit? rerr) (set-box! latches (cons (proc-pump child-stderr (current-error-port) #f) (unbox latches))))
          (make-jhost "process" pst))))))

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
(define proc-poll-step-max (* 10 1000000))       ; 10ms, in nanoseconds
(define (proc-wait-blocking st)
  (let ((code
          (with-mutex (proc-p-mutex st)
            (or (unbox (proc-p-exit-box st))
                (let loop ((step 200000))        ; 0.2ms
                  (call-with-values (lambda () (proc-waitpid-once (proc-p-pid st) #t))
                    (lambda (rc decoded err)
                      (cond
                        ((and decoded (= rc (proc-p-pid st)))
                         (set-box! (proc-p-exit-box st) decoded) decoded)
                        ;; another caller reaped it between our check and our wait
                        ((unbox (proc-p-exit-box st)))
                        ;; still running (WNOHANG rc = 0), or merely interrupted:
                        ;; both mean "ask again", after a short wait.
                        ((or (= rc 0) (and (< rc 0) (= err proc-EINTR)))
                         (sleep (make-time 'time-duration step 0))
                         (loop (if (< step proc-poll-step-max)
                                   (min proc-poll-step-max (* step 2))
                                   proc-poll-step-max)))
                        ;; unwaitable (ECHILD) or waitpid unavailable — no number of
                        ;; retries changes that.
                        (else (let ((c (proc-lost-status st)))
                                (set-box! (proc-p-exit-box st) c) c))))))))))
    (for-each proc-latch-wait (unbox (proc-p-inherit-latches st)))
    code))

;; Non-blocking liveness poll (reaps and caches on exit). An unwaitable child is
;; reported dead AND has its status cached, so a waitFor after it cannot go looking
;; for a child that will never be there.
(define (proc-alive? st)
  (with-mutex (proc-p-mutex st)
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
              ;; (waitFor timeout unit): babashka always passes MILLISECONDS.
              (proc-wait-timed self (jnum->exact (car args))))))
        (cons "exitValue" (lambda (self)
          (with-mutex (proc-p-mutex self)
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
            (else (sleep (make-time 'time-duration (* step 1000000) 0))
                  (loop (- remaining step)))))))

;; --- java.lang.ProcessHandle (destroy-tree) ----------------------------------
;; jolt does not track process trees, so descendants is always empty; destroy-tree
;; then reduces to destroying the process itself. descendants returns an empty
;; collection whose .iterator flows through the generic make-jiterator path, so
;; (iterator-seq (.iterator (.descendants h))) yields nothing.
(define (make-proc-handle pid) (make-jhost "process-handle" pid))
(register-host-methods! "process-handle"
  (list (cons "destroy" (lambda (self) (when proc-kill (proc-kill (jhost-state self) proc-SIGTERM)) #t))
        (cons "pid"     (lambda (self) (->num (jhost-state self))))
        (cons "descendants" (lambda (self) (jolt-vector)))))

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
;; addShutdownHook stores Thread hooks run at jolt exit; shell's default :shutdown
;; registers one to kill the child if jolt dies mid-run. shell derefs before
;; returning, so in practice the hook is removed (JDK9 onExit path) before exit.
(define proc-shutdown-hooks (box '()))
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
          (lambda (self hook) (set-box! proc-shutdown-hooks (cons hook (unbox proc-shutdown-hooks))) jolt-nil))
        (cons "removeShutdownHook"
          (lambda (self hook)
            (set-box! proc-shutdown-hooks (remq hook (unbox proc-shutdown-hooks))) #t))
        (cons "availableProcessors" (lambda (self) (->num (jolt-available-processors))))
        ;; The memory trio, over Chez's own heap accounting: current-memory-bytes
        ;; is what the collector has reserved from the OS (the JVM's totalMemory)
        ;; and bytes-allocated is what is live inside it, so free is the
        ;; difference. maxMemory is unbounded here — Chez grows the heap on demand
        ;; with no configured ceiling — and Long/MAX_VALUE is what the JVM reports
        ;; for exactly that case. criterium reads all four for its report, and
        ;; without them a benchmark namespace crashes rather than running.
        (cons "totalMemory" (lambda (self) (->num (current-memory-bytes))))
        (cons "freeMemory"
          (lambda (self) (->num (max 0 (- (current-memory-bytes) (bytes-allocated))))))
        (cons "maxMemory" (lambda (self) (->num 9223372036854775807)))
        ;; Runtime.gc routes to System/gc on the JVM, so it gets the same guarded
        ;; hint semantics — Chez's collect refuses while multiple threads are live,
        ;; and neither of these ever throws on the JVM.
        (cons "gc" (lambda (self)
                     (guard (e (#t #f)) (collect (collect-maximum-generation)))
                     jolt-nil))
        ;; No finalizers on this host, so running them is genuinely a no-op — which
        ;; is also all the JVM promises (a hint, deprecated for removal since 18).
        (cons "runFinalization" (lambda (self) jolt-nil))
        (cons "exec" (lambda (self . args) (proc-runtime-exec args)))))
(register-class-statics! "java.lang.Runtime" (list (cons "getRuntime" (lambda () the-jolt-runtime))))

;; instance? and (class x) for the ProcessBuilder / Process / Redirect shims are
;; DERIVED from the jhost-tag->fqn rows in class-hierarchy.ss (via the arms in
;; host-static-classes.ss) — no per-class instance-check arm here.
