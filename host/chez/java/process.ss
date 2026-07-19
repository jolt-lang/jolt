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
(define proc-waitpid (jolt-foreign-proc-safe "waitpid" '(int void* int) 'int))
(define proc-kill    (jolt-foreign-proc-safe "kill"    '(int int)       'int))

(define proc-WNOHANG 1)        ; macOS + Linux
(define proc-SIGTERM 15)
(define proc-SIGKILL 9)

;; WEXITSTATUS / signalled-process convention: a process killed by signal N
;; reports 128+N, matching the JVM's Process.exitValue on Unix.
(define (proc-decode-status raw)
  (let ((termsig (bitwise-and raw #x7f)))
    (if (= termsig 0)
        (bitwise-and (bitwise-arithmetic-shift-right raw 8) #xff)
        (+ 128 termsig))))

;; One waitpid call; returns (values rc decoded-or-#f). rc = pid on reap, 0 when
;; WNOHANG and still running, -1 on EINTR/error.
(define (proc-waitpid-once pid nohang?)
  (if (not proc-waitpid)
      (values -1 #f)
      (let ((buf (foreign-alloc 4)))
        (let ((rc (proc-waitpid pid buf (if nohang? proc-WNOHANG 0))))
          (let ((raw (foreign-ref 'int buf 0)))
            (foreign-free buf)
            (values rc (and (= rc pid) (proc-decode-status raw))))))))

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
;;          stdin-port inherit-latches)
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
                            child-stdout child-stdin latches)))
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
(define (proc-wait-blocking st)
  (let ((code
          (with-mutex (proc-p-mutex st)
            (or (unbox (proc-p-exit-box st))
                (let loop ()
                  (call-with-values (lambda () (proc-waitpid-once (proc-p-pid st) #f))
                    (lambda (rc decoded)
                      (cond ((and decoded (= rc (proc-p-pid st)))
                             (set-box! (proc-p-exit-box st) decoded) decoded)
                            (else (or (unbox (proc-p-exit-box st)) (loop)))))))))))
    (for-each proc-latch-wait (unbox (proc-p-inherit-latches st)))
    code))

;; Non-blocking liveness poll (reaps and caches on exit).
(define (proc-alive? st)
  (with-mutex (proc-p-mutex st)
    (if (unbox (proc-p-exit-box st)) #f
        (call-with-values (lambda () (proc-waitpid-once (proc-p-pid st) #t))
          (lambda (rc decoded)
            (cond ((= rc 0) #t)
                  (decoded (set-box! (proc-p-exit-box st) decoded) #f)
                  (else #f)))))))

(define (proc-signal st sig) (when proc-kill (proc-kill (proc-p-pid st) sig)) st)

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
                  (lambda (rc decoded)
                    (cond (decoded (set-box! (proc-p-exit-box self) decoded) (->num decoded))
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
        (cons "availableProcessors" (lambda (self) (->num 1)))
        (cons "exec" (lambda (self . args) (proc-runtime-exec args)))))
(register-class-statics! "java.lang.Runtime" (list (cons "getRuntime" (lambda () the-jolt-runtime))))

;; instance? and (class x) for the ProcessBuilder / Process / Redirect shims are
;; DERIVED from the jhost-tag->fqn rows in class-hierarchy.ss (via the arms in
;; host-static-classes.ss) — no per-class instance-check arm here.
