;; jolt.process gate — exercises the public sub-process API against real programs.
;; Run: bin/jolt run test/chez/process-test.clj (smoke.sh greps for "PROCESS-TEST OK").
(ns process-test
  (:require [jolt.process :as p :refer [process sh check pipeline]]
            [jolt.fs :as fs]
            [jolt.scheme :as scm]
            [clojure.string :as str]))

(def failures (atom []))

;; A MACRO, not a fn, so the label is announced BEFORE the expression under test runs
;; — as a fn, `got` is evaluated at the call site and a check that blocks never
;; reaches the announcement. Every check here spawns a real child process, so any one
;; of them can block forever on a pipe or a wait, and this file used to print nothing
;; until the final verdict: a hung check left an empty log with no way to tell which.
;; (It hung the CI gate for over three hours exactly this way; jolt-pgbh.)
;;
;; The flush is load-bearing, not decoration: smoke.sh redirects this to a file, where
;; output is block-buffered, so a killed process loses whatever it had not flushed —
;; which is how "it printed nothing at all" survived even a 120s cap.
(defmacro check-eq [label got want]
  `(do
     (print (str "  .. " ~label "\n"))
     (flush)
     (let [g# ~got w# ~want]
       (when-not (= g# w#)
         (swap! failures conj (str ~label ": want " (pr-str w#) " got " (pr-str g#)))))))

;; tokenize (pure)
(check-eq "tokenize" (p/tokenize "a  b 'c d'") ["a" "b" "c d"])
(check-eq "tokenize empty" (p/tokenize "") [])

;; capture stdout / exit codes
(check-eq "sh out" (:out (sh ["echo" "hello"])) "hello\n")
(check-eq "sh exit 0" (:exit (sh ["true"])) 0)
(check-eq "sh exit 1" (:exit (sh ["false"])) 1)
(check-eq "exit code passthrough" (:exit (sh ["sh" "-c" "exit 7"])) 7)

;; args are literal (no shell splitting/globbing of an argument)
(check-eq "literal arg" (:out (sh ["echo" "a b  c"])) "a b  c\n")

;; stderr: capture, and merge into stdout
(check-eq "err capture" (:err (sh ["sh" "-c" "echo boom 1>&2"] {:err :string})) "boom\n")
(check-eq "err->out" (:out (sh ["sh" "-c" "echo e 1>&2"] {:err :out})) "e\n")
;; A spawn runs the program itself where it can, with no /bin/sh in between
;; (process.ss proc-direct-spec). What the shell did on the way has to survive
;; that: a merged stderr lands in order on stdout and the error stream is empty,
;; a program named without a path is found on PATH, and an executable script
;; with no #! line still runs — execve refuses it (ENOEXEC), and the JDK and the
;; shell run it with /bin/sh, so it goes through the shell here.
(let [pr (.start (doto (java.lang.ProcessBuilder. ["sh" "-c" "echo a; echo b 1>&2; echo c"])
                   (.redirectErrorStream true)))]
  (check-eq "redirectErrorStream merges in order"
            [(slurp (.getInputStream pr)) (slurp (.getErrorStream pr)) (.waitFor pr)]
            ["a\nb\nc\n" "" 0]))
(check-eq "a program named without a path is found on PATH"
          (:out (sh ["printf" "%s-%s" "x" 1])) "x-1")
(let [f (str (fs/create-temp-file {:prefix "jp-noshebang-" :suffix ".sh"}))]
  (spit f "echo NO-SHEBANG \"$1\"\n")
  (.setExecutable (java.io.File. f) true)
  (check-eq "an executable script with no #! line runs through the shell"
            (:out (sh [f "arg"])) "NO-SHEBANG arg\n")
  (fs/delete-if-exists f))

;; stdin: feed a string
(check-eq "in string" (:out (sh ["cat"] {:in "line1\nline2\n"})) "line1\nline2\n")

;; :dir and :env / :extra-env
;; the canonical path, so the child's `pwd` (getcwd) answers it back on hosts
;; whose temp dir sits behind a symlink (macOS /var -> /private/var).
(let [d (.getCanonicalPath (java.io.File. (System/getProperty "java.io.tmpdir")))]
  (check-eq "dir" (:out (sh ["pwd"] {:dir d})) (str d "\n")))
(check-eq "env replace" (:out (sh ["sh" "-c" "echo $JP_VAR"] {:env {"JP_VAR" "set"}})) "set\n")
(check-eq "extra-env keeps PATH" (:out (sh ["sh" "-c" "echo $JP_X"] {:extra-env {"JP_X" "y"}})) "y\n")

;; check throws on non-zero, returns the derefed process on success
(check-eq "check ok exit" (:exit (check (process ["true"]))) 0)
(check-eq "check throws"
          (try (check (process ["false"])) :no-throw (catch Exception _ :threw)) :threw)

;; every byte value through a child's pipes both ways, written from an offset into
;; the source array and read into an offset of the destination, in chunks that
;; straddle the port's 32 KB buffer: the pipe ports copy blocks between their
;; foreign buffer and the bytevector, and an offset goes through a scratch
(let [n 100000
      src (byte-array (map #(unchecked-byte %) (range (+ n 7))))
      p (.start (ProcessBuilder. ["cat"]))
      w (future (with-open [out (.getOutputStream p)]
                  (.write out src 7 50000)
                  (.write out src 50007 (- n 50000))))
      dst (byte-array (+ n 3))
      got (with-open [in (.getInputStream p)]
            (loop [off 3]
              (let [r (.read in dst off (min 40000 (- (+ n 3) off)))]
                (if (or (neg? r) (= off (+ n 3))) (- off 3) (recur (+ off r))))))]
  @w
  (.waitFor p)
  (check-eq "binary pipe round trip count" got n)
  (check-eq "binary pipe round trip bytes"
            (= (seq (java.util.Arrays/copyOfRange dst 3 (+ n 3)))
               (seq (java.util.Arrays/copyOfRange src 7 (+ n 7))))
            true))

;; pipelines via threading and via pipeline
(check-eq "pipe ->" (-> (process ["printf" "a\nb\nc\n"]) (process ["grep" "b"]) :out slurp) "b\n")
(check-eq "pipeline count" (count (pipeline (-> (process ["echo" "x"]) (process ["cat"])))) 2)

;; process record deref carries :out/:exit
(let [res @(process ["echo" "derefed"] {:out :string})]
  (check-eq "deref out" (:out res) "derefed\n")
  (check-eq "deref exit" (:exit res) 0))

;; timed deref honours the timeout BOTH ways (jolt-go9n): a live process answers
;; the timeout value, a finished one its map — and neither throws a cast error.
;; This is what the jolt-lang/process fork existed for: upstream guards its
;; IBlockingDeref arm behind #?@(:bb [] :clj […]), so while jolt matched :bb it
;; took the empty splice. It reads the :clj arm now (#893), and vendor/process is
;; upstream babashka/process again — which is exactly what this row pins.
(let [slow (process ["sleep" "30"])]
  (check-eq "timed deref times out" (deref slow 150 :timed-out) :timed-out)
  (p/destroy slow))
(check-eq "timed deref after exit" (:exit (deref (process ["true"]) 5000 :hung)) 0)

;; :out to a file
(let [tmp (str (fs/create-temp-file {:prefix "jp-" :suffix ".txt"}))]
  @(process ["echo" "to-file"] {:out tmp})
  (check-eq "out->file" (slurp tmp) "to-file\n")
  (fs/delete-if-exists tmp))

;; alive? / destroy / signal exit code
(let [proc (process ["sleep" "10"])]
  (check-eq "alive?" (p/alive? proc) true)
  (p/destroy proc)
  (check-eq "sigterm exit" (:exit @proc) 143)
  (check-eq "dead after destroy" (p/alive? proc) false))

;; a spawned child inherits the user's cwd (user.dir / JOLT_PWD), not jolt's OS cwd
;; (the launcher cd's to the repo root but preserves the user's cwd in JOLT_PWD)
(check-eq "child cwd = user.dir"
          (str (fs/canonicalize (str/trim (:out (sh ["pwd"])))))
          (str (fs/canonicalize (System/getProperty "user.dir"))))
;; an explicit :dir sets the child's cwd (pwd echoes the logical cd path)
(let [sub (fs/create-temp-dir {:prefix "jp-dir-"})]
  (check-eq "dir set" (str/trim (:out (sh ["pwd"] {:dir (str sub)}))) (str sub))
  (fs/delete-tree sub))
;; JOLT_PWD is the launcher's message to THIS process — bin/jolt exports the user's
;; cwd before cd'ing to its checkout. A child's cwd is whatever the spawn chose, so
;; the variable is never forwarded: a child jolt reads user.dir from its own cwd
;; instead of inheriting the parent's project. A caller that puts it in the env map
;; asked for it and gets it.
(check-eq "JOLT_PWD is not forwarded"
          (str/trim (:out (sh ["sh" "-c" "echo ${JOLT_PWD-unset}"]))) "unset")
(check-eq "JOLT_PWD is not forwarded through :extra-env"
          (str/trim (:out (sh ["sh" "-c" "echo ${JOLT_PWD-unset}"] {:extra-env {"JP_Y" "1"}}))) "unset")
(check-eq "an explicit JOLT_PWD is honored"
          (str/trim (:out (sh ["sh" "-c" "echo $JOLT_PWD"] {:extra-env {"JOLT_PWD" "/explicit"}}))) "/explicit")
;; End to end, in the shape a test harness takes: this process's own directory
;; has a README.md (the jolt checkout's), and a child jolt rooted at another
;; project — one with its own README.md — must read THAT one, whether it is put
;; there by :dir or by a shell's cd. The wrong answer is the parent's README, a
;; different text rather than a missing file. The child is the jolt under test:
;; smoke.sh hands it down in JOLT_EXE, and bin/jolt exports the same.
(let [sub (fs/create-temp-dir {:prefix "jp-proj-"})
      exe (or (System/getenv "JOLT_EXE") (some-> (fs/which "jolt") str) "jolt-not-found:set-JOLT_EXE")
      expr "(print (slurp \"README.md\"))"]
  (spit (str sub "/README.md") "PROJECT-README-MARKER")
  (check-eq "the parent's own README is not the project's"
            (str/includes? (slurp "README.md") "PROJECT-README-MARKER") false)
  (check-eq "a child jolt under :dir reads its own project"
            (:out (sh [exe "-e" expr] {:dir (str sub)})) "PROJECT-README-MARKER")
  (check-eq "a child jolt behind a cd reads its own project"
            (:out (sh ["sh" "-c" (str "cd '" sub "' && '" exe "' -e '" expr "'")])) "PROJECT-README-MARKER")
  (fs/delete-tree sub))

;; ProcessBuilder.start throws (like the JVM) when the program can't be resolved,
;; with a "No such file" message — not a shell "not found" after spawning
(check-eq "missing program throws"
          (try (sh ["definitely-no-such-program-xyz"]) :no-throw
               (catch Exception e (if (re-find #"No such file" (str (ex-message e))) :nosuch :other)))
          :nosuch)

;; A child that cannot be waited on must still produce an answer. With SIGCHLD set
;; to SIG_IGN the kernel reaps every child itself, so waitpid can only ever fail
;; with ECHILD — and the reap loop used to treat that as "ask again", spinning
;; forever while holding the process mutex. No output, no exit, for as long as the
;; caller waited: that is what sat on a CI gate for 3h42m (jolt-pgbh). The
;; disposition survives exec, so jolt can inherit it from a parent it never chose,
;; which is why it reproduced on one runner and nowhere else.
;;
;; Set it here deliberately, after a spawn has already run (so the spawn path's
;; SIG_DFL restore has happened and is not what is under test), to put the reap
;; loop in exactly that state. If this check hangs, the spin is back — smoke.sh's
;; per-case cap names it.
(jolt.ffi/load-library)
(def c-signal (jolt.ffi/__cfn "signal" [:int :pointer] :pointer))
(def SIGCHLD (if (str/includes? (System/getProperty "os.name") "Mac") 20 17))
;; Every blocking call stays INSIDE a check-eq, so the label is announced before it
;; runs: written as a `let` binding instead, the deref hangs before anything is
;; printed and the log ends on the PREVIOUS check's label — pointing at the wrong
;; one. (Verified by reintroducing the spin: that is exactly what it did.)
(let [prev (c-signal SIGCHLD 1)]                     ; 1 = SIG_IGN
  ;; 0 when the kernel auto-reaped it (the status is then unrecoverable — a
  ;; documented divergence from the JVM, which always reaps its own children), or
  ;; the true 5 on a platform that still let us reap. Never a hang.
  (check-eq "unwaitable child still yields an exit code"
            (contains? #{0 5} (:exit @(process ["sh" "-c" "exit 5"]))) true)
  ;; a signalled child's status IS recoverable without waitpid: 128+signal
  (let [proc (process ["sleep" "10"])]
    (p/destroy proc)
    (check-eq "unwaitable signalled child reports 128+SIGTERM" (:exit @proc) 143))
  (c-signal SIGCHLD prev))                           ; put the disposition back

;; class / instance? derive from the central registry
(check-eq "pb instance?" (instance? java.lang.ProcessBuilder (java.lang.ProcessBuilder. ["true"])) true)
(check-eq "proc class" (.getName (class (:proc @(process ["true"])))) "java.lang.Process")

;; --- ProcessBuilder.inheritIO and the redirect getters (jolt-674) -------------
;; inheritIO() is defined as redirectInput(INHERIT).redirectOutput(INHERIT)
;; .redirectError(INHERIT) returning this. Before it existed the miss reported as
;; "No matching field found: inheritIO" — jolt reads a 0-arg method miss as a
;; field probe, like the JVM reflector — and callers had to spell the three out.
;; Every value below is JVM Clojure's.
(check-eq "inheritIO runs the issue's repro" (.. (java.lang.ProcessBuilder. ["true"]) inheritIO start waitFor) 0)
(check-eq "inheritIO propagates a non-zero exit" (.. (java.lang.ProcessBuilder. ["false"]) inheritIO start waitFor) 1)
(let [pb (java.lang.ProcessBuilder. ["true"])]
  (check-eq "inheritIO returns this" (identical? pb (.inheritIO pb)) true))
(let [pb (doto (java.lang.ProcessBuilder. ["true"]) .inheritIO)]
  (check-eq "inheritIO sets stdin"  (= (.redirectInput pb)  java.lang.ProcessBuilder$Redirect/INHERIT) true)
  (check-eq "inheritIO sets stdout" (= (.redirectOutput pb) java.lang.ProcessBuilder$Redirect/INHERIT) true)
  (check-eq "inheritIO sets stderr" (= (.redirectError pb)  java.lang.ProcessBuilder$Redirect/INHERIT) true))
;; the getters: an unset stream reads back as PIPE, the documented default
(let [pb (java.lang.ProcessBuilder. ["true"])]
  (check-eq "an unset redirect is PIPE" (= (.redirectInput pb) java.lang.ProcessBuilder$Redirect/PIPE) true)
  (check-eq "redirectErrorStream defaults false" (.redirectErrorStream pb) false))
(check-eq "redirectErrorStream round-trips"
          (.redirectErrorStream (doto (java.lang.ProcessBuilder. ["true"]) (.redirectErrorStream true))) true)
;; the two are independent on the JVM: inheritIO does not clear a merge
(check-eq "inheritIO leaves redirectErrorStream alone"
          (.redirectErrorStream (doto (java.lang.ProcessBuilder. ["true"])
                                  (.redirectErrorStream true) .inheritIO)) true)
;; a later redirect overrides only the stream it names
(let [pb (doto (java.lang.ProcessBuilder. ["true"])
           .inheritIO (.redirectOutput java.lang.ProcessBuilder$Redirect/PIPE))]
  (check-eq "a later redirect overrides one stream"
            [(= (.redirectOutput pb) java.lang.ProcessBuilder$Redirect/PIPE)
             (= (.redirectInput pb) java.lang.ProcessBuilder$Redirect/INHERIT)] [true true]))

;; --- the JDK 9 File overloads (jolt-947) --------------------------------------
;; redirectInput(File) is defined as redirectInput(Redirect.from(file)), and
;; redirectOutput/redirectError(File) as Redirect.to(file). jolt used to STORE
;; the File and then ignore it — proc-redir-fragment only understood a Redirect
;; jhost — so fd 0 stayed jolt's own and a child like `cat` read the terminal
;; forever instead of seeing EOF. That is the whole repro below: with the
;; redirect honoured the child finishes, without it the waitFor times out.
(check-eq "redirectInput(File) gives the child the file, not jolt's stdin"
          (let [pb (doto (java.lang.ProcessBuilder. ["sh" "-c" "cat; echo FINISHED"])
                     (.redirectInput (java.io.File. "/dev/null"))
                     (.redirectErrorStream true))
                proc (.start pb)]
            [(.waitFor proc 10 java.util.concurrent.TimeUnit/SECONDS)
             (.exitValue proc)
             (str/trim (slurp (.getInputStream proc)))])
          [true 0 "FINISHED"])
;; the same through babashka.process's File :in, which is how the issue was hit
(check-eq "a File :in reaches the child through babashka.process"
          (:exit (deref (process ["sh" "-c" "cat; echo FINISHED"]
                                 {:in (fs/file "/dev/null") :out :pipe :err :pipe})
                        10000 {:exit :timeout}))
          0)
;; redirectOutput(File) truncates into the file, redirectError(File) likewise
(let [out (fs/file (str (fs/create-temp-dir) "/out.txt"))
      err (fs/file (str (fs/create-temp-dir) "/err.txt"))]
  (.waitFor (.start (doto (java.lang.ProcessBuilder. ["sh" "-c" "echo TO-OUT; echo TO-ERR 1>&2"])
                      (.redirectOutput out)
                      (.redirectError err))))
  (check-eq "redirectOutput(File) / redirectError(File) write the files"
            [(str/trim (slurp out)) (str/trim (slurp err))] ["TO-OUT" "TO-ERR"]))
;; a File setter reads back as a Redirect, not as the File — the JDK's getter
;; answers Redirect.from/to(file), so .type is what distinguishes them
(let [pb (doto (java.lang.ProcessBuilder. ["true"])
           (.redirectInput (java.io.File. "/dev/null"))
           (.redirectOutput (java.io.File. "/dev/null")))]
  (check-eq "a File setter reads back as a Redirect"
            (mapv #(.name (.type %)) [(.redirectInput pb) (.redirectOutput pb)]) ["READ" "WRITE"]))
;; anything that is neither says so, instead of being stored and ignored
(check-eq "a non-file, non-Redirect argument is named"
          (try (.redirectInput (java.lang.ProcessBuilder. ["true"]) 42) :no-throw
               (catch IllegalArgumentException e (str/includes? (ex-message e) "redirectInput")))
          true)

;; --- fd-level INHERIT ---------------------------------------------------------
;; Redirect.INHERIT hands the child jolt's REAL fds (posix_spawn leaves 0/1/2
;; untouched), not a pump-fed pipe. Two things only real inheritance can do:
;; the child sees a tty when jolt runs on one, and successive INHERIT-stdin
;; children share the fd offset. Both run a nested jolt, because this test's
;; own stdio belongs to the smoke harness.
;; The jolt under test. JOLT_EXE first: smoke.sh sets it to exactly that, and
;; bin/jolt exports itself in it. Reading only JOLT_BIN fell back to bin/jolt
;; whenever the caller had not exported JOLT_BIN too, and a stale dev cache makes
;; bin/jolt source mode: ~6s of CPU per child jolt, and this file starts ~20 of
;; them, which is how a run took 140s and 115s of CPU against smoke's 120s cap.
(def jolt-bin (or (System/getenv "JOLT_EXE") (System/getenv "JOLT_BIN") "bin/jolt"))
(def mac? (str/includes? (System/getProperty "os.name") "Mac"))

;; a child that writes through INHERIT lands on the nested jolt's stdout
(let [nested (str "(-> (java.lang.ProcessBuilder. [\"sh\" \"-c\" \"echo INHERITED-OUT\"])"
                  " (.redirectOutput java.lang.ProcessBuilder$Redirect/INHERIT)"
                  " (.start) (.waitFor))")
      out (:out (sh [jolt-bin "-e" nested]))]
  (check-eq "INHERIT stdout reaches the parent's stdout"
            (str/includes? out "INHERITED-OUT") true))

;; inheritIO is real fd inheritance, not a recorded flag: BOTH streams land on
;; the nested jolt's stdio, and stderr with them (the single-stream test above
;; only covers stdout).
(let [nested (str "(.. (java.lang.ProcessBuilder. [\"sh\" \"-c\" \"echo IIO-OUT; echo IIO-ERR 1>&2\"])"
                  " inheritIO start waitFor)")
      r (sh [jolt-bin "-e" nested])]
  (check-eq "inheritIO stdout reaches the parent" (str/includes? (:out r) "IIO-OUT") true)
  (check-eq "inheritIO stderr reaches the parent" (str/includes? (str (:out r) (:err r)) "IIO-ERR") true))

;; isatty: under a pty (script(1)), an INHERIT child's stdout IS the terminal.
;; A pump-fed pipe can never answer true here.
(when (fs/which "script")
  (let [nested (str "(-> (java.lang.ProcessBuilder. [\"sh\" \"-c\" \"test -t 1 && echo IS-A-TTY || echo NOT-A-TTY\"])"
                    " (.redirectOutput java.lang.ProcessBuilder$Redirect/INHERIT)"
                    " (.start) (.waitFor))")
        cmd (if mac?
              ["script" "-q" "/dev/null" jolt-bin "-e" nested]
              ["script" "-qec" (str jolt-bin " -e '" nested "'") "/dev/null"])
        out (:out (sh cmd))]
    (check-eq "INHERIT stdout is the real fd (isatty under a pty)"
              (str/includes? out "IS-A-TTY") true)))

;; INHERIT stdin shares the fd AND its read offset: a first child consuming
;; exactly two bytes leaves the rest for the second. The pumps slurped ahead
;; into the first child's pipe, starving the second.
(let [nested (str "(let [rd (fn [cmd] (let [p (-> (java.lang.ProcessBuilder. cmd)"
                  " (.redirectInput java.lang.ProcessBuilder$Redirect/INHERIT) (.start))]"
                  " (.waitFor p) (slurp (.getInputStream p))))]"
                  " (rd [\"sh\" \"-c\" \"dd bs=1 count=2 2>/dev/null >/dev/null\"])"
                  " (print (str \"SECOND=<\" (rd [\"cat\"]) \">\")))")
      out (:out (sh [jolt-bin "-e" nested] {:in "ABCD"}))]
  (check-eq "INHERIT stdin shares the fd offset between children"
            (str/includes? out "SECOND=<CD>") true))

;; --- shutdown hooks -----------------------------------------------------------
;; Runtime.addShutdownHook is what babashka.process's `:shutdown` option registers,
;; so if the hooks never run, `:shutdown destroy-tree` cleans up nothing (#571).
;; A nested jolt, because a hook can only be observed by letting a process exit.
(let [out (:out (sh [jolt-bin "-e" (str "(.addShutdownHook (Runtime/getRuntime)"
                                        " (Thread. (fn [] (println \"HOOK\"))))"
                                        " (println \"MAIN\")")]))]
  (check-eq "shutdown hook runs when the process exits" (str/split-lines out) ["MAIN" "HOOK"]))

(let [out (:out (sh [jolt-bin "-e" (str "(.addShutdownHook (Runtime/getRuntime)"
                                        " (Thread. (fn [] (println \"HOOK\"))))"
                                        " (println \"MAIN\") (System/exit 0)")]))]
  (check-eq "shutdown hook runs on System/exit" (str/split-lines out) ["MAIN" "HOOK"]))

;; The watcher takes only a signal from its set (HUP, INT, TERM). macOS's sigwait
;; answers 0 for a kernel EINTR without writing the signal number, so a handled
;; signal outside the set landing on the watcher thread (a profiling timer's
;; SIGPROF) read as signal 0, and the watcher ended the process with status 128,
;; hooks unrun. A buffer holding what the last answer left, or nothing, is the
;; case: jolt-sigwait-signal's answer decides whether the watcher acts or waits
;; again. The kernel path can't be staged from here (it needs the signal to land
;; on that thread), so the decision is checked on its own.
(let [answer (fn [r n]
               (scm/eval-string (str "(let ([b (sa-foreign-alloc 4)])"
                                     " (sa-foreign-set! 'int b 0 " n ")"
                                     " (let ([s (jolt-sigwait-signal " r " b)]) (sa-foreign-free b) s))")))]
  (check-eq "sigwait answer: SIGTERM from the set" (answer 0 15) 15)
  (check-eq "sigwait answer: SIGINT from the set" (answer 0 2) 2)
  (check-eq "sigwait answer: 0 returned with no signal written (macOS EINTR)" (answer 0 0) false)
  (check-eq "sigwait answer: a signal outside the set" (answer 0 27) false)
  (check-eq "sigwait answer: an error number" (answer 4 15) false))

;; Chez's exit-handler is a THREAD parameter, so a hook registered from a worker
;; installs the wrapper on that worker and nowhere else. The main thread has to
;; carry one of its own or this hook would be dropped on the way out.
(let [out (:out (sh [jolt-bin "-e" (str "(let [t (Thread. (fn [] (.addShutdownHook (Runtime/getRuntime)"
                                        "                          (Thread. (fn [] (println \"HOOK\"))))))]"
                                        "  (.start t) (.join t) (println \"MAIN\"))")]))]
  (check-eq "a hook registered off the main thread still runs" (str/split-lines out) ["MAIN" "HOOK"]))

(let [out (:out (sh [jolt-bin "-e" (str "(let [rt (Runtime/getRuntime)"
                                        "      h (Thread. (fn [] (println \"HOOK\")))]"
                                        "  (.addShutdownHook rt h) (.removeShutdownHook rt h)"
                                        "  (println \"MAIN\"))")]))]
  (check-eq "a removed shutdown hook does not run" (str/split-lines out) ["MAIN"]))

;; --- File.deleteOnExit ---------------------------------------------------------
;; Was a silent no-op: it answered nil and left every path behind, which leaves a
;; decrypted PEM key in $TMPDIR after each run of a program that relies on it.
;; The JDK deletes in reverse order of registration, once per path, after the
;; program's own hooks, and skips what File.delete can't remove. So a directory
;; registered before its file goes, one registered after its file is still
;; non-empty when its turn comes and stays, and a hook can read a registered
;; file and register one more.
(let [root (str (fs/create-temp-dir {:prefix "jp-doe-"}))
      script (str root "/doe.clj")
      _ (spit script
              (str "(let [[mode root] *command-line-args*"
                   "      f (fn [& p] (java.io.File. (clojure.string/join \"/\" (cons root p))))"
                   "      d1 (f mode \"d1\") a (f mode \"d1\" \"a.pem\")"
                   "      d2 (f mode \"d2\") b (f mode \"d2\" \"b\")"
                   "      late (f mode \"late\")]"
                   "  (.mkdirs d1) (.mkdirs d2)"
                   "  (.deleteOnExit d1) (spit a \"x\") (.deleteOnExit a) (.deleteOnExit a)"
                   "  (spit b \"y\") (.deleteOnExit b) (.deleteOnExit d2)"
                   "  (spit late \"z\")"
                   "  (.addShutdownHook (Runtime/getRuntime)"
                   "    (Thread. (fn [] (println (slurp a)) (.deleteOnExit late))))"
                   "  (println (.deleteOnExit (java.io.File. \"\\u0000bad\")))"
                   "  (when (= mode \"exit\") (System/exit 0)))"))
      left (fn [mode] (vec (for [p ["d1" "d1/a.pem" "d2" "d2/b" "late"]
                                 :when (fs/exists? (str root "/" mode "/" p))]
                             p)))]
  (doseq [mode ["return" "exit"]]
    (let [r (sh [jolt-bin script mode root] {:err :string})]
      (check-eq (str "deleteOnExit: the program and its hook run (" mode ")")
                [(:exit r) (str/split-lines (:out r))] [0 ["nil" "x"]])
      (check-eq (str "deleteOnExit: reverse order, once, after the hooks (" mode ")")
                (left mode) ["d2"])))
  (fs/delete-tree root))

;; System/exit from a thread other than the main one ends the process through
;; _exit, which skipped the hooks and the deleteOnExit paths: the JVM runs both
;; whichever thread asks it to exit. "both" exits from a future and from the
;; main thread together, the hook slow enough that the second caller arrives
;; while it runs: the hooks run once and finish, and the first caller's status
;; is the process's, as the JVM blocks the second caller.
(let [root (str (fs/create-temp-dir {:prefix "jp-exit-off-main-"}))
      script (str root "/exit.clj")
      _ (spit script
              (str "(let [[mode dir] *command-line-args*"
                   "      f (java.io.File. dir \"doomed\")]"
                   "  (.mkdirs (java.io.File. dir)) (spit f \"x\") (.deleteOnExit f)"
                   "  (.addShutdownHook (Runtime/getRuntime)"
                   "    (Thread. (fn [] (Thread/sleep 200) (println \"HOOK\"))))"
                   "  (case mode"
                   "    \"future\" @(future (System/exit 3))"
                   "    \"thread\" (doto (Thread. (fn [] (System/exit 4))) .start .join)"
                   "    \"both\" (do (future (System/exit 5)) (Thread/sleep 50) (System/exit 6))))"))]
  (doseq [[mode code] [["future" 3] ["thread" 4] ["both" 5]]]
    (let [dir (str root "/" mode)
          r (sh [jolt-bin script mode dir] {:err :string})]
      (check-eq (str "System/exit off the main thread runs the hooks once (" mode ")")
                [(:exit r) (str/split-lines (:out r))] [code ["HOOK"]])
      (check-eq (str "System/exit off the main thread deletes deleteOnExit paths (" mode ")")
                (fs/exists? (str dir "/doomed")) false)))
  (fs/delete-tree root))

;; File("").deleteOnExit registers "" on the JDK, which names nothing; resolving
;; it to user.dir first deleted the working directory at exit when it was empty.
(let [root (str (fs/create-temp-dir {:prefix "jp-doe-empty-"}))
      r (sh [jolt-bin "-e" "(println (.deleteOnExit (java.io.File. \"\")))"] {:dir root :err :string})]
  (check-eq "deleteOnExit of the empty path leaves the working directory"
            [(:out r) (fs/exists? root)] ["nil\n" true])
  (fs/delete-tree root))

;; Is a pid still alive? `kill -0` is the portable answer (exit 0 = alive). Run
;; through `sh` so it is the shell BUILTIN: a standalone kill(1) is not on every
;; Linux image, and ProcessBuilder.start rejects a program it cannot resolve.
(defn- pid-alive? [pid]
  (zero? (:exit (sh ["sh" "-c" (str "kill -0 " pid)] {:err :string}))))

;; SIGTERM to a jolt process must run its shutdown hooks BEFORE it exits — the
;; whole point of `:shutdown destroy-tree` is that a supervisor's `kill` does not
;; leave the child tree running (#571). Both spawn paths are covered: the default
;; (piped stdio, Chez's own fork) and fd-level INHERIT (posix_spawn).
;;
;; The nested jolt writes its child's pid out through a shell `$$` — the same
;; probe the bug report used — so this side can ask whether that exact process
;; outlived the parent.
(doseq [[label redirs] [["piped stdio" ""]
                        ["fd-level INHERIT" " :out :inherit :err :inherit"]]]
  (let [pidf (str (fs/create-temp-file {:prefix "jp-shutdown-" :suffix ".pid"}))
        nested (str "(require '[jolt.process :as p])"
                    " @(p/process [\"sh\" \"-c\" \"echo $$ > \\\"$CHILD_PID_FILE\\\"; sleep 30\"]"
                    " {:extra-env {\"CHILD_PID_FILE\" \"" pidf "\"}" redirs
                    "  :shutdown p/destroy-tree})")
        parent (process [jolt-bin "-e" nested])]
    ;; the grandchild publishes its pid before it sleeps
    (loop [n 0]
      (when (and (< n 200) (str/blank? (slurp pidf)))
        (Thread/sleep 50)
        (recur (inc n))))
    (let [gpid (str/trim (slurp pidf))]
      (p/destroy parent)
      ;; poll rather than deref: a regression here is "the parent ignores
      ;; SIGTERM", and @parent would then hang the gate instead of failing it
      (loop [n 0] (when (and (< n 60) (p/alive? parent)) (Thread/sleep 50) (recur (inc n))))
      (when (p/alive? parent) (.destroyForcibly (:proc parent)) (Thread/sleep 200))
      ;; the hook's kill and the child's death are not instantaneous
      (loop [n 0] (when (and (< n 40) (pid-alive? gpid)) (Thread/sleep 50) (recur (inc n))))
      (check-eq (str "SIGTERM runs :shutdown hooks (" label ")")
                (and (seq gpid) (pid-alive? gpid)) false)
      (when (pid-alive? gpid) (sh ["sh" "-c" (str "kill -9 " gpid)])))
    (fs/delete-if-exists pidf)))

;; --- java.lang.ProcessHandle, the class (jolt-lang/jolt#1087) ----------------
;; The handle shim answered .pid / .descendants / .destroy and Process.toHandle
;; built one, but the CLASS was not registered, so every reference to it died at
;; the first touch: (ProcessHandle/current) reported RFC 0014's "No dependency
;; provides java.lang.ProcessHandle" — advice nothing can act on for a class the
;; runtime already models. Asking for your own pid, to stamp into a log or temp
;; filename, is the portable spelling that needs it (kmet's
;; terminal/capture-log-path died at namespace load on it).
(let [self (ProcessHandle/current)]
  (check-eq "ProcessHandle/current has a live pid" (pos? (.pid self)) true)
  (check-eq "...and it is this process's, the one a child's $PPID reports"
            (str (.pid self)) (str/trim (:out (sh ["sh" "-c" "echo $PPID"]))))
  (check-eq "...and it is an instance of the class"
            (instance? java.lang.ProcessHandle self) true)
  (check-eq "current is alive" (.isAlive self) true)
  ;; of(pid) answers an Optional, as the JVM's does — present for a running pid
  (check-eq "of(own pid) is present" (.isPresent (ProcessHandle/of (.pid self))) true)
  (check-eq "of(own pid) round-trips" (.pid (.get (ProcessHandle/of (.pid self)))) (.pid self))
  ;; ...and empty for one nothing is running under. 2^22 is over every default
  ;; pid_max, so no race can hand this one a live process.
  (check-eq "of(unused pid) is empty" (.isPresent (ProcessHandle/of 4194304)) false)
  ;; ...and so is a non-positive one, which kill(2) would read as a process GROUP
  (check-eq "of(0) is empty" (.isPresent (ProcessHandle/of 0)) false)
  (check-eq "of(negative) is empty" (.isPresent (ProcessHandle/of -1)) false)
  ;; pid 1 always exists and is usually root's, so kill(pid, 0) answers EPERM
  ;; rather than 0 — "exists, you may not signal it". Reading any failure as
  ;; dead reported every process but our own as gone.
  (check-eq "of(pid 1) is present" (.isPresent (ProcessHandle/of 1)) true)
  (check-eq "toString is the pid" (.toString self) (str (.pid self)))
  (check-eq "two handles on one pid are equal" (.equals self (ProcessHandle/current)) true))

;; Process.toHandle now answers something that reports the class, which is what
;; babashka.process's destroy-tree rides on
(let [p (process ["sh" "-c" "sleep 5"])
      h (.toHandle (:proc p))]
  (check-eq "toHandle is a ProcessHandle" (instance? java.lang.ProcessHandle h) true)
  (check-eq "toHandle carries the child's pid" (.pid h) (.pid (:proc p)))
  (check-eq "the child is alive" (.isAlive h) true)
  (.destroy h)
  (loop [n 0] (when (and (< n 60) (p/alive? p)) (Thread/sleep 50) (recur (inc n))))
  (check-eq "handle destroy kills it" (p/alive? p) false))

;; --- descendants / destroy-tree over a real grandchild (jolt-hpdu) -----------
;; ProcessHandle.descendants was hardcoded empty, so destroy-tree WAS destroy:
;; killing a wrapper left whatever the wrapper spawned running (a `lake env repl`
;; wrapper's repl survived as a 4.8GB orphan). The direct child here is a sh that
;; publishes its background sleep's pid ($!) and waits, so the sleep is a genuine
;; grandchild: descendants must see it, and destroy-tree must kill it.
(let [pidf (str (fs/create-temp-file {:prefix "jp-desc-" :suffix ".pid"}))
      p (process ["sh" "-c" (str "sleep 30 & echo $! > " pidf "; wait")])]
  (loop [n 0]
    (when (and (< n 200) (str/blank? (slurp pidf)))
      (Thread/sleep 50)
      (recur (inc n))))
  (let [gpid (str/trim (slurp pidf))
        handles (iterator-seq (.iterator (.descendants (.toHandle (:proc p)))))]
    (check-eq "descendants sees the grandchild"
              (boolean (some #(= (str (.pid %)) gpid) handles)) true)
    (p/destroy-tree p)
    (loop [n 0] (when (and (< n 60) (pid-alive? gpid)) (Thread/sleep 50) (recur (inc n))))
    (check-eq "destroy-tree kills the grandchild"
              (and (seq gpid) (pid-alive? gpid)) false)
    (when (pid-alive? gpid) (sh ["sh" "-c" (str "kill -9 " gpid)]))
    (loop [n 0] (when (and (< n 60) (p/alive? p)) (Thread/sleep 50) (recur (inc n))))
    (check-eq "destroy-tree kills the direct child" (p/alive? p) false))
  (fs/delete-if-exists pidf))

;; A jolt sitting at a stdin prompt must still take SIGTERM, hooks and all. It
;; used to wait INSIDE Chez's blocking read, which holds the whole Scheme world:
;; nothing else runs, so the watcher could not have woken there (jolt-p9ua).
;; The nested jolt registers a hook, then blocks on read-line with a pipe stdin
;; nothing ever writes to.
;;
;; Nothing here blocks without a bound: a failure has to report itself, not hang
;; the gate.
(let [readyf (str (fs/create-temp-file {:prefix "jp-stdin-" :suffix ".txt"}))
      hookf (str (fs/create-temp-file {:prefix "jp-stdin-hook-" :suffix ".txt"}))
      nested (str "(.addShutdownHook (Runtime/getRuntime)"
                  "  (Thread. (fn [] (spit \"" hookf "\" \"RAN\"))))"
                  " (spit \"" readyf "\" \"ready\") (read-line)")
      proc (process [jolt-bin "-e" nested])]
  (loop [n 0]
    (when (and (< n 200) (str/blank? (slurp readyf)))
      (Thread/sleep 50)
      (recur (inc n))))
  (p/destroy proc)
  (loop [n 0] (when (and (< n 60) (p/alive? proc)) (Thread/sleep 50) (recur (inc n))))
  (check-eq "SIGTERM reaches a jolt parked at a stdin prompt" (p/alive? proc) false)
  ;; SIGKILL, since a process that survived SIGTERM will survive another one
  (when (p/alive? proc) (.destroyForcibly (:proc proc)) (Thread/sleep 200))
  (check-eq "and its shutdown hooks run there too" (slurp hookf) "RAN")
  (fs/delete-if-exists readyf)
  (fs/delete-if-exists hookf))

;; ^C must run the shutdown hooks too, and exit 130 (128+SIGINT) the way the JVM
;; and the shell both report it. SIGINT used to be left to Chez's
;; keyboard-interrupt-handler, which unwinds to Chez's top level and exits 255
;; without ever reaching the exit handler — so a `:shutdown destroy-tree` cleaned
;; up on `kill` and cleaned up nothing on ^C (jolt-na7). The shutdown watcher takes
;; SIGINT along with SIGTERM/SIGHUP now.
(let [readyf (str (fs/create-temp-file {:prefix "jp-sigint-" :suffix ".txt"}))
      hookf  (str (fs/create-temp-file {:prefix "jp-sigint-hook-" :suffix ".txt"}))
      nested (str "(.addShutdownHook (Runtime/getRuntime)"
                  "  (Thread. (fn [] (spit \"" hookf "\" \"RAN\"))))"
                  " (spit \"" readyf "\" \"ready\") (Thread/sleep 30000)")
      proc (process [jolt-bin "-e" nested] {:out :string :err :string})]
  (loop [n 0]
    (when (and (< n 200) (str/blank? (slurp readyf)))
      (Thread/sleep 50)
      (recur (inc n))))
  (sh ["sh" "-c" (str "kill -INT " (.pid (:proc proc)))])
  (loop [n 0] (when (and (< n 60) (p/alive? proc)) (Thread/sleep 50) (recur (inc n))))
  (check-eq "SIGINT kills a jolt that registered a shutdown hook" (p/alive? proc) false)
  (when (p/alive? proc) (.destroyForcibly (:proc proc)) (Thread/sleep 200))
  (check-eq "SIGINT runs the shutdown hooks" (slurp hookf) "RAN")
  (check-eq "and exits 128+SIGINT" (:exit @proc) 130)
  (fs/delete-if-exists readyf)
  (fs/delete-if-exists hookf))

;; The stdin-prompt case from the SIGTERM test above, for ^C: a jolt blocked
;; INSIDE Chez's read holds the whole Scheme world, so the hooks can only run
;; from the watcher thread parked in sigwait.
(let [readyf (str (fs/create-temp-file {:prefix "jp-sigint-in-" :suffix ".txt"}))
      hookf  (str (fs/create-temp-file {:prefix "jp-sigint-in-hook-" :suffix ".txt"}))
      nested (str "(.addShutdownHook (Runtime/getRuntime)"
                  "  (Thread. (fn [] (spit \"" hookf "\" \"RAN\"))))"
                  " (spit \"" readyf "\" \"ready\") (read-line)")
      proc (process [jolt-bin "-e" nested])]
  (loop [n 0]
    (when (and (< n 200) (str/blank? (slurp readyf)))
      (Thread/sleep 50)
      (recur (inc n))))
  (sh ["sh" "-c" (str "kill -INT " (.pid (:proc proc)))])
  (loop [n 0] (when (and (< n 60) (p/alive? proc)) (Thread/sleep 50) (recur (inc n))))
  (check-eq "SIGINT reaches a jolt parked at a stdin prompt" (p/alive? proc) false)
  (when (p/alive? proc) (.destroyForcibly (:proc proc)) (Thread/sleep 200))
  (check-eq "and its shutdown hooks run there too (SIGINT)" (slurp hookf) "RAN")
  (fs/delete-if-exists readyf)
  (fs/delete-if-exists hookf))

;; The REPL's ^C: while an interrupt token is installed (interrupt-on-sigint!),
;; SIGINT interrupts the evaluation and the process goes on — a computation, a
;; sleep, a stdin read and a deref alike. An interrupt does not outlive its
;; evaluation (the sleep after them finishes), and with nothing installed SIGINT
;; is the 130 exit again.
(let [readyf (str (fs/create-temp-file {:prefix "jp-repl-int-" :suffix ".txt"}))
      prog (str "(defn step [ready f]"
                "  (let [t (jolt.host/make-interrupt)]"
                "    (jolt.host/interrupt-on-sigint! t)"
                "    (spit \"" readyf "\" ready)"
                "    (try (jolt.host/run-interruptible t f) :finished"
                "         (catch :default e (if (jolt.host/interrupted? t) :interrupted (ex-message e)))"
                "         (finally (jolt.host/interrupt-on-sigint! nil)))))"
                "(println (step \"1\" (fn [] (loop [] (recur)))))"
                "(println (step \"2\" (fn [] (Thread/sleep 30000))))"
                "(println (step \"3\" (fn [] (read-line))))"
                "(println (step \"4\" (fn [] @(promise))))"
                "(println (step \"5\" (fn [] (Thread/sleep 300))))"
                "(flush)"
                "(spit \"" readyf "\" \"6\")"
                "(Thread/sleep 30000)")
      proc (process [jolt-bin "-e" prog] {:out :string :err :string})
      await-ready (fn [s]
                    (loop [n 0]
                      (when (and (< n 400) (not= s (slurp readyf)))
                        (Thread/sleep 25)
                        (recur (inc n))))
                    (Thread/sleep 200))
      sigint! #(sh ["sh" "-c" (str "kill -INT " (.pid (:proc proc)))])]
  (doseq [s ["1" "2" "3" "4"]] (await-ready s) (sigint!))
  (await-ready "6")
  (sigint!)
  (loop [n 0] (when (and (< n 100) (p/alive? proc)) (Thread/sleep 50) (recur (inc n))))
  (when (p/alive? proc) (.destroyForcibly (:proc proc)) (Thread/sleep 200))
  (check-eq "SIGINT interrupts each evaluation and the process goes on"
            (str/split-lines (:out @proc))
            [":interrupted" ":interrupted" ":interrupted" ":interrupted" ":finished"])
  (check-eq "and with no evaluation installed SIGINT exits 130" (:exit @proc) 130)
  (fs/delete-if-exists readyf))

;; …and no signal may be swallowed by a thread that was already running, nor
;; depend on WHICH thread registered the hook. The kernel delivers to a thread
;; that does not block the signal, so before the watcher was armed up front it
;; skipped the masked arming thread and delivered to one that was forked earlier —
;; a dependency's accept loop or pool, started at namespace-load time long before
;; -main could register anything. ^C landed there, where Chez's keyboard-interrupt
;; handler is a no-op, and did nothing at all: no exit, no hooks. SIGTERM landed
;; there at SIG_DFL and killed the process with its hooks unrun. Minimal programs
;; have no load-time threads, which is why every case above passed while a real
;; app's ^C did nothing (#1098).
;;
;; The JVM answers all of this uniformly — its signal dispatcher thread owns the
;; three from VM startup — and these rows are its answers, measured on OpenJDK 20
;; with Clojure 1.12.6 rather than assumed: hooks run and the status is 128+signal
;; for a hook registered on the main thread, for one registered on a worker, and
;; the status alone for a program with no hooks at all.
(doseq [[label prog sig code hook?]
        (let [hookform (fn [f] (str "(.addShutdownHook (Runtime/getRuntime)"
                                    " (Thread. (fn [] (spit \"" f "\" \"RAN\"))))"))
              ;; a thread started before the hook, as a dependency's would be
              prefork (fn [f] (str "(.start (Thread. (fn [] (Thread/sleep 30000))))" (hookform f)))
              ;; …and the hook itself registered from a worker, not from -main
              offmain (fn [f] (str "(let [t (Thread. (fn [] " (hookform f) "))]"
                                   " (.start t) (.join t))"))
              none    (fn [_] "nil")]
          [["a thread forked before the hook" prefork "INT"  130 true]
           ["a thread forked before the hook" prefork "TERM" 143 true]
           ["a hook registered off the main thread" offmain "INT"  130 true]
           ["a hook registered off the main thread" offmain "TERM" 143 true]
           ["a hook registered off the main thread" offmain "HUP"  129 true]
           ["no shutdown hook at all" none "INT"  130 false]
           ["no shutdown hook at all" none "TERM" 143 false]])]
  (let [readyf (str (fs/create-temp-file {:prefix "jp-presig-" :suffix ".txt"}))
        hookf  (str (fs/create-temp-file {:prefix "jp-presig-hook-" :suffix ".txt"}))
        nested (str (prog hookf)
                    " (spit \"" readyf "\" \"ready\") (Thread/sleep 30000)")
        proc (process [jolt-bin "-e" nested] {:out :string :err :string})]
    (loop [n 0]
      (when (and (< n 200) (str/blank? (slurp readyf)))
        (Thread/sleep 50)
        (recur (inc n))))
    (sh ["sh" "-c" (str "kill -" sig " " (.pid (:proc proc)))])
    (loop [n 0] (when (and (< n 60) (p/alive? proc)) (Thread/sleep 50) (recur (inc n))))
    (check-eq (str "SIG" sig " is not swallowed: " label) (p/alive? proc) false)
    (when (p/alive? proc) (.destroyForcibly (:proc proc)) (Thread/sleep 200))
    (when hook?
      (check-eq (str "SIG" sig " runs the hooks: " label) (slurp hookf) "RAN"))
    (check-eq (str "SIG" sig " exits " code ": " label) (:exit @proc) code)
    (fs/delete-if-exists readyf)
    (fs/delete-if-exists hookf)))

;; Same root cause from the other side: while the main thread waits on stdin the
;; rest of the program has to keep running. A future stopped ticking the moment a
;; prompt was reached, which is not what a JVM does with a thread in
;; System.in.read() (jolt-p9ua).
;;
;; The future APPENDS a line per tick and the row counts lines. It used to spit
;; the tick number over the file, and a read landing between spit's truncate and
;; its write saw an empty file, which counted as tick 0 and failed "keeps
;; running" on a loaded runner (jolt-nb5ng); so did a fixed 400ms window that a
;; starved future could miss. The file only grows now, and each wait polls for
;; progress under a deadline far past anything a running future needs, so only
;; a future that has really stopped can fail the row.
(let [tickf (str (fs/create-temp-file {:prefix "jp-ticks-" :suffix ".txt"}))
      nested (str "(future (dotimes [i 600] (Thread/sleep 50) (spit \"" tickf "\" \"t\\n\" :append true)))"
                  " (read-line)")
      proc (process [jolt-bin "-e" nested])
      ticks (fn [] (count (re-seq #"t\n" (slurp tickf))))
      wait-past (fn [n] (loop [i 0]
                          (when (and (< i 400) (<= (ticks) n)) (Thread/sleep 50) (recur (inc i))))
                  (ticks))]
  (check-eq "a future runs while the main thread waits on stdin" (pos? (wait-past 0)) true)
  ;; and keeps running, rather than getting one tick in before the read parks
  (let [before (ticks)]
    (check-eq "and keeps running" (> (wait-past (+ before 2)) (+ before 2)) true))
  (.destroyForcibly (:proc proc))
  (fs/delete-if-exists tickf))

;; A child must be interruptible by ^C, on every spawn path. Chez's fork leaves
;; SIGINT set to SIG_IGN in the child — the system(3) leak the convention exists
;; to avoid, not the convention — so a child spawned the default way could not be
;; interrupted at all where the JVM's dies (jolt-a4hs). posix_spawn drives both
;; paths now, and `sh` reports 128+SIGINT when the signal lands.
(check-eq "a child dies on SIGINT (piped stdio)"
          (:exit (sh ["sh" "-c" "kill -INT $$"])) 130)
(check-eq "a child dies on SIGINT (fd-level INHERIT)"
          (:exit @(process ["sh" "-c" "kill -INT $$"] {:out :string :err :inherit})) 130)

;; jolt blocks signals in its own threads — SIGINT so ^C reaches the thread parked
;; in park-until-interrupt rather than a worker in a foreign call, SIGTERM/SIGHUP
;; so the shutdown watcher can take them — and a spawn hands the calling thread's
;; mask straight to the child. Neither may travel: a child with SIGTERM blocked
;; survives the very destroy the hook exists to call, and one with SIGINT blocked
;; ignores ^C (jolt-e5sb).
(.addShutdownHook (Runtime/getRuntime) (Thread. (fn [] nil)))
(let [proc (process ["sleep" "10"])]
  (p/destroy proc)
  (check-eq "a child spawned after a shutdown hook is still killable" (:exit @proc) 143))
(check-eq "a child does not inherit jolt's blocked SIGTERM"
          (:exit @(process ["sh" "-c" "kill -TERM $$"] {:out :string :err :inherit})) 143)
(jolt.host/block-sigint)
(check-eq "a child does not inherit jolt's blocked SIGINT"
          (:exit @(process ["sh" "-c" "kill -INT $$"] {:out :string :err :inherit})) 130)

;; Nor does it inherit jolt's open FILES AND SOCKETS. A descriptor with no file
;; action is the parent's own, and posix_spawn hands the child every one of them
;; unless told otherwise — where the JVM's ProcessBuilder gives a child the three
;; stdio streams and nothing more. The cost is not tidiness: a child holding a
;; copy of a listening socket keeps that port BOUND after the parent closes it,
;; and an orphaned child (parent killed by a test runner's timeout) keeps it for
;; as long as the orphan lives, so the next run cannot bind the port at all
;; (#910 — curl children aged hours still pinning fixed callback ports).
(require 'jolt.socket)
(let [server (java.net.ServerSocket. 0)
      port   (.getLocalPort server)
      child  (process ["sleep" "30"])]
  (.close server)                       ; the parent is done with the listener…
  (check-eq "the port a closed listener held is free while a child still runs"
            (try (.close (java.net.ServerSocket. port)) :bound
                 (catch java.io.IOException _ :bind-failed))
            :bound)
  (p/destroy child)
  @child)

;; The same thing said exactly, where the OS will show the table: three stdio
;; pipes, no jolt source file, no socket. Linux-only (/proc); the port case above
;; is the portable half.
;; Read until the table settles: a freshly exec'd `sleep` opens files of its own
;; for a moment (the dynamic loader's ld.so.cache and libc, setlocale's locale
;; files) as fd 3, and a read that lands there sees it. A descriptor jolt leaked
;; is there on every read, so it still fails.
(when (fs/exists? "/proc/self/fd")
  (let [child (process ["sleep" "30"])
        read-fds #(->> (sh ["ls" (str "/proc/" (.pid (:proc child)) "/fd")]) :out
                       str/split-lines (remove str/blank?) sort vec)
        fds   (loop [n 0]
                (let [fds (read-fds)]
                  (if (or (= fds ["0" "1" "2"]) (>= n 20))
                    fds
                    (do (Thread/sleep 50) (recur (inc n))))))]
    (check-eq "a child's descriptor table is its own stdio and nothing else"
              fds ["0" "1" "2"])
    (p/destroy child)
    @child))

;; posix_spawn_file_actions_addclosefrom_np is glibc 2.34+, so on this machine
;; the case above can only ever exercise that one action. Every macOS and every
;; older glibc — most of the range the released Linux binary targets — takes the
;; enumeration fallback instead, and it is gated here by re-running the same
;; question in a child jolt that has the closefrom path switched off.
(let [exe  (or (System/getenv "JOLT_EXE") (some-> (fs/which "jolt") str) "jolt-not-found:set-JOLT_EXE")
      expr (str "(require 'jolt.socket)"
                "(require '[jolt.process :as p])"
                "(let [s (java.net.ServerSocket. 0) port (.getLocalPort s)"
                "      c (p/process [\"sleep\" \"30\"])]"
                "  (.close s)"
                "  (print (try (.close (java.net.ServerSocket. port)) \"FREE\""
                "              (catch java.io.IOException _ \"BOUND\")))"
                "  (p/destroy c) @c nil)")]
  (check-eq "and the same holds on the enumeration fallback (no closefrom)"
            (:out (sh [exe "-e" expr] {:extra-env {"JOLT_NO_SPAWN_CLOSEFROM" "1"}}))
            "FREE"))

;; Spawning while another thread closes descriptors. The enumeration fallback
;; snapshots the parent's table between pipe() and posix_spawn, and a descriptor
;; closed in that window — a sibling's drained pipe, a file, a port a finalizer
;; released — leaves a close action on a dead fd. glibc skips it in the child; the
;; Darwin kernel fails the whole spawn with EBADF, and three workers sharing a
;; ThreadLocal<Process> (below) lost one subprocess to "posix_spawn failed
;; (errno 9)". macOS spawns under POSIX_SPAWN_CLOEXEC_DEFAULT now, where the
;; kernel decides the set at spawn time and nothing is snapshotted.
(when (fs/exists? "/etc/hosts")
  (let [errs   (atom []) pids (atom []) stop (atom false)
        closer (Thread. (fn [] (while (not @stop)
                                 (.close (java.io.FileInputStream. "/etc/hosts")))))
        work   (fn [] (dotimes [_ 16]
                        (try (let [p (.start (ProcessBuilder. ["sh" "-c" "echo $$"]))
                                   s (str/trim (slurp (.getInputStream p)))]
                               (.waitFor p)
                               (swap! pids conj s))
                             (catch Exception e (swap! errs conj (.getMessage e))))))
        ts     (mapv (fn [_] (Thread. work)) (range 4))]
    (.start closer)
    (doseq [t ts] (.start t))
    (doseq [t ts] (.join t))
    (reset! stop true)
    (.join closer)
    (check-eq "spawning from four threads while a fifth closes descriptors" @errs [])
    (check-eq "and every one of those spawns was its own child" (count (distinct @pids)) 64)))

;; A per-thread subprocess — ThreadLocal<Process>, the shape a worker pool uses to
;; give each thread its own long-lived helper program. It only works if the child
;; threads run initialValue themselves: jolt's ThreadLocal was a Chez thread
;; parameter, which a forked thread inherits, so every worker got the PARENT's
;; already-drained process and read an empty string off it (jolt-uecg).
(let [tl   (proxy [ThreadLocal] []
             (initialValue [] (.start (ProcessBuilder. ["sh" "-c" "echo $$"]))))
      ;; each caller reaps the process it owns, on its own thread — the child
      ;; threads are the only holders of theirs once .join returns
      pid  (fn [] (let [p (.get tl) s (str/trim (slurp (.getInputStream p)))]
                    (.waitFor p)
                    s))
      main (pid)
      seen (atom [])
      ts   (mapv (fn [_] (Thread. (fn [] (swap! seen conj (pid))))) (range 3))]
  (doseq [t ts] (.start t))
  (doseq [t ts] (.join t))
  (check-eq "each thread's ThreadLocal<Process> is its own subprocess"
            (count (distinct (cons main @seen))) 4)
  (check-eq "and every worker actually read a pid"
            (every? (fn [s] (and (seq s) (parse-long s))) @seen) true))

;; The inheritable variant is the opposite contract and must keep working: the
;; child shares the parent's process rather than spawning a second one.
(let [tl   (proxy [InheritableThreadLocal] []
             (initialValue [] (.start (ProcessBuilder. ["sh" "-c" "echo $$"]))))
      main (.get tl)
      got  (promise)]
  (.start (Thread. (fn [] (deliver got (identical? (.get tl) main)))))
  (check-eq "an InheritableThreadLocal<Process> child shares the parent's process"
            (deref got 5000 :TIMED-OUT) true)
  (slurp (.getInputStream main))
  (.waitFor main))

;; A timed waitFor keeps its bound. It counted 10 ms steps instead of reading a
;; clock, and every step's overrun accumulated: 1000 ms came back at ~1170,
;; 5000 at ~5840. The broken loop reads ~2340 here even on a fast machine (and
;; more under load, where every step overruns further), so the upper bound sits
;; 250 ms past the deadline, below it. One late wakeup on a loaded machine can
;; cross that too, so a reading past it is taken again before it counts: the
;; broken loop is late on both, a delayed wakeup rarely twice. The lower bound
;; is exact and checked on every reading: a waitFor never returns early.
;; Timed on nanoTime, the clock the deadline is on: the wall clock truncates
;; separately and read 1999 for a full 2000 ms wait on bionic.
(defn- timed-wait-for []
  (let [p (.start (java.lang.ProcessBuilder. ["sleep" "30"]))
        t0 (System/nanoTime)
        r (.waitFor p 2000 java.util.concurrent.TimeUnit/MILLISECONDS)
        waited (quot (- (System/nanoTime) t0) 1000000)]
    (.destroyForcibly p)
    (.waitFor p)
    [r waited]))
(check-eq "a timed waitFor returns at the deadline it was given"
          (let [[r waited] (timed-wait-for)]
            [r (>= waited 2000)
             (or (<= waited 2250)
                 (let [[r2 waited2] (timed-wait-for)] (and (false? r2) (<= 2000 waited2 2250))))])
          [false true true])

;; Reading a child's output as text closes its pipe. slurp of an InputStream,
;; and closing an io/reader or InputStreamReader over one, reach the stream
;; underneath, as on the JVM; a closed text reader that left the pipe open leaked
;; two descriptors per :string run and three per sh, and nothing reclaims a pipe.
;; 20 runs each, judged against a slack of a few descriptors for whatever the
;; runtime opens on its own in between.
(defn- open-fds [] (count (.list (java.io.File. "/dev/fd"))))
(defn- fd-growth [f]
  (f)                                   ; the first run opens anything lazy
  (let [before (open-fds)]
    (dotimes [_ 20] (f))
    (- (open-fds) before)))
(check-eq "process :out/:err :string closes both pipes"
          (<= (fd-growth #(deref (process {:in "" :out :string :err :string} "true"))) 4)
          true)
(check-eq "sh closes its pipes"
          (<= (fd-growth #(sh "true")) 4)
          true)
(check-eq "slurp of a Process stream closes it"
          (<= (fd-growth #(let [pr (.start (java.lang.ProcessBuilder. ["true"]))]
                            (slurp (.getInputStream pr)) (slurp (.getErrorStream pr))
                            (.close (.getOutputStream pr)) (.waitFor pr)))
              4)
          true)
;; A child's pipes are let go of at its exit when nothing is waiting in them, as
;; the JDK does, so output nobody reads does not hold descriptors until a
;; collection; output written before the exit is still there to read after it,
;; and stdin is closed, so a write to it then raises.
;; Pipes dropped unread are released through a guardian that every spawn drains,
;; so spawning threads drain it together. Unserialized, that corrupted it: every
;; run of this on bionic either raised an invalid memory reference or killed the
;; process. Four threads spawning at once, then one more on this thread, round
;; after round, so collections keep handing dropped ports to concurrent drains.
(check-eq "spawning threads drain dropped pipes without corrupting them"
          (let [errs (atom [])
                safe #(try (let [pr (.start (java.lang.ProcessBuilder. ["sh" "-c" "echo $$"]))]
                             (slurp (.getInputStream pr)) (.waitFor pr))
                           (catch Throwable e (swap! errs conj (ex-message e))))]
            (dotimes [_ 40]
              (let [ts (mapv (fn [_] (Thread. #(dotimes [_ 16] (safe)))) (range 4))]
                (doseq [t ts] (.start t))
                (doseq [t ts] (.join t))
                (safe)))
            (take 3 @errs))
          [])
(check-eq "unread pipes are released at exit"
          (<= (fd-growth #(deref (process "true"))) 4)
          true)
(check-eq "output survives the exit, stdin does not"
          (let [pr (.start (java.lang.ProcessBuilder. ["sh" "-c" "echo x; exit 3"]))]
            [(.waitFor pr) (slurp (.getInputStream pr))
             (try (doto (.getOutputStream pr) (.write (.getBytes "zz")) (.flush)) :wrote
                  (catch Exception _ :threw))])
          [3 "x\n" :threw])

;; Closing a child's stdout under a thread blocked reading it ends the read with
;; EOF, as the JVM's does (it prints -1). With the readiness poller loaded, the
;; thread waits on it, and close never woke that wait, so the read hung.
(require 'jolt.io-poller)
(check-eq "closing a pipe wakes a thread blocked reading it"
          (let [pr (.start (java.lang.ProcessBuilder. ["sleep" "5"]))
                in (.getInputStream pr)
                r  (future (.read in))]
            (Thread/sleep 200)
            (.close in)
            (try (deref r 5000 :hung) (finally (.destroy pr))))
          -1)

;; A child starts with SIGPIPE at SIG_DFL, as the JVM's do (jolt-lang/jolt#1196).
;; The runtime ignores SIGPIPE for its own writes, and an ignored disposition
;; survives exec, so a producer whose consumer quit early got EPIPE and took its
;; error path ("yes: stdout: Broken pipe") instead of dying quietly. No shell can
;; undo it: POSIX forbids a non-interactive sh resetting a signal ignored on entry.
(check-eq "a child's SIGPIPE is at its default"
          (let [err (:err (sh ["sh" "-c" "yes e | head -c 100000 >&2"] {:err :string}))]
            [(count err) (str/includes? err "Broken pipe")])
          [100000 false])

(if (empty? @failures)
  (println "PROCESS-TEST OK")
  (do (doseq [f @failures] (println "FAIL:" f))
      (println "PROCESS-TEST FAILED:" (count @failures))))

;; Done with the agent system's pools (futures, agents): end them, as a JVM
;; program does, or their idle workers hold the process up for their keep-alive.
(shutdown-agents)
