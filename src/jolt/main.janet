# Jolt REPL
# Read-eval-print loop for Clojure expressions.

(use ./api)
(use ./types)
(use ./phm)
(use ./pv)
(use ./plist)
(use ./config)
(use ./reader)

(def jolt-version "0.1.0")

# Compile by default: the shipped runtime runs each form through the self-hosted
# pipeline (portable Clojure analyzer -> IR -> Janet back end) to native bytecode
# (hybrid — forms the analyzer can't compile fall back to the interpreter, so the
# result always matches the interpreter; see backend.janet / loader/eval-toplevel).
# Set JOLT_INTERPRET=1 to force the tree-walking interpreter (debugging / A-B).
(def compile-default? (not (= "1" (os/getenv "JOLT_INTERPRET"))))
(def ctx (init {:compile? compile-default?}))
(ctx-set-current-ns ctx "user")

(defn read-line [prompt]
  (prin prompt)
  (flush)
  (let [line (file/read stdin :line)]
    (if line (string/trim line) nil)))

# Forward declaration for mutual recursion
(var write-value nil)

(defn- push-str [buf s]
  (buffer/push-string buf s))

(defn- write-collection [v buf]
  (cond
    (pvec? v)
    (do
      (push-str buf "[")
      (let [a (pv->array v) n (pv-count v)]
        (var i 0)
        (while (< i n)
          (write-value (in a i) buf)
          (when (< (+ i 1) n) (push-str buf " "))
          (++ i)))
      (push-str buf "]"))

    (plist? v)
    (do
      (push-str buf "(")
      (let [a (pl->array v) n (length a)]
        (var i 0)
        (while (< i n)
          (write-value (in a i) buf)
          (when (< (+ i 1) n) (push-str buf " "))
          (++ i)))
      (push-str buf ")"))

    (tuple? v)
    (do
      (push-str buf "[")
      (var i 0)
      (let [n (length v)]
        (while (< i n)
          (write-value (in v i) buf)
          (when (< (+ i 1) n) (push-str buf " "))
          (++ i)))
      (push-str buf "]"))

    # LazySeq — realize the cell chain and print as a list. Capped to avoid
    # hanging on infinite sequences; prints "..." when truncated.
    (and (table? v) (= :jolt/lazy-seq (v :jolt/type)))
    (do
      (push-str buf "(")
      (var cur v)
      (var i 0)
      (var go true)
      (while (and go (< i 1000))
        (let [cell (realize-ls cur)]
          (if (or (nil? cell) (= :jolt/pending cell) (= 0 (length cell)))
            (set go false)
            (do
              (when (> i 0) (push-str buf " "))
              (write-value (in cell 0) buf)
              (++ i)
              (let [rt (in cell 1)]
                (if (nil? rt) (set go false) (set cur (make-lazy-seq rt))))))))
      (when (and go (>= i 1000)) (push-str buf " ..."))
      (push-str buf ")"))

    (array? v)
    (do
      # mutable mode: arrays are vectors -> [] ; immutable: arrays are lists -> ()
      (push-str buf (if mutable? "[" "("))
      (var i 0)
      (let [n (length v)]
        (while (< i n)
          (write-value (in v i) buf)
          (when (< (+ i 1) n) (push-str buf " "))
          (++ i)))
      (push-str buf (if mutable? "]" ")")))

    (and (table? v) (= :jolt/set (v :jolt/type)))
    (do
      (push-str buf "#{")
      (var first? true)
      (each k (phs-seq v)
        (if first? (set first? false) (push-str buf " "))
        (write-value k buf))
      (push-str buf "}"))

    (and (table? v) (= :jolt/transient (v :jolt/type)))
    (push-str buf (string "#<transient " (v :kind) ">"))

    (and (table? v) (= :jolt/chan (v :jolt/type)))
    (push-str buf "#<channel>")

    (phm? v)
    (do
      (push-str buf "{")
      (var first? true)
      (each pair (phm-entries v)
        (if first? (set first? false) (push-str buf ", "))
        (write-value (in pair 0) buf) (push-str buf " ") (write-value (in pair 1) buf))
      (push-str buf "}"))

    (and (table? v) (= :jolt/regex (v :jolt/type)))
    (do (push-str buf "#\"") (push-str buf (v :source)) (push-str buf "\""))

    # sorted colls: their comparator-ordered :entries vector (a pvec in
    # immutable mode, an array in mutable mode) is all the printer reads.
    (and (table? v) (= :jolt/sorted-map (v :jolt/type)))
    (do
      (push-str buf "{")
      (var first? true)
      (each e (let [es (v :entries)] (if (pvec? es) (pv->array es) es))
        (if first? (set first? false) (push-str buf ", "))
        (write-value (if (pvec? e) (pv-nth e 0) (in e 0)) buf)
        (push-str buf " ")
        (write-value (if (pvec? e) (pv-nth e 1) (in e 1)) buf))
      (push-str buf "}"))

    (and (table? v) (= :jolt/sorted-set (v :jolt/type)))
    (do
      (push-str buf "#{")
      (var first? true)
      (each x (let [es (v :entries)] (if (pvec? es) (pv->array es) es))
        (if first? (set first? false) (push-str buf " "))
        (write-value x buf))
      (push-str buf "}"))

    (and (table? v) (get v :jolt/deftype))
    (do
      (push-str buf "{")
      (var first? true)
      (each [k val] (pairs v)
        (when (and (not= k :jolt/deftype) (not= k :cnt) (not= k :buckets)
                   (not= k :_meta) (not= k :jolt/type) (not= k :phm))
          (if first? (set first? false) (push-str buf " "))
          (write-value k buf)
          (push-str buf " ")
          (write-value val buf)))
      (push-str buf "}"))

    (struct? v)
    (do
      (push-str buf "{")
      (var first? true)
      (each [k val] (pairs v)
        (if first? (set first? false) (push-str buf " "))
        (write-value k buf)
        (push-str buf " ")
        (write-value val buf))
      (push-str buf "}"))

    (table? v)
    (do
      (push-str buf "{")
      (var first? true)
      (each [k val] (pairs v)
        (when (not= k :jolt/type)
          (if first? (set first? false) (push-str buf " "))
          (write-value k buf)
          (push-str buf " ")
          (write-value val buf)))
      (push-str buf "}"))))

(set write-value (fn [v buf]
  (cond
    (nil? v) (push-str buf "nil")
    (= true v) (push-str buf "true")
    (= false v) (push-str buf "false")
    (number? v) (push-str buf (string v))
    (string? v) (push-str buf v)
    (keyword? v) (do (push-str buf ":") (push-str buf (string v)))
    (and (struct? v) (= :jolt/char (get v :jolt/type)))
    (do (push-str buf "\\")
        (push-str buf (case (v :ch)
                        10 "newline" 32 "space" 9 "tab" 13 "return"
                        12 "formfeed" 8 "backspace" 0 "nul"
                        (string/from-bytes (v :ch)))))
    (and (struct? v) (= :symbol (get v :jolt/type)))
    (let [ns (get v :ns) name (get v :name)]
      (if ns
        (push-str buf (string ns "/" name))
        (push-str buf name)))
    (and (table? v) (= :jolt/var (v :jolt/type)))
    (push-str buf (string "#'" (ctx-current-ns ctx) "/" (var-name v)))
    (or (tuple? v) (array? v) (struct? v) (table? v))
    (write-collection v buf)
    true (push-str buf (string v)))))

(defn print-value [v]
  (def buf @"")
  (write-value v buf)
  (print (string buf)))

(defn- err-message [err]
  (cond
    (string? err) err
    (and (or (table? err) (struct? err)) (= :jolt/exception (get err :jolt/type)))
      (err-message (get err :value))
    (and (or (table? err) (struct? err)) (= :jolt/ex-info (get err :jolt/type)))
      (let [m (get err :message) d (get err :data)]
        (if (and d (not (empty? d))) (string m " " (string/format "%q" d)) (string m)))
    (string? err) err
    (string/format "%q" err)))

(defn- report-error [err fib]
  (eprint "Error: " (err-message err))
  # Janet-level stack trace of where evaluation failed
  (when fib (debug/stacktrace fib "")))

(defn- run-repl []
  (print "Jolt — Clojure on Janet")
  (print "Type (exit) to quit.\n")
  (var running true)
  (var pending "")   # accumulates a form split across multiple input lines
  (while running
    (let [prompt (if (= pending "") (string (ctx-current-ns ctx) "=> ") "  #_=> ")
          line (read-line prompt)]
      (cond
        (nil? line) (set running false)
        (let [input (if (= pending "") line (string pending "\n" line))
              trimmed (string/trim input)]
          (cond
            (= trimmed "(exit)") (set running false)
            (= trimmed "") (set pending "")
            # Try to parse the accumulated input; if it's an incomplete form
            # (unterminated list/vector/map/string), keep reading more lines.
            (let [parsed (protect (parse-string input))]
              (if (and (= (parsed 0) false)
                       (string/find "nterminated" (string (parsed 1))))
                (set pending input)
                (do
                  (set pending "")
                  (try
                    (print-value (eval-string ctx input))
                    ([err fib] (report-error err fib))))))))))))

(defn- set-command-line-args [argv]
  # bind clojure.core/*command-line-args* to a vector of the remaining args
  (ns-intern (ctx-find-ns ctx "clojure.core") "*command-line-args*"
             (tuple/slice (tuple ;argv))))

(defn- run-file [path argv]
  (set-command-line-args argv)
  (ns-intern (ctx-find-ns ctx "clojure.core") "*file*" path)
  (if (not (os/stat path))
    (do (eprint "Error: file not found: " path) (os/exit 1))
    (let [src (slurp path)]
      (try
        (load-string ctx src)
        ([err fib] (report-error err fib) (os/exit 1))))))

(defn- run-eval [expr argv]
  (set-command-line-args argv)
  (try
    (let [v (load-string ctx expr)]
      (when (not (nil? v)) (print-value v)))
    ([err fib] (report-error err fib) (os/exit 1))))

(defn- ensure-nrepl-loaded []
  # jolt.nrepl is part of the baked-in stdlib, so require finds it anywhere.
  (eval-string ctx "(require '[jolt.nrepl])"))

(defn- run-nrepl [argv]
  # addr is [host:]port; bare number is a port. Default 127.0.0.1:7888.
  (def addr (get argv 0))
  (var host "127.0.0.1")
  (var port 7888)
  (when addr
    (if-let [i (string/find ":" addr)]
      (do (when (> i 0) (set host (string/slice addr 0 i)))
          (set port (scan-number (string/slice addr (+ i 1)))))
      (set port (scan-number addr))))
  (ensure-nrepl-loaded)
  (eval-string ctx (string "(jolt.nrepl/start-server! {:host \"" host "\" :port " port "})"))
  # Editors auto-discover the port from this file (nREPL convention).
  (spit ".nrepl-port" (string port))
  # Remove .nrepl-port on exit — on a clean unwind (defer) and on Ctrl-C/SIGTERM
  # (signal handlers). A hard SIGKILL can't be caught, so it may still be left.
  (def cleanup (fn [&] (protect (os/rm ".nrepl-port"))))
  (os/sigaction :int (fn [&] (cleanup) (os/exit 0)) true)
  (os/sigaction :term (fn [&] (cleanup) (os/exit 0)) true)
  (print "Jolt nREPL server started on " host ":" port)
  (print "Wrote .nrepl-port — connect your editor; Ctrl-C to stop.")
  (flush)
  # Keep the main fiber alive so the event loop serves connections.
  (defer (cleanup)
    (forever (ev/sleep 60))))

(defn- print-version []
  (print "jolt v" jolt-version))

(defn- run-main [ns-name argv]
  (when (nil? ns-name) (eprint "Error: -m/--main requires a namespace") (os/exit 1))
  (set-command-line-args argv)
  (try
    (do
      (load-string ctx (string "(require '[" ns-name "])"))
      (load-string ctx (string "(apply " ns-name "/-main *command-line-args*)")))
    ([err fib] (report-error err fib) (os/exit 1))))

(defn- run-uberscript [out main-ns]
  # Bundle main-ns and everything it requires (from JOLT_PATH roots) into one
  # .clj that runs on a plain jolt — no deps, no jpm. We require the entry and
  # collect the load order the loader records (deps before dependents).
  (when (or (nil? out) (nil? main-ns))
    (eprint "Usage: jolt uberscript OUT.clj -m NS") (os/exit 1))
  (put (ctx :env) :loaded-files @[])
  (try
    (load-string ctx (string "(require '[" main-ns "])"))
    ([err fib] (report-error err fib) (os/exit 1)))
  (def seen @{})
  (def files @[])
  (each f (get (ctx :env) :loaded-files)
    (unless (get seen f) (put seen f true) (array/push files f)))
  (def buf @"")
  (buffer/push-string buf (string ";; Generated by `jolt uberscript` — " (length files) " namespace(s)\n\n"))
  (each f files
    (buffer/push-string buf (string ";; --- " f " ---\n"))
    (buffer/push-string buf (slurp f))
    (buffer/push-string buf "\n"))
  (buffer/push-string buf (string "\n(apply " main-ns "/-main *command-line-args*)\n"))
  (spit out (string buf))
  (print "Wrote " out " (" (length files) " namespace(s))"))

(defn- print-help []
  (print "Jolt — a Clojure interpreter on Janet\n")
  (print "Usage: jolt [opt] [args]\n")
  (print "  (no args), repl       Start a REPL")
  (print "  FILE [args]           Run a Clojure file (binds *command-line-args*, *file*)")
  (print "  -                     Run a program read from stdin")
  (print "  -e, --eval EXPR       Evaluate EXPR and print the result")
  (print "  -f, --file FILE       Run a Clojure file")
  (print "  -m, --main NS [args]  Require NS and call its -main with the remaining args")
  (print "  nrepl-server [addr]   Start an nREPL server (addr = [host:]port, default 7888)")
  (print "                          (aliases: --nrepl-server, nrepl)")
  (print "  uberscript OUT -m NS  Bundle NS + its required namespaces into one .clj")
  (print "  --version, version    Print the Jolt version")
  (print "  -h, --help, help      Show this help\n")
  (print "Dependencies (deps.edn) are handled by the separate jolt-deps tool."))

(def- help-flags    {"-h" true "--help" true "help" true "-?" true})
(def- version-flags {"--version" true "version" true})
(def- nrepl-flags   {"nrepl-server" true "--nrepl-server" true "nrepl" true})
(def- eval-flags    {"-e" true "--eval" true})
(def- file-flags    {"-f" true "--file" true})
(def- main-flags    {"-m" true "--main" true})

(defn main [&]
  (def args (or (dyn :args) @[]))            # @["jolt" arg1 arg2 ...]
  (def argv (if (> (length args) 1) (array/slice args 1) @[]))
  (ctx-set-current-ns ctx "user")
  # JOLT_PATH must be applied at runtime: this `ctx` is built into the image at
  # build time, so its source-paths can't capture the runtime environment.
  # `jolt-deps` sets JOLT_PATH to the resolved deps.edn source roots.
  (when-let [jp (os/getenv "JOLT_PATH")]
    (each p (string/split ":" jp)
      (when (> (length p) 0) (array/push (get (ctx :env) :source-paths) p))))
  (cond
    (empty? argv) (run-repl)
    (help-flags (argv 0)) (print-help)
    (version-flags (argv 0)) (print-version)
    (= (argv 0) "repl") (run-repl)
    (nrepl-flags (argv 0)) (run-nrepl (array/slice argv 1))
    (eval-flags (argv 0)) (run-eval (get argv 1 "") (array/slice argv 2))
    (file-flags (argv 0)) (run-file (get argv 1) (array/slice argv 2))
    (main-flags (argv 0)) (run-main (get argv 1) (array/slice argv 2))
    (= (argv 0) "uberscript")
      (let [out (get argv 1)
            rest (array/slice argv 2)
            mi (or (index-of "-m" rest) (index-of "--main" rest))]
        (run-uberscript out (if mi (get rest (+ mi 1)) nil)))
    (= (argv 0) "-") (run-file "/dev/stdin" (array/slice argv 1))
    (run-file (argv 0) (array/slice argv 1))))
