# Jolt REPL
# Read-eval-print loop for Clojure expressions.

(use ./api)
(use ./types)
(use ./phm)
(use ./phs)
(use ./lazyseq)
(use ./pv)
(use ./plist)
(use ./config)
(use ./reader)
(import ./core :as jcore)

(def jolt-version "0.1.0")

# Compile by default: the shipped runtime runs each form through the self-hosted
# pipeline (portable Clojure analyzer -> IR -> Janet back end) to native bytecode
# (hybrid — forms the analyzer can't compile fall back to the interpreter, so the
# result always matches the interpreter; see backend.janet / loader/eval-toplevel).
# Set JOLT_INTERPRET=1 to force the tree-walking interpreter (debugging / A-B).
(def compile-default? (not (= "1" (os/getenv "JOLT_INTERPRET"))))
# A var, not a def: a -m run may replace it with a forked deps-image ctx that
# already has every dependency compiled (see run-main / the deps image cache).
(var ctx (init {:compile? compile-default?}))
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
                (if (nil? rt) (set go false) (set cur (ls-rest-cached cur rt))))))))
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

# --- error presentation (jolt-2o7.2, rephrase-inspired) ----------------------
# Host messages rewritten into Clojure-shaped ones; stack frames filtered to
# the USER'S code (compiled jolt fns carry _r$ns/name--N janet names — round
# 2o7.1). JOLT_DEBUG=1 restores the raw janet trace for jolt development.

(defn- demangle
  "_r$app.deep/level3--105 -> app.deep/level3 (nil for non-jolt names)."
  [nm]
  (when (string/has-prefix? "_r$" nm)
    (def s (string/slice nm 3))
    # the counter suffix is the LAST --N run
    (var cut (length s))
    (var i (string/find "--" s))
    (while (not (nil? i))
      (set cut i)
      (set i (string/find "--" s (+ i 1))))
    (string/slice s 0 cut)))

(defn- fmt-val [x]
  (cond
    (string? x) (string `"` x `"`)
    (nil? x) "nil"
    (string x)))

(def- op-words
  {"+" "add" "-" "subtract" "*" "multiply" "/" "divide"
   "<" "compare" ">" "compare" "<=" "compare" ">=" "compare"})

(defn- rewrite-message
  "Host/janet error text -> a user-facing message. Unknown messages verbatim."
  [msg]
  (def msg (string msg))
  (cond
    # janet polymorphic arithmetic. Binary: "could not find method :+ for 1 or
    # :r+ for "a"". Unary (inc/dec/-): "could not find method :+ for "x"" — no
    # "or :r" clause, so orpos is nil; handle both without crashing the reporter.
    (string/has-prefix? "could not find method :" msg)
    (let [rest* (string/slice msg (length "could not find method :"))
          sp (string/find " " rest*)
          op (string/slice rest* 0 sp)
          tail (string/slice rest* (+ sp (length " for ")))
          orpos (string/find " or :r" tail)]
      (if (nil? orpos)
        # unary form: one operand
        (string "Cannot " (get op-words op op) " " tail
                " — " op " expects numbers")
        (let [a (string/slice tail 0 orpos)
              forpos (string/find " for " tail (+ orpos 1))
              b (string/slice tail (+ forpos 5))]
          (string "Cannot " (get op-words op op) " " a " and " b
                  " — " op " expects numbers"))))
    # janet fixed-arity: <function _r$ns/f--N> called with 2 arguments, expected 1
    (and (string/has-prefix? "<function " msg) (string/find "> called with " msg))
    (let [nm-end (string/find ">" msg)
          nm (string/slice msg (length "<function ") nm-end)
          pretty (or (demangle nm) nm)
          tail (string/slice msg (+ nm-end (length "> called with ")))
          n-end (string/find " " tail)
          n (string/slice tail 0 n-end)
          exp (if-let [i (string/find "expected " tail)]
                (string " (expected " (string/slice tail (+ i 9)) ")") "")]
      (string "Wrong number of args (" n ") passed to: " pretty exp))
    # a typo'd symbol compiles to nil today (round 2o7.3 will fix resolution)
    (= msg "Cannot call nil as a function")
    (string msg " — often an undefined (misspelled?) symbol")
    msg))

(defn- print-user-trace
  "Filter the stashed janet trace down to frames a jolt user can act on:
  compiled jolt fns (the _r$ns/name--N janet names, demangled) and frames
  from non-jolt source files. Internal janet/jolt frames are dropped."
  [trace]
  (var shown 0)
  (each line (string/split "\n" trace)
    (def t (string/trim line))
    (when (string/has-prefix? "in " t)
      (def rest* (string/slice t 3))
      (def sp (or (string/find " " rest*) (length rest*)))
      (def nm (string/slice rest* 0 sp))
      (cond
        (string/has-prefix? "_r$" nm)
        (do (eprint "  at " (demangle nm)) (++ shown))
        # a frame from a real source file outside jolt internals
        (and (string/find "[" rest*)
             (not (string/find "src/jolt/" rest*))
             (not (string/find "boot.janet" rest*))
             (not (string/find "[eval]" rest*)))
        (do (eprint "  " t) (++ shown))
        nil)))
  shown)

(defn- report-error [err fib]
  (eprint "Error: " (rewrite-message (err-message err)))
  (def env (ctx :env))
  (def stashed (get env :error-trace))
  (def pos (get env :error-pos))
  (def chain (get env :error-loading))
  (put env :error-trace nil)
  (put env :error-pos nil)
  (put env :error-loading nil)
  # <eval>:1 is the synthetic require/apply string the CLI feeds itself (and
  # any one-line -e) — no information there
  (when (and pos (not (and (= (pos :file) "<eval>") (= (pos :line) 1))))
    (eprint "  at " (pos :file) ":" (pos :line)))
  (cond
    (os/getenv "JOLT_DEBUG")
      (if stashed (eprin stashed) (when fib (debug/stacktrace fib "")))
    stashed (print-user-trace stashed)
    fib (when fib nil))
  # requires unwound through, innermost first; the failing file itself is
  # already on the 'at' line
  (when chain
    (each f chain
      (unless (or (= f "<eval>") (and pos (= f (pos :file))))
        (eprint "  while loading " f)))))

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
        (load-string ctx src path)
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

# --- Deps image cache: compile the dependencies ONCE -------------------------
# The baked binary loads core in ~10ms (it's marshaled in), but a program still
# re-compiles ALL of its dependency namespaces (reitit, ring, …) from source on
# every run — seconds of analyzer->IR->emit that never change between runs. Like
# Clojure's AOT or Stalin's compile-once model, snapshot the ctx AFTER the
# require chain and reuse it: the first run compiles + caches, later runs fork
# the image (~10ms) and skip compilation entirely. The key includes the jolt
# version, entry ns, source roots, and the compile flags; the image carries a
# manifest of every loaded source file's mtime, so any source edit (app or dep)
# invalidates it. JOLT_NO_DEPS_CACHE=1 disables.
(defn- deps-image-path [ns-name]
  (def dir (or (os/getenv "JOLT_IMAGE_CACHE_DIR") (os/getenv "TMPDIR") "/tmp"))
  (def env (ctx :env))
  (def key (string/format "%q|%q|%q|%q|%q|%q|%q|%q|%q"
                          jolt-version ns-name (os/getenv "JOLT_PATH")
                          (get env :direct-linking?) (get env :inline?)
                          (get env :shapes?) (get env :map-shapes?)
                          (get env :whole-program?)
                          (os/getenv "JOLT_FEATURES")))
  (string dir "/jolt-deps-" (band (hash key) 0x7FFFFFFF) ".jimg"))

(defn- manifest-of [files]
  (def m @{})
  (each f files (when-let [st (os/stat f)] (put m f (st :modified))))
  m)

(defn- manifest-current? [manifest]
  (var ok true)
  (eachp [f mt] manifest
    (def st (os/stat f))
    (unless (and st (= (st :modified) mt)) (set ok false)))
  ok)

(defn- try-load-deps-image [path]
  (when (and (not (os/getenv "JOLT_NO_DEPS_CACHE")) (os/stat path))
    (def r (protect (fork (slurp path))))
    (when (and (r 0) (ctx? (r 1)))
      (def c (r 1))
      (def manifest (get (c :env) :deps-manifest))
      (when (and manifest (manifest-current? manifest))
        # per-process wiring an image restore must redo (module state, not in the
        # marshaled ctx) — same as init-cached does for the core image.
        (jcore/install-print-method-cb! c)
        c))))

(defn- save-deps-image [c path]
  (unless (os/getenv "JOLT_NO_DEPS_CACHE")
    (put (c :env) :deps-manifest (manifest-of (or (get (c :env) :loaded-files) @[])))
    (def tmp (string path "." (os/getpid) ".tmp"))
    (when (protect (spit tmp (snapshot c)))
      (protect (os/rename tmp path)))))

(defn- run-main [ns-name argv]
  (when (nil? ns-name) (eprint "Error: -m/--main requires a namespace") (os/exit 1))
  (set-command-line-args argv)
  (try
    (do
      (def path (deps-image-path ns-name))
      (if-let [cached (try-load-deps-image path)]
        # cache hit: every dependency is already compiled in the image
        (do (set ctx cached) (ctx-set-current-ns ctx "user"))
        # cache miss: compile the requires, then snapshot for next time. Track the
        # loaded files for the manifest.
        (do
          (put (ctx :env) :loaded-files @[])
          (load-string ctx (string "(require '[" ns-name "])"))
          # whole-program (jolt-t34): every unit is loaded now — run the one
          # closed-world fixpoint over all of them before -main.
          (when (get (ctx :env) :whole-program?)
            (when-let [ip (get (ctx :env) :infer-program!)] (protect (ip ctx)))
            (put (ctx :env) :infer-program-done? true))
          (save-deps-image ctx path)))
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
  (print "Running a program (a file, -m/-M) direct-links and optimizes by default;")
  (print "the repl, -e, and nrepl-server stay open so you can redefine vars.")
  (print "Environment:")
  (print "  JOLT_NO_DIRECT_LINK=1   keep a program run open/redefinable (no optimization)")
  (print "  JOLT_NO_WHOLE_PROGRAM=1 direct-link but skip the cross-namespace pass")
  (print "  JOLT_DIRECT_LINK=1      force direct-linking on (e.g. for -e)")
  (print "  JOLT_WHOLE_PROGRAM=1    force the whole-program pass on")
  (print "  JOLT_INTERPRET=1        run the tree-walking interpreter\n")
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
  # JOLT_FEATURES, likewise, must be applied at runtime: reader-features-set!
  # runs at module load, which for a baked binary is BUILD time — so a process
  # that sets JOLT_FEATURES (e.g. to read a clj-targeted lib's :clj branches)
  # would otherwise be ignored, and unmatched #?(...) forms silently splice to
  # nothing. Re-read it here so the env wins in the running process.
  (when-let [jf (os/getenv "JOLT_FEATURES")]
    (reader-features-set! (filter |(> (length $) 0) (string/split "," jf))))
  # Linking default depends on the run MODE. Running a PROGRAM (a file, -f, -m/-M,
  # stdin) is a closed world — all code is loaded, then it executes to completion
  # — so it direct-links by default: user code gets inlining + shapes + the type
  # inference's specialization (jolt-87f). INTERACTIVE modes (repl, -e, the nREPL
  # server) stay indirect/open so redefinition works — direct-linking would seal
  # callers against a redef. Explicit env always wins: JOLT_NO_DIRECT_LINK forces
  # the open path even for a program run (runtime redefinition / hot-reload),
  # JOLT_DIRECT_LINK forces it on even for -e. Core is already compiled into the
  # image; this only governs user code compiled at runtime.
  (def open-mode?
    (or (empty? argv)
        (help-flags (argv 0)) (version-flags (argv 0))
        (= (argv 0) "repl") (nrepl-flags (argv 0)) (eval-flags (argv 0))
        (= (argv 0) "uberscript")))
  (def main-entry? (and (not (empty? argv)) (main-flags (argv 0))))
  (def dl-forced
    (cond (os/getenv "JOLT_NO_DIRECT_LINK") :off
          (= "1" (os/getenv "JOLT_DIRECT_LINK")) :on
          :none))
  (def dl (case dl-forced :off false :on true (not open-mode?)))
  (put (ctx :env) :direct-linking? dl)
  # Inference / specialization (inlining + scalar replacement + structural type
  # inference + the per-namespace re-emit fixpoint) is the EXPENSIVE part — like
  # Stalin's whole-program analysis or a Clojure AOT pass, it belongs at BUILD
  # time or behind an explicit opt-in, not paid on every program startup. Direct-
  # linking (cheap compile-time call resolution; FASTER startup + calls, per
  # Clojure's :direct-linking) does not need it. So default it OFF and turn it on
  # with JOLT_OPTIMIZE (or an explicit JOLT_DIRECT_LINK, which signals a fully
  # optimized build). The native build bakes an optimized image once.
  (def optimize? (and dl (truthy? (or (os/getenv "JOLT_OPTIMIZE")
                                       (= "1" (os/getenv "JOLT_DIRECT_LINK"))))))
  (put (ctx :env) :inline? optimize?)
  # Mark direct-linking that was AUTO-enabled by the run mode (vs explicitly
  # requested). The success checker (RFC 0006) rides on the inference for free,
  # but a casual program run shouldn't spam type warnings just because it now
  # direct-links — so the checker's default-on is suppressed in the auto case
  # (JOLT_TYPE_CHECK still opts in). API/build callers never set this flag, so an
  # explicit :direct-linking? there keeps the checker as before. See backend.
  (put (ctx :env) :direct-link-auto? (and dl (= dl-forced :none)))
  # Shape gates, recomputed from the runtime env (the baked ctx computed them at
  # build time). :shapes? = shape-recs active (records), on with direct-linking;
  # :map-shapes? = also shape generic maps (opt-in JOLT_SHAPE).
  (put (ctx :env) :shapes?
    (and dl (not (os/getenv "JOLT_NO_SHAPE"))))
  (put (ctx :env) :map-shapes?
    (and (os/getenv "JOLT_SHAPE") (not (os/getenv "JOLT_NO_SHAPE"))))
  # Whole-program (closed-world cross-namespace inference) auto-enables for a
  # -m/-M program entry under direct-linking — that's the point where every
  # require is done and -main is about to run. JOLT_WHOLE_PROGRAM forces it on in
  # other direct-linked modes; JOLT_NO_WHOLE_PROGRAM opts out. Namespaces required
  # later (inside -main) fall back to per-ns inference (see loader).
  (put (ctx :env) :whole-program?
    (and optimize?
         (not (os/getenv "JOLT_NO_WHOLE_PROGRAM"))
         (or main-entry? (os/getenv "JOLT_WHOLE_PROGRAM"))))
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
