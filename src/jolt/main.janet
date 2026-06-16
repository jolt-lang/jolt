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
(import ./deps :as deps)

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

    # sorted colls: their comparator-ordered entries, materialized from the
    # red-black tree via the value's own :entries op (jolt-0hbr), is all the
    # printer reads.
    (and (table? v) (= :jolt/sorted-map (v :jolt/type)))
    (do
      (push-str buf "{")
      (var first? true)
      (each e (let [ef (let [o (v :ops)] (and o (o :entries))) es (if ef (ef v) (v :entries))] (if (pvec? es) (pv->array es) es))
        (if first? (set first? false) (push-str buf ", "))
        (write-value (if (pvec? e) (pv-nth e 0) (in e 0)) buf)
        (push-str buf " ")
        (write-value (if (pvec? e) (pv-nth e 1) (in e 1)) buf))
      (push-str buf "}"))

    (and (table? v) (= :jolt/sorted-set (v :jolt/type)))
    (do
      (push-str buf "#{")
      (var first? true)
      (each x (let [ef (let [o (v :ops)] (and o (o :entries))) es (if ef (ef v) (v :entries))] (if (pvec? es) (pv->array es) es))
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
  # Key on the jolt version + entry ns + every ctx-shaping env var (ctx-cache-key,
  # jolt-q5ql): the run-mode flags this image bakes are all derived from those
  # vars, so keying on the canonical list can't miss one the way the old
  # hand-built positional key could.
  (def key (ctx-cache-key [:jolt-version jolt-version :ns ns-name]))
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
  (unless (os/getenv "JOLT_NO_DEPS_CACHE")
    # load-ctx-image forks + validates + rewires (shared with init-cached, jolt-q5ql);
    # the deps-specific validity check is the source-mtime manifest.
    (load-ctx-image path jcore/install-print-method-cb!
                (fn [c] (let [m (get (c :env) :deps-manifest)]
                          (and m (manifest-current? m)))))))

(defn- save-deps-image [c path]
  (unless (os/getenv "JOLT_NO_DEPS_CACHE")
    (put (c :env) :deps-manifest (manifest-of (or (get (c :env) :loaded-files) @[])))
    (save-ctx-image c path)))

(defn- run-main [ns-name argv]
  (when (nil? ns-name) (eprint "Error: -m/--main requires a namespace") (os/exit 1))
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
      # Bind *command-line-args* on the FINAL ctx, AFTER any cache swap: a cache
      # hit replaces ctx with the saved image, which carries the args baked when
      # it was saved — the current run's argv must win (jolt-4mui).
      (set-command-line-args argv)
      (load-string ctx (string "(apply " ns-name "/-main *command-line-args*)")))
    ([err fib] (report-error err fib) (os/exit 1))))

# --- uberscript dead-code elimination (jolt-atg) ---------------------------
# A bundle is closed-world: everything it needs is inlined and nothing is
# required afterward, so a user `defn` unreachable from the entry's reference
# graph can be dropped. Conservative + sound: only plain defn/defn- are
# prunable; a defn is kept if its (bare or ns-qualified) name appears in any
# kept form, the closure runs to a fixpoint, and any use of dynamic resolution
# disables pruning entirely. The drop is by exact source span, so formatting
# and reader macros in the surviving code are untouched.
(defn- dce-sym? [x] (and (struct? x) (= :symbol (get x :jolt/type))))
(def- dce-bailout-syms
  {"resolve" true "ns-resolve" true "requiring-resolve" true "find-var" true
   "intern" true "eval" true "load-string" true})
(defn- dce-collect-syms [form acc]
  (cond
    (dce-sym? form) (do (put acc (get form :name) true)
                        (when (get form :ns)
                          (put acc (string (get form :ns) "/" (get form :name)) true)))
    (indexed? form) (each x form (dce-collect-syms x acc))
    (or (struct? form) (table? form)) (each k (keys form)
                                        (dce-collect-syms k acc)
                                        (dce-collect-syms (get form k) acc)))
  acc)
(defn- dce-defn-name [form]
  (when (and (indexed? form) (>= (length form) 2) (dce-sym? (get form 0))
             (let [nm (get (in form 0) :name)] (or (= nm "defn") (= nm "defn-")))
             (dce-sym? (get form 1)))
    (get (in form 1) :name)))
(defn- dce-strip-spans [src dead]
  (if (empty? dead) src
    (do
      (sort dead (fn [a b] (< (a 0) (b 0))))
      (def buf @"")
      (var cur 0)
      (each [s e] dead
        (when (> s cur) (buffer/push-string buf (string/slice src cur s)))
        (set cur (max cur e)))
      (buffer/push-string buf (string/slice src cur))
      (string buf))))

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
  # read every file's source and parse it into top-level forms with byte spans;
  # if ANY file fails to parse, fall back to verbatim bundling (DCE off) so the
  # uberscript stays exactly as robust as a plain concatenation.
  (def srcs @{})
  (def file-forms @{})
  (def all-forms @[])
  (var dce-ok true)
  (each f files
    (def src (slurp f))
    (put srcs f src)
    (def lst @[])
    (try
      (each [form s e] (parse-all-spans src f)
        (def entry @{:start s :end e :dname (dce-defn-name form) :form form})
        (array/push lst entry)
        (array/push all-forms entry))
      ([_err _fib] (set dce-ok false)))
    (put file-forms f lst))
  # disable DCE if the bundle resolves names dynamically (a defn could be
  # reached by a string/symbol the reference scan can't see).
  (when dce-ok
    (def allsyms @{})
    (each e all-forms (dce-collect-syms (e :form) allsyms))
    (each nm (keys allsyms) (when (get dce-bailout-syms nm) (set dce-ok false))))
  # reachability: seed from the entry (-main) and every non-prunable form, then
  # close over the bodies of defns that become live.
  (def live @{})
  (when dce-ok
    (def referenced @{"-main" true})
    (each e all-forms (unless (e :dname) (dce-collect-syms (e :form) referenced)))
    (var changed true)
    (while changed
      (set changed false)
      (each e all-forms
        (when (and (e :dname) (not (get live e)) (get referenced (e :dname)))
          (put live e true)
          (dce-collect-syms (e :form) referenced)
          (set changed true)))))
  (var dropped 0)
  (def buf @"")
  (buffer/push-string buf (string ";; Generated by `jolt uberscript` — " (length files) " namespace(s)\n\n"))
  (each f files
    (buffer/push-string buf (string ";; --- " f " ---\n"))
    (def src (get srcs f))
    (if-not dce-ok
      (buffer/push-string buf src)
      (do
        (def dead @[])
        (each e (get file-forms f)
          (when (and (e :dname) (not (get live e)))
            (++ dropped)
            (array/push dead [(e :start) (e :end)])))
        (buffer/push-string buf (dce-strip-spans src dead))))
    (buffer/push-string buf "\n"))
  (buffer/push-string buf (string "\n(apply " main-ns "/-main *command-line-args*)\n"))
  (spit out (string buf))
  (print "Wrote " out " (" (length files) " namespace(s)"
         (if (and dce-ok (> dropped 0)) (string ", " dropped " dead fn(s) dropped") "") ")"))

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
  (print "Dependencies (deps.edn, git + :local deps — resolved into JOLT_PATH):")
  (print "  -M:a[:b] [args]       Run the alias(es) :main-opts ++ args")
  (print "  -A:a[:b] CMD [args]   Run CMD (repl/-m/nrepl-server/…) with the alias paths")
  (print "  run FILE [args]       Run FILE with deps.edn resolved")
  (print "  path                  Print the resolved source roots (':'-joined)")
  (print "  tasks                 List :tasks from deps.edn")
  (print "  task NAME [args]      Run a :tasks entry (shell string or :main-opts)")
  (print "  A deps.edn in the working dir is auto-resolved for repl/-m/-e/nrepl-server/FILE.\n")
  (print "Running a program (a file, -m/-M) direct-links by default; type inference")
  (print "and specialization are opt-in via JOLT_OPTIMIZE (the cost is paid once, then")
  (print "cached). The repl, -e, and nrepl-server stay open so you can redefine vars.")
  (print "Environment:")
  (print "  JOLT_OPTIMIZE=1         type-infer + specialize (whole-program over app nses)")
  (print "  JOLT_NO_DIRECT_LINK=1   keep a program run open/redefinable (no optimization)")
  (print "  JOLT_NO_WHOLE_PROGRAM=1 direct-link but skip the cross-namespace pass")
  (print "  JOLT_DIRECT_LINK=1      force direct-linking + optimization on (e.g. for -e)")
  (print "  JOLT_WHOLE_PROGRAM=1    force the whole-program pass on")
  (print "  JOLT_INTERPRET=1        run the tree-walking interpreter\n")
  (print "  JOLT_PATH=dir1:dir2     extra source roots (set automatically by deps resolution)"))

(def- help-flags    {"-h" true "--help" true "help" true "-?" true})
(def- version-flags {"--version" true "version" true})
(def- nrepl-flags   {"nrepl-server" true "--nrepl-server" true "nrepl" true})
(def- eval-flags    {"-e" true "--eval" true})
(def- file-flags    {"-f" true "--file" true})
(def- main-flags    {"-m" true "--main" true})

# --- deps.edn integration (the old jolt-deps tool, folded in) ----------------
# The runtime stays deps-agnostic: it reads source roots from JOLT_PATH (applied
# in `main`, below). This layer resolves a deps.edn into those roots IN-PROCESS,
# so a single `jolt` binary both resolves dependencies and runs code. ./deps
# loads jpm (git fetch + cache) lazily — only at an actual resolve — so a run
# with no deps.edn never pulls it in, and an app baked from its own entry (which
# imports jolt/api, not this CLI) never links the resolver at all.

(defn- parse-alias-flag
  ``"-A:dev:test" / "-M:dev" -> [:dev :test].``
  [arg]
  (map keyword (filter |(not= "" $) (string/split ":" (string/slice arg 2)))))

(defn- deps-roots [aliases]
  (if (os/stat "deps.edn") (deps/resolve-deps-cached "deps.edn" nil aliases) @[]))

(defn- set-deps-env! [aliases]
  # Prepend the resolved roots to any existing JOLT_PATH; `main` (below) applies
  # them to the ctx source-paths. JOLT_APP_PATHS scopes whole-program inference
  # to the project's own namespaces — deps load-infer per-ns instead (jolt-87e).
  (def rs (string/join (deps-roots aliases) ":"))
  (def existing (os/getenv "JOLT_PATH"))
  (os/setenv "JOLT_PATH" (if (and existing (> (length existing) 0)) (string rs ":" existing) rs))
  (when (os/stat "deps.edn")
    (os/setenv "JOLT_APP_PATHS" (string/join (deps/project-source-roots "deps.edn" aliases) ":"))))

(defn- resolve-deps-argv
  ``Resolve deps.edn (when relevant) and de-sugar the deps subcommands into a
  plain runtime argv that `main` then dispatches on. The pure deps queries
  (path/tasks/task) print and exit. A runnable command resolves a deps.edn in
  cwd when one is present — so `jolt repl` / `-m` / `nrepl-server` pick up the
  project and its dependencies — and is a no-op (resolver untouched) with none.``
  [argv]
  # leading -A:alias flags apply to whatever command follows
  (var aliases nil)
  (var rest argv)
  (while (string/has-prefix? "-A" (or (get rest 0) ""))
    (set aliases (array/concat (or aliases @[]) (parse-alias-flag (get rest 0))))
    (set rest (array/slice rest 1)))
  (def cmd (get rest 0))
  (cond
    # pure deps queries — produce output and exit, never start the runtime
    (= cmd "path")
      (do (print (string/join (deps-roots aliases) ":")) (os/exit 0))
    (= cmd "tasks")
      (do (each row (deps/tasks "deps.edn")
            (print (row 0) (if (row 1) (string "\t" (row 1)) "")))
          (os/exit 0))
    (= cmd "task")
      (let [name (get rest 1)
            spec (when name (deps/task-spec "deps.edn" name))]
        (cond
          (nil? name) (do (eprint "jolt: task needs a name") (os/exit 1))
          (nil? spec) (do (eprint "jolt: no such task: " name) (os/exit 1))
          (= :shell (spec :type))
            (os/exit (os/execute ["sh" "-c" (string/join [(spec :cmd) ;(array/slice rest 2)] " ")] :p))
          (do (set-deps-env! aliases)
              (array/concat @[] (spec :argv) (array/slice rest 2)))))
    # -M:aliases — resolve and run the alias's :main-opts ++ extra args
    (and cmd (string/has-prefix? "-M" cmd))
      (let [als (array/concat (or aliases @[]) (parse-alias-flag cmd))
            mo (deps/alias-main-opts "deps.edn" als)]
        (if mo
          (do (set-deps-env! als) (array/concat @[] mo (array/slice rest 1)))
          (do (eprint "jolt: no :main-opts in alias(es) " (string/format "%j" (map string als)))
              (os/exit 1))))
    # explicit `run FILE` — resolve, then run the file
    (= cmd "run")
      (do (set-deps-env! aliases) (array/slice rest 1))
    # any runnable command: resolve a deps.edn if present (or if -A forced it);
    # help/version never resolve, and with no deps.edn + no -A the resolver and
    # jpm are never touched.
    (and (not (help-flags cmd)) (not (version-flags cmd))
         (or aliases (os/stat "deps.edn")))
      (do (set-deps-env! aliases) rest)
    # nothing deps-related — argv unchanged
    true rest))

(defn main [&]
  (def args (or (dyn :args) @[]))            # @["jolt" arg1 arg2 ...]
  # Resolve deps.edn + de-sugar deps subcommands (jolt-deps folded in): sets
  # JOLT_PATH/JOLT_APP_PATHS in our env (read just below) and rewrites argv to a
  # plain runtime command. A no-deps invocation passes through untouched.
  (def argv (resolve-deps-argv (if (> (length args) 1) (array/slice args 1) @[])))
  (ctx-set-current-ns ctx "user")
  # JOLT_PATH must be applied at runtime: this `ctx` is built into the image at
  # build time, so its source-paths can't capture the runtime environment.
  # resolve-deps-argv (above) sets it from the resolved deps.edn source roots.
  (when-let [jp (os/getenv "JOLT_PATH")]
    (each p (string/split ":" jp)
      (when (> (length p) 0) (array/push (get (ctx :env) :source-paths) p))))
  # JOLT_APP_PATHS (jolt-87e), likewise applied at runtime: the project's own
  # source roots, set by jolt-deps. Whole-program inference is scoped to
  # namespaces under these (deps compile at default cost), so a dep-heavy app's
  # optimize-mode startup doesn't re-infer every transitive dependency. Read here
  # for the same baked-at-build-time reason as JOLT_PATH above.
  (let [aps @[]]
    (when-let [jap (os/getenv "JOLT_APP_PATHS")]
      (each p (string/split ":" jap) (when (> (length p) 0) (array/push aps p))))
    (put (ctx :env) :app-source-paths aps))
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
  # Run-mode policy lives in config/resolve-run-mode now (jolt-q5ql) — unit-
  # testable without the CLI, and the same canonical knob list backs the cache
  # keys. Install the resolved knobs onto the runtime env (the baked ctx computed
  # them at build time; a program run recomputes from the live env).
  (let [mode (resolve-run-mode open-mode? main-entry?)]
    (eachp [k v] mode (put (ctx :env) k v)))
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
