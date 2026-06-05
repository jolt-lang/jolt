# Jolt REPL
# Read-eval-print loop for Clojure expressions.

(use ./api)
(use ./types)
(use ./phm)
(use ./pv)
(use ./plist)
(use ./config)
(use ./reader)

(def ctx (init))
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

    (and (table? v) (= :jolt/sorted-map (v :jolt/type)))
    (do
      (push-str buf "{")
      (var first? true)
      (each k (sort (array ;(keys (v :map))))
        (if first? (set first? false) (push-str buf ", "))
        (write-value k buf) (push-str buf " ") (write-value (get (v :map) k) buf))
      (push-str buf "}"))

    (and (table? v) (= :jolt/sorted-set (v :jolt/type)))
    (do
      (push-str buf "#{")
      (var first? true)
      (each x (v :items)
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

(defn- print-help []
  (print "Jolt — a Clojure interpreter on Janet\n")
  (print "Usage:")
  (print "  jolt                 Start a REPL")
  (print "  jolt FILE.clj [args] Run a Clojure file (binds *command-line-args*)")
  (print "  jolt -e EXPR [args]  Evaluate EXPR and print the result")
  (print "  jolt -h | --help     Show this help"))

(defn main [&]
  (def args (or (dyn :args) @[]))            # @["jolt" arg1 arg2 ...]
  (def argv (if (> (length args) 1) (array/slice args 1) @[]))
  (ctx-set-current-ns ctx "user")
  (cond
    (empty? argv) (run-repl)
    (or (= (argv 0) "-h") (= (argv 0) "--help")) (print-help)
    (= (argv 0) "-e") (run-eval (get argv 1 "") (array/slice argv 2))
    (= (argv 0) "-") (run-file "/dev/stdin" (array/slice argv 1))
    (run-file (argv 0) (array/slice argv 1))))
