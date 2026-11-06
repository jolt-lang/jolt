(ns jolt.profile
  "Execution-count profiling, over Chez Scheme's compile-profile counters.

  Code compiled while instrumentation is on carries a counter on every
  expression, and the counters are read back here by Clojure file, line and
  function. The counts are exact, not sampled: a line that ran 1973 times says
  1973, as long as only one thread runs it.

  Instrumentation is a compile-time switch. It applies to code compiled AFTER it
  is turned on, so turn it on and then load (or reload) what you want to
  measure:

    (require '[jolt.profile :as prof])
    (prof/instrument!)
    (require 'my.app :reload)
    (prof/instrument! false)   ; optional: stop instrumenting new code
    (prof/clear!)
    (my.app/run)
    (prof/report)

  Or for a whole run, set JOLT_PROFILE=1 and the report prints on stderr as the
  process exits:

    JOLT_PROFILE=1 jolt run -m my.app

  The same switch has a second mode, (prof/instrument! :debug) or
  JOLT_PROFILE=debug, that compiles code for jolt.debug instead of counting it:
  each fn's params and locals stay visible to its frame. See jolt.debug.

  Only code jolt compiles at runtime is instrumented: your namespaces and the
  libraries they load from source. The runtime and the stdlib that ships
  compiled in the jolt binary are not, so a call into clojure.core counts as
  one evaluation of the calling expression. While instrumentation is on,
  namespaces load from source rather than from the AOT cache, and the code it
  compiles is slower than uninstrumented code. Counters are not atomic: code
  running on several threads at once loses increments, often most of them.

  A method of a defrecord, deftype, extend-type or extend-protocol is named
  ns/Type.method, and a defmethod's body ns/multi[dispatch-value].

  Two numbers describe a line or a fn:

    :hits   how many times it ran. For a fn this is its most-run line, which is
            its call count unless it loops.
    :evals  how many expressions it evaluated in all, which is the closest
            thing to how much work it did.

  `measure` is separate from the counters: it reports the time, allocation and
  collector work of evaluating its body.

  The sampler measures time instead of counts, and needs no recompilation, so
  it sees all code: the runtime, clojure.core, AOT-cached namespaces and built
  binaries. Every thread running jolt code is interrupted at an interval of its
  own CPU time and its stack is recorded:

    (prof/sample! {:interval-ms 1})
    (my.app/run)
    (prof/stop-sampling!)
    (prof/sample-report)
    (prof/write-folded \"app.folded\")   ; for flame graph tools

  or for a whole run, JOLT_PROFILE=sample. A thread started while sampling is on
  is sampled from its start, and one already running joins within a few milliseconds
  of CPU time. Frames erased by tail calls are not on the stack, and a
  thread blocked in a wait uses no CPU and is not sampled."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [jolt.host :as host]))

(defn instrument!
  "Turn on instrumentation for code compiled from now on, or with false turn it
  off. mode is true (or :counts) for the execution counters, or :debug for
  debug locals: code compiled under :debug keeps every fn's params and locals
  visible to its frame, for jolt.debug/frames to read. One mode at a time; code
  already compiled keeps what it was compiled with either way. Returns true;
  throws on any other mode, or when the host cannot instrument."
  ([] (instrument! true))
  ([mode]
   (when-not (contains? #{true false nil :counts :debug} mode)
     (throw (ex-info (str "jolt.profile: unknown instrument! mode " (pr-str mode)
                          "; expected true, :counts, :debug or false")
                     {:mode mode})))
   (let [debug? (= mode :debug)]
     (if (host/profile-instrument! (if debug? :debug (boolean mode)))
       true
       (throw (ex-info (if debug?
                         "jolt.profile: this host cannot keep debug locals"
                         "jolt.profile: this host has no compile-profile counters")
                       {:host :unsupported}))))))

(defn instrumenting?
  "True while code being compiled is instrumented, in either mode."
  []
  (boolean (host/profile-instrumenting?)))

(defn instrument-mode
  "What code compiled now carries: :counts, :debug, or nil."
  []
  (host/profile-instrument-mode))

(defn clear!
  "Reset every counter to zero."
  []
  (host/profile-clear!)
  nil)

(defn- same-file? [a b]
  (and (string? a) (string? b)
       (or (= a b) (str/ends-with? a (str "/" b)) (str/ends-with? b (str "/" a)))))

;; [file line] -> {:hits :evals :fn-hits :fn-evals :owner :form-line :label
;; :wins?}. A counter is one expression of one compiled copy of a line: within
;; a copy the line ran as often as its most-run expression, and copies of the
;; same line in different forms (an inlined call, a redefinition) ran
;; separately, so they add up. The :fn- pair leaves out top-level code — what a
;; form does as it loads, a defn's own included — which ran on that line but
;; belongs to no fn. RETIRED are the lines of code that is gone, merged the
;; same way already.
(defn- line-table [sites retired]
  (let [merge-copy (fn [r n top?]
                     (cond-> (-> r
                                 (update :max (fnil max 0) n)
                                 (update :sum (fnil + 0) n))
                       (not top?) (-> (update :fn-max (fnil max 0) n)
                                      (update :fn-sum (fnil + 0) n))))
        per-copy (reduce (fn [acc [file line n owner source form-line top? label wins?]]
                           (update acc [source file line]
                                   (fn [r]
                                     (-> (merge-copy r n top?)
                                         (assoc :owner (or owner (:owner r))
                                                :form-line form-line
                                                :label (or label (:label r))
                                                :wins? wins?)))))
                         {} sites)
        add (fnil + 0)
        merge-line (fn [acc k r]
                     (update acc k
                             (fn [t]
                               {:hits (add (:hits t) (:hits r))
                                :evals (add (:evals t) (:evals r))
                                :fn-hits (add (:fn-hits t) (:fn-hits r))
                                :fn-evals (add (:fn-evals t) (:fn-evals r))
                                :owner (or (:owner r) (:owner t))
                                :form-line (or (:form-line r) (:form-line t))
                                :label (or (:label r) (:label t))
                                :wins? (or (:wins? r) (:wins? t))})))]
    (reduce (fn [acc [file line hits evals fn-hits fn-evals owner form-line label wins?]]
              (merge-line acc [file line]
                          {:hits hits :evals evals :fn-hits fn-hits :fn-evals fn-evals
                           :owner owner :form-line form-line :label label :wins? wins?}))
            (reduce-kv (fn [acc [_ file line] r]
                         (merge-line acc [file line]
                                     (assoc r :hits (:max r) :evals (:sum r)
                                            :fn-hits (:fn-max r 0) :fn-evals (:fn-sum r 0))))
                       {} per-copy)
            retired)))

;; file -> defs sorted by line, each [fqn line]
(defn- defs-by-file [files]
  (->> (host/profile-defs (vec files))
       (group-by second)
       (reduce-kv (fn [m file ds] (assoc m file (sort-by second (map (fn [[q _ l]] [q l]) ds))))
                  {})))

;; The def a line belongs to: the last one that starts at or after the top-level
;; form the line came from and at or before the line. None means top-level code.
(defn- owning-def [defs form-line line]
  (when (and form-line line)
    (last (filter (fn [[_ l]] (<= form-line l line)) defs))))

;; A line's fn: the fn an inlined copy came from, else the method its form
;; names (a record's, a defmethod's), else the def it falls in, else the def its
;; form names, which is how code from -e or the REPL, with no file to find its
;; defs by, gets one.
(defn- rows []
  (let [table (line-table (host/profile-sites) (host/profile-retired))
        defs (defs-by-file (distinct (keep first (keys table))))]
    (for [[[file line] {:keys [hits evals fn-hits fn-evals owner form-line label wins?]}] table
          :when (pos? evals)]
      (let [d (when (and (pos? fn-evals) (not owner) (not wins?))
                (owning-def (get defs file) form-line line))
            f (when (pos? fn-evals) (or owner (when wins? label) (first d) label))]
        {:file file :line line :hits hits :evals evals
         :fn (some-> f symbol)
         :fn-hits fn-hits :fn-evals fn-evals
         :def-line (second d)}))))

(defn line-counts
  "Every instrumented line that ran, most-run first, as maps of :file, :line,
  :hits (times the line ran), :evals (expressions it evaluated) and :fn (the
  symbol of the fn it is in, or nil for top-level code)."
  []
  (->> (rows)
       (map #(dissoc % :def-line :fn-hits :fn-evals))
       (sort-by (juxt (comp - :hits) :file :line))))

(defn fn-counts
  "Every instrumented fn that ran, most work first, as maps of :fn, :file,
  :line (where it is defined), :hits (its most-run line: the call count unless
  it loops) and :evals (expressions evaluated in its body)."
  []
  (->> (rows)
       (filter :fn)
       (group-by :fn)
       (map (fn [[f rs]]
              {:fn f
               :file (:file (first rs))
               :line (or (some :def-line rs) (apply min (keep :line rs)))
               :hits (apply max (map :fn-hits rs))
               :evals (reduce + (map :fn-evals rs))}))
       (sort-by (juxt (comp - :evals) (comp str :fn)))))

(defn- source-lines [file]
  (try
    (let [f (io/file file)
          src (cond (.exists f) f
                    :else (io/resource file))]
      (when src (vec (str/split-lines (slurp src)))))
    (catch Exception _ nil)))

(defn- pad-left [s n]
  (let [s (str s)] (str (apply str (repeat (- n (count s)) \space)) s)))

(defn- pad-right [s n]
  (let [s (str s)] (str s (apply str (repeat (- n (count s)) \space)))))

(defn- location [{:keys [file line]}]
  (str (or file "NO_SOURCE_FILE") (when line (str ":" line))))

(defn report
  "Print the most-run fns and lines. Options:

    :limit  how many rows of each table (default 20)"
  ([] (report {}))
  ([{:keys [limit] :or {limit 20}}]
   (let [fns (take limit (fn-counts))
         lines (take limit (line-counts))
         sources (memoize source-lines)]
     (if (empty? lines)
       (println "jolt.profile: no instrumented code has run")
       (do
         (println "jolt.profile: fns by work")
         (println (str (pad-left "evals" 12) (pad-left "hits" 12) "  fn"))
         (doseq [{:keys [evals hits] :as r} fns]
           (println (str (pad-left evals 12) (pad-left hits 12) "  "
                         (pad-right (:fn r) 32) " " (location r))))
         (println)
         (println "jolt.profile: lines by hits")
         (println (str (pad-left "hits" 12) "  line"))
         (doseq [{:keys [file line hits] :as r} lines]
           (let [text (when (and file line) (get (sources file) (dec line)))]
             (println (str (pad-left hits 12) "  " (pad-right (location r) 40)
                           (when text (str "  " (str/trim text))))))))))))

(defn annotate
  "Print the source of file with each line's hit count in the margin. file is
  the path the code was loaded from, or its classpath-relative name."
  [file]
  (let [hits (into {} (for [r (line-counts) :when (same-file? (:file r) file)]
                        [(:line r) (:hits r)]))
        lines (or (source-lines file)
                  (throw (ex-info (str "jolt.profile: cannot read " file) {:file file})))
        width (max 6 (count (str (apply max 0 (vals hits)))))]
    (doseq [[i text] (map-indexed vector lines)]
      (println (str (pad-left (get hits (inc i) "") width) " | " text)))))

(defn report-at-exit
  "The JOLT_PROFILE exit hook: the report, on stderr."
  []
  (host/profile-instrument! false)
  (binding [*out* *err*]
    (report)
    (flush)))

(defn stats*
  "One reading of the process's time, allocation and collector counters, as a
  map of nanos and bytes. `measure` takes one on each side of its body."
  []
  (let [[cpu real gcs gc-cpu _gc-real _gc-bytes allocated] (host/profile-stats)]
    {:cpu-nanos cpu :real-nanos real :gc-count gcs :gc-cpu-nanos gc-cpu
     :allocated-bytes allocated}))

(defn- ms [nanos] (/ (double nanos) 1e6))

(defn measure*
  "Call f with no arguments and return a map of its :result with the :real-ms
  and :cpu-ms it took, the :allocated-bytes it allocated, and the :gc-count
  collections and :gc-ms of collector cpu time that ran meanwhile."
  [f]
  (let [before (stats*)
        result (f)
        after (stats*)
        d #(- (% after) (% before))]
    {:result result
     :real-ms (ms (d :real-nanos))
     :cpu-ms (ms (d :cpu-nanos))
     :allocated-bytes (d :allocated-bytes)
     :gc-count (d :gc-count)
     :gc-ms (ms (d :gc-cpu-nanos))}))

(defmacro measure
  "Evaluate body and return a map of its :result with the :real-ms and :cpu-ms
  it took, the :allocated-bytes it allocated, and the :gc-count collections and
  :gc-ms of collector cpu time that ran meanwhile."
  [& body]
  `(measure* (fn [] ~@body)))

;; --- sampling -------------------------------------------------------------------

(defn sample!
  "Start sampling every thread that runs jolt code, dropping the samples of any
  earlier run. Options:

    :interval-ms  thread CPU time between samples (default 1)
    :depth        frames kept per stack, innermost first (default 64)

  Returns true; throws when the host cannot sample."
  ([] (sample! {}))
  ([{:keys [interval-ms depth] :or {interval-ms 1 depth 64}}]
   (if (host/sample-start! (long (* interval-ms 1000000)) (long depth))
     true
     (throw (ex-info "jolt.profile: this host cannot sample stacks"
                     {:host :unsupported})))))

(defn stop-sampling!
  "Stop sampling. The samples stay readable until the next sample!."
  []
  (host/sample-stop!)
  nil)

(defn sampling?
  "True while the sampler runs."
  []
  (boolean (host/sampling?)))

(defn- sample-frame [[name file line def-line]]
  {:fn (symbol name) :file file :line line :def-line def-line})

(def ^:private runtime-frame {:fn '[runtime] :file nil :line nil :def-line nil})

(defn samples
  "Every distinct stack the sampler recorded, as maps of :thread (its id),
  :count (samples of that stack), :ms (thread CPU time they stand for) and
  :stack, root first, a vector of {:fn :file :line}, where :line is the line the
  frame was at. A stack whose every frame is runtime plumbing reads as [runtime]."
  []
  (let [[_ rows] (host/sample-read)]
    (for [[thread n nanos frames] rows]
      {:thread thread :count n :ms (ms nanos)
       :stack (if (seq frames) (mapv sample-frame frames) [runtime-frame])})))

(defn- by-fn [acc f ms n]
  (update acc (:fn f)
          (fn [r]
            (-> (or r {:fn (:fn f) :file (:file f) :line (:def-line f)
                       :self-ms 0.0 :total-ms 0.0 :samples 0})
                (update :total-ms + ms)
                (update :samples + n)))))

(defn sample-counts
  "Time per fn, most self time first, as maps of :fn, :file, :line (where it is
  defined), :self-ms (in the fn itself), :total-ms (in it and what it called)
  and :samples (samples it was on the stack for). A recursive fn counts once per
  stack."
  []
  (->> (samples)
       (reduce (fn [acc {:keys [stack ms count]}]
                 (let [fns (vals (into {} (map (juxt :fn identity)) stack))
                       acc (reduce #(by-fn %1 %2 ms count) acc fns)]
                   (update-in acc [(:fn (peek stack)) :self-ms] + ms)))
               {})
       vals
       (sort-by (juxt (comp - :self-ms) (comp - :total-ms) (comp str :fn)))))

(defn sample-lines
  "Self time per line, most first, as maps of :file, :line, :fn, :self-ms and
  :samples. A sample is charged to its innermost frame that has a source line."
  []
  (->> (samples)
       (reduce (fn [acc {:keys [stack ms count]}]
                 (if-let [f (last (filter :line stack))]
                   (update acc [(:file f) (:line f)]
                           (fn [r]
                             (-> (or r {:file (:file f) :line (:line f) :fn (:fn f)
                                        :self-ms 0.0 :samples 0})
                                 (update :self-ms + ms)
                                 (update :samples + count))))
                   acc))
               {})
       vals
       (sort-by (juxt (comp - :self-ms) (comp str :file) :line))))

(defn write-folded
  "Write the stacks in the collapsed format flame graph tools read (flamegraph.pl,
  speedscope, inferno): one line per stack, root first and separated by ;, then
  its weight in microseconds of CPU time. dest is a path or a writer."
  [dest]
  (let [stacks (reduce (fn [acc {:keys [stack ms]}]
                         (update acc (str/join ";" (map :fn stack)) (fnil + 0.0) ms))
                       {} (samples))
        write (fn [w]
                (doseq [[k v] (sort-by key stacks)]
                  (.write w (str k " " (Math/round (* 1000.0 (double v))) "\n")))
                (.flush w))]
    (if (string? dest)
      (with-open [w (io/writer dest)] (write w))
      (write dest))
    nil))

(defn- fmt-ms [x] (format "%.1f" (double x)))

(defn sample-report
  "Print the fns by self time, the fns by total time and the lines by self time.
  Options:

    :limit  how many rows of each table (default 20)"
  ([] (sample-report {}))
  ([{:keys [limit] :or {limit 20}}]
   (let [[gc rows] (host/sample-read)
         counts (sample-counts)
         total (reduce + 0.0 (map :self-ms counts))
         n (reduce + 0 (map second rows))
         sources (memoize source-lines)
         pct (fn [x] (str (format "%.1f" (if (pos? total) (* 100.0 (/ x total)) 0.0)) "%"))]
     (if (zero? n)
       (println "jolt.profile: no samples")
       (do
         (println (str "jolt.profile: " n " samples, " (fmt-ms total) " ms of CPU time, "
                       (fmt-ms (ms gc)) " ms in the collector"))
         (println)
         (println "fns by self time")
         (println (str (pad-left "self ms" 12) (pad-left "self" 8) (pad-left "total ms" 12) "  fn"))
         (doseq [r (take limit counts)]
           (println (str (pad-left (fmt-ms (:self-ms r)) 12) (pad-left (pct (:self-ms r)) 8)
                         (pad-left (fmt-ms (:total-ms r)) 12) "  " (pad-right (:fn r) 32)
                         (when (:file r) (str " " (location r))))))
         (println)
         (println "fns by total time")
         (println (str (pad-left "total ms" 12) (pad-left "total" 8) "  fn"))
         (doseq [r (take limit (sort-by (comp - :total-ms) counts))]
           (println (str (pad-left (fmt-ms (:total-ms r)) 12) (pad-left (pct (:total-ms r)) 8)
                         "  " (pad-right (:fn r) 32)
                         (when (:file r) (str " " (location r))))))
         (println)
         (println "lines by self time")
         (println (str (pad-left "self ms" 12) "  line"))
         (doseq [{:keys [file line] :as r} (take limit (sample-lines))]
           (let [text (when (and file line) (get (sources file) (dec line)))]
             (println (str (pad-left (fmt-ms (:self-ms r)) 12) "  " (pad-right (location r) 40)
                           (when text (str "  " (str/trim text))))))))))))

(defmacro with-sampling
  "Sample while body runs, and return a map of its :result and the
  sample-counts rows of what it did as :counts. opts as for sample!."
  [opts & body]
  `(do (sample! ~opts)
       (let [r# (try (do ~@body) (finally (stop-sampling!)))]
         {:result r# :counts (sample-counts)})))

(defn sample-report-at-exit
  "The JOLT_PROFILE=sample exit hook: the sample report, on stderr."
  []
  (stop-sampling!)
  (binding [*out* *err*]
    (sample-report)
    (flush)))
