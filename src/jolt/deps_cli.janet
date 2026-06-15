# jolt-deps — a separate tool that resolves a deps.edn into Jolt source roots.
#
# Mirrors how jpm is a tool beside the janet runtime: the jolt runtime knows
# nothing about deps.edn — it just searches the roots in JOLT_PATH (see
# api/init). This tool does the resolution (git + :local deps, via jpm's fetch
# cache) and either prints the roots or launches jolt with JOLT_PATH set.
#
#   jolt-deps [-A:a:b] path        print the resolved roots (':'-joined), e.g.
#                                  JOLT_PATH=$(jolt-deps path) jolt file.clj
#   jolt-deps [-A:a:b] run FILE [args]
#                                  resolve, then `jolt FILE args` with JOLT_PATH set
#   jolt-deps [-A:a:b] repl        resolve, then a jolt REPL with JOLT_PATH set
#   jolt-deps [-A:a:b] -e EXPR     resolve, then `jolt -e EXPR ...`
#   jolt-deps -M:a[:b] [args]      resolve with the aliases, then run jolt with
#                                  the last alias's :main-opts ++ args
#   jolt-deps uberscript OUT -m NS resolve, then bundle NS + deps into one .clj
#   jolt-deps tasks                list :tasks (merged user+project deps.edn)
#   jolt-deps task NAME [args]     run a task: a string task is a shell command
#                                  (args appended); a {:main-opts [...]} task
#                                  runs jolt with those args ++ extra args
#
# -A:dev:test selects aliases (tools.deps style): their :extra-paths and
# :extra-deps join the resolution. A user-level deps.edn ($JOLT_CONFIG, else
# $XDG_CONFIG_HOME/jolt, else ~/.jolt) merges under the project's.
# The jolt binary is found via $JOLT_BIN, else `jolt` on PATH.

(import ./deps :as deps)

(defn- parse-alias-flag
  "\"-A:dev:test\" -> [:dev :test] (also accepts -M:...)."
  [arg]
  (map keyword (filter |(not= "" $) (string/split ":" (string/slice arg 2)))))

(defn- roots [aliases]
  (if (os/stat "deps.edn") (deps/resolve-deps-cached "deps.edn" nil aliases) @[]))

(defn- jolt-bin
  "The jolt executable: $JOLT_BIN, else the `jolt` sitting NEXT TO this
  jolt-deps binary (the pair is built together — running a checkout's
  build/jolt-deps by path must not pick up some other jolt, or fail when
  none is on PATH), else `jolt` from PATH."
  []
  (or (os/getenv "JOLT_BIN")
      (let [self (or (first (dyn :args)) (dyn :executable))
            slashes (when self (string/find-all "/" self))
            dir (when (and slashes (> (length slashes) 0))
                  (string/slice self 0 (last slashes)))
            sibling (when dir (string dir "/jolt"))]
        (when (and sibling (os/stat sibling)) sibling))
      "jolt"))

(defn- exec-jolt [aliases extra-args]
  # Set JOLT_PATH in our own env and let the child inherit it (os/execute's env
  # arg isn't honored here; inheriting is reliable).
  (def rs (string/join (roots aliases) ":"))
  (def existing (os/getenv "JOLT_PATH"))
  (os/setenv "JOLT_PATH" (if (and existing (> (length existing) 0)) (string rs ":" existing) rs))
  # The project's OWN source roots (jolt-87e): the runtime scopes whole-program
  # inference to namespaces under these, inferring deps per-ns at load instead of
  # re-inferring every transitive dep in one fixpoint. Absent => the runtime
  # treats all namespaces as app (whole-program over everything, as before).
  (when (os/stat "deps.edn")
    (os/setenv "JOLT_APP_PATHS" (string/join (deps/project-source-roots "deps.edn" aliases) ":")))
  (os/execute [(jolt-bin) ;extra-args] :p))

(defn- usage []
  (print "usage: jolt-deps [-A:alias[:alias]] [path | run FILE [args] | repl | -e EXPR [args]]")
  (print "       jolt-deps -M:alias[:alias] [args]   (runs the alias :main-opts)")
  (print "       jolt-deps uberscript OUT -m NS"))

(defn main [&]
  (var argv (tuple/slice (or (dyn :args) @[]) 1))
  (var aliases nil)
  # leading -A:... selects aliases for whatever command follows
  (while (string/has-prefix? "-A" (or (get argv 0) ""))
    (set aliases (array/concat (or aliases @[]) (parse-alias-flag (get argv 0))))
    (set argv (tuple/slice argv 1)))
  (def cmd (get argv 0))
  (cond
    (or (nil? cmd) (= cmd "help") (= cmd "-h") (= cmd "--help")) (usage)
    (string/has-prefix? "-M" cmd)
      (let [als (array/concat (or aliases @[]) (parse-alias-flag cmd))
            mo (or (deps/alias-main-opts "deps.edn" als)
                   (do (eprint "jolt-deps: no :main-opts in aliases " (string/format "%j" (map string als)))
                       (os/exit 1)))]
        (os/exit (exec-jolt als [;mo ;(tuple/slice argv 1)])))
    (= cmd "path") (print (string/join (roots aliases) ":"))
    (= cmd "tasks")
      (each row (deps/tasks "deps.edn")
        (print (row 0) (if (row 1) (string "\t" (row 1)) "")))
    (= cmd "task")
      (let [name (get argv 1)
            spec (when name (deps/task-spec "deps.edn" name))]
        (cond
          (nil? name) (do (eprint "jolt-deps: task needs a name") (os/exit 1))
          (nil? spec) (do (eprint "jolt-deps: no such task: " name) (os/exit 1))
          (= :shell (spec :type))
            (os/exit (os/execute ["sh" "-c" (string/join [(spec :cmd) ;(tuple/slice argv 2)] " ")] :p))
          (os/exit (exec-jolt aliases [;(spec :argv) ;(tuple/slice argv 2)]))))
    (= cmd "run")  (os/exit (exec-jolt aliases (tuple/slice argv 1)))
    (= cmd "repl") (os/exit (exec-jolt aliases []))
    (= cmd "-e")   (os/exit (exec-jolt aliases argv))
    (= cmd "uberscript") (os/exit (exec-jolt aliases argv))
    (do (eprint "jolt-deps: unknown command " cmd) (usage) (os/exit 1))))
