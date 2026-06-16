# jolt-deps folded into jolt: the single `jolt` binary resolves a deps.edn into
# JOLT_PATH in-process and dispatches the deps subcommands (path/-M/run/tasks/
# task), and auto-resolves a deps.edn for runnable commands (repl/-m/-e/FILE).
# Drives the BUILT binary (baked overlay -> cwd-independent) from a fixture
# project dir so deps.edn in cwd is picked up. :local/root deps only — no
# network. Skips cleanly if build/jolt is absent (source-only run).

(def repo-root (os/cwd))
(def jolt (string repo-root "/build/jolt"))

(if-not (os/stat jolt)
  (do (print "deps-merged-cli: SKIP (no build/jolt — run from source)") (os/exit 0)))

(def base (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-merged-cli-" (os/time)))
(defn rmrf [p]
  (when (os/stat p)
    (if (= :directory (os/stat p :mode))
      (do (each e (os/dir p) (rmrf (string p "/" e))) (os/rmdir p))
      (os/rm p))))
(rmrf base)
(defn mkdirs [p]
  (var acc nil)
  (each seg (filter |(not= "" $) (string/split "/" p))
    (set acc (if (nil? acc) (string "/" seg) (string acc "/" seg)))
    (unless (os/stat acc) (os/mkdir acc))))

# project depends on a local lib; an alias with :main-opts; an :extra-paths
# alias; a :tasks map (shell task)
(each d ["proj/src/app" "lib/src/mylib"] (mkdirs (string base "/" d)))
(spit (string base "/proj/deps.edn")
      `{:paths ["src"]
        :deps {my/lib {:local/root "../lib"}}
        :aliases {:run {:main-opts ["-m" "app.core"]}
                  :dev {:extra-paths ["dev"]}}
        :tasks {greet "echo hello-task"}}`)
(spit (string base "/lib/deps.edn") `{:paths ["src"]}`)
(spit (string base "/lib/src/mylib/core.clj") "(ns mylib.core)\n(defn answer [] 42)\n")
(spit (string base "/proj/src/app/core.clj")
      "(ns app.core (:require [mylib.core :as m]))\n(defn -main [& args] (println \"MAIN\" (m/answer) (count args)))\n")
(spit (string base "/proj/script.clj")
      "(require '[mylib.core :as m])\n(println \"SCRIPT\" (m/answer))\n")

# Run the built jolt from a given dir (cwd matters: deps.edn is read from cwd).
# Direct spawn (no shell) so arbitrary -e exprs need no quoting. Returns out+err.
(defn- run-in [dir & args]
  (def prev (os/cwd))
  (os/cd dir)
  (def p (os/spawn [jolt ;args] :p {:out :pipe :err :pipe}))
  (def out (:read (p :out) :all))
  (def err (:read (p :err) :all))
  (os/proc-wait p)
  (os/cd prev)
  (string (or out "") (or err "")))

(var fails 0)
(defn check [label got pred]
  (if (pred got) (print "  ok   " label)
    (do (++ fails) (printf "  FAIL %s: got %q" label got))))
(defn- has [sub] (fn [s] (truthy? (string/find sub s))))

(def proj (string base "/proj"))

# `path` prints the resolved roots: project's own src + the local dep's src
(check "path includes project src"  (run-in proj "path") (has "/proj/src"))
(check "path includes local dep src" (run-in proj "path") (has "/lib/src"))

# `-M:run` runs the alias :main-opts (-m app.core), requiring the local dep
(check "-M:run runs -main through resolved deps" (run-in proj "-M:run" "x" "y") (has "MAIN 42 2"))

# `run FILE` resolves deps then runs the file
(check "run FILE resolves deps" (run-in proj "run" "script.clj") (has "SCRIPT 42"))

# `tasks` lists the :tasks entries; `task NAME` runs a shell task
(check "tasks lists greet" (run-in proj "tasks") (has "greet"))
(check "task runs shell command" (run-in proj "task" "greet") (has "hello-task"))

# auto-resolve: a plain -e in a deps.edn dir can require the local dep
(check "-e auto-resolves deps.edn in cwd"
       (run-in proj "-e" "(require '[mylib.core :as m]) (println (m/answer))")
       (has "42"))

# -A:alias forces resolution (with that alias) for a runnable command
(check "-A:dev with -e resolves + runs"
       (run-in proj "-A:dev" "-e" "(println (+ 1 2))")
       (has "3"))

# no deps.edn: plain run is unaffected (resolver/jpm never touched)
(mkdirs (string base "/plain"))
(check "-e in a no-deps dir works unchanged"
       (run-in (string base "/plain") "-e" "(println (* 6 7))")
       (has "42"))
# help/version never trigger resolution
(check "version works in a deps dir" (run-in proj "--version") (has "jolt v"))

(rmrf base)
(if (> fails 0)
  (do (printf "deps-merged-cli: %d FAILED" fails) (os/exit 1))
  (print "deps-merged-cli: all cases passed"))
