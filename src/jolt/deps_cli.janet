# jolt-deps — a separate tool that resolves a deps.edn into Jolt source roots.
#
# Mirrors how jpm is a tool beside the janet runtime: the jolt runtime knows
# nothing about deps.edn — it just searches the roots in JOLT_PATH (see
# api/init). This tool does the resolution (git + :local deps, via jpm's fetch
# cache) and either prints the roots or launches jolt with JOLT_PATH set.
#
#   jolt-deps path            print the resolved roots (':'-joined), e.g. for
#                             JOLT_PATH=$(jolt-deps path) jolt file.clj
#   jolt-deps run FILE [args] resolve, then run `jolt FILE args` with JOLT_PATH set
#   jolt-deps repl            resolve, then start a jolt REPL with JOLT_PATH set
#   jolt-deps -e EXPR [args]  resolve, then `jolt -e EXPR ...` with JOLT_PATH set
#
# The jolt binary is found via $JOLT_BIN, else `jolt` on PATH.

(import ./deps :as deps)

(defn- roots []
  (if (os/stat "deps.edn") (deps/resolve-deps-cached "deps.edn") @[]))

(defn- exec-jolt [extra-args]
  # Set JOLT_PATH in our own env and let the child inherit it (os/execute's env
  # arg isn't honored here; inheriting is reliable).
  (def rs (string/join (roots) ":"))
  (def existing (os/getenv "JOLT_PATH"))
  (os/setenv "JOLT_PATH" (if (and existing (> (length existing) 0)) (string rs ":" existing) rs))
  (os/execute [(os/getenv "JOLT_BIN" "jolt") ;extra-args] :p))

(defn- usage []
  (print "usage: jolt-deps [path | run FILE [args] | repl | -e EXPR [args]]"))

(defn main [&]
  (def argv (tuple/slice (or (dyn :args) @[]) 1))
  (def cmd (get argv 0))
  (cond
    (or (nil? cmd) (= cmd "help") (= cmd "-h") (= cmd "--help")) (usage)
    (= cmd "path") (print (string/join (roots) ":"))
    (= cmd "run")  (os/exit (exec-jolt (tuple/slice argv 1)))
    (= cmd "repl") (os/exit (exec-jolt []))
    (= cmd "-e")   (os/exit (exec-jolt argv))
    (do (eprint "jolt-deps: unknown command " cmd) (usage) (os/exit 1))))
