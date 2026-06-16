# jolt-deps — deprecated shim. Dependency resolution is now built into `jolt`
# itself (the runtime stays deps-agnostic; the CLI front-end resolves deps.edn
# into JOLT_PATH in-process). This binary forwards everything to `jolt` so
# existing scripts keep working — prefer calling `jolt` directly:
#
#   jolt-deps -M:nrepl     ->  jolt -M:nrepl
#   jolt-deps path         ->  jolt path
#   jolt-deps run FILE     ->  jolt run FILE
#
# The jolt binary is found via $JOLT_BIN, else the `jolt` sitting next to this
# shim (built as a pair), else `jolt` on PATH.

(defn- jolt-bin []
  (or (os/getenv "JOLT_BIN")
      (let [self (or (first (dyn :args)) (dyn :executable))
            slashes (when self (string/find-all "/" self))
            dir (when (and slashes (> (length slashes) 0))
                  (string/slice self 0 (last slashes)))
            sibling (when dir (string dir "/jolt"))]
        (when (and sibling (os/stat sibling)) sibling))
      "jolt"))

(defn main [&]
  (def argv (tuple/slice (or (dyn :args) @[]) 1))
  (eprint "jolt-deps is deprecated — dependency resolution is built into `jolt` now "
          "(e.g. `jolt -M:nrepl`, `jolt path`, `jolt run FILE`).")
  (os/exit (os/execute [(jolt-bin) ;argv] :p)))
