(declare-project
  :name "jolt"
  :description "Clojure interpreter on Janet")

(declare-source
  :source @["src"])

(declare-executable
  :name "jolt"
  :entry "src/jolt/main.janet")

# Deprecated shim kept for back-compat: deps.edn resolution is built into `jolt`
# now (the CLI front-end resolves into JOLT_PATH in-process; the runtime core
# stays deps-agnostic). Forwards to `jolt`; prefer `jolt -M:…` / `jolt path`.
(declare-executable
  :name "jolt-deps"
  :entry "src/jolt/deps_cli.janet")
