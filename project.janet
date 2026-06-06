(declare-project
  :name "jolt"
  :description "Clojure interpreter on Janet")

(declare-source
  :source @["src"])

(declare-executable
  :name "jolt"
  :entry "src/jolt/main.janet")

# Separate tool (like jpm beside janet): resolves deps.edn into Jolt source
# roots. The jolt runtime stays deps-agnostic — it just reads JOLT_PATH.
(declare-executable
  :name "jolt-deps"
  :entry "src/jolt/deps_cli.janet")
