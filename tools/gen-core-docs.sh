#!/bin/sh
# Regenerate jolt-core/clojure/core/9N-docs.clj (reference :doc/:arglists for the
# image-baked namespaces, sharded 90-docs.clj, 91-docs.clj, ...) from a JVM Clojure's own var metadata.
#
#   tools/gen-core-docs.sh [path-to-jolt]
#
# Needs a jolt binary (defaults to ./bin/jolt) for the var inventory and a JVM
# clojure on ~/.m2 (override the jars via CLOJURE_JAR / SPEC_JAR / CORE_SPECS_JAR).
# Run after adding/removing core or image-stdlib vars, then make remint.
set -e
cd "$(dirname "$0")/.."

JOLT="${1:-./bin/jolt}"
M2="$HOME/.m2/repository/org/clojure"
CLOJURE_JAR="${CLOJURE_JAR:-$(ls "$M2"/clojure/*/clojure-[0-9]*.jar 2>/dev/null | grep -v -- '-alpha\|-beta\|-rc' | sort -V | tail -1)}"
SPEC_JAR="${SPEC_JAR:-$(ls "$M2"/spec.alpha/*/spec.alpha-*.jar | sort -V | tail -1)}"
CORE_SPECS_JAR="${CORE_SPECS_JAR:-$(ls "$M2"/core.specs.alpha/*/core.specs.alpha-*.jar | sort -V | tail -1)}"

INV="$(mktemp)"
trap 'rm -f "$INV"' EXIT

"$JOLT" -e '(prn (into {} (map (fn [n] [n (vec (sort (map name (keys (ns-interns (symbol n))))))]) ["clojure.core" "clojure.string" "clojure.walk" "clojure.template" "clojure.edn" "clojure.set" "clojure.pprint" "clojure.repl" "clojure.java.io"])))' > "$INV"

java -cp "$CLOJURE_JAR:$SPEC_JAR:$CORE_SPECS_JAR" clojure.main \
  tools/gen-core-docs.clj "$INV" jolt-core/clojure/core
