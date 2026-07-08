# Clojure libraries known to work with Jolt

Libraries confirmed to load and pass their conformance checks on Jolt. A library
listed here works. See the [examples](https://github.com/jolt-lang/examples),
e.g. the [ring-app example](https://github.com/jolt-lang/examples/tree/main/ring-app).

* [aero](https://github.com/juxt/aero) — EDN configuration with tag literals
  (`#ref`/`#env`/`#or`/`#profile`/`#long`/…)
* [config](https://github.com/yogthos/config) — environment configuration
* [Selmer](https://github.com/yogthos/Selmer) — Django-style templates
* [medley](https://github.com/weavejester/medley) — collection utilities
* [cuerdas](https://github.com/funcool/cuerdas) — string manipulation
* [ring-core](https://github.com/ring-clojure/ring) — via `:deps/root "ring-core"`,
  on the ring-app example
* [ring-codec](https://github.com/ring-clojure/ring-codec) — URL/form encoding
* [ring-defaults](https://github.com/ring-clojure/ring-defaults) — the standard
  middleware stack (params, static resources + content-type, session, security
  headers); its session/CSRF crypto comes from
  [jolt-lang/jolt-crypto](https://github.com/jolt-lang/jolt-crypto) (OpenSSL)
* [reitit-core](https://github.com/metosin/reitit) — data-driven routing; the
  `reitit.Trie` Java class is mirrored by
  [jolt-lang/router](https://github.com/jolt-lang/router).
* [integrant](https://github.com/weavejester/integrant) — data-driven system
  configuration (`#ig/ref`), with its
  [dependency](https://github.com/weavejester/dependency) and
  [meta-merge](https://github.com/weavejester/meta-merge) deps
* [honeysql](https://github.com/seancorfield/honeysql) — SQL formatter and helpers
* [clojure.jdbc](https://github.com/yogthos/clojure.jdbc) — via
  [jolt-lang/db](https://github.com/jolt-lang/db)'s `jdbc.core`, over the built-in
  SQLite access (libsqlite3 via Chez's FFI)
* [tools.logging](https://github.com/clojure/tools.logging) — runs verbatim over a
  native `clojure.tools.logging.impl` stderr backend
* [migratus](https://github.com/yogthos/migratus) — database migrations over
  [jolt-lang/db](https://github.com/jolt-lang/db)
* [malli](https://github.com/metosin/malli) — data schema validation, on the
  malli-app example.
* [markdown-clj](https://github.com/yogthos/markdown-clj) — Markdown → HTML, on the
  markdown-app example
* [hiccup](https://github.com/weavejester/hiccup) — HTML from Clojure data, on the
  hiccup-app example
* [clojure.data.json](https://github.com/clojure/data.json) — JSON reading and writing
* [clojure.spec.alpha](https://github.com/clojure/spec.alpha) — data specs
* [core.match](https://github.com/clojure/core.match) — pattern matching.
* [core.cache](https://github.com/clojure/core.cache) — caching (Basic/FIFO/LRU/
  LU/TTL/Soft + the wrapped atom API), over
  [data.priority-map](https://github.com/clojure/data.priority-map).
* [core.memoize](https://github.com/clojure/core.memoize) — function memoization
  over [core.cache](https://github.com/clojure/core.cache).
* [core.async](https://github.com/clojure/core.async) — CSP channels and `go` blocks
  (`<!`/`>!`/`alts!`, `pipeline`, `mult`/`mix`/`pub`/`sub`) on real OS threads.
* [core.logic](https://github.com/clojure/core.logic) — relational logic programming
  (unification, `run`/`fresh`/`conde`, finite domains).
* [math.combinatorics](https://github.com/clojure/math.combinatorics) — permutations,
  combinations, subsets, selections, cartesian products, partitions.
* [core.contracts](https://github.com/clojure/core.contracts) — programming by
  contract (`contract`/`with-constraints`/`provide`), over
  [core.unify](https://github.com/clojure/core.unify).
* [data.zip](https://github.com/clojure/data.zip) — zipper navigation, including
  `clojure.data.zip.xml`; XML parsing via [jolt-lang/xml](https://github.com/jolt-lang/xml)
  (which now ships `clojure.xml/parse`).
* [data.csv](https://github.com/clojure/data.csv) — reading and writing CSV.
* [data.codec](https://github.com/clojure/data.codec) — base64 encode/decode over
  byte arrays.
* [data.priority-map](https://github.com/clojure/data.priority-map) — priority
  maps (incl. keyfn / custom comparator), with `subseq`/`rsubseq`.
* [tools.macro](https://github.com/clojure/tools.macro) — local macros
  (`macrolet`/`symbol-macrolet`), `mexpand`/`mexpand-all`.
* [algo.monads](https://github.com/clojure/algo.monads) — monad macros and
  monads (maybe/seq/state/writer/reader/…), over
  [tools.macro](https://github.com/clojure/tools.macro).
* [test.check](https://github.com/clojure/test.check) — property-based testing
  (generators, `quick-check`, shrinking).
* [tools.reader](https://github.com/clojure/tools.reader) — a Clojure reader in
  Clojure (edn + full reader, indexing/pushback reader types).
* [rewrite-clj](https://github.com/clj-commons/rewrite-clj) — parse/rewrite Clojure
  source while preserving whitespace and comments (nodes + zipper), over
  [tools.reader](https://github.com/clojure/tools.reader).
* [tick](https://github.com/juxt/tick) — date/time over Jolt's `java.time`;
  `#time/…` literals via `time-literals`.
* [transit-jolt](https://github.com/jolt-lang/transit-jolt) — Transit (JSON) read/write
* [yamlstar](https://github.com/yaml/yamlstar) — YAML load/dump (pure-Clojure
  parser, JSON-safe integer policy).
