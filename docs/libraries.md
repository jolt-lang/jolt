# Clojure libraries known to work with Jolt

Libraries confirmed to load and pass their conformance checks on Jolt
(see the [examples](https://github.com/jolt-lang/examples), e.g. the
[ring-app example](https://github.com/jolt-lang/examples/tree/main/ring-app)).

* [aero](https://github.com/juxt/aero) — EDN configuration with tag literals;
  `read-config` resolves `#ref`/`#env`/`#or`/`#profile`/`#long`/… and map/vector/set
  configs round-trip
* [config](https://github.com/yogthos/config)
* [Selmer](https://github.com/yogthos/Selmer)
* [medley](https://github.com/weavejester/medley)
* [cuerdas](https://github.com/funcool/cuerdas)
* [ring-core](https://github.com/ring-clojure/ring) — via `:deps/root "ring-core"`,
  on the [ring-app example](https://github.com/jolt-lang/examples/tree/main/ring-app)'s
  spork/http adapter
* [ring-codec](https://github.com/ring-clojure/ring-codec)
* [reitit-core](https://github.com/metosin/reitit) — data-driven routing; the
  reitit.Trie Java class is mirrored in Clojure by
  [jolt-lang/router](https://github.com/jolt-lang/router). Load with
  `JOLT_FEATURES` including `clj`.
* [integrant](https://github.com/weavejester/integrant) — data-driven system
  configuration; `ig/init`/`ig/halt!` build and tear down a component graph wired
  with `#ig/ref`, on the ring-app example. Loads unmodified with its
  [dependency](https://github.com/weavejester/dependency) and
  [meta-merge](https://github.com/weavejester/meta-merge) deps.
* [honeysql](https://github.com/seancorfield/honeysql) — full formatter + helpers
  (select/insert/update/delete/joins/:inline), loaded unmodified from git
* [clojure.jdbc](https://github.com/yogthos/clojure.jdbc) — as [jolt-lang/db](https://github.com/jolt-lang/db)'s
  `jdbc.core`, over the built-in SQLite access (libsqlite3 via Chez's FFI)
* [next.jdbc](https://github.com/seancorfield/next-jdbc) — a compatibility layer in
  [jolt-lang/db](https://github.com/jolt-lang/db) (`next.jdbc`, `next.jdbc.sql`,
  `next.jdbc.prepare`, `next.jdbc.transaction`) over `jdbc.core`, for libraries
  that target the next.jdbc API
* [tools.logging](https://github.com/clojure/tools.logging) — the real
  `clojure.tools.logging` source runs verbatim. jolt provides a native
  `clojure.tools.logging.impl` backend (a stderr `LoggerFactory` — the library's
  designed extension point, where slf4j/log4j/jul adapters normally plug in) plus
  the host shims it needs (`agent`/`send-off`, `clojure.lang.LockingTransaction`,
  a `clojure.pprint` subset, `clojure.string/trim-newline`). The level macros,
  `logf`/`logp`, `spy`, and `enabled?` all work; output goes to stderr.
* [migratus](https://github.com/yogthos/migratus) — database migrations; loads
  unmodified and runs filesystem SQL/EDN migrations against SQLite through the
  next.jdbc layer above. `migrate`/`rollback` round-trip end to end.
* [malli](https://github.com/metosin/malli) — data schema validation, on the
  [malli-app example](https://github.com/jolt-lang/examples/tree/main/malli-app).
  `m/validate` and `m/explain` work across the vocabulary (predicates, `:int`/
  `:string`/`:keyword`, `:map` incl. nested + optional, `:vector`, `:tuple`,
  `:enum`, `:maybe`, `:and`/`:or`, `:re`, bounded int/string). Load with
  `JOLT_FEATURES` including `clj` (malli's `.cljc` keys class-schemas off the
  `:clj` reader-conditional branches).
* [markdown-clj](https://github.com/yogthos/markdown-clj) — Markdown → HTML, on the
  [markdown-app example](https://github.com/jolt-lang/examples/tree/main/markdown-app).
  Renders headings, emphasis, inline code, links, lists, tables, strikethrough.
* [hiccup](https://github.com/weavejester/hiccup) — HTML from Clojure data, on the
  [hiccup-app example](https://github.com/jolt-lang/examples/tree/main/hiccup-app).
  Element tags, attribute maps, nested elements, and `for` comprehensions; its
  `html` macro pre-compiles the markup (a good compiler stress test).
* [clojure.data.json](https://github.com/clojure/data.json) — JSON reading and
  writing; `read-str`/`write-str` with key/value fns and options. Its own test
  suite passes 138/139.
* [clojure.spec.alpha](https://github.com/clojure/spec.alpha) — data specs;
  `s/def`, `s/valid?`, `s/conform`, `s/cat`/`s/keys`, `s/explain-str`, and
  `s/check-asserts` work over the registry.
* [tick](https://github.com/juxt/tick) — date/time over Jolt's `java.time`. Its
  `api` and `alpha.interval` test suites pass in full, including named-zone DST,
  nanosecond instants, and French locale formatting. Loads with `JOLT_FEATURES`
  including `clj`; `#time/…` literals work via `time-literals`' data readers.
