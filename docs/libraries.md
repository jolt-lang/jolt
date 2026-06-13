# Clojure libraries known to work with Jolt

Libraries confirmed to load and pass their conformance checks on Jolt
(see `test/integration/deps-conformance-test.janet` and the
[ring-app example](https://github.com/jolt-lang/examples/tree/main/ring-app)).

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
* [honeysql](https://github.com/seancorfield/honeysql) — full formatter + helpers
  (select/insert/update/delete/joins/:inline), loaded unmodified from git
* [clojure.jdbc](https://github.com/yogthos/clojure.jdbc) — as [jolt-lang/db](https://github.com/jolt-lang/db)'s
  `jdbc.core`, reimplemented over janet sqlite3/pq drivers (SQLite + PostgreSQL)
* [next.jdbc](https://github.com/seancorfield/next-jdbc) — a compatibility layer in
  [jolt-lang/db](https://github.com/jolt-lang/db) (`next.jdbc`, `next.jdbc.sql`,
  `next.jdbc.prepare`, `next.jdbc.transaction`) over `jdbc.core`, for libraries
  that target the next.jdbc API
* [migratus](https://github.com/yogthos/migratus) — database migrations; loads
  unmodified and runs filesystem SQL/EDN migrations against SQLite through the
  next.jdbc layer above. `migrate`/`rollback` round-trip end to end. Caveat:
  migration ids are 14-digit timestamps, and the janet-lang/sqlite3 driver
  currently truncates INTEGER columns to 32 bits, so completion tracking needs
  the one-line upstream fix (`sqlite3_column_int64`); ids under 2^31 work as is.
