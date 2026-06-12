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
* [honeysql](https://github.com/seancorfield/honeysql) — full formatter + helpers
  (select/insert/update/delete/joins/:inline), loaded unmodified from git
* [clojure.jdbc](https://github.com/yogthos/clojure.jdbc) — as [jolt-lang/db](https://github.com/jolt-lang/db)'s
  `jdbc.core`, reimplemented over janet sqlite3/pq drivers (SQLite + PostgreSQL)
