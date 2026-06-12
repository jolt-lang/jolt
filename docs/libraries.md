# Clojure libraries known to work with Jolt

Libraries confirmed to load and pass their conformance checks on Jolt
(see `test/integration/deps-conformance-test.janet` and the
[greeter example](https://github.com/jolt-lang/examples/tree/main/greeter)).

* [config](https://github.com/yogthos/config)
* [Selmer](https://github.com/yogthos/Selmer)
* [medley](https://github.com/weavejester/medley)
* [cuerdas](https://github.com/funcool/cuerdas)
* [ring-core](https://github.com/ring-clojure/ring) — via `:deps/root "ring-core"`,
  on the [ring-app example](https://github.com/jolt-lang/examples/tree/main/ring-app)'s
  spork/http adapter
* [ring-codec](https://github.com/ring-clojure/ring-codec)
