# Jolt string-hash cache boundary

Run from the repository root with Babashka and Jolt on `PATH`.

```sh
bb benchmarks/string-hash-cache/run.bb
```

The Jolt worker builds a vector of distinct, equal-length account strings. Each
pass walks the vector in order, calls `hash` on each string, and sums the
results. Every pass reuses the same string objects. Jolt 0.8.26 caches up to
2,048 string hashes per thread and clears the table on the next miss. With
2,048 keys the warmed table holds the whole vector. With 2,049 keys, cycling
through the vector repeatedly clears and refills the table. [Cache implementation](../../host/chez/hasheq.ss)

The Babashka driver launches a fresh Jolt process per size in each of three
rounds, rotating the order between rounds. Each batch makes as many complete
passes as fit in one million hash calls. The worker times only the loop.
The driver checks every checksum against Babashka's own hashes, discards five
warmups, and divides the median of seven measured batch times by the actual
hash count. The headline number is the median of the three launch medians.

The default run on Jolt 0.8.26 took 4.78 times as long per hash when the
working set grew from 2,048 strings to 2,049.

| Distinct strings | Median ns/hash |
| ---: | ---: |
| 2,047 | 19.41 |
| 2,048 | 19.13 |
| 2,049 | 91.52 |
| 10,000 | 94.87 |

To compare a local Jolt build at particular sizes, pass the executable through
`JOLT_BIN` and the key counts as positional arguments.

```sh
JOLT_BIN=./target/release/jolt bb benchmarks/string-hash-cache/run.bb 2048 2049
```
