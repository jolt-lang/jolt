# clojure.string/escape

On a batch of 524,288 input characters with one replacement every 32 characters, Jolt went from 44.93 to 7.64 ms. Babashka took 13.17 ms and JVM Clojure took 3.55 ms.

## Warm execution

Times are milliseconds per batch. Each cell is the median of three fresh process medians. Each process ran ten warmups and seven measured batches per profile. The Babashka and JVM columns are from the sweep alongside the fixed Jolt build. Both sweeps, including every individual sample, are in [samples.csv](samples.csv).

| Input length | Replacement frequency and lookup | Jolt before | Jolt after | Speedup | Babashka | JVM Clojure |
|---:|---|---:|---:|---:|---:|---:|
| 32 | 1 in 32, map | 43.06 | 8.78 | 4.90× | 26.97 | 4.92 |
| 256 | No matches, map | 45.07 | 6.84 | 6.59× | 12.82 | 3.52 |
| 256 | 1 in 32, map | 44.93 | 7.64 | 5.88× | 13.17 | 3.55 |
| 256 | 1 in 2, map | 43.63 | 15.68 | 2.78× | 13.96 | 4.20 |
| 4096 | 1 in 32, map | 44.73 | 7.73 | 5.79× | 11.25 | 3.70 |
| 256 | 1 in 32, function wrapping the map | 48.58 | 10.50 | 4.63× | 19.68 | 4.56 |

The dense workload still takes 12% longer on Jolt than on Babashka. The remaining gap to JVM Clojure ranges from 1.78× on short strings to 3.73× on dense replacements.

## Startup and load

Median milliseconds from launching the process until the benchmark flushed its ready marker. This includes runtime startup, requiring `clojure.string`, and loading the benchmark definitions. Input and oracle construction happen after the marker, before the warmups.

| Sweep | Jolt | Babashka | JVM Clojure |
|---|---:|---:|---:|
| Before | 92.65 | 15.41 | 430.92 |
| After | 91.65 | 15.20 | 429.17 |

## Where the time went

The baseline implementation mapped a function over the string as a character sequence. It called `str` for each unchanged character, then passed the mapped sequence to `apply str`. On the sparse 256-character profile, consuming precomputed output took 0.04 ms per batch. Scanning the input and invoking the replacement map took 7.72 ms. The complete baseline operation took 47.57 ms in that diagnostic run. An indexed Clojure loop appending each character or replacement to a `StringBuilder` took 14.68 ms. Appending contiguous unchanged spans brought it to 9.30 ms. These controls used the same varying inputs, with one warmup and five measured batches each.

The fix scans the string directly, calls the supplied map or function once per character in order, and joins the replacements and unchanged spans. For non-string replacement values it uses Jolt's existing `str` renderer. A separate measurement with `jolt.profile/measure*` on the same 256-character batches shows how much allocation this removes. Values are median MB allocated per batch over three calls after warmup, in decimal units.

| Replacement frequency | Jolt before | Jolt after |
|---|---:|---:|
| No matches | 153.68 | 2.13 |
| 1 in 32 | 153.65 | 6.40 |
| 1 in 2 | 153.40 | 35.43 |

## Workload and correctness

Each profile cycles through 64 prebuilt inputs containing ASCII, accented Latin text, and CJK characters. The offsets and replacement characters vary. Four entries map `&`, `<`, `>`, and `"` to their HTML entities. Before timing, four literal string replacements produce an independent expected string for each input. Every runtime checks every output against it. Every timed and warmup batch consumes output lengths and sampled characters and checks the resulting checksum. All 1,836 batches across both sweeps agreed with the oracle.

Additional semantic probes covered empty strings, nil input, a `StringBuilder` input, nil and false replacements, deletion, non-string replacement values, callback order and exceptions, and adjacent surrogate replacement values. Both Jolt builds produced identical results. Supplementary input characters retain an existing runtime difference. For `🙂`, a Jolt callback sees one code point (128578), while Babashka and JVM Clojure call it with two UTF-16 units (55357 and 56898). The timed inputs use characters in the basic multilingual plane so that all runtimes perform the same number of lookups.

## Reproduction

Measurements used an Apple M3 Ultra VM running arm64 macOS 15.7.3, Chez Scheme 10.4.1, Babashka 1.12.218, and JVM Clojure 1.12.4 on OpenJDK 25.0.2. The baseline was a release build of `218a9ba584c4`. The fixed release used the same checkout and build recipe with the escape change applied. Each build had its own Jolt cache. The runner launches processes sequentially and rotates runtime order across the three launches.

From the checkout containing this benchmark, prefetch the JVM dependencies once. Build each Jolt revision with `make jolt-release` in its own checkout. Run this block once per build, setting `jolt_checkout` to the corresponding checkout.

```sh
kit="$PWD/benchmarks/string-escape"
(cd "$kit" && clojure -Srepro -P -M)
work=$(mktemp -d)
release=$(cd "$jolt_checkout/target/release" && pwd)
(
  export PATH="$release:$PATH" JOLT_CACHE_DIR="$work/cache"
  command -v jolt
  python3 "$kit/run.py" > "$work/samples.csv"
)
```

## Cherry-pick note

Cherry-pick `2ee39f2dbe36` for the runtime fix, both regenerated seeds, and the regression cases. The following benchmark commit contains the standalone workload, runner, raw samples, and this report.
