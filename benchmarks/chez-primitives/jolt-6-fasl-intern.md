# jolt-6: a Chez fasl hook that interns Jolt keywords

Question: if Chez interned Jolt's keyword objects as it loads a compiled file,
so a keyword literal could be a real quoted constant in the `.so`, how much
would that save over jolt-5's emitter-only constants table
(`JOLT_CONST_DATA=1`), and is it enough to justify a Chez patch Jolt would
then depend on?

Short answer: **no.** The hook works, and it is small (patch 0009, +39/−2
lines), but on the 200k project it buys about 1% of compile time over the
table, makes the `.so` 1.8% *larger* than the table's, and leaves warm and
edit times where the table left them (numbers below). The table already took
the construction code out of the forms; what the hook removes on top of that
is a `vector-ref` per keyword site, and a keyword record in a fasl costs
more bytes than the string row it replaces.

## What Chez 10.4.1 already does at load time

Read in `c/fasl.c`, `s/fasl.ss` and `s/vfasl.ss`:

- **Symbols** are interned by `fasl-read` (by name; gensyms by unique name),
  so a quoted Chez symbol already loads as the canonical object.
- **Record types with a uid** are canonicalized: the reader looks the uid's
  `*rtd*` property up and returns the registered rtd, checking the fasl'd
  copy for compatibility (`c/fasl.c` `fasl_type_rtd`). With
  `fasl-omit-rtds` (only reachable through `compile-to-port`, not
  `compile-file`) the type is written as its uid alone and must be
  registered when the file loads.
- **Record instances** get no such treatment: `fasl_type_record` allocates
  a fresh record every time. That is why a quoted `keyword-t` cannot
  appear in compiled code today; it would load as a copy and break
  keyword identity, which Jolt's `=`, `case` and map lookups rely on.
  Measured on stock Chez: a quoted keyword record written by
  `compile-file` and loaded in another process is a different object.
- **vfasl** (`s/vfasl.ss`, patch 0004's file) interns rtds by uid within the
  image and `c/vfasl.c` re-registers them at load, but copies ordinary
  records as raw memory, so no per-record hook runs there. Jolt's seed and
  `jolt build` images are vfasl, which is why the experiment never quotes a
  keyword in the seed mint or a direct-link build.
- Also present: `fasl-write`/`compile-to-port`'s *externals*
  (`external?-pred` plus a vector supplied to `load-compiled-from-port`).
  It could substitute keywords at load with no patch, but its indices are
  per fasl entry, and `compile-file` writes one entry per top-level form
  against one vector per file, so Jolt would have to drive the fasl entries
  itself. Not tried.

## What was built

**Chez patch 0009** (`host/chez/chez-patches/0009-fasl-intern-records.patch`,
`c/fasl.c`, `c/globals.h`, `s/fasl.ss`; +39/−2 lines, about 30 of them
code):

- A record whose type's uid carries the property `$fasl-intern` is interned
  as `fasl-read` builds it, exactly the way an rtd is interned by its uid:
  its first field must be a symbol, and the record is replaced by the one
  on that symbol's `$fasl-intern` property, or registers itself there when
  there is none. Under the tc-mutex, like the rtd case. One plist walk on
  the record's uid per record read when no type is marked.
- The writer emits such a type by uid alone, as `fasl-omit-rtds` would. (The
  first version wrote the full rtd in every fasl entry that held a keyword,
  about 115 bytes per top-level form, 3 MB of `.so` over the project.)

Nothing in Chez marks a type, so the patch is inert until a runtime does.

**Jolt, `JOLT_FASL_INTERN=1`** (off by default):

- `keyword-t` gains a first field, `ikey` (`#f` when off; the uid moves to
  `keyword-v2`). Turning the variant on publishes every existing keyword on
  the Chez symbol interned from its intern key (`"ns\0name"`), then marks
  the type; `keyword` publishes each new one, adopting a keyword a `.so`
  registered first. Both sides take Chez's tc-mutex, so a load on one
  thread and a first `keyword` call on another agree on one object.
- A probe (`keyword-fasl-intern-supported?`) round-trips a keyword through
  `fasl-write`/`fasl-read`. Only if it comes back `eq?` does the emitter
  change, so on stock Chez or the 0001–0007 build the variable does nothing
  and the run is whatever `JOLT_CONST_DATA` makes it (verified: no `-fi`
  cache generation, same `.scm`).
- The emitter (`hoist.clj keyword-const`) spells a keyword literal
  `(jolt-kw-lit ns name)`, a macro that expands to the quoted keyword object,
  so the eval path gets the live object and `compile-file` writes the record.
  With `JOLT_CONST_DATA=1` the per-form table becomes `(jolt-kd-lit #(...))`,
  quoted with its keyword rows already keyword objects; collection rows still
  build at load and refer to them. Same scope as the table: not the seed
  mint, not direct-link builds, not var-cache-off emits.
- Symbol literals are unchanged (jolt-5's per-site rows). A quoted
  `symbol-t` would load with an unpooled name string and a copy of
  `jolt-nil` as its meta; making that right needs `jolt-nil` interned too
  and the name pool rebuilt from the fasl'd strings, which is more than
  this slice.
- The cache generation gets a `-fi` suffix.

Checked by hand: a keyword that a `.so` loads before the runtime has made it
(`:fresh-kw-xyz`) is the object `(keyword "fresh-kw-xyz")` later returns;
`case` on keywords, map lookups, `(identical? 'q 'q)` false, all from a warm
cache, on stock, 0001–0007 and 0001–0009 builds.

## Results

Jolt at this branch, Apple M3 Ultra (virtual), 24 cores. Clojure 200k
project (`--seed 42 --lines 200k --namespace-lines 400`, args `42 3`).
Checksum 374878 on every run.

### Text, `.so`, compile only (`compact-ceiling/measure.ss`)

In-process `compile-file` of each cache's 521 `.scm` files, inspector
information on. Pass 1 on stock Chez (5 interleaved rounds, the patch-0009
writer not in play); pass 2 on the 0001–0009 Chez (3 rounds), whose writer
omits the keyword rtd, which gives the `.so` sizes that build really writes.

| | `.scm` bytes | keyword sites | table slot reads | compile, pass 1 | vs off | vs `kd` | compile, pass 2 | `.so` (0001–0009) | vs off | vs `kd` |
|---|---|---|---|---|---|---|---|---|---|---|
| off | 72.08 MB | 106,438 `(keyword …)` | | 27.06 s | | | 26.78 s | 123.5 MB | | |
| `kd` (jolt-5) | 70.15 MB | 14 | 112,920 | 25.25 s | −6.7% | | 24.10 s | 112.4 MB | −9.0% | |
| **`kd` + hook** | 70.24 MB | 51,336 quoted | 61,584 | **24.78 s** | −8.4% | **−1.9%** | 24.56 s | **114.5 MB** | −7.3% | **+1.8%** |
| hook alone | 70.85 MB | 128,280 quoted | | 25.75 s | −4.8% | | 26.14 s | 119.3 MB | −3.4% | |

Pass 2's per-round ratios of `kd`+hook to `kd` were +2.2%, −1.4% and −0.6%
(its third round ran about 12% slower across all four caches), pass 1's
−0.0%, −2.1%, −1.6%, −2.1%, −3.3%: the hook's compile gain over the table is
about 1–2%, at the edge of the run-to-run spread.

### Whole runs (`run.sh`, cold and common median of 3, warm 5, leaf 3)

Each column is one series on one binary. The stock and 0001–0007 columns
set `JOLT_CONST_DATA=1 JOLT_FASL_INTERN=1`: their probe fails, so they are
the table alone, as the patch's fallback would run.

| | stock, off | stock, `kd`+`fi` (fallback) | 0001–0007, `kd`+`fi` (fallback) | 0001–0009, `kd` | **0001–0009, `kd`+`fi` (hook)** |
|---|---|---|---|---|---|
| Cold build | 46.18 s | 44.99 s | 40.75 s | 39.07 s | **38.46 s** |
| compile phase (phased run) | 22.24 s | 21.81 s | 19.19 s | 17.93 s | **17.71 s** |
| analyze+emit phase | 16.44 s | 18.73 s | 16.08 s | 14.91 s | 15.33 s |
| phased run total | 45.58 s | 48.08 s | 41.71 s | 38.55 s | 38.86 s |
| Warm | 2.59 s | 2.98 s | 2.48 s | 2.42 s | **2.36 s** |
| Leaf edit | 2.59 s | 3.13 s | 2.63 s | 2.52 s | 2.56 s |
| Common edit | 40.09 s | 44.19 s | 40.49 s | 38.00 s | **37.57 s** |

The series drift as a whole (the stock fallback column is slow in every row,
analyze+emit included, which the hook cannot touch), so only the last two
columns compare: same binary, back to back. There the hook is −0.6 s cold
(−1.6%), −0.2 s of compile phase (−1.2%), −0.4 s common edit (−1.1%), and
the same leaf edit. The stock fallback's slow warm was checked separately:
9 interleaved runs on one stock binary and cache, `JOLT_FASL_INTERN` unset
and set (the failed probe), gave 2.78 s and 2.76 s, so the probe costs
nothing measurable and that column is drift.

### Warm, interleaved (15 runs each, one process at a time)

| 0001–0009 binary | median (min) | vs off | vs `kd` |
|---|---|---|---|
| off | 3.11 s (3.00) | | |
| `kd` | 2.96 s (2.89) | −4.9% | |
| **`kd` + hook** | 2.98 s (2.93) | −4.2% | +0.6% |
| hook alone | 2.95 s (2.90) | −5.1% | |
| stock binary, `kd` | 2.99 s (2.85) | | |

(Interleaving five binaries and caches puts every run about 0.5 s above the
back-to-back series, as jolt-5 saw; the ratios are what compare.) The hook
does not move warm load relative to the table.

## Reading it

- **Compile.** The table already removed the keyword constructions, the
  pool bindings and the per-def setup; what the hook adds is turning
  51k `(#3%vector-ref _kt$ i)` reads into quoted constants. Chez compiles
  either cheaply, so the difference is ~1–2% of compile time, against
  the table's −6.7%. Without the table (hook alone) it recovers −4.8%,
  less than the table's −6.7%: the collections and metadata maps around the
  keywords are still constructed per def, and they were half of the table's
  gain.
- **`.so`.** A keyword in the fasl is a record: its intern-key symbol (whose
  name is another copy of the name), the ns and name strings, the hash and
  a type reference. Measured in isolation, the first keyword in a fasl entry
  costs 56 bytes and each further one 28, against 10 and 6 bytes for the
  string row the table holds. The table pays 104k such rows; the code-site
  savings (a relocation instead of a `vector-ref`) do not cover the
  difference, so the `.so` ends up 2 MB larger than the table's. A slimmer
  record (only the key symbol, with ns/name read off it) would need the
  hot `keyword-t-name`/`keyword-t-ns` accessors to change.
- **Load.** The work the table's `intern-constants!` did per keyword (a
  string hash lookup) moves into the C reader (a symbol intern and a plist
  walk), so warm time could not move much, and did not (above).
- **Patch cost.** It is small and self-contained, but it would make every
  patched-Chez build of Jolt a different program from a stock-Chez one: on
  stock Chez the hook cannot be emulated, so Jolt must keep the table path
  anyway. A mandatory-feature patch is not justified by ~1–2% compile time
  and a larger `.so`.

What would change the answer: quoting whole constant collections (metadata
maps, arglists), which is where ceiling B's remaining gain sits. That needs
every record inside them to survive fasl: `jolt-nil` interned the same way
(the hook supports it, given a key field), symbols' pooled strings rebuilt,
and the persistent collections' internals checked for identity-bearing
singletons. That is a different, larger experiment; this one shows the
hook itself is cheap and correct, and that keywords alone are not where the
remaining cost is.

## Reproduce

```sh
# patched Chez with 0001-0009 (its own prefix)
git clone --depth 1 --branch v10.4.1 --recurse-submodules https://github.com/cisco/ChezScheme.git $SRC
sh ci/apply-chez-patches.sh $SRC
(cd $SRC && ./configure --installprefix=$P9 --threads --disable-x11 && make -j8 && make install)
make remint
JOLT_CHEZ=$P9/bin/scheme $P9/bin/scheme --script host/chez/build-jolt.ss release target/release/jolt-p9
# whole runs: the hook with the table, and the table alone, on the same binary
JOLT_CONST_DATA=1 JOLT_FASL_INTERN=1 sh benchmarks/chez-primitives/run.sh target/release/jolt-p9 $P9/bin/scheme <clj200k> - <work>
JOLT_CONST_DATA=1 sh benchmarks/chez-primitives/run.sh target/release/jolt-p9 $P9/bin/scheme <clj200k> - <work2>
# gates with the hook on
JOLT_CONST_DATA=1 JOLT_FASL_INTERN=1 make corpus CHEZ=$P9/bin/scheme
```

Verification: GATES
