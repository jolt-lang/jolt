# jolt-5: namespace constants as quoted data, interned by one call

Question: if the emitter hands Chez each form's load-time constants as quoted
data plus one runtime intern call, instead of construction code, how much
compile time, `.so` size and warm-load time does that save, with no Chez
change? jolt-2's rewrite B ("constants") is the ceiling to compare against:
−14.1% compile and −13.7% `.so` in its offline measurement.

## What was built

`JOLT_CONST_DATA`, off by default. Off, the emitted Scheme is byte-identical:
all 521 `.scm` files of the 200k project's cold cache match the base's
(checked after each slice with `cmp` against the jolt-2 cache of `7ed36439`).

With `JOLT_CONST_DATA=1`, `emit-top-form` collects each top-level form's
constants into one table (`jolt-core/jolt/backend_scheme/hoist.clj`
`with-const-data`) and wraps the form:

```scheme
(let ((_kt$ (intern-constants! '#("arglists" (v) (l #&1) "line" "column" "file"
                                   (am #&0 #&2 #&3 2 #&4 1 #&5 "t.clj")))))
  (def-var-with-meta! "t" "f" ... (#3%vector-ref _kt$ 6)))
```

- **Slice 1, keywords and symbols.** A string row is the keyword `:name`,
  `("ns" . "name")` a qualified keyword, `#(ns-or-#f "name")` a metadata-free
  symbol. Keyword rows are shared by every site in the form (keywords intern, so
  `identical?` and `case` see the same objects as before); a symbol row belongs
  to one site, so `(identical? 'a 'a)` still answers false. Sites read
  `(#3%vector-ref _kt$ i)`; the constant pool no longer binds them.
- **Slice 2, constant collections.** A vector, list, map or set literal, a
  quoted collection, and a metadata map or arglist whose parts are all constants
  becomes a row `(v|l|m|am|s elt ...)`: `#&i` reads row i, `nil` is jolt-nil,
  strings, numbers, booleans and chars stand for themselves.
  `values.ss` `intern-constants!` calls the constructor the emit would have
  called. Site-stamped literals (`jolt-site-const`) and literals whose keys the
  runtime must check stay constructions.
- **Not wrapped:** a form containing `(define` (direct-link defs, `define-ftype`,
  contagion clones) goes out with the constructions spliced back in. Direct-link,
  the seed mint and var-cache-off emits never take the table.
- `JOLT_CONST_DATA=bind` keeps slice 1's table but binds each slot read once per
  def (`(_kc$N (#3%vector-ref _kt$ i))`), the shape the constant pool had. It
  measured worse than reading the slot at each site (below), so slice 2 only
  extends `=1`.
- The cache generation gets a `-kd` (or `-kdb`) suffix so variants never share
  artifacts. The gate scripts honour the variable too, so any gate can run with
  the variant on.

Two bugs found on the way, both visible only as size: discarded renderings
(fnsrc's read-back comparison) left rows behind and, with per-site symbol slots,
made every fn source fail the comparison and fall back to constructed form
(text +61%); and symbol slots inside the fnsrc quote pool defeated its
hash-consing. Both fixed (`without-kdata`, `symbol-const` shared mode).

## Results

Jolt `exp/chez-primitives` (`d85178d8`) plus this branch, Apple M3 Ultra
(virtual), 24 cores, the same binary for off and on in every pair. Clojure 200k
project as in jolt-1 (`--seed 42 --lines 200k --namespace-lines 400`, args
`42 3`). Checksum 374878 on every run.

### Compile only (`compact-ceiling/measure.ss`, in-process `compile-file`)

Median of 5 interleaved runs, stock Chez, inspector information on. Each pass
compares only within itself (the machine's single-thread speed drifts between
passes, as jolt-2 found).

| | Text, non-ws | Compile | vs off | `.so` | vs off |
|---|---|---|---|---|---|
| pass 1: off | 65.88 MB | 27.05 s | | 123.7 MB | |
| pass 1: slice 1 (`=1`) | 66.31 MB | 26.11 s | −3.5% | 118.5 MB | −4.2% |
| pass 1: slice 1 (`=bind`) | 68.06 MB | 26.29 s | −2.8% | 120.0 MB | −3.0% |
| pass 2: off | 65.88 MB | 25.83 s | | 123.8 MB | |
| pass 2: slice 1 | 66.31 MB | 25.03 s | −3.1% | 118.5 MB | −4.3% |
| **pass 2: slice 2** | **63.96 MB** | **24.27 s** | **−6.0%** | **112.7 MB** | **−9.0%** |
| jolt-2 ceiling B | 59.6 MB | | −14.1% | 105.6 MB | −13.7% |

A third pass, run while another worker loaded the machine, drifted from 33 s
to 26 s over its rounds; per-round ratios gave slice 1 −4.1% and slice 2 −9.3%
(medians), consistent with the two clean passes.

### Whole runs (`run.sh`, cold and common median of 3, warm of 5, leaf of 3)

Slice 2 (`c128f208`), off and on back to back per Chez build:

| | stock off | stock on | patched off | patched on |
|---|---|---|---|---|
| Cold build | 42.99 s | 40.50 s | 43.14 s | 42.39 s |
| compile phase (one phased run) | 20.91 s | 19.48 s | 21.53 s | 19.48 s |
| analyze+emit phase | 15.33 s | 16.40 s | 16.05 s | 16.47 s |
| phased run total | 42.42 s | 42.33 s | 44.10 s | 42.42 s |
| Warm | 2.76 s | 2.41 s | 2.75 s | 2.65 s |
| Leaf edit | 2.77 s | 2.60 s | 2.76 s | 2.85 s |
| Common edit | 42.15 s | 38.96 s | 43.00 s | 41.29 s |
| `.so` (stock) | 124.2 MB | 113.1 MB | | |

Slice 1 (`efd401fa`), same layout:

| | stock off | stock on | patched off | patched on |
|---|---|---|---|---|
| Cold build | 43.79 s | 43.46 s | 43.65 s | 42.17 s |
| compile phase | 21.36 s | 20.81 s | 21.45 s | 20.03 s |
| analyze+emit phase | 15.99 s | 16.22 s | 15.80 s | 15.41 s |
| Warm | 2.89 s | 2.73 s | 2.83 s | 2.47 s |
| Leaf edit | 2.92 s | 2.91 s | 2.88 s | 2.83 s |
| Common edit | 42.90 s | 42.53 s | 42.76 s | 41.99 s |
| `.so` (stock) | 124.2 MB | 118.9 MB | | |

The warm rows disagree between the builds in opposite directions per slice
(slice 1: stock −6%, patched −13%; slice 2: stock −13%, patched −4%), so they
were remeasured interleaved, 15 runs per configuration, one process at a time:

| Warm, interleaved, median (min) | off | on | change |
|---|---|---|---|
| slice 1, stock | 2.58 s (2.47) | 2.52 s (2.37) | −2.6% |
| slice 1, patched | 2.63 s (2.46) | 2.52 s (2.36) | −4.1% |
| slice 2, stock | 2.93 s (2.75) | 2.78 s (2.64) | −5.1% |
| slice 2, patched | 2.91 s (2.79) | 2.79 s (2.63) | −4.2% |

(The absolute level moved between those two sessions; each row's pair ran
interleaved.) So warm load gains about 3% with slice 1 and 4–5% with slice 2 on
both builds; the harness's larger warm numbers were drift between back-to-back
series.

### Reading it

- **Compile: slice 2 recovers about 40% of ceiling B** (−6.0% against
  −14.1%), and slice 1 about a quarter. In the cold build the eval-path compile
  phase falls by 1.4–2.0 s (7–10%), but analyze+emit rises by 0.4–1.1 s, so the
  phased totals move by −0.1 s (stock) and −1.7 s (patched); the cold medians
  (−2.5 s and −0.75 s) sit within that range plus noise. The emit cost is the
  table bookkeeping done on strings (slot parsing, sorting keys rebuilt by
  `kdata-expand`); an IR-level collector would avoid most of it.
- **`.so`: −9.0%**, two-thirds of ceiling B's −13.7%.
- **Why not the ceiling.** B substituted a quoted literal at each use, which is
  a code-object constant; here every use is `(#3%vector-ref _kt$ i)`, about 20 bytes of
  text against `_kc$403994`'s 10, so the text barely shrinks (66.3 MB, then
  64.0 MB, against B's 59.6 MB), and each closure referencing a constant
  captures `_kt$`. B also moved the 20,252 `image-register-fn-form!` rows into
  its per-namespace table, which this does not. Binding each slot once per def
  (`=bind`) is worse, not better: −2.8% compile, −3.0% `.so`.
- **Patched against stock Chez:** the same within run-to-run spread, as in
  jolt-1 and jolt-2; no patch interacts with this.

## Which constants dominated

Rows in the tables, slice 2 (26,234 tables, 3.24 MB of quoted text in all):
about 104k keyword rows (shared within a form; 106,424 constructions before),
34,493 symbol rows, and 81,187 collection rows: 48,364 vectors (arglists and
argument vectors in metadata, `require` specs, fn-form free-name vectors),
12,658 array maps (metadata maps), 10,827 lists (arglists), 8,974 hash maps,
364 sets. Keywords were the most numerous constant and collections the largest
share of the gain: slice 1 (keywords and symbols) gave −3.5% compile and −4.2%
`.so`, slice 2's collections and metadata as much again.

What remains of the per-form setup code after slice 2, from the emitted text:

| Construct | Count | Belongs to |
|---|---|---|
| `jolt-var` var-cell lookups, now nearly all of the 81,549 pool bindings left | 81,874 | calls (jolt-2 rewrite C) |
| `jolt-register-callsite!` | 81,136 | tracing (rewrite A) |
| `image-register-fn-form!` rows with `image-fn-form-src` strings | 20,252 | fn forms (in B, not done here) |
| non-constant `jolt-vector` / `amap-slots->pmap` | 7,866 / 6,864 | not constants |
| `jolt-register-source!` | 6,248 | source registry |
| `aot-replay-def-ordinals!`, already quoted data | 521 | |

So the remaining load-time setup is dominated by var-cell lookups and callsite
registrations, then the fn-form rows. Constants proper are gone: 14 keyword
constructions are left in the whole project, in forms that hold a `define`.

## Not done

- Lazy metadata (building a var's metadata map on first read). The metadata
  maps are table rows now, built eagerly at load.
- Moving the fn-form registrations (`image-register-fn-form!`) into the table,
  which B did.
- A per-namespace table instead of one per top-level form: the runtime spine
  emits and runs one form at a time, so the namespace boundary is not visible
  where the table is built.

## Reproduce

```sh
make remint && make build                       # stock binary, target/release/jolt
cp target/release/jolt target/release/jolt-stock
P=/Users/admin/.cache/jolt-chez-patched/10.4.1-p0001-0007/bin/chez
JOLT_CHEZ=$P $P --script host/chez/build-jolt.ss release target/release/jolt-patched
# whole runs, off then on (any other value but "" / "0" / "bind" means on)
sh benchmarks/chez-primitives/run.sh target/release/jolt-stock /opt/homebrew/bin/chez <clj200k> - <work-off>
JOLT_CONST_DATA=1 sh benchmarks/chez-primitives/run.sh target/release/jolt-stock /opt/homebrew/bin/chez <clj200k> - <work-on>
# compile only, from two cold caches (JOLT_CACHE_DIR=<c-off> / JOLT_CONST_DATA=1 JOLT_CACHE_DIR=<c-on>)
chez --script benchmarks/chez-primitives/compact-ceiling/measure.ss on <c-on>/<generation>/v1 <so-dir> '#t' 1
# a gate with the variant on
JOLT_CONST_DATA=1 make corpus
```

Verification with the variant on: `make corpus` 7664/7674 (the 10 known
divergences, no new ones), `make unit`, `errorreport` (52 cases, stack traces
unchanged), `tracesmoke` (82), `traceeval` (10/10), `degradedbacktrace` (6/6)
all pass.
