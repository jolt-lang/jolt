# jolt-7: compact call sites for calls through vars

Can a call through a var be spelled more compactly for Chez, either by the
emitter alone or with a Chez primitive, and what does each spelling cost in
compile time, `.so` size and run time? Three variants (four spellings) against
the default, on stock Chez 10.4.1, Jolt's patched Chez (0001-0007, `p7`) and
that Chez plus this pellet's patch 0010 (`p10`). Generated Clojure 200k project,
machine and checksum (374878) as in `jolt-1-harness.md`; `exp/chez-primitives`
7f66478c plus this pellet's commits.

## What a call through a var is today

```scheme
(let* ((_a$1 (var-cell-deref _kc$9)) (_a$2 (jolt-n- n 1)))   ; only when order is observable
  (jolt-site-call (lambda () (jolt-invoke-t1 _a$1 _a$2))))    ; the restore: tracing on, non-tail
```

`_kc$9` is bound once per def to `(jolt-var "ns" "name")`. `var-cell-deref`
(dyn-binding.ss) and `jolt-invoke-t1` (seq.ss) are out-of-line runtime
procedures: the deref checks for a thread binding, the invoke tests for a
procedure, checks its arity mask and applies it, or dispatches a keyword, map,
set, vector, multimethod or IFn record. On the 200k project: 81,874 `jolt-var`
bindings, 101,882 `var-cell-deref`, 102,044 `jolt-invoke*` calls.

## The variants

All off by default; unset, the emission is byte-identical (all 521 `.scm` of the
200k project's cold cache `cmp`-equal to the base binary's). The AOT cache
generation gets a `-cc<mode>` / `-dl<mode>` suffix.

| knob | spelling at the site | where |
|---|---|---|
| `JOLT_CLJ_CALL=inline` (variant 1, the pellet's shape) | `(jolt-clj-call f a ...)`: a runtime macro, `(let ((g f) (t a) ...) (if (and (procedure? g) (fxlogbit? n (procedure-arity-mask g))) (g t ...) (jolt-invoke-tN g t ...)))` | seq.ss |
| `JOLT_CLJ_CALL=extract` (variant 1, Racket CS's spelling) | `(jolt-clj-call-x f a ...)` = `((let ((g f)) (if (and (procedure? g) (fxlogbit? n (procedure-arity-mask g))) g (jolt-clj-callable g))) a ...)`: the test only picks what to apply, one call site, no temporaries | seq.ss |
| `JOLT_DIRECT_LINK=link` (variant 2) | the back end's existing direct linking (what `jolt build` sets) turned on for the runtime spine: app defs bind `jv$ns/name` with `def-var-linked!`, app calls apply it, seed calls apply a hoisted root. No inline pass, no inference | compile-eval.ss |
| `JOLT_DIRECT_LINK=full` (variant 2, as at the REPL) | `*compiler-options* {:direct-linking true}` for the run: linking plus the inline pass and inference | compile-eval.ss |
| `JOLT_CLJ_CALL=prim` (variant 3) | `(jolt-clj-call-prim f a ...)` → `(#%$clj-call f a ...)` where the Chez has the primitive, else the `inline` expansion | seq.ss, scheme-adapter-runtime.ss `sa-clj-call` |

The ordering `let*` the emitter adds when an operand's effect could be observed
is kept in every variant; the deref stays a `var-cell-deref` call. Untraced and
not in tail position the macros wrap the call in `jolt-site-kept`, as
`jolt-invokeN` does.

### Patch 0010: `$clj-call`

`host/chez/chez-patches/0010-clj-call-primitive.patch`, 33 added lines in 3
Scheme files of the compiler, nothing changed, no C:

| | lines |
|---|---|
| `s/cpnanopass.ss` | one clause in `np-recognize-attachment` (the pass 0008 uses), +24 |
| `s/primdata.ss` | the signature `[(ptr ptr ...) -> (ptr ...)]`, +1 |
| `s/prims.ss` | the out-of-line definition (interpreter, first-class use) and the handler's default, +8 |

`($clj-call f a ...)` is one opaque primitive call through cp0, cptypes and
cpletrec. `np-recognize-attachment` lowers it to: bind the operands; if `f` is a
procedure, load its code object and test the arity bit inline (the code's mask,
or the closure's for an arity-wrapper procedure, exactly what
`procedure-arity-mask` reads), and apply `f` with no further procedure check;
otherwise call the top-level value of `$clj-call-handler` with `f` first. Jolt
registers `jolt-invoke` there (seq.ss, through the adapter), which does what
`jolt-invoke-tN` does for every non-procedure and raises the same
ArityException for a wrong arity. Stock Chez and p7 get the `inline` expansion:
`sa-clj-call` checks `($primitive $clj-call)` when the adapter is compiled, as
0008's `sa-jolt-site-call` does.

### Racket CS and applicable structs

Racket CS has no Chez primitive for applicable structs. Its schemify pass emits
`|#%app|` where it cannot prove the operator is a procedure, and rumble defines
that as `(#3%$app (extract-procedure rator n-args) rand ...)` with
`extract-procedure` inlined to `(if (#%procedure? f) f (#3%$app/no-inline
slow-extract-procedure f n-args))`; the slow path reads `prop:procedure` and
returns a procedure to apply (or a wrong-arity wrapper). It does not check the
arity on the fast path: Chez raises the arity error and Racket converts it.
`JOLT_CLJ_CALL=extract` is that spelling, plus the arity check Jolt needs to
keep its ArityException message ("Wrong number of args (2) passed to: user/f5";
Chez's own condition converts to the class but not the message).
What came from Racket's fork and is upstream in 10.4.1: `$app`,
`$app/no-inline`, `$app/value` (primdata.ss, cp0.ss, prims.ss) and wrapper
procedures (`make-wrapper-procedure`, `make-arity-wrapper-procedure`, release
notes 10.0.0), which Racket uses for `procedure-reduce-arity`, renaming and
impersonators, not for structs. None of it is a test-and-fallback call.

## Results

### Compile only (`compact-ceiling/measure.ss`, in-process `compile-file`)

Each variant's cold cache (below) compiled again, all 521 `.scm`, runtime
loaded, inspector information on, the Chez the cache was built with.
Interleaved, median of 3 (the runs agree within 2.5%):

| | text, non-ws | compile | vs default | `.so` | vs default |
|---|---|---|---|---|---|
| stock default | 65,882,015 | 27.06 s (27.29 27.06 26.87) | | 124.0 MB | |
| stock `inline` (= `prim` on stock and p7) | 65,789,359 | 33.18 s (33.95 33.18 33.03) | **+22.6%** | 143.7 MB | **+15.9%** |
| stock `extract` | 65,984,901 | 32.59 s (32.51 32.59 32.63) | **+20.4%** | 139.9 MB | +12.8% |
| stock `link` | 59,835,384 (-9.2%) | 27.04 s (27.27 27.00 27.04) | -0.1% | 143.9 MB | **+16.0%** |
| p10 default | 65,882,015 | 26.99 s (26.99 27.09 26.73) | | 124.0 MB | |
| **p10 `prim`** | 66,278,214 | **30.66 s** (30.66 30.40 30.78) | **+13.6%** | **131.5 MB** | **+6.0%** |
| p10 `extract` | 65,984,901 | 32.77 s (32.48 32.77 33.54) | +21.4% | 139.8 MB | +12.8% |

### Cold build phases (`JOLT_PHASE_TIMES=1 JOLT_AOT_ASYNC=0`, one run each, ms)

The `.so` compile runs in-process (`aot-compile`), so the run is the whole cost
of a cold build. Checksum 374878 on every run.

| config | analyze+emit | expand | compile | `.so` compile | total |
|---|---|---|---|---|---|
| stock default | 14,738 | 1,160 | 19,577 | 25,155 | 65,751 |
| stock `inline` | 15,057 | 1,525 | **23,885** | **31,098** | 76,652 |
| stock `extract` | 15,119 | 1,406 | 23,890 | 30,261 | 75,667 |
| stock `link` | 15,159 | 1,262 | 19,908 | 26,868 | 68,332 |
| p7 default | 15,655 | 1,241 | 20,543 | 26,457 | 69,018 |
| p7 `prim` (the fallback) | 15,129 | 1,605 | 24,556 | 30,827 | 77,108 |
| p7 `link` | 14,737 | 1,212 | 19,649 | 26,154 | 66,443 |
| p10 default | 14,936 | 1,223 | 19,932 | 25,787 | 67,038 |
| **p10 `prim`** | 14,723 | **1,215** | **22,149** | **28,143** | 71,289 |
| p10 `extract` | 14,982 | 1,405 | 23,950 | 30,493 | 75,801 |
| p10 `link` | 15,307 | 1,260 | 20,267 | 26,945 | 68,743 |

`full` fails on this project (Semantics below). A first stock series an hour
earlier gave the same picture (default 20.1 / 25.7 s, `inline` 24.6 / 32.1 s,
`link` 19.8 / 26.7 s of compile / `.so` compile).

### What `link` removes and adds (emitted text, stock)

| | default | `link` |
|---|---|---|
| `jolt-var` lookups | 81,874 | 52,642 |
| `var-cell-deref` | 101,882 | 6,340 |
| `jolt-invoke*` calls | 102,044 | 8,260 |
| `def-var-linked!` defs, each with a setter closure `(lambda (v) (set! jv$… v))` | 0 | 13,463 |
| `jolt-seed-root` hoists of a clojure.core procedure | 0 | 42,425 |
| `jolt-register-source!` | 6,248 | 9,001 |

### Warm run (kept cold caches, interleaved, 5 rounds after a discarded first)

Median (all five runs), s; checksum 374878 every run:

| | stock | p7 | p10 |
|---|---|---|---|
| default | 2.91 (2.83 2.86 2.91 2.93 3.10) | 2.84 (2.79 2.82 2.84 2.97 3.16) | 2.92 (2.84 2.86 2.92 2.92 3.00) |
| `inline` / `prim` fallback | **3.19** (3.17 3.17 3.19 3.20 3.49) | **3.00** (2.97 2.99 3.00 3.08 3.28) | |
| `extract` | 2.89 (2.88 2.89 2.89 2.93 3.08) | | 2.87 (2.83 2.85 2.87 2.90 3.02) |
| `link` | 2.89 (2.87 2.87 2.89 2.94 3.06) | | 2.88 (2.84 2.84 2.88 2.91 3.12) |
| **`prim`** | | | **2.80** (2.76 2.76 2.80 2.81 2.95) |

The warm run is mostly loading (jolt-1); the variants that grow the `.so` by
16% do not slow it except `inline` (+0.16 to +0.28 s), and `prim` is the only
one below its default (-0.12 s).

### Call-heavy benches (`jolt -m <ns>`, fresh cache, tracing on, median of 3 means, ms)

`mfib` is jolt-3's: fib split across two fns (`fa` calls `fb` calls `fa`), so
every call is a non-tail call through a var.

| | fib | tak | mfib | dispatch | mono_dispatch |
|---|---|---|---|---|---|
| stock default | 22.8 | 7.0 | 70.5 | 1990.5 | 1574.5 |
| stock `inline` | 22.8 | 7.4 | 67.5 (-4%) | 1971.0 | 1556.4 |
| stock `extract` | 22.6 | 6.9 | 67.7 (-4%) | 1974.0 | 1555.0 |
| stock `link` | 21.2 | 6.3 | **47.0 (-33%)** | 1764.1 (-11%) | 1351.8 (-14%) |
| stock `full` | 21.2 | 6.1 | **19.3 (-73%)** | **892.7 (-55%)** | **821.0 (-48%)** |
| p10 default | 23.0 | 7.0 | 70.0 | 1989.8 | 1570.8 |
| p10 `extract` | 22.6 | 6.6 | 67.5 (-4%) | 1976.2 | 1549.0 |
| **p10 `prim`** | 22.7 | 7.2 | **56.5 (-19%)** | 1930.5 (-3%) | 1514.0 (-4%) |

`fib` and `tak` only self-call, which no variant touches. In `mfib`, `fa`'s
call to `fb` is a forward reference (`declare`), so under `link` only `fb`'s
calls link: half the calls, a third of the time. `full` also inlines.

## Reading it

- **No call-site spelling makes Chez compile less.** The default call is
  already compact for Chez: two out-of-line calls whose code is the runtime's,
  compiled once. Open-coding the test at every site costs the back end what it
  costs to compile a branch and two calls per site (and, in the emitter-only
  spellings, a third call to `procedure-arity-mask`): `inline` +23% compile,
  +16% `.so`; `extract`, which writes the operands once, saves little of that
  (+20%, +13%), so the duplicated operand list was not the cost. The primitive
  hides the shape from cp0, cptypes and the expander (expand 1.2 s, as default,
  against 1.5 s for `inline`) and open-codes the arity test, and still costs
  +14% compile and +6% `.so`: jolt-4's finding again, the back end pays for
  what a site does, however it is spelled. jolt-2's rewrite C (-12%) used an
  opaque one-call stub and dropped the `_kc$` bindings; it measured a site that
  does less, not a spelling of today's.
- **The runtime gain is small without linking.** `inline`/`extract` save 4% on
  `mfib` (one runtime call per call), `prim` 19% (no `procedure-arity-mask`
  call, no second procedure check).
- **Direct linking removes the work instead of respelling it.** `link` drops
  94% of the derefs and invokes and 30k var lookups, writes 9% less text,
  compiles in the same time, and is the only variant with a large run-time gain
  short of inlining (`mfib` -33%, `dispatch` -11%, `mono_dispatch` -14%). Its
  `.so` is 16% larger with no more compile time; the setter closure each linked
  def carries (13,463 of them, each its own code object with inspector
  information) is the likely share, not measured separately.
- Stock, p7 and p10 agree on everything that does not use the primitive
  (default, `link`, `extract` within run-to-run spread).

## Recommendation

**Variant 2, direct linking, for anything that can accept its semantics.** It is
the only variant that is not slower to compile, it is the one that makes calls
faster (`link` -33% on `mfib`, `full` -73%), and it already exists in the back
end (`jolt build`, `*compiler-options*`). The trade-off is the closed world: a
redefinition still reaches linked callers (def-var-linked!), but a var that
stops holding a procedure is applied as one (ClassCastException where Clojure
dispatches the new value), a clojure.core var the runtime defined is read once
per def, and under `full` an inlined body is frozen, as on the JVM. That fits
a cache build of a finished program or an opt-in at the REPL, not the open
world `jolt run` promises by default. Two things to fix before using it on the
runtime path: the `full` failure (jolt-9) and the run-to-run variation in
whether a REPL caller links. The `.so` growth (+16%) is worth finding: a
setter that does not need its own closure per def would recover it.

**Not variant 1** (either spelling): slower to compile, larger, and 4% at run
time. **Not variant 3 for compile time**: +14% compile and +6% `.so`; it is the
best open-world option for run time (`mfib` -19%, warm -4%) and the only one
that keeps every semantic, so it is worth keeping in mind if open-world call
speed matters more than compile time, but it does not serve this group's
question.

## Semantics

`test/chez/clj-call-redef-test.clj` (`jolt run`, each form `eval`'d as a REPL
compiles it) passes in the open world, under `inline`, `extract` and `prim`
(stock and p10, tracing on and off) and under `link`:

- `inline`, `extract`, `prim` change nothing observable: a `defn`,
  `alter-var-root` or `with-redefs` of the callee reaches the caller; keywords,
  maps and multimethods in vars are called as IFns; a wrong arity is the same
  ArityException with the same message.
- `link`: redefinition still reaches linked callers (the def is linked through
  `def-var-linked!`, whose setter writes every root write through to the `jv$`
  binding), as do `alter-var-root` and `with-redefs`; `^:redef` and
  `^:dynamic` opt out. What changes: a caller compiled against a fn applies the
  binding as a procedure, so if the var later holds a map, the call throws
  ClassCastException ("attempt to apply non-procedure {1 :one}") where the open
  world answers `:one`. Whether a caller links also depends on the session's
  link state: in a `jolt -e` session the same three forms gave the
  ClassCastException in some runs and `:one` in others, so the test accepts
  either under `link`. A seed (clojure.core) var the runtime defined itself is
  hoisted once per def (`jolt-seed-root`) and a later write to it does not
  reach that caller.
- `full` adds the JVM contract: an inlined callee keeps the body its caller was
  compiled against (test/chez/repl-direct-link-test.clj covers it).
  **It does not run the 200k project**: `workload.lalhi/brombri` fails with
  "variable ... is not bound". Pre-existing (the base binary fails the same):
  a `letfn` passed as an argument to an inlined call is emitted as a `let*` of
  fn-form makers that read each other before they are bound. Filed as jolt-9
  with a six-line repro. So `full` is measured on the benches only.

## Reproduce

```sh
E=/Users/admin/.cache/jolt-exp/jolt1/clj200k
P7=/Users/admin/.cache/jolt-chez-patched/10.4.1-p0001-0007/bin/chez
# patched Chez with 0010: clone v10.4.1, sh ci/apply-chez-patches.sh <src>,
#   ./configure --installprefix=<prefix> --threads --disable-x11 && make -j8 && make install
P10=<prefix>/bin/chez
make remint && make build && cp target/release/jolt target/release/jolt-stock
JOLT_CHEZ=$P7 $P7 --script host/chez/build-jolt.ss release target/release/jolt-p7
JOLT_CHEZ=$P10 $P10 --script host/chez/build-jolt.ss release target/release/jolt-p10
# one phased cold run with the .so compile in-process (cache kept)
(cd $E && JOLT_CLJ_CALL=prim JOLT_PHASE_TIMES=1 JOLT_AOT_ASYNC=0 JOLT_CACHE_DIR=<fresh> \
  target/release/jolt-p10 -m workload.main 42 3)
# compile only, over that cache (run from the repo root, with the matching Chez)
$P10 --script benchmarks/chez-primitives/compact-ceiling/measure.ss prim <cache>/<generation>/v1 <so-dir> '#t' 1
# warm: the same command again on the kept cache; benches: jolt -m <ns> in a
# directory holding bench/{fib,tak,dispatch,mono_dispatch}.clj, a fresh cache each
# semantics
JOLT_DIRECT_LINK=link target/release/jolt run test/chez/clj-call-redef-test.clj
```
