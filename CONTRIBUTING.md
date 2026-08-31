# Contributing to Jolt

This is the contributor-side document: building from source, what lives where,
the second Scheme backend, and the test gates. For using jolt, see
[README.md](README.md) and [jolt-lang.github.io](https://jolt-lang.github.io).

## Contents

- [Build from source](#build-from-source)
- [The seed and re-minting](#the-seed-and-re-minting)
- [Standalone jolt binary](#standalone-jolt-binary)
- [Architecture](#architecture)
- [Scheme backends](#scheme-backends)
- [Build profiles](#build-profiles)
- [Test](#test)
- [Documentation](#documentation)

## Build from source

Running from source has no build step. The bootstrap seed
(`host/chez/seed/{prelude,image}.ss`) is checked in, so a fresh clone runs
immediately:

```bash
git clone --recurse-submodules https://github.com/jolt-lang/jolt.git
cd jolt
bin/jolt -e '(+ 1 2)'        # => 3
```

The `--recurse-submodules` matters: jolt vendors its regex engine, its Maven
resolver, and its test suites as git submodules. In a checkout that's missing
them (a plain `git clone`, or after pulling a commit that adds one), fetch them
with:

```bash
git submodule update --init --recursive
```

GitHub's auto-generated "Source code (zip/tar.gz)" archives on the releases page
do **not** contain submodules, so they can't run or build — clone the repo
instead.

`bin/jolt` needs a **threaded Chez Scheme 10.x** on `PATH` as `chez` or
`chezscheme`; set `JOLT_CHEZ` to point at a specific one. `make` uses a Chez on
`PATH` at or above its pinned version as-is, and provisions its own 10.4.1 only
when nothing qualifies. It exports `JOLT_CHEZ` so both halves of a build agree —
running `bin/jolt` by hand against a 9.x picks up whatever primitive that
release predates (`variable flvector? is not bound`) — and, when provisioning
did run, `JOLT_CC` too, so the standalone binary links with the same GCC that
built Chez instead of whatever `cc` happens to resolve to.

`make build` provisions [Chez Scheme](https://cisco.github.io/ChezScheme/) and a
C compiler locally through [Makes](https://github.com/makeplus/makes), then
builds the standalone binary. An explicit `CHEZ=/path/to/chez` (or
`CHEZSCHEME=/path/to/scheme`) is authoritative and bypasses local provisioning;
release builders use this to retain their threaded Chez and platform toolchain.
The conformance gate additionally uses Clojure on the JVM as an optional oracle,
but running jolt does not.

## The seed and re-minting

After changing a compiler source — the reader (`host/chez/reader.ss`), the
analyzer/IR/backend (`jolt-core/jolt/*.clj`), or the `clojure.core` overlay
(`jolt-core/clojure/core/*.clj`) — re-mint the seed:

```bash
make remint                   # iterates host/chez/bootstrap.ss to a byte-fixpoint
```

A change that is not followed by `make remint` silently does nothing: the
checked-in seed still carries the old code, and rebuilding the binary alone does
not help.

That trap extends past `jolt-core/`. `ei-prelude-ns-files` in
`host/chez/emit-image.ss` also compiles seven `stdlib/` namespaces into the seed
— `clojure.string`, `clojure.walk`, `clojure.template`, `clojure.edn`,
`clojure.set`, `clojure.pprint`, `clojure.repl`. At runtime a `require` of any of
them no-ops, because the vars are already in the image, so an edit to one is
invisible until the seed is re-minted. Every other `stdlib/` namespace loads off
the source roots and needs no re-mint.

## Standalone jolt binary

`make` (or `make build`) installs the build dependencies locally through Makes
and builds jolt itself into a single self-contained native binary. The runtime,
compiler, `jolt-core`/`stdlib` source, and the Chez boots are baked in, so the
result runs and `build`s jolt apps on a machine with neither Chez nor a C
compiler.

```bash
make build                    # => target/release/jolt (optimize-level 3, compressed)
make install                  # => ~/.local/bin/jolt, or /usr/local/bin/jolt as root
make install PREFIX=/opt/jolt # explicitly override the installation prefix
make jolt-release             # force-rebuild the release binary
make jolt-debug               # => target/debug/jolt   (optimize-level 0, inspector + debug info)
make jolt                     # re-mint the seed first, then both
```

`make jolt` re-mints the seed so the embedded compiler image is current before
linking; `jolt-release`/`jolt-debug` force their respective builds without
re-minting. `make clean` removes build products; `make distclean` also removes
the locally provisioned Makes toolchain.

## Architecture

A small Chez runtime (`host/chez/*.ss`: value model, persistent collections, seqs,
vars/namespaces, host interop) hosts a portable Clojure overlay split across two
source roots by *when* they load:

- **`jolt-core/`** is baked into the seed — the compiler (`jolt-core/jolt/`:
  reader/analyzer/IR/backend, plus `jolt.main`/`jolt.deps`) and `clojure.core` in
  dependency-ordered tiers (`jolt-core/clojure/core/NN-*.clj`). Changing anything
  here means re-minting the seed.
- **`stdlib/`** loads lazily at runtime off the source roots — the rest of the
  standard library (`clojure.string`/`set`/`walk`/`edn`/`pprint`/…) plus the
  `jolt.ffi` host library. Editing most of these needs no re-mint; the seven
  listed in `ei-prelude-ns-files` are the exception (see below).

`bin/jolt` loads the checked-in seed and the spine, then compiles and evaluates on
Chez (read → analyze → IR → emit → eval). `host/chez/bootstrap.ss` rebuilds that
seed from source on pure Chez; the build is a self-hosting fixpoint (a rebuild
reproduces the checked-in seed byte-for-byte).

`host/gambit/` is the same overlay on a second Scheme — its own adapter, kernel,
and cross-minted seed. See [Scheme backends](#scheme-backends).

The per-module map is on the site:
[Module Map](https://jolt-lang.github.io/docs/MODULES.html) and
[Seed & Overlay Registry](https://jolt-lang.github.io/docs/seed-overlay-registry.html).

## Scheme backends

Chez is the default target: every gate, library, and release runs there, and it
is the only target with FFI, native compilation, program images, and standalone
binaries. Gambit is a second, demo-grade target that also compiles to a single
JavaScript file — the live REPL on the [website](https://jolt-lang.github.io) is
jolt evaluating in the browser.

Host-specific runtime code sits behind an adapter contract
(`host/scheme-adapter/CONTRACT.txt` lists the names and capability tiers;
`TARGET-CONTRACT.md` next to it is the porting document). A target implements a
capability or degrades it honestly — an absent one raises rather than faking a
result.

The Gambit targets need `gambit-scheme` (brew) and skip cleanly without it:

```bash
make gambitcheck              # adapter + shims on native gsi
make gambitkernel             # the booted kernel and natives (113 checks)
make gambiteval               # jolt source through the compiler, renders pinned to Chez
make gambitseed               # re-mint host/gambit/seed/ (runs on Chez, after a seed change)
make gambitweb                # => target/gambit/jolt-web.js, the browser bundle
make gambitweb PROFILE=repl   # a smaller bundle (see Build profiles below)
make gambitprofile            # gate: reduced profile runs, excluded features report
```

`make gambitweb` compiles the whole stack — kernel, seed, compiler, and a
queue-polling REPL loop (`host/gambit/repl-main.ss`) — into one self-contained
JavaScript file in about 30 seconds. The build is reproducible: the same sources
produce a byte-identical bundle. Point it at a site checkout to refresh the live
demo:

```bash
make gambitweb GAMBIT_WEB_OUT=../jolt-lang.github.io/resources/static/js/jolt-web.js
```

Some Gambit host files are generated from their Chez counterparts (for example
`records-gambit.ss` from `records.ss`); run `make gambitgen` after editing the
source, and `make gambitgencheck` gates the drift.

### Build profiles

`PROFILE` selects how much of the language a build carries.
`host/gambit/profiles.ss` lists the profiles and the optional feature groups
they are built from; `boot.ss` remains the source of load order, and a group only
names which of its files are optional.

```bash
make gambitweb PROFILE=repl    # clojure.core + compiler, no regex
make gambitweb PROFILE=full    # everything (the default)
```

Excluding a group does two things. Its files are left out, and **every name it
owned is bound to a raise that names the group** — derived by scanning the
excluded files for their definitions, so the error surface tracks the code
instead of a hand-kept list. A dropped feature reports itself:

```
user=> (re-seq #"[a-z]+" "ab cd")
java.lang.UnsupportedOperationException: jolt-re-pattern is not in this build:
the regex feature group was excluded
```

A predicate over a type the build cannot hold answers `false` rather than
raising — a value simply is not a regex — while anything that would produce or
consume that type raises. `make gambitprofile` gates both halves: the reduced
profile still runs the language, and an excluded feature names its group,
including through indirection like `clojure.string/split`.

Measured cost of each group in the bundle (raw / gzipped, and gzipped is what a
web server ships):

| group | cost | without it |
|---|---|---|
| regex | 2.4 MB / 0.4 MB | no `re-*`, no `#"..."` |
| compiler | 2.9 MB / 0.5 MB | no `eval`, no REPL, no runtime macros |
| clojure.core | 8.7 MB / 0.7 MB | the kernel alone — an embedding, not a Clojure |
| **kernel (floor)** | **19.4 MB / 2.1 MB** | the Gambit runtime plus jolt's kernel |
| full | 31.0 MB / 3.3 MB | |

The floor dominates, so a profile trades features for the last third of the
bundle: `repl` ships 27.7 MB / 3.1 MB against `full`'s 32.6 MB / 3.5 MB. Adding
a group is worth it when it is separable *and* measurable — a group worth
kilobytes is churn.

The page defines `joltQueue` and `joltOut` before loading the bundle; a Scheme
thread inside it polls the queue and hands results back, so page JavaScript never
calls into Scheme.

## Test

```bash
make test                     # the full gate
make corpus                   # conformance corpus vs the JVM-sourced spec
make unit                     # host-specific unit cases
make selfhost                 # bootstrap fixpoint (rebuild == checked-in seed)
make smoke                    # bin/jolt CLI smoke
make sci                      # load borkdude/sci's source through jolt (compat stress)
make ffi                      # the foreign-function interface, against C witnesses
make transient                # transient mutation + linear-time builds
make certify                  # JVM oracle (skips if clojure is absent)
make libconformance           # replay the downstream library suites vs recorded tallies
```

None of those measure throughput, and that is a real hole rather than an
oversight to live with: `bench/arrays` once went 5.4x slower on a codegen change
with all 88 ci targets and all 47 libraries still green — every answer was still
correct. **Run `bench/run.sh` after any change to the compiler passes, the
emitter, or the runtime's hot paths**, and read the table rather than the exit
code; the suite reports, it does not judge.

```bash
NO_JVM=1 bench/run.sh          # the suite, optimized AOT binaries
bench/run.sh sorted-access     # one benchmark, to re-check a suspicious row
ci/bench-gate.sh A B           # two compilers head to head, ratios, exits nonzero
```

Suite noise is around 1.07x per benchmark on a quiet machine, and the FIRST
benchmark of a run can be much further out than that, so a single suite run is
not evidence on its own: re-measure anything that moved by running that
benchmark alone, both before and after. A release runs `ci/bench-gate.sh`
against the previous release automatically (`.github/workflows/release.yml`),
and `publish` waits on it.

The conformance corpus (`test/chez/corpus.edn`) is a host-neutral language spec
whose expected values are sourced from reference JVM Clojure. See
[test/conformance/SPEC.md](test/conformance/SPEC.md).

Divergences from JVM Clojure are tracked, not tolerated silently:
`test/conformance/known-divergences.edn` holds both the corpus rows whose value
differs and the deliberate behavioural divergences that are not corpus rows.
`make certify` fails on a *new* (unlisted) divergence and on a stale entry, so a
behaviour change either matches the JVM or gets an entry explaining why it
doesn't.

## Documentation

User-facing documentation lives in the site repo
([jolt-lang/jolt-lang.github.io](https://github.com/jolt-lang/jolt-lang.github.io)),
not in this one: markdown under `resources/md/`, every page registered in
`resources/docpages.edn`. This repo carries only `README.md`, `CHANGELOG.md`,
this file, and `llms.txt`.
