# RFC 0007 — Compilation modes and binary output

- **Status**: Draft. No code yet; this fixes the design before Phase 4 work
  (beads `jolt-cf1q.5`) starts.
- **Champions**: jolt maintainers
- **Created**: 2026-06-22

## Summary

Give jolt a `jolt build` command that emits a standalone executable, and a
three-mode model that trades dynamism for speed:

- **dev** — open/indirect linking, redefinition works, no perf focus. What
  `repl`/`-e`/`nrepl` already are.
- **release** (default for a built program) — direct-linked, closed-world,
  per-namespace inference. Fast, still a recognizable Clojure runtime.
- **optimized** — whole-program inference, `fl*`/`fx*` typed emission, Chez
  whole-program optimization. Fastest, sacrifices dynamic redefinition.

All three already have their machinery in the tree — the inference and inline
passes were ported into `jolt-core/jolt/passes/`. What is missing is (a) a code
path that writes emitted Scheme to disk and AOT-compiles it instead of
eval'ing it in process, and (b) a switch that turns the dormant passes on. This
RFC specifies both.

## Motivation

The Janet host could produce binaries (`jolt uberscript` with dead-code
elimination, `jolt cgen-build` for a single native binary). The Chez rehost
dropped that machinery with the Janet host — it was Janet-specific (IR→C made
sense when the host was Janet). On Chez the natural target is Chez's own native
compiler, so the old emitters were deleted rather than re-pointed.

The result today: `bin/joltc` only ever loads the checked-in seed and
compile-evals in process. `jolt.main/-main` dispatches `run / -M / -A / repl /
nrepl / task` and nothing else. There is no way to ship an app as a binary, and
the optimization passes are inert — `jolt.host/inline-enabled?` is a stub
returning `#f` (`host/chez/host-contract.ss:283`), so every call links
indirectly and nothing inlines. Jolt on Chez runs only in what this RFC calls
dev mode.

The passes themselves survived intact:

- `jolt/passes/types.clj` — structural collection-type inference (RFC 0005) +
  success-type checking (RFC 0006).
- `jolt/passes/inline.clj` — inline + flatten-lets + scalar-replace, already
  gated "direct-link only".
- `jolt/passes/fold.clj` — const-fold, including predicate folding.

So this is not a port of lost code. It is wiring: a build front-end, a
file-emitting back-end path, and a mode switch over passes that already exist.

## The three modes

| Mode | Linking | Inference | Redefinition | Driver |
|---|---|---|---|---|
| **dev** | indirect (var-deref per call) | off | yes | `repl`, `-e`, `nrepl`, `run` of a file by default |
| **release** | direct, closed-world | per-namespace | no (closed world) | `jolt build` default |
| **optimized** | direct + whole-program | whole-program fixpoint, `fl*`/`fx*` | no | `jolt build --opt` / `-M`-style entry |

The modes are points on one axis (how much the back end may assume is fixed),
not three code paths. Each mode is a setting of two independent knobs the passes
already understand:

- **direct-link?** — may a call to a var compile to a direct procedure
  reference instead of a `var-deref`? Enables inlining and call-site folding.
  Opt-out is per-target: a `^:redef` or `^:dynamic` var always links indirect.
- **whole-program?** — does inference see the whole reachable program at once
  (closed world), so a record param's callers in other namespaces are visible
  and its field reads specialize? Without it, inference is per-namespace and a
  cross-ns param de-specializes to `:any` (the cross-ns penalty documented in
  the `cross-ns-param-penalty` memory; declared `^RecordType` hints are the
  open-world escape hatch).

```
dev:        direct-link? = false   whole-program? = false
release:    direct-link? = true    whole-program? = false
optimized:  direct-link? = true    whole-program? = true
```

`fl*`/`fx*` typed emission (unchecked flonum/fixnum Scheme ops) rides on
optimized: only whole-program inference proves the types that make dropping the
numeric-tower dispatch sound. Release keeps the tower.

## CLI surface

```
jolt build [-m NS | FILE] [-o OUT] [--opt] [--dev]
```

- Resolves `deps.edn` exactly as `run` does (reuse `jolt.deps`).
- Default mode is **release**. `--opt` selects optimized; `--dev` builds an
  unoptimized binary (useful to ship a debuggable build, not for the REPL).
- `-o` names the output (default the entry ns / file stem).
- Output is a single executable: a Chez boot file plus the compiled program,
  launched by a thin wrapper, or a fully linked image where the platform allows.
  App libraries are baked in — no source roots needed at runtime.

Env opt-outs for the build (mirrors the Janet knobs, now keyed off the mode
rather than the run): `JOLT_NO_DIRECT_LINK` forces open linking even in a build,
`JOLT_NO_WHOLE_PROGRAM` keeps direct-link but per-namespace, `JOLT_WHOLE_PROGRAM=1`
forces whole-program. These already name the two knobs above.

## Emission pipeline

The in-process spine today (`host/chez/compile-eval.ss`) is, per form:

```
source → read → analyze (→ IR) → emit (→ Scheme string) → (eval (read …))
```

`jolt build` keeps everything up to `emit` and replaces the per-form `eval` with
accumulate-then-compile:

1. **Assemble the program.** Starting from the entry ns's `-main`, load the
   transitive `require` graph (the loader already does this) and collect every
   reachable top-level form, in dependency order, with its compile namespace.
2. **Dead-code elimination.** Re-target the uberscript DCE idea: compute
   reachability from `-main` plus non-prunable forms, drop dead `defn`/`defn-`.
   Bail to keep-all on `resolve`/`ns-resolve`/`requiring-resolve`/`find-var`/
   `intern`/`eval`/`load-string` (anything that defeats static reachability);
   keep and scan `defmethod`/`defrecord`/`extend` bodies so dispatch targets
   stay live.
3. **Emit to a file.** Run `analyze → emit` for each surviving form under the
   mode's knobs, concatenating the Scheme strings into one program source (the
   core overlay prelude first, exactly as the seed image is built today).
4. **Compile.** Feed that source to Chez `compile-program` (release) or
   `compile-whole-program` (optimized, which also lets Chez cross-module
   inline), producing a compiled object, then link a boot file / wrapper into
   the final executable.

Steps 3–4 are the only genuinely new back-end code. Step 2 is a re-port of a
deleted pass. Steps before them already run on every `joltc` invocation.

## Turning the passes on

`inline-enabled?` is the existing gate. Today `host-contract.ss` hardwires it to
`#f`. Under this RFC the build sets it (and a parallel `whole-program?` flag)
from the chosen mode before compiling, so:

- release: `inline-enabled?` → true, whole-program off. Per-ns inference and
  inlining light up; `fl*`/`fx*` stays off.
- optimized: both on; the types pass runs its whole-program fixpoint and the
  back end may emit unchecked numeric ops where a flonum/fixnum is proven.

No new pass is required to reach release — it is the ported passes, ungated.

## Staging

1. **Spike (de-risk Chez AOT).** Emit a trivial whole program to disk and prove
   `compile-program` + boot/static link yields a standalone binary that runs.
   This is the only real unknown.
2. **`jolt build` release.** Front-end + file-emitting back-end path + flip
   `inline-enabled?` from the mode. Gate against the bench/corpus suites; binary
   output must pass the corpus a `run` passes.
3. **DCE.** Re-port the reachability pass; gate with a test like the old
   `uberscript-dce` case.
4. **Optimized.** Whole-program flag, `compile-whole-program`, `fl*`/`fx*`
   emission. Gate on the bench suite (ray tracer, binary-trees) for size and
   speed vs the spike baseline.

Each stage is TDD against the existing gates (`make test`, `make corpus`, the
`bench/` programs). Modes land behind the build command, so dev — the only mode
today — is unaffected until a stage proves out.

## Open questions

- **Static vs. boot-file linking.** A fully static Chez image is the smallest,
  most portable artifact but the most work to link; a boot file plus a stub
  launcher is the easy first cut. Spike decides which step 1 targets.
- **FFI in a built binary.** `jolt.ffi` loads native libraries at runtime; a
  closed-world build still needs that to work. The build must bake the FFI
  Clojure side and keep dynamic `dlopen` at run time.
- **Macro and `eval` at runtime.** Release/optimized are closed-world, but an
  app that calls `eval`/`load-string` needs the compiler present. Either ship
  the compiler image in the binary (larger) or reject those builds (the DCE
  bail-out already detects the calls).

## Prior art in this repo

The optimization design these modes turn on is RFC 0004 (type hints), RFC 0005
(structural inference), RFC 0006 (success checking). The linking model — direct
linking as a per-unit property, `^:redef`/`^:dynamic` as the only opt-out — and
the cross-ns specialization penalty are recorded in beads memories
(`jolt-linking-model`, `cross-ns-param-penalty`). Phase 4 (`jolt-cf1q.5`) is the
tracking issue.
