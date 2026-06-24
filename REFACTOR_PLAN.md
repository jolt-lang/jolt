# Architecture refactor plan

Working doc for the `spike/arch-refactor` branch. Goal: make the codebase easier
to understand and safely modify — optimized for both human and LLM maintainers.
Derived from a four-part review (compiler core, Chez runtime, build/tooling,
top-level organization) plus call-graph analysis of the compiler.

The codebase is in better shape than its 58-file runtime suggests: `rt.ss` is a
real load-order map, the `passes/` split is clean, filename prefixes encode tier
order, and the Makefile self-documents the gate. The work below targets the few
genuine liabilities.

## The re-mint constraint shapes risk

- **Seed sources** (`jolt-core/jolt/*.clj`, `jolt-core/clojure/core/*.clj`,
  `host/chez/reader.ss`): a change needs `make remint` + the byte-fixpoint
  `make selfhost`. Behaviour-preserving refactors are *verified* by selfhost, but
  every touch costs a re-mint. Higher risk, batch them.
- **Runtime `.ss` + runtime-loaded `.clj`** (`build.ss`, `emit-image.ss`,
  `main.clj`, the dispatcher shims, …): no re-mint; verified by `make test` +
  `make shakesmoke`. Lower risk.
- **Docs / renames of non-seed files**: zero behaviour risk.

## Tier 0 — navigability (zero risk, highest LLM value, do first)

1. **`docs/MODULES.md` — the repo map.** One table: area → directory → key files →
   re-mint? Plus a per-feature "touch points" list for cross-cutting features
   (tree-shaking, direct-linking, numeric fl/fx, multimethods, the deps resolver).
   Collapses the current 3–4-source lookup ("where does X live?") into one read.
2. **`docs/rfc/README.md`** — index the 7 RFCs (number, title, status), mirroring
   `docs/spec/README.md`.
3. **Document the shipped-but-undocumented compiler features**: IR inlining and
   fl*/fx* numeric lowering (the `passes/` pipeline). A short "compilation passes"
   doc or an RFC, referenced from RFC 0007.
4. **CLAUDE.md — one "Invariants you must preserve" block**: the `var-deref`
   calling convention (the compiler is reached from `.ss` by string lookup), the
   `def-var!` native-binding pattern, and the seed dual-home shadowing rule (today
   only in `seed-overlay-registry.md`).
5. **First-line purpose comment on every `.ss`** (audit the laggards) and note the
   reader's surprising location (`host/chez/reader.ss`, Scheme, re-mint applies).

## Tier 1 — runtime structure (no re-mint; verify with `make test` + shakesmoke)

6. **Registry pattern for the core dispatchers — the biggest runtime win.**
   `jolt-pr-str`, `jolt-pr-readable`, `jolt=2`, `jolt-get`, `jolt-class`,
   `jolt-hash` are extended by `set!`-capture-and-rebind across ~8 files (e.g.
   `jolt-pr-str` is wrapped 9× in 8 files). The registry pattern already exists in
   tree (`register-str-render!`, `register-instance-check-arm!`) and is strictly
   better: type-disjoint arms gathered in one walker, load-order-independent,
   greppable. Add `register-pr-arm!` / `register-eq-arm!` / `register-get-arm!` /
   `register-hash-arm!` / `register-class-arm!`; convert the ~35 `set!` sites to
   one-line registrations. Eliminates "read 8 files to understand one dispatcher."
7. **Extract a `host/chez/dce.ss` module for tree-shaking.** The DCE logic is split
   across `emit-image.ss` (the `dce-*` helpers + record producer) and `build.ss`
   (`bld-shake-all` + the manifest splice). Pull every `dce-*` def + `bld-shake-all`
   (→ `dce-shake`) into `dce.ss`; give the record a named accessor API
   (`dce-rec-keep?`/`-fqn`/`-refs`/`-str`) instead of `(vector-ref r 0..3)`; split
   `bld-shake-all`'s five jobs (root-seed, edge-build, BFS, bail-detect, partition)
   into named steps. This is recently-added code and the loosest contract in the
   build.
8. **Tag the runtime manifest.** `bld-emit-runtime` decides "splice shaken core" /
   "drop compiler" by substring-matching `(load "…seed/prelude.ss")` strings.
   Replace the manifest with tagged entries (`(prelude)`/`(image)`/`(compile-eval)`/
   `(load path)`) and dispatch on the tag — removes a silent-failure coupling.
9. **`build-binary` options map.** It takes 8 positional args ending in two bare
   booleans (`direct-link?` `tree-shake?`); a swap compiles and misbehaves silently.
   Pass one options map from `main.clj`; new flags become additive.
10. **`dynamic-wind` the compiler-global set/reset** in `build-binary` so
    `set-optimize!`/`set-direct-link!` always revert (today they leak on a build
    error; harmless for the CLI, a trap for any in-process caller).
11. **Re-split `host-static-{,statics,objects}.ss`** on a real axis — static
    methods/fields vs host object classes — instead of the current "grew too big"
    chain (one file is 502 lines, the largest non-seed runtime file). Move the
    System-property/env plumbing out to `io.ss` / a `host-system.ss`.
12. **Consolidate the seq/transducer/parity natives** and dissolve
    `natives-parity.ss` (a literal leftovers file). One `natives-transduce.ss` for
    `td-*`/`into-xform`/`transduce`/`sequence`/`cat`; relocate parity's hash /
    macroexpand / reader-conditional pieces to their real homes.
13. **Smaller renames/moves:** `dynamic-vars.ss` (5 constants) vs `dyn-binding.ss`
    (the binding stack) — rename the former; pull the `format` engine out of
    `natives-misc.ss` into `natives-format.ss`.

## Tier 2 — compiler core (seed sources; re-mint + selfhost + corpus)

14. **Split the success-type checker out of `types.clj`** (716 lines = inference +
    checker + driver). Move the checker (`check-*`, the error-domain predicates) to
    `jolt.passes.types.check`. Highest reduction in edit blast-radius in the
    compiler — inference and checker stop being able to break each other.
15. **Decompose `infer`'s `:invoke` arm** (≈120 lines, 8 hand-coded call patterns)
    into named `infer-<pattern>` helpers (as `numeric.clj`'s `an-invoke` already
    does), and add `ty`/`nd` accessors for the positional `[type node]` tuple
    (a transposition is currently silent and type-correct).
16. **Single-source the const-keyword-lookup recognizer.** It's implemented three
    times divergently (`inline.clj`, `types.clj` ×2 arms, `backend_scheme.clj`).
    Lift one predicate into `jolt.passes.fold` (the shared-predicate home) for the
    two analysis copies; the backend's value-emission copy can stay.
17. **Co-locate the numeric op tables.** `backend_scheme.clj` (`dbl-ops`/`lng-ops`,
    the Scheme strings) and `numeric.clj` (`dbl-spec`/`lng-spec`, specializable
    names) must agree or `emit-numeric` splices a `nil` op string. Cross-link or
    share the name→op table.
18. **Collapse `local-escapes?`'s mechanical recursion** (≈12 of its arms are just
    "does any child escape" = `reduce-ir-children`); keep only the binder/lookup
    arms explicit. Removes ~50 lines and a new-op soundness hazard. Fold
    `recur-kinds`/`recur-arg-lists` (numeric) into one `recur-tails`.
19. **Touch-when-nearby:** drop the dead `form-char?` refer in `analyzer.clj:21`;
    standardize on `(get node :k)` in the pass files; promote a few inline-comment
    soundness arguments to function docstrings (`an-invoke` `:wild`, `dbl/lng-spec`).

## Sequencing

Tier 0 lands immediately (docs only). Tier 1 is the bulk of the maintainability win
and carries no re-mint — do 6 (registry) and 7–10 (DCE module + build hygiene)
first; they target the code most likely to be edited and most recently grown.
Tier 2 batches into one or two re-mints once Tier 1 is stable, each verified by
`make selfhost` + `make corpus`.

Every code change in Tiers 1–2 is behaviour-preserving and gated; no feature work.
