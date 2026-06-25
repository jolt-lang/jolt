# RFC 0002 — Reader-Conditional Feature Set

- **Status**: Superseded (2026-06-25) — jolt now includes `:clj` in the default
  set; see the note below.
- **Created**: 2026-06-10
- **Spec**: `docs/spec/02-reader.md` §2.3 S18

> **Update (2026-06-25).** The default set is now **`#{:jolt :clj :default}`** —
> `:clj` is satisfied by default. The clj ecosystem's `.cljc` libraries gate
> their host code behind `#?(:clj …)` with no `:jolt`/`:default` fallback, so
> the conformance libraries (core.cache, core.match, tick, malli, …) only load
> with `:clj` present; requiring an opt-in for each was friction with no payoff
> once jolt's `clojure.lang.*`/`java.*` emulation was broad enough to run those
> `:clj` branches. Matching is still by **clause order**, so a library can place
> a `:jolt` branch first to override. There is no `JOLT_FEATURES` environment
> variable; a loading context overrides the set at runtime with
> `reader-features-set!`. The rest of this RFC is the original (reverted)
> design.

## Summary

jolt's reader-conditional feature set is **`#{:jolt :default}`**, matched in
**clause order** (the first clause whose key the platform satisfies wins).
A loading context may opt a foreign, clj-targeted library into `:clj`
compatibility via `reader-features-set!` (or process-wide via the
`JOLT_FEATURES` environment variable). jolt does **not** satisfy `:clj` by
default.

## Background

`#?(:clj … :cljs … :default …)` selects a branch by platform feature at read
time. Until now jolt satisfied `:clj` — a compatibility shortcut inheriting
the JVM branches of `.cljc` files, on the theory that the `:clj` branch is
usually the "main" implementation. Each dialect chooses its own policy:
ClojureScript satisfies only `:cljs`; jank uses `:jank`; babashka includes
`:clj` because it genuinely is JVM-Clojure-compatible to a deep degree.

Two defects forced the decision:

1. jolt is *not* JVM-compatible where it matters for `:clj` branches: they
   contain interop (`java.util.*`, `deftype` over JVM classes) and encode
   JVM-specific *expectations* in tests (e.g. `parse-uuid`'s reference
   permissiveness), both of which jolt fails.
2. The old implementation also matched by **key priority** (`:clj` first,
   then `:default`) rather than clause order — `#?(:default 5 :clj 6)` read
   as `6`, diverging from Clojure on all platforms.

## Decision and evidence

Measured A/B over the cross-dialect clojure-test-suite (identical tree,
2026-06-10):

| Feature set | Assertions reached | Pass | Fail | Error | Clean files |
|---|---|---|---|---|---|
| `clj, default` (old) | 4967 | 4324 | 524 | 119 | 78 |
| `jolt, default` (new) | **5069** | **4470** | **518** | **81** | **86** |

The portable convention reads *more* of the suite (`:default` branches were
being shadowed by `:clj` ones jolt can't satisfy) and improves every metric:
+146 passes, −38 errors, +8 clean files. The `:clj` shortcut was a net
liability, not a compatibility win.

The opposing case — loading real-world clj-targeted libraries — is real:
SCI's `.cljc` sources select their implementation via `#?(:clj …)`/`:cljs`
with no `:jolt` branches, and fail to load under the portable set. That is a
property of the **loading context**, not of the platform: the resolution is
per-context opt-in, exactly how the SCI bootstrap now loads
(`(reader-features-set! ["jolt" "clj" "default"])`).

## Specification (normative, mirrored in spec §2.3 S18)

1. The platform feature set is implementation-defined and MUST be
   documented. jolt's is `#{:jolt :default}`.
2. Matching MUST be by clause order: the first clause whose key is in the
   feature set wins. `:default` matches on every platform.
   `#?(:default 5 :clj 6)` is `5` everywhere.
3. An unmatched conditional reads as nothing (no form); an unmatched
   `#?@(…)` splices nothing.
4. Implementations SHOULD provide a per-loading-context override so foreign
   libraries written for other dialects can be read under a compatibility
   set; using it is a deliberate, scoped decision (jolt:
   `reader-features-set!` / `JOLT_FEATURES`).

## Consequences

- Suite baselines re-measured and raised: `baseline-pass` 4324 → 4470,
  `baseline-clean-files` 78 → 86.
- Reader tests assert the portable set + clause-order semantics, plus one
  opt-in round-trip through `reader-features-set!`.
- Loading clj-ecosystem libraries via deps requires deciding their feature
  set; the deps loader currently inherits the process default — a future
  refinement is per-dependency feature configuration (filed with the deps
  work).
- `.cljc` authors targeting jolt can write `:jolt` branches and rely on
  `:default` fallbacks.
