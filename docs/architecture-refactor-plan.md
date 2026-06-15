# Architecture Refactor Plan

Goal: make the jolt codebase easier to understand and safe to change — for human
maintainers and, specifically, for LLM agents. An agent should be able to find
where a feature lives, see its related code, and make a change without scanning
3000-line files or keeping invisible global state in mind.

This plan synthesizes a six-part architectural review (one reviewer per
subsystem). It is organized as **independent, gate-validated phases**, ordered by
value-to-risk. Each phase is a PR-sized unit. Nothing here changes behavior; it is
pure reorganization plus a small set of dead-code/bug deletions.

## Non-negotiable constraints (apply to every phase)

- **Every phase passes the full gate**: `jpm test` green (conformance ×3 modes,
  `clojure-test-suite` ≥ baseline, bench back-to-back vs main, real exit code).
  `rm -rf build && jpm clean` before trusting the binary.
- **Load order is load-bearing.** The Janet seed (`src/jolt/*.janet`) and the
  Clojure overlay tiers (`jolt-core/clojure/core/NN-*.clj`) load in a fixed order;
  a module may only use what loads before it. Splitting a file must preserve the
  net load order (a new file is imported where the old code ran).
- **Seed-tier discipline** (the jolt-tzo rules in CLAUDE.md): nothing the
  analyzer/IR use may move below the kernel tier; a tier may only use macros from
  earlier tiers; expander-called fns stay in 00-syntax.
- **One concern per PR.** Do not combine a file split with a behavior fix.
  Dead-code/bug deletions (Phase 0) land first and separately so later diffs are
  pure moves.

## Guiding principles (the target state)

1. **One obvious home per feature.** Adding a `.someMethod` interop shim, a reader
   macro, or a clojure.core fn should have exactly one file an agent edits.
2. **Files map to concerns, not to history.** No 3000-line grab-bags; no module
   whose name lies about its contents (`javatime.janet` is 80% not java.time;
   `phm.janet` contains LazySeq; `types.janet` holds seven concerns).
3. **Make implicit contracts explicit and checked.** The seed↔overlay split, the
   ctx-shaping env-knob list, and the IR op set are tribal knowledge today; turn
   each into a single source of truth with a drift check.
4. **No copy-paste dispatch.** Where the same op-set / member-dispatch / cache
   dance is hand-written N times, extract one combinator.

---

## Phase 0 — Dead code & concrete bugs (low risk, do first)

Pure deletions and small fixes, each with a regression row. These remove
*actively misleading* code (comments that contradict behavior) before any moves.

| Item | Location | Note |
|---|---|---|
| `find` defined twice; the dead copy returns a plain vector and is *wrong* | `jolt-core/clojure/core/20-coll.clj:347-349` | live def at :787 returns a real map-entry; delete the dead one |
| `core-satisfies?` always returns `false` (latent bug + misleading comment) | `src/jolt/core.janet:2412` | either implement over the protocol registry or document why inert; fix the `eval-list` comment that claims it's an overlay fn |
| `File.toURL` stores `:url` but every `:jolt/url` method reads `:spec` | `src/jolt/javatime.janet:636` | broken shim; use `:spec`, add a spec row |
| `core-type->str` — zero references | `src/jolt/core.janet:2416` | delete defn + binding |
| `core-resolve` — unreachable (overridden by `install-stateful-fns!`) | `src/jolt/core.janet:2365` | delete defn + binding; fix stale comment |
| `mark-hint` — unreachable | `jolt-core/jolt/passes.clj:836` | delete |
| `pad2` defined twice | `src/jolt/javatime.janet:41,522` | keep one |
| `phs-to-struct`, `shape-vals`, `ns-imports-fn` — zero references | `phm.janet:302`, `types.janet:655`, `types.janet:527` | delete unless reserved API |
| `pl-rest` no-op `(if (plist? r) r r)` | `src/jolt/plist.janet:62` | collapse / fix; regression check |
| `read-quote` unused `pos` param | `src/jolt/reader.janet:608` | drop |
| Stale/contradictory comments (`extend`, orphan `;; trampoline:` / `;; rand-int:` headers, migration breadcrumbs) | `30-macros.clj:402-404`, `20-coll.clj:558-560,780,790` | sweep |
| **`:map-shapes?` missing from the deps-image cache key** (possible stale-image correctness gap) | `src/jolt/main.janet:440-448` | add to key; confirm with a test |

---

## Phase 1 — Extract the host-interop subsystem (highest value)

**Problem (corroborated by 3 reviewers).** The JVM-emulation shim layer is the
single worst sprawl, and it is exactly where the recent hiccup/markdown/malli work
landed ad hoc. A single class is split across up to three files:

- registry *machinery* (`class-statics`/`tagged-methods`/`class-ctors` +
  `register-*!`) lives in `evaluator.janet:624-651`;
- four static tables (`Math`/`Thread`/`System`/`Long`/`Number`) are hardcoded in
  `evaluator.janet:537-619` and dispatched *outside* the registry in `resolve-sym`
  (780-794); instance-method tables (`string/number/object-methods`, 698-775) are
  likewise inline;
- the bulk of the shims are in `javatime.janet` (674 lines, ~80% not java.time:
  java.io/util/net/nio/sql/text/math all in one `install-io!`);
- `api.janet:18-56` wires the malli statics + `set-coll-interop!` as load-time
  side effects in the public-API module;
- `core.janet` holds `File`/JDBC constructors; `types.janet` holds the `:jolt/inst`
  representation.

**Target.** A `src/jolt/interop/` directory, one mechanism file + one file per JDK
area, each owning a class's *full* surface (ctor + statics + methods + `instance?`
predicate + canonical name) and exposing one `install!`:

```
src/jolt/interop/
  registry.janet   # MOVED from evaluator: class-statics/tagged-methods/class-ctors,
                   #   register-*!, canonical-names, value-overrides, instance? registry
  coerce.janet     # shared: chr, char->byte, render-piece/writer-piece, pad2,
                   #   the date-format token walker (kills the pad2/render-piece dups)
  java_time.janet  java_io.janet  java_net.janet  java_util.janet
  java_lang.janet  # absorbs evaluator's inline Math/Thread/System/Long/Number +
                   #   string/number/object method tables — the registry becomes the
                   #   ONLY static/method-dispatch mechanism
  jdbc.janet
  install.janet    # (install-all! ctx) — the ONE place api.janet calls (replaces api 18-56)
```

`evaluator.janet`'s `resolve-sym` and the dot-dispatch consult only the registry.
`regex.janet` and `async.janet` stay put — they are engines/library-ports consumed
by interop, not shims.

**Cheap 80% if the full split is too big for one PR:** rename
`javatime.janet`→`host_interop.janet`, pull the four inline static tables + three
method tables out of `evaluator.janet` into it, move `api.janet:18-56` into it,
dedup `pad2`/`chr`/`render-piece`. Collapses the scatter from 5 files to 2.

**Risk:** medium — touches the hot dot-dispatch path; load order must keep the
registry available before any `install!`. Validate with `host-interop-spec` +
the hiccup/markdown/malli example apps + full gate.

---

## Phase 2 — Decompose the god-files

The three biggest interpreter/runtime files are the top LLM-navigability tax.
Split along the cohesive clusters the review mapped (line ranges in the review
notes). Each split is a mechanical move + import; behavior unchanged.

### 2a. `evaluator.janet` (2681 → ~5 files)
- `special_forms.janet` — explode the **680-line `eval-list`** (1921-2599) into
  named `eval-<form>` fns (`eval-fn*`, `eval-let*`, `eval-try`, `eval-dot`, …) +
  a dispatch table. This is the highest-leverage single change: today "where is
  `try` handled" means scanning a 680-line body.
- `resolution.janet` — `resolve-sym`/`resolve-var`/binding/destructuring.
- `ns_loader.janet` — module/bridge plumbing + require/in-ns/use/import/refer.
- `runtime_registration.janet` — protocol/defmulti/deftype/reify setup +
  `install-stateful-fns!` (1506-1909) split into per-domain registration fns.
- (host-interop already left in Phase 1.)
- **De-dup the two `.method` dot arms** (2456-2507 vs 2512-2550): extract one
  `dispatch-member [target field-name args]` used by both. They are copy-pasted
  today and must be hand-synced on every interop change.

### 2b. `core.janet` (3017 → ~6 leaf files)
Clusters are already sequential and mostly leaf — low risk:
`core-types` (predicates/eq/arith/bits), `core-coll` (assoc/seq/transducers/lazy/
transients), `core-print` (pr-str/str), `core-io`, `core-refs` (atoms/vars/delays/
arrays/type), `core-bindings` (the table + `init-core!` + a *labeled* stub
section). The `core-bindings` table stays the single registration point.

### 2c. `passes.clj` (1487 → 3-4 files behind one façade)
The Louvain communities and the cycle analysis agree: the IR-rewriting passes and
the type subsystem are weakly coupled (only `run-passes` + `dirty` shared).
- `jolt/passes.clj` (keep, ~150 lines): `run-passes` + shared state + re-exports —
  the only file the back end imports.
- `jolt/passes/fold.clj` — const-fold (always-on).
- `jolt/passes/inline.clj` — inline + flatten-lets + scalar-replace (share the
  alpha-rename invariant).
- `jolt/passes/types.clj` — type lattice + `infer` + success-checker + driver API
  (kept as one module to respect the inference↔checker cycle; no `declare`
  gymnastics). Extract the `infer` `:invoke` arm (1051-1160) into per-shape
  helpers regardless of the split.
- Fix the **stale ns docstring** (lists 4 passes, omits the type system that is
  >50% of the file).

---

## Phase 3 — Kill the structural duplication

### 3a. One IR op-walk combinator
There are **11 hand-written recursive walks** over the IR op set (const-fold,
inline, subst, body-closed?, pure?, flatten-lets, local-escapes?, subst-lookup,
scalar-replace, infer, backend `emit`). Adding an IR op means editing all of them,
and the "unknown ops pass through" promise is only partly kept. Introduce one
`map-ir-children` (in `ir.clj` or `jolt/ir/walk.clj`) that knows each op's child
positions; rewrite the walks as `(map-ir-children f node)` + their few specials.
Collapses ~400 lines and makes adding an op a one-site change.

### 3b. IR shape hygiene
`:let`/`:loop` bindings are `[name init]` vectors, `:map` pairs are `[k v]`, `:fn`
arities are maps; optional keys are present-or-absent, which is *why* the backend
needs `norm-node`/phm-densification everywhere. Make `ir.clj` constructors always
emit optional keys (nil-valued); delete the defensive `norm-node` calls.

### 3c. Smaller dedups
- `read-delimited` driver for the 4 collection readers (`read-list/vec/set/kvs`)
  in `reader.janet` (the skip/splice logic already drifted once).
- `bucket-index-of` for the 5 stride-2 bucket scans in `phm.janet`.
- Unify the kw-lookup head-matching reimplemented in analyzer/passes/backend
  (a kw-lookup the inference tags but the backend doesn't specialize is a silent
  miss) behind one shared predicate + fn-name table.

---

## Phase 4 — Config & caching coherence

### 4a. Lift run-mode/config resolution into `config.janet`
`config.janet` is 15 lines (one constant); meanwhile `main.janet:585-630` holds
~45 lines of pure env-knob policy (`open-mode?`/`dl`/`optimize?`/shape/whole-program
gates) that can't be unit-tested without the CLI and that the cache keys need to
share. Promote it: `config/resolve-run-mode [argv env] → {:direct-linking? :inline?
:shapes? :map-shapes? :whole-program? :direct-link-auto?}`, plus a canonical
`ctx-shaping-env-vars` list. `main` shrinks to: parse argv → resolve → install →
dispatch.

### 4b. One ctx-image module, two policies
`init-cached` (core image, api.janet) and the deps-image (main.janet) duplicate the
fork→validate→reinstall-print-cb→atomic-publish dance, each with a **hand-built
positional `%q|...` cache key** that silently misaligns if a knob is added.
Extract `ctx_image.janet`: `load-image [path predicate]`, `save-image [ctx path]`,
`ctx-cache-key [pairs]` (derived from `ctx-shaping-env-vars`, impossible to
misalign). Both callers differ only in the validity predicate (source-fingerprint
vs mtime-manifest). This also closes the cache-key footgun for good and resolves
`aot.janet`'s status (fold its marshal helpers in, or delete it — it is exercised
only by one integration test and is on no run path).

---

## Phase 5 — Data structures, reader, types, and the seed↔overlay index

### 5a. `types.janet` (699 → 3-4 files)
Seven concerns share a generic name. Split: `value.janet` (char/inst/uuid),
keep the cohesive `var`+`ns`+`ctx` core, `protocols.janet` (the protocol/type
registry, `type-satisfies?`), `records.janet` (shape-records). Minimum win:
extract the protocol registry + shape-records (the two least "types"-like).

### 5b. `phm.janet` (303 → 3 files)
Split out `lazyseq.janet` (LazySeq has nothing to do with hash maps — pure
mislabel) and `phs.janet` (PersistentHashSet). `phm.janet` keeps the map.

### 5c. Internal collection protocol
phm/pv/plist each re-implement count/seq/conj/predicate/meta with diverging
naming, and `core.janet` dispatches on them with giant per-op `cond`s (every new
structure edits every cond). A minimal `:jolt/type`-keyed vtable (`-count -seq
-conj`) lets core dispatch once. Normalize the trio's naming (`pv` vs `pvec`,
`->`/`-to-`, `EMPTY` vs `EMPTY-PLIST`).

### 5d. Make the seed↔overlay boundary self-documenting
Five fns exist in both the seed (`core-X`) and overlay (`X`) as dispatch twins with
no cross-reference; nothing indexes which copy is authoritative (`transduce` is
overlay-public but `into` is seed-public — surprising and undocumented). Add:
- a generated `REGISTRY` (name → home → public-source → seed-twin? → dispatch-only?)
  with a build-time drift check;
- `SEED-TWIN:` provenance comments on each twin (greppable);
- a distinct prefix for dispatch-only seed helpers so they don't read like public
  ones. Mirrors the existing "delete the seed defn + binding in the same change"
  rule.

### 5e. Boundary doc-comments
Add rep-vs-API pointers between the data structures and `core.janet` (e.g. "the
persistent vector trie lives in `pv.janet`; Clojure-facing vector ops and
tuple/pvec polymorphism live in `core.janet`"), so an agent grepping "vector" in
core knows where the representation is.

---

## Sequencing & rationale

```
Phase 0  (dead code/bugs)         — independent, do first, unblocks clean diffs
Phase 1  (host-interop extract)   — highest value; isolates the recent shim sprawl
Phase 2  (god-file splits)        — biggest navigability win; 2a/2b/2c independent
Phase 3  (op-walk + IR hygiene)   — removes the largest single duplication tax
Phase 4  (config + caching)       — fixes the cache-key footgun; makes boot legible
Phase 5  (data/reader/types + index) — finishes the "one home per feature" goal
```

Phases are independent; within a phase the sub-items (2a/2b/2c, etc.) are separate
PRs. Highest LLM-friendliness per unit risk: **Phase 1**, **2a (`eval-list` split)**,
**3a (op-walk combinator)**, and **5d (seed↔overlay index)**.

Each PR: one concern, full gate green, no behavior change (Phase 0 deletions carry
a regression row).
