# Jolt — Self-Hosted Clojure on Janet · Handoff

Onboarding for a fresh agent picking up this work. Read this, then
`bd prime` and `bd memories` for the live issue/knowledge state.

---

## 1. What this project is

**Jolt** is a Clojure implementation written in [Janet](https://janet-lang.org).
It has two execution paths and a **self-hosting compiler**:

- **Interpreter** — `src/jolt/evaluator.janet`. A tree-walking evaluator over
  reader forms. Always correct; the fallback for anything the compiler can't yet
  handle. The *live path* for stateful/context-modifying forms.
- **Self-hosted compiler** — the portable front end lives in **Clojure** under
  `jolt-core/jolt/` (`analyzer.clj` reader-form → host-neutral IR `ir.clj`), and a
  **Janet back end** (`src/jolt/backend.janet`) emits Janet from that IR. This is
  the default compile path. It is *self-hosted*: the compiler that compiles
  clojure.core is itself (mostly) Clojure compiled by jolt.
- **Bootstrap compiler** — `src/jolt/compiler.janet`. A Janet-native compiler used
  **only** to bootstrap-compile the kernel tier before the self-hosted analyzer
  exists. Not the main path.
- **Hybrid fallback** — the analyzer throws `:jolt/uncompilable` on forms it can't
  handle; the loader catches that and interprets instead. Three "uncompilable"
  lists are kept in sync (see compile-pipeline notes in the code).

Entry point: `src/jolt/api.janet` — `(init opts)` builds a context, installs the
host contract, and loads clojure.core (seed + overlay). `:compile? true` enables
the self-hosted pipeline; off = interpret.

---

## 2. The architecture that matters: seed + overlay

clojure.core is split into a shrinking **Janet seed** and a growing **Clojure
overlay**. This split *is* the project's main arc.

### The Janet seed — `src/jolt/core.janet`  (~3200 lines, ~365 `core-*` fns)
The irreducible base: the `core-renames` primitives the compiler emits directly
(`first`/`nth`/`conj`/`get`/…) plus genuinely host-coupled fns (atoms, vars,
transients, arrays, futures, meta, print, the persistent-collection kernel). Each
fn is `core-<name>` and interned into the `clojure.core` namespace via the
`core-bindings` table near the bottom of the file.

### The Clojure overlay — `jolt-core/clojure/core/NN-*.clj`  (loaded in order)
Plain Clojure expressing the *rest* of clojure.core on top of the seed. Tiers:

| Tier | Role |
|---|---|
| `00-syntax.clj` | control macros (`when`/`cond`/`and`/`or`/`let`/`loop`/`fn`/`for`/…), `destructure`, `when-let`. Interpreted, loaded **first** so macros exist before any code compiles. |
| `00-kernel.clj` | structural fns the analyzer itself needs (`second`/`peek`/`subvec`/`mapv`/`update`). **Bootstrap-compiled** into clojure.core before the analyzer is built. |
| `10-seq.clj` | seq-tier fns |
| `20-coll.clj` | pure collection/misc fns + the Phase-4 host-primitive wrappers |
| `30-macros.clj` | the remaining user-facing macros |
| `40-lazy.clj` | lazy seq transformers (Phase 5) |

Loader: `api.janet` → `load-core-overlay!` / `core-tiers`. Sources are read **fresh
from disk** at startup when running from the repo (`stdlib_embed.janet` collects
`jolt-core/` and `src/jolt/clojure/`), so editing a `.clj` tier takes effect with
no rebuild. (A `jpm build` bakes them into the image; that can go stale — tests
run from source.)

### The host contract — `src/jolt/host_iface.janet`  (ns `jolt.host`)
The portability seam. jolt-core (analyzer/IR/overlay) calls **only** `jolt.host`
fns, never Janet directly. Originally compiler-facing (`form-sym?`, `form-list?`,
`resolve-global`, …). Phase 4 added the first runtime primitive: **`ref-put!`**
(set/remove a key on a mutable reference cell) — the minimal mutation kernel the
overlay uses for atom watches/validators, volatiles, and `aset`. The overlay calls
these qualified, e.g. `(jolt.host/ref-put! ...)`.

---

## 3. The migration epic (`jolt-1j0`) — essentially COMPLETE

**Goal:** shrink the Janet seed to `core-renames` + genuinely host-coupled fns;
express everything else (pure fns, macros, lazy machinery) in the self-hosted
overlay. Started at core.janet = 4145 lines / 421 `core-*` fns.

**Phases (all done):**
- **Phase 1** — compiler-dependency kernel tier. (Was found already essentially
  complete — the analyzer needs nothing beyond the kernel tier + atom/swap!/reset!.)
- **Phase 2** — ~193 movable pure-eager fns → overlay.
- **Phase 3** (`jolt-461`, closed) — ~46 core macros → `defmacro` in the overlay.
  Last one was `when-let`.
- **Phase 4** (`jolt-ldf`, closed) — host-coupled fns. ~27 moved over the `ref-put!`
  primitive + pure composition (vary-meta, reduce-kv, ex-info accessors,
  tagged-value predicates, atom peripheral ops, volatiles, future predicates,
  ns-name, array reads/aset). The rest stay native by design (atom/swap!/reset!/
  deref, transients, var cells, meta tables, namespace, constructors, proxy,
  print dispatch).
- **Phase 5** (`jolt-c09`, closed) — true laziness. Lazy seq generators +
  transformers, the `40-lazy.clj` tier, realization-boundary discipline. See
  `phase-5.md` for the full implementation + testing plan and what landed
  (representation decision = **Option B / hybrid**: lazy over lazy input, eager
  representation-preserving over concrete finite collections).

The epic issue (`jolt-1j0`) may still read IN_PROGRESS — verify with `bd show
jolt-1j0` and close it if all five phase issues are closed and gates are green.

**Where to confirm current state:** `phase-5.md` (detailed, step-annotated),
`jolt-core/clojure/core/MIGRATION.md` (the worklist + bucket classification), and
the bd memories `phase4-host-primitive-pattern` / `phase4-movable-classification`.

---

## 4. Representation facts you MUST know (the trap floor)

Jolt's value/form representations bite every time. The essentials:

- **Reader forms:** a *call/list* `(f x)` is a Janet **array**; a *vector literal*
  `[a b]` is a Janet **tuple**; a *map literal* `{..}` is a Janet **struct** (or a
  **phm** when a key/val is nil or a key is a collection). A **symbol** is a struct
  `{:jolt/type :symbol :ns _ :name _}`; a **keyword** is a Janet keyword.
- **Runtime values:** vectors are persistent-vectors (`pvec`, tagged tables) or
  tuples; lists/seq-results are Janet arrays or `plist`; sets are `phs`; maps are
  struct-or-phm. `vector?` is true for tuple **and** pvec. `seq?` is arrays/plists/
  lazy-seqs (not vectors). In `JOLT_MUTABLE` builds vectors are plain arrays — so
  `vector?`/`array?` collapse (this is why `ifn?` couldn't move — see `jolt-1vx`).
- **Tagged values** carry their kind in `:jolt/type` (atoms, volatiles, delays,
  futures, ex-info, reader-conditional, lazy-seq) or `:jolt/deftype` (records). The
  overlay can **read** these via `(get x :jolt/type)` / `(get x :field)` — `get`
  returns nil on non-tables, no error. It **cannot construct** them without a host
  primitive. This is the Phase-4 movability rule: accessors/predicates move,
  constructors stay.
- **`canon-key`** (core.janet ~line 51) is the canonical-hashing kernel of the
  whole persistent-collection system — woven into `get`/`count`/`contains?`. This
  is why transients are irreducibly host.
- **LazySeq** (`phm.janet`): `@{:jolt/type :jolt/lazy-seq :fn thunk ...}`; thunk →
  `nil` or `[first rest-thunk]`; `realize-ls` memoizes with a `:jolt/pending` guard
  that makes self-referential seqs (`lazy-cat` fib) work.

### Macro/overlay-authoring gotchas (learned the hard way)
- Build binding/forms via syntax-quote templates `` `[~@xs] `` (a tuple form), not
  `conj`/`list` (those make pvecs/plists the analyzer/compiler rejects).
- A fresh symbol inside a macro body: `(symbol (str (gensym)))` — a bare `(gensym)`
  returns a *Janet* symbol the destructurer rejects.
- A `.clj` tier is **Clojure** (`;;` comments). A `.janet` test/spec is **Janet**
  (`#` comments — `;` is splice!). Mixing them is a frequent self-inflicted error.
- In a tier, a fn must be defined *after* the macro it uses is defined; use `def` +
  `fn*` if you need it before `defn` exists (as `destructure` does in 00-syntax).

---

## 5. Build, run, and the test gate

No special build needed to run from source — Janet reads the tiers off disk.

```bash
# Smoke
janet -e '(use ./src/jolt/api) (pp (eval-string (init) "(+ 1 2)"))'

# THE GATE — run all of these green before committing any core change:
janet test/integration/conformance-test.janet        # 229 cases × 3 modes (interpret/compile/self-host)
janet test/integration/bootstrap-fixpoint-test.janet  # stage1 == stage2 == stage3
janet test/integration/self-host-test.janet
janet test/integration/sci-bootstrap-test.janet       # loads vendored SCI through jolt
janet test/integration/clojure-test-suite-test.janet  # battery; baseline-pass=3971, clean-files=45
for f in test/spec/*.janet test/unit/*.janet; do janet "$f"; done   # all must exit 0
```

- **Specs** (`test/spec/*-spec.janet`) — data-driven `defspec` tables, behavioral.
- **Conformance** — real-Clojure-semantics assertions, run in all 3 execution modes.
- **clojure-test-suite** — runs `lread/clojure-test-suite` (from `~/src/clojure-
  test-suite`) via a per-file **subprocess under a 6 s deadline** (infinite seqs are
  CPU-bound and uninterruptible in-process — never probe them inline). Skips if the
  suite dir is absent. Raise `baseline-pass` as jolt improves; never lower it.
- **Laziness** must be tested via the deadlined subprocess harness, not in-process.

### Per-change workflow (mirror this)
1. Make a small, single-purpose change.
2. Add/extend spec + (for subtle behavior) 3-mode conformance cases.
3. Run the full gate. Commit only if green.
4. `git push` (the project's session-close protocol requires pushed work).

---

## 6. Conventions

- **Issue tracker: beads (`bd`)**, not TodoWrite/markdown. `bd ready`, `bd show
  <id>`, `bd create`, `bd update <id> --status=…`, `bd close`. `bd remember
  --key … "…"` for durable knowledge; `bd memories` to recall. The `.beads/`
  dir is git-ignored and auto-synced — don't `git add` it.
- **Commits/PRs**: terse, factual, human-dev tone. No marketing words, no emoji, no
  "This commit…". Say what changed and why it matters.
- **Branch**: work happens on `compiler-research` (main is `main`).
- Don't lower `baseline-pass`. If a moved fn surfaces a latent bug, fix it to match
  Clojure and add a regression test rather than preserving the bug (this happened
  with `reduce-kv` on vectors and `ifn?` on lists).

---

## 7. Where to pick up

The migration epic is functionally complete; the seed is at its intended floor
(core-renames + genuinely host-coupled). Candidate next work:

- **Close out `jolt-1j0`** if not already closed (verify all phase issues closed,
  gates green).
- **`jolt-1vx`** (filed) — `ifn?` is wrongly true for lists; move to overlay but
  it's representation-mode-sensitive (`JOLT_MUTABLE`). Needs both-mode verification.
- **Phase-5 loose ends** (see `phase-5.md`): a few transformers were kept eager or
  reverted due to compile-mode `~@`/defrecord splice issues (`partition-by`,
  `dedupe`, `tree-seq`, lazy `mapcat`). Re-verify the ~9 previously-timing-out suite
  files actually stopped timing out. The Step 4 "apply/`~@` over lazy" fix would
  unblock the reverted lazy `mapcat`.
- **Bigger lifts not attempted** (deliberately): the `print-method`/`pr-str`
  dispatch machinery and the `deftype`/`defrecord`/`defprotocol`/multimethod surface
  — both substantial and host-entangled.
- **Open issues**: `bd ready` for the current actionable list (CI, edn/walk/zip
  stdlib, `into #{}` bug, recur-into-variadic hang, real futures via ev/thread,
  etc. — these predate the migration).

### Map of the territory
- `src/jolt/core.janet` — the Janet seed (`core-*` fns, `core-bindings`, `core-renames`).
- `src/jolt/evaluator.janet` — interpreter. `src/jolt/compiler.janet` — bootstrap compiler.
- `src/jolt/backend.janet` — IR → Janet emitter. `src/jolt/host_iface.janet` — `jolt.host`.
- `src/jolt/phm.janet` — persistent maps/sets/vectors + LazySeq.
- `src/jolt/api.janet` — context init + tier loading. `src/jolt/reader.janet` — reader.
- `jolt-core/jolt/{analyzer,ir}.clj` — portable self-hosted front end.
- `jolt-core/clojure/core/*.clj` — the overlay tiers + `MIGRATION.md`.
- `phase-5.md` — the laziness plan, annotated with what landed.
- `CLAUDE.md` / `AGENTS.md` — project agent instructions (beads, session-close).
