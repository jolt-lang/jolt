# Handoff — removing Janet (Chez re-host, Phase 4 → Phase 5)

**Branch:** `spike/chez-bootstrap` · **LOCAL COMMITS ONLY — NEVER push.** This
overrides the session-close git-push protocol.
**Epic:** jolt-cf1q (Re-host Jolt on Chez, zero Janet). **Last session:** finished
self-hosting (inc8/inc9) + the portable conformance spec + fixed 4 certified bugs.

## Where we are

Done and gated (all local commits, latest `6abbea3`):
- **Self-hosting proven.** The on-Chez compiler reproduces itself (stage2==stage3,
  prelude pstage3==pstage4). `bin/joltc` + `host/chez/bootstrap.ss` build and run
  jolt with **zero Janet in the loop**, off the checked-in seed `host/chez/seed/`.
- **Correctness proven.** Both Chez corpus gates at **2534/2756**, 0 divergences.
  The corpus is a JVM-certified host-neutral spec (`test/conformance/`, see SPEC.md):
  2670 portable cases, 249 feature-gated.

**We are NOT ready to delete Janet.** Ripping it out now regresses real capability
(below). The remaining work is Phase 4 (runtime features + deployment/perf) → then
Phase 5 (jolt-cf1q.6) deletes `src/jolt/*.janet` + the Janet cross-compile.

## Hard rules (every batch)

1. **Never `git push`.** Local commits only on this branch.
2. **Gate discipline.** After any change run, with a real exit code:
   - `jpm build && janet run-tests.janet` (full Janet gate; must end `All tests passed.`)
   - `JOLT_CHEZ_ZEROJANET_CORPUS=1 janet test/chez/run-corpus-zero-janet.janet` (fast, ~2s)
   - `JOLT_CHEZ_PRELUDE_CORPUS=1 janet test/chez/run-corpus-prelude.janet` (~18min; run ISOLATED — no concurrent chez work)
   - `clojure -M test/conformance/certify.clj` (JVM cert; fails on new/stale divergence)
3. **If `clojure.core` or any seed source changes, re-mint the Chez seed**, or
   `test/chez/bootstrap-test.janet` fails (rebuilt ≠ committed seed). Re-mint:
   ```janet
   (import host/chez/driver :as d) (import host/chez/jolt-chez :as jc)
   (def ctx (d/make-ctx))
   (d/mint-chez-seed* (jc/ensure-prelude ctx)
     (d/ensure-compiler-image ctx "/tmp/stage1.ss")
     "host/chez/seed/prelude.ss" "host/chez/seed/image.ss")
   ```
   (run from repo root via a throwaway `test/chez/_x.janet`). Raise the corpus
   floors when parity rises.
4. **Use beads** (`bd ready`, `bd show <id>`, `bd update <id> --claim`, `bd close`).
   `bd remember` for persistent notes — see memories `chez-phase3-progress`,
   `conformance-spec`. Run `bd close` as its own command then verify (close-race).

## Verify current state (run these first next session)

```sh
git log --oneline -8
bin/joltc -e '(+ 1 2)'                 # => 3  (pure-Chez runtime works)
bin/joltc -e '(eval (quote (+ 1 2)))'  # => FAILS today — that's blocker #1
```

## The blockers, in priority order

### 1. jolt-r8ku — runtime eval / load-string / defmacro on the Chez spine  ⭐ DO FIRST
**Why first:** biggest functional gap, and mostly *wiring* — the compiler image is
already resident at runtime on Chez (`host/chez/compile-eval.ss` loads it). Turns
`joltc` from a one-shot `-e` into a real runtime/REPL. Unblocks ~the eval/load-string
corpus crashes too.

**Symptoms (tested):**
```
joltc -e '(eval (quote (+ 1 2)))'           -> jolt/uncompilable: special form eval
joltc -e '(load-string "(+ 1 2)")'          -> not a fn (jolt-nil)
joltc -e '(defmacro m [x] ...) (m 5)'       -> jolt/uncompilable: special form defmacro
```

**Why they punt:** `eval`/`defmacro` are in the special-symbol list, so the analyzer
treats them as special forms and the `:else` arm punts:
- `host/chez/host-contract.ss:110` and `:183` — `hc-special-symbols` / `form-special?`
  list (includes `eval`, `defmacro`).
- `jolt-core/jolt/analyzer.clj:216` `analyze-special` (`case op …`), `:279`
  `(uncompilable (str "special form " op))`.

**Approach:**
- **eval / load-string are FUNCTIONS, not special forms.** Remove `eval` from the
  special-symbol lists so it resolves as a normal var, then `def-var!` them in a Chez
  `.ss` (e.g. a new `host/chez/eval.ss` loaded by `compile-eval.ss`):
  `eval` = `(lambda (form) (jolt-compile-eval-form form (current-ns)))` — but
  `jolt-compile-eval` (compile-eval.ss) takes a SOURCE STRING and reads it; add a
  sibling that takes an already-read FORM and runs analyze→emit→eval on it.
  `load-string` = read-all + eval each form.
  - Mirror the Janet seed semantics: check `src/jolt/eval_runtime.janet` /
    `eval_base.janet` for what `eval`/`load-string` do there (the contract to match).
- **defmacro must stay special** (it defines a macro). The prelude already handles
  defmacro at build time (`host/chez/emit-image.ss` → `ei-defmacro->fn` emits a def of
  the expander fn + `mark-macro!`). For RUNTIME user defmacro, add a `defmacro` branch
  to `analyze-special` that lowers to the same shape: a `def` of the `(fn …)` expander
  + a runtime `mark-macro!` call, so the macro is usable in subsequent forms of the
  same program. Re-analysis of later forms must see the macro flag (`hc-macro?` reads
  `var-macro-table`).
- Watch `jolt-lpvi` (build-ctx vs eval-ctx isolation) — runtime eval defining vars
  must land in the user ns, not leak into the compiler image ns.

**Verify:** `joltc -e` of all three above; add cases to `test/chez/spine-test.janet`;
the eval/load-string corpus crashes (660/661/2157/2158/… per the old chez-phase2
notes) should now run; re-run both corpus gates.

### 2. jolt-byjr — concurrency (future / promise / agent / pmap / pvalues / pcalls)
`joltc -e '(deref (future (+ 1 2)))'` → `Unknown class clojure.core`. The Janet
oracle uses isolated-heap SNAPSHOT futures (the `:concurrency/snapshot` feature in
`test/conformance/profile.edn`) which neither a sync shim nor raw Chez threads
reproduce exactly. **Decision needed:** real Chez threads vs a sync shim vs snapshot
emulation. The corpus snapshot cases are oracle-coupled (JVM gives shared-heap
answers, jolt gives snapshot) — already classified as a deliberate delta, so don't
chase JVM parity; match the *jolt* documented semantics. Revisit with a Chez
thread-safety review of the RT (`host/chez/*.ss` mutable tables: var-table,
ns-registry, atoms).

### 3. jolt-cf1q.7 — host/Java interop breadth
~167 runtime crashes in the corpus are host-coupled (`profile.edn`:
`:host/jvm-interop` 174, `:host/arrays` 12, `:host/janet` 16). Java class shims,
arrays, `bean`/`proxy`/`definterface`. **Apply `vendor-before-scratch`**: for
non-trivial interop (date/time, crypto, etc.) vendor a mature Chez lib rather than
hand-rolling. Many of these are intentionally non-portable — decide per case whether
Chez should implement it or it stays a host-gated feature. `janet.*` cases are N/A on
Chez (delete from corpus in Phase 5).

### 4. jolt-cf1q.5 — Phase 4: deployment & optimization modes
The optimizing compiler is **Janet-only**: inference, direct-linking, cgen-to-C,
whole-program, and **native binary builds** (`jpm build`, the ring-app example). Chez
emits Scheme and relies on Chez's own native compiler — perf is **unmeasured**. The
Phase 5 bead literally gates on *"perf confirmed."* Tasks: benchmark the Chez path
vs the Janet/JVM baselines; decide the Chez deployment story (Chez `compile-program`/
boot file vs script); decide which Janet optimization modes (if any) must be
reproduced. This is the largest unknown — scope it before committing to deletion.

### 5. Move the test oracle off Janet, then Phase 5 (jolt-cf1q.6) delete
- Today `build/jolt` (Janet) is the oracle for `test/chez/_*.janet`, and
  `run-corpus-prelude` analyzes on Janet. The corpus is now JVM-certified, so the
  oracle can shift to **spec-corpus + JVM**. Do this swap before deleting Janet.
- Tooling still Janet: `jolt-deps`, `nrepl-server`, uberscript/DCE — port or drop.
- **Then** Phase 5: delete `src/jolt/*.janet` (reader, value layer, vars/ns, the
  tree-walking interpreter `evaluator.janet`, the Janet backend) AND
  `host/chez/emit.janet` + the Janet cross-compile in `driver.janet`. The
  tree-walking interpreter (the literal "drop the interpreter" goal) dies here.

## Sequencing

```
jolt-r8ku (eval/macros)  ─┐
jolt-byjr (concurrency)   ├─► jolt-cf1q.7 (host interop) ─► jolt-cf1q.5 (Phase 4 perf/deploy)
                          ─┘                                          │
                                          oracle → JVM/corpus ◄───────┘
                                                   │
                                                   ▼
                                      jolt-cf1q.6 (Phase 5: DELETE Janet)
```

## Key file map

| Area | File |
|---|---|
| Pure-Chez runtime CLI | `bin/joltc` → `host/chez/cli.ss` |
| Zero-Janet compile spine | `host/chez/compile-eval.ss` (`jolt-compile-eval`) |
| Host contract (special syms, resolve, macro?) | `host/chez/host-contract.ss` |
| On-Chez image/prelude emitter | `host/chez/emit-image.ss` |
| Pure-Chez self-build + seed | `host/chez/bootstrap.ss`, `host/chez/seed/` |
| Portable analyzer / backend | `jolt-core/jolt/analyzer.clj`, `…/backend_scheme.clj` |
| clojure.core overlay | `jolt-core/clojure/core/NN-*.clj` |
| Janet seed (to delete in Phase 5) | `src/jolt/*.janet` (reader, evaluator, host_iface, …) |
| Chez RT shims | `host/chez/*.ss` |
| Conformance spec | `test/conformance/` (SPEC.md, certify.clj, profile.edn, known-divergences.edn) |
| Corpus + gates | `test/chez/corpus.edn`, `run-corpus-{zero-janet,prelude}.janet`, `extract-corpus.janet` |
| Self-host gates | `test/chez/{spine,fixpoint,bootstrap,cli}-test.janet` |
