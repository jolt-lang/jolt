# Project Instructions for AI Agents

This file provides instructions and context for AI coding agents working on this project.

<!-- BEGIN BEADS INTEGRATION v:1 profile:minimal hash:7510c1e2 -->
## Beads Issue Tracker

This project uses **bd (beads)** for issue tracking. Run `bd prime` to see full workflow context and commands.

### Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work
bd close <id>         # Complete work
```

### Rules

- Use `bd` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `bd prime` for detailed command reference and session close protocol
- Use `bd remember` for persistent knowledge — do NOT use MEMORY.md files

**Architecture in one line:** issues live in a local Dolt DB; sync uses `refs/dolt/data` on your git remote; `.beads/issues.jsonl` is a passive export. See https://github.com/gastownhall/beads/blob/main/docs/SYNC_CONCEPTS.md for details and anti-patterns.

## Session Completion

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds
<!-- END BEADS INTEGRATION -->


## Build & Test

```bash
jpm build              # build/jolt (one binary; ctx baked at build time)
jpm build; janet run-tests.janet   # FULL gate, PARALLEL (across CPU workers) — build first so binary-spawning tests don't skip; JOLT_TEST_JOBS overrides worker count, -v shows all output
jpm test               # FULL gate, serial (same file set; slower — recursive over test/)
janet test/spec/<f>.janet            # one spec file
janet test/integration/conformance-test.janet   # 3-mode conformance (interpret/compile/self-host)
JOLT_BENCH=1 janet test/bench/core-bench.janet   # bench (opt-in; skipped in the gate) — back-to-back vs main, never absolute
```

**Run the gate with a REAL exit code.** `jpm test | grep ...` reports grep's
exit, not jpm's — this once shipped masked spec failures. Correct form:

```bash
jpm test > /tmp/gate.out 2>&1; echo "EXIT: $?"
grep -E "non-zero exit|All tests" /tmp/gate.out
```

The literal `All tests passed.` line must be present. CI (.github/workflows/
tests.yml) runs the same gate on every push/PR.

`jpm build` output goes STALE silently — `rm -rf build && jpm clean` before
trusting the binary, or test from source (authoritative).

## Architecture Overview

Clojure on Janet. A shrinking Janet seed (`src/jolt/*.janet`: reader, value
layer, vars/ns, evaluator, the self-hosted pipeline's back end) hosts a
Clojure overlay (`jolt-core/`): the analyzer/IR (`jolt-core/jolt/`) and
`clojure.core` in dependency-ordered tiers (`jolt-core/clojure/core/NN-*.clj`,
loaded in order: 00-syntax, 00-kernel (bootstrap-compiled), 10-seq, 20-coll,
25-sorted, 30-macros, 40-lazy, 50-io). Compile is the default path (analyzer
-> IR -> Janet bytecode, hybrid with interpreter fallback); `JOLT_INTERPRET=1`
forces the tree-walking interpreter, `JOLT_INTERPRET_MACROS=1` additionally
keeps macro expanders interpreted (the pure oracle). `api/init-cached` serves
a disk-cached ctx image (~5ms vs ~2.4s); the cache key fingerprints sources +
env knobs — add any NEW ctx-shaping env var to `image-cache-path` in
api.janet or tests will see stale language behavior.

Issue tracking and design notes live in beads (`bd prime`, `bd memories`).

## Conventions & Patterns

Porting seed fns to the overlay (the jolt-tzo shrink ladder) — traps that have
each bitten at least once:

- **Verify leaf-ness first**: grep ALL `src/jolt/*.janet` for the `core-X`
  name (defn + core-bindings entry only), and check that tiers loading
  EARLIER than the target tier don't call it. Nothing the analyzer/ir use may
  move below the kernel tier.
- **Delete the seed defn + binding in the same change.** A leftover stub
  breaks direct-linked self-recursion: the overlay fn's recursive call binds
  to the STUB's root at compile time (line-seq once truncated after one
  element this way).
- **A tier may only use macros from tiers that load before it.** Compile mode
  expands macros at tier LOAD; the interpreter expands lazily — so an
  if-let (30-macros) inside a 20-coll fn passes every interpreted test and
  breaks compiled init.
- **Never read your own wrapper's fields with `get`** in attached-ops values
  (sorted colls): `get` on the wrapper IS the dispatched lookup and recurses
  forever. Use `jolt.host/ref-get`.
- **Map literals with `:jolt/type` as a key** parse as tagged reader forms —
  don't tag overlay value maps in source.
- **Expander-called fns live in 00-syntax** (empty?/keys/vals): expansion
  first happens during the kernel-tier compile, before later tiers exist.
  Early defns and expanders are interpreted during init and recompiled by the
  staged passes (recompile-defns!/recompile-macros!) once the analyzer is
  alive.
- **Fix latent bugs to match Clojure** rather than preserving them, with a
  regression spec row. Canonical Clojure definitions are preferred verbatim.
- **Gate every batch**: conformance x3 modes, suite >= baseline
  (clojure-test-suite-test.janet — raise the baseline when it rises), full
  jpm test with a real exit code, bench back-to-back vs main.
