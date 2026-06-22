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

No build step — `bin/joltc` runs off the checked-in seed (`host/chez/seed/`).
The gate is pure Chez (+ Clojure for the JVM oracle).

```bash
bin/joltc -e EXPR                          # run a Clojure expression on Chez
make test                                  # FULL gate (self-host + corpus + unit + smoke + certify)
make corpus                                # conformance corpus vs the JVM-sourced spec (floor 2678)
make unit                                  # host-specific unit cases (test/chez/unit.edn)
make selfhost                              # bootstrap fixpoint (rebuild == checked-in seed)
make certify                               # JVM oracle (skips if clojure absent)
chez --script host/chez/run-corpus.ss      # the corpus gate directly; JOLT_CORPUS_LIMIT=N for a fast stride
make remint                                # re-mint the seed after a seed-source change
```

**Re-mint after changing a seed source.** The reader (`host/chez/reader.ss`), the
analyzer/IR/backend (`jolt-core/jolt/*.clj`), or the `clojure.core` overlay
(`jolt-core/clojure/core/*.clj`) are baked into the seed — change one and run
`make remint` (iterates `host/chez/bootstrap.ss` to a byte-fixpoint) or `make
selfhost` fails. Runtime-only `host/chez/*.ss` shims do NOT need a re-mint.

**Run the gate with a REAL exit code.** `make test > /tmp/gate.out 2>&1; echo
"EXIT: $?"` — the final `OK: all gates passed` line must be present. CI
(`.github/workflows/tests.yml`) runs `make test` on every push/PR.

## Architecture Overview

Clojure on Chez Scheme — the sole substrate. A small Chez runtime
(`host/chez/*.ss`: value model, persistent collections, seqs, vars/ns, host
interop) hosts a portable Clojure overlay (`jolt-core/`): the
reader/analyzer/IR/backend (`jolt-core/jolt/`) and `clojure.core` in
dependency-ordered tiers (`jolt-core/clojure/core/NN-*.clj`, loaded in order:
00-syntax, 00-kernel, 10-seq, 20-coll, 25-sorted, 30-macros, 40-lazy, 50-io).
The stdlib namespaces (`clojure.string`/`set`/`walk`/`edn`/`pprint`/…) are
portable Clojure under `src/jolt/clojure/`.

`bin/joltc` (`host/chez/cli.ss`) loads the checked-in seed
(`host/chez/seed/{prelude,image}.ss`) + the spine and compiles+evals on Chez
(read → analyze → IR → emit → eval). `host/chez/bootstrap.ss` rebuilds that seed
from source on pure Chez; the build is a self-hosting fixpoint (a rebuild
reproduces the checked-in seed byte-for-byte — `make selfhost`). The correctness
oracle is the JVM-sourced conformance corpus (`test/chez/corpus.edn`,
`test/conformance/`).

Issue tracking and design notes live in beads (`bd prime`, `bd memories`).

## Conventions & Patterns

- **A tier may only use macros from tiers that load before it.** Compile mode
  expands macros at tier LOAD, so an `if-let` (30-macros) inside a 20-coll fn
  breaks compiled init even though it passes when expanded lazily. Same ordering
  for expander-called fns (empty?/keys/vals live in 00-syntax).
- **Never read your own wrapper's fields with `get`** in attached-ops values
  (sorted colls): `get` on the wrapper IS the dispatched lookup and recurses
  forever. Use `jolt.host/ref-get`.
- **Map literals with `:jolt/type` as a key** parse as tagged reader forms —
  don't tag overlay value maps in source.
- **Fix latent bugs to match Clojure** rather than preserving them, with a
  regression case. Match the JVM (or provide a superset); the JVM-sourced corpus
  is the contract.
- **Gate every change**: `make test` with a real exit code (self-host fixpoint,
  corpus floor, unit, cli smoke, certify). Re-mint if a seed source changed.
