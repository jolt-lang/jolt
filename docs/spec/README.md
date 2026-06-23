# The Clojure Language Specification (Draft)

A normative, implementation-independent specification of the Clojure
language, developed alongside jolt's self-hosted compiler and validated by
its executable conformance suites. **Why**: Clojure has no spec — every
alternative implementation re-derives semantics from the reference
implementation and folklore. See the RFC for motivation, scope, evidence
sources, and process: [`../rfc/0001-language-specification.md`](../rfc/0001-language-specification.md).

## Documents

| Doc | Content | Status |
|---|---|---|
| [`00-front-matter.md`](00-front-matter.md) | conformance terms, entry format, host classification | drafted |
| [`02-reader.md`](02-reader.md) | token grammar + reader-macro catalog | drafted |
| `01`, `04`–`08` | see chapter plan in front matter | planned |
| [`03-special-forms.md`](03-special-forms.md) | special-form catalog + normative exemplars (`if`, `let*`) | exemplars |
| [`09-core-library.md`](09-core-library.md) | per-var entry format + exemplars (`first`, `reduce`, `parse-uuid`) | exemplars |
| [`coverage.md`](coverage.md) | generated dashboard over the 694-var surface | generated |
| [`../grammar.ebnf`](../grammar.ebnf) | reader surface syntax (EBNF), companion to `02-reader.md` | reference |

Regenerate the dashboard after surface changes:
`python3 tools/spec_coverage.py` (reads `tools/clojuredocs-export.json` and
probes a working jolt checkout via `bin/joltc`).

## Current numbers (2026-06-22)

Of the 694 `clojure.core` vars in the ClojureDocs inventory, jolt interns 574.
Broadly:

- **568** implemented in jolt *and* exercised by the behavioral suites
- **6** implemented but not directly tested — each gets a test with its spec entry
- **6** portable but absent from jolt's resolvable surface (the REPL history
  vars `*1`/`*2`/`*3`/`*e`, plus `letfn`/`re-groups`, which work but aren't
  interned where `resolve` can see them) — tracked as gaps
- the rest classified host/JVM/concurrency (see the dashboard for the full
  per-var breakdown — it is the source of truth)

## How this connects to the test suites

- `test/chez/corpus.edn` — the host-neutral behavioral corpus, one row per
  case (`{:suite :label :expected :actual}`). The Chez compiler evaluates each
  case via `host/chez/run-corpus.ss` (run with `make corpus`), and
  `test/conformance/certify.clj` certifies every `:expected` against reference
  JVM Clojure (run with `make certify`). Spec entries cite these cases.
- `test/conformance/` — the certification tooling and classified divergences
  (`certify.clj`, `known-divergences.edn`); see its `README.md` and `SPEC.md`.
- `vendor/clojure-test-suite` — the cross-dialect suite (≥4081 assertions
  passing); dialect splits there are classification evidence.
- jank's per-construct corpus (`~/src/jank/compiler+runtime/test/jank`) is
  the granularity model for §2/§3 conformance.

The invariant: **every numbered normative statement names its conformance
test**, or is marked UNVERIFIED. The spec cannot drift from the
implementations that check it.
