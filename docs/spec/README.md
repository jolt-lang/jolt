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
| `01-evaluation.md` … `08-macros.md` | see chapter plan in front matter | planned |
| [`03-special-forms.md`](03-special-forms.md) | special-form catalog + normative exemplars (`if`, `let*`) | exemplars |
| [`09-core-library.md`](09-core-library.md) | per-var entry format + exemplars (`first`, `reduce`, `parse-uuid`) | exemplars |
| [`coverage.md`](coverage.md) | generated dashboard over the 694-var surface | generated |

Regenerate the dashboard after surface changes:
`python3 tools/spec_coverage.py` (requires `clojuredocs-export.json` in the
repo root and a working jolt checkout).

## Current numbers (2026-06-10)

Of the 694 `clojure.core` vars in the ClojureDocs inventory:

- **380** implemented in jolt *and* exercised by the behavioral suites
- **154** implemented but not directly tested — each gets a test with its spec entry
- **35** portable but missing from jolt (`parse-long`/`parse-double`/
  `parse-boolean`, `update-keys`/`update-vals`, `macroexpand`, `time`,
  `partitionv`/`partitionv-all`/`splitv-at`, `with-redefs`, `with-open`,
  reader fns, ns-introspection stragglers, …) — tracked as implementation gaps
- **22** resolvable in code but invisible to ns introspection
  (`resolve`/`ns-publics` can't see seed-fallback names like `compare`,
  `gensym`, `type`) — a conformance finding in its own right
- the rest classified host/JVM/concurrency (see dashboard)

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
