# RFC 0001 — A Specification for the Clojure Language

- **Status**: Draft
- **Champions**: jolt maintainers
- **Created**: 2026-06-10

## Summary

Produce a normative, implementation-independent specification of the Clojure
language — the reader, the evaluation model, the special forms, the data types
and their equality/hashing/ordering contracts, sequences and laziness, and the
`clojure.core` library — to the standard set by R7RS Scheme and the Racket
reference. The specification is developed *in this repository*, validated
continuously by jolt's executable conformance suite, and intended to be useful
to every alternative implementation (ClojureScript, jank, babashka/sci,
Basilisp, ClojureCLR, jolt).

## Motivation

Clojure has no specification. The language is defined by:

1. the reference JVM implementation's source,
2. docstrings (frequently silent on edge cases),
3. community folklore (ClojureDocs examples, mailing-list threads),
4. each alternative implementation's reverse-engineering effort.

Every alternative implementation independently re-derives answers to the same
questions — *what does `(nth coll nil)` do? is `(first "")` an error? does
`conj` on `nil` produce a list or vector? in what order does `reduce-kv` visit
a map?* — and they routinely diverge. The cross-dialect
[clojure-test-suite](https://github.com/jank-lang/clojure-test-suite) exists
precisely because these divergences are real and frequent: it currently
encodes hundreds of edge-case assertions that no normative document captures.

Building jolt's self-hosted compiler forced us to answer these questions
one at a time (the conformance harness runs every behavior through three
independent execution paths and demands agreement). That work product — over
300 three-way-validated conformance assertions, ~1,500 behavioral spec cases,
and a frozen catalog of which forms are language vs. host — is the seed of a
specification, currently trapped in test files. This RFC proposes promoting it
into prose with normative force.

### Why us / why now

A useful spec needs an implementation that can *afford* to be strict. The
reference implementation can't adopt a spec retroactively without breaking
changes; an alternative implementation chasing drop-in compatibility can't
deviate from the reference even where the reference is accidental. jolt's
goals (self-hosted, minimal seed, multiple execution paths that must agree)
already require us to decide, for every form, *what the contract is* — we are
writing the spec anyway, in test form. The marginal cost of writing it down
properly is small; the value to the ecosystem is large.

## Goals

1. **Normative core**: reader grammar, evaluation model, all special forms,
   data types with equality/hashing/ordering contracts, seq/laziness
   contracts, namespaces/vars, and per-var entries for the portable
   `clojure.core` surface.
2. **Executable**: every normative statement is paired with at least one
   conformance test. The spec and the suite are maintained together; a spec
   claim without a test is marked `unverified`.
3. **Host classification**: every `clojure.core` var is classified
   **portable** (specified normatively), **host-dependent** (interface
   specified, behavior host-defined — e.g. `slurp`, `*out*`), or
   **JVM-specific** (documented as outside the portable language — e.g.
   `bases`, `definline`, agents/STM as currently scoped).
4. **Versioned against reference Clojure**: each spec edition states the
   reference version it describes (initially 1.12) and records *deliberate*
   divergences (e.g. where reference behavior is accidental — these become
   labeled "implementation-defined" with the reference behavior noted).
5. **Useful to other implementations**: no jolt-specific concepts in
   normative text. jolt appears only in conformance-suite references.

## Non-goals

- Specifying the JVM interop surface (`proxy`, `gen-class`, `.`-forms beyond
  their syntax), agents, STM refs, or the Java class hierarchy mapping.
  These are catalogued as host/JVM surface, not specified.
- Specifying `clojure.spec`, `core.async`, or other contrib libraries
  (candidates for later, separate documents).
- Changing the language. The spec describes Clojure as it is; divergence
  decisions document reality, they don't invent semantics.
- Replacing clojure-test-suite — we contribute to it and cite it.

## The specification document

Lives in `docs/spec/`. Shape (mirroring R7RS chapters):

| § | Document | Content |
|---|---|---|
| 0 | `00-front-matter.md` | conformance terms (RFC 2119), entry format, host classification |
| 1 | `01-evaluation.md` | evaluation model: forms, environments, vars, macroexpansion order |
| 2 | `02-reader.md` | lexical syntax: formal grammar, all reader macros, reader conditionals |
| 3 | `03-special-forms.md` | the special forms, one normative entry each |
| 4 | `04-data-types.md` | nil/booleans/numbers/strings/chars/keywords/symbols/colls; equality, hashing, ordering |
| 5 | `05-sequences.md` | the seq abstraction, laziness contract, realization boundaries |
| 6 | `06-namespaces-vars.md` | namespaces, vars, dynamic binding, resolution |
| 7 | `07-polymorphism.md` | protocols, records/types, multimethods, hierarchies |
| 8 | `08-macros.md` | defmacro, syntax-quote/hygiene, `&env`/`&form` |
| 9 | `09-core-library.md` | normative per-var entries for the portable surface |
| A | `coverage.md` | generated status dashboard: 694 vars × {specified, tested, implemented, classification} |

### The normative entry format

Every special form and library var gets an entry with these fields
(exemplars in `03-special-forms.md` and `09-core-library.md`):

```
### name
Signature(s), since-version
1. Semantics — numbered MUST/SHOULD statements
2. Edge cases — nil, empty, bounds, wrong-type behavior (normative)
3. Errors — what MUST throw, and when error type is implementation-defined
4. Examples — executable, drawn from ClojureDocs where community-validated
5. Conformance — test IDs that verify each numbered statement
```

### Evidence sources, in priority order

1. **Differential testing** against reference Clojure 1.12 (the ground truth
   for behavior questions).
2. **clojure-test-suite** (cross-dialect agreement = portable semantics;
   dialect splits = host-dependent candidates).
3. **ClojureDocs export** (`clojuredocs-export.edn`, 694 core vars, 648 with
   community examples) — examples become spec examples after verification.
4. **jank's language test corpus** (~800 per-form tests under
   `test/jank/{form,call,metadata,reader-macro,syntax-quote,var}`) — the
   per-construct granularity model for §2–§3 conformance.
5. Reference implementation source — last resort, for intent.

## Current baseline (measured 2026-06-10)

- ClojureDocs inventory: **694** `clojure.core` vars (648 with examples).
- jolt implements **572**; **373 (66%)** are exercised by the behavioral
  spec/conformance suites; 139 implemented-but-untested.
- Initial classification of the 182 unimplemented: ~31 dynamic vars, ~20
  agents/taps, ~11 STM, ~15 special-form docs, ~105 to adjudicate
  (genuinely-portable gaps spotted already: `compare`, `any?`, `update-keys`,
  `update-vals`, `parse-long`, `parse-double`, `parse-boolean`,
  `partitionv`, `splitv-at`, `macroexpand`, `time`, `with-redefs`).
- Conformance: 302 assertions × 3 execution paths; ~1,500 behavioral cases;
  clojure-test-suite ≥ 4081/4707 assertions.

## Process

1. **Section by section**, in chapter order. §2 (reader) and §3 (special
   forms) first — they are the smallest closed sets and jank's corpus gives
   per-construct conformance shape immediately.
2. Each PR that adds/edits normative text MUST add or cite the conformance
   tests for every numbered statement, and update `coverage.md`.
3. Divergences from reference Clojure discovered during writing get filed,
   then either fixed in jolt or recorded as a labeled divergence — never
   silently spec'd to jolt's behavior.
4. Editions: spec snapshots versioned independently of jolt releases
   (`Clojure Language Specification, Draft N`).
5. When a chapter stabilizes, solicit review from other implementations
   (jank, babashka, Basilisp maintainers) before marking it Stable.

## Alternatives considered

- **Contribute prose to clojure-test-suite instead**: the suite is the right
  *conformance* home but tests can't express rationale, classification, or
  grammar; both are needed and they cross-reference.
- **Spec only what jolt implements**: rejected — the host classification of
  the *full* 694-var surface is half the value.
- **EDN/data-format spec only** (edn already has a loose spec): far too
  narrow; the evaluation model and core library are where divergence lives.

## Open questions

1. Numerics: the reference has longs/doubles/ratios/BigInt with promotion
   rules; CLJS has JS numbers. Resolved: jolt carries the Scheme numeric tower
   (exact integers/bignums, exact ratios, flonum doubles), matching the
   reference's tower — see the numerics note in §4.
2. Where do `*print-length*`-style dynamic vars land — host-dependent
   interface or portable with defaults?
3. License/venue if the spec outgrows this repo (likely CC-BY; separate repo
   once §1–§3 stabilize).
