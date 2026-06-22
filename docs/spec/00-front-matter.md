# Clojure Language Specification — Front Matter

**Edition**: Draft 1 · **Describes**: Clojure 1.12 (reference) · **Status**: in progress

This document specifies the Clojure programming language independently of any
implementation. See `docs/rfc/0001-language-specification.md` for motivation,
process, and scope.

## 1. Conformance terminology

The key words **MUST**, **MUST NOT**, **SHOULD**, **SHOULD NOT**, and **MAY**
are to be interpreted as described in RFC 2119.

- A statement marked **MUST** is normative: a conforming implementation
  exhibits exactly this behavior, and the conformance suite tests it.
- **implementation-defined** marks behavior a conforming implementation must
  document but may choose (e.g. the concrete error type thrown where the
  reference throws a JVM exception class).
- **host-defined** marks behavior delegated to the host platform (e.g. what
  `slurp` accepts as a source).
- **⚠ reference-divergence** marks a place where this spec deliberately
  differs from observed reference behavior, with rationale; the reference
  behavior is always recorded alongside.

## 2. Classification of the core surface

Every `clojure.core` var carries exactly one classification (dashboard:
`coverage.md`):

| Class | Meaning | Spec treatment |
|---|---|---|
| **portable** | semantics independent of host | full normative entry (§9) |
| **host-dependent** | portable *interface*, host-defined behavior | interface entry; behavior host-defined |
| **JVM-specific** | meaningful only on the JVM | catalogued in Appendix; not specified |

Initial classifications are mechanical and reviewable; reclassification is an
ordinary spec change.

## 3. The normative entry format

Each special form (§3) and portable var (§9) is specified as:

```
### name                                 — since <version>
(signature ...)  (signature ...)

Semantics
  S1. <numbered normative statement, MUST/SHOULD/MAY>
  S2. ...
Edge cases
  E1. <nil / empty / bounds / wrong-type behavior — normative>
Errors
  X1. <what MUST throw; error TYPE is implementation-defined unless stated>
Examples
  <executable; verified against the reference; sourced from ClojureDocs
   where community-validated>
Conformance
  S1 → <suite>/<test id>; E1 → ...   (statements without a test: UNVERIFIED)
```

The **Conformance** field is load-bearing: every numbered statement names the
test(s) that verify it. A normative statement with no test is labeled
`UNVERIFIED` and is a defect in the spec.

## 4. Evidence and verification

Behavioral questions are settled in this order: differential testing against
the reference implementation → cross-dialect agreement in clojure-test-suite
→ ClojureDocs community examples (verified before inclusion) → reference
source (for intent). Conformance tests live in this repository
(the corpus `test/chez/corpus.edn`, run on Chez via `host/chez/run-corpus.ss`
and certified against reference JVM Clojure by `test/conformance/certify.clj`)
and in the cross-dialect clojure-test-suite.

## 5. Chapter plan

| § | File | Status |
|---|---|---|
| 1 | `01-evaluation.md` | planned |
| 2 | `02-reader.md` | **drafted** (grammar + reader-macro catalog; 2 divergences open) |
| 3 | `03-special-forms.md` | **exemplars written** (`if`, `let*`); catalog complete |
| 4 | `04-data-types.md` | planned (numeric-tower design note required) |
| 5 | `05-sequences.md` | planned (laziness contract from jolt Phase-5 work) |
| 6 | `06-namespaces-vars.md` | planned |
| 7 | `07-polymorphism.md` | planned |
| 8 | `08-macros.md` | planned |
| 9 | `09-core-library.md` | **exemplars written** (`first`, `reduce`, `parse-uuid`) |
| A | `coverage.md` | **generated** (regenerate: `python3 tools/spec_coverage.py`) |
