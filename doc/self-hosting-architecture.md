# Self-hosting architecture: portable jolt-core over a host runtime

Design for splitting Jolt into a **portable Clojure-in-Clojure core** and a
**host runtime** (Janet today, another runtime tomorrow), so the language is
truly self-hosted and `jolt-core` can be lifted out and re-hosted.

This is the design that must be right *before* writing the compiler in Clojure —
see [[self-hosting-compiler]] for the staged plan it plugs into.

## What "truly self-hosted + portable" requires

Two independent properties:

1. **Self-hosted** — the compiler and most of `clojure.core` are written in
   Clojure and compiled by Jolt itself.
2. **Portable** — that Clojure code (`jolt-core`) depends only on a small,
   explicit **host contract**, never on Janet directly. Re-hosting means
   implementing the contract for a new runtime; `jolt-core` is reused verbatim.

The enemy is `jolt-core` calling `janet/tuple`, `make-vec`, `ns-find`, etc.
directly — that welds it to Janet. Every host dependency must go through the
contract.

## Prior art (the seam everyone uses)

- **Clojure (JVM).** `clojure.lang.*` (Java) is the host: `RT`/`Numbers` runtime
  helpers, the `Compiler` (form → JVM bytecode), persistent data structures,
  `Var`/`Namespace`, the reader. `clojure/core.clj` is the language, in Clojure.
  Seam: ~20 primitive special forms + `RT` static methods. Everything else is
  Clojure.
- **ClojureScript (self-hosted).** Two portable passes — `cljs.analyzer`
  (form → AST **as data**, reading a **compiler-state map** of
  namespaces/defs/macros, *not* host objects) and `cljs.compiler` (AST → JS, the
  host-specific back end). `cljs.core` is Clojure compiled to JS. Platform splits
  live in `.cljc` reader conditionals. This is the closest model to what we want:
  **the analyzer is host-agnostic; only the back end and the runtime are
  host-specific.**
- **Nanopass / Guile Tree-IL.** A high-level IR is the portability seam; multiple
  back ends consume it.
- **ClojureCLR / ClojureDart / jank.** Same shape every time: portable analyzer +
  host back end + host runtime.

The invariant across all of them: **the IR (analyzer output) and a small runtime
protocol are the contract; the front end is portable, the back end and runtime
are per-host.**

## Decisions (locked)

- **Seam = a minimal host protocol.** `jolt-core` calls a small documented set of
  host fns (in ns `jolt.host`): `resolve-sym`, `macro?`, `macroexpand-1`,
  `current-ns`, `intern!`, plus the `RT` primitives. Each host provides `jolt.host`
  (+ RT). Re-hosting = reimplement that handful of fns. The protocol *is* the
  boundary; `jolt-core` never touches Janet directly.
- **Physical split now.** Portable Clojure lives under `jolt-core/` (a new source
  root, embedded into the binary like the rest of the stdlib); host Janet code for
  the new pipeline under `host/janet/`. Legacy host modules under `src/jolt/*.janet`
  are the existing Janet host and get relocated under `host/janet/` in a later
  mechanical pass (tracked) — not moved big-bang now, to keep the suite green.

## The Jolt split

```
jolt-core/            PORTABLE Clojure — no Janet. Depends only on the contract.
  ir                  the IR spec (data shapes the analyzer emits)
  analyzer            form -> IR        (macroexpands; resolves via host protocol)
  macros              when/cond/->/defn/... (the macro library, in Clojure)
  core                clojure.core fns expressible in Clojure, over RT primitives

host/janet/           THE HOST — Janet. Implements the contract.
  reader              text -> jolt forms
  rt                  data structures + RT primitive fns (cons/first/+/get/apply…)
  backend             IR -> Janet forms -> Janet compile -> bytecode  (the emitter)
  cenv                the compile-time host protocol impl (resolve/macro?/intern)
  bootstrap           load jolt-core, wire analyzer+backend into the loader
  interop             janet.* bridge
```

Two contracts cross the seam:

### 1. The IR (analyzer → back end)
The existing `:op`-tagged AST, made **host-neutral**:
- `{:op :const :val v}`, `:if`, `:do`, `:let`, `:fn` (arities), `:invoke`,
  `:vector`/`:map`/`:set`, `:quote`, `:throw`/`:try`, `:loop`/`:recur`.
- **Globals reference vars by NAME, not by host cell:**
  `{:op :var :ns "clojure.core" :name "map"}`. (compiler.janet today embeds the
  Janet var cell as a constant — that's a host leak and breaks AOT. Name-based
  refs are both portable and AOT-friendly; the back end resolves the cell.)
- No embedded host function values. Calls to runtime primitives are
  `{:op :rt :name "cons"}` resolved by the back end to the host's RT fn.

### 2. The host contract (two protocols)
- **Compile-time (`cenv`)** — what the analyzer needs from the host while
  analyzing: `(current-ns)`, `(resolve-sym sym) -> {:kind :var|:macro|:local|:special|:host, :ns, :name}`,
  `(macroexpand-1 form)`, `(intern! ns sym meta)`. The analyzer calls only these;
  it never touches Janet ns/var tables. (CLJS keeps this as pure data; we use a
  small protocol — a minimal, documented boundary — because Jolt already has live
  ns/var objects. The protocol *is* the seam.)
- **Runtime (`RT`)** — the primitive fns emitted code and `jolt-core` call by
  stable name: arithmetic/compare, `cons/first/rest/seq/conj/get/assoc/count`,
  `apply`, `=`, vector/map/set constructors, var deref/bind, keyword/symbol
  construction. The back end maps each to the host (on Janet, mostly the existing
  `core-*`). To re-host, implement this set.

## Why name-based vars (not embedded cells)

`compiler.janet` compiles a global ref to a closure over the Janet var cell. That
(a) is a Janet value baked into the IR — not portable, and (b) can't be marshaled
for AOT without the runtime-dict trick. Compiling instead to *resolve var by
(ns,name) at call time* through an RT primitive keeps redefinition live, makes the
IR host-neutral, and makes images trivially portable. The per-call lookup is the
cost; it can be cached/direct-linked later as an opt-in optimization.

## Bootstrap & staging (keeps the suite green throughout)

`compiler.janet` stays as the **bootstrap back end** until the Clojure pipeline is
proven. Order:

1. **Freeze the IR** spec and refactor `compiler.janet`'s emit to consume
   name-based `:var` (no behavior change; bootstrap still works).
2. **Define the host contract** (`cenv` + `RT`) and implement it on Janet,
   exposed under a stable namespace the Clojure core can call.
3. **Write `jolt.analyzer` in Clojure** producing IR, against `cenv`. Diff its IR
   against the Janet analyzer on the conformance corpus until identical.
4. **Janet back end consumes IR** from the Clojure analyzer; wire into the loader
   behind a flag. Validate at parity (dual-mode conformance + clojure-test-suite).
5. **Flip** the loader to the Clojure analyzer + Janet back end; `compiler.janet`
   shrinks to the back end only.
6. **Move `clojure.core`** macros then fns into `jolt-core` incrementally, each
   compiled by the prior stage, isolating host bits behind `RT`.

Guards at every step: the dual-mode conformance harness (interpret vs compile)
and the clojure-test-suite baseline.

## The portability test

When done, re-hosting Jolt to runtime X means writing only: `host/X/{reader, rt,
backend, cenv, bootstrap}`. `jolt-core/{ir, analyzer, macros, core}` is reused
unchanged. That is the concrete bar for "truly self-hosted and portable."
